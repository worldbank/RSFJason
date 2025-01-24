COHORTS_SELECTED <- reactiveVal(c())
LOAD_REPORTING_COHORT <- reactiveVal(0)
REFRESH_SELECTED_COHORT_DATA <- reactiveVal(1)

#All "reported" cohorts uploaded under the given program
COHORTS_LIST <- eventReactive(c(SELECTED_PROGRAM_ID(),
                                REFRESH_SELECTED_COHORT_DATA(),
                                LOAD_REPORTING_COHORT(),
                                input$dataset_review_filter_client,
                                input$dataset_review_filter),
{

  rsf_program_id <- SELECTED_PROGRAM_ID()
  rsf_facility_id <- as.numeric(NA)
  clientest_id <- as.numeric(input$dataset_review_filter_client)
  rx1<-REFRESH_SELECTED_COHORT_DATA()
  load_cohort_id <- LOAD_REPORTING_COHORT()
  phrase <- input$dataset_review_filter
  phrase <- trimws(phrase,whitespace="[ \\t\\r\\n\\v\\h\\s]")
  
  if (!isTruthy(rsf_program_id)) return (NULL)
  
  load_by_limit <- as.numeric(NA)
  if (isTruthy(phrase) &&
      any(grepl("recent\\s+\\d+$",phrase))==TRUE) {
    load_by_limit <- as.numeric(gsub("[[:alpha:][:space:]]+(\\d+)","\\1",phrase))
    phrase <- NULL
  } else {
    load_by_limit <- as.numeric(NA)
  }
  
  if (isTruthy(clientest_id)) {
    rsf_facility_id <- DBPOOL %>% dbGetQuery("
      select ids.rsf_facility_id
      from p_rsf.rsf_pfcbl_ids ids
      where ids.rsf_pfcbl_id = $1::int",
      params=list(clientest_id))
    rsf_facility_id <- as.numeric(unlist(rsf_facility_id))
  } else if (!is.na(load_by_limit)) {
    rsf_facility_id <- DBPOOL %>% dbGetQuery("
      select ids.rsf_facility_id
      from p_rsf.rsf_pfcbl_ids ids
      where ids.rsf_program_id = $1::int
        and ids.pfcbl_category = 'facility'",
      params=list(rsf_program_id))
    rsf_facility_id <- as.numeric(unlist(rsf_facility_id))
  }
  
  cohorts <- DBPOOL %>% dbGetQuery("
                                   select 
                                      ids.rsf_program_id,
                                      ids.rsf_facility_id,
                                      ids.rsf_client_id,
                                      ids.pfcbl_category,
                                      rc.reporting_cohort_id,
                                      rc.data_asof_date,
                                      rc.reporting_asof_date,
                                      rc.source_name,
                                      rc.reporting_user_id,
                                      rc.reporting_time,
                                      (rc.reporting_time) as reporting_date,
                                      rc.source_reference,
                                      rc.source_note,
                                      rc.reporting_rsf_pfcbl_id,
                                      rc.linked_reporting_cohort_id,
                                      rci.data_count_reported,
                                      rci.data_count_calculated,
                                      rci.data_current_count_reported,
                                      rci.data_current_count_calculated,
                                      rci.reporting_cohort_id is not null 
                                          and rc.parent_reporting_cohort_id is NULL
                                          and rc.linked_reporting_cohort_id is NULL 
                                      as is_deletable

                                    from p_rsf.reporting_cohorts rc
                                    inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = rc.reporting_rsf_pfcbl_id
                                    left join p_rsf.reporting_cohort_info rci on rci.reporting_cohort_id = rc.reporting_cohort_id
                                    where rc.rsf_program_id = $1::int
                                      and case when NULLIF($2::text,'NA') is NULL AND NULLIF($3::text,'NA') is NULL then rc.rsf_facility_id is NULL
                                               when NULLIF($2::text,'NA') is NULL then true
                                               else rc.rsf_facility_id = any(select unnest(string_to_array($2::text,','))::int) end
                                      and rc.parent_reporting_cohort_id is NULL -- top level reported cohorts
                                    order by rc.reporting_cohort_id desc
                                    limit (NULLIF($3::text,'NA'))::int",
                                   params=list(rsf_program_id,
                                               paste0(rsf_facility_id,collapse=","),
                                               as.character(load_by_limit)))

  #--and rc.rsf_facility_id is not distinct from $2::int
  if (empty(cohorts)) return (NULL)
  setDT(cohorts)
  
  names <- DBPOOL %>% dbGetQuery("
    select
      nids.rsf_pfcbl_id as reporting_rsf_pfcbl_id,
      nids.rsf_name as entity_name,
      nids.id as entity_id,
      nids.rsf_full_name
    from p_rsf.view_current_entity_names_and_ids nids
    where nids.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)",
    params=list(paste0(unique(cohorts$reporting_rsf_pfcbl_id),collapse=",")))
  setDT(names)
  
  cohorts[names,
          `:=`(entity_name=i.entity_name,
               endity_id=i.entity_id,
               rsf_full_name=i.rsf_full_name),
          on=.(reporting_rsf_pfcbl_id)]

  accounts <- DBPOOL %>% dbGetQuery("
    select
    accounts.account_id as reporting_user_id,
    accounts.users_name as reporting_user_name
    from p_rsf.view_account_info accounts
    where accounts.account_id = any(select unnest(string_to_array($1::text,','))::text)",
    params=list(paste0(unique(cohorts$reporting_user_id),collapse=",")))
  setDT(accounts)
  
  cohorts[,users_name:='UNKNOWN']
  cohorts[accounts,
          `:=`(reporting_user_name=i.reporting_user_name,
               users_name=sapply(i.reporting_user_name,format_name_abbreviation)),
          on=.(reporting_user_id)]
  
  cohorts[,upload_text:=paste0(toupper(format.Date(reporting_date,"%b%d %Hh%M")))]
  
  flags <- DBPOOL %>% dbGetQuery("
    select 
      ids.rsf_program_id,
      ids.rsf_facility_id,
      rdc.check_asof_date,
      count(*) as data_checks_active,
      count(*) filter(where coalesce(icg.overwrite_check_class,ic.check_class) = 'critical') as data_checks_critical_active,
      count(*) filter(where coalesce(icg.overwrite_check_class,ic.check_class) = 'error') as data_checks_error_active,
      count(*) filter(where coalesce(icg.overwrite_check_class,ic.check_class) = 'warning') as data_checks_warning_active,
      count(*) filter(where coalesce(icg.overwrite_check_class,ic.check_class) = 'info') as data_checks_info_active
    from p_rsf.rsf_pfcbl_ids ids
    inner join p_rsf.rsf_data_checks rdc on rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id
    inner join p_rsf.indicator_checks ic on ic.indicator_check_id = rdc.indicator_check_id
    left join p_rsf.indicator_check_guidance icg on icg.indicator_check_guidance_id = rdc.indicator_check_guidance_id
    where ids.rsf_program_id = $1::int
      and case when NULLIF($2::text,'NA') is NULL then true
               else ids.rsf_facility_id = any(select unnest(string_to_array($2::text,','))::int) end
    	and rdc.check_data_id_is_current = true
    	and rdc.check_status = 'active'
      and rdc.check_asof_date = any(select unnest(string_to_array($3::text,','))::date)
    group by
    ids.rsf_program_id,
    ids.rsf_facility_id,
    rdc.check_asof_date",
    params=list(rsf_program_id,
                paste0(rsf_facility_id,collapse=","),
                paste0(unique(cohorts$reporting_asof_date),collapse=",")))
  
  setDT(flags)
  
  cohorts[,
          `:=`(data_checks_active=0,
               data_checks_critical_active=0,
               data_checks_error_active=0,
               data_checks_warning_active=0,
               data_checks_info_active=0)]
  
  cohorts[flags,
          `:=`(data_checks_active=i.data_checks_active,
               data_checks_critical_active=i.data_checks_critical_active,
               data_checks_error_active=i.data_checks_error_active,
               data_checks_warning_active=i.data_checks_warning_active,
               data_checks_info_active=i.data_checks_info_active),
          on=.(rsf_program_id,
               rsf_facility_id,
               reporting_asof_date=check_asof_date)]
  
  cohorts[,clientest_rsf_pfcbl_id:=clientest_id]
  
  #0=Normal cohort reporting
  cohorts[,cohort_checks:=0]
  
  #1=Question mark flag (shouldn't happen)
  cohorts[(is.na(data_count_reported) | is.na(data_count_calculated)),
          cohort_checks:=1]

  #2=Didn't report anything
  cohorts[data_current_count_reported==0 &
          data_current_count_calculated==0,
          cohort_checks:=2]

  #3=Only system-calculated data reported (probably means overwrote whatever reported data triggered the calculation)
  cohorts[data_current_count_reported==0 &
          data_current_count_calculated >0,
          cohort_checks:=3]
  
  cohorts[(data_current_count_reported >0 |
          data_current_count_calculated >0) &
          data_checks_active==0,
          cohort_checks:=4]
  
  cohorts[,reporting_asof_date_label:=format_asof_date_label(reporting_asof_date)]
  cohorts[,flags:="<div style='display:inline-block;'>"]
  
  cohorts[cohort_checks==1,
          flags:=paste0(flags,"<i class='fa-solid fa-circle-question icon-info' title='Dataset may not have uploaded correctly: recommended to delete and try again'></i>")]
  
  cohorts[cohort_checks==2,
          flags:=paste0(flags,"<i class='fa-solid fa-ban icon-error' title='Dataset has reported nothing: is this a duplicated upload?'></i>")]

  cohorts[cohort_checks==3,
          flags:=paste0(flags,"<i class='fa-solid fa-calculator icon-warning fas' title='Dataset only has calcuated data (no reported data)' ",
                        "data-count='",data_current_count_calculated,"'></i>")]

  cohorts[cohort_checks==4,
          flags:=paste0(flags,"<i class='fa-solid fa-check icon-info' style='font-weight:bold;color:green;' title='Dataset has no active flags' ",
                        "data-count='",(data_current_count_reported+data_current_count_calculated),"'></i>")]
  
  cohorts[cohort_checks==0 & data_checks_critical_active>0,flags:=paste0(flags,"<i class='fas fa-fire icon-critical' data-count='",data_checks_critical_active,"'></i>")]
  cohorts[cohort_checks==0 & data_checks_error_active>0,flags:=paste0(flags,"<i class='fas fa-times-circle icon-error' data-count='",data_checks_error_active,"'></i>")]
  cohorts[cohort_checks==0 & data_checks_warning_active>0,flags:=paste0(flags,"<i class='fas fa-exclamation-triangle icon-warning' data-count='",data_checks_warning_active,"'></i>")]
  cohorts[cohort_checks==0 & data_checks_info_active>0,flags:=paste0(flags,"<i class='fas fa-info-circle icon-info' data-count='",data_checks_info_active,"'></i>")]
  cohorts[,flags:=paste0(flags,"</div>")]
  
  #https://stackoverflow.com/questions/51145207/r-shiny-datatable-how-to-prevent-row-selection-deselection-in-columns-containing    
  cohorts[,delete:=paste0("<input type='checkbox' ",ifelse(is_deletable==FALSE,"disabled='disabled'","")," name='cohort_actions' value=",reporting_cohort_id," onmousedown='event.stopPropagation();' onclick='Shiny.setInputValue(\"action_cohort_selected\",",reporting_cohort_id,",{priority:\"event\"})' />")] 
  cohorts[,
          actions:=paste0("<div style='display:inline-block;'>
<div onmousedown='event.stopPropagation();'  style='display:inline-block;'>
<i class='fa fa-eye icon-view pointer' title='View' onclick='Shiny.setInputValue(\"action_cohort_view\",",reporting_cohort_id,",{priority:\"event\"})'></i>
</div>
</div>")]
  
  {
    
    if (!isTruthy(phrase) || nchar(phrase) < 3) return (cohorts) 
    
    filter <- c()
    words <- unlist(strsplit(phrase,
                             split="[^[:alnum:]]"))
    
    for (i in 1:length(words)) {
      word <- words[i]
      if (nchar(word) > 5) { 
        filter <- c(filter,word) 
      } else if (i==length(words)) {
        filter <- c(filter,word) 
      } else {
        words[i+1] <- paste0(word," ",words[i+1])
      }
    }
    
    matches <- lapply(filter,function(x) {
      lapply(cohorts[,
                     .(reporting_cohort_id,
                       reporting_user_name,
                       source_name,
                       entity_name,
                       source_reference,
                       source_note,
                       reporting_asof_date,
                       reporting_asof_date_label)],
             grep,pattern=x,ignore.case=TRUE)
    })
    
    results <- list(reporting_cohort_id=NA,
                    reporting_user_name=NA,
                    source_name=NA,
                    entity_name=NA,
                    source_reference=NA,
                    source_note=NA,
                    reporting_asof_date=NA,
                    reporting_asof_date_label=NA)
    
    for (i in 1:length(matches)) results <- Map(c,results,matches[[i]])
    
    blanks <- sapply(results,function(x) all(is.na(x)))
    if (any(blanks)) results <- results[!blanks]
    
    if (length(filter)==1) { results <- unlist(results,use.names = F) #if one space, ie one term, then OR search
    } else { results <- Reduce(intersect,results)  }         #if space, ie multiple terms, then AND search across fields
    
    results <- results[!is.na(results)]
    results <- unique(results)
    
    if (length(matches) > 0) {
      filter_cohorts <- cohorts[results]
      if (!empty(filter_cohorts)) setorder(filter_cohorts,-reporting_asof_date,source_reference)
      return (filter_cohorts)
      
    } else {
      filter_cohorts <- parent_cohorts[FALSE]
      return (filter_cohorts)
    }
  }
  
  return (cohorts)
})

#When a user clicks on an icon in the main datasets view panel
SELECTED_COHORT_ID <- eventReactive(c(input$action_cohort_view,
                                      COHORTS_LIST()), {
  
  cohort_id <- as.numeric(input$action_cohort_view)
  cohorts <- COHORTS_LIST()
  if (!isTruthy(cohort_id)) return (NULL)
  if (!isTruthy(cohorts)) return (NULL)
  if (!cohort_id %in% cohorts$reporting_cohort_id) return (NULL)
  cohort_id
}, 
ignoreInit=TRUE,ignoreNULL=FALSE)


SELECTED_COHORT_INFO <- eventReactive(SELECTED_COHORT_ID(), {
  
  cohort_id <- SELECTED_COHORT_ID()
  if (!isTruthy(cohort_id)) return (NULL)
  cohorts <- COHORTS_LIST()
  
  if (!isTruthy(cohorts)) return (NULL)
  
  cohort <- cohorts[reporting_cohort_id==cohort_id]
  
  if (empty(cohort)) return (NULL)
  
  return (cohort)
  
}, 
ignoreNULL=FALSE)


#For the DATASE REVIEW panel: based on 
#Selected cohort will pull all flags for all cohorts under this selected cohort rsf_client_id
#Unless the panel is specifically requesting an individiual cohort
SELECTED_COHORT_FLAGS <- eventReactive(c(SELECTED_COHORT_INFO(),
                                         input$cohort_collection_selected_id, #NA or a specific reporting_cohort_id: collections are all timeseries uploads for the client
                                         REFRESH_SELECTED_COHORT_DATA()), { 
                                          
                                          
  cohort <- SELECTED_COHORT_INFO()

  
  if (!isTruthy(cohort)) return (NULL)
  
  reporting_current_date <- cohort$reporting_asof_date
  collection_id <- suppressWarnings(as.numeric(input$cohort_collection_selected_id)) #"all" will return NA
  
  flags_data <- NULL
  
  
  #select the entire collection: this will be the overwhelming majority of use cases.
  if (!isTruthy(collection_id)) {
    
    flags_data <- DBPOOL %>% dbGetQuery("
      select 
        rdc.evaluation_id,
        rdc.data_id,
        rdc.rsf_pfcbl_id,
        rdc.indicator_id,
        ind.indicator_name,
        ind.data_type,
        ind.data_category,
        ind.is_system as indicator_is_system,
        ind.is_calculated as indicator_is_calculated,
        ic.is_system as check_is_system,
        rdc.check_asof_date,
        rdc.indicator_check_id, 
        rdc.check_formula_id,
        ic.check_name,
        ic.check_type,
        coalesce(icg.overwrite_check_class,ic.check_class) as check_class,
        rdc.check_status,
        rdc.indicator_check_guidance_id,
        rdc.consolidated_from_indicator_id,
        rdc.consolidated_from_indicator_check_id,
        pis.is_subscribed,
        pis.formula_id as indicator_formula_id,
        pis.is_calculated as formula_is_calculated,
        indf.formula_title,
        indf.is_primary_default,
        indcf.check_formula_title
      from p_rsf.rsf_data_checks rdc
      inner join p_rsf.rsf_data rd on rd.data_id = rdc.data_id 
      inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
      inner join p_rsf.indicators ind on ind.indicator_id = rdc.indicator_id
      inner join p_rsf.indicator_checks ic on ic.indicator_check_id = rdc.indicator_check_id
      left join p_rsf.indicator_check_guidance icg on icg.indicator_check_guidance_id = rdc.indicator_check_guidance_id
      left join p_rsf.view_rsf_pfcbl_indicator_subscriptions pis on pis.rsf_pfcbl_id = rdc.rsf_pfcbl_id
                                                                and pis.indicator_id = rdc.indicator_id
      left join p_rsf.indicator_formulas indf on indf.formula_id = pis.formula_id
      left join p_rsf.indicator_check_formulas indcf on indcf.check_formula_id = rdc.check_formula_id
      where rc.reporting_asof_date = $1::date
        and rc.reporting_rsf_pfcbl_id = (select rcc.reporting_rsf_pfcbl_id 
                                         from p_rsf.reporting_cohorts rcc
                                         where rcc.reporting_cohort_id = $2::int)
      and rdc.check_data_id_is_current = true",
      params=list(reporting_current_date,
                  cohort$reporting_cohort_id))
    
  
  } 
  else {
    flags_data <- DBPOOL %>% dbGetQuery("
      select 
        rdc.evaluation_id,
        rdc.data_id,
        rdc.rsf_pfcbl_id,
        rdc.indicator_id,
        ind.indicator_name,
        ind.data_type,
        ind.data_category,
        ind.is_system as indicator_is_system,
        ind.is_calculated as indicator_is_calculated,
        ic.is_system as check_is_system,
        rdc.check_asof_date,
        rdc.indicator_check_id, 
        rdc.check_formula_id,
        ic.check_name,
        ic.check_type,
        coalesce(icg.overwrite_check_class,ic.check_class) as check_class,
        rdc.check_status,
        rdc.indicator_check_guidance_id,
        rdc.consolidated_from_indicator_id,
        rdc.consolidated_from_indicator_check_id,
        pis.is_subscribed,
        pis.formula_id as indicator_formula_id,
        pis.is_calculated as formula_is_calculated,
        indf.formula_title,
        indf.is_primary_default,
        indcf.check_formula_title
      from p_rsf.rsf_data_checks rdc
      inner join p_rsf.rsf_data rd on rd.data_id = rdc.data_id 
      inner join p_rsf.indicators ind on ind.indicator_id = rdc.indicator_id
      inner join p_rsf.indicator_checks ic on ic.indicator_check_id = rdc.indicator_check_id
      left join p_rsf.indicator_check_guidance icg on icg.indicator_check_guidance_id = rdc.indicator_check_guidance_id
      left join p_rsf.view_rsf_pfcbl_indicator_subscriptions pis on pis.rsf_pfcbl_id = rdc.rsf_pfcbl_id
                                                                and pis.indicator_id = rdc.indicator_id
      left join p_rsf.indicator_formulas indf on indf.formula_id = pis.formula_id
      left join p_rsf.indicator_check_formulas indcf on indcf.check_formula_id = rdc.check_formula_id
      where rd.reporting_cohort_id = any (select $1::int as reporting_cohort_id
      																		union all
      																		select reporting_cohort_id 
      																		from p_rsf.reporting_cohorts 
      																		where parent_reporting_cohort_id = $1::int)
      and rdc.check_data_id_is_current = true",
      params=list(collection_id))
  }

  setDT(flags_data)
  
  flags_data[,
             pfcbl_category_rank:=fcase(data_category=="global",0,
                                        data_category=="program",1,
                                        data_category=="facility",2,
                                        data_category=="client",3,
                                        data_category=="borrower",4,
                                        data_category=="loan",5,
                                        default=NA)]
  
  
  flags_data[,check_rank:=fcase(check_class=="critical",1,
                                check_class=="error",2,
                                check_class=="warning",3,
                                check_class=="info",4,
                                default=5)]
  

  flags_data[,
             indicator_flag_id:=.GRP,
             by=.(indicator_id,
                  indicator_check_id,
                  check_formula_id,
                  indicator_check_guidance_id,
                  consolidated_from_indicator_id,
                  consolidated_from_indicator_check_id)]
  
  return(flags_data)  
}, ignoreNULL=FALSE)



#Indicators-level view for cohort review panel: collapsed evaluation_ids by indicator and check
SELECTED_COHORT_INDICATOR_FLAGS_FILTERS <- eventReactive(c(SELECTED_COHORT_FLAGS(),
                                                            input$cohort_view_flagged_data,  #ALL/RESOLVED/ACTIVE
                                                            input$cohort_view_flag_classes,
                                                            input$cohort_view_flag_types), #error/warning/info  
{
  cohort_flags <- SELECTED_COHORT_FLAGS()
  if (is.null(cohort_flags)) return (NULL)
  
  if (!isTruthy(cohort_flags) || empty(cohort_flags)) return (NULL)
  
  cohort_indicator_flags <- cohort_flags[,
                                         .(active_count=sum(check_status=="active"),
                                           resolved_count=sum(check_status=="resolved"),
                                           evaluation_ids=list(evaluation_id)),
                                         by=.(indicator_flag_id,
                                              indicator_id,
                                              indicator_name,
                                              data_type,
                                              data_category,
                                              indicator_check_id,
                                              check_name,
                                              check_class,
                                              check_type,
                                              check_formula_id,
                                              check_formula_title,
                                              consolidated_from_indicator_id,
                                              consolidated_from_indicator_check_id,
                                              indicator_is_system,
                                              indicator_is_calculated,
                                              check_is_system,
                                              check_rank,
                                              pfcbl_category_rank,
                                              formula_is_calculated,
                                              indicator_formula_id,
                                              formula_title,
                                              is_primary_default)]
  

  view_data_flags <- toupper(input$cohort_view_flagged_data)
  if (!isTruthy(view_data_flags)) view_data_flags <- "ALL"
  
  flagged_filter <- NA
  
  if ("ACTIVE" %in% view_data_flags) flagged_filter <- flagged_filter | cohort_indicator_flags$active_count > 0
  if ("RESOLVED" %in% view_data_flags) flagged_filter <- flagged_filter | cohort_indicator_flags$resolved_count > 0
  if ("ALL" %in% view_data_flags) flagged_filter <- flagged_filter | TRUE
  
  cohort_indicator_flags <- cohort_indicator_flags[flagged_filter==TRUE]
  
  view_data_classes <- tolower(input$cohort_view_flag_classes)
  view_data_classes <- intersect(c("critical","error","warning","info"),view_data_classes)
  if (length(view_data_classes)>0) {
    cohort_indicator_flags <- cohort_indicator_flags[check_class %in% view_data_classes]
  }

  rsf_check_types <- RSF_CHECK_TYPES()
  view_flag_types <- tolower(input$cohort_view_flag_types)
  
  view_flag_types <- intersect(c(rsf_check_types$type_class,unique(cohort_indicator_flags$data_category)),view_flag_types)
  
  if (length(view_flag_types) >0) {
    cohort_indicator_flags <- cohort_indicator_flags[check_type %in% rsf_check_types[type_class %in% view_flag_types,check_type] |
                                                     data_category %in% view_flag_types]
  }
  
  if (empty(cohort_indicator_flags)) { 
    return(NULL) 
  }
  
  cohort_indicator_flags[,indicator_html:=mapply(format_html_indicator,
                                                 indicator_name=indicator_name,
                                                 data_category=data_category,
                                                 data_type=data_type,
                                                 is_system=indicator_is_system,
                                                 is_calculated=indicator_is_calculated)]
  
  cohort_indicator_flags[,check_html:=mapply(format_html_check,
                                             check_name=check_name,
                                             check_class=check_class,
                                             check_type=check_type,
                                             is_subscribed=!(active_count==0), #to gray-out checks with zero activity; just formatting
                                             is_system=check_is_system)]
  cohort_indicator_flags[,formula_html:=""]
  cohort_indicator_flags[indicator_is_calculated==TRUE &
                         formula_is_calculated==FALSE,
                         formula_title:="Formula Disabled"]
  
  cohort_indicator_flags[indicator_is_calculated==TRUE |
                         formula_is_calculated==TRUE,
                         formula_html:=mapply(format_html_indicator,
                                              indicator_name=gsub("'","%39;",formula_title),
                                              data_category=fcase(is_primary_default==TRUE,"formula",
                                                                  is_primary_default==FALSE,"customformula",
                                                                  default="none"),
                                              data_type=data_type,
                                              is_system=FALSE,
                                              is_calculated=TRUE,
                                              is_subscribed=TRUE,
                                              id=indicator_formula_id)]
  
  cohort_indicator_flags[,check_formula_html:=""]
  cohort_indicator_flags[is.na(check_formula_id)==FALSE &
                         nchar(check_formula_title)==0,
                         check_formula_title:="Untitled Check Formula"]
  
  cohort_indicator_flags[is.na(check_formula_id)==FALSE,
                         check_formula_html:=mapply(format_html_indicator,
                                                    indicator_name=gsub("'","%39;",check_formula_title),
                                                    data_category="formula",
                                                    data_type="",
                                                    is_system=FALSE,
                                                    is_calculated=FALSE,
                                                    is_subscribed=TRUE,
                                                    id=check_formula_id)]
  
  
  consolidated_flags <- cohort_indicator_flags[is.na(consolidated_from_indicator_id)==FALSE &
                                               is.na(consolidated_from_indicator_check_id)==FALSE,
                                               .(consolidated_from_indicator_id,
                                                 consolidated_from_indicator_check_id,
                                                 is_active=!(active_count==0),
                                                 check_class)]
  
  if (!empty(consolidated_flags)) {
    consolidated_flags <- unique(consolidated_flags)
    src_indicators <- RSF_INDICATORS()
    
    #querying to ensure getting system checks that are consolidated
    src_indicators_checks <- DBPOOL %>% dbGetQuery("
      select
        ic.indicator_check_id,
        ic.check_name,
        ic.check_type,
        ic.is_system
      from p_rsf.indicator_checks ic
      where ic.indicator_check_id = any(select unnest(string_to_array($1::text,','))::int)",
      params=list(unique(consolidated_flags$consolidated_from_indicator_check_id)))
    
    consolidated_flags[src_indicators,
                       `:=`(src_indicator_html=mapply(format_html_indicator,
                                                      indicator_name=i.indicator_name,
                                                      data_category=i.data_category,
                                                      data_type=i.data_type,
                                                      is_system=i.is_system,
                                                      is_calculated=i.is_calculated),
                            src_pfcbl_rank=fcase(i.data_category=="global",0,
                                                 i.data_category=="program",1,
                                                 i.data_category=="facility",2,
                                                 i.data_category=="client",3,
                                                 i.data_category=="borrower",4,
                                                 i.data_category=="loan",5,
                                                 default=NA)),
                       on=.(consolidated_from_indicator_id=indicator_id)]
    
    consolidated_flags[src_indicators_checks,
                       src_check_html:=mapply(format_html_check,
                                              check_name=i.check_name,
                                              check_class=check_class,
                                              check_type=check_type,
                                              is_subscribed=is_active,
                                              is_system=i.is_system),
                       on=.(consolidated_from_indicator_check_id=indicator_check_id)]
    
    #A little bit confusing...but consolidated flags will go under an entity reporting indicator as a placeholder.  But really nobody cares what the placeholder is.
    #And if we want to dig into the details, then we want to look at the problems on the actual indicators, too.
    cohort_indicator_flags[consolidated_flags,
                           `:=`(indicator_html=i.src_indicator_html,
                                for_indicator_id=i.consolidated_from_indicator_check_id,
                                pfcbl_category_rank=i.src_pfcbl_rank,
                                check_html=paste0("<div>",check_html," ",src_check_html,"</div>")),
                           on=.(consolidated_from_indicator_id,
                                consolidated_from_indicator_check_id)]
  }
  
  setorder(cohort_indicator_flags,
           pfcbl_category_rank,
           check_rank,
           check_name)

  cohort_indicator_flags[,action_review:=paste0("<div style='display:inline-block;'>
  <div onmousedown='event.stopPropagation();' style='display:inline-block;'>
  <i class='fas fa-eye icon-view pointer' 
  title='Review Flags' 
  onclick='Shiny.setInputValue(\"action_indicator_flags_review\",\"",indicator_flag_id,"\",{priority:\"event\"})' /></i>
  </div>
  </div")]
  
  cohort_indicator_flags
  
}, ignoreNULL = FALSE)

SELECTED_COHORT_INDICATOR_FLAGS_CLASSIFICATION <- eventReactive(SELECTED_COHORT_INDICATOR_FLAGS_FILTERS(), {
  if (is.null(SELECTED_COHORT_INDICATOR_FLAGS_FILTERS())) return (NULL)                                                                    
  if (empty(SELECTED_COHORT_INDICATOR_FLAGS_FILTERS())) return (data.table(indicator_id=numeric(0),
                                                                           classification=character(0),
                                                                           indicator_name=character(0)))

  flags <- SELECTED_COHORT_INDICATOR_FLAGS_FILTERS()
  
  classification_indicators <- DBPOOL %>% dbGetQuery("
    select ind.indicator_id,ind.classification,ind.indicator_name
    from p_rsf.view_rsf_program_facility_indicator_subscriptions pis
    inner join p_rsf.indicators ind on ind.indicator_id = pis.indicator_id
    where pis.rsf_pfcbl_id=$1::int
      and ind.classification is not null
      and pis.is_subscribed = true
    
    union 	
    
    select parameter_id,ind.classification,ind.indicator_name
    from p_rsf.view_rsf_program_facility_indicator_subscriptions pis
    inner join p_rsf.indicators ind on ind.indicator_id = pis.indicator_id
    inner join p_rsf.indicator_formulas indf on indf.formula_id = pis.formula_id
    inner join lateral unnest(indf.formula_indicator_id_requirements) as parameter_id on true
    where pis.rsf_pfcbl_id=$1::int
      and ind.classification is not null
      and pis.is_subscribed = true",
    params=list(SELECTED_COHORT_INFO()$reporting_rsf_pfcbl_id))
  
  setDT(classification_indicators)
  
  classification_indicators <- classification_indicators[indicator_id %in% flags$indicator_id]
  setorder(classification_indicators,
           indicator_name,
           indicator_id)
  
  return (classification_indicators)

},ignoreNULL=TRUE)

SELECTED_COHORT_INDICATOR_FLAGS_FILTERED <- eventReactive(c(input$cohort_view_flag_indicator_classifications,
                                                            SELECTED_COHORT_INDICATOR_FLAGS_FILTERS()), {
  flags <- SELECTED_COHORT_INDICATOR_FLAGS_FILTERS()
  filter <- input$cohort_view_flag_indicator_classifications
  
  if (isTruthy(filter)) {
    indicator_classifications <- SELECTED_COHORT_INDICATOR_FLAGS_CLASSIFICATION()
    
    if (filter=="all") {
      flags <- flags[indicator_id %in% indicator_classifications$indicator_id |
                     check_class %in% "critical"]  
    } else {
      
      indicator_classifications <- indicator_classifications[indicator_name==filter]
      flags <- flags[indicator_id %in% indicator_classifications$indicator_id |
                     check_class %in% "critical"]
    }
    
  }
  return (flags)
})

#Populates select input to filter on client/facility name
observeEvent(SELECTED_PROGRAM_CLIENTS_LIST(), {
  clients <- SELECTED_PROGRAM_CLIENTS_LIST()
  
  if (empty(clients)) {
    updateSelectizeInput(session=session,
                         inputId="dataset_review_filter_client",
                         choices = c(`Client Filter...`=""),
                         selected = "")
    
  } else {
    setorder(clients,
             client_name)
    clients_choices <- c(`Client Filter...`="",
                         setNames(clients$rsf_pfcbl_id,clients$client_name))
    updateSelectizeInput(session=session,
                         inputId="dataset_review_filter_client",
                         choices = clients_choices,
                         selected = "")
                         
  }
}, ignoreNULL = FALSE)

observeEvent(SELECTED_COHORT_INDICATOR_FLAGS_FILTERS(), {
  
  if (empty(SELECTED_COHORT_INDICATOR_FLAGS_FILTERS()) ||
      empty(SELECTED_COHORT_INDICATOR_FLAGS_CLASSIFICATION())) {
    updateSelectizeInput(session=session,
                         inputId="cohort_view_flag_indicator_classifications",
                         choices = "",
                         selected = "",
                         options=list(placeholder="No priority indicator flags"))
  
  } else {
    indicators <- SELECTED_COHORT_INDICATOR_FLAGS_CLASSIFICATION()
    indicators <- unique(indicators$indicator_name)
    class_choices <- c("",
                       `All Priority Flags`="all",
                       indicators)
    
    updateSelectizeInput(session=session,
                         inputId="cohort_view_flag_indicator_classifications",
                         choices = class_choices,
                         selected = "",
                         options=list(placeholder=paste0(length(indicators)," priority indicators have flags...")))
  }
},ignoreNULL=FALSE)

# observeEvent(SELECTED_COHORT_INDICATOR_FLAGS_CLASSIFICATION(), {
#   
#   indicators <- SELECTED_COHORT_INDICATOR_FLAGS_CLASSIFICATION()
#   if (empty(indicators)) {
#     updateSelectizeInput(session=session,
#                          inputId="cohort_view_flag_indicator_classifications",
#                          choices = "",
#                          selected = "",
#                          options=list(placeholder="No priority indicator flags"))
#     
#   } else {
#     
#     
#     
#   }
# },ignoreNULL = TRUE)

observeEvent(SELECTED_COHORT_ID(), {
  
  req(SELECTED_PROGRAM_ID())
  
  cohort_id <- SELECTED_COHORT_ID()

  if (!isTruthy(cohort_id)) {
    hideElement("dataset_review_header")
    current_panel <- input$datasetsTabset
    
    #had selected a cohort and now have changed it (ie, changed by not by user click, but programmatic click)
    if (isTruthy(current_panel) && current_panel=="review") { # && isTruthy(as.numeric(input$action_cohort_view))) {
      
      updateSelectizeInput(session=session,
                           inputId="cohort_collection_selected_id",
                           choices="",
                           selected="")
      
      updateTabsetPanel(session=session,inputId="datasetsTabset",selected="list")
    }
  
  } else {
    showElement("dataset_review_header")
    
    cids <- unique(COHORTS_LIST()[reporting_cohort_id==cohort_id,
                                  .(rsf_program_id,
                                    rsf_facility_id,
                                    rsf_client_id,
                                    reporting_asof_date)])
    cohorts <- COHORTS_LIST()[cids,
                              on=.(rsf_program_id,
                                   rsf_facility_id,
                                   rsf_client_id,
                                   reporting_asof_date),
                              nomatch=NULL][,.(reporting_cohort_id,
                                               source_name)]
    
    choices <- NULL
    chocies.selected <- NULL

    if (nrow(cohorts)==1) {
      choices.selected <- cohort_id
      choices <- setNames(cohorts$reporting_cohort_id,
                          cohorts$source_name)
      
    } else {
      cohort_info <- SELECTED_COHORT_INFO()
      choices.selected <- cohort_id
      choices <- setNames(c("all",cohorts$reporting_cohort_id),
                          c(paste0("All ",cohort_info$reporting_asof_date_label," ",cohort_info$entity_name," reporting"),
                            cohorts$source_name))
    }
    updateSelectizeInput(session=session,
                         inputId="cohort_collection_selected_id",
                         choices=choices,
                         selected=choices.selected)
    
    current_panel <- input$datasetsTabset
    
    if (!isTruthy(current_panel) || current_panel != "review") {
      updateTabsetPanel(session=session,inputId="datasetsTabset",selected="review")
    }
  } 
  
},ignoreNULL=FALSE,ignoreInit = TRUE,priority=10)

#
observeEvent(SELECTED_COHORT_FLAGS(), {
  flags_data <- SELECTED_COHORT_FLAGS()
  
  if (is.null(flags_data)) return (NULL)

  {
    view_data.choices <- c("")
    view_data.selected <- ""
    if (any(flags_data$check_status=="active")) {
      view_data.choices <- c(view_data.choices,`Active Flags`="ACTIVE")
      view_data.selected <- "ACTIVE"
    }
    if (any(flags_data$check_status=="resolved")) view_data.choices <- c(view_data.choices,`Resolved Flags`="RESOLVED")
    if (empty(flags_data)) view_data.choices <- c(view_data.choices,`Not Flagged`="NONE")
    
    
    
    #if (isTruthy(input$cohort_view_flagged_data)) view_data.selected <- intersect(view_data.choices,input$cohort_view_flagged_data)
    
    updateSelectizeInput(session = session,
                         inputId="cohort_view_flagged_data",
                         choices=view_data.choices,
                         selected=view_data.selected)
    
    if (!empty(flags_data)) {
      cohort_flags <- flags_data[order(check_rank),unique(check_class)]
      cohort_flags <- tolower(cohort_flags)
      
      flag.choices <- c()
      if ("critical" %in% cohort_flags) flag.choices <- c(flag.choices,setNames("critical","Critical <i class='fas fa-fire icon-critical' title='Critical flags'></i>"))
      if ("error" %in% cohort_flags) flag.choices <- c(flag.choices,setNames("error","Error <i class='fas fa-times-circle icon-error' title='Error flags'></i>"))
      if ("warning" %in% cohort_flags) flag.choices <- c(flag.choices,setNames("warning","Warning <i class='fas fa-exclamation-triangle icon-warning' title='Warning flags'></i>"))
      if ("info" %in% cohort_flags) flag.choices <- c(flag.choices,setNames("info","Info <i class='fas fa-info-circle icon-info' title='Info flags'></i>"))
      
      flags.selected <- cohort_flags
      
      #if (isTruthy(input$cohort_view_flag_classes)) flags.selected <- input$cohort_view_flag_classes
      
      updateSelectizeInput(session = session,
                           inputId="cohort_view_flag_classes",
                           choices=flag.choices,
                           selected=flags.selected)

      rsf_check_types <- RSF_CHECK_TYPES()
      cohort_types <- rsf_check_types[check_type %in% unique(flags_data$check_type),unique(type_class)]
      cohort_types <- tolower(cohort_types)
      
      data_types <- unique(flags_data$data_category)

      type.choices <- c(`Any Flag Types`="")
      if ("contract" %in% cohort_types) type.choices <- c(type.choices,setNames("contract","Contract Compliance <i class='fa-solid fa-square' style='color:limegreen'></i>"))
      if ("business" %in% cohort_types) type.choices <- c(type.choices,setNames("business","Business Rules <i class='fa-solid fa-square' style='color:skyblue'></i>"))
      if ("data" %in% cohort_types) type.choices <- c(type.choices,setNames("data","Data Validity <i class='fa-solid fa-square' style='color:violet'></i>"))
      if ("none" %in% cohort_types) type.choices <- c(type.choices,setNames("none","Unclassified <i class='fa-solid fa-square' style='color:pink'></i>"))
      
      if ("loan" %in% data_types) type.choices <- c(type.choices,setNames("loan","<i class='fa-solid fa-circle icon-loan'></i> Loan"))
      if ("borrower" %in% data_types) type.choices <- c(type.choices,setNames("borrower","<i class='fa-solid fa-circle icon-borrower'></i> Borrower"))
      if ("client" %in% data_types) type.choices <- c(type.choices,setNames("client","<i class='fa-solid fa-circle icon-client'></i> Client"))
      if ("facility" %in% data_types) type.choices <- c(type.choices,setNames("facility","<i class='fa-solid fa-circle icon-facility'></i> Facility"))
      if ("program" %in% data_types) type.choices <- c(type.choices,setNames("program","<i class='fa-solid fa-circle icon-program'></i> Program"))
      if ("global" %in% data_types) type.choices <- c(type.choices,setNames("global","<i class='fa-solid fa-circle icon-global'></i> Global"))
      
      types.selected <- "" #By default none selected.
      #if (isTruthy(input$cohort_view_flag_types)) types.selected <- input$cohort_view_flag_types
      
      updateSelectizeInput(session = session,
                           inputId="cohort_view_flag_types",
                           choices=type.choices,
                           selected=types.selected)
      

    } else {
      updateSelectizeInput(session = session,
                           inputId="cohort_view_flag_classes",
                           choices="",
                           selected="")
      updateSelectizeInput(session = session,
                           inputId="cohort_view_flag_types",
                           choices="",
                           selected="")
      
    }
  }  
}, ignoreNULL=FALSE, priority=100)


observeEvent(input$action_dataset_review_filter_clear, {
  
  shinyjs::runjs(paste0("Shiny.setInputValue('action_cohort_view','',{priority:'event'})"))
  
  updateTextInput(session=session,inputId="dataset_review_filter",value="")
  updateTextInput(session=session,inputId="dataset_review_filter_client",value="")
  
}, ignoreInit = TRUE)

observeEvent(input$action_cohort_delete, {
  delete_cohort_ids <- COHORTS_SELECTED()
  program_id <- SELECTED_PROGRAM_ID()
  
  tryCatch({
    

  withProgress(value=.15,message="Deleting datasets...",
               {
                 delete_cohort_ids <- unique(na.omit(delete_cohort_ids))
                 if (length(delete_cohort_ids)==0) {
                   return(showNotification(type="error",
                                           h3("Bad selection: reporting cohorts do not exist or may have already been deleted")))
                 }
                 
                 affected_program <- DBPOOL %>% dbGetQuery("select distinct
                                                              rc.rsf_program_id
                                                             from p_rsf.reporting_cohorts rc
                                                             where rc.reporting_cohort_id = any(select unnest(string_to_array($1::text,','))::int)",
                                                            params=list(paste0(delete_cohort_ids,collapse=",")))
                 
                 affected_program <- unlist(affected_program)
                 if (length(affected_program) == 0) {
                   return(showNotification(type="error",
                                           h3("Bad selection: reporting cohorts do not exist or may have already been deleted")))
                   
                 } else if (length(affected_program) != 1) {
                   return(showNotification(type="error",
                                           h3("Bad selection: Deletes can only affect one Program at a time")))
                 }
                 
                 incProgress(amount=0.20,message="Deleting datasets . . . ")
                 
                 # DBPOOL %>% dbExecute("delete
                 #                       from p_rsf.reporting_cohorts rc
                 #                       where rc.reporting_cohort_id = any(select unnest(string_to_array($1::text,','))::int)",
                 #                       params=list(paste0(delete_cohort_ids,collapse=",")))

                 DBPOOL %>% dbExecute("insert into p_rsf.deleted_reporting_cohorts(reporting_cohort_id,deleting_user_id)
                                       select
                                         unnest(string_to_array($1::text,','))::int,
                                         $2::text",
                                      params=list(paste0(delete_cohort_ids,collapse=","),
                                                  USER_ID()))
                 
                 #If program doesnt exist after delete then it means we've deleted the entire program
                 exists <- DBPOOL %>% dbGetQuery("select exists(select * from p_rsf.rsf_programs 
                                                                where rsf_program_id = $1::int)::bool as program_exists",
                                                 params=list(affected_program))

                 if (unlist(exists)==FALSE) {
                   LOAD_PROGRAM_ID((-program_id))
                   
                 } else {
                   
                   incProgress(amount=0.20,
                               message="Recalculating affected data . . . ")
                   
                   progress_status_message <- function(class,...) {
                     dots <- list(...)
                     dots <- paste0(unlist(dots),collapse=" ")
                     incProgress(amount=0,
                                 message=paste0("Recalculating affected data: ",dots))
                   }
                   
                   DBPOOL %>% rsf_program_calculate(rsf_program_id = affected_program,
                                                    rsf_indicators = RSF_INDICATORS(),
                                                    rsf_pfcbl_id.family = NULL,
                                                    status_message=progress_status_message)
                     
                   incProgress(amount=0.30,
                               message="Rechecking affected data . . . ")
  
                   progress_status_message <- function(class,...) {
                     dots <- list(...)
                     dots <- paste0(unlist(dots),collapse=" ")
                     incProgress(amount=0,
                                 message=paste0("Rechecking affected data: ",dots))
                   }
  
                   DBPOOL %>% rsf_program_check(rsf_program_id=affected_program,
                                                rsf_indicators=RSF_INDICATORS(),
                                                rsf_pfcbl_id.family=NULL,
                                                check_future=TRUE,
                                                check_consolidation_threshold=NA,
                                                reference_asof_date=NULL,
                                                status_message= progress_status_message)
                 }
                 incProgress(amount=1,message="Done")
               })

  },
  error=function(e) {
    showNotification(type="error",
                     duration=NULL,
                     ui=h3(paste0("An error occurred when deleting, recalculating and rechecking datasets: ",
                                  conditionMessage(e))))
  },
  warning=function(w) {
    showNotification(type="error",
                     duration=NULL,
                     ui=h3(paste0("An error occurred when deleting, recalculating and rechecking datasets: ",
                                  conditionMessage(w))))  
    
  })
  
  COHORTS_SELECTED(c())
  
  LOAD_REPORTING_COHORT(delete_cohort_ids)
  
},ignoreInit=TRUE,ignoreNULL=TRUE,priority = 100)

#For deleting selected cohorts through checkboxes in datatable
observeEvent(input$action_cohort_selected, {
  action <- as.numeric(input$action_cohort_selected)
  selected <- COHORTS_SELECTED()
  if (action %in% selected) selected <- selected[-which(action==selected)]
  else selected <- c(selected,action)
  COHORTS_SELECTED(selected)
  
},ignoreNULL = FALSE,ignoreInit = TRUE)

#OBSOLETE: intended to display in dashboard precisely what was uploaded.  Not used/useful
#Button to View Dashboard from Upload Review panel
SERVER_DATASETS_COHORT_DASHBOARD <- function(selected_cohort_id=SELECTED_COHORT_ID(),
                                             flags_filter="",
                                             dashboard_parameters=list()) {
  
  uploaded_cohorts <- DBPOOL %>% dbGetQuery("select 
                                              rc.reporting_cohort_id,
                                              rc.reporting_asof_date::text,
                                              rc.reporting_rsf_pfcbl_id
                                              from p_rsf.reporting_cohorts rc
                                              where rc.reporting_cohort_id = $1::int
                                           
                                           union 
                                           
                                           select 
                                              rc.reporting_cohort_id,
                                              rc.reporting_asof_date::text,
                                              rc.reporting_rsf_pfcbl_id
                                              from p_rsf.reporting_cohorts rc
                                              where rc.parent_reporting_cohort_id = $1::int
                                           
                                           union 
                                           
                                           select 
                                              rc.reporting_cohort_id,
                                              rc.reporting_asof_date::text,
                                              rc.reporting_rsf_pfcbl_id
                                              from p_rsf.reporting_cohorts rc
                                              where rc.linked_reporting_cohort_id = $1::int",
                                           params=list(selected_cohort_id))
  
  # uploaded_clients <- DBPOOL %>% dbGetQuery("select ft.to_family_rsf_pfcbl_id as rsf_pfcbl_id
  #                                            from p_rsf.view_rsf_pfcbl_id_family_tree ft
  #                                            where ft.from_rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
  #                                              and ft.to_pfcbl_category = 'client'",
  #                                           params=list(unique(uploaded_cohorts$reporting_rsf_pfcbl_id)))
  
  uploaded_clients <- DBPOOL %>% dbGetQuery("
    select distinct
    fam.parent_rsf_pfcbl_id as rsf_pfcbl_id
    from p_rsf.reporting_cohorts rc
    inner join p_rsf.rsf_data rd on rd.reporting_cohort_id = rc.reporting_cohort_id
    inner join p_rsf.indicators ind on ind.indicator_id = rd.indicator_id
    inner join p_rsf.rsf_pfcbl_id_family fam on fam.child_rsf_pfcbl_id = rd.rsf_pfcbl_id
    where rc.reporting_cohort_id = any(select unnest(string_to_array($1::text,','))::int)
     and ind.indicator_sys_category = 'entity_reporting'
     and fam.parent_pfcbl_rank <= 3",
    params=list(paste0(unique(uploaded_cohorts$reporting_cohort_id),collapse=",")))
  
  uploaded_indicators <- DBPOOL %>% dbGetQuery("
    select 
     ind.indicator_name,
     rpc.pfcbl_rank 
    from p_rsf.reporting_cohorts rc
    inner join p_rsf.rsf_data rd on rd.reporting_cohort_id = rc.reporting_cohort_id
    inner join p_rsf.indicators ind on ind.indicator_id = rd.indicator_id
    inner join p_rsf.rsf_pfcbl_categories rpc on rpc.pfcbl_category = ind.data_category
    where rc.reporting_cohort_id = any(select unnest(string_to_array($1::text,','))::int)
     and ind.is_system = false
    group by
    ind.indicator_name ,
    rpc.pfcbl_rank
    order by 
    rpc.pfcbl_rank desc,
    min(rd.data_id) asc",
  params=list(paste0(unique(uploaded_cohorts$reporting_cohort_id),collapse=",")))

  uploaded_rank <- max(uploaded_indicators$pfcbl_rank)
  
  uploaded_entities <- DBPOOL %>% dbGetQuery("
  select distinct 
    ids.rsf_pfcbl_id
  from p_rsf.reporting_cohorts rc
  inner join p_rsf.rsf_data rd on rd.reporting_cohort_id = rc.reporting_cohort_id
  inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = rd.rsf_pfcbl_id
  where rc.reporting_cohort_id = any(select unnest(string_to_array($1::text,','))::int)
    and ids.pfcbl_category_rank = $2::int",
  params=list(paste0(unique(uploaded_cohorts$reporting_cohort_id),collapse=","),
              uploaded_rank))
  
  
  dashboard_parameters[["format_pivot"]] <- "NAME"
  dashboard_parameters[["format_pivot_category"]] <-  ifelse(uploaded_rank >= 4,
                                                             "client",
                                                             "parent")
  
  dashboard_parameters[["filter_flags"]] <- input$action_indicator_flags_review
  dashboard_parameters[["name_filter"]] <- uploaded_entities$rsf_pfcbl_id
  
  cohort <- SELECTED_COHORT_INFO()
  ad_hoc_title <- paste0(toupper(cohort$pfcbl_category),": ",
                         cohort$entity_name,
                         " as-of ",
                         cohort$reporting_asof_date,
                         " upload#",
                         cohort$reporting_cohort_id)

  SERVER_DASHBOARD_REPORT_SELECTED(list(report_id=0,
                                        report_title=ad_hoc_title,
                                        for_client_sys_names=unique(uploaded_clients$rsf_pfcbl_id),
                                        for_indicator_names=uploaded_indicators$indicator_name,
                                        for_asof_dates=unique(uploaded_cohorts$reporting_asof_date),
                                        report_parameters=list(dashboard_parameters)))
  
}

#Dashboard button in Review Datasets panel
observeEvent(input$cohort_action_dashboard, {
 
  # SERVER_DATASETS_COHORT_DASHBOARD(flags_filter = input$action_indicator_flags_review)
  
  cohort_info <- SELECTED_COHORT_INFO()
  
  if (empty(cohort_info)) return(NULL)
  
  
  filtered_flags <- SELECTED_COHORT_INDICATOR_FLAGS_FILTERED()
 
  for_indicator_names <- unique(filtered_flags$indicator_name)
  for_indicator_names <- c(for_indicator_names,":include:IDs")
  
  
  dashboard_parameters <- SERVER_DASHBOARD_RUN_OPTIONS_INIT
  #dashboard_parameters$flags_filter <- "any"
  #dashboard_parameters$flags_display <- "active"
  dashboard_parameters$format_unchanged <- "black"
  
  dashboard_parameters$name_filter <- unique(SELECTED_COHORT_FLAGS()[evaluation_id %in% unlist(filtered_flags$evaluation_ids),rsf_pfcbl_id])
  dashboard_parameters$format_pivot <- "DATA"
  
  if (length(dashboard_parameters$name_filter) <= length(for_indicator_names)) {
    dashboard_parameters$format_pivot <- "NAME"
  }
  
  for_client_sys_names <- SELECTED_PROGRAM_CLIENTS_LIST()[rsf_program_id %in% cohort_info$rsf_program_id &
                                                            rsf_facility_id %in% cohort_info$rsf_facility_id,
                                                          rsf_pfcbl_id]
  
  
  ad_hoc_title <- paste0(toupper(cohort_info$pfcbl_category),": ",
                         cohort_info$entity_name,
                         " as-of ",
                         cohort_info$reporting_asof_date,
                         " Datasets Review #",as.numeric(input$cohort_action_dashboard))
  
  SERVER_DASHBOARD_REPORT_SELECTED(list())
  SERVER_DASHBOARD_REPORT_SELECTED(list(report_id=0,
                                        report_title=ad_hoc_title,
                                        for_client_sys_names=for_client_sys_names,
                                        for_indicator_names=for_indicator_names,
                                        for_asof_dates=cohort_info$reporting_asof_date,
                                        report_parameters=list(dashboard_parameters)))
  
  # SERVER_DASHBOARD_DO_LOAD(for_client_sys_names=for_client_sys_names,
  #                          for_indicator_names=for_indicator_names,
  #                          for_asof_dates=cohort_info$reporting_asof_date,
  #                          dashboard_parameters=dashboard_parameters)
  
  
})


#Review panel dataset selection outputs: source title, name, upload info, flag types and totals, download handlers....
{
  #Title for datasets review panel in "review tab"
  output$datasets_review_title <- renderUI({
    
    cohort <- SELECTED_COHORT_INFO()
    if (!isTruthy(cohort)) return (HTML("Please select a dataset..."))
    
    cohort_flags <- SELECTED_COHORT_FLAGS()[check_status=="active"]
    title_flags <- ""
    if (!empty(cohort_flags)) {
      
      cohort_flags <- cohort_flags[check_rank==min(cohort_flags$check_rank),
                                   .(checks=.N,
                                     by=.(check_class))]
      flag_class <- paste0("icon-",cohort_flags$check_class)
      data_count <- cohort_flags$checks
      
      title_flags <- paste0("<div style='display:inline-block;font-size:12px;padding-left:5px;'><i class='fas fa-flag ",flag_class,"' title='Dataset Flags' data-count='",data_count,"'></i></div")
    
    }
    
    title <- div(style="display:inline-block;width:100%;padding-right:50px;",
                 div(style="display:inline-block",paste0(toupper(cohort$pfcbl_category),": ",
                                                         cohort$entity_name,
                                                         " as-of ",cohort$reporting_asof_date)),
                 div(style="display:inline-block;padding-left:10px;padding-right:10px;","|"),
                 div(style="display:inline-block;",
                     paste0("Upload: ",cohort$source_name)),
                 HTML(title_flags))
  
    return (title)
    
  })
  
  output$cohort_view_reported_flags_active_total <- renderText({
    #flags <- SELECTED_COHORT_INDICATOR_FLAGS_FILTERED()
    flags <- SELECTED_COHORT_FLAGS()
    if (!isTruthy(flags) || empty(flags)) return (0)
    return (nrow(flags[check_status=="active"]))
  })
  
  output$cohort_view_reported_flags_resolved_total <- renderText({
    flags <- SELECTED_COHORT_FLAGS()
    if (!isTruthy(flags) || empty(flags)) return (0)
    return (nrow(flags[check_status=="resolved"]))
  })
  
  output$cohort_view_html_flags_active <- renderUI({
    
    flags <- SELECTED_COHORT_FLAGS()
    if (is.null(flags) || empty(flags)) return (HTML("<div>0</div>"))

    flags <- flags[check_status=="active"]
    if (empty(flags)) return (HTML("<div>0</div>"))
    
    critical <- nrow(flags[check_class=="critical"])
    errors <- nrow(flags[check_class=="error"])
    warnings <- nrow(flags[check_class=="warning"])
    info <- nrow(flags[check_class=="info"])
    
    html_critical <- ifelse(critical > 0,paste0("<i class = 'fas fa-fire icon-critical' data-count='",critical,"'></i>"),"") #only show if there's any
    html_error <- ifelse(errors > 0,paste0("<i class='fas fa-times-circle icon-error' data-count='",errors,"'></i>"),"") #only show if there's any
    html_warning <- ifelse(warnings > 0,paste0("<i class='fas fa-exclamation-triangle icon-warning' data-count='",warnings,"'></i>"),"") #only show if there's any
    html_info <- ifelse(info > 0,paste0("<i class='fas fa-info-circle icon-info' data-count='",info,"'></i>"),"") #only show if there's any

    html_flags <- paste0("
                  <div style='display:inline-block;'>",
                         html_critical,
                         html_error,
                         html_warning,
                         html_info,
                         "</div>")
    
    return(HTML(html_flags))
  })
  
  output$cohort_view_html_flags_resolved <- renderUI({
    
    flags <- SELECTED_COHORT_FLAGS()
    if (is.null(flags) || empty(flags)) return (HTML("<div>0</div>"))
    
    flags <- flags[check_status=="resolved"]
    if (empty(flags)) return (HTML("<div>0</div>"))
    
    critical <- nrow(flags[check_class=="critical"])
    errors <- nrow(flags[check_class=="error"])
    warnings <- nrow(flags[check_class=="warning"])
    info <- nrow(flags[check_class=="info"])
    
    html_critical <- ifelse(critical > 0,paste0("<i class = 'fas fa-fire icon-critical' data-count='",critical,"'></i>"),"") #only show if there's any
    html_error <- ifelse(errors > 0,paste0("<i class='fas fa-times-circle icon-error' data-count='",errors,"'></i>"),"") #only show if there's any
    html_warning <- ifelse(warnings > 0,paste0("<i class='fas fa-exclamation-triangle icon-warning' data-count='",warnings,"'></i>"),"") #only show if there's any
    html_info <- ifelse(info > 0,paste0("<i class='fas fa-info-circle icon-info' data-count='",info,"'></i>"),"") #only show if there's any
    
    html_flags <- paste0("
                  <div style='display:inline-block;'>",
                         html_critical,
                         html_error,
                         html_warning,
                         html_info,
                         "</div>")
    
    return(HTML(html_flags))
  })
  
  #Download the file that was uploaded
  output$datasets_review_download_source_action <- downloadHandler(
    filename = function() {
      selected_id <- input$cohort_collection_selected_id
      cohort <- NULL
      if (identical(selected_id,"all") || is.na(suppressWarnings(as.numeric(selected_id)))) {
        cohort <- SELECTED_COHORT_INFO()
      } else {
        cohort <- COHORTS_LIST()[reporting_cohort_id==as.numeric(selected_id)]
      }
      cohort$source_name
    },
    content=function(file) {

      withProgress(message="Downloading file",value=0.5, {
        
        selected_id <- input$cohort_collection_selected_id
        cohort <- NULL
        if (identical(selected_id,"all") || is.na(suppressWarnings(as.numeric(selected_id)))) {
          cohort <- SELECTED_COHORT_INFO()
        } else {
          cohort <- COHORTS_LIST()[reporting_cohort_id==as.numeric(selected_id)]
        }
        

        outpath <- DBPOOL %>% db_cohort_download_file(reporting_cohort_id=cohort$reporting_cohort_id)
        
        if (!is.null(outpath)) {
          #file.rename(from=outpath,to=file)
          #print("Downloading file in output$datasets_review_download_source_action")
          file.copy(from=outpath,
                    to=file,
                    overwrite = TRUE)
          if (file.exists(outpath)) file.remove(outpath)
        } else {
          linked_file <- DBPOOL %>% dbGetQuery("
           select 
            lrc.source_name,
            lrc.reporting_cohort_id,
            lrc.reporting_asof_date
          from p_rsf.reporting_cohorts rc
          inner join p_rsf.reporting_cohorts lrc on lrc.reporting_cohort_id = rc.linked_reporting_cohort_id
          where rc.reporting_cohort_id = $1::int",
          params=list(cohort$reporting_cohort_id))
          
          if (!empty(linked_file)) {
            outpath <- DBPOOL %>% db_cohort_download_file(reporting_cohort_id=linked_file$reporting_cohort_id)
            if (!is.null(outpath)) {
              
              file.copy(from=outpath,
                        to=file,
                        overwrite = TRUE)
              if (file.exists(outpath)) file.remove(outpath)
              
            } else {
              outpath <- NULL
            }
          }
        }        
        incProgress(amount=1.0,message="Completed")
      })
    }
  )
  
  
}

####RENDER DATA TABLES####

###Summary of flags reported on dataset for bulk flags review and management 
output$datasets_review_flags_summary <- DT::renderDataTable({
  
  cohort_indicator_flags <- SELECTED_COHORT_INDICATOR_FLAGS_FILTERED()
  if (!isTruthy(cohort_indicator_flags)) return (DT::datatable(data.frame(Error="This selection has no flags to display"),
                                               rownames=FALSE,
                                               fillContainer = TRUE,
                                               options=list(
                                                 dom="t",
                                                 autoWidth=TRUE,
                                                 paging=FALSE
                                               )))
  
  display_cols <- c()
  defs <- NULL
  
  if (any(nchar(cohort_indicator_flags$formula_html) > 0)) {
    cohort_indicator_flags <- cohort_indicator_flags[,
                                                     .(indicator_html,
                                                       formula_html,
                                                       check_html,
                                                       active_count,
                                                       resolved_count,
                                                       action_review)]
    display_cols <- c("Indicator","Formula","Flag","Active","Resolved","Review")
    defs <- list(list(className = 'dt-left', targets = c(0,1,2)),  #Zero-based targets
                 list(className = 'dt-center', targets = c(3,4,5)))
  } else {
    cohort_indicator_flags <- cohort_indicator_flags[,
                                                     .(indicator_html,
                                                       check_html,
                                                       active_count,
                                                       resolved_count,
                                                       action_review)]
    display_cols <- c("Indicator","Flag","Active","Resolved","Review")
    defs <- list(list(className = 'dt-left', targets = c(0,1)),  #Zero-based targets
                 list(className = 'dt-center', targets = c(2,3,4)))
  }
  # cohort_indicator_flags[nchar(formula_html) > 0,
  #                        indicator_html:=paste0("<div style='display:flex;flex-direction:row;'>",
  #                                               indicator_html,
  #                                               formula_html,
  #                                               "</div>")]

  DT::datatable(cohort_indicator_flags,
                rownames = FALSE,
                fillContainer=TRUE,
                colnames=display_cols,
                escape = FALSE,
                
                #height = "100%",
                options=list(
                  dom="t",
                  ordering=FALSE,  
                  #scrollY="40vh",
                  #scrollCollapse=TRUE,
                  paging=FALSE,
                  #                  autoWidth=TRUE #For some reason this really screws up the table, misaligning headers and content
                  columnDefs = defs
                )) 
    
})

###MAIN LISTINGS on "Uploads Tab"
output$list_reporting_cohorts <- DT::renderDataTable({
  cohorts <- COHORTS_LIST()
  if (!isTruthy(cohorts) || all(is.na(cohorts))) return (DT::datatable(data.frame(Error="There is no data to display"),
                                                                       rownames=FALSE,
                                                                       fillContainer = TRUE,
                                                                       options=list(
                                                                         dom="t",
                                                                         autoWidth=TRUE,
                                                                         paging=FALSE
                                                                       )))
  
  #cohorts <- as.data.table(cohorts)
  
  #cohorts[,reporting_time:=format.Date(reporting_time,"%Y-%m-%d %H:%M")]
  
  cohorts <- cohorts[,.(actions,
                        entity_name,
                        reporting_asof_date_label,
                        source_name,
                        users_name,
                        upload_text,
                        flags,
                        delete)]
  
  trashButton <- paste0("<div>
                        <i class='fa fa-trash icon-trash pointer' 
                           title='Delete Selected...' 
                           onclick='event.stopPropagation();Shiny.setInputValue(\"action_cohort_delete\",-1,{priority:\"event\"})'>
                        </i></div>")
  #flags <- paste0("<i class='fas fa-flag icon-red'></i>")
  #df <- as.data.frame(cohorts)
  ##print(paste0("Displaying ",nrow(df)," cohorts"))
  DT::datatable(cohorts,
                rownames = FALSE,
                fillContainer=TRUE,
                #          0        1      2            3             4    5    6       7                           
                colnames=c("Review","Name","As-Of Date","Source Name","By","On","Flags",trashButton),
                #filter="top",
                escape = FALSE, #Shouldn't be any HTML escapable text
                options=list(
                  dom="tir",
                  scrollY="70vh",
                  #scrollCollapse=TRUE,
                  paging=TRUE,
                  pageLength=250,
                  orderable=F,
                  ordering=F,
                  #autoWidth=TRUE,
                  columnDefs = list(list(className = 'dt-left', targets = c(1,2,3,4)),  #Zero-based targets
                                    list(className = 'dt-center', targets = c(0,5,6,7)))
                  #,
                  #initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'white-space' : 'nowrap'});}")
                )) %>% 
    formatStyle(columns=c(0,1,3,4,6,7),whiteSpace="nowrap") %>%
    formatStyle(columns=c(1),minWidth="75px",width="75px") %>%
    formatStyle(columns=c(2),minWidth="150px",width="200px") %>%
    formatStyle(columns=c(3),minWidth="150px",width="200px") %>%
    formatStyle(columns=c(4),minWidth="100px") %>%
    formatStyle(columns=c(5),minWidth="50px") %>%
    formatStyle(columns=c(6),minWidth="75px") %>%
    formatStyle(columns=c(7),minWidth="25px")
})
