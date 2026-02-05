COHORTS_SELECTED <- reactiveVal(c())
LOAD_IMPORT <- reactiveVal(0)
REFRESH_SELECTED_COHORT_DATA <- reactiveVal(1)

#All "reported" cohorts uploaded under the given program
COHORTS_LIST <- eventReactive(c(SELECTED_PROGRAM_ID(),
                                REFRESH_SELECTED_COHORT_DATA(),
                                LOAD_IMPORT(),
                                input$dataset_review_filter_facility,
                                input$dataset_review_filter),
{

  rsf_program_id <- SELECTED_PROGRAM_ID()
  filter_facility_id <- as.numeric(input$dataset_review_filter_facility)
  
  rx1<-REFRESH_SELECTED_COHORT_DATA()
  load_import_id <- LOAD_IMPORT()
  phrase <- input$dataset_review_filter
  phrase <- trimws(phrase,whitespace="[ \\t\\r\\n\\v\\h\\s]")
  
  if (!isTruthy(rsf_program_id)) return (NULL)
  if (!isTruthy(filter_facility_id)) filter_facility_id <- rsf_program_id
  
  load_by_limit <- as.numeric(NA)
  if (isTruthy(phrase) &&
      any(grepl("recent\\s+\\d+$",phrase))==TRUE) {
    load_by_limit <- as.numeric(gsub("[[:alpha:][:space:]]+(\\d+)","\\1",phrase))
    phrase <- NULL
  } else {
    load_by_limit <- as.numeric(NA)
  }
  
  cohorts <- DBPOOL %>% dbGetQuery("
    select 
      ids.rsf_program_id,
      ids.rsf_facility_id,
      ids.rsf_client_id,
      ids.pfcbl_category,
      ri.import_id,                                      
      ri.reporting_asof_date,
      ri.pfcbl_name as entity_name,
      ri.file_name,
      ri.import_user_id,
      ri.import_time,                                      
      ri.import_rsf_pfcbl_id,
      ri.import_comments,
      accounts.account_id as reporting_user_id,
      accounts.users_name as reporting_user_name,
      rt.template_name,
      rt.file_extension,
      rt.template_id
    from p_rsf.rsf_pfcbl_ids ids 
    inner join p_rsf.reporting_imports ri on ri.import_rsf_pfcbl_id = ids.rsf_pfcbl_id
    inner join p_rsf.reporting_templates rt on rt.template_id = ri.template_id
    left join p_rsf.view_account_info accounts on accounts.account_id = ri.import_user_id
        where $1::int in (ids.rsf_program_id,
                          ids.rsf_facility_id,
                          ids.rsf_client_id)
        order by ri.import_id desc
        limit (NULLIF($2::text,'NA'))::int",
        params=list(filter_facility_id,
                    as.character(load_by_limit)))
    
  #--and rc.rsf_facility_id is not distinct from $2::int
  if (empty(cohorts)) return (NULL)
  setDT(cohorts)
  
  cohorts[,source_name:=gsub("\\.gz$","",file_name)]
  
  cohorts[,
          users_name:=sapply(reporting_user_name,format_name_abbreviation)]
  
  cohorts[,upload_text:=paste0(toupper(format.Date(import_time,"%b%d %Hh%M")))]
  
  cohorts[,
          `:=`(data_checks_active=0,
               data_checks_critical_active=0,
               data_checks_error_active=0,
               data_checks_warning_active=0,
               data_checks_info_active=0)]
  
  counts <- DBPOOL %>% dbGetQuery("
    select * from p_rsf.view_reporting_imports_data_counts idc
    where idc.import_id = any(select unnest(string_to_array($1::text,','))::int)",
  params=list(paste0(unique(cohorts$import_id),collapse=",")))
  
  setDT(counts)
  
  flags <- DBPOOL %>% dbGetQuery("
    select * from p_rsf.view_reporting_imports_data_checks_current_active cca
    where cca.import_id = any(select unnest(string_to_array($1::text,','))::int)",
    params=list(paste0(unique(cohorts$import_id),collapse=",")))
  
  setDT(flags)
  
  cohort_dates <- unique(rbindlist(list(unique(flags[,.(import_id,
                                                        cohort_asof_date=check_asof_date)]),
                                        unique(counts[,.(import_id,
                                                         cohort_asof_date=reporting_asof_date)]))))  
  cohorts[cohort_dates,
          cohort_asof_date:=i.cohort_asof_date,
          on=.(import_id)]
  
  cohorts[,import_cohort_date_group:=1:.N]
  
  cohorts[,
          is_deletable := reporting_asof_date == cohort_asof_date]
  
  cohorts[flags,
          `:=`(data_checks_active=i.data_checks_active,
               data_checks_critical_active=i.data_checks_critical_active,
               data_checks_error_active=i.data_checks_error_active,
               data_checks_warning_active=i.data_checks_warning_active,
               data_checks_info_active=i.data_checks_info_active),
          on=.(import_id,
               cohort_asof_date=check_asof_date)]
  cohorts[counts,
          `:=`(data_count_reported=i.data_count_reported,
               data_count_calculated=i.data_count_calculated,
               data_current_count_reported=i.data_current_count_reported,
               data_current_count_calculated=i.data_current_count_calculated),
          on=.(import_id,
               cohort_asof_date=reporting_asof_date)]
  
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
  
  cohorts[,reporting_asof_date_label:=format_asof_date_label(cohort_asof_date)]
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
  #delete is by individual import_id (delete the whole im port)
  cohorts[,delete:=paste0("<input type='checkbox' ",
                          ifelse(is_deletable==FALSE,"disabled='disabled'",""),
                          " name='cohort_actions' value=",import_id,
                          " onmousedown='event.stopPropagation();' onclick='Shiny.setInputValue(\"action_cohort_selected\",",
                          import_id,",{priority:\"event\"})' />")] 
  #View is by pseudo cohort_id that will disaggregate by check_asof_date/reporting_asof_date for the respective import if there are timeseries information in the single import file
  cohorts[,
          actions:=paste0("<div style='display:inline-block;'>
<div onmousedown='event.stopPropagation();'  style='display:inline-block;'>
<i class='fa fa-eye icon-view pointer' title='View' onclick='Shiny.setInputValue(\"action_cohort_view\",",import_cohort_date_group,",{priority:\"event\"})'></i>
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
                     .(import_id,
                       reporting_user_name,
                       file_name,
                       entity_name,
                       import_comments,
                       cohort_asof_date,
                       reporting_asof_date_label)],
             grep,pattern=x,ignore.case=TRUE)
    })
    
    results <- list(import_id=NA,
                    reporting_user_name=NA,
                    file_name=NA,
                    entity_name=NA,
                    import_comments=NA,
                    cohort_asof_date=NA,
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
      if (!empty(filter_cohorts)) setorder(filter_cohorts,-cohort_asof_date,import_id)
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
  
  import_cohort_date_group <- as.numeric(input$action_cohort_view)
  cohorts <- COHORTS_LIST()
  if (!isTruthy(import_cohort_date_group)) return (NULL)
  if (!isTruthy(cohorts)) return (NULL)
  if (!import_cohort_date_group %in% cohorts$import_cohort_date_group) return (NULL)
  import_cohort_date_group
}, 
ignoreInit=TRUE,ignoreNULL=FALSE)


SELECTED_IMPORT_COHORT_GROUP <- eventReactive(SELECTED_COHORT_ID(), {
  
  icdg_id <- SELECTED_COHORT_ID()
  if (!isTruthy(icdg_id)) return (NULL)
  cohorts <- COHORTS_LIST()
  
  if (!isTruthy(cohorts)) return (NULL)
  
  cohort <- cohorts[import_cohort_date_group==icdg_id]
  
  if (empty(cohort)) return (NULL)
  
  return (cohort)
  
}, 
ignoreNULL=FALSE)


#For the DATASE REVIEW panel: based on 
#Selected cohort will pull all flags for all cohorts under this selected cohort rsf_client_id
#Unless the panel is specifically requesting an individiual cohort
SELECTED_COHORT_FLAGS <- eventReactive(c(SELECTED_IMPORT_COHORT_GROUP(),
                                         input$cohort_collection_selected_id, #NA or a specific reporting_cohort_id: collections are all timeseries uploads for the client
                                         REFRESH_SELECTED_COHORT_DATA()), { 
                                          
                                          
  cohort <- SELECTED_IMPORT_COHORT_GROUP()

  
  if (!isTruthy(cohort)) return (NULL)
  
  reporting_current_date <- cohort$cohort_asof_date
  #collection_id <- suppressWarnings(as.numeric(input$cohort_collection_selected_id)) #"all" will return NA
  
  flags_data <- NULL
  
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
        ic.is_calculator_check as check_is_calculator,
        rdc.check_asof_date,
        rdc.indicator_check_id, 
        rdc.check_formula_id,
        ic.check_name,
        ic.check_type,
        coalesce(scc.config_check_class,ic.check_class) as check_class,
        rdc.check_status,
        
        sis.is_subscribed,
        sis.formula_id as indicator_formula_id,
        sis.is_calculated as formula_is_calculated,
        indf.formula_title,
        indf.is_primary_default,
        indcf.check_formula_title
      from p_rsf.reporting_cohorts rc
      inner join p_rsf.rsf_data rd on rd.reporting_cohort_id = rc.reporting_cohort_id
      inner join p_rsf.rsf_data_checks rdc on rdc.data_id = rd.data_id      
      inner join p_rsf.indicators ind on ind.indicator_id = rdc.indicator_id
      inner join p_rsf.indicator_checks ic on ic.indicator_check_id = rdc.indicator_check_id
     
      left join p_rsf.view_rsf_setup_indicator_subscriptions sis on sis.rsf_pfcbl_id = rdc.rsf_pfcbl_id
                                                                and sis.indicator_id = rdc.indicator_id
      
      left join p_rsf.indicator_formulas indf on indf.formula_id = sis.formula_id
      
      left join p_rsf.indicator_check_formulas indcf on indcf.check_formula_id = rdc.check_formula_id
      
      left join p_rsf.view_rsf_setup_check_config scc on scc.rsf_pfcbl_id = rdc.rsf_pfcbl_id
                                                     and scc.for_indicator_id = rdc.indicator_id
                                                     and scc.indicator_check_id = rdc.indicator_check_id
      where rc.import_id = $1::int 
        and rdc.check_asof_date = $2::date
        and rdc.check_data_id_is_current = true
                                      ",
                                      params=list(cohort$import_id,
                                                  cohort$cohort_asof_date))

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
                  check_formula_id)]

  return(flags_data)  
}, ignoreNULL=FALSE)



#Indicators-level view for cohort review panel: collapsed evaluation_ids by indicator and check
SELECTED_COHORT_INDICATOR_FLAGS_FILTERS <- eventReactive(c(SELECTED_COHORT_FLAGS(),
                                                            input$cohort_view_flagged_data,  #ALL/RESOLVED/ACTIVE
                                                            input$cohort_view_flag_classes,  #error/warning/info  
                                                            input$cohort_view_flag_types),   
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
                                              
                                              indicator_is_system,
                                              indicator_is_calculated,
                                              check_is_system,
                                              check_is_calculator,
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
  
  if (any(view_flag_types=="nosystem")) {
    cohort_indicator_flags <- cohort_indicator_flags[check_is_system==FALSE]
  }
  
  if (any(view_flag_types=="nocalculator")) {
    cohort_indicator_flags <- cohort_indicator_flags[check_is_calculator==FALSE]
  }
  
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
  
  cohort_indicator_flags[formula_is_calculated==TRUE,
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
  
  cohort_indicator_flags[,
                         check_formula_html:=""]
  
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
  cohort_indicator_flags[,
                         check_display_html:=""]
  
  ############
  cohort_indicator_flags[is.na(check_formula_id)==FALSE,
                         check_display_html:=mapply(format_html_indicator,
                                                    indicator_name=gsub("'","%39;",check_formula_title),
                                                    data_category=data_category,
                                                    data_type=data_type,
                                                    is_system=FALSE,
                                                    is_calculated=FALSE,
                                                    is_subscribed=TRUE,
                                                    id=check_formula_id)]
  
  #Check must be a system check
  cohort_indicator_flags[is.na(check_formula_id)==TRUE,
                         check_display_html:=mapply(format_html_indicator,
                                                    indicator_name=gsub("'","%39;",indicator_name),
                                                    data_category=data_category,
                                                    data_type=data_type,
                                                    is_system=FALSE,
                                                    is_calculated=FALSE,
                                                    is_subscribed=TRUE,
                                                    id=check_formula_id)]

  cohort_indicator_flags[is.na(check_formula_id)==TRUE & formula_is_calculated==TRUE,
                         check_display_html:=paste0(check_display_html,
                                                    mapply(format_html_indicator,
                                                           indicator_name=gsub("'","%39;",formula_title),
                                                           data_category=fcase(is_primary_default==TRUE,"formula",
                                                                               is_primary_default==FALSE,"customformula",
                                                                               default="none"),
                                                           data_type=data_type,
                                                           is_system=FALSE,
                                                           is_calculated=TRUE,
                                                           is_subscribed=TRUE,
                                                           id=indicator_formula_id))]                         
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
  
  #will pull-in indicator subscriptions across the full family tree
  classification_indicators <- DBPOOL %>% dbGetQuery("
    select ind.indicator_id,ind.classification,ind.indicator_name
    from p_rsf.view_rsf_setup_indicator_subscriptions sis
    inner join p_rsf.indicators ind on ind.indicator_id = sis.indicator_id
    where sis.rsf_pfcbl_id=$1::int
      and sis.is_subscribed = true
      and ind.classification is not null
    
    union 	
    
    select parameter_id,ind.classification,ind.indicator_name
    from p_rsf.view_rsf_setup_indicator_subscriptions sis
    inner join p_rsf.indicators ind on ind.indicator_id = sis.indicator_id
    inner join p_rsf.indicator_formulas indf on indf.formula_id = sis.formula_id
    inner join lateral unnest(indf.formula_indicator_id_requirements) as parameter_id on true
    where sis.rsf_pfcbl_id = $1::int
      and sis.is_subscribed = true
      and ind.classification is not null",
    params=list(SELECTED_IMPORT_COHORT_GROUP()$import_rsf_pfcbl_id))
  
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
observeEvent(SELECTED_PROGRAM_FACILITIES_LIST(), {
  facilities <- SELECTED_PROGRAM_FACILITIES_LIST()
  
  if (empty(facilities)) {
    updateSelectizeInput(session=session,
                         inputId="dataset_review_filter_facility",
                         choices = c(`Project Filter...`=""),
                         selected = "")
    
  } else {
    setorder(facilities,
             facility_name)
    facilities_choices <- c(`Project Filter...`="",
                         setNames(facilities$rsf_pfcbl_id,
                                  facilities$facility_name))
    updateSelectizeInput(session=session,
                         inputId="dataset_review_filter_facility",
                         choices = facilities_choices,
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

observeEvent(SELECTED_IMPORT_COHORT_GROUP(), {
  
  req(SELECTED_PROGRAM_ID())
  
  cohort <- SELECTED_IMPORT_COHORT_GROUP()

  if (empty(cohort)) {
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
    
    cohorts <- COHORTS_LIST()[import_id==cohort$import_id]
 
    choices.selected <- cohort$import_cohort_date_group



    choices <- setNames(cohorts$import_cohort_date_group,
                        paste0(cohorts$reporting_asof_date_label," ",cohorts$entity_name,": ",cohort$source_name))

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
      

      if (any(flags_data$check_is_system==TRUE,na.rm = T)) {
        type.choices <- c(type.choices,setNames("nosystem","Hide SYS Flags <i class='fa-solid fa-cog' style='color:gray'></i>"))
      }

      if (any(flags_data$check_is_calculator==TRUE,na.rm = T)) {
        type.choices <- c(type.choices,setNames("nocalculator","Hide Calcualtor Flags <i class='fa-solid fa-calculator' style='color:black'></i>"))
      }
      
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
  updateTextInput(session=session,inputId="dataset_review_filter_facility",value="")
  
}, ignoreInit = TRUE)

observeEvent(input$action_cohort_delete, {
  delete_import_ids <- COHORTS_SELECTED()
  program_id <- SELECTED_PROGRAM_ID()
  
  tryCatch({
    

  withProgress(value=.15,message="Deleting datasets...",
               {
                 delete_import_ids <- unique(na.omit(delete_import_ids))
                 if (length(delete_import_ids)==0) {
                   return(showNotification(type="error",
                                           h3("Bad selection: reporting cohorts do not exist or may have already been deleted")))
                 }
                 
                 affected_ids <- DBPOOL %>% dbGetQuery("select distinct 
                                                          ri.import_rsf_pfcbl_id,
                                                          ids.pfcbl_category_rank as pfcbl_rank,
                                                          ids.rsf_program_id,
                                                          ids.rsf_facility_id,
                                                          sn.pfcbl_name
                                                        from p_rsf.reporting_imports ri
                                                        inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = ri.import_rsf_pfcbl_id
                                                        inner join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.rsf_pfcbl_id = ids.rsf_pfcbl_id
                                                        where ri.import_id = any(select unnest(string_to_array($1::text,','))::int)",
                                                        params=list(paste0(delete_import_ids,collapse=",")))
                 
                 setDT(affected_ids)

                 if (empty(affected_ids)) {
                   return(showNotification(type="error",
                                           h3("Bad selection: reporting cohorts do not exist or may have already been deleted")))
                   
                 } else if (length(unique(affected_ids$rsf_program_id)) != 1) {
                   return(showNotification(type="error",
                                           h3("Bad selection: Deletes can only affect one Program at a time")))
                 }
                 
                 incProgress(amount=0.20,message="Deleting datasets . . . ")
                 
                 DBPOOL %>% dbExecute("insert into p_rsf.reporting_imports_deleted_archive(import_id,
                                                                                           deleting_user_id)
                                       select
                                         unnest(string_to_array($1::text,','))::int,
                                         $2::text",
                                      params=list(paste0(delete_import_ids,collapse=","),
                                                  USER_ID()))
                 
                 #If program doesnt exist after delete then it means we've deleted the entire program
                 stillexists <- DBPOOL %>% dbGetQuery("select distinct ids.rsf_pfcbl_id as import_rsf_pfcbl_id
                                                       from p_rsf.rsf_pfcbl_ids ids
                                                       where ids.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)",
                                                       params=list(paste0(unique(affected_ids$import_rsf_pfcbl_id),collapse=",")))

                 affected_ids[,exists:=FALSE]
                 affected_ids[stillexists,
                              exists:=TRUE,
                              on=.(import_rsf_pfcbl_id)]
                 
                 if (!any(affected_ids$exists==TRUE,na.rm=T)) {
                   LOAD_PROGRAM_ID((-program_id))
                   
                 } else {
                   
                   incProgress(amount=0.20,
                               message="Recalculating affected data . . . ")
                   
                   
                   }
                   for (id in unique(affected_ids[exists==TRUE,import_rsf_pfcbl_id])) {
                     who <- unique(affected_ids[import_rsf_pfcbl_id==id,pfcbl_name])
                     
                     progress_status_message <- function(class,...) {
                       dots <- list(...)
                       dots <- paste0(unlist(dots),collapse=" ")
                       incProgress(amount=0,
                                   message=paste0("Recalculating affected ",who," data: ",dots))
                       
                     DBPOOL %>% rsf_program_calculate(rsf_indicators = RSF_INDICATORS(),
                                                      rsf_pfcbl_id.family = id,
                                                      for_import_id=NA,
                                                      status_message=progress_status_message)
                     
                     incProgress(amount=(0.30/length(affected_ids[exists==TRUE,import_rsf_pfcbl_id])),
                                 message=paste0("Rechecking affected data . . . "))
    
                     progress_status_message <- function(class,...) {
                       dots <- list(...)
                       dots <- paste0(unlist(dots),collapse=" ")
                       incProgress(amount=0,
                                   message=paste0("Rechecking affected ",who," data: ",dots))
                     }

                     DBPOOL %>% rsf_program_check(rsf_indicators=RSF_INDICATORS(),
                                                  rsf_pfcbl_id.family=id,
                                                  check_future=TRUE,
                                                  check_consolidation_threshold=NA,
                                                  reference_asof_date=NULL,
                                                  status_message= progress_status_message)
                   }
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
  REFRESH_SELECTED_COHORT_DATA(REFRESH_SELECTED_COHORT_DATA()+1)
  #LOAD_IMPORT(-(LOAD_IMPORT()-1)) #just to force a refresh.
  
},ignoreInit=TRUE,ignoreNULL=TRUE,priority = 100)

#For deleting selected cohorts through checkboxes in datatable
observeEvent(input$action_cohort_selected, {
  action <- as.numeric(input$action_cohort_selected)
  selected <- COHORTS_SELECTED()
  if (action %in% selected) selected <- selected[-which(action==selected)]
  else selected <- c(selected,action)
  COHORTS_SELECTED(selected)
  
},ignoreNULL = FALSE,ignoreInit = TRUE)


#Dashboard button in Review Datasets panel
observeEvent(input$cohort_action_dashboard, {
 
  # SERVER_DATASETS_COHORT_DASHBOARD(flags_filter = input$action_indicator_flags_review)
  
  cohort_group <- SELECTED_IMPORT_COHORT_GROUP()
  
  if (empty(cohort_groupcohor)) return(NULL)
  
  
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
  
  for_facility_sys_names <- SELECTED_PROGRAM_FACILITIES_LIST()[rsf_facility_id %in% cohort_group$rsf_facility_id,
                                                             rsf_pfcbl_id]
  
  
  ad_hoc_title <- paste0(toupper(cohort_info$pfcbl_category),": ",
                         cohort_info$entity_name,
                         " as-of ",
                         cohort_info$reporting_asof_date,
                         " Datasets Review #",as.numeric(input$cohort_action_dashboard))
  
  SERVER_DASHBOARD_REPORT_SELECTED(list())
  SERVER_DASHBOARD_REPORT_SELECTED(list(report_id=0,
                                        report_title=ad_hoc_title,
                                        for_facility_sys_names=for_facility_sys_names,
                                        for_indicator_names=for_indicator_names,
                                        for_asof_dates=cohort_group$cohort_asof_date,
                                        report_parameters=list(dashboard_parameters)))
  
})


#Review panel dataset selection outputs: source title, name, upload info, flag types and totals, download handlers....
{
  #Title for datasets review panel in "review tab"
  output$datasets_review_title <- renderUI({
    
    cohort <- SELECTED_IMPORT_COHORT_GROUP()
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
                 div(style="display:inline-block",paste0(cohort$import_id," ",toupper(cohort$pfcbl_category),": ",
                                                         cohort$entity_name,
                                                         " as-of ",cohort$cohort_asof_date)),
                 div(style="display:inline-block;padding-left:10px;padding-right:10px;","|"),
                 div(style="display:inline-block;",
                     paste0('"',cohort$source_name,'"')),
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
     
     cohort <- SELECTED_IMPORT_COHORT_GROUP()
     cohort$source_name
    },
    content=function(file) {

      tryCatch({
        cohort <- SELECTED_IMPORT_COHORT_GROUP()
        withProgress(message="Downloading file",value=0.5, {
          
          outpath <- DBPOOL %>% db_import_download_file(import_id=SELECTED_IMPORT_COHORT_GROUP()$import_id)
          
          if (!is.null(outpath)) {
            #file.rename(from=outpath,to=file)
            #print("Downloading file in output$datasets_review_download_source_action")
            
            
            file.copy(from=outpath,
                      to=file,
                      overwrite = TRUE)
            
            if (file.exists(outpath)) file.remove(outpath)

            if (isTruthy(as.logical(input$datasets_review_download_source_insert_flags))) {
              
              if (cohort$template_name=="IFC-QR-TEMPLATE2025") {
                
                flag_details <- SERVER_DATASETS_REVIEW_FLAGS_QUERY_DETAILS(unlist(SELECTED_COHORT_INDICATOR_FLAGS_FILTERED()$evaluation_ids))
  
                flag_details <- SELECTED_COHORT_INDICATOR_FLAGS_FILTERED()[,.(evaluation_id=unlist(evaluation_ids,recursive=FALSE)),
                                                           by=.(indicator_id,indicator_check_id,check_name,check_class,check_type,check_formula_id,check_formula_title)
                                                           ][flag_details,
                                                             on=.(evaluation_id,
                                                                  indicator_id)]
                 if (!empty(flag_details)) {
                   lookup <- db_export_get_template(pool=DBPOOL,
                                                    template_name="IFC-QR-TEMPLATE2025")
                   
                   wbflags <- parse_template_IFC_QR2025(pool=DBPOOL,
                                                        template_file=file,
                                                        template_lookup=lookup,
                                                        rsf_indicators=db_indicators_get_labels(DBPOOL),
                                                        return.insert_flags=flag_details,
                                                        return.next_date=NULL,
                                                        status_message = function(...) {},
                                                        CALCULATIONS_ENVIRONMENT=CALCULATIONS_ENVIRONMENT)
                   
                   wbflags$save(file=file,overwrite=TRUE)
                 }
                
                
              } else {
                showNotification(type="error",
                                 ui=h3("Insert flags is unavailable for this template, version '",cohort$template_name,"'"))
              }
            }
          }        
          incProgress(amount=1.0,message="Completed")
        })
      },
      error=function(e) { showNotification(type="error",
                                           ui=h3(conditionMessage(e))); 
        NULL
      },
      warning=function(w) { showNotification(type="error",
                                             ui=h3(conditionMessage(w)));
        NULL
      })
    }
  )
  
  #Download the file that was uploaded: with the flags inserted into the file
  #WORK IN PROGRESS
  # output$datasets_review_download_sourceflags_action <- downloadHandler(
  #   filename = function() {
  #     #selected_id <- input$cohort_collection_selected_id
  #     
  #     cohort <- SELECTED_IMPORT_COHORT_GROUP()
  #     cohort$source_name
  #   },
  #   content=function(file) {
  #     
  #     withProgress(message="Downloading file",value=0.5, {
  #       
  #       cohort <- SELECTED_IMPORT_COHORT_GROUP()
  #       
  #       outpath <- DBPOOL %>% db_cohort_download_file(reporting_cohort_id=cohort$reporting_cohort_id)
  #       
  #       if (!is.null(outpath)) {
  #         #file.rename(from=outpath,to=file)
  #         #print("Downloading file in output$datasets_review_download_source_action")
  #         file.copy(from=outpath,
  #                   to=file,
  #                   overwrite = TRUE)
  #         if (file.exists(outpath)) file.remove(outpath)
  #       } else {
  #         linked_file <- DBPOOL %>% dbGetQuery("
  #          select 
  #           lrc.source_name,
  #           lrc.reporting_cohort_id,
  #           lrc.reporting_asof_date
  #         from p_rsf.reporting_cohorts rc
  #         inner join p_rsf.reporting_cohorts lrc on lrc.reporting_cohort_id = rc.linked_reporting_cohort_id
  #         where rc.reporting_cohort_id = $1::int",
  #                                              params=list(cohort$reporting_cohort_id))
  #         
  #         if (!empty(linked_file)) {
  #           outpath <- DBPOOL %>% db_cohort_download_file(reporting_cohort_id=linked_file$reporting_cohort_id)
  #           if (!is.null(outpath)) {
  #             
  #             file.copy(from=outpath,
  #                       to=file,
  #                       overwrite = TRUE)
  #             if (file.exists(outpath)) file.remove(outpath)
  #             
  #           } else {
  #             outpath <- NULL
  #           }
  #         }
  #       }        
  #       incProgress(amount=1.0,message="Completed")
  #     })
  #   }
  # )
  
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

  display_cols <- c("Flag","Check","Active","Resolved","Review")
  cohort_indicator_flags <- cohort_indicator_flags[,
                                                   .(check_display_html,
                                                     check_html,
                                                     active_count,
                                                     resolved_count,
                                                     action_review)]
  defs <- list(list(className = 'dt-left', targets = c(0,1)),  #Zero-based targets
               list(className = 'dt-center', targets = c(2,3,4)))
  

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
                        file_name=gsub("\\.gz$","",file_name),
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
