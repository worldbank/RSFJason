IMPORT_IDS_SELECTED <- reactiveVal(c())

IMPORT_LIST__REFRESH <- reactiveVal(1)
IMPORT_FLAGS_SELECTED_VIEW_FLAGGED_DATA_VALUE <- reactiveVal("ACTIVE")

#All "reported" cohorts uploaded under the given program
IMPORTS_LIST <- eventReactive(c(IMPORT_LIST__REFRESH(),
                                input$server_programs__selected_facility,
                                input$dataset_review_filter),
{

  selected_entity_id <- as.numeric(input$server_programs__selected_facility)

  
  rx1<-IMPORT_LIST__REFRESH()
  #load_import_id <- LOAD_IMPORT()
  phrase <- input$dataset_review_filter
  phrase <- trimws(phrase,whitespace="[ \\t\\r\\n\\v\\h\\s]")
  
  if (!isTruthy(selected_entity_id)) return (NULL)
  if (empty(SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST())) return (NULL)
  
  selected_entity <- SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST()[(rsf_pfcbl_id==selected_entity_id) | (selected_entity_id==-1 & pfcbl_category %in% c("global","program"))]
  selected_category <- selected_entity$pfcbl_category

  if (empty(selected_entity)) return (NULL)
  if (!length(selected_category) || selected_entity_id==-1) selected_category <- "ALL"

  load_by_limit <- as.numeric(NA)
  if (isTruthy(phrase) &&
      any(grepl("recent\\s+\\d+$",phrase))==TRUE) {
    load_by_limit <- as.numeric(gsub("[[:alpha:][:space:]]+(\\d+)","\\1",phrase))
    phrase <- NULL
  } else {
    load_by_limit <- as.numeric(NA)
  }
  
  imports <- DBPOOL %>% dbGetQuery("
    select 
      ids.rsf_program_id,
      ids.rsf_facility_id,
      ids.rsf_client_id,
      ids.pfcbl_category,
      ri.import_id,                                      
      ri.reporting_asof_date,
      ri.pfcbl_name as entity_name,
      coalesce(NULLIF(ri.file_sequence_name,''),ri.file_name) as file_name,
      ri.import_user_id,
      ri.import_time,                                      
      ri.import_rsf_pfcbl_id,
      ri.import_comments,
      accounts.account_id as reporting_user_id,
      accounts.users_name as reporting_user_name,
      rt.template_name,
      rt.file_extension,
      rt.template_id,
      rt.template_name,
      rt.is_zero_versionable,
      rtn.entity_name as entity_file_name,
      coalesce(rtn.current_template_sequence_number,0) as current_template_sequence_number,
      rtn.next_reporting_asof_date::text as next_reporting_asof_date,
      ri.reporting_asof_date = (max(ri.reporting_asof_date) over(partition by ids.rsf_pfcbl_id)) as currentest_reporting_template,
      rtn.entity_id
    from p_rsf.rsf_pfcbl_ids ids 
    inner join p_rsf.reporting_imports ri on ri.import_rsf_pfcbl_id = ids.rsf_pfcbl_id
    inner join p_rsf.reporting_templates rt on rt.template_id = ri.template_id
    left join p_rsf.view_account_info accounts on accounts.account_id = ri.import_user_id
    left join p_rsf.view_rsf_setup_export_reporting_template_names rtn on rtn.import_id = ri.import_id
                                                                      and rtn.next_reporting_asof_date is not null
    where $1::int in (ids.rsf_program_id,
                      ids.rsf_facility_id,
                      ids.rsf_client_id)
      and case when $2::text = 'global' then $1::int = ids.rsf_pfcbl_id
               when $2::text = 'program' then $1::int = ids.rsf_pfcbl_id
               when $2::text = 'facility' then $1::int in (ids.rsf_facility_id,ids.rsf_client_id)
               else true end
    order by ri.import_id desc
    limit (NULLIF($3::text,'NA'))::int",
        params=list(selected_entity$rsf_pfcbl_id,
                    selected_category,
                    as.character(load_by_limit)))
  
  #--and rc.rsf_facility_id is not distinct from $2::int
  if (empty(imports)) return (NULL)
  setDT(imports)
  
  imports[,source_name:=gsub("\\.gz$","",file_name)]
  
  imports[,
          users_name:=sapply(reporting_user_name,format_name_abbreviation)]
  
  imports[,upload_text:=paste0(toupper(format.Date(import_time,"%b%d %Hh%M")))]
  
  imports[,
          `:=`(data_checks_active=0,
               data_checks_critical_active=0,
               data_checks_error_active=0,
               data_checks_warning_active=0,
               data_checks_info_active=0,
               data_checks_new=0,
               data_checks_critical_new=0,
               data_checks_error_new=0,
               data_checks_warning_new=0,
               data_checks_info_new=0)]
  
  counts <- DBPOOL %>% dbGetQuery("
    select 
      idc.reporting_asof_date,
      idc.import_rsf_pfcbl_id,
      idc.import_id, -- June4
      sum(idc.data_count_reported) as data_count_reported,
      sum(idc.data_count_calculated) as data_count_calculated,
      sum(idc.data_current_count_reported) as data_current_count_reported,
      sum(idc.data_current_count_calculated) as data_current_count_calculated
    from p_rsf.view_reporting_imports_data_counts idc
    where idc.import_id = any($1::int[])
    group by 
    idc.reporting_asof_date,
    idc.import_rsf_pfcbl_id,
                                  idc.import_id",
  params=list(dbMakeIntArray(imports$import_id)))
  
  # counts <- DBPOOL %>% dbGetQuery("
  #   select * from p_rsf.view_reporting_imports_data_counts idc
  #   where idc.import_id = any(select unnest(string_to_array($1::text,','))::int)",
  # params=list(paste0(unique(cohorts$import_id),collapse=",")))
  
  setDT(counts)

  flags <- DBPOOL %>% dbGetQuery("
    select 
      cca.import_rsf_pfcbl_id,
      cca.import_id, -- June4
      cca.check_asof_date as reporting_asof_date,
      sum(cca.data_checks_active) as data_checks_active,
      sum(cca.data_checks_critical_active) as data_checks_critical_active,
      sum(cca.data_checks_error_active) as data_checks_error_active,
      sum(cca.data_checks_warning_active) as data_checks_warning_active,
      sum(cca.data_checks_info_active) as data_checks_info_active,
      
      sum(cca.data_checks_new) as data_checks_new,
      sum(cca.data_checks_critical_new) as data_checks_critical_new,
      sum(cca.data_checks_error_new) as data_checks_error_new,
      sum(cca.data_checks_warning_new) as data_checks_warning_new,
      sum(cca.data_checks_info_new) as data_checks_info_new
      
    from p_rsf.view_reporting_imports_data_checks_current_active cca
    where cca.import_id = any($1::int[])
    group by
    cca.import_rsf_pfcbl_id,
    cca.check_asof_date
                                 
                                 ,cca.import_id",
  params=list(dbMakeIntArray(imports$import_id)))
  
  
  setDT(flags)
  
  imports[,is_deletable:=TRUE]  
  
  setorder(imports,
           -reporting_asof_date,
           -is_deletable,
           -import_id)
  
  #date groups, etc are all legacy stuff that doesn't matter but retained because it works.  Only actual user imports are now displayed
  #and where cross-date cohorts are generated, these are now available in the "collections" review
  imports[,import_cohort_date_group:=1:.N]
  
  imports[is_deletable==FALSE,
          source_name:=paste0("[SYSTEM] ",source_name)]

  imports[flags,
          `:=`(data_checks_active=i.data_checks_active,
               data_checks_critical_active=i.data_checks_critical_active,
               data_checks_error_active=i.data_checks_error_active,
               data_checks_warning_active=i.data_checks_warning_active,
               data_checks_info_active=i.data_checks_info_active,
               
               
               data_checks_new=i.data_checks_new,
               data_checks_critical_new=i.data_checks_critical_new,
               data_checks_error_new=i.data_checks_error_new,
               data_checks_warning_new=i.data_checks_warning_new,
               data_checks_info_new=i.data_checks_info_new,
               
               #effectively placeholder counts because checks that are flagged outside of import reporting date may simply be the result of 
               #non reported data getting calcualted out of the reporting timeline.
               #and if there is real reporting data, these values will all be over-written.
               data_count_reported=0,
               data_count_calculated=i.data_checks_active,
               data_current_count_reported=0,
               data_current_count_calculated=i.data_checks_active),
          on=.(import_rsf_pfcbl_id,
               reporting_asof_date,import_id)]
  
  imports[counts,
          `:=`(data_count_reported=i.data_count_reported,
               data_count_calculated=i.data_count_calculated,
               data_current_count_reported=i.data_current_count_reported,
               data_current_count_calculated=i.data_current_count_calculated),
          on=.(import_rsf_pfcbl_id,
               reporting_asof_date,import_id)]
  
  #0=Normal cohort reporting
  imports[,cohort_checks:=0]
  
  #1=Question mark flag (shouldn't happen)
  imports[(is.na(data_count_reported) | is.na(data_count_calculated)),
          cohort_checks:=1]

  #2=Didn't report anything
  imports[data_current_count_reported==0 &
          data_current_count_calculated==0,
          cohort_checks:=2]

  #3=Only system-calculated data reported (probably means overwrote whatever reported data triggered the calculation)
  imports[
          (data_current_count_reported==0 &
          data_current_count_calculated >0),
          cohort_checks:=3]
  
  imports[(data_current_count_reported >0 |
          data_current_count_calculated >0) &
          data_checks_active==0,
          cohort_checks:=4]
  
  imports[(data_current_count_reported >0 |
           data_current_count_calculated >0) &
           data_checks_active >0 &
           data_checks_new == 0,
          cohort_checks:=5]
  
  imports[,reporting_asof_date_label:=format_asof_date_label(reporting_asof_date)]
  imports[,flags:="<div style='display:inline-block;'>"]
  
  imports[cohort_checks==1,
          flags:=paste0(flags,"<i class='fa-solid fa-circle-question icon-info' title='Dataset may not have uploaded correctly: recommended to delete and try again'></i>")]
  
  imports[cohort_checks==2,
          flags:=paste0(flags,"<i class='fa-solid fa-ban icon-error' title='Dataset has reported nothing: is this a duplicated upload?'></i>")]

  imports[cohort_checks==3,
          flags:=paste0(flags,"<i class='fa-solid fa-calculator icon-warning fas' title='Dataset only has calcuated data (no reported data)' ",
                        "data-count='",data_current_count_calculated,"'></i>")]

  imports[cohort_checks==4,
          flags:=paste0(flags,"<i class='fa-solid fa-check icon-info' style='font-weight:bold;color:green;' title='Dataset has no active flags' ",
                        "data-count='",(data_current_count_reported+data_current_count_calculated),"'></i>")]
  
  imports[cohort_checks==0 & data_checks_critical_new>0,flags:=paste0(flags,"<i class='fas fa-fire icon-critical' data-count='",data_checks_critical_new,"'></i>")]
  imports[cohort_checks==0 & data_checks_error_new>0,flags:=paste0(flags,"<i class='fas fa-times-circle icon-error' data-count='",data_checks_error_new,"'></i>")]
  imports[cohort_checks==0 & data_checks_warning_new>0,flags:=paste0(flags,"<i class='fas fa-exclamation-triangle icon-warning' data-count='",data_checks_warning_new,"'></i>")]
  imports[cohort_checks==0 & data_checks_info_new>0,flags:=paste0(flags,"<i class='fas fa-info-circle icon-info' data-count='",data_checks_info_new,"'></i>")]
  
  imports[cohort_checks==5,
          flags:=paste0(flags,"<i title='Dataset has ACTIVE flags (but no NEW flags)' class='fas fa-flag ",
                        fcase(data_checks_critical_active > 0,paste0(" icon-critical' data-count='",data_checks_critical_active,"'"),
                              data_checks_error_active > 0,paste0(" icon-error' data-count='",data_checks_error_active,"'"),
                              data_checks_warning_active > 0,paste0(" icon-warning' data-count='",data_checks_warning_active,"'"),
                              data_checks_info_active > 0,paste0(" icon-info' data-count='",data_checks_info_active,"'")),
                        "></i>")]
  
  imports[,flags:=paste0(flags,"</div>")]
  
  
  
  #https://stackoverflow.com/questions/51145207/r-shiny-datatable-how-to-prevent-row-selection-deselection-in-columns-containing    
  #delete is by individual import_id (delete the whole im port)
  imports[,delete:=paste0("<input type='checkbox' ",
                          ifelse(is_deletable==FALSE,"disabled='disabled'",""),
                          " name='cohort_actions' value=",import_id,
                          " onmousedown='event.stopPropagation();' onclick='Shiny.setInputValue(\"import_action_id_selected\",",
                          import_id,",{priority:\"event\"})' />")] 
  #View is by pseudo cohort_id that will disaggregate by check_asof_date/reporting_asof_date for the respective import if there are timeseries information in the single import file
  imports[,
          actions:=paste0("<div style='display:inline-block;'>
<div onmousedown='event.stopPropagation();'  style='display:inline-block;'>
<i class='fa fa-eye icon-view pointer' title='View' onclick='Shiny.setInputValue(\"import_action_id_view\",",import_id,",{priority:\"event\"})'></i>
</div>
</div>")]
  
  imports[,zeroversion:=""]
  imports[is_zero_versionable==TRUE & currentest_reporting_template==TRUE,
          zeroversion:=paste0("<div style='display:inline-block;padding-left:10px;'>
<div onmousedown='event.stopPropagation();'  style='display:inline-block;color:orange;'>
<i class='fa-solid fa-sun icon-view pointer' style='color:orange;' title='Create Zero Version for ",
format_asof_date_label(as.Date(next_reporting_asof_date)),
"' onclick='Shiny.setInputValue(\"import_action_id_zeroversion\",",import_id,",{priority:\"event\"})'></i>
</div>
</div>")]
  
  imports[,
          download:=paste0("<div style='display:inline-block;'>
<div onmousedown='event.stopPropagation();'  style='display:inline-block;'>
<i class='fa fa-download icon-view pointer' title='Download' onclick='Shiny.setInputValue(\"import_action_id_download\",",import_id,",{priority:\"event\"})'></i>
</div>
</div>")]
  

  {
    
    if (!isTruthy(phrase) || nchar(phrase) < 3) return (imports) 
    
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
      lapply(imports[,
                     .(import_id,
                       reporting_user_name,
                       file_name,
                       entity_name,
                       import_comments,
                       reporting_asof_date,
                       reporting_asof_date_label)],
             grep,pattern=x,ignore.case=TRUE)
    })
    
    results <- list(import_id=NA,
                    reporting_user_name=NA,
                    file_name=NA,
                    entity_name=NA,
                    import_comments=NA,
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
      filter_imports <- imports[results]
      if (!empty(filter_imports)) setorder(filter_imports,-reporting_asof_date,import_id)
      return (filter_imports)
      
    } else {
      filter_imports <- parent_cohorts[FALSE]
      return (filter_imports)
    }
  }
  
  return (imports)
})

#When a user clicks on an icon in the main datasets view panel
observeEvent(input$import_action_id_download, {
  
  export_id <- as.numeric(input$import_action_id_download)
  if (!isTruthy(export_id) || !export_id %in% IMPORTS_LIST()$import_id) {
    showNotification(type="error",
                     ui=h3("Download failed to find file ID#",export_id))
  } else { 

    shinyjs::runjs("document.getElementById('server_datasets_import_action_id_download').click();")

  }
})

observeEvent(input$import_action_id_zeroversion, {
  
  export_id <- as.numeric(input$import_action_id_zeroversion)
  if (!isTruthy(export_id) || !export_id %in% IMPORTS_LIST()$import_id) {
    showNotification(type="error",
                     ui=h3("Create Zero Version failed to find file ID#",export_id))
  } else if (empty(IMPORTS_LIST()[import_id==export_id & is_zero_versionable==TRUE & !is.na(entity_file_name)])) { 
    showNotification(type="error",
                     ui=h3("Create Zero Version failed to find file ID#",export_id," template ",IMPORTS_LIST()[import_id==export_id,template_name]))
  } else {    
    shinyjs::runjs("document.getElementById('server_datasets_import_action_id_zeroversion').click();")
    
  }
})

IMPORT_SELECTED_ID <- eventReactive(c(input$import_action_id_view,
                                      IMPORTS_LIST()), {
  
  import_id <- as.numeric(input$import_action_id_view)
  imports <- IMPORTS_LIST()
  if (!isTruthy(import_id)) return (NULL)
  if (!isTruthy(imports)) return (NULL)
  if (!import_id %in% imports$import_id) return (NULL)
  
  
  
  import_id
}, 
ignoreInit=TRUE,ignoreNULL=FALSE)

observeEvent(input$import_action_id_view, {
  
  #Because if its left on "NEW" its easy to forget there are still active flags to review.  So each load, reset the box.
  IMPORT_FLAGS_SELECTED_VIEW_FLAGGED_DATA_VALUE("ACTIVE")
},priority=300)

IMPORT_SELECTED <- eventReactive(c(IMPORT_SELECTED_ID(),
                                   IMPORTS_LIST()), {
  

  if (!isTruthy(IMPORT_SELECTED_ID())) return (NULL)
  imports <- IMPORTS_LIST()
  
  if (!isTruthy(imports)) return (NULL)
  selected_import <- imports[import_id==IMPORT_SELECTED_ID()]

  if (!empty(selected_import)) {
    stale <- DBPOOL %>% dbGetQuery("
      select exists(select true
                    from p_rsf.rsf_data_calculation_evaluations dce
                    where dce.rsf_pf_id = $1::int
                      and dce.calculation_asof_date <= $2::date)::bool",
      params=list(selected_import$import_rsf_pfcbl_id,
                  selected_import$reporting_asof_date))
    
    if (any(unlist(stale),na.rm=T)) {
      withProgress(message="Recalculating...",value=0.25, {
        progress_status_message <- function(class,...) {
          dots <- list(...)
          dots <- paste0(unlist(dots),collapse=" ")
          incProgress(amount=0,
                      message=paste0("Recalculating affected data: ",dots))
        }
        
        incProgress(amount=0.25,message="Recalculating data...")
        
        DBPOOL %>% rsf_program_calculate(rsf_indicators=RSF_INDICATORS(),
                                         rsf_pf_id=selected_import$import_rsf_pfcbl_id,
                                         for_import_id=selected_import$import_id,
                                         calculate_future=FALSE,
                                         reference_asof_date=selected_import$reporting_asof_date,
                                         status_message=progress_status_message)
      })
    }

    stale <- DBPOOL %>% dbGetQuery("
      select exists(select true
                    from p_rsf.rsf_data_check_evaluations dce
                    inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.to_family_rsf_pfcbl_id = dce.rsf_pfcbl_id
                    where ft.from_rsf_pfcbl_id = $1::int
                      and dce.check_asof_date <= $2::date)::bool",
                                   params=list(selected_import$import_rsf_pfcbl_id,
                                               selected_import$reporting_asof_date))
    
    if (any(unlist(stale),na.rm=T)) {
      withProgress(message="Rechecking...",value=0.25, {
        
        progress_status_message <- function(class,...) {
          dots <- list(...)
          dots <- paste0(unlist(dots),collapse=" ")
          incProgress(amount=0,
                      message=paste0("Rechecking affected data: ",dots))
        }
        
        incProgress(amount=0.25,message="Rechecking data...")
        DBPOOL %>% rsf_program_check(rsf_indicators=RSF_INDICATORS(),
                                     rsf_pf_id=selected_import$import_rsf_pfcbl_id,
                                     for_import_id=selected_import$import_id,
                                     check_future=FALSE,
                                     check_consolidation_threshold=NA,
                                     reference_asof_date=selected_import$reporting_asof_date,
                                     status_message=progress_status_message)
      })
    }    
  }
  
  return (selected_import)
  
}, 
ignoreNULL=FALSE)

SERVER_DATASETS_FLAGS_DATA_SYS_FLAGS <- eventReactive(LOGGEDIN(), {
  
  df <- DBPOOL %>% dbGetQuery("select data_flag_value,data_flag_name,comments from p_rsf.rsf_data_sys_flags")
  setDT(df)
  return(df)
})

SERVER_DATASETS_FLAGS_RSF_CHECKS_CALCULATOR <- eventReactive(LOGGEDIN(), {
  
  
  cchecks <- DBPOOL %>% dbGetQuery("select indicator_check_id,check_name,is_calculator_check from p_rsf.indicator_checks where is_calculator_check is true")
  setDT(cchecks)
  
  ccheck_names <- c("sys_calculator_vs_reported_calculation",      #NOT overwritten. Contains check_data for value that was NOT overwritten by system. CALCULATE tag: forced re-calculate to allow overwrite
                    "sys_calculator_overwrote_manual_calculation", #OVERWRITTEN. Does not contain check_data. MANUAL tag: soft-deletes overwritten data to allow reported data to be current, tagged to deny future overwrites
                    "sys_calculator_vs_missing_calculation",       #WRITTEN. Does not contain check_data. MANUAL tag: soft-deleted written data (ie, new for reporting_asof_date) to allow reported data to be current, tagged to deny future overwrites
                    "sys_calculator_failed")                       #Nothing written due to errors. No tag available. Ideally will prompt formula fix and proceed as normal.
  
  if (!setequal(cchecks$check_name,ccheck_names)) {
    stop("RSF Jason expects to see 4 defined checks used by the system calculator. If a check was deleted or renamed, it should be reverted. The following cannot be found: ",
         paste0(setdiff(ccheck_names,ccheck$check_name),collapse=", "))
  }
  
  cchecks[,data_flag_value:=as.numeric(NA)]
  cchecks[check_name=="sys_calculator_vs_reported_calculation",
          `:=`(data_flag_name="CALCULATE",
               data_flag_value=SERVER_DATASETS_FLAGS_DATA_SYS_FLAGS()[data_flag_name=="CALCULATE",data_flag_value])]
  
  cchecks[check_name=="sys_calculator_overwrote_manual_calculation",
          `:=`(data_flag_name="MANUAL",
               data_flag_value=SERVER_DATASETS_FLAGS_DATA_SYS_FLAGS()[data_flag_name=="MANUAL",data_flag_value])]
  
  cchecks[check_name=="sys_calculator_vs_missing_calculation",
          `:=`(data_flag_name="MANUAL",
               data_flag_value=SERVER_DATASETS_FLAGS_DATA_SYS_FLAGS()[data_flag_name=="MANUAL",data_flag_value])]
  
  cchecks <- cchecks[,.(indicator_check_id,is_calculator_check,check_name,data_flag_name,data_flag_value)]
  
  return(cchecks)
  
  
},ignoreNULL=FALSE)
#For the DATASE REVIEW panel: based on 
#Selected cohort will pull all flags for all cohorts under this selected cohort rsf_client_id
#Unless the panel is specifically requesting an individiual cohort
IMPORT_FLAGS_SELECTED <- eventReactive(IMPORT_SELECTED(), { 
                                          
  import <- IMPORT_SELECTED()
  if (empty(import)) return (NULL)

  flags_data <- NULL
  
  flags_data <- DBPOOL %>% dbGetQuery("
      select 
        rc.import_id,
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
        
        rdc.check_status is not distinct from 'active' AND
          (rdc.check_status_comment is NULL 
           or 
           scc.config_time is not distinct from rdc.status_time
          )::bool 
        as is_new_status,
        
        sis.is_subscribed,
        sis.formula_id as indicator_formula_id,
        sis.is_calculated as formula_is_calculated,
        indf.formula_title,
        indf.is_primary_default,
        indcf.check_formula_title,
        rdc.data_sys_flags,
        dic.data_flag_name,
        coalesce(dic.data_flag_value,0) & (~coalesce(rdc.data_sys_flags,0)) as data_flag_value,
        ind.unit_fx_indicator_id,
        fxind.indicator_name as unit_fx_indicator_name,
        ind.unit_fx_source,
        ind.unit_fx_method

      from p_rsf.reporting_cohorts rc
      inner join p_rsf.rsf_data rd on rd.reporting_cohort_id = rc.reporting_cohort_id
      inner join p_rsf.rsf_data_checks rdc on rdc.data_id = rd.data_id      
      inner join p_rsf.indicators ind on ind.indicator_id = rdc.indicator_id
      inner join p_rsf.indicator_checks ic on ic.indicator_check_id = rdc.indicator_check_id
      left join p_rsf.indicators fxind on fxind.indicator_id = ind.unit_fx_indicator_id
      left join p_rsf.view_rsf_setup_indicator_subscriptions sis on sis.rsf_pfcbl_id = rdc.rsf_pfcbl_id
                                                                and sis.indicator_id = rdc.indicator_id
      
      left join p_rsf.indicator_formulas indf on indf.formula_id = sis.formula_id
      
      left join p_rsf.indicator_check_formulas indcf on indcf.check_formula_id = rdc.check_formula_id
      left join p_rsf.rsf_setup_checks_config scc on scc.config_id = rdc.config_id
      left join p_rsf.view_indicator_checks_data_is_correctable dic on dic.check_formula_id is not distinct from rdc.check_formula_id 
                                                                   and dic.correctable_indicator_id = rdc.indicator_id
                                                     
      where rc.reporting_rsf_pfcbl_id in (select x.rsf_pfcbl_id
                                          from p_rsf.reporting_imports ri
                                          inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = ri.import_rsf_pfcbl_id
                                          cross join lateral (values (ids.rsf_program_id),(ids.rsf_facility_id)) as x(rsf_pfcbl_id)
                                          where ri.import_id = $1::int)
                                            
                                            --changed because a PROGRAM uploading a FACILITY and triggering calculatins would pull-in calculated data for other facilities
                                            --(select distinct reporting_rsf_pfcbl_id from p_rsf.reporting_cohorts rc where import_id = ::int)
      
        and rdc.check_asof_date = $2::date
        and rdc.check_data_id_is_current = true",
      params=list(import$import_id,
                  import$reporting_asof_date))

  setDT(flags_data)
  
  flags_data[is.na(formula_title) & !is.na(unit_fx_indicator_name),
             formula_title:=paste0("FX: ",unit_fx_indicator_name," (on ",unit_fx_method," date",ifelse(unit_fx_source != "default",paste0("/",toupper(unit_fx_source)),""),")")]
  
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

  
  flags_data[SERVER_DATASETS_FLAGS_RSF_CHECKS_CALCULATOR(),
                   `:=`(data_flag_name=i.data_flag_name,
                        data_flag_value=i.data_flag_value),
                   on=.(indicator_check_id)]

  return(flags_data)  
}, ignoreNULL=FALSE)

observeEvent(input$cohort_view_flagged_data,{

  if (!isTruthy(input$cohort_view_flagged_data)) return (NULL)
  if (!input$cohort_view_flagged_data==IMPORT_FLAGS_SELECTED_VIEW_FLAGGED_DATA_VALUE()) {
    IMPORT_FLAGS_SELECTED_VIEW_FLAGGED_DATA_VALUE(input$cohort_view_flagged_data)
  }

},ignoreNULL=TRUE,ignoreInit = T)


#Indicators-level view for cohort review panel: collapsed evaluation_ids by indicator and check
IMPORT_FLAGS_SELECTED_SUMMARY <- eventReactive(c(IMPORT_FLAGS_SELECTED(),
                                                  input$cohort_view_flagged_data,  #ALL/RESOLVED/ACTIVE/NEW
                                                  input$cohort_view_flag_classes,  #error/warning/info  
                                                  input$cohort_view_flag_types),   {
  cohort_flags <- IMPORT_FLAGS_SELECTED()
  if (is.null(cohort_flags)) return (NULL)
  
  if (!isTruthy(cohort_flags) || empty(cohort_flags)) return (NULL)
  
  cohort_indicator_flags <- cohort_flags[,
                                 .(active_count=sum(check_status=="active"),
                                   resolved_count=sum(check_status=="resolved"),
                                   is_new_count=sum(is_new_status),
                                   evaluation_ids=list(evaluation_id),
                                   import_ids=list(unique(import_id))),
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
  if (!isTruthy(view_data_flags)) view_data_flags <- ""
  
  flagged_filter <- TRUE
  
  if ("ACTIVE" %in% view_data_flags) flagged_filter <- cohort_indicator_flags$active_count > 0
  else if ("RESOLVED" %in% view_data_flags) flagged_filter <- cohort_indicator_flags$resolved_count > 0
  else if ("NEW" %in% view_data_flags) flagged_filter <- (cohort_indicator_flags$is_new_count > 0)
  #if (all(view_data_flags=="")) flagged_filter <- TRUE
  
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
  
  if (any(view_flag_types=="client_attention")) {
    cohort_indicator_flags <- cohort_indicator_flags[data_category %in% c("loan","borrower","client") &
                                                     check_is_calculator==FALSE & 
                                                     check_is_system==FALSE]
                                                       
  }
  
  
  
  view_flag_types <- intersect(rsf_check_types$type_class,view_flag_types)
  
  if (length(view_flag_types) >0) {
    cohort_indicator_flags <- cohort_indicator_flags[check_type %in% view_flag_types]
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

IMPORT_FLAGS_SELECTED_CLASSIFICATION <- eventReactive(IMPORT_FLAGS_SELECTED_SUMMARY(), {
  if (is.null(IMPORT_FLAGS_SELECTED_SUMMARY())) return (NULL)                                                                    
  if (empty(IMPORT_FLAGS_SELECTED_SUMMARY())) return (data.table(indicator_id=numeric(0),
                                                                           classification=character(0),
                                                                           indicator_name=character(0)))

  flags <- IMPORT_FLAGS_SELECTED_SUMMARY()
  
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
    params=list(IMPORT_SELECTED()$import_rsf_pfcbl_id))
  
  setDT(classification_indicators)
  
  classification_indicators <- classification_indicators[indicator_id %in% flags$indicator_id]
  setorder(classification_indicators,
           indicator_name,
           indicator_id)
  
  return (classification_indicators)

},ignoreNULL=TRUE)

IMPORT_FLAGS_SELECTED_SUMMARY_FILTERED <- eventReactive(c(input$cohort_view_flag_indicator_classifications,
                                                  IMPORT_FLAGS_SELECTED_SUMMARY()), {
  flags <- IMPORT_FLAGS_SELECTED_SUMMARY()
  filter <- input$cohort_view_flag_indicator_classifications
  
  if (isTruthy(filter)) {
    indicator_classifications <- IMPORT_FLAGS_SELECTED_CLASSIFICATION()
    
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

observeEvent(IMPORT_FLAGS_SELECTED_SUMMARY(), {
  
  if (empty(IMPORT_FLAGS_SELECTED_SUMMARY()) ||
      empty(IMPORT_FLAGS_SELECTED_CLASSIFICATION())) {
    updateSelectizeInput(session=session,
                         inputId="cohort_view_flag_indicator_classifications",
                         choices = "",
                         selected = "",
                         options=list(placeholder="No priority indicator flags"))
  
  } else {
    indicators <- IMPORT_FLAGS_SELECTED_CLASSIFICATION()
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

observeEvent(IMPORT_SELECTED(), {
  
  if (empty(IMPORT_SELECTED())) {
    current_panel <- input$datasetsTabset
    hideElement("dataset_review_header")
    #had selected a cohort and now have changed it (ie, changed by not by user click, but programmatic click)
    if (isTruthy(current_panel) && current_panel=="review") { 

      updateTabsetPanel(session=session,inputId="datasetsTabset",selected="list")
    }
  
  } else {
    showElement("dataset_review_header")
    current_panel <- input$datasetsTabset
    
    # updateSelectizeInput(session = session,
    #                      inputId="cohort_view_flagged_data",
    #                      choices=c(`All`="ALL"),
    #                      selected="")
    
    if (!isTruthy(current_panel) || current_panel != "review") {
      updateTabsetPanel(session=session,inputId="datasetsTabset",selected="review")
    }
    
    
  } 
  
},ignoreNULL=FALSE,ignoreInit = TRUE,priority=10)

observeEvent(IMPORT_FLAGS_SELECTED(), {
  flags_data <- IMPORT_FLAGS_SELECTED()
  
  if (is.null(flags_data)) return (NULL)

  {
    view_data.choices <- c(`All`="ALL")
    view_data.choices <- c(view_data.choices,`Active`="ACTIVE")
    view_data.selected <- "ACTIVE"
    
    if (any(flags_data$check_status=="active")) {
      
      
      
      if (any(flags_data$is_new_status==FALSE & flags_data$check_status=="active") &
          !all(flags_data$is_new_status==TRUE)) {
        view_data.choices <- c(view_data.choices,`New`="NEW")
        

        if (isTruthy(IMPORT_FLAGS_SELECTED_VIEW_FLAGGED_DATA_VALUE()) && IMPORT_FLAGS_SELECTED_VIEW_FLAGGED_DATA_VALUE()=="NEW") {
          view_data.selected = "NEW"
        }

      } 
    }
    if (any(flags_data$check_status=="resolved")) view_data.choices <- c(view_data.choices,`Resolved`="RESOLVED")
    
    if (empty(flags_data)) view_data.choices <- c(view_data.choices,`No Flags`="")

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
      
      if ("loan" %in% data_types | 
          "borrower" %in% data_types |
          "contract" %in% cohort_types) {
        type.choices <- c(setNames("client_attention","<i class='fa-solid fa-user' style='color:red'></i> Client Attention"),type.choices)
      }
      
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

observeEvent(IMPORT_LIST__REFRESH(), { 
  
  SERVER_SETUP_INDICATORS_LIST_REFRESH(SERVER_SETUP_INDICATORS_LIST_REFRESH()+1) #Datasets can create/affect/auto-subscribed and should be up-to-date
                                                                                 #Will also trigger checks refresh
  SERVER_DASHBOARD_REFRESH(SERVER_DASHBOARD_REFRESH()+1) #Ensure new data is reflected in any reports.
  
},ignoreInit=TRUE)

observeEvent(input$action_dataset_review_filter_clear, {
  
  shinyjs::runjs(paste0("Shiny.setInputValue('import_action_id_view','',{priority:'event'})"))
  
  updateTextInput(session=session,inputId="dataset_review_filter",value="")
}, ignoreInit = TRUE)

observeEvent(input$action_cohort_delete, {
  delete_import_ids <- IMPORT_IDS_SELECTED()
  program_id <- SELECTED_PROGRAM_ID()
  
  tryCatch({
    

    withProgress(value=.15,message="Deleting datasets...", {
       delete_import_ids <- unique(na.omit(delete_import_ids))
       if (length(delete_import_ids)==0) {
         return(showNotification(type="error",
                                 h3("Bad selection: reporting cohorts do not exist or may have already been deleted")))
       }
       
       affected_ids <- DBPOOL %>% dbGetQuery("
       select distinct
         pf.rsf_pf_id,
         sn.rsf_pfcbl_id,
         sn.pfcbl_name,
         rpc.pfcbl_rank as pfcbl_category_rank,
         ids.rsf_program_id
       from p_rsf.reporting_imports ri
       inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = ri.import_rsf_pfcbl_id
       cross join lateral (values (ids.rsf_facility_id),
                                  (ids.rsf_program_id)) as pf(rsf_pf_id)
       inner join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.rsf_pfcbl_id = pf.rsf_pf_id
       inner join p_rsf.rsf_pfcbl_categories rpc on rpc.pfcbl_category = sn.pfcbl_category       
       where ri.import_id = any($1::int[])
       order by rpc.pfcbl_rank desc",
       params=list(dbMakeIntArray(delete_import_ids)))                                              
    
       setDT(affected_ids)

       if (empty(affected_ids)) {
         return(showNotification(type="error",
                                 h3("Bad selection: reporting cohorts do not exist or may have already been deleted")))
         
       } else if (length(unique(affected_ids$rsf_program_id)) != 1) {
         return(showNotification(type="error",
                                 h3("Bad selection: Deletes can only affect one Program at a time")))
       }
       
       incProgress(amount=0.20,message="Deleting datasets . . . ")
       
       #Inserting into archive triggers the delete statement via a database trigger
       #Done this way so that imports can be manually deleted without triggering an archive, for testing, maintenance or other purposes.
       #But deletes via application layer will always generate an archive via here
       DBPOOL %>% dbExecute("insert into p_rsf.reporting_imports_deleted_archive(import_id,
                                                                                 deleting_user_id)
                             select
                               unnest($1::int[])::int,
                               $2::text",
                            params=list(dbMakeIntArray(delete_import_ids),
                                        USER_ID()))

       #If program doesnt exist after delete then it means we've deleted the entire program
       stillexists <- DBPOOL %>% dbGetQuery("select distinct ids.rsf_pfcbl_id
                                             from p_rsf.rsf_pfcbl_ids ids
                                             where ids.rsf_pfcbl_id = any($1::int[])
                                               and ids.pfcbl_category_rank <= 2",
                                             params=list(dbMakeIntArray(affected_ids$rsf_pfcbl_id)))

       affected_ids[,exists:=FALSE]
       affected_ids[rsf_pfcbl_id %in% stillexists$rsf_pfcbl_id,
                    exists:=TRUE]

       if (!any(affected_ids$exists==TRUE,na.rm=T)) {
         LOAD_PROGRAM_ID((-program_id))
         
       } 
       
       # July 2020
       # I took out all this code to recalculate and re-check.
       # If there's pending stuff we want to validate it, of course! That's why it's here.
       # On the other hand, deletes occur because users want to correct/refresh a dataset.
       # And now the dashboard's get_data_current() function will check if there are pending evaluations.  So even if the users
       # immediately after the delete then tries to run a report, the data will be revalidated then.  Basically, we don't need the user to 
       # wait re-calculations and re-checks after delete because they can't experience an issue that results from stale data: if they re-view and refresh
       # the list, it will recalculate.  And if they try to access any data from a report, it will refresh.  And next they upload the corrected dataset,
       # it will refresh.  So it's actuallay for evaluations to simply be left pending.
       #
       # else {
       #   
       #   incProgress(amount=0.20,
       #               message="Recalculating affected data . . . ")
       #   
       #   
       # }
       # for (rnum in 1:nrow(affected_ids)) {
       #   
       #   who <- affected_ids[rnum]
       #   
       #   limit_date <- max(who$reporting_asof_date)
       #   
       #   progress_status_message <- function(class,...) {
       #     dots <- list(...)
       #     dots <- paste0(unlist(dots),collapse=" ")
       #     incProgress(amount=0,
       #                 message=paste0("Recalculating affected ",who$pfcbl_name," data: ",dots))
       #   }
       #   
       #   DBPOOL %>% rsf_program_calculate(rsf_indicators = RSF_INDICATORS(),
       #                                    rsf_pf_id = who$rsf_pf_id,
       #                                    for_import_id=NA,
       #                                    calculate_future=FALSE,
       #                                    reference_asof_date=limit_date,
       #                                    status_message=progress_status_message)
       #   
       #   incProgress(amount=(0.30/(1/nrow(affected_ids))),
       #               message=paste0("Rechecking affected data . . . "))
       # 
       #   progress_status_message <- function(class,...) {
       #     dots <- list(...)
       #     dots <- paste0(unlist(dots),collapse=" ")
       #     incProgress(amount=0,
       #                 message=paste0("Rechecking affected ",who$pfcbl_name," data: ",dots))
       #   }
       # 
       #   DBPOOL %>% rsf_program_check(rsf_indicators=RSF_INDICATORS(),
       #                                rsf_pf_id=who$rsf_pf_id,
       #                                check_future=FALSE,
       #                                reference_asof_date=limit_date,
       #                                check_consolidation_threshold=NA,
       #                                status_message= progress_status_message)
       # 
       # }
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
  
  IMPORT_IDS_SELECTED(c())
  IMPORT_LIST__REFRESH(IMPORT_LIST__REFRESH()+1)
  
},ignoreInit=TRUE,ignoreNULL=TRUE,priority = 100)

#For deleting selected cohorts through checkboxes in datatable
observeEvent(input$import_action_id_selected, {
  action <- as.numeric(input$import_action_id_selected)
  selected <- IMPORT_IDS_SELECTED()
  if (action %in% selected) selected <- selected[-which(action==selected)]
  else selected <- c(selected,action)
  IMPORT_IDS_SELECTED(selected)
  
},ignoreNULL = FALSE,ignoreInit = TRUE)


#Dashboard button in Review Datasets panel
observeEvent(input$cohort_action_dashboard, {
  
  import <- IMPORT_SELECTED()
  
  if (empty(cohort_groupcohor)) return(NULL)
  
  
  filtered_flags <- IMPORT_FLAGS_SELECTED_SUMMARY_FILTERED()
 
  for_indicator_names <- unique(filtered_flags$indicator_name)
  for_indicator_names <- c(for_indicator_names,":include:IDs")
  
  
  dashboard_parameters <- SERVER_DASHBOARD_RUN_OPTIONS_INIT
  #dashboard_parameters$flags_filter <- "any"
  #dashboard_parameters$flags_display <- "active"
  dashboard_parameters$format_unchanged <- "black"
  
  dashboard_parameters$name_filter <- unique(IMPORT_FLAGS_SELECTED()[evaluation_id %in% unlist(filtered_flags$evaluation_ids),rsf_pfcbl_id])
  dashboard_parameters$format_pivot <- "DATA"
  
  if (length(dashboard_parameters$name_filter) <= length(for_indicator_names)) {
    dashboard_parameters$format_pivot <- "NAME"
  }
  
  for_facility_sys_names <- SELECTED_PROGRAM_FACILITIES_LIST()[rsf_facility_id %in% import$rsf_facility_id,
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
                                        for_asof_dates=import$reporting_asof_date,
                                        report_parameters=list(dashboard_parameters)))
  
})


#Title for datasets review panel in "review tab"
output$datasets_review_title <- renderUI({
  
  import <- IMPORT_SELECTED()
  if (!isTruthy(import)) return (HTML("Please select a dataset..."))
  
  flags <- IMPORT_FLAGS_SELECTED()[check_status=="active"]
  title_flags <- ""
  # if (!empty(flags)) {
  #   
  #   flags <- flags[check_rank==min(flags$check_rank),
  #                                .(checks=.N,
  #                                  by=.(check_class))]
  #   flag_class <- paste0("icon-",flags$check_class)
  #   data_count <- flags$checks
  #   
  #   title_flags <- paste0("<div style='display:inline-block;font-size:12px;padding-left:5px;'><i class='fas fa-flag ",flag_class,"' title='Dataset Flags' data-count='",data_count,"'></i></div")
  # 
  # }
  
  title <- div(style="display:inline-block;width:100%;padding-right:50px;",
               div(style="padding-left:10px;display:inline-block;font-weight:bold;",
                   paste0("Import#",import$import_id," for ",'"',import$source_name,'"')),
               div(style="padding-left:10px;display:inline-block",import$entity_name," as-of ",import$reporting_asof_date))

  return (title)
  
})

# output$cohort_view_reported_flags_active_total <- renderText({
#   
#   flags <- IMPORT_FLAGS_SELECTED()
#   if (!isTruthy(flags) || empty(flags)) return (0)
#   return (nrow(flags[check_status=="active"]))
# })
# 
# output$cohort_view_reported_flags_resolved_total <- renderText({
#   flags <- IMPORT_FLAGS_SELECTED()
#   if (!isTruthy(flags) || empty(flags)) return (0)
#   return (nrow(flags[check_status=="resolved"]))
# })
# 
# output$cohort_view_html_flags_active <- renderUI({
#   
#   flags <- IMPORT_FLAGS_SELECTED()
#   if (is.null(flags) || empty(flags)) return (HTML("<div>0</div>"))
# 
#   flags <- flags[check_status=="active"]
#   if (empty(flags)) return (HTML("<div>0</div>"))
#   
#   critical <- nrow(flags[check_class=="critical"])
#   errors <- nrow(flags[check_class=="error"])
#   warnings <- nrow(flags[check_class=="warning"])
#   info <- nrow(flags[check_class=="info"])
#   
#   html_critical <- ifelse(critical > 0,paste0("<i class = 'fas fa-fire icon-critical' data-count='",critical,"'></i>"),"") #only show if there's any
#   html_error <- ifelse(errors > 0,paste0("<i class='fas fa-times-circle icon-error' data-count='",errors,"'></i>"),"") #only show if there's any
#   html_warning <- ifelse(warnings > 0,paste0("<i class='fas fa-exclamation-triangle icon-warning' data-count='",warnings,"'></i>"),"") #only show if there's any
#   html_info <- ifelse(info > 0,paste0("<i class='fas fa-info-circle icon-info' data-count='",info,"'></i>"),"") #only show if there's any
# 
#   html_flags <- paste0("
#                 <div style='display:inline-block;'>",
#                        html_critical,
#                        html_error,
#                        html_warning,
#                        html_info,
#                        "</div>")
#   
#   return(HTML(html_flags))
# })
# 
# output$cohort_view_html_flags_resolved <- renderUI({
#   
#   flags <- IMPORT_FLAGS_SELECTED()
#   if (is.null(flags) || empty(flags)) return (HTML("<div>0</div>"))
#   
#   flags <- flags[check_status=="resolved"]
#   if (empty(flags)) return (HTML("<div>0</div>"))
#   
#   critical <- nrow(flags[check_class=="critical"])
#   errors <- nrow(flags[check_class=="error"])
#   warnings <- nrow(flags[check_class=="warning"])
#   info <- nrow(flags[check_class=="info"])
#   
#   html_critical <- ifelse(critical > 0,paste0("<i class = 'fas fa-fire icon-critical' data-count='",critical,"'></i>"),"") #only show if there's any
#   html_error <- ifelse(errors > 0,paste0("<i class='fas fa-times-circle icon-error' data-count='",errors,"'></i>"),"") #only show if there's any
#   html_warning <- ifelse(warnings > 0,paste0("<i class='fas fa-exclamation-triangle icon-warning' data-count='",warnings,"'></i>"),"") #only show if there's any
#   html_info <- ifelse(info > 0,paste0("<i class='fas fa-info-circle icon-info' data-count='",info,"'></i>"),"") #only show if there's any
#   
#   html_flags <- paste0("
#                 <div style='display:inline-block;'>",
#                        html_critical,
#                        html_error,
#                        html_warning,
#                        html_info,
#                        "</div>")
#   
#   return(HTML(html_flags))
# })

#Download the file that was uploaded
output$server_datasets_import_action_id_download <- downloadHandler(
  filename = function() {

    download_id <- as.numeric(input$import_action_id_download)
    if (!isTruthy(download_id)) {
      #showNotification(type="error",ui=h3("Failed to download file.  Please try again by clicking on the download icon next to the file name"))
      "error.txt"
    } else {
      
    
      import <- IMPORTS_LIST()[import_id==download_id]
      
      if (empty(import)) { 
        "error.txt"
      } else {
        import$source_name
      }
    }
  },
  content=function(file) {
    
    download_id <- as.numeric(input$import_action_id_download)
    
    if (!isTruthy(download_id) || !any(IMPORTS_LIST()$import_id==download_id,na.rm=T)) {
      if (!file.exists(file)) {
        write(x="Failed to download file.  Please try again by clicking on the download icon next to the file name.",
              file=file)
      }
    } else {
    
      tryCatch({
        
        import <- IMPORTS_LIST()[import_id==download_id]
        withProgress(message="Downloading file",value=0.5, {
          
          outpath <- DBPOOL %>% db_import_download_file(import_id=import$import_id)
          
          if (!is.null(outpath)) {
            #file.rename(from=outpath,to=file)
            #print("Downloading file in output$datasets_review_download_source_action")
            
            
            file.copy(from=outpath,
                      to=file,
                      overwrite = TRUE)
            
            if (file.exists(outpath)) file.remove(outpath)
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
  }
)

output$server_datasets_import_action_id_zeroversion <- downloadHandler(
  filename = function() {
    
    download_id <- as.numeric(input$import_action_id_zeroversion)
    if (!isTruthy(download_id)) {
      "error.txt"
    } else {
      
      import <- IMPORTS_LIST()[import_id==download_id]
      
      if (empty(import)) { 
        "error.txt"
      } else if (!any(as.logical(import$is_zero_versionable),na.rm=T)) {
        
        showNotification(type="error",
                         ui=h3("This template does not support system-generated zero versions"))
        "error.txt"
      } else {
        paste0("#",
               import$current_template_sequence_number+1," ",
               import$entity_file_name," ",
               format_asof_date_label(as.Date(import$next_reporting_asof_date),add_month=F),
               " - v0.xlsx")
      }
    }
  },
  content=function(file) {
    
    download_id <- as.numeric(input$import_action_id_zeroversion)
    
    if (!isTruthy(download_id) || !any(IMPORTS_LIST()$import_id==download_id,na.rm=T)) {
      
        write(x="Failed to download file.  Please try again by clicking on the download icon next to the file name.",
              file=file)
      
    } else {
      
      tryCatch({
        
        import <- IMPORTS_LIST()[import_id==download_id]
        
        withProgress(message=paste0("Generating zero-version file: ",
                                    paste0("#",
                                           import$current_template_sequence_number+1," ",
                                           import$entity_file_name," ",
                                           format_asof_date_label(as.Date(import$next_reporting_asof_date),add_month=F),
                                           " - v0.xlsx")),value=0.5, {
          excelwb <- NULL
          
          outpath <- DBPOOL %>% db_import_download_file(import_id=import$import_id)
          
          if (import$template_name=="IFC-QR-TEMPLATE2025") {
            
            #browser()
            
            lookup <- db_export_get_template(pool=DBPOOL,
                                             template_name="IFC-QR-TEMPLATE2025")
            
            rsf_indicators <- db_indicators_get_labels(pool=DBPOOL)
            
            excelwb <- parse_template_IFC_QR2025(pool=DBPOOL,
                                                template_file=outpath,
                                                template_lookup=lookup,
                                                rsf_indicators=rsf_indicators,
                                                return.insert_flags=NULL, #To insert and return current flags tab based on current QR of template_file in system: this is a DATA TABLE of flags exported by UI
                                                return.next_date=TRUE,    #To automatically create zero-version of next QR based on current QR in system
                                                reporting_user_id=USER_ID(),
                                                status_message=function(...) { },
                                                CALCULATIONS_ENVIRONMENT=CALCULATIONS_ENVIRONMENT) 
          
          } else {
            write(x=paste0("Failed to download file. Zero version cannot be generated by the system for ",import$template_name,
            "Templates. Please try again by clicking on the download icon next to the file name."),
                  file=file)
            
          }
          
          
          if (!is.null(outpath) & !is.null(excelwb)) {

            wb_save(excelwb,
                    file=file,
                    overwrite=T)
          
            if (file.exists(outpath)) file.remove(outpath)
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
  }
)


####RENDER DATA TABLES####

###Summary of flags reported on dataset for bulk flags review and management 
output$datasets_review_flags_summary <- DT::renderDataTable({
  
  cohort_indicator_flags <- IMPORT_FLAGS_SELECTED_SUMMARY_FILTERED()
  
  
  
  
  if (!isTruthy(cohort_indicator_flags)) {
    view_flags_data <- input$cohort_view_flagged_data
    if (empty(IMPORT_FLAGS_SELECTED())) {
      return (DT::datatable(data.frame(`Nothing`=paste0("This selection has no flags (no active and no resolved flags) at all")),
                            rownames=FALSE,
                            fillContainer = TRUE,
                            options=list(
                              dom="t",
                              autoWidth=TRUE,
                              paging=FALSE
                            )))
      
    } else if (!isTruthy(view_flags_data)) {
      return (DT::datatable(data.frame(`Nothing`=paste0("Please select ACTIVE/RESOLVED flags in the View Flags menu")),
                            rownames=FALSE,
                            fillContainer = TRUE,
                            options=list(
                              dom="t",
                              autoWidth=TRUE,
                              paging=FALSE
                            )))
      
    } else {
      return (DT::datatable(data.frame(`Nothing`=paste0("There are no ",view_flags_data," flags to display")),
                                                 rownames=FALSE,
                                                 fillContainer = TRUE,
                                                 options=list(
                                                   dom="t",
                                                   autoWidth=TRUE,
                                                   paging=FALSE
                                                 )))
    }
  }
  display_cols <- c()
  defs <- NULL
  display_cols <- c(Review="action_review",
                    Flag="check_display_html",
                    Check="check_html",
                    Active="active_count",
                    Resolved="resolved_count",
                    New="is_new_count")
  
  # if (length(unique(unlist(cohort_indicator_flags$import_ids)))>1) {
  #   cohort_indicator_flags[,imports:=sapply(import_ids,function(x) { paste0(unlist(x),collapse=", ") })]
  #   display_cols <- c(display_cols,UploadID="imports")
  # }

  cohort_indicator_flags <- cohort_indicator_flags[,..display_cols]
  
                                                   # .(action_review,      #0
                                                   #   check_display_html, #1
                                                   #   check_html,         #2
                                                   #   active_count,       #3
                                                   #   resolved_count,     #4
                                                   #   new_count)]         #5
  
  defs <- list(list(className = 'dt-left', targets = c(1,2)),  #Zero-based targets
               list(className = 'dt-center', targets = c(0,3,4,5)))
  

  DT::datatable(cohort_indicator_flags,
                rownames = FALSE,
                fillContainer=TRUE,
                colnames=names(display_cols),
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
  cohorts <- IMPORTS_LIST()
  
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
  
  cohorts <- cohorts[,.(
                        actions,
                        entity_name,
                        reporting_asof_date_label=paste("<div style='display:flex;flex-direction:row;flex-wrap:nowrap;'>",
                                                        "  <div style='display:flex;flex-grow:1'>",reporting_asof_date_label,
                                                        "  <div style='display:flex;flex-shrink:1;'>",zeroversion,"</div>",
                                                        "</div>"),
                        file_name=paste("<div style='display:flex;flex-direction:row;flex-wrap:nowrap;'>",
                                          "<div style='display:flex;flex-shrink:1'>",download,"</div>",
                                          "<div style='display:flex;flex-grow:1'>",
                                             gsub("\\.gz$","",
                                                  fcase(is_deletable==TRUE,file_name,
                                                        is_deletable==FALSE,paste0("[SYSTEM] ",file_name))),
                                          "</div>",
                                         "</div>"),
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
    formatStyle(columns=c(0,1,2,3,4,6,7),whiteSpace="nowrap") %>%
    formatStyle(columns=0,minWidth="50px",width="50px") %>%   #Eye
    formatStyle(columns=1,minWidth="100px",width="150px") %>% #Name
    formatStyle(columns=2,minWidth="50px",width="75px") %>%   #Date
    formatStyle(columns=3,minWidth="150px",width="200px") %>% #Source
    formatStyle(columns=4,minWidth="100px") %>%
    formatStyle(columns=5,minWidth="50px") %>%
    formatStyle(columns=6,minWidth="75px") %>%
    formatStyle(columns=7,minWidth="25px")
})
