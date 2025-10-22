db_cohort_create <- function(pool,
                             reporting_user_id,
                             reporting_asof_date,
                             data_asof_date=reporting_asof_date, 
                             reporting_rsf_pfcbl_id,
                             from_reporting_template_id,
                             source_reference, #System defined [Entity | Template]
                             source_name, #User specified, akin to filename
                             source_note,  #User specified, notes on upload
                             reporting_pfcbl_categories=NA,
                             fail_on_check_class=c("critical"),       #Relevant for checking on flags and whether to fail to create because so
                             fail_on_check_submitted_indicators=NULL, #If user is submitting an indicator that has a failed check, allow it to pass
                             fail_on_incomplete_cohorts=TRUE, #If a child entity has a cohort with cohort_processing_completed = false, fail to create a new cohort
                             linked_reporting_cohort_id=NA) 
{

  
  reporting_rsf_pfcbl_id <- unlist(reporting_rsf_pfcbl_id)
  t1 <- Sys.time()

  if (length(reporting_pfcbl_categories)==0) reporting_pfcbl_categories <- NA
  
  fail_on_check_class <- tolower(fail_on_check_class)
  if (length(fail_on_check_class)==0) fail_on_check_class <- "none"
  
  if (is.null(fail_on_incomplete_cohorts) || 
      !fail_on_incomplete_cohorts %in% c(TRUE,FALSE)) {
    fail_on_incomplete_cohorts <- TRUE
  }
  
  if (is.null(linked_reporting_cohort_id) || length(linked_reporting_cohort_id) != 1) {
    linked_reporting_cohort_id <- NA
  }
  
  #conn <- poolCheckout(pool)
  # dbRollback(conn)
  #dbBegin(conn);
  
  valid_user_id <- dbGetQuery(pool,"
                              select exists(select * from p_rsf.view_account_info vai
                                            where vai.account_id = $1::text)::bool as is_valid",
                              params=list(reporting_user_id))
  
  if (empty(valid_user_id) || !all(valid_user_id$is_valid)) {
    stop(paste0("Invalid user or user does not have permissions for source=",source_name,". Failed to create reporting cohort for user ID=",reporting_user_id))
  }
  
  reporting_level <- dbGetQuery(pool,"
  select pfcbl_category,pfcbl_category_rank
  from p_rsf.rsf_pfcbl_ids where rsf_pfcbl_id = $1::int",
                                params=list(reporting_rsf_pfcbl_id))
  
  if (empty(reporting_level)) {
    stop(paste0("Invalid reporting_rsf_pfcbl_id: ",reporting_rsf_pfcbl_id,". Reporting entity does not exist"))
  }
  
  if (reporting_level$pfcbl_category_rank > 3) {
    stop(paste0("Invalid reporting_rsf_pfcbl_id: ",reporting_rsf_pfcbl_id,". ",reporting_level$pfcbl_category,"s may not create a reporting cohort"))
  }
  
  if (any(fail_on_check_class %in% c("critical","error"))) {
    
    #Critical fails to report new data until its fixed, including current period
    if (any(fail_on_check_class == "critical")) {
      
      invalid_checks_critical <- dbGetQuery(pool,"
        select 
          fam.parent_pfcbl_category,
          fam.child_pfcbl_category,
          sn.sys_name,
          ic.check_name,
          ind.indicator_name,
          rdc.check_asof_date
        from p_rsf.rsf_pfcbl_id_family fam
        inner join p_rsf.rsf_data_checks rdc on rdc.rsf_pfcbl_id = fam.child_rsf_pfcbl_id
        inner join p_rsf.indicators ind on ind.indicator_id = rdc.indicator_id
        inner join p_rsf.indicator_checks ic on ic.indicator_check_id = rdc.indicator_check_id
        inner join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.rsf_pfcbl_id = rdc.rsf_pfcbl_id
        left join p_rsf.indicator_check_guidance icg on icg.indicator_check_guidance_id = rdc.indicator_check_guidance_id
        where fam.parent_rsf_pfcbl_id = $1::int
          and coalesce(icg.overwrite_check_class,ic.check_class) = 'critical'
          and rdc.check_asof_date <= $3::date -- If a future dataset has critical, historic still allow corrections
          and case when NULLIF($2::varchar,'NA') IS NULL 
                   then true
                   else fam.child_pfcbl_category = any(string_to_array(NULLIF($2::varchar,'NA'),',')::varchar[])
              end
          and not exists(select * from p_rsf.reporting_templates rt
                         where rt.template_id is not distinct from nullif($4::text,'')::int
                           and (rt.is_system = true OR rt.is_setup_template = true))",
        params=list(reporting_rsf_pfcbl_id,
                    paste0(reporting_pfcbl_categories,collapse=","),
                    reporting_asof_date,
                    from_reporting_template_id))
      
      if (!empty(invalid_checks_critical)) {
        
        #Allows correcting indicators that are critical - practically, only allows correcting data_disallowed errors and to fix whatever is disallowed
        if (!(!is.null(fail_on_check_submitted_indicators) &
              all(reporting_asof_date %in% unique(invalid_checks_critical$check_asof_date)) &
              all(invalid_checks_critical$parent_pfcbl_category==invalid_checks_critical$child_pfcbl_category) &
              all(invalid_checks_critical$indicator_name %in% fail_on_check_submitted_indicators) &
              all(unique(invalid_checks_critical$child_pfcbl_category) %in% unique(indicatorNameResolveCategory(fail_on_check_submitted_indicators))))) {
          
          setDT(invalid_checks_critical)
          setorder(invalid_checks_critical,
                   sys_name,
                   check_asof_date,
                   check_name)
          message <- paste0(paste0(invalid_checks_critical$sys_name," ",invalid_checks_critical$check_asof_date,": ",invalid_checks_critical$check_name),
                            collapse=" \n")
          stop(paste0("Failed to upload template due to critical errors on: \n",
                      message," \n",
                      "These must be corrected before reporting new data. \n",
                      "Unlike other errors that may be corrected in-situ, critical errors require deleting the template with issues, correcting and re-uploading the fixed dataset."))
        }
      }
    }
  }
  
  if (fail_on_incomplete_cohorts == TRUE) {
    failed_cohorts <- dbGetQuery(pool,"
        select 
        rc.reporting_cohort_id,
        rc.source_name,
        rc.reporting_asof_date,
        sn.sys_name
        from p_rsf.reporting_cohorts rc
        inner join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.rsf_pfcbl_id = rc.reporting_rsf_pfcbl_id
        where rc.cohort_processing_completed = false 
          and rc.linked_reporting_cohort_id is null 
          and rc.is_reported_cohort = true 
          and rc.reporting_cohort_id is distinct from NULLIF($2::text,'NA')::int
          and rc.reporting_rsf_pfcbl_id = any(select fam.child_rsf_pfcbl_id 
                                              from p_rsf.rsf_pfcbl_id_family fam
                                              where fam.parent_rsf_pfcbl_id = $1::int)",
        params=list(reporting_rsf_pfcbl_id,
                    linked_reporting_cohort_id))
    
    if (!empty(failed_cohorts)) {
      setDT(failed_cohorts)
      setorder(failed_cohorts,
               sys_name,
               reporting_asof_date,
               source_name)
      
      message <- paste0("The following datasets failed to upload properly: \n",
                        paste0(paste0(failed_cohorts$sys_name," ",failed_cohorts$reporting_asof_date,": ",failed_cohorts$source_name),
                               collapse=" \n"),"\n",
                        "To ensure all data is properly reported, these datasets should be deleted and re-uploaded before uploading any new datasets.")
      stop(message)
    }
  }
  
  reporting_cohort <- dbGetQuery(pool,"
          insert into p_rsf.reporting_cohorts(rsf_program_id,
                                              rsf_facility_id,
                                              reporting_rsf_pfcbl_id,
                                              reporting_asof_date,
                                              reporting_user_id,
                                              reporting_time,
                                              data_asof_date,
                                              from_reporting_template_id,
                                              source_reference,
                                              source_name,
                                              source_note,
                                              is_reported_cohort,
                                              is_calculated_cohort,
                                              is_redundancy_cohort,
                                              cohort_processing_completed,
                                              linked_reporting_cohort_id)
          select 
            ids.rsf_program_id,
            ids.rsf_facility_id,
            $5::int as reporting_rsf_pfcbl_id,
            $6::date as reporting_asof_date,
            $1::text as reporting_user_id,
            TIMEOFDAY()::timestamptz as reporting_time,
            $9::date as data_asof_date,
            $7::int as from_reporting_template_id,
            concat(upper(ids.pfcbl_category),
                   coalesce('/ ' || $2::text,'')) as source_reference,
            $3::text as source_name,
            $4::text as source_note,
            true as is_reported_cohort,
            false as is_calculated_cohort,
            false as is_redundancy_cohort,
            false as cohort_processing_completed,
            $8::int as linked_reporting_cohort_id
          from p_rsf.rsf_pfcbl_ids ids
          where ids.rsf_pfcbl_id = $5::int
          returning reporting_cohort_id,
                    rsf_program_id,
                    reporting_rsf_pfcbl_id,
                    reporting_asof_date,
                    reporting_user_id,
                    (reporting_time::timestamptz)::text as reporting_time,
                    from_reporting_template_id,
                    source_reference,
                    source_name,
                    parent_reporting_cohort_id,
                    is_reported_cohort,
                    is_calculated_cohort",
          params=list(reporting_user_id,
                      source_reference,
                      source_name,
                      source_note,
                      reporting_rsf_pfcbl_id,
                      reporting_asof_date,
                      from_reporting_template_id,
                      linked_reporting_cohort_id,
                      data_asof_date))

  if (empty(reporting_cohort)) {
    stop("Failed to create new reporting cohort")
  }
  
  if(SYS_PRINT_TIMING) debugtime("db_cohort_create","Done!",as.numeric(Sys.time()-t1,"secs"))
  
  
  reporting_cohort$pfcbl_category <- reporting_level$pfcbl_category
  reporting_cohort$pfcbl_category_rank <- reporting_level$pfcbl_category_rank
  
  return (reporting_cohort)
}
