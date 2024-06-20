db_cohort_create <- function(pool,
                             rsf_program_id,
                             reporting_user_id,
                             reporting_asof_date,
                             data_asof_date=reporting_asof_date, 
                             cohort_pfcbl_id,
                             from_reporting_template_id,
                             source_reference, #System defined [Entity | Template]
                             source_name, #User specified, akin to filename
                             source_note,  #User specified, notes on upload
                             reporting_pfcbl_categories=NA,
                             fail_on_check_class=c("critical"),       #Relevant for checking on flags and whether to fail to create because so
                             fail_on_check_submitted_indicators=NULL, #If user is submitting an indicator that has a failed check, allow it to pass
                             linked_reporting_cohort_id=NA) 
{

  cohort_pfcbl_id <- unlist(cohort_pfcbl_id)
  t1 <- Sys.time()


  if (length(reporting_pfcbl_categories)==0) reporting_pfcbl_categories <- NA
  
  fail_on_check_class <- tolower(fail_on_check_class)
  if (length(fail_on_check_class)==0) fail_on_check_class <- "none"
  
  #conn <- poolCheckout(pool)
  # dbRollback(conn)
  #dbBegin(conn);
  reporting_cohort <- poolWithTransaction(pool,function(conn) { 

      valid_user_id <- dbGetQuery(conn,"select * from p_rsf.view_account_info vai
                                        where vai.account_id = $1::text",
                                  params=list(reporting_user_id))
      
      if (empty(valid_user_id) || all(is.na(valid_user_id))) {
        stop(paste0("Invalid user or user does not have permissions for source=",source_name,". Failed to create reporting cohort for user ID=",reporting_user_id))
      }
      
      #if(SYS_PRINT_TIMING) debugtime("db_cohort_create","dates successfully validated")
      
      #Only prevent user-reported data for critical and error checks
      #System checks (which will be generated WITH a parent_reporting_cohort_id) can add additional data and flags on top of user reported data as part of the same upload
      if (any(fail_on_check_class %in% c("critical","error"))) {
        
        #Critical fails to report new data until its fixed, including current period
        if (any(fail_on_check_class == "critical")) {
          invalid_checks_critical <- dbGetQuery(conn,"select distinct
                                                        fam.parent_pfcbl_category,
                                                        fam.child_pfcbl_category,
                                                        ic.check_name,
                                                        ind.indicator_name,
                                                        rdc.check_asof_date
                                                      from p_rsf.rsf_pfcbl_id_family fam
                                                      inner join p_rsf.rsf_data rd on rd.rsf_pfcbl_id = fam.child_rsf_pfcbl_id
                                                      inner join p_rsf.indicators ind on ind.indicator_id = rd.indicator_id
                                                      inner join p_rsf.rsf_data_checks rdc on rdc.data_id = rd.data_id
                                                      inner join p_rsf.indicator_checks ic on ic.indicator_check_id = rdc.indicator_check_id
                                                      left join p_rsf.indicator_check_guidance icg on icg.indicator_check_guidance_id = rdc.indicator_check_guidance_id
                                                      where fam.parent_rsf_pfcbl_id = $1::int
                                                        and coalesce(icg.overwrite_check_class,ic.check_class) = 'critical'
                                                        and rdc.check_asof_date <= $3::date -- If a future dataset has critical, historic still allow corrections
                                                        and case when NULLIF($2::varchar,'NA') IS NULL 
                                                                 then true
                                                                 else fam.child_pfcbl_category = any(string_to_array(NULLIF($2::varchar,'NA'),',')::varchar[])
                                                            end
                                                      ",
                                                params=list(cohort_pfcbl_id,
                                                            paste0(reporting_pfcbl_categories,collapse=","),
                                                            reporting_asof_date))
          
          if (nrow(invalid_checks_critical) > 0) {
            
            #Allows correcting indicators that are critical - practically, only allows correcting data_disallowed errors and to fix whatever is disallowed
            if (!(!is.null(fail_on_check_submitted_indicators) &
                  all(reporting_asof_date %in% unique(invalid_checks_critical$check_asof_date)) &
                  all(invalid_checks_critical$parent_pfcbl_category==invalid_checks_critical$child_pfcbl_category) &
                  all(invalid_checks_critical$indicator_name %in% fail_on_check_submitted_indicators) &
                  all(unique(invalid_checks_critical$child_pfcbl_category) %in% unique(indicatorNameResolveCategory(fail_on_check_submitted_indicators))))) {
              entity <- unique(invalid_checks_critical$parent_pfcbl_category)
              entity_checks <- unique(invalid_checks_critical$child_pfcbl_category)
              stop(paste0("Failed to upload template due to critical errors existing on the ",
                          toTitleCase(entity)," at the following levels: ",paste0(toTitleCase(entity_checks),collapse=", "),
                          ". These must be corrected before reporting new data."))
            }
          }
        }
        
        #Error fails prevent uploading new data in new/future reporting periods
        if (any(fail_on_check_class == "error")) {
          invalid_checks_error <- dbGetQuery(conn,"
                                                  select
                                                    fam.parent_pfcbl_category,
                                                    fam.child_pfcbl_category,
                                                    count(*) as count_critical_checks
                                                  from p_rsf.rsf_pfcbl_id_family fam
                                                  inner join p_rsf.rsf_data rd on rd.rsf_pfcbl_id = fam.child_rsf_pfcbl_id
                                                  inner join p_rsf.rsf_data_checks rdc on rdc.data_id = rd.data_id
                                                  inner join p_rsf.indicator_checks ic on ic.indicator_check_id = rdc.indicator_check_id
                                                  left join p_rsf.indicator_check_guidance icg on icg.indicator_check_guidance_id = rdc.indicator_check_guidance_id
                                                  where fam.parent_rsf_pfcbl_id = $1::int
                                                    and coalesce(icg.overwrite_check_class,ic.check_class) = 'error'
                                                    and rdc.check_asof_date < $3::date -- only < and not <= so fixes can be uploaded for this reporting date
                                                    and case when NULLIF($2::varchar,'NA') IS NULL 
                                                             then true
                                                             else fam.child_pfcbl_category = any(string_to_array(NULLIF($2::varchar,'NA'),',')::varchar[])
                                                        end
                                                  group by 
                                                  fam.parent_pfcbl_category,
                                                  fam.child_pfcbl_category",
                                            params=list(cohort_pfcbl_id,
                                                        paste0(reporting_pfcbl_categories,collapse=","),
                                                        reporting_asof_date))
          
          if (nrow(invalid_checks_error) > 0) {
            
            if (!(!is.null(fail_on_check_submitted_indicators) &
                  all(reporting_asof_date %in% unique(invalid_checks_critical$check_asof_date)) &
                  all(invalid_checks_critical$parent_pfcbl_category==invalid_checks_critical$child_pfcbl_category) &
                  all(invalid_checks_critical$indicator_name %in% fail_on_check_submitted_indicators) &
                  all(unique(invalid_checks_critical$child_pfcbl_category) %in% unique(indicatorNameResolveCategory(fail_on_check_submitted_indicators))))) {
              
              entity <- unique(invalid_checks_error$parent_pfcbl_category)
              entity_checks <- unique(invalid_checks_error$child_pfcbl_category)
              entity_dates <- unique(invalid_checks_error$evaluation_asof_date)
              
              stop(paste0("Failed to upload template due to ERROR FLAGS existing on the ",
                          toTitleCase(entity)," at the following levels: ",paste0(toTitleCase(entity_checks),collapse=", "),
                          " and reporting dates: ",paste0(entity_dates,collapse=", "),
                          ". These must be resolved or corrected before reporting data in ",reporting_asof_date,"."))
            }
          }
        }
      }
      
      reporting_cohort <- dbGetQuery(conn,
                                      "
                                      insert into p_rsf.reporting_cohorts(rsf_program_id,
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
                                        $5::int as reporting_rsf_pfcbl_id,
                                        $7::date as reporting_asof_date,
                                        $1::text as reporting_user_id,
                                        TIMEOFDAY()::timestamptz as reporting_time,
                                        $10::date as data_asof_date,
                                        $8::int as from_reporting_template_id,
                                        concat(upper(ids.pfcbl_category),
                                               coalesce('/ ' || $2::text,'')) as source_reference,
                                        $3::text as source_name,
                                        $4::text as source_note,
                                        true as is_reported_cohort,
                                        false as is_calculated_cohort,
                                        false as is_redundancy_cohort,
                                        false as cohort_processing_completed,
                                        $9::int as linked_reporting_cohort_id
                                      from p_rsf.rsf_pfcbl_ids ids
                                      where ids.rsf_pfcbl_id = $5::int
                                        and ids.rsf_program_id = $6::int
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
                                                is_calculated_cohort,
                                                is_redundancy_cohort",
                                      params=list(reporting_user_id,
                                                  source_reference,
                                                  source_name,
                                                  source_note,
                                                  cohort_pfcbl_id,
                                                  rsf_program_id,
                                                  reporting_asof_date,
                                                  from_reporting_template_id,
                                                  linked_reporting_cohort_id,
                                                  data_asof_date))
      
      reporting_cohort
       
  })

  if (empty(reporting_cohort)) {
    stop("Failed to create new reporting cohort")
  }
  
  if(SYS_PRINT_TIMING) debugtime("db_cohort_create","Done!",as.numeric(Sys.time()-t1,"secs"))
  
  
  return (reporting_cohort)
}
