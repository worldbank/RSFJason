db_program_revalidate_calculations <- function(pool,
                                               verification_date,
                                               verification_ids) {
  stop("DEPRECATED")
  t1 <- Sys.time()
  
  # if (!inherits(calculation_verifications,"data.table")) stop("calculation_verifications expects a data.table")
  # if (!all(names(calculation_verifications) %in% c("data_id",
  #                                                  "rsf_pfcbl_id",
  #                                                  "indicator_id",
  #                                                  "reporting_asof_date"))) stop("Calculation verifications must define these column names: data_id,rsf_pfcbl_id,indicator_id,reporting_asof_date,reporting_cohort_id,calculation_failed")
  # 
  if (length(verification_date) != 1) {
    stop("Only one verification date allowed at a time")
  }
  
  if (length(verification_ids)==0) {
    return (NULL)
  }
  #browser()
  # conn <- poolCheckout(pool);
  # dbBegin(conn)
  # dbRollback(conn)
  
  
  validated <- dbGetQuery(pool,"
      with data_ids as (
  
        select unnest(string_to_array($1::text,','))::int as data_id
      ),
      validate_checks as (
      
      delete from p_rsf.rsf_data_checks rdc
      using data_ids 
      where data_ids.data_id = rdc.data_id
        and rdc.check_asof_date = $2::date
        and rdc.check_data_id_is_current = true
        and rdc.indicator_check_id = any(select ic.indicator_check_id from p_rsf.indicator_checks ic where ic.is_calculator_check = true)
      returning null as done
      ),
      validate_calculations as (
      
        
      delete from p_rsf.rsf_data_calculation_evaluations dce
      using data_ids
      inner join p_rsf.rsf_data_current rdc on rdc.data_id = data_ids.data_id
      where rdc.rsf_pfcbl_id = dce.rsf_pfcbl_id
        and rdc.indicator_id = dce.indicator_id
        and dce.calculation_asof_date =  $2::date
      returning null as done
      )
      select count(*) as checks_validated from validate_checks
      union all
      select count(*) as calculations_validated from validate_calculations",
             params=list(paste0(verification_ids,collapse=","),
                         as.character(verification_date)))
  
  
  # poolWithTransaction(pool,function(conn) {
  # 
  #   #t2 <- Sys.time()
  #   
  #   # 
  #   # if(SYS_PRINT_TIMING) debugtime("db_program_revalidate_calculations","Done!",as.numeric(Sys.time()-t2,"secs"))
  #   # 
  #   # dbExecute(conn,"create TEMP table _temp_revalidations(data_id int,
  #   #                                                       rsf_pfcbl_id int,
  #   #                                                       indicator_id int,
  #   #                                                       reporting_asof_date date)
  #   #                 ON COMMIT DROP;")
  #   # 
  #   # dbAppendTable(conn=conn,
  #   #               name="_temp_revalidations",
  #   #               value=calculation_verifications)
  #   # 
  #   # 
  #   # #dbExecute(conn,"alter table _temp_revalidations add primary key(rsf_pfcbl_id,indicator_id,reporting_asof_date)")
  #   # dbExecute(conn,"analyze _temp_revalidations")
  #   # 
  #   # #If the calculator is (re)calculating, then it will also (re)flag if any check messages change: sometimes they do, any multiple conflicting flags is very confusing.
  #   # #And sometimes data triggers (re)calculations that voids the flag, so if it is no longer relevant, then remove it.
  #   # dbExecute(conn,"delete from p_rsf.rsf_data_checks rdc
  #   #                 where exists(select * from _temp_revalidations tr
  #   #                              where tr.data_id = rdc.data_id
  #   #                                and tr.reporting_asof_date = rdc.check_asof_date)
  #   #                   and rdc.check_data_id_is_current = true
  #   #                   and rdc.indicator_check_id = any(select ic.indicator_check_id from p_rsf.indicator_checks ic where ic.is_calculator_check = true)")
  #   # 
  #   # 
  #   # nx <- dbExecute(conn,"
  #   #                 delete from p_rsf.rsf_data_calculation_evaluations dce
  #   #                 using _temp_revalidations tr
  #   #                 where tr.rsf_pfcbl_id = dce.rsf_pfcbl_id
  #   #                   and tr.indicator_id = dce.indicator_id
  #   #                   and tr.reporting_asof_date = dce.calculation_asof_date
  #   #                   and exists(select * from p_rsf.rsf_data_current rdc
  #   #                              where rdc.data_id = tr.data_id)")
  #   # 
  #   # if(SYS_PRINT_TIMING) debugtime("db_program_revalidate_calculations","Done!",as.numeric(Sys.time()-t2,"secs"))
  #   # nx <- dbExecute(conn,"
  #   #               delete from p_rsf.rsf_data_calculation_evaluations dce
  #   #               where exists(select * from  _temp_revalidations tr
  #   #                            inner join p_rsf.rsf_data_current rdc on rdc.data_id = tr.data_id -- really an exists check that calculated data_id is still current
  #   #                                                                                              -- and if not, keep in evaluations and will be re-calculated
  #   #                            where tr.rsf_pfcbl_id = dce.rsf_pfcbl_id
  #   #                              and tr.indicator_id = dce.indicator_id
  #   #                              and tr.reporting_asof_date = dce.calculation_asof_date) ")
  # 
  #   return (nx)
  # })

  
  if(SYS_PRINT_TIMING) debugtime("db_program_revalidate_calculations","Done!",as.numeric(Sys.time()-t1,"secs"))
  return (validated) 
}