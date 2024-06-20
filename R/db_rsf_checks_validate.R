db_rsf_checks_validate <- function(pool,
                                   data_checks) {
  
  if (empty(data_checks)) return (NULL)
  if (!all(names(data_checks) %in% c("rsf_pfcbl_id","for_indicator_id","indicator_check_id","check_asof_date"))) stop("data_checks requires columns data_id,check_asof_date,indicator_check_id")
  
  data_checks <- unique(data_checks)
  
  #conn <- poolCheckout(pool)
  #dbBegin(conn);
  #dbRollback(conn);
  #poolReturn(conn)
  poolWithTransaction(pool,function(conn) {  
    dbExecute(conn,"create TEMP table _temp_validations(rsf_pfcbl_id int,
                                                        for_indicator_id int,
                                                        check_asof_date date,
                                                        indicator_check_id int)
              on commit drop;")
    
    dbAppendTable(conn,
                  name="_temp_validations",
                  value=data_checks)
   
    dbExecute(conn,"
              update p_rsf.rsf_data_check_evaluations dce
              set checked_status = true
              from _temp_validations tv  
              where dce.rsf_pfcbl_id = tv.rsf_pfcbl_id
                and dce.indicator_id = tv.for_indicator_id
                and dce.check_asof_date = tv.check_asof_date
                and dce.indicator_check_id = tv.indicator_check_id")
    
  })
}