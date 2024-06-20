db_rsf_checks_remove_resolve <- function(pool,
                                         evaluation_ids) {

stop("deprecated")
  dbExecute(pool,"update p_rsf.rsf_data_checks rdc
                 set check_status = 'resolved',
                     check_status_comment = concat('Resolved by System: flag no longer exists following data correction'),
                     check_status_user_id = (select account_id from p_rsf.view_account_info where is_system_account = true and users_name = 'RSF SYS Calculator'),
                     status_time = TIMEOFDAY()::timestamptz
                 where rdc.check_status = 'active'
                   and rdc.evaluation_id = any(select unnest(string_to_array($1::text,','))::int)",
            params=list(paste0(unique(evaluation_ids),collapse=",")))
  
  dbExecute(pool,"
            update p_rsf.rsf_data_check_evaluations dce
               set checked_status = true
              from p_rsf.rsf_data_checks rdc
              where dce.rsf_pfcbl_id = rdc.rsf_pfcbl_id
                and dce.indicator_id = rdc.indicator_id
                and dce.check_asof_date = rdc.check_asof_date
                and dce.indicator_check_id = rdc.indicator_check_id
                and rdc.evaluation_id = any(select unnest(string_to_array($1::text,','))::int)
                and rdc.check_data_id_is_current = true",
            params=list(paste0(unique(evaluation_ids),collapse=",")))
}