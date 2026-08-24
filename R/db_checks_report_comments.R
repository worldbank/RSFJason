db_checks_report_comments <- function(pool,
                                      current_flags,
                                      reporting_user_id) {
  
  if (!all(c("evaluation_id","status","ifc_comments","client_comments") %in% names(current_flags))) {
    stop(paste0("current_flags should define evaluation_id,status,ifc_comments,client_comments and is missing: ",
                paste0(setdiff(c("evaluation_id","status","ifc_comments","client_comments"),names(current_flags)),collapse=", ")))
  }
  
  # conn <- poolCheckout(pool)
  # dbBegin(conn)
  # dbRollback(conn)
  notfound <- poolWithTransaction(pool,function(conn) {
    
    dbExecute(conn,"create temp table _temp_flags(evaluation_id int,
                                                            status text,
                                                            ifc_comments text,
                                                            client_comments text)
                              on commit drop")
    
    dbAppendTable(conn,
                  name="_temp_flags",
                  value=current_flags[,.(evaluation_id,
                                         status,
                                         ifc_comments,
                                         client_comments)])
    
    dbExecute(conn,"
                delete from _temp_flags tf
                using p_rsf.rsf_data_checks rdc
                where rdc.evaluation_id = tf.evaluation_id 
                  and NULLIF(rdc.check_status_comment,'') is not distinct from NULLIF(tf.ifc_comments,'')
                  and NULLIF(rdc.check_reporting_comment,'') is not distinct from NULLIF(client_comments,'')
                  and rdc.check_status is not distinct from tf.status")
    
    dbExecute(conn,"
                
                update p_rsf.rsf_data_checks rdc
                set check_status = tf.status,
                    check_status_comment = tf.ifc_comments,
                    check_status_user_id = $1::text
                from _temp_flags tf
                where tf.evaluation_id = rdc.evaluation_id
                  and NULLIF(tf.ifc_comments,'') is not null
                  and tf.ifc_comments is distinct from rdc.check_status_comment",
              params=list(reporting_user_id))
    
    dbExecute(conn,"
                update p_rsf.rsf_data_checks rdc
                set check_reporting_comment = tf.client_comments
                from _temp_flags tf
                where tf.evaluation_id = rdc.evaluation_id
                  and NULLIF(tf.client_comments,'') is not null
                  and tf.client_comments is distinct from rdc.check_reporting_comment")
    
    dbExecute(conn,"
                update p_rsf.rsf_data_checks_archive dca
                set check_status = tf.status,
                    check_status_comment = tf.ifc_comments,
                    check_status_user_id = $1::text
                from _temp_flags tf
                where tf.evaluation_id = dca.archive_id
                  and NULLIF(tf.ifc_comments,'') is not null
                  and tf.ifc_comments is distinct from dca.check_status_comment",
              params=list(reporting_user_id))
    
    dbExecute(conn,"
                update p_rsf.rsf_data_checks_archive dca
                set check_reporting_comment = tf.client_comments
                from _temp_flags tf
                where tf.evaluation_id = dca.archive_id
                  and NULLIF(tf.client_comments,'') is not null
                  and tf.client_comments is distinct from dca.check_reporting_comment")
    
    unlist(dbGetQuery(conn,"
                select tf.evaluation_id
                from _temp_flags tf
                where not exists(select true from p_rsf.rsf_data_checks rdc
                                 where rdc.evaluation_id = tf.evaluation_id)
                  and not exists(select true from p_rsf.rsf_data_checks_archive dca
                                 where dca.archive_id = tf.evaluation_id)
              "),use.names=F)
    
  })
  
  notfound
}