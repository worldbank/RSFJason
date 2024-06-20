db_data_update_flags <- function(pool,
                                 user_id,
                                 flags) {

  #browser()
  # test <<- as.data.frame(flags)
  # user_id <<- user_id
  # stop("Test")
  # flags <- as.data.table(test)
  t1 <- Sys.time()
  
  valid_user <- dbGetQuery(pool,"
                           select exists(select * from p_rsf.view_account_info where account_id = $1::text)::bool as valid
                           ",
                           params=list(user_id))
  
  if (unlist(valid_user)==FALSE) {
    stop(paste0("Invalid user: ",user_id))
  }
  
  if (!all(unique(flags$check_status_updated) %in% c('resolved','active'))) {
    stop("Only valid check_status values are: resolved, active")
  }
  
  if (any(is.na(flags$check_status_comment_updated))) {
    stop("A comment is required when updating a check status (including reverting to active status)")
  }    
  
  flags <- flags[!(is.na(check_status_comment_updated) | is.na(check_status_updated))]
  flags <- flags[check_class != "critical"]

  if (empty(flags)) return (FALSE)
  
  flags <- flags[,
                 .(evaluation_id,
                    check_status_updated,
                    check_status_comment_updated,
                    check_status_user_id=user_id)]
  
  #conn <- poolCheckout(pool)
  #dbBegin(conn)
  #dbRollback(conn)

  affected_flags <- poolWithTransaction(pool,function(conn) {
    
    
    dbExecute(conn,"create TEMP table _temp_update_flags(evaluation_id int,
                                                         check_status_updated text,
                                                         check_status_comment_updated text,
                                                         check_status_user_id text)
                    on commit DROP;")

    dbAppendTable(conn,
                  name="_temp_update_flags",
                  value=flags)
    
   
    #This will resolve other related checks that aren't explicitly ignored (yet)
    dbExecute(conn,"update p_rsf.rsf_data_checks rdc
                    set 
                      check_status = taf.check_status_updated,
                      check_status_comment = taf.check_status_comment_updated,
                      check_status_user_id = taf.check_status_user_id,
                      status_time = timeofday()::timestamptz
                    from _temp_update_flags taf
                    where taf.evaluation_id = rdc.evaluation_id
                      and (taf.check_status_updated is distinct from rdc.check_status
                           or
                           taf.check_status_comment_updated is distinct from rdc.check_status_comment)")
  
    TRUE
  })
  if(SYS_PRINT_TIMING) debugtime("db_data_update_flags","Done!",as.numeric(Sys.time()-t1,"secs"))
  
  return (affected_flags)
}