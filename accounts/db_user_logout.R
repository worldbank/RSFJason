db_user_logout <- function(pool,application_hashid,session_id) {
  
  if (is.null(application_hashid) || length(application_hashid) != 1) application_hashid <- NA
  if (is.null(session_id) || length(session_id) != 1) session_id <- NA
  
  status <- dbGetQuery(pool,"
                           select * from arlapplications.accounts_logout(NULLIF($1::text,'NA'),NULLIF($2::text,'NA'))",
                             params=list(paste0(application_hashid),paste0(session_id)))
  
  status <- unlist(status)
  if (all(is.na(status))) return (FALSE)
  else return (status)
}