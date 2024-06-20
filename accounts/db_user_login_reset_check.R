db_user_login_reset_check <- function(pool,application_hashid,username) {
  print(paste0("db_user_login_reset_check application_hashid=",application_hashid," username=",username))
  reset <- dbGetQuery(pool,"
                       select * from arlapplications.accounts_check_reset($1::text,$2::text)",
                      params=list(application_hashid,username))
  reset <- as.data.table(reset)
  return (reset)
}