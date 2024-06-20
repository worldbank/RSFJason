db_user_check_email_exists <- function(pool,application_hashid,email) {
  lookup <- dbGetQuery(pool,"select * from arlapplications.account_info where users_email = $1",params=list(email))
  return (lookup)
}