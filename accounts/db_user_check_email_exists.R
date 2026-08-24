db_user_check_email_exists <- function(pool,application_hashid,email) {
  lookup <- dbGetQuery(pool,"select users_email
                       from arlapplications.account_info 
                       where login_name = NULLIF(trim(lower(regexp_replace(split_part($1,'@', 1),'[^[:alnum:]_\\.]','','gi'))),'')",
                       
                       
                       params=list(email))
  return (lookup$users_email)
}