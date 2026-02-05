db_user_login <- function(pool,application_hashid,username,password) {
  
  if (is.null(application_hashid) || length(application_hashid) != 1) application_hashid <- NA
  if (is.null(username) || length(username) != 1) username <- NA
  if (is.null(password) || length(password) != 1) password <- NA
  
  user_account <- dbGetQuery(pool,"
                           select
                           al.session_id,
                           aa.application_permissions,
                           ai.users_name,
                           al.account_id
                           from arlapplications.accounts_login(NULLIF($1::text,'NA'),
                                                               NULLIF($2::text,'NA'),
                                                               NULLIF($3::text,'NA')) al
                           inner join arlapplications.account_applications aa on aa.account_id = al.account_id
                           inner join arlapplications.view_account_info ai on ai.account_id = al.account_id
                           where aa.application_hashid=NULLIF($1::text,'NA')",
                             params=list(paste0(application_hashid),
                                         paste0(username),
                                         paste0(password)))
  if (empty(user_account)) return (NULL)
  user_account$user_login <- username
  user_account <- as.data.table(user_account)
  return (user_account)
}