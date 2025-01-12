db_user_reset_password <- function(pool,
                                   application_hashid,
                                   sysadmin_id,
                                   username) {
  
  print(paste0("db_user_reset_password application_hashid=",application_hashid," sysadmin_id=",sysadmin_id," username=",username))
  reset <- dbGetQuery(pool,"
                      select 
                        arp.account_id,
                        arp.login_email,
                        arp.reset_password,
                        arp.reset_password_time,
                        arp.reset_success,
                        ai.users_name
                      from arlapplications.accounts_reset_password($1::text,$2::text,$3::text) arp
                      inner join arlapplications.view_account_info ai on ai.account_id = arp.account_id",
                      params=list(application_hashid,
                                  sysadmin_id,
                                  username))
  setDT(reset)
  return (reset)
}