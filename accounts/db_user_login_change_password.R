db_user_login_change_password <- function(pool,
                                          application_hashid,
                                          request_by_account_id,
                                          username,
                                          old_password,
                                          new_password) {
  
  print(paste0("db_user_login_change_password application_hashid=",application_hashid," username=",username," old_password=",old_password," new_password=",new_password))

  user_account_id <- dbGetQuery(pool,
                                "select * 
                                from arlapplications.accounts_change_password(NULLIF($1::text,'NA'),
                                                                              NULLIF($2::text,'NA'),
                                                                              NULLIF($3::text,'NA'),
                                                                              NULLIF($4::text,'NA'),
                                                                              NULLIF($5::text,'NA'))",
                                params=list(paste0(application_hashid),
                                            paste0(request_by_account_id),
                                            paste0(username),
                                            paste0(old_password),
                                            paste0(new_password)))
  
  user_account_id <- unlist(user_account_id)
  return (user_account_id)
}
