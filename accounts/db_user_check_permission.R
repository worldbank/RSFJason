
db_user_check_permission <- function(pool,
                                     application_hashid, 
                                     email, 
                                     permission_name) {
  
  lookup <- dbGetQuery(pool,"select login_email from arlapplications.check_permission($1, $2, $3);",
                       params=list(email, application_hashid, permission_name))
  return (lookup)
}
