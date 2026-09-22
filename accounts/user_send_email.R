
user_send_email <- function(arl_pool,
                            from="\"RSF JASON\"<noreply@positconnect.int.worldbank.org>",
                            to,
                            subject,
                            html) {
  
  #to may be an actual email address or a user login_name and we'll lookup the email address actually tied to the account here.
  lookup_email <- db_user_check_email_exists(pool=arl_pool,
                                             RSF_MANAGEMENT_APPLICATION_ID,
                                             to)
  
  #to <- "sheitmann@ifc.org"
  
  if (!length(lookup_email)) stop(paste0("Invalid email address: ",to))
  if (length(lookup_email) != 1) stop("Only one receipient may be defined")
  
  print(paste0("user_send_email : emailing ",lookup_email," from ",from," subject: ",subject))
  html <- as.character(html)
  html <- gsub(">[[:space:]]+","> ",html) #Pandoc won't format html when line breaks exist; and it will crash when not having some spaces.
  
  page <- paste0("<html>
          <head><title>",subject,"</title></head>
          <body style='font-family:Verdana;'>
            <section id='emailbody'>",html,"</section>
          </body>
        </html>")
  
  msg <- page
  msg <- mime_part(msg)
  msg[["headers"]][["Content-Type"]] <- "text/html"
  
  tryCatch({
    sendmail(from=from,
             to=lookup_email,
             subject=subject,
             msg=msg,
             control=list(smtpServer="lmail.worldbank.org"))
  },
  error=function(e) {
    print(conditionMessage(e))
  },
  warning=function(w) {
    print(conditionMessage(w))
  })
}

