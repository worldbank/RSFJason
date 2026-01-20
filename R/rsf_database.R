# library(yaml)
# library(pool)
# library(RPostgres)

RSF_MANAGEMENT_APPLICATION_ID <- "4608A309E2E38860DC98FAC53F967CF2"

account_credentials_rsf_sys_calculator <- yaml.load_file('./credentials/credentials-account-rsf-sys-calculator.yaml')
account_credentials_rsf_sys_admin <- yaml.load_file('./credentials/credentials-account-rsf-sys-admin.yaml')

dbStart <- function(credentials_file,
                    ob_name,
                    drv=DBI::dbDriver("Postgres"),
                    destination="Postgres") {
  
  credentials_list <- yaml.load_file(credentials_file)
  credentials_list$drv <- drv
  
  if (destination=="Dremio") {
    stop("Deprecated.  Dremio no longer uses a pool")
    # DREMIO_DRIVERS <- odbc::odbcListDrivers()
    # DREMIO_DRIVERS <- grep("dremio",DREMIO_DRIVERS$name,ignore.case = T,value=T)
    # credentials_list$DRIVER <- DREMIO_DRIVERS[[1]]
  }
  
  credentials_list$idleTimeout <- 30
  
  tryCatch({ 
    #R.utils::withTimeout({ do.call(dbPool, credentials_list)  },timeout=10,elapsed=10)
      { do.call(dbPool, credentials_list)  }
    },
           error=function(e) {
             warning(conditionMessage(e))
             NULL
           })
}



#
# dremioConnect <- function(credentials_file=paste0(getwd(),"/credentials/credentials-remote-dremio-arrow.yaml"),
#                           location=LOCATION) {


# dremioReturn <- function(pool=DBPOOL_DREMIO) {
#   
#   if (is.null(DBPOOL_DREMIO)) return()
#   tryCatch({ poolReturn(pool) },error=function(e) {})
# }

dremioConnect <- function(pool,
                          credentials_file=list(Dremio=paste0(getwd(),"/credentials/credentials-remote-dremio-direct.yaml"),
                                                Arrow=paste0(getwd(),"/credentials/credentials-remote-dremio-arrow.yaml"))) {
  
  # if (!is.null(DBPOOL_DREMIO) && any(pool::dbIsValid(DBPOOL_DREMIO)==TRUE,DBPOOL_DREMIO$valid)) {
  #   #cat("Retreiving Dremio Pool")
  #   return(DBPOOL_DREMIO)
  # }

  DREMIO_DRIVERS <- odbc::odbcListDrivers()

  if (any(grepl("Arrow Flight SQL ODBC Driver",DREMIO_DRIVERS$name,ignore.case = T))) {
    if (is.list(credentials_file)) credentials_file <- credentials_file$Arrow
    DREMIO_DRIVERS <- grep("Arrow Flight SQL ODBC Driver",DREMIO_DRIVERS$name,ignore.case = T,value=T)

  } else if (any(grepl("Dremio",DREMIO_DRIVERS$name,ignore.case = T))) {
    if (is.list(credentials_file)) credentials_file <- credentials_file$Dremio
    DREMIO_DRIVERS <- grep("Dremio",DREMIO_DRIVERS$name,ignore.case = T,value=T)
    
  } else {
    stop("Failed to recognized Dremio Driver")
  }
  
  login <- dbGetQuery(pool,"select uid,access_token from public.dremio")
  
  credentials_list <- yaml.load_file(credentials_file)
  credentials_list$UID <- login$uid
  credentials_list$PWD <- login$access_token
  credentials_list$DRIVER <- DREMIO_DRIVERS[[1]]
  credentials_list$drv <- odbc::odbc()
  credentials_list$idleTimeout <- 30
  credentials_list$CommandTimeout <- 600000
  
  tryCatch({ 
    { 
      do.call(dbPool, credentials_list)  
      #do.call(dbConnect, credentials_list)  
    }
  },
  error=function(e) {
    warning(conditionMessage(e))
    NULL
  })
  
  #DBPOOL_DREMIO <<- dremioConn
  #return(dremioConn)
}

dremioQuery <- function(sql,
                        dremio=DBPOOL_DREMIO) {
  
  #if (is.null(dremio) || !dbIsValid(dremio)) dremio <- 
  dbGetQuery(dremio,sql)
}




# dremioQuery <- function(sql,
#                         retry=0) {
#   
#   retry <- as.numeric(retry)
#   if (all(is.na(retry)) || length(retry) != 1) retry <- 0
#   if (retry > 3) {
#     stop(paste0("dremioQuery failed after 3 attempts for query: ",sql))
#   }
#   
#   conn <- tryCatch({
#     dremioConnect()
#   },
#   error=function(e) {
#     message(paste0("Dremio Connection error: ",conditionMessage(e)))
#     NULL
#   },
#   warning=function(w) {
#     message(paste0("Dremio Connection warning: ",conditionMessage(w)))
#     NULL
#   })
#   
#   if (is.null(conn) || !dbIsValid(conn)) {
#     message(paste0("Dremio Connection error retrying query: ",sql))
#     return(dremioQuery(sql,retry=retry+1))
#   }
#   
#   result <- tryCatch({
#     result<-dbGetQuery(conn,sql)
#     #poolReturn(conn)
#     dremioReturn()
#     #dbDisconnect(conn)
#     conn <- NULL
#     result
#   },
#   error=function(e) { 
#     if (dbIsValid(conn)) dremioReturn() #dbDisconnect(conn)
#     conn <- NULL
#     message(paste0("Error in dremioQuery '",sql,"': ",conditionMessage(e)))
#     return(dremioQuery(sql,retry=retry+1))
#   },
#   warning=function(w) {
#     if (dbIsValid(conn)) dremioReturn() #dbDisconnect(conn)
#     conn <- NULL
#     
#     message(paste0("Warning in dremioQuery '",sql,"': ",conditionMessage(w)))
#     return(dremioQuery(sql,retry=retry+1))
#     
#   })
#   return(result)
# }

#poolClose(DBPOOL); rm(DBPOOL); DBPOOL <- dbStart(credentials_file=paste0(getwd(),LOCATIONS[[LOCATION]]),ob_name="DBPOOL"); pool <- DBPOOL
# DBPOOL_GLOBAL <- dbStart(credentials_file=paste0(getwd(),LOCATIONS[[LOCATION]]))
# if (!is.null(DBPOOL_GLOBAL) && pool::dbIsValid(DBPOOL_GLOBAL)) { print("DBPOOL_GLOBAL (MAIN) Started")
# } else { print("DBPOOL_GLOBAL (MAIN) FAILED TO START") }

#DBPOOL_APPLICATIONS is required to manage login/logout


#ODBC doesn't work well with poolings
# DBPOOL_DREMIO <- dbStart(credentials_file=paste0(getwd(),"/credentials/credentials-remote-dremio.yaml"),
#                          drv=odbc::odbc(),
#                          destination="Dremio")
# if (!is.null(DBPOOL_DREMIO) && pool::dbIsValid(DBPOOL_DREMIO)) { print("DBPOOL (DREMIO) Started")
# } else { print("DBPOOL (DREMIO) FAILED TO START") }

#onStart:
{
  
  
  DBPOOL_APPLICATIONS <- dbStart(credentials_file=paste0(getwd(),LOCATIONS[["ARL"]]))
  if (!is.null(DBPOOL_APPLICATIONS) && pool::dbIsValid(DBPOOL_APPLICATIONS)) { print("DBPOOL (APPLICATIONS) Started")
  } else { print("DBPOOL (APPLICATIONS) FAILED TO START") }
  
  
  DBPOOL_DREMIO <- dremioConnect(pool=DBPOOL_APPLICATIONS)
  if (!is.null(DBPOOL_DREMIO) && pool::dbIsValid(DBPOOL_DREMIO)) { print("DBPOOL (DREMIO) Started")
  } else { print("DBPOOL (DREMIO) FAILED TO START") }
  
  ACCOUNT_SYS_ADMIN <- dbGetQuery(DBPOOL_APPLICATIONS,
    "select account_id,session_id
     from arlapplications.accounts_login($1::text,$2::text,$3::text)",
    params=list(RSF_MANAGEMENT_APPLICATION_ID,
                account_credentials_rsf_sys_admin$login,
                account_credentials_rsf_sys_admin$password))
  
  ACCOUNT_SYS_CALCULATOR <- dbGetQuery(DBPOOL_APPLICATIONS,
    "select account_id,session_id
     from arlapplications.accounts_login($1::text,$2::text,$3::text)",
         params=list(RSF_MANAGEMENT_APPLICATION_ID,
                     account_credentials_rsf_sys_calculator$login,
                     account_credentials_rsf_sys_calculator$password))
  
}


onStop(function() {
  
  for(i in seq_len(sink.number())) { suppressWarnings(sink(NULL)) }
  
  tryCatch({ 
    if (!is.null(DBPOOL_APPLICATIONS) && any(pool::dbIsValid(DBPOOL_APPLICATIONS)==TRUE,DBPOOL_APPLICATIONS$valid)) {
      print("Closing DBPOOL_APPLICATIONS")
      poolClose(DBPOOL_APPLICATIONS)
      DBPOOL_APPLICATIONS <<- NULL
    }
  },
  error=function(e) { print(conditionMessage(e)) },
  warning=function(w) { print(conditionMessage(w)) })
  
  tryCatch({ 
    if (!is.null(DBPOOL_DREMIO) && any(pool::dbIsValid(DBPOOL_DREMIO)==TRUE,DBPOOL_DREMIO$valid)) {
      print("Closing DBPOOL_DREMIO")
      poolClose(DBPOOL_DREMIO)
      DBPOOL_DREMIO <<- NULL
    }
  },
  error=function(e) { print(conditionMessage(e)) },
  warning=function(w) { print(conditionMessage(w)) })
  
})


