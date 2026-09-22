# library(yaml)
# library(pool)
# library(RPostgres)

dbCredentialsLoad <- function(database) {
  # 
  # _file=list(Dremio=paste0(getwd(),"/credentials/credentials-remote-dremio-direct.yaml"),
  #            Arrow=paste0(getwd(),"/credentials/credentials-remote-dremio-arrow.yaml"))
  # 
  # 
  # LOCATIONS <- list(ARL="/credentials/credentials-remote-ARL.yaml",
  #                   #SSA_DEV="/credentials/credentials-remote-dev.yaml",
  #                   #SSA_PROD="/credentials/credentials-remote-RSF.yaml",
  #                   #Jason_DEV_Shafique="/credentials/credentials-rsfjson-rsfshafique.yaml",
  #                   Jason_DEV="/credentials/credentials-rsfjson-rsfdev.yaml",
  #                   Jason_PROD="/credentials/credentials-rsfjson-rsfprod.yaml",
  #                   Jason_STAGE="/credentials/credentials-rsfjson-rsfstaging.yaml")
  # 
  database <- toupper(database)
  if (database=="DREMIO") {
    yaml::yaml.load_file(here::here("credentials","credentials-remote-dremio-arrow.yaml"))
  } else if (database=="JASON_DEV") {
    yaml::yaml.load_file(here::here("credentials","credentials-rsfjson-rsfdev.yaml"))
  } else if (database=="JASON_PROD") {
    yaml::yaml.load_file(here::here("credentials","credentials-rsfjson-rsfprod.yaml"))
  } else if (database=="ARL") {
    yaml::yaml.load_file(here::here("credentials","credentials-remote-ARL.yaml"))
  } else {
    stop(paste0("Database not recognized: ",database))
  }
    
}

dbMakeIntArray <- function(x) {
  
  x <- as.integer(suppressWarnings(unique(x)))
  x <- x[!is.na(x)]
  
  if (!length(x)) return("{}")
  
  paste0("{", paste0(x, collapse = ","), "}")
}


dbStart <- function(database,
                    drv=DBI::dbDriver("Postgres")) {
  
  credentials <- dbCredentialsLoad(database=database)
  credentials$drv <- drv
  credentials$idleTimeout <- 30
  
  dbpool <- tryCatch({ 
    #R.utils::withTimeout({ do.call(dbPool, credentials_list)  },timeout=10,elapsed=10)
      { do.call(dbPool, credentials)  }
    },
           error=function(e) {
             warning(conditionMessage(e))
             NULL
           })
  
  
}

dremioConnect <- function() {
  
  DREMIO_DRIVERS <- odbc::odbcListDrivers()
  credentials <- dbCredentialsLoad(database="Dremio")
  
  if (any(grepl("Arrow Flight SQL ODBC Driver",DREMIO_DRIVERS$name,ignore.case = T))) {
    #if (is.list(credentials)) credentials <- credentials$Arrow
    DREMIO_DRIVERS <- grep("Arrow Flight SQL ODBC Driver",DREMIO_DRIVERS$name,ignore.case = T,value=T)
    cat(paste0("Selected Driver: Arrow Flight SQL ODBC Driver"))
    
  # } else if (any(grepl("Dremio",DREMIO_DRIVERS$name,ignore.case = T))) {
  #   if (is.list(credentials)) credentials <- credentials$Dremio
  #   DREMIO_DRIVERS <- grep("Dremio",DREMIO_DRIVERS$name,ignore.case = T,value=T)
  #   cat(paste0("Selected Driver: Dremio Proprietary ODBC Driver"))
  }else {
    stop("Failed to recognized Dremio Driver")
  }
  
  ARL_pool <- dbStart(database="ARL")
  ARL_login <- dbGetQuery(ARL_pool,"select uid,access_token from public.dremio")
  poolClose(ARL_pool)
  

  credentials$UID <- ARL_login$uid
  credentials$PWD <- ARL_login$access_token
  credentials$DRIVER <- DREMIO_DRIVERS[[1]]
  credentials$drv <- odbc::odbc()
  credentials$idleTimeout <- 30
  credentials$CommandTimeout <- 600000
  credentials$dbms.name <- "Dremio"
  
  tryCatch({ 
    { 
      #print("Trying:  ")
      #print(credentials_list)
      do.call(dbPool, credentials)  
      #do.call(dbConnect, credentials_list)  
    }
  },
  error=function(e) {
    
    if (grepl("Failed to authenticate",conditionMessage(e),ignore.case = T)) {
      warning(paste0("DREMIO Failed to Authenticate Error: This is likely due to an expired Dremio Access Token for user ",login$uid," as dremio acess tokens are valid for only 1-year and must be renewed.\n",
                     "(1) Generate a new Dremio Access Token with access to FX rates view
                      (2) Update PostgreSQL database table: ARL.public.dremio with the new token\n",
                     conditionMessage(e)))
    } else {
      warning(conditionMessage(e))
    }
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




