{
  library(DBI)
  library(purrr)
  library(lubridate)
  library(stringr)
  library(data.table)
  library(jsonlite)
  library(tools)
  library(plyr)
  library(glue)
  library(R.utils)
  library(digest)
  library(rlang)
  library(odbc)
  library(RPostgres)
  library(pool)
  library(yaml)
  library(sendmailR)
  library(english)
  
  back <- ".."
  source(here::here(back,"R","rsf_database.R")) #First
  source(here::here(back,"R","rsf_calculations_environment.R")) #Second
  
  source(here::here(back,"R","db_rsf_checks_validate.R"))
  
  source(here::here(back,"R","db_program_get_data.R"))
  source(here::here(back,"R","rsf_computation_fx_conversion.R"))
  source(here::here(back,"R","rsf_program_calculate.R"))
  source(here::here(back,"R","rsf_program_check.R"))
  source(here::here(back,"R","rsf_program_perform_calculations.R"))
  source(here::here(back,"R","rsf_program_perform_checks.R"))
  source(here::here(back,"R","db_program_get_stale_calculations.R"))
  source(here::here(back,"R","db_program_get_stale_checks.R"))
  source(here::here(back,"R","db_indicators_get_labels.R"))
  source(here::here(back,"R","db_indicators_get_calculation_parameter_rsf_pfcbl_ids.R"))
  source(here::here(back,"R","db_checks_get_calculation_parameter_rsf_pfcbl_ids.R"))
  source(here::here(back,"R","rsf_checks_calculate.R"))
  source(here::here(back,"R","rsf_indicators_calculate.R"))
  source(here::here(back,"R","rsf_calculations_resolve_parameters.R"))
  source(here::here(back,"R","rsf_checks_resolve_parameters.R"))
  source(here::here(back,"R","global_formatting_functions.R"))
  source(here::here(back,"R","db_add_update_data_system.R"))
  source(here::here(back,"R","db_data_get_fx_ratio.R"))  
  source(here::here(back,"R","db_add_update_checks.R"))  
  source(here::here(back,"accounts","db_user_login.R"))
  
  DATABASE_LOCATION <- "Jason_DEV"
  debugtime <- function(...) { }
}


