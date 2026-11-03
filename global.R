
#DATABASE_LOCATION <- "Jason_DEV"
DATABASE_LOCATION <- "Jason_PROD"


options(shiny.reactlog=TRUE) #press Ctrl+F3 to launch the reactive log visualization
options(shiny.autoload.r = FALSE)
options(shiny.autoreload = FALSE)
options(stringsAsFactors = FALSE)
options(scipen=999)
options(warning.length=8170)
#options(shiny.maxRequestSize=30*1024^2)
options(shiny.maxRequestSize=500*1024^2) #500MB
 
#options(warn=2, error=recover)
#options(warn=0, error=recover)
options(warn=0, error=NULL)
#Sys.setlocale("LC_ALL",c("English"))



#LOCATION <- "Jason_DEV_Shafique"

#LOCATION <- "Jason_STAGE"

if (grepl("DEV",DATABASE_LOCATION)==TRUE) {
  #options(shiny.erctror = browser)
  options(shiny.error = NULL)
}



library(purrr)
library(pdftools)
library(lubridate)
library(stringr)
library(data.table)

library(shiny)
library(shinydashboard)
library(shinybusy)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(bslib)
library(DT)
library(jsonlite)
library(tools)
library(plyr)
library(glue)
library(R.utils)
library(digest)

library(openxlsx)
library(openxlsx2)
#library(reshape2)
library(rlang)
library(odbc)
library(RPostgres)
library(pool)
library(yaml)
library(sendmailR)
library(english)

source("./R/rsf_database.R") #First


source("./R/openxlsx_helper_functions.R")

source('./accounts/module_accounts_server.R')
source('./accounts/ui_objects.R')
source('./accounts/db_user_login_change_password.R')
source('./accounts/db_user_login_reset_check.R')
source('./accounts/db_user_check_email_exists.R')
source('./accounts/db_user_reset_password.R')
source('./accounts/db_user_check_permission.R')
source('./accounts/db_user_login.R')
source('./accounts/db_user_logout.R')
source('./accounts/user_send_email.R')

#source("./R/reports/export_reporting_cohort_to_excel.R")

source("./R/db_create_new_rsf_ids.R")
source("./R/db_create_entity_indicator_requirements.R")
source("./R/db_checks_report_comments.R")
#source("./R/db_data_get_info.R")
#source("./R/db_data_get_flags.R")
source("./R/db_data_update_flags.R")
source("./R/db_data_get_fx_ratio.R")
source("./R/db_data_get_current.R")
source("./R/db_data_pivot_family.R")

#source("./R/db_cohort_get_data.R")

#source("./R/db_cohort_create.R")
#source("./R/db_import_upload_file.R")
source("./R/db_import_download_file.R")
source("./R/db_export_template_download_file.R")

source("./R/db_reporting_import_create.R")

source("./R/db_rsf_reports_list.R")

source("./R/db_export_create.R")
source("./R/db_export_get_template.R")
source("./R/db_dashboard_load_report.R")
source("./R/rsf_setup_export_create_entity.R")

source("./R/db_add_update_data_user.R")
source("./R/db_add_update_data_system.R")
source("./R/db_add_update_checks.R")
source("./R/db_rsf_checks_validate.R")

source("./R/db_program_get_data.R")
source("./R/db_program_create.R")
source("./R/db_program_download.R")
source("./R/db_program_toggle_indicator_subscription.R")
source("./R/db_program_toggle_check_subscription.R")

source("./R/rsf_computation_fx_conversion.R")
source("./R/rsf_program_calculate.R")
source("./R/rsf_program_check.R")
source("./R/rsf_program_perform_calculations.R")
source("./R/rsf_program_perform_checks.R")
source("./R/db_program_get_stale_calculations.R")
source("./R/db_program_get_stale_checks.R")
#source("./R/db_program_facility_checks_add_update_guidance.R")
source("./R/db_indicators_get_labels.R")
source("./R/db_indicators_get_header_actions.R")
source("./R/db_indicator_update.R")
source("./R/db_indicator_create.R")
source("./R/db_indicator_delete.R")
source("./R/db_indicators_get_formulas.R")
source("./R/db_indicators_get_calculation_parameter_rsf_pfcbl_ids.R")
source("./R/db_checks_get_calculation_parameter_rsf_pfcbl_ids.R")

source("./R/rsf_indicators_calculate_do_test.R")
source("./R/rsf_calculations_recalculate.R")


source("./R/db_check_create.R")
source("./R/db_check_update.R")
source("./R/db_checks_get_formulas.R")

source("./R/db_rsf_options_create.R")
source("./R/db_rsf_options_update.R")
source("./R/db_rsf_options_delete.R")


source("./R/db_rsf_get_options_groups.R")

source("./R/rsf_checks_calculate.R")
source("./R/rsf_checks_do_test.R")
source("./R/rsf_indicators_calculate.R")

#source("./R/reports/export_rsf_setup_files_to_excel.R")
source("./R/rsf_reports_data_integrity_key.R")
source("./R/rsf_reports_create_excel_sheet.R")
source("./R/rsf_reports_excel_read_rsf_data.R")

#source("./R/db_rsf_get_pfcbl_from_sys_ids.R")
source("./R/db_get_rsf_pfcbl_id_by_sys_name.R")
source("./R/global_formatting_functions.R")
source("./R/rsf_calculations_resolve_parameters.R")
source("./R/rsf_checks_resolve_parameters.R")
source('./R/templates/template_excel_read_sheet.R')
source('./R/templates/template_set_data_match_rsf_ids.R')
source('./R/templates/template_set_data_match_pfcbl_ids.R')
source("./R/templates/template_set_redundancy_reporting.R")
source("./R/templates/template_match_labels.R")

source('./R/templates/template_upload.R')
source('./R/templates/template_process.R')

source("./R/reports/export_dashboard_view_to_excel.R")
source("./R/reports/export_dashboard_edits_to_excel.R")
source("./R/reports/export_backup_data_to_csv.R")
source("./R/reports/export_create_entity_to_excel.R")

source('./R/templates/parse_data_formats.R')
source('./R/templates/parse_text_to_paragraphs.R')

source('./R/templates/parse_template_labels.R')

source('./R/templates/template_parse_file.R')
source('./R/templates/template_parse_process_and_upload.R')

source('./R/templates/parse_template_IFC_QR2018.R')
source('./R/templates/parse_template_IFC_QR2025.R')

source('./R/templates/parse_template_RSA.R')
source('./R/templates/parse_template_csv.R')
#source('./R/templates/parse_template_rsf_setup.R')
source('./R/templates/parse_template_csv_backup_data.R')


source('./R/templates/parse_template_pfcbl_editor_report.R')
source('./R/templates/parse_template_rsf_create_entities.R')

source("./R/rsf_calculations_environment.R") #LAST

# #######################


DASH_DISPLAY_BLUE_C <- "35,150,240,0.2"
DASH_DISPLAY_BLUE_CH <- "35,150,240,0.4"
DASH_DISPLAY_BLUE_HH <- "35,150,240,0.05"

DASH_DISPLAY_YELLOW_C <- "255,235,200,0.05"
DASH_DISPLAY_YELLOW_CH <- "255,235,200,0.35"
DASH_DISPLAY_YELLOW_HH <- "255,235,200,0.025"

status_message <- function(...) { 
  intercept <- dynGet("intercept_status_message",inherits=TRUE,ifnotfound = NULL)
  if (!is.null(intercept)) exec(intercept,...)
  cat(...)
  #else cat("intercept_status_message not found\n",...) 
}

# debug_count <- 0
# debug_time_running <- Sys.time()
# debug_time_now <- Sys.time()
# debug_msg <- c()

debugtime <- function(...) { }

if (grepl("DEV",DATABASE_LOCATION)==TRUE) {
  devmode()
  debugtime <- function(...,reset=FALSE) {
    
    args <- unlist(list(...))
    msg <- paste0(args,collapse=" ")
    
    cat(msg,"\n")
  }
  
} else {
  debugtime <- function(...) { }
}

#start the databases and assign to calling environment

DBPOOL_APPLICATIONS <- dbStart(database="ARL")

if (!is.null(DBPOOL_APPLICATIONS) && pool::dbIsValid(DBPOOL_APPLICATIONS)) { print("DBPOOL (APPLICATIONS) Started")
} else { print("DBPOOL (APPLICATIONS) FAILED TO START") }


DBPOOL_DREMIO <- dremioConnect()
if (!is.null(DBPOOL_DREMIO) && pool::dbIsValid(DBPOOL_DREMIO)) { print("DBPOOL (DREMIO) Started")
} else { print("DBPOOL (DREMIO) FAILED TO START") }

RSF_MANAGEMENT_APPLICATION_ID <- "4608A309E2E38860DC98FAC53F967CF2"
ACCOUNT_SYS_ADMIN <- do.call(db_user_login,
                             append(list(pool=DBPOOL_APPLICATIONS,
                                         application_hashid=RSF_MANAGEMENT_APPLICATION_ID),
                                    yaml.load_file(here::here("credentials","credentials-account-rsf-sys-admin.yaml"))))

ACCOUNT_SYS_CALCULATOR <- do.call(db_user_login,
                             append(list(pool=DBPOOL_APPLICATIONS,
                                         application_hashid=RSF_MANAGEMENT_APPLICATION_ID),
                                    yaml.load_file(here::here("credentials","credentials-account-rsf-sys-calculator.yaml"))))


CALCULATIONS_ENVIRONMENT <- rsf_calculations_initialize_enivonment(dremio_database_pool=DBPOOL_DREMIO)

onStop(function() {
  
  for(i in seq_len(sink.number())) { suppressWarnings(sink(NULL)) }
  
  tryCatch({ 
    if (!is.null(DBPOOL_APPLICATIONS) && 
        any(pool::dbIsValid(DBPOOL_APPLICATIONS)==TRUE,DBPOOL_APPLICATIONS$valid)) {
      print("Closing DBPOOL_APPLICATIONS")
      poolClose(DBPOOL_APPLICATIONS)
      DBPOOL_APPLICATIONS <<- NULL
    }
    
    CALCULATIONS_ENVIRONMENT$closeConnections();
    CALCULATIONS_ENVIRONMENT <<- NULL
  },
  error=function(e) { print(conditionMessage(e)) },
  warning=function(w) { print(conditionMessage(w)) })
  
})

