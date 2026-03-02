#shiny::runApp(display.mode="showcase")
options(shiny.reactlog=TRUE) #press Ctrl+F3 to launch the reactive log visualization
options(shiny.autoload.r = FALSE)
options(shiny.autoreload = FALSE)
options(stringsAsFactors = FALSE)
options(scipen=999)
options(warning.length=8170)
#options(shiny.maxRequestSize=30*1024^2)
options(shiny.maxRequestSize=500*1024^2) #500MB
 
#options(warn=2, error=recover)
options(warn=0, error=NULL)
#Sys.setlocale("LC_ALL",c("English"))

  
LOCATIONS <- list(ARL="/credentials/credentials-remote-ARL.yaml",
                  #SSA_DEV="/credentials/credentials-remote-dev.yaml",
                  #SSA_PROD="/credentials/credentials-remote-RSF.yaml",
                  #Jason_DEV_Shafique="/credentials/credentials-rsfjson-rsfshafique.yaml",
                  Jason_DEV="/credentials/credentials-rsfjson-rsfdev.yaml",
                  Jason_PROD="/credentials/credentials-rsfjson-rsfprod.yaml",
                  Jason_STAGE="/credentials/credentials-rsfjson-rsfstaging.yaml")

#LOCATION <- "Jason_DEV_Shafique"

#LOCATION <- "Jason_STAGE"

LOCATION <- "Jason_DEV"
#LOCATION <- "Jason_PROD"

if (grepl("DEV",LOCATION)==TRUE) {
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
#https://appsilon.com/forget-about-excel-use-r-shiny-packages-instead/?nabe=4634331497365504:1


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
source("./R/rsf_calculations_environment.R") #Second


source("./R/openxlsx_get_formulas.R")

source('./accounts/module_accounts_server.R')
source('./accounts/ui_objects.R')
source('./accounts/db_user_login_change_password.R')
source('./accounts/db_user_login_reset_check.R')
source('./accounts/db_user_check_email_exists.R')
source('./accounts/db_user_reset_password.R')
source('./accounts/db_user_check_permission.R')
source('./accounts/db_user_login.R')
source('./accounts/db_user_logout.R')

source("./R/reports/export_reporting_cohort_to_excel.R")

source("./R/db_create_new_rsf_ids.R")
source("./R/db_create_entity_indicator_requirements.R")

source("./R/db_data_get_info.R")
#source("./R/db_data_get_flags.R")
source("./R/db_data_update_flags.R")
source("./R/db_data_get_fx_ratio.R")
source("./R/db_data_get_current.R")
source("./R/db_data_pivot_family.R")

source("./R/db_cohort_get_data.R")

#source("./R/db_cohort_create.R")
#source("./R/db_import_upload_file.R")
source("./R/db_import_download_file.R")
source("./R/db_export_template_download_file.R")

source("./R/db_reporting_import_create.R")

source("./R/db_rsf_reports_list.R")

source("./R/db_export_create.R")
source("./R/db_export_get_template.R")
source("./R/db_export_load_report.R")
source("./R/rsf_setup_export_create_entity.R")

source("./R/db_add_update_data_user.R")
source("./R/db_add_update_data_system.R")
source("./R/db_rsf_checks_add_update.R")
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

source("./R/reports/export_rsf_setup_files_to_excel.R")
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



source("./R/templates/template_remove_data_category_misalignments.R")

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
source('./R/templates/parse_template_rsf_setup.R')
source('./R/templates/parse_template_csv_backup_data.R')


source('./R/templates/parse_template_pfcbl_editor_report.R')
source('./R/templates/parse_template_rsf_create_entities.R')

# ########################
# source("./R/app/server_dashboard.R",local=serverENV)
# source("./R/app/server_dashboard_options.R",local=serverENV)
# source("./R/app/server_dashboard_reports.R",local=serverENV)
# source("./R/app/server_dashboard_edit.R",local=serverENV)
# source("./R/app/server_dashboard_exports.R",local=serverENV)
# source("./R/app/server_dashboard_exports_reports.R",local=serverENV)
# source("./R/app/server_datasets.R",local=serverENV)
# source("./R/app/server_datasets_review_flags.R",local=serverENV)
# 
# source("./R/app/server_datasets_upload.R",local=serverENV)
# 
# #When permissions are implemented, only load administrateive modules if an admin
# source("./R/app/server_setup.R",local=serverENV)
# source("./R/app/server_setup_program.R",local=serverENV)
# source("./R/app/server_setup_indicators.R",local=serverENV)
# source("./R/app/server_setup_checks.R",local=serverENV)
# source("./R/app/server_setup_templates.R",local=serverENV)
# source("./R/app/server_setup_create.R",local=serverENV)
# 
# source("./R/app/server_admin_options.R",local=serverENV)
# source("./R/app/server_admin_options_module.R",local=serverENV)
# 
# source("./R/app/server_admin_indicators.R",local=serverENV)
# source("./R/app/server_admin_indicator_formulas.R",local=serverENV)
# source("./R/app/server_admin_indicators_review.R",local=serverENV)
# 
# source("./R/app/server_admin_checks.R",local=serverENV)
# source("./R/app/server_admin_checks_formulas.R",local=serverENV)
# source("./R/app/server_admin_checks_review.R",local=serverENV)
# 
# source("./R/app/server_admin_users.R",local=serverENV)
# 
# source("./R/app/server_datasets_guidance_module.R",local=serverENV)
# #######################


DASH_DISPLAY_BLUE_C <- "35,150,240,0.2"
DASH_DISPLAY_BLUE_CH <- "35,150,240,0.4"
DASH_DISPLAY_BLUE_HH <- "35,150,240,0.05"

DASH_DISPLAY_YELLOW_C <- "255,235,200,0.05"
DASH_DISPLAY_YELLOW_CH <- "255,235,200,0.35"
DASH_DISPLAY_YELLOW_HH <- "255,235,200,0.025"

SYS_PRINT_TIMING <- FALSE
if (grepl("DEV",LOCATION)==TRUE) SYS_PRINT_TIMING <- TRUE

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
debugtime <- function(...,reset=FALSE,showmsg=SYS_PRINT_TIMING) {
  if (showmsg==FALSE) return (NULL)
  args <- unlist(list(...))
  msg <- paste0(args,collapse=" ")

  # if (reset) {
  #   ret <- debug_msg
  #   debug_count <<- 0
  #   debug_time_running <<- Sys.time()
  #   debug_time_now <<- Sys.time()
  #   debug_msg <<- c()
  #   
  #   if (...length()==0) return (ret)
  # }
  
  # debug_count <<- debug_count+1
  # t1 <- format(Sys.time() - debug_time_now)
  # t2 <- format(Sys.time()-debug_time_running)
  # 
  # debug_time_now <<- Sys.time()
  # msg <- paste0(debug_count,": ",msg," @",t1," running total @",(t2))
  # debug_msg <<- c(debug_msg,msg)
  #print(msg)
  cat(msg,"\n")
  #status_message(class="info",msg,"\n")
}

#SYS_PRINT_TIMING <- FALSE
SYS_PRINT_TIMING <- TRUE
if (grepl("DEV",LOCATION)==TRUE) devmode()
#devmode126(FALSE)





labelMatches <- function(find_sections=NA, #match any section if NA, section may match regular expression, notably :ALL will be .*$
                         find_labels=NA,   #match any section if NA, template_label must always have an exact "normalized" match (also may match content)
                         search_sections=NA,
                         search_labels=NA,
                         match_id=NA,
                         match_postion=NA) {
  
  if (!is.na(find_sections) && nchar(find_sections)==0) find_sections <- NA
  if (!is.na(find_labels) && nchar(find_labels)==0) find_labels <- NA
  if (length(search_sections) != length(search_labels)) stop(paste0("seach_sections and search_labels should be equal length vectors to ensure which() returns equivalent indexes: ",
                                                                    length(search_sections)," vs ",length(search_labels)))
  
  if (all(is.na(find_sections)) && all(is.na(find_labels))) return (NULL)
  add_start_stop <- function(str) {
    none <- grep("^\\^",str,invert = T)
    if (any(none)) str[none] <- paste0("^",str[none])
    
    none <- grep("\\$$",str,invert = T)
    if (any(none)) str[none] <- paste0(str[none],"$")
    
    return (str)
  }
  
  if (!is.na(find_sections) && !is.na(find_labels)) {
    
    matches <- intersect(which(stringr::str_detect(string=search_sections,pattern=find_sections)),
                         which(stringr::str_detect(string=search_labels,pattern=find_labels)))
    
  } else if (!is.na(find_sections)) {
    
    matches <- which(stringr::str_detect(string=search_sections,pattern=find_sections))
    
  } else {
    
    matches <- which(stringr::str_detect(string=search_labels,pattern=find_labels))
    
  }
  
  if (length(matches)==0) { return(NULL) 
  } else { 
   
    return (list(match_id=match_id,
                 match_position=match_postion,
                 match_rows=unlist(matches)))
  }
}

openxlsx_getNamedRegionsTable <- function(excelwb) {
  nregions <- openxlsx::getNamedRegions(excelwb)
  nregions_locations <- attr(nregions,"position")
  nregions_allowed <- grep("^[A-Z]+[0-9]+$|^[A-Z]+[0-9]+:[A-Z]+[0-9]+$",nregions_locations)
  nregions_locations <- nregions_locations[nregions_allowed]
  nregions_sheets <- attr(nregions,"sheet")[nregions_allowed]
  nregions <- nregions[nregions_allowed]
  
  nregion_data <- mapply(SIMPLIFY=F,FUN=function(sheet,cell,nr) {
    
    nregion_cols <- convertFromExcelRef(nregions_locations)
    cols <- NULL
    rows <- NULL

    if (grepl("^_xl|\\.wvu\\.",nr)) { return (NULL); }
    
    if (grepl(":",cell)) {
      cell <- strsplit(cell,":",fixed=T)[[1]]
      cols <- seq(from=convertFromExcelRef(cell[[1]]),
                  to=convertFromExcelRef(cell[[2]]),
                  by=1)
      
      rows <- seq(from=as.numeric(gsub("[^0-9]","",cell[[1]])),
                  to=as.numeric(gsub("[^0-9]","",cell[[2]])),
                  by=1)
    } else {
      cols <- convertFromExcelRef(cell)
      rows <- as.numeric(gsub("[^0-9]","",cell))
    }
    
    #if (nr=="Data_PeriodEndDate") { browser() }
    
    val <- suppressWarnings(openxlsx::readWorkbook(xlsxFile=excelwb,
                                  sheet=sheet,
                                  rows=rows,
                                  cols=cols,
                                  colNames=F,
                                  rowNames=F,
                                  skipEmptyRows = T,
                                  skipEmptyCols = T))
    
    value <- NA

    if (length(val)==0) val <- NA
    

    range_val <- NULL    
    if (!is.data.frame(val)) {
      range_val <- as.data.frame(val)
    } else {
      range_val <- val
    }
    
    if (all(dim(val)==1)) {
      value <- unlist(unique(val))
    }
    
    if (nrow(range_val)==1 || ncol(range_val)==1) {
      range_val <- unlist(range_val,use.names = F)
      range_val <- list(c(range_val))
    }
    #browser()
    dt <- data.table(sheet=sheet,
                     range_name=nr,
                     range_value=as.character(value),
                     range_list=list(),
                     range_rows=list(rows),
                     range_cols=list(cols))
    
    dt[,range_list:=list(range_val)]
    

    #dt
    return(dt)
  },
  sheet=nregions_sheets,
  cell=nregions_locations,
  nr=nregions)
  
  nregion_data <- rbindlist(nregion_data)
  return (nregion_data)
}

user_send_email <- function(pool,
                            from="\"RSF JASON\"<noreply@positconnect.int.worldbank.org>",
                            to,
                            subject,
                            html) {
  
  lookup <- db_user_check_email_exists(pool=pool,
                                       RSF_MANAGEMENT_APPLICATION_ID,
                                       to)
  
  #to <- "sheitmann@ifc.org"
  valid <- !empty(lookup)
  
  if (!valid) stop(paste0("Invalid email address: ",to))
  
  if (length(to) != 1) stop("Only one receipient may be defined")
  print(paste0("user_send_email : emailing ",to," from ",from," subject: ",subject))
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
             to=to,
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

