#shiny::runApp(display.mode="showcase")
options(shiny.reactlog=TRUE) #press Ctrl+F3 to launch the reactive log visualization
options(shiny.autoload.r = FALSE)
options(stringsAsFactors = FALSE)
options(scipen=999)
options(warning.length=8170)
#options(shiny.maxRequestSize=30*1024^2)
options(shiny.maxRequestSize=300*1024^2)

#options(warn=2, error=recover)
options(warn=0, error=NULL)
#Sys.setlocale("LC_ALL",c("English"))

RSF_MANAGEMENT_APPLICATION_ID <- "4608A309E2E38860DC98FAC53F967CF2"
  
LOCATIONS <- list(ARL="/credentials/credentials-remote-ARL.yaml",
                  #SSA_DEV="/credentials/credentials-remote-dev.yaml",
                  #SSA_PROD="/credentials/credentials-remote-RSF.yaml",
                  Jason_DEVB="/credentials/credentials-rsfjson-rsfdevb.yaml",
                  Jason_DEV="/credentials/credentials-rsfjson-rsfdev.yaml",
                  Jason_PROD="/credentials/credentials-rsfjson-rsfprod.yaml")

LOCATION <- "Jason_DEV"
#LOCATION <- "Jason_PROD"

if (grepl("DEV",LOCATION)==TRUE) {
  #options(shiny.erctror = browser)
  options(shiny.error = NULL)
}

library(lubridate)
library(stringr)
library(data.table)

library(shiny)
library(shinydashboard)
library(shinybusy)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
#library(shinyglide)
library(DT)
library(jsonlite)
library(tools)
library(plyr)
#library(tidyr)
library(glue)
library(R.utils)
library(digest)
#https://appsilon.com/forget-about-excel-use-r-shiny-packages-instead/?nabe=4634331497365504:1


library(openxlsx)
#library(reshape2)
library(rlang)

library(RPostgres)
library(pool)
library(yaml)
#library(sendmailR)


source("./R/openxlsx_get_formulas.R")

source('./accounts/module_accounts_server.R')
source('./accounts/ui_objects.R')
source('./accounts/db_user_login_change_password.R')
source('./accounts/db_user_login_reset_check.R')
source('./accounts/db_user_check_email_exists.R')
source('./accounts/db_user_reset_password.R')
source('./accounts/db_user_login.R')
source('./accounts/db_user_logout.R')

source("./R/reports/export_reporting_cohort_to_excel.R")

source("./R/db_create_new_rsf_ids.R")
source("./R/db_create_entity_indicator_requirements.R")

source("./R/db_data_get_info.R")
source("./R/db_data_get_flags.R")
source("./R/db_data_update_flags.R")
source("./R/db_data_get_fx_ratio.R")
source("./R/db_data_get_current.R")
source("./R/db_data_pivot_family.R")

source("./R/db_cohort_delete.R")
source("./R/db_cohort_get_data.R")

source("./R/db_cohort_create.R")
source("./R/db_cohort_upload_file.R")
source("./R/db_cohort_download_file.R")

source("./R/db_rsf_reports_list.R")

source("./R/db_export_create.R")
source("./R/db_export_get_template.R")
source("./R/db_export_load_report.R")

source("./R/db_add_update_data_user.R")
source("./R/db_add_update_data_system.R")
source("./R/db_rsf_checks_add_update.R")
source("./R/db_rsf_checks_validate.R")

source("./R/db_program_get_data.R")
source("./R/db_program_create.R")
source("./R/db_program_download.R")
source("./R/db_program_revalidate_calculations.R")
source("./R/db_program_toggle_indicator_subscription.R")
source("./R/db_program_toggle_check_subscription.R")

source("./R/rsf_computation_fx_conversion.R")
source("./R/rsf_program_calculate.R")
source("./R/rsf_program_check.R")
source("./R/rsf_program_perform_calculations.R")
source("./R/rsf_program_perform_checks.R")
source("./R/db_program_get_stale_calculations.R")
source("./R/db_program_get_stale_checks.R")
source("./R/db_program_facility_checks_add_update_guidance.R")
source("./R/db_indicators_get_labels.R")
source("./R/db_indicator_update.R")
source("./R/db_indicator_create.R")
source("./R/db_indicator_delete.R")

source("./R/rsf_indicators_calculate_do_test.R")
source("./R/rsf_calculations_recalculate.R")


source("./R/db_check_create.R")
source("./R/db_check_update.R")

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

source("./R/db_rsf_get_pfcbl_from_sys_ids.R")

source("./R/slgp_helpers_trim.R")
source("./R/rsf_database.R")
source("./R/rsf_calculations_environment.R")
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
source('./R/templates/parse_template_labels.R')

source('./R/templates/template_parse_file.R')
source('./R/templates/template_parse_process_and_upload.R')

source('./R/templates/parse_template_IFC_QR.R')
source('./R/templates/parse_template_csv.R')
source('./R/templates/parse_template_rsf_setup.R')
source('./R/templates/parse_template_csv_backup_data.R')

source('./R/templates/parse_template_pfcbl_editor_report.R')
source('./R/templates/parse_template_rsf_create_entities.R')


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

debug_count <- 0
debug_time_running <- Sys.time()
debug_time_now <- Sys.time()
debug_msg <- c()
debugtime <- function(...,reset=FALSE) {
  args <- unlist(list(...))
  msg <- paste0(args,collapse=" ")

  if (reset) {
    ret <- debug_msg
    debug_count <<- 0
    debug_time_running <<- Sys.time()
    debug_time_now <<- Sys.time()
    debug_msg <<- c()
    
    if (...length()==0) return (ret)
  }
  
  debug_count <<- debug_count+1
  t1 <- format(Sys.time() - debug_time_now)
  t2 <- format(Sys.time()-debug_time_running)

  debug_time_now <<- Sys.time()
  msg <- paste0(debug_count,": ",msg," @",t1," running total @",(t2))
  debug_msg <<- c(debug_msg,msg)
  #print(msg)
  cat(msg,"\n")
  #status_message(class="info",msg,"\n")
}

#SYS_PRINT_TIMING <- FALSE
SYS_PRINT_TIMING <- TRUE
if (grepl("DEV",LOCATION)==TRUE) devmode()
#devmode126(FALSE)




format_html_option <- function(options_group_name) {
  html <- glue("<div class='indicator_bubble' style='vertical-align:middle;white-space:nowrap;padding-right:10px;display:inline-block;background-color:#3db83b'>
                              <div style='display:inline-block;vertical-align:middle;'><i class='fa fa-sitemap'></i></div>
                              <div style='display:inline-block;vertical-align:middle;'>{options_group_name}</div>
                              </div>")
  return (html)
}

format_html_check_icon <- function(check_class,is_reporting_flag=FALSE,title=check_class,css_class="") {
  
  icon_class <- switch(check_class,
                       critical='fas fa-fire',
                       error='fas fa-times-circle',
                       warning='fas fa-exclamation-triangle',
                       info='fas fa-info-circle',
                       "")

  reporting_icon <- ""
  if (is_reporting_flag==TRUE) reporting_icon <- "<i class='fas fa-file-upload icon-reporting' style='display:inline-block;'></i>"
  
  html <- paste0(reporting_icon,"<i class='",icon_class," icon-",check_class," ",css_class,"' style='display:inline-block;' title=\"",title,"\"></i>")
  return (html)
}

format_html_check <- function(check_name,
                              check_class,
                              check_type=NA,
                              is_subscribed=TRUE,
                              is_system=FALSE,
                              user_subscription=NULL,
                              id="") {

  htmlid <- ifelse(nchar(id)>1,paste0("id='",id,"'"),"")
  htmluserid <- ifelse(nchar(id)>1,paste0("id='",id,"subscription'"),"")
  
  check_name <- gsub("[[:punct:]]"," ",check_name) #easier to read in html bubble
  check_name <- sub("(loan|borrower|client|facility|program|global)","\\1:",check_name,ignore.case = T)
  check_name <- tools::toTitleCase(check_name)
  check_name <- gsub("^sys","SYS",check_name,ignore.case = T)
  
  check_type_color <- fcase(check_type=="data_audit","violet",
                            check_type=="data_validity","violet",
                            check_type=="contract_breach","limegreen",
                            check_type=="contract_criteria","limegreen",
                            check_type=="business_integrity","skyblue",
                            check_type=="business_monitoring","skyblue",
                            check_type=="none","pink",
                            default="gray")
  
 
  icon_class <- switch(check_class,
                       critical='fas fa-fire',
                       error='fas fa-times-circle',
                       warning='fas fa-exclamation-triangle',
                       info='fas fa-info-circle',
                       "")
  
  unsubscribed <- ifelse(is_subscribed==FALSE," unsubscribed","")
  user_subscribed <- fcase(is.null(user_subscription),"",
                           user_subscription==TRUE,
                           paste0("<i ",htmluserid," class='fa-regular fa-circle-check' style='color:white;font-size:12px;'></i>"),
                           user_subscription==FALSE,
                           paste0("<i ",htmluserid," class='fa-regular fa-circle-xmark' style='color:white;font-size:12px;'></i>"),
                           default=paste0("<i ",htmluserid," class='fa-regular fa-circle-question' style='color:white;font-size:12px;'></i>"))
  
  
  
  #user_subscribed <- "<i name='userSubscription' class='fa-regular fa-circle-check' style='color:green'></i>"
  html <- glue("<div class='check_mark' style='display:flex;flex-flow:row nowrap;white-space:nowrap;background:linear-gradient(to right,{check_type_color},white)'>
                  <div {htmlid} class='check_bubble {check_class}{unsubscribed}' style='display:flex;flex-flow:row nowrap;justify-content:flex-end;align-items: center;'>
                    <div style='color:rgba(250,250,250,.5);display:flex;flex-flow:row nowrap;margin-left:5px;'><i class='{icon_class}'></i></div>
                    <div style='margin-left:5px;margin-right:5px;display:flex;flex-flow:row nowrap;'>{check_name}</div>
                    {user_subscribed}
                  </div>
                </div>")
  
  return (html)
}

format_html_indicator <- function(indicator_name,
                                  data_category,
                                  data_type,
                                  is_system,
                                  is_calculated,
                                  options_group_name=NA,
                                  is_subscribed=TRUE,
                                  user_subscription=NULL,
                                  id="") {
  
  data_category <- tolower(data_category)
  data_type <- tolower(data_type)
  is_calculated <- toupper(as.character(is_calculated))
  is_system <- as.logical(is_system) #should only be passed-in by system
  htmluserid <- ifelse(nchar(id)>1,paste0("id='",id,"subscription'"),"")
  html_attributes <- "<div style='display:inline-block;white-space:nowrap;color:rgba(250,250,250,.5);vertical-align:middle;'>"
  if (data_type=="number")        html_attributes <- paste(html_attributes,"<i class='fa fa-hashtag' title='Data type: number' style='padding-left:5px;'></i>")
  else if (data_type=="currency") html_attributes <- paste(html_attributes,"<i class='fa fa-coins' title='Data type: currency' style='padding-left:5px;'></i>")
  else if (data_type=="currency_ratio") html_attributes <- paste(html_attributes,"<i class='fa fa-coins' title='Data type: currency' style='padding-left:5px;'></i>")
  else if (data_type=="percent")  html_attributes <- paste(html_attributes,"<i class='fa fa-percent' title='Data type: percent' style='padding-left:5px;'></i>")
  else if (data_type=="date")     html_attributes <- paste(html_attributes,"<i class='fa fa-calendar-alt' title='Data type: date' style='padding-left:5px;'></i>")
  else if (data_type=="text")     html_attributes <- paste(html_attributes,"<i class='fa fa-paragraph' title='Data type: text' style='padding-left:5px;'></i>")
  else if (data_type=="logical")  html_attributes <- paste(html_attributes,"<i class='fa fa-grip-lines-vertical' title='Data type: true-false' style='padding-left:5px;'></i>")
  
  if (is_system==TRUE) html_attributes <- paste(html_attributes,"<i class='fa fa-cogs' title='System Indicator' style='padding-left:5px;'></i>")
  if (toupper(is_calculated)=="TRUE") html_attributes <- paste(html_attributes,"<i class='fa fa-calculator' title='Calculated Indicator' style='padding-left:5px;'></i>")
  if (is.na(options_group_name)==FALSE) html_attributes <- paste(html_attributes,"<i class='fa fa-sitemap' title='Assigned choices type: ",options_group_name,"' style='padding-left:5px;'></i>")
  
  user_subscribed <- fcase(is.null(user_subscription),"",
                           user_subscription==TRUE,
                           paste0("<i ",htmluserid," class='fa-regular fa-circle-check' style='color:white;font-size:12px;'></i>"),
                           user_subscription==FALSE,
                           paste0("<i ",htmluserid," class='fa-regular fa-circle-xmark' style='color:white;font-size:12px;'></i>"),
                           default=paste0("<i ",htmluserid," class='fa-regular fa-circle-question' style='color:white;font-size:12px;'></i>"))
  
  
  html_attributes <- paste(html_attributes,"</div>")
  
  html_status <- "<div style='display:inline-block;vertical-align:middle'>"
  #if (is_hidden==TRUE)      html_status <- paste(html_status,"<i class='fa fa-user-secret' style='color:purple;padding-left:5px;' title='Indicator is hidden: will not appear in searches or downloads; but will be saved on uploads'></i>")
  #if (is_deprecated==TRUE)  html_status <- paste(html_status,"<i class='fa fa-ban' style='color:red;padding-left:5px;' title='Indicator is deprecated and no longer used by this RSF Program (however old data reported when inidcator was previously used is retained)'></i>")
  html_status <- paste(html_status,"</div>")
  
  indicator_html <- paste0("<div style='display:inline-block;vertical-align:middle;'>",html_status,"
                              <div ",ifelse(isTruthy(id),
                                            paste0("id='",id,"'"),
                                            ""),
                              "class='indicator_bubble indicator ",data_category," ",ifelse(is_subscribed==FALSE,"unsubscribed","")," ",ifelse(is_system,"disabled",""),"' style='vertical-align:middle;white-space:nowrap;display:inline-block;'>
                                       ",html_attributes,"
                                       <div ",
                                       ifelse(isTruthy(id),
                                              paste0("id='",id,"-text'"),
                                              "")
                                ," style='display:inline-block;vertical-align:middle;font-size:12px;font-weight:bold;padding-left:5px;padding-right:10px;'>",indicator_name,"</div>
                               ",user_subscribed,"
                                       </div>
                                       </div>")

  return (indicator_html)
}


is.same_text <- function(a,b) { 
  x<-(is.na(a) & is.na(b)) | (as.character(a)==as.character(b)) 
  ifelse(is.na(x),FALSE,x)
}

is.same_number <- function(a,b,sig_digits=CALCULATIONS_ENVIRONMENT$SIG_DIGITS) { 
  
  nas <- is.na(a) & is.na(b)
  
  a <- suppressWarnings(as.numeric(a))
  b <- suppressWarnings(as.numeric(b))
  
  invert <- a < 1 & b < 1
  invert[is.na(invert)] <- FALSE
  a[invert] <- 1/a[invert]
  b[invert] <- 1/b[invert]
  
  x <- abs(a-b) < (1/10^sig_digits)
  x[is.na(x)] <- FALSE #Both NAs will be TRUE, so means either A or B is NA, but not both.  Means, not the same.
  x[nas] <- TRUE       #Both are NAs so means is the same.
  x
}

user_send_email <- function(pool,from="ARL System <sheitmann@ifc.org>",to,subject,html) {
  
  lookup <- db_user_check_email_exists(pool,APPLICATION_ID,to)
  valid <- ifelse(is.nanu(lookup),FALSE,TRUE)
  
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
    sendmail(from, to, subject, msg=msg)
  },
  error=function(e) {
    print(conditionMessage(e))
  },
  warning=function(w) {
    print(conditionMessage(w))
  })
}

format_name_abbreviation <- function(person_name)
{
  
  re <- "^\\w{1}|\\w+-?\\w+$"
  
  name <- unlist(regmatches(person_name, gregexpr(re, person_name)))
  if (is.null(name) || all(is.na(name)) || length(name) != 2) name <- person_name
  else name <- paste0(name[1],".",name[2])
  return(name)
}

#See: https://stackoverflow.com/questions/60977641/r-function-for-rgba-to-hex-color-conversion
rgba2rgb <- function(color_RGBA,background_RGB=col2rgb("white")){
  
  # get alpha
  if (length(color_RGBA)==1) color_RGBA <- as.numeric(trimws(unlist(str_split(color_RGBA,","))))
  if (length(color_RGBA) != 4) stop("color_RGBA should have 4 elements")
  
  alpha=color_RGBA[4]
  
  # get new color  
  new_col=matrix(c(
    (1 - alpha) * background_RGB[1] + alpha * color_RGBA[1],
    (1 - alpha) * background_RGB[2] + alpha * color_RGBA[2],
    (1 - alpha) * background_RGB[3] + alpha * color_RGBA[3]),
    nrow=3,ncol=1,dimnames=list(c("red","green","blue"))
  )
  return(new_col)
}

rgb2hex <- function(x) rgb(x[1], x[2], x[3], maxColorValue = 255)
rgba2hex <- function(color_RGBA,background_RGB) {
  rgb2hex(rgba2rgb(color_RGBA = color_RGBA))
}


