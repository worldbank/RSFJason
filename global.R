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
source("./R/slgp_helpers_trim.R")
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


# 
# in calculations environment for replaceing == and !=
# `%equal%` <- function(e1,e2) { 
#    mapply(function(a,b) { isTRUE(base::all.equal(a,b,check.class=F)) },a=e1,b=e2,USE.NAMES=F)
# }
# 
# 
# `%unequal%` <- function(e1,e2) { 
#          mapply(function(a,b) { !isTRUE(base::all.equal(a,b,check.class=F)) },a=e1,b=e2,USE.NAMES=F)
# }
# 


is.same_text <- function(a,b) { 
  x<-(is.na(a) & is.na(b)) | (as.character(a)==as.character(b)) 
  ifelse(is.na(x),FALSE,x)
}

#all forms of nothing, with zero being a numeric nothing
is.nothing <- function(x) {
  (is.null(x) || length(x)==0 || all(x==0))
}

is.same_number <- function(a,b,tolerance=1/10^CALCULATIONS_ENVIRONMENT$SIG_DIGITS) { 
  
  
  x <- mapply(function(a,b) { 
    if (!is.na(a) & !is.na(suppressWarnings(as.numeric(a)))) a<-as.numeric(a)
    if (!is.na(b) & !is.na(suppressWarnings(as.numeric(b)))) b<-as.numeric(b)
    
    #isTRUE(base::all.equal(a,b,check.class=F,tolerance=tolerance)) 
    isTRUE(base::all.equal(a,b,check.class=F)) | isTRUE(base::`==`(e1=a,e2=b))  | all(c(is.nothing(a),is.nothing(b)))
    
  },a=a,b=b,USE.NAMES=F)
  x[is.na(x)] <- FALSE
  
  if (is.nothing(x)) {
    
    return (FALSE)
    
  } else {
    
    return (x)
    
  }

  
  # nas <- is.na(a) & is.na(b)
  # 
  # a <- suppressWarnings(as.numeric(a))
  # b <- suppressWarnings(as.numeric(b))
  # 
  # invert <- a < 1 & b < 1 & a !=0 & b != 0
  # invert[is.na(invert)] <- FALSE
  # a[invert] <- 1/a[invert]
  # b[invert] <- 1/b[invert]
  # 
  # x <- abs(a-b) < (1/10^sig_digits)
  # x[is.na(x)] <- FALSE #Both NAs will be TRUE, so means either A or B is NA, but not both.  Means, not the same.
  # x[nas] <- TRUE       #Both are NAs so means is the same.
  # x
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

format_name_abbreviation <- function(person_name) {
  
  re <- "^\\w{1}|\\w+-?\\w+$"
  
  name <- unlist(regmatches(person_name, gregexpr(re, person_name)))
  if (is.null(name) || all(is.na(name)) || length(name) != 2) name <- person_name
  else name <- paste0(name[1],".",name[2])
  return(name)
}

#See: https://stackoverflow.com/questions/60977641/r-function-for-rgba-to-hex-color-conversion
rgba2rgb <- function(color_RGBA,background_RGB=col2rgb("white")) {
  
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

rgb2hex <- function(x) { rgb(x[1], x[2], x[3], maxColorValue = 255) }
rgba2hex <- function(color_RGBA,background_RGB) {
  rgb2hex(rgba2rgb(color_RGBA = color_RGBA))
}

words_to_numbers <- function(s) {
  s <- stringr::str_to_lower(s)
  for (i in rev(c(0:19,20,30,40,50,60,70,80,90,100,1000))) {
    s <- stringr::str_replace_all(s, words(i), as.character(i))
    s <- stringr::str_replace_all(s, ordinal(i), as.character(i))
  }
  #s
  
  n <- suppressWarnings(as.numeric(s))
  
  #if, eg, "thirty three" is passed as an argument, 's' will be "30 3"
  #correct format should be "thirty-three"
  #this is fine for small numbers.  For "one million" this will result in NA, for better or worse?
  if (anyNA(n)) {
    nn <- which(is.na(n))
    x <- strsplit(s[nn],"[\\s-]+")
    x <- sapply(x,FUN=function(nums) { sum(suppressWarnings(as.numeric(nums))) })
    n[nn] <- x
  }
  return (n)
}




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


