
SERVER_SETUP_AGREEMENT__RSA <- eventReactive(input$server_programs__selected_facility, {
  facilities <- SELECTED_PROGRAM_FACILITIES_LIST()
  selected_rsf_pfcbl_id <- as.numeric(input$server_programs__selected_facility)
  
  if (!isTruthy(selected_rsf_pfcbl_id)) return (NULL)
  if (empty(facilities)) return (NULL)
  
  #We might verify that its pfcbl_category is "facility" but perhaps programs might one day upload config files, too?
  if (!selected_rsf_pfcbl_id %in% facilities$rsf_pfcbl_id) return (NULL)
  
  current_rsa_template <- DBPOOL %>% dbGetQuery("
    select 
      ri.import_id,
      ri.import_rsf_pfcbl_id,
      ri.import_user_id,
      vai.users_name,
      ri.import_time,
      ri.meta_data
    from p_rsf.reporting_imports ri
    inner join p_rsf.reporting_templates rt on rt.template_id = ri.template_id
    left join p_rsf.view_account_info vai on vai.account_id = ri.import_user_id
    where ri.import_rsf_pfcbl_id = $1::int
      and rt.template_name = 'IFC-RSA-TEMPLATE'
    order by
      ri.reporting_asof_date desc
      ri.import_time desc
      ri.import_id desc
    limit 1",
                                                params=list(selected_rsf_pfcbl_id))
  
})

#The value last set by the paste event listener; or reset by the event receivier.
SERVER_SETUP_AGREEMENT__PASTE_EVENT <- reactiveVal(NA)
SERVER_SETUP_AGREEMENT__PASTE <- eventReactive(input$server_setup_agreement__paste_event, {
  event <- input$server_setup_agreement__paste_event
  if (isTruthy(event)) {
    SERVER_SETUP_AGREEMENT__PASTE_EVENT(event)
  } else {
    SERVER_SETUP_AGREEMENT__PASTE_EVENT(NA)
  }
  return (SERVER_SETUP_AGREEMENT__PASTE_EVENT())
}) %>% debounce(50)

SERVER_SETUP_AGREEMENT__PASTE_AUTO_FORMAT <- function(what,...) {
  if (!isTruthy(input[[what]]) || 
      !identical(SERVER_SETUP_AGREEMENT__PASTE_EVENT(),what)) return (NULL)
  
  SERVER_SETUP_AGREEMENT__PASTE_EVENT(NA) #Reset so does not re-trigger on regular typing.
  #pasted_content <<- input$server_setup_agreement__rsa_terms
  ptext <- parse_text_to_paragraphs(content_lines=readLines(textConnection(input[[what]])),output="text",...)
  
  updateTextAreaInput(session=session,
                      inputId=what,
                      value=ptext)
}

observeEvent(input$server_setup_agreement__rsa_terms, { 
  SERVER_SETUP_AGREEMENT__PASTE_AUTO_FORMAT("server_setup_agreement__rsa_terms",
                                            paragraph.bullets=FALSE) 
  
},ignoreInit=TRUE,ignoreNULL=TRUE)

observeEvent(input$server_setup_agreement__rsa_determination, { 
  SERVER_SETUP_AGREEMENT__PASTE_AUTO_FORMAT("server_setup_agreement__rsa_determination",
                                            paragraph.bullets=TRUE) 
  
},ignoreInit=TRUE,ignoreNULL=TRUE)

observeEvent(input$server_setup_agreement__rsa_costs, { 
  SERVER_SETUP_AGREEMENT__PASTE_AUTO_FORMAT("server_setup_agreement__rsa_costs",
                                            paragraph.bullets=TRUE,
                                            paragraph.custom=) 
  
},ignoreInit=TRUE,ignoreNULL=TRUE)

observeEvent(input$server_setup_agreement__rsa_termination, { 
  SERVER_SETUP_AGREEMENT__PASTE_AUTO_FORMAT("server_setup_agreement__rsa_termination",
                                            paragraph.bullets=TRUE) 
  
},ignoreInit=TRUE,ignoreNULL=TRUE)

observeEvent(input$server_setup_agreement__rsa_criteria, { 
  SERVER_SETUP_AGREEMENT__PASTE_AUTO_FORMAT("server_setup_agreement__rsa_criteria",
                                            paragraph.bullets=TRUE)
  
},ignoreInit=TRUE,ignoreNULL=TRUE)

observeEvent(input$server_setup_agreement__rsa_reporting, { 
  SERVER_SETUP_AGREEMENT__PASTE_AUTO_FORMAT("server_setup_agreement__rsa_reporting",
                                            paragraph.bullets=TRUE,
                                            paragraph.custom=c("^Comments, if any$",
                                                               "^Facility information$"))
  
},ignoreInit=TRUE,ignoreNULL=TRUE)

observeEvent(input$server_setup_agreement__rsa_other, { 
  SERVER_SETUP_AGREEMENT__PASTE_AUTO_FORMAT("server_setup_agreement__rsa_other",
                                            paragraph.bullets=FALSE) 
  
},ignoreInit=TRUE,ignoreNULL=TRUE)

# observeEvent(input$server_setup_agreement__rsa_terms, {
#   
#   if (!isTruthy(input$server_setup_agreement__rsa_terms) || 
#       !identical(SERVER_SETUP_AGREEMENT__PASTE_EVENT(),"server_setup_agreement__rsa_terms")) return (NULL)
#   
#   SERVER_SETUP_AGREEMENT__PASTE_EVENT(NA) #Reset so does not re-trigger on regular typing.
#   #pasted_content <<- input$server_setup_agreement__rsa_terms
#   ptext <- parse_text_to_paragraphs(content_lines=readLines(textConnection(input$server_setup_agreement__rsa_terms)),"text")
#    
#   updateTextAreaInput(session=session,
#                        inputId="server_setup_agreement__rsa_terms",
#                        value=ptext)
#   
# },ignoreInit = T,ignoreNULL=T)






observeEvent(input$server_setup_agreement__apply, {
  
  facilities <- SELECTED_PROGRAM_FACILITIES_LIST()
  selected_rsf_pfcbl_id <- as.numeric(input$server_programs__selected_facility)
  
  if (!isTruthy(selected_rsf_pfcbl_id)) return (NULL)
  if (empty(facilities)) return (NULL)
  if (!selected_rsf_pfcbl_id %in% facilities$rsf_pfcbl_id) return (NULL)
  
  template <- DBPOOL %>% dbGetQuery("
    select
      rt.template_id,
      rt.template_name,
      rt.template_key
    from p_rsf.reporting_templates rt
    where rt.template_name = 'IFC-RSA-TEMPLATE'")
  
  sys_name <- DBPOOL %>% dbGetQuery("
    select
      sn.sys_name,
      sn.pfcbl_name,
      sn.pfcbl_category
    from p_rsf.view_rsf_pfcbl_id_current_sys_names sn
    where sn.rsf_pfcbl_id = $1::int",
    params=list(selected_rsf_pfcbl_id))                         
                         
  rsa_terms <- paste0("<TERMS>\n",trimws(input$server_setup_agreement__rsa_terms),"\n\n")
  rsa_determination <- paste0("<DETERMINATION>\n",trimws(input$server_setup_agreement__rsa_determination),"\n\n")
  rsa_costs <- paste0("<COSTS>\n",trimws(input$server_setup_agreement__rsa_costs),"\n\n")
  rsa_termination <- paste0("<TERMINATION>\n",trimws(input$server_setup_agreement__rsa_termination),"\n\n")
  rsa_criteria <- paste0("<CRITERIA>\n",trimws(input$server_setup_agreement__rsa_criteria),"\n\n")
  rsa_reporting <- paste0("<REPORTING>\n",trimws(input$server_setup_agreement__rsa_reporting),"\n\n")
  rsa_other <- paste0("<OTHER>\n",trimws(input$server_setup_agreement__rsa_other),"\n\n")
  
  rsa_text <- c(rsa_terms,rsa_determination,rsa_costs,rsa_termination,rsa_criteria,rsa_reporting,rsa_other)
  
  rsa_text <- paste0(rsa_text,collapse="\n")
  rsa_text <- paste0("<KEY>",template$template_key,"\n",
                     "<SYSNAME>",sys_name$sys_name,"\n",
                     "<USERID>",USER_ID(),"\n",
                     "<TIMESTAMP>",as.character(now()),"\n",
                     rsa_text)
  
  
  file_name <- gsub("\\s{2,}"," ",paste0(gsub("[^[:alnum:]\\s]"," ",sys_name$pfcbl_name),
                      "-",as.character(today()),
                      ".txt"))
  
  writeLines(rsa_text,
             con=file_name)
  
  
  tryCatch({
    
    withProgress(value=.15,message="Uploading agreement...", {
      
      progress_status_message <- function(class,...) {
        dots <- list(...)
        dots <- paste0(unlist(dots),collapse=" ")
        incProgress(amount=0,
                    message=paste0("Processing agreement text: ",sys_name$pfcbl_name," ",dots))
      }
      
      DBPOOL %>% template_parse_process_and_upload(reporting_user_id=USER_ID(),
                                                   template_files=file_name,
                                                   source_note="Agreement settings",
                                                   parse_rsf_pfcbl_id=selected_rsf_pfcbl_id,
                                                   email_report=FALSE,
                                                   delete_after_upload=TRUE,
                                                   status_message=progress_status_message,
                                                   continue_on_error=FALSE,
                                                   delete_on_error=TRUE)
      incProgress(amount=1,message="Done")
    })
  },
  error=function(e) {
    showNotification(type="error",
                     duration=NULL,
                     ui=h3(paste0("An error occurred when uploading agreement: ",
                                  conditionMessage(e))))
  },
  warning=function(w) {
    showNotification(type="error",
                     duration=NULL,
                     ui=h3(paste0("An error occurred when uploading agreement: ",
                                  conditionMessage(w))))  
    
  })
  
},ignoreInit=TRUE)
