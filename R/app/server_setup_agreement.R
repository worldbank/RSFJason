
SERVER_SETUP_AGREEMENT__RSA <- eventReactive(COHORTS_LIST(), { #input$server_programs__selected_facility, {
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
      ri.reporting_asof_date,
      ri.file_name
    from p_rsf.reporting_imports ri
    inner join p_rsf.reporting_templates rt on rt.template_id = ri.template_id
    left join p_rsf.view_account_info vai on vai.account_id = ri.import_user_id
    where ri.import_rsf_pfcbl_id = $1::int
      and rt.template_name = 'IFC-RSA-TEMPLATE'
    order by
      ri.reporting_asof_date desc,
      ri.import_time desc,
      ri.import_id desc
    limit 1",
                                                params=list(selected_rsf_pfcbl_id))
  
})

SERVER_SETUP_AGREEMENT__RSA_CONTENT <- eventReactive(SERVER_SETUP_AGREEMENT__RSA(), {
 rsa <- SERVER_SETUP_AGREEMENT__RSA()
 if (empty(rsa)) return (NULL)
 
 content <- DBPOOL %>% dbGetQuery("
  select 
    import_id,
    x.key as section, 
    x.value as text
  from p_rsf.reporting_imports ri
  inner join lateral (select * from jsonb_each_text(metadata -> 'RSA') ) as x on true
  where ri.import_id = $1::int",
  params=list(rsa$import_id))
  setDT(content)
  
  return (content)
},ignoreNULL=FALSE,ignoreInit=TRUE)

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
  ptext <- gsub("\\n{3,}","\\n\\n",ptext)
  
  updateTextAreaInput(session=session,
                      inputId=what,
                      value=ptext)
}

observeEvent(SERVER_SETUP_AGREEMENT__RSA_CONTENT(), {
  
  content <- SERVER_SETUP_AGREEMENT__RSA_CONTENT()

  for (sec in c("server_setup_agreement__rsa_terms",
                    "server_setup_agreement__rsa_determination",
                    "server_setup_agreement__rsa_costs",
                    "server_setup_agreement__rsa_termination",
                    "server_setup_agreement__rsa_criteria",
                    "server_setup_agreement__rsa_reporting",
                    "server_setup_agreement__rsa_other")) {
    
      
      value <- NULL
      
      if (!empty(content)) {
        value <- content[section==toupper(gsub("^.*_([a-z]+)+$","\\1",sec)),
                         text]
      }
      
      if (!isTruthy(value)) value <- ""
      else value <- gsub("\\n{3,}","\\n\\n",value)
      
      updateTextAreaInput(session=session,
                          inputId=sec,
                          value=value)
  }
},ignoreNULL=FALSE)

observeEvent(input$server_setup_agreement__rsa_terms, { 
  SERVER_SETUP_AGREEMENT__PASTE_AUTO_FORMAT("server_setup_agreement__rsa_terms",
                                            paragraph.bullets=FALSE,
                                            paragraph.custom=c('^"[A-Z].*$')) 
  
},ignoreInit=TRUE,ignoreNULL=TRUE)

observeEvent(input$server_setup_agreement__rsa_determination, { 
  SERVER_SETUP_AGREEMENT__PASTE_AUTO_FORMAT("server_setup_agreement__rsa_determination",
                                            paragraph.bullets=TRUE) 
  
},ignoreInit=TRUE,ignoreNULL=TRUE)

observeEvent(input$server_setup_agreement__rsa_costs, { 
  SERVER_SETUP_AGREEMENT__PASTE_AUTO_FORMAT("server_setup_agreement__rsa_costs",
                                            paragraph.bullets=TRUE) 
  
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
       
  created_date <- DBPOOL %>% dbGetQuery("
    select
      ids.created_in_reporting_asof_date::text
    from p_rsf.rsf_pfcbl_ids ids
    where ids.rsf_pfcbl_id = $1::int",
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
                     "<ASOFDATE>",created_date$created_in_reporting_asof_date,"\n",
                     "<USERID>",USER_ID(),"\n",
                     "<TIMESTAMP>",as.character(now()),"\n",
                     rsa_text)
  
  
  file_name <- gsub("\\s{2,}"," ",paste0(gsub("[^[:alnum:]\\s]"," ",sys_name$pfcbl_name),
                      "-",as.character(today()),
                      ".txt"))
  
  writeLines(rsa_text,
             con=file_name)
  
  withProgress(value=.15,message="Uploading agreement...", {
    
    progress_status_message <- function(class,...) {
      dots <- list(...)
      dots <- paste0(unlist(dots),collapse=" ")
      incProgress(amount=0,
                  message=paste0("Processing agreement text: ",sys_name$pfcbl_name," ",dots))
    }
    
    tryCatch({
    
    
      
      template_parse_process_and_upload(pool=dbStart(credentials_file=paste0(getwd(),LOCATIONS[[LOCATION]])),
                                        reporting_user_id=USER_ID(),
                                        template_files=file_name,
                                        source_note="Agreement settings",
                                        parse_rsf_pfcbl_id=selected_rsf_pfcbl_id,
                                        email_report=FALSE,
                                        delete_after_upload=TRUE,
                                        status_message=progress_status_message,
                                        continue_on_error=FALSE,
                                        delete_on_error=TRUE)
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
    incProgress(amount=1,message="Done")
  })
  
  REFRESH_SELECTED_COHORT_DATA(REFRESH_SELECTED_COHORT_DATA()+1)
  
},ignoreInit=TRUE)

output$setup_agreement__title <- renderText({
  rsa <- SERVER_SETUP_AGREEMENT__RSA()
  if (empty(rsa)) { "No RSA on file: Upload the RSA using the Upload Template; or copy-paste the relevant RSA sections into these form fields" 
  } else {
    paste0("RSA Content as-of ",format_asof_date_label(rsa$reporting_asof_date),": ",gsub(".gz$","",rsa$file_name)," (",toupper(format.Date(rsa$import_time,"%b%d %Hh%M")),")")
  }
})
