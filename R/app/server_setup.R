#If we change the filter for the indicator view, also update it for checks
#And also show subscribed by default for program and unsubscribed by default for facilities.
observeEvent(input$ui_setup__indicator_program_facilities, {
  
  if (empty(SELECTED_PROGRAM_FACILITIES_LIST())) return (NULL)
  
  selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__indicator_program_facilities)
  if (!isTruthy(selected_rsf_pfcbl_id)) return (NULL)
  
  #If indicator program/facility is selected: update other fileters to match
  if (!identical(selected_rsf_pfcbl_id,as.numeric(input$ui_setup__checks_program_facilities))) {
    updateSelectizeInput(session=session,
                         inputId="ui_setup__checks_program_facilities",
                         selected = selected_rsf_pfcbl_id)
  }

  if (!identical(selected_rsf_pfcbl_id,as.numeric(input$ui_setup__templates_program_facilities))) {
    updateSelectizeInput(session=session,
                         inputId="ui_setup__templates_program_facilities",
                         selected = selected_rsf_pfcbl_id)
  }
  
  
  
  # if (selected_rsf_pfcbl_id %in% SELECTED_PROGRAM_FACILITIES_LIST()$rsf_pfcbl_id) {
  #   updateSelectizeInput(session=session,
  #                        inputId="ui_setup__indicator_monitoring_filter",
  #                        selected = "unreported")
  # } else {
  #   updateSelectizeInput(session=session,
  #                        inputId="ui_setup__indicator_monitoring_filter",
  #                        selected = "subscribed")
  # }
  
},ignoreInit=F,priority=3)

observeEvent(input$ui_setup__checks_program_facilities, {

  if (empty(SELECTED_PROGRAM_FACILITIES_LIST())) return (NULL)
  
  selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__checks_program_facilities)
  
  if (!isTruthy(selected_rsf_pfcbl_id)) return (NULL)
  
  #If check program/facility is selected: update other fileters to match
  if (!identical(selected_rsf_pfcbl_id,as.numeric(input$ui_setup__indicator_program_facilities))) {
    updateSelectizeInput(session=session,
                         inputId="ui_setup__indicator_program_facilities",
                         selected = selected_rsf_pfcbl_id)
  }

  #If check program/facility is selected: update other fileters to match
  if (!identical(selected_rsf_pfcbl_id,as.numeric(input$ui_setup__templates_program_facilities))) {
    updateSelectizeInput(session=session,
                         inputId="ui_setup__templates_program_facilities",
                         selected = selected_rsf_pfcbl_id)
  }
  

  
  
  # if (selected_rsf_pfcbl_id %in% SELECTED_PROGRAM_FACILITIES_LIST()$rsf_pfcbl_id) {
  #   updateSelectizeInput(session=session,
  #                        inputId="ui_setup__checks_monitoring_filter",
  #                        selected = "unreported")
  # } else {
  #   updateSelectizeInput(session=session,
  #                        inputId="ui_setup__checks_monitoring_filter",
  #                        selected = "subscribed")
  #   
  # }
  
},ignoreInit=F,priority=2)

observeEvent(input$ui_setup__templates_program_facilities, {
  
  if (empty(SELECTED_PROGRAM_FACILITIES_LIST())) return (NULL)
  
  selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__templates_program_facilities)
  
  if (!isTruthy(selected_rsf_pfcbl_id)) return (NULL)
  
  #If check program/facility is selected: update other fileters to match
  if (!identical(selected_rsf_pfcbl_id,as.numeric(input$ui_setup__indicator_program_facilities))) {
    updateSelectizeInput(session=session,
                         inputId="ui_setup__indicator_program_facilities",
                         selected = selected_rsf_pfcbl_id)
  }
  
  #If check program/facility is selected: update other fileters to match
  if (!identical(selected_rsf_pfcbl_id,as.numeric(input$ui_setup__checks_program_facilities))) {
    updateSelectizeInput(session=session,
                         inputId="ui_setup__checks_program_facilities",
                         selected = selected_rsf_pfcbl_id)
  }
},ignoreInit=F,priority=1)

#Update indicators to monitor when program is selected or new facility is created and facilities list refreshes
observeEvent(SELECTED_PROGRAM_FACILITIES_LIST(), {
  
  program <- SELECTED_PROGRAM()
  facilities <- SELECTED_PROGRAM_FACILITIES_LIST()
  
  if (empty(program)) return (NULL)
  if (is.null(facilities)) return (NULL)
  
  
  facilities <- facilities[!rsf_pfcbl_id %in% program$rsf_pfcbl_id] #for global and programs without facilities
  
  program_name <- paste0("program:",program$program_nickname," (all facilities)")
  if (program$rsf_pfcbl_id==0) program_name <- paste0("program:",program$program_nickname)
  
  select <- NULL
  if (!empty(facilities)) {
    select <- setNames(c(program$rsf_pfcbl_id,
                         facilities$rsf_pfcbl_id),
                       c(program_name,
                         paste0("facility:",facilities$facility_nickname)))
  } else {
    select <- setNames(program$rsf_pfcbl_id,
                       program_name)
  }
  #indicator setup
  {
    updateSelectizeInput(session=session,
                         inputId="ui_setup__indicator_program_facilities",
                         choices=select,
                         selected = program$rsf_pfcbl_id)
    
    updateSelectizeInput(session=session,
                         inputId="ui_setup__indicator_monitoring_filter",
                         selected = "subscribed")
    
    updateSelectizeInput(session=session,
                         inputId="ui_setup__indicator_category_filter",
                         selected = "all")
    
    updateTextInput(session=session,
                    inputId="ui_setup__indicator_search_filter",
                    value="")

  }  
  
  #Checks setup
  
  {
    updateSelectizeInput(session=session,
                         inputId="ui_setup__checks_program_facilities",
                         choices=select,
                         selected = program$rsf_pfcbl_id)
    
    updateSelectizeInput(session=session,
                         inputId="ui_setup__checks_monitoring_filter",
                         selected = "subscribed")
    
    updateSelectizeInput(session=session,
                         inputId="ui_setup__checks_category_filter",
                         selected = "all")
    
    updateTextInput(session=session,
                    inputId="ui_setup__checks_search_filter",
                    value="")
  }
  
  #templates
  {
   
    updateSelectizeInput(session=session,
                         inputId="ui_setup__templates_program_facilities",
                         choices=select,
                         selected = program$rsf_pfcbl_id)
    
   }
},ignoreNULL=FALSE)


output$server_setup_download_archive_title <- renderText({
  
  program <- SELECTED_PROGRAM()
  facilities <- SELECTED_PROGRAM_FACILITIES_LIST()  
  
  if (empty(program)) return ("Select RSF Program")
  
  selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__indicator_program_facilities)
  
  message <- "Select Program or Facility"
  if (selected_rsf_pfcbl_id %in% program$rsf_pfcbl_id) message <- "Program Zip Archive"
  else if (selected_rsf_pfcbl_id %in% facilities$rsf_pfcbl_id) message <- paste0(facilities[rsf_pfcbl_id==selected_rsf_pfcbl_id,facility_nickname],
                                                                                 " Zip Archive")
  return (message)
})

output$server_setup_download_setup_title <- renderText({
  
  program <- SELECTED_PROGRAM()
  facilities <- SELECTED_PROGRAM_FACILITIES_LIST()  
  
  if (empty(program)) return ("Select RSF Program")
  
  selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__indicator_program_facilities)
  
  message <- "Select Program or Facility"
  if (selected_rsf_pfcbl_id %in% program$rsf_pfcbl_id) message <- "Program Setup File"
  else if (selected_rsf_pfcbl_id %in% facilities$rsf_pfcbl_id) message <- paste0(facilities[rsf_pfcbl_id==selected_rsf_pfcbl_id,facility_nickname],
                                                                                 " Setup File")
  return (message)
  
})

output$server_setup_download_backup_title <- renderText({
  
  program <- SELECTED_PROGRAM()
  facilities <- SELECTED_PROGRAM_FACILITIES_LIST()  
  
  if (empty(program)) return ("Select RSF Program")
  
  selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__indicator_program_facilities)
  
  message <- "Select Program or Facility"
  if (selected_rsf_pfcbl_id %in% program$rsf_pfcbl_id) message <- "Program Backup Data"
  else if (selected_rsf_pfcbl_id %in% facilities$rsf_pfcbl_id) message <- paste0(facilities[rsf_pfcbl_id==selected_rsf_pfcbl_id,facility_nickname],
                                                                                 " Backup Data")
  return (message)
  
})