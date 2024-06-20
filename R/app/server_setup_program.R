###################################################################
## Manages Program Admin: Indicator Selection and Check Assignments
###################################################################


####################
###########OBSERVERS
####################





##################
###########OUTPUTS
##################



output$program_download_archive <- downloadHandler(
  filename = function() {
    program <- SELECTED_PROGRAM()
    if (!isTruthy(program)) return (NULL)
    archive_name <- paste0("RSF PROGRAM ARCHIVE for ",program$program_nickname,".zip")
    
    selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__indicator_program_facilities)
    facilities <- SELECTED_PROGRAM_FACILITIES_LIST()
    if (any(selected_rsf_pfcbl_id %in% facilities$rsf_pfcbl_id)) {
      archive_name <- paste0("RSF ARCHIVE for ",
                             paste0(sort(facilities[rsf_pfcbl_id %in% selected_rsf_pfcbl_id,facility_nickname]),collapse=", ")
                             ,".zip")
    }
    
    archive_name    
  },
  content=function(file) {
    withProgress(message="Downloading file... This may take some time",value=0.5, {
      
      program <- SELECTED_PROGRAM()
      if (!isTruthy(program)) return(showNotification(h2("Please select an RSF Program to to enable download archive")))
      selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__indicator_program_facilities)
      facilities <- SELECTED_PROGRAM_FACILITIES_LIST()
      if (!isTruthy(selected_rsf_pfcbl_id) ||
          !any(selected_rsf_pfcbl_id %in% facilities$rsf_pfcbl_id)) selected_rsf_pfcbl_id <- as.numeric(NA)
      
      program_id <- program$rsf_program_id
      out_path <- DBPOOL %>% db_program_download(rsf_program_id=program_id,
                                                 rsf_pfcbl_ids.filter=selected_rsf_pfcbl_id,
                                                 out_path=".",
                                                 exporting_user_id = USER_ID(),
                                                 archive_name=file,
                                                 verbatim = FALSE) #Will consolidate setup into a single file.  Verbatim TRUE is only via maual utility call

      incProgress(amount=1.0,message="Completed")
    })
  }
)

output$program_download_setup <- downloadHandler(
  filename = function() {
    program <- SELECTED_PROGRAM()
    if (!isTruthy(program)) return (NULL)
    setup_name <- paste0("RSF PROGRAM SETUP FILE for ",program$program_nickname,".xlsx")
    
    selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__indicator_program_facilities)
    facilities <- SELECTED_PROGRAM_FACILITIES_LIST()
    if (any(selected_rsf_pfcbl_id %in% facilities$rsf_pfcbl_id)) {
      setup_name <- paste0("RSF SETUP FILE for ",
                             paste0(sort(facilities[rsf_pfcbl_id %in% selected_rsf_pfcbl_id,facility_nickname]),collapse=", ")
                             ,".xlsx")
    }
    setup_name
  },
  content=function(file) {
    
    withProgress(message="Downloading file... This may take some time",value=0.5, {
      
      program <- SELECTED_PROGRAM()
      if (!isTruthy(program)) return(showNotification(h2("Please select an RSF Program to to enable download archive")))
      
      selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__indicator_program_facilities)
      facilities <- SELECTED_PROGRAM_FACILITIES_LIST()
      if (!isTruthy(selected_rsf_pfcbl_id) ||
          !any(selected_rsf_pfcbl_id %in% facilities$rsf_pfcbl_id)) selected_rsf_pfcbl_id <- as.numeric(NA)
      
      programs_export <- DBPOOL %>% export_rsf_setup_files_to_excel(rsf_program_id=program$rsf_program_id,
                                                                    rsf_pfcbl_ids.filter=selected_rsf_pfcbl_id,
                                                                    exporting_user_id=USER_ID(),
                                                                    include_never_reported=TRUE, #If TRUE, will include blank facility parameters that maybe a user should enter data for
                                                                                                 #Reading in a new file, should filter-out missings as we do not want to import {MISSING} data
                                                                    include=c("data",
                                                                              "settings",
                                                                              "indicators",
                                                                              "checks",
                                                                              "guidance",
                                                                              "actions"))
      
      
      openxlsx::saveWorkbook(wb=programs_export,
                             file=file,
                             overwrite=TRUE)

      return (file)      

      incProgress(amount=1.0,message="Completed")
    })
  }
)

