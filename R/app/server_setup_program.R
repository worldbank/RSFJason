###################################################################
## Manages Program Admin: Indicator Selection and Check Assignments
###################################################################


####################
###########OBSERVERS
####################





##################
###########OUTPUTS
##################


SERVER_SETUP_PROGRAM_ARCHIVE <- eventReactive(c(SELECTED_PROGRAM_ID(),
                                                input$ui_setup__indicator_program_facilities), {
  
  program <- SELECTED_PROGRAM()
  if (!isTruthy(program)) return (NULL)
  
  
  selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__indicator_program_facilities)
  facilities <- SELECTED_PROGRAM_FACILITIES_LIST()
  if (any(selected_rsf_pfcbl_id %in% facilities$rsf_pfcbl_id)) {
    archive_name <- paste0("RSF ARCHIVE for ",
                           paste0(sort(facilities[rsf_pfcbl_id %in% selected_rsf_pfcbl_id,facility_name]),collapse=", ")
                           ,".zip")
    return (list(export_rsf_pfcbl_id=selected_rsf_pfcbl_id,
                 export_name=archive_name,
                 export_pfcbl_category="facility"))
  } else {
    
    archive_name <- paste0("RSF PROGRAM ARCHIVE for ",program$program_nickname,".zip")  
    return (list(export_rsf_pfcbl_id=program$rsf_pfcbl_id,
                 export_name=archive_name,
                 export_pfcbl_category="program"))
  }
})

output$program_download_archive <- downloadHandler(
  filename = function() {
    
    SERVER_SETUP_PROGRAM_ARCHIVE()$export_name
    
  },
  content=function(file) {
    withProgress(message="Downloading file... This may take some time",value=0.5, {
      
      program <- SELECTED_PROGRAM()
      if (!isTruthy(program)) return(showNotification(h2("Please select an RSF Program to to enable download archive")))
      
      archive <- SERVER_SETUP_PROGRAM_ARCHIVE()
      program_id <- program$rsf_program_id
      
      #if it's a program-level archive, instruct get everything
      #importantly, this will include Global settings associated for the Program
      #whereas facility-level filters will not include Global (or Program) level settings.
      export_rsf_pfcbl_id <- NA
      if (archive$export_pfcbl_category=="facility") {
        export_rsf_pfcbl_id <- archive$export_rsf_pfcbl_id
      } else {
        export_rsf_pfcbl_id <- program_id
      }
      
      out_path <- DBPOOL %>% db_program_download(export_pfcbl_id=export_rsf_pfcbl_id,
                                                 out_path=".",
                                                 exporting_user_id = USER_ID(),
                                                 archive_name=archive$export_name,
                                                 verbatim = FALSE) #Will consolidate setup into a single file.  Verbatim TRUE is only via maual utility call

      #incProgress(amount=1.0,message="Completed")
      success <- file.copy(from=out_path,
                           to=file,
                           overwrite=T)
      
      return (file)
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
                             paste0(sort(facilities[rsf_pfcbl_id %in% selected_rsf_pfcbl_id,facility_name]),collapse=", ")
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
          !any(selected_rsf_pfcbl_id %in% facilities$rsf_pfcbl_id)) selected_rsf_pfcbl_id <- program$rsf_program_id
      
      programs_export <- DBPOOL %>% export_rsf_setup_files_to_excel(export_pfcbl_id=selected_rsf_pfcbl_id,
                                                                    exporting_user_id=USER_ID(),
                                                                    include_never_reported=TRUE, #If TRUE, will include blank facility parameters that maybe a user should enter data for
                                                                                                 #Reading in a new file, should filter-out missings as we do not want to import {MISSING} data
                                                                    include=c("data",
                                                                              "settings",
                                                                              "indicators",
                                                                              "checks",
                                                                              "config",
                                                                              "actions",
                                                                              "flags",
                                                                              "review"))
      
      
      openxlsx::saveWorkbook(wb=programs_export,
                             file=file,
                             overwrite=TRUE)

      return (file)      

      incProgress(amount=1.0,message="Completed")
    })
  }
)


output$program_download_backup <- downloadHandler(
  filename = function() {
    program <- SELECTED_PROGRAM()
    if (!isTruthy(program)) return (NULL)
    setup_name <- paste0("RSF PROGRAM BACKUP DATA FILE for ",
                         program$program_nickname
                         ," ",
                         format.Date(now(tzone="EST"),"%Y-%m-%d %Hh%M"),
                         ".csv.gz")
    
    selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__indicator_program_facilities)
    facilities <- SELECTED_PROGRAM_FACILITIES_LIST()
    if (any(selected_rsf_pfcbl_id %in% facilities$rsf_pfcbl_id)) {
      setup_name <- paste0("RSF BACKUP DATA FILE for ",
                           paste0(sort(facilities[rsf_pfcbl_id %in% selected_rsf_pfcbl_id,facility_name]),collapse=", ")
                           ," ",
                           format.Date(now(tzone="EST"),"%Y-%m-%d %Hh%M"),
                           ".csv.gz")
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
          !any(selected_rsf_pfcbl_id %in% facilities$rsf_pfcbl_id)) selected_rsf_pfcbl_id <- program$rsf_pfcbl_id
      
      backup_export <- DBPOOL %>% export_backup_data_to_csv(rsf_pfcbl_id.family=selected_rsf_pfcbl_id,
                                                            exporting_user_id=USER_ID())
      
      if (file.exists(file)) {
        file.remove(file)
      }
      
      fwrite(x=backup_export,
             file=file,
             compress="gzip")
      
      # openxlsx::saveWorkbook(wb=programs_export,
      #                        file=file,
      #                        overwrite=TRUE)
      
      return (file)      
      
      incProgress(amount=1.0,message="Completed")
    })
  }
)

