template_parse_process_and_upload <- function(pool,
                                              rsf_program_id=NA,
                                              reporting_user_id,
                                              template_files,
                                              source_note,
                                              email_report=FALSE,
                                              delete_after_upload=FALSE,
                                              status_message=function(...) {},
                                              continue_on_error=FALSE,
                                              delete_on_error=TRUE) {
  
  #delete_on_error <- FALSE
  rsf_program_id <- as.numeric(rsf_program_id)
  zip_file <- NULL
  if (any(dir.exists(template_files))) {
    stop("Only individual files may be uploaded, not a whole directory.  If you seek to upload a directory, try zipping files into an archive and uploading one archive file")
  }
  
  if (length(template_files)==1 && all(file_ext(template_files)=="zip")) {
    zip_file <- template_files
    manifest <- zip::zip_list(template_files)
    
    zip::unzip(zipfile=template_files,
               files=NULL,
               exdir=dirname(template_files),
               overwrite=T)
    
    print(paste0(list.files(dirname(template_files)),collapse=","))

    template_files <- paste0(dirname(template_files),.Platform$file.sep,manifest$filename)
    
  }
  
  exists <- file.exists(template_files)
  if (!all(exists)) stop(paste0("File note found: ",template_files[!exists]))
  
  template_files <- sort(template_files)
  ppu_results <- NULL
  
  #tf <- template_files[[1]]
  for (tf in template_files) {
    
    current_cohort_id <- NULL
    if(SYS_PRINT_TIMING) debugtime(reset=T)
    #status_message(class="none","Reading file",tf,"\n",reset=T)
    
    #PARSE FILE
    {
      template <- tryCatch({
        template_parse_file(pool=pool,
                            rsf_program_id=rsf_program_id,
                            template_file=tf,
                            reporting_user_id=reporting_user_id,
                            source_note=source_note,
                            status_message=status_message)
      },
      warning = function(w) {
        status_message(class="error",conditionMessage(w))
        NULL;
      },
      error = function(e) {
        status_message(class="error",conditionMessage(e))
        NULL;
      })
      
      if (is.null(template) && continue_on_error==FALSE) {
        stop("Failed to parse file")
      } else if (is.null(template)) {
        status_message(class="error","Continuing on error")
        next;
      }

      if (is.na(rsf_program_id)) {
          rsf_program_id <- template$rsf_program_id
      }
    }
    
    current_cohort_id <- template$reporting_cohort$reporting_cohort_id
    
    if (is.null(current_cohort_id)) {
      stop("Failed to create reporting_cohort_id")
    }
    
    #PROCESS
    template <- tryCatch({
      template_process(pool=pool,
                       template=template,
                       status_message=status_message)
    },
    warning = function(w) {
      status_message(class="error",conditionMessage(w))
      NULL;
    },
    error = function(e) {
      status_message(class="error",conditionMessage(e))
      NULL;
    })
    
    #On error
    if (is.null(template)) {
      status_message("Rolling back upload #",current_cohort_id)
      
      if (delete_on_error==TRUE) {
        dbExecute(pool,"
                        delete from p_rsf.reporting_cohorts rc
                        where rc.reporting_cohort_id = $1::int",
                  params=list(current_cohort_id))
      }
      if (continue_on_error==FALSE) {
        stop("Failed to process file")
      } else {
        status_message(class="error","Continuing on error")
        next;
      }
    }    
    
    template <- tryCatch({
      template_upload(pool=pool,
                      template=template,
                      status_message=status_message)
    },
    warning = function(w) {
      status_message(class="error",conditionMessage(w))
      NULL;
    },
    error = function(e) {
      status_message(class="error",conditionMessage(e))
      NULL;
    })
    
    #On error
    if (is.null(template)) {
      
      if (delete_on_error==TRUE) {
        status_message("Rolling back upload #",current_cohort_id)
        dbExecute(pool,"
                        delete from p_rsf.reporting_cohorts rc
                        where rc.reporting_cohort_id = $1::int",
                  params=list(current_cohort_id))
      }

      if (continue_on_error==FALSE) {
        stop("Failed to process file")
      } else {
        status_message(class="error","Continuing on error")
        next;
      }
    }    
    
    dm <- debugtime(reset=TRUE)
    if (is.null(template)==FALSE) {
     
      
      template$log <- ""
      if (length(dm) > 0) {
        template$log <- dm
      }
      
      result <- data.table(reporting_cohort_id=current_cohort_id,
                           rsf_program_id=rsf_program_id,
                           reporting_asof_date=template$reporting_asof_date,
                           template_source=template$reporting_cohort$source_name,
                           parse_time=template$parse_time,
                           process_time=template$process_time,
                           upload_time=template$upload_time,
                           calculate_time=template$calculate_time,
                           check_time=template$check_time,
                           backup_time=template$backup_time,
                           total_time=template$parse_time+
                             template$process_time+
                             template$upload_time,
                           log=paste0(template$log,collapse=" | "))
      
      if (which(tf==template_files) < length(template_files)) {
        status_message(clear.panel=TRUE)
      }
      
      
      # cohort_flags <- dbGetQuery(pool,"
      #                        select 
      #                         reporting_cohort_id,
      #                         data_current,
      #                         data_checks_critical_active,
      #                         data_checks_error_active,
      #                         data_checks_warning_active,
      #                         data_checks_info_active
      #                        from p_rsf.reporting_cohort_info rcd
      #                        where rcd.reporting_cohort_id = $1::int",
      #                        params=list(template$reporting_cohort$reporting_cohort_id))
      # 
      # if (!empty(cohort_flags)) {
      #   
      #   cohort_flags <- as.data.table(cohort_flags)
      #   cohort_flags <- cohort_flags[,.(critical_flags=sum(data_checks_critical_active),
      #                                   error_flags=sum(data_checks_error_active),
      #                                   warning_flags=sum(data_checks_warning_active),
      #                                   info_flags=sum(data_checks_info_active))]
      #   
      #   cohort_flags[,total_flags:=sum(critical_flags,error_flags,warning_flags,info_flags)]
      #   
      #   status_message(class="info",paste0("Total flags for upload: ",cohort_flags$total_flags,"\n"))
      #   if (cohort_flags$critical_flags > 0) status_message(class="error",paste0("Critical: ",cohort_flags$critical_flags," flags\n"))
      #   if (cohort_flags$error_flags > 0) status_message(class="error",paste0("Error: ",cohort_flags$error_flags," flags\n"))
      #   if (cohort_flags$warning_flags > 0) status_message(class="warning",paste0("Warning: ",cohort_flags$warning_flags," flags\n"))
      #   if (cohort_flags$info_flags > 0) status_message(class="info",paste0("Info: ",cohort_flags$info_flags," flags\n"))
      # } else {
      #   status_message(class="info",paste0("Zero flags for upload\n"))
      # }
      #if(SYS_PRINT_TIMING)  debugtime("template_upload","COMPLETED SETTING COHORT DATA COUNTS")
      
      #status_message(class="none",paste0("Completed with new dataset#",template$reporting_cohort$reporting_cohort_id,"\n"))
      
      status_message(class="info",
                     paste0("Completed file",basename(tf),"\n"),
                     paste0("\n\nTotal: ",
                            round(as.numeric(as.difftime(result$total_time,units="secs")))," seconds",
                            " for ",result$template_source,"\n"))
      
      
      poolWithTransaction(pool,function(conn) {
        
        dbExecute(conn,"create TEMP table _temp_upload_results(reporting_cohort_id int,
                                                               parse_time numeric,
                                                               process_time numeric,
                                                               upload_time numeric,
                                                               backup_time numeric,
                                                               total_time numeric,
                                                               log text)
                  on commit drop;")
        
        dbAppendTable(conn,
                      name="_temp_upload_results",
                      value=result[,.(reporting_cohort_id,
                                           parse_time=as.numeric(parse_time,units="secs"),
                                           process_time=as.numeric(process_time,units="secs"),
                                           upload_time=as.numeric(upload_time,units="secs"),
                                           backup_time=as.numeric(backup_time,units="secs"),
                                           total_time=as.numeric(total_time,units="secs"),
                                           log=as.character(log))])
        
        dbExecute(conn,"update p_rsf.reporting_cohort_info rcd
                        set metadata = rcd.metadata || 
                            jsonb_build_object('timing',
                              jsonb_build_object('parse_time',parse_time,
                                                 'process_time',process_time,
                                                 'upload_time',upload_time,
                                                 'backup_time',backup_time,
                                                 'total_time',total_time),
                                                'log',log)
                        from _temp_upload_results tur
                        where tur.reporting_cohort_id = rcd.reporting_cohort_id")
      })
      
      
      ppu_results <- rbindlist(list(ppu_results,result))
    }    
  }
  
  if (delete_after_upload==TRUE) {
    if (!is.null(zip_file) && file.exists(zip_file)) {
      file.remove(zip_file)
    }
    file.remove(template_files)
  }
  

  # if (!empty(ppu_results)) {
  #  
  #  if (nrow(ppu_results) > 0) {
  #     for (i in 1:nrow(ppu_results)) status_message(class="info",
  #                                               paste0("Total: ",
  #                                                      round(as.numeric(as.difftime(ppu_results[i,total_time],units="secs")))," seconds",
  #                                                                   " for ",ppu_results[i,template_source],"\n"))
  #  }
  # }    
    

    
  #   
  #   if (email_report==TRUE) {
  #     processed_reporting_cohort_ids <- upresults$reporting_cohort_id
  #     excels <- list()
  #     for (reporting_cohort_id in processed_reporting_cohort_ids) {
  #       status_message(class="info","Generating email for upload #",reporting_cohort_id,"...\n")
  #       
  #       
  #       excel <- DBPOOL %>% export_reporting_cohort_to_excel(reporting_cohort_id=reporting_cohort_id,
  #                                                            exporting_user_id=reporting_user_id,
  #                                                            view="cohort-related",
  #                                                            flags.only=FALSE,
  #                                                            indicator_type_filters=NA,
  #                                                            indicator_category_filters=NA)
  #       excel_file <- paste0("RSF",reporting_cohort_id,".xlsx")
  #       saveWorkbook(excel,file=excel_file,overwrite=T)
  #       excels[[length(excels)+1]] <- excel_file
  #     }
  #     
  #     from <- "sheitmann@ifc.org"
  #     to <- "sheitmann@ifc.org"
  #     subject <- "RSF Upload Report"
  #     mailbody <- paste0("Dear User, Please find attached upload repo4x6rts for ",today())
  #     
  #     print(paste0(getwd(),"/",excels))
  #     attachments <- mapply(mime_part,x=paste0(getwd(),"/",excels),name=excels)
  #     sendmail(from=from,
  #              to=to,
  #              subject=subject,
  #              msg= as.list(c(body=mailbody,attachments)))
  #     
  #     status_message(class="info","Please check your email for your upload status report.\n")
  #   }
  

  poolClose(pool)
  pool <- NULL
  #uploads <- processed$uploads
  return(ppu_results)
}
