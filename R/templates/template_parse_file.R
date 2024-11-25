template_parse_file <- function(pool,
                                rsf_program_id,
                                template_file,
                                reporting_user_id,
                                source_note=NA,
                                status_message=function(...) {}) {
  
#Setups
  {
    if (length(template_file) != 1) stop("One file must be provided to template_parse_file")
    if (length(rsf_program_id) != 1) stop("Only one program ID allowed, or NA for new/undefined program")
  
    if (!all(file.exists(template_file))) stop(paste0("File note found: ",template_file))
  
    if (!grepl("\\.xlsx?|\\.csv|\\.csv\\.gz$",template_file,ignore.case = TRUE)) {
      status_message(class="error","Error: Only .xlsx or .csv files can be uploaded: ",template_file," is not allowed.\n")
      #.xls files are prohibbted because openxlsx package cannot read them and this package is used to manage the excel sheets, downloads, etc.
      if (grepl("\\.xls$",template_file,ignore.case = TRUE)) status_message(class="info","Older .xls files cannot be uploaded.  In Excel, use 'Save As' to save the file to a modern version format.\n")
      
      status_message(class="error","Unable to continue.\n")
      return (NULL)
    }
    
    t1 <- Sys.time()
    rsf_program_id <- suppressWarnings(as.numeric(rsf_program_id))
      
      status_message(class="none","Parsing template: ",basename(template_file),"\n")
      
      
      template <- db_export_load_report(pool=pool,
                                        template_file=template_file,
                                        reporting_user_id=reporting_user_id,
                                        rsf_data_sheet="RSF_DATA")
      
      rsf_indicators <- db_indicators_get_labels(pool=pool)
      
      if (any(rsf_indicators$redundancy_error,na.rm=T)) {
        bad_indicators <- rsf_indicators[redundancy_error==TRUE,
                                     .(indicator_name,
                                       labels)]
        bad_indicators <- bad_indicators[,unlist(labels,recursive=F),
                                         by=.(indicator_name)][redundancy_error==TRUE]
        
        if (nrow(bad_indicators) > 1) {
          
          
          setorder(bad_indicators,
                   label_normalized,
                   -is_primary)
          
          status_message(class="error","Error: Redundant indicator titles have been added for different indicators.  These MUST be corrected in Indicator Admin before new templates can be uploaded\n")
          
          ui <- tagList()
          for (i in 1:nrow(bad_indicators)) {
            status_message(class="none",
                           paste(unlist(bad_indicators[i,.(indicator_name,
                                                           key=paste0(key_type,"=",label_key),
                                                           paste0("'",label,"'"),
                                                           primary=ifelse(is_primary," (primary)"," (alias) <- should this one be deleted?"))]),collapse=" "),"\n")
          }
          status_message(class="error",
                         "If a template is using redundant lables (this is bad practice), these may be specified in RSF Setup -> Template Setup, where header instructions may be added to dis-ambiguate these labels\n")
        }
      }
  }  
  
  {
    #it is NOT a valid RSF template
    if (is.null(template)) { 
      
      #setups
      {    
        #Non pfcbl templates cannot (re)define their own program ID        
        if (is.na(rsf_program_id)) {
          stop("A target PROGRAM # must be selected")
        }
  
        if (empty(rsf_indicators)) {
          stop(paste0(paste0("RSF Program #",rsf_program_id," does not exists OR has no indicators to check or load.  Add indicator subscriptions for this program template.\n")))
        }
        
        nregions <- NULL
        snames <- NULL
        
        is_excel <- grepl("\\.xlsx$",template_file,ignore.case = TRUE)
        if (is_excel==TRUE) {    
          nregions <- c(openxlsx::getNamedRegions(template_file))
          snames <- c(openxlsx::getSheetNames(template_file))
        }    
        
        template_name <- {
          
          if (grepl("\\.csv$",template_file,ignore.case = TRUE)) {
            
            headers <- names(fread(file=template_file,nrows=0))
            if (setequal(headers,
                         c("SYSNAME",
                           "INDID",
                           "reporting_asof_date",
                           "indicator_name",
                           "data_value"))) {
              "RSF-CSV-BACKUP-TEMPLATE"
              
            } else { #TODO if valid template then meta data within the file name itself.
              "RSF-CSV-TEMPLATE"
            }
          }
          
          ##################
          #NON JASON TEMPLATES#
          ##################
          
          #Sheet names expected to be:
          #"1. Summary" & "2. Current QReport"
          #And QDD named receive either of S_DET or S_QDD depending on the template's version.
          else if (length(grep("(summary)|(current qreport)",snames,ignore.case=T))==2 & 
                   any(grepl("S_DET|S_QDD",nregions,ignore.case=F))) {
            
            #template_format
            "IFC-QR-TEMPLATE"
            
          } 
          
          else {
            
            if (length(grep("(summary)|(current qreport)",snames,ignore.case=T)) > 0 ||
                any(grepl("S_DET|S_QDD",nregions,ignore.case=F))) {
              
              #paste0(grep("(summary)|(current qreport)",snames,ignore.case=T,value=T),collapse=", ")
              #paste0(grep("S_DET|S_QDD",nregions,ignore.case=F,value=T))
              
              status_message(class="error",
                             "It looks like you are trying to upload an IFC RSF QReport template?\n",
                             "Jason identifies templates by reading from Sheets called 'Summary' and 'Current QReport' (or 1. Summary and 2. Current QReport)\n",
                             "And it expects a defined named rage of S_DET or S_QDD\n",
                             "This sheet defines these sheets (there should be two and only two): \n",
                             paste0(paste0("[",grep("(summary)|(current qreport)",snames,ignore.case=T,value=T),"]"),collapse=" & ")," \n",
                             "and these named ranges (there may be one or two):\n", 
                             paste0(grep("S_DET|S_QDD",nregions,ignore.case=F,value=T)),"\n",
                             "If this message sees multiple Sheet names, be sure to look in hidden sheets in your file and either delete or rename those that are not relevant")
              
              stop("Unable to identify template: possible IFC QR Template that has multiple sheets or incorrectly named sheets or named ranges.")
            } else {
            
              status_message(class="error","Unable to identify appropriate template format for file: ",template_file,"\n")
              stop("Unable to continue.")
            }
          }
        }
        
        template_lookup <- db_export_get_template(pool=pool,
                                                  template_name=template_name)
        
      }
      
      if (template_name=="IFC-QR-TEMPLATE") {

        template <- parse_template_IFC_QR(pool=pool,
                                          template_lookup = template_lookup,
                                          rsf_program_id=rsf_program_id,
                                          template_file=template_file,
                                          rsf_indicators=rsf_indicators,
                                          status_message = status_message)
        
        if (all(is.na(template))) {
          status_message(class="error",paste0("Failed to parse template for: ",template_file,"/",template_format))
          stop(paste0("Failed to parse template for: ",template_file,"/",template_format))
        }
        
        template$rsf_program_id <- rsf_program_id #only obtained via function argument for SLGP
        template$template_source_reference <- "SLGP Template"
        template$template_ids_method <- "rsf_id"
        
      }
      
      else if (template_name=="RSF-CSV-BACKUP-TEMPLATE") {
        
        template <- parse_template_csv_backup_data(pool=pool,
                                                   template_lookup = template_lookup,
                                                   template_file=template_file,
                                                   reporting_user_id=reporting_user_id,
                                                   rsf_indicators=rsf_indicators)
        
        if (!identical(as.numeric(template$rsf_program_id),as.numeric(rsf_program_id))) {
          stop("Exported program ID in backup file does not match program selected for file upload")
        }
      }
      
      else if (template_name=="RSF-CSV-TEMPLATE") {
        
        template <- parse_template_csv(pool=pool,
                                       template_id = template_lookup$template_id,
                                       rsf_program_id=rsf_program_id,
                                       csv_file=template_file,
                                       rsf_indicators=rsf_indicators,
                                       status_message = status_message)
        
        template$rsf_program_id <- rsf_program_id #only obtained via function argument for SLGP
        template$template_source_reference <- "csv_file"
        template$template_ids_method <- "rsf_id"
        
      }
      
      else {
        stop(paste0("Failed to find parse instructions for template: ",template_format))
      }
      
      
      template$template_id <- template_lookup$template_id
      template$template_name <- template_lookup$template_name
      template$template_key <- template_lookup$template_key
      template$data_integrity_key <- as.character(NA)
      
      template$template_settings <- list()
      template$template_settings$template_has_static_row_ids <- template_lookup$template_has_static_row_ids
      template$template_settings$template_is_reportable <- template_lookup$is_reportable
      
    } 
    
    #it IS a valid RSF template
    else { #It IS an RSF template
      
      
      #note: Only RSF-PROGRAMS-TEMPLATE is allowed to submit without a program_id
      if (is.na(rsf_program_id) && template$template_settings$template_is_setup==FALSE) {
        stop("NA rsf_program_id submitted and template is not a valid setup template")
      }
      
      if (!identical(as.integer(template$rsf_program_id),as.integer(rsf_program_id))) {
        if (is.na(template$rsf_program_id) && !is.na(rsf_program_id)) {
          template$rsf_program_id <- rsf_program_id
        } else if (is.na(rsf_program_id) && !is.na(template$rsf_program_id)) {
          message(paste0("Uploading data for template-defined PROGRAM #",template$rsf_program_id))
        } else if (template$template_settings$template_is_setup) {
          message(paste0("Warning: Changing target from PROGRAM #",rsf_program_id,". ",
                         basename(template_file)," is an RSF SETUP file that uploading to PROGRAM #",template$rsf_program_id))
        } else {
          stop(paste0("Reqeusted to upload template for PROGRAM #",rsf_program_id," but template specifies data for PROGRAM #",template$rsf_program_id))
        }
        
        #Argument and Template programs are differnet, but if we're here, it's allowed.
        #But ensure argument and template programs agree and rsf_indicators refer to correct program
        

      }
      
      #PFCBL templates are far less likely to experience these types of errors.
      #More importantly they are more likely to upload/overwrite data that will correct such errors
      #And very likely to be generated via a web UI upload where the upload is unrelated to an error and a failure will cause a user to lose data
      #and have no idea what caused the failure.
      template$fail_on_incomplete_cohorts <- FALSE
      
      if (is.na(template$rsf_program_id) && template$template_settings$template_is_setup==FALSE) {
        stop("Template does not define a PROGRAM #")
      }
      
      rsf_program_id <- template$rsf_program_id

      #Generated through download program archive and download setup files
      if (template$template_name=="RSF-SETUP-TEMPLATE") {
        
        
        template <- parse_template_rsf_setup(pool=pool,
                                             template=template,
                                             template_file=template_file,
                                             reporting_user_id=reporting_user_id,
                                             rsf_indicators=rsf_indicators)
        
        if (!is.na(rsf_program_id) && !identical(as.integer(rsf_program_id),as.integer(template$rsf_program_id))) {
          program_matches <- dbGetQuery(pool,"select rsf_program_id,rsf_name as name 
                                              from p_rsf.view_current_entity_names_and_ids
                                              where array[rsf_program_id] && string_to_array($1::text,',')::int[]
                                                and pfcbl_category = 'program'",
                                        params=list(paste0(rsf_program_id,",",template$rsf_program_id)))
          program_matches <- as.data.table(program_matches)
          status_message(class="error","Invalid program selected\n")
          status_message(class="info","Template is for '",program_matches[rsf_program_id==template$rsf_program_id,name],"' but selected program is '",program_matches[rsf_program_id!=template$rsf_program_id,name],"'\n")
          status_message(class="info","Action: Select correct program for this template; or upload correct template for this program; or selected templates are for different programs.")
          stop("Upload failed: unable to load data for one program into another program.")
          
        }
        
        #means parsing template setup actually created the program.
        #if it was not NA earlier, then the program exists and the setup file is re-uploading data and/or creating new information in the existing program.
        if (!identical(as.integer(template$rsf_program_id),as.integer(rsf_program_id))) {
          rsf_program_id <- template$rsf_program_id
        }
        
        #Setup templates are more likely to overwrite everything, anyway.
        #
      }
      
      #Generated through "Create New" UI in Programs Setup when creating a new facility, etc through UI
      else if (template$template_name=="RSF-ENTITIES-TEMPLATE") {
        
        template <- parse_template_rsf_create_entities(pool=pool,
                                                       template=template)
        
        # if (!"CRITICAL" %in% toupper(template$get_program_setting("on_upload_cohort_fail_on_check_class"))) {
        #   
        #   #Critical errors on create generally mean a real and legitimate timeline issue (eg, created a new facility in the future or something).
        #   #These should be nearly impossible, but if they exist, really don't allow it.
        #   template$program_settings$on_upload_cohort_fail_on_check_class <- "CRITICAL"   
        # }
        
        
      }
    
      #Generated through ad-hoc updates in the Dashboard
      else if (template$template_name=="PFCBL-EDITOR-TEMPLATE") {
        
        template <- parse_template_pfcbl_editor_report(pool=pool,
                                                       template=template,
                                                       template_file=template_file,
                                                       reporting_user_id=reporting_user_id,
                                                       rsf_indicators=rsf_indicators)
        
        #pisses off users if they don't know why their edits didn't work.
        #template$program_settings$on_upload_cohort_fail_on_check_class <- "None"
      } 
      
      else if (template$template_name=="PFCBL-DASHBOARD-TEMPLATE") {
        stop("Template Rewrite to conform to post-migration requirements")
        #Dashboard template enables users to download data from template into Excel, edit it and then re-upload it.  This may not be a meaningful use case
        #and better to push users to upload a standard reporting template or edit small changes in the PFCBL Editor?
        template <- parse_template_pfcbl_dashboard_report(pool=pool,
                                                          template_file=template_file,
                                                          rsf_indicators=rsf_indicators,
                                                          rsf_program_id=rsf_program_id)
        if (all(is.na(template))) {
          status_message(class="error",paste0("Failed to parse template for: ",template_file,"/",template_format))
          warning(paste0("Failed to parse template for: ",template_file,"/",template_format))
          return (NULL)
        }
        
        template$template_source_reference <- "RSF Editor Report"
        template$template_ids_method <- "pfcbl_id"
        
        
      }
      
      else {
        stop(paste0("Failed to find parse instructions for template: ",template$template_name))
      }
      
      
    ###########################################################################
    ###########################################################################
    ###########################################################################
    
    }    
  }
  
  { 
    if (is.null(template$rsf_program_id) || all(is.na(template$rsf_program_id))) {
      stop("Template was unable to resolve rsf_program_id")
    }
    
    if (any(sapply(template$template_data,class)=="factor")) {
      stop("template_parse_file created factors in template_data, which are not allowed.  Review template dispatch function")
    }
    
    if (!is.na(template$template_settings$template_is_reportable) && template$template_settings$template_is_reportable==FALSE) {
      stop(paste0("This template has been marked as non-reportable and cannot be uploaded again."))
    }
    
    
    template$rsf_indicators <- rsf_indicators
    template$template_file <- template_file
    template$template_source <- basename(template_file)
    template$reporting_user_id <- reporting_user_id
  
  }
  
  {
    {
      program_settings <- dbGetQuery(pool,"select 
                                            rsf_program_id,
                                            setting_name,
                                            setting_value,
                                            default_data_type,
                                            setting_group
                                          from p_rsf.view_rsf_program_settings vrps
                                          where vrps.rsf_program_id = $1::int",
                                     params=list(template$rsf_program_id))
      
      setDT(program_settings)
      
      settings <- dcast.data.table(program_settings,
                                   formula=rsf_program_id ~ setting_name,value.var="setting_value")
      
      for (sname in names(settings)) {
        setting <- program_settings[setting_name==sname]
        if (empty(setting)) next;
        
        if (setting$default_data_type=="logical") set(settings,i=NULL,j=setting$setting_name,value=as.logical(settings[[setting$setting_name]]))
        else if (setting$default_data_type %in% c("number","currency","currency_ratio","percent")) set(settings,i=NULL,j=setting$setting_name,value=as.numeric(settings[[setting$setting_name]]))
        else if (setting$default_data_type == "date") set(settings,i=NULL,j=setting$setting_name,value=as.Date(settings[[setting$setting_name]]))
        else  set(settings,i=NULL,j=setting$setting_name,value=toupper(as.character(settings[[setting$setting_name]])))
      }
      
      template$program_settings <- settings
      template$get_program_setting <- function(setting) {
        
        ps <- (get("template",envir=parent.env(environment())))$program_settings
        if (is.null(ps)) stop("Unable to locate program settings object in template")
        
        ps <- ps[[setting]]
        if (is.null(ps)) stop(paste0("Invalid program setting: ",setting,". Verify setting exists in database table p_rsf.program_settings"))
        if (is.na(ps) || length(ps)==0 || nchar(as.character(ps))==0) stop(paste0("Invalid program setting value: ",setting," is <NA> and must be specified."))
        ps
      }
    }

    if (template$template_name %in% c("PFCBL-EDITOR-TEMPLATE",
                                      "RSF-ENTITIES-TEMPLATE",
                                      "RSF-SETUP-TEMPLATE")) {

      template$fail_on_incomplete_cohorts <- FALSE
      
      if (template$template_name %in% c("PFCBL-EDITOR-TEMPLATE",
                                        "RSF-SETUP-TEMPLATE")) {
        
        template$program_settings$on_upload_cohort_fail_on_check_class <- "None"    
      }
      

    }
    
    #Create a new reporting cohort, user-created cohort will be parent of any subsequent sys-created cohorts
    #must be created first, for this chronology to have an entry for potenitally new rsf_ids yet to be created under a specific reporting cohort
    {
      reporting_pfcbl_categories <- unique(template$template_data[,
                                                                  .(indicator_name)
                                                                  ])[rsf_indicators[,
                                                                                    .(indicator_name,data_category)],
                                                         on=.(indicator_name),
                                                         nomatch=NULL
                                                         ][,unique(data_category)]
      
       
      
      reporting_cohort <- db_cohort_create(pool=pool,
                                           rsf_program_id=template$rsf_program_id,
                                           reporting_user_id=template$reporting_user_id,
                                           reporting_asof_date=template$reporting_asof_date,
                                           cohort_pfcbl_id=template$cohort_pfcbl_id,
                                           from_reporting_template_id=template$template_id,
                                           source_reference=template$template_source_reference,
                                           source_name=template$template_source, 
                                           source_note=source_note,
                                           reporting_pfcbl_categories=reporting_pfcbl_categories,
                                           fail_on_check_class = template$get_program_setting("on_upload_cohort_fail_on_check_class"),
                                           fail_on_check_submitted_indicators = unique(template$template_data$indicator_name),
                                           fail_on_incomplete_cohorts=template$fail_on_incomplete_cohorts)
      
      if (all(is.na(reporting_cohort))) stop(paste0("Failed to create a reporting cohort for ",source_name))
      
      status_message(class="info",paste0("Reporting created for ",reporting_cohort$source_reference,"\n"))
      
      template$reporting_cohort <- reporting_cohort
      template$template_source <- NULL #Now available in reporting_cohort$source_name
      template$cohort_pfcbl_id <- NULL #Now available in reporting_cohort$reporting_rsf_pfcbl_id
      template$template_source_reference <- NULL #Now available in reporting_cohort$source_reference, automatically adjusted
    } 
  }
  
  #create template$pfcbl_reporting_flags
  { 
    #template_parse_file may have already created and added this data table as a result of parsing indicators.
    #but if not, no errors were found.  Yay!  But create an empty table because we query it later when trying to upload any errors that may exist.
    if (is.null(template$pfcbl_reporting_flags)) {
      template$pfcbl_reporting_flags <- data.table(rsf_pfcbl_id=numeric(0),
                                                   indicator_id=numeric(0),
                                                   reporting_asof_date=as.Date(numeric(0)),
                                                   check_name=character(0),
                                                   check_message=character(0))
    } else {
      if (!setequal(names(template$pfcbl_reporting_flags),
                    c("rsf_pfcbl_id",
                      "indicator_id",
                      "reporting_asof_date",
                      "check_name",
                      "check_message"))) {
        stop(paste0("parse_template function for ",template_lookup$template_name," has added template$pfcbl_reporting_flags with incorrect columns: ",
                    paste0(names(template$pfcbl_reporting_flags),collapse=", ")))
      }
    }
  }
  
  #if template/program monitors headers, save them
  if (template$get_program_setting("on_upload_save_template_headers") &
      !empty(template$template_headers)) {
    
    #conn <- poolCheckout(pool);dbBegin(conn)
    poolWithTransaction(pool,function(conn) {
      dbExecute(conn,"
        create temp table theaders(reporting_cohort_id int,
                                   rsf_pfcbl_id int,
                                   indicator_id int,
                                   template_header text,
                                   template_header_position text)
        on commit drop;")
      
      dbAppendTable(conn,
                    name="theaders",
                    value=template$template_headers[,.(indicator_id,
                                                       template_header=label,
                                                       template_header_position=data_source_index)])
      
      dbExecute(conn,"update theaders
                set reporting_cohort_id = $1::int,
                    rsf_pfcbl_id = $2::int",
                params=list(reporting_cohort$reporting_cohort_id,
                            reporting_cohort$reporting_rsf_pfcbl_id))
      dbExecute(conn,"
        insert into p_rsf.reporting_cohort_template_headers(reporting_cohort_id,
                                                            rsf_pfcbl_id,
                                                            indicator_id,
                                                            template_header,
                                                            template_header_position)
        select 
          reporting_cohort_id,
          rsf_pfcbl_id,
          indicator_id,
          template_header,
          template_header_position        
        from theaders
        on conflict do nothing;")
    })
    
    
  }

  
  template$parse_time <- as.numeric(Sys.time()-t1,"secs")
  return (template)
}
