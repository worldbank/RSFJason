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
  
    if (!grepl("\\.xlsx?|\\.csv$",template_file,ignore.case = TRUE)) {
      status_message(class="error","Error: Only .xlsx or .csv files can be uploaded.\n")
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
            "RSF-CSV-TEMPLATE"
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
            
            status_message(class="error","Unable to identify appropriate template format for file: ",template_file,"\n")
            stop("Unable to continue.")
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
      }
      
      #Generated through "Create New" UI in Programs Setup when creating a new facility, etc through UI
      else if (template$template_name=="RSF-ENTITIES-TEMPLATE") {
        
        template <- parse_template_rsf_create_entities(pool=pool,
                                                       template=template)
        
      }
    
      #Generated through ad-hoc updates in the Dashboard
      else if (template$template_name=="PFCBL-EDITOR-TEMPLATE") {
        
        template <- parse_template_pfcbl_editor_report(pool=pool,
                                                       template=template,
                                                       template_file=template_file,
                                                       reporting_user_id=reporting_user_id,
                                                       rsf_indicators=rsf_indicators)
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
                                           fail_on_check_submitted_indicators = unique(template$template_data$indicator_name))
      
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
  
  template$parse_time <- as.numeric(Sys.time()-t1,"secs")
  return (template)
}
