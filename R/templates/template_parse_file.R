template_parse_file <- function(pool,
                                template_file,
                                reporting_user_id,
                                source_note=NA,
                                parse_rsf_pfcbl_id=NULL, #For those templates that do not report the entity (like pdf files), this is manually specified at upload
                                auto_delete_old_versions=TRUE,
                                parse_file.retry=TRUE,
                                status_message=function(...) {}) {
  
#Setups
  {
    if (length(template_file) != 1) stop("One file must be provided to template_parse_file")
    #if (length(rsf_program_id) != 1) stop("Only one program ID allowed, or NA for new/undefined program")
  
    if (!all(file.exists(template_file))) stop(paste0("File note found: ",template_file))
  
    if (!grepl("\\.xlsx?|\\.csv|\\.csv\\.gz|.pdf|.txt$",template_file,ignore.case = TRUE)) {
      status_message(class="error","Error: Only .xlsx files can be uploaded: ",template_file," is not allowed.\n")
      #.xls files are prohibbted because openxlsx package cannot read them and this package is used to manage the excel sheets, downloads, etc.
      if (grepl("\\.xls$",template_file,ignore.case = TRUE)) status_message(class="info","Older .xls files cannot be uploaded.  In Excel, use 'Save As' to save the file to a modern version format.\n")
      
      status_message(class="error","Unable to continue.\n")
      return (NULL)
    }
    
    t1 <- Sys.time()
    
    status_message(class="warning","\n\nParsing template: ",basename(template_file),"\n")
    
    #Latency check
    {
      measure_baseline_latency <- function(pool) {
        start_time <- Sys.time()
        
        # Checkout a connection and execute the lightest possible query
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT TRUE;")
        })
        
        end_time <- Sys.time()
        
        # Return time difference in milliseconds
        latency_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000
        return(latency_ms)
      }
      latency <- measure_baseline_latency(pool=pool)
      
      if (latency > 1000) {
        status_message(class="warning",
                       paste0("\n\nWARNING: latency between Jason and RSF Database is currently very slow: ",round(latency/1000,2)," seconds\nCheck internet connectivity or try again when internet traffic is less if delays continue\n\n"))
        Sys.sleep(3)
      }
    }    
    
    template <- db_dashboard_load_report(pool=pool,
                                      template_file=template_file,
                                      reporting_user_id=reporting_user_id,
                                      rsf_data_sheet="RSF_DATA")
    
    rsf_indicators <- db_indicators_get_labels(pool=pool)
    if (empty(rsf_indicators)) {
      stop("Failed to load RSF_INDICATORS")
    }
    
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
  
  #Parse the template
  #Differentiate between RSF templates and general IFC QR templtes
  {
    #it is NOT a valid RSF template
    if (is.null(template)) { 
      
      #setups
      {    
        
        nregions <- NULL
        snames <- NULL
        
        is_excel <- grepl("\\.xlsx$",template_file,ignore.case = TRUE)
        if (is_excel==TRUE) {   
          excelwb <- tryCatch({ openxlsx2::wb_load(file=template_file) },
                              warning=function(w) {
                                status_message(conditionMessage(w))
                                suppressWarnings(openxlsx2::wb_load(file=template_file))
                              })
          
          nregions <- excelwb$get_named_regions()
          snames <- excelwb$sheet_names
          
          setDT(nregions)
          # nregions <- c(openxlsx::getNamedRegions(template_file))
          # snames <- c(openxlsx::getSheetNames(template_file))
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
          else if (tolower(file_ext(template_file))=="txt") {
            
            template_lines <- readLines(con=template_file,n=4)
            tkey <- unique(grep("^<KEY>",template_lines,value=T))
            
            tname <- NULL
            if (length(tkey)==1) {
              tkey <- gsub("<KEY>","",tkey)
              tkey <- dbGetQuery(pool,"
                select rt.template_name
                from p_rsf.reporting_templates rt
                where rt.template_key = $1::text",
                params=list(toupper(tkey)))
              
              if (empty(tkey)) {
                stop(paste0("It looks like you're trying to upload a Jason agreement configuration template? A <KEY> can be found in the file, but the value '",tkey,
                            "' is not a recognized template key"))
              }
              tname <- tkey$template_name
              
              sname <- unique(grep("^<SYSNAME>",template_lines,value=T))
              sname <- unique(gsub("<SYSNAME>","",sname))
              
              if (length(sname)==1) {
                ids <- db_get_rsf_pfcbl_id_by_sys_name(pool=pool,
                                                       sys_names=sname)
                if (!empty(ids) && nrow(ids)==1) {
                  if (!is.null(parse_rsf_pfcbl_id) && !(as.numeric(parse_rsf_pfcbl_id)==as.numeric(ids$rsf_pfcbl_id))) {
                    stop(paste0("Failed to match parse request SYSID ",parse_rsf_pfcbl_id," with agreement config request and file SYSNAME '",sname,"'"))
                  }
                  #this will set/reset the value passed to template_parse_file()
                  parse_rsf_pfcbl_id <- as.numeric(ids$rsf_pfcbl_id)
                }
              } else {
                stop(paste0("It looks like you're trying to upload a Jason agreement configuration template? A <SYSNAME> can be found in the file, but the value '",sname,
                            "' is not a recognized Facility. If the facility has been entirely deleted, it needs to be re-setup from its setup file before configuring its agreement"))
              }
            } 
            tname
          }
          
          else if (tolower(file_ext(template_file))=="pdf") {
            
            if (!length(parse_rsf_pfcbl_id) || all(is.na(suppressWarnings(as.numeric(parse_rsf_pfcbl_id))),na.rm=T)) {
              stop("PDF uploads require a SYSID for upload: parse_rsf_pfcbl_id cannot be missing")
            }
            
            pfcbl_category <- dbGetQuery(pool,"select pfcbl_category from p_rsf.rsf_pfcbl_ids where rsf_pfcbl_id=$1::int",parse_rsf_pfcbl_id)
            
            if (!pfcbl_category %in% "facility") {
              stop("Only RSA agreements can be uploaded for .pdf documents.  When uploading an RSA, the RSF Program must be selected from the main drop-down menu AND ALSO the facility/client must be selected in the drop-down menu 'Client Filter' in the Datasets/Uploads List pane")
            }
            #only RSA template can be defined as pdf.
            "IFC-RSA-TEMPLATE"
          }
          ##################
          #NON JASON TEMPLATES#
          ##################
          else if (any(nregions$name=="Template_ID",na.rm=T)) {
            
            template_id <- unlist(wb_to_df(excelwb,named_region = "Template_ID",col_names = F))
            # # template_id <- openxlsx::read.xlsx(xlsxFile=template_file,
            # #                                    namedRegion = "Template_ID")
            # template_id <- names(template_id)
            found <- dbGetQuery(pool,"
              select exists(select * from p_rsf.reporting_templates rt where rt.template_key ~* $1::text)::bool as template_exists",
              params=list(template_id))
            if (any(unlist(found),na.rm=T)) {
              template_id
            } else {
              NULL
            }
          }
          #Sheet names expected to be:
          #"1. Summary" & "2. Current QReport"
          #And QDD named receive either of S_DET or S_QDD depending on the template's version.
          else if (length(grep("(summary)|(current qreport)",snames,ignore.case=T))==2 & 
                   any(grepl("S_DET|S_QDD",nregions$name,ignore.case=F))) {
            
            #template_format
            "IFC-QR-TEMPLATE2018"
            
          } 
          
          else {
            
            if (length(grep("(summary)|(current qreport)",snames,ignore.case=T)) > 0 ||
                any(grepl("S_DET|S_QDD",nregions$name,ignore.case=F))) {
              
              #paste0(grep("(summary)|(current qreport)",snames,ignore.case=T,value=T),collapse=", ")
              #paste0(grep("S_DET|S_QDD",nregions,ignore.case=F,value=T))
              
              status_message(class="error",
                             "It looks like you are trying to upload an IFC RSF QReport template?\n",
                             "Jason identifies templates by reading from Sheets called 'Summary' and 'Current QReport' (or 1. Summary and 2. Current QReport)\n",
                             "And it expects a defined named rage of S_DET or S_QDD\n",
                             "This sheet defines these sheets (there should be two and only two): \n",
                             paste0(paste0("[",grep("(summary)|(current qreport)",snames,ignore.case=T,value=T),"]"),collapse=" & ")," \n",
                             "and these named ranges (there may be one or two):\n", 
                             paste0(grep("S_DET|S_QDD",nregions$names,ignore.case=F,value=T)),"\n",
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
        #in case lookup is passed the templateID
        template_name <- template_lookup$template_name
      }
      
      if (template_name=="IFC-QR-TEMPLATE2018") {

        template <- parse_template_IFC_QR2018(pool=pool,
                                              template_lookup = template_lookup,
                                              template_file=template_file,
                                              rsf_indicators=rsf_indicators,
                                              reporting_user_id=reporting_user_id,
                                              status_message = status_message,
                                              CALCULATIONS_ENVIRONMENT=CALCULATIONS_ENVIRONMENT)
        
        if (all(is.na(template))) {
          status_message(class="error",paste0("Failed to parse template for: ",template_file,"/",template_format))
          stop(paste0("Failed to parse template for: ",template_file,"/",template_format))
        }
        
        template$template_source_reference <- "RSF QR2018 Template"
        template$template_ids_method <- "rsf_id"
        
      }
      else if (template_name=="IFC-QR-TEMPLATE2025") {
        
        template <- parse_template_IFC_QR2025(pool=pool,
                                              template_lookup = template_lookup,
                                              template_file=template_file,
                                              rsf_indicators=rsf_indicators,
                                              reporting_user_id=reporting_user_id,
                                              status_message = status_message,
                                              CALCULATIONS_ENVIRONMENT=CALCULATIONS_ENVIRONMENT)
        
        if (all(is.na(template))) {
          status_message(class="error",paste0("Failed to parse template for: ",template_file,"/",template_format))
          stop(paste0("Failed to parse template for: ",template_file,"/",template_format))
        }

        template$template_source_reference <- "RSF QR2025 Template"
        template$template_ids_method <- "rsf_id"
        
      }
      else if (template_name=="RSF-CSV-BACKUP-TEMPLATE") {
        
        template <- parse_template_csv_backup_data(pool=pool,
                                                   template_lookup = template_lookup,
                                                   template_file=template_file,
                                                   reporting_user_id=reporting_user_id,
                                                   rsf_indicators=rsf_indicators)
      }
      
      else if (template_name=="RSF-CSV-TEMPLATE") {
        
        template <- parse_template_csv(pool=pool,
                                       template_id = template_lookup$template_id,
                                       csv_file=template_file,
                                       rsf_indicators=rsf_indicators,
                                       status_message = status_message)
        
        template$template_source_reference <- "csv_file"
        template$template_ids_method <- "rsf_id"
        
      }
      
      else if (template_name=="IFC-RSA-TEMPLATE") {
        
        ids <- dbGetQuery(pool,"
                          select 
                            rsf_pfcbl_id
                          from p_rsf.rsf_pfcbl_ids 
                          where rsf_pfcbl_id = $1::int
                            and pfcbl_category in ('program','facility')",
                          params=list(parse_rsf_pfcbl_id))
        
        #if this is a text file, parse_rsf_pfcbl_id will be read-in while determining the template_name validity.
        if (empty(ids) || is.na(ids$rsf_pfcbl_id)) {
          stop(paste0("IFC-RSA-TEMPLATE must pass rsf_facility_id but facility could not be found for: ",parse_rsf_pfcbl_id))
        }
        
        template <- parse_template_RSA(pool=pool,
                                       template_id = template_lookup$template_id,
                                       for_rsf_pfcbl_id=ids$rsf_pfcbl_id, #This is checked in parse template
                                       template_file=template_file,
                                       rsf_indicators=rsf_indicators,
                                       rsf_indicator_formulas=db_indicators_get_formulas(pool=pool),
                                       rsf_check_formulas=db_checks_get_formulas(pool=pool),
                                       reporting_user_id=reporting_user_id,
                                       status_message = status_message)
        
        template$template_source_reference <- "RSA Configuration File"
        template$template_ids_method <- "pfcbl_id" #set as pfcbl_id for simplicity, but this file cannot create new entities (or match any entities)
      }
      
      else {
        stop(paste0("Failed to find parse instructions for template: ",template_format))
      }
      
      
      template$template_id <- template_lookup$template_id
      template$template_name <- template_lookup$template_name
      template$template_key <- template_lookup$template_key
      template$data_integrity_key <- as.character(NA)
      
      template$template_settings <- list()
      template$template_settings$is_complete_portfolio <- template_lookup$is_complete_portfolio
      template$template_settings$template_is_reportable <- template_lookup$is_reportable
      
    } 
    
    #it IS a valid RSF template
    else { #It IS an RSF template

      #PFCBL templates are far less likely to experience these types of errors.
      #More importantly they are more likely to upload/overwrite data that will correct such errors
      #And very likely to be generated via a web UI upload where the upload is unrelated to an error and a failure will cause a user to lose data
      #and have no idea what caused the failure.
      template$fail_on_incomplete_cohorts <- FALSE

      
      
      if (template$template_name=="RSF-ENTITIES-TEMPLATE") {
        
        template <- parse_template_rsf_create_entities(pool=pool,
                                                       template=template,
                                                       rsf_indicators=rsf_indicators,
                                                       reporting_user_id=reporting_user_id)
      }
    
      #Generated through ad-hoc updates in the Dashboard
      else if (template$template_name=="PFCBL-EDITOR-TEMPLATE") {
        
        template <- parse_template_pfcbl_editor_report(pool=pool,
                                                       template=template,
                                                       template_file=template_file,
                                                       reporting_user_id=reporting_user_id,
                                                       rsf_indicators=rsf_indicators)
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
    if (is.null(template$cohort_pfcbl_id) || all(is.na(template$cohort_pfcbl_id))) {
      stop("Template was unable to resolve cohort_pfcbl_id")
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
  #testing:
  #template$template_data[grepl("[a-z]_EUR$",indicator_name),indicator_name:=gsub("_EUR$","",indicator_name)]
  #template$template_data[grepl("_$",indicator_name),indicator_name:=gsub("_$","",indicator_name)]
  #template$template_data[grepl("[a-z]_EUR$",indicator_name)]
  #template$template_data[grepl("loan_original_balance",indicator_name)]
  
  {
   
    #setup reporting_flags
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
    
    # (1) Parse indicators, ensure they're valid for this program
    # (2) Parse indicator formats and flag/omit invalid data formats and options 
    #     !important: this must be done before hashvalues and further controls to ensure comparisons are being made against normalized data (eg, trimmed, etc)
    #     especially before matching IDs using things like name where normalized name is trimmed and upper case and only useful to compare using standardized formatting
    # (3) Get program settings to control further processing behavior
    
    {
      if (!all(c("cohort_pfcbl_id",
                 "rsf_indicators",
                 "reporting_asof_date",
                 "template_ids_method",
                 "template_data") %in% names(template))) stop("Templates must return: cohort_pfcbl_id, rsf_indicators,  reporting_import, reporting_asof_date, template_source_reference, template_ids_method and template_data")
      
      reqs <- c("reporting_asof_date",
                "indicator_name",
                "reporting_submitted_data_unit",
                "reporting_submitted_data_value",
                "reporting_submitted_data_formula",
                "reporting_template_row_group")
      
      if (length(setdiff(reqs,
                         names(template$template_data)))>0) {
        
        stop(paste0("Template Data must define: ",
                    paste0(reqs,collapse=", ")))
      
      }
      
      if (!template$template_ids_method %in% c("rsf_id","pfcbl_id")) stop("Template IDs method must define either 'rsf_id' or 'pfcbl_id'")
      
      if (template$template_ids_method=="pfcbl_id" && !any(names(template$template_data)=="SYSID")) {
        stop("Templates defining pfcbl_id lookup must define SYSID column in template_data")
      }
      
      if (length(template$reporting_asof_date) != 1) {
        stop("Template must report one (and only one) reporting date")
      }
      
      # if (!all(template$reporting_asof_date %in% template$template_data$reporting_asof_date) && length(template$template_data$reporting_asof_date) > 0) {
      #   stop(paste0("Template reporting_asof_date ",template$reporting_asof_date," must be present in template_data.  Must data dates are: ",
      #               paste0(unique(template$template_data$reporting_asof_date),collapse=", ")))
      # }
      
      #Templates imported via fread return an "IDate" "Date" class that can later cause conflicts when rbindlist with regular Date classes;
      #for all other circumstances, this turns dates into dates and is otherwise not useful...
      template$template_data[,
                             reporting_asof_date:=as.Date(reporting_asof_date)]
    }
    
    #Parsing template data
    {
      
      status_message("Parsing template data.\n")
      
      # if (empty(template$template_data)) {
      #   
      #   stop("Template contains no data after omitting unrecognized/unsubscribed indicators.  Ensure program is subscribed to indicators and labels are properly defined.")
      # }
      #if unknown indicator name is submitted, NA indicator_id. This will be filtered later after rsf_pfcbl_ids are identified along with relevant subscriptions
      template$template_data[template$rsf_indicators,
                             `:=`(indicator_id=i.indicator_id,
                                  indicator_sys_category=i.indicator_sys_category,
                                  data_category=i.data_category),
                             on=.(indicator_name)]
      
      template$template_data <- unique(template$template_data)
     
      #Instances where templates repeat indicators on multiple rows for convenience sake of the user but use different labels, aliases of labels or language combination of
      #labels. So merge these all together.
      
      #parse_data_formats adds a list column: data_flags_new
      #this is because we do not yet know the rsf_pfcbl_id associated with this line-item entry.
      #and therefore, we keep it tied to the data point so that after it is processed, we can disaggregate these flags and assign it to the appropraite rsf_pfcbl_id,
      #after it is known.
      template$template_data <- parse_data_formats(template_data=template$template_data,
                                                   rsf_indicators=template$rsf_indicators)
     
      
      
      
      #Fixed currency vs LCU currency redundancies
      #The templates themselves should filter these out.  But in case they don't...
      {
        #unit_fx_indicator_id is the base indicator in LCU
        unit_fx_indicators <- rsf_indicators[!is.na(unit_fx_indicator_id),.(unit_fx_indicator_id,indicator_id,data_unit,indicator_name)]
        if (any(unit_fx_indicators$indicator_id %in% unique(template$template_data$indicator_id,na.rm=T))) {
        
          template$template_data[,
                      `:=`(unit_fx_defined=NA,
                           omit=NA)]
  
          template$template_data[unit_fx_indicators,
                      unit_fx_defined:=mapply(grepl,pattern=i.data_unit,x=data_unit,MoreArgs=list(ignore.case=T)),
                      on=.(indicator_id)]
  
          template$template_data[,
                      omit:=is.na(unit_fx_defined) & any(!is.na(unit_fx_defined),na.rm=T),
                      by=.(reporting_template_row_group,reporting_template_data_rank)]
          
          #template$template_data[omit==T]
          template$template_data <- template$template_data[omit==FALSE]
        }  
        
      }
    } 
    
    #Validate template_reporting_row_group: ensures max/min and sorting of reporting_template_row_group are as expected
    #Starts with a row number, upper case dataset name and/or sheet name
    {
      if (any(grepl("^[[:digit:]]+[A-Z_-]+$",template$template_data$reporting_template_row_group)==FALSE)) {
        first <- template$template_data$reporting_template_row_group[which(!grepl("^[[:digit:]]+[A-Z_-]+$",template$template_data$reporting_template_row_group))[[1]]]
        
        stop(paste0("template_data$reporting_template_row_group expects format: ROW-numberROW-name. Where ROW-name is Letters or _ or - (not numbers or other punctuation).  First malformed row group is: ",first))
      }
    }
    
    #Good practice for all templates to add this.  But it's really only an issue if duplicated data is being reported, which most usually (intentionally) occurs when 
    #the same data point is repeated in a template either for convenience sake, or as a different currency value using a current fx rate.
    if (!any(names(template$template_data)=="reporting_template_data_rank")) {
      dups <- template$template_data[,
                                     .(n=.N,
                                       duplicates=paste0(data_submitted,collapse=" & ")),
                                     by=.(reporting_template_row_group,
                                          indicator_id,
                                          indicator_name,
                                          reporting_asof_date)][n>1]
      
      if (!empty(dups)) {
        stop(paste0("Template coding error (contact your system admin): Template ",
        template$template_name," has reported duplicate data for metrics:\n\n",
        paste0(unique(dups$indicator_name),collapse=", "),"\n\n",
        " but the parse_template() has not created a column for reporting_template_data_rank. Verify that the indicator names and alises are correctly assigned for this template for ",
        " these metrics. Duplicates may be incorrectly appearing as a result of mis-assigning indicator names to the wrong column name"))
      }
      
      template$template_data[,reporting_template_data_rank:=1:.N]
    
    
    
    
    } else {

      
      #nrow(unique(template$template_data[,.(reporting_template_row_group,reporting_template_data_rank)])) != nrow(template)
      bad_rows <- c(which(!template$template_data[,.(reporting_template_row_group,reporting_template_data_rank)][,c(1:.N)]==1:nrow(template$template_data)),
                    template$template_data[,.(n=.N,r=.I),by=.(reporting_template_row_group,reporting_template_data_rank)][n>1,r])
      if (length(bad_rows)) {
        
        status_message(class="error",
                       paste0("parse_template() function for ",template$template_name," sets reporting_template_row_group and reporting_template_data_rank but these do not uniquely identify all ",
                   nrow(template$template_data)," rows of data\n:",
                   paste0(capture.output(template$template_data[bad_rows]),collapse="\n")))
        
        template$template_data[,omit:=FALSE]
        template$template_data[,
                               `:=`(n=.N,
                                    omit=.N>1 & (1:.N)>1),
                               .(reporting_template_row_group,reporting_template_data_rank)]
        bad_duplicates <- template$template_data[n>1,
                                                 .(rsf_pfcbl_id=NA,
                                                   indicator_id,
                                                   reporting_asof_date,
                                                   check_name="sys_reporting_data_discarded",
                                                   check_message=paste0("Template import error resulted in ambiguous data on ",
                                                                        reporting_template_row_group,"#",reporting_template_data_rank," for: \n",
                                                                        paste0(paste0("'",indicator_name,"' = ",data_submitted),collapse=" AND \n"),
                                                                        "System will keep: ",unique(.SD[omit==F,paste0(indicator_name,"=",data_submitted)]),"\n",
                                                                        "And DSICARD: ",paste0(unique(.SD[omit==T,paste0(indicator_name,"=",data_submitted)]),collapse=" AND \n"))),
                                                 by=.(reporting_template_row_group,reporting_template_data_rank)]
        template$pfcbl_reporting_flags <- rbindlist(list(template$pfcbl_reporting_flags,
                                                         bad_duplicates[,.(rsf_pfcbl_id,indicator_id,reporting_asof_date,check_name,check_message)]))
        template$template_data <-  template$template_data[omit==FALSE]
        template$template_data[,`:=`(omit=NULL,n=NULL)]
      }
      
      set(template$template_data,
          j="reporting_template_data_rank",
          value=frank(template$template_data[,.(reporting_template_row_group,reporting_template_data_rank)],ties.method="dense"))
    }
    
   
    cross_references <- unique(template$template_data[,
                                               .(reporting_template_group=gsub("^[[:digit:]]+","",reporting_template_row_group),
                                                 indicator_name,
                                                 indicator_id,
                                                 data_value)])[,.(n=.N,
                                                                  data_values=list(unique(data_value)),
                                                                  data_counts=length(unique(data_value)),
                                                                  reporting_template_group),
                                                                   by=.(indicator_name,indicator_id)][n>1]
    
    
    cross_references <- template$template_data[,
                                               .(reporting_template_group=gsub("^[[:digit:]]+","",reporting_template_row_group),
                                                 indicator_name,
                                                 indicator_id,
                                                 data_value,
                                                 data_unit)]
    
    cross_references[,n:=length(unique(reporting_template_group)),
                    by=.(indicator_name,indicator_id)]
    cross_references <- cross_references[n>1]
    
    if (!empty(cross_references)) {
    
      cross_references[,
                       n:=length(unique(paste0(data_value," ",data_unit))),
                       by=.(indicator_name,indicator_id)] 
      
      cross_references <- cross_references[n>1]
      if (!empty(cross_references)) {

        setorder(cross_references,
                 indicator_id,
                 reporting_template_group)
        
        cross_references[,cross_id:=1:.N,
                         by=.(indicator_id)]
        
        if (!empty(template$template_headers)) {
          cross_headers <- template$template_headers[indicator_id %in% unique(cross_references$indicator_id)]
          setorder(cross_headers,
                   indicator_id,
                   data_source_index)
          cross_headers[,cross_id:=.GRP,
                        by=.(indicator_id,
                             data_source_index)]
          cross_references[cross_headers,
                           label:=i.label,
                           on=.(indicator_id,
                                cross_id)]
          cross_references <- cross_references[,
                                               message:=paste0(reporting_template_group," SHEET @",label,": ",indicator_name," {",data_value,"}\n")]
          
        } else {
          cross_references <- cross_references[,
                                               message:=paste0("SHEET ",reporting_template_group," ",indicator_name," {",data_value,"}\n")]
        }
        
        
        stop(paste("Indicators cannot be reported on different template data sheets with different values:\n",paste0(cross_references$message,collapse="\n AND\n"),
                   "\nEnsure that headers are properly labled in each section to ensure the correct indicator is mapped to the right header"))
      }
    }
    cross_references <- NULL
    
    {
      #NOTE: A cohort with a "today" reporting date will have a valid reporting_asof_date.  That cohort MAY ALSO have FUTURE reporting_asof_dates in its
      #template_data in which case, cohort triggers will insert those future dates as valid dates.
      #However, a template with a "tomorrow" reporting date will not be allowed.
      #Ie, I can know today what future requirements will be (because they're contracted to be so).
      #But I cannot report future data from a future perspective.
      valid_date_range <- dbGetQuery(pool,"
                                     select 
                                      ids.pfcbl_category,
                                      ids.created_in_reporting_asof_date::text as first_date,
                                      ((date_trunc('quarter',(now()::date)::timestamp with time zone + '3 mons'::interval) - interval '1 day')::date)::text as current_date
                                      from p_rsf.rsf_pfcbl_ids ids
                                      where ids.rsf_pfcbl_id = $1::int",
                                     params=list(template$cohort_pfcbl_id))
      
     
      if (any(is.na(valid_date_range$first_date),is.na(valid_date_range$current_date))) {
        stop(paste0("Failed to determine valid reporting date ranges for this template report. Verify that the IFC project ID is valid"))
      }
      
      if (template$reporting_asof_date < as.Date(valid_date_range$first_date)) {
        stop(paste0(valid_date_range$first_date," is the earliest reporting allowed for this ",valid_date_range$pfcbl_category))
      }
      
      if (template$reporting_asof_date > as.Date(valid_date_range$current_date)) {
        stop(paste0("Future reporting is not allowed: Template reports QDD for '",template$reporting_asof_date,"' which is ",(template$reporting_asof_date-today())," DAYS into the future. ",
                    valid_date_range$current_date," is the maximum allowed reporting date"))
      }
    }
  }  
  
  {
    
    if (template$template_name %in% c("PFCBL-EDITOR-TEMPLATE",
                                      "RSF-ENTITIES-TEMPLATE")) {

      template$fail_on_incomplete_cohorts <- FALSE

    }
    
    #Do we need to create new fx metrics due to currency requirements?
    {
      currencies <- unique(template$template_data[rsf_indicators[,.(indicator_id,data_type)],
                                           on=.(indicator_id)
                                           ][data_type %in% c("currency","currency_ratio") | 
                                             indicator_sys_category %in% c("entity_currency_unit","entity_local_currency_unit")
                                             ][,.(data_type,
                                                  unit=fifelse(indicator_sys_category %in% c("entity_currency_unit","entity_local_currency_unit"),
                                                        yes=data_value,
                                                        no=data_unit))])[unit != "LCU"]
      
      ratios <- unique(currencies[data_type=="currency_ratio",unit])
      currencies <- unique(currencies[data_type!="currency_ratio",unit])
      currencies <- unique(c(currencies,unlist(strsplit(ratios,"/"))))
      
      currencies <- currencies[currencies %in% CALCULATIONS_ENVIRONMENT$VALID_CURRENCIES]
      currencies <- unlist(lapply(data.frame(t(unique(CJ(currencies,currencies)))),
                                 function(x) { if (length(x) != 2 || x[1] != x[2]) paste0(sort(x),collapse="/") }),use.names=F)
      
      ratios <- data.table(fx_ratio=unique(c(ratios,currencies)))
      
      ratios <- ratios[,.(data_category=c("facility","global")),
                       by=.(fx_ratio)]
      
      required_ratios <- rsf_indicators[data_type=="currency_ratio",.(indicator_id,data_unit,data_category)
                               ][ratios,
                                 on=.(data_unit=fx_ratio,
                                      data_category),
                                 nomatch=NA][is.na(indicator_id)]
      
      if (!empty(required_ratios)) {
        
        for (i in 1:nrow(required_ratios)) {
          status_message(class="warning",
                         paste0("\nCreating new ",required_ratios[i,data_category]," FX ratio: ",required_ratios[i,data_unit],"\n"))
        }
        
        # conn <- poolCheckout(pool)
        # dbBegin(conn)
        # dbRollback(conn)
        new_indicators <- poolWithTransaction(pool,function(conn) { 
          
          dbExecute(conn,"create temporary table _temp_currency_codes(alphabetic_lookup_ratio text,
                                                                      data_category text)
                        on commit drop;")
          
          dbAppendTable(conn,
                        name="_temp_currency_codes",
                        value=unique(required_ratios[,.(alphabetic_lookup_ratio=data_unit,
                                                        data_category)]))
          
          
          dbExecute(conn,"
            delete from _temp_currency_codes tcc
            where exists(select true from p_rsf.indicators ind 
                         where ind.data_category = tcc.data_category
                           and ind.data_type = 'currency_ratio'
                           and p_rsf.fx_currency_ratio_in_alphabetic_order(ind.data_unit) = 
                               p_rsf.fx_currency_ratio_in_alphabetic_order(tcc.alphabetic_lookup_ratio))")
          
          #dbGetQuery(conn,"select * from _temp_currency_codes")
          dbExecute(conn,"DO $$ BEGIN
                        if exists(select true from _temp_currency_codes) then
                        
                          -- Just to be sure
                          update _temp_currency_codes tcc
                          set alphabetic_lookup_ratio = p_rsf.fx_currency_ratio_in_alphabetic_order(tcc.alphabetic_lookup_ratio);
                        
                          with new_currencies as materialized (
                          	select 
                          		alphabetic_lookup_ratio,
                          		'sys_global_fx_' || regexp_replace(alphabetic_lookup_ratio,'/','_') as indicator_name,
                          		true as is_system,
                          		tcc.alphabetic_lookup_ratio || ' WBG Exchange Rate' as label,
                          		tcc.data_category,
                          		alphabetic_lookup_ratio || ' WBG corporate exchange rate, internally managed by System' as definition
                          	from _temp_currency_codes tcc
                          	where tcc.data_category = 'global'
                          	
                          	union all
                          	
                          	select
                          		alphabetic_lookup_ratio,
                          		'facility_fx_defined_exchange_rate_' || regexp_replace(alphabetic_lookup_ratio,'/','_') as indicator_name,
                          		false as is_system,
                          		'Facility defined FX rate ' || tcc.alphabetic_lookup_ratio as label,
                          		tcc.data_category,
                          		alphabetic_lookup_ratio || 'FX rate as defined by the facility and reported in the QR (not automatically calculated). Used when RSA requires a source external to IFC as basis of truth for FX reporting.' as definition

                          	from _temp_currency_codes tcc
                          	where tcc.data_category = 'facility'
                          ),
                          new_indicators as materialized (
                          	insert into p_rsf.indicators(indicator_name,
                          															 data_category,
                          															 data_type,
                          															 is_calculated,
                          															 is_system,
                          															 definition,
                          															 data_unit,
                          															 modification_time,
                          															 is_required,
                                                         default_subscription)
                           select 
                          	ncu.indicator_name,
                          	ncu.data_category,
                          	'currency_ratio' as data_type,
                          	true as is_calculated,
                          	ncu.is_system,
                          	ncu.definition,
                          	alphabetic_lookup_ratio as data_unit,
                          	(timeofday())::timestamptz as modification_time,
                          	false as is_required,
                            true as default_subscription
                           from new_currencies ncu
                           returning indicator_id,label_id,data_unit,data_category,indicator_name
                          ),
                          new_labels as materialized (
                            
                            insert into p_rsf.labels(label_id,label_key,primary_label,label_id_group)
                              select 
                              ni.label_id,
                              'EN' as label_key,
                              ncu.label as primary_label,
                              'indicators' as label_id_group
                            from new_indicators ni
                            inner join new_currencies ncu on ncu.alphabetic_lookup_ratio = ni.data_unit
                                                         and ncu.data_category = ni.data_category
                          )
                            insert into p_rsf.indicator_formulas(indicator_id,formula,overwrite,formula_title)
                            select 
                              ni.indicator_id,
                              'get_IFC_FX_rate(exchange_rate_date=global_reporting_quarter_end_date.current,
                                               currency_code_ratio=' || ni.data_unit || ')' as formula,
                              case when ni.data_category = 'global' then 'allow'
                                   else 'deny' end 
                              as overwrite,
                              
                              case when ni.data_category = 'global' 
                                   then 'FX ' || ni.data_unit || ' IFC Official Rate'
                                   else 'FX ' || ni.data_unit || ' Compare with IFC Official Rate (Do not overwrite)' 
                              end as formula_title
                            from new_indicators ni;
                          
                          
                                            
                          RAISE INFO 'NEW FX INDICATOR CREATED';
                      end if;
                  
                  END $$;")
          
          dbGetQuery(conn,"
            select
              ind.indicator_id,
              ind.data_unit,
              ind.data_category
            from p_rsf.indicators ind
            inner join _temp_currency_codes tcc on tcc.alphabetic_lookup_ratio = ind.data_unit
                                               and tcc.data_category = ind.data_category")
        })
        
        setDT(new_indicators)
        if (!empty(new_indicators) && isTRUE(parse_file.retry)) {
          #Because template may have had its currency ratio indicator defined that we failed to read-in properly because the indicator and
          #its label didn't exist yet.  Now that it does, let's re-try.
          return (template_parse_file(pool=pool,
                                      template_file=template_file,
                                      reporting_user_id=reporting_user_id,
                                      source_note=source_note,
                                      parse_rsf_pfcbl_id=parse_rsf_pfcbl_id,
                                      auto_delete_old_versions=auto_delete_old_versions,
                                      parse_file.retry=FALSE,
                                      status_message=status_message))
        }
      }      
    }
    
    #Initialize users, if so defined
    {
      users <- template$template_data[indicator_sys_category=="entity_user_correspondence",data_value]
      if (length(users)) {
        users <- strsplit(users,split="[;,]")[[1]]
        users <- rbindlist(lapply(users,function(u) {
          if (grepl("<.*>$",u)) {
            data.frame(name=trimws(gsub("^([^>]+)<.*>$","\\1",u)) , email=trimws(gsub("^[^>]+<(.*)>$","\\1",u)))
          }
        }))
        
        users[,c("user","domain"):=tstrsplit(email,"@")]
        users <- users[grepl("^(ifc|miga|worldbank|worldbankgroup)\\.(org|onmicrosoft.com)$",domain)]
        users[,user:=tolower(trimws(user))]
        
        accounts <- dbGetQuery(pool,"
        select vai.login_name as user,vai.account_id,apg.permission_name
        from p_rsf.view_account_info vai
        left join users.view_account_permissions_granted apg on apg.account_id = vai.account_id
                                                            and apg.rsf_pfcbl_id = $2::int
                                                            and apg.permission_name = 'WRITE'
        where vai.login_name = any(select unnest(string_to_array($1::text,',')::text[])::text)",
        params=list(paste0(users$user,collapse=","),
                    template$cohort_pfcbl_id))
        setDT(accounts)
        
        accounts <- accounts[users,
                             on=.(user)]
        
        
        accounts <- accounts[is.na(account_id) | is.na(permission_name)]
        #We need to create accounts here or grant existing accounts permissions
        if (!empty(accounts)) {
          
          accounts[is.na(name) | nchar(trimws(name))==0,
                   name:=user]
          
          if (!empty(accounts[is.na(account_id)])) {
            create <- accounts[is.na(account_id),.(name,email)]
            for (i in 1:nrow(create)) {

              new_account_id <- DBPOOL_APPLICATIONS %>% dbGetQuery("
              select * 
              from arlapplications.accounts_create(v_application_hashid => $1::text,
                                                   v_request_by_account_id => $2::text,
                                                   v_name => $3::text,
                                                   v_login => $4::text)",
                                                   params=list(RSF_MANAGEMENT_APPLICATION_ID,
                                                               ACCOUNT_SYS_ADMIN$account_id,
                                                               create[i]$name,
                                                               create[i]$email))
              
              accounts[email==create[i]$email,
                       account_id:=new_account_id]
            } 
          }
          
          if (!empty(accounts[is.na(permission_name)])) {
            grant <- accounts[is.na(permission_name)]
            
            
            for (i in 1:nrow(grant)) {
            
              status_message(class="warning",
                             paste0("\n\nGRANTING WRITE PRMISSIONS to <",grant[i,email],"> from template's IFC Correspondence Field\n"))  
              Sys.sleep(2)
              
              dbExecute(pool,"
              insert into users.permissions(account_id,rsf_pfcbl_id,sys_name,granted,notes)
              select 
              vai.account_id,
              sn.rsf_pfcbl_id,
              sn.sys_name,
              coalesce(roles.role_permissions,0) as granted,
              concat('Permissions set by QR Template: ',($2::text),' on ',(select timeofday()::date)) as notes
              from p_rsf.view_account_info vai,
                   p_rsf.view_rsf_pfcbl_id_current_sys_names sn,
                   users.roles 
              where vai.account_id = $1::text
                and sn.rsf_pfcbl_id = 0
                and roles.role_name = 'VIEWER'
              on conflict(account_id,sys_name)
              do update
              set granted = permissions.granted|excluded.granted,
                  notes = excluded.notes;",
                                   params=list(grant[i]$account_id,
                                               template$template_file))
              
              dbExecute(pool,"
              insert into users.permissions(account_id,rsf_pfcbl_id,sys_name,granted,notes)
              select 
              vai.account_id,
              sn.rsf_pfcbl_id,
              sn.sys_name,
              coalesce(roles.role_permissions,0) as granted,
              concat('Permissions set by QR Template: ',($3::text),' on ',(select timeofday()::date)) as notes
              from p_rsf.view_account_info vai,
                   p_rsf.view_rsf_pfcbl_id_current_sys_names sn,
                   users.roles 
              where vai.account_id = $1::text
                and sn.rsf_pfcbl_id = $2::int
                and sn.pfcbl_category = 'facility'
                and roles.role_name = 'USER'
              on conflict(account_id,sys_name)
              do update
              set granted = permissions.granted|excluded.granted,
                  notes = excluded.notes;",
              params=list(grant[i]$account_id,
                          template$cohort_pfcbl_id,
                          template$template_file))
            }
          }
          
        }
      }
    }
    
    #Create a new reporting cohort, user-created cohort will be parent of any subsequent sys-created cohorts
    #must be created first, for this chronology to have an entry for potenitally new rsf_ids yet to be created under a specific reporting cohort
    {
      #should be null for most templates.
      #But some templates (eg, parse_template_RSA) creates its own import and assigns it in order to facilitate asigning cohort ID to setup parameters
      if (is.null(template$reporting_import)) {
        reporting_import <- db_reporting_import_create(pool=pool,
                                                       import_rsf_pfcbl_id=template$cohort_pfcbl_id,
                                                       import_user_id=template$reporting_user_id,
                                                       reporting_asof_date=template$reporting_asof_date,
                                                       template_id=template$template_id,
                                                       file_path=template_file,
                                                       import_comments=NA,
                                                       auto_delete_old_versions=auto_delete_old_versions) 
        
        template$reporting_import <- reporting_import
      }  
    
      template$template_source <- NULL #Now available in reporting_cohort$source_name
      template$cohort_pfcbl_id <- NULL #Now available in reporting_cohort$reporting_rsf_pfcbl_id
      template$template_source_reference <- NULL #Now available in reporting_cohort$source_reference, automatically adjusted
    } 
  }
  
 
  
  #if template/program monitors headers, save them
  if (!empty(template$template_headers)) {
    
    #conn <- poolCheckout(pool);dbBegin(conn)
    poolWithTransaction(pool,function(conn) {
      dbExecute(conn,"
        create temp table theaders(import_id int,
                                   rsf_pfcbl_id int,
                                   indicator_id int,
                                   template_header text,
                                   template_header_formula text,
                                   template_header_position text)
        on commit drop;")
      
      dbAppendTable(conn,
                    name="theaders",
                    value=template$template_headers[,.(indicator_id,
                                                       template_header=label,
                                                       template_header_formula=label_formula,
                                                       template_header_position=data_source_index)])
      
      dbExecute(conn,"update theaders
                set import_id = $1::int,
                    rsf_pfcbl_id = $2::int",
                params=list(template$reporting_import$import_id,
                            template$reporting_import$import_rsf_pfcbl_id))
      
      #In case there's no indicator ID uploaded for this header
      dbExecute(conn,"update theaders
                set indicator_id = ind.indicator_id
                from p_rsf.rsf_pfcbl_ids ids
                inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
                where ind.indicator_sys_category = 'entity_reporting'
                  and theaders.indicator_id is NULL")
      
      dbExecute(conn,"
        insert into p_rsf.reporting_import_template_headers(import_id,
                                                            rsf_pfcbl_id,
                                                            indicator_id,
                                                            template_header,
                                                            template_header_formula,
                                                            template_header_position)
        select distinct
          import_id,
          rsf_pfcbl_id,
          indicator_id,
          template_header,
          template_header_formula,
          template_header_position        
        from theaders
        on conflict do nothing;")
    })
    
    
  }

  if (template$reporting_asof_date != template$reporting_import$reporting_asof_date) {
    stop(paste0("Mismatch error: template$reporting_asof_date = ",
                as.character(template$reporting_asof_date)," and reporting_import$reporting_asof_date = ",
                as.character(template$reporting_import$reporting_asof_date)))
  }

  template$parse_time <- as.numeric(Sys.time()-t1,"secs")
  return (template)
}
