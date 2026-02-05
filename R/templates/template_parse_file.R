template_parse_file <- function(pool,
                                template_file,
                                reporting_user_id,
                                source_note=NA,
                                parse_rsf_pfcbl_id=NULL, #For those templates that do not report the entity (like pdf files), this is manually specified at upload
                                status_message=function(...) {}) {
  
#Setups
  {
    if (length(template_file) != 1) stop("One file must be provided to template_parse_file")
    #if (length(rsf_program_id) != 1) stop("Only one program ID allowed, or NA for new/undefined program")
  
    if (!all(file.exists(template_file))) stop(paste0("File note found: ",template_file))
  
    if (!grepl("\\.xlsx?|\\.csv|\\.csv\\.gz|.pdf$",template_file,ignore.case = TRUE)) {
      status_message(class="error","Error: Only .xlsx or .csv files can be uploaded: ",template_file," is not allowed.\n")
      #.xls files are prohibbted because openxlsx package cannot read them and this package is used to manage the excel sheets, downloads, etc.
      if (grepl("\\.xls$",template_file,ignore.case = TRUE)) status_message(class="info","Older .xls files cannot be uploaded.  In Excel, use 'Save As' to save the file to a modern version format.\n")
      
      status_message(class="error","Unable to continue.\n")
      return (NULL)
    }
    
    t1 <- Sys.time()
    
    status_message(class="none","Parsing template: ",basename(template_file),"\n")
    
    
    template <- db_export_load_report(pool=pool,
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
          else if (tolower(file_ext(template_file))=="pdf") {
            
            pfcbl_category <- dbGetQuery(pool,"select pfcbl_category from p_rsf.rsf_pfcbl_ids where rsf_pfcbl_id=$1::int",upload_rsf_pfcbl_id)
            
            if (!pfcbl_category %in% "facility") {
              stop("Only RSA agreements can be uploaded for .pdf documents.  When uploading an RSA, the RSF Program must be selected from the main drop-down menu AND ALSO the facility/client must be selected in the drop-down menu 'Client Filter' in the Datasets/Uploads List pane")
            }
            #only RSA template can be defined as pdf.
            "IFC-RSA-TEMPLATE"
          }
          ##################
          #NON JASON TEMPLATES#
          ##################
          else if (any(nregions=="Template_ID")) {
            
            template_id <- openxlsx::read.xlsx(xlsxFile=template_file,
                                               namedRegion = "Template_ID")
            template_id <- names(template_id)
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
        #in case lookup is passed the templateID
        template_name <- template_lookup$template_name
      }
      
      if (template_name=="IFC-QR-TEMPLATE") {

        template <- parse_template_IFC_QR(pool=pool,
                                          template_lookup = template_lookup,
                                          template_file=template_file,
                                          rsf_indicators=rsf_indicators,
                                          status_message = status_message,
                                          CALCULATIONS_ENVIRONMENT=CALCULATIONS_ENVIRONMENT)
        
        if (all(is.na(template))) {
          status_message(class="error",paste0("Failed to parse template for: ",template_file,"/",template_format))
          stop(paste0("Failed to parse template for: ",template_file,"/",template_format))
        }
        
        template$template_source_reference <- "SLGP Template"
        template$template_ids_method <- "rsf_id"
        
      }
      else if (template_name=="IFC-QR-TEMPLATE2025") {
        
        template <- parse_template_IFC_QR2025(pool=pool,
                                              template_lookup = template_lookup,
                                              template_file=template_file,
                                              rsf_indicators=rsf_indicators,
                                              status_message = status_message,
                                              CALCULATIONS_ENVIRONMENT=CALCULATIONS_ENVIRONMENT)
        
        if (all(is.na(template))) {
          status_message(class="error",paste0("Failed to parse template for: ",template_file,"/",template_format))
          stop(paste0("Failed to parse template for: ",template_file,"/",template_format))
        }

        template$template_source_reference <- "SLGP Template"
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
                            rsf_program_id,
                            rsf_facility_id 
                          from p_rsf.rsf_pfcbl_ids 
                          where rsf_pfcbl_id = $1::int",parse_rsf_pfcbl_id)
        if (empty(ids) || is.na(ids$rsf_facility_id)) {
          stop(paste0("IFC-RSA-TEMPLATE must pass rsf_facility_id but facility could not be found for: ",parse_rsf_pfcbl_id))
        }
        
        template <- parse_template_RSA(pool=pool,
                                       template_id = template_lookup$template_id,
                                       rsf_facility_id=ids$rsf_facility_id, #This is checked in parse template
                                       template_file=template_file,
                                       rsf_indicators=rsf_indicators,
                                       rsf_indicator_formulas=db_indicators_get_formulas(pool=pool),
                                       rsf_check_formulas=db_checks_get_formulas(pool=pool),
                                       reporting_user_id=reporting_user_id,
                                       status_message = status_message)
        
        template$template_source_reference <- "RSA Setup PDF File"
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
      template$template_settings$template_has_static_row_ids <- template_lookup$template_has_static_row_ids
      template$template_settings$template_is_reportable <- template_lookup$is_reportable
      
    } 
    
    #it IS a valid RSF template
    else { #It IS an RSF template

      #PFCBL templates are far less likely to experience these types of errors.
      #More importantly they are more likely to upload/overwrite data that will correct such errors
      #And very likely to be generated via a web UI upload where the upload is unrelated to an error and a failure will cause a user to lose data
      #and have no idea what caused the failure.
      template$fail_on_incomplete_cohorts <- FALSE

      # #Generated through download program archive and download setup files
      # if (template$template_name=="RSF-SETUP-TEMPLATE") {
      #   
      #   
      #   template <- parse_template_rsf_setup(pool=pool,
      #                                        template=template,
      #                                        template_file=template_file,
      #                                        reporting_user_id=reporting_user_id,
      #                                        rsf_indicators=rsf_indicators)
      # }
      # 
      # #Generated through "Create New" UI in Programs Setup when creating a new facility, etc through UI
      
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
        
        #pisses off users if they don't know why their edits didn't work.
        #template$program_settings$on_upload_cohort_fail_on_check_class <- "None"
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
      
      
      if (length(setdiff(c("reporting_asof_date",
                           "indicator_name",
                           "reporting_submitted_data_unit",
                           "reporting_submitted_data_value",
                           "reporting_submitted_data_formula",
                           "reporting_template_row_group"),
                         names(template$template_data)))>0) {
        
        stop("Template Data must define: reporting_asof_date,indicator_name,reporting_submitted_data_unit,reporting_submitted_data_value,reporting_submitted_data_formula,reporting_template_row_group,<optional>labels_submitted")
      }
      
      if (!template$template_ids_method %in% c("rsf_id","pfcbl_id")) stop("Template IDs method must define either 'rsf_id' or 'pfcbl_id'")
      
      if (template$template_ids_method=="pfcbl_id" && !any(names(template$template_data)=="SYSID")) {
        stop("Templates defining pfcbl_id lookup must define SYSID column in template_data")
      }
      
      if (is.null(template$reporting_asof_date) || is.null(template$template_data$reporting_asof_date) || length(template$reporting_asof_date) != 1 || 
          !all(template$reporting_asof_date %in% template$template_data$reporting_asof_date)) stop("Template's reporting_asof_date must uniquely match template's data program_id")
      
      #Templates imported via fread return an "IDate" "Date" class that can later cause conflicts when rbindlist with regular Date classes
      template$template_data[,
                             reporting_asof_date:=as.Date(reporting_asof_date)]
    }
    
    {
      
      status_message("Parsing template data.\n")
      
      if (empty(template$template_data)) stop("Template contains no data after omitting unrecognized/unsubscribed indicators.  Ensure program is subscribed to indicators and labels are properly defined.")
      
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
     
      
      futures <- template$template_data[indicator_sys_category=="entity_creation_date" & !is.na(data_value)][ymd(data_value) > reporting_asof_date]
      if (!empty(futures)) {
        max_future <- futures[data_value==max(data_value),paste0(unique(indicator_name)," ",unique(as.character(data_value)))]
        futures <- futures[,
                           .(message=paste0("Future creation date on ",.N,
                                            " rows starting from {",min(as.character(data_value)),"} on ",reporting_template_row_group[[1]],
                                            " to {",max(as.character(data_value)),"} on ",reporting_template_row_group[[.N]])),
                           by=.(indicator_name)]
        
        
        for (i in 1:nrow(futures)) status_message(class="error",
                                                  paste0("Creation date is in the future: template reporting date is ",
                                                         template$reporting_asof_date,
                                                         " but ",futures[i,indicator_name]," has: ",futures[i,message],"\n"))
        
        stop(paste0("Template failed to parse: entities are reported to have been created in the future.  Correct reporting date or correct the creation date to the past: ",max_future," cannot be greater than template reporting date ",template$reporting_asof_date))
      }
      futures <- NULL
      
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

      if (!all(unique(template$template_data[,.(reporting_template_row_group,reporting_template_data_rank)])[,c(1:.N)]==1:nrow(template$template_data))) {
        stop(paste0("parse_template() function for ",template$template_name," sets reporting_template_row_group and reporting_template_data_rank but these do not uniquely identify all ",
                   nrow(template$template_data)," rows of data"))
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
        stop("Future reporting is not allowed: ",valid_date_range$current_date," is the maximum allowed reporting date")
      }
    }
  }  
  
  {
    {
      #NOTE: Oct-2025
      #This is all but obsolete ... Barely used.
      #And in current versions almost everything is paramaeterized around program/facility level
      program_settings <- dbGetQuery(pool,"select 
                                            vrps.rsf_program_id,
                                            vrps.setting_name,
                                            vrps.setting_value,
                                            vrps.default_data_type,
                                            vrps.setting_group
                                          from p_rsf.view_rsf_program_settings vrps
                                          inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_program_id = vrps.rsf_program_id
                                          where ids.rsf_pfcbl_id = $1::int",
                                     params=list(template$cohort_pfcbl_id))
      
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
    
      reporting_import <- db_reporting_import_create(pool=pool,
                                                     import_rsf_pfcbl_id=template$cohort_pfcbl_id,
                                                     import_user_id=template$reporting_user_id,
                                                     reporting_asof_date=template$reporting_asof_date,
                                                     template_id=template$template_id,
                                                     file_path=template_file,
                                                     import_comments=NA,
                                                     auto_delete_old_versions=TRUE) 
      template$reporting_import <- reporting_import
      
      template$template_source <- NULL #Now available in reporting_cohort$source_name
      template$cohort_pfcbl_id <- NULL #Now available in reporting_cohort$reporting_rsf_pfcbl_id
      template$template_source_reference <- NULL #Now available in reporting_cohort$source_reference, automatically adjusted
    } 
  }
  
 
  
  #if template/program monitors headers, save them
  if (template$get_program_setting("on_upload_save_template_headers") &
      !empty(template$template_headers)) {
    
    #conn <- poolCheckout(pool);dbBegin(conn)
    poolWithTransaction(pool,function(conn) {
      dbExecute(conn,"
        create temp table theaders(import_id int,
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
                set import_id = $1::int,
                    rsf_pfcbl_id = $2::int",
                params=list(reporting_import$import_id,
                            reporting_import$import_rsf_pfcbl_id))
      dbExecute(conn,"
        insert into p_rsf.reporting_import_template_headers(import_id,
                                                            rsf_pfcbl_id,
                                                            indicator_id,
                                                            template_header,
                                                            template_header_position)
        select 
          import_id,
          rsf_pfcbl_id,
          indicator_id,
          template_header,
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
