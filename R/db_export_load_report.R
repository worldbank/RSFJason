db_export_load_report <- function(pool,
                                  template_file,
                                  reporting_user_id=NULL,
                                  rsf_data_sheet="RSF_DATA") {
  
  is_excel <- grepl("\\.xlsx$",template_file,ignore.case = TRUE)
  if (!is_excel) {
    return (NULL)
  }
  
  #openxlsx has some bug where it can't read some types of workbooks with pivot tables
  #https://github.com/ycphs/openxlsx/issues/124
  excelwb <- tryCatch({
    openxlsx::loadWorkbook(template_file)
  },
  warning = function(w) { 
  
    wb <- suppressWarnings(openxlsx::loadWorkbook(template_file))
    message(paste0("Notice: Pivot tables can cause errors in loadWorkdbook.  Loading with suppressWarnings because: ",conditionMessage(w)))
    wb
  },
  error = function(e) { 
    stop(paste0("This file appears to be corrupted. \n",
                "Can it be opened regularly in Excel? \n",
                "Is it password protected? If so, open in Excel: \n",
                "  (1) select File -> Info -> Protect Workbook -> Encrypt with Password: Delete the password (leave the field empty). -> Click OK button \n",
                "  (2) select File -> Save As: Save a copy using a new file name to your local File Folder. \n",
                "  (3) Upload this new file into Jason \n",
                " \n",
                "Error when trying to read file: ",conditionMessage(e)))
  })
  
  #excelwb <- openxlsx::loadWorkbook()
  if (!inherits(excelwb,"Workbook")) stop("Required openxlsx Workbook input")
  
  #TODO: print("Todo: Enable db_export_load_report to verify upload_user_id is allowed to use specified reporting_template")
  
  readRegion <- function(excelwb,namedRegion) { 
      tryCatch({
        x <- names(openxlsx::readWorkbook(excelwb,
                                          namedRegion=namedRegion,
                                          sep.names=" "))
        x <- unique(x)
        if (length(x) !=1) return (NA)
        else return(x)
      },
      error = function(err) { NA },
      warning = function(war) { NA })
  }
  
  
  template_key <- readRegion(excelwb=excelwb,namedRegion="RSF_TEMPLATE_KEY")  #Either RSF_TEMPLATE_KEY
  if (!is.na(template_key)) {
    stop("Named region RSF_TEMPLATE_KEY has been deprecated.  Change to RSF_REPORT_KEY.  Report key may be REPORT_KEY:TEMPLATE_KEY or TEMPLATE_KEY values")
  }
  
  reporting_key <- readRegion(excelwb=excelwb,namedRegion="RSF_REPORT_KEY")   #OR RSF_REPORT_KEY:RSF_TEMPLATE_KEY must be defined

  if (is.na(reporting_key)) {
    #This isnt an RSF template, perhaps
    return (NULL)
  }
  
  data_integrity_key <- readRegion(excelwb=excelwb,namedRegion="RSF_DATA_INTEGRITY_KEY")
  
  template_reporting_date <- readRegion(excelwb=excelwb,namedRegion="RSF_REPORTING_DATE")
  template_reporting_entity <- readRegion(excelwb=excelwb,namedRegion="RSF_REPORTING_ENTITY")
  
  #sheetName <- "RSF DATA"
  if (!any(excelwb$sheet_names == rsf_data_sheet)) {
    all_names <- strsplit(excelwb$sheet_names,
                          split="[[:cntrl:][:space:][:punct:]]")
    
    requested_sheet <- unlist(strsplit(rsf_data_sheet,
                                       split="[[:cntrl:][:space:][:punct:]]"))
    
    found <- sapply(all_names,setequal,y=requested_sheet)
    if (!any(found)) {
      stop(paste0("Requested sheet name '",rsf_data_sheet,"' does not exist in workbook"))  
    } else if (length(found[found]) != 1) {
      stop(paste0("Requested sheet name '",rsf_data_sheet,"' matches multiple potential sheets: ",
                  paste0(excelwb$sheet_names,collapse=" & ")))  
    } else {
      rsf_data_sheet <- excelwb$sheet_names[which(found)]
    }
  }
    
  
  

  template_key <- NULL
  
  #In case Excel has formatted Date as numeric date
  if (is.na(as.Date(template_reporting_date,optional=TRUE))) template_reporting_date <- suppressWarnings(openxlsx::convertToDate(template_reporting_date,origin=openxlsx::getDateOrigin(template_file)))
  
  if (is.na(as.Date(template_reporting_date,optional=TRUE))) {
    stop(paste0("Unable to resolve RSF_REPORTING_DATE for value '",names(openxlsx::readWorkbook(template_file,namedRegion="RSF_REPORTING_DATE"))))
  } else {
    template_reporting_date <- as.Date(template_reporting_date)
  }
  
  if (grepl(":",reporting_key) && nchar(reporting_key)==65) {
    keys <- unlist(str_split(reporting_key,":"))
    reporting_key <- keys[1]
    template_key <- keys[2]
  } else if (nchar(reporting_key)==32) {
    #only the template key is defined, a specific reporting key is not provided
    template_key <- reporting_key
    reporting_key <- NA
  } else {
    stop("RSF Template error: RSF_REPORTING_KEY named region is defined, but expects format of <REPORT KEY>:<TEMPLATE KEY> or simply <TEMPLATE KEY> that cannot be resolved")
  }
  
  export <- dbGetQuery(pool,"
                       select 
                        rt.template_id,
                        rt.template_name,
                        rt.is_reportable,
                        rt.is_setup_template,
                        rt.template_has_static_row_ids,
                        ec.exporting_cohort_id,
                        ec.rsf_program_id,
                        ec.exporting_rsf_pfcbl_id,
                        ec.exporting_asof_date,
                        upper(ec.data_integrity_key) = upper($3::text) as data_integrity_validated
                       from p_rsf.reporting_templates rt
                       left join p_rsf.exporting_cohorts ec on ec.for_reporting_template_id = rt.template_id
                                                           and upper(ec.reporting_key) = upper($2::text)
                        where upper(rt.template_key) = upper($1::text)",
                       params=list(template_key,
                                   reporting_key,
                                   data_integrity_key))
  
  setDT(export)
  
  if (nrow(export)==0) {
    stop(paste0("RSF Template failed to lookup template '",toupper(template_key),"'"))
  } else if (nrow(export) > 1) {
    stop(paste0("RSF Template failed: multiple templates identified for template '",toupper(template_key),"'",
                " and reporting key '",toupper(reporting_key),"'"))
  }

  #RSF Programs Template does not need to report a reporting entity as it is assumed it will create its own program and any other defined entities  
  if (export$is_setup_template==FALSE &&
      (is.na(template_reporting_entity) || nchar(trim(template_reporting_entity))==0)) {
    stop("Failed to find named region RSF_REPORTING_ENTITY")
  }
  
  if (!is.na(export$exporting_asof_date) &&
      as.character(export$exporting_asof_date) != as.character(template_reporting_date)) {
    stop(paste0("RSF Template failed: Template reporting date not allowed.  Exported reporting date is '",
         as.character(export$exporting_asof_date),"' but the template reporting date is '",
         as.character(template_reporting_date),"'"))
  }

  #Reporting IDs
  {
    reporting_rsf_pfcbl_id <- dbGetQuery(pool,"
      select p_rsf.get_rsf_pfcbl_id_by_sys_name($1::text) as rsf_pfcbl_id",
      params=list(template_reporting_entity))
    
    reporting_rsf_pfcbl_id <- unlist(reporting_rsf_pfcbl_id)
    if (length(reporting_rsf_pfcbl_id)==0) reporting_rsf_pfcbl_id <- as.numeric(NA)
    
    if (!is.na(export$exporting_rsf_pfcbl_id)) {
      
      if (!identical(as.integer(export$exporting_rsf_pfcbl_id),as.integer(reporting_rsf_pfcbl_id))) {
        stop(paste0("RSF Template error: Template was generated from a Jason data export request under SYSID:",
                    export$exporting_rsf_pfcbl_id," but template has altered the exporting SYSNAME:",template_reporting_entity,
                    ", which is associated with SYSID:",reporting_rsf_pfcbl_id,". Modifying the template SYSNAME is not allowed."))
      }
    
    } else if (is.na(reporting_rsf_pfcbl_id) &&
               export$is_setup_template==FALSE) {
      stop(paste0("RSF Template error: Failed to find SYSID from reported SYNAME. ",reporting_rsf_pfcbl_id," does not exist in the database?"))
    } 
  }
  
  
  {
    rsf_program_id <- dbGetQuery(pool,
                                 "select distinct ids.rsf_program_id
                                  from p_rsf.rsf_pfcbl_ids ids
                                  where ids.rsf_pfcbl_id = $1::int",
                                 params=list(reporting_rsf_pfcbl_id))
    if (empty(rsf_program_id)) {
      rsf_program_id <- as.numeric(NA)
    } else {
      rsf_program_id <- as.numeric(unlist(rsf_program_id))
    }
  }
  
  #Load the data
  {
    tables <- tolower(openxlsx::getTables(excelwb,rsf_data_sheet))
    if (any(tables=="rsf_data")) stop("Obsolete table name: 'rsf_data' -- change Excel file table name to 'rsf_template_data'")
    if (!any(tables=="rsf_template_data")) stop("Data table not found: Template expected to define data table 'RSF_TEMPLATE_DATA'")
    
    
    rsf_data <- rsf_reports_excel_read_rsf_data(excelwb = excelwb,
                                                table_name="rsf_template_data")
    if (empty(rsf_data)) {
      stop(paste0("Template defines VALID but EMPTY rsf_template_data table"))
    }
    
    formulaSheet <- openxlsx_get_formulas(excelwb=excelwb,
                                          sheetName=rsf_data_sheet,
                                          truncate_predata_rows = TRUE)
    
    formulaSheet <- formulaSheet[attr(rsf_data,"excelRows")[1:nrow(rsf_data)],attr(rsf_data,"excelColumns")[1:ncol(rsf_data)]]
    setDT(formulaSheet)
    setnames(formulaSheet,
             old=names(formulaSheet),
             new=names(rsf_data))
    
    if (any(names(rsf_data)=="SYSID")) {
      rsf_data[,SYSID:=as.numeric(SYSID)]
    }
    #rsf_data[,SYSID:=as.numeric(SYSID)]
    
    if (any(grepl("^REPORTING_date",names(rsf_data)))) {
      setnames(rsf_data,
               old="REPORTING_date",
               new="reporting_asof_date")
    }
    
    if (any(grepl("^reporting_asof_date$",names(rsf_data)))) {
      
      if (class(rsf_data$reporting_asof_date) %in% c("numeric","integer") || 
          all(grepl("^[[:digit:]]+$",rsf_data$reporting_asof_date))) {
        rsf_data[,reporting_asof_date:=openxlsx::convertToDate(reporting_asof_date,
                                                               origin=openxlsx::getDateOrigin(template_file))]
      } else if (class(rsf_data$reporting_asof_date)=="character") {
        rsf_data[,reporting_asof_date:=as.Date(reporting_asof_date,
                                               origin=openxlsx::getDateOrigin(template_file))]  
      }
      
      if (anyNA(rsf_data$reporting_asof_date)) {
        stop("RSF Template error: Template provides column 'reporting_asof_date' but rows are {MISSING} valid YYYY-MM-DD date values")
      }
    } else {
      rsf_data[,reporting_asof_date:=as.Date(template_reporting_date)]
      formulaSheet[,reporting_asof_date:=as.Date(NA)]
    }
    
    rsf_data[,reporting_template_row_group:=paste0(1:.N,"RSFDATA")]
    formulaSheet[,reporting_template_row_group:=rsf_data$reporting_template_row_group]
    
    #VALID SYSIDS
    {
      sysids <- unique(rsf_data$SYSID)
      sysids <- na.omit(sysids)
      ids_not_in_reporting_family_tree <- dbGetQuery(pool,"
                              
                              select unnest(string_to_array($1::text,','))::int as rsf_pfcbl_id
                              
                              except
                              
                              select ft.to_family_rsf_pfcbl_id as rsf_pfcbl_id
                              from p_rsf.view_rsf_pfcbl_id_family_tree ft 
                              where ft.from_rsf_pfcbl_id = $2::int",
                              params=list(paste0(sysids,collapse=","),
                                          reporting_rsf_pfcbl_id))
      
      if (!empty(ids_not_in_reporting_family_tree)) {
        if (!any(names(rsf_data)=="SYSNAME")) {
          ids_not_in_reporting_family_tree <- unlist(ids_not_in_reporting_family_tree)
          if (length(ids_not_in_reporting_family_tree) > 100) ids_not_in_reporting_family_tree <- paste0("more than 100 IDs, starting with these 3: ",
                                                                                                         paste0(ids_not_in_reporting_family_tree[1:3],collapse=", "),
                                                                                                         "...")
          stop(paste0("Failed to lookup SYSIDs and SYSNAME column not available: ",paste0(unique(ids_not_in_reporting_family_tree),collapse=", ")))
        } else {
          
          sysnames <- unique(rsf_data$SYSNAME)
          if (any(grepl(",",sysnames))) {
            stop("Formatting error in SYSNAME column: commas are not allowed")
          }
          rsf_data[,SYSID:=as.integer(NA)]
          
          sysnameids <- db_get_rsf_pfcbl_id_by_sys_name(pool=pool,
                                                        sys_names=sysnames,
                                                        rsf_pfcbl_id.family_tree=reporting_rsf_pfcbl_id,
                                                        error.if.missing=FALSE)
          
          # sysids <- NULL
          # sysnameids <- dbGetQuery(pool,"
          # select 
          #   sn.sys_name as \"SYSNAME\",
          #   rsf_pfcbl_id
          # from
          # (select unnest(string_to_array($2::text,','))::text as sys_name) as sn
          # left join lateral p_rsf.get_rsf_pfcbl_id_by_sys_name(sn.sys_name) as rsf_pfcbl_id on true
          # where rsf_pfcbl_id = any(select ft.to_family_rsf_pfcbl_id
          #                          from p_rsf.view_rsf_pfcbl_id_family_tree ft 
          #                          where ft.from_rsf_pfcbl_id= $1::int)
          # ",params=list(reporting_rsf_pfcbl_id,
          #               paste0(sysnames,collapse=",")))
          
          # sysnameids <- dbGetQuery(pool,"
          #                          select 
          #                           ft.to_family_rsf_pfcbl_id::int,
          #                           sn.sys_name as \"SYSNAME\"
          #                           from p_rsf.view_rsf_pfcbl_id_family_tree ft 
          #                           inner join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
          #                           where ft.from_rsf_pfcbl_id = $1::int
          #                             and sn.sys_name = any(select unnest(string_to_array($2::text,','))::text)
          #                          ",params=list(reporting_rsf_pfcbl_id,
          #                                        paste0(sysnames,collapse=",")))
          setDT(sysnameids)
          rsf_data[sysnameids,
                   SYSID:=as.integer(i.rsf_pfcbl_id),
                   on=.(SYSNAME=lookup_sys_name)]
          
          #In case they're creating new entities by having blank rows with no entry for SYSNAME and SYSID
          if (anyNA(rsf_data[!is.na(SYSNAME)]$SYSID)) {
            badids <- rsf_data[!is.na(SYSNAME) & is.na(SYSID),unique(SYSNAME)]
            stop(paste0("RSF Template error: Failed to lookup SYSID using SYSNAME for: ",
                        paste0(badids,collapse=", ")))
          }
        }
      }
    }
    
    cols <- c("SYSID",
              "SYSNAME",
              "reporting_asof_date",
              "reporting_template_row_group")
    cols <- cols[cols %in% names(rsf_data)]
    setcolorder(rsf_data,
                neworder=cols)
    setcolorder(formulaSheet,
                neworder=names(rsf_data))
  }  
  

  
  
  template <- list()
  template$template_id <- export$template_id
  template$template_name <- export$template_name
  template$template_key <- reporting_key
  template$reporting_asof_date <- ceiling_date(template_reporting_date,"quarter") - days(1) #Ensure end of quarter reporting date (data may be within quarter)
  template$rsf_program_id <- rsf_program_id
  template$cohort_pfcbl_id <- reporting_rsf_pfcbl_id
  template$data_integrity_key <- data_integrity_key
  template$template_data <- rsf_data
  template$template_data_formula_sheet <- formulaSheet
  
  template$template_settings <- list()
  template$template_settings$template_has_static_row_ids <- export$template_has_static_row_ids
  template$template_settings$template_is_reportable <- export$is_reportable
  template$template_settings$template_is_setup <- export$is_setup_template
  
  return(template)
}