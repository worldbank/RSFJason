

parse_template_csv <- function(pool,
                               template_id,
                               rsf_program_id,
                               csv_file,
                               rsf_indicators,
                               status_message) 
{
  
  
  
  
  ifc_client_id_ex <- ".*#(\\d{4,}).*"
  reporting_asof_date_ex <- ".*(\\d{4}-\\d{2}-\\d{2}).*"
  
  ifc_client_id <- NULL
  reporting_asof_date <- NULL
  
  filename_has_ids <- grepl(ifc_client_id_ex,basename(csv_file)) & grepl(reporting_asof_date_ex,basename(csv_file))

  if(filename_has_ids) {
    
    ifc_client_id <- as.numeric(gsub(ifc_client_id_ex,"\\1",basename(csv_file)))
    reporting_asof_date <- ymd(gsub(reporting_asof_date_ex,"\\1",basename(csv_file)))
        
  } else {
    stop("Filename must include the CLIENT ID number (IFC Partner ID) in format of '#XXXXX' and also the reporting-asof-date for calendar quarter in which the data is current in format 'YYYY-MM-DD'")
  }
  
  csvf <- tryCatch({
    
    fread(file=csv_file,
          header=T)
    
  },
  error = function(e) { 
    stop(conditionMessage(e))
  },
  warning = function(w) { 
    stop(conditionMessage(w))
  })

  
  
  csvf[,reporting_template_row_group:=paste0(1:.N,"CSV")]
  col_names <- names(csvf)
  col_names <- col_names[-which(col_names=="reporting_template_row_group")]
  
  submitted_names <- data.table(labels_submitted=col_names)
  submitted_names[,id:=1:.N]
  
  parse_labels <- parse_template_labels(rsf_indicators=rsf_indicators,
                                        template_labels=submitted_names,
                                        lookup_cols=c("labels_submitted"),
                                        label_key_priority=NA,
                                        label_key_filter=NA)
  
  keep_labels <- c("reporting_template_row_group",parse_labels$labels_submitted)
  discard_labels <- col_names[!col_names %in% keep_labels]
  for (col in discard_labels) {
    status_message(paste0("Reject unrecognized column name: {",col,"}. WARNING: This will not be flagged"))
    set(csvf,i=NULL,j=col,value=NULL)
    
  }
  

  #TODO: add indicator not found reporting flags
  setnames(csvf,
           old=parse_labels$labels_submitted,
           new=parse_labels$indicator_name)

  if (is.null(ifc_client_id) || is.null(reporting_asof_date)) {
    parse_labels[rsf_indicators,
                 `:=`(pfcbl_rank=i.indicator_pfcbl_rank,
                      indicator_sys_category=i.indicator_sys_category),
                 on=.(indicator_name)]
    
    if (!any(parse_labels$indicator_sys_category=="id") ||
        !any(parse_labels$indicator_sys_category=="reporting_date")) {
      stop("Failed to find dataset root reporting ID (eg, IFC partner ID number) and reporting_asof_date in dataset filename or data columns")
    }
    
    asof_col <- unique(parse_labels[indicator_sys_category=="reporting_date",indicator_name])
    id_col <- unique(parse_labels[indicator_sys_category=="id",indicator_name])
    if (length(asof_col) != 1) stop(paste0("Multiple reporting_date columns found: ",paste0(asof_col,collapse=" & ")))

    #Data set could contain IDs for any number of entities, eg many loans, borrowers, client(s), etc.
    #Find the columns that have IDs and identify the highest pfcbl_rank that has one unique ID value (ie, a root value under which we're reporting)
    id_vals <- rbindlist(lapply(id_col,function(x) {
      data.table(indicator_name=x,
                 id=unique(csvf[[x]]))
    }))
    
    
    id_vals[,n:=.N,
            by=.(indicator_name)]
    
    id_vals <- id_vals[n==1]
    id_vals[parse_labels,
            pfcbl_rank:=i.pfcbl_rank,
            on=.(indicator_name)]
    ifc_client_id <- as.numeric(id_vals[pfcbl_rank==max(pfcbl_rank),id])
    reporting_asof_date <- ymd(unique(csvf[[asof_col]]))
    
    if (length(ifc_client_id) != 1 || all(is.na(ifc_client_id))==TRUE) stop("Failed to identify a root reporting ID")
    if (length(reporting_asof_date) != 1 || all(is.na(reporting_asof_date))==TRUE) stop("Failed to identify a reporting date in YYYY-MM-DD format")
  }
  
  #Presently quarterly program reporting is embedded in the system.
  #Filename may be any arbitrary report generation daily date within the quarter.
  reporting_asof_date <- ceiling_date(x=ymd(reporting_asof_date),unit="quarter")-1
  
  col_names <- names(csvf)
  col_names <- col_names[-which(col_names=="reporting_template_row_group")]
  for (col in col_names) {
    
    set(csvf,i=NULL,j=col,value=as.character(csvf[[col]]))
  }
  
  csvf <- melt.data.table(csvf,
                          id.vars = "reporting_template_row_group",
                          measure.vars=col_names,
                          value.name = "reporting_submitted_data_value",
                          variable.name = "indicator_name",
                          value.factor = F,
                          variable.factor = F)
  
  csvf[,reporting_submitted_data_formula:=as.character(NA)] #CSV cannot have embedded formulas (as Excel does)
  csvf[,reporting_submitted_data_unit:=as.character(NA)] #CSV expects wide format and so data units are not reported in columns
  csvf[,labels_submitted:=as.character(NA)]
  
  csvf <- csvf[,.(reporting_template_row_group,
                  indicator_name,
                  reporting_submitted_data_value,
                  reporting_submitted_data_unit,
                  reporting_submitted_data_formula,
                  labels_submitted)]
  
  csvf[,reporting_asof_date:=reporting_asof_date]
  
  
  pfcbl_ids <- dbGetQuery(pool,"
                          select distinct
                            ids.rsf_program_id,
                            ids.rsf_pfcbl_id as reporting_rsf_pfcbl_id
                          from p_rsf.rsf_pfcbl_ids ids
                          inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
                          inner join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id
                                                               and rdc.indicator_id = ind.indicator_id
                          where ids.rsf_program_id = $1::int
                          	and ids.pfcbl_category = 'client'
                          	and ind.indicator_sys_category = 'id'
                          	and rdc.data_value ~* $2::text",
                        params=list(rsf_program_id,
                                    toupper(trimws(ifc_client_id))))
  
  if (nrow(pfcbl_ids) > 1) stop(paste0("Multiple IDs found for '",ifc_client_id,"' for rsf_pfcbl_ids: ",paste0(rsf_ids$rsf_pfcbl_id,collapse=" & ")))
  if (empty(pfcbl_ids)) stop(paste0("Filename '",basename(csv_file),"' name format contains an ID number and a YYYY-MM-DD reporting date: but Client ID (ie, IFC Partner ID in iDesk) #",ifc_client_id," cannot be found in the database.  Perhaps the iDesk project number was provided instead of the client partner number?"))
  
  if (pfcbl_ids$rsf_program_id != rsf_program_id) {
    stop(paste0("Incorrect program is selected: Uploaded specifies RSF program number #",rsf_program_id,
                " but the IFC Partner ID number ",ifc_client_id," is for RSF program number #",pfcbl_ids$rsf_program_id))
  }
  
  # #In case a RSA project ID number is passed in
  # blanks <- sapply(rsf_ids,function(x) all(is.na(x)))
  # rsf_cols <- names(rsf_ids)[!blanks]
  # setDT(rsf_ids)
  # rsf_ids <- rsf_ids[,..rsf_cols]
  # 
  # csvf <- cbind(rsf_ids,   
  #               csvf)
  
  template <- list(cohort_pfcbl_id=pfcbl_ids$reporting_rsf_pfcbl_id,
                   reporting_asof_date=reporting_asof_date,
                   template_data=csvf)

  status_message(class="info","Success: Completed Parsing File:\n")
  status_message(class="info",". . . Project: ",ifc_client_id,"\n")
  status_message(class="info",". . . Period: ",as.character(reporting_asof_date),"\n")

  return (template)
}
