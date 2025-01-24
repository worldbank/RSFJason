

parse_template_csv_backup_data <- function(pool,
                                           template_lookup,
                                           template_file,
                                           reporting_user_id,
                                           rsf_indicators) { 
  
  
  
  report_data <- fread(file=template_file)
  
  
  headers <- names(report_data)
  if (!setequal(headers,
               c("SYSNAME",
                 "INDID",
                 "reporting_asof_date",
                 "indicator_name",
                 "data_value"))) {
    stop("parse_template_csv_backup_data expects headers: SYSNAME, INDID, reporting_asof_date, indicator_name, data_value")
  }
  
  report_data[,row_number:=1:.N]
  
  {
    
    snames <- report_data[,.(SYSNAME=unique(SYSNAME))]
    snames[,name_tree:=strsplit(snames$SYSNAME,
                                split="\\s+>\\s+",
                                fixed=F)]
    
    program <- unique(sapply(snames$name_tree,`[`,1))
    facility <- unique(sapply(snames$name_tree,`[`,2))
    entity <- NULL
    if (length(facility)==1 && !any(is.na(facility))) {
      entity <- facility
    } else if (length(program)==1 && !any(is.na(program))) {
      entity <- program
    } else {
      stop(paste0("Failed to uniquely identify entity from SYSNAME: ",paste0(program,collapse=", ")))
    }
    
    entity_category <- gsub(":.*$","",entity)
    if (!any(entity_category %in% c("program","facility"))) {
      stop(paste0("SYSNAME resolved '",entity,"' but backup files can only be generated at the program or facility levels"))
    }
    
    
    ids <- db_get_rsf_pfcbl_id_by_sys_name(pool=pool,
                                           sys_names=unique(entity),
                                           rsf_program_id=NA,
                                           error.if.missing=FALSE)
    
    ids <- sysids[pfcbl_category==entity_category]
    
    # 
    # 
    # ids <- dbGetQuery(pool,"
    #   select
    #     ids.rsf_program_id,
    #     ids.rsf_pfcbl_id
    #   from p_rsf.rsf_pfcbl_ids ids
    #   where ids.rsf_pfcbl_id = (select p_rsf.get_rsf_pfcbl_id_by_sys_name($1::text) as rsf_pfcbl_id)
    #     and ids.pfcbl_category = $2::text
    # ",params=list(entity,entity_category))
    
    if (empty(ids) || nrow(ids) != 1 || anyNA(ids$rsf_pfcbl_id)) {
      stop("Failed to look up reporting entity ID and rsf_program_id")
    }
  }
  
  {
    setnames(report_data,
             old=c("INDID"),
             new=c("indicator_id"))
    
    report_data[,indicator_id:=as.numeric(indicator_id)]
    
    bad_indicators <- fsetdiff(unique(report_data[,.(indicator_id,indicator_name)]),
                               rsf_indicators[,.(indicator_id,indicator_name)])
    
    bad_indicators <- as.data.frame(bad_indicators)
    setDT(bad_indicators)
    
    bad_indicators[rsf_indicators,
                   current_name:=i.indicator_name,
                   on=.(indicator_id)]
    
    bad_indicators[,similarity:=mapply(FUN=function(a,b) { 
      a<-unlist(strsplit(a,split="_",fixed=T))
      b<-unlist(strsplit(b,split="_",fixed=T))
      n <- min(length(a),length(b))
      n <- length(intersect(a,b))/n
      if (all(is.na(n)) || length(n)==0) n <- 0
      n;
    },a=indicator_name,b=current_name)]
    
    auto_correct <- bad_indicators[similarity >= 0.7]
    report_data[auto_correct,
                indicator_name:=i.current_name,
                on=.(indicator_id)]
    
    bad_indicators <- bad_indicators[similarity < 0.7]
    
    if (!empty(bad_indicators)) {
      bad_indicators[,
                     message:=paste0(indicator_name," (",
                                     indicator_id,
                                     ifelse(is.na(current_name),
                                            " is undefined/deleted",
                                            paste0(" is now '",current_name,"'")),")")]
      
      stop(paste0("The setup file defines the following indicators, which are NOT defined in the database.\n",
                  "Perhaps the indicator names have been changed (or deleted) since the file was generated?\n",
                  "If so, review the current master list and revise the upload file accordingly.\n",
                  "Bad indicators: ",
                  paste0(bad_indicators$message,collapse=", \n")))
    }
  }
  
  {

    setorder(report_data,
             SYSNAME,
             reporting_asof_date,
             indicator_id,
             row_number)

    report_data[,
                reporting_template_row_group:=paste0(.GRP,"RSFDATA"),
                by=.(SYSNAME,
                     reporting_asof_date)] #data_value because data backup archive will include any redundancy reporting, which should be on a subsequent row number
    
    report_data[,
                redundancy_rank:=(.N:1)-1,
                by=.(SYSNAME,
                     reporting_asof_date,
                     indicator_id)]

    report_data[,
                row_number:=NULL]
    
    if (any(report_data$redundancy_rank > 0)) {
      
      report_data[,
                  has_multiples:=any(redundancy_rank > 0),
                  by=.(SYSNAME,
                       reporting_asof_date)]
      
      multiple_reporting <- report_data[has_multiples==TRUE]
      multiple_reporting[,has_multiples:=NULL]
      
      latest_reporting <- multiple_reporting[redundancy_rank==0,
                                             .(SYSNAME,indicator_id,reporting_asof_date,indicator_name,data_value,redundancy_rank)]
      
      multiple_reporting <- multiple_reporting[redundancy_rank > 0,
                                               .(SYSNAME,indicator_id,reporting_asof_date,indicator_name,data_value,redundancy_rank)]
      
      redundancy_reporting <- latest_reporting[,
                                               .(SYSNAME,indicator_id,reporting_asof_date,indicator_name,data_value)
                                               ][unique(multiple_reporting[,.(SYSNAME,reporting_asof_date,redundancy_rank)]),
                                               .(indicator_id,indicator_name,data_value,redundancy_rank),
                                               on=.(SYSNAME,reporting_asof_date),
                                               by=.EACHI]
      
      redundancy_reporting[multiple_reporting,
                           data_value:=i.data_value,
                           on=.(SYSNAME,
                                reporting_asof_date,
                                indicator_id,
                                redundancy_rank)]
      
      setcolorder(redundancy_reporting,
                  neworder=names(latest_reporting))
      
      redundancy_reporting <- rbindlist(list(redundancy_reporting,
                                             latest_reporting)) #LAST!  Latest
      
      
      report_data <- report_data[has_multiples==FALSE]
      report_data[,has_multiples:=NULL]
      report_data[,reporting_template_row_group:=NULL] #redo based on redundancy_rank
      
      report_data <- rbindlist(list(report_data,
                                    redundancy_reporting))
      
      setorder(report_data,
               SYSNAME,
               reporting_asof_date,
               -redundancy_rank,
               indicator_id)
      
      
      report_data[,reporting_template_row_group:=paste0(.GRP,"RSFDATA"),
                  by=.(SYSNAME,
                       reporting_asof_date,
                       redundancy_rank)]
      
    } 
    
    report_data[,redundancy_rank:=NULL]
    
    setnames(report_data,
             old=c("data_value"),
             new=c("reporting_submitted_data_value"))
    
    report_data[,
                `:=`(reporting_submitted_data_unit=as.character(NA),
                     reporting_submitted_data_formula=as.character(NA))]
    
    setcolorder(report_data, c("SYSNAME",
                               "reporting_asof_date",
                               "indicator_name",
                               "reporting_submitted_data_unit",
                               "reporting_submitted_data_value",
                               "reporting_submitted_data_formula",
                               "reporting_template_row_group"))
  }  
  
  template <- list()
  # template$template_id <- template_lookup$template_id
  # template$template_name <- template_lookup$template_name
  # template$template_key <- template_lookup$template_key
  template$rsf_program_id <- ids$rsf_program_id
  template$cohort_pfcbl_id <- ids$rsf_pfcbl_id
  template$template_settings <- list()
  # template$template_settings$template_has_static_row_ids <- template_lookup$template_has_static_row_ids
  # template$template_settings$template_is_reportable <- template_lookup$is_reportable
  # template$template_settings$template_is_setup <- FALSE
  
  #must be MAX because historical reporting and/or retroactive reporting is semi-expected
  #Where as future reporting, while allowed, is not expected and can result in unusual issues (notable where entity matching will only match retrospectively)
  template$reporting_asof_date <- ceiling_date(max(report_data$reporting_asof_date),"quarter") - days(1) #Ensure end of quarter reporting date (data may be within quarter)
  
  template$template_data <- report_data
  template$template_data_formula_sheet <- NULL
  
  template$template_source_reference <- "RSF Backup Data Restore"
  template$template_ids_method <- "rsf_id"
  
  return (template)
}


