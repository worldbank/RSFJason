

parse_template_pfcbl_editor_report <- function(pool,
                                               template=template,
                                               template_file=template_file,
                                               reporting_user_id=reporting_user_id,
                                               rsf_indicators=rsf_indicators) { 
  
  
  report_data <- template$template_data
  {
    setnames(report_data,
             old=c("INDID"),
             new=c("indicator_id"))
    
    report_data[,SYSID:=as.numeric(SYSID)]
    report_data[,indicator_id:=as.numeric(indicator_id)]
  
    bad_indicators <- setdiff(report_data[,.(indicator_id,indicator_name)],
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
  
  #Formula checks
  {
    formulaSheet <- template$template_data_formula_sheet
    formulaSheet <-  formulaSheet[,
                                  .(has_formulas=any(!is.na(.SD))),
                                  by=.(reporting_template_row_group)]
    formulaSheet <- formulaSheet[has_formulas==TRUE]
    if (!empty(formulaSheet)) {
      stop(paste0("Formulas are not allowed in pfcbl reporting templates. See errors on rows: ",
                  paste0(formulaSheet$reporting_template_row_group,collapse=", ")))
    }
  }  

  setorder(report_data,
           SYSID,
           -reporting_asof_date,
           indicator_name)
  
  report_data[,reporting_template_row_group:=paste0(.GRP,"RSFDATA"),
              by=.(SYSID,
                   reporting_asof_date)]
  
  integrity_check <- rsf_reports_data_integrity_key(reporting_asof_date=report_data$reporting_asof_date,
                                                    rsf_pfcbl_ids=report_data$SYSID,
                                                    indicator_ids=report_data$indicator_id)
  

  
  setnames(report_data,
           old=c("data_value"),
           new=c("reporting_submitted_data_value"))
  
  report_data[,
              `:=`(reporting_submitted_data_unit=as.character(NA),
                   reporting_submitted_data_formula=as.character(NA))]
  
  setcolorder(report_data, c("SYSID",
                             "reporting_asof_date",
                             "indicator_name",
                             "reporting_submitted_data_unit",
                             "reporting_submitted_data_value",
                             "reporting_submitted_data_formula",
                             "reporting_template_row_group"))
  
  template$template_data <- report_data
  template$template_source_reference <- "RSF Editor Report"
  template$template_ids_method <- "pfcbl_id"
  
  return (template)
}


