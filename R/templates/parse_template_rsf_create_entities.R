

parse_template_rsf_create_entities <- function(pool,
                                               template) { 
  
  if (template$template_name != "RSF-ENTITIES-TEMPLATE") {
    stop(paste0("parse_template_rsf_create_entities expects template format for 'RSF-ENTITIES-TEMPLATE' but received format '",template$template_name,"'"))
  }
  
  report_data <- template$template_data
  
  rsf_names <- c("SYSID","SYSNAME")
  if (!all(rsf_names %in% names(report_data))) {
    stop(paste0("Error: Expected column names ",paste0(rsf_names,collapse=",")," not found in ",paste0(names(tdata),collapse=",")))
  } 

  if (any(grepl("^SYSNAME$",names(report_data)))) {
    report_data[,SYSNAME:=NULL]
  }
  
  report_data <- melt.data.table(report_data,
                                id.vars=c("SYSID","reporting_asof_date","reporting_template_row_group"),
                                variable.name="indicator_name",
                                value.name="data_value",
                                value.factor = F,
                                variable.factor = F)
                  
  # report_data[rsf_indicators,
  #             indicator_id:=i.indicator_id,
  #             on=.(indicator_name)]            
  # 
  # if (anyNA(report_data$indicator_id)) {
  #   bad_indicators <- which(is.na(report_data$indicator_id))
  #   stop(paste0("This template expects column names in Jason system names.  Failed to look-up indicators: ",
  #               paste0(unique(report_data[bad_indicators,indicator_name]),collapse=", ")))
  # }
  # 
  # integrity_check <- rsf_reports_data_integrity_key(reporting_asof_date=report_data$reporting_asof_date,
  #                                                   rsf_pfcbl_ids=report_data$SYSID,
  #                                                   indicator_ids=report_data$indicator_id)
  # 
  
  #integrity check not used for this template currently
  
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
  template$template_source_reference <- "SLGP Template"
  template$template_ids_method <- "pfcbl_id" #Create entities should create all specified entities under the reporting_rsf_pfcbl_id and lookup by name and ID first
  
  return (template)
}


