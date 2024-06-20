template_set_reporting_checks <- function(template,rsf_indicators=NULL,add_reporting_checks=NULL) {
  stop("deprecated")
  if (!is.null(add_reporting_checks)) {
    if (all(is.na(add_reporting_checks))) return (template)
    if (!all(names(add_reporting_checks) %in% c("rsf_pfcbl_id","indicator_id","check_name","check_message"))) {
      stop("add_reporting_checks expects data.table with fields rsf_pfcbl_id, indicator_id, check_name, check_message")
    }
    
    add_reporting_checks$reporting_current_date <- template$reporting_asof_date
    setcolorder(add_reporting_checks,neworder=c("reporting_current_date","rsf_pfcbl_id","indicator_id","check_name","check_message"))
    
    if (is.null(template$reporting_checks) || all(is.na(template$reporting_checks))) template$reporting_checks <- add_reporting_checks
    else template$reporting_checks <- rbindlist(list(template$reporting_checks,add_reporting_checks))
  }
  else if (!is.null(template$match_results)) {
    
    match_action_issues <- template$match_results[!is.na(match_issues) & match_issues != ""]
    if (nrow(match_action_issues) > 0) {
      
      reporting_indicators <- rsf_indicators[indicator_sys_category=="entity_reporting"]
      reporting_match_issues <- reporting_indicators[match_action_issues,on=.(data_category=pfcbl_category)]
      reporting_match_issues <- reporting_match_issues[,.(rsf_pfcbl_id,
                                                          indicator_id=indicator_id,
                                                          check_name="sys_flag_template_match_actions",
                                                          check_message=paste0("row=",reporting_template_row_group," action=",match_action,"/ ",match_issues))]
      
      reporting_match_issues$reporting_current_date <- template$reporting_asof_date
      
      setcolorder(reporting_match_issues,neworder=c("reporting_current_date","rsf_pfcbl_id","indicator_id","check_name","check_message"))
      if (is.null(template$reporting_checks) || all(is.na(template$reporting_checks))) template$reporting_checks <- reporting_match_issues
      else template$reporting_checks <- rbindlist(list(template$reporting_checks,reporting_match_issues))
    }
  } 
  
  if (is.null(template$reporting_checks)) template$reporting_checks <- NA

  return (template)
}