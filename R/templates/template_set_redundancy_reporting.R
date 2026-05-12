template_set_redundancy_reporting <- function(pool,
                                              template,
                                              indicator_subscriptions) {

  if (is.null(template$pfcbl_data)) stop("Template does not define template$pfcbl_data")
  if (empty(template$pfcbl_data)) return (template) #If all hashvalues are unchanged, will have filtered out everything
  
  template$pfcbl_data[,
                      n:=.N,
                      by=.(rsf_pfcbl_id,
                           indicator_id,
                           reporting_asof_date)]

  multiple_data <- template$pfcbl_data[n>1]
  
  if (!empty(multiple_data)) {
    
    #Splits by row number (digit) and sheet name (non-digit) to ensure sort order (higher row on same sheet equlas more recent data)
    multiple_data[,c("template_row","template_sheet"):=tstrsplit(x=reporting_template_row_group, split="(?=\\D)(?<=\\d)", perl=TRUE)]
    multiple_data[,template_row:=as.numeric(template_row)]

    setorder(multiple_data,
             rsf_pfcbl_id,
             indicator_id,
             reporting_asof_date,
             template_sheet,
             template_row)
    
    multiple_data[,
                  `:=`(latest_row=template_row[length(template_row)],
                       multiples=nrow(unique(.SD[,.(data_value,data_unit)]))),
                   by=.(rsf_pfcbl_id,
                        indicator_id,
                        reporting_asof_date,
                        template_sheet)]

    template$pfcbl_data[multiple_data[template_row==latest_row,
                                      .(reporting_template_row_group=paste0(latest_row,template_sheet),
                                        rsf_pfcbl_id,
                                        indicator_id,
                                        reporting_asof_date)],
                        n:=0,
                        on=.(rsf_pfcbl_id,
                             indicator_id,
                             reporting_asof_date,
                             reporting_template_row_group)]
    
    template$pfcbl_data <- template$pfcbl_data[n<=1]  #template$pfcbl_data[n==0]
    multiple_data <- multiple_data[multiples>1]
    
    if (!empty(multiple_data)) {
      
      multiple_data[indicator_subscriptions,
                    is_calculated:=i.is_calculated, #ie, don't flag calculated redundancies
                    on=.(rsf_pfcbl_id,
                         indicator_id)]
      
      multiple_data <- multiple_data[is_calculated==FALSE]
      
      if (!empty(multiple_data)) {
        multiple_data[,
                      message_value:=fcase(!is.na(data_value) & !is.na(data_unit),paste0(data_value," ",data_unit),
                                          is.na(data_value) & !is.na(data_unit),data_unit,
                                          is.na(data_unit) & !is.na(data_value),data_value,
                                          default="BLANK")]
        multiple_data[,
                      changed_value:=message_value != shift(message_value,n=1),
                      by=.(rsf_pfcbl_id,
                           indicator_id,
                           reporting_asof_date)]
        
        multiple_data[is.na(changed_value),
                      changed_value:=TRUE]
        
        multiple_data <- multiple_data[changed_value==TRUE]
        multiple_data <- multiple_data[,
                                      .(check_name="sys_flag_multiple_data_points_reported",
                                        check_message=paste0("Different values cannot all be true on ",as.character(reporting_asof_date),": ",
                                                             paste0(paste0("{",reporting_template_row_group,"=",message_value,"}"),collapse=" "),
                                                             " SAVING {",message_value[length(message_value)],"} on row ",template_row[length(template_row)]," and DISCARDING rows: ",
                                                             paste0(template_row[-length(template_row)],collapse=","))),
                                      by=.(rsf_pfcbl_id,
                                           indicator_id,
                                           reporting_asof_date)]
        
        template$pfcbl_reporting_flags <- rbindlist(list(template$pfcbl_reporting_flags,
                                                         multiple_data[,
                                                                       .(rsf_pfcbl_id,
                                                                         indicator_id,
                                                                         reporting_asof_date,
                                                                         check_name,
                                                                         check_message)]))
      }
    }
  }
  
  multiple_data <- NULL
  
  template$pfcbl_data[,n:=NULL]
  
  return (template)  
}
