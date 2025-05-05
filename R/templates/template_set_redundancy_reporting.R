template_set_redundancy_reporting <- function(pool,
                                              template,
                                              indicator_subscriptions) {

  if (is.null(template$pfcbl_data)) stop("Template does not define template$pfcbl_data")
  if (empty(template$pfcbl_data)) return (NULL) #If all hashvalues are unchanged, will have filtered out everything
  
  template$pfcbl_data[,
                      n:=.N,
                      by=.(rsf_pfcbl_id,
                           indicator_id,
                           reporting_asof_date)]

  multiple_data <- template$pfcbl_data[n>1]
  
  #template$pfcbl_data[,n:=NULL]
  
  #if there aren't any multiples, redundancy rank will always be zero
  # template$pfcbl_data[,
  #                     reporting_redundancy_rank:=0] #0th is "current" end of period reporting ie, not redundant

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
  
  # if (!empty(multiple_data)) {
  #   
  #   #Splits by row number (digit) and sheet name (non-digit) to ensure sort order (higher row on same sheet equlas more recent data)
  #   multiple_data[,c("template_row","template_sheet"):=tstrsplit(x=reporting_template_row_group, split="(?=\\D)(?<=\\d)", perl=TRUE)]
  #   multiple_data[,template_row:=as.numeric(template_row)]
  #   
  #   setorder(multiple_data,
  #            rsf_pfcbl_id,
  #            indicator_id,
  #            reporting_asof_date,
  #            template_sheet,
  #            template_row)
  #   
  #   multiple_data[,
  #                 `:=`(repeat_rank=(1:.N)-1, #0 is first observation, LOWEST as-of date and LOWEST template_row (this will be reversed later for upload!)
  #                      boundary=(1:.N) %in% (c(1,.N)),
  #                      endpoint=(1:.N)==.N),
  #                 by=.(rsf_pfcbl_id,
  #                      indicator_id,
  #                      reporting_asof_date)]
  #   # 
  #   # #redundant in time: for each multiple reporting, is value the same as on its preceding row?
  #   # multiple_data[,redundant_in_time:=repeat_rank > 0 &
  #   #                 is.same_text(data_value,shift(data_value,n=1,fill=as.character(NA))) &
  #   #                 is.same_text(data_unit,shift(data_unit,n=1,fill=as.character(NA))),
  #   #               by=.(rsf_pfcbl_id,
  #   #                    indicator_id)]
  # 
  #   #Changed redundancy to always consider highest reporting_template_row_group as the current time to enable the last source row ID to compare to the currenest value
  #   #entered and not the most historic value entered.  Observed a change, change back situation where the change-back didn't get saved because it reverted to the first historical
  #   #row and then compared it as a non-change when it should have compared to the latest row where a change occured and change-back should be overwritten.
  #   # multiple_data[,repeat_rank:=(.N:1)-1, #0 is first observation, LOWEST as-of date and LOWEST template_row (this will be reversed later for upload!)
  #   #               by=.(rsf_pfcbl_id,
  #   #                    indicator_id)]
  #   
  #   #redundant in time: for each multiple reporting, is value the same as on its preceding row?
  #   multiple_data[,redundant_in_time:=boundary==FALSE &
  #                  is.same_text(data_value,shift(data_value,n=1,fill=as.character(NA))) &
  #                  is.same_text(data_unit,shift(data_unit,n=1,fill=as.character(NA))),
  #                 by=.(rsf_pfcbl_id,
  #                      indicator_id,
  #                      reporting_asof_date)]
  #   
  #   multiple_data[redundant_in_time==FALSE,
  #                 redundant_in_time:= endpoint==FALSE &
  #                                     is.same_text(data_value,shift(data_value,n=-1,fill=as.character(NA))) &
  #                                     is.same_text(data_unit,shift(data_unit,n=-1,fill=as.character(NA))),
  #                 by=.(rsf_pfcbl_id,
  #                      indicator_id,
  #                      reporting_asof_date)]
  #   
  #   redundancies <- multiple_data[redundant_in_time==TRUE]
  #   
  #   pfcbl_data <- template$pfcbl_data
  #   if (!empty(redundancies)) {
  #     
  #     pfcbl_data[,omit:=FALSE]
  #     pfcbl_data[redundancies,
  #                 omit:=TRUE,
  #                 on=.(reporting_template_row_group,
  #                      rsf_pfcbl_id,
  #                      indicator_id,
  #                      reporting_asof_date,
  #                      data_value)]
  #     pfcbl_data <- pfcbl_data[omit==FALSE] 
  #     pfcbl_data[,omit:=NULL]
  #     
  #   } 
  #   
  #   #if any redundancies, these row groups will be omitted and redundancy rank no longer in sequential order
  #   #So we need to re-rank 
  #   #And also, we want redundany rank to be WITHIN reporting_asof_dates, not across them, since a separate chronology rank will be computed later
  #   pfcbl_data[multiple_data,
  #              reporting_redundancy_rank:=repeat_rank,
  #              on=.(rsf_pfcbl_id,
  #                   indicator_id,
  #                   reporting_asof_date,
  #                   reporting_template_row_group)]
  #   
  #   setorder(pfcbl_data,
  #            rsf_pfcbl_id,
  #            indicator_id,
  #            reporting_asof_date,
  #            -reporting_redundancy_rank) #Negative rank, ie, a 1 or 2 or 3...will be ranked above a 0 within the reporting_asof_date
  #   
  #   pfcbl_data[,
  #              reporting_redundancy_rank:=(1:.N)-1, #Re-rank: within as-of date, the highest row number will be 0 (ie, current) and 1,2,3,...will be a redundancy reporting
  #              by=.(rsf_pfcbl_id,
  #                   indicator_id,
  #                   reporting_asof_date)]
  # 
  #   multiple_data <- NULL
  #   redundancies <- NULL
  #   
  #   template$pfcbl_data <- pfcbl_data
  #   
  # }
  # 
  return (template)  
}
