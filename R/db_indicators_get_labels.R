db_indicators_get_labels <- function(pool) {
  #conn <- poolCheckout(pool)
  #dbBegin(conn)
  #dbRollback(conn)
  
    
  indicators <- poolWithTransaction(pool,function(conn) {
    
    indicators <- dbGetQuery(conn,"
                            select 

                              ind.indicator_id,
                              ind.indicator_name,
                              ind.data_category,
                              rpc1.pfcbl_rank as indicator_pfcbl_rank,
                              ind.data_type,
                              ind.data_unit,
                              ind.default_value,
                              ind.is_system,
                              ind.is_calculated,
                              ind.is_data_unit,
                              ind.label_id,
                              ind.indicator_sys_category,
                              ind.indicator_options_group_id as options_group_id,
                              ind.indicator_options_group_allows_blanks,
                              ind.indicator_options_group_allows_multiples,
                              ind.is_setup,
                              ind.is_periodic_or_flow_reporting,
                              ind.default_subscription,
                              ind.classification,
                              ind.definition,
                              ind.is_system_calculated,
                              ind.is_static_nonreporting,
                              ind.sort_preference,
                              exists(select * from p_rsf.indicator_formulas indf
                                     where indf.indicator_id = ind.indicator_id
                                       and indf.overwrite <> 'allow') as is_user_calculatable
                            from p_rsf.indicators ind
                            inner join p_rsf.rsf_pfcbl_categories rpc1 on rpc1.pfcbl_category = ind.data_category")

    setDT(indicators)
    

    indicator_labels <- dbGetQuery(conn,"
                                  select 
                                    lab.indicator_id,
                                    lab.label_id,
                                    lab.label_key,
                                    lab.label,
                                    lab.is_primary,
                                    lk.key_type,
                                    lk.key_type_template_id,
                                    lab.indicator_id || '-0' as indicator_header_id
                                  from p_rsf.view_indicator_labels lab
                                  left join p_rsf.label_keys lk on lk.label_key = lab.label_key")  
    setDT(indicator_labels)
    # indicator_labels <- indicator_labels[is.na(key_type) |
    #                                      key_type=="language"]
    
    indicator_labels[,
                     label_normalized:=normalizeLabel(label)]
    
    indicator_labels[,
                     redundancy_error:=length(unique(indicator_id))>1,
                     by=.(label_normalized)]
    
    # if (any(indicator_labels$redundancy_error==TRUE)) {
    #   dups <- indicator_labels[n>1]
    #   dups[indicators,
    #        indicator_name:=i.indicator_name,
    #        on=.(indicator_id)]
    #   
    #   dups <- dups[,.(msg=paste0(sort(unique(paste0(label,"=",indicator_name," (",indicator_id,")"))),collapse=" & ")),
    #                by=.(label_normalized)]
    #   
    #   stop(paste0("Identical indicator labels identify different indicators (This must be fixed in JASON SYSTEM ADMIN for these indicator aliases:\n ",
    #               paste0(dups$msg,collapse=" \n ")))
    # }
    
    indicator_labels <- indicator_labels[,
                                         .(labels=list(.SD),
                                           redundancy_error=any(redundancy_error)), 
                                         by=.(label_id),
                                         .SDcols = c("indicator_id",
                                                     "indicator_header_id",
                                                     "label_key",
                                                     "label",
                                                     "label_normalized",
                                                     "key_type",
                                                     "key_type_template_id",
                                                     "is_primary",
                                                     "redundancy_error")]
    indicators[,labels:=list()]
    
    indicators[indicator_labels,
               `:=`(labels=i.labels,
                    redundancy_error=i.redundancy_error),
               on=.(label_id)]
   
  
    options_labels <- dbGetQuery(conn,"
                                select 
                                  vol.options_group_id,
                                  vol.options_group_name,
                                  vol.options_group_data_type,
                                  vol.options_group_key,
                                  vol.label_id,
                                  vol.label_key,
                                  vol.label,
                                  vol.is_primary
                                from p_rsf.view_options_labels vol")
                                 
    setDT(options_labels)
    
   
      bad_data_types <- indicators[!is.na(options_group_id),.(indicator_id,options_group_id,indicator_name,indicator_data_type=data_type)
                                   ][unique(options_labels[,.(options_group_id,options_group_data_type)]),
                                     on=.(options_group_id)][indicator_data_type != options_group_data_type]
      if (!empty(bad_data_types)) {
        stop(paste0("Fetched indicators ",paste0(bad_data_types$indicator_name,collapse=", "),
                    " with respective data types ",
                    paste0(bad_data_types$indicator_data_type,collapse=", "), 
                    " that do not match respective options group data types ",paste0(bad_data_types$options_group_data_type,collapse=", ")))
      }
      bad_data_types <- NULL
   
      options_labels <- options_labels[,.(options_group=list(.SD)),
                                       by=.(options_group_id,
                                            options_group_name),
                                       .SDcols=c("options_group_key","label_id","label_key","label","is_primary")]
      indicators[options_labels,
                 `:=`(options_group_name=i.options_group_name,
                      options_group=i.options_group),
                 on=.(options_group_id)]

    
    indicators
  })
    

  
  
  return (indicators)
}
