db_data_get_current <- function(pool,
                                rsf_pfcbl_ids.familytree,
                                indicator_ids,
                                reporting_current_date,
                                include.sys_name=TRUE,
                                include.rsf_name=TRUE,
                                include.status=TRUE,
                                include.flags=c("active","resolved"),
                                fx_currency=NA,
                                fx_force_global=TRUE,
                                fx_reported_date=FALSE,
                                fx_concatenate_LCU=TRUE, #Put date unit into same colummn (or not)
                                retry=0)
{  
  if (length(retry) != 1 || all(is.na(retry))) retry <- 0
  
  retry <- retry+1
  
  stale <- dbGetQuery(pool,"
    select 
      ids.rsf_program_id,
      ft.from_rsf_pfcbl_id as rsf_pfcbl_id,
      min(calculation_asof_date) as calculation_asof_date,
      count(*) as calculations
    from p_rsf.view_rsf_pfcbl_id_family_tree ft
    inner join p_rsf.rsf_data_calculation_evaluations dce on dce.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
    inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = ft.from_rsf_pfcbl_id
    where ft.from_rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
      and dce.calculation_asof_date <= $2::date
    group by ids.rsf_program_id,ft.from_rsf_pfcbl_id",
                      params=list(paste0(unique(c(0,rsf_pfcbl_ids.familytree)),collapse=","),
                                  reporting_current_date))
  
  
  if (!empty(stale)) {
    
    showNotification(type="warning",
                     duration=3,
                     ui=h3("Data extract encountered pending calculations.  Calculating..."))
    
    rsf_indicators <- db_indicators_get_labels(pool=pool)
    setDT(stale)
    for (i in 1:nrow(stale)) {
      
      rsf_program_calculate(pool=pool,
                            rsf_indicators=rsf_indicators,
                            rsf_pfcbl_id.family=stale[i,rsf_pfcbl_id],
                            for_import_id=NA,
                            calculate_future=TRUE,
                            reference_asof_date=NULL)
    }
    
    dbExecute(pool,"
      delete from p_rsf.rsf_data_calculation_evaluations dce
      using p_rsf.view_rsf_pfcbl_id_family_tree ft
      where ft.to_family_rsf_pfcbl_id = dce.rsf_pfcbl_id
        and dce.calculation_asof_date <= $2::date
        and ft.from_rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)",
        params=list(paste0(unique(c(0,rsf_pfcbl_ids.familytree)),collapse=","),
                    reporting_current_date))
  }  
    
  #   return(db_data_get_current(pool=pool,
  #                              rsf_pfcbl_ids.familytree=rsf_pfcbl_ids.familytree,
  #                              indicator_ids=indicator_ids,
  #                              reporting_current_date=reporting_current_date,
  #                              include.sys_name=include.sys_name,
  #                              include.rsf_name=include.rsf_name,
  #                              include.status=include.status,
  #                              include.flags=include.flags,
  #          fx_currency=fx_currency,
  #          fx_force_global=fx_force_global,
  #          fx_reported_date=fx_reported_date,
  #          fx_concatenate_LCU=fx_concatenate_LCU, #Put date unit into same colummn (or not)
  #          retry=retry+1))
  # }
  
  rsf_data <- dbGetQuery(pool,"
                    select 
                    
                      dft.rsf_pfcbl_id,
                      dft.pfcbl_category,
                      dft.pfcbl_rank,
                      coalesce(dft.parentest_rsf_pfcbl_id,dft.rsf_pfcbl_id) as parentest_rsf_pfcbl_id,
                      coalesce(dft.parentest_pfcbl_category,dft.pfcbl_category) as parentest_pfcbl_category,
                      dft.indicator_id,
                      dft.data_type,
                      dft.indicator_name,
                      dft.data_id,
                      dft.data_value,
                      dft.data_unit,
                      dft.data_asof_date,
                      dft.data_value_updated,
                      dft.exchange_rate_date,
                      dft.fx_indicator_id,
                      dft.fx_calculation_category,
                      dft.currency_ratio,
                      dft.exchange_rate_data_id,
                      dft.exchange_rate,
                      dft.current_asof_date,
                      dft.current_value,
                      dft.current_unit,
                      array_to_string(dft.flag_ids_active,',') as flag_ids_active,
                      array_to_string(dft.flag_ids_resolved,',') as flag_ids_resolved,
                    
                      sn.sys_name,
                      nids.rsf_full_name,
                      status.quarter_end_reporting_status as reporting_status,
                      status.quarter_reporting_expected as reporting_expected,
                      status.quarter_reporting_exists as reporting_happened

                    from p_rsf.get_data_by_family_tree(input_rsf_pfcbl_ids_familytree => string_to_array($1::text,',')::int[], 
                    																	 input_indicator_ids => string_to_array($2::text,',')::int[],
                    																	 input_current_date => $3::date,
                    																	 input_to_currency => $4::text,
                    																	 fx_force_global => $8::bool,
                                                       fx_reported_date => $9::bool,
                                                       include_flags => $10::bool) as dft 
                    
                    left join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on $5::bool = true
                                                                          and sn.rsf_pfcbl_id = dft.rsf_pfcbl_id																						 
                    left join p_rsf.view_current_entity_names_and_ids nids on $6::bool = true
                                                                          and nids.rsf_pfcbl_id = dft.rsf_pfcbl_id
                    left join lateral p_rsf.get_rsf_pfcbl_id_reporting_status_asof_date(input_rsf_pfcbl_id => dft.rsf_pfcbl_id,
                    																																		input_pfcbl_category => dft.pfcbl_category,
                    																																		input_current_date => $3::date) as status on $7::bool = true
                    ",
                         params=list(paste0(rsf_pfcbl_ids.familytree,collapse=","),
                                     paste0(indicator_ids,collapse=","),
                                     reporting_current_date,
                                     fx_currency,
                                     include.sys_name,
                                     include.rsf_name,
                                     include.status,
                                     fx_force_global,
                                     fx_reported_date,
                                     length(include.flags) > 0))
  
  setDT(rsf_data)
  
  rsf_data[,flags:=list()]
  
  if (empty(rsf_data)) {
    return (NULL)  
  }
  
  if (length(include.flags) > 0) {
    
    flag_ids <- c()
    if (any(include.flags=="resolved",na.rm=T) &&
        length(rsf_data[is.na(flag_ids_resolved)==FALSE,flag_ids_resolved])>0) {
      flag_ids <- paste0(c(flag_ids,
                           paste0(rsf_data[is.na(flag_ids_resolved)==FALSE,flag_ids_resolved],collapse=",")),
                         collapse=",")  
    }
    
    if (any(include.flags=="active",na.rm=T) &&
        length(rsf_data[is.na(flag_ids_active)==FALSE,flag_ids_active])>0) {
      flag_ids <- paste0(c(flag_ids,
                           paste0(rsf_data[is.na(flag_ids_active)==FALSE,flag_ids_active],collapse=",")),
                         collapse=",")  
    }
    
    if (length(flag_ids) > 0 &&
        nchar(flag_ids) > 0) {
      
      flags_data <- dbGetQuery(pool,"
        select 
          rdc.rsf_pfcbl_id,
          rdc.indicator_id,
          rdc.check_asof_date as current_asof_date,
          rdc.check_message,
          ic.check_name,
          coalesce(scc.config_check_class,ic.check_class) as check_class,
          ic.check_type,
          rdc.check_status
        from p_rsf.rsf_data_checks rdc
        inner join p_rsf.indicator_checks ic on ic.indicator_check_id = rdc.indicator_check_id
        left join p_rsf.view_rsf_setup_check_config scc on scc.rsf_pfcbl_id = rdc.rsf_pfcbl_id
                                                       and scc.for_indicator_id = rdc.indicator_id
                                                       and scc.indicator_check_id = rdc.indicator_check_id
        where rdc.evaluation_id = any(select unnest(string_to_array($1::text,','))::int)
        order by rdc.rsf_pfcbl_id,rdc.check_asof_date,rdc.check_status,coalesce(scc.config_check_class,ic.check_class)",
                               params=list(flag_ids))
      
      setDT(flags_data)
      
      if (!empty(flags_data)) {
        flags_data <- flags_data[,
                                 .(flags=list(.SD)),
                                 by=.(rsf_pfcbl_id,
                                      indicator_id,
                                      current_asof_date),
                                 .SDcols=c("check_name",
                                           "check_class",
                                           "check_type",
                                           "check_status",
                                           "check_message")]
        rsf_data[flags_data,
                 flags:=i.flags,
                 on=.(rsf_pfcbl_id,
                      indicator_id,
                      current_asof_date)]
      }
    }        
  }
  rsf_data[,
           `:=`(flag_ids_active=NULL,
                flag_ids_resolved=NULL)]
  
  
  {
    if (!is.na(fx_currency)) {
      data_currency <- rsf_data[data_type == "currency"]
      
      if (fx_concatenate_LCU==TRUE) {
        data_currency[,
                      `:=`(current_unit=as.character(NA),
                           indicator_name=paste0(indicator_name,"@LCU"),
                           data_type="text")]
        
        data_currency[!is.na(data_value),
                      current_value:=paste0(data_value," ",data_unit)]
        
      } else {
        cols <- names(data_currency)
        data_units <- data_currency[,..cols]
        
        data_units[,
                   `:=`(current_value=data_unit,
                        current_unit=as.character(NA),
                        indicator_name=paste0(indicator_name,"@LCUnit"),
                        data_type="text")]
        
        data_currency[,
                      `:=`(current_value=data_value,
                           current_unit=as.character(NA),
                           indicator_name=paste0(indicator_name,"@LCU"))]
        
        data_currency <- rbindlist(list(data_currency,
                                        data_units))
        
        
        
      }
      rsf_data <- rbindlist(list(rsf_data,
                                 data_currency))
    }
    
    
    #I requested to convert to the given fx_currency
    #And I have indicators that should be converted
    if (!is.na(fx_currency) &
        !empty(rsf_data[data_type=="currency" & is.na(exchange_rate)==TRUE & is.na(data_id)==FALSE & is.na(data_value)==FALSE])) {
      
      
      bad_fx <- unique(rsf_data[data_type=="currency" & is.na(exchange_rate)==TRUE & is.na(data_id)==FALSE & is.na(data_value)==FALSE,
                                .(currency_ratio,exchange_rate_date,fx_indicator_id,fx_calculation_category)])
      bad_fx[,c("from_currency","to_currency"):=tstrsplit(currency_ratio,"/")]
      bad_fx[,rsf_pfcbl_id:=0]
      
      good_fx <- db_data_get_fx_ratio(pool,
                                      fx_lookup=unique(bad_fx[,.(rsf_pfcbl_id,exchange_rate_date,to_currency,from_currency)]),
                                      create.indicators=TRUE,
                                      force.global=TRUE,
                                      ignore.validation=FALSE)
      good_fx[,currency_ratio:=paste0(from_currency,"/",to_currency)]
      bad_fx[,omit:=FALSE]
      bad_fx[good_fx,
             omit:=TRUE,
             on=.(currency_ratio,exchange_rate_date,rsf_pfcbl_id)]
      
      if (any(bad_fx$omit==TRUE,na.rm=T) && retry <= 3) {
        
        showNotification(type="warning",
                         duration=3,
                         ui=h3("Data extract encountered an FX issue. Re-try ",retry,"..."))
        
        return(db_data_get_current(pool=pool,
                                   rsf_pfcbl_ids.familytree=rsf_pfcbl_ids.familytree,
                                   indicator_ids=indicator_ids,
                                   reporting_current_date=reporting_current_date,
                                   include.sys_name=include.sys_name,
                                   include.rsf_name=include.rsf_name,
                                   include.status=include.status,
                                   include.flags=include.flags,
               fx_currency=fx_currency,
               fx_force_global=fx_force_global,
               fx_reported_date=fx_reported_date,
               fx_concatenate_LCU=fx_concatenate_LCU, #Put date unit into same colummn (or not)
               retry=retry+1))
      }
      
      stop(paste0("FX failed to convert for ",
                  fx_currency," asof ",
                  as.character(reporting_current_date),
                  ".  No available fx rate exists for the requested currency and date.  Is this a valid currency? Are calculations up to date?",
                  paste0(format(bad_fx[,.(currency_ratio,exchange_rate_date)]),collapse="\n")))
    }
    
    if (any(rsf_data$current_unit=="ERROR",na.rm = T)) {
      stop("Bad data units contain ERROR")
    }
    
    currency_units <- unique(rsf_data[data_type=="currency",current_unit])
    #error check: unifrom fx units
    #if fx_currency is defined, all units should be the same...
    if (fx_concatenate_LCU==FALSE) {
      rsf_data[!is.na(current_unit) &
                 data_type == "currency",
               indicator_name := paste0(indicator_name,"@",current_unit)]
    } else {
      if (!is.na(fx_currency)) {
        rsf_data[data_type=="currency",
                 n:=length(unique(current_unit)),
                 by=.(indicator_id)]
        
        if (any(rsf_data$n > 1,na.rm=T)) {
          rsf_data[n>1]
          stop("Bad data units")
        }
        
        rsf_data[,
                 n:=NULL]
        
        #make the fx converstion visually explicit in the column name
        rsf_data[!is.na(current_unit) &
                   data_type == "currency" &
                   current_unit==fx_currency,
                 indicator_name := paste0(indicator_name,"@",current_unit)] #if it converts to indicator_name@LCU this will possibly make unintentional duplicates to rbind above
        
        #else it's not defined, so we might have uniform data units conveniently; or many different ones, depending on the query
      }
      else { 
        
        rsf_data[,
                 n:=length(unique(current_unit)),
                 by=.(indicator_id)]
        
        #if we have multiple different data units, without request to convert them, this is assumed to be expected/anticipated
        #so the units will be appended into the data value and converted into a text field.
        if (any(rsf_data$n > 1)) {
          rsf_data[n>1 &
                     data_type == "currency",
                   `:=`(indicator_name = paste0(indicator_name,"@LCU"),
                        current_value = paste0(current_value," ",current_unit),
                        data_type="text")]
          
          rsf_data[n>1 &
                     data_type == "currency_ratio",
                   `:=`(current_value = paste0(current_value," ",current_unit),
                        data_type="text")]
          
        }
      }
    }
  }
  
  
  return (rsf_data)
}

# 
# 
# db_data_get_current <- function(pool,
#                                 rsf_pfcbl_ids.familytree,
#                                 indicator_ids,
#                                 reporting_current_date,
#                                 include.sys_name=TRUE,
#                                 include.rsf_name=TRUE,
#                                 include.status=TRUE,
#                                 include.flags=c("active","resolved"),
#                                 fx_currency=NA,
#                                 fx_force_global=TRUE,
#                                 fx_reported_date=FALSE,
#                                 fx_concatenate_LCU=TRUE #Put date unit into same colummn (or not)
#                                 )
# {  
#   
#   rsf_data <- dbGetQuery(pool,"
#                     select 
#                     
#                       dft.rsf_pfcbl_id,
#                       dft.pfcbl_category,
#                       dft.pfcbl_rank,
#                       dft.parent_rsf_pfcbl_id,
#                       dft.parent_pfcbl_category,
#                       dft.indicator_id,
#                       dft.data_type,
#                       dft.indicator_name,
#                       dft.data_id,
#                       dft.data_value,
#                       dft.data_unit,
#                       dft.data_asof_date,
#                       dft.data_value_updated,
#                       dft.exchange_rate_date,
#                       dft.fx_indicator_id,
#                       dft.fx_calculation_category,
#                       dft.currency_ratio,
#                       dft.exchange_rate_data_id,
#                       dft.exchange_rate,
#                       dft.current_asof_date,
#                       dft.current_value,
#                       dft.current_unit,
#                       array_to_string(dft.flag_ids_active,',') as flag_ids_active,
#                       array_to_string(dft.flag_ids_resolved,',') as flag_ids_resolved,
#                     
#                       sn.sys_name,
#                       nids.rsf_full_name,
#                       status.quarter_end_reporting_status as reporting_status,
#                       status.quarter_reporting_expected as reporting_expected,
#                       status.quarter_reporting_exists as reporting_happened
# 
#                     from p_rsf.get_data_by_family_tree(input_rsf_pfcbl_ids_familytree => string_to_array($1::text,',')::int[], 
#                     																	 input_indicator_ids => string_to_array($2::text,',')::int[],
#                     																	 input_current_date => $3::date,
#                     																	 input_to_currency => $4::text,
#                     																	 fx_force_global => $8::bool,
#                                                        fx_reported_date => $9::bool,
#                                                        include_flags => $10::bool) as dft 
#                     
#                     left join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on $5::bool = true
#                                                                           and sn.rsf_pfcbl_id = dft.rsf_pfcbl_id																						 
#                     left join p_rsf.view_current_entity_names_and_ids nids on $6::bool = true
#                                                                           and nids.rsf_pfcbl_id = dft.rsf_pfcbl_id
#                     left join lateral p_rsf.get_rsf_pfcbl_id_reporting_status_asof_date(input_rsf_pfcbl_id => dft.rsf_pfcbl_id,
#                     																																		input_pfcbl_category => dft.pfcbl_category,
#                     																																		input_current_date => $3::date) as status on $7::bool = true
#                     ",
#                     params=list(paste0(rsf_pfcbl_ids.familytree,collapse=","),
#                                 paste0(indicator_ids,collapse=","),
#                                 reporting_current_date,
#                                 fx_currency,
#                                 include.sys_name,
#                                 include.rsf_name,
#                                 include.status,
#                                 fx_force_global,
#                                 fx_reported_date,
#                                 length(include.flags) > 0))
#   
#   setDT(rsf_data)
#   
#   rsf_data[,flags:=list()]
#   
#   if (empty(rsf_data)) {
#     return (NULL)  
#   }
#   
#   if (length(include.flags) > 0) {
#     
#     flag_ids <- c()
#     if (any(include.flags=="resolved",na.rm=T) &&
#         length(rsf_data[is.na(flag_ids_resolved)==FALSE,flag_ids_resolved])>0) {
#       flag_ids <- paste0(c(flag_ids,
#                            paste0(rsf_data[is.na(flag_ids_resolved)==FALSE,flag_ids_resolved],collapse=",")),
#                          collapse=",")  
#     }
#     
#     if (any(include.flags=="active",na.rm=T) &&
#         length(rsf_data[is.na(flag_ids_active)==FALSE,flag_ids_active])>0) {
#       flag_ids <- paste0(c(flag_ids,
#                            paste0(rsf_data[is.na(flag_ids_active)==FALSE,flag_ids_active],collapse=",")),
#                          collapse=",")  
#     }
# 
#     if (length(flag_ids) > 0 &&
#         nchar(flag_ids) > 0) {
#       
#       flags_data <- dbGetQuery(pool,"
#         select 
#           rdc.rsf_pfcbl_id,
#           rdc.indicator_id,
#           rdc.check_asof_date as current_asof_date,
#           rdc.check_message,
#           ic.check_name,
#           coalesce(icg.overwrite_check_class,ic.check_class) as check_class,
#           ic.check_type,
#           rdc.check_status
#         from p_rsf.rsf_data_checks rdc
#         inner join p_rsf.indicator_checks ic on ic.indicator_check_id = rdc.indicator_check_id
#         left join p_rsf.indicator_check_guidance icg on icg.indicator_check_guidance_id = rdc.indicator_check_guidance_id
#         where rdc.evaluation_id = any(select unnest(string_to_array($1::text,','))::int)
#         order by rdc.rsf_pfcbl_id,rdc.check_asof_date,rdc.check_status,coalesce(icg.overwrite_check_class,ic.check_class)",
#         params=list(flag_ids))
#       
#       setDT(flags_data)
#       
#       if (!empty(flags_data)) {
#         flags_data <- flags_data[,
#                                  .(flags=list(.SD)),
#                                  by=.(rsf_pfcbl_id,
#                                       indicator_id,
#                                       current_asof_date),
#                                  .SDcols=c("check_name",
#                                            "check_class",
#                                            "check_type",
#                                            "check_status",
#                                            "check_message")]
#         rsf_data[flags_data,
#                  flags:=i.flags,
#                  on=.(rsf_pfcbl_id,
#                       indicator_id,
#                       current_asof_date)]
#       }
#     }        
#   }
#   rsf_data[,
#            `:=`(flag_ids_active=NULL,
#                 flag_ids_resolved=NULL)]
#   {
#     if (!is.na(fx_currency)) {
#       data_currency <- rsf_data[data_type == "currency"]
#   
#       if (fx_concatenate_LCU==TRUE) {
#         data_currency[,
#                    `:=`(current_unit=as.character(NA),
#                         indicator_name=paste0(indicator_name,"@LCU"),
#                         data_type="text")]
#         
#         data_currency[!is.na(data_value),
#                       current_value:=paste0(data_value," ",data_unit)]
#         
#       } else {
#         cols <- names(data_currency)
#         data_units <- data_currency[,..cols]
#         
#         data_units[,
#                    `:=`(current_value=data_unit,
#                         current_unit=as.character(NA),
#                         indicator_name=paste0(indicator_name,"@LCUnit"),
#                         data_type="text")]
#         
#         data_currency[,
#                       `:=`(current_value=data_value,
#                            current_unit=as.character(NA),
#                            indicator_name=paste0(indicator_name,"@LCU"))]
#         
#         data_currency <- rbindlist(list(data_currency,
#                                         data_units))
#         
#      
#         
#       }
#       rsf_data <- rbindlist(list(rsf_data,
#                              data_currency))
#     }
#     
#     
#     #I requested to convert to the given fx_currency
#     #And I have indicators that should be converted
#     if (!is.na(fx_currency) &
#         !empty(rsf_data[data_type=="currency" & is.na(exchange_rate)==TRUE & is.na(data_id)==FALSE & is.na(data_value)==FALSE])) {
#       
#       stop(paste0("FX failed to convert for ",
#                   fx_currency," asof ",
#                   as.character(reporting_current_date),
#                   ".  No available fx rate exists for the requested currency and date.  Is this a valid currency? Are calculations up to date?"))
#     }
# 
#     if (any(rsf_data$current_unit=="ERROR",na.rm = T)) {
#       stop("Bad data units contain ERROR")
#     }
#     
#     currency_units <- unique(rsf_data[data_type=="currency",current_unit])
#     #error check: unifrom fx units
#     #if fx_currency is defined, all units should be the same...
#     if (fx_concatenate_LCU==FALSE) {
#       rsf_data[!is.na(current_unit) &
#            data_type == "currency",
#            indicator_name := paste0(indicator_name,"@",current_unit)]
#     } else {
#       if (!is.na(fx_currency)) {
#         rsf_data[data_type=="currency",
#              n:=length(unique(current_unit)),
#              by=.(indicator_id)]
#         
#         if (any(rsf_data$n > 1,na.rm=T)) {
#           rsf_data[n>1]
#           stop("Bad data units")
#         }
#         
#         rsf_data[,
#              n:=NULL]
#         
#         #make the fx converstion visually explicit in the column name
#         rsf_data[!is.na(current_unit) &
#              data_type == "currency" &
#              current_unit==fx_currency,
#              indicator_name := paste0(indicator_name,"@",current_unit)] #if it converts to indicator_name@LCU this will possibly make unintentional duplicates to rbind above
#         
#       #else it's not defined, so we might have uniform data units conveniently; or many different ones, depending on the query
#       }
#       else { 
#         
#         rsf_data[,
#              n:=length(unique(current_unit)),
#              by=.(indicator_id)]
#         
#         #if we have multiple different data units, without request to convert them, this is assumed to be expected/anticipated
#         #so the units will be appended into the data value and converted into a text field.
#         if (any(rsf_data$n > 1)) {
#           rsf_data[n>1 &
#                data_type == "currency",
#                `:=`(indicator_name = paste0(indicator_name,"@LCU"),
#                     current_value = paste0(current_value," ",current_unit),
#                     data_type="text")]
#           
#           rsf_data[n>1 &
#                data_type == "currency_ratio",
#                `:=`(current_value = paste0(current_value," ",current_unit),
#                     data_type="text")]
#           
#         }
#       }
#     }
#   }
# 
#  
#   return (rsf_data)
# }