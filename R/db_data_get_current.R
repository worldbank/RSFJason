db_data_get_current <- function(pool,
                                for_program_id,
                                rsf_pf_ids, 
                                indicator_ids,
                                reporting_current_date,
                                include.sys_name=TRUE,
                                include.flags=c("active","resolved"),
                                fx_currency=NA,
                                fx_force_global=TRUE,
                                fx_reported_date=FALSE,
                                fx_concatenate_LCU=TRUE, #Put date unit into same colummn (or not)
                                retry=0,
                                CALCULATIONS_ENVIRONMENT)
{  
  if (length(retry) != 1 || all(is.na(retry))) retry <- 0
  #filter.status <- toupper(match.arg(filter.status))
  if (!length(rsf_pf_ids)) return (NULL)
  if (anyNA(suppressWarnings(as.numeric(rsf_pf_ids)))) stop("Missing rsf_pf_id submitted to db_get_data_current()")
  
  retry <- retry+1

  pf_family <- dbGetQuery(pool,"
    select 
      pf.rsf_pfcbl_id,
      pf.rsf_pf_id,
      min(calculation_asof_date) as calculation_asof_date,
      pf.is_collection,
      pf.pfcbl_rank,
      count(dce.calculation_asof_date) as calculations
    from (
      select distinct gpf.rsf_pf_id,gpf.is_collection,gpf.pfcbl_rank,ids.rsf_pfcbl_id
      from p_rsf.rsf_pfcbl_ids ids 
      cross join lateral (values (ids.rsf_gpfcbl_family[1],false,0),
                                 (ids.rsf_gpfcbl_family[2],false,1),
                                 (ids.rsf_gpfcbl_family[3],false,2)
                                 
                          union all
                          
                          select rf.rsf_program_id,true,1
                          from p_rsf.rsf_facilities rf
                          where rf.rsf_facility_id = ids.rsf_facility_id
                            and rf.rsf_program_id <> ids.rsf_program_id
                         ) gpf(rsf_pf_id,is_collection,pfcbl_rank)
      where ids.rsf_pfcbl_id = any($1::int[])
        and ids.pfcbl_category_rank <= 2
        and gpf.rsf_pf_id is not null
    ) as pf
    left join p_rsf.rsf_data_calculation_evaluations dce on dce.rsf_pf_id = pf.rsf_pf_id
                                                        and dce.calculation_asof_date <= $2::date
    group by 
      pf.rsf_pfcbl_id,
      pf.rsf_pf_id,
      pf.pfcbl_rank,
      pf.is_collection",
    params=list(dbMakeIntArray(rsf_pf_ids),
                reporting_current_date))
  
  setDT(pf_family)
  stale <- unique(pf_family[calculations > 0,rsf_pf_id])
  if (length(stale)) {
    
    showNotification(type="warning",
                     duration=3,
                     ui=h3("Data extract encountered pending calculations.  Calculating..."))
    
    rsf_indicators <- db_indicators_get_labels(pool=pool)
    #setDT(stale)
    for (sid in stale) {
      
      rsf_program_calculate(pool=pool,
                            rsf_indicators=rsf_indicators,
                            rsf_pf_id=sid,
                            for_import_id=NA,
                            reference_asof_date=reporting_current_date,
                            CALCULATIONS_ENVIRONMENT=CALCULATIONS_ENVIRONMENT)
    }
    
    #rsf program calculation should clean-up after itself quite nicely.
    #this is here because sometimes, rarely, a calculation can "get stuck" for evaluation such as if a user changes an indicator from a previously
    #defined calculation formula to decide it should be manually reported data with no calculation -- and then the calculator will pick-up the evaluation
    #and find there's no formula and not be able to compute it and leave it hanging.  This too should clean up after itself; but just in case as there's 
    #little overhead.
    dbExecute(pool,"
      delete from p_rsf.rsf_data_calculation_evaluations dce
      where dce.rsf_pf_id = any($1::int[])
        and dce.calculation_asof_date <= $2::date",
        params=list(dbMakeIntArray(stale),
                    reporting_current_date))
  }  

  #include all requested pf_ids,primary program,collections programs,global
  #query_pf_ids <- pf_family[pfcbl_rank==min(pfcbl_rank),rsf_pf_id]

  # #But querying multiple progams/collections (unless explicitly asked, will be very confusing results)
  # if (!any(rsf_pf_ids %in% pf_family[pfcbl_rank==1,rsf_pf_id],na.rm=T)) {
  #   #here we haven't explicitly asked for any rsf_program_ids, so default to primary 
  #   query_pf_ids <- unique(pf_family[rsf_pf_id %in% rsf_pf_ids | is_collection==FALSE,rsf_pf_id])
  # } else {
  #   query_pf_ids <- unique(pf_family[rsf_pf_id %in% rsf_pf_ids,rsf_pf_id])
  # }

  # Note that if program-level indicators are requested, all upstream programs and collections will be returned 
  #And then must be filtered-out based on functions rsf_program_id (both to mitigate confusion and ensure read permissions to the selected program)
  
  if (!all(rsf_pf_ids==for_program_id,na.rm=T) & any(rsf_pf_ids==for_program_id,na.rm=T)) {
    #Because if user selects program from drop-down list and also a facility, then query with family-tree will select all facilities since facility-level
    #indicators will bring-in all program's facilities, whic his probably not the intention.  But if only the program is selected and facility+ metrics
    #selected, then it serves as a "select all"
    rsf_pf_ids <- rsf_pf_ids[!(rsf_pf_ids==for_program_id)]
  } 
  
  rsf_data <- dbGetQuery(pool,"
                    select 
                      ids.rsf_program_id,
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
                      nids.sys_name,
                      nids.rsf_full_name,
                      case
                        when $3::date < ids.created_in_reporting_asof_date then 'NA'
                        when ids.deactivated_in_reporting_asof_date is NULL then 'ACTIVE'
                        when ids.deactivated_in_reporting_asof_date < $3::date then 'INACTIVE'
                        when ids.deactivated_in_reporting_asof_date >= $3::date then 'ACTIVE'
                        else 'ERROR' end as reporting_status

                    from p_rsf.get_data_by_family_tree(input_rsf_pfcbl_ids_familytree => ($1::int[]), 
                    																	 input_indicator_ids => ($2::int[]),
                    																	 input_current_date => $3::date,
                    																	 input_to_currency => $4::text,
                    																	 fx_force_global => $5::bool,
                                                       fx_reported_date => $6::bool,
                                                       include_flags => $7::bool) as dft 
                    inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = dft.rsf_pfcbl_id
                    left join p_rsf.view_current_entity_names_and_ids nids on $8::bool = true
                                                                          and nids.rsf_pfcbl_id = dft.rsf_pfcbl_id
                    ",
                         params=list(dbMakeIntArray(rsf_pf_ids), #1
                                     dbMakeIntArray(indicator_ids),            #2
                                     reporting_current_date,                        #3
                                     fx_currency,                                   #4
                                     fx_force_global,                               #5
                                     fx_reported_date,                              #6
                                     length(include.flags) > 0,                     #7
                                     include.sys_name                               #8
                                     ))

  # where case when NULLIF($9::text,'NA') is NULL then true
  # when $9::text = 'BOTH' then true
  # else $9::text is not distinct from reporting.status end
  
  setDT(rsf_data)
  
  if (!is.na(for_program_id)) {
    
    #because if we request a collection, p_rsf.get_data_by_family_tree() will return both the collection AND the primary program
    rsf_data <- rsf_data[(pfcbl_category != "program") |
                         (pfcbl_category == "program" & rsf_program_id %in% for_program_id)]
    
    rsf_data <- rsf_data[(parentest_pfcbl_category  != "program") |
                           (parentest_pfcbl_category  == "program" & parentest_rsf_pfcbl_id  %in% for_program_id)]
    
  }
  
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
    flag_ids <- as.character(flag_ids)
    flag_ids <- suppressWarnings(as.numeric(unlist(strsplit(flag_ids,split=",",fixed=TRUE))))
    flag_ids <- na.omit(flag_ids)
    
    if (length(flag_ids)) {
      
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
        left join p_rsf.rsf_setup_checks_config scc on scc.config_id = rdc.config_id
        where rdc.evaluation_id = any($1::int[])
        order by 
          rdc.rsf_pfcbl_id,
          rdc.check_asof_date,
          rdc.check_status,
          coalesce(scc.config_check_class,ic.check_class)",
      params=list(dbMakeIntArray(flag_ids)))
      
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
                                      ignore.validation=FALSE,
                                      CALCULATIONS_ENVIRONMENT=CALCULATIONS_ENVIRONMENT)
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
                                   rsf_pf_ids=rsf_pf_ids,
                                   indicator_ids=indicator_ids,
                                   reporting_current_date=reporting_current_date,
                                   include.sys_name=include.sys_name,
                                   include.flags=include.flags,
                                   fx_currency=fx_currency,
                                   fx_force_global=fx_force_global,
                                   fx_reported_date=fx_reported_date,
                                   fx_concatenate_LCU=fx_concatenate_LCU, #Put date unit into same colummn (or not)
                                   retry=retry+1,
                                   CALCULATIONS_ENVIRONMENT=CALCULATIONS_ENVIRONMENT))
        
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
