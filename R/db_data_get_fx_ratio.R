



db_data_get_fx_ratio <- function(pool,
                                 fx_lookup,
                                 create.indicators=TRUE,
                                 force.global=FALSE) {
  #browser()
  #fx_table <- fx_required
  #fxt <<- as.data.frame(fx_lookup)
  #fx_lookup <- as.data.table(fxt)
  #fx_lookup[,`:=`(rsf_pfcbl_id=lookup_rsf_pfcbl_id)];fx_lookup <- fx_lookup[,.(rsf_pfcbl_id,exchange_rate_date,to_currency,from_currency)]
  if (!is.data.table(fx_lookup)) stop("fx_table should be data.table")
  if (!all(c("rsf_pfcbl_id",
             "exchange_rate_date",
             "to_currency",
             "from_currency") %in% names(fx_lookup))) stop("fx_table should define columns rsf_pfcbl_id,currency_ratio,exchange_rate_date")
  #if (length(unique(fx_table$exchange_rate_date)) != 1) stop("Only one exchange rate date can be requested/uploaded per query")
  
  #If we're passing-in an rsf_client_id, it means all entities here are children of this client_id
  fx_lookup <- unique(fx_lookup)

  #indicator check constraint requires currency_ratio indicators to be defined at global, program or facility level.  Therefore, we can lookup fx values
  #at an entitiy's facility level and save a number of fx conversion loops
  pfcbl_ids.facility <- dbGetQuery(pool,"
                                   select
                                    fam.parent_rsf_pfcbl_id as lookup_rsf_pfcbl_id,
                                    fam.child_rsf_pfcbl_id as rsf_pfcbl_id
                                   from p_rsf.rsf_pfcbl_id_family fam
                                   where fam.child_rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
                                     and fam.parent_pfcbl_category = 'facility'",
                                   params=list(paste0(unique(fx_lookup$rsf_pfcbl_id),collapse=",")))
  
  setDT(pfcbl_ids.facility)  
   
  fx_lookup[,
            lookup_rsf_pfcbl_id:=rsf_pfcbl_id]
  

  fx_lookup[pfcbl_ids.facility,
            lookup_rsf_pfcbl_id:=i.lookup_rsf_pfcbl_id,
            on=.(rsf_pfcbl_id)]

  fx_lookup <- fx_lookup[,
                         .(rsf_pfcbl_ids=list(rsf_pfcbl_id)),
                         by=.(lookup_rsf_pfcbl_id,
                              exchange_rate_date,
                              to_currency,
                              from_currency)]
  
  
  fx_lookup <- fx_lookup[grepl("LCU|LCY",to_currency)==FALSE]
  fx_lookup <- fx_lookup[grepl("LCU|LCY",from_currency)==FALSE]
  
  
  fx_lookup <- fx_lookup[grepl("^[A-Z]{3}$",to_currency)==TRUE]
  fx_lookup <- fx_lookup[grepl("^[A-Z]{3}$",from_currency)==TRUE]
  
  #Internal fx functions for lookups
  {
    fx_currency_ratio_in_alphabetic_order <- function(to_currency,
                                                      from_currency) {
      
      mapply(function(f,t) {
        if (f==t) paste0(f,"/",t)
        else if (f < t) paste0(f,"/",t)
        else paste0(t,"/",f)
        
      },
      f=from_currency,
      t=to_currency)
    }
    
    get_fx_rate_catch <- function(exchange_rate_date,
                                  currency_code_ratio) {
      tryCatch({
        #print(paste0("Dremio lookup: ",currency_code_ratio," ",exchange_rate_date))
        get_fx_rate(exchange_rate_date=exchange_rate_date,
                    currency_code_ratio=currency_code_ratio)
      },
      warning=function(w) { stop(paste0("Error: failed to connect to IFC fx Dremio database and lookup rates for: ",exchange_rate_date," ",currency_code_ratio))},
      error=function(e) { stop(paste0("Error: failed to connect to IFC fx Dremio database and lookup rates for: ",exchange_rate_date," ",currency_code_ratio)) })
    }
    
    db_lookup_fx_ratios <- function(pool,
                                    lookup,
                                    force.global) {
      
      force.global <- as.logical(force.global)
      if (is.na(force.global)) force.global <- FALSE
      
      lookup <- lookup[,
                       .(lookup_rsf_pfcbl_id,
                         exchange_rate_date,
                         to_currency,
                         from_currency)]
      
      #conn <- poolCheckout(pool)
      #dbBegin(conn)
      #dbRollback(conn)
      fx_ratios <- poolWithTransaction(pool,function(conn) {
        
        dbExecute(conn,"create temp table _temp_fx(lookup_rsf_pfcbl_id int,
                                               exchange_rate_date date,
                                               to_currency text,
                                               from_currency text)
                        on commit drop;")
        
        dbAppendTable(conn,
                      name="_temp_fx",
                      value=lookup)
        
        dbExecute(conn,"analyze _temp_fx")
        
        dbGetQuery(conn,"
                         select
                         tfx.lookup_rsf_pfcbl_id,
                         tfx.exchange_rate_date,
                         tfx.to_currency,
                         tfx.from_currency,
                         rates.currency_ratio,
                         rates.fx_pfcbl_category,
                         rates.fx_indicator_id,
                         rates.exchange_rate,
                         rates.exchange_rate_data_id,
                         coalesce(rates.is_invalidated,false) as is_invalidated,
                         coalesce(rates.is_unreported,true) as is_unreported
                         from _temp_fx tfx
                         left join lateral p_rsf.fx_pfcbl_convert_currency(input_rsf_pfcbl_id => tfx.lookup_rsf_pfcbl_id,
                                                                           input_to_currency => tfx.to_currency,
                                                                           input_from_currency => tfx.from_currency,
                                                                           input_exchange_rate_date => tfx.exchange_rate_date,
                                                                           input_force_global_fx_rate => $1::bool) rates on true",
                   params=list(force.global))
        
        
      })
      
      setDT(fx_ratios)
      
      return (fx_ratios)
    }
  }
  
  if (empty(fx_lookup)) {
    return (data.table(rsf_pfcbl_id=numeric(0),
                       to_currency=character(0),
                       from_currency=character(0),
                       currency_ratio=character(0),
                       exchange_rate_date=as.Date(numeric(0)),
                       exchange_rate=numeric(0),
                       exchange_rate_data_id=numeric(0),
                       fx_pfcbl_category=character(0)))
  }


  
  #browser()
  #conn <- poolCheckout(pool); dbBegin(conn)
  #dbRollback(conn)

  
  
  fx_ratios <- db_lookup_fx_ratios(pool=pool,
                                   lookup=fx_lookup[,
                                                    .(lookup_rsf_pfcbl_id,
                                                      exchange_rate_date,
                                                      to_currency,
                                                      from_currency)],
                                   force.global=force.global)

  fx_lookup[,
            `:=`(currency_ratio=as.character(NA),
                 exchange_rate=as.numeric(NA),
                 fx_pfcbl_category=as.character(NA),
                 fx_indicator_id=as.numeric(NA),
                 exchange_rate_data_id=as.numeric(NA),
                 is_invalidated=as.logical(NA),
                 is_unreported=as.logical(NA))]
  
  
  fx_lookup[fx_ratios,
           `:=`(currency_ratio=i.currency_ratio,
                exchange_rate=i.exchange_rate,
                fx_pfcbl_category=i.fx_pfcbl_category,
                fx_indicator_id=i.fx_indicator_id,
                exchange_rate_data_id=i.exchange_rate_data_id,
                is_invalidated=i.is_invalidated,
                is_unreported=i.is_unreported),
           on=.(lookup_rsf_pfcbl_id,
                exchange_rate_date,
                to_currency,
                from_currency)]
  
  
  #Creates a new indicator, or looks up existing one if that indicator has no entry in timeseries data for some reason
  if (!empty(fx_lookup[is.na(fx_indicator_id) & is.na(exchange_rate)]) && create.indicators==TRUE) {
   
    valid_codes <- get_fx_codes()
    
    
    fx_codes <- unique(fx_lookup[is.na(fx_indicator_id) & 
                                 is.na(exchange_rate) &
                                 to_currency %in% valid_codes &
                                 from_currency %in% valid_codes,
                                 .(to_currency,
                                   from_currency)])
    
    fx_codes[,alphabetic_lookup_ratio:=fx_currency_ratio_in_alphabetic_order(to_currency=to_currency,
                                                                             from_currency = from_currency)]
    
    fx_codes[,`:=`(fx_pfcbl_category=as.character(NA),
                   fx_indicator_id=as.numeric(NA))]
    
    fx_indicators <- dbGetQuery(pool,"
                                select
                                  ind.indicator_id,
                                  ind.data_unit as alphabetic_lookup_ratio
                                from p_rsf.indicators ind
                                where ind.data_category = 'global'
                                  and data_unit = any(select unnest(string_to_array($1::text,','))::text)
                                ",params=list(paste0(unique(fx_codes$alphabetic_lookup_ratio),collapse=",")))
    
    setDT(fx_indicators)
    
    fx_codes[fx_indicators,
             `:=`(fx_pfcbl_category="global",
                  fx_indicator_id=i.indicator_id),
             on=.(alphabetic_lookup_ratio)]
    
    
    if (!empty(fx_codes[is.na(fx_indicator_id)==TRUE])) {
      
      #conn <- poolCheckout(pool)
      #dbBegin(conn)
      #dbRollback(conn)
      new_indicators <- poolWithTransaction(pool,function(conn) { 
    
        dbExecute(conn,"create temporary table _temp_currency_codes(alphabetic_lookup_ratio text,
                                                                    indicator_id int default NULL,
                                                                    label_id int default NULL,
                                                                    create_indicator bool default true)
                        on commit drop;")
        
        dbAppendTable(conn,
                      name="_temp_currency_codes",
                      value=unique(fx_codes[is.na(fx_indicator_id)==TRUE,
                                            .(alphabetic_lookup_ratio)])) #Unique ensures no duplicate currency_ratio variables 
                                                                                        #where currency_ratio_requested may be inverted
        
        
        dbExecute(conn,"update _temp_currency_codes tcc
                        set create_indicator = false
                        where exists(select * from p_rsf.indicators ind 
                                     where ind.data_category = 'global'
                                       and ind.data_type = 'currency_ratio'
                                       and p_rsf.fx_currency_ratio_in_alphabetic_order(ind.data_unit) = 
                                           p_rsf.fx_currency_ratio_in_alphabetic_order(tcc.alphabetic_lookup_ratio))")
        
        #dbGetQuery(conn,"select * from _temp_currency_codes")
        dbExecute(conn,"DO $$ BEGIN
                        if exists(select * from _temp_currency_codes where create_indicator=true) then
                        
                          update _temp_currency_codes tcc
                          set alphabetic_lookup_ratio = p_rsf.fx_currency_ratio_in_alphabetic_order(tcc.alphabetic_lookup_ratio)
                          where tcc.create_indicator = true;
                        
                          with new_currencies as (
                          	select 
                          		alphabetic_lookup_ratio,
                          		regexp_replace(alphabetic_lookup_ratio,'/','_') as currency_ratio_name
                          	from _temp_currency_codes
                          	where create_indicator = true
                          ),
                          new_indicators as (
                          	insert into p_rsf.indicators(indicator_name,
                          															 data_category,
                          															 data_type,
                          															 is_calculated,
                          															 is_system,
                          															 definition,
                          															 data_unit,
                          															 modification_time,
                          															 is_system_calculated,
                                                         default_subscription)
                           select 
                          	'sys_global_fx_' || currency_ratio_name,
                          	'global' as data_category,
                          	'currency_ratio' as data_type,
                          	true as is_calculated,
                          	true as is_system,
                          	alphabetic_lookup_ratio || ' WBG corporate exchange rate, internally managed by System' as definition,
                          	alphabetic_lookup_ratio as data_unit,
                          	(timeofday())::timestamptz as modification_time,
                          	false as is_system_calculated,
                            true as default_subscription
                           from new_currencies
                           returning indicator_id,label_id,data_unit
                          )
                          update _temp_currency_codes tcc
                          set 
                            indicator_id = ni.indicator_id,
                            label_id = ni.label_id
                          from new_indicators ni
                          where ni.data_unit = tcc.alphabetic_lookup_ratio;
                          
                          insert into p_rsf.labels(label_id,label_key,primary_label,label_id_group)
                          select 
                            tcc.label_id,
                            'EN',
                            tcc.alphabetic_lookup_ratio || ' WBG Exchange Rate','indicators'
                          from _temp_currency_codes tcc
                          where tcc.create_indicator = true
                            and tcc.label_id is not null;
                          
                          insert into p_rsf.indicator_formulas(indicator_id,formula,overwrite,perform_calculation_by_row,formula_title)
                          select 
                            tcc.indicator_id,
                            'get_IFC_FX_rate(exchange_rate_date=global_reporting_quarter_end_date.current,
                                             currency_code_ratio=' || ind.indicator_name || '.current.unit)' as formula,
                            'allow' as overwrite,
                            TRUE as perform_calculation_by_row,
                           'Lookup IFC FX Rate for ' || tcc.alphabetic_lookup_ratio
                          from _temp_currency_codes tcc
                          inner join p_rsf.indicators ind on ind.indicator_id = tcc.indicator_id
                          where tcc.create_indicator = true;
                                            
                          RAISE NOTICE 'NEW GLOBAL FX INDICATOR CREATED';
                      end if;
                  
                  END $$;")
        
        dbGetQuery(conn,"select
                          indicator_id,
                          data_unit as alphabetic_ratio,
                          data_category
                        from p_rsf.indicators ind
                        where ind.data_type = 'currency_ratio' and ind.data_category = 'global'
                          and exists(select * from _temp_currency_codes tcc where tcc.alphabetic_lookup_ratio = ind.data_unit)")
      })
      setDT(new_indicators)
      
      fx_codes[new_indicators,
               `:=`(fx_indicator_id=i.indicator_id,
                    fx_pfcbl_category=i.data_category),
               on=.(alphabetic_lookup_ratio=alphabetic_ratio)]
    }
    
    
    fx_codes <- unique(fx_codes[,.(fx_pfcbl_category,
                                   fx_indicator_id,
                                   to_currency,
                                   from_currency)])
    
    fx_codes[,joincondition:=as.numeric(NA)]
    
    fx_lookup[fx_codes,
              `:=`(fx_indicator_id=i.fx_indicator_id,
                   fx_pfcbl_category=i.fx_pfcbl_category),
              on=.(to_currency,
                   from_currency,
                   fx_indicator_id=joincondition)]
    
    
  }  
  
  #Only for GLOBAL because all other indicators will for respective rsf_pfcbl_ids will calculate as part of data-uploads
  #Whereas GLOBAL will appear for non-calculated entries by default or in cases like dashbaord currency conversion requests
  calc_fx <- fx_lookup[fx_pfcbl_category=="global" &
                       (is.na(exchange_rate)==TRUE |
                        is_invalidated==TRUE |
                        is_unreported==TRUE)]
  
 
  #matches to global indicators that are (a) not every uploaded (b) not validated
  #calcuates the fx ratio from Dremio get_fx function
  #uploads data to rsf_data
  #validates data
  if (!empty(calc_fx)) {
    
    calc_fx[,
            `:=`(rsf_pfcbl_id=0,
                 currency_ratio=paste0(to_currency,"/",from_currency),
                 exchange_rate=as.numeric(NA))]
    
    calc_fx <- unique(calc_fx[,
                              .(rsf_pfcbl_id,
                                exchange_rate_date,
                                to_currency,
                                from_currency,
                                currency_ratio,
                                exchange_rate,
                                fx_indicator_id)])
    calc_fx[,
            exchange_rate:=mapply(get_fx_rate_catch,
                                  exchange_rate_date=exchange_rate_date,
                                  currency_code_ratio=currency_ratio)]
    
    
    #This should now be handled on a per-get_fx call and thrown if dremio can't connect
    #Left in for the unlikely case that Dremio returns an NA value
    if (anyNA(calc_fx$exchange_rate)) {
      errors <- calc_fx[is.na(exchange_rate)]
      stop(paste0("Dremio error.  Is the server down?  Failed to lookup fx rates for ",
                  paste0(paste0(errors$currency_ratio," as-of ",errors$exchange_rate_date),collapse=" AND ALSO ")))
    }
    
    #fx_indicator_id will be global (or not), so not enforcing join on fx_pfcbl_category=="global" as well
    fx_lookup[calc_fx,
              `:=`(exchange_rate=i.exchange_rate,
                   currency_ratio=i.currency_ratio),
              on=.(fx_indicator_id,
                   exchange_rate_date)]
    
    #Saving is hard-coded for GLOBAL since only these indicators will self-calculate here
    if (!empty(calc_fx[!is.na(fx_indicator_id)])) {
      calc_dates <- as.character(sort(unique(calc_fx$exchange_rate_date)))
      
      for (calc_date in calc_dates) {
        calc_date <- as.Date(calc_date)
        save_fx <- calc_fx[!is.na(fx_indicator_id) & exchange_rate_date==calc_date]

        save_fx[,alphabetic_ratio:=fx_currency_ratio_in_alphabetic_order(to_currency=to_currency,
                                                                         from_currency=from_currency)]
        save_fx[alphabetic_ratio != currency_ratio,
                `:=`(currency_ratio=alphabetic_ratio,
                     exchange_rate=1/exchange_rate)]
        
        setnames(save_fx,
                 old=c("exchange_rate_date",
                       "exchange_rate",
                       "fx_indicator_id",
                       "currency_ratio"),
                 new=c("reporting_asof_date",
                       "data_value",
                       "indicator_id",
                       "data_unit"))
        
        
        save_fx <- save_fx[,.(current_data_id=as.numeric(NA),
                              rsf_pfcbl_id,
                              indicator_id,
                              reporting_asof_date,
                              data_unit,
                              data_value,
                              flagged=FALSE)]
        
        save_fx <- unique(save_fx) #can have duplicated values due to requested_currency_ratio being inverse
                                   #ensure no duplicates are passed to db_add_update_data_user() else a stop() error will occur
        
        db_add_update_data_system(pool=pool,
                                  system_upload_data=save_fx)
      }
    }
  }
  
  #Missing data IDs will result from global indicators going through the calc_fx block and saving new entries and new data_ids that now must be looked-up
  missing_ids <- unique(fx_lookup[is.na(exchange_rate_data_id) |
                                  is_invalidated==TRUE |
                                  is_unreported==TRUE,
                                  .(lookup_rsf_pfcbl_id,
                                    exchange_rate_date,
                                    to_currency,
                                    from_currency)])
  
  #An fx data_id is not expected as there is effectively no fx performed.
  missing_ids <- missing_ids[!(to_currency==from_currency)] 
  if (!empty(missing_ids)) {
    
    fx_ratios <- db_lookup_fx_ratios(pool=pool,
                                     lookup=missing_ids,
                                     force.global=force.global)
    
    fx_lookup[fx_ratios,
              `:=`(currency_ratio=i.currency_ratio,
                   exchange_rate=i.exchange_rate,
                   exchange_rate_data_id=i.exchange_rate_data_id,
                   is_invalidated=i.is_invalidated,
                   is_unreported=i.is_unreported),
              on=.(lookup_rsf_pfcbl_id,
                   fx_indicator_id,
                   exchange_rate_date,
                   to_currency,
                   from_currency)]
  }
    
  if (anyNA(fx_lookup$exchange_rate_data_id) || any(fx_lookup$is_invalidated) || any(fx_lookup$is_unreported)) {
    bad_ids <- fx_lookup[is.na(exchange_rate_data_id) |
                         is_invalidated==TRUE |
                         is_unreported==TRUE]
    bad_ids <- bad_ids[!(to_currency==from_currency)]
    
    if (!empty(bad_ids)) {
      bad_ids[,message:=paste0("FX lookup failed for ",
                               currency_ratio," on ",
                               as.character(exchange_rate_date)," using ",
                               fx_pfcbl_category," indicator_id ",
                               fx_indicator_id, " BECAUSE ",
                               fcase(is.na(exchange_rate_data_id)==TRUE," exchange_rate_data_id is {MISSING}",
                                     is_invalidated==TRUE,paste0("data_id #",exchange_rate_data_id," calculation is invalidated: ",
                                                                 "This occurs when the currency ratio indicator ",fx_indicator_id,
                                                                 " is pending validation; but another calculation queries its value first. ",
                                                                 "This is usually a calculation sequencing problem and result of database value 'computation_priority_rank' not being set (it should be =1). ",
                                                                 "Try modifying the indicator's fomula in a trivial way, such as appending a blank space, to reset the priority rank."),
                                     is_unreported,paste0(fx_pfcbl_category," does not have a reporting entry for the requested date"),
                                     default="Unknown"))]
      #browser()
      stop(paste0("Missing exchange_rate_data_id:\n",paste0(bad_ids$message,collapse="\n AND ")))
    }
  }
  
  fx_lookup <- fx_lookup[,
                         .(rsf_pfcbl_id=unlist(rsf_pfcbl_ids,recursive=F)),
                         by=.(lookup_rsf_pfcbl_id,
                              exchange_rate_date,
                              to_currency,
                              from_currency,
                              currency_ratio,
                              exchange_rate,
                              fx_pfcbl_category,
                              fx_indicator_id,
                              exchange_rate_data_id)]
  
  fx_lookup[,lookup_rsf_pfcbl_id:=NULL]
  setcolorder(fx_lookup,
              neworder=c("rsf_pfcbl_id",
                         "from_currency",
                         "to_currency",
                         "currency_ratio",
                         "exchange_rate_date",
                         "exchange_rate",
                         "fx_pfcbl_category",
                         "fx_indicator_id",
                         "exchange_rate_data_id"))
  return(fx_lookup)
}
