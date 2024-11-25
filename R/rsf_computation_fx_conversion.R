
#Currency conversions!
rsf_computation_fx_conversion <- function(pool,
                                          computation,
                                          comp_data,
                                          computation_asof_date,
                                          fx_table,
                                          update_fx_table_function=NULL,
                                          add_data_flag_function=NULL,
                                          add_fx_conversions_function=NULL)
{
  
  if (!is.data.table(computation)) stop("Computation must be a data.table")
  if (!is.data.table(comp_data)) stop("Computation must be a data.table")
  if (nrow(computation) != 1) stop("Computation must be a single row")
  
  compute_currency_unit <- computation$calculate_indicator_currency_unit
  default_currency_unit <- computation$entity_local_currency_unit
  
  if (!is.null(compute_currency_unit) && 
      !is.na(compute_currency_unit)) {
    
    #browser()
    if (is.null(default_currency_unit) || is.na(default_currency_unit)) {
      stop("Defualt currency unit is undefined")
    }
    
    
    if (!compute_currency_unit %in% CALCULATIONS_ENVIRONMENT$VALID_CURRENCIES) compute_currency_unit <- default_currency_unit
    if (!compute_currency_unit %in% CALCULATIONS_ENVIRONMENT$VALID_CURRENCIES) {
      print(computation)
      stop(paste0("Request to calculate ",computation$indicator_name,
                  " fx for invalid currency code: ",
                  computation$calculate_indicator_currency_unit,
                  " and with default ",computation$entity_local_currency_unit))
    }
    
    parameters <- rbindlist(computation$parameters_dt)
    
    #.all parameteters should already have been converted individually and re-grouped in a list. 
    #In any case, ignore the list itself.
    parameters <- parameters[!(parameter_variable=="all")]
    
    if (!any(parameters$for_fx,na.rm=T)) {
      #nothing to convert.  If this is  a currency calculation, it must be hard-coded and not parameter based.  Onus of correct conversion is on calculation!
      return (comp_data)
    }
    
    fx_indicator_unit_colnames <- parameters[for_fx==TRUE & 
                                             grepl("unit$",parameter_variable),
                                             parameter_column_name]
    
    if (length(fx_indicator_unit_colnames) == 0) {
      print(computation)
      stop(paste0("Request to compute currency unit ",compute_currency_unit," but received no indicator .unit values"))
    }
    data_units <- unique(unlist(comp_data[,..fx_indicator_unit_colnames]))
    
    #This can only happen if a default indicator is queried that never had any values submitted, so gives default data unit
    #We can assume default data unit is equal to calculation's local currency unit.
    #It can also occur if .previous is requested where no "previous" reporting exists.  Here we can also assume that the entity's
    #current LCU was the same in previous quarter.
    if (anyNA(data_units) || any(data_units=="LCU")) {
      for(lcu_col in fx_indicator_unit_colnames) {
        lcu <- which((is.na(comp_data[[lcu_col]]) | comp_data[[lcu_col]]=="LCU"))
        if (length(lcu) > 0) {
          set(comp_data,
              i=lcu,
              j=lcu_col,
              value=default_currency_unit)
        }
      }
      data_units <- unique(unlist(comp_data[,..fx_indicator_unit_colnames]))
    }
    
    fx_unit_grouping_cols <- c("grouping",fx_indicator_unit_colnames)
    fx_unit_id_vars <- "grouping"
    if (any(names(comp_data)=="subgrouping")) {
      fx_unit_id_vars <- c(fx_unit_id_vars,"subgrouping")
    }
    require_fx <- comp_data[,..fx_unit_grouping_cols]
    require_fx <- melt.data.table(require_fx,
                                  id.vars=fx_unit_id_vars)
    
    #calculate_data_unit <- computation$calculate_indicator_data_unit
    #if (!identical(calculate_data_unit,compute_currency_unit)) stop(paste0("Legacy issue calculated_data_unit=",calculate_data_unit," and compute_currency_unit=",compute_currency_unit))
    
    calculatable_data_type <- computation$data_type %in% c("currency","currency_ratio") #ie, NOT a percentage
    
    #if (length(calculate_data_unit)==0) calculate_data_unit <- NULL
    if (length(calculatable_data_type)==0) calculatable_data_type <- FALSE
    
    #At the calculation grouping level: do we need an fx conversion?
    require_fx <- require_fx[,
                             .(required=fcase(#Inputs have more than one currency, regardless of output
                                 length(unique(value)) > 1,
                                 TRUE,
                                 
                                 #Calculated output is currency or ratio and all input currencies are not the same as output
                                 #computation$calculate_indicator_data_unit is really specified to calculations and not checks
                                 #since calculations will compute a value, whereas checks will compute a TRUE/FALSE flag
                                 calculatable_data_type &
                                 !all(compute_currency_unit==value),
                                 #old see stop legacy issue above !all(calculate_data_unit == value),
                                 TRUE,
                                 
                                 default=FALSE)
                             ),
                             by = .(grouping)][required==TRUE]
    
    #Yes, for these calculation groups we need an fx
    if (!empty(require_fx)) {
      
      #browser()
      #Calculations are only performed on a per unique UNIT value
      to_currency <- compute_currency_unit

      #Subgrouping comes in via checks not indicators.
      if (!any(names(parameters)=="for_subgrouping")) parameters[,for_subgrouping:=FALSE]
      
      #FX cols will include BOTH currency columns that may need to be fx rates
      
      fx_cols <- parameters[parameter_indicator_name %in% parameters[for_fx==TRUE,unique(parameter_indicator_name)] &
                              (for_calculation==TRUE | for_fx==TRUE | for_subgrouping==TRUE),
                            unique(parameter_column_name)]
      
      if (length(fx_cols) == 0) {
        stop(paste0("System calculator determined fx required to currency ",to_currency,".  But no fx_cols identified in dataset"))
      }
      
      fx_date_method <- computation$formula_fx_date
      #fx_date_method <- "parameter"
      #fx_date_method <- "calculation"
      if (is.null(fx_date_method) || all(is.na(fx_date_method)) || !any(fx_date_method %in% c("calculation","parameter","fx"))) {
        stop("Formula FX date method is not defined.  Expected either by 'calculation' or by 'parameter' or by 'fx' change")
      }
      
      fx_cols <- unique(c(fx_unit_id_vars,
                   "rsf_pfcbl_id", #remember that individual entities can specify the fx rates they want to use at the entity/indicator level
                   #presently, this is widely used in SLGP for specific facilities that specify it; and also for EC EUR sources rates
                   fx_cols))
      
      fx_data <- comp_data[,..fx_cols]
      
      
      fx_computation_date <- NULL
      if (fx_date_method=="calculation") {
        fx_date_cols <- grep("reporteddate$",names(fx_data),value=T)
        fx_date_cols <- c("grouping","rsf_pfcbl_id",fx_date_cols)
        
        fx_computation_date <- melt.data.table(fx_data[,..fx_date_cols],
                                            id.vars = c(fx_unit_id_vars,"rsf_pfcbl_id"),
                                            variable.name="parameter_column_name",
                                            value.name="fx_date",
                                            variable.factor=FALSE)
      
        fx_computation_date <- fx_computation_date[,
                                                 .(fx_date=as.Date(ifelse(all(is.na(fx_date)),
                                                                          as.character(NA),
                                                                          max(as.character(fx_date),na.rm=T)))),
                                                 by=c(fx_unit_id_vars,"rsf_pfcbl_id")]
      
      #This shouldn't happen...because why would it have been triggered otherwise?
      #Wrong, this can reasonably happen: when a new entity is created and its indicators are prompted to be calculated for the very first time
      #and/or if the entity never ever reported on one of the required parameters (and rsf_program_get_data therefore returned a default value
      #but will not return a default reporting date that never occurred).  Presumably, the result of a calculation that uses only defaults
      #will not be correct.
      #Decided to use the current calculation date as the default fx date and let the calculation proceed instead of forcing an NA result

      #This means that for the calculate_rsf_pfcbl_id entity, we are getting the maximum parameter date
      #So for aggregates, it will be the latest parameter value
      #fx_parameter_dates <- NULL
      
      
      } else if (fx_date_method=="fx") {
        fx_method_cols <- unique(c(fx_unit_id_vars,"rsf_pfcbl_id"))
        fx_computation_date <- unique(fx_data[,..fx_method_cols])
        fx_computation_date[,
                            fx_date:=computation_asof_date]
      }
      
      
      fx_currency_cols <- names(fx_data)[names(fx_data) %in% parameters[parameter_data_type=="currency",parameter_column_name]]
      
      fx_currency_cols <- c(fx_unit_id_vars,"rsf_pfcbl_id",fx_currency_cols)
      fx_currency_data <- fx_data[,..fx_currency_cols]
      fx_data_cols <- names(fx_currency_data)[-which(names(fx_currency_data) %in% c(fx_unit_id_vars,"rsf_pfcbl_id"))]
      for (col in fx_data_cols) set(fx_currency_data,i=NULL,j=col,value=as.character(fx_currency_data[[col]]))
      fx_data_id_vars <- names(fx_currency_data)[-which(names(fx_currency_data) %in% fx_data_cols)]

      #we need the row order to be maintained to ensure the UNIT/VALUE/DATE relationships remain in the order relative to their underlying
      #rsf_X_id, which has been lost in the transformations because the rsf_pfcbl_id is for the calculation and not the entity column data
      fx_currency_data[,entity_id:=1:.N]
      
      fx_currency_data <- melt.data.table(fx_currency_data,
                                          id.vars=c(fx_data_id_vars,"entity_id"),
                                          variable.name="parameter_column_name",
                                          value.name = "data_value",
                                          value.factor = F,
                                          variable.factor=F)
     
      
      fx_currency_data[,
                       is_unit:=grepl("\\.unit$",parameter_column_name)]
      
      fx_currency_data[,
                       is_date:=grepl("\\.reporteddate$",parameter_column_name)]
      
      fx_currency_units <- fx_currency_data[is_unit==TRUE]
      fx_currency_dates <- fx_currency_data[is_date==TRUE]
      
      fx_currency_data <- fx_currency_data[is_unit==FALSE & is_date==FALSE]
      
      fx_currency_units[,parameter_column_unit:=parameter_column_name]
      fx_currency_units[,parameter_column_name:=gsub("\\.unit$","",parameter_column_name)]
      
      fx_currency_dates[,parameter_column_date:=parameter_column_name]
      fx_currency_dates[,parameter_column_name:=gsub("\\.reporteddate$","",parameter_column_name)]
      fx_currency_dates[,data_value:=as.Date(data_value)]
      fx_currency_dates[is.na(data_value),
                        data_value:=as.Date(computation_asof_date)]
      
      if (nrow(fx_currency_data) != nrow(fx_currency_units)) {
        stop("Mismatch: fx_currency_data has ",nrow(fx_currency_data)," rows but fx_currency_units has ",nrow(fx_currency_units))
      }
      
      if (nrow(fx_currency_data) != nrow(fx_currency_dates)) {
        stop("Mismatch: fx_currency_data has ",nrow(fx_currency_data)," rows but fx_currency_dates has ",nrow(fx_currency_dates))
      }
      
      fx_currency_data[fx_currency_units,
                       `:=`(fx_from=i.data_value,
                            parameter_column_unit=i.parameter_column_unit),
                       on=c(fx_data_id_vars,"entity_id","parameter_column_name")]

      fx_currency_data[fx_currency_dates,
                       `:=`(fx_date=i.data_value,
                            parameter_column_date=i.parameter_column_date),
                       on=c(fx_data_id_vars,"entity_id","parameter_column_name")]
      
      fx_currency_units <- NULL
      fx_currency_dates <- NULL
      fx_currency_data[,is_unit:=NULL]
      fx_currency_data[,is_date:=NULL]
      
      fx_currency_data[,data_value:=as.numeric(data_value)]
      fx_currency_data[,fx_to:=to_currency]
      
      fx_currency_data <- fx_currency_data[fx_from != fx_to]
      fx_currency_data <- fx_currency_data[is.na(data_value)==FALSE]
      
      
      if (fx_date_method=="calculation" || fx_date_method=="fx") {
        
        fx_currency_data[fx_computation_date,
                         fx_date:=i.fx_date,
                         on=.(grouping,
                              rsf_pfcbl_id)]
        
      }
      
      if (anyNA(fx_currency_data$fx_date)) {
        blank_fx_dates <- fx_currency_data[is.na(fx_date)]
        
        blank_fx_cols <- unique(indicatorNames_getBasenames(fx_cols))
        blank_fx_cols <- paste0(blank_fx_cols[!blank_fx_cols %in% c(fx_unit_id_vars,"rsf_pfcbl_id")],collapse=" & ")
        if (!is.null(add_data_flag_function)) {
          #This is important because a calculated indicator could be reported and never legeitimately triggered because
          #no parameters have ever reported...Ie, it should ever have been calculated in the first place and presumably
          #be NA or default.  But the self reported data point would trigger itself in the absence of any parameter dates
          #to guide the FX.  Therefore, we use the calculations own date.  But this may not be reasonable assumption in some
          #instances.
          add_data_flag_function(rsf_pfcbl_id=unlist(blank_fx_dates$rsf_pfcbl_id),
                                 #indicator_id=calculation$calculate_indicator_id,
                                 check_name="sys_calculator_failed",
                                 check_message=paste0("FX date {UNKNOWN} entity has reported {NOTHING} for required inputs: ",
                                                      blank_fx_cols,". Therefore, a default exchange rate date of ",
                                                      as.character(computation_asof_date)," will be used; however, calculated results (if any) may be unexpected."))
        }
        fx_currency_data[is.na(fx_date),
                         fx_date:=computation_asof_date]
      }
      
      #fx_currency_data <- fx_currency_data[(data_value==0)==FALSE] #Because zero times any fx rate will be zero, so no need to lookup the rate.
      #NA ids found to come into fx request due to entities not existing (yet) within the timeline being requested and therefore having been filtered out and set to NA by
      #the calling function.
      fx_requirements <- unique(fx_currency_data[is.na(rsf_pfcbl_id)==FALSE,
                                                 .(rsf_pfcbl_id,
                                                   fx_date,
                                                   fx_from,
                                                   fx_to)])
      
            
      if (!empty(fx_requirements)) {
        fx_requirements[,
                        `:=`(fx_rate=as.numeric(NA),
                             exchange_rate_data_id=as.numeric(NA))]  
        
        fx_requirements[fx_table,
                        `:=`(fx_rate=i.fx_rate,
                             fx_pfcbl_category=i.fx_pfcbl_category,
                             exchange_rate_data_id=i.exchange_rate_data_id),
                        on=.(rsf_pfcbl_id,
                             fx_from=from_currency,
                             fx_to=to_currency,
                             fx_date)]
        
        #db lookup: differentiate between manually defined FX rates, global rates, and whether to draw from Jason database or lookup in Dremio
        if (anyNA(fx_requirements$fx_rate) || anyNA(fx_requirements$exchange_rate_data_id)) {
          fx_lookup <- fx_requirements[is.na(fx_rate),
                                       .(rsf_pfcbl_id,
                                         exchange_rate_date=fx_date,
                                         from_currency=fx_from,
                                         to_currency=fx_to,
                                         exchange_rate_data_id)]
          
          fx_rates <- db_data_get_fx_ratio(pool=pool,
                                           fx_lookup=fx_lookup,
                                           create.indicators=TRUE,
                                           force.global = FALSE)
          
#browser()
          if (!empty(fx_rates[is.na(exchange_rate) & fx_pfcbl_category=="global"])) {
            
            print("fx_requirements[is.na(fx_rate)] :")
            print(fx_requirements[is.na(fx_rate) & fx_pfcbl_category=="global"])
            print("fx_lookup :")
            print(fx_rates)
            stop(paste0("Failed to lookup fx rates"))
          }
          
          if (!empty(fx_rates[is.na(exchange_rate) & fx_pfcbl_category !="global"])) {
            
            fx_not_defined <- fx_rates[is.na(exchange_rate) & fx_pfcbl_category !="global"]
            fx_not_defined[,fx_indicator_id:=as.numeric(fx_indicator_id)]

            fx_indicators <- dbGetQuery(pool,"
             select distinct on (fam.child_rsf_pfcbl_id,indf.formula_id)
              fam.child_rsf_pfcbl_id,
              pis.indicator_id,
              pis.indicator_name,
              indf.formula_id,
              coalesce(indf.overwrite,'allow') as overwrite 
              from p_rsf.view_rsf_pfcbl_indicator_subscriptions pis
              inner join p_rsf.rsf_pfcbl_id_family fam on fam.parent_rsf_pfcbl_id = pis.rsf_pfcbl_id
              inner join p_rsf.indicator_formulas indf on indf.formula_id = pis.formula_id
              where pis.indicator_id = any(select unnest(string_to_array($1::text,','))::int)
                and pis.is_subscribed = true
                and fam.child_rsf_pfcbl_id = any(select unnest(string_to_array($2::text,','))::int)
              order by 	fam.child_rsf_pfcbl_id,indf.formula_id,pis.rsf_facility_id is not NULL desc",
              params=list(paste0(unique(fx_not_defined$fx_indicator_id),collapse=","),
                          paste0(unique(fx_not_defined$rsf_pfcbl_id),collapse=",")))
            #                                 select 
            #                                   ind.indicator_id,
            #                                   ind.indicator_name,
            #                                   coalesce(indf.overwrite,'allow') as overwrite 
            #                                 from p_rsf.indicators ind
            #                                 -- All currency ratios should be calculated, so this should be inner join...just in case
            #                                 --left join p_rsf.indicator_formulas indf on indf.indicator_id = ind.indicator_id
            #                                 where ind.indicator_id = any(select unnest(string_to_array($1::text,','))::int)",
            #                             params=list(paste0(unique(fx_not_defined$fx_indicator_id),collapse=",")))
            setDT(fx_indicators)
            fx_not_defined[,overwrite:="allow"]
            fx_not_defined[fx_indicators,
                           `:=`(indicator_name=i.indicator_name,
                                overwrite=i.overwrite),
                           on=.(fx_indicator_id=indicator_id)]
            
            
            fx_indicators_overwrite <- fx_not_defined[overwrite != "deny"]
            fx_indicators_fail <- fx_not_defined[overwrite == "deny"]
            
            #Currently, mannually reported currency ratios are set to "missing" and therefore this block will not occur in present code base (Feb 2024)
            
            if (!empty(fx_indicators_overwrite)) {
              
              fx_overwrite_rates <- db_data_get_fx_ratio(pool=pool,
                                                         fx_lookup=fx_indicators_overwrite[,
                                                                                          .(rsf_pfcbl_id,
                                                                                            exchange_rate_date,
                                                                                            from_currency,
                                                                                            to_currency)],
                                                          create.indicators=TRUE,
                                                          force.global = TRUE)
              
              fx_indicators_fail <- rbindlist(list(fx_indicators_fail,
                                                   fx_indicators_overwrite[fx_overwrite_rates[is.na(exchange_rate),
                                                                                               .(rsf_pfcbl_id,
                                                                                                  from_currency,
                                                                                                  to_currency,
                                                                                                  exchange_rate_date)],
                                                                           on=.(rsf_pfcbl_id,
                                                                                from_currency,
                                                                                to_currency,
                                                                                exchange_rate_date),
                                                                           nomatch=NULL]))
              
              fx_overwrite_rates <- fx_overwrite_rates[is.na(exchange_rate)==FALSE]
              fx_rates <- fx_rates[is.na(exchange_rate)==FALSE]
              setcolorder(fx_overwrite_rates,
                          neworder=names(fx_rates))
              fx_rates <- rbindlist(list(fx_rates,
                                         fx_overwrite_rates))
              
              if (!is.null(add_data_flag_function) && !empty(fx_overwrite_rates)) {
                fx_overwrite_rates <- fx_overwrite_rates[fx_not_defined[,.(rsf_pfcbl_id,
                                                                             from_currency,
                                                                             to_currency,
                                                                             exchange_rate_date,
                                                                             overwrite_fx_pfcbl_category=fx_pfcbl_category,
                                                                             indicator_name)],
                                    on=.(rsf_pfcbl_id,
                                         from_currency,
                                         to_currency,
                                         exchange_rate_date)]
                
                add_data_flag_function(rsf_pfcbl_id=unlist(fx_not_defined$rsf_pfcbl_ids),
                                       check_name="sys_calculator_overwrote_manual_calculation",
                                       check_message=paste0(fx_overwrite_rates$overwrite_fx_pfcbl_category," requires manually reported ",
                                                            fx_overwrite_rates$currency_ratio," FX but ",
                                                            "'",fx_overwrite_rates$indicator_name,"' is {MISSING}. Defaulting to IFC official rate of ",
                                                            fx_overwrite_rates$exchange_rate,
                                                            ". RSF Program should not monitor this indicator if manual FX rates are not expected"))
                                                            
                                                            
              }
              
            }
            
            if (!empty(fx_indicators_fail)) {
              
              if (!is.null(add_data_flag_function)) {
                add_data_flag_function(rsf_pfcbl_id=unlist(fx_indicators_fail$rsf_pfcbl_ids),
                                       #indicator_id=calculation$calculate_indicator_id,
                                       check_name="sys_calculator_failed",
                                       check_message=paste0(fx_indicators_fail$currency_ratio," FX conversion FAILED. FX rate for this calculation is reported by the ",
                                                            toTitleCase(fx_indicators_fail$fx_pfcbl_category)," in '",
                                                            fx_indicators_fail$indicator_name,"' but {NOTHING} is reported for ",
                                                            as.character(fx_indicators_fail$exchange_rate_date),
                                                            ". If this RSF Program expects to use System-calculated FX rates for ",fx_indicators_fail$currency_ratio,
                                                            ", then RSF Program must not monitor '",fx_indicators_fail$indicator_name,"'"))
              }
            }
          }
          
          fx_requirements[fx_rates,
                          `:=`(fx_rate=as.numeric(i.exchange_rate),
                               fx_pfcbl_category=i.fx_pfcbl_category,
                               exchange_rate_data_id=as.numeric(i.exchange_rate_data_id)),
                          on=.(rsf_pfcbl_id,
                               fx_date=exchange_rate_date,
                               fx_from=from_currency,
                               fx_to=to_currency)]
          
        }       
        
        
        fx_currency_data[fx_requirements,
                          `:=`(fx_rate=i.fx_rate,
                               fx_pfcbl_category=i.fx_pfcbl_category,
                               exchange_rate_data_id=i.exchange_rate_data_id),
                          on=.(rsf_pfcbl_id,
                               fx_date,
                               fx_from,
                               fx_to)]

        if (!is.null(update_fx_table_function)) {
          update_fx_table_function(cache_fx=unique(fx_currency_data[,.(rsf_pfcbl_id,
                                                                     from_currency=fx_from,
                                                                     to_currency=fx_to,
                                                                     fx_date,
                                                                     fx_rate,
                                                                     fx_pfcbl_category,
                                                                     exchange_rate_data_id)]))
        }

        if (!is.null(add_fx_conversions_function)) {
          add_fx_conversions_function(report_conversions=unique(fx_currency_data[,
                                                                                  .(rsf_pfcbl_id,
                                                                                    exchange_rate_data_id)]))

        }
        
        fx_grouping_vars <- fx_data_id_vars[-which(fx_data_id_vars=="rsf_pfcbl_id")]
        fx_grouping_vars <- c(fx_grouping_vars,"fx_from","fx_to","fx_rate","exchange_rate_data_id","parameter_column_name","parameter_column_unit",
                              "fx_date","fx_pfcbl_category")
        
        if (fx_date_method=="parameter") {
          fx_grouping_vars <- c(fx_grouping_vars,"parameter_column_date")
        }
        
        fx_grouping_vars <- unique(fx_grouping_vars)

        fx_currency_data <- fx_currency_data[,
                                       .(rsf_pfcbl_ids=list(unique(rsf_pfcbl_id))),
                                       by=c(fx_grouping_vars)]

        #fxg <- 1
        #fxg <- 2
        for (fxg in 1:nrow(fx_currency_data)) {
          fx <- fx_currency_data[fxg]
          
          whichRows <- integer(0)
          if (fx_date_method=="parameter") {
            if (any(names(comp_data)=="subgrouping")) {
              whichRows <- which(comp_data[[fx$parameter_column_unit]]==fx$fx_from &
                                 comp_data[[fx$parameter_column_date]]==fx$fx_date &
                                 comp_data$grouping==fx$grouping &
                                 comp_data$subgrouping==fx$subgrouping &
                                 comp_data$rsf_pfcbl_id %in% unlist(fx$rsf_pfcbl_ids))
              
            } else {
              whichRows <- which(comp_data[[fx$parameter_column_unit]]==fx$fx_from &
                                 comp_data[[fx$parameter_column_date]]==fx$fx_date &
                                 comp_data$grouping==fx$grouping &
                                 comp_data$rsf_pfcbl_id %in% unlist(fx$rsf_pfcbl_ids))
            }
            
          
          } else {
            
            if (any(names(comp_data)=="subgrouping")) {
              whichRows <- which(comp_data[[fx$parameter_column_unit]]==fx$fx_from &
                                   comp_data$grouping==fx$grouping &
                                   comp_data$subgrouping==fx$subgrouping &
                                   comp_data$rsf_pfcbl_id %in% unlist(fx$rsf_pfcbl_ids))
              
            } else {
              whichRows <- which(comp_data[[fx$parameter_column_unit]]==fx$fx_from &
                                   comp_data$grouping==fx$grouping &
                                   #subgrouping
                                   comp_data$rsf_pfcbl_id %in% unlist(fx$rsf_pfcbl_ids))
            }
          }
          
          set(comp_data,
              i=whichRows,
              j=fx$parameter_column_name,
              value=comp_data[[fx$parameter_column_name]][whichRows] * fx$fx_rate) #FX rate FROM/TO means MULTIPLY FROM to equal TO
          
          set(comp_data,
              i=whichRows,
              j=fx$parameter_column_unit,
              value=fx$fx_to)
          
          
          if (!is.null(add_data_flag_function)) {
            
            add_data_flag_function(rsf_pfcbl_id=unlist(fx$rsf_pfcbl_ids),
                                   check_name="sys_fx_conversion",
                                   check_message=paste0(fx$parameter_column_name,": ",
                                                        fx$fx_from," -> ",fx$fx_to,
                                                        " @",fx$fx_rate," ",as.character(fx$fx_date),
                                                        " (",fx_date_method," date, ",fx$fx_pfcbl_category," rate)"))
            # add_data_flag_function(rsf_pfcbl_id=unlist(fx$rsf_pfcbl_ids),
            #                        check_name="sys_fx_conversion",
            #                        check_message=paste0(fx$parameter_column_name,": ",
            #                                             fx$fx_from," -> ",fx$fx_to,
            #                                             " @",fx$fx_rate," ",as.character(fx$fx_date),
            #                                             " ",as.character(fx$fx_date),
            #                                             " (",fx_date_method," date)"))
          }
        }          
      }
    }
  }
  
  return (comp_data)
}
