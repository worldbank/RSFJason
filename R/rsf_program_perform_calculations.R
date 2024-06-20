
rsf_program_perform_calculations <- function(pool,
                                             current_data,
                                             rsf_indicators,
                                             perform.test = FALSE,
                                             status_message=function(...) {}) {
  

  t1 <- Sys.time()
  #setups
  {
    #datac <<- as.data.frame(current_data)
    #ri <<- as.data.frame(rsf_indicators)
    
    #current_data <- as.data.table(datac)
    #rsf_indicators <- as.data.table(ri)
    
    #if (is.null(rsf_indicators) || empty(rsf_indicators)) stop("Null or empty() rsf_indicators passed to rsf_data_perform_calculations")
    if (empty(current_data)) {
      status_message(class="info","No calculations require update.\n")
      return (NULL)
    }
    # CHECKS <- c("sys_calculator_overwrote_manual_calculation",
    #             "sys_calculator_vs_manual_calculation",
    #             "sys_calculator_failed")
    # 
    global <- ifelse(all(current_data$calculate_rsf_pfcbl_id==0),
                     "GLOBAL ",
                     "")
    
    status_message(class="none",paste0("Performing ",global,"calculations for ",current_data$calculate_asof_date[[1]],
                                       " and ",length(unique(current_data$computation_group))," groups: ",
                                       paste0(sort(unique(current_data$computation_group)),collapse=", "),
                                       "\n")) 
    
    if(SYS_PRINT_TIMING) debugtime("rsf_program_perform_calculations",paste0("Performing ",global,"calculations for ",current_data$calculate_asof_date[[1]],
                                                                             " and ",
                                                                             length(unique(current_data$computation_group))," groups:",
                                                                             paste0(sort(unique(current_data$computation_group)),collapse=", ")))
    
    calculation_flags <- data.table(rsf_pfcbl_id=numeric(0),
                                    indicator_id=numeric(0),
                                    reporting_asof_date=as.Date(numeric(0)),
                                    check_name=character(0),
                                    check_message=character(0))
  
  }  
    
  {
    #setup indicators to calculate based on the current rank AND ALSO any stale calculations that MAY HAVE been added as a result of the previous calculations cycle.
    { 


      current_data_indicators <- current_data[,
                                              .(calculate_rsf_pfcbl_ids=list(calculate_rsf_pfcbl_id),
                                                calculation_group=.GRP
                                                ),
                                              by=.(calculate_indicator_id,
                                                   calculate_asof_date,
                                                   entity_local_currency_unit,
                                                   calculate_indicator_data_unit,
                                                   formula_id,
                                                   computation_group)] #Computation group aims to efficiently query similar pfcbl category data needs
      
      formulas <- dbGetQuery(pool,"
                             select
                              indf.formula_id,
                              indf.formula,
                              indf.formula_sort,
                              indf.perform_calculation_by_row,
                              indf.formula_grouping_pfcbl_rank,
                              'rsf_' || grouping.pfcbl_category || '_id' as formula_grouping_rsf_id,
                              array_to_string(pids.pfcbl_id_categories,',') as formula_pfcbl_id_categories,
                              indf.overwrite as formula_overwrite,
                              indf.formula_fx_date,
                              ind.data_category,
                              ind.data_type,
                              ind.indicator_name,
                              ind.is_system,        -- all the global calculations are designated system indicators
                              true as is_calculated -- because we may see a facility add a custom calculation for a non-calculated indicator
                                                    -- this will make it overwrite any defaults coming from rsf_indicators via the formula
                             from p_rsf.indicator_formulas indf 
                             inner join p_rsf.indicators ind on ind.indicator_id = indf.indicator_id
                             left join p_rsf.rsf_pfcbl_categories grouping on grouping.pfcbl_rank = indf.formula_grouping_pfcbl_rank
                             left join lateral (select array_agg(distinct 'rsf_pfcbl_id.' || ifp.parameter_pfcbl_category) as pfcbl_id_categories
                              									from p_rsf.indicator_formula_parameters ifp 
                              									where ifp.formula_id = indf.formula_id) as pids on true
                             
                             where indf.formula_id = any(select unnest(string_to_array($1::text,','))::int)",
                             params=list(paste0(unique(current_data_indicators$formula_id),collapse=",")))
      
      formulas[["formula_pfcbl_id_categories"]] <- lapply(formulas[["formula_pfcbl_id_categories"]],
                                                          function(x) as.character(strsplit(x,split=',',fixed=T)[[1]]))
      setDT(formulas)
      
      calculations <- current_data_indicators[formulas,
                                              on=.(formula_id)]
      
      #!!NOTE!!
      #calculate_indicator_currency_unit may be non-NA and a valid currency code when the calculation uses currency parameters
      #BUT
      #calculate_indicator_data_unit may be NA when, for example, the calculation is a percentage without any associated data unit
      #!!!!!!!!
      
      calculations[,formula:= gsub("[[:cntrl:]]+"," ",formula)]
      calculations[,formula_sort:= gsub("[[:cntrl:]]+"," ",formula_sort)]
      
      #creates the parameters_dt column
      calculations <- rsf_calculations_resolve_parameters(calculations=calculations,
                                                          rsf_indicators=rsf_indicators)
      
      calculations[,calculate_indicator_currency_unit:=as.character(NA)]
      
      #If my output is currency, then I must calculate a currency unit
      calculations[data_type == "currency",
                   calculate_indicator_currency_unit:=toupper(calculate_indicator_data_unit)]
      
      
      #If my output isn't currency, but I have any input variables that are currency,
      #then we may have an fx requirement if different input parameters have different units.  (eg, for a percentage calculation)
      #in which case, all input variables need to have a common currency for a ratio to be meaningful.
      calculations[is.na(calculate_indicator_currency_unit) == TRUE &
                   sapply(calculations$parameters_dt,function(x) { any(x$parameter_data_type=="currency" & 
                                                                       grepl("\\.unit$",x$parameter_variable))
                                                                 }) == TRUE,
                   calculate_indicator_currency_unit:=entity_local_currency_unit]
      
    }
    
    #Run calculations by group 
    {
      
      
      
      # _computation_ group is relevant for getting the data most efficiently.
      # _calculation_ group is necessary to segment different currency units and formula calculations
      
      #Originally, I had separate sub-loops to get the data by computation group most efficiently and then run each individual calculation group
      #however, this also requires subsetting the rsf_data_wide to pass only the relevant rsf_pfcbl_ids; but this is complex and largely what happens in 
      #rsf_indicators_calculate and is much more complex than it seems for edge cases that require grouping at parent-level entities to calculate aggregate
      #values that need data for entities that aren't included in the calculate_rsf_pfcbl_id value.  Given this complexity and that the instances where
      #an individual computation group has multiple calculation groups is *rare* that this isn't a significant efficiency gain and instead we should just
      #re-query data and parameter IDs for each calculation group directly.
      
      calculations_groups_results <- NULL
      
      
      computation_groups <- sort(unique(calculations$computation_group))
      current_data[,calculated:=FALSE]
      #compg <- computation_groups[[1]]
      for (compg in computation_groups) {
        
        calculations_group <- calculations[computation_group==compg]
        
        #get the data
        {
         
          request_indicator_variables <- rbindlist(calculations_group$parameters_dt)
          
          #Because if formula doesn't define any parameters (in the case of formula calculation definitions, possibly)
          if (empty(request_indicator_variables)) request_indicator_variables <- data.table(parameter_indicator_id=numeric(0),
                                                                                            parameter_variable=character(0))
          
          request_indicator_variables <- request_indicator_variables[,.(parameter_indicator_id,parameter_variable)]
          request_indicator_variables <- unique(request_indicator_variables)
          
          request_indicator_variables <-request_indicator_variables[,
                                                                    .(parameter_variables=paste0(parameter_variable,collapse=",")),
                                                                    by=.(parameter_indicator_id)]
          
          request_indicator_variables <- setNames(request_indicator_variables$parameter_indicator_id,
                                                  request_indicator_variables$parameter_variables)
          
          #can aggregate at this level and/or apply the formula result back-onto the 
          #entity that is requesting it.  For this reason, simplify.indicators = FALSE
           
          calculate_rsf_pfcbl_ids <- unique(unlist(calculations_group$calculate_rsf_pfcbl_ids))
          
          #TODO: need hierarchy to be 'sibling'
          parameter_rsf_pfcbl_ids <- dbGetQuery(pool,"
                                                  select 
                                                    distinct cids.to_parameter_rsf_pfcbl_id as rsf_pfcbl_id
                                                  from p_rsf.compute_calculation_to_parameter_rsf_pfcbl_ids cids
                                                  where cids.from_calculate_rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
                                                    and cids.from_calculate_indicator_id = any(select unnest(string_to_array($2::text,','))::int)
                                                    and cids.parameter_rsf_pfcbl_id_created_date <= $3::date
                                                    and cids.to_parameter_rsf_pfcbl_id <> cids.from_calculate_rsf_pfcbl_id
                                                ",
                                                params=list(paste0(calculate_rsf_pfcbl_ids,collapse=","),
                                                            paste0(unique(calculations_group$calculate_indicator_id),collapse=","),
                                                            as.character(calculations_group$calculate_asof_date[[1]])))
    
          
           data_rsf_pfcbl_ids <- unique(c(calculate_rsf_pfcbl_ids,parameter_rsf_pfcbl_ids$rsf_pfcbl_id))
           rsf_data_wide <- db_program_get_data(pool=pool,
                                               reporting_current_date=calculations_group$calculate_asof_date[[1]],
                                               indicator_variables=request_indicator_variables, #a named vector of indicator_id values; names, a csv concatenated string of variable attribute requirements
                                               for_rsf_pfcbl_ids=data_rsf_pfcbl_ids,
                                               indicator_ids.simplify=FALSE) #When TRUE, will return data columns specified in indicator_ids (and omit sys_reporting pseudo indicators); #Note that rsf_checks_calculate expects these for by() groupings
       
        }      
          
        #run the calculations
        {
          calculations_results <- rsf_indicators_calculate(pool=pool,
                                                           rsf_indicators=rsf_indicators,
                                                           rsf_data_wide=rsf_data_wide,
                                                           calculations=calculations_group,
                                                           status_message=status_message)
          
          #This can happen if there is a complete failure and calculation gets rejected from calculation queue.  That reporting_asof_date gets omitted from the 
          #empty results is an oversight and this is a workarond to permit validating-with-errors
          calculations_results[is.na(reporting_asof_date),
                               reporting_asof_date:=rsf_data_wide$reporting_current_date[1]]
          
          current_data[calculations_results,
                       calculated:=TRUE,
                       on=.(calculate_rsf_pfcbl_id=rsf_pfcbl_id,
                            calculate_indicator_id=indicator_id,
                            calculate_asof_date=reporting_asof_date)]
          
          
          
          #This may not happen now since calculations will return default/NA with update to include data checks on calculated results
          if (empty(calculations_results)) next; #Calculations didn't calculate anything
          
          #db_program_get_data() can return so-called pseudo rsf_ids/rsf_pfcbl_ids that result when indicators are specifically requested to db_program_get_data
          #for which that entity doesn't exist or has never submitted data in that category.  db_program_get_data will make-up a pseudo ID to ensure consistency in joins.
          #When passed to rsf_indicators_calculate() the results will be NAs or default 0s, etc that have no meaning since data is fabricated.  Ensure these are removed, if they exist.
          #Any uploads will subsequently fail since data cannot be saved for an invalid rsf_pfcbl_id
          
          if (!empty(calculations_results[rsf_pfcbl_id < 0])) {
            print(paste0("Rejecting calculations due to non-existant entity: ",paste0(sort(calculations_results[rsf_pfcbl_id < 0,unique(indicator_name)]),collapse=", ")))
            calculations_results <- calculations_results[rsf_pfcbl_id > 0]
            
            if (empty(calculations_results)) next; #Calculations omitted all pseudo_ids
          }
        }
        
        calculations_groups_results <- rbindlist(list(calculations_groups_results,
                                                      calculations_results))
        
      }
      
      
      # setdiff(current_data[,
      #                      .(calculate_rsf_pfcbl_id,
      #                        calculate_indicator_id,
      #                        calculate_asof_date)],
      #         calculations_groups_results[,
      #                      .(calculate_rsf_pfcbl_id=rsf_pfcbl_id,
      #                        calculate_indicator_id=indicator_id,
      #                        calculate_asof_date=reporting_asof_date)])

      #Note: calculator should return a result for all requests, hence the hard stop error
      #If the calculation itself throws errors, they should be caught, flagged and then return NA results for the request.
      if (any(!current_data$calculated)) {
        calc_failures <- current_data[calculated==FALSE]
        #browser()
        if (!empty(calc_failures)) {
          calc_failures <- calc_failures[,.(failed_ids=paste0(calculate_rsf_pfcbl_id,collapse=",")),by=.(calculate_indicator_id)]
          stop(paste0("Calculator failed to calculate requests for: \n ",paste0(paste0("indicator_id:",calc_failures$calculate_indicator_id," for SYSIDs:",calc_failures$failed_ids),collapse="\n ")))
        }
      }
    }
    
    #Create current results to compare current data values and the calculated results
    {
      
      
      ###############
      
      current_results <- calculations_groups_results[current_data[,.(rsf_pfcbl_id=calculate_rsf_pfcbl_id,
                                                                     indicator_id=calculate_indicator_id,
                                                                     reporting_asof_date=calculate_asof_date,
                                                                     current_reporting_cohort_id,
                                                                     current_data_id,
                                                                     current_data_value,
                                                                     current_data_unit,
                                                                     current_data_is_system_calculation,
                                                                     current_value_is_user_monitored,
                                                                     current_value_updated_in_reporting_current_date,
                                                                     entity_local_currency_unit,
                                                                     formula_id,
                                                                     data_type,
                                                                     is_periodic_or_flow_reporting)],
                                                     .(rsf_pfcbl_id,
                                                       indicator_id,
                                                       reporting_asof_date,
                                                       data_type,
                                                       data_value,
                                                       data_unit,
                                                       data_flags_new,
                                                       fx_data_ids,
                                                       current_data_id,
                                                       current_reporting_cohort_id,
                                                       current_data_value,
                                                       current_data_unit,
                                                       current_data_is_system_calculation,
                                                       current_value_is_user_monitored,
                                                       current_value_updated_in_reporting_current_date,
                                                       entity_local_currency_unit,
                                                       formula_id,
                                                       is_periodic_or_flow_reporting),
                                                     on=.(rsf_pfcbl_id,
                                                          indicator_id,
                                                          reporting_asof_date,
                                                          formula_id),
                                                     nomatch=NULL]
      
      ##############
      #calculations_results <- calculations_results[rsf_pfcbl_id %in% calculations_pfcbl_ids]
      
      if (empty(current_results)) next;  #Calculations doesn't have any valid rsf_pfcbl_ids to apply results to (or what it did calculate is outside of reporting)

      

      #2023-07-12: Removed parse_data_formats for calculated data
      #All data formats are guaranteed to be the correct data type (or NA) as the result of formulas built-in as.numeric() etc.,
      #So the only place this can be relevant is calculated options results that yield category data -- presently no such formulas exist
      #and even if they did and the formula is wrong, then this is an extraordinary overhead for each upload and calculation iteration to
      #enable flagging a super rare formula result
      # parsed_calculated_data <- parse_data_formats(template_data=calculations_results,
      #                                              rsf_indicators=rsf_indicators,
      #                                              sys_indicator_format_check=TRUE)

      status_message(class="none","\n")
      

      current_results[formulas,
                      `:=`(formula_overwrite=i.formula_overwrite,
                           is_system=i.is_system),
                      on=.(formula_id)]
      
      # current_results[rsf_indicators[,.(data_type,
      #                                          formula_overwrite,
      #                                          is_system,indicator_id)],
      #                        `:=`(data_type=i.data_type,
      #                             formula_overwrite=i.formula_overwrite,
      #                             is_system=i.is_system),
      #                        on=.(indicator_id)]

      current_results[is.na(formula_overwrite)==TRUE,
                             formula_overwrite:="allow"]
      
      current_results[,data_changed:=is.na(current_data_id) |
                                     !(is.same_text(data_value,current_data_value))| 
                                     !(is.same_text(data_unit,current_data_unit))]
      
      #where changed is TRUE, possible change it back to FALSE if these are numeric valuse that are effectively the same (within .001% of the same value of eachother)
      current_results[data_changed==TRUE
                             & data_type %in% c("number","percent","currency","currency_ratio")
                             & !is.na(current_data_id)
                             & is.same_text(data_unit,current_data_unit),
                             data_changed:=!(is.same_number(data_value,current_data_value))] #if it's deny then allowed to have different values; and if hashids are the same, calculation attempt and flag has already been done
      #1004
      #parsed_calculated_data[data_changed==TRUE & data_value==current_data_value,.(indicator_name,data_type,data_value,current_data_value,data_unit,current_data_unit)]
      current_results[,current_value_reported_in_current_asof_date:=ifelse(is.na(current_data_id),
                                                                                FALSE,
                                                                                current_value_updated_in_reporting_current_date)]

      #No current_data_id means no value has EVER been entered by user or calculator (Fresh start!)
      current_results[,current_value_is_missing:=is.na(current_data_id)] 
      
      #Means that NA/Blank value is INTENTIONALLY uploaded in THIS reporting period.  Ie, user set it to BLANK (from non-blank presumably)      
      current_results[current_value_is_missing==FALSE,
                      current_value_is_missing:=is.na(current_data_value) & 
                                                current_value_updated_in_reporting_current_date==TRUE]
      
      
      
      current_results[,flag_overwrite:=FALSE]            
      current_results[is_system ==FALSE &                          #not a system indicator
                             is.na(current_data_id)==FALSE &              #has an existing data point
                             (current_data_is_system_calculation==FALSE | #current data is user reported (not system reported)
                              current_value_is_user_monitored==TRUE),     #or it's system reported, but user previously reported it
                             flag_overwrite:=TRUE]
      
    }

    #Set insert actions
    {
      #overwrite types: 
      #allow - always allow calculator to overwrite, when appropriate
      #deny  - never allow calculator to overwrite, even if there's a calculated difference
      #missing - only allow calculator to overwrite if the entry is missing, never (ever) been entered
      #unchanged - only allow calculator to overwrite if the entry is missing for the current reporting reporting (ie, nobody changed it this reporting cycle)
      #          - this essentially allows users to enter a manual entry for a reporting period and if a manual entry is there for that period, calculator will ignore;
      #          - but if nothing is entered for the period, then calculator will write; but if a user re-updates or corrects, the calculator won't re-overwrite
      
      #If it's a flow data value, and it's the same as what's already there, and it's not already there in the same timeline, then consider
      #it a flow of the same magnitude as what was previously reported and allow it to upload.
      current_results[data_changed==FALSE &
                      is_periodic_or_flow_reporting==TRUE &
                      current_value_updated_in_reporting_current_date==FALSE,
                      data_changed:=TRUE]
      
      current_results[,insert_action:=data_changed]
      
      #If set to deny, we don't want the calculator to provide a value...
      #Unless no value has EVER been submitted, in which case calculator is allowed to calculate it once, for the first time 
      #(Else, if its never allowed to calculate it ever, then why is the program subscribed to the indicator?)
      #
      #QUESTION: should deny report NA/default value instead of the calculated value if calculator is prompted to calculate it and also instructed
      #to deny reporting results?  
      #Or should deny work as currently set now (OCT-2023) to only deny after first calculation?
      current_results[formula_overwrite=="deny"
                      & is.na(current_data_id)==FALSE,
                      insert_action:=FALSE]
      
      #for indicator formulas of data type "missing"
      #missing means permanent manual override
      #this setting is especially useful for templates that omit enitre columns and therefore the data point will never be reported; in which
      #case the system can calculate it according to its formula.  But for templates where the column does exist, it's effectively a user-reported
      #data point that will receive a system calculation disagreement flag if it calculates something differently.
      #This can also be useful for backfilling templates where columns don't exist and then are added later.  At the point they're later added (and
      #when the user submits a new value), then the user reporting will take over and replace the automated system calculation.
      current_results[formula_overwrite=="missing" &
                             data_changed==TRUE,
                             insert_action:= current_value_is_missing==TRUE |          #Either its missing...
                                             current_data_is_system_calculation==TRUE  #Or it's being calculated by the system and system may update its own calculations
                             ]
      

      #unchanged means manual override for current reporting period,
      #but unless new value reported in subsequent periods then calculator will recalculate when triggered to do so
      #this allows a user to manually overwrite a value for a given QR, which can be useful for ad-hoc billing adjustments, for example
      #and the system will let manual overwrites stand for the given reporting period and re-calculate itself starting next period.
      current_results[formula_overwrite=="unchanged" &
                             data_changed==TRUE,
                             insert_action:= current_value_updated_in_reporting_current_date==FALSE | #Either nothing reported in this asof date....
                                             current_data_is_system_calculation==FALSE                #Or it's being calculated by the system 
                                                                                                      #and system may update its own calculations
      ]
      
      #This shouldn't happen (but it formerly did when formula_overwrite deny categorically denied -- now, it permits first calculate)
      #Here just in case
      #Because if no current_data_id exists and the checker attempts to validate it, it will valiate it AND set the validated_data_id to
      #whatever exists in rsf_data_current -- but if none exists, it will never be validated and infinite loop.  
      current_results[is.na(current_data_id) &
                      insert_action==FALSE,
                      insert_action:=TRUE]

    }
    
    #create current flags
    #calculation_flags
    {
      format_flags <- current_results[,unlist(data_flags_new,recursive=F),
                                              by=.(rsf_pfcbl_id,
                                                   indicator_id,
                                                   reporting_asof_date)]
      
      current_results[,data_flags_new:=NULL]
      current_results[,
                      equivalent:=FALSE]
      
      #For periodic or flow reporting type indicators, these are measures of change in the period or amounts of something that happened in the period.
      #For numeric data types, NA and 0 both report a lack of change: nothing happened.  
      #The value that results from the calculation will be uploaded if overwrite is appropriate.
      #But there is no need to flag that system calculated a different type of nothing.
      current_results[indicator_id %in% rsf_indicators[is_periodic_or_flow_reporting==TRUE,indicator_id] &
                      is.same_text(data_unit,current_data_unit) == TRUE &
                      (
                        is.same_text(data_value,current_data_value) |
                        (suppressWarnings(as.numeric(current_data_value))==0 & is.na(data_value)) |
                        (suppressWarnings(as.numeric(data_value))==0 & is.na(current_data_value))
                      ),
                      equivalent:=TRUE]
      
      if (!empty(format_flags)) calculation_flags <- rbindlist(list(calculation_flags,
                                                                    format_flags))
      
      overwrite_flags <- current_results[flag_overwrite==TRUE &
                                         data_changed ==TRUE &
                                         insert_action==TRUE &
                                         equivalent==FALSE,
                                                .(rsf_pfcbl_id,
                                                  indicator_id,
                                                  reporting_asof_date,
                                                  check_name="sys_calculator_overwrote_manual_calculation",
                                                  check_message=paste0(ifelse(formula_overwrite != "allow",
                                                                              paste0("(overwrite=\"",formula_overwrite,"\") "),
                                                                              ""),
                                                                      "{",ifelse(is.na(current_data_value),"MISSING",current_data_value),
                                                                           ifelse(is.na(current_data_unit),"",paste0(" ",current_data_unit)),"}",
                                                                      ' -> ',
                                                                      "{",ifelse(is.na(data_value),"MISSING",data_value),
                                                                          ifelse(is.na(data_unit),"",paste0(" ",data_unit)),"}"))]
      
      if (!empty(overwrite_flags)) calculation_flags <- rbindlist(list(calculation_flags,
                                                                       overwrite_flags))
      overwrite_flags <- NULL
      
      
      vs_flags <- current_results[flag_overwrite==TRUE &
                                  data_changed ==TRUE & 
                                  insert_action==FALSE & 
                                  equivalent==FALSE,
                                         .(rsf_pfcbl_id,
                                           indicator_id,
                                           reporting_asof_date,
                                           check_name="sys_calculator_vs_manual_calculation",
                                           check_message=paste0("System calculated {",
                                                                ifelse(is.na(data_value),"MISSING",data_value),
                                                                ifelse(is.na(data_unit),"",paste0(" ",data_unit)),"}",
                                                               " vs Reported {",
                                                                ifelse(is.na(current_data_value),"MISSING",current_data_value),
                                                                ifelse(is.na(current_data_unit),"",paste0(" ",current_data_unit)),"}",
                                                               " / Manual validation or correction required. Not overwritten because indicator overwrite=\"",formula_overwrite,"\""))]
      
      if (!empty(vs_flags)) calculation_flags <- rbindlist(list(calculation_flags,
                                                                vs_flags))
      
      vs_flags <- NULL
      
      #all system flags have NA formulas as they are by definition internally computed.
      calculation_flags[,
                        check_formula_id:=as.numeric(NA)]
      
      current_results[,flagged:=FALSE]
      current_results[unique(calculation_flags[,.(rsf_pfcbl_id,indicator_id,reporting_asof_date)]),
                          flagged:=TRUE,
                          on=.(rsf_pfcbl_id,indicator_id,reporting_asof_date)]
      
      
    }
    
    #Upload changes and verify validations
    {
      
      if (perform.test==TRUE) {
        message("Early function termination.  Returning parsed calculated data because perform.test=TRUE")
        flags <- calculation_flags[,.(flags=paste0(
                                        paste0(check_name,"=",check_message),
                                        collapse=" AND ALSO ")),
                                   by=.(rsf_pfcbl_id,
                                        indicator_id,
                                        reporting_asof_date)]
        current_results <- flags[current_results,
              on=.(rsf_pfcbl_id,indicator_id,reporting_asof_date)]
        
        return (list(inputs=rsf_data_wide,
                     results=current_results,
                     flags=flags))
      }
      
      if (any(is.na(current_results$insert_action))) {
        stop("Failed to set insert action on current_results")
      }
      
      fx_calculations <- unique(current_results[,
                                                .(fx_data_id=as.numeric(unlist(fx_data_ids,recursive = F))),
                                                by=.(rsf_pfcbl_id,
                                                     indicator_id,
                                                     reporting_asof_date)])
      #This test is made BEFORE insert actions so that the offending calculation won't be cleared-out of the evaluations queue and easier to track down.
      #But the fx_calculation data points are uploaded AFTER insert actions so that they can reference the current/correct data point in rsf_data_current
      if (!empty(fx_calculations)) {

        if (anyNA(fx_calculations$fx_data_id)) {
          bad_ids <- fx_calculations[is.na(fx_data_id),
                                     .(rsf_pfcbl_ids=paste0(na.omit(unique(rsf_pfcbl_id)[1:20]),collapse=",")),
                                     by=.(indicator_id)]
          bad_ids[,
                  message:=paste0("indicator_id=",indicator_id," calculated rsf_pfcbl_ids=",rsf_pfcbl_ids)]
          stop("NA fx_data_id returned, which should not happen.  This may occur when the requested FX currency unit is invalid or the entity's local currency unit is invalid (which also should not happen).\n ",
               "Issue experienced for these calculations:\n ",
               paste0(bad_ids$message,collapse=" \n")) 
        }
      }
      
      if (any(current_results$insert_action==TRUE)) {
        
        status_message(class="none","Saving ",nrow(current_results)," results to database...\n")

        system_upload_data <- current_results[insert_action==TRUE,
                                                    .(current_data_id,
                                                      rsf_pfcbl_id,
                                                      indicator_id,
                                                      reporting_asof_date,
                                                      data_unit,
                                                      data_value,
                                                      flagged)]

        #This should occur in BEFORE trigger on rsf.data
        #revert currency codes to LCU before uploaded, where applicable.  This will allow correct comparisons for changes in data units in the database
        #and keeping LCU values as "LCU" enables system to manage changes to LCU context if there's a change in currency regeime (see Ghana change from GHC to GHS in 2007)
        lcu_calculations <- current_data[rsf_indicators[data_type=="currency" & data_unit=="LCU",
                                                        .(calculate_indicator_id=indicator_id)],
                                          .(rsf_pfcbl_id=calculate_rsf_pfcbl_id,
                                            indicator_id=calculate_indicator_id,
                                            reporting_asof_date=calculate_asof_date,
                                            entity_local_currency_unit),
                                          on=.(calculate_indicator_id),
                                          nomatch=NULL][!is.na(entity_local_currency_unit) &
                                                        entity_local_currency_unit != "LCU"]
        
        system_upload_data[lcu_calculations,
                           data_unit:="LCU",
                           on=.(rsf_pfcbl_id,
                                indicator_id,
                                reporting_asof_date,
                                data_unit=entity_local_currency_unit)]
        
        
        db_add_update_data_system(pool=pool,
                                  system_upload_data=system_upload_data)
        
      }
      
      if (any(current_results$insert_action==FALSE)) {
        unchanged_results <- current_results[insert_action==FALSE,
                                                    .(data_id=current_data_id,
                                                      rsf_pfcbl_id,
                                                      indicator_id,
                                                      reporting_asof_date)]  #Calculation_failed presently unused!
        
        status_message(class="none","Validating ",nrow(unchanged_results)," previously calculated results.\n")

        db_program_revalidate_calculations(pool=pool,
                                           calculation_verifications=unchanged_results)

        unchanged_results<-NULL
      }
      
      #For those calculations that required an fx conversion, save the data ID of the fx value used as part of the calculation
      #(so that if that data_id becomes stale, it can invalidate the calculation that relied on it)
      if (!empty(fx_calculations)) {
        
        poolWithTransaction(pool,function(conn) {
          
          dbExecute(conn,"create temp table _temp_fx_ids(rsf_pfcbl_id int,
                                                         indicator_id int,
                                                         reporting_asof_date date,
                                                         fx_data_id int)
                          on commit drop;")
          
          dbAppendTable(conn,
                        name="_temp_fx_ids",
                        value=fx_calculations)
          
          dbExecute(conn,"analyze _temp_fx_ids")
          
          dbExecute(conn,"delete from p_rsf.rsf_data_current_fx dcf
                          where exists(select * from _temp_fx_ids fids
                                       where fids.rsf_pfcbl_id = dcf.rsf_pfcbl_id
                                         and fids.indicator_id = dcf.indicator_id
                                         and fids.reporting_asof_date = dcf.reporting_asof_date)
                            and not exists(select * from _temp_fx_ids fids
                                           where fids.rsf_pfcbl_id = dcf.rsf_pfcbl_id
                                             and fids.indicator_id = dcf.indicator_id
                                             and fids.reporting_asof_date = dcf.reporting_asof_date
                                             and fids.fx_data_id = dcf.fx_data_id)")
          
          dbExecute(conn,"insert into p_rsf.rsf_data_current_fx(rsf_pfcbl_id,
                                                                indicator_id,
                                                                reporting_asof_date,
                                                                fx_data_id)
                          select
                            fids.rsf_pfcbl_id,
                            fids.indicator_id,
                            fids.reporting_asof_date,
                            fids.fx_data_id
                          from _temp_fx_ids fids
                          on conflict do nothing;
                    ")
        })
      }

    }
  
  }
  
  if (!empty(calculation_flags)) {
    #testing
    #testing <- sapply(strsplit(calculation_flags$check_message,split=" -> "),function(x) { length((trimws(unlist(x))))==2 && length(unique(trimws(unlist(x))))==1 })
    #if (any(testing)) { browser() }
    
    variance_results <- current_results[flagged == TRUE &
                                        data_type %in% c("number","percent","currency","date","currency_ratio"),
                                        .(rsf_pfcbl_id,
                                          indicator_id,
                                          reporting_asof_date,
                                          data_type,
                                          data_value,
                                          current_data_value,
                                          current_data_id)]
    variance_results[data_type=="date",
                     `:=`(data_value=as.numeric(ymd(data_value)),
                          current_data_value=as.numeric(ymd(data_value)))]
    variance_results[,
                     variance:=abs(fcase(data_type=="percent",100*(as.numeric(current_data_value)-as.numeric(data_value)),  #Percentage points
                                         data_type=="date",(as.numeric(current_data_value)-as.numeric(data_value)),     #Date variance in days
                                         !data_type %in% c("date","percent"),100*((as.numeric(current_data_value)/as.numeric(data_value))-1), #Percent
                                         default=0)
                                   )]
    
    variance_results <- variance_results[!is.na(variance) &
                                         !is.na(current_data_id)]
    
    #if we have divide by zero or other oddity in calculating the variance, set to 101%
    variance_results[is.nan(variance) |
                     is.infinite(variance),
                     variance:=101]
    
    calculation_flags <- calculation_flags[,
                                           .(rsf_pfcbl_id,
                                             for_indicator_id=indicator_id,
                                             check_formula_id,
                                             check_asof_date=reporting_asof_date,
                                             check_name,
                                             check_message)]
    
    check_names <- unique(calculation_flags$check_name)
    check_ids <- dbGetQuery(pool,
                            "select 
                                ic.check_name,
                                ic.indicator_check_id,
                                ic.variance_tolerance_allowed
                              from p_rsf.indicator_checks ic
                              where ic.is_calculator_check = true")
    
    setDT(check_ids)
    
    if (!all(check_names %in% check_ids$check_name)) {
      stop("Failed to lookup system check names designated as is_calculator_check=true: ",
           paste0(check_names[-which(check_names %in% check_ids$check_name)],
                  collapse=" AND ALSO "))
    }
    
    calculation_flags[,
                      variance_tolerance_allowed:=FALSE]
    
    calculation_flags[check_ids,
                      `:=`(indicator_check_id=i.indicator_check_id,
                           variance_tolerance_allowed=i.variance_tolerance_allowed),
                      on=.(check_name)]
    
    variance_results[,variance_tolerance_allowed:=TRUE]
    calculation_flags[,variance:=as.numeric(NA)]
    calculation_flags[variance_results,
                      `:=`(variance=i.variance,
                           check_message=paste0(check_message," (",round(i.variance,1),"% variance)")),
                      on=.(rsf_pfcbl_id,
                           for_indicator_id=indicator_id,
                           check_asof_date=reporting_asof_date,
                           variance_tolerance_allowed)]
    
    calculation_flags <- calculation_flags[,
                                           .(rsf_pfcbl_id,
                                             for_indicator_id,
                                             check_asof_date,
                                             indicator_check_id,
                                             check_formula_id,
                                             check_message,
                                             variance)]

    db_rsf_checks_add_update(pool=pool,
                             data_checks=calculation_flags)
    
  }  
  if(SYS_PRINT_TIMING) debugtime("rsf_program_perform_calculations","Done!",format(Sys.time()-t1))
}
