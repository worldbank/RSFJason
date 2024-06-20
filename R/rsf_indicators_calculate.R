rsf_indicators_calculate <- function(pool,
                                     rsf_indicators,
                                     rsf_data_wide,
                                     calculations,
                                     status_message=function(...) {}) { 
  
  #Setups
  {
    # rdw <<- as.data.frame(rsf_data_wide)
    # ca <<- as.data.frame(calculations)
    # 
    # rsf_data_wide <- as.data.table(rdw)
    # calculations <- as.data.table(ca)
    
    if (is.null(calculations) || empty(calculations)) return (NULL) #nothing to calculate
    if (empty(rsf_data_wide)) return (NULL) #no data?  Shouldn't happen
    
    # if (!setequal(calculations$calculation_group,1:nrow(calculations))) {
    #   stop(paste0("Requested ",nrow(calculations)," but specified different calculation groups"))
    # }
    
    reporting_current_date <- unique(rsf_data_wide[,reporting_current_date])
    #calculation_rank <- unique(calculations$formula_calculation_rank)
    
    if (length(reporting_current_date) != 1) stop(paste0("reporting_current_date is not unique: ",paste0(reporting_current_date)))
    #if (length(calculation_rank) != 1) stop(paste0("calculations_dt must supply a data.table of calculations with only one unique formula_calculation_rank value at a time"))
    #if (is.null(calculations$for_rsf_pfcbl_id)) calculations[,for_rsf_pfcbl_id:=NA]
    
    #each for_rsf_pfcbl_id should be calculated once per indicator/formula
    dups <- calculations[,.(rsf_pfcbl_id=unlist(calculate_rsf_pfcbl_ids,recursive=F)),
                                  by=.(indicator_name)][,.(n=.N),
                                                          by=.(indicator_name,
                                                               rsf_pfcbl_id)][n>1]
    if (!empty(dups)) {
      stop(paste0("The following indicators were passed to rsf_indicators_calculate multiple times: ",paste0(dups$indicator_name,collapse=",")))
    }
    dups <- NULL
    
    results_list <- list() 
    reporting_checks_list <- list()
    
    data_flags_list <- list()
    
    add_data_flag <- function(rsf_pfcbl_id,
                              indicator_id,
                              check_name,
                              check_message,
                              formula_id) {

      if (class(rsf_pfcbl_id)=="list") stop(paste0("List of IDs passed to add_data_flag for indicator_id=",indicator_id,
                                                   ": ",check_name,": ",check_message))
      
      flag_ids <- intersect(unlist(calculations$calculate_rsf_pfcbl_ids),rsf_pfcbl_id)
      flag_ids <- flag_ids[is.na(flag_ids)==FALSE]
      flag_ids <- flag_ids[flag_ids >= 0]
      
      if (length(flag_ids)==0) return(NULL)
      #if (!rsf_pfcbl_id %in% unique(c(calculations$for_rsf_pfcbl_id,unlist(calculations$calculate_rsf_pfcbl_ids)))) return(NULL)
      
      data_flags_list[[length(data_flags_list)+1]] <<- data.table(rsf_pfcbl_id=flag_ids,
                                                                 indicator_id=indicator_id,
                                                                 check_name=check_name,
                                                                 check_message=check_message,
                                                                 formula_id=formula_id)
    }
  }

    #START 
  #testing specific claculation:
  #i<-which(sapply(calculations_list,'[[','indicator_name')=='loan_defaulted_recovery_amount_total')
  #
  #i<-1
  
  fx_conversions <- data.table(rsf_pfcbl_id=numeric(0),
                               indicator_id=numeric(0),
                               reporting_asof_date=as.Date(numeric(0)),
                               exchange_rate_data_id=numeric(0))
  
  fx_table <- data.table(rsf_pfcbl_id=numeric(0),
                         from_currency=character(0),
                         to_currency=character(0),
                         fx_date=as.Date(numeric(0)),
                         fx_rate=numeric(0),
                         fx_pfcbl_category=character(0),
                         exchange_rate_data_id=numeric(0))
  #i<-1
  for (i in 1:nrow(calculations)) {
    calc_time <- Sys.time()
    status_message(class="none"," ",paste0("#",i))

    calculation <- calculations[i]
    
    
    {
      calculation_rsf_id_col <- paste0("rsf_",calculation$data_category,"_id")
      calculation_rsf_pfcbl_id_col <- paste0("rsf_pfcbl_id.",calculation$data_category)
      
      data_received_for_rsf_pfcbl_ids <- unique(rsf_data_wide[[calculation_rsf_pfcbl_id_col]])
      calculate_for_rsf_pfcbl_ids <- unlist(calculation$calculate_rsf_pfcbl_ids)
      #check parameters compliance
      {
        
        parameters <- rbindlist(calculation$parameters_dt)
        
        if (any(is.na(calculation$formula)) || empty(parameters)) {
            add_data_flag(rsf_pfcbl_id = calculate_for_rsf_pfcbl_ids,
                          indicator_id=calculation$calculate_indicator_id,
                          check_name="sys_calculator_failed",
                          check_message=paste0(calculation$indicator_name," has no defined formula. Enter a calculation formula for this indicator in Admin Panel."),
                          formula_id=calculation$formula_id)
            
            status_message(class="error",paste0("\nCalculation failed: ",calculation$indicator_name," has no defined formula.\n"))

          next;
        }
        
        
        parameters[,missing:=!(parameter_column_name %in% names(rsf_data_wide))]
        
        if (any(parameters$missing)) {
          stop(paste0("Failed to calculate ",calculation$indicator_name," due to missing parameters in the database: ",
                      paste0(parameters[missing==TRUE,parameter_column_name],collapse=",")))
        }
        
        #Calc results adds in the calculate_indicator_data_unit into the results set, so must be length 1.
        if (length(calculation$calculate_indicator_data_unit) != 1) {
          stop(paste0("Failed to calculate ",
                      calculation$indicator_name,
                      " due to calculate_indicator_data_unit=",
                      paste0(calculation$calculate_indicator_data_unit,collapse=" & ")))
        }
        
      }      
      
      
      #formula_indicator_ids may specify more than the required indicators -- calculated_by formulas have array for ALL the parameters for ALL individual formulas
      #that are used by ALL the programs.  Eg, "this" calculation might have a formula definition for indicators that other programs do not even subscribe to!
      #And therefore, there is not even a gurantee that all the values in formula_indicator_id_requirements even exist in this calculation.
      #The reverse lookup uses parses the formula text to identify the _actual_ indicators that are being used to filter for exactly what's needed (and also to ensure that
      #what's needed--by name--exists in the rsf_data_wide that's provided)
      #valid_indicators <- rsf_indicators[,.(indicator_id,indicator_name)]

      # calculation_formula_sort <- trimws(calculation_formula_sort,whitespace="[ \\t\\r\\n\\v\\h\\s]")
      # if (!is.na(calculation_formula_sort) && nchar(calculation_formula_sort)==0) calculation_formula_sort <- NA
      
      
      
      #IDs compliances
      {
        
        
        #Except for aggregates at program level (or in the non-existent case of facility having multiple clients)
        # if (length(unique(rsf_data_wide$rsf_client_id))>1) {
        #   stop(paste0("Multiple rsf_client_ids sent to calculation ",calculation$indicator_name,": IDs ",
        #   paste0(unique(rsf_data_wide$rsf_client_id),collapse=" & "),". Calculations must be performed only at the client level."))
        # }
        
        bad_ids <- which(data_received_for_rsf_pfcbl_ids < 0 | is.na(data_received_for_rsf_pfcbl_ids))
        if (any(bad_ids)) {
          data_received_for_rsf_pfcbl_ids[-bad_ids]
        }
        
        if (!all(calculate_for_rsf_pfcbl_ids %in% data_received_for_rsf_pfcbl_ids)) {
          stop(paste0("Requested to calculate SYSIDs: ",paste0(sort(calculate_for_rsf_pfcbl_ids),collapse=",")," BUT received data for ",
                      paste0(sort(data_received_for_rsf_pfcbl_ids),collapse=",")))
        }
        
        if (length(data_received_for_rsf_pfcbl_ids)==0) {
          stop(paste0("rsf_data_wide has missing rsf_pfcbl_ids for calculation ",calculation$indicator_name," in column ",calculation_rsf_pfcbl_id_col))
        }
        
        if (any(is.na(calculation$formula))) {
          #system calculations may have an internal/custom calculation that's performed elsewhere.  Those that exclusively calculate internally will have a NULL 
          #formula value.  Others may have a formula that's manually called internally and also may be calculated through this automated process
          #hashids are an example: may be manually calculated to support look-up of IDs before data is saved; but also auto-calculated here in response to any updates
          #to their input values.  Whereas hashvalue values are only internally calculated (as they are artifacts of what gets uploaded rather than a response to changes in saved data)
          if (is_system==FALSE) {
            add_data_flag(rsf_pfcbl_id = calculate_for_rsf_pfcbl_ids,
                          indicator_id=calculation$calculate_indicator_id,
                          check_name="sys_calculator_failed",
                          check_message=paste0(calculation$indicator_name," has no defined formula. Enter a calculation formula for this indicator in Admin Panel."),
                          formula_id=calculation$formula_id)
            
            status_message(class="error",paste0("\nCalculation failed: ",calculation$indicator_name," has no defined formula.\n"))
          }
          
          next;
        }
        
        if (!calculation_rsf_id_col %in% names(rsf_data_wide)) {
          status_message(class="error",paste0("\nCalculation failed: ",calculation$indicator_name,"\n  Formula unable to find sys_id colum for calculations: ",calculation_rsf_id_col,".  Skipping.\n"))
          stop("This error should not occur -- db_program_get_data() should return void indicators")
          next;
        }
        
        if (!calculation_rsf_pfcbl_id_col %in% names(rsf_data_wide)) { 
          status_message(class="error",paste0("\nCalculation failed: ",calculation$indicator_name,"\n  Formula unable to find rsf_pfcbl_id colum for calculations: ",calculation_rsf_pfcbl_id_col,".  Skipping.\n"))
          add_data_flag(rsf_pfcbl_id = calculate_for_rsf_pfcbl_ids,
                        indicator_id=calculation$calculate_indicator_id,
                        check_name="sys_calculator_failed",
                        check_message=paste0(calculation$indicator_name,
                                             " formula unable to identify SYSID column for calculations. Correct bugs in formula definition in Admin Panel."),
                        formula_id=calculation$formula_id)
          next;
        }
        
        if (any(parameters$missing)) { 
          status_message(class="error",
                         paste0("\nCalculation failed: ",calculation$indicator_name,
                                "\nCalculation formula: ",calculation$formula,
                                "\nDoes not define any formula_indicator_ids. Verify definition in Admin Panel.  Verify that parameters are valid indicator names\n"))
          
          add_data_flag(rsf_pfcbl_id = calculate_for_rsf_pfcbl_ids,
                        indicator_id=calculation$calculate_indicator_id,
                        check_name="sys_calculator_failed",
                        check_message=paste0(calculation$indicator_name," formula {",calculation$formula,"} does not define any parameters. Verify that parameters are valid indicator names. 
                                             Correct bugs in formula definition in Admin Panel."),
                        formula_id=calculation$formula_id)
          
          next;
        }
      }  
      
      formula_rsf_ids <- unique(gsub("^rsf_pfcbl_id.([a-z]+)$","rsf_\\1_id",unlist(calculation$formula_pfcbl_id_categories)))
      
      cols <- c("reporting_current_date",
                calculation_rsf_pfcbl_id_col,
                calculation_rsf_id_col,
                formula_rsf_ids,
                parameters$parameter_column_name)
      

      cols <- unique(cols) #indicator_rsf_id and formula_rsf_ids may be the same
      
      if (!all(cols %in% names(rsf_data_wide))) {
        bad_cols <- cols[!cols %in% names(rsf_data_wide)]
        add_data_flag(rsf_pfcbl_id = calculate_for_rsf_pfcbl_ids,
                      indicator_id=calculation$calculate_indicator_id,
                      check_name="sys_calculator_failed",
                      check_message=paste0(paste0("Calculation failed for '",
                                                  calculation$indicator_name,
                                                  "' because required input columns missing from database query: ",
                                                  paste0(bad_cols,collapse=", "),
                                                  " (verify that all formula attribute/variables are valid)")),
                      formula_id=calculation$formula_id)
        
        status_message(class="error",paste0("Calculation failed for '",
                                            calculation$indicator_name,"' because required input columns missing in rsf_data_wide: ",
                                            paste0(cols[which(!cols %in% names(rsf_data_wide))],collapse=", ")),"\n")
        next;
      }
      
      
      
      calc_data <- NULL
      

      
      calc_data <- unique(rsf_data_wide[,..cols])
      
      setnames(calc_data,
               old=c(calculation_rsf_pfcbl_id_col),
               new=c("rsf_pfcbl_id"))
      
      #If we're calculating it for 2 new rsf_pfcbl_id loans, for example, but the grouping is at the client level, we can't filter the dataset for
      #only the entities being calculated if we need to review the entire dataset.
      
      #check this for inclusion rank 
      #calc_data <- calc_data[rsf_pfcbl_id %in% data_received_for_rsf_pfcbl_ids]
      
      #can happen if an entity comes in via a default or missing indicator
      
      #missing_rsf_ids <- which(Reduce(any,sapply(calc_data[,..formula_rsf_ids],is.na))) #returns rows that have NA for any input rsf_ids
      missing_rsf_ids <- which(sapply(as.data.frame(t(calc_data[,..formula_rsf_ids])),anyNA))
      if (length(missing_rsf_ids)>0) {
        calc_data[,missing_ids:=FALSE]
        calc_data[missing_rsf_ids,
                  missing_ids:=TRUE]
        
        missing_ids <- calc_data[missing_ids==TRUE]
        calc_data <- calc_data[missing_ids==FALSE]
        calc_data[,missing_ids:=NULL]
        
        if (!empty(missing_ids)) {
          status_message(class="error",paste0("\nCalculation failed: ",calculation$indicator_name,"\n  Formula has no data to calculate after filtering for entity missing IDs.  Skipping.\n"))
          add_data_flag(rsf_pfcbl_id = unlist(missing_ids$rsf_pfcbl_id),
                        indicator_id=calculation$calculate_indicator_id,
                        check_name="sys_calculator_failed",
                        check_message=paste0(calculation$indicator_name," formula has no data after filtering for missing ",
                                             paste0(formula_rsf_ids,collapse=" AND/OR "),
                                             ". Ensure ",toTitleCase(calculation$data_category)," has reported any ",
                                             fcase(any(formula_rsf_ids=="rsf_loan_id"),"Loans",
                                                   any(formula_rsf_ids=="rsf_borrower_id"),"Borrowers",
                                                   any(formula_rsf_ids=="rsf_client_id"),"Clients",
                                                   any(formula_rsf_ids=="rsf_facility_id"),"Facilities",
                                                   any(formula_rsf_ids=="rsf_program_id"),"Programs",
                                                   default="data"),
                                             " as-of ",as.character(calculation$calculate_asof_date)," ",
                                             " and all input variables are defined and reported for this formula to calculate."),
                        formula_id=calculation$formula_id)
        }
        
        if (empty(calc_data)) {
          next; #all IDs are missing
        }
      }
      
    
      #this can happen if formula has variables with parent-level and child-level indicators whereby the parent does not have any defined children.
      #eg, facility level indicator compares loan level indicator but the facility client never uploaded any data and there are no loans... so the
      #application of calculation results at loan-level rsf_pfcbl_id will be NA.  This is an artifact of db program get data(...rsf_pfcbl.familytree.nomatch=NA) 
      #(versus rsf_pfcbl.familytree.nomatch=NULL which would omit in the get data function instead)
      #calc_data <- calc_data[!is.na(rsf_pfcbl_id)]
      
      
      
      
  
      #2023:
      #Deprecated lists in calculations.  Zero uses cases presently and adds much complexity.
      #That said, there could be a use case in the future: it's more about ensuring the user-defined formula is written to manage list objects?
      #Could also convert it to a string here and throw a warning?
      has_lists <- any(sapply(calc_data,class)=="list")
      if (has_lists) {
        stop("List objects are no longer allowed in calculations.  Any .all or .intraperiod data types should not be allowed in indicator formulas")
      }
      
      calc_data[,row_id:=1:.N]
      
      {    
        grouping_id <- NULL
        
        #Grouping will be: NA (not grouped), rsf_X_id (ie, by pfcbl category), or by row_id (also not grouped, but a de-facto looped calculation)
        if (!is.na(calculation$formula_grouping_rsf_id)) {
          grouping_id <- calculation$formula_grouping_rsf_id
          if (!any(grouping_id==names(calc_data))) {
            stop(paste0("grouping_id '",grouping_id,"' is not present in calc_data columns"))
          }
        }
        
        if (!is.na(calculation$perform_calculation_by_row) && calculation$perform_calculation_by_row==TRUE) {
          grouping_id <- "row_id"
        }

        if (is.na(calculation$formula_grouping_rsf_id) && 
            calculation$perform_calculation_by_row==FALSE &&
            any(grepl("(?<!p)sum\\(|(?<!p)mean\\(|count\\(|all\\(|any\\(|(?<!p)max\\(|(?<!p)min\\(|length\\(",calculation$formula,perl=T))) {
          stop("Formula uses aggregate function but formula grouping is undefined for:\n",
               " {",calculation$indicator_name,"}\n",
               " Calculating: {",calculation$formula,"}\n",
               " Perhaps rewrite using, for example, using row-level formulas: add(), average(), least(), greatest(); or, eg, sum(x,y) as if.missing(x,0) + if.missing(y,0); or explicitly cast a grouping using a formula_sort value?")
        }
        
        grouping_cols <- c("reporting_current_date",grouping_id)
      }
      
      #NOTE: we are NOT filtering calc_data for calculation$calculate_rsf_pfcbl_ids !
      #This is intentional and see note below, because some calculations may require rsf_pfcbl_id values for aggregated calculations where those IDs are not among
      #the required calculate_rsf_pfcbl_ids
      
      #However, this is only relevant for grouped calculations.  Therefore, we do want to filter-out irrelevant rsf_pfcbl_ids for non-grouped calculations
      #This simplifies that work done for calc_data.  This is also in response to observing that leaving these in can result in unnecessary fx conversions for
      #rsf_pfcbl_ids that are not part of the calculation, as those data points for non-calculated rsf_pfcbl_ids are converted into the current calculation$calculate_indicator_data_unit
      #which results in both unnecessary work and also very confusing flags about totally irrelevant currencies being converted.

      if (is.null(grouping_id) || is.na(grouping_id) || grouping_id=="row_id") {
        calc_data <- calc_data[rsf_pfcbl_id %in% calculate_for_rsf_pfcbl_ids]
      } 
      
      calc_data[,
                `:=`(grouping=.GRP,
                     grouping_count=.N),
                by=c(grouping_cols)]
      
      grouping_cols <- c("reporting_current_date","grouping","rsf_pfcbl_id")
      if (!is.na(calculation$formula_grouping_pfcbl_rank)) {
        #Column names that are equal-to or parent-level from the current grouping-level
        #These columns will be passed to the data.table by() clause and ensure that any aggregate functions will see them only onces and not count/sum repeated rows
        grouped_parameters <- parameters[rsf_indicators[indicator_pfcbl_rank <= calculation$formula_grouping_pfcbl_rank,
                                                        .(parameter_indicator_id =indicator_id)],
                                         on=.(parameter_indicator_id ),
                                         nomatch=NULL,
                                         parameter_column_name]
        #Should be true, but just in case
        grouped_parameters <- grouped_parameters[grouped_parameters %in% names(calc_data)]
        grouping_cols <- c(grouping_cols,grouped_parameters)
        
        #If true, we cannot group by rsf_pfcbl_id because the rsf_pfcbl_id is a CHILD-LEVEL hierarchy and it would negate the specific setting to group-at a parent-level
        #(but apply at child-level)
        if (calculation$formula_grouping_pfcbl_rank < rsf_indicators[indicator_id==calculation$calculate_indicator_id,indicator_pfcbl_rank]) {
          grouping_cols <- grouping_cols[-which(grouping_cols=="rsf_pfcbl_id")]
        }
      }
    }    
    
    #Currency conversions!
    {
      
      if (!is.na(calculation$calculate_indicator_currency_unit)) {
        
        #calculation$entity_local_currency_unit
        update_fx_table_function <- function(cache_fx) {
          if (!empty(cache_fx)) {
            setcolorder(cache_fx,
                        neworder = names(fx_table))
            
            fx_table <<- rbindlist(list(fx_table,
                                        cache_fx))
          }
        }
        
        add_data_flag_function <- function(rsf_pfcbl_id,
                                           check_name,
                                           check_message) {
          add_data_flag(rsf_pfcbl_id=rsf_pfcbl_id,
                        indicator_id=calculation$calculate_indicator_id,
                        check_name=check_name,
                        check_message=check_message,
                        formula_id=calculation$formula_id)
        }
        
        add_fx_conversions_function <- function(report_conversions) {
          report_conversions[,
                             `:=`(indicator_id=calculation$calculate_indicator_id,
                                  reporting_asof_date=reporting_current_date)]
          setcolorder(report_conversions,
                      neworder=c("rsf_pfcbl_id",
                                 "indicator_id",
                                 "reporting_asof_date",
                                 "exchange_rate_data_id"))
          
          fx_conversions <<- rbindlist(list(fx_conversions,
                                           report_conversions))
        }
        
        calc_data <- rsf_computation_fx_conversion(pool=pool,
                                                   computation=calculation,
                                                   comp_data=calc_data,
                                                   computation_asof_date=calculation$calculate_asof_date,
                                                   fx_table=fx_table,
                                                   update_fx_table_function=update_fx_table_function, 
                                                   add_data_flag_function=add_data_flag_function, #we don't flag the flags
                                                   add_fx_conversions_function=add_fx_conversions_function) #if fx rates change we don't redo checks
      }
      
      
      if (calculation$data_type=="currency_ratio") {
        #The resulting data_unit of the calculation is defined by calculation$calculate_indicator_data_unit and specified below.
        #This comes from the definition of the indicator itself.  HOWEVER, for currency_ratios that are defined relative to LCU,
        #eg, USD/LCU or EUR/LCU, when the pending calculation is queried, the LCU value will be interpreted to the entity's LCU definition
        #and so the calculate_indicator_data_unit may be USD/NGN or EUR/NGN.
        #BUT the actual data unit saved in the database will have already been interprted according to the entity's LCU defintion and then saved
        #according to the alphabetic order of the currency pairs.  And so these examples would read a parameter-defined data unit as:
        #NGN/USD or EUR/NGN, depending on the relative currencies of the requested ratio and the alphabetic order of the LCU value.
        #So in this case, the indicator's definition would specify calculation$calculate_indicator_data_unit=USD/NGN and the parameter's data point
        #would be NGN/USD.  The parameter SHOULD drive the fx calculation request, and so the result of the calculation would be, say, 0.0123 NGN/USD
        #And then later, the data_unit of the calculation is set using calculation$calculate_indicator_data_unit=USD/NGN that would incorrectly 
        #invert these units.  
        #Therefore, we now check for possible discrepancies and then we set the calculation$calculate_indicator_data_unit equal to the parameter's value
        #that will drive the actual results.
      
        #there SHOULD be only one (and exactly one)
        fx_unit_indicators <- parameters[grepl("unit$",parameter_variable),
                                         parameter_column_name]
        
        fx_calculation_unit <- NULL
        #FX units are hard-coded, not as .unit parameters
        if (length(fx_unit_indicators)==0) {
          fx_calculation_unit <- unique(unlist(str_extract_all(string=calculation$formula,
                                                              pattern="[A-Z]{3}/[A-Z]{3}")))
        } else { #Not hard-coded, so extract .unit parameters
          fx_calculation_unit <- unique(unlist(calc_data[,..fx_unit_indicators]))
          
          #A LCY FX calculation has been triggered.  But the entity hasn't reported (yet) on this indicator!
          #Maybe it never will but is accidentally subscribed to it?
          #So there isn't an existing .unit value to use since it's not reported.
          #But so we can use the calculation$calculate_indicator_data_unit since we know that the output of the calculation must equal that unit.
          if (anyNA(fx_calculation_unit)) {
            
            #So all entities needing to compute have never reported and therefore never reported its .unit
            if (all(is.na(fx_calculation_unit))) {
              #Shouldn't be any need for a loop, as why would someone make a function with multiple .units?  But just in case...
              fx_calculation_unit <- calculation$calculate_indicator_data_unit
              for (fxi in fx_unit_indicators) set(calc_data,
                                                  i=NULL,
                                                  j=fxi,
                                                  value=calculation$calculate_indicator_data_unit)
            #This calculation group has some entities that reported .unit and some that never reported.              
            
            } else {
              fx_calculation_unit <- na.omit(unique(fx_calculation_unit))
              #Uniform units aside from the NA value.  If not, we'll fail below.
              if (length(fx_calculation_unit)==1) {
                for (fxi in fx_unit_indicators) {
                  whichNA <- which(is.na(calc_data[[fxi]]))
                  set(calc_data,
                      i=whichNA,
                      j=fxi,
                      value=fx_calculation_unit)
                }
              }
            }
          }
        }
        if (is.na(fx_calculation_unit)) {
          fx_calculation_unit <- calculation$calculate_indicator_data_unit
        }
        if (length(fx_calculation_unit) != 1) {

          stop(paste0("Failed to calculate: ",calculation$indicator_name," because currency_ratios expect one (and only one) '.unit' parameter OR a hard-coded value. ",
                      "This formula requested: ",
                      ifelse(length(fx_calculation_unit)==0,"NOTHING",
                             paste0(fx_calculation_unit,collapse=" AND ALSO "))))
        
        } else if (fx_calculation_unit != calculation$calculate_indicator_data_unit) {
          calculation$calculate_indicator_data_unit <- fx_calculation_unit
        }
      }
    }
    
  
    setkeyv(calc_data,
            cols=grouping_cols)
    
    calc_results <- NULL
    
    t2 <- Sys.time()
    
    calc_results <- tryCatch({
      
      #to ensure that any formulas that return NA don't resolve inconsistent classes.
      #Note: adding indicator_name as a classes NA_ type then is then written as a known class may be better than as.X({ }) 
      #This also prevents user defined functions from also having unnamed execution functions { } (but maybe that's a good thing?)
      {
        calculation_formula <- NULL
        if (calculation$data_type %in% c("number","currency","currency_ratio","percent","date")) calculation_formula <- paste0("as.numeric({ ",calculation$formula," })") #dates are resolved to numeric regardless, but converted back below to date
        else if (calculation$data_type == "logical") calculation_formula <- paste0("as.logical({ ",calculation$formula," })")
        else calculation$data_type <- calculation_formula <- paste0("as.character({ ",calculation$formula," })")
        
        calculation_formula_sort <- calculation$formula_sort
      }      
      #Ensure a closed environment that won't expose potentially malicious user-defined functions to db_ function groups or other session$ info
      calc_env <- new_environment(data=list(indicator_name=calculation$indicator_name,
                                            calculation_formula=calculation_formula,
                                            calculation_formula_sort=calculation_formula_sort,
                                            grouping_cols=grouping_cols,
                                            calc_data=calc_data),
                                  parent=CALCULATIONS_ENVIRONMENT)
      
      
      do_calculations <- with(calc_env,
      {
        
        if (!is.na(calculation_formula_sort)) {
          #NOTE: no because some formula_sorts are themselves computed, eg sort=rsf_client_id,pmax(loan_contract_commitment_date.current,loan_contract_renewal_date.current,na.rm=T)
          #setorderv(calc_data,cols=strsplit(calculation_formula_sort,",")[[1]]) 
          has_data <- nrow(calc_data) > 0
          calc_data <- calc_data[eval(parse(text=paste0("order(",calculation_formula_sort,")")))]
          if (has_data && nrow(calc_data)==0) stop(paste0("calculation_formula_sort = {",calculation_formula_sort,"} caused an empty dataset"))
        }
        
        do_calc <- NULL
        if (any(grouping_cols=="rsf_pfcbl_id")) {
          do_calc <- calc_data[,
                               .(data_value=eval(parse(text=calculation_formula))),
                               by=grouping_cols]
        } else {
          do_calc <- calc_data[,
                               .(rsf_pfcbl_id,
                                 data_value=eval(parse(text=calculation_formula))),
                               by=grouping_cols]
        }
        do_calc <- do_calc[,
                           .(rsf_pfcbl_id,
                             reporting_current_date,
                             grouping,
                             data_value)]
        
        do_calc <- unique(do_calc,
                          by=c("rsf_pfcbl_id","data_value")) #formulas that mix rsf categories may have identical redundancies even with agg functions
                                                             #eg "sum(loan_outstanding_balance.current[loan_overdue_days.current >= 1],na.rm=TRUE) /  client_portfolio_outstanding_balance.current"
        do_calc
        
      })
      ##
      ##grouping_cols <- "reporting_current_date"
      ##grouping_cols <- c("reporting_current_date","row_id")
      ##test <- as.data.table(as.data.frame(do_calculations))
      ##
      do_calculations
    },
    warning = function(w) { 
      print(conditionMessage(w))
      status_message(class="warning",paste0("\n",calculation$indicator_name,": ",conditionMessage(w),"\n"))
      add_data_flag(rsf_pfcbl_id = calc_data$rsf_pfcbl_id,
                    indicator_id=calculation$calculate_indicator_id,
                    check_name="sys_calculator_failed",
                    check_message=paste0(calculation$indicator_name,": ",conditionMessage(w)),
                    formula_id=calculation$formula_id)
      
      do_calc <- unique(calc_data[,.(data_value=as.character(NA)),
                                  by=c(grouping_cols)][,
                                                       .(rsf_pfcbl_id,
                                                         reporting_current_date,
                                                         grouping,
                                                         data_value)])
      do_calc
    },
    error = function(err) { 
    
      print(conditionMessage(err))
      status_message(class="error",paste0("\n",calculation$indicator_name,": ",conditionMessage(err),"\n"))
      add_data_flag(rsf_pfcbl_id = calc_data$rsf_pfcbl_id,
                    indicator_id=calculation$calculate_indicator_id,
                    check_name="sys_calculator_failed",
                    check_message=paste0(calculation$indicator_name,": ",conditionMessage(err)),
                    formula_id=calculation$formula_id)
      
      do_calc <- unique(calc_data[,.(data_value=as.character(NA)),
                                  by=c(grouping_cols)][,
                                                       .(rsf_pfcbl_id,
                                                         reporting_current_date,
                                                         grouping,
                                                         data_value)])
      do_calc
    })
    
    if(SYS_PRINT_TIMING) debugtime("rsf_indicators_calculate",calculation$indicator_name," calculation time=",format((Sys.time()-t2)))
    
    #Results filter
    #Note: results filter is different than calculations filter in situations where calculations require aggregate of all children entities within 
    #the grouping level; and the indicator is applied at the children level. "loan_facility_inclusion_rank" as an example 
    #Eg, if we're uploading a single data point change at the loan-level -- maybe adding one new renewal, for example --
    #then we require all client-level data for that loan to aggregate at the client-level, to calculate the value for the one loan rsf_pfcbl_id
    #that's requested in the calculation.  However, we CANNOT exclude all the rsf_pfcbl_id loans before because the aggregate requires them to 
    #be present.  Otherwise the loan's inclusion rank would always be #1 as it would always be the only non-excluded loan
    calc_results <- calc_results[rsf_pfcbl_id %in% calculate_for_rsf_pfcbl_ids]
    
    
    calc_results[,n:=.N,by=.(reporting_current_date,rsf_pfcbl_id)]
    if (!empty(calc_results[n>1])) {
      status_message(class="error",
                     paste0("\n",calculation$indicator_name," formula '",
                            calculation_formula,"' results in duplicated rows.  Perhaps formula intends apply an aggregating function, like sum()?  Skipping.\n"))
      
      add_data_flag(rsf_pfcbl_id = calc_results$rsf_pfcbl_id,
                    indicator_id=calculation$calculate_indicator_id,
                    check_name="sys_calculator_failed",
                    check_message=paste0(calculation$indicator_name," formula {",
                                         calculation_formula,
                                         "} results in duplicated rows.  Perhaps formula intends apply an aggregating function, like sum()?  Correct bugs in formula definition in Admin Panel."),
                    formula_id=calculation$formula_id)
      
      calc_results <- calc_results[n==1];
      calc_results[,data_value:=NA]
    }
    
    
    calc_results[,`:=`(indicator_id=calculation$calculate_indicator_id,
                       data_unit=calculation$calculate_indicator_data_unit)]
    
    #IMPORTANT
    #When currency data is submitted to database rsf_data table and the currency unit is defined, and the indicators table default data_unit is LCU
    #then the trigger for rsf_data_timeseries will NOT insert the data_unit_data_id value to manage that data point's currency_unit according to the 
    #determined entity local_curreny_unit and INSTEAD will assume that the value in rsf_data is intentional and correct and that a user upload has
    #intentionally defined this data point in a different currency
    #And therefore, if the overall entity local_currency_unit were to change, this data point's currency unit would NOT change (because its been
    #defined otherwise).  The calculations output has calculated the calculate_inidcator_data_unit and would submit this defined data unit
    #as part of the calculation results into the database. This would make it appear that the data value is being set in a defined currency unit.
    #However, it is really just the result of interpreting the LCU value and calculating it accordingly.  So now we set its data unit back to LCU
    #since it should continue to be managed and interpreted as an LCU result and not as an intentionally defined currency.
    #NOTE:
    #2022-08-16 this LCU reversion is moved to rsf_program_perform_calculations() just before upload to better check changes in user-uploaded and calculated units
    # if (!is.na(calculation_currency_is_entity_LCU) && calculation_currency_is_entity_LCU==TRUE) {
    #   calc_results[,data_unit:="LCU"]
    # }
    
    setnames(calc_results,
             old="reporting_current_date",
             new="reporting_asof_date")

    calc_results[,
                 formula_id:=calculation$formula_id]
    
    calc_results <- calc_results[,
                                 .(rsf_pfcbl_id,
                                   indicator_id,
                                   reporting_asof_date,
                                   data_value,
                                   data_unit,
                                   formula_id)]
    
    #min, max, etc and other formulas resolve Date input date to numeric.  Convert back into a date to convert into a string, else data info is lost.
    if (calculation$data_type=="date") calc_results[,data_value:=as.Date(data_value,origin="1970-01-01")]
    #calc_results[,data_submitted:=as.character(data_value)]
    
    nans_or_infs <- is.nan(calc_results$data_value) | is.infinite(calc_results$data_value)
    if (any(nans_or_infs)) calc_results[nans_or_infs==TRUE,data_value:=NA]
    
    calc_results[,data_value:=as.character(data_value)]

    #removed defaults for calculations: calculations can be responsible for their own default values if the result is NA
    #and for failed calculations, then the result is very reasonably NA and not an actual default value
    # if (!is.na(calculation$default_value) && nrow(calc_results[is.na(data_value)==TRUE]) >0) {
    #   calc_results[is.na(data_value)==TRUE,
    #                data_value:=as.character(calculation$default_value)]
    # }
    
    
    results_list[[length(results_list)+1]] <- calc_results
    
    calc_time <- format(round(Sys.time() - calc_time,2))
    #fx indicators take forever because of the Dremio connection
    if (calc_time > 1.5 & grepl("sys_global_fx",calculation$indicator_name)==FALSE) {
      status_message(class="warning",",\n",calculation$indicator_name," took ",calc_time,"s to calculate\n")
    }
    
    if(SYS_PRINT_TIMING & calc_time > 0.25 & grepl("sys_global_fx",calculation$indicator_name)==FALSE)  {

      debugtime("rsf_indicators_calculate",
                "calculaton time for '",calculation$indicator_name," #",
                calculation$calculate_indicator_id,": ",calc_time," formula=",
                calculation_formula,"/ grouping: ",paste0(grouping_cols,collapse=","))

    }
  }

  calculated_data <- rbindlist(results_list)
  if (empty(calculated_data)) {
    calculated_data <- data.table(rsf_pfcbl_id=numeric(0),
                                  indicator_id=numeric(0),
                                  reporting_asof_date=as.Date(numeric(0)),
                                  data_value=character(0),
                                  data_unit=character(0),
                                  formula_id=numeric(0))
  }
  
  check_flags <- unique(rbindlist(data_flags_list)) #it's possible to get redundant entries if flags are added before filter_rsf_pfcbl_ids_results
  
  calculated_data[,data_flags_new:=list()]
  if (!empty(check_flags)) {
    
    check_flags[grepl("^.*object\\s+'.*'\\s+not found$",check_message),
                check_message:=gsub("^(.*)object(\\s+'.*'\\s+not found)$","\\1parameter\\2. Verify parameter exists, check spelling; names are case-sensitive.",check_message)]
    
    check_flags <- check_flags[,.(data_flags_dt=list(.SD)),
                                 by=.(rsf_pfcbl_id,
                                      indicator_id,
                                      formula_id),
                                 .SDcols=c("check_name",
                                           "check_message")]

   
    calculated_data[check_flags,
                    data_flags_new:=i.data_flags_dt,
                    on=.(rsf_pfcbl_id,
                         indicator_id,
                         formula_id)]

    #if nothing calculated at all, such as removed from calc_data due to missing IDs or other total calculation failure,
    #re-join the failed messages back onto calculated data to obtain a list of non-results to attach the flag and report
    #back to process that it has been "calculated" even if no valid result is returned, it will return a NA result with a flag and mark as calculated
    failed_calcs <- calculated_data[,.(rsf_pfcbl_id,
                                       indicator_id,
                                       reporting_asof_date,
                                       formula_id)
                                    ][check_flags,
                                      on=.(rsf_pfcbl_id,
                                           indicator_id,
                                           formula_id)
                                      ][is.na(reporting_asof_date)] #is.na(reporting_asof_date) is the key filter
    
    if (!empty(failed_calcs)) {
      failed_calcs[,`:=`(data_value=as.character(NA),
                         data_unit=as.character(NA))]
      
      #Even if the failed to calculate the formula, we can resolve that it "failed" in the correct units, not NA units -- since NA will be posted to the database, which controls unit validity.
      failed_calcs[calculations[!is.na(calculate_indicator_data_unit)],
                   data_unit:=i.calculate_indicator_data_unit,
                   on=.(formula_id)]
      
      failed_calcs[,reporting_asof_date:=unique(rsf_data_wide$reporting_current_date)]
      
      
      calculated_data <- rbindlist(list(calculated_data,
                                         failed_calcs[,.(rsf_pfcbl_id,
                                                         indicator_id,
                                                         reporting_asof_date,
                                                         data_value,
                                                         data_unit,
                                                         formula_id,
                                                         data_flags_new=data_flags_dt)]))
    }
    
    
  }
  
  #Because one calculation might have multiple fx inputs (if there are multiple currencies that go into an aggregate, for example)
  fx_conversions <- fx_conversions[,
                                   .(fx_data_ids_str=paste0(sort(exchange_rate_data_id),collapse="-"),
                                     fx_data_ids=list(exchange_rate_data_id)),
                                   by=.(rsf_pfcbl_id,
                                        indicator_id,
                                        reporting_asof_date)]
  
  calculated_data[fx_conversions,
                  `:=`(fx_data_ids=i.fx_data_ids),
                  on=.(rsf_pfcbl_id,
                       indicator_id,
                       reporting_asof_date)]
  
  return (calculated_data)
}
