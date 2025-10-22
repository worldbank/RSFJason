rsf_program_calculate <- function(pool,
                                  rsf_program_id,
                                  rsf_indicators,
                                  rsf_pfcbl_id.family=NULL,
                                  calculate_future=TRUE,
                                  reference_asof_date=NULL,
                                  status_message=function(...) {}) { 
  {
    t1 <- Sys.time()
    #created_calculations_cohorts <- list()
    status_message(class="none",paste0("Performing calculations...\n"))
    #We need a new reporting_cohort_id because everything user uploads should be in its own separate cohort
    #where as calculations are system and user should be able to view these separately.
    #unique constraint on database will deny recycling cohort_ids and enforce new cohorts on upload
    
    #source_reference <- paste0("System Calculator: Process Calculations for rsf_program_id=",rsf_program_id)
    #rsf_indicators <-db_indicators_get_labels(pool=pool)
    #rsf_pfcbl_id.family = 40159
    #using_reporting_cohort_id=NA

    if (all(is.na(reference_asof_date))) reference_asof_date <- NULL

    processed_calculations <- data.table(calculate_rsf_pfcbl_id=numeric(0),
                                         calculate_indicator_id=numeric(0),
                                         calculate_asof_date=as.Date(numeric(0)),
                                         processed_times=numeric(0))
    
    limit_date <- NULL
    if (calculate_future==FALSE &&
        is.null(reference_asof_date)==FALSE) {
      limit_date <- as.Date(reference_asof_date)
      status_message(class="warning",
                     "Setting calculate_future set to FALSE.  Skipping calculations after ",as.character(reference_asof_date),": update program setting to avoid this warning\n")
      
    }
    
    rsf_calculator_checks <- dbGetQuery(pool,
                                        "select 
                                            ic.check_name,
                                            ic.indicator_check_id,
                                            ic.variance_tolerance_allowed
                                          from p_rsf.indicator_checks ic
                                          where ic.is_calculator_check = true")
    
    setDT(rsf_calculator_checks)
    aggregate_calculation_flags <- NULL
    
    repeat {
      #Grouped by:
      #dce.rsf_pfcbl_id = 0 desc,
      #dce.calculation_asof_date asc,
      #ids.rsf_client_id,
      #indf.formula_calculation_rank asc
      #
        
        required_calculations <- db_program_get_stale_calculations(pool=pool,
                                                                   rsf_program_id=rsf_program_id,
                                                                   rsf_pfcbl_id.family = rsf_pfcbl_id.family,
                                                                   limit_future=limit_date)
        
        #Global program does not define a global-local currency unit and therefore is excluded from this check.
        
        
        #using_calculations_cohort_id <- using_reporting_cohort_id
        #any(required_calculations$calculate_indicator_id %in% c(157541))
        #if(any(required_calculations$calculate_indicator_id==157541)) { stop("Found!") }
        #required_calculations[rsf_indicators,on=.(calculate_indicator_id=indicator_id),nomatch=NULL][,.(indicator_name=sort(unique(indicator_name)))][indicator_name=="client_risk_sharing_fee_billing_period_DAYS"]
        current_calculate_asof_date <- unique(as.character(required_calculations$calculate_asof_date))

        if (empty(required_calculations)) break;
       
        {
          if(SYS_PRINT_TIMING) debugtime("rsf_program_calculate","performing ",format(nrow(required_calculations),big.mark=",")," calculations") 
          
          if (!all(required_calculations[calculate_rsf_pfcbl_id != 0]$entity_local_currency_unit %in% CALCULATIONS_ENVIRONMENT$VALID_CURRENCIES)) {
            bad_currencies <- required_calculations[!entity_local_currency_unit %in% CALCULATIONS_ENVIRONMENT$VALID_CURRENCIES]
            print(head(bad_currencies))
            stop(paste0("System calculator asked to calculate for entities with invalid currency units: ",
                        paste0(unique(bad_currencies$entity_local_currency_unit)),collapse=" and "))
          }
          
          #This can occur if we've had to CREATE a NEW Global FX ratio indicator and are now being prompted to calculated it: it will not be in the
          #cashed rsf_indicators data table and therefore the various joins used for parameters, etc will yield NA and cause problems.
          #So if an indicator isn't there--refresh it.
          if (any(!required_calculations$calculate_indicator_id %in% rsf_indicators$indicator_id)) {
            rsf_indicators <- db_indicators_get_labels(pool=pool)
          }
        }    
        
        #Create processing entry to avoid any infinite loops or excessive calculations due to errors that could come up.
        {
          required_calculations[,processed_times:=1]
          
          processed_calculations <- rbindlist(list(processed_calculations,
                                                   required_calculations[,.(calculate_rsf_pfcbl_id,
                                                                            calculate_indicator_id,
                                                                            calculate_asof_date,
                                                                            processed_times)]))
          
          processed_calculations <- processed_calculations[,
                                                           .(processed_times=sum(processed_times)),
                                                           by=.(calculate_rsf_pfcbl_id,
                                                                calculate_indicator_id,
                                                                calculate_asof_date)]
          
          if (any(processed_calculations$processed_times > 1))
          {
            repeat_calcs <- processed_calculations[processed_times > 1
                                                    ][required_calculations,
                                                      on=.(calculate_rsf_pfcbl_id,
                                                           calculate_indicator_id,
                                                           calculate_asof_date),
                                                      nomatch=NULL
                                                    ][rsf_indicators[,.(calculate_indicator_id=indicator_id,indicator_name)],
                                                      on=.(calculate_indicator_id),
                                                      nomatch=NULL]
            if (!empty(repeat_calcs)) {
              repeat_calcs <- unique(repeat_calcs[,.(indicator_name,calculate_asof_date,processed_times)])
              for (rc in 1:nrow(repeat_calcs)) {
                
                status_message(class="error",
                               paste0(repeat_calcs[rc,indicator_name],
                                      " has been re-calculated multiple times (x",repeat_calcs[rc,processed_times],")",
                                      " in ",
                                      as.character(repeat_calcs[rc,calculate_asof_date]),
                                      " for rsf_program_id=",rsf_program_id," and rsf_pfcbl_id.family=",rsf_pfcbl_id.family))
              }
            }
            
            if (any(processed_calculations$processed_times > 3)) {
              print(repeat_calcs)
              status_message(class="error",
                             "rsf_program_calculate appears to be in an infinite loop with re-calculations > 3.  Check if calculation verifications are occuring")
              status_message(class="warning","This erorr requires a system fix.  Try complete re-fresh for this client")

            }
            
            if (any(processed_calculations$processed_times > 5)) {
              stop("Calculator is in infinite loop.  See logs")
            }
          }
        }
        
        #perform calculations
        {
          #Note: if there's an indicator filter, then we might have more than the needed parmeters -- meaning more than necessary data may be pulled.
          #parameter_rsf_pfcbl_ids <- attr(required_calculations,"calculation_parameter_requirements")
          flags <- rsf_program_perform_calculations(pool=pool,
                                                    current_data=required_calculations,
                                                    rsf_indicators=rsf_indicators,
                                                    rsf_calculator_checks=rsf_calculator_checks,
                                                    perform.test = FALSE,
                                                    status_message=status_message)
          
          aggregate_calculation_flags <- rbindlist(list(aggregate_calculation_flags,
                                                        flags))
          flags <- NULL
        }
    }
    
    if (!empty(aggregate_calculation_flags)) {
      
      db_rsf_checks_add_update(pool=pool,
                               data_checks=aggregate_calculation_flags,
                               consolidation_threshold=NA)
      
    }
  }
  
  if(SYS_PRINT_TIMING) debugtime("rsf_program_calculate","Done!",format(Sys.time()-t1))
  return (processed_calculations)
}
