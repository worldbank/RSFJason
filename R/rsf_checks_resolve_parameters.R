rsf_checks_resolve_parameters <- function(checks,
                                          rsf_indicators) {
  
    
  request_indicator_variables <- rbindlist(list(
                                            checks[,
                                              .(formula_variable=unlist(indicatorNames_extractIndicatorVariables(formula),
                                                                        recursive=TRUE),
                                                 for_calculation=TRUE,
                                                 for_message=FALSE,
                                                 for_subgrouping=FALSE),
                                              by=.(check_formula_id
                                                   )][is.na(formula_variable)==FALSE],
  
                                            checks[,
                                             .(formula_variable=unlist(indicatorNames_extractIndicatorVariables(formula_result_message),
                                                                       recursive=TRUE),
                                               for_calculation=FALSE,
                                               for_message=TRUE,
                                               for_subgrouping=FALSE),
                                             by=.(check_formula_id
                                                  #check_grouping
                                                  )][is.na(formula_variable)==FALSE],
                                            
                                            checks[,
                                                   .(formula_variable=unlist(indicatorNames_extractIndicatorVariables(subgrouping),
                                                                             recursive=TRUE),
                                                     for_calculation=FALSE,
                                                     for_message=FALSE,
                                                     for_subgrouping=TRUE),
                                                   by=.(check_formula_id
                                                        #check_grouping
                                                        )][is.na(formula_variable)==FALSE]))
  
  request_indicator_variables <- request_indicator_variables[,
                                                             .(for_calculation=any(for_calculation),
                                                               for_message=any(for_message),
                                                               for_subgrouping=any(for_subgrouping)),
                                                             by=.(check_formula_id,
                                                                  #check_grouping,
                                                                  formula_variable)]
  
  request_indicator_variables[,parameter_indicator_name:=indicatorNames_getBasenames(formula_variable)]
  request_indicator_variables[,parameter_variable:=indicatorNames_getAttributes(formula_variable)]
  request_indicator_variables[,formula_variable:=NULL]
  
  request_indicator_variables[rsf_indicators,
                              `:=`(parameter_indicator_id=i.indicator_id,
                                   parameter_data_type=i.data_type,
                                   parameter_data_category=i.data_category),
                              on=.(parameter_indicator_name=indicator_name)]
  
  #Can occur if a formula definition requests a non-existent indicator.  Which can happen if formula simply has mistakes, or if an indicator is renamed
  #or formula_indicator_formula specification has those issues
  if (anyNA(request_indicator_variables$parameter_indicator_id)) {
    #no warning etc because the calculator will throw errors
    request_indicator_variables <- request_indicator_variables[!is.na(parameter_indicator_id)]
  }

  request_indicator_variables[,
                              for_fx:=FALSE]
  
  
  #For fx and currency conversions (which are not necessarily needed for the calculator to calculate it)
  {
    #for FX we want to add .unit and .reporteddate to all currency data types
    fx_variable_requirements <- request_indicator_variables[parameter_data_type=="currency" &
                                                            grepl("(current|previous|first)$",parameter_variable) &
                                                            # parameter_variable %in% c("current",
                                                            #                           "first",
                                                            #                           "previous") &
                                                              (for_calculation==TRUE | for_subgrouping==TRUE)]
    
    if (!empty(fx_variable_requirements)) {
      
      #Because we want to recalculate the parameter's fx date as of the latest PARAMETER update (of any varaible type, not just if current values are)
      fx_unit_variable_requirements <- fx_variable_requirements[,
                                                           .(parameter_variable=paste0(parameter_variable,".unit"),
                                                             for_calculation=any(for_calculation),
                                                             for_message=any(for_message),
                                                             for_subgrouping=any(for_subgrouping),
                                                             for_fx=TRUE),
                                                           by=.(check_formula_id,
                                                                parameter_indicator_name,
                                                                parameter_indicator_id,
                                                                parameter_data_type,
                                                                parameter_data_category)]
      
      fx_date_variable_requirements <- fx_variable_requirements[,
                                                                   .(parameter_variable=paste0(parameter_variable,".reporteddate"),
                                                                     for_calculation=any(for_calculation),
                                                                     for_message=any(for_message),
                                                                     for_subgrouping=any(for_subgrouping),
                                                                     for_fx=TRUE),
                                                                   by=.(check_formula_id,
                                                                        parameter_indicator_name,
                                                                        parameter_indicator_id,
                                                                        parameter_data_type,
                                                                        parameter_data_category)]
      
      setcolorder(fx_unit_variable_requirements,
                  neworder=names(request_indicator_variables))
      
      setcolorder(fx_date_variable_requirements,
                  neworder=names(request_indicator_variables))
      
      request_indicator_variables <- rbindlist(list(request_indicator_variables,
                                                    fx_unit_variable_requirements,
                                                    fx_date_variable_requirements))
    }
    
    request_indicator_variables <- request_indicator_variables[,
                                                               .(for_calculation=any(for_calculation),
                                                                 for_message=any(for_message),
                                                                 for_subgrouping=any(for_subgrouping),
                                                                 for_fx=any(for_fx)),
                                                               by=.(check_formula_id,
                                                                   # check_grouping,
                                                                    parameter_indicator_name,
                                                                    parameter_variable,
                                                                    parameter_indicator_id,
                                                                    parameter_data_type,
                                                                    parameter_data_category)]
    
    request_indicator_variables[,parameter_column_name:=paste0(parameter_indicator_name,".",parameter_variable)]
  }
  
  request_indicator_variables <- unique(request_indicator_variables)
  
  calculate_indicator_variables <- request_indicator_variables[,
                                                               .(parameters_dt=list(.SD)),
                                                               by=.(check_formula_id
                                                                    #check_grouping
                                                                    )]
  
  
  checks[calculate_indicator_variables,
               parameters_dt:=i.parameters_dt,
               on=.(check_formula_id
                    #check_grouping
                    )]
  
  return (checks)
}