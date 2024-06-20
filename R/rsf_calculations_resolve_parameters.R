rsf_calculations_resolve_parameters <- function(calculations,
                                                rsf_indicators) {
    
  request_indicator_variables <- rbindlist(list(
                                            calculations[,
                                              .(formula_variable=unlist(indicatorNames_extractIndicatorVariables(formula),
                                                                           recursive=TRUE),
                                                   for_calculation=TRUE,
                                                   for_sort=FALSE),
                                              by=.(calculate_indicator_id,
                                                   calculation_group)][is.na(formula_variable)==FALSE],
  
                                            calculations[,
                                             .(formula_variable=unlist(indicatorNames_extractIndicatorVariables(formula_sort),
                                                                       recursive=TRUE),
                                               for_calculation=FALSE,
                                               for_sort=TRUE),
                                             by=.(calculate_indicator_id,
                                                  calculation_group)][is.na(formula_variable)==FALSE]))
  
  request_indicator_variables <- request_indicator_variables[,
                                                             .(for_calculation=any(for_calculation),
                                                               for_sort=any(for_sort)),
                                                             by=.(calculate_indicator_id,
                                                                  calculation_group,
                                                                  formula_variable)]
  
  request_indicator_variables[,parameter_indicator_name:=indicatorNames_getBasenames(formula_variable)]
  request_indicator_variables[,parameter_variable:=indicatorNames_getAttributes(formula_variable)]

  request_indicator_variables[rsf_indicators,
                              `:=`(parameter_indicator_id=i.indicator_id,
                                   parameter_data_type=i.data_type),
                              on=.(parameter_indicator_name=indicator_name)]
  
  #Can occur if a formula definition requests a non-existent indicator.  Which can happen if formula simply has mistakes, or if an indicator is renamed
  #or formula_indicator_formula specification has those issues
  if (anyNA(request_indicator_variables$parameter_indicator_id)) {
    #no warning etc because the calculator will throw errors
    bad_names <- request_indicator_variables[is.na(parameter_indicator_id) & !is.na(parameter_indicator_name)]
    if (!empty(bad_names)) {
      bad_names[rsf_indicators,
                for_indicator_name:=indicator_name,
                on=.(calculate_indicator_id=indicator_id)]
      bad_names <- bad_names[,
                message:=paste0("Calculation: '",for_indicator_name,"' cannot find parameter ",paste0(paste0("'",formula_variable,"'"),collapse=" AND ALSO "),".  Does ",paste0(paste0("'",parameter_indicator_name,"'"),collapse=" AND ALSO ")," exist? Did this indicator change its name? If yes, fix the formula definition for this calculation in Jason Admin Panel."),
                by=.(for_indicator_name)]
      
      stop(paste0(bad_names$message,collapse=" \n"))
    }
    request_indicator_variables <- request_indicator_variables[!is.na(parameter_indicator_id)]
  }

  request_indicator_variables[,
                              for_fx:=FALSE]
  
  
  #For fx and currency conversions (which are not necessarily needed for the calculator to calculate it)
  {
    #for FX we want to add .unit and .reporteddate to all currency data types
    fx_variable_requirements <- request_indicator_variables[parameter_data_type=="currency" &
                                                            grepl("(current|previous|first)$",parameter_variable)]
    
    if (!empty(fx_variable_requirements)) {
      
      #Because we want to recalculate the parameter's fx date as of the latest PARAMETER update (of any varaible type, not just if current values are)
      fx_unit_variable_requirements <- fx_variable_requirements[,
                                                           .(parameter_variable=paste0(parameter_variable,".unit"),
                                                             formula_variable=paste0(formula_variable,".unit"),
                                                             for_calculation=any(for_calculation),
                                                             for_sort=any(for_sort),
                                                             for_fx=TRUE),
                                                           by=.(calculate_indicator_id,
                                                                calculation_group,
                                                                parameter_indicator_name,
                                                                parameter_indicator_id,
                                                                parameter_data_type)]
      
      # fx_variable_requirements <- fx_variable_requirements[parameter_variable %in% c("current",
      #                                                                                "first",
      #                                                                                "previous"),
      #                                                      .(parameter_variable=paste0(parameter_variable,".unit"),
      #                                                        formula_variable=paste0(formula_variable,".unit"),
      #                                                        for_calculation=any(for_calculation),
      #                                                        for_sort=any(for_sort),
      #                                                        for_fx=TRUE),
      #                                                      by=.(calculate_indicator_id,
      #                                                           calculation_group,
      #                                                           parameter_indicator_name,
      #                                                           parameter_indicator_id,
      #                                                           parameter_data_type)]

      fx_date_variable_requirements <- fx_variable_requirements[,
                                                                   .(parameter_variable=paste0(parameter_variable,".reporteddate"),
                                                                     formula_variable=paste0(formula_variable,".reporteddate"),
                                                                     for_calculation=any(for_calculation),
                                                                     for_sort=any(for_sort),
                                                                     for_fx=TRUE),
                                                                   by=.(calculate_indicator_id,
                                                                        calculation_group,
                                                                        parameter_indicator_name,
                                                                        parameter_indicator_id,
                                                                        parameter_data_type)]   
      
      # fx_date_variable_requirements <- request_indicator_variables[parameter_variable %in% c("current",
      #                                                                                        "first",
      #                                                                                        "previous"),
      #                                                              .(parameter_variable=paste0(parameter_variable,".reporteddate"),
      #                                                                formula_variable=paste0(formula_variable,".reporteddate"),
      #                                                                for_calculation=any(for_calculation),
      #                                                                for_sort=any(for_sort),
      #                                                                for_fx=TRUE),
      #                                                              by=.(calculate_indicator_id,
      #                                                                   calculation_group,
      #                                                                   parameter_indicator_name,
      #                                                                   parameter_indicator_id,
      #                                                                   parameter_data_type)]
      
      setcolorder(fx_unit_variable_requirements,
                  neworder=names(request_indicator_variables))
      
      setcolorder(fx_date_variable_requirements,
                  neworder=names(request_indicator_variables))
      
      request_indicator_variables <- rbindlist(list(request_indicator_variables,
                                                    fx_unit_variable_requirements,
                                                    fx_date_variable_requirements))
      request_indicator_variables <- unique(request_indicator_variables)
    }
    
    request_indicator_variables <- request_indicator_variables[,
                                                               .(for_calculation=any(for_calculation),
                                                                 for_sort=any(for_sort),
                                                                 for_fx=any(for_fx)),
                                                               by=.(calculate_indicator_id,
                                                                    calculation_group,
                                                                    parameter_indicator_name,
                                                                    parameter_variable,
                                                                    parameter_indicator_id,
                                                                    parameter_data_type)]
    
    request_indicator_variables[,parameter_column_name:=paste0(parameter_indicator_name,".",parameter_variable)]
  }
  
  
  calculate_indicator_variables <- request_indicator_variables[,
                                                               .(parameters_dt=list(.SD)),
                                                               by=.(calculate_indicator_id,
                                                                    calculation_group)]
  
  calculations[calculate_indicator_variables,
               parameters_dt:=i.parameters_dt,
               on=.(calculate_indicator_id,
                    calculation_group)]
  
  return (calculations)
}