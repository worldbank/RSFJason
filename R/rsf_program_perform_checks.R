rsf_program_perform_checks <- function(pool=pool,
                                       rsf_indicators,
                                       perform_checks,
                                       perform.test = FALSE,
                                       status_message=function(...) {}) {

  #if (nrow(perform_checks) != 1) { stop("Multiple perform checks not allowed") }
  # #if (!all(names(perform_checks) %in% c("indicator_check_id","for_indicator_id"))) stop("Perform checks expects a two-column data.table with indicator_check_id and for_indicator_id")
  check_asof_date <- unique(perform_checks$check_asof_date)
  if (length(check_asof_date) != 1) { stop("Multiple check dates not allowed") }
 

  # rsf_program_checks <- rbindlist(rsf_indicators$checks_for_indicator)
  # rsf_program_checks <- rsf_program_checks[is_system==FALSE] #This shouldn't happen anyway
  # rsf_program_checks <- rsf_program_checks[is_check_enabled==TRUE]
  

  check_formulas <- dbGetQuery(pool,"
                               select
                               ic.indicator_check_id,
                               ic.check_name,
                               icf.check_formula_id,
                               ic.is_system,
                               icf.check_pfcbl_category,
                               icf.parent_grouping_pfcbl_category as grouping,
                               icf.parent_grouping_pfcbl_rank as grouping_rank,
                               ic.subgrouping,
                               icf.formula,
                               icf.formula_result_message,
                               icf.formula_fx_date
                               from p_rsf.indicator_check_formulas icf
                               inner join p_rsf.indicator_checks ic on ic.indicator_check_id = icf.indicator_check_id
                               where icf.check_formula_id = any(select unnest(string_to_array($1::text,','))::int)
                               ",params=list(paste0(unique(perform_checks$check_formula_id),collapse=",")))
  setDT(check_formulas)
  
  checks <- perform_checks[,
                           .(check_rsf_pfcbl_ids,
                             check_asof_date,
                             entity_local_currency_unit,
                             check_formula_id,
                             current_evaluation_ids)
                          ][check_formulas,
                            on=.(check_formula_id),
                            nomatch=NULL
                            ][,
                              .(check_rsf_pfcbl_ids,
                                check_asof_date,
                                indicator_check_id,
                                entity_local_currency_unit,
                                check_formula_id,
                                check_pfcbl_category,
                                check_name,
                                grouping,
                                grouping_rank,
                                subgrouping,
                                formula,
                                formula_result_message,
                                formula_fx_date)]
  
 
   checks <- rsf_checks_resolve_parameters(checks=checks,
                                           rsf_indicators=rsf_indicators)
   
   checks[,calculate_indicator_currency_unit:=as.character(NA)]
   
   #If my output isn't currency, but I have any input variables that are currency,
   #then we may have an fx requirement if different input parameters have different units.  (eg, for a percentage calculation)
   #in which case, all input variables need to have a common currency for a ratio to be meaningful.
   checks[sapply(checks$parameters_dt,function(x) { any(x$parameter_data_type=="currency" &
                                                        grepl("\\.unit$",x$parameter_variable) & #we've requested or inserted units
                                                        (x$for_calculation==TRUE | x$for_subgrouping==TRUE)) #because don't want to fx for messages
                                                  }) == TRUE,
          calculate_indicator_currency_unit:=entity_local_currency_unit]
 
  #Resolve parameters and get data
  {
    
    request_indicator_variables <- rbindlist(checks$parameters_dt)[,.(parameter_indicator_id,parameter_variable)]
    request_indicator_variables <- unique(request_indicator_variables)
    
    request_indicator_variables <-request_indicator_variables[,
                                                              .(parameter_variables=paste0(parameter_variable,collapse=",")),
                                                              by=.(parameter_indicator_id)]
    
    request_indicator_variables <- setNames(request_indicator_variables$parameter_indicator_id,
                                            request_indicator_variables$parameter_variables)
    
    #can aggregate at this level and/or apply the formula result back-onto the 
    #entity that is requesting it.  For this reason, simplify.indicators = FALSE
    
    check_rsf_pfcbl_ids <- unique(unlist(checks$check_rsf_pfcbl_ids))
    
    
    data_rsf_pfcbl_ids <- db_checks_get_calculation_parameter_rsf_pfcbl_ids(pool=pool,
                                                                            check_rsf_pfcbl_ids=check_rsf_pfcbl_ids,
                                                                            check_formula_ids=unique(checks$check_formula_id),
                                                                            check_asof_date=check_asof_date)
    # parameter_rsf_pfcbl_ids <- dbGetQuery(pool,"
    #                                           select 
    #                                             distinct cids.to_parameter_rsf_pfcbl_id as rsf_pfcbl_id
    #                                           from p_rsf.compute_check_to_parameter_rsf_pfcbl_ids cids
    #                                           where cids.from_check_rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
    #                                             and cids.from_check_formula_id = any(select unnest(string_to_array($2::text,','))::int)
    #                                             and cids.parameter_rsf_pfcbl_id_created_date <= $3::date
    #                                             and cids.to_parameter_rsf_pfcbl_id <> cids.from_check_rsf_pfcbl_id
    #                                         ",
    #                                       params=list(paste0(check_rsf_pfcbl_ids,collapse=","),
    #                                                   paste0(unique(checks$check_formula_id),collapse=","),
    #                                                   as.character(check_asof_date)))
    
    
    #data_rsf_pfcbl_ids <- unique(c(check_rsf_pfcbl_ids,parameter_rsf_pfcbl_ids$rsf_pfcbl_id))

    for_pfcbl_categories <- unique(c(checks$check_pfcbl_category,
                                     checks$grouping))
    
    rsf_data_wide <- db_program_get_data(pool=pool,
                                         reporting_current_date=check_asof_date,
                                         indicator_variables=request_indicator_variables,
                                         for_rsf_pfcbl_ids=data_rsf_pfcbl_ids,
                                         for_pfcbl_categories = for_pfcbl_categories,
                                         rsf_indicators=rsf_indicators)
  }
  #lobstr::obj_size(rsf_data_wide)
  computed_checks <- rsf_checks_calculate(pool=pool,
                                          rsf_indicators = rsf_indicators,
                                          rsf_data_wide=rsf_data_wide,
                                          checks=checks,
                                          keep_false_flags = TRUE,
                                          status_message = status_message)
  
  if (perform.test==TRUE) {
    return (list(input=rsf_data_wide,
                 results=computed_checks))
  }
  
  
  return (computed_checks)
}