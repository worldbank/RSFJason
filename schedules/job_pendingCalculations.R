

do_pendingCalculations <- function(schedulePool,
                                   scheduleCalcEnv) {

  pending_calcs <- unlist(schedulePool %>% dbGetQuery("
  select exists(select true from p_rsf.rsf_data_calculation_evaluations)::bool as ending
  "))
  
  
  if(length(pending_calcs) && isTRUE(pending_calcs)) {
  
    stale <- dbGetQuery(schedulePool,"
      select 
        pen.rsf_pf_id,
        pen.calculation_asof_date,
        pen.calculations,
        sn.pfcbl_name,
        sn.pfcbl_category
      
      from (
        
        select 
        dce.rsf_pf_id,
        min(calculation_asof_date) as calculation_asof_date,
        count(*) as calculations
      from p_rsf.rsf_data_calculation_evaluations dce
     
      group by dce.rsf_pf_id
      ) as pen
      inner join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.rsf_pfcbl_id = pen.rsf_pf_id
      order by 
        (pen.rsf_pf_id = 0) desc,
        (sn.pfcbl_category = 'facility') desc,
        calculations asc")
    setDT(stale)
    
    
    if (!empty(stale)) {
      pending_calcs <- sum(as.numeric(stale$calculations))
      rsf_indicators <- db_indicators_get_labels(pool=schedulePool)
      
      for (i in 1:nrow(stale)) {
        cat(paste0("Calculating ",
                   stale[i,calculations],
                   " pending calculations for ",
                   stale[i,pfcbl_name],
                   " from ",as.character(stale[i,calculation_asof_date]),"\n"))

        tryCatch({
          
          msgs <- character()
          
          withCallingHandlers({
            rsf_program_calculate(pool=schedulePool,
                                  rsf_indicators=rsf_indicators,
                                  rsf_pf_id=stale[i,rsf_pf_id],
                                  for_import_id=NA,
                                  reference_asof_date=today(), #when are future (and therefore probably erroneous) calculations relevant? When we ask for them 
                                                               #only. Eg, no data upload will ever encounter these because data uploads are always in the present
                                                               #(unless uploading some future-defined term value--but even then, it would have no influence on anything present)
                                  status_message= function(...) { invisible(NULL) },
                                  CALCULATIONS_ENVIRONMENT = scheduleCalcEnv)
          },
          message = function(m) {
            msgs <<- c(msgs, conditionMessage(m))
            invokeRestart("muffleMessage")
          })
          
        },
        message = function(m) { invisible(NULL) },
        warning = function(w) { cat(paste0("WARNING: skipping ",stale[i,pfcbl_name],"\n",
                                            "WARNING: ",conditionMessage(w),"\n")) 
          },
        error = function(e)  { cat(paste0("WARNING: skipping ",stale[i,pfcbl_name],"\n",
                                           "WARNING: ",conditionMessage(e),"\n")) 
        })
      }
      
      #rsf program calculation should clean-up after itself quite nicely.
      #this is here because sometimes, rarely, a calculation can "get stuck" for evaluation such as if a user changes an indicator from a previously
      #defined calculation formula to decide it should be manually reported data with no calculation -- and then the calculator will pick-up the evaluation
      #and find there's no formula and not be able to compute it and leave it hanging.  This too should clean up after itself; but just in case as there's 
      #little overhead.
      
    }  
  }
  return (as.numeric(pending_calcs))
}
