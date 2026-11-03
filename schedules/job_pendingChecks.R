

do_pendingChecks <- function(schedulePool,
                             scheduleCalcEnv) {

  pending_checks <- unlist(schedulePool %>% dbGetQuery("
  select exists(select true from p_rsf.rsf_data_check_evaluations)::bool as ending
  "))
  
  message(paste0("Identified pending checks\n"))
  
  if(length(pending_checks) && isTRUE(pending_checks)) {
  
    stale <- dbGetQuery(schedulePool,"
      select 
        pen.rsf_pf_id,
        pen.check_asof_date,
        pen.checks,
        sn.pfcbl_name,
        sn.pfcbl_category
      
      from (
        
        select 
        dce.rsf_pf_id,
        min(check_asof_date) as check_asof_date,
        count(*) as checks
      from p_rsf.rsf_data_check_evaluations dce
     
      group by dce.rsf_pf_id
      ) as pen
      inner join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.rsf_pfcbl_id = pen.rsf_pf_id
      order by 
        (pen.rsf_pf_id = 0) desc,
        (sn.pfcbl_category = 'facility') desc,
        checks asc")
    setDT(stale)
    
    
    if (!empty(stale)) {
      pending_checks <- sum(as.numeric(stale$checks))
      rsf_indicators <- db_indicators_get_labels(pool=schedulePool)
      
      for (i in 1:nrow(stale)) {
        cat(paste0("Checking ",
                   stale[i,checks],
                   " pending checks for SYSID ",stale[i,rsf_pf_id],
                   " ",
                   stale[i,pfcbl_name],
                   " from ",as.character(stale[i,check_asof_date]),"\n"))

        
        tryCatch({
          
          msgs <- character()
          
          withCallingHandlers({
            rsf_program_check(pool=schedulePool,
                              rsf_pf_id=stale[i,rsf_pf_id],
                              rsf_indicators=rsf_indicators,
                              for_import_id=NA,  #when for_import_id is unassigned, will assign to the last-uploaded import by date.
                              check_consolidation_threshold=NA,
                              reference_asof_date=NULL,
                              CALCULATIONS_ENVIRONMENT = scheduleCalcEnv,
                              status_message= function(...) { invisible(NULL) })
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
  return (as.numeric(pending_checks))
}
