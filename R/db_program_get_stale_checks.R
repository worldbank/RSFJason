db_program_get_stale_checks <- function(pool,
                                        rsf_program_id,
                                        rsf_pfcbl_id.family=NULL,
                                        limit_future=today()) {
  
  rsf_program_id <- suppressWarnings(as.numeric(rsf_program_id))
  if (is.na(rsf_program_id)) stop("Bad rsf program_id")
  if (is.null(rsf_pfcbl_id.family) || all(is.na(rsf_pfcbl_id.family)) || length(rsf_pfcbl_id.family)==0) {
    rsf_pfcbl_id.family <- dbGetQuery(pool,"
                                           select ids.rsf_pfcbl_id 
                                           from p_rsf.rsf_pfcbl_ids ids
                                           where ids.rsf_program_id = $1::int
                                             and ids.pfcbl_category = 'program';",
                                       params=list(rsf_program_id))
    rsf_pfcbl_id.family <- unique(unlist(rsf_pfcbl_id.family))
  }
  
  if (length(rsf_pfcbl_id.family)==0) stop("Invalid request")
  if (length(limit_future)==0) limit_future <- NA
  
  #Global shouldn't have any checks...but maybe someday?
  #if (!any(rsf_pfcbl_id.family==0)) rsf_pfcbl_id.family <- c(0,rsf_pfcbl_id.family)
  
  #rsf_pfcbl_id.family <- 13930
  #rsf_program_id <- 11
  #conn <- poolCheckout(DBPOOL); 
  #dbBegin(conn) 
  #dbRollback(conn);dbBegin(conn)
  #poolReturn(conn)
  
  
  
  t1 <- Sys.time()
  
  #For OBA the stale checks being grouped here reduced download size from 1.5BG to 5.6Mb!!!  And massively reduced download time
  #at the modest expense of increasing the query time 
  #Disassociating the evaluation IDs from the entity they're evaluating is a big efficiency loss.
  #But also assumes that re-evaluating a check is not common and most entities are NOT flagged even when they're triggered to be evaluated
  #So the IO to re-query is low (to be tested?)
  checks <- dbGetQuery(pool,"
                       select
                      	array_to_string(array_agg(distinct dce.rsf_pfcbl_id),',')::text as check_rsf_pfcbl_ids,
                      	array_to_string(array_agg(rdc.evaluation_id) filter(where rdc.evaluation_id is not NULL),',')::text as current_evaluation_ids,
                      	dce.check_asof_date,
                      	dce.check_formula_id,
                        icf.computation_group,
                      	lcu.data_unit_value as entity_local_currency_unit
                      	from p_rsf.view_rsf_pfcbl_id_family_tree ft
                      	inner join p_rsf.rsf_data_check_evaluations dce on dce.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
                      	inner join p_rsf.indicator_check_formulas icf on icf.check_formula_id = dce.check_formula_id
                        left join lateral (select lcu.data_unit_value
                      	                   from p_rsf.rsf_data_current_lcu lcu
                      										 where lcu.for_rsf_pfcbl_id = dce.rsf_pfcbl_id
                      										   and lcu.reporting_asof_date <= dce.check_asof_date
                      											order by lcu.reporting_asof_date desc
                      											limit 1) lcu on true

                        left join p_rsf.rsf_data_checks rdc on rdc.rsf_pfcbl_id = dce.rsf_pfcbl_id
																													 and rdc.check_asof_date = dce.check_asof_date
																													 and rdc.check_formula_id = dce.check_formula_id
																													 and rdc.check_data_id_is_current	= true

                      	where ft.from_rsf_pfcbl_id = $1::int
                      	  and coalesce(dce.check_asof_date <= $2::date,true)
                      group by
                      	dce.check_formula_id,
                      	dce.check_asof_date,
                        icf.computation_group,
                        icf.check_formula_id,
                      	lcu.data_unit_value",
                       params=list(rsf_pfcbl_id.family,
                                   limit_future))

  
  checks[["check_rsf_pfcbl_ids"]] <- lapply(checks[["check_rsf_pfcbl_ids"]],function(x) as.numeric(strsplit(x,split=',',fixed=T)[[1]]))
  checks[["current_evaluation_ids"]] <- lapply(checks[["current_evaluation_ids"]],function(x) as.numeric(strsplit(x,split=',',fixed=T)[[1]]))
  
  setDT(checks)
  
  if(SYS_PRINT_TIMING) debugtime("db_program_get_stale_checks","Downloaded:",nrow(checks),format(Sys.time()-t1))
  
  setorder(checks,
           check_asof_date,
           computation_group)
  
  checks[,
         check_group:=.GRP,
         by=.(check_asof_date,
              computation_group)]
  
  checks[,rsf_pfcbl_id.family:=rsf_pfcbl_id.family]
  checks[,rsf_program_id:=rsf_program_id]
  if(SYS_PRINT_TIMING) debugtime("rsf_program_get_stale_checks","Done!",format(Sys.time()-t1)," with ",checks$check_group[length(checks$check_group)]," GROUPS")
  
  return(checks)
}