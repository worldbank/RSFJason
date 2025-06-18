

	
# check
# rsf_program_id <- 367701
# pfcbl_ids.familytree <- 367701
# reporting_current_date='2023-09-30';
# check_formula_id = 151
#Revise to take check_id and then check/warn if program is not subscribed to it
rsf_checks_do_test <- function(pool,
                               rsf_program_id,
                               pfcbl_ids.familytree=NULL,
                               reporting_current_date,
                               check_formula_id,
                               status_message=function(...) {})
{
  
  
  
  if (is.null(pfcbl_ids.familytree) || all(is.na(pfcbl_ids.familytree))) {
    pfcbl_ids.familytree <- dbGetQuery(pool,"
                                      select ids.rsf_pfcbl_id 
                                      from p_rsf.rsf_pfcbl_ids ids
                                      where ids.rsf_program_id = $1::int
                                        and ids.pfcbl_category = 'program'",
                                      params=list(rsf_program_id))
    pfcbl_ids.familytree <- as.numeric(unlist(pfcbl_ids.familytree))
  } else {
    pfcbl_ids.familytree <- as.numeric(unlist(pfcbl_ids.familytree))
  }
  
  check_formula <- dbGetQuery(pool,"
    select 
      icf.check_formula_title,
      ic.check_name,
      icf.check_pfcbl_category,
      ic.indicator_check_id
    from p_rsf.indicator_check_formulas icf
    inner join p_rsf.indicator_checks ic on ic.indicator_check_id = icf.indicator_check_id
    where icf.check_formula_id = $1::int
  ",params=list(check_formula_id))
  
  setDT(check_formula)
  
  
  perform_checks <- dbGetQuery(pool,"
    select 
      ids.rsf_program_id,
      array_to_string(array_agg(distinct ids.rsf_pfcbl_id),',')::text as check_rsf_pfcbl_ids,
    	array_to_string(array_agg(rdc.evaluation_id) filter(where rdc.evaluation_id is not NULL),',')::text as current_evaluation_ids,
    	$3::date as check_asof_date,
    	icf.check_formula_id,
      icf.computation_group,
    	lcu.data_unit_value as entity_local_currency_unit
    from p_rsf.rsf_pfcbl_ids ids 
    inner join p_rsf.indicator_check_formulas icf on icf.check_pfcbl_category = ids.pfcbl_category
    
    left join lateral (select * from p_rsf.rsf_data_current_lcu lcu
                       where lcu.for_rsf_pfcbl_id = ids.rsf_pfcbl_id
    									   and lcu.reporting_asof_date <= $3::Date
    									 order by lcu.reporting_asof_date desc
    									 limit 1) as lcu on true
    left join p_rsf.rsf_data_checks rdc on rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id
  																		 and rdc.check_asof_date = $3::date
  																		 and rdc.check_formula_id = icf.check_formula_id
  																		 and rdc.check_data_id_is_current	= true
    where icf.check_formula_id = $2::int
      and ids.created_in_reporting_asof_date <= $3::Date
      and ids.rsf_pfcbl_id = any(select ft.to_family_rsf_pfcbl_id
                                 from p_rsf.view_rsf_pfcbl_id_family_tree ft
                                 where ft.from_rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int))
    group by 
      ids.rsf_program_id,
      icf.check_formula_id,
      icf.computation_group,
    	lcu.data_unit_value",
    params=list(paste0(pfcbl_ids.familytree,collapse=","),
                check_formula_id,
                reporting_current_date))
                                    
  perform_checks[["check_rsf_pfcbl_ids"]] <- lapply(perform_checks[["check_rsf_pfcbl_ids"]],function(x) as.numeric(strsplit(x,split=',',fixed=T)[[1]]))
  perform_checks[["current_evaluation_ids"]] <- lapply(perform_checks[["current_evaluation_ids"]],function(x) as.numeric(strsplit(x,split=',',fixed=T)[[1]]))
  setDT(perform_checks)
  setorder(perform_checks,
           check_asof_date,
           computation_group)
  
  perform_checks[,
         check_group:=.GRP,
         by=.(check_asof_date,
              computation_group)]
  
  rsf_indicators <- db_indicators_get_labels(pool=pool)
  
  test <- rsf_program_perform_checks(pool=pool,
                                    rsf_indicators=rsf_indicators,
                                    perform_checks=perform_checks,
                                    perform.test=TRUE,
                                    status_message=status_message)
  
  results_data <- test$input
  check_results <- test$results

  if (anyNA(check_results$rsf_pfcbl_id)) {
    status_message("Warning: NA rsf_pfcbl_ids were returned for this check")
  }
  
  check_results <- check_results[is.na(rsf_pfcbl_id)==FALSE]
  
  if (empty(check_results)) return (NULL)
  

  check_pfcbl_ids <- unique(check_results$rsf_pfcbl_id)
  eval_ids <- na.omit(unique(unlist(perform_checks$current_evaluation_ids)))
  current_flags <- NULL
  if (length(eval_ids) > 0) {
    current_flags <- dbGetQuery(pool,"
                                select 
                                 rd.rsf_pfcbl_id,
                                 rdc.check_asof_date as evaluation_asof_date,
                                 rd.reporting_cohort_id,
                                 rc.source_name,
                                 rc.source_reference,
                                 rdc.indicator_check_id,
                                 rdc.check_formula_id,
                                 ic.check_name,
                                 coalesce(icg.overwrite_check_class,ic.check_class) as check_class,
                                 rdc.check_message,
                                 rdc.check_status_comment
                                from p_rsf.rsf_data_checks rdc
                                inner join p_rsf.indicator_checks ic on ic.indicator_check_id = rdc.indicator_check_id
                                inner join p_rsf.rsf_data rd on rd.data_id = rdc.data_id
                                inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
                                left join p_rsf.indicator_check_guidance icg on icg.indicator_check_guidance_id = rdc.indicator_check_guidance_id
                                 where rdc.evaluation_id = any(select unnest(string_to_array(NULLIF($1::text,'NA'),','))::int)",
                               params=list(paste0(eval_ids,collapse=",")))
    
    setDT(current_flags)
  }
  
  if (empty(current_flags)) {
    current_flags <- data.table(rsf_pfcbl_id=numeric(0),db_flags=character(0))
  } else {
    current_flags[,flag_text:=paste0(toupper(check_class),": ",check_name,ifelse(nchar(check_message)>0,paste0("/ ",check_message),""),
                                     " \U2208 ",evaluation_asof_date,": Upload #",reporting_cohort_id," ",source_name," ",source_reference)]
    
    current_flags <- current_flags[,.(db_flags=paste0(flag_text,collapse="\n")),by=.(rsf_pfcbl_id)]
  }  
  
  omit_cols <- c("reporting_current_date",
                 "reporting_group",
                 "rsf_pfcbl_id.global",
                 "rsf_pfcbl_id.program",
                 "rsf_pfcbl_id.facility",
                 "rsf_pfcbl_id.client",
                 "rsf_pfcbl_id.borrower",
                 "rsf_pfcbl_id.loan",
                 "rsf_program_id",
                 "rsf_facility_id",
                 "rsf_client_id",
                 "rsf_borrower_id",
                 "rsf_loan_id",
                 "row_id")
  
  setnames(results_data,
           old=paste0("rsf_",check_formula$check_pfcbl_category,"_id"),
           new="rsf_pfcbl_id")
  
  omit_cols <- omit_cols[omit_cols %in% names(results_data)]
  for (col in omit_cols) set(results_data,
                             j=col,
                             value=NULL)
  
  
  param_cols <- names(results_data)[-which(names(results_data)=="rsf_pfcbl_id")]
  for (col in param_cols) {
    col_data <- results_data[[col]]
    col_data <- lapply(col_data,function(x) {
      
      if (all(class(x)=="list")) x <- unlist(x)
      else if (inherits_any(x,"data.frame") && !is.null(x$reporting_asof_date) && !is.null(x$data_value)) x<-paste0(x$reporting_asof_date," = ",x$data_value)
      
      x <- as.character(x)
      
      if (length(x)==0) x <- '{VOID}'
      else if (any(is.na(x))) x[is.na(x)] <- '{MISSING}'
      
      if (length(x) > 1) x <- paste0(x,collapse=", ")
      x
    })
    col_data <- unlist(col_data)
    if (length(col_data) != nrow(results_data)) {
      warning("length(col_data) != nrow(formula_indicators).  Skipping formatting to character()")
    }
    set(x=results_data,i=NULL,j=col,value=col_data)
  }
  
  setnames(results_data,
           old=param_cols,
           new=paste0("parameter:",param_cols))
  
  check_results[,
                `:=`(check_formula_id=NULL)]
  
  check_results <- check_results[results_data,
                                 on=.(rsf_pfcbl_id),
                                 nomatch=NULL]
  setnames(check_results,
           old="check_message",
           new=paste0(check_formula$check_name,":",check_formula$check_formula_title))
  

  sys_names <- dbGetQuery(pool,"
                          select sn.rsf_pfcbl_id,sn.sys_name
                          from p_rsf.view_rsf_pfcbl_id_current_sys_names sn
                          where sn.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)",
                          params=list(paste0(check_results$rsf_pfcbl_id,collapse=",")))
  
  setDT(sys_names)
  
  check_results <- sys_names[check_results,
                             on=.(rsf_pfcbl_id),
                             nomatch=NULL]
  
  setnames(check_results,
           old=c("rsf_pfcbl_id","sys_name"),
           new=c("SYSID","SYSNAME"))
  
  setorder(check_results,
           SYSNAME)
  
  return (check_results)
}
