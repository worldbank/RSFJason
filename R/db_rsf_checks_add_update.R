#NEW
db_rsf_checks_add_update <- function(pool,
                                     data_checks,
                                     consolidation_threshold=100) #this should probably be made a program parameter
{
  #setups
  {
  
    if (empty(data_checks)) return (NULL)
    if (anyNA(data_checks$check_asof_date)) stop("NA check_asof_date not allowed")
    if (anyNA(data_checks$indicator_check_id)) stop("NA indicator_check_id not allowed")
    if (anyNA(data_checks$rsf_pfcbl_id)) stop("NA rsf_pfcbl_id not allowed")
    if (anyNA(data_checks$for_indicator_id)) stop("NA for indicator_id not allowed: ensure user checks identify indicator_id on which to apply before upload")
    #if (!empty(data_checks[is.na(indicator_id) & is.na(check_formula_id)])) stop("system checks must submit NA check_formula_id and a value for indicator_id and user checks, the inverse")
    #if (length(using_reporting_cohort_id) != 1) { stop("Only one using_reporting_cohort_id value allowed, NA by default") }
    #lobstr::obj_size(data_checks)
    t1 <- Sys.time()
   
    expected_cols <- c("rsf_pfcbl_id",
                       "for_indicator_id",
                       "check_asof_date",
                       "check_formula_id",
                       "indicator_check_id",
                       "check_message")
    
    if (length((bad_cols <- setdiff(union(names(data_checks),expected_cols),names(data_checks))))>0) {
      stop(paste0("data_checks must define columns: ",paste0(bad_cols,collapse=", ")))
    }
    
    if (!any(names(data_checks)=="variance")) {
      data_checks[,
                  variance:=as.numeric(NA)]
    }
  }  
  
  #Get facility and program IDs for guidance
  {
    ids <- dbGetQuery(pool,
                      "select 
                         ids.rsf_program_id,
                         ids.rsf_facility_id,
                         ids.rsf_pfcbl_id
                       from p_rsf.rsf_pfcbl_ids ids
                       where ids.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
                       ",params=list(paste0(unique(data_checks$rsf_pfcbl_id),collapse=",")))
    
    setDT(ids)
    data_checks[ids,
                `:=`(rsf_program_id=i.rsf_program_id,
                     rsf_facility_id=i.rsf_facility_id),
                on=.(rsf_pfcbl_id)]
    ids <- NULL
  }
  
  
  
  #get guidances
  {
   
    #conn <- poolCheckout(pool)
    #dbBegin(conn)
    #dbRollback(conn)
    guidances <- poolWithTransaction(pool,function(conn) {
      dbExecute(conn,"create temp table _lookups(for_indicator_id int,
                                                 indicator_check_id int,
                                                 rsf_program_id int,
                                                 rsf_facility_id int)
                on commit drop")
      
      
      dbAppendTable(conn,
                    name="_lookups",
                    value=unique(data_checks[,.(for_indicator_id,
                                                indicator_check_id,
                                                rsf_program_id,
                                                rsf_facility_id)]))
      
      dbGetQuery(conn,"
                 select distinct on (icg.for_indicator_id,icg.indicator_check_id)
                   icg.for_indicator_id,
                   icg.indicator_check_id,
                   lup.rsf_program_id,
                   lup.rsf_facility_id,
                   pfcg.indicator_check_guidance_id,
                   icg.overwrite_check_class,
                   icg.variance_threshold,
                   icg.is_ignoring_guidance,
                   pfcg.rsf_pfcbl_id as guidance_rsf_pfcbl_id
                   --icg.is_resolving_guidance
                 from _lookups lup
                 inner join p_rsf.indicator_check_guidance icg on icg.for_indicator_id = lup.for_indicator_id
                                                              and icg.indicator_check_id = lup.indicator_check_id
                 inner join p_rsf.rsf_program_facility_check_guidance pfcg on pfcg.indicator_check_guidance_id = icg.indicator_check_guidance_id
                 where (pfcg.rsf_program_id = 0
                        or 
                        pfcg.rsf_program_id = lup.rsf_program_id)
                   and (pfcg.rsf_facility_id is NULL
                        or 
                        pfcg.rsf_facility_id is NOT DISTINCT from lup.rsf_facility_id
                       )
                 order by
                  icg.for_indicator_id,
                  icg.indicator_check_id,
                  pfcg.rsf_program_id <> 0 desc,
                  pfcg.rsf_facility_id is not null desc
               ")
    })
    
    setDT(guidances)
    guidances[is.na(variance_threshold),
              variance_threshold:=0]
    
    data_checks[,
                `:=`(guidance_id=as.numeric(NA),
                     check_class=as.character(NA),
                     variance_threshold=0,
                     is_ignoring_guidance=FALSE)]
    
    if (!empty(guidances)) {
      #TBD: join also on formula_id?
      #No: because guidance is strictly about which flag is applied to which indicator, regardless of the formula used.
      data_checks[guidances,
                  `:=`(guidance_id=i.indicator_check_guidance_id ,
                       check_class=i.overwrite_check_class,
                       variance_threshold=i.variance_threshold,
                       is_ignoring_guidance=i.is_ignoring_guidance),
                  on=.(rsf_program_id,
                       rsf_facility_id,
                       for_indicator_id,
                       indicator_check_id)]
      
    }
    
    data_checks[,
                variance_threshold_breached:=FALSE] #Beach means don't apply the guidance because the variance is beyond the threshold
    
    #Guidance is applied only for variances BELOW the defined threshold
    #Ie, if variance is 2% and threshold to ignore/auto resolve is 10% then guidance will be applied.
    #Otherwise, guidance does not apply
    data_checks[!is.na(guidance_id) &
                !is.na(variance) & 
                !is.na(variance_threshold) & 
                variance_threshold > 0 &           #A threshold of zero implies don't apply a variance threshold.
                variance >= variance_threshold,
                variance_threshold_breached:=TRUE]
    
    data_checks[!is.na(guidance_id) &
                !is.na(variance) & 
                !is.na(variance_threshold) & 
                variance_threshold < 0 &           #A negative threshold means apply when variance is ABOVE the threshold
                variance <= abs(variance_threshold),
                variance_threshold_breached:=TRUE]
    
    data_checks[variance_threshold_breached==TRUE,
                guidance_id:=as.numeric(NA)]
    
    data_checks[,omit:=FALSE]
    data_checks[!is.na(guidance_id) & 
                is_ignoring_guidance==TRUE,
                omit:=TRUE]
    
    data_checks <- data_checks[omit==FALSE]
    data_checks[,
                `:=`(omit=NULL,
                     variance_threshold_breached=NULL)]
  }
  
  #Group checks (to consolidate multi-messages and to speed uploads)
  {
    #Data checks may be empty after filtering out ignoring guidance.
    #Ie, there was one check. We found guidance to ignore it.  We ignored it here before uploading and removing from
    #checks table.  Now checks table is empty.  So nothing more to do.
    if (empty(data_checks)) {
      return (FALSE)
    }
    
    data_checks[is.na(check_message),
                check_message:="Warning: message undefined for this check. Update in admin section"]
    
    #for IO speed, consolidate by inidicator_id,date and check
    #eg, for OBA testing and 28 million checks and lobstr::obj_size(data_checks)=111.55
    #consolidated 41.90MB
    
    #system flag that has no formula ID
    data_checks[is.na(check_formula_id),
                check_formula_id:=-1]
    
    #Now that we're consolidating checks, let maximum of 100
    data_checks[,rsf_pfcbl_id:=as.numeric(rsf_pfcbl_id)]
    data_checks <- data_checks[,
                               .(n=.N,
                                 checks_dt=list(.SD)),
                               by=.(for_indicator_id,
                                    check_asof_date,
                                    indicator_check_id,
                                    check_formula_id,
                                    check_class,
                                    rsf_program_id,
                                    rsf_facility_id,
                                    guidance_id),
                               .SDcols=c("rsf_pfcbl_id",
                                         "check_message",
                                         "variance")]
  }
  #consolidation only works if you can cansolidate under something...so in this case the reporting_cohort
  #(and where "Reporting") is at client level
  data_checks[,
              `:=`(consolidated_from_indicator_id=as.character(NA),
                   consolidated_from_indicator_check_id=as.character(NA))]
  
  data_checks[,original_auto_resolve:=as.logical(NA)]
  
  if (any(data_checks$n > consolidation_threshold,na.rm=T)) {
    
    consolidations <- data_checks[n>=consolidation_threshold]
    data_checks <- data_checks[n<consolidation_threshold]
    
    consolidate_reporting <- dbGetQuery(pool,"
                                          select distinct on (fam.child_rsf_pfcbl_id)
                                            fam.child_rsf_pfcbl_id as rsf_pfcbl_id,
                                            fam.parent_rsf_pfcbl_id as consolidate_rsf_pfcbl_id,
                                            ind.indicator_id as consolidate_indicator_id
                                          from p_rsf.rsf_pfcbl_id_family fam
                                          inner join p_rsf.indicators ind on ind.data_category = fam.parent_pfcbl_category
                                          where fam.child_rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
                                            and ind.indicator_sys_category = 'entity_reporting'
                                            and fam.parent_pfcbl_rank <= 3
                                          order by
                                            fam.child_rsf_pfcbl_id,
                                            fam.parent_pfcbl_rank desc",
                                        params=list(paste0(rbindlist(consolidations$checks_dt)[,unique(rsf_pfcbl_id)],collapse=",")))
    setDT(consolidate_reporting)
    #conn <- poolCheckout(pool)
    #dbBegin(conn)
    #dbRollback(conn)
    consolidate_checks <- poolWithTransaction(pool,function(conn) {
      dbExecute(conn,"create temp table _temp_checks(for_indicator_id int,
                                                     indicator_check_id int,
                                                     check_formula_id int)
                on commit drop;")
      
      dbAppendTable(conn,
                    name="_temp_checks",
                    value=unique(consolidations[,
                                                .(for_indicator_id,
                                                  indicator_check_id,
                                                  check_formula_id)]))
      
      dbGetQuery(conn,"
                 select 
                   tc.for_indicator_id,
                   tc.indicator_check_id,
                   tc.check_formula_id,
                   ic.check_name as check_name,
                   ic.check_class,
                   ind.indicator_name as indicator_name,
                   cic.indicator_check_id as consolidate_check_id,
                   coalesce(ic.auto_resolve_system_check,icf.auto_resolve) as original_auto_resolve
                 
                 from _temp_checks tc 
                 inner join p_rsf.indicator_checks ic on ic.indicator_check_id = tc.indicator_check_id
                 inner join p_rsf.indicators ind on ind.indicator_id = tc.for_indicator_id
                 inner join p_rsf.indicator_checks cic on cic.check_class = ic.check_class
                 left join p_rsf.indicator_check_formulas icf on icf.check_formula_id = tc.check_formula_id
                 where cic.check_name ~ 'sys_consolidation_too_many_flags'")
    })
    
    setDT(consolidate_checks)
    
    consolidate_checks[,
                       check_message:=paste0(indicator_name," [",check_name,"]")]
    
    consolidations <- consolidations[,
                                     unlist(checks_dt,recursive=F),
                                     by=.(for_indicator_id,
                                          check_asof_date,
                                          indicator_check_id,
                                          check_formula_id,
                                          rsf_program_id,
                                          rsf_facility_id,
                                          guidance_id,
                                          check_class,
                                          consolidated_from_indicator_id,
                                          consolidated_from_indicator_check_id)]
    
    consolidations <- consolidations[consolidate_reporting,
                                     on=.(rsf_pfcbl_id),
                                     nomatch=NULL]
    
    consolidations <- consolidations[,
                                     .(n=.N,
                                       checks_dt=list(.SD)),
                                     by=.(for_indicator_id,
                                          check_asof_date,
                                          indicator_check_id,
                                          check_formula_id,
                                          rsf_program_id,
                                          rsf_facility_id,
                                          guidance_id,
                                          check_class,
                                          consolidated_from_indicator_id,
                                          consolidated_from_indicator_check_id,
                                          consolidate_rsf_pfcbl_id,
                                          consolidate_indicator_id),
                                     .SDcols=c("rsf_pfcbl_id",
                                               "check_message",
                                               "variance")]
    
    consolidations[consolidate_checks,
                   `:=`(check_message=i.check_message,
                        consolidate_check_id=i.consolidate_check_id,
                        original_auto_resolve=i.original_auto_resolve,
                        check_class=ifelse(is.na(check_class),
                                           i.check_class,
                                           check_class)), #if guidance defines, else default
                   on=.(for_indicator_id,
                        indicator_check_id,
                        check_formula_id)]
    
    consolidations[,
                   check_message:=paste0(check_message," raised ",format(n,big.mark=",")," flags")]
    
    for (r in 1:nrow(consolidations)) {
      check <- consolidations[r]
      messages <- check$checks_dt[[1]]
      
      
      variances <- sort(messages$variance,na.last=NA)
      variance_msg <- NULL
      if (length(variances)==1) {
        variance_msg <- paste0("Calculation variance is ",variances)
      } else if (length(variances) >= 2) {
        variance_msg <- paste0("Calculation variance ranges from ",variances[1]," to ",variances[length(variances)])
      }

      messages <- messages[,
                           .(n=.N,
                             first_id=min(rsf_pfcbl_id)),
                           by=.(check_message)]
      
      
      issues <- ""
      if (nrow(messages) >= 3) {
        setorder(messages,
                 -n,
                 first_id)
        
        num <- nrow(messages)  
        messages <- messages[1:3]
        issues <- paste0("top 3 issues (of ",num," total) are:\n",
                         paste0(
                          paste0(messages$check_message," (",format(messages$n,big.mark=",")," observations)"),
                          collapse=" \n{AND ALSO} "))
      } else {
        issues <- paste0("issues are:\n",
                          paste0(messages$check_message," (",format(messages$n,big.mark=",")," observations)"),
                          collapse=" \n{AND ALSO} ")
        
      }
      
      if (!is.null(variance_msg)) issues <- paste0(variance_msg," and ",issues)
      
      consolidations[for_indicator_id==check$for_indicator_id &
                     check_asof_date==check$check_asof_date &
                     indicator_check_id==check$indicator_check_id &
                     check_formula_id == check$check_formula_id,
                     check_message:=paste0(check_message,": ",issues)]
      
    }
    
    consolidations[,checks_dt:=NULL]
    
    
    consolidations[,
                   `:=`(row=1:.N,
                        rsf_pfcbl_id=as.numeric(consolidate_rsf_pfcbl_id),
                        for_indicator_id=as.numeric(consolidate_indicator_id),
                        indicator_check_id=consolidate_check_id,
                        consolidated_from_indicator_id=for_indicator_id,
                        consolidated_from_indicator_check_id=indicator_check_id)]
    
    consolidations[,
                   `:=`(consolidate_check_id=NULL,
                        consolidate_rsf_pfcbl_id=NULL,
                        consolidate_indicator_id=NULL)]
    
    consolidations[,variance:=as.numeric(NA)]
    #regroup for rbindlist with non-consolidated checks
    consolidations <- consolidations[,
                               .(n=sum(n),
                                 checks_dt=list(.SD)),
                               by=.(for_indicator_id,
                                    check_asof_date,
                                    indicator_check_id,
                                    check_formula_id,
                                    original_auto_resolve,
                                    consolidated_from_indicator_id,
                                    consolidated_from_indicator_check_id,
                                    rsf_program_id,
                                    rsf_facility_id,
                                    guidance_id,
                                    check_class,
                                    row),
                               .SDcols=c("rsf_pfcbl_id",
                                         "check_message",
                                         "variance")]
    
    #row keeps the by() aggregation separate per check per indicator as pre-consolidated.  Else can wrap-up a lot of consolidations into one big messy message
    #that is tough to read or respond to
    consolidations[,row:=NULL]
    setcolorder(consolidations,
                neworder=names(data_checks))
    
    data_checks <- rbindlist(list(data_checks,
                                  consolidations))
    consolidations <- NULL
  }
  
  data_checks[,
              n:=NULL]
  
  #Because multiple checks with different messages can be flagged to the same indicator as a result of consolidations
  data_checks[,row:=1:.N]
  
  data_checks <- data_checks[,
                             unlist(checks_dt,
                                    recursive=F),
                             by=.(row,
                                  for_indicator_id,
                                  check_asof_date,
                                  indicator_check_id,
                                  check_formula_id,
                                  guidance_id,
                                  consolidated_from_indicator_id,
                                  consolidated_from_indicator_check_id,
                                  original_auto_resolve)]
  data_checks[,row:=NULL]
  data_checks[,variance:=NULL]
  
  setcolorder(data_checks,
              neworder=c("rsf_pfcbl_id",
                         "for_indicator_id",
                         "check_asof_date",
                         "indicator_check_id",
                         "check_formula_id",
                         "check_message",
                         "guidance_id",
                         "consolidated_from_indicator_id",
                         "consolidated_from_indicator_check_id",
                         "original_auto_resolve"
                         ))
  
  #conn <- poolCheckout(pool)
  #dbBegin(conn);
  #dbRollback(conn);
  #poolReturn(conn)

  nx <- poolWithTransaction(pool,function(conn) {
    
        {
          dbExecute(conn,"
                    create temp table _temp_data_checks(data_id int,
                                                        rsf_pfcbl_id int,
                                                        indicator_id int,
                                                        check_asof_date date,
                                                        indicator_check_id int,
                                                        check_formula_id int,
                                                        status_time timestamptz,
                                                        check_message text,
                                                        check_status text,
                                                        check_status_comment text,
                                                        check_status_user_id text,
                                                        indicator_check_guidance_id int,
                                                        check_data_id_is_current bool,
                                                        consolidated_from_indicator_id int,
                                                        consolidated_from_indicator_check_id int)
                    on commit drop")
  
          dbExecute(conn,"create TEMP table _temp_add_checks(rsf_pfcbl_id int,
                                                             for_indicator_id int,
                                                             check_asof_date date,
                                                             indicator_check_id int,
                                                             check_formula_id int,
                                                             check_message text,
                                                             data_id int,
                                                             guidance_id int,
                                                             consolidated_from_indicator_id int,
                                                             consolidated_from_indicator_check_id int,
                                                             original_auto_resolve bool)
                      on commit drop;")
          #dbExecute(conn,"create index _tac_idx on _temp_add_checks(rsf_pfcbl_id)")        
        }   
    
        {
          dbAppendTable(conn,
                        name="_temp_add_checks",
                        value=data_checks)     
          
          dbExecute(conn,"analyze _temp_add_checks;")
          
          dbExecute(conn,"update _temp_add_checks tac
                          set data_id = (select rdc.data_id
                                         from p_rsf.rsf_data_current rdc
                                         where rdc.rsf_pfcbl_id = tac.rsf_pfcbl_id
                                           and rdc.indicator_id = tac.for_indicator_id
                                           and rdc.reporting_asof_date <= tac.check_asof_date
                                         order by
                                           rdc.reporting_asof_date desc
                                         limit 1)")
          
          #if a flag is generated on an indicator/data point that the entity has NEVER reported on before,
          #then reassign it to the entity's reporting indicator for this period.
          non_reporting_data <- dbGetQuery(conn,"select exists(select * from _temp_add_checks where data_id is NULL)::bool")
          if (unlist(non_reporting_data)==TRUE) {
            dbExecute(conn,"
                                  with reassign_checks as MATERIALIZED (
                                    select
                                    	tac.rsf_pfcbl_id,
                                    	concat(c_ind.data_category,' has never reported \"',
                                    	       c_ind.indicator_name,'\" but system checker flagged it for ',
                                    				 c_ic.check_name,': ',
                                    				 tac.check_message) as check_message,
                                    	r_ind.indicator_id as reassign_indicator_id,
                                    	r_ic.indicator_check_id as reassign_check_id,
                                    	reporting.data_id,
                                    	tac.for_indicator_id,
                                    	tac.check_asof_date,
                                    	tac.indicator_check_id,
                                    	tac.for_indicator_id as consolidated_from_indicator_id,
                                      tac.indicator_check_id as consolidated_from_indicator_check_id
                                    from _temp_add_checks tac
                                    inner join p_rsf.indicators c_ind on c_ind.indicator_id = tac.for_indicator_id
                                    inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = tac.rsf_pfcbl_id
                                    inner join p_rsf.indicators r_ind on r_ind.data_category = ids.pfcbl_category
                                    inner join p_rsf.indicator_checks c_ic on c_ic.indicator_check_id = tac.indicator_check_id
                                    inner join p_rsf.indicator_checks r_ic on r_ic.check_class = c_ic.check_class
                                                                          and r_ic.check_name ~ 'sys_unreported_data_flag'
                                    inner join lateral (select rdc.data_id 
                                                        from p_rsf.rsf_data_current rdc 
                                                        where rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id
                                    											and rdc.indicator_id = r_ind.indicator_id
                                    											and rdc.reporting_asof_date <= tac.check_asof_date::date
                                    										order by rdc.reporting_asof_date desc
                                    										limit 1) as reporting on true
                                    where tac.data_id is NULL
                                    	and r_ind.indicator_sys_category = 'entity_reporting'
                                  )
                                  update _temp_add_checks tac
                                  set check_message = rc.check_message,
                                      for_indicator_id = rc.reassign_indicator_id,
                                  		indicator_check_id = rc.reassign_check_id,
                                  		data_id = rc.data_id,
                                      consolidated_from_indicator_id = rc.consolidated_from_indicator_id,
                                      consolidated_from_indicator_check_id = rc.consolidated_from_indicator_check_id
                                  from reassign_checks rc
                                  where tac.data_id is NULL
                                    and tac.rsf_pfcbl_id = rc.rsf_pfcbl_id
                                  	and tac.for_indicator_id = rc.for_indicator_id
                                  	and tac.indicator_check_id = rc.indicator_check_id
                                  	and tac.check_asof_date = rc.check_asof_date")
          }
          
          
        }
    
        {
          dbExecute(conn,"
                    update _temp_add_checks tac
                    set check_formula_id = NULL
                    where exists(select * from p_rsf.indicator_checks ic
                                 where ic.indicator_check_id = tac.indicator_check_id
                                   and ic.is_system = true)
                      and check_formula_id is distinct from NULL")

          #delete if guidance is set to ignore
          dbExecute(conn,"
                    delete from _temp_add_checks tac
                    where exists(select * from p_rsf.indicator_check_guidance icg
                                 where icg.indicator_check_guidance_id = tac.guidance_id
                                   and icg.is_ignoring_guidance = true)")
          
          #delete where data already has this flag and this message (its refundant)
          dbExecute(conn,"
                  delete from _temp_add_checks tac
                  where exists(select * from p_rsf.rsf_data_checks rdc
                               where rdc.data_id = tac.data_id
                                 and rdc.check_asof_date = tac.check_asof_date
                                 and rdc.indicator_check_id = tac.indicator_check_id::int
                                 and rdc.check_message is not distinct from tac.check_message)")
          
          dbExecute(conn,"
                  delete from _temp_add_checks tac
                  where exists(select * from p_rsf.rsf_data_checks rdc
                               where rdc.rsf_pfcbl_id = tac.rsf_pfcbl_id
                                 and rdc.check_asof_date = tac.check_asof_date
                                 and rdc.indicator_check_id = tac.indicator_check_id::int
                                 and rdc.check_formula_id is not distinct from tac.check_formula_id
                                 and tac.check_formula_id is not null
                                 and rdc.check_data_id_is_current = true)")
          
          nothing <- dbGetQuery(conn,"select not exists(select * from _temp_add_checks)::bool")
          
          if (unlist(nothing)==TRUE) {
            return (0);
          }
          
          
          
          nx <- dbExecute(conn,"
                          insert into _temp_data_checks(data_id,
                      																	rsf_pfcbl_id,
                      																	indicator_id,
                      																	check_asof_date,
                      																	indicator_check_id,
                                                        check_formula_id,
                      																	status_time,
                      																	check_message,
                      																	check_status,
                      																	check_status_comment,
                      																	check_status_user_id,
                      																	indicator_check_guidance_id,
                      																	check_data_id_is_current,
                      																	consolidated_from_indicator_id,
                                                        consolidated_from_indicator_check_id)																
																	
                          select 
                          	tac.data_id,
                          	tac.rsf_pfcbl_id,
                          	tac.for_indicator_id,
                          	tac.check_asof_date,
                          	tac.indicator_check_id,
                            tac.check_formula_id,
                          	(timeofday())::timestamptz as status_time,
                          	tac.check_message,
                          	
                          	case 
                          		when gow.is_resolving_guidance is NOT NULL and gow.is_resolving_guidance = true then 'resolved'
                          		when gow.is_resolving_guidance is NOT NULL and gow.is_resolving_guidance = false then 'active'
                          		when coalesce(tac.original_auto_resolve,icf.auto_resolve,ic.auto_resolve_system_check,false) = true then 'resolved'
                          		else 'active'
                          	end as check_status,
                          	
                          	case 
                          		when gow.is_resolving_guidance is NOT NULL and gow.is_resolving_guidance = true then gow.guidance
                          		when gow.is_resolving_guidance is NOT NULL 
                          				 and gow.is_resolving_guidance = false 
                          				 and coalesce(tac.original_auto_resolve,icf.auto_resolve,ic.auto_resolve_system_check,false) = true then gow.guidance || ' (Guidance requires manual review instead of auto-resolve)'
                          		when coalesce(tac.original_auto_resolve,icf.auto_resolve,ic.auto_resolve_system_check,false) = true then 'Auto-resolved by SYSTEM'
                          		else NULL
                          	end as check_status_comment,
                          	
                          	case 
                          		when gow.is_resolving_guidance is NOT NULL and gow.is_resolving_guidance = true then gow.user_id
                          		when gow.is_resolving_guidance is NOT NULL 
                          		 and gow.is_resolving_guidance = false 
                          		 and coalesce(tac.original_auto_resolve,icf.auto_resolve,ic.auto_resolve_system_check,false) = true then gow.user_id
                          		when coalesce(tac.original_auto_resolve,icf.auto_resolve,ic.auto_resolve_system_check,false) = true then (select account_id 
                          																																			 from p_rsf.view_account_info 
                          																																			 where is_system_account = true 
                          																																				 and users_name = 'RSF SYS Calculator')
                          		else NULL
                          	end as check_status_user_id,
                          	tac.guidance_id,
                          	exists(select * from p_rsf.rsf_data_current rdc where rdc.data_id = tac.data_id) as check_data_id_is_current,
                            tac.consolidated_from_indicator_id,
                            tac.consolidated_from_indicator_check_id
                          from _temp_add_checks tac
                          left join p_rsf.rsf_data rd on rd.data_id = tac.data_id
                                                     and rd.reporting_asof_date = tac.check_asof_date
                          left join p_rsf.indicator_checks ic on ic.indicator_check_id = tac.indicator_check_id
                          left join p_rsf.indicator_check_formulas icf on icf.check_formula_id = tac.check_formula_id
                          left join p_rsf.indicator_check_guidance gow on gow.indicator_check_guidance_id = tac.guidance_id")
          
          
          #if(SYS_PRINT_TIMING)  debugtime("db_rsf_checks_add_update","staged",format(nx,big.mark = ",")," checks in ",format(Sys.time()-t1))
          
        }
        
        {
          nx <- dbExecute(conn,"
                          
                                insert into p_rsf.rsf_data_checks(data_id,
                                																	rsf_pfcbl_id,
                                																	indicator_id,
                                																	check_asof_date,
                                																	indicator_check_id,
                                                                  check_formula_id,
                                																	status_time,
                                																	check_message,
                                																	check_status,
                                																	check_status_comment,
                                																	check_status_user_id,
                                																	indicator_check_guidance_id,
                                																	check_data_id_is_current,
                                																	consolidated_from_indicator_id,
                                                                  consolidated_from_indicator_check_id)																
                                select 
                                  data_id,
  																rsf_pfcbl_id,
  																indicator_id,
  																check_asof_date,
  																indicator_check_id,
  																check_formula_id,
  																status_time,
  																check_message,
  																check_status,
  																check_status_comment,
  																check_status_user_id,
  																indicator_check_guidance_id,
  																check_data_id_is_current,
  																consolidated_from_indicator_id,
  																consolidated_from_indicator_check_id

                                from _temp_data_checks	
                                on conflict do nothing
                          ")
          
        }
      
      nx    
    })
  
  
  if(SYS_PRINT_TIMING) debugtime("db_rsf_checks_add_update","Done! uploading",nx," in ",format(Sys.time()-t1))
  return(TRUE)
}
