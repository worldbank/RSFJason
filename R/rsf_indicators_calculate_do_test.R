


# reporting_current_date <- '2025-06-30'
# indicator_id <-  157657
# formula_pfcbl_id.familytree <-367732
# rsf_program_id <- 367701
# formula_pfcbl_id.familytree <- 397397 ;rsf_program_id <- unlist(dbGetQuery(pool,"select rsf_program_id from p_rsf.rsf_pfcbl_ids where rsf_pfcbl_id=$1::int",params=list(formula_pfcbl_id.familytree)))

#x<-rsf_indicators_calculate_do_test(pool,rsf_program_id,reporting_current_date,indicator_id,formula_pfcbl_id.familytree,all_parameters=FALSE)

rsf_indicators_calculate_do_test <- function(pool,
                                             rsf_program_id,
                                             reporting_current_date,
                                             indicator_id,
                                             formula_pfcbl_id.familytree=NA,
                                             all_parameters=TRUE,
                                             parent_formula_id=as.numeric(NA),
                                             status_message=function(...) {})
{
  
  {
    #test_indicator_id <- 451
    if (is.null(formula_pfcbl_id.familytree) || all(is.na(formula_pfcbl_id.familytree))) {
      pfcbl_ids.familytree <- dbGetQuery(pool,"select rsf_pfcbl_id
                                               from p_rsf.rsf_pfcbl_ids
                                               where rsf_program_id = $1::int
                                                 and pfcbl_category = (select data_category from p_rsf.indicators where indicator_id = $2::int)",
                                         params=list(rsf_program_id,
                                                     indicator_id))
      pfcbl_ids.familytree <- as.numeric(unlist(pfcbl_ids.familytree))
    } else {
      pfcbl_ids.familytree <- as.numeric(formula_pfcbl_id.familytree)
    }
    
    status_message(paste0("Running test for SYSID=",paste0(formula_pfcbl_id.familytree,collapse=","),
                          " indicator_id=",indicator_id,
                          " rsf_program_id=",rsf_program_id))
    
    test_calculation <- dbGetQuery(pool,"select 
                                          ids.rsf_pfcbl_id as calculate_rsf_pfcbl_id,
                                          ind.indicator_id as calculate_indicator_id,
                                          $3::date as calculate_asof_date,
                                          
                                          cd.data_id as current_data_id,
                                          cd.data_value as current_data_value,
                                          cd.data_unit as current_data_unit,
                                          rd.data_sys_flags as current_data_sys_flags,
                                          coalesce(lcu.data_unit_value,'LCU') as entity_local_currency_unit,
                                          coalesce(rc.parent_reporting_cohort_id,rc.reporting_cohort_id) as current_reporting_cohort_id,
                                          coalesce(rc.reporting_asof_date = $3::date,false) as current_value_updated_in_reporting_current_date,
                                          coalesce(rc.is_reported_cohort,false) as current_value_is_user_monitored,
                                          coalesce(rc.is_calculated_cohort,false) as current_data_is_system_calculation,
                                          
                                          case when ind.data_type = 'currency'     -- LCU-defined currency metrics whose settings are overwriting the output
                                                and ind.data_unit = 'LCU'
                                                and pis.formula_calculation_unit is not NULL
                                                then pis.formula_calculation_unit
                                                
                                                when ind.data_type in ('currency','currency_ratio')     -- only calculate relevant indicator types
                                                and ind.data_unit ~ 'LCU'
                                                and cd.data_unit is distinct from lcu.data_unit_value -- eg, this indicator hasn't been calculated yet, it's default 
                                                then regexp_replace(ind.data_unit,'LCU',lcu.data_unit_value)
                                                
                                                when ind.data_type in ('currency','currency_ratio')
                                                and ind.data_unit ~ 'LCU' = false             -- its a defined value, eg a _USD or _EUR indicator: then this must be the calculated output
                                                then ind.data_unit
                                                
                                                else coalesce(cd.data_unit,ind.data_unit)
                                          end as calculate_indicator_data_unit,
                                          indf.computation_group,
                                          indf.formula_id,
                                          indf.formula_fx_date,
                                          pis.formula_calculation_unit,
                                          --indf.formula_unit_set_by_indicator_id,
    
    
                                          ind.data_type,
                                          ind.data_unit as default_data_unit,
                                          ind.is_periodic_or_flow_reporting,
                                          indf.formula_calculation_rank,
                                          ids.rsf_program_id,
                                          ids.pfcbl_category_rank
                  
                                        from p_rsf.rsf_pfcbl_ids ids 
                                        inner join p_rsf.view_rsf_pfcbl_indicator_subscriptions pis on pis.rsf_pfcbl_id = ids.rsf_pfcbl_id
                                        inner join p_rsf.indicators ind on ind.indicator_id = pis.indicator_id
                                        inner join p_rsf.indicator_formulas indf on indf.formula_id = pis.formula_id

                                        left join lateral (select 
                                                             rdc.data_id,
                                                             rdc.data_value,
                                                             rdc.data_unit
                                                           from p_rsf.rsf_data_current rdc
                                                           where rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id
                                                             and rdc.indicator_id = ind.indicator_id
                                                             and rdc.reporting_asof_date <= $3::date
                                                           order by rdc.reporting_asof_date desc
                                                           limit 1) as cd on true
                                        left join p_rsf.rsf_data rd on rd.data_id = cd.data_id
                                        left join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
                                        left join lateral (select 
                                          									 lcu.data_unit_value,
                                          									 lcu.reporting_asof_date as lcu_current_date
                                          								 from p_rsf.rsf_data_current_lcu lcu
                                          								 where lcu.for_rsf_pfcbl_id = ids.rsf_pfcbl_id
                                          									 and lcu.reporting_asof_date <= $3::date
                                          								 order by lcu.reporting_asof_date desc
                                          								 limit 1) lcu on true			
                                        where pis.is_subscribed = true
                                          and pis.indicator_id = $2
                                          and $3::date >= ids.created_in_reporting_asof_date
                                  			and ids.rsf_pfcbl_id = any(select ft.to_family_rsf_pfcbl_id 
                                                                   from p_rsf.view_rsf_pfcbl_id_family_tree ft 
                                                                   where ft.from_rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int))",
                                                         params=list(paste0(unique(pfcbl_ids.familytree),collapse=","),
                                                                     indicator_id,
                                                                     reporting_current_date))
    
    setDT(test_calculation)
    if (empty(test_calculation)) {
      stop(paste0("Invalid calculation request and/or no entities in this tree exist asof ",as.character(reporting_current_date)))
    }
    
    
    # set_units <- test_calculation[!is.na(formula_unit_set_by_indicator_id),
    #                           .(calculate_rsf_pfcbl_id,
    #                             calculate_indicator_id,
    #                             calculate_asof_date,
    #                             formula_unit_set_by_indicator_id)]
    # if (!empty(set_units)) {
    #   
    #   formula_units <- poolWithTransaction(pool,function(conn) {
    #     
    #     dbExecute(conn,"create temp table _temp_units(calculate_rsf_pfcbl_id int,
    #                                                   calculate_indicator_id int,
    #                                                   calculate_asof_date date,
    #                                                   formula_unit_set_by_indicator_id int)
    #                 on commit drop;")
    #     
    #     dbAppendTable(conn,
    #                   name="_temp_units",
    #                   value=set_units)
    #     
    #     dbExecute(conn,"analyze _temp_units")
    #     dbGetQuery(conn,"
    #                  with units as (
    #                    select
    #                      tu.calculate_rsf_pfcbl_id,
    #                      tu.calculate_indicator_id,
    #                      tu.calculate_asof_date,
    #                      ft.to_family_rsf_pfcbl_id as unit_rsf_pfcbl_id,
    #                      tu.formula_unit_set_by_indicator_id,
    #                      ind.is_data_unit
    #                    from _temp_units tu
    #                    inner join p_rsf.indicators ind on ind.indicator_id = tu.formula_unit_set_by_indicator_id
    #                    inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = tu.calculate_rsf_pfcbl_id
    #                                                                     and ft.to_pfcbl_category = ind.data_category
    #                  )
    #                  select 
    #                    units.calculate_rsf_pfcbl_id,
    #                    units.calculate_indicator_id,
    #                    units.calculate_asof_date,
    #                    unit.data_unit,
    #                    units.formula_unit_set_by_indicator_id
    #                  from units
    #                  inner join lateral (select
    #                                       case when coalesce(units.is_data_unit,false) = true
    #                                            then rdc.data_value
    #                                            else rdc.data_unit
    #                                       end as data_unit
    #                                      from p_rsf.rsf_data_current rdc
    #                                      where rdc.rsf_pfcbl_id = units.unit_rsf_pfcbl_id
    #                                        and rdc.indicator_id = units.formula_unit_set_by_indicator_id
    #                                        and rdc.reporting_asof_date <= units.calculate_asof_date
    #                                      order by
    #                                        rdc.reporting_asof_date desc
    #                                      limit 1) as unit on true")
    #   })
    #   
    #   setDT(formula_units)
    #   formula_units <- formula_units[is.na(data_unit)==FALSE &
    #                                  data_unit != "LCU"]
    #   test_calculation[formula_units,
    #                calculate_indicator_data_unit:=i.data_unit,
    #                on=.(calculate_rsf_pfcbl_id,
    #                     calculate_indicator_id,
    #                     calculate_asof_date,
    #                     formula_unit_set_by_indicator_id)]
    #   
    #   formula_units <- NULL
    # }
    

  }
  
  {
    rsf_indicators <- db_indicators_get_labels(pool=pool)
    
    rsf_calculator_checks <- dbGetQuery(pool,
                                        "select 
                                            ic.check_name,
                                            ic.indicator_check_id,
                                            ic.variance_tolerance_allowed
                                          from p_rsf.indicator_checks ic
                                          where ic.is_calculator_check = true")
    
    setDT(rsf_calculator_checks)
    
    test_results <- rsf_program_perform_calculations(pool=pool,
                                                     current_data=test_calculation,
                                                     rsf_indicators=rsf_indicators,
                                                     rsf_calculator_checks=rsf_calculator_checks,
                                                     perform.test = TRUE,
                                                     status_message=status_message)
    
    
    
    calculation <- dbGetQuery(pool,"
                              select 
                                ind.*,
                                indf.formula,
                                indf.formula_id,
                                indf.formula_title
                              from p_rsf.indicators ind
                              inner join p_rsf.indicator_formulas indf on indf.indicator_id = ind.indicator_id
                              where indf.formula_id = any(select unnest(string_to_array($1::text,','))::int)",
                              params=list(paste0(unique(test_calculation$formula_id),collapse=",")))

    results <- test_results$results[,.(rsf_pfcbl_id,
                                       indicator_id,
                                       reporting_asof_date,
                                       formula_id,
                                       current_data_value_in_system=current_data_value,
                                       current_data_unit,
                                       current_data_is_system_calculation,
                                       calculated_data_value=data_value,
                                       calculated_data_unit=data_unit,
                                       data_changed)]

    # reporting <- dbGetQuery(pool,"
    #                                  select 
    #                                    ids.rsf_pfcbl_id,
    #                                    coalesce(has.reported,false) as reported,
    #                                    last.reporting_asof_date as last_reporting_date,
    #                                    status.*
    #                                  from p_rsf.rsf_pfcbl_ids ids
    #                                  inner join lateral p_rsf.get_rsf_pfcbl_id_reporting_status_asof_date(ids.rsf_pfcbl_id,
    #                                                                                                      ids.pfcbl_category,
    #                                                                                                      $2::date) status on true
    #                                  left join lateral (select true as reported
    #                                                     from p_rsf.rsf_pfcbl_reporting rpr 
    #                                                     where rpr.rsf_pfcbl_id = ids.rsf_pfcbl_id
    #                                                       and rpr.reporting_asof_date = $2::date) as has on true
    #                                  left join lateral (select reporting_asof_date
    #                                                     from p_rsf.rsf_pfcbl_reporting rpr 
    #                                                     where rpr.rsf_pfcbl_id = ids.rsf_pfcbl_id
    #                                                       and rpr.reporting_asof_date <= $2::date
    #                                                     order by rpr.reporting_asof_date desc
    #                                                     limit 1) as last on true                                                          
    #                                  where ids.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
    #                                  ",params=list(paste0(results$rsf_pfcbl_id,collapse=","),
    #                                                as.character(reporting_current_date)))
    # 
    # setDT(reporting)
    # unexpected <- reporting[reported==FALSE |
    #                         quarter_reporting_expected==FALSE][results,
    #                                                            on=.(rsf_pfcbl_id),
    #                                                            nomatch=NULL]
    # if (!empty(unexpected)) {
    #   
    #   unexpected[,
    #              flags:=paste0("Warning:System does not expect to calculate asof ",
    #                            reporting_asof_date,
    #                            ". Entity is ",quarter_end_reporting_status," and last reported on ",
    #                            last_reporting_date,". Results may have unexpected values")]
    #   
    #   #If multiple formulas are calculated, may include multiple formulas.
    #   if (any(grepl("\\.previous",calculation$formula))) {
    #     unexpected[test_calculation,
    #                formula_id:=i.formula_id,
    #                on=.(rsf_pfcbl_id=calculate_rsf_pfcbl_id)]
    #     
    #     unexpected[calculation,
    #                flags:=paste0(flags,". Calculation uses .previous -- if entity did not report in previous quarter, results may be unreliable"),
    #                on=.(formula_id)]
    #     unexpected[,formula_id:=NULL]
    #   }
    #   
    #   test_results$flags <- rbindlist(list(test_results$flags,
    #                                        unexpected[,
    #                                              .(rsf_pfcbl_id,
    #                                                indicator_id,
    #                                                reporting_asof_date,
    #                                                flags)]))
    #   
    #   
    # }

    results <- test_results$flags[,
                                   .(flags=paste0(unique(flags),collapse=" AND ALSO ")),
                                   by=.(rsf_pfcbl_id,
                                        indicator_id,
                                        reporting_asof_date)][results,
                                                              on=.(rsf_pfcbl_id,
                                                                   indicator_id,
                                                                   reporting_asof_date),
                                                              nomatch=NA]
    

    results[!is.na(calculated_data_unit),
            calculated_data_value:=paste0(calculated_data_value," ",calculated_data_unit)]
      
    results[!is.na(current_data_unit),
            current_data_value_in_system:=paste0(current_data_value_in_system," ",current_data_unit)]
      
    results[,
           `:=`(calculated_data_unit=NULL,
                current_data_unit=NULL)]
      
    setcolorder(results,
                neworder=names(results)[-which(names(results)=="calculated_data_value")])
    
    setnames(results,
             old="calculated_data_value",
             new=paste0("calculated:",unique(calculation$indicator_name))) #calculations can have multiple because of different formula_ids
  }
  
  #omit inputs fields
  {
    inputs <- test_results$inputs

    pfcbl_cols <- grep("^rsf_[a-z]+_id$",names(inputs),value=T)
    sys_ids <- do.call(pmax,args=cbind(inputs[,..pfcbl_cols],data.frame(na.rm=T)))
    inputs[,SYSID:=sys_ids]
  
    setnames(inputs,
             old=paste0("rsf_",unique(calculation$data_category),"_id"),
             new="rsf_pfcbl_id")
      
    omit_cols <- c("reporting_current_date",
                   "reporting_group",
                   "rsf_global_id",
                   "rsf_program_id",
                   "rsf_facility_id",
                   "rsf_client_id",
                   "rsf_borrower_id",
                   "rsf_loan_id")
      
    omit_cols <- omit_cols[omit_cols %in% names(inputs)]
    
    for(d in names(inputs)[sapply(inputs,is.Date)]) set(inputs,j=d,value=as.character(inputs[[d]]))
    
    
    for(d in omit_cols) set(inputs,j=d,value=NULL)
    
    #For .all parameters in formulas
    if (any(sapply(inputs,is.list))) {
      list_cols <- names(inputs)[sapply(inputs,is.list)]
      for (col in list_cols) {
        set(inputs,
            i=NULL,
            j=col,
            value=    as.character(sapply(inputs[[col]],
                               function(x) {
                                 if (is.data.table(x) && all(c("timeseries","timeseries.unit") %in% names(x))) {
                                   return (paste(paste0(ifelse(is.na(x$timeseries),"{MISSING}",as.character(x$timeseries)),
                                                        ifelse(is.na(x$timeseries.unit),"",paste0(" ",x$timeseries.unit)),
                                                        collapse=", ")))
                                 } else {
                                   return (paste(x,collapse=", "))
                                 }
                               }))
        )
      }
    }
    
    #Without all ID columns can be more simple
    inputs <- unique(inputs)
    input_cols <- names(inputs)[!names(inputs) %in% c("rsf_pfcbl_id","SYSID")]
    
    setnames(inputs,
             old=input_cols,
             new=paste0("parameter:",input_cols))
    
  }
  
  #SYS NAMES
  {
    sys_names <- dbGetQuery(pool,"select sn.sys_name,sn.rsf_pfcbl_id
                              from p_rsf.view_rsf_pfcbl_id_current_sys_names sn
                              where sn.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)",
                            params=list(paste0(unique(c(results$rsf_pfcbl_id,inputs$SYSID)),collapse=",")))
    setDT(sys_names)
    setnames(sys_names,
             old="sys_name",
             new="SYSNAME")
    
    results[,SYSID:=rsf_pfcbl_id]
    results <- results[sys_names,
                       on=.(rsf_pfcbl_id),
                       nomatch=NULL]
    
    
    setorder(results,
             SYSNAME)
    
    inputs <- inputs[sys_names,
                     on=.(SYSID=rsf_pfcbl_id),
                     nomatch=NULL]
    
    setorder(inputs,
             SYSNAME)
  }
  
  if (empty(results)) {
    status_message(class="warning","Warning: no result could be calculated.")
    return (NA);
  }
  
  results[calculation,
          formula_title:=i.formula_title,
          on=.(formula_id)]

  setnames(results,
           old="reporting_asof_date",
           new="calculation_asof_date")
  
  setcolorder(results,
              neworder=c("rsf_pfcbl_id","indicator_id","calculation_asof_date","SYSNAME","SYSID"))
  
  setcolorder(results,
              neworder=names(results)[-which(names(results)=="flags")])
  
  setcolorder(inputs,
              neworder=c("rsf_pfcbl_id","SYSNAME","SYSID"))
  
  setcolorder(inputs,
              neworder=names(results)[-which(names(results)=="parameter:fx_asof_date")])
  
  results[,indicator_id:=NULL]
  
  col_types <- sapply(inputs,class)
  if (any(col_types=="list")) {
    for (col in which(col_types=="list")) set(inputs,
                                              i=NULL,
                                              j=col,
                                              value=as.character(lapply(inputs[[col]],function(x) paste0(paste(names(x),"=",x),collapse=" "))))
  }
  
  if (all_parameters==TRUE) {
    
    {
      id_vars <- c("rsf_pfcbl_id","SYSID","calculation_asof_date","SYSNAME","formula_id")
      
      {

        for(p in names(results)[!names(results) %in% id_vars]) set(results,
                                                                   i=NULL,
                                                                   j=p,
                                                                   value=as.character(results[[p]]))
        results[calculation,
                formula_calculation:=i.formula,
                on=.(formula_id)]
        
        # results[,
        #         formula_calculation:=calculation$formula]
        # 
        
        setcolorder(results,
                    neworder = c(grep("^calculated",names(results),value=T),
                                 "formula_title",
                                 "formula_calculation"))
        
        
        results <- melt.data.table(results,
                                   id.vars=c(id_vars,"formula_title"),
                                   variable.factor = F,
                                   variable.name = "name",
                                   value.factor = F,
                                   value.name="value")
      }
      
      {      
        for(p in names(inputs)[!names(inputs) %in% id_vars]) set(inputs,
                                                                 i=NULL,
                                                                 j=p,
                                                                 value=as.character(inputs[[p]]))
        
        inputs[,calculation_asof_date:=unique(results$calculation_asof_date)]
        inputs[unique(results[,.(rsf_pfcbl_id,
                                 formula_id)]),
               formula_id:=i.formula_id,
               on=.(rsf_pfcbl_id)]
        
        
        inputs <- melt.data.table(inputs,
                                   id.vars=intersect(id_vars,names(inputs)),
                                   variable.factor = F,
                                   variable.name = "name",
                                   value.factor = F,
                                   value.name="value")
      }
      
      inputs[unique(results[,.(formula_id,formula_title)]),
             formula_title:=i.formula_title,
             on=.(formula_id)]
      
      setcolorder(inputs,
                  neworder=names(results))
      
      results <- rbindlist(list(results,
                                inputs))
      
      results[,col_order:=1:.N]
      results[,parent_formula_id:=parent_formula_id]
    }
    
    results[,
            formula_order:=.GRP,
            by=.(formula_id)]
    
    setorder(results,
             SYSNAME,
             formula_order,
             col_order)
    
    results[,
            col_order:=1:.N]
    results[,formula_order:=NULL]
    
    if (is.na(parent_formula_id)==TRUE) {
      
      prerequisites <- dbGetQuery(pool,"
        with requirements as materialized (
          select 
          calc.formula_id as parent_formula_id,
  				pids.to_parameter_rsf_pfcbl_id,
  				requirement_indicator_id
  				
          from (
    				select
    				pis.rsf_pfcbl_id,
    				pis.formula_id
    				from p_rsf.view_rsf_pfcbl_indicator_subscriptions pis 
    				where pis.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
    				  and pis.formula_id = any(select unnest(string_to_array($2::text,','))::int)
  				) as calc
  				inner join p_rsf.indicator_formulas indf on indf.formula_id = calc.formula_id
  				inner join lateral unnest(indf.formula_indicator_id_requirements) as requirement_indicator_id on true
  				inner join p_rsf.indicators ind on ind.indicator_id = requirement_indicator_id
  				inner join p_rsf.compute_calculation_to_parameter_rsf_pfcbl_ids pids on pids.from_calculate_rsf_pfcbl_id = calc.rsf_pfcbl_id
  				                                                                    and pids.from_calculate_formula_id = calc.formula_id
          where pids.to_parameter_pfcbl_category = ind.data_category																																								
        )
        select 
          req.parent_formula_id,
          req.requirement_indicator_id,
          pis.rsf_pfcbl_id,
          pis.formula_id
        from requirements req
        inner join p_rsf.view_rsf_pfcbl_indicator_subscriptions pis on pis.rsf_pfcbl_id = req.to_parameter_rsf_pfcbl_id
                                                                   and pis.indicator_id = req.requirement_indicator_id
        inner join p_rsf.indicator_formulas indf on indf.formula_id = pis.formula_id																								 
        where pis.formula_id is not null
        order by 
          indf.formula_calculation_rank,
          indf.computation_group,
          indf.formula_id",
  			params=list(paste0(unique(results$rsf_pfcbl_id),collapse=","),
  			            paste0(unique(results$formula_id),collapse=",")))
      

#             prerequisites <- dbGetQuery(pool,"
#         select 
#           pis.rsf_pfcbl_id,
#           requirements.requirement_indicator_id,
#           requirements.parent_formula_id,
#           indf.formula_id
#           from (
#             select 
#             indf.formula_id as parent_formula_id,
#             unnest(indf.formula_indicator_id_requirements) as requirement_indicator_id
#             from p_rsf.indicator_formulas indf
#             where indicator_id = $1::int
#               and indf.formula_id = any(select unnest(string_to_array($2::text,','))::int)
#           ) as requirements
#         inner join p_rsf.indicator_formulas indf on indf.indicator_id = requirements.requirement_indicator_id
#         inner join p_rsf.view_rsf_pfcbl_indicator_subscriptions pis on pis.indicator_id = requirements.requirement_indicator_id
#         																													 and pis.formula_id = indf.formula_id
#         where pis.rsf_pfcbl_id = any(select ft.to_family_rsf_pfcbl_id
# 				                             from p_rsf.view_rsf_pfcbl_id_family_tree ft
# 																		 where ft.from_rsf_pfcbl_id = any(select unnest(string_to_array($3::text,','))::int))
#           and (indf.formula_id <> any(select unnest(string_to_array($2::text,','))::int))
#         order by 
#         indf.formula_calculation_rank,
#         indf.computation_group,
#         indf.formula_id",
#         params=list(indicator_id,
#                     paste0(unique(results$formula_id),collapse=","),
#                     paste0(unique(pfcbl_ids.familytree),collapse=",")))
#       
      if (!empty(prerequisites)) {
        setDT(prerequisites)
        for (fId in unique(prerequisites$formula_id)) {
          pre <- prerequisites[formula_id==fId]  
          
          preResults <- rsf_indicators_calculate_do_test(pool=pool,
                                                         rsf_program_id=rsf_program_id,
                                                         reporting_current_date=reporting_current_date,
                                                         indicator_id=unique(pre$requirement_indicator_id),
                                                         formula_pfcbl_id.familytree=unique(pfcbl_ids.familytree),
                                                         all_parameters=TRUE,
                                                         parent_formula_id=unique(pre$parent_formula_id),
                                                         status_message=function(...) {})
          
          
          # if (any(names(preResults)=="loan_inclusion_rank") &&
          #     !any(names(results)=="loan_inclusion_rank")) {
          #   results[,
          #           loan_inclusion_rank:=as.numeric(NA)]
          # }
          # 
          # if (any(names(results)=="loan_inclusion_rank") &&
          #     !any(names(preResults)=="loan_inclusion_rank")) {
          #   preResults[,
          #              loan_inclusion_rank:=as.numeric(NA)]
          # }
          
          setcolorder(preResults,
                      neworder = names(results))
          
          results <- rbindlist(list(results,
                                    preResults))
        }
      }
    
    } else {
      return(results)
    }
    
    rank_ids <- dbGetQuery(pool,"
      select distinct on (ft.to_family_rsf_pfcbl_id,rdc.data_value)
        ft.to_family_rsf_pfcbl_id as rsf_pfcbl_id,
        rdc.data_value::numeric
      from p_rsf.view_rsf_pfcbl_id_family_tree ft
      inner join p_rsf.indicators ind on ind.data_category = ft.to_pfcbl_category
      inner join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
                                           and rdc.indicator_id = ind.indicator_id
      where ft.from_rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
        and ind.indicator_sys_category = 'rank_id'
        and rdc.reporting_asof_date <= $2::date
      order by ft.to_family_rsf_pfcbl_id,rdc.data_value,rdc.reporting_asof_date desc",
      params=list(paste0(unique(pfcbl_ids.familytree),collapse=","),
                  reporting_current_date))
    setDT(rank_ids)
    if (!empty(rank_ids)) {
      results[rank_ids,
              loan_inclusion_rank:=i.data_value,
              on=.(SYSID=rsf_pfcbl_id)]
    }
  
  
  
  } else {
    
    inputs[,
           `:=`(SYSID=NULL,
                SYSNAME=NULL)]
    
    results <- results[inputs,
                       on=.(rsf_pfcbl_id),
                       nomatch=NULL]
    setcolorder(results,
                neworder=names(results)[-which(names(results)=="flags")])
  }

  setattr(results,
          name="formula",
          value=calculation$formula)
  
  return (results)
}
