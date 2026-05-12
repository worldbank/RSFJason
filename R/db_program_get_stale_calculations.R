
db_program_get_stale_calculations <- function(pool,
                                              rsf_pfcbl_id.family,
                                              limit_future=today()) 
{
  
  

  if (length(limit_future)==0) limit_future <- NA
  
      
      t1 <- Sys.time()
      calculations <- dbGetQuery(pool,"
                                 select 
                                  	calc.rsf_pfcbl_id as calculate_rsf_pfcbl_id,
                                  	calc.indicator_id as calculate_indicator_id,
                                  	calc.calculation_asof_date as calculate_asof_date,
                                  	cd.current_data_id,
                                  	cd.current_data_value,
                                  	cd.current_data_unit,
                                  	cd.current_data_date,
                                  	coalesce(lcu.data_unit_value,'LCU') as entity_local_currency_unit,
                                  	coalesce(rc.reporting_asof_date = calc.calculation_asof_date,false) as current_value_updated_in_reporting_current_date,
                                  	coalesce(rc.is_reported_cohort,false) as current_value_is_user_monitored,
                                  	coalesce(rc.is_calculated_cohort,false) as current_data_is_system_calculation,
                                 
                                    case when ind.data_type = 'currency'     -- LCU-defined currency metrics whose settings are overwriting the output
                                          and ind.data_unit = 'LCU'
                                          and calc.formula_calculation_unit is not NULL
                                         then calc.formula_calculation_unit

                                         when ind.data_type = 'currency' -- only calculate relevant indicator types
                                  				and ind.data_unit = 'LCU'
                                  				and cd.current_data_unit is distinct from lcu.data_unit_value -- eg, this indicator hasn't been calculated yet, it's default 
                                  			 then lcu.data_unit_value
                                 
                                         when ind.data_type = 'currency'
                                          and (ind.unit_fx_indicator_id is not NULL OR (ind.data_unit ~ 'LCU' is false))
                                         then ind.data_unit
                                         
                                         when ind.data_type = 'currency_ratio'
                                         then ind.data_unit

                                  			 else coalesce(cd.current_data_unit,ind.data_unit)
                                  	end as calculate_indicator_data_unit,
                                    calc.formula_calculation_unit,
                                    coalesce(calc.computation_group,1) as computation_group, -- unit_fx_indicator_ids are self-hierarchy calculations, a group of 1.
                                    calc.formula_id,
                                    ind.data_type,
                                    ind.is_periodic_or_flow_reporting,
                                    rd.data_sys_flags as current_data_sys_flags
                                  from (
                                      select * 
                                      from (
                                    		select 
                                      		dce.rsf_pfcbl_id,
                                      		dce.indicator_id,
                                      		dce.calculation_asof_date,
                                      		indf.formula_calculation_rank,
                                      		sis.formula_calculation_unit,
                                      		indf.computation_group,
                                      		indf.formula_id,
                                      		dense_rank() over(order by dce.rsf_pfcbl_id = 0 desc,          -- global always first
                                      															 dce.calculation_asof_date asc,      -- oldest calculations first
                                      															 --sis.data_type = 'currency_ratio' desc -- currency ratios always first.
                                      															 indf.formula_calculation_rank asc nulls last,  -- lowest ranks first
                                      															 indf.computation_priority_rank desc nulls last -- higher computation priorities first
                                      										 ) calc_rank
                                      	from p_rsf.rsf_data_calculation_evaluations dce 
                                      	
                                      	-- does not judge on subscription status to calculated or not, ie, may return default calc if undefined
                                      	-- but entry shouldn't exist in dce if undefiend
                                      	inner join p_rsf.view_rsf_setup_indicator_subscriptions sis on sis.rsf_pfcbl_id = dce.rsf_pfcbl_id
                                      																													   and sis.indicator_id = dce.indicator_id
                                      									 
                                      	left join p_rsf.indicator_formulas indf on indf.formula_id = sis.formula_id -- left join for unit_fx indicators
                                      	where dce.rsf_pfcbl_id = any(select ft.to_family_rsf_pfcbl_id
                                      															 from p_rsf.view_rsf_pfcbl_id_family_tree ft
                                      															 where ft.from_rsf_pfcbl_id = $1::int)
                                      		and coalesce(dce.calculation_asof_date <= $2::date,true)
                                      		and sis.is_calculated is true
                                    ) x
                                    where x.calc_rank = 1
                                  ) calc 
                                  inner join p_rsf.indicators ind on ind.indicator_id = calc.indicator_id
                                  left join lateral (select
                                  									rdc.data_id as current_data_id,
                                  									rdc.data_value as current_data_value,
                                  									rdc.data_unit as current_data_unit,
                                  									rdc.reporting_asof_date as current_data_date
                                  								 from p_rsf.rsf_data_current rdc
                                  								 where rdc.rsf_pfcbl_id = calc.rsf_pfcbl_id
                                  									 and rdc.indicator_id = calc.indicator_id
                                  									 and rdc.reporting_asof_date <= calc.calculation_asof_date
                                  								 order by rdc.reporting_asof_date desc
                                  								 limit 1) cd on true
                                  																		
                                  left join lateral (select 
                                  									 lcu.data_unit_value,
                                  									 lcu.reporting_asof_date as lcu_current_date
                                  								 from p_rsf.rsf_data_current_lcu lcu
                                  								 where lcu.for_rsf_pfcbl_id = calc.rsf_pfcbl_id
                                  									 and lcu.reporting_asof_date <= calc.calculation_asof_date
                                  								 order by lcu.reporting_asof_date desc
                                  								 limit 1) lcu on true																
                                  left join p_rsf.rsf_data rd on rd.data_id = cd.current_data_id
                                  left join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
                                  ",
                                 params=list(rsf_pfcbl_id.family,
                                             as.character(limit_future)))
      
      setDT(calculations)
      if (empty(calculations)) return (NULL)
     
    if(SYS_PRINT_TIMING) debugtime("db_program_get_stale_calculations","Done!",as.numeric(Sys.time()-t1,"secs"))
    return (calculations)
}