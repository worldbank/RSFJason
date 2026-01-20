
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
                                  			 
                                         when ind.data_type in ('currency_ratio')     -- only calculate relevant indicator types
                                  				and ind.data_unit ~ 'LCU'
                                          and lcu.data_unit_value is NOT NULL
                                          and coalesce(cd.current_data_unit ~ lcu.data_unit_value,false) = false -- LCU changed OR never reported
                                          
                                         -- in alphabetic order because if it HAD been reported then rsf_data_current WOULD have saved the fx ratio this way
                                  			 then p_rsf.fx_currency_ratio_in_alphabetic_order(regexp_replace(ind.data_unit,'LCU',lcu.data_unit_value))
                                 
                                  			 when ind.data_type in ('currency','currency_ratio')
                                  				and ind.data_unit ~ 'LCU' = false -- its a defined value, eg a _USD or _EUR indicator: then this must be the calculated output
                                  			 then ind.data_unit
                                  	
                                  			 else coalesce(cd.current_data_unit,ind.data_unit)
                                  	end as calculate_indicator_data_unit,
                                    calc.formula_calculation_unit,
                                    calc.computation_group,
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
                                      		indf.formula_fx_date,
                                      		indf.formula_id,
                                      		dense_rank() over(order by dce.rsf_pfcbl_id = 0 desc,          -- global always first
                                      															 dce.calculation_asof_date asc,      -- oldest calculations first
                                      															 indf.formula_calculation_rank asc,  -- lowest ranks first
                                      															 indf.computation_priority_rank desc -- higher computation priorities first
                                      										 ) calc_rank
                                      	from p_rsf.rsf_data_calculation_evaluations dce 
                                      	
                                      	-- does not judge on subscription status to calculated or not, ie, may return default calc if undefined
                                      	-- but entry shouldn't exist in dce if undefiend
                                      	inner join p_rsf.view_rsf_setup_indicator_subscriptions sis on sis.rsf_pfcbl_id = dce.rsf_pfcbl_id
                                      																													   and sis.indicator_id = dce.indicator_id
                                      									 
                                      	inner join p_rsf.indicator_formulas indf on indf.formula_id = sis.formula_id
                                      	where dce.rsf_pfcbl_id = any(select ft.to_family_rsf_pfcbl_id
                                      															 from p_rsf.view_rsf_pfcbl_id_family_tree ft
                                      															 where ft.from_rsf_pfcbl_id = $1::int)
                                      		and coalesce(dce.calculation_asof_date <= $2::date,true)
                                    ) x
                                    where x.calc_rank = 1
                                  ) calc 
                                  inner join p_rsf.indicators ind on ind.indicator_id = calc.indicator_id
                                  left join lateral (select
                                  									rdc.data_id as current_data_id,
                                  									rdc.data_value as current_data_value,
                                  									rdc.data_unit as current_data_unit
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
     
      # #See comment in SQL above that calculations groups can grow after being recalculated.
      # if (any(calculations$calculation_group > 1)) {
      #   calculations <- calculations[calculation_group==1]
      # }
      # set_units <- calculations[!is.na(formula_unit_set_by_indicator_id),
      #                           .(calculate_rsf_pfcbl_id,
      #                             calculate_indicator_id,
      #                             calculate_asof_date,
      #                             formula_unit_set_by_indicator_id)]
      # if (!empty(set_units)) {
      # 
      #   formula_units <- poolWithTransaction(pool,function(conn) {
      #     
      #     dbExecute(conn,"create temp table _temp_units(calculate_rsf_pfcbl_id int,
      #                                                      calculate_indicator_id int,
      #                                                      calculate_asof_date date,
      #                                                      formula_unit_set_by_indicator_id int)
      #               on commit drop;")
      #     
      #     dbAppendTable(conn,
      #                   name="_temp_units",
      #                   value=set_units)
      #     
      #     dbExecute(conn,"analyze _temp_units")
      #     dbGetQuery(conn,"
      #                with units as (
      #                  select
      #                    tu.calculate_rsf_pfcbl_id,
      #                    tu.calculate_indicator_id,
      #                    tu.calculate_asof_date,
      #                    ft.to_family_rsf_pfcbl_id as unit_rsf_pfcbl_id,
      #                    tu.formula_unit_set_by_indicator_id,
      #                    ind.is_data_unit
      #                  from _temp_units tu
      #                  inner join p_rsf.indicators ind on ind.indicator_id = tu.formula_unit_set_by_indicator_id
      #                  inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = tu.calculate_rsf_pfcbl_id
      #                                                                   and ft.to_pfcbl_category = ind.data_category
      #                )
      #                select 
      #                  units.calculate_rsf_pfcbl_id,
      #                  units.calculate_indicator_id,
      #                  units.calculate_asof_date,
      #                  unit.data_unit,
      #                  units.formula_unit_set_by_indicator_id
      #                from units
      #                inner join lateral (select
      #                                     case when coalesce(units.is_data_unit,false) = true
      #                                          then rdc.data_value
      #                                          else rdc.data_unit
      #                                     end as data_unit
      #                                    from p_rsf.rsf_data_current rdc
      #                                    where rdc.rsf_pfcbl_id = units.unit_rsf_pfcbl_id
      #                                      and rdc.indicator_id = units.formula_unit_set_by_indicator_id
      #                                      and rdc.reporting_asof_date <= units.calculate_asof_date
      #                                    order by
      #                                      rdc.reporting_asof_date desc
      #                                    limit 1) as unit on true")
      #   })
      #   
      #   setDT(formula_units)
      #   formula_units <- formula_units[is.na(data_unit)==FALSE &
      #                                  data_unit != "LCU"]
      #   calculations[formula_units,
      #                calculate_indicator_data_unit:=i.data_unit,
      #                on=.(calculate_rsf_pfcbl_id,
      #                     calculate_indicator_id,
      #                     calculate_asof_date,
      #                     formula_unit_set_by_indicator_id)]
      #     
      #   formula_units <- NULL
      # }

    if(SYS_PRINT_TIMING) debugtime("db_program_get_stale_calculations","Done!",as.numeric(Sys.time()-t1,"secs"))
    return (calculations)
}