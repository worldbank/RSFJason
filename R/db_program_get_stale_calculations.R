
db_program_get_stale_calculations <- function(pool,
                                              rsf_pf_id,
                                              limit_future=today()) 
{
   
  

  if (length(limit_future)==0) limit_future <- NA
  
      
      t1 <- Sys.time()
      
      #I go back and forth... do we calculate the facility-project-global hierarchy when just a project_id is submited?
      #But a program shouldn't be able to upload facility-level information anymore...Facility creation info and names, etc should be created under its own
      #facility ID now.  So a program-level upload cannot submit information that would trigger facility-level calculations that in turn trigger program-level
      #aggregating functions.
      # select ids.rsf_program_id,ids.rsf_facility_id,dce.calculation_asof_date,dce.formula_calculation_rank
      # from p_rsf.view_rsf_pfcbl_id_family_tree ft 
      # /*
      #   this is much faster but removed because if pfcbl family is program, we do actually want to calculate all its child facilities.
      # p_rsf.rsf_pfcbl_ids ids
      # CROSS JOIN LATERAL (
      #   VALUES
      #   (ids.rsf_gpfcbl_family[1]),
      #   (ids.rsf_gpfcbl_family[2]),
      #   (ids.rsf_gpfcbl_family[3])
      # ) AS pf(rsf_pf_id)
      # */
      #   inner join p_rsf.rsf_data_calculation_evaluations dce on dce.rsf_pf_id = ft.to_family_rsf_pfcbl_id
      # inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
      # where ft.from_rsf_pfcbl_id =  $1::int
      # and ft.to_pfcbl_rank <= 2
      calculations <- dbGetQuery(pool,"
        with evaluation_priority as (
          select 
        
          pids.rsf_pf_id,
          ep.calculation_asof_date,
          ep.formula_calculation_rank,
          ep.fx_priority
          from (
            select 
              -- Idea here is for rsf_facility_ids to pull-in calculations that its program needs to calculate, too, at the same calcualtion rank.
              -- eg, sums of facility metrics, etc.  So that program get calculated along with child-level calculations.
              -- since rsf_pfcbl_ids ids is (mostly) pull-in in 'from' rsf_facility_id, if we're returning the prioritized global ID 0, then we want to 
              -- ensure that _only_ global metrics are included in that priority group (so we can't simply unnest gpfcbl_family as it would include the facility 
              -- and program IDs).  But if there are no global priorities, then pull in only facility+program and then program, in that order.
              case when dce.rsf_pfcbl_id = 0 then 0 else ids.rsf_program_id end as rsf_program_id,
              case when dce.rsf_pfcbl_id = 0 then NULL else ids.rsf_facility_id end as rsf_facility_id,
              dce.calculation_asof_date,
              dce.formula_calculation_rank,
              ind.data_type is not distinct from 'currency_ratio' as fx_priority
            from p_rsf.rsf_pfcbl_ids ids
            CROSS JOIN LATERAL (
                VALUES
                    (ids.rsf_gpfcbl_family[1]),
                    (ids.rsf_gpfcbl_family[2]),
                    (ids.rsf_gpfcbl_family[3])

/* very slightly less efficient?                    
VALUES
                    (ids.rsf_gpfcbl_family[1],array[ids.rsf_gpfcbl_family[1]]::int[]),  -- global only
                    (ids.rsf_gpfcbl_family[2],array[ids.rsf_gpfcbl_family[2]]::int[]),  -- program (but not global)
                    (ids.rsf_gpfcbl_family[3],array[ids.rsf_gpfcbl_family[2:3]]::int[]) -- program and facility IDs
            ) AS pf(rsf_pf_id,rsf_gpf_id)                    
*/                    

            ) AS pf(rsf_pf_id)
            inner join p_rsf.rsf_data_calculation_evaluations dce on dce.rsf_pf_id = pf.rsf_pf_id
            inner join p_rsf.indicators ind on ind.indicator_id = dce.indicator_id
            where ids.rsf_pfcbl_id =  $1::int
              and ids.pfcbl_category_rank <= 2
              and coalesce(dce.calculation_asof_date <= $2::date,true)
            order by 
              dce.rsf_pfcbl_id = 0 desc,          -- global always first
              dce.calculation_asof_date asc,      -- oldest calculations first
              dce.formula_calculation_rank asc nulls last,  -- lowest ranks first
              
              -- comments below are retained but this is now maintained through the calculation of formula_calculation_rank itself:
              -- facility-level currency_ratios are only allowed to have global parameters and, knowing global is always calculated as a pre-requisite, have formula_calculation_rank = 0
              -- currency_ratio data type can only be defined at global and facility levels (restricted by table check currency_ratio_allowed_data_categories)
              -- All global metrics are always calculated first; and global metrics cannot include any non-global metrics. Therefore, global calculation ratios will always be calculated before any consumer needs them.
              -- And constrainted by indicator_formula triggers, facility-level currency ratios can only have global parameters (eg, the quarter end date; or possibly a matched fx rate's unit value)
              -- Meaning, these should always have a calculation rank of 1 (or zero if it's a constant, eg, USD/USD fx ratio); and there are no calculation pre-requisites required within this tree and they can be computed alongside
              --ind.data_type is not distinct from 'currency_ratio' desc, -- currency ratios first: formula_calculation_ranks may be identical based on parameter requirements, but a currency ratio
              -- formulas with a formula_calculation_rank of zero.  Which non-formulaic unit_fx_indicator_ids will have and they *may* require pre-requisite FX values to compute properly.
              -- exists(select true from p_rsf.indicators ind where ind.indicator_id = dce.indicator_id and ind.data_type = 'currency_ratio') desc, -- previously joined indicators to test data_type, but the data is not needed for the sort.
              fx_priority desc,
              ids.pfcbl_category_rank desc -- facility before program (because facility can unnest program)
            limit 1         
          ) as ep
            cross join lateral (
              values (ep.rsf_facility_id),
                     (ep.rsf_program_id)) as pids(rsf_pf_id) 
                     
          --values is juuuuuust a little faster.
          --cross join lateral (select ep.rsf_facility_id as rsf_pf_id
          --                    union all
          --                    select ep.rsf_program_id as rsf_pf_id) as pids
        
        ) 
        
        select 
          
          calc.rsf_pfcbl_id as calculate_rsf_pfcbl_id,
          calc.indicator_id as calculate_indicator_id,
          calc.calculation_asof_date as calculate_asof_date,
          calc.rsf_pf_id,
          cd.current_data_id,
          cd.current_data_value,
          cd.current_data_unit,
          cd.current_data_date,
          coalesce(lcu.data_unit_value,'LCU') as entity_local_currency_unit,
          cd.current_data_date is NOT distinct from calc.calculation_asof_date as current_value_updated_in_reporting_current_date,
          NOT coalesce(cd.is_calculated,false) as current_value_is_user_monitored,
          coalesce(cd.is_calculated,false) as current_data_is_system_calculation,
          
          case when ind.data_type = 'currency'     -- LCU-defined currency metrics whose settings are overwriting the output
                and ind.data_unit = 'LCU'
                and rsi.formula_calculation_unit is not NULL
               then rsi.formula_calculation_unit
          
               when ind.data_type = 'currency' -- only calculate relevant indicator types
                and ind.data_unit = 'LCU'
                and cd.current_data_unit is distinct from lcu.data_unit_value -- eg, this indicator hasn't been calculated yet, it's default 
               then lcu.data_unit_value
          
               when ind.data_type = 'currency'
                and (ind.unit_fx_indicator_id is not NULL OR (ind.data_unit is distinct from 'LCU')) -- controlled by unit_fx_indicator_id or it's hard-coded as non-LCU, then use indicator-defined data unit, eg, USD, EUR....
               then ind.data_unit
               
               when ind.data_type = 'currency_ratio'
               then ind.data_unit
               else coalesce(cd.current_data_unit,ind.data_unit)     
          end as calculate_indicator_data_unit,
          
          rsi.formula_calculation_unit,
          coalesce(indf.computation_group,1) as computation_group, -- unit_fx_indicator_ids are self-hierarchy calculations, a group of 1.
          rsi.formula_id,
          ind.data_type,
          ind.is_periodic_or_flow_reporting,
          rd.data_sys_flags as current_data_sys_flags,
          coalesce(indf.formula_calculation_rank,0) as formula_calculation_rank
        
        from p_rsf.rsf_data_calculation_evaluations calc
        inner join evaluation_priority ep on ep.rsf_pf_id = calc.rsf_pf_id
                                         and ep.calculation_asof_date = calc.calculation_asof_date
                                         and ep.formula_calculation_rank = calc.formula_calculation_rank
        inner join p_rsf.rsf_setup_indicators rsi on rsi.rsf_pfcbl_id = ep.rsf_pf_id
                                                 and rsi.indicator_id = calc.indicator_id
        inner join p_rsf.indicators ind on ind.indicator_id = calc.indicator_id       
                                          
        inner join p_rsf.rsf_pfcbl_ids cids on cids.rsf_pfcbl_id = calc.rsf_pfcbl_id
                                          
        left join p_rsf.indicator_formulas indf on indf.formula_id = rsi.formula_id -- left join for unit_fx indicators

        -- if the indicator is grouped at a parent-level, then calculate at that group's LCU value.
        -- otherwise, downstream, the formulas will be segmented based on currency of calculation and input data will be partitioned 
        -- and aggregates will be wrong (based on LCU partitions instead of the full group unified by that group's LCU)
        cross join lateral (values (cids.rsf_gpfcbl_family[1+(coalesce(indf.formula_grouping_pfcbl_rank,cids.pfcbl_category_rank))])) as baselcu(rsf_pfcbl_id) 
        
        -- The current data value (used downstream to compare if the system calculation has changed compared to current)
        left join lateral (select
                          rdc.data_id as current_data_id,
                          rdc.data_value as current_data_value,
                          rdc.data_unit as current_data_unit,
                          rdc.reporting_asof_date as current_data_date,
                          rdc.is_calculated
                         from p_rsf.rsf_data_current rdc
                         where rdc.rsf_pfcbl_id = calc.rsf_pfcbl_id
                           and rdc.indicator_id = calc.indicator_id
                           and rdc.reporting_asof_date <= calc.calculation_asof_date
                         order by rdc.reporting_asof_date desc
                         limit 1) cd on true
                         
        -- LCU currency based on the calculation parent grouping level 
        left join lateral (select 
                           lcu.data_unit_value,
                           lcu.reporting_asof_date as lcu_current_date
                         from p_rsf.rsf_data_current_lcu lcu
                         where lcu.for_rsf_pfcbl_id = baselcu.rsf_pfcbl_id
                           and lcu.reporting_asof_date <= calc.calculation_asof_date
                         order by lcu.reporting_asof_date desc
                         limit 1) lcu on true			

        -- To secure sytem flags, if any                                           
        left join p_rsf.rsf_data rd on rd.data_id = cd.current_data_id
        where rsi.is_subscribed is true
          and ((ep.fx_priority is true and ind.data_type = 'currency_ratio')
               or ep.fx_priority is not true)",
                                 params=list(rsf_pf_id,
                                             as.character(limit_future)))
      
      # calculations <- dbGetQuery(pool,"
      #                            select 
      #                             	calc.rsf_pfcbl_id as calculate_rsf_pfcbl_id,
      #                             	calc.indicator_id as calculate_indicator_id,
      #                             	calc.calculation_asof_date as calculate_asof_date,
      #                             	cd.current_data_id,
      #                             	cd.current_data_value,
      #                             	cd.current_data_unit,
      #                             	cd.current_data_date,
      #                             	coalesce(lcu.data_unit_value,'LCU') as entity_local_currency_unit,
      #                             	coalesce(rc.reporting_asof_date = calc.calculation_asof_date,false) as current_value_updated_in_reporting_current_date,
      #                             	coalesce(rc.is_reported_cohort,false) as current_value_is_user_monitored,
      #                             	coalesce(rc.is_calculated_cohort,false) as current_data_is_system_calculation,
      #                            
      #                               case when ind.data_type = 'currency'     -- LCU-defined currency metrics whose settings are overwriting the output
      #                                     and ind.data_unit = 'LCU'
      #                                     and calc.formula_calculation_unit is not NULL
      #                                    then calc.formula_calculation_unit
      # 
      #                                    when ind.data_type = 'currency' -- only calculate relevant indicator types
      #                             				and ind.data_unit = 'LCU'
      #                             				and cd.current_data_unit is distinct from lcu.data_unit_value -- eg, this indicator hasn't been calculated yet, it's default 
      #                             			 then lcu.data_unit_value
      #                            
      #                                    when ind.data_type = 'currency'
      #                                     and (ind.unit_fx_indicator_id is not NULL OR (ind.data_unit ~ 'LCU' is false))
      #                                    then ind.data_unit
      #                                    
      #                                    when ind.data_type = 'currency_ratio'
      #                                    then ind.data_unit
      # 
      #                             			 else coalesce(cd.current_data_unit,ind.data_unit)
      #                             	end as calculate_indicator_data_unit,
      #                               calc.formula_calculation_unit,
      #                               coalesce(calc.computation_group,1) as computation_group, -- unit_fx_indicator_ids are self-hierarchy calculations, a group of 1.
      #                               calc.formula_id,
      #                               ind.data_type,
      #                               ind.is_periodic_or_flow_reporting,
      #                               rd.data_sys_flags as current_data_sys_flags
      #                             from (
      #                                 select * 
      #                                 from (
      #                               		select 
      #                                 		dce.rsf_pfcbl_id,
      #                                 		dce.indicator_id,
      #                                 		dce.calculation_asof_date,
      #                                 		indf.formula_calculation_rank,
      #                                 		sis.formula_calculation_unit,
      #                                 		indf.computation_group,
      #                                 		indf.formula_id,
      #                                 		dense_rank() over(order by dce.rsf_pfcbl_id = 0 desc,          -- global always first
      #                                 															 dce.calculation_asof_date asc,      -- oldest calculations first
      #                                 															 --sis.data_type = 'currency_ratio' desc -- currency ratios always first.
      #                                 															 indf.formula_calculation_rank asc nulls last,  -- lowest ranks first
      #                                 															 indf.computation_priority_rank desc nulls last -- higher computation priorities first
      #                                 										 ) calc_rank
      #                                 	from p_rsf.rsf_data_calculation_evaluations dce 
      #                                 	
      #                                 	-- does not judge on subscription status to calculated or not, ie, may return default calc if undefined
      #                                 	-- but entry shouldn't exist in dce if undefiend
      #                                 	inner join p_rsf.view_rsf_setup_indicator_subscriptions sis on sis.rsf_pfcbl_id = dce.rsf_pfcbl_id
      #                                 																													   and sis.indicator_id = dce.indicator_id
      #                                 									 
      #                                 	left join p_rsf.indicator_formulas indf on indf.formula_id = sis.formula_id -- left join for unit_fx indicators
      #                                 	where dce.rsf_pfcbl_id = any(select ft.to_family_rsf_pfcbl_id
      #                                 															 from p_rsf.view_rsf_pfcbl_id_family_tree ft
      #                                 															 where ft.from_rsf_pfcbl_id = $1::int)
      #                                 		and coalesce(dce.calculation_asof_date <= $2::date,true)
      #                                 		and sis.is_calculated is true
      #                               ) x
      #                               where x.calc_rank = 1
      #                             ) calc 
      #                             inner join p_rsf.indicators ind on ind.indicator_id = calc.indicator_id
      #                             left join lateral (select
      #                             									rdc.data_id as current_data_id,
      #                             									rdc.data_value as current_data_value,
      #                             									rdc.data_unit as current_data_unit,
      #                             									rdc.reporting_asof_date as current_data_date
      #                             								 from p_rsf.rsf_data_current rdc
      #                             								 where rdc.rsf_pfcbl_id = calc.rsf_pfcbl_id
      #                             									 and rdc.indicator_id = calc.indicator_id
      #                             									 and rdc.reporting_asof_date <= calc.calculation_asof_date
      #                             								 order by rdc.reporting_asof_date desc
      #                             								 limit 1) cd on true
      #                             																		
      #                             left join lateral (select 
      #                             									 lcu.data_unit_value,
      #                             									 lcu.reporting_asof_date as lcu_current_date
      #                             								 from p_rsf.rsf_data_current_lcu lcu
      #                             								 where lcu.for_rsf_pfcbl_id = calc.rsf_pfcbl_id
      #                             									 and lcu.reporting_asof_date <= calc.calculation_asof_date
      #                             								 order by lcu.reporting_asof_date desc
      #                             								 limit 1) lcu on true																
      #                             left join p_rsf.rsf_data rd on rd.data_id = cd.current_data_id
      #                             left join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
      #                             ",
      #                            params=list(rsf_pfcbl_id.family,
      #                                        as.character(limit_future)))
      
      setDT(calculations)
      if (empty(calculations)) return (NULL)
    
    if(SYS_PRINT_TIMING) debugtime("db_program_get_stale_calculations","Done!",as.numeric(Sys.time()-t1,"secs"))
    return (calculations)
}