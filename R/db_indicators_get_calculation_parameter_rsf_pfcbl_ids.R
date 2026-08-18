db_indicators_get_calculation_parameter_rsf_pfcbl_ids <- function(pool,
                                                                  rsf_pf_id,
                                                                  calculate_rsf_pfcbl_ids,
                                                                  calculate_indicator_ids,
                                                                  calculate_asof_date) {
  
  # parameter_rsf_pfcbl_ids <- dbGetQuery(pool,"
  #   select distinct 
  #     ft.to_family_rsf_pfcbl_id as parameter_rsf_pfcbl_id
  #   from (
  #   select distinct
  #     req.from_rsf_pf_id,
  #     calculation.from_rsf_pfcbl_id as calculate_from_rsf_pfcbl_id,
  #     dce.calculation_asof_date,  
  #     parameter_pfcbl_rank
  #   from p_rsf.view_rsf_pf_calculation_requirements req
  #   inner join p_rsf.rsf_data_calculation_evaluations dce on dce.rsf_pf_id = req.to_calculate_pf_id
  #                                                        and dce.indicator_id = req.indicator_id
  #   inner join p_rsf.rsf_pfcbl_ids calc on calc.rsf_pfcbl_id = dce.rsf_pfcbl_id
  #   cross join lateral (values (calc.rsf_gpfcbl_family[req.formula_calculate_from_pfcbl_rank+1])) as calculation(from_rsf_pfcbl_id) 
  #   cross join lateral unnest(req.formula_pfcbl_rank_range) as parameter_pfcbl_rank
  #   where req.from_rsf_pf_id = $1::int
  #     and req.indicator_id = any($2::int[])
  #     and dce.calculation_asof_date = $3::date
  #   ) as calc
  #   inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = calc.calculate_from_rsf_pfcbl_id
  #                                                    and ft.to_pfcbl_rank = parameter_pfcbl_rank
  #   where exists(select true 
  #                from p_rsf.rsf_pfcbl_ids params 
  #                where params.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
  #                  and params.created_in_reporting_asof_date <= calc.calculation_asof_date)",
  #   params=list(rsf_pf_id,
  #               dbMakeIntArray(calculate_indicator_ids),
  #               as.character(calculate_asof_date)))
  
  parameter_rsf_pfcbl_ids <- dbGetQuery(pool,"
  select distinct 
      ft.to_family_rsf_pfcbl_id as parameter_rsf_pfcbl_id
    from (
      select distinct
        pcf.from_rsf_pf_id,
        calculation.from_rsf_pfcbl_id as calculate_from_rsf_pfcbl_id,
        parameter_pfcbl_rank,
        pcf.formula_calculate_from_pfcbl_rank
      from p_rsf.view_rsf_pf_calculation_requirements pcf     
      inner join p_rsf.rsf_pfcbl_ids calc on calc.rsf_pf_id = pcf.to_calculate_pf_id
                                         and calc.pfcbl_category_rank = pcf.data_category_rank
      cross join lateral (values (calc.rsf_gpfcbl_family[pcf.formula_calculate_from_pfcbl_rank+1])) as calculation(from_rsf_pfcbl_id) 
      cross join lateral unnest(pcf.formula_pfcbl_rank_range) as parameter_pfcbl_rank
      where pcf.from_rsf_pf_id = $1::int
        and pcf.indicator_id = any($3::int[])
        and calc.rsf_pfcbl_id = any($2::int[])
        
    ) as calc
    inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = calc.calculate_from_rsf_pfcbl_id
                                                     and ft.to_pfcbl_rank = parameter_pfcbl_rank
    where exists(select true 
                 from p_rsf.rsf_pfcbl_ids params 
                 where params.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
                   and params.created_in_reporting_asof_date <= $4::date)",
    params=list(rsf_pf_id,
                dbMakeIntArray(calculate_rsf_pfcbl_ids),
                dbMakeIntArray(calculate_indicator_ids),
                as.character(calculate_asof_date)))
  
  return (c(parameter_rsf_pfcbl_ids$parameter_rsf_pfcbl_id))
  
  # parameter_rsf_pfcbl_ids <- dbGetQuery(pool,"
  # 
  #   with calcs as (
  #     select 
  #     sis.rsf_pfcbl_id,
  #     sis.indicator_id,
  #     sis.formula_id,
  #     cpc.parent_pfcbl_category,
  #     cpc.calculate_pfcbl_category,
  #     cpc.parameter_pfcbl_category,
  #     cpc.parameter_pfcbl_hierarchy
  #     from p_rsf.view_rsf_setup_indicator_subscriptions sis
  #     inner join p_rsf.compute_calculation_to_parameter_categories cpc on cpc.calculate_formula_id = sis.formula_id
  #     where sis.rsf_pfcbl_id = any($1::int[])
  #       and sis.indicator_id = any($2::int[])
  #       and cpc.parameter_pfcbl_hierarchy <> 'self' -- because calculate_rsf_pfcbl_ids will be merged-back in afterward.
  #   ),
  #   parameter_ids as materialized (
  #     select 
  #     ft.to_family_rsf_pfcbl_id as rsf_pfcbl_id
  #     from calcs
  #     inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = calcs.rsf_pfcbl_id
  #                                                      and ft.from_pfcbl_category = calcs.calculate_pfcbl_category
  #     where ft.pfcbl_hierarchy = calcs.parameter_pfcbl_hierarchy
  #       and ft.to_pfcbl_category = calcs.parameter_pfcbl_category
  #       
  #     union 
  #     
  #     select 
  #     ft.to_family_rsf_pfcbl_id as rsf_pfcbl_id
  #     
  #     from
  #     (select distinct  
  #     (array[ids.rsf_program_id,ids.rsf_facility_id,ids.rsf_client_id,ids.rsf_borrower_id,ids.rsf_loan_id])[rpc.pfcbl_rank] as related_pfcbl_id,
  #     calcs.parent_pfcbl_category as calculate_pfcbl_category,
  #     calcs.parameter_pfcbl_category
  #     from calcs
  #     inner join p_rsf.rsf_pfcbl_categories rpc on rpc.pfcbl_category = calcs.parent_pfcbl_category
  #     inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = calcs.rsf_pfcbl_id
  #     where calcs.parameter_pfcbl_hierarchy = 'relative'
  #     ) as related
  #     inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = related.related_pfcbl_id
  #                                                      and ft.from_pfcbl_category = related.calculate_pfcbl_category
  #     where ft.to_pfcbl_category = related.parameter_pfcbl_category
  #       and ft.pfcbl_hierarchy = 'child'
  #   )
  #   select rsf_pfcbl_id 
  #   from parameter_ids
  #   where exists(select * from p_rsf.rsf_pfcbl_ids ids
  #                where ids.rsf_pfcbl_id = parameter_ids.rsf_pfcbl_id
  #                  and ids.created_in_reporting_asof_date <= $3::date)",
    # params=list(dbMakeIntArray(calculate_rsf_pfcbl_ids),
    #             dbMakeIntArray(calculate_indicator_ids),
    #             as.character(calculate_asof_date)))

  return (unique(c(calculate_rsf_pfcbl_ids,parameter_rsf_pfcbl_ids$rsf_pfcbl_id)))
}