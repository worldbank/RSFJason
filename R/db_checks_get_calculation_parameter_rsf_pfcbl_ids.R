db_checks_get_calculation_parameter_rsf_pfcbl_ids <- function(pool,
                                                              rsf_pf_id,
                                                              check_asof_date,
                                                              check_formula_ids) {
  
  
  parameter_rsf_pfcbl_ids <- dbGetQuery(pool,"
        
    with requirements as materialized (
    select distinct
        req.from_rsf_pf_id,
        req.formula_calculate_from_pfcbl_rank,
        req.check_formula_id,
        $2::date as check_asof_date,
        req.to_rsf_pf_id,
        req.formula_pfcbl_rank_range    
      from p_rsf.view_rsf_pf_check_requirements req
      where req.from_rsf_pf_id = $1::int
        and req.check_formula_id = any($3::int[])
    )
    select distinct 
      ft.to_family_rsf_pfcbl_id as parameter_rsf_pfcbl_id
    from (
      select distinct 
      req.from_rsf_pf_id,
      calculation.from_rsf_pfcbl_id as check_from_rsf_pfcbl_id,
      req.check_asof_date,  
      parameter_pfcbl_rank
      from requirements req
      inner join p_rsf.rsf_data_check_evaluations dce on dce.rsf_pf_id = req.to_rsf_pf_id
                                                     and dce.check_formula_id = req.check_formula_id
                                                     and dce.check_asof_date = req.check_asof_date
      inner join p_rsf.rsf_pfcbl_ids chk on chk.rsf_pfcbl_id = dce.rsf_pfcbl_id
      cross join lateral (values (chk.rsf_gpfcbl_family[req.formula_calculate_from_pfcbl_rank+1])) as calculation(from_rsf_pfcbl_id) 
      cross join lateral unnest(req.formula_pfcbl_rank_range) as parameter_pfcbl_rank            
    ) chk     
    inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = chk.check_from_rsf_pfcbl_id
                                                     and ft.to_pfcbl_rank = chk.parameter_pfcbl_rank
    where  exists(select true 
                  from p_rsf.rsf_pfcbl_ids ids 
                  where ids.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
                    and ids.created_in_reporting_asof_date <= chk.check_asof_date)",
    params=list(rsf_pf_id,
                as.character(check_asof_date),
                dbMakeIntArray(check_formula_ids)))
  
  # 
  # #DEC2025: no reason to query subscriptions since no check for being subscribed??
  # #original below
  # parameter_rsf_pfcbl_ids <- dbGetQuery(pool,"
  #   with calcs as (
  #     select 
  #     ids.rsf_pfcbl_id,
  #     cpc.indicator_check_id,
  #     cpc.check_formula_id,
  #     cpc.parent_pfcbl_category,
  #     cpc.for_pfcbl_category as calculate_pfcbl_category,
  #     cpc.parameter_pfcbl_category,
  #     cpc.parameter_pfcbl_hierarchy
  #   from p_rsf.rsf_pfcbl_ids ids,
  #        p_rsf.compute_check_to_parameter_categories cpc
  #   where ids.rsf_pfcbl_id = any($1::int[])
  #   and cpc.check_formula_id = any($2::int[])
  #   and cpc.parameter_pfcbl_hierarchy <> 'self'
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
  #   where exists(select true from p_rsf.rsf_pfcbl_ids ids
  #                where ids.rsf_pfcbl_id = parameter_ids.rsf_pfcbl_id
  #                  and ids.created_in_reporting_asof_date <= $3::date)
  # ",params=list(dbMakeIntArray(check_rsf_pfcbl_ids),
  #               dbMakeIntArray(check_formula_ids),
  #               as.character(check_asof_date)))
  
  return (parameter_rsf_pfcbl_ids$parameter_rsf_pfcbl_id)
}