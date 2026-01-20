db_indicators_get_calculation_parameter_rsf_pfcbl_ids <- function(pool,
                                                                  calculate_rsf_pfcbl_ids,
                                                                  calculate_indicator_ids,
                                                                  calculate_asof_date) {
  
  parameter_rsf_pfcbl_ids <- dbGetQuery(pool,"

    with calcs as (
      select 
      sis.rsf_pfcbl_id,
      sis.indicator_id,
      sis.formula_id,
      cpc.parent_pfcbl_category,
      cpc.calculate_pfcbl_category,
      cpc.parameter_pfcbl_category,
      cpc.parameter_pfcbl_hierarchy
      from p_rsf.view_rsf_setup_indicator_subscriptions sis
      inner join p_rsf.compute_calculation_to_parameter_categories cpc on cpc.calculate_formula_id = sis.formula_id
      where sis.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
        and sis.indicator_id = any(select unnest(string_to_array($2::text,','))::int)
        and cpc.parameter_pfcbl_hierarchy <> 'self' -- because calculate_rsf_pfcbl_ids will be merged-back in afterward.
    ),
    parameter_ids as materialized (
      select 
      ft.to_family_rsf_pfcbl_id as rsf_pfcbl_id
      from calcs
      inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = calcs.rsf_pfcbl_id
                                                       and ft.from_pfcbl_category = calcs.calculate_pfcbl_category
      where ft.pfcbl_hierarchy = calcs.parameter_pfcbl_hierarchy
        and ft.to_pfcbl_category = calcs.parameter_pfcbl_category
        
        --moved to materialized subquery below -- we don't need to extract parameters that don't have any data yet
        --if we're doing an historic update, it would pull-in future entities
        --and exists(select * from p_rsf.rsf_pfcbl_ids x where x.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_Id and x.created_in_reporting_asof_date <= '2023-12-31'::date)
        
      union 
      
      select 
      ft.to_family_rsf_pfcbl_id as rsf_pfcbl_id
      
      from
      (select distinct  
      (array[ids.rsf_program_id,ids.rsf_facility_id,ids.rsf_client_id,ids.rsf_borrower_id,ids.rsf_loan_id])[rpc.pfcbl_rank] as related_pfcbl_id,
      calcs.parent_pfcbl_category as calculate_pfcbl_category,
      calcs.parameter_pfcbl_category
      from calcs
      inner join p_rsf.rsf_pfcbl_categories rpc on rpc.pfcbl_category = calcs.parent_pfcbl_category
      inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = calcs.rsf_pfcbl_id
      where calcs.parameter_pfcbl_hierarchy = 'relative'
      ) as related
      inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = related.related_pfcbl_id
                                                       and ft.from_pfcbl_category = related.calculate_pfcbl_category
      where ft.to_pfcbl_category = related.parameter_pfcbl_category
        and ft.pfcbl_hierarchy = 'child'
    )
    select rsf_pfcbl_id 
    from parameter_ids
    where exists(select * from p_rsf.rsf_pfcbl_ids ids
                 where ids.rsf_pfcbl_id = parameter_ids.rsf_pfcbl_id
                   and ids.created_in_reporting_asof_date <= $3::date)",
    params=list(paste0(unique(calculate_rsf_pfcbl_ids),collapse=","),
                paste0(unique(calculate_indicator_ids),collapse=","),
                as.character(calculate_asof_date)))
  
  return (unique(c(calculate_rsf_pfcbl_ids,parameter_rsf_pfcbl_ids$rsf_pfcbl_id)))
}