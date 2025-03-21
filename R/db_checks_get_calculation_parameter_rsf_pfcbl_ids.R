db_checks_get_calculation_parameter_rsf_pfcbl_ids <- function(pool,
                                                              check_rsf_pfcbl_ids,
                                                              check_formula_ids,
                                                              check_asof_date) {
  
  parameter_rsf_pfcbl_ids <- dbGetQuery(pool,"
    with calcs as (
      select 
      pcs.rsf_pfcbl_id,
      pcs.indicator_check_id,
      pcs.check_formula_id,
      cpc.parent_pfcbl_category,
      cpc.for_pfcbl_category as calculate_pfcbl_category,
      cpc.parameter_pfcbl_category,
      cpc.parameter_pfcbl_hierarchy
    from p_rsf.view_rsf_pfcbl_check_subscriptions pcs
    inner join p_rsf.compute_check_to_parameter_categories cpc on cpc.check_formula_id = pcs.check_formula_id
    where pcs.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
    and pcs.check_formula_id = any(select unnest(string_to_array($2::text,','))::int)
    and cpc.parameter_pfcbl_hierarchy <> 'self'
    ),
    parameter_ids as materialized (
      select 
      ft.to_family_rsf_pfcbl_id as rsf_pfcbl_id
      from calcs
      inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = calcs.rsf_pfcbl_id
                                                       and ft.from_pfcbl_category = calcs.calculate_pfcbl_category
      where ft.pfcbl_hierarchy = calcs.parameter_pfcbl_hierarchy
        and ft.to_pfcbl_category = calcs.parameter_pfcbl_category
        

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
                   and ids.created_in_reporting_asof_date <= $3::date)
  ",params=list(paste0(unique(check_rsf_pfcbl_ids),collapse=","),
                paste0(unique(check_formula_ids),collapse=","),
                as.character(check_asof_date)))
  
  return (unique(c(check_rsf_pfcbl_ids,parameter_rsf_pfcbl_ids$rsf_pfcbl_id)))
}