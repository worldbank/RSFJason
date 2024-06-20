db_create_entity_indicator_requirements <- function(pool,
                                                    entity_type) {
  
  indicator_requirements <- dbGetQuery(pool,"
                                       select
                                         indicator_id,
                                         indicator_name,
                                         indicator_sys_category,
                                         data_category,
                                         default_value,
                                         data_type,
                                         is_setup
                                       from (
                                         select
                                           ind.indicator_id,
                                           ind.indicator_name,
                                           ind.data_category,
                                           ind.default_value,
                                           ind.data_type,
                                           coalesce(ind.is_setup,'optional') as is_setup,
                                           ind.indicator_sys_category
                                         from p_rsf.indicators ind
                                         where ind.data_category = $1::text
                                           and (ind.is_setup IS NOT NULL 
                                                and 
                                                ind.is_system = false) -- programs have few indicators and presenta ll
                                          
                                         
                                         --union 
                                         
                                         --select 
                                          -- 0 as indicator_id,
                                          -- $1::text || '_creation_date' as indicator_name,
                                          -- $1::text as data_category,
                                          -- NULL::text as default_value,
                                          -- 'date' as data_type,
                                          -- 'required' as is_setup,
                                          -- 'entity_creation_date' as indicator_sys_category
                                         --where not exists(select * from p_rsf.indicators ind
                                          --                where ind.data_category = $1::text
                                          --                  and ind.is_setup IS NOT NULL
                                          --                  and ind.indicator_sys_category = 'entity_creation_date')
                                       ) ir
                                       order by 
                                        ir.is_setup='required' desc,
                                        coalesce(ir.indicator_sys_category = 'entity_creation_date',false) desc,
                                        ir.indicator_name",
                                      params=list(entity_type))
  setDT(indicator_requirements)
  return (indicator_requirements)
}