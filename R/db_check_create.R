db_check_create <- function(pool) {
  new_check <- dbGetQuery(pool,"
    insert into p_rsf.indicator_checks(check_name,is_system,check_class,check_type,check_pfcbl_category)
    select 
      'rsf_new_indicator_check' || coalesce(max(substring(newind.check_name from '^rsf_new_indicator_check([[:digit:]]+)$')::int),0)+1 as check_name,
      false as is_system,
      'warning',
      'none',
      'loan'
    from p_rsf.indicator_checks newind
    where newind.check_name ~ E'^rsf_new_indicator_check[[:digit:]]+$'
    having count(*) < 5 -- limit of pending new indicator checks that can be created
    returning indicator_check_id")
  
  if (empty(new_check)) {
    new_check <- dbGetQuery(pool,"
      select ic.indicator_check_id
      from p_rsf.indicator_checks ic
      where ic.indicator_name ~ E'^rsf_new_indicator[[:digit:]]+$'
      order by ic.indicator_check_id asc
      limit 1")
  }
  
  return (new_check$indicator_check_id)
}
