db_indicator_create <- function(pool) {
  new_indicator <- dbGetQuery(pool,
                              "insert into p_rsf.indicators(indicator_name,data_category,data_type)
                                    select
                                      'rsf_new_indicator' || coalesce(max(substring(newind.indicator_name from '^rsf_new_indicator([[:digit:]]+)$')::int),0)+1 as indicator_name,
                                      'program' as data_category,
                                      'number' as data_type
                                    from p_rsf.indicators newind
                                    where newind.indicator_name ~ E'^rsf_new_indicator[[:digit:]]+$'
                                        having count(*) < 5 -- limit of pending new indicators that can be created
                                        returning indicator_id,indicator_name")
  if (empty(new_indicator)) {
    new_indicator <- dbGetQuery(pool,
                                "select indicator_id,indicator_name
                                 from p_rsf.indicators
                                 where indicator_name ~ E'^rsf_new_indicator[[:digit:]]+$'
                                order by indicator_id
                                limit 1;")
  }
  print(paste0("New indicator: ",new_indicator$indicator_id))
  if (empty(new_indicator)) return (NULL)
  else return (new_indicator)
}