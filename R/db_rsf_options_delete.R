db_rsf_options_delete <- function(pool,options_group_id) {
    dbExecute(pool,"with options_data as (
  select iog.options_group_id,iog.options_group_name,coalesce(count(distinct rd.data_id),0) as data_count
  from p_rsf.indicator_options_groups iog
  left join p_rsf.indicator_options_group_keys iogk on iogk.options_group_id = iog.options_group_id
  left join p_rsf.indicators ind on ind.indicator_options_group_id = iog.options_group_id
  left join p_rsf.rsf_data rd on rd.indicator_id = ind.indicator_id 
  													 and rd.data_sequence_rank = 1
  													 and rd.data_value = iogk.options_group_key
  group by iog.options_group_id,iog.options_group_name
  )
  delete from p_rsf.indicator_options_groups iog
  where iog.options_group_id = $1::int
  and exists(select * from options_data od where od.options_group_id = iog.options_group_id and od.data_count = 0)",
    params=list(options_group_id))
}