db_data_get_info <- function(pool,data_id) {
  

  data_info <- dbGetQuery(pool,"select 
                                rd.data_id,
                                rd.reporting_asof_date,
                                rd.indicator_id,
                                ind.indicator_name,
                                ind.is_calculated,
                                case when coalesce(rd.data_value,'') <> coalesce(rd.data_submitted,'') 
                                     then 'Data in system: ' || coalesce(rd.data_value,'{MISSING}') || ' vs Data reported: ' || coalesce('\"' || rd.data_submitted || '\"','{MISSING}')
                                		 else coalesce(rd.data_value,'{MISSING}')
                                end as data_value,
                                rd.data_sequence_rank,
                                rd.reporting_sequence_rank,
                                snames.sys_name,
                                rc.source_name,
                                vai.users_name as reporting_cohort_users_name,
                                vai.is_system_account
                                from p_rsf.rsf_data rd
                                inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
                                inner join p_rsf.indicators ind on ind.indicator_id = rd.indicator_id
                                inner join p_rsf.rsf_pfcbl_ids ids_c on ids_c.rsf_pfcbl_id = rc.reporting_rsf_pfcbl_id
                                left join p_rsf.view_rsf_pfcbl_id_current_sys_names snames on snames.rsf_pfcbl_id = rd.rsf_pfcbl_id
                                left join p_rsf.view_account_info vai on vai.account_id = reporting_user_id
                                where rd.data_id = $1::int",
                          params=list(data_id))
  
  if (empty(data_info)) return (NA)
  
  return (as.data.table(data_info))
}
