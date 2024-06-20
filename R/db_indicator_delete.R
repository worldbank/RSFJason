db_indicator_delete <- function(pool,
                                    indicator_id) {

  poolWithTransaction(pool,function(conn) {

    dbExecute(conn,"
              delete from p_rsf.rsf_data d_rd
              where d_rd.indicator_id = $1::int
                and not exists(select * from p_rsf.rsf_data rd
                               inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = rd.rsf_pfcbl_id
                               where rd.indicator_id = d_rd.indicator_id
                                 and ids.created_by_reporting_cohort_id <> rd.reporting_cohort_id)",
              params=list(indicator_id))
    
    id <- dbGetQuery(conn,"
              delete from p_rsf.indicators ind
              where ind.indicator_id = $1::int
                and not exists(select * from p_rsf.rsf_data rd
                               inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = rd.rsf_pfcbl_id
                               where rd.indicator_id = ind.indicator_id
                                 and ids.created_by_reporting_cohort_id <> rd.reporting_cohort_id)
              returning indicator_id",
               params=list(indicator_id))
    return (!empty(id))
  })
  
  return (deleted)
}