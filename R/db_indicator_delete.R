db_indicator_delete <- function(pool,
                                    indicator_id) {

  #conn <- poolCheckout(pool)
  #dbBegin(conn)
  
  deleted <- poolWithTransaction(pool,function(conn) {

    dbExecute(conn,"
              delete from p_rsf.rsf_data d_rd
              where d_rd.indicator_id = $1::int",
              params=list(indicator_id))
    
    id <- dbGetQuery(conn,"
              delete from p_rsf.indicators ind
              where ind.indicator_id = $1::int
              returning indicator_id",
               params=list(indicator_id))
    return (!empty(id))
  })
  
  return (deleted)
}