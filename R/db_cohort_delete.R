#cohort_ids <- 39916
#cohort_ids <- c(39883,39851)
db_cohort_delete <- function(pool,
                             cohort_ids) {
  stop("Deprecated")
  cohort_ids <- as.numeric(cohort_ids)
  cohort_ids <- unique(cohort_ids[!is.na(cohort_ids)])
  if (length(cohort_ids)==0) return (NULL)
  

  #conn <- poolCheckout(DBPOOL); 
  #dbBegin(conn) 
  #dbRollback(conn);dbBegin(conn)
  if(SYS_PRINT_TIMING)  debugtime("db_cohort_delete",reset=FALSE)
  
  #delete_asof_time <- dbGetQuery(pool,"select (timeofday()::timestamptz)::text")
  affected_reporting <- poolWithTransaction(pool,function(conn) {
    
    affected_reporting <- tryCatch({
  
      #dbExecute(conn,"create table _temp_delete_cohorts(reporting_cohort_id int,primary key (reporting_cohort_id))")
      
      dbExecute(conn,"create temp table _temp_delete_cohorts(reporting_cohort_id int,
                                                                primary key (reporting_cohort_id))
                      on commit drop;")
      
      dbAppendTable(conn,
                    name="_temp_delete_cohorts",
                    value=data.table(reporting_cohort_id=unique(cohort_ids)))

      dbExecute(conn,"insert into _temp_delete_cohorts(reporting_cohort_id)
                      select reporting_cohort_id
                      from p_rsf.reporting_cohorts rc
                      where exists(select * from _temp_delete_cohorts tdci
                                   where tdci.reporting_cohort_id = rc.parent_reporting_cohort_id
                                      or tdci.reporting_cohort_id = rc.linked_reporting_cohort_id)
                      on conflict do nothing;")
      
      #re <- dbGetQuery(conn,"select  from _temp_delete_cohorts")
      dbExecute(conn,"create temp table _temp_affected_entities(rsf_program_id int,
                                                                rsf_pfcbl_id int,
                                                                reporting_asof_date date,
                                                                pfcbl_category text,
                                                                reporting_rsf_pfcbl_id_deleted bool)
                      on commit drop;")
      
      
      dbExecute(conn,"
                      insert into _temp_affected_entities(rsf_program_id,
                                                          rsf_pfcbl_id,
                                                          reporting_asof_date,
                                                          pfcbl_category,
                                                          reporting_rsf_pfcbl_id_deleted)
                      select distinct
                        rc.rsf_program_id,
                        fam.parent_rsf_pfcbl_id as rsf_pfcbl_id,
                        rc.reporting_asof_date,
                        fam.parent_pfcbl_category as pfcbl_category,
                        fam.parent_rsf_pfcbl_id = rc.reporting_rsf_pfcbl_id as reporting_rsf_pfcbl_id_deleted
                      from _temp_delete_cohorts tdc
                      inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = tdc.reporting_cohort_id
                      inner join p_rsf.rsf_pfcbl_id_family fam on fam.child_rsf_pfcbl_id = rc.reporting_rsf_pfcbl_id")
      #ae <- dbGetQuery(conn,"select * from _temp_affected_entities")
      #dbExecute(conn,"delete from _temp_affected_entities")
      
      nx <- dbExecute(conn,"delete from p_rsf.reporting_cohorts rc
                            where exists(select * 
                                         from _temp_delete_cohorts dci 
                                         where dci.reporting_cohort_id = rc.reporting_cohort_id)")
     
      
      affected_entities <- dbGetQuery(conn,"select 
                                            tae.rsf_program_id,
                                            tae.rsf_pfcbl_id,
                                            tae.reporting_asof_date,
                                            tae.pfcbl_category
                                            from  _temp_affected_entities tae
                                            where exists(select * from p_rsf.rsf_data rd
                                                         where rd.rsf_pfcbl_id = tae.rsf_pfcbl_id
                                                           and rd.reporting_asof_date = tae.reporting_asof_date)
                                           except 
                                           
                                           select
                                            tae.rsf_program_id,
                                            fam.parent_rsf_pfcbl_id as rsf_pfcbl_id,
                                            tae.reporting_asof_date,
                                            fam.parent_pfcbl_category
                                            from  _temp_affected_entities tae
                                            inner join p_rsf.rsf_pfcbl_id_family fam on fam.child_rsf_pfcbl_id = tae.rsf_pfcbl_id
                                            where fam.parent_pfcbl_rank < fam.child_pfcbl_rank
                                            and exists(select * from p_rsf.rsf_data rd
                                                         where rd.rsf_pfcbl_id = fam.child_rsf_pfcbl_id
                                                           and rd.reporting_asof_date = tae.reporting_asof_date)
                                      ")

      return(affected_entities)
     
    },
    error = function(e) {
      print(conditionMessage(e))
      print("db_cohort_delete: ERROR ROLLING BACK TRANSACTION")
      stop(e)
    },
    warning = function(w) {
      print(conditionMessage(w))
      print("db_cohort_delete: WARNING ROLLING BACK TRANSACTION")
      stop(w)
    })    
  })

  setDT(affected_reporting)

  if(SYS_PRINT_TIMING)  debugtime("db_cohort_delete","Done!")

  return (affected_reporting)
}