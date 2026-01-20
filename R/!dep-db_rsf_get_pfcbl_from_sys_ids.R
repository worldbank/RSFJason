db_rsf_get_pfcbl_from_sys_ids <- function(pool,
                                          sys_ids,
                                          include.sysnames=FALSE) {
  stop("DEPRECATED!")
  valid_cols <- c("reporting_SYSID","pfcbl_category")
  invalid_cols <- setdiff(union(valid_cols,names(sys_ids)),valid_cols)
  if (length(invalid_cols) >0) stop(paste0("Expecting sys_ids to have two columns: reporting_SYSID and pfcbl_category.  Invalid columns: ",paste0(invalid_cols,collapse=", ")))
  
  sys_ids <- unique(sys_ids)
  
  pfcbl_ids <- poolWithTransaction(pool, function(conn) {
    
    dbExecute(conn,
              'create TEMP table _temp_sys_ids("reporting_SYSID" int,
                                              pfcbl_category text)
              on COMMIT DROP;')
    
    dbAppendTable(conn,
                  name="_temp_sys_ids",
                  value=sys_ids)
    
    pfcbl_ids <- NULL
    if (include.sysnames==TRUE) {
      pfcbl_ids <- dbGetQuery(conn,
                              'select 
                                ids."reporting_SYSID",
                                ids.pfcbl_category,
                                fam.parent_rsf_pfcbl_id as rsf_pfcbl_id,
                                snames.sys_name,
                                rids.rsf_program_id,
                                rids.rsf_facility_id,
                                rids.rsf_client_id,
                                rids.rsf_borrower_id,
                                rids.rsf_loan_id
                              from _temp_sys_ids ids
                              inner join p_rsf.rsf_pfcbl_id_family fam on fam.child_rsf_pfcbl_id = ids."reporting_SYSID"
                              																			  and fam.parent_pfcbl_category = ids.pfcbl_category
                              inner join p_rsf.rsf_pfcbl_ids rids on rids.rsf_pfcbl_id = fam.parent_rsf_pfcbl_id
                              inner join p_rsf.view_rsf_pfcbl_id_current_sys_names snames on snames.rsf_pfcbl_id = fam.parent_rsf_pfcbl_id')
    } else {
      pfcbl_ids <- dbGetQuery(conn,
                              'select 
                                ids."reporting_SYSID",
                                ids.pfcbl_category,
                                fam.parent_rsf_pfcbl_id as rsf_pfcbl_id,
                                rids.rsf_program_id,
                                rids.rsf_facility_id,
                                rids.rsf_client_id,
                                rids.rsf_borrower_id,
                                rids.rsf_loan_id

                              from _temp_sys_ids ids
                              inner join p_rsf.rsf_pfcbl_id_family fam on fam.child_rsf_pfcbl_id = ids."reporting_SYSID"
                              																			  and fam.parent_pfcbl_category = ids.pfcbl_category
                              inner join p_rsf.rsf_pfcbl_ids rids on rids.rsf_pfcbl_id = fam.parent_rsf_pfcbl_id')
    }
    return (pfcbl_ids)
  })
  setDT(pfcbl_ids)
  
  pfcbl_ids <- pfcbl_ids[sys_ids,
                         on=.(reporting_SYSID,
                              pfcbl_category)]
  return (pfcbl_ids)
}