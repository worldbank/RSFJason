template_set_data_match_pfcbl_ids <- function(pool,
                                              template) {
  


  lookup <- unique(template$template_data[,
                                          .(reporting_template_row_group,
                                            pfcbl_category=data_category,
                                            SYSID)])
  

  if (anyNA(lookup$SYSID)) stop(paste0("NA lookup IDs for reporting_rsf_pfcbl_id found in template_match_data.  If template expects to {CREATE} new entities, these should be specified with {CREATE} and the template parser should assign a pseudo ID using a negative value"))
  #lookup[SYSID < 0]  
  lookup <- lookup[SYSID > 0]  
  
  #if they're all {CREATE}
  if (empty(lookup)) {
    return (template)
  }
  
  # lookup <- lookup[,
  #                  .(reporting_template_row_groups=paste0(reporting_template_row_group,collapse=",")),
  #                  by=.(pfcbl_category,
  #                       sysid=SYSID)][,.(sysid,pfcbl_category,reporting_template_row_groups)]
  lookup <- lookup[,
                   .(reporting_template_row_groups=list(reporting_template_row_group)),
                   by=.(pfcbl_category,
                        SYSID)][,.(SYSID,pfcbl_category,reporting_template_row_groups)]
  #conn <- poolCheckout(pool)
  #dbBegin(conn)
  #dbRollback(conn)
  match_ids <- poolWithTransaction(pool,function(conn) {
    dbExecute(conn,"create temp table _lookup(sysid int,
                                              pfcbl_category text)
              on commit drop;")
    dbAppendTable(conn,
                  name="_lookup",
                  value=lookup[,.(sysid=SYSID,pfcbl_category)])
    
    dbExecute(conn,"analyze _lookup")
    
    dbGetQuery(conn,"
               select 
                 lk.pfcbl_category,
                 lk.sysid,
                 fam.parent_rsf_pfcbl_id as rsf_pfcbl_id,
                 ids.parent_rsf_pfcbl_id
                 
               from _lookup lk
               inner join p_rsf.rsf_pfcbl_id_family fam on fam.child_rsf_pfcbl_id = lk.sysid
                                                       and fam.parent_pfcbl_category = lk.pfcbl_category
               inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = fam.parent_rsf_pfcbl_id")
               #where ids.created_in_reporting_asof_date <= $1::date NO, because pfcbl templates can report into the future
               #",
               #params=list(template$reporting_asof_date))
    
  })
  setDT(match_ids)
  lookup[match_ids,
         `:=`(rsf_pfcbl_id=i.rsf_pfcbl_id,
              parent_rsf_pfcbl_id=i.parent_rsf_pfcbl_id),
         on=.(SYSID=sysid,
              pfcbl_category)]
  
  
  lookup <- lookup[,
                   .(reporting_template_row_group=unlist(reporting_template_row_groups,recursive=T)),
                   by=.(SYSID,
                        pfcbl_category,
                        rsf_pfcbl_id,
                        parent_rsf_pfcbl_id)]
  
  
 
  template$match_results[lookup[!is.na(rsf_pfcbl_id)],
                         `:=`(parent_rsf_pfcbl_id=i.parent_rsf_pfcbl_id,
                              rsf_pfcbl_id=i.rsf_pfcbl_id,
                              match_action="update",
                              matched_by="SYSID"),
                         on=.(reporting_template_row_group,
                              pfcbl_category)]
  
 return (template)
}