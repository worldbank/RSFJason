
db_get_rsf_pfcbl_id_by_sys_name <- function(pool,
                                         sys_names,
                                         rsf_program_id=NA,
                                         rsf_pfcbl_id.family_tree=NA,
                                         include.global=TRUE,
                                         error.if.missing=TRUE) {
  
  #ensure no delimiters exist in the name...they should be normalized in the normalize_SystemNames function when coming from database.
  #But perhasp a user does something in an upload file?
  if (any(grepl(",",sys_names))) {
    stop(paste0("Failed to lookup SYSNAME due to malformatted name: ',' and '>' in name are not allowed: ",
                paste0(grep("[,>]",sys_names,value = T),collapse=" & ")))
  }

  #conn <- poolCheckout(pool)
  #dbBegin(conn)
  sysids <- poolWithTransaction(pool=pool,function(conn) {
    dbExecute(conn,"create temp table _sysids(sys_name text,
                                              rsf_pfcbl_id int default NULL)
              on commit drop;")
    
    dbAppendTable(conn,
                  name="_sysids",
                  value=data.frame(sys_name=unique(sys_names)))
    
    dbExecute(conn,"
      update _sysids sys
      set rsf_pfcbl_id = nai.rsf_pfcbl_id
      from p_rsf.rsf_data_current_names_and_ids nai
      where nai.sys_name = sys.sys_name")
    
    dbExecute(conn,"
      update _sysids sys
      set rsf_pfcbl_id = p_rsf.get_rsf_pfcbl_id_by_sys_name(sys.sys_name)
      where sys.rsf_pfcbl_id is null")
    
    
    if (!is.na(rsf_pfcbl_id.family_tree)) {
      dbExecute(conn,"
        delete from _sysids sys
        where sys.rsf_pfcbl_id is not null
          and not (sys.rsf_pfcbl_id = any(select ft.to_family_rsf_pfcbl_id
                                          from p_rsf.view_rsf_pfcbl_id_family_tree ft 
                                          where ft.from_rsf_pfcbl_id= $1::int))",
                params=list(rsf_pfcbl_id.family_tree))
    }
    
    dbGetQuery(conn,"
      select 
        sys.sys_name as lookup_sys_name,
        ids.rsf_pfcbl_id,
        ids.rsf_program_id,
        ids.pfcbl_category
      from _sysids sys
      left join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = sys.rsf_pfcbl_id;
    ")
  })
  
  setDT(sysids)
  
  if (error.if.missing==TRUE &&
      anyNA(sysids$rsf_pfcbl_id)) {
    
    bad_names <- sysids[is.na(rsf_pfcbl_id),unique(lookup_sys_name)]
    stop("Failed to lookup SYSNAME for: ",paste0(bad_names,collapse=" & "))
  }
  
  if (!is.na(rsf_program_id)) {
    
    bad_matches <- NULL
    if (include.global==TRUE) {
      test_rsf_program_id <- rsf_program_id
      bad_matches <- unique(sysids[!rsf_program_id %in% c(test_rsf_program_id,0,NA)])
    } else {
      test_rsf_program_id <- rsf_program_id
      bad_matches <- unique(sysids[!rsf_program_id %in% c(test_rsf_program_id,NA)])
    }
    
    if (!empty(bad_matches)) {
      stop(paste0("Multiple programs found by sys_name lookup. Requested: ",rsf_program_id," and found: ",
                  paste0(unique(as.character(bad_matches$lookup_sys_name)),collapse=", ")))
    }
  }
  
  # 
  # sysids <- dbGetQuery(pool,"
  #           with matches as (
  #             select p_rsf.get_rsf_pfcbl_id_by_sys_name(sn.sys_name) as rsf_pfcbl_id,sn.sys_name
  #             from (select unnest(string_to_array($2::text,',')) as sys_name) sn
  #           )
  #           select
  #             mat.sys_name,
  #             ids.rsf_pfcbl_id
  #           from matches mat
  #           left join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = mat.rsf_pfcbl_id
  #           where (ids.rsf_pfcbl_id is NULL or 
  #                  ids.rsf_program_id = $1::int or 
  #                  ids.pfcbl_category = 'global')",
  #                      params=list(template$rsf_program_id,
  #                                  paste0(unique(sys_names),collapse=",")))
  return (sysids)
}