db_reporting_import_create <- function(pool,
                                       import_rsf_pfcbl_id,
                                       import_user_id,
                                       reporting_asof_date,
                                       template_id,
                                       file_path,
                                       import_comments=NA,
                                       auto_delete_old_versions=TRUE) #only quarter is implemented 
{
  
  valid_user_id <- dbGetQuery(pool,"
                              select exists(select * from p_rsf.view_account_info vai
                                            where vai.account_id = $1::text)::bool as is_valid",
                              params=list(import_user_id))
  
  if (empty(valid_user_id) || !all(valid_user_id$is_valid)) {
    stop(paste0("Failed to import: Invalid user ID=",import_user_id))
  }
  
  auto_delete_imports <- NULL

  reporting_entity <- dbGetQuery(pool,"
    select 
      ids.pfcbl_category,
      ids.pfcbl_category_rank,
      ids.created_in_reporting_asof_date::text,
      users.rsf_pfcbl_id_validate_permissions($1::text, $2::int, 'WRITE')::bool as has_write
    from p_rsf.rsf_pfcbl_ids ids
    where ids.rsf_pfcbl_id = $2::int",
    params=list(import_user_id,
                import_rsf_pfcbl_id))
  
  if (empty(reporting_entity)) {
    stop(paste0("Invalid import_rsf_pfcbl_id: ",import_rsf_pfcbl_id,". Reporting entity does not exist"))
  }
  
  if (reporting_entity$pfcbl_category_rank > 3) {
    stop(paste0("Import not allowed.  Only Programs, Facilities and Clients are allowed to create imports"))
  }
  
  if (reporting_asof_date < ymd(reporting_entity$created_in_reporting_asof_date)) {
    stop(paste0("Template cannot declare a reporting_asof_date '",as.character(reporting_asof_date),"' that pre-dates the ",
                reporting_entity$pfcbl_category,
                " creation date of '",reporting_entity$created_in_reporting_asof_date,"'"))
  }
  
  if (import_rsf_pfcbl_id > 0) {
    parent_reporting <- dbGetQuery(pool,"
      with parent as (
        select 
        ft.to_family_rsf_pfcbl_id as parent_rsf_pfcbl_id
        from p_rsf.view_rsf_pfcbl_id_family_tree ft
        where ft.from_rsf_pfcbl_id = $1::int
          and ft.pfcbl_hierarchy <> 'child'
        order by to_pfcbl_rank desc
        limit 1 
      )
      select exists(select * from p_rsf.reporting_cohorts rc 
                    where rc.reporting_rsf_pfcbl_id = (select parent_rsf_pfcbl_id from parent)
                      and rc.reporting_asof_date <= $2::date)::bool as parent_reported",
      params=list(import_rsf_pfcbl_id,
                  reporting_asof_date))
    
    if (!unlist(parent_reporting)) {
      stop(paste0("Import cannot be created for this ",reporting_entity$pfcbl_category," for ",as.character(reporting_asof_date),
      " because parent-level reporting must exist"))
    }
    parent_reporting <- NULL
  }
  
  
  if (auto_delete_old_versions==TRUE) {
    
    existing_reporting <- dbGetQuery(pool,"
      select
      rt.template_id,
      rt.template_has_static_row_ids,
      ri.import_id,
      ri.file_name
      from p_rsf.reporting_templates rt
      left join p_rsf.reporting_imports ri on ri.import_rsf_pfcbl_id = $1::int
                                          and ri.reporting_asof_date = $2::date
                                          and ri.template_id = rt.template_id
      where rt.template_id = $3::int
        and rt.template_has_static_row_ids is TRUE",
      params=list(import_rsf_pfcbl_id,
                  reporting_asof_date,
                  template_id))
    
    auto_delete_imports <- na.omit(unlist(existing_reporting$import_id))
    
  }
  
  gzip_file <- NULL
  {
    gzip(file_path,ext="gz",remove=F,overwrite=T)
    gzip_file <- paste0(file_path,".gz")
    
    file_con <- file(description=gzip_file,open="rb")
    file_info <- file.info(gzip_file)
    
    record_file_name <- basename(gzip_file)
    record_file_name <- sub("\\s+\\{.*\\s+import\\d+}","",record_file_name) #removes {facility import34242}
    record_file_name <- sub("^\\d{1,3}-([A-Z])","\\1",record_file_name) #32- 
    binfile <- readBin(file_con,what="raw",n=file_info$size,endian=.Platform$endian)
    close.connection(file_con)
  }
  
  {
    reporting_import <- dbGetQuery(pool,"
          insert into p_rsf.reporting_imports(import_rsf_pfcbl_id,
                                              import_pfcbl_category,
                                              import_user_id,
                                              import_time,
                                              import_completed,
                                              reporting_asof_date,
                                              template_id,
                                              import_comments,
                                              file_name,
                                              file_data,
                                              pfcbl_name)
          select 
            ids.rsf_pfcbl_id,
            ids.pfcbl_category,
            $2::text as import_user_id,
            timeofday()::timestamptz as import_time,
            false as import_completed,
            $3::date as reporting_asof_date,
            $4::int as template_id,
            NULLIF($5::text,'NA') as import_comments,
            $6::text as file_name,
            $7::bytea as file_data,
            regexp_replace(sn.pfcbl_name,'^[a-z]+:','') as pfcbl_name
          from p_rsf.rsf_pfcbl_ids ids
          left join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.rsf_pfcbl_id = ids.rsf_pfcbl_id
          where ids.rsf_pfcbl_id = $1::int
          returning import_id,
                    import_rsf_pfcbl_id,
                    import_pfcbl_category,
                    reporting_asof_date,
                    template_id,
                    file_name,
                    pfcbl_name",
         params=list(import_rsf_pfcbl_id,
                     import_user_id,
                     reporting_asof_date,
                     template_id,
                     as.character(import_comments),
                     record_file_name,
                     paste0("\\x",paste0(binfile,collapse=""))))
   
    file.remove(gzip_file) 
  }
  
  #Delete conflicting imports after successful upload and creation of new import that replaces it.
  if (!empty(reporting_import) && length(auto_delete_imports) > 0) {
    #Import delete trigger will back-up deletes.
    #Just in case this new import fails to upload
    deleted <- dbExecute(pool,"
        insert into p_rsf.reporting_imports_deleted_archive(import_id,deleting_user_id)
        select ri.import_id,$2::text
          from p_rsf.reporting_imports ri
          where ri.import_id = any(select unnest(string_to_array($1::text,','))::int)
        returning import_id",
                         params=list(paste0(auto_delete_imports,collapse=","),
                                     import_user_id))
  }
  
  return (reporting_import)
}
