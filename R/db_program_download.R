db_program_download <- function(pool,
                                export_pfcbl_id,
                                out_path=".",
                                exporting_user_id,
                                archive_name,
                                consolidate.setup=TRUE, #If multiple setup files are uploaded, just take everything that's current and consolidate into one file
                                verbatim=FALSE,         #When true, download everything that's uploaded and don't differentiate setupfiles.
                                template_filter=NA) {

  
  export_pfcbl_category <- unlist(dbGetQuery(pool,"select pfcbl_category from p_rsf.rsf_pfcbl_ids ids where ids.rsf_pfcbl_id = $1::int",export_pfcbl_id))
  
  if (length(export_pfcbl_category)==0 || !export_pfcbl_category %in% c("global","program","facility")) {
    stop("export_rsf_pfcbl_id must be a valid entity and be either global, program or facility-level")  
  }
  
  if (grepl("/$",out_path)) out_path <- gsub("/$","",out_path)
  
  template_ids <- NULL
  if (!all(is.na(template_filter))) {
    template_ids <- dbGetQuery(pool,
                               "select template_id from p_rsf.reporting_templates where template_name = any(select unnest(string_to_array($1::text,','))::text)
               ",params=list(paste0(template_filter,collapse=",")))
    
  }
  programs_export <- NULL
  
  
  if (verbatim==FALSE) {
    programs_export <- export_rsf_setup_files_to_excel(pool=pool,
                                                       export_pfcbl_id=export_pfcbl_id,
                                                       exporting_user_id=exporting_user_id,
                                                       include_never_reported=FALSE,
                                                       include=c("data",
                                                                 "settings",
                                                                 "indicators",
                                                                 "checks",
                                                                 "config",
                                                                 "actions",
                                                                 "flags"))
  } else {
    consolidate.setup <- FALSE
  }
  

  # if (is.null(rsf_pfcbl_ids.filter) || all(is.na(rsf_pfcbl_ids.filter))) {
  #   rsf_pfcbl_ids.filter <- dbGetQuery(pool,"
  #     select ids.rsf_pfcbl_id
  #     from p_rsf.rsf_pfcbl_ids ids
  #     where ids.rsf_program_id = $1::int
  #       and ids.pfcbl_category in ('program','global')",
  #     params=list(rsf_program_id))
  #   rsf_pfcbl_ids.filter <- unlist(rsf_pfcbl_ids.filter)
  # }
  # 
  
  #if we're exporting the Facility, we want program-level setup information (else facility can't exist without its program!)
  #if we're exporting the Program, we want all its children entities
  program_upload_files <- dbGetQuery(pool,"

  select
    ri.import_id,
    ri.import_time,
    ri.reporting_asof_date,
    ft.to_pfcbl_category as pfcbl_category,
    ft.to_pfcbl_rank as pfcbl_category_rank,
    ri.file_name as upload_filename,
    regexp_replace(ri.file_name,'.gz','') as source_name,
    ri.pfcbl_name as upload_name,

    rt.is_setup_template,
    rt.template_id
    from p_rsf.view_rsf_pfcbl_id_family_tree ft
    inner join p_rsf.reporting_imports ri on ri.import_rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
    inner join p_rsf.reporting_templates rt on rt.template_id = ri.template_id
    where ft.from_rsf_pfcbl_id = $1::int and (ft.from_rsf_pfcbl_id = 0 OR ft.to_family_rsf_pfcbl_id <> 0)
    order by 
      rt.is_setup_template desc,
      ft.to_pfcbl_rank,
      ft.to_pfcbl_category,
      ft.to_family_rsf_pfcbl_id,
      ri.reporting_asof_date,
      ri.import_time",
    params=list(export_pfcbl_id))
  
  setDT(program_upload_files)
  
  if (!grepl("\\.zip",archive_name)) stop("Archive file name is expected to end in '.zip'")
  
  if (consolidate.setup==TRUE) {
    program_upload_files <- program_upload_files[is_setup_template==FALSE]
  }
  
  if (length(template_ids) > 0) {
    program_upload_files <- program_upload_files[template_id %in% template_ids] 
  }

  {
    
    program_upload_files[,ext:=file_ext(source_name)]
    program_upload_files[,
                         download_name:=paste0(upload_name," {",pfcbl_category," import",import_id,"} ",format_asof_date_label(reporting_asof_date))]
    
    program_upload_files[,
                         version_name:=""]
    
    program_upload_files[grepl("[vV]\\d",source_name)==TRUE,
                         version_name:=gsub("^.*([vV]\\d.*)$","\\1",source_name)]
    
    program_upload_files[,
                         version_name:=mapply(gsub,
                                              pattern=paste0("\\.",ext,"$"),
                                              x=version_name,
                                              replacement="")]
    
    program_upload_files[nchar(version_name) > 0,
                         version_name:=paste0(" - ",version_name)]
    program_upload_files[,
                         nname:=str_pad(string=as.character(1:.N),
                                        width=nchar(nrow(program_upload_files)),
                                        side="left",
                                        pad="0")]
    
    
    program_upload_files[,
                         download_filename:=paste0(nname,"-",download_name,version_name,".",ext)]
    
    
  }  
  
  download_files <- mapply(db_import_download_file,
                           import_id=program_upload_files$import_id,
                           save_as_filename=program_upload_files$download_filename,
                           MoreArgs=list(pool=pool,
                                         file_path=out_path,
                                         unpack_and_remove=TRUE))
  
  download_files <- unlist(download_files)
  # archive_name <- paste0(paste0(rep(x="0",nchar(nrow(program_upload_files))),collapse = ""),"-",file_path_sans_ext(archive_name))
  
  if (!is.null(programs_export)) {
  
    programs_name <- paste0(paste0(rep(x="0",nchar(nrow(program_upload_files))),collapse = ""),"-",file_path_sans_ext(archive_name),".xlsx")  
    programs_name <- paste0(out_path,"/",programs_name)
    
    if (!grepl("\\.xlsx$",programs_name)) programs_name <- paste0(programs_name,".xlsx")
    
    download_files <- c(programs_name,
                        download_files)
    
    openxlsx::saveWorkbook(wb=programs_export,
                           file=programs_name,
                           overwrite=TRUE)
  }
  
  
  
  
  if (!all(file.exists(download_files))) {
    stop("Failed to download and find all files")
  }
  
  zip_files <- download_files
  # if (!is.null(programs_export)) {
  #   zip_files <- c(paste0(out_path,"/",programs_name),
  #                  zip_files)
  # }
  
  zip_files <- gsub(paste0("^",out_path,"/?"),"",zip_files)
  
  zip::zip(root=out_path,
           files=sort(zip_files),
           zipfile=archive_name)
  
  file.remove(paste0(out_path,"/",zip_files))
  
  return (archive_name)
}  