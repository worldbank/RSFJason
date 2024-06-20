db_program_download <- function(pool,
                                rsf_program_id,
                                exporting_user_id,
                                out_path=".",
                                consolidate.setup=TRUE, #If multiple setup files are uploaded, just take everything that's current and consolidate into one file
                                verbatim=FALSE,         #When true, download everything that's uploaded and don't differentiate setupfiles.
                                template_filter=NA,
                                archive_name=NULL) {
  
  
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
                                                       rsf_program_id=rsf_program_id,
                                                       exporting_user_id=exporting_user_id,
                                                       include_never_reported=FALSE)
  } else {
    consolidate.setup <- FALSE
  }
  
  pname <- dbGetQuery(pool,"
                        select 
                          nids.rsf_name,
                          nids.created_in_reporting_asof_date
                        from p_rsf.view_current_entity_names_and_ids nids 
                        where rsf_program_id = $1::int
                        and pfcbl_category = 'program'",
                      params=list(rsf_program_id))

  if (is.null(archive_name) || all(is.na(archive_name))) {
    archive_name <- paste0("RSF PROGRAM ARCHIVE for ",pname$rsf_name,".zip")
  }
  
 
  download_files <- NULL
  program_export_name <- paste0(pname$rsf_name," RSF PROGRAM SETUP FILE.xlsx")
  
  program_upload_files <- dbGetQuery(pool,"select
                                          rc.reporting_cohort_id,
                                          rc.reporting_time,
                                          rc.reporting_asof_date,
                                          ids.pfcbl_category,
                                          ids.pfcbl_category_rank,
                                          rci.upload_filename,
                                          rc.source_name,
                                          coalesce(nids.nickname,nids.rsf_name) as upload_name,
                                          coalesce(nids.id,upper(ids.pfcbl_category)) as upload_id,
                                          rt.is_setup_template,
                                          rt.template_id
                                          from p_rsf.reporting_cohorts rc
                                          inner join p_rsf.reporting_templates rt on rt.template_id = rc.from_reporting_template_id
                                          inner join p_rsf.reporting_cohort_info rci on rci.reporting_cohort_id = rc.reporting_cohort_id
                                          inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = rc.reporting_rsf_pfcbl_id
                                          inner join lateral(select 
                                                              fam.parent_rsf_pfcbl_id,
                                                              fam.parent_pfcbl_rank
                                                            from p_rsf.rsf_pfcbl_id_family fam
                                                            where fam.child_rsf_pfcbl_id = rc.reporting_rsf_pfcbl_id
                                                              and fam.parent_pfcbl_rank <= 3
                                                            order by fam.parent_pfcbl_rank desc
                                                            limit 1) parent on true
                                          inner join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = parent.parent_rsf_pfcbl_id
                                          where rc.rsf_program_id = $1::int
                                           and rci.upload_file is not null
                                           and rci.upload_filename is not null
                                           and rc.is_reported_cohort = true
                                          order by 
                                            rt.is_setup_template desc,
                                            parent.parent_pfcbl_rank,
                                            parent.parent_rsf_pfcbl_id,
                                            ids.pfcbl_category_rank,
                                            ids.rsf_pfcbl_id,
                                            rc.reporting_asof_date,
                                            rc.reporting_time,
                                            rc.reporting_cohort_id",
                                     params=list(rsf_program_id))
  
  setDT(program_upload_files)
  
  if (consolidate.setup==TRUE) {
    program_upload_files <- program_upload_files[is_setup_template==FALSE]
  }
  
  if (length(template_ids) > 0) {
    program_upload_files <- program_upload_files[template_id %in% template_ids] 
  }
  
  {
    
    program_upload_files[,ext:=file_ext(source_name)]
    program_upload_files[,
                         download_name:=paste0(upload_name," ",upload_id," ",format_asof_date_label(reporting_asof_date))]
    
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
  
  program_export_name <- paste0(paste0(rep(x="0",nchar(nrow(program_upload_files))),collapse = ""),"-",program_export_name)
  if (!is.null(programs_export)) {
  
    
    openxlsx::saveWorkbook(wb=programs_export,
                           file=paste0(out_path,"/",program_export_name),
                           overwrite=TRUE)
  }
  
  download_files <- mapply(db_cohort_download_file,
                           reporting_cohort_id=program_upload_files$reporting_cohort_id,
                           save_as_filename=program_upload_files$download_filename,
                           MoreArgs=list(pool=pool,
                                         file_path=out_path,
                                         unpack_and_remove=TRUE))
  
  
  if (!all(file.exists(download_files))) {
    stop("Failed to download and find all files")
  }
  
  zip_files <- download_files
  if (!is.null(programs_export)) {
    zip_files <- c(paste0(out_path,"/",program_export_name),
                   zip_files)
  }
  
  zip_files <- gsub(paste0("^",out_path,"/?"),"",zip_files)
  
  zip::zip(root=out_path,
           files=sort(zip_files),
           zipfile=archive_name)
  
  file.remove(paste0(out_path,"/",zip_files))
  
  return (archive_name)
}  