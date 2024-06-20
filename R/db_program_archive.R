db_program_archive <- function(pool,
                                rsf_program_id,
                                out_path) {
  
  program_upload_files <- dbGetQuery(pool,"select
                                          rc.reporting_cohort_id,
                                          rc.reporting_time,
                                          rc.reporting_asof_date,
                                          ids.pfcbl_category,
                                          ids.pfcbl_category_rank,
                                          rci.upload_filename,
                                          lpad(dense_rank() over(partition by ids.rsf_pfcbl_id order by rc.reporting_asof_date,rc.reporting_time)::text,
                                          		 char_length((count(*) over())::text),'0') as entity_upload_rank,
                                          lpad(row_number() over()::text,char_length((count(*) over())::text),'0') as program_upload_rank
                                          from p_rsf.reporting_cohorts rc
                                          inner join p_rsf.reporting_cohort_info rci on rci.reporting_cohort_id = rc.reporting_cohort_id
                                          inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = rc.reporting_rsf_pfcbl_id
                                          where rc.rsf_program_id = $1::int
                                           and rci.upload_filename is not null
                                          order by 
                                          ids.pfcbl_category_rank,
                                          ids.rsf_pfcbl_id,
                                          rc.reporting_asof_date,
                                          rc.reporting_time,
                                          rc.reporting_cohort_id",
                                     params=list(rsf_program_id))
  
  setDT(program_upload_files)
 
  program_upload_files[grepl("^[[:digit:]-]+",upload_filename),
                       upload_filename:=trimws(gsub("^[[:digit:]-]+","",upload_filename))]
  
  program_upload_files[,
                       upload_filename:=paste0(program_upload_rank,"-",entity_upload_rank,"-",upload_filename)]
  
  download_files <- mapply(db_cohort_download_file,
                           reporting_cohort_id=program_upload_files$reporting_cohort_id,
                           save_as_filename=program_upload_files$upload_filename,
                           MoreArgs=list(pool=pool,
                                         unpack_and_remove=TRUE))
  

  if (file.exists(out_path)) file.remove(out_path)
  
  zip(files=download_files,zipfile=out_path)
  
  file.remove(download_files)
  
  return (out_path)
}