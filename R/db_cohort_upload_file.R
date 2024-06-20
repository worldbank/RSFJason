db_cohort_upload_file <- function(pool,
                                  file_path,
                                  reporting_cohort_id)
{
  gzip(file_path,ext="gz",remove=F,overwrite=T)
  gzip_file <- paste0(file_path,".gz")
  
  file_con <- file(description=gzip_file,open="rb")
  file_info <- file.info(gzip_file)
  
  record_file_name <- basename(gzip_file)
  record_file_name <- stringr::str_replace_all(record_file_name,"[[:digit:]]{1,3}\\-","")
  
  binfile <- readBin(file_con,what="raw",n=file_info$size,endian=.Platform$endian)
  close.connection(file_con)
  dbExecute(pool,"insert into p_rsf.reporting_cohort_info(reporting_cohort_id,
                                                          upload_filename,
                                                          upload_file)
                  values($1::int,
                         $2::text,
                         $3::bytea)
                  on conflict(reporting_cohort_id) do update
                  set upload_filename = EXCLUDED.upload_filename,
                      upload_file = EXCLUDED.upload_file;",
            params=list(reporting_cohort_id,
                        record_file_name,
                        paste0("\\x",paste0(binfile,collapse=""))))
  
  file.remove(gzip_file)
  return (TRUE)
}