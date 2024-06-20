db_cohort_download_file <- function(pool,
                                    reporting_cohort_id,
                                    file_path=".",
                                    save_as_filename=NA,
                                    unpack_and_remove=TRUE)
{
  
  if (grepl("/$",file_path)) file_path <- gsub("/$","",file_path)
  
  download_file <- dbGetQuery(pool,
                              "select upload_filename,upload_file from p_rsf.reporting_cohort_info 
                                    where reporting_cohort_id = $1::int",
                              params=list(reporting_cohort_id))
  
  download_filename <- download_file$upload_filename
  download_file <- unlist(download_file$upload_file,use.names = F)
  
  out_path <- paste0(file_path,"/",download_filename)
  if (file.exists(out_path)) file.remove(out_path)
  
  out_file <- file(out_path,"wb",raw=TRUE)
  
  
  writeBin(as.raw(download_file),out_file)
  close.connection(out_file)
  
  if (unpack_and_remove==TRUE) {
    x <- gunzip(out_path,remove=T,overwrite=T)
    x <- as.character(x)
    if (!is.na(save_as_filename)) {
      ext <- file_ext(x)
      save_as_filename <- gsub("\\.[a-z\\.]+$","",save_as_filename,ignore.case=T)
      save_as_filename <- paste0(save_as_filename,".",ext)
      file.rename(from=x,to=paste0(file_path,"/",save_as_filename))
      x <- paste0(file_path,"/",save_as_filename)
    }
    out_path <- x
  }
  return (out_path)
}
