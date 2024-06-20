rsf_reports_data_integrity_key <- function(reporting_asof_date,
                                           rsf_pfcbl_ids,
                                           indicator_ids=NA) {
  
  reporting_asof_date <- max(reporting_asof_date)
  rsf_pfcbl_ids <- sort(unique(rsf_pfcbl_ids))
  indicator_ids <- sort(unique(indicator_ids))
  
  # if (length(reporting_asof_date) != 1) stop("reporting_asof_date must have length of 1.  If multiple reporting_asof_dates exist in the export dataset, use reporting_current_date for the key")
  if (all(is.na(indicator_ids))) indicator_ids <- NULL
  digest(paste0(as.character(reporting_asof_date),
                digest(paste0(as.character(rsf_pfcbl_ids),
                              digest(as.character(indicator_ids),
                                     "sha256")
                              ),"sha256")
                ),"sha256")
}