template_remove_data_category_misalignments <- function(template) {
  
  rsf_indicators <- template$rsf_indicators
  pfcbl_data <- template$pfcbl_data
  match_data <- unique(template$match_results[,.(rsf_pfcbl_id,pfcbl_category)])
  
  pfcbl_data <- match_data[pfcbl_data,on=.(rsf_pfcbl_id)]
  pfcbl_data <- rsf_indicators[,.(indicator_id,data_category)][pfcbl_data,on=.(indicator_id)]
  pfcbl_data[,mismatch:=FALSE]
  pfcbl_data[data_category != pfcbl_category,mismatch:=TRUE]
  
  mismatches <- unique(pfcbl_data[mismatch==TRUE,.(reporting_template_row_group,indicator_id,data_category,pfcbl_category)])
  if (empty(mismatches)) return (template)
  else {
    for (r in 1:nrow(mismatches)) {
      mismatch <- mismatches[r]
      status_message(class="error",paste0("Error: removing row ",mismatch$reporting_template_row_group,
                     " indicator ",mismatch$indicator_id,
                     " because of indicator/data mismatch: Expected ",mismatch$data_category," but received: ",mismatch$pfcbl_category,"\n"))
    }
    pfcbl_data <- pfcbl_data[mismatch==FALSE]
    pfcbl_data[,`:=`(data_category=NULL,
                     pfcbl_category=NULL,
                     mismatch=NULL)]
    template$pfcbl_data <- pfcbl_data
    return (template)
  }
}