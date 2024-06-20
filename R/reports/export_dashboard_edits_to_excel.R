export_dashboard_edits_to_excel <- function(pool,
                                             edits_dt,
                                             exporting_user_id,
                                             report_note="None") {
  # t0 <- Sys.time()
  #edits <<- as.data.frame(edits_dt)
  # edits_dt <- as.data.table(edits)
  #browser()
  #SHEET_NAME <- "RSF DATA"
  #START_ROW <- 10
  
  USE_TEMPLATE_NAME <- "PFCBL-EDITOR-TEMPLATE"
  TEMPLATE <- db_export_get_template(pool=pool,
                                     template_name=USE_TEMPLATE_NAME)
  
  export_cohort <- db_export_create(pool=pool,
                                    exporting_user_id=exporting_user_id,
                                    exporting_asof_date=max(edits_dt$reporting_asof_date),
                                    exporting_pfcbl_ids=unique(edits_dt$SYSID),
                                    exporting_indicator_ids=unique(edits_dt$INDID),
                                    template_id=TEMPLATE$template_id,
                                    export_name=NA)
  
  #REPORT_KEY <- export_cohort$reporting_key

  # exporting_name <- unlist(strsplit(export_cohort$exporting_entity_name,">"))
  # exporting_name <- exporting_name[length(exporting_name)]
  # exporting_name <- gsub("[[:punct:]]"," ",exporting_name)
  # exporting_name <- gsub("[[:space:]]+"," ",exporting_name)
  # exporting_name <- trimws(exporting_name)
  
  # source_name <- rbindlist(export_cohort$exporting_parents)
  # if (nrow(source_name)==1) {
  #   source_name <- source_name$rsf_full_name
  # } else {
  #   source_name <- source_name[,.(source_name=paste0(pfcbl_category,": ",paste0(rsf_name,collapse=" & "))),by=.(pfcbl_category)][,source_name]
  # }
  
  if (is.na(report_note) || nchar(trim(report_note))==0) report_note = "None"
  
  excelwb <- rsf_reports_create_excel_sheet(excelwb=NULL,
                                            sheet_name="RSF_DATA",
                                            sheet_data_table_name="RSF_TEMPLATE_DATA",
                                            sheet_data=edits_dt,
                                            
                                            program_name=export_cohort$program_name,
                                            
                                            template_key=TEMPLATE$template_key,
                                            report_key=export_cohort$reporting_key,
                                            #data_integrity_key=export_cohort$data_integrity_key, 
                                            reporting_entity=export_cohort$exporting_entity_name,
                                            reporting_asof_date=export_cohort$exporting_asof_date,
                                            reporting_user=format_name_abbreviation(export_cohort$exporting_users_name),
                                            reporting_time=as.character(export_cohort$exporting_time),
                                            reporting_notes=report_note)
  
  
  
  #saveWorkbook(excelwb,file="dashboard_test.xlsx",overwrite=TRUE)
  
  return(excelwb)  
}