export_backup_data_to_csv <- function(pool,
                                      rsf_pfcbl_id.familytree,
                                      exporting_user_id,
                                      report_note="Reported data archive: calculations excluded") {
  # t0 <- Sys.time()
  #edits <<- as.data.frame(edits_dt)
  # edits_dt <- as.data.table(edits)
  #browser()
  #SHEET_NAME <- "RSF DATA"
  #START_ROW <- 10
  
  # USE_TEMPLATE_NAME <- "PFCBL-EDITOR-TEMPLATE"
  # TEMPLATE <- db_export_get_template(pool=pool,
  #                                    template_name=USE_TEMPLATE_NAME)
  
  reported_data <- dbGetQuery(pool,'
    select 
      prd.sys_name as "SYSNAME",
      prd.indicator_id as "INDID",
      prd.reporting_asof_date,
      prd.indicator_name,
      prd.data_value
    from p_rsf.rsf_pfcbl_id_family fam 
    inner join p_rsf.view_rsf_pfcbl_reported_data prd on prd.rsf_pfcbl_id = fam.child_rsf_pfcbl_id
    where fam.parent_rsf_pfcbl_id = $1::int
    order by prd.rsf_pfcbl_id,prd.indicator_id,prd.reporting_asof_date,prd.redundancy_rank nulls last',
  params=list(rsf_pfcbl_id.familytree))
  
  setDT(reported_data)
  
  # export_cohort <- db_export_create(pool=pool,
  #                                   exporting_user_id=exporting_user_id,
  #                                   exporting_asof_date=max(reported_data$reporting_asof_date),
  #                                   exporting_pfcbl_ids=rsf_pfcbl_id.familytree,
  #                                   exporting_indicator_ids=unique(reported_data$INDID),
  #                                   template_id=TEMPLATE$template_id,
  #                                   export_name=NA)
  # 
  # if (is.na(report_note) || nchar(trim(report_note))==0) report_note = "None"
  # 
  # excelwb <- rsf_reports_create_excel_sheet(excelwb=NULL,
  #                                           sheet_name="RSF_DATA",
  #                                           sheet_data_table_name="RSF_TEMPLATE_DATA",
  #                                           sheet_data=reported_data,
  #                                           
  #                                           program_name=export_cohort$program_name,
  #                                           
  #                                           template_key=TEMPLATE$template_key,
  #                                           report_key=export_cohort$reporting_key,
  #                                           #data_integrity_key=export_cohort$data_integrity_key, 
  #                                           reporting_entity=export_cohort$exporting_entity_name,
  #                                           reporting_asof_date=export_cohort$exporting_asof_date,
  #                                           reporting_user=format_name_abbreviation(export_cohort$exporting_users_name),
  #                                           reporting_time=as.character(export_cohort$exporting_time),
  #                                           reporting_notes=report_note)
  
  
  
  #saveWorkbook(excelwb,file="archive_test.xlsx",overwrite=TRUE)
  
  return(reported_data)  
}