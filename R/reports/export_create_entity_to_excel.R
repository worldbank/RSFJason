export_create_entity_to_excel <- function(pool,
                                          parent_rsf_pfcbl_id,
                                          reporting_asof_date,
                                          entity_data,
                                          exporting_user_id) {
  
  USE_TEMPLATE_NAME <- "RSF-ENTITIES-TEMPLATE"
  SHEET_NAME <- "RSF_DATA"
  START_ROW <- 9
  SHEET_COLS <- pmax(5,nrow(entity_data))
  REPORTING_FREQUENCY <- dbGetQuery(pool,"
                                    select reporting_period
                                    from p_rsf.rsf_programs rp
                                    inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_program_id = rp.rsf_program_id
                                    where ids.rsf_pfcbl_id = $1::int",
                                    params=list(parent_rsf_pfcbl_id))
  REPORTING_FREQUENCY <- unlist(REPORTING_FREQUENCY)
  
  
  TEMPLATE <- db_export_get_template(pool=pool,
                                     template_name=USE_TEMPLATE_NAME)
  
  # #Pseudo indicators maybe included here to ensure that a reporting_asof_date can be interpreted.  Those indicator IDs will be equal to 0.
  # #Remove these after determining reporting_asof_date
  # reporting_asof_date <- entity_data[indicator_sys_category=="entity_creation_date",data_value]
  # reporting_asof_date <- ymd(reporting_asof_date)
  # reporting_asof_date <- lubridate::ceiling_date(reporting_asof_date,unit=REPORTING_FREQUENCY)-1
  
  if (length(unique(entity_data$data_category)) != 1) stop(paste0("Indicators for multiple entity types provided, only one type allowed: ",paste0(unique(entity_data$data_category),collapse=", ")))
  if (length(reporting_asof_date) != 1) stop("Only one entity_creation_date value is allowed")
  if (anyNA(reporting_asof_date)) stop("Undefined reporting_asof_date")
  
  entity_data <- entity_data[indicator_id > 0]
  
  parent_creation_date <- dbGetQuery(pool,"
                                     select 
                                      created_in_reporting_asof_date 
                                     from p_rsf.rsf_pfcbl_ids where rsf_pfcbl_id = $1",
                                     params=list(parent_rsf_pfcbl_id))
  
  parent_name <- dbGetQuery(pool,"
                            select
                              nai.sys_name
                            from p_rsf.rsf_data_current_names_and_ids nai
                            where nai.rsf_pfcbl_id = $1::int
                              and nai.reporting_asof_date <= $2::date
                            order by nai.reporting_asof_date desc
                            limit 1",
                            params=list(parent_rsf_pfcbl_id,
                                        reporting_asof_date))
  parent_name <- unlist(parent_name)
  
  #Some entities pre-date parent creation dates (and that's okay)
  #For example, borrower's creation date is their establishment date -- which could be decades ago and pre-date the entire RSF concept.
  #However, no entities can start their first reporting before their parents' first reporting, so default to parent's first reporting date as earliest possible
  #reporting_asof_date
  reporting_asof_date <- pmax(parent_creation_date$created_in_reporting_asof_date,reporting_asof_date)
  
  exporting_users_name <- dbGetQuery(pool,"select users_name from p_rsf.view_account_info where account_id = $1::text",params=list(exporting_user_id))
  entity_data[,`:=`(SYSID=parent_rsf_pfcbl_id,
                    SYSNAME=parent_name)]
  
  export_data <- dcast.data.table(entity_data,
                                  formula = SYSID+SYSNAME ~ indicator_name ,
                                  value.var = "data_value")
  
  
  
  export_cohort <- db_export_create(pool=pool,
                                    exporting_user_id=exporting_user_id,
                                    exporting_asof_date=reporting_asof_date,
                                    exporting_pfcbl_ids=parent_rsf_pfcbl_id,
                                    exporting_indicator_ids=entity_data$indicator_id,
                                    template_id=TEMPLATE$template_id,
                                    export_name=NA)
 
  creation_names <- entity_data[indicator_sys_category=="name",data_value]
  if (length(creation_names)==0) creation_names <- paste0("NEW ",unique(entity_data$data_category))
  
  report_note = paste0("Creating: ",paste0(creation_names,collapse=" & "))
  


  excelwb <- rsf_reports_create_excel_sheet(excelwb=NULL,
                                            sheet_name="RSF_DATA",
                                            sheet_data_table_name="RSF_TEMPLATE_DATA",
                                            sheet_data=export_data,
                                            
                                            header_name=export_cohort$exporting_entity_name,
                                            
                                            template_key=TEMPLATE$template_key,
                                            report_key=export_cohort$reporting_key,

                                            reporting_entity=export_cohort$exporting_entity_name,
                                            reporting_asof_date=export_cohort$exporting_asof_date,
                                            reporting_user=format_name_abbreviation(export_cohort$exporting_users_name),
                                            reporting_time=as.character(export_cohort$exporting_time),
                                            reporting_notes=report_note)
  
  
  
  #saveWorkbook(excelwb,file="create_test.xlsx",overwrite=TRUE)
  
  return(excelwb)  
}