rsf_setup_export_create_entity <- function(pool,
                                           export_rsf_pfcbl_id,
                                           file_path=".") {
  
  if (grepl("/$",file_path)) file_path <- gsub("/$","",file_path)
  
  entity <- dbGetQuery(pool,"
    select 
      ids.rsf_pfcbl_id,
      case when ids.pfcbl_category = 'client' then ids.rsf_facility_id
           when ids.pfcbl_category = 'facility' then ids.rsf_program_id
           when ids.pfcbl_category = 'program' then 0
           else NULL
      end as parent_rsf_pfcbl_id,
      ids.pfcbl_category,
      ids.created_in_reporting_asof_date,
      ids.created_by_reporting_cohort_id,
      rc.reporting_user_id,
      rc.reporting_time,
      coalesce(nids.id,'00000') as id,
      coalesce(nids.nickname,nids.name) as name,
      nids.rsf_full_name
    from p_rsf.rsf_pfcbl_ids ids      
    inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = ids.created_by_reporting_cohort_id
    inner join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = ids.rsf_pfcbl_id
    where ids.rsf_pfcbl_id = $1::int",
    params=list(export_rsf_pfcbl_id))
  
  if (empty(entity)) stop(paste0("Entity SYSID #",export_rsf_pfcbl_id," does not exist"))
  if (!any(entity$pfcbl_category %in% c("program","facility","client"))) stop(paste0("Export entity may only request Program, Facility or Client entities"))
  if (anyNA(entity$parent_rsf_pfcbl_id)) stop("An error occurred: parent could not be identified")
  
  export_data <- dbGetQuery(pool,"
    
select 
      ids.rsf_pfcbl_id,
      ids.pfcbl_category,
      ind.indicator_id,
      ind.indicator_name,
      rdc.reporting_asof_date,
      ind.data_category,
      ind.indicator_sys_category,
      p_rsf.rsf_data_value_unit(v_data_value => rdc.data_value,
                                v_data_unit => rdc.data_unit) as data_value
    from p_rsf.rsf_pfcbl_ids ids
    inner join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id
                                         and rdc.reporting_asof_date = ids.created_in_reporting_asof_date
    inner join p_rsf.indicators ind on ind.indicator_id = rdc.indicator_id
    where ids.rsf_pfcbl_id = $1::int
      and ind.is_system is false
      and not exists(select * from p_rsf.view_rsf_setup_indicator_subscriptions sis 
                     where sis.rsf_pfcbl_id = ids.rsf_pfcbl_id        
                       and sis.indicator_id = ind.indicator_id
                       and sis.filter_matched_pfcbl_indicators is true  
                       and (sis.is_unsubscribed is true
                            or
                            sis.is_calculated is true))
                            
union 

select 
      ids.rsf_pfcbl_id,
      ids.pfcbl_category,
      ind.indicator_id,
      ind.indicator_name,
      ids.created_in_reporting_asof_date as reporting_asof_date,
      ind.data_category,
      ind.indicator_sys_category,
      p_rsf.rsf_data_value_unit(v_data_value => rdc.data_value,
                                v_data_unit => rdc.data_unit) as data_value
    from p_rsf.rsf_pfcbl_ids ids
    inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
                                   and ind.is_setup is not null
    inner join lateral (select * from p_rsf.rsf_data_current dat 
                        where dat.rsf_pfcbl_id = ids.rsf_pfcbl_id
                          and dat.indicator_id = ind.indicator_id
                          and dat.data_value is not null
                        order by dat.reporting_asof_date asc
                        limit 1) as rdc on true

    
    where ids.rsf_pfcbl_id = $1::int
      and ind.is_system is false

",
  params=list(export_rsf_pfcbl_id))
  
  setDT(export_data)
  export_data[,n:=1:.N,
              by=.(rsf_pfcbl_id,
                   indicator_id)]
  
  export_data <- export_data[n==1]
  
  excelwb <- export_create_entity_to_excel(pool=pool,
                                           parent_rsf_pfcbl_id=entity$parent_rsf_pfcbl_id,
                                           reporting_asof_date=entity$created_in_reporting_asof_date,
                                           entity_data=export_data,
                                           exporting_user_id=entity$reporting_user_id)
  
  filename <- paste0("#0 ",gsub("[^[:alnum:][:space:]]","",paste0(entity$id," ",entity$name))," - ",
                     format_asof_date_label(entity$created_in_reporting_asof_date)," - ",
                     "RSF ",entity$pfcbl_category," - v1.xlsx")
  
  outpath <- paste0(file_path,"/",filename)
  openxlsx::saveWorkbook(excelwb,
                         file=outpath,
                         overwrite=T)
  
  if (!file.exists(outpath)) stop(paste0("Failed to save file: ",outpath))
  
  return (outpath)
  
}