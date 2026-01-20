export_backup_data_to_csv <- function(pool,
                                      rsf_pfcbl_id.family,
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
  
  reported_data <- dbGetQuery(pool,"
    select 
      prd.sys_name as \"SYSNAME\",
      prd.indicator_id as \"INDID\",
      prd.reporting_asof_date,
      prd.indicator_name,
      prd.data_value
    from p_rsf.view_rsf_pfcbl_id_family_tree ft 
    inner join (select 
        sn.sys_name,
        rd.rsf_pfcbl_id,
        rd.indicator_id,
        rd.reporting_asof_date,
        rd.data_id,
        ind.indicator_name,
        rpc.pfcbl_category,
        rpc.pfcbl_rank,
        trim(concat(rd.data_value,' ' || rd.data_unit)) as data_value,
        NULLIF((dense_rank() over(partition by 
        										rd.rsf_pfcbl_id,
        										rd.indicator_id,
        										rd.reporting_asof_date
                          order by 
        										(rc.is_redundancy_cohort = false) desc,
        										rd.data_id desc))-1,0) as redundancy_rank
      from p_rsf.rsf_data rd
      inner join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.rsf_pfcbl_id = rd.rsf_pfcbl_id
      inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
      inner join p_rsf.indicators ind on ind.indicator_id = rd.indicator_id
      inner join p_rsf.rsf_pfcbl_categories rpc on rpc.pfcbl_category = ind.data_category
      left join p_rsf.view_rsf_setup_indicator_subscriptions sis on sis.rsf_pfcbl_id = rd.rsf_pfcbl_id
                                                                and sis.indicator_id = rd.indicator_id
      left join p_rsf.indicator_formulas indf on indf.formula_id = sis.formula_id
      where rc.is_reported_cohort = true
        and ind.is_system = false
        and (
      				(indf.formula_id IS NULL) OR
      				(indf.formula_id IS NOT NULL AND indf.overwrite = 'deny') OR
      				(indf.formula_id IS NOT NULL AND indf.overwrite = 'missing')
      			)
      order by 
      rd.rsf_pfcbl_id,
      rd.indicator_id,
      rd.reporting_asof_date,
      redundancy_rank nulls last
      
    ) as prd on prd.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
    where ft.from_rsf_pfcbl_id = $1::int
      and ft.pfcbl_hierarchy <> 'parent'
    order by prd.rsf_pfcbl_id,prd.indicator_id,prd.reporting_asof_date nulls last",
  params=list(rsf_pfcbl_id.family))
  
  setDT(reported_data)
  
  #saveWorkbook(excelwb,file="archive_test.xlsx",overwrite=TRUE)
  
  return(reported_data)  
}