export_rsf_setup_files_to_excel <- function(pool,
                                            export_pfcbl_id,
                                            # rsf_program_id=NA, #NA returns the entire RSF database program, facility, clients
                                            #                    #And export will be in a single file
                                            #                    #Otherwise, a separate LIST of files will be returned for each program/facilities/clients setup
                                            # rsf_pfcbl_ids.filter=as.numeric(NA),
                                            exporting_user_id,
                                            include_never_reported=FALSE,
                                            exclude_auto_subscribe=TRUE,
                                            include=c("data",
                                                      "settings",
                                                      "indicators",
                                                      "checks",
                                                      "config",
                                                      "actions",
                                                      "flags",
                                                      "review")
                                            ) #Setup only will filter out only those data submitted by setup files or not 
                                                                  #subsequently overwritten by a non-setup file or non-setup creation date
                                                                  #FALSE has few use cases -- maybe when QRs update values where they shouldn't?
{
  #browser()
  #setups
  {
    SHEET_NAME <- "RSF DATA"
    
    export_pfcbl_category <- unlist(dbGetQuery(pool,"select pfcbl_category from p_rsf.rsf_pfcbl_ids ids where ids.rsf_pfcbl_id = $1::int",export_pfcbl_id))
    
    if (length(export_pfcbl_category)==0 || !export_pfcbl_category %in% c("global","program","facility")) {
      stop("export_rsf_pfcbl_id must be a valid entity and be either global, program or facility-level")  
    }
   
    if (length(include)==0) stop("Include must have one or all of: data, settings, indicators, checks, config, actions")
    if (!all(include %in% c("data",
                            "settings",
                            "indicators",
                            "checks",
                            "config",
                            "actions",
                            "flags",
                            "review") )) stop("Include must have one or all of: data, settings, indicators, checks, config, actions, flags")
    
    if (!any(include=="data")) stop("Include must at least include: data")
    
    USE_TEMPLATE_NAME <- "RSF-SETUP-TEMPLATE"
    TEMPLATE <- db_export_get_template(pool=pool,
                                       template_name=USE_TEMPLATE_NAME)
  }
  
  {
    program_data <- NULL
    if (any(include=="data")) {
      #If we're backing up and downloading all files, then we just want is_setup_data = true (because subsequent non-setup files will be downloaded and the data retained in separate uploads)
      #But if we want to review all program/facility settings, then we should download everything for the sake of a complete review.
#       program_data <- dbGetQuery(pool,"
#                                  select *
#                                  from p_rsf.view_rsf_setup_programs_data spd
#                                  where coalesce(spd.rsf_program_id = $1::int,true)
#                                    and case when NULLIF($2::text,'NA') is NULL then true
#                                        else spd.rsf_pfcbl_id in (select child_rsf_pfcbl_id
# 	                                                          from p_rsf.rsf_pfcbl_id_family fam
# 													                                  where fam.parent_rsf_pfcbl_id in (select unnest(string_to_array($2::text,','))::int))
#                                        end",
#                                  params=list(rsf_program_id,
#                                              paste0(rsf_pfcbl_ids.filter,collapse=",")))
#       
      
      program_data <- dbGetQuery(pool,"
                                 select * 
                                 from p_rsf.view_rsf_setup_programs_data spd
                                 where case when $1::text = 'facility'
                                            then $2::int = spd.rsf_facility_id
                                       else $2::int = spd.rsf_program_id end",
                                 params=list(export_pfcbl_category,
                                             export_pfcbl_id))
      
      
      setDT(program_data)

      if (include_never_reported==FALSE) {
        program_data <- program_data[entity_never_reported==FALSE]
      }
    }
    
    program_review <- NULL
    if (any(include=="review")) {

      program_review <- dbGetQuery(pool,'
                                 select * 
                                 from p_rsf.view_rsf_setup_review spd
                                 where spd."SYSID" = $1::int',
                                 params=list(export_pfcbl_id))
      
      
      setDT(program_review)
      
    }
    
    program_settings <- NULL
    if (any(include=="settings")) {
      program_settings <- dbGetQuery(pool,"
                                     select * 
                                     from p_rsf.view_rsf_setup_programs_settings rps
                                     where rps.pfcbl_category = $1::text
                                       and rps.rsf_pfcbl_id = $2::int",
                                     params=list(export_pfcbl_category,
                                                 export_pfcbl_id))
      setDT(program_settings)
    }
    
    program_indicators <- NULL
    if (any(include=="indicators")) {
      program_indicators <- dbGetQuery(pool,"
                                     select * 
                                     from p_rsf.view_rsf_setup_programs_indicators spi
                                      where case when $1::text = 'facility'
                                            then $2::int = spi.rsf_facility_id
                                       else $2::int = spi.rsf_program_id end",
                                       params=list(export_pfcbl_category,
                                                   export_pfcbl_id))
      setDT(program_indicators)
      
      if (exclude_auto_subscribe==TRUE) { program_indicators <- program_indicators[is_auto_subscribed==FALSE] }
    }
    
    program_checks <- NULL
    if (any(include=="checks")) {
      
      
      program_checks <- dbGetQuery(pool,"
                                     select *
                                     from p_rsf.view_rsf_setup_programs_checks pic
                                     where case when $1::text = 'facility'
                                            then $2::int = pic.rsf_facility_id
                                       else $2::int = pic.rsf_program_id end",
                                   params=list(export_pfcbl_category,
                                               export_pfcbl_id))
      setDT(program_checks)
      
      if (exclude_auto_subscribe==TRUE) { program_checks <- program_checks[is_auto_subscribed==FALSE] }
    }
    
    checks_config <- NULL
    if (any(include=="config")) {
      
      checks_config <- dbGetQuery(pool,"
                                     select * 
                                     from p_rsf.view_rsf_setup_check_config scc
                                     where scc.rsf_pfcbl_id = $1::int",
                                     params=list(export_pfcbl_id))
      setDT(checks_config)
    }

    program_template_actions <- NULL
    if (any(include=="actions")) {
      #Can inherit GLOBAL template actions
      program_template_actions <- dbGetQuery(pool,"
                                             select * 
                                             from p_rsf.view_rsf_program_facility_template_headers fth
                                             where fth.rsf_pfcbl_id = any(select ft.to_family_rsf_pfcbl_id
                                                                          from p_rsf.view_rsf_pfcbl_id_family_tree ft
                                                                          where ft.from_rsf_pfcbl_id = $1::int
                                                                            and ft.to_pfcbl_category in ('global','program','facility'))",
                                             params=list(export_pfcbl_id))
      setDT(program_template_actions)
    }
    
    program_flags <- NULL
    if (any(include=="flags")) {
      
      program_flags <- dbGetQuery(pool,"
                                  select 
                                    cae.evaluation_id as archive_id,
                                    cae.sys_name,
                                    cae.indicator_id,
                                    cae.indicator_check_id,
                                    cae.check_asof_date::text,
                                    cae.check_status,
                                    cae.check_status_user_id,
                                    cae.check_status_comment,
                                    cae.check_message,
                                    cae.data_sys_flags,
                                    cae.data_value_unit
                                  from p_rsf.view_rsf_data_checks_archive_eligible cae
                                  inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = cae.rsf_pfcbl_id
                                  where case when $1::text = 'facility'
                                            then $2::int = ids.rsf_facility_id
                                       else $2::int = ids.rsf_program_id end",
                                  params=list(export_pfcbl_category,
                                              export_pfcbl_id))
      setDT(program_flags)
    }
  }  
  
  exporting_users_name <- dbGetQuery(pool,"
                                     select users_name 
                                     from p_rsf.view_account_info where account_id = $1::text",
                                     params=list(exporting_user_id))
  
  exporting_asof_date <- ymd(min(as.character(program_data$reporting_asof_date)))
  
  exporting <- dbGetQuery(pool,"
    select 
      nai.sys_name, 
      coalesce(nai.nickname,nai.name,nai.pfcbl_name,'RSF ' || nai.pfcbl_category) as name
    from p_rsf.rsf_data_current_names_and_ids nai
    where nai.rsf_pfcbl_id = $1::int
    order by nai.reporting_asof_date desc
    limit 1",
    params=list(export_pfcbl_id))
  
  exporting_entity_name  <- exporting$sys_name
  exporting_name <- exporting$name
  
  #print(paste0("Exporting: ",exporting_name))

  program_data[entity_never_reported==TRUE &
               is.na(data_value),
               data_value:="{NOTHING}"]

  program_data[entity_never_reported==FALSE &
               is.na(data_value),
               data_value:="{NA}"]
  
  program_data <- program_data[,
                               .(INDID=indicator_id,
                                 SYSNAME=sys_name,
                                 reporting_asof_date,
                                 indicator_name,
                                 value=data_value,
                                 unit=data_unit)]
  
  excelwb <- rsf_reports_create_excel_sheet(excelwb=NULL,
                                            sheet_name="RSF_DATA",
                                            sheet_data_table_name="RSF_TEMPLATE_DATA",
                                            sheet_data=program_data,
                                            
                                            program_name=exporting_name,

                                            template_key=TEMPLATE$template_key,
                                            
                                            reporting_entity=exporting_entity_name,
                                            reporting_asof_date=exporting_asof_date,
                                            reporting_user=format_name_abbreviation(exporting_users_name),
                                            reporting_time=as.character(now()),
                                            reporting_notes="")

  if (!empty(program_indicators)) {
    program_indicators <- program_indicators[,
                                         .(INDID=indicator_id,
                                           SYSNAME=sys_name,
                                           indicator_name,
                                           monitored=toupper(as.character(monitored)),
                                           is_auto_subscribed,
                                           FRMID=formula_id,
                                           
                                           formula_title,
                                           formula_calculation_unit,
                                           sort_preference,
                                           subscription_comments,
                                           comments_user_id,
                                           options_group_id)]
    
    excelwb <- rsf_reports_create_excel_sheet(excelwb=excelwb,
                                              sheet_name="PROGRAM_INDICATORS",
                                              sheet_data_table_name="RSF_PROGRAM_INDICATORS",
                                              sheet_data=program_indicators,
                                              
                                              program_name=exporting_name,
                                              
                                              template_key=TEMPLATE$template_key,
                                              
                                              reporting_entity=exporting_entity_name,
                                              reporting_asof_date=exporting_asof_date,
                                              reporting_user=format_name_abbreviation(exporting_users_name),
                                              reporting_time=as.character(now()),
                                              reporting_notes="")
  }  
  
  if (!empty(program_checks)) {
    program_checks <- program_checks[,
                                     .(CHKID=indicator_check_id,
                                       FRMID=check_formula_id,
                                       SYSNAME=sys_name,
                                       check_name,
                                       check_class,
                                       check_type,
                                       check_formula_title,
                                       monitored=toupper(as.character(monitored)),
                                       is_auto_subscribed,
                                       subscription_comments,
                                       comments_user_id)]
    
    excelwb <- rsf_reports_create_excel_sheet(excelwb=excelwb,
                                              sheet_name="PROGRAM_CHECKS",
                                              sheet_data_table_name="RSF_PROGRAM_CHECKS",
                                              sheet_data=program_checks,
                                              
                                              program_name=exporting_name,
                                              
                                              template_key=TEMPLATE$template_key,
                                              
                                              reporting_entity=exporting_entity_name,
                                              reporting_asof_date=exporting_asof_date,
                                              reporting_user=format_name_abbreviation(exporting_users_name),
                                              reporting_time=as.character(now()),
                                              reporting_notes="")
  }  
  
  if (!empty(program_settings)) {
    program_settings <- program_settings[,
                                         .(SYSNAME=sys_name,
                                           setting=setting_name,
                                           value=setting_value)]
    
    excelwb <- rsf_reports_create_excel_sheet(excelwb=excelwb,
                                              sheet_name="PROGRAM_SETTINGS",
                                              sheet_data_table_name="RSF_SETTINGS_DATA",
                                              sheet_data=program_settings,
                                              
                                              program_name=exporting_name,
                                              
                                              template_key=TEMPLATE$template_key,
                                              
                                              reporting_entity=exporting_entity_name,
                                              reporting_asof_date=exporting_asof_date,
                                              reporting_user=format_name_abbreviation(exporting_users_name),
                                              reporting_time=as.character(now()),
                                              reporting_notes="")
  }  
  
  if (!empty(checks_config)) {
    setcolnames(checks_config,
                old="sys_name",
                new="SYSNAME")
    
    excelwb <- rsf_reports_create_excel_sheet(excelwb=excelwb,
                                              sheet_name="checks_config",
                                              sheet_data_table_name="RSF_CONFIG_DATA",
                                              sheet_data=checks_config,
                                              
                                              program_name=exporting_name,
                                              
                                              template_key=TEMPLATE$template_key,
                                              
                                              reporting_entity=exporting_entity_name,
                                              reporting_asof_date=exporting_asof_date,
                                              reporting_user=format_name_abbreviation(exporting_users_name),
                                              reporting_time=as.character(now()),
                                              reporting_notes="")
  }
  
  if (!empty(program_template_actions)) {
    
    excelwb <- rsf_reports_create_excel_sheet(excelwb=excelwb,
                                              sheet_name="PROGRAM_TEMPLATE_ACTIONS",
                                              sheet_data_table_name="RSF_TEMPLATE_ACTIONS",
                                              sheet_data=program_template_actions,
                                              
                                              program_name=exporting_name,
                                              
                                              template_key=TEMPLATE$template_key,
                                              
                                              reporting_entity=exporting_entity_name,
                                              reporting_asof_date=exporting_asof_date,
                                              reporting_user=format_name_abbreviation(exporting_users_name),
                                              reporting_time=as.character(now()),
                                              reporting_notes="")
  }
  
  if (!empty(program_flags)) {
    program_flags <- program_flags[,
                                   .(ARCID=archive_id,
                                     SYSNAME=sys_name,
                                     INDID=indicator_id,
                                     CHKID=indicator_check_id,
                                     check_asof_date,
                                     check_status,
                                     check_status_user_id,
                                     check_status_comment,
                                     check_message,
                                     data_sys_flags,
                                     data_value_unit)]  
    
    excelwb <- rsf_reports_create_excel_sheet(excelwb=excelwb,
                                              sheet_name="PROGRAM_FLAGS",
                                              sheet_data_table_name="RSF_PROGRAM_FLAGS",
                                              sheet_data=program_flags,
                                              
                                              program_name=exporting_name,
                                              
                                              template_key=TEMPLATE$template_key,
                                              
                                              reporting_entity=exporting_entity_name,
                                              reporting_asof_date=exporting_asof_date,
                                              reporting_user=format_name_abbreviation(exporting_users_name),
                                              reporting_time=as.character(now()),
                                              reporting_notes="")
  }
  
  if (!empty(program_review)) {
    
    
    excelwb <- rsf_reports_create_excel_sheet(excelwb=excelwb,
                                              sheet_name="REVIEW",
                                              sheet_data_table_name="RSF_REVIEW",
                                              sheet_data=program_review,
                                              
                                              program_name=exporting_name,
                                              
                                              template_key=TEMPLATE$template_key,
                                              
                                              reporting_entity=exporting_entity_name,
                                              reporting_asof_date=exporting_asof_date,
                                              reporting_user=format_name_abbreviation(exporting_users_name),
                                              reporting_time=as.character(now()),
                                              reporting_notes="")
  }
  
  return(excelwb)
}