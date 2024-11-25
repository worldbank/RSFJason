export_rsf_setup_files_to_excel <- function(pool,
                                            rsf_program_id=NA, #NA returns the entire RSF database program, facility, clients
                                                               #And export will be in a single file
                                                               #Otherwise, a separate LIST of files will be returned for each program/facilities/clients setup
                                            rsf_pfcbl_ids.filter=as.numeric(NA),
                                            exporting_user_id,
                                            include_never_reported=FALSE,
                                            include=c("data",
                                                      "settings",
                                                      "indicators",
                                                      "checks",
                                                      "guidance",
                                                      "actions",
                                                      "flags")
                                            ) #Setup only will filter out only those data submitted by setup files or not 
                                                                  #subsequently overwritten by a non-setup file or non-setup creation date
                                                                  #FALSE has few use cases -- maybe when QRs update values where they shouldn't?
{
  
  #setups
  {
    SHEET_NAME <- "RSF DATA"
    
    if (!is.na(rsf_program_id)) {
      valid <- dbGetQuery(pool,
                         "select exists(select * from p_rsf.rsf_programs rp
                                         where rp.rsf_program_id = $1::int)::bool as valid",
                          params=list(rsf_program_id))
      valid <- as.logical(unlist(valid))
      if (!valid) stop(paste0("Program ID:",rsf_program_id," is not a valid program.  Use rsf_program_id=NA to generate setup files for all rsf programs"))
    } else {
      stop("rsf program ID required")
    }
    if (length(rsf_pfcbl_ids.filter)==0) rsf_pfcbl_ids.filter <- as.numeric(NA)
    
    if (length(include)==0) stop("Include must have one or all of: data, settings, indicators, checks, guidance, actions")
    if (!all(include %in% c("data",
                            "settings",
                            "indicators",
                            "checks",
                            "guidance",
                            "actions",
                            "flags") )) stop("Include must have one or all of: data, settings, indicators, checks, guidance, actions, flags")
    
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
      program_data <- dbGetQuery(pool,"
                                 select * 
                                 from p_rsf.view_rsf_setup_programs_data spd
                                 where coalesce(spd.rsf_program_id = $1::int,true)
                                   and case when NULLIF($2::text,'NA') is NULL then true
                                       else spd.rsf_pfcbl_id in (select child_rsf_pfcbl_id
	                                                          from p_rsf.rsf_pfcbl_id_family fam 
													                                  where fam.parent_rsf_pfcbl_id in (select unnest(string_to_array($2::text,','))::int))
                                       end",
                                 params=list(rsf_program_id,
                                             paste0(rsf_pfcbl_ids.filter,collapse=",")))
      
      setDT(program_data)

      if (include_never_reported==FALSE) {
        program_data <- program_data[entity_never_reported==FALSE]
      }
    }
    
    program_settings <- NULL
    if (any(include=="settings")) {
      program_settings <- dbGetQuery(pool,"
                                     select * 
                                     from p_rsf.view_rsf_setup_programs_settings rps
                                     where coalesce(rps.rsf_program_id = $1::int,true)
                                       and case when NULLIF($2::text,'NA') is NULL then true
                                         else rps.rsf_pfcbl_id in (select child_rsf_pfcbl_id
  	                                                          from p_rsf.rsf_pfcbl_id_family fam 
  													                                  where fam.parent_rsf_pfcbl_id in (select unnest(string_to_array($2::text,','))::int))
                                         end",
                                     params=list(rsf_program_id,
                                                 paste0(rsf_pfcbl_ids.filter,collapse=",")))
      setDT(program_settings)
    }
    
    program_indicators <- NULL
    if (any(include=="indicators")) {
      program_indicators <- dbGetQuery(pool,"
                                     select * 
                                     from p_rsf.view_rsf_setup_programs_indicators spi
                                     where coalesce(spi.rsf_program_id = $1::int,true)
                                       and case when NULLIF($2::text,'NA') is NULL then true
                                         else spi.rsf_pfcbl_id in (select child_rsf_pfcbl_id
  	                                                          from p_rsf.rsf_pfcbl_id_family fam 
  													                                  where fam.parent_rsf_pfcbl_id in (select unnest(string_to_array($2::text,','))::int))
                                         end",
                                     params=list(rsf_program_id,
                                                 paste0(rsf_pfcbl_ids.filter,collapse=",")))
      setDT(program_indicators)
    }
    
    program_checks <- NULL
    if (any(include=="checks")) {
      program_checks <- dbGetQuery(pool,"
                                     select * 
                                     from p_rsf.view_rsf_setup_programs_checks pic
                                     where coalesce(pic.rsf_program_id = $1::int,true)
                                       and case when NULLIF($2::text,'NA') is NULL then true
                                         else pic.rsf_pfcbl_id in (select child_rsf_pfcbl_id
  	                                                          from p_rsf.rsf_pfcbl_id_family fam 
  													                                  where fam.parent_rsf_pfcbl_id in (select unnest(string_to_array($2::text,','))::int))
                                         end",
                                   params=list(rsf_program_id,
                                               paste0(rsf_pfcbl_ids.filter,collapse=",")))
      
      setDT(program_checks)
    }
    
    program_guidance <- NULL
    if (any(include=="guidance")) {
      #Can inherit GLOBAL guidance
      program_guidance <- dbGetQuery(pool,"
                                     select * 
                                     from p_rsf.view_rsf_setup_programs_guidance spg
                                     where spg.rsf_program_id in ($1::int,0)
                                       and case when NULLIF($2::text,'NA') is NULL then true
                                           else spg.rsf_pfcbl_id in (select child_rsf_pfcbl_id
    	                                                               from p_rsf.rsf_pfcbl_id_family fam 
    													                                       where fam.parent_rsf_pfcbl_id in (select unnest(string_to_array($2::text,','))::int))
                                           end",
                                       params=list(rsf_program_id,
                                                   paste0(rsf_pfcbl_ids.filter,collapse=",")))
      setDT(program_guidance)
    }
    
    program_template_actions <- NULL
    if (any(include=="actions")) {
      #Can inherit GLOBAL template actions
      program_template_actions <- dbGetQuery(pool,"
                                             select * 
                                             from p_rsf.view_rsf_setup_programs_template_actions pta
                                             where pta.rsf_program_id in ($1::int,0)
                                               and case when NULLIF($2::text,'NA') is NULL then true
                                                   else pta.rsf_pfcbl_id in (select child_rsf_pfcbl_id
            	                                                               from p_rsf.rsf_pfcbl_id_family fam 
            													                                       where fam.parent_rsf_pfcbl_id in (select unnest(string_to_array($2::text,','))::int))
                                                   end",
                                                   params=list(rsf_program_id,
                                                               paste0(rsf_pfcbl_ids.filter,collapse=",")))
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
                                    cae.consolidated_from_indicator_id,
                                    cae.consolidated_from_indicator_check_id,
                                    cae.data_sys_flags,
                                    cae.data_value_unit
                                  from p_rsf.view_rsf_data_checks_archive_eligible cae
                                  inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = cae.rsf_pfcbl_id
                                  where ids.rsf_program_id in ($1::int,0)
                                    and case when NULLIF($2::text,'NA') is NULL then true
                                        else cae.rsf_pfcbl_id in (select child_rsf_pfcbl_id
	                                                                from p_rsf.rsf_pfcbl_id_family fam 
													                                        where fam.parent_rsf_pfcbl_id in (select unnest(string_to_array($2::text,','))::int))
                                        end",
                                  params=list(rsf_program_id,
                                              paste0(rsf_pfcbl_ids.filter,collapse=",")))
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
      sys_name, 
      coalesce(nids.nickname,nids.name,'RSF ' || nids.pfcbl_category) as name
    from p_rsf.view_rsf_pfcbl_id_current_sys_names sn
    inner join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = sn.rsf_pfcbl_id
    where sn.rsf_program_id = $1::int
      and sn.pfcbl_category = 'program'",
    params=list(rsf_program_id))
  
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
                                 unit=data_unit,
                                 source_name)]
  
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
                                           sort_preference,
                                           subscription_comments,
                                           comments_user_id)]
    
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
  
  if (!empty(program_guidance)) {
    program_guidance <- program_guidance[,
                                         .(GUIDANCEID=indicator_check_guidance_id,
                                           CHKID=indicator_check_id,
                                           INDID=for_indicator_id,
                                           FOR=for_pfcbl_category,
                                           SYSNAME=sys_name,
                                           indicator_name,
                                           check_name,
                                           guidance,
                                           autoresolve=toupper(as.character(is_resolving_guidance)),
                                           guidance_class=overwrite_check_class,
                                           created_by_user_id,
                                           applied_by_user_id)]
    
    excelwb <- rsf_reports_create_excel_sheet(excelwb=excelwb,
                                              sheet_name="PROGRAM_GUIDANCE",
                                              sheet_data_table_name="RSF_GUIDANCE_DATA",
                                              sheet_data=program_guidance,
                                              
                                              program_name=exporting_name,
                                              
                                              template_key=TEMPLATE$template_key,
                                              
                                              reporting_entity=exporting_entity_name,
                                              reporting_asof_date=exporting_asof_date,
                                              reporting_user=format_name_abbreviation(exporting_users_name),
                                              reporting_time=as.character(now()),
                                              reporting_notes="")
  }
  
  if (!empty(program_template_actions)) {
    program_template_actions <- program_template_actions[,
                                                         .(HEADERID=header_id,
                                                           TEMPLATE=template_name,
                                                           SYSNAME=sys_name,
                                                           SHEET_NAME=template_header_sheet_name,
                                                           HEADER_NAME=template_header,
                                                           HEADER_INDEX=template_header_encounter_index,
                                                           ACTION=action,
                                                           REMAP_HEADER=remap_header)]
    
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
                                     CINDID=consolidated_from_indicator_id,
                                     CCHKID=consolidated_from_indicator_check_id,
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
  
  return(excelwb)
}