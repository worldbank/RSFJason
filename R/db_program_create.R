db_program_create <- function(pool,
                              rsf_indicators,
                              program_name,
                              program_nickname,
                              program_inception_date,
                              program_lcu,
                              program_ifc_project_id=NA, #optional
                              reporting_user_id,
                              template_id,
                              program_reporting_frequency="quarter",
                              source_name) {
  
  if (!length(program_name)) program_name <- NA
  if (!length(program_nickname)) program_nickname <- "RSF Short Name"
  if (!length(program_lcu)) program_lcu <- "USD"
  
  #program_reporting_frequency <- match.arg(program_reporting_frequency)
  program_name <- toupper(trimws(program_name,whitespace="[ \\t\\r\\n\\v\\h\\s]"))
  program_nickname <- toupper(trimws(program_nickname,whitespace="[ \\t\\r\\n\\v\\h\\s]"))
  program_inception_date <- suppressWarnings(ymd(program_inception_date))
  
  
  if (nchar(program_name) < 4 || nchar(program_nickname) < 4) stop("Invalid names: minimum of 4 characters required for name and nickname")
  if (is.na(program_inception_date)) stop("Unable to establish reporting_asof_date")
  
  reporting_asof_date <- ceiling_date(ymd(program_inception_date),unit=program_reporting_frequency)-1
  #source_reference <- paste0("CREATE ",program_nickname)
  
  
  
  #conn <- poolCheckout(pool)
  #dbBegin(conn)
  
  exists <- dbGetQuery(pool,"select *
                                 from p_rsf.rsf_data rd
                                 inner join p_rsf.indicators ind on ind.indicator_id = rd.indicator_id
                                 where ind.data_category = 'program'
                                   and ind.indicator_sys_category in ('name','nickname')
                                   and array[upper(rd.data_value)] && string_to_array($1::text,',')::text[]",
                       params=list(paste0(toupper(c(program_name,program_nickname)),collapse=",")))
  
  if (!empty(exists)) {
    message(paste0("A program already exists with name or nickname: ",paste(program_name," or ",program_nickname)))
    if (length(unique(exists$rsf_pfcbl_id)) != 1) stop(paste0("Multiple program(s) already exists with name or nickname: ",psate0(program_name," or ",program_nickname)))
    
    return (list(exists=TRUE,
                 reporting_rsf_pfcbl_id=unique(exists$rsf_pfcbl_id))) 
  }
  
  dbExecute(pool,"
                do $$
                begin
                  if (not exists(select * from p_rsf.rsf_programs where rsf_program_id = 0)
                      or not exists(select * from p_rsf.rsf_pfcbl_ids where rsf_pfcbl_id = 0)
                      or not exists(select * from p_rsf.reporting_cohorts where reporting_cohort_id = 0)
                      or not exists(select * from p_rsf.rsf_data where rsf_pfcbl_id = 0)
                  ) then
                      raise info 'Initializing Global Program';
                      perform p_rsf.initialize_global_program();

                  end if;          
                end;
                $$ language plpgsql;")
  
  min_reporting_date <- dbGetQuery(pool,"select created_in_reporting_asof_date::date as min_date from p_rsf.rsf_pfcbl_ids where rsf_pfcbl_id = 0")
  if (reporting_asof_date < min_reporting_date$min_date) {
    stop(paste0("Unable to create a program whose first reporting_asof_date is earlier than ",as.character(min_reporting_date$min_date),
                ". If this is a requirement, the Global program p_rsf.initialize_global_program() function in the PostGreSQL database must adjust this date earlier."))
  }

  #browser()
  #conn <- poolCheckout(pool)
  #dbBegin(conn)
  #dbRollback(conn)
  
  create_cohort <- poolWithTransaction(pool,function(conn) {

      
    new_program_reporting_cohort_id <- dbGetQuery(conn,"select nextval('p_rsf.rsf_data_cohort_sequence'::regclass)::int")
    new_program_reporting_cohort_id <- as.numeric(unlist(new_program_reporting_cohort_id))
      
    new_program <- dbGetQuery(conn,"
      insert into p_rsf.rsf_programs(rsf_program_id,
                                     reporting_period) 
      values(nextval('p_rsf.rsf_pfcbl_ids_rsf_pfcbl_id_seq'::regclass),
             $1::text)
      returning rsf_program_id,reporting_period",
      params=list(program_reporting_frequency))

    new_program_rsf_pfcbl_id <- new_program$rsf_program_id
    new_program_rsf_pfcbl_id <- as.numeric(unlist(new_program_rsf_pfcbl_id))

    program <- dbGetQuery(conn,"
      insert into p_rsf.rsf_pfcbl_ids(rsf_pfcbl_id,
                                     rsf_program_id,
                                     rsf_facility_id,
                                     rsf_client_id,
                                     rsf_borrower_id,
                                     rsf_loan_id,
                                     pfcbl_category,
                                     pfcbl_category_rank,
                                     created_by_reporting_cohort_id,
                                     created_in_reporting_asof_date)
       select 
         $1::int,
         $1::int as rsf_program_id,
         NULL::int as rsf_facility_id,
         NULL::int as rsf_client_id,
         NULL::int as rsf_borrower_id,
         NULL::int as rsf_loan_id,
         rpc.pfcbl_category,
         rpc.pfcbl_rank,
         $2::int as created_by_reporting_cohort_id,
         $3::date as created_in_reporting_asof_date
       from p_rsf.rsf_pfcbl_categories rpc
       where rpc.pfcbl_category = 'program'
       returning rsf_pfcbl_id,rsf_program_id",
       params=list(new_program_rsf_pfcbl_id,
                   new_program_reporting_cohort_id,
                   reporting_asof_date))
      
      if (empty(program)) {
        return (NULL)
      }
      
      create_import <- dbGetQuery(conn,"
        insert into p_rsf.reporting_imports(import_rsf_pfcbl_id,
                                            import_pfcbl_category,
                                            import_user_id,
                                            import_time,
                                            import_completed,
                                            reporting_asof_date,
                                            template_id,
                                            file_name,
                                            file_data,
                                            is_finalized,
                                            finalized_by_user_id,
                                            finalized_time,
                                            import_comments,
                                            finalized_comments,
                                            pfcbl_name)
        select $1::int as import_rsf_pfcbl_id,
               'program' as import_pfcbl_category,
               $2::text as import_user_id,
               timeofday()::timestamptz as import_time,
               true as import_completed,
               $3::date,
               $4::int as template_id,
               $5::text as file_name,
               ''::bytea as file_data,
               true as is_finalized,
               $2::text as finalized_by_user_id,
               timeofday()::timestamptz as finalized_time,
               'Creating new program' as import_comments,
               'Finalized as empty dataset (no file data associated with this import)' as finalized_comments,
               $6::text as pfcbl_name
        returning import_id;",
        params=list(program$rsf_pfcbl_id,
                    reporting_user_id,
                    reporting_asof_date,
                    template_id,
                    source_name,
                    program_nickname))
        
      new_program_import_id <- create_import$import_id
      
      create_cohort <- dbGetQuery(conn,"
        insert into p_rsf.reporting_cohorts(reporting_cohort_id,
                                            reporting_rsf_pfcbl_id,
                                            reporting_asof_date,
                                            reporting_user_id,
                                            reporting_time,
                                            is_calculated_cohort,
                                            is_reported_cohort,
                                            data_asof_date,
                                            import_id,
                                            reporting_type)
        select
          $2::int as reporting_cohort_id,
          ri.import_rsf_pfcbl_id,
          ri.reporting_asof_date,
          ri.import_user_id,
          timeofday()::timestamptz,
          false,
          false,
          ri.reporting_asof_date,
          ri.import_id,
          0 as reporting_type
        from p_rsf.reporting_imports ri
        where ri.import_id = $1::int
        returning *",
         params=list(new_program_import_id,
                     new_program_reporting_cohort_id))

      
      create_cohort
    })

  if (empty(create_cohort)) stop("Failed to create new program")
  
  nodefault_setup_indicators <- c('name',
                                  'nickname',
                                  'entity_creation_date')
  
  program_create_indicators <- rsf_indicators[data_category=="program" & !is.na(is_setup),
                                              .(indicator_id,
                                                indicator_name,
                                                data_unit,
                                                data_type,
                                                default_value,
                                                indicator_sys_category,
                                                is_setup)]
  
  
  program_create_indicators_nodefault <- program_create_indicators[is.na(default_value)==TRUE,indicator_sys_category]
  
  missing_setups <- setdiff(union(program_create_indicators_nodefault,nodefault_setup_indicators),program_create_indicators_nodefault)
  if (length(missing_setups) > 0) {
    stop(paste0("Failed to create program due to required setup indicators not submitted and not having a valid default value: ",
                paste0(missing_setups,collapse=" & "),
                " System also requires that ",paste0(nodefault_setup_indicators,collapse=","),": verify database p_rsf.indicators::is_setup is NOT NULL"))
  }

  program_create_indicators[,reporting_rsf_pfcbl_id:=create_cohort$reporting_rsf_pfcbl_id]
  program_create_indicators[,reporting_asof_date:=create_cohort$reporting_asof_date]
  program_create_indicators[,rsf_pfcbl_id:=create_cohort$reporting_rsf_pfcbl_id]
  
  program_create_indicators[,data_value:=as.character(NA)]
  program_create_indicators[indicator_sys_category=="name",data_value:=program_name]
  
  if (length(program_ifc_project_id)==1 &&
      !is.na(program_ifc_project_id) && 
      !is.na(suppressWarnings(as.numeric(program_ifc_project_id)))) {
    program_create_indicators[indicator_sys_category=="id",data_value:=program_ifc_project_id]
  }
  
  program_create_indicators[indicator_sys_category=="nickname",data_value:=program_nickname]
  program_create_indicators[indicator_sys_category=="entity_creation_date",data_value:=as.character(program_inception_date)]
  program_create_indicators[indicator_sys_category=="entity_local_currency_unit",data_value:=as.character(program_lcu)]
  program_create_indicators[indicator_sys_category=="SYSID",data_value:=as.character(rsf_pfcbl_id)]
  program_create_indicators[!(indicator_sys_category %in% nodefault_setup_indicators),data_value:=default_value]

  if (any(is.na(program_create_indicators[is_setup=="required",data_value]))) {
    stop(paste0("Failed to create program due to {MISSING} values for required setup indicators: ",
                paste0(program_create_indicators[is.na(data_value),indicator_name],collapse=" & ")))
  }
  
  program_create_indicators[,default_value:=NULL]
  
  program_create_indicators[,data_submitted:=data_value]
  program_create_indicators[,data_source_row_id:="PROGRAM_INIT"]
  
  setcolorder(program_create_indicators,neworder=c("reporting_rsf_pfcbl_id",
                                                   "reporting_asof_date",
                                                   "rsf_pfcbl_id",
                                                   "indicator_id",
                                                   "indicator_name",
                                                   "indicator_sys_category",
                                                   "data_value",
                                                   "data_type",
                                                   "data_unit",
                                                   "data_submitted",
                                                   "data_source_row_id"))

  
  
  dbExecute(pool,"
        insert into users.permissions(account_id,
                                      rsf_pfcbl_id,
                                      sys_name,
                                      granted,
                                      denied,
                                      notes)
        select 
        $1::text,
        sn.rsf_pfcbl_id,
        sn.sys_name,
        ur.role_permissions,
        0,
        'Setting creating user to program manager'
        from p_rsf.view_rsf_pfcbl_id_current_sys_names sn,users.roles ur
        where sn.pfcbl_category = 'program' 
          and sn.rsf_pfcbl_id = $2::int
          and ur.role_name = 'MANAGER'",
            params=list(reporting_user_id,
                        create_cohort$reporting_rsf_pfcbl_id))
  
  
  create_data <- db_add_update_data_user(pool=pool,
                                         import_id=create_cohort$import_id,
                                         upload_data=program_create_indicators,
                                         upload_user_id=reporting_user_id,
                                         rsf_indicators = rsf_indicators)
  
  return (create_cohort)
  
}