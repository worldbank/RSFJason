db_program_create <- function(pool,
                              program_name,
                              program_nickname,
                              program_inception_date,
                              program_lcu,
                              reporting_user_id,
                              #program_reporting_frequency=c("quarter","month"),
                              program_reporting_frequency="quarter",
                              source_name=NA) {
  
  #program_reporting_frequency <- match.arg(program_reporting_frequency)
  program_name <- toupper(trimws(program_name,whitespace="[ \\t\\r\\n\\v\\h\\s]"))
  program_nickname <- toupper(trimws(program_nickname,whitespace="[ \\t\\r\\n\\v\\h\\s]"))
  program_inception_date <- suppressWarnings(ymd(program_inception_date))
  
  if (nchar(program_name) < 4 || nchar(program_nickname) < 4) stop("Invalid names: minimum of 4 characters required for name and nickname")
  if (is.na(program_inception_date)) stop("Unable to establish reporting_asof_date")
  
  reporting_asof_date <- ceiling_date(ymd(program_inception_date),unit=program_reporting_frequency)-1
  source_reference <- paste0("CREATE ",program_nickname)
  
  
  
  #conn <- poolCheckout(pool)
  #dbBegin(conn)
  
  exists <- dbGetQuery(pool,"select *
                                 from p_rsf.rsf_data rd
                                 inner join p_rsf.indicators ind on ind.indicator_id = rd.indicator_id
                                 where ind.data_category = 'program'
                                   and ind.indicator_sys_category in ('name','nickname')
                                   and array[lower(rd.data_value)] && string_to_array($1::text,',')::text[]",
                       params=list(paste0(c(program_name,program_nickname),collapse=",")))
  
  if (!empty(exists)) stop(paste0("A program already exists with name or nickname: ",psate0(program_name," or ",program_nickname)))
  
  dbExecute(pool,"
                do $$
                begin
                  if (not exists(select * from p_rsf.rsf_programs where rsf_program_id = 0)
                      or not exists(select * from p_rsf.rsf_pfcbl_ids where rsf_pfcbl_id = 0)
                      or not exists(select * from p_rsf.rsf_pfcbl_id_family where parent_rsf_pfcbl_id = 0)
                      or not exists(select * from p_rsf.reporting_cohorts where reporting_cohort_id = 0)
                      or not exists(select * from p_rsf.rsf_data where rsf_pfcbl_id = 0)
                      or not exists(select * from p_rsf.rsf_program_reporting_dates where rsf_pfcbl_id = 0)) then
                      
                      raise info 'Initializing Global Program';
                      perform p_rsf.initialize_global_program();

                  end if;          
                end;
                $$ language plpgsql;")
  
  min_reporting_date <- dbGetQuery(pool,"select min(valid_reporting_date)::date as min_date from p_rsf.rsf_program_reporting_dates")
  if (reporting_asof_date < min_reporting_date$min_date) {
    stop(paste0("Unable to create a program whose first reporting_asof_date is earlier than ",as.character(min_reporting_date$min_date),
                ". If this is a requirement, the Global program p_rsf.initialize_global_program() function in the PostGreSQL database must adjust this date earlier."))
  }
  #browser()
  #conn <- poolCheckout(pool)
  #dbBegin(conn)
  #dbRollback(conn)
  
    reporting_cohort <- poolWithTransaction(pool,function(conn) {

      
      
      #p_rsf.initialize_global_program() should create an entry in all of the following tables -- if not there for any reason, ensure it's performed again
      
      
      new_program <- dbGetQuery(conn,"
                                        insert into p_rsf.rsf_programs(rsf_program_id,
                                                                       reporting_period) 
                                        values(nextval('p_rsf.rsf_pfcbl_ids_rsf_pfcbl_id_seq'::regclass),
                                               $1::text)
                                        returning rsf_program_id,reporting_period",
                                   params=list(program_reporting_frequency))

      rsf_program_id <- new_program$rsf_program_id
      new_program_rsf_pfcbl_id <- dbGetQuery(conn,"select nextval('p_rsf.rsf_pfcbl_ids_rsf_pfcbl_id_seq'::regclass)::int")
      new_program_rsf_pfcbl_id <- as.numeric(unlist(new_program_rsf_pfcbl_id))

      
      new_program_reporting_cohort_id <- dbGetQuery(conn,"select nextval('p_rsf.rsf_data_cohort_sequence'::regclass)::int")
      new_program_reporting_cohort_id <- as.numeric(unlist(new_program_reporting_cohort_id))
      
      dbExecute(conn,"
        insert into p_rsf.rsf_program_reporting_dates(rsf_program_id,valid_reporting_date,reporting_sequence_rank,rsf_pfcbl_id)
        values($1::int,$2::date,1,$3::int)",
                params=list(rsf_program_id,
                            reporting_asof_date,
                            new_program_rsf_pfcbl_id))
      
      program <- dbGetQuery(conn,"
                                    insert into p_rsf.rsf_pfcbl_ids(rsf_pfcbl_id,
                                                                   rsf_program_id,
                                                                   rsf_facility_id,
                                                                   rsf_client_id,
                                                                   rsf_borrower_id,
                                                                   rsf_loan_id,
                                                                   rsf_id,
                                                                   pfcbl_category,
                                                                   pfcbl_category_rank,
                                                                   created_by_reporting_cohort_id,
                                                                   created_in_reporting_asof_date)
                                   select 
                                     $1::int,
                                     $4::int as rsf_program_id,
                                     NULL::int as rsf_facility_id,
                                     NULL::int as rsf_client_id,
                                     NULL::int as rsf_borrower_id,
                                     NULL::int as rsf_loan_id,
                                     $4::int as rsf_id, --pfcbl_category = 'program' assures this is true
                                     rpc.pfcbl_category,
                                     rpc.pfcbl_rank,
                                     $2::int as created_by_reporting_cohort_id,
                                     $3::date as created_in_reporting_asof_date
                                   from p_rsf.rsf_pfcbl_categories rpc
                                   where rpc.pfcbl_category = 'program'
                                  returning rsf_pfcbl_id,rsf_program_id",
                            params=list(new_program_rsf_pfcbl_id,
                                        new_program_reporting_cohort_id,
                                        reporting_asof_date,
                                        rsf_program_id))
      
      if (empty(program)) {
        return (NULL)
      }
      
      
      source_note <- paste0("System: Creating new program '",program_name,"' with ID#",rsf_program_id)
      reporting_cohort <- dbGetQuery(conn,
                                     "
                                  with creation_cohort as (
                                     insert into p_rsf.reporting_cohorts(rsf_program_id,
                                                                          reporting_rsf_pfcbl_id,
                                                                          reporting_asof_date,
                                                                          reporting_user_id,
                                                                          reporting_time,
                                                                          data_asof_date,
                                                                          source_reference,
                                                                          source_name,
                                                                          source_note,
                                                                          reporting_cohort_id)
                                      select 
                                        pr.rsf_program_id,
                                        $2::int as reporting_rsf_pfcbl_id,
                                        $3::date as reporting_asof_date,
                                        $4::text as reporting_user_id,
                                        TIMEOFDAY()::timestamptz as reporting_time,
                                        $3::date as data_asof_date,
                                        concat('NEW PROGRAM/', $5::text) as source_reference,
                                        $6::text as source_name,
                                        $7::text as source_note,
                                        $8::int as reporting_cohort_id
                                      from p_rsf.rsf_programs pr
                                      where pr.rsf_program_id = $1::int
                                       returning reporting_cohort_id,
                                                rsf_program_id,
                                                reporting_rsf_pfcbl_id,
                                                reporting_asof_date,
                                                data_asof_date,
                                                reporting_user_id,
                                                source_reference,
                                                source_name
                                   )
                                   insert into p_rsf.reporting_cohorts(rsf_program_id,
                                                                          reporting_rsf_pfcbl_id,
                                                                          reporting_asof_date,
                                                                          reporting_user_id,
                                                                          reporting_time,
                                                                          data_asof_date,
                                                                          source_reference,
                                                                          source_name,
                                                                          source_note,
                                                                          parent_reporting_cohort_id,
                                                                          is_reported_cohort)
                                  select 
                                  cc.rsf_program_id,
                                  cc.reporting_rsf_pfcbl_id,
                                  cc.reporting_asof_date,
                                  cc.reporting_user_id,
                                  timeofday()::timestamptz as reporting_time,
                                  cc.data_asof_date,
                                  'CREATE PROGRAM DATA COHORT' as source_reference,
                                  cc.source_name,
                                  NULL::text as source_note,
                                  cc.reporting_cohort_id as parent_reporting_cohort_id,
                                  true as is_reported_cohort
                                  from creation_cohort cc
                                  returning reporting_cohort_id,
                                            rsf_program_id,
                                            reporting_rsf_pfcbl_id,
                                            reporting_asof_date,
                                            reporting_user_id,
                                            source_reference,
                                            source_name,
                                            parent_reporting_cohort_id
                                  ",
                                     params=list(rsf_program_id,
                                                 new_program_rsf_pfcbl_id,
                                                 reporting_asof_date,
                                                 reporting_user_id,
                                                 source_reference,
                                                 source_name,
                                                 source_note,
                                                 new_program_reporting_cohort_id))
      
             

      
      reporting_cohort
    })
    
  if (empty(reporting_cohort)) stop("Failed to create new program")
  
  nodefault_setup_indicators <- c('name',
                                  'nickname',
                                  'entity_creation_date')
  
  program_create_indicators <- dbGetQuery(pool,"
                                          select 
                                            ind.indicator_id,
                                            ind.indicator_name,
                                            ind.data_unit,
                                            ind.data_type,
                                            ind.default_value,
                                            ind.indicator_sys_category
                                          from p_rsf.indicators ind
                                          where ind.data_category = 'program' and ind.is_setup IS NOT NULL")
  setDT(program_create_indicators)
  program_create_indicators_nodefault <- program_create_indicators[is.na(default_value)==TRUE,indicator_sys_category]
  
  missing_setups <- setdiff(union(program_create_indicators_nodefault,nodefault_setup_indicators),program_create_indicators_nodefault)
  if (length(missing_setups) > 0) {
    stop(paste0("Failed to create program due to required setup indicators not submitted and not having a valid default value: ",
                paste0(missing_setups,collapse=" & "),
                " System also requires that ",paste0(nodefault_setup_indicators,collapse=","),": verify database p_rsf.indicators::is_setup is NOT NULL"))
  }

  program_create_indicators[,reporting_rsf_pfcbl_id:=reporting_cohort$reporting_rsf_pfcbl_id]
  program_create_indicators[,reporting_asof_date:=reporting_cohort$reporting_asof_date]
  program_create_indicators[,rsf_pfcbl_id:=reporting_cohort$reporting_rsf_pfcbl_id]
  
  program_create_indicators[,data_value:=as.character(NA)]
  program_create_indicators[indicator_sys_category=="name",data_value:=program_name]
  program_create_indicators[indicator_sys_category=="nickname",data_value:=program_nickname]
  program_create_indicators[indicator_sys_category=="entity_creation_date",data_value:=as.character(program_inception_date)]
  program_create_indicators[indicator_sys_category=="entity_local_currency_unit",data_value:=as.character(program_lcu)]
  program_create_indicators[indicator_sys_category=="SYSID",data_value:=as.character(rsf_pfcbl_id)]
  program_create_indicators[!(indicator_sys_category %in% nodefault_setup_indicators),data_value:=default_value]

  if (any(is.na(program_create_indicators$data_value))) {
    stop(paste0("Failed to create program due to {MISSING} values for required setup indicators: ",
                paste0(program_create_indicators[is.na(data_value),indicator_name],collapse=" & ")))
  }
  
  program_create_indicators[,default_value:=NULL]
  #program_create_indicators[,indicator_sys_category:=NULL]
  
  #program_create_indicators[,data_flags_new:=list()]
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

  db_add_update_data_user(pool=pool,
                          reporting_cohort=reporting_cohort,
                          cohort_upload_data=program_create_indicators,
                          template_has_static_row_ids=FALSE,
                          is_redundancy_reporting=FALSE)
  
  program <- data.table(rsf_program_id=reporting_cohort$rsf_program_id,
                        rsf_pfcbl_id=reporting_cohort$reporting_rsf_pfcbl_id)
  
  return (program)
}