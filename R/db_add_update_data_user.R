db_add_update_data_user <- function(pool,
                                    reporting_cohort,
                                    cohort_upload_data,
                                    template_has_static_row_ids=FALSE)
{
  if (empty(cohort_upload_data)) return (NULL)
  if (is.null(template_has_static_row_ids) || any(is.na(template_has_static_row_ids))) template_has_static_row_ids <- FALSE

  t1<-Sys.time()
  #SETUPS
  {
    
    
    

    valid_cols <- c("reporting_asof_date",
                    "rsf_pfcbl_id",
                    "indicator_id",
                    "data_value",
                    "data_unit",
                    "data_submitted",
                    "data_source_row_id")
    
    bad_cols <- setdiff(union(names(cohort_upload_data),valid_cols),names(cohort_upload_data))
    if (length(bad_cols)>0) stop(paste0("cohort_upload_data must define columns: ",paste0(valid_cols,collapse=","), ".  Issues with: ",paste0(bad_cols,collapse=",")))
    if (!all(c("reporting_cohort_id","reporting_user_id","reporting_asof_date","rsf_program_id") %in% names(reporting_cohort))) stop("reporting_cohort must provide rsf_program_id, reporting_cohort_id, reporting_asof_date and reporting_user_id")
    if (length(unique(cohort_upload_data$reporting_asof_date)) != 1) stop("Only one reporting_asof_date can be reported per function call")
    
    #if(SYS_PRINT_TIMING)  debugtime("db_add_update_data_create_temp_upload_rsf_data")
    
   
    
    
    cohort_upload_data[,n:=.N,
                   by=.(reporting_asof_date,rsf_pfcbl_id,indicator_id)]
    redundants <- cohort_upload_data[n>1]
    #conn <- poolCheckout(pool)
    #dbBegin(conn)
    if (nrow(redundants) > 0) {
      
      print(redundants)
      red <- poolWithTransaction(pool,function(conn) { 
        dbExecute(conn,"create temp table _temp_red(rsf_pfcbl_id int, indicator_id int) on commit drop;")
        dbAppendTable(conn,
                      name="_temp_red",
                      value=unique(redundants[,.(rsf_pfcbl_id,indicator_id)]))
        dbGetQuery(conn,"select
        sn.rsf_pfcbl_id,
        sn.pfcbl_name,
        ind.indicator_id,
        ind.indicator_name
        from
        _temp_red tr
        inner join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.rsf_pfcbl_id = tr.rsf_pfcbl_id
        inner join p_rsf.indicators ind on ind.indicator_id = tr.indicator_id")
      })
      
      setDT(red)
      red <- red[redundants,
                 on=.(rsf_pfcbl_id,indicator_id)]
      
      red[is.na(pfcbl_name),pfcbl_name:=rsf_pfcbl_id]
      red[is.na(indicator_name),sys_name:=indicator_id]
      
      red <- red[,.(pfcbl_name,
             indicator_name,
             data_source_row_id,
             data_value=fcase(!is.na(data_unit) & !is.na(data_value),paste0(data_value," ",data_unit),
                                                                     !is.na(data_value),data_value,
                                                                     !is.na(data_unit),data_unit,
                                                                     default="{BLANK}"))]
      red[,message:=paste0(pfcbl_name," reported: ",indicator_name," {",data_value,"} ON ",data_source_row_id)]
      
      stop(paste0("Data upload has conflicting/redundant data across different sections:\n ",paste0(red$message,collapse=" [AND] \n")))
    }
  }
  
  
  
  #browser()
  #conn <- poolCheckout(pool);
  #dbBegin(conn)
  #dbRollback(conn)
#dbExecute(conn,"drop table _temp_upload_rsf_data")
  inserted_rows <- poolWithTransaction(pool,function(conn) {
      
      #Create and upload temp tables
      #set insert actions
      {

        #print(paste0("- - - - Uploading ",nrow(cohort_upload_data)," data points"))
        
        dbExecute(conn,"create temporary table _temp_upload_rsf_data(rsf_pfcbl_id int4 not null,
                                                                     indicator_id int4 not null,
                                                                     reporting_asof_date date not null,
                                                                     data_value text,
                                                                     data_unit text,
                                                                     data_submitted text,
                                                                     data_source_row_id text,
                                                                     reporting_cohort_id int,
                                                                     inserted_row_number serial not null)
              ON COMMIT DROP;")
        
        t1 <- Sys.time()      
        dbAppendTable(conn=conn,
                      name="_temp_upload_rsf_data",
                      value=cohort_upload_data[,
                                               .(rsf_pfcbl_id,
                                                 indicator_id,
                                                 reporting_asof_date,
                                                 data_value,
                                                 data_unit,
                                                 data_submitted,
                                                 data_source_row_id,
                                                 reporting_cohort_id=as.numeric(NA))])
        
        if(SYS_PRINT_TIMING)  debugtime("db_add_update_data_user _temp_upload_rsf_data created: ",format(Sys.time()-t1))
        
        #x<-dbGetQuery(conn,"select * from _temp_upload_rsf_data")
        dbExecute(conn,"analyze _temp_upload_rsf_data")
        #dbExecute(conn,"create unique index _temp_upload_rsf_data_udx on _temp_upload_rsf_data(rsf_pfcbl_id,indicator_id,reporting_asof_date)")
        dbExecute(conn,"
          with updates as (
            select 
            urd.rsf_pfcbl_id,
            urd.indicator_id,
            urd.reporting_asof_date
            from _temp_upload_rsf_data urd
            inner join p_rsf.indicators ind on ind.indicator_id = urd.indicator_id
            inner join lateral (select * 
                                from p_rsf.rsf_data_current_lcu lcu
            					          where lcu.for_rsf_pfcbl_id = urd.rsf_pfcbl_id
            						          and lcu.reporting_asof_date <= urd.reporting_asof_date
            					          order by lcu.reporting_asof_date desc
            					          limit 1) as lcu on lcu.data_unit_value = urd.data_unit
            where ind.data_type = 'currency'
              and ind.data_unit = 'LCU'
            	and urd.data_unit is not null
            	and urd.data_unit <> 'LCU'
          )
          update _temp_upload_rsf_data urd
          set data_unit = 'LCU'
          from updates up 
          where up.rsf_pfcbl_id = urd.rsf_pfcbl_id
            and up.indicator_id = urd.indicator_id
          	and up.reporting_asof_date = urd.reporting_asof_date")
        
        deletes <- dbExecute(conn,"delete from _temp_upload_rsf_data urd
                        where p_rsf.data_value_is_meaningfully_different(input_rsf_pfcbl_id => urd.rsf_pfcbl_id,
                              																					 input_indicator_id => urd.indicator_id,
                              																					 input_reporting_asof_date => urd.reporting_asof_date,
                              																					 input_data_value => urd.data_value,
                                                                         input_data_unit => urd.data_unit,
                                                                         compare_data_source_row_id => case when $1::bool = true
                                                                                                            then urd.data_source_row_id
                                                                                                            else NULL 
                                                                                                       end::text,
                                                                         is_redundancy_reporting => false::bool
                                                                        ) = false;",
                        params=list(template_has_static_row_ids))
        
        
        has_data <- unlist(dbGetQuery(conn,"select exists(select * from _temp_upload_rsf_data urd)::bool as has_data"))
        #x <- dbGetQuery(conn,"select * from _temp_upload_rsf_data");x
        if (has_data==FALSE) return(0)
        
        #reporting_cohort and reported data are in the same reporting period (overwhelming majority of cases)
        data_reporting_cohort_id <- NULL
        if (as.character(reporting_cohort$reporting_asof_date)==unique(as.character(cohort_upload_data$reporting_asof_date))) {
          data_reporting_cohort_id <- as.numeric(reporting_cohort$reporting_cohort_id)
        } else { #Otherwise, this must be a "chronology reporting" and data must be reported under a linked cohort
          
          data_reporting_asof_date <- unique(as.character(cohort_upload_data$reporting_asof_date))
          linked_cohort <- dbGetQuery(conn,"
                                      select rc.reporting_cohort_id 
                                      from p_rsf.reporting_cohorts rc
                                      where rc.linked_reporting_cohort_id = $1::int
                                        and rc.reporting_asof_date = $2::date",
                                      params=list(reporting_cohort$reporting_cohort_id,
                                                  data_reporting_asof_date))
          
          if (empty(linked_cohort)) {
            linked_cohort <- dbGetQuery(conn,
                                       "insert into p_rsf.reporting_cohorts(rsf_program_id,
                                                                            reporting_rsf_pfcbl_id,
                                                                            reporting_asof_date,
                                                                            reporting_user_id,
                                                                            reporting_time,
                                                                            data_asof_date,
                                                                            from_reporting_template_id,
                                                                            source_reference,
                                                                            source_name,
                                                                            source_note,
                                                                            linked_reporting_cohort_id,
                                                                            is_reported_cohort,
                                                                            is_calculated_cohort,
                                                                            is_redundancy_cohort)
                                      select 
                                        rc.rsf_program_id,
                                        rc.reporting_rsf_pfcbl_id,
                                        $2::date as reporting_asof_date,
                                        rc.reporting_user_id,
                                        TIMEOFDAY()::timestamptz as reporting_time,
                                        $2::date as data_asof_date,
                                        rc.from_reporting_template_id,
                                        'LINKED REPORTING COHORT to ' || rc.reporting_asof_date as source_reference,
                                        rc.source_name,
                                        NULL::text as source_note,
                                        $1::int as linked_reporting_cohort_id,
                                        true::bool as is_reported_cohort,
                                        false::bool as is_calculated_cohort,
                                        false::bool as is_redundancy_cohort
                                      from p_rsf.reporting_cohorts rc
                                      where rc.reporting_cohort_id = $1::int
                                      returning reporting_cohort_id",
                                      params=list(reporting_cohort$reporting_cohort_id,
                                                  data_reporting_asof_date))
          }
          data_reporting_cohort_id <- as.numeric(linked_cohort$reporting_cohort_id)
        }
        
        dbExecute(conn,"
                       with child_cohort as (
                        insert into p_rsf.reporting_cohorts(rsf_program_id,
                                                            reporting_rsf_pfcbl_id,
                                                            reporting_asof_date,
                                                            reporting_user_id,
                                                            reporting_time,
                                                            data_asof_date,
                                                            from_reporting_template_id,
                                                            source_reference,
                                                            source_name,
                                                            source_note,
                                                            parent_reporting_cohort_id,
                                                            is_reported_cohort,
                                                            is_calculated_cohort,
                                                            is_redundancy_cohort)
                        select 
                          rc.rsf_program_id,
                          rc.reporting_rsf_pfcbl_id,
                          rc.reporting_asof_date as reporting_asof_date,
                          rc.reporting_user_id,
                          TIMEOFDAY()::timestamptz as reporting_time,
                          rc.data_asof_date,
                          rc.from_reporting_template_id,
                          'DATA REPORTING COHORT' as source_reference,
                          rc.source_name,
                          NULL::text as source_note,
                          $1::int as parent_reporting_cohort_id,
                          true::bool as is_reported_cohort,
                          false::bool as is_calculated_cohort,
                          false::bool as is_redundancy_cohort
                        from p_rsf.reporting_cohorts rc
                        where rc.reporting_cohort_id = $1::int
                        returning reporting_cohort_id
                      )
                      update _temp_upload_rsf_data urd
                      set reporting_cohort_id = cc.reporting_cohort_id
                      from child_cohort cc",
                     params=list(data_reporting_cohort_id))

        #If we have meaningfully different data to insert, then we need a reporting cohort under which to report it.
        #If the reporting_asof_date of the data is DIFFERENT than the reporting_as_of_date of the submitted reporting_cohort, then lookup AND/OR create a LINKED cohort.
        #Otherwise, create a child cohort under which the data will be reported.
      }        
      
      ################
      ###RSF_REPORTING
      ################
      #These are all the entities that are *trying* to report
      #But recall that Excel templates are just appending new updates and leaving old data.  So it's possible that some entities are reporting
      #"present" this period with no changes.  But more likely entities with no changes are legacy reporting and should be ignored
      #But since we don't know yet if any changes are present or not, create a provisional entry with -1 value that will either be updated later to actual 
      #reported values (or zero).  If not, we'll delete this entity later as an irrelevant legacy entry.
      {
        
        dbExecute(conn,"
                  with entity_reporting as (
                    select
                      urd.rsf_pfcbl_id,
                      urd.reporting_asof_date,
                      urd.reporting_cohort_id,
                      coalesce(
                          NULLIF(
                            array_to_string(
                              array_agg(distinct ind.indicator_name order by ind.indicator_name) 
                              filter(where ind.is_system = false),
                            ','),
                          ''),
                          'NOTHING-CHANGED') as data_value,
                     case when $1::bool = true 
                          then array_to_string(array_agg(distinct data_source_row_id),',')
                          else NULL
                     end as data_source_row_id
                    
                    from _temp_upload_rsf_data urd
                    inner join p_rsf.indicators ind on ind.indicator_id = urd.indicator_id 
                    group by
                      urd.rsf_pfcbl_id,
                      urd.reporting_asof_date,
                      urd.reporting_cohort_id
                  )
                  insert into _temp_upload_rsf_data(rsf_pfcbl_id,
                                                    indicator_id,
                                                    reporting_asof_date,
                                                    reporting_cohort_id,
                                                    data_value,
                                                    data_submitted,
                                                    data_unit,
                                                    data_source_row_id)
                  select
                    er.rsf_pfcbl_id,
                    ind.indicator_id,
                    er.reporting_asof_date,
                    er.reporting_cohort_id,
                    er.data_value,
                    'SYSTEM DATA'::text as data_submitted,
                    ind.data_unit,
                    er.data_source_row_id
                  from entity_reporting er
                  inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = er.rsf_pfcbl_id
                  inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
                  where ind.indicator_sys_category = 'entity_reporting'",
                  params=list(template_has_static_row_ids))
      }
      
      
      #Regular and redundancy cohorts can both insert data! (but redundancy won't trigger calculations or parent-level reporting triggers)
      {

        #x <- as.data.table(dbGetQuery(conn,"select * from _temp_upload_rsf_data"));x
        #x <- as.data.table(dbGetQuery(conn,"select * from _temp_upload_rsf_data urd where urd.insert_action = true"))
        

        dbExecute(conn,"
                        
                          insert into p_rsf.rsf_data(rsf_pfcbl_id,
                                                     indicator_id,
                                                     reporting_asof_date,
                                                     reporting_cohort_id,
                                                     data_value,
                                                     data_submitted,
                                                     data_unit,
                                                     data_source_row_id)
                          select 
                            rsf_pfcbl_id,
                            indicator_id,
                            reporting_asof_date,
                            reporting_cohort_id,
                            data_value,
                            data_submitted,
                            data_unit,
                            data_source_row_id
                          from _temp_upload_rsf_data
                         ")
        
        inserted_rows <- dbGetQuery(conn,"
                                    select inserted_row_number 
                                    from _temp_upload_rsf_data
                                    where inserted_row_number <= $1::int",
                                    params=list(nrow(cohort_upload_data)))
        
        inserted_rows <- inserted_rows$inserted_row_number
      }      
      

      #Intraperiod formula definitions doesn't make sense.  But if it happens to be... then only update the indicator's formula variables for the 
      #forthcoming parent data update
      #MOVED TO DATABASE TRIGGER
      return (inserted_rows)
    })
  
  if(SYS_PRINT_TIMING) debugtime("db_add_update_data_user","Done!",format(Sys.time()-t1))
  if (length(inserted_rows)==0) inserted_rows <- NULL
  
  return (inserted_rows)
}