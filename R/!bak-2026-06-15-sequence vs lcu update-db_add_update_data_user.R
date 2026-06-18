db_add_update_data_user <- function(pool,
                                    import_id,
                                    upload_data,
                                    upload_user_id,
                                    rsf_indicators) {
  
  if (empty(upload_data)) {
    return (data.table(rsf_pfcbl_id=numeric(0),
                       indicator_id=numeric(0),
                       reporting_asof_date=as.Date(numeric(0)),
                       data_value=character(0),
                       data_unit=character(0),
                       data_submitted=character(0),
                       inserted=logical(0)))
  }
  

  t1<-Sys.time()
  #SETUPS
  {
    valid_cols <- c("reporting_asof_date",
                    "rsf_pfcbl_id",
                    "indicator_id",
                    "data_value",
                    "data_unit",
                    "data_submitted")
    
    bad_cols <- setdiff(union(names(upload_data),valid_cols),names(upload_data))
    if (length(bad_cols)>0) stop(paste0("cohort_upload_data must define columns: ",paste0(valid_cols,collapse=","), ".  Issues with: ",paste0(bad_cols,collapse=",")))
    #if (length(unique(upload_data$reporting_asof_date)) != 1) stop("Only one reporting_asof_date can be reported per function call")
    
    #if(SYS_PRINT_TIMING)  debugtime("db_add_update_data_create_temp_upload_rsf_data")
    
   
    
    
    upload_data[,
                n:=.N,
                by=.(rsf_pfcbl_id,
                     indicator_id,
                     reporting_asof_date)]
    
    redundants <- upload_data[n>1]
    #Redundancies aren't allowed.
     
  
    #This is to give detailed name-based error messages for redundancies
    if (nrow(redundants) > 0) {
      
      
     
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
             data_value=fcase(!is.na(data_unit) & !is.na(data_value),paste0(data_value," ",data_unit),
                                                                     !is.na(data_value),data_value,
                                                                     !is.na(data_unit),data_unit,
                                                                     default="{BLANK}"))]
      red[,message:=paste0(pfcbl_name," reported: ",indicator_name," {",data_value,"}")]
      
      stop(paste0("Data upload has conflicting/redundant data across different sections:\n ",paste0(red$message,collapse=" [AND] \n")))
    }
    
    
    
    upload_data[,n:=NULL]
    

    upload_indicator_priorities <- unique(upload_data[,.(indicator_id)])[rsf_indicators[,.(indicator_sys_category,
                                                                                         indicator_pfcbl_rank,
                                                                                         indicator_name,
                                                                                         indicator_id,
                                                                                         data_type,
                                                                                         default_unit=data_unit)],
                                                                       on=.(indicator_id),
                                                                       nomatch=NULL]
    
    upload_indicator_priorities[,priority:=fcase(indicator_sys_category=="entity_local_currency_unit",1,
                                               indicator_sys_category=="entity_currency_unit",2,
                                               default=3)]
    
    upload_data[upload_indicator_priorities,
                priority_rank:=i.priority,
                on=.(indicator_id)]
    
    upload_data[order(reporting_asof_date),
                chronology_rank:=.GRP,
                by=.(reporting_asof_date)]
    
    
    upload_data[order(chronology_rank,priority_rank),
                sequence_rank:=.GRP,
                by=.(chronology_rank,priority_rank)]
    
    if (anyNA(upload_data$sequence_rank)) {
      stop("Failed to set upload data sequence rank")
    }
  }
  
  #upload_data[,inserted_row_number:=1:.N]
  
  #conn <- poolCheckout(pool);
  #dbBegin(conn)
  #dbRollback(conn)
  #dbExecute(conn,"drop table _temp_upload_rsf_data")
  ###NEW 2025-10-23: Embedded indicator prioritization and cohort creation
  errors <- NULL
  deleted_rows <- tryCatch({
    poolWithTransaction(pool,function(conn) {
    
    #Create and upload temp tables
    #set insert actions
    {
      
      #print(paste0("- - - - Uploading ",nrow(cohort_upload_data)," data points"))
      {
        dbExecute(conn,"create temporary table _temp_upload_rsf_data(sequence_rank int2 not null,
                                                                     rsf_pfcbl_id int4 not null,
                                                                     indicator_id int4 not null,
                                                                     reporting_asof_date date not null,
                                                                     data_value text,
                                                                     data_unit text,
                                                                     data_submitted text,
                                                                     
                                                                     reporting_rsf_pfcbl_id int,
                                                                     reporting_cohort_id int default NULL,
                                                                     reporting_segment int default NULL,
                                                                     inserted_row_number serial not null)
                ON COMMIT DROP;")
        
        t1 <- Sys.time()      
        dbAppendTable(conn=conn,
                      name="_temp_upload_rsf_data",
                      value=upload_data[,
                                        .(sequence_rank,
                                          rsf_pfcbl_id,
                                          indicator_id,
                                          reporting_asof_date,
                                          data_value,
                                          data_unit,
                                          data_submitted)])
        
        #if(SYS_PRINT_TIMING)  debugtime("db_add_update_data_user _temp_upload_rsf_data created: ",format(Sys.time()-t1))
        #dbExecute(conn,"drop table _temp_upload_rsf_data")
        #x<-dbGetQuery(conn,"select * from _temp_upload_rsf_data")
        dbExecute(conn,"analyze _temp_upload_rsf_data")
        
        dbExecute(conn,"
          with segments as (
            select
            dense_rank() over(partition by reporting.rsf_pfcbl_id,reporting_asof_date order by urd.reporting_asof_date asc,reporting.rsf_pfcbl_id asc) as reporting_segment,
            reporting.rsf_pfcbl_id as reporting_rsf_pfcbl_id,
            urd.rsf_pfcbl_id,
            urd.indicator_id,
            urd.reporting_asof_date
            from _temp_upload_rsf_data urd
            inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = urd.rsf_pfcbl_id
            inner join lateral (select coalesce(ids.rsf_client_id,ids.rsf_facility_id,ids.rsf_program_id) as rsf_pfcbl_id) as reporting on true
          )
          update _temp_upload_rsf_data urd
          set reporting_segment = rs.reporting_segment,
              reporting_rsf_pfcbl_id = rs.reporting_rsf_pfcbl_id
          from segments rs
          where rs.rsf_pfcbl_id = urd.rsf_pfcbl_id
            and rs.indicator_id = urd.indicator_id
            and rs.reporting_asof_date = urd.reporting_asof_date")
        
        dbExecute(conn,"create index _tmp_seq_idx on _temp_upload_rsf_data(sequence_rank)");
      }      
      
      #Priority 1: Entity local currency units: template$rsf_indicators[indicator_sys_category=="entity_local_currency_unit",indicator_id] (facility and program defined)
      #Priority 2: Entity local currency units: template$rsf_indicators[indicator_sys_category=="entity_currency_unit",indicator_id] (generally loan defined, but could be others?)
      #Priority 3: Everything else
      
      
      
      #current_sequence <- 1
      #current_sequence <- 2
      #current_sequence <- 3
      deletes <- c()
      for (current_sequence in sort(unique(upload_data$sequence_rank))) {
        
        #This is ultimately the driver for sequencing uploads
        #Currency-unit definitions are uploaded FIRST (and those are NOT "currency" data types).
        #Therefore, this statement can only be executed for last-priority data
        #What we're doing here is if an entity uploads, for example, "1000 EUR" and it's base currency unit (inherited or defined) is "EUR"
        #Then we want to change it from "EUR" to "LCU" (and rsf_data_current will actually change it back again)
        #This helps ensure the "meaningfully different" analysis works reliably for metrics that provide data units and users may enter "1000" that is normalized to "1000 LCU"
        #And then alternatively enter "1000 EUR" and this will generate a change entry and updated data timeseries data point of "1000 LCU" and "1000 EUR" whenever the user enters
        #the value differently.  This helps eliminate unintended timeseries junk data (this is emperically experienced with some frequency).
        #
        #This is also if any defined or local currency units CHANGE in the future (which empirically, we know is true, eg GHANA Cedi to Shillings), 
        #then we want the "1000 LCU" value to inherit the new base currency at the time of that change.
        #Eg, if in the future, base currency changes to USD and is reported "1050 USD" then we want to know that it's 
        #1050 in the newly revised base currency.  (This circumstance happens very rarely....but has been observed).
        
        #But if we're uploading new base unit definitions, these must happen a-priori to updating the LCU unit.
        #NOTE: As of today (2026), this is not observed in the dataset.  Ghana RSFs were retroactively corrected and historically re-uploaded to convert all Cedi to GHS artificially to maintain historical record for management report
        if (!empty(upload_data[(sequence_rank==current_sequence) & (indicator_id %in% rsf_indicators[data_type %in% "currency",indicator_id])])) {
          
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
            	and urd.sequence_rank = $1::int
          )
          update _temp_upload_rsf_data urd
          set data_unit = 'LCU'
          from updates up 
          where up.rsf_pfcbl_id = urd.rsf_pfcbl_id
            and up.indicator_id = urd.indicator_id
          	and up.reporting_asof_date = urd.reporting_asof_date",
                    params=list(current_sequence))
          
        }
        
        dbExecute(conn,"
          insert into _temp_upload_rsf_data(sequence_rank,
                                            rsf_pfcbl_id,
                                            indicator_id,
                                            reporting_asof_date,
                                            data_value,
                                            data_unit,
                                            data_submitted,
                                            reporting_rsf_pfcbl_id,
                                            reporting_cohort_id,
                                            reporting_segment)
          select distinct on(urd.rsf_pfcbl_id,
                             fx_defined.unit_fx_indicator_id,
                             urd.reporting_asof_date)
            urd.sequence_rank,
            urd.rsf_pfcbl_id,
            fx_defined.unit_fx_indicator_id as indicator_id,
            urd.reporting_asof_date,
            urd.data_value,
            urd.data_unit,
            urd.data_submitted,
            urd.reporting_rsf_pfcbl_id,
            urd.reporting_cohort_id,
            urd.reporting_segment
          from _temp_upload_rsf_data urd
          inner join p_rsf.indicators fx_defined on fx_defined.indicator_id = urd.indicator_id
          left join _temp_upload_rsf_data d_urd on d_urd.rsf_pfcbl_id = urd.rsf_pfcbl_id
                                               and d_urd.indicator_id = fx_defined.unit_fx_indicator_id
                                               and d_urd.reporting_asof_date = urd.reporting_asof_date
                                               and d_urd.reporting_segment = urd.reporting_segment
          left join lateral (select lcu.data_unit_value
                             from p_rsf.rsf_data_current_lcu lcu
                             where lcu.for_rsf_pfcbl_id = urd.rsf_pfcbl_id
                               and lcu.reporting_asof_date <= urd.reporting_asof_date
                             order by lcu.reporting_asof_date desc
                             limit 1) as lcu on true           
          where urd.sequence_rank = $1::int 
            and fx_defined.unit_fx_indicator_id is not null
            and d_urd.inserted_row_number is NULL -- repoted a defined FX indicator but not a value for its governing indicator
          order by 
            urd.rsf_pfcbl_id,
            fx_defined.unit_fx_indicator_id,
            urd.reporting_asof_date,
            urd.data_unit is not distinct from lcu.data_unit_value desc,
            urd.inserted_row_number asc",
          params=list(current_sequence))
        
        current_deletes <- dbGetQuery(conn,"
                              delete from _temp_upload_rsf_data urd
                              where urd.sequence_rank = $1::int
                                and p_rsf.data_value_is_meaningfully_different(input_rsf_pfcbl_id => urd.rsf_pfcbl_id,
                                                      													input_indicator_id => urd.indicator_id,
                                                      													input_reporting_asof_date => urd.reporting_asof_date,
                                                      													input_data_value => urd.data_value,
                                                                                input_data_unit => urd.data_unit,
                                                                                is_user_reporting => true) = false
                              returning inserted_row_number;",
                             params=list(current_sequence))
        
        deletes <- c(deletes,unlist(current_deletes,use.names = F))
        
        has_data <- unlist(dbGetQuery(conn,"
                                      select exists(select * from _temp_upload_rsf_data urd where sequence_rank = $1::int)::bool as has_data",
                                      params=list(current_sequence)))
        
        if (has_data==FALSE) { next; }
        
        #dbGetQuery(conn,"select distinct reporting_cohort_id,sequence_rank from _temp_upload_rsf_data")
        
        
        
        dbExecute(conn,"
          with reporting as (
            select distinct 
            urd.reporting_rsf_pfcbl_id,
            urd.reporting_asof_date,
            urd.reporting_segment
            from _temp_upload_rsf_data urd
            where urd.reporting_cohort_id is null -- on sequence 1, all other reporting_rsf_pfcbl_ids of same date will also be updated in other sequences.
              and urd.sequence_rank = $3::int
            order by reporting_segment
          ),
          cohorts as (
            insert into p_rsf.reporting_cohorts(import_id,
                                                reporting_rsf_pfcbl_id,
                                                reporting_asof_date,                                    
                                                reporting_user_id,
                                                reporting_time,
                                                reporting_type,
                                                is_reported_cohort,
                                                is_calculated_cohort,
                                                data_asof_date)
            select 
            $1::int as import_id,
            reporting.reporting_rsf_pfcbl_id,
            reporting.reporting_asof_date,
            $2::text as reporting_user_id,
            TIMEOFDAY()::timestamptz as reporting_time,
            1 as reporting_type, -- 1=User import
            true as is_reported_cohort,
            false as is_calculated_cohort,
            reporting.reporting_asof_date as data_asof_date
            from reporting
            returning 
              reporting_cohorts.reporting_cohort_id,
              reporting_cohorts.reporting_rsf_pfcbl_id,
              reporting_cohorts.reporting_asof_date
          )
          update _temp_upload_rsf_data urd
          set reporting_cohort_id = cohorts.reporting_cohort_id
          from cohorts
          where urd.reporting_rsf_pfcbl_id = cohorts.reporting_rsf_pfcbl_id
            and urd.reporting_asof_date = cohorts.reporting_asof_date
            and urd.reporting_cohort_id is null",
          params=list(import_id,
                      upload_user_id,
                      current_sequence))
        
        dbExecute(conn,"
          with reporting as (
            select distinct
              urd.rsf_pfcbl_id,
              ind.indicator_id,
              urd.reporting_asof_date,
              min(urd.reporting_cohort_id) as reporting_cohort_id,
              ($2::int)::text as data_value,
              'Reporting for import #' || $2::int as data_submitted,
              NULL::text as data_unit
            from _temp_upload_rsf_data urd
            inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = urd.rsf_pfcbl_id
            inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
                                           and ind.indicator_sys_category = 'entity_reporting'
            where urd.sequence_rank = $1::int
            group by
              urd.rsf_pfcbl_id,
              ind.indicator_id,
              urd.reporting_asof_date
          )
          insert into p_rsf.rsf_data(rsf_pfcbl_id,
                                     indicator_id,
                                     reporting_asof_date,
                                     reporting_cohort_id,
                                     data_value,
                                     data_submitted,
                                     data_unit)
          select 
            urd.rsf_pfcbl_id,
            urd.indicator_id,
            urd.reporting_asof_date,
            urd.reporting_cohort_id,
            urd.data_value,
            urd.data_submitted,
            urd.data_unit
          from _temp_upload_rsf_data urd
          where urd.sequence_rank = $1::int
                  
          union all
                  
          select 
            rep.rsf_pfcbl_id,
            rep.indicator_id,
            rep.reporting_asof_date,
            rep.reporting_cohort_id,
            rep.data_value,
            rep.data_submitted,
            rep.data_unit
          from reporting rep
          where not exists(select * from p_rsf.rsf_data rd
                           where rd.rsf_pfcbl_id = rep.rsf_pfcbl_id
                             and rd.indicator_id = rep.indicator_id
                             and rd.reporting_asof_date = rep.reporting_asof_date
                             and rd.reporting_cohort_id = rep.reporting_cohort_id) -- min reporting_cohort_id should be static for entire import_id
                  ",
          params=list(current_sequence,
                      import_id))
      }
  
      ###############
      ###RSF_REPORTING
      ################
      #This is semi-deprecated and move above to insert each sequence (unless already present)
      #The reason being there are various data triggers that look at presence in rsf_entity_reporting AND ALSO certain types of data
      #so we ensure reporting is present for the very first data insert and/or all data inserts so that all data triggers will correctly verify presence.
      
      #These are all the entities that are *trying* to report
      #But recall that Excel templates are just appending new updates and leaving old data.  So it's possible that some entities are reporting
      #"present" this period with no changes.  But more likely entities with no changes are legacy reporting and should be ignored
      #But since we don't know yet if any changes are present or not, create a provisional entry with -1 value that will either be updated later to actual 
      #reported values (or zero).  If not, we'll delete this entity later as an irrelevant legacy entry.
    
      
      return (deletes)
    }
    
  })
  },
  error=function(e) {
    errors <<- e
    NULL
  })
  
  if (!is.null(errors)) {
    if (grepl("rsf_data_current_names_and_ids_sys_name_udx",as.character(errors$message))) {
      detail <- gsub("^.*ERROR:.*DETAIL:(.*)TEXT:.*$","\\1",as.character(errors$message))
      conflict_name <- gsub("^.*=\\((.*)\\,.*$","\\1",detail)
      conflicts <- dbGetQuery(pool,"
        select 
          sn.sys_name,
          ids.created_in_reporting_asof_date::text
        from p_rsf.rsf_pfcbl_ids ids
        inner join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.rsf_pfcbl_id = ids.rsf_pfcbl_id
        where ids.rsf_pfcbl_id = any(select p_rsf.get_rsf_pfcbl_id_by_sys_name($1::text))",
        params=list(conflict_name))
      
      if (!empty(conflicts)) {
        message <- paste0("\nFailed to create: ",conflict_name,"\n",
                          paste0("Conflict with: ",conflicts$sys_name," created in ",conflicts$created_in_reporting_asof_date,collapse=" \n"),
                          "\nEnsure names and IDs match across all reporting entries (and for all time periods).\n",
                          "If the current information is a data correction for an historic error, that error must be corrected in the historic dataset (which must be deleted, corrected and re-reported)\n",
                          "If the old data is correct and the new data is a change in ID, change the reported data using the format:\n",
                          "OLD >> NEW\n",
                          "For example, if a borrower last QR had ID #4321 and the client has changed its ID to #1234 this QR, then change the ID column in the template to:\n",
                          "4321 >> 1234")
        stop(message)
      }
    } else {
      stop(errors$message)
    }
  }
  
  upload_data[,inserted:=!(1:.N %in% deleted_rows)]
  
  
  return (upload_data)
  
}