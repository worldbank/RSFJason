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
        
        dbExecute(conn,"create index _tmp_seq_idx on _temp_upload_rsf_data(sequence_rank)");
        
        #Asign segments
        {
          #Reporting SEGMENTS will assign the data to a respective cohort for each reporting_asof_date and data at respective program,facility,client+ levels.
          dbExecute(conn,"
          with segments as (
            select
            dense_rank() over(partition by reporting.rsf_pfcbl_id,reporting_asof_date 
                              order by 
                              urd.reporting_asof_date asc,
                              reporting.rsf_pfcbl_id asc) as reporting_segment,
            reporting.rsf_pfcbl_id as reporting_rsf_pfcbl_id,
            urd.rsf_pfcbl_id,
            urd.indicator_id,
            urd.reporting_asof_date
            from _temp_upload_rsf_data urd
            inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = urd.rsf_pfcbl_id
            inner join lateral (select coalesce(ids.rsf_facility_id,ids.rsf_program_id) as rsf_pfcbl_id) as reporting on true
          )
          update _temp_upload_rsf_data urd
          set reporting_segment = rs.reporting_segment,
              reporting_rsf_pfcbl_id = rs.reporting_rsf_pfcbl_id
          from segments rs
          where rs.rsf_pfcbl_id = urd.rsf_pfcbl_id
            and rs.indicator_id = urd.indicator_id
            and rs.reporting_asof_date = urd.reporting_asof_date")
          
        }
        
        #NEW Update Entity local currency units based on what's been previously reported and waht is GOING to be reported
        {
          
          dbExecute(conn,"
            with new_currency_units as (
            
            -- all currency unit data that IS different than what is uploaded now
            select 
            urd.rsf_pfcbl_id,
            urd.indicator_id,
            urd.reporting_asof_date,
            urd.data_value,
            ind.data_category,
            ind.pfcbl_rank,
            ind.indicator_sys_category,
            ind.indicator_name,
            ind.indicator_sys_category = 'entity_currency_unit' as is_defined_lcu
            from _temp_upload_rsf_data urd
            inner join p_rsf.indicators ind on ind.indicator_id = urd.indicator_id
            where ind.indicator_sys_category in ('entity_currency_unit','entity_local_currency_unit')
             and p_rsf.data_value_is_meaningfully_different(input_rsf_pfcbl_id => urd.rsf_pfcbl_id,
                                                            input_indicator_id => urd.indicator_id,
                                                            input_reporting_asof_date => urd.reporting_asof_date,
                                                            input_data_value => urd.data_value,
                                                            input_data_unit => urd.data_unit,
                                                            is_user_reporting => true) = true
            ),
            all_currency_units as (
            
              select 
                NULL as lcu_unit_data_id,
                ncu.rsf_pfcbl_id,
                ncu.reporting_asof_date, -- remember! Some datasets may upload future/historic data that is not the same as template reporting date
                ncu.data_value,
                ncu.is_defined_lcu
              from new_currency_units ncu
            
            union all 
            
              -- these are existing data already in the database, so query all urd.rsf_pfcbl_ids
              
              select 
                lcu.lcu_unit_data_id,
                lcu.for_rsf_pfcbl_id as rsf_pfcbl_id,
                lcu.reporting_asof_date,
                lcu.data_unit_value as data_value,
                lcu.is_defined_lcu
              from p_rsf.rsf_data_current_lcu lcu
              where exists(select * from _temp_upload_rsf_data urd where urd.rsf_pfcbl_id = lcu.for_rsf_pfcbl_id)
              
            ),
            selected_currency_units as (
            
              select distinct on (ids.rsf_pfcbl_id,greatest(acu.reporting_asof_date,ids.created_in_reporting_asof_date))
                acu.lcu_unit_data_id,
                ids.rsf_pfcbl_id as for_rsf_pfcbl_id,
                greatest(acu.reporting_asof_date,ids.created_in_reporting_asof_date) as reporting_asof_date,
                acu.data_value as data_unit_value,
                ft.to_pfcbl_rank as data_id_pfcbl_rank,
                acu.is_defined_lcu
              from all_currency_units acu
              inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = acu.rsf_pfcbl_id
              inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = ids.rsf_pfcbl_id
              
              where ft.pfcbl_hierarchy <> 'child'
                and acu.data_value is distinct from 'LCU'  -- don't include generic LCU, ie, undefined values
                and acu.data_value is not null             -- dot unclude undefiend values
              
              order by 
                ids.rsf_pfcbl_id, -- unique by entity 
                greatest(acu.reporting_asof_date,ids.created_in_reporting_asof_date), -- if its before it was created, use created date
                acu.is_defined_lcu desc, -- prioritize entity defined currency over generic local currency unity (parent)
                acu.reporting_asof_date desc,  -- if there's no defined unit and there are multiple parent local units, prioritize the most recent one
                ft.to_pfcbl_rank desc, -- prioritize the most localist ie, facility over program
                acu.lcu_unit_data_id is null desc -- prioritize new information
              ),
            updates as (
              select 
                urd.data_unit as reported_unit,
                ind.data_unit as indicator_unit,
                lcu.data_unit_value,
                ind.indicator_name,
                urd.inserted_row_number
              from _temp_upload_rsf_data urd
              inner join p_rsf.indicators ind on ind.indicator_id = urd.indicator_id
              left join lateral (select scu.data_unit_value
                                 from selected_currency_units scu
                                 where scu.for_rsf_pfcbl_id = urd.rsf_pfcbl_id
                                   and scu.reporting_asof_date <= urd.reporting_asof_date
                                 order by scu.reporting_asof_date desc
                                 limit 1) as lcu on true 
              where ind.data_type = 'currency'
                and ind.data_unit = 'LCU'
                --and ind.data_unit is distinct from urd.data_unit
                and urd.data_unit is distinct from 'LCU'
                and urd.data_unit is not distinct from lcu.data_unit_value  -- eg, urd.'EUR' is not distct from lcu.'EUR' -> set to 'LCU'
            )
            update _temp_upload_rsf_data urd
            set data_submitted = coalesce(urd.data_submitted,p_rsf.rsf_data_value_unit(urd.data_value,urd.data_unit)),
                data_unit = 'LCU'
            from updates
            where updates.inserted_row_number = urd.inserted_row_number")
          
        }
        
        #Insert required fx_unit_indicator_id indicators and values when template is not reporting these but only reporting defined currencies
        {
          #For example, if there's facility_maximum_risk_amount and facility_maximum_risk_amount_USD
          #Then the _USD metric is governed by the LCU metric via its unit_fx_indicator_id setting
          #Here, if the template data upload is reporting on facility_maximum_risk_amount_USD and NOT on facility_maximum_risk_amount
          #then, add the facility_maximum_risk_amount into the upload to ensure that facility_maximum_risk_amount_USD actually has
          #its governing metric available to govern its fx.
          #
          #Distinct on() to prefer governing indicator to be in LCU terms.  This can be an issue if template is reporting
          #facility_maximum_risk_amount_USD and facility_maximum_risk_amount_EUR and NOT facility_maximum_risk_amount (governer) but
          #facility's LCU is in USD. Therefore, we want to insert and subscribe to facility_maximum_risk_amount and we want its value to be the
          #reported facility_maximum_risk_amount_USD (the LCU term) and not facility_maximum_risk_amount_EUR
          dbExecute(conn,"
          insert into _temp_upload_rsf_data(sequence_rank,
                                            rsf_pfcbl_id,
                                            indicator_id,           -- inserting the governing indicator_id
                                            reporting_asof_date,    -- for the same date
                                            data_value,             -- with the same value
                                            data_unit,              -- with a unit that preferres LCU but since defined fx indicators must have LCU data unit
                                                                    -- they are allowed to have any abitrary unit. But if template is submitting repeats of
                                                                    -- the same metric in different units (which is done with some frequency) then insert
                                                                    -- the governing value and unit as the LCU value over a defined value.
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
          
          -- get all indicators with an fx_defined indicator_id
          inner join p_rsf.indicators fx_defined on fx_defined.indicator_id = urd.indicator_id
                                                and fx_defined.unit_fx_indicator_id is not null

           -- uploaded data corresponding to fx_unit_indicator IDs that are required.                                                                             
           -- not exists: repoted a defined FX indicator but not a value for its governing indicator                             
          where not exists(select true
                           from _temp_upload_rsf_data d_urd 
                           where d_urd.rsf_pfcbl_id = urd.rsf_pfcbl_id
                             and d_urd.indicator_id = fx_defined.unit_fx_indicator_id
                             and d_urd.reporting_asof_date = urd.reporting_asof_date
                             and d_urd.reporting_segment = urd.reporting_segment)
          order by 
            urd.rsf_pfcbl_id,                 -- for each entity
            fx_defined.unit_fx_indicator_id,  -- and each defined fx indicator thats missing a governor
            urd.reporting_asof_date,          -- for each date being reported
            urd.data_unit is not distinct from 'LCU' desc,  -- prioritize: Data unit equals LCU unit (Remember: we've already set units=LCU where applicable as of this update), 
                                                            -- eg facility_maximum_risk_amount_USD is reported AND USD is its LCU
            urd.inserted_row_number asc                     -- prioritize: first come first serve
          ")
        }
        
      }      
     
      #obsolete!
      {
        #Priority 1: Entity local currency units: template$rsf_indicators[indicator_sys_category=="entity_local_currency_unit",indicator_id] (facility and program defined)
        #Priority 2: Entity local currency units: template$rsf_indicators[indicator_sys_category=="entity_currency_unit",indicator_id] (generally loan defined, but could be others?)
        #Priority 3: Everything else
        
        
        
        #current_sequence <- 1
        #current_sequence <- 2
        #current_sequence <- 3
        
        #for (current_sequence in sort(unique(upload_data$sequence_rank))) {
          
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
          # if (!empty(upload_data[(sequence_rank==current_sequence) & (indicator_id %in% rsf_indicators[data_type %in% "currency",indicator_id])])) {
          #   
          #   dbExecute(conn,"
          #   with updates as (
          #     select 
          #     urd.rsf_pfcbl_id,
          #     urd.indicator_id,
          #     urd.reporting_asof_date
          #     from _temp_upload_rsf_data urd
          #     inner join p_rsf.indicators ind on ind.indicator_id = urd.indicator_id
          #     inner join lateral (select * 
          #                         from p_rsf.rsf_data_current_lcu lcu
          #     					          where lcu.for_rsf_pfcbl_id = urd.rsf_pfcbl_id
          #     						          and lcu.reporting_asof_date <= urd.reporting_asof_date
          #     					          order by lcu.reporting_asof_date desc
          #     					          limit 1) as lcu on lcu.data_unit_value = urd.data_unit
          #     where ind.data_type = 'currency'
          #       and ind.data_unit = 'LCU'
          #     	and urd.data_unit is not null
          #     	and urd.data_unit <> 'LCU'
          #     	and urd.sequence_rank = $1::int
          #   )
          #   update _temp_upload_rsf_data urd
          #   set data_unit = 'LCU'
          #   from updates up 
          #   where up.rsf_pfcbl_id = urd.rsf_pfcbl_id
          #     and up.indicator_id = urd.indicator_id
          #   	and up.reporting_asof_date = urd.reporting_asof_date",
          #             params=list(current_sequence))
          #   
          # }
      }        
      
      #delete where not meaningfully different 
      {
        deletes <- dbGetQuery(conn,"
          delete from _temp_upload_rsf_data urd
          where p_rsf.data_value_is_meaningfully_different(input_rsf_pfcbl_id => urd.rsf_pfcbl_id,
                                  													input_indicator_id => urd.indicator_id,
                                  													input_reporting_asof_date => urd.reporting_asof_date,
                                  													input_data_value => urd.data_value,
                                                            input_data_unit => urd.data_unit,
                                                            is_user_reporting => true) = false
          returning inserted_row_number;")
          
        deletes <- unlist(deletes,use.names = F)
          
        has_data <- unlist(dbGetQuery(conn,"
                                      select exists(select true from _temp_upload_rsf_data urd)::bool as has_data"))
          
        if (has_data==FALSE) { return(c()) }
      }
        #dbGetQuery(conn,"select distinct reporting_cohort_id,sequence_rank from _temp_upload_rsf_data")
        
      #Insert the data (and add its reporting entity metric)  
      {
        dbExecute(conn,"
          with reporting as (
            select distinct 
            urd.reporting_rsf_pfcbl_id,
            urd.reporting_asof_date,
            urd.reporting_segment
            from _temp_upload_rsf_data urd
            where urd.reporting_cohort_id is null
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
                      upload_user_id))
        
        dbExecute(conn,"
          with reporting as (
            select distinct
              urd.rsf_pfcbl_id,
              ind.indicator_id,
              urd.reporting_asof_date,
              min(urd.reporting_cohort_id) as reporting_cohort_id,
              ($1::int)::text as data_value,
              'Reporting for import #' || $1::int as data_submitted,
              NULL::text as data_unit
            from _temp_upload_rsf_data urd
            inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = urd.rsf_pfcbl_id
            inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
                                           and ind.indicator_sys_category = 'entity_reporting'
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
                  
          union all
                  
          select 
            rep.rsf_pfcbl_id,
            rep.indicator_id,
            rep.reporting_asof_date,
            rep.reporting_cohort_id,
            rep.data_value,
            rep.data_submitted,
            rep.data_unit
          from reporting rep",
          params=list(import_id))
      }
      
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