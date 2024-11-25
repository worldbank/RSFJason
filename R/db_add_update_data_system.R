db_add_update_data_system <- function(pool,
                                      system_upload_data)
{
  expected_cols <- c("current_data_id",
                     "rsf_pfcbl_id",
                     "indicator_id",
                     "reporting_asof_date",
                     "data_unit",
                     "data_value",
                     "flagged")

  if(!all(expected_cols %in% names(system_upload_data))) {
    stop(paste0("db_add_update_data_system expects system_upload_data to define: ",paste0(expected_cols,collapse=",")))
  }
  
  system_upload_data[,n:=.N,
                     by=.(rsf_pfcbl_id,
                          indicator_id)]
  
  redundants <- system_upload_data[n>1]
  if (!empty(redundants) > 0) {
    print(redundants)
    stop("System Data upload has redundant data")
  }
  redundants <- NULL
  system_upload_data[,n:=NULL]
  
  t1<-Sys.time()
  #SYS_PRINT_TIMING <- FALSE
  #if(SYS_PRINT_TIMING)  debugtime("db_add_update_data_system","start")  
  #system_upload_data <<- as.data.frame(system_upload_data)
  #setDT(system_upload_data)
  system_upload_data <- system_upload_data[,.(current_data_id,
                                              rsf_pfcbl_id,
                                              indicator_id,
                                              reporting_asof_date,
                                              data_unit,
                                              data_value,
                                              flagged)]
  
  #browser()
  #conn <- poolCheckout(pool)
  #dbBegin(conn)
  #dbRollback(conn)

  results_added <- poolWithTransaction(pool,function(conn) {
    
      
      #format(object.size(cohort_upload_data),units="KB")
      #format(object.size(cohort_upload_data[,..upload_cols]),units="KB") -- on testing cuts IO size nearly in half to filter uploaded
      #format(object.size(cohort_upload_data[,.(rsf_pfcbl_id,indicator_id,reporting_asof_date,reporting_rsf_pfcbl_id,data_flags,data_value,data_type,data_unit,data_submitted)]),units="KB")
      #format(object.size(cohort_upload_data[,.(rsf_pfcbl_id,indicator_id,reporting_asof_date,reporting_rsf_pfcbl_id,data_flags,data_value,data_type,data_unit,indicator_name,data_submitted)]),units="KB")
      
      {
        
        print(paste0("- - - - Uploading ",nrow(system_upload_data)," data points"))
        
        
        dbExecute(conn,"create TEMP TABLE _temp_upload_system_data(current_data_id int,
                                                                   rsf_pfcbl_id int,
                                                                   indicator_id int,
                                                                   reporting_asof_date date,
                                                                   reporting_cohort_id int,
                                                                   data_unit text,
                                                                   data_value text,
                                                                   calculated_cohort_id int,
                                                                   flagged bool default false)
                        ON COMMIT DROP;")
        
        
        dbAppendTable(conn=conn,
                      name="_temp_upload_system_data",
                      value=system_upload_data)
        
        dbExecute(conn,"alter table _temp_upload_system_data add primary key(rsf_pfcbl_id,
                                                                               indicator_id,
                                                                               reporting_asof_date)")
        
        #Has meanwhile been overwritten since validation calculation or something else amiss.  So reset it to be revalidated.
        dbExecute(conn,"update _temp_upload_system_data usd
                        set current_data_id = NULL
                        where usd.current_data_id is not NULL
                          and not exists(select *
                                         from p_rsf.rsf_data_current rdc
                                         where rdc.data_id = usd.current_data_id)")
      }
      
      
      {
        #x <- as.data.table(dbGetQuery(conn,"select * from _temp_upload_system_data"))
        # x <- as.data.table(dbGetQuery(conn,"
        #                               select * from _temp_upload_system_data usd
        #                               where p_rsf.data_value_is_meaningfully_different(usd.rsf_pfcbl_id,
        #                                                                    usd.indicator_id,
        #                                                                    usd.reporting_asof_date,
        #                                                                    usd.data_value,
        #                                                                    usd.data_unit) = false"))
        #print(system_upload_data)
        #somehow non-changed data got into the upload... so don't upload it, but do update its validation, ie, report that it's been calculated (with the result equal to the current value)
        #possibly a re-calculation for a flow data type?
        nx <- dbExecute(conn,"
                        with redundancies as MATERIALIZED (
                          delete from _temp_upload_system_data usd
                          where p_rsf.data_value_is_meaningfully_different(usd.rsf_pfcbl_id,
                                                                           usd.indicator_id,
                                                                           usd.reporting_asof_date,
                                                                           usd.data_value,
                                                                           usd.data_unit) = false
                          returning current_data_id,
                                    rsf_pfcbl_id,
                                    indicator_id,
                                    reporting_asof_date
                        )
                        delete from p_rsf.rsf_data_calculation_evaluations dce
                        where exists(select * from redundancies red
                                    where red.rsf_pfcbl_id = dce.rsf_pfcbl_id
                                      and red.indicator_id = dce.indicator_id
                                      and red.reporting_asof_date = dce.calculation_asof_date)")
        
      
        
        #Find the most recent reporting cohort that reported an entry for this rsf_pfcbl_id:
        #Either this entity reported directly something for this timeline OR 
        #it didn't report anything and it has an entry in rsf_pfcbl_reporting because a child entity reported something.
        #(And if it doesn't have an entry in rsf_pfcbl_reporting then it can't insert calculated results anyway.)
        dbExecute(conn,"with reporting as MATERIALIZED (
                          select distinct on(reporting.rsf_pfcbl_id)
                            reporting.rsf_pfcbl_id,
                            coalesce(rc.parent_reporting_cohort_id,rc.reporting_cohort_id) as reporting_cohort_id
                          from (
                          	select 
                              usd.rsf_pfcbl_id,
                              rd.reporting_cohort_id 
                          	from _temp_upload_system_data usd
                          	inner join p_rsf.rsf_data rd on rd.rsf_pfcbl_id = usd.rsf_pfcbl_id
                          															and rd.reporting_asof_date = usd.reporting_asof_date														
                          	where exists(select * from p_rsf.indicators ind 
                          							 where ind.indicator_id = rd.indicator_id
                          								 and ind.indicator_sys_category = 'entity_reporting')
                          
                          	union all
                          
                          	select 
                              usd.rsf_pfcbl_id,
                              rd.reporting_cohort_id 
                          	from _temp_upload_system_data usd
                          	inner join p_rsf.rsf_pfcbl_reporting rpr on rpr.rsf_pfcbl_id = usd.rsf_pfcbl_id
                          																					and rpr.reporting_asof_date = usd.reporting_asof_date
                          	inner join p_rsf.rsf_data rd on rd.data_id = rpr.created_by_data_id
                          ) reporting
                          inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = reporting.reporting_cohort_id
                          order by 
                            reporting.rsf_pfcbl_id,
                            reporting.reporting_cohort_id desc
                        ),
                        -- create program cohorts can uniquely nest parent cohorts.
                        parent_cohorts as (
                      		select reporting.rsf_pfcbl_id,coalesce(rc.parent_reporting_cohort_id,rc.reporting_cohort_id) as reporting_cohort_id
                      		from reporting 
                      		inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = reporting.reporting_cohort_id
                      	)
                        update _temp_upload_system_data usd
                        set reporting_cohort_id = rep.reporting_cohort_id
                        from parent_cohorts rep
                        where rep.rsf_pfcbl_id = usd.rsf_pfcbl_id
                          and exists(select * from p_rsf.reporting_cohorts rc
                                     where rc.reporting_cohort_id = rep.reporting_cohort_id
                                       and rc.parent_reporting_cohort_id is NULL);") #ensure it is a parent-level cohort that was selected
       
        #x <- dbGetQuery(conn,"select * from _temp_upload_system_data");setDT(x);x
        dbExecute(conn,"with calculations as MATERIALIZED (
                          select
                          	usd.rsf_pfcbl_id,
                          	usd.indicator_id,
                          	usd.reporting_cohort_id,
                          	max(rc.reporting_cohort_id) as calculated_cohort_id
                          from _temp_upload_system_data usd
                          inner join p_rsf.reporting_cohorts rc on rc.parent_reporting_cohort_id = usd.reporting_cohort_id
                          where rc.is_calculated_cohort = true
                          group by
                            usd.reporting_cohort_id,
                            usd.rsf_pfcbl_id,
                            usd.indicator_id
                        )
                        update _temp_upload_system_data usd
                        set calculated_cohort_id = calcs.calculated_cohort_id
                        from calculations calcs
                        where usd.rsf_pfcbl_id = calcs.rsf_pfcbl_id
                          and usd.indicator_id = calcs.indicator_id
                          and usd.reporting_cohort_id = calcs.reporting_cohort_id
                          and not exists(select * from p_rsf.rsf_data rd
                        	               where rd.rsf_pfcbl_id = calcs.rsf_pfcbl_id
                        									 and rd.indicator_id = calcs.indicator_id
                        									 and rd.reporting_cohort_id = calcs.calculated_cohort_id)")
        
        dbExecute(conn,"
                  with create_cohorts as (
                    insert into p_rsf.reporting_cohorts(reporting_asof_date,
                    																	  reporting_rsf_pfcbl_id,
                    																		reporting_user_id,
                    																		reporting_time,
                    																		data_asof_date,
                    																		source_name,
                    																		source_reference,
                    																		source_note,
                    																		rsf_program_id,
                    																		parent_reporting_cohort_Id,
                    																		is_calculated_cohort,
                    																		is_reported_cohort,
                    																		is_redundancy_cohort,
                    																		from_reporting_template_id)
                    																		
                    select 
                      rc.reporting_asof_date,
                      rc.reporting_rsf_pfcbl_id,
                      rc.reporting_user_id,
                      timeofday()::timestamptz as reporting_time,
                      rc.reporting_asof_date as data_asof_date,
                      rc.source_name,
                      'CALCULATIONS COHORT: Created by system' as source_reference,
                      NULL::text as source_note,
                      rc.rsf_program_id,
                      rc.reporting_cohort_id as parent_reporting_cohort_Id,
                      true as is_calculated_cohort,
                      false as is_reported_cohort,
                      false as is_redundancy_cohort,
                      NULL::int as from_reporting_template_id
                    from p_rsf.reporting_cohorts rc
                    where exists(select * from _temp_upload_system_data usd
                                 where usd.reporting_cohort_id = rc.reporting_cohort_id
                                   and usd.calculated_cohort_id IS NULL)
                    returning 
                      parent_reporting_cohort_id as reporting_cohort_id,
                      reporting_cohort_id as calculated_cohort_id
                  )
                  update _temp_upload_system_data usd
                  set calculated_cohort_id = cc.calculated_cohort_id
                  from create_cohorts cc
                  where cc.reporting_cohort_id = usd.reporting_cohort_id
                    and usd.calculated_cohort_id is null;")
      }
    
      {
        nx <- dbExecute(conn,"insert into p_rsf.rsf_data(rsf_pfcbl_id,
                                                         reporting_asof_date,
                                                         reporting_cohort_id,
                                                         indicator_id,
                                                         data_value,
                                                         data_unit)
                                select ud.rsf_pfcbl_id,
                                       ud.reporting_asof_date,
                                       ud.calculated_cohort_id,
                                       ud.indicator_id,
                                       ud.data_value,
                                       ud.data_unit
                                from _temp_upload_system_data ud")
      
                       
      }    
    
      return (nx)
  })
  
  if(SYS_PRINT_TIMING) debugtime("db_add_update_data_system","Done!",format(Sys.time()-t1))
  #setDT(results_data)
  return (results_added)
}
