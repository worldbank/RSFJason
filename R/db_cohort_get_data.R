
# reporting_cohort_id<-40186 
# view <- "related"
# indicator_ids.simplify = FALSE
#cgd <- db_cohort_get_data(DBPOOL,20270,"related")
db_cohort_get_data <- function(pool,
                               reporting_cohort_id,
                               indicator_ids.simplify=TRUE) #If TRUE then remove system indicators UNLESS those indicators carry flags
{
  

  if(SYS_PRINT_TIMING)  debugtime("db_cohort_get_data: reporting_cohort_id=",reporting_cohort_id,reset=FALSE)
  if (length(reporting_cohort_id) != 1) stop(paste0("Invalid reporting_cohort_id: ",paste0(reporting_cohort_id,collapse=",")))

#conn<-poolCheckout(pool);
  #dbBegin(conn)
  #dbRollback(conn)
  {
    cohort_data <- poolWithTransaction(pool,function(conn) {
  
      source_data <- dbGetQuery(conn,"select 
                                  	rc.rsf_program_id,
                                  	rc.reporting_rsf_pfcbl_id as reporting_entity_pfcbl_id,
                                  	rc.reporting_cohort_id as source_cohort_id,
                                  	rc.source_name,
                                  	rc.source_reference,
                                  	rc.source_note,
                                  	rc.reporting_time,
                                  	rc.reporting_asof_date
                                  	from p_rsf.reporting_cohorts rc
                                  	where rc.reporting_cohort_id = $1::int",
                                params=list(reporting_cohort_id))
      setDT(source_data)
      

      dbExecute(conn,
                "create TEMP table _temp_cohort_data(data_id int,
                                                     rsf_pfcbl_id int,
                                                     reporting_asof_date date,
                                                     indicator_id int,
                                                     data_value text,
                                                     data_unit text,
                                                     data_type text,
                                                     data_is_current bool,
                                                     reporting_cohort_id int,
                                                     indicator_name text,
                                                     data_category text,
                                                     label_id int,
                                                     is_system bool,
                                                     is_calculated bool,
                                                     is_hidden bool,
                                                     is_sys_reporting bool,
                                                     primary key (data_id))
                                                     
                on commit drop;")
      
      
      
      {
        
          dbExecute(conn,"with related_cohorts as (
                      select
                      reporting_cohort_id,
                      reporting_asof_date
                      from p_rsf.reporting_cohorts rc 
                      where reporting_cohort_id = $1::int 
                      	 or parent_reporting_cohort_id = $1::int
                      ),
                      related_data as (
                      	
                      	select 
                      	rd.data_id,
                      	rd.rsf_pfcbl_id,
                      	rdc.check_asof_date as reporting_asof_date,
                      	rd.indicator_id,
                      	rd.data_value,
                      	rd.data_unit,	
                      	rd.reporting_cohort_id
                      	from related_cohorts trc
                      	inner join p_rsf.rsf_data_checks rdc on rdc.evaluated_by_cohort_id = trc.reporting_cohort_id
                      																			and rdc.check_asof_date = trc.reporting_asof_date
                      	inner join p_rsf.rsf_data rd on rd.data_id = rdc.data_id
                      	
                      	union 
                      	
                      	select 
                      	rd.data_id,
                      	rd.rsf_pfcbl_id,
                      	rd.reporting_asof_date,
                      	rd.indicator_id,
                      	rd.data_value,
                      	rd.data_unit,
                      	rd.reporting_cohort_id
                      	from related_cohorts trc
                      	inner join p_rsf.rsf_data rd on rd.reporting_cohort_id = trc.reporting_cohort_id
                      )
                      insert into _temp_cohort_data(data_id,
                                                    rsf_pfcbl_id,
                                                    reporting_asof_date,
                                                    indicator_id,
                                                    data_value,
                                                    data_unit,
                                                    data_type,
                                                    data_is_current,
                                                    reporting_cohort_id,
                                                    indicator_name,
                                                    data_category,
                                                    label_id,
                                                    is_system,
                                                    is_calculated,
                                                    is_hidden,
                                                    is_sys_reporting)
                      select
                      rcd.data_id,
                      rcd.rsf_pfcbl_id,
                      rcd.reporting_asof_date,
                      rcd.indicator_id,
                      rcd.data_value,
                      rcd.data_unit,
                      ind.data_type,
                      rdc.data_id is not null as data_is_current,
                      rcd.reporting_cohort_id,
                      ind.indicator_name,
                      ind.data_category,
                      ind.label_id,
                      ind.is_system,
                      ind.is_calculated,
                      coalesce(isc.is_hidden,false) as is_hidden,
                      coalesce(ind.indicator_sys_category = 'entity_reporting',false) as is_sys_reporting
                      from related_data rcd
                      inner join p_rsf.indicators ind on ind.indicator_id = rcd.indicator_id
                      left join p_rsf.rsf_data_current rdc on rdc.data_id = rcd.data_id
                      left join p_rsf.indicator_sys_categories isc on isc.indicator_sys_category = ind.indicator_sys_category",
                    params=list(reporting_cohort_id))

      }

      cohort_data <- dbGetQuery(conn,"select 
                                      cd.data_id,
                                      cd.rsf_pfcbl_id,
                                      cd.reporting_asof_date,
                                      cd.indicator_id,
                                      cd.data_value,
                                      cd.data_unit,
                                      cd.data_is_current,
                                      cd.reporting_cohort_id,
                                      cd.data_category,
                                      rpc.pfcbl_rank as category_priority,
                                      cd.indicator_name,
                                      cd.data_type,
                                      cd.label_id as indicator_label_id,
                                      cd.is_system,
                                      cd.is_calculated,
                                      cd.is_hidden,
                                      cd.is_sys_reporting as indicator_is_sys_reporting,
                                      nids.rsf_name as entity_name,
                                      nids.id as entity_id
                                      from _temp_cohort_data cd
                                      inner join p_rsf.rsf_pfcbl_categories rpc on rpc.pfcbl_category = cd.data_category
                                      inner join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = cd.rsf_pfcbl_id")
      
      setDT(cohort_data)
      
      flags_data <- dbGetQuery(conn,"select 
                                      rdc.data_id,
                                      tcd.rsf_pfcbl_id,
                                      rdc.indicator_check_id,
                                      tcd.indicator_id as applied_on_indicator_id,
                                      rdc.evaluated_by_cohort_id as reporting_cohort_id,
                                      rdc.check_asof_date as evaluation_asof_date,
                                      ic.check_name,
                                      ic.is_system,
                                      coalesce(scc.config_check_class,ic.check_class) as check_class,
                                      concat(scc.config_comments || '\n',ic.definition) as definiition,
                                      rdc.check_status,
                                      rdc.check_status_comment,
                                      rdc.check_status_user_id,
                                      vai.users_name as check_status_users_name,
                                      rdc.check_message,
                                      rdc.evaluation_id
                                      
                                    from _temp_cohort_data tcd 
                                    inner join p_rsf.rsf_data_checks rdc on rdc.data_id = tcd.data_id
                                    inner join p_rsf.indicator_checks ic on ic.indicator_check_id = rdc.indicator_check_id
                                    left join p_rsf.view_rsf_setup_check_config scc on scc.rsf_pfcbl_id = rdc.rsf_pfcbl_id
                                                                                   and scc.for_indicator_id = rdc.indicator_id
                                                                                   and scc.indicator_check_id = rdc.indicator_check_id
                                    left join p_rsf.view_account_info vai on vai.account_id = rdc.check_status_user_id
                                   
                                    where rdc.check_asof_date = $1::date     --ensure evaluations are for the current source data date
                                      and exists(select * from p_rsf.rsf_data_current rd
                                                 where rd.data_id = tcd.data_id)", #Only include/count flags for current data points
                                                                                   #de-facto active flags on data points that have been over written 
                                                                                   #are resolved by whatever data point overwrote it.
                               params=list(source_data$reporting_asof_date))        

      flags_data <- setDT(flags_data)
      flags_data[,flag_priority:=fcase(check_class=='critical',0,
                                       check_class=='error',1,
                                       check_class=='warning',2,
                                       check_class=='info',3,
                                       default=4)]

      #This error occured when system had reporting_cohort_check_evaluations flags added to historic data points that were not present in the current cohorts/related cohorts
      #Cause seemed due to a bug that flagged wrong data -- resolution was to delete the evaluation_id for the problem check.
      data_id_check <- unique(flags_data$data_id)[!unique(flags_data$data_id) %in% unique(cohort_data$data_id)]
      if (length(data_id_check) > 0) {
        stop("An error has occured in db_cohort_get_data: unreported flags for data_id=",paste0(data_id_check,collapse=" & "),". Please report this message to SYS Admin")
      }
      
      flags_data[,flag_text:=paste0(toupper(check_class),
                                   ": ",check_name,
                                   ifelse(nchar(check_message)>0,
                                          paste0("/ ",check_message),
                                          ""))]
    
      flags_data[check_status=="resolved" | nchar(check_status_comment) > 0,
                 flag_text:=paste0(flag_text," \U22EE ",toupper(check_status),"--",check_status_comment)]
      
      setorder(flags_data,data_id,check_status,flag_priority)
      
      flags_data_active <- flags_data[check_status=="active",.(has_data_flags_active=TRUE),by=.(data_id)]
      flags_data_resolved <- flags_data[check_status=="resolved",.(has_data_flags_resolved=TRUE),by=.(data_id)]
      
      flags_data <- flags_data[,
                               .(data_flags_dt=list(.SD)),
                               by=.(data_id),
                               .SDcols=c("data_id","evaluation_id","indicator_check_id","applied_on_indicator_id","reporting_cohort_id","evaluation_asof_date",
                                        "check_name","is_system","check_class","definition","check_status","check_status_comment","check_status_user_id",
                                        "check_status_users_name","check_message","flag_priority","flag_text")]
      
      cohort_data[,has_any_flags:=FALSE]
      cohort_data[flags_data,
                  `:=`(flags_dt=i.data_flags_dt,
                       has_any_flags=TRUE),
                  on=.(data_id)]
      
      cohort_data[flags_data_active,
                  has_data_flags_active:=i.has_data_flags_active,
                  on=.(data_id)]
      
      cohort_data[is.na(has_data_flags_active),has_data_flags_active:=FALSE]

      cohort_data[flags_data_resolved,
                  has_data_flags_resolved:=i.has_data_flags_resolved,
                  on=.(data_id)]
      cohort_data[is.na(has_data_flags_resolved),has_data_flags_resolved:=FALSE]

      cohort_data <- source_data[cohort_data,on=.(reporting_asof_date)]
      
      cohort_data     
    })    
  }
  
 
  {
    if (empty(cohort_data)) return (NULL)
    
    setorder(cohort_data,category_priority,rsf_pfcbl_id,-indicator_is_sys_reporting,is_system,is_calculated,indicator_name)
    
    #users don't review data at all, removed data_submitted from rsf_data_timeseries as it really doesn't need to be replicated
    #cohort_data[is.na(data_value) & !is.na(data_submitted),data_value:=data_submitted]
    
    if (indicator_ids.simplify==TRUE) cohort_data <- cohort_data[!((is_system == TRUE | is_hidden == TRUE) & has_any_flags==FALSE)]
    
    cohort_data[,indicator_type:="USER"]
    cohort_data[is_calculated==TRUE,indicator_type:="CALCULATED"]
    cohort_data[is_system==TRUE,indicator_type:="SYSTEM"]
  }
  if(SYS_PRINT_TIMING)  debugtime("db_cohort_get_data: done!")
  
  return (cohort_data)
}