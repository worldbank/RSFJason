SERVER_DASHBOARD_RUN_OPTIONS_RESET <- function() {
  options <- SERVER_DASHBOARD_RUN_OPTIONS_INIT
  for(opt in names(options)) {
    if (!identical(SERVER_DASHBOARD_RUN_OPTIONS[[opt]],options[[opt]])) SERVER_DASHBOARD_RUN_OPTIONS[[opt]] <- options[[opt]]
  }
}

#SETS:
#rsfdash_reporting_asof_date
#rsfdash_view_currency
#resets DASH_DATA_SELECTED_INDICATORS_WRITE
SERVER_DASHBOARD_SPECIAL_INDICATORS <- c(":exclude:reported",
                                         ":exclude:calculated",
                                         ":exclude:system",
                                         ":exclude:unsubscribed",
                                         ":include:loan",
                                         ":include:borrower",
                                         ":include:client",
                                         ":include:facility",
                                         ":include:program",
                                         ":include:IDs",
                                         ":include:NAMES",
                                         ":expand:calculations-shallow",
                                         ":expand:calculations-deep",
                                         ":expand:indicators")

SERVER_DASHBOARD_CELL_INFO <- reactiveVal("")
SERVER_DASHBOARD_REFRESH <- reactiveVal(0)
SERVER_DASHBOARD_CURRENT_QUERY <- reactiveVal(data.table())
SERVER_DASHBOARD_CURRENT_INDICATORS <- reactiveVal(data.table())
SERVER_DASHBOARD_CHOICE_INDICATORS <- reactiveVal(character(0))

SERVER_DASHBOARD_RUN_OPTIONS_INIT <- list(rsf_pfcbl_ids=numeric(0),
                                          indicator_names=character(0),
                                          asof_dates=c(), #currentest date for the selected IDs is rank 1.
                                          
                                          format_raw=FALSE, #will download the native extract, no family.  No browser UI functions.
                                          format_blank="text",
                                          format_unchanged="gray",
                                          format_exceldates=FALSE,
                                          format_filter="",
                                          format_pivot="DATA",
                                          format_pivot_category="parent",
                                          syscols = c("reporting_asof_date", 
                                                      "SYSID",              
                                                      "rsf_full_name"),
                                          flags_filter="",
                                          flags_display = c("active"),
                                          
                                          name_filter=c(),
                                          reporting_filter="",

                                          fx_currency="LCU", #FX can not be allowed if FUTURE dates are selected
                                          fx_force_global=TRUE,
                                          fx_concatenate_LCU=TRUE, #when different currency units in same col, merge number+unit
                                          fx_reported_date=FALSE,
                                          fx_audit=FALSE)

SERVER_DASHBOARD_RUN_OPTIONS <- reactiveValues()

SERVER_DASHBOARD_DOWNLOAD_FILENAME <- eventReactive(c(SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids,
                                                      SERVER_DASHBOARD_RUN_ASOF_DATES()), { 

  client_ids <- unique(as.numeric(SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids))
  reporting_dates <- SERVER_DASHBOARD_RUN_ASOF_DATES()
  
  if (!isTruthy(reporting_dates)) {
    reporting_dates <- "asof UNDEFINED"
  } else {
    reporting_dates <- sort(ymd(reporting_dates))
    reporting_dates <- format_asof_date_label(reporting_dates)
    reporting_dates <- gsub("-.*$","",reporting_dates)
    
    if (length(reporting_dates) >= 3) reporting_dates <- paste0("from ",reporting_dates[1]," to ",reporting_dates[length(reporting_dates)])
    reporting_dates <- paste0(reporting_dates,collapse=", ")
  }

  clients <- SERVER_DASHBOARD_CLIENTS_LIST()
  clients <- clients[rsf_pfcbl_id %in% client_ids]
  
  reporting_names <- ""
  if (empty(clients) || nrow(clients) >= 3) {
    
    program <- SELECTED_PROGRAM()  
    reporting_names <- paste0("RSF PROGRAM ",program$program_name)
    
    if (nrow(clients) >= 3) reporting_names <- paste0(reporting_names," (for ",nrow(clients)," Clients)")
    
  } else {
    reporting_names <- paste0("RSF CLIENT ",paste0(sort(clients$client_name),collapse=", "))
  }

  gen <- format.Date(now(),"%b%d %Hh%M")
  default_filename <- paste0(reporting_names," - ",reporting_dates," - ",gen,".xlsx")
  
  if (nchar(default_filename) >= 100) default_filename <- paste0("RSF DATA Extract - ",gen,".xlsx")
  
  return(default_filename)

})

SERVER_DASHBOARD_CLIENTS_LIST <- eventReactive(c(SELECTED_PROGRAM_CLIENTS_LIST()), {
  
  MODE_CROSS_PROGRAM <- TRUE
  
  if (MODE_CROSS_PROGRAM==TRUE) {
    selected_program_id <- SELECTED_PROGRAM_ID()
    if (!isTruthy(selected_program_id)) return (NULL)
    
    if(SYS_PRINT_TIMING) debugtime("eventReactive: SELECTED_PROGRAM_CLIENTS_LIST")
    
    clients <- DBPOOL %>% dbGetQuery("
    select distinct on (ids.rsf_program_id,ids.rsf_facility_id)
      ids.rsf_program_id,
      ids.rsf_facility_id,
      ids.rsf_client_id,
      ids.rsf_pfcbl_id,
      ids.pfcbl_category,
      ids.pfcbl_category_rank as pfcbl_rank,
      case when nids.pfcbl_category not in ('global','client') 
           then nids.pfcbl_category || ':' || nids.rsf_name
           else nids.rsf_name 
      end as client_name,
      nids.name,
      nids.id,
      nids.created_in_reporting_asof_date
    from p_rsf.rsf_pfcbl_ids ids
    inner join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = ids.rsf_pfcbl_id
    where ids.pfcbl_category_rank <= 3
      and ids.pfcbl_category <> 'program'
      and ids.rsf_program_id = $1::int
    order by ids.rsf_program_id,ids.rsf_facility_id,ids.rsf_client_id nulls last",
    params=list(selected_program_id))
    
    setDT(clients)
    return (clients)
  } else {
    return (SELECTED_PROGRAM_CLIENTS_LIST())
  }
  
  
},ignoreNULL = FALSE)

SERVER_DASHBOARD_INDICATORS <- eventReactive(c(SERVER_DASHBOARD_REPORT_SELECTED(),
                                               RSF_INDICATORS()), { 
                                                 
  if (empty(RSF_INDICATORS())) return (NULL)
                                                 
  dash_indicators <- RSF_INDICATORS()[,
                                   .(indicator_pfcbl_rank,
                                     indicator_id,
                                     indicator_name,
                                     indicator_sys_category,
                                     sort_category=data_category,
                                     is_system)]
  setorder(dash_indicators,
        -indicator_pfcbl_rank,
        is_system,
        indicator_name)
  
  dash_indicators <- rbindlist(list(dash_indicators,
                                 data.table(indicator_pfcbl_rank=-1,
                                            indicator_id=-1 * (seq_along(SERVER_DASHBOARD_SPECIAL_INDICATORS)),
                                            indicator_name=SERVER_DASHBOARD_SPECIAL_INDICATORS,
                                            indicator_sys_category=as.character(NA),
                                            sort_category="special",
                                            is_system=TRUE)))
  
  if (length(SERVER_DASHBOARD_REPORT_SELECTED()) != 0) {
  dash_indicators <- rbindlist(list(dash_indicators,
                                   data.table(indicator_pfcbl_rank=-1,
                                              indicator_id=0,
                                              indicator_name=paste0(":report:",SERVER_DASHBOARD_REPORT_SELECTED()$report_title),
                                              indicator_sys_category=as.character(NA),
                                              sort_category="special",
                                              is_system=TRUE)))
  
  }
  
  return (dash_indicators)
  
}, ignoreNULL=FALSE)

#Converts the selected indicator names (and special names and report names) to indicator_ids
SERVER_DASHBOARD_SELECTED_INDICATORS <- eventReactive(SERVER_DASHBOARD_RUN_OPTIONS$indicator_names, {
  
  if (empty(SERVER_DASHBOARD_INDICATORS())) return (NULL)

  selected_dashboard_indicators <- SERVER_DASHBOARD_INDICATORS()[indicator_name %in% SERVER_DASHBOARD_RUN_OPTIONS$indicator_names]
  selected_dashboard_indicator_names <- SERVER_DASHBOARD_RUN_OPTIONS$indicator_names
  if (empty(selected_dashboard_indicators)) return (NULL)
  
  if (any(selected_dashboard_indicators$indicator_id %in% 0)) {
    
    report_indicator_names <- unlist(SERVER_DASHBOARD_REPORT_SELECTED()$for_indicator_names)
    
    parent_report_indicator_names <- function(report_names) {
      parent_indicator_names <- c()
      if (any(grepl(":report:",report_names),na.rm=T)) {
        parent_reports <- grep(":report:",report_indicator_names,value=T)
        parent_reports <- gsub("^:report:","",parent_reports)
        for (rn in parent_reports) {
          pr_names <- DBPOOL %>% dbGetQuery("select for_indicator_names from
                                                                p_rsf.reports
                                                                where report_title = $1::text",
                                                                params=list(rn))
          pr_names <- fromJSON(pr_names$for_indicator_names)
          parent_indicator_names <- c(parent_indicator_names,
                                      pr_names)
        }
      }
      
      if (any(grepl(":report:",parent_indicator_names),na.rm=T)) {
        parent_indicator_names <- c(parent_report_indicator_names(report_names=parent_indicator_names),
                                    parent_indicator_names)
      }
      return (parent_indicator_names)
    }
    parent_indicator_names <- parent_report_indicator_names(report_names = report_indicator_names)
    
    report_location <- which(selected_dashboard_indicators$indicator_id==0)
    exclude_report_names <- selected_dashboard_indicators[1:report_location,indicator_name]
    #If user puts an indicator first, before the report name, filter-out the report's instance of the indicator so that the requsted indicator
    #becomes first in sort. 
    report_indicator_names <- report_indicator_names[!(report_indicator_names %in% exclude_report_names)]
    report_indicator_names <- c(parent_indicator_names,report_indicator_names)
    selected_dashboard_indicator_names <- c(selected_dashboard_indicator_names,report_indicator_names)
    #selected_dashboard_indicator_names <- c(parent_indicator_names,selected_dashboard_indicator_names,report_indicator_names)
    #which(SERVER_DASHBOARD_INDICATORS()$indicator_name %in% report_indicator_names)
    selected_dashboard_indicators <- rbindlist(list(selected_dashboard_indicators,
                                                    SERVER_DASHBOARD_INDICATORS()[indicator_name %in% report_indicator_names]))
    
  }
  
  
  selected_dashboard_indicators <- selected_dashboard_indicators[grepl("^:report:",indicator_name)==FALSE]
  selected_dashboard_indicators[,n:=.N,
                                by=.(indicator_id)]
  
  selected_dashboard_indicators <- selected_dashboard_indicators[n==1]
  #selected_dashboard_indicator_names <- unique(selected_dashboard_indicator_names)
  selected_dashboard_indicators[,n:=NULL]
  
  selected_dashboard_indicators[,
                                sort:=1:.N]
  
  for(i in seq_along(selected_dashboard_indicator_names)) {
    selected_dashboard_indicators[indicator_name==selected_dashboard_indicator_names[[i]],
                                  sort:=i]
    
  }
  
  #special :include: are generic to the indicator definitions
  #but :exclude: and :expand: can be dependent on calculation definitions and subscription monitoring for the individual clients selectec
  include_special <- selected_dashboard_indicators[grepl("^:include:",indicator_name)==TRUE,indicator_name]
  if (length(include_special) > 0) {
    
    for (inc in include_special) {
      
      include <- NULL
      included_categories <- unique(selected_dashboard_indicators$sort_category)
      if (length(included_categories)==0) included_categories <- "client"
      
      if (inc==":include:loan") include <- SERVER_DASHBOARD_INDICATORS()[sort_category=="loan"]
      else if (inc==":include:borrower") include <- SERVER_DASHBOARD_INDICATORS()[sort_category=="borrower"]
      else if (inc==":include:client") include <- SERVER_DASHBOARD_INDICATORS()[sort_category=="client"]
      else if (inc==":include:facility") include <- SERVER_DASHBOARD_INDICATORS()[sort_category=="facility"]
      else if (inc==":include:program") include <- SERVER_DASHBOARD_INDICATORS()[sort_category=="program"]
      else if (inc==":include:IDs") include <- SERVER_DASHBOARD_INDICATORS()[indicator_sys_category %in% c("id","rank_id") & 
                                                                             sort_category %in% included_categories]
      else if (inc==":include:NAMES") include <- SERVER_DASHBOARD_INDICATORS()[indicator_sys_category %in% c("name","nickname","rank_id") &
                                                                               sort_category %in% included_categories]
      
      include_sort <- selected_dashboard_indicators[indicator_name==inc,sort]
      dont_include <- selected_dashboard_indicators[sort < include_sort,indicator_name]
      include <- include[!(indicator_name %in% dont_include)]
      include[,sort:=include_sort]
      selected_dashboard_indicators <- rbindlist(list(selected_dashboard_indicators,
                                                      include))
      
    }
  }
  
  selected_dashboard_indicators[,n:=.N,
                                by=.(indicator_id)]
  
  selected_dashboard_indicators <- selected_dashboard_indicators[n==1]
  selected_dashboard_indicators[,n:=NULL]
  
  setorder(selected_dashboard_indicators,
           sort,
           -indicator_pfcbl_rank,
           indicator_name)
  
  selected_dashboard_indicators[,
                                sort:=1:.N]
  
  setorder(selected_dashboard_indicators,
           sort)
  
  selected_dashboard_indicators <- selected_dashboard_indicators[,.(indicator_id,
                                                                    indicator_name,
                                                                    sort)]

  return (selected_dashboard_indicators)
  
})


SERVER_DASHBOARD_VALID_ASOF_DATES <- eventReactive(SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids, {

  program_id <- SELECTED_PROGRAM_ID()
  selected_clients <- as.numeric(SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids)
  
  if (!isTruthy(program_id)) return (NULL)
  
  if (!isTruthy(selected_clients)) { 
    return (NULL)
  }
  
  
  valid_dates <- DBPOOL %>% dbGetQuery("
    with dates as (                                       
      select 
        prd.valid_reporting_date::text as text_date,
        valid_reporting_date > (now()::date) as is_future
      from p_rsf.rsf_program_reporting_dates prd
      where prd.rsf_program_id = $1::int
        and prd.valid_reporting_date 
      	between (select min(created_in_reporting_asof_date) 
      	         from p_rsf.rsf_pfcbl_ids ids 
      					 where ids.rsf_pfcbl_id = any(select unnest(string_to_array($2::text,','))::int))
            and (select max(reporting_asof_date) 
          	     from p_rsf.rsf_pfcbl_reporting rpr
          			 where rpr.rsf_pfcbl_id = any(select unnest(string_to_array($2::text,','))::int))
      order by prd.valid_reporting_date desc
    )
    select 
      text_date,
      (dense_rank() over(order by text_date desc))::int as date_rank,
      text_date as date_value
    from dates where is_future = false
    
    union all
    
    select 
      'Future Date' as text_date,
      0::int as date_rank,
      array_to_string(array_agg(text_date order by text_date),',') as date_value
    from dates where is_future = true",
      params=list(program_id,
                  paste0(selected_clients,collapse=",")))
  
  setDT(valid_dates)
  
  valid_dates[,date_rank:=as.numeric(date_rank)]
  
  valid_dates <- valid_dates[is.na(date_value)==FALSE]
  setorder(valid_dates,
           date_rank)
  
  return (valid_dates)  
},
ignoreNULL = FALSE)


SERVER_DASHBOARD_RUN_ASOF_DATES <- eventReactive(c(SERVER_DASHBOARD_VALID_ASOF_DATES(),
                                                   SERVER_DASHBOARD_RUN_OPTIONS$asof_dates), {
   
   selected_asof_dates <- na.omit(unique(as.numeric(SERVER_DASHBOARD_RUN_OPTIONS$asof_dates)))
   if (empty(SERVER_DASHBOARD_VALID_ASOF_DATES())) return (c())
   if (!isTruthy(selected_asof_dates)) return (c())

   date_values <- c()   
   if (any(is.infinite(selected_asof_dates))) {
     date_values <- SERVER_DASHBOARD_VALID_ASOF_DATES()$date_value
   } else {
     date_values <- c(SERVER_DASHBOARD_VALID_ASOF_DATES()[date_rank %in% selected_asof_dates,date_value],
                      SERVER_DASHBOARD_VALID_ASOF_DATES()[(date_rank-1-max(date_rank)) %in% selected_asof_dates,date_value])
   }

   date_values <- strsplit(date_values,split=",",fixed=T) #because rank 0, future dates, are concatenated.
   date_values <- sort(unlist(date_values))
   return (date_values)
},ignoreNULL = FALSE)

SERVER_DASHBOARD_DATA_RUN <- eventReactive(SERVER_DASHBOARD_REFRESH(), {
  
  SERVER_DASHBOARD_CURRENT_QUERY(data.table())
  SERVER_DASHBOARD_REFRESH()  
  
}) %>% debounce(500)

SERVER_DASHBOARD_RUN_AUTORUN <- eventReactive(c(input$tabset_dashboard,
                                                input$server_dashboard__autorun), {
  
  if (!(input$tabset_dashboard %in% "dashboard")) return (FALSE)
  else return(isTruthy(as.logical(input$server_dashboard__autorun)))
})

observeEvent(input$server_dashboard__autorun, {
  if (is.null(input$server_dashboard__autorun) || all(is.na(as.logical(input$server_dashboard__autorun)))) {
    do_run <- FALSE
  } else {
    do_run <- as.logical(input$server_dashboard__autorun)
  }
  
  if (do_run==TRUE)  SERVER_DASHBOARD_REFRESH(SERVER_DASHBOARD_REFRESH()+1)
  
}, ignoreNULL=FALSE)

#############################################################################################
#Queries database and caches result in SERVER_DASHBOARD_CURRENT_QUERY
observeEvent(SERVER_DASHBOARD_DATA_RUN(), {
  
  run <- SERVER_DASHBOARD_RUN_AUTORUN()
  if (!isTruthy(run)) return (NULL)
  
  selected_client_ids <- unique(as.numeric(SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids))
  selected_indicators <- SERVER_DASHBOARD_SELECTED_INDICATORS()
  selected_asof_dates <- SERVER_DASHBOARD_RUN_ASOF_DATES()
  
  
  
  fx_currency <- SERVER_DASHBOARD_RUN_OPTIONS$fx_currency
  fx_reported_date <- as.logical(SERVER_DASHBOARD_RUN_OPTIONS$fx_reported_date)
  fx_force_global <- as.logical(SERVER_DASHBOARD_RUN_OPTIONS$fx_force_global)
  if (!isTruthy(fx_currency) || fx_currency=="LCU") fx_currency <- NA
  if (!isTruthy(fx_reported_date)) fx_reported_date <- FALSE
  if (!isTruthy(fx_force_global)) fx_force_global <- FALSE
  
  
  
  if (!isTruthy(selected_client_ids) ||
      empty(selected_indicators) ||
      !isTruthy(selected_asof_dates)) {
    SERVER_DASHBOARD_CURRENT_QUERY(data.table())
    SERVER_DASHBOARD_CURRENT_INDICATORS(data.table())
    return (NULL)
  }
  
  include.rsf_name <- any("rsf_full_name" %in% SERVER_DASHBOARD_RUN_OPTIONS$syscols)
  include.status <- any(c("reporting_status","reporting_expected") %in% SERVER_DASHBOARD_RUN_OPTIONS$syscols)
  fx_concatenate_LCU <- SERVER_DASHBOARD_RUN_OPTIONS$fx_concatenate_LCU
  sys_cols <- SERVER_DASHBOARD_RUN_OPTIONS$syscols
  
  flags_display <- intersect(c("active","resolved"),
                             tolower(SERVER_DASHBOARD_RUN_OPTIONS$flags_display))
  
  #flags_query <- c() #c() means include flags (for later filtering or manipulation, active and resolved)
  #NULL means do not include any flags data at all -- query will be faster
  #when flags_display is ONLY active AND OPTIONS$flags_query is TRUE, then query only data that has active flags at the filtered-level
  #This is overall much faster than querying a potentially much larger dataset.

  expand_indicators <- grep("^:expand:calculations",selected_indicators$indicator_name,value=T)
  if (length(expand_indicators) > 0) {
    
    deep <- FALSE
    if (":expand:calculations-deep" %in% expand_indicators) deep <- TRUE
    
    expand_formulas <- DBPOOL %>% dbGetQuery("
      with params as (                                             
        select distinct
        unnest(case when $3::bool = false then formula_indicator_ids
                    when $3::bool = true then formula_indicator_id_requirements
               end) as parameter_id
        from p_rsf.rsf_pfcbl_id_family fam 
        inner join p_rsf.view_rsf_program_facility_indicator_subscriptions fis on fis.rsf_pfcbl_id = fam.parent_rsf_pfcbl_id
        inner join p_rsf.indicator_formulas indf on indf.formula_id = fis.formula_id
        where fam.child_rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
          and fam.parent_pfcbl_category = 'facility'
          and fis.indicator_id = any(select unnest(string_to_array($2::text,','))::int)
      )
      select
      ind.indicator_id,
      ind.indicator_name
      from p_rsf.indicators ind
      where exists(select * from params where params.parameter_id = ind.indicator_id)",
      params=list(paste0(selected_client_ids,collapse=","),
                  paste0(selected_indicators$indicator_id,collapse=","),
                  deep))
    
    setDT(expand_formulas)
    if (!empty(expand_formulas)) {
      expand_rank <- 0
      if (deep==TRUE) {
        expand_rank <- selected_indicators[indicator_name==":expand:calculations-deep",sort]
      } else {
        expand_rank <- selected_indicators[indicator_name==":expand:calculations-shallow",sort]
      }
      
      expand_formulas <- expand_formulas[!indicator_id %in% selected_indicators[sort < expand_rank,indicator_id]]
      expand_formulas[,
                      sort:=expand_rank]
      
      setcolorder(expand_formulas,
                  neworder=names(selected_indicators))
      
      selected_indicators <- rbindlist(list(selected_indicators,
                                            expand_formulas))
      selected_indicators[,n:=.N,
                          by=.(indicator_id)]
      selected_indicators <- selected_indicators[n==1]
      selected_indicators[,n:=NULL]
     
    }
  }
  
  exclude_indicators <- grep("^:exclude:",selected_indicators$indicator_name,value=T)
  if (length(exclude_indicators) > 0) {
    
    query_indicators <- DBPOOL %>% dbGetQuery("
    select 
      fis.indicator_id,
      fis.indicator_name,
      bool_and(ind.is_system) as is_system,
      bool_or(fis.is_subscribed) as is_subscribed,  --is subscribed by any
      bool_and(fis.formula_id is NOT NULL AND coalesce(indf.overwrite='allow',false)) as is_calculated --is calculated by all, all the time (ie, overwritten always)
    from p_rsf.rsf_pfcbl_id_family fam 
    inner join p_rsf.view_rsf_program_facility_indicator_subscriptions fis on fis.rsf_pfcbl_id = fam.parent_rsf_pfcbl_id
    inner join p_rsf.indicators ind on ind.indicator_id = fis.indicator_id
    left join p_rsf.indicator_formulas indf on indf.formula_id = fis.formula_id
    where fam.child_rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
      and fam.parent_pfcbl_category = 'facility'
      and fis.indicator_id = any(select unnest(string_to_array($2::text,','))::int)
    group by fis.indicator_id,fis.indicator_name",
    params=list(paste0(selected_client_ids,collapse=","),
                paste0(selected_indicators$indicator_id,collapse=",")))
    
    setDT(query_indicators)
    exclude_ids <- c()
    
    if (":exclude:reported" %in% exclude_indicators) {
      exclude_rank <- selected_indicators[indicator_name==":exclude:reported",sort]
      exclude_ids <-  c(exclude_ids,
                        selected_indicators[(indicator_id %in% query_indicators[is_calculated==FALSE,indicator_id]) &
                                              sort > exclude_rank,
                                            indicator_id])
    }
    
    if (":exclude:calculated" %in% exclude_indicators) {
      exclude_rank <- selected_indicators[indicator_name==":exclude:calculated",sort]
      exclude_ids <-  c(exclude_ids,
                        selected_indicators[(indicator_id %in% query_indicators[is_calculated==TRUE,indicator_id]) &
                                              sort > exclude_rank,
                                            indicator_id])
    }
    
    if (":exclude:system" %in% exclude_indicators) {
      exclude_rank <- selected_indicators[indicator_name==":exclude:system",sort]
      exclude_ids <-  c(exclude_ids,
                        selected_indicators[(indicator_id %in% query_indicators[is_system==TRUE,indicator_id]) &
                                              sort > exclude_rank,
                                            indicator_id])
    }
    if (":exclude:unsubscribed" %in% exclude_indicators) {
      exclude_rank <- selected_indicators[indicator_name==":exclude:unsubscribed",sort]
      exclude_ids <-  c(exclude_ids,
                        selected_indicators[(indicator_id %in% query_indicators[is_subscribed==FALSE,indicator_id]) &
                                              sort > exclude_rank,
                                            indicator_id])
    }
    
    selected_indicators <- selected_indicators[!(indicator_id %in% exclude_ids)]
  }
  
  setorder(selected_indicators,
           sort,
           indicator_name)
  selected_indicators[,sort:=1:.N]
  setorder(selected_indicators,
           sort)
  
  SERVER_DASHBOARD_CURRENT_INDICATORS(selected_indicators)
  
  rsf_data <- NULL  
  withProgress(session=session,
               message="Loading data...", {
                 for (asof in selected_asof_dates) {
                   incProgress(amount=(1/length(selected_asof_dates)),
                               message=paste0("Loading ",asof," data..."))
                   
                   if (is.na(asof)) next;
                   rd <- DBPOOL %>% db_data_get_current(rsf_pfcbl_ids.familytree=selected_client_ids,
                                                        indicator_ids=selected_indicators$indicator_id,
                                                        reporting_current_date=ymd(asof),
                                                        fx_currency=fx_currency,
                                                        include.sys_name=TRUE,
                                                        include.rsf_name=TRUE,
                                                        include.status=include.status,
                                                        include.flags=flags_display,
                                                        fx_force_global=fx_force_global,
                                                        fx_reported_date=fx_reported_date,
                                                        fx_concatenate_LCU=fx_concatenate_LCU)
                   rsf_data <- rbindlist(list(rsf_data,
                                              rd))
                   
                   incProgress(amount=(1/length(selected_asof_dates)))
                 }
               })
  
  if (empty(rsf_data)) {
    SERVER_DASHBOARD_CURRENT_QUERY(data.table())
    return (NULL)
  }
  
  
  setorder(rsf_data,
           rsf_pfcbl_id,
           current_asof_date)
  
  rsf_data[is.na(data_id),
           data_value_updated:=(1:.N)==1, #For timeseries requests, the never-entered data will be "updated" at first timeseries requst and not updated thereafter
           by=.(rsf_pfcbl_id,
                indicator_id)]
  
  if (!empty(rsf_data[,.(n=.N),by=.(rsf_pfcbl_id,current_asof_date,indicator_name)][n>1])) {
    stop("Error: multiple rows received with rsf_data[,.(n=.N),by=.(rsf_pfcbl_id,current_asof_date,indicator_name)][n>1]")
  }
  
  setorder(rsf_data,
           rsf_pfcbl_id,
           -current_asof_date)
  
  #Checks
  {
    
    rsf_data[,
             `:=`(checks="",
                  check_html="",
                  check_count=0)]
    
    if (length(flags_display) > 0) {
      
      rsf_data[,checks_row:=1:.N]
      
      data_flags <- rsf_data[,
                             unlist(flags,recursive=F),
                             by=.(checks_row)]
      
      
      # if (!empty(data_flags)) {
      #   data_flags <- data_flags[check_status %in% display_flags]
      # }  
      
      if (!empty(data_flags)) {
        data_flags[,
                   check_message:=gsub("'","",check_message)]
        
        data_flags[,
                   check_message:=gsub('"','',check_message)]
        
        data_flags[,
                   check_message:=gsub('[<>]','-',check_message)]
        
        data_flags[,`:=`(checks=paste0(toupper(check_class),
                                       ": ",toupper(check_status),
                                       " ",check_name,
                                       ifelse(nchar(check_message)>0,
                                              paste0("/ ",check_message),
                                              "")),
                         check_html="",
                         check_count=0)]
        
        #Consolidate multiple checks of same class on same data point into one "icon" or check text
        data_flags <- data_flags[,.(checks=paste0(checks,collapse=" {AND ALSO} \\n"),
                                    check_html=mapply(format_html_check_icon,
                                                      check_class=check_class,
                                                      title=paste0(checks,collapse="
                                                                      "),
                                                      css_class="flag-dashboard"),
                                    check_count=.N),
                                 by=.(checks_row,
                                      check_class)]
        
        #Consolidate multiple checks of different classes into a single entry
        data_flags <- data_flags[,
                                 .(checks=paste0(checks,collapse="\\n"),
                                   check_html=paste0("<div style='float:left;display:inline-block;'>",
                                                     paste0(check_html,collapse=" "),
                                                     "</div>"),
                                   check_count=sum(check_count)),
                                 by=.(checks_row)]
        
        rsf_data[data_flags,
                 `:=`(checks=i.checks,
                      check_html=i.check_html,
                      check_count=i.check_count),
                 on=.(checks_row)]
        
        # rsf_data <- data_flags[rsf_data,
        #                        on=.(checks_row)]
      }      
      rsf_data[,checks_row:=NULL]
      
    }
  }
  
  
  
  omit_cols <- c("parent_rsf_pfcbl_category",
                 "indicator_id",
                 "data_value", #current_data_value reflects fx conversion, where applicable, ie 1   -> 0.89
                 "data_unit",  #current_data_unit reflects fx conversion, where applicable,  ie USD -> EUR
                 "exchange_rate_date",
                 "fx_indicator_id",
                 "fx_calculation_category",
                 "currency_ratio",
                 "exchange_rate_data_id",
                 "exchange_rate")
  
  omit_cols <- c(omit_cols,
                 intersect(names(rsf_data),
                           setdiff(SERVER_DASHBOARD_OPTIONS_SYSCOLS,
                                   sys_cols)))
  
  keep_cols <- names(rsf_data)[!(names(rsf_data) %in% omit_cols)]
  
  fx_data_audit <- NULL
  if (isTruthy(SERVER_DASHBOARD_RUN_OPTIONS$fx_audit) &
      !is.na(fx_currency)) {
    fx_keep_cols <- c(keep_cols,
                      "exchange_rate_date",
                      "fx_calculation_category",
                      "currency_ratio",
                      "exchange_rate",
                      "exchange_rate_data_id")
    
    fx_data_audit <- rsf_data[grepl(paste0("@",fx_currency,"$"),indicator_name),
                              ..fx_keep_cols]
    
    if (!empty(fx_data_audit)) {
      fx_data_audit[,
                    data_type:="text"]
      fx_data_audit[,
                    data_value_updated:=current_asof_date==exchange_rate_date]
      fx_data_audit[,
                    `:=`(indicator_name=paste0(indicator_name,":FX_audit"),
                         current_value=paste0(currency_ratio," ",
                                              exchange_rate," ",
                                              fx_calculation_category,"@",
                                              exchange_rate_date))]
      
      fx_data_audit <- fx_data_audit[,
                                     ..keep_cols]
    } else {
      fx_data_audit <- NULL
    }
  }
  
  rsf_data <- rsf_data[,..keep_cols]
  
  if (!empty(fx_data_audit)) {
    rsf_data <- rbindlist(list(rsf_data,
                               fx_data_audit))
  }
  
  setnames(rsf_data,
           old=c("current_value","current_unit","data_value_updated","data_id"),
           new=c("value","unit","updated","id"))
  
  if (any(names(rsf_data) %in% SERVER_DASHBOARD_OPTIONS_SYSCOLS)) {
    rename_cols <- SERVER_DASHBOARD_OPTIONS_SYSCOLS[SERVER_DASHBOARD_OPTIONS_SYSCOLS %in% names(rsf_data)]
    setnames(rsf_data,
             old=rename_cols,
             new=names(rename_cols))  
    
    #Aggregating dates by rsf_pfcbl_id because when cast wide to rsf_family_data, each row will represent an aggregate of currentest reporting dates
    #for all indicators selected and included in the requested as-of date.
    #When multiple indicators and/or multiple timeseries dates are selected, including this column could be very confusing....
    #More so, when multiple entities are selected (eg, facility and loan), the rsf_family_data will contain the currentest dates of the SYSID entity
    #Ie, loan-level data dates and any facility-level data-data will be omitted.
    if (any(names(rsf_data)=="SYSID_currentest_date"))  {
      rsf_data[,SYSID_currentest_date:=as.character(SYSID_currentest_date)]
      
      rsf_data[is.na(SYSID_currentest_date),
               SYSID_currentest_date:="{NEVER}"]
      
      rsf_data[,
               SYSID_currentest_date:=paste0(sort(unique(SYSID_currentest_date)),collapse=", "),
               by=.(rsf_pfcbl_id)]
      
      
    }
  }
  
  
  #FORMATTING and html, text data columns
  {
    
    format_blank <- SERVER_DASHBOARD_RUN_OPTIONS$format_blank
    format_unchanged <- SERVER_DASHBOARD_RUN_OPTIONS$format_unchanged
    #FORMATTING
    {
      rsf_indicators <- RSF_INDICATORS()
      rsf_data[,`:=`(text=value)]
      
      # rsf_data[is.na(data_value_updated),
      #          data_value_updated:=FALSE]
      
      rsf_data[!is.na(text) & data_type %in% c("number","percent") &
                 indicator_name %in% rsf_indicators[is.na(indicator_sys_category),indicator_name],
               text:=as.character(
                 format(round(as.numeric(text),3),big.mark = ",",scientific=F))]
      
      rsf_data[!is.na(text) & data_type=="currency",
               text:=as.character(
                 format(
                   round(as.numeric(text),2),
                   big.mark = ",",scientific = F))]
      
      # 
      #       #remove currency codes from currency values
      #       rsf_data[!is.na(text) & !is.na(data_unit),
      #                text:=paste(text,data_unit)]
      
      rsf_data[,html:=text]
      
      
      { #BLANKS
        #If its an intentional change then keep default formating (ie, black) else gray-out data that's simpy never been entered.
        #If it changed to NA then it must have changed from something to nothing; ie, it's been deleted
        if (format_blank=="empty") {
          rsf_data[is.na(value) ==TRUE, #&updated==FALSE,
                   `:=`(html="",
                        text="")]
          
        } else if (format_blank=="text") {
          rsf_data[is.na(value) ==TRUE, #&updated==FALSE,
                   `:=`(html=fcase(is.na(id)==TRUE,"{BLANK}",
                                   is.na(id)==FALSE,"{NOTHING}",
                                   default="{UNKNOWN}"),
                        
                        text=fcase(is.na(id)==TRUE,"{BLANK}",
                                   is.na(id)==FALSE,"{NOTHING}",
                                   default="{UNKNOWN}"))]
          
        }
        
        if (format_unchanged=="gray") {
          rsf_data[updated==FALSE,
                   html:=paste0("<div style='color:rgba(180,180,180,0.8);'>",text,"</div>")]
          
        } else if (format_unchanged=="arrow") {
          rsf_data[updated==FALSE,
                   html:=paste0("<div style='color:rgba(145,15,60,0.8);font-size:1.3em;' title='",gsub("'","",text)," reported in the past.'>&uarr;</div>")]
          
        } else if (format_unchanged=="empty") {
          rsf_data[updated==FALSE,
                   `:=`(text="",
                        html="")]
        }
        
        # rsf_data[is.na(data_value) &
        #          data_value_updated==TRUE, #means that I updated it to be blank! (ie, whatever it was before wasn't blank...so deleted)
        #         `:=`(html="{DELETED}",
        #              text="{DELETED}")]
        # 
        # rsf_data[is.na(data_value),
        #          `:=`(html="<div style='color:rgba(220,220,220,0.8);'>{NOTHING CHANGED}</div>",
        #               text="{NOTHING CHANGED}")]
      }
    }
    
    rsf_data[!is.na(check_html),
             html:=paste0("<div>",
                          check_html,
                          "<div style='float:left;display:inline-block;'>",html,"</div>",
                          "</div>")]
  }
  
  setnames(rsf_data,
           old="sys_name",
           new="SYSNAME")
  
  
  if (identical(SERVER_DASHBOARD_RUN_OPTIONS$format_raw,TRUE)) {
    
    #rsf_data[,flags:=NULL] #it's a list object and won't render into Excel.
    keep_cols <- c("rsf_pfcbl_id",
                   "pfcbl_category",
                   "indicator_name",
                   "id",
                   "updated",
                   "current_asof_date",
                   "value",
                   "unit",
                   "flags",
                   "html",
                   "text",
                   "checks")
    
    keep_cols <- c(grep("^[A-Z]",names(rsf_data),value=T),
                   keep_cols)
    
    rsf_family_data <- rsf_data[,..keep_cols]
    setnames(rsf_family_data,
             old=c("rsf_pfcbl_id","pfcbl_category"),
             new=c("SYSID","SYSCATEGORY"))
    #SERVER_DASHBOARD_CURRENT_QUERY(rsf_data) 
    # showNotification(type="message",
    #                  h3("RAW mode is selected: use Export to download dataset. Dashboard browser is not available in RAW mode."))
    # return (NULL)
  } else {
    
    rsf_family_data <- db_data_pivot_family(rsf_data=rsf_data,
                                            value.vars=c("id",
                                                         "value",
                                                         "updated",
                                                         "flags",
                                                         "html",
                                                         "text",
                                                         "checks"))
    
    if (any(names(rsf_family_data)=="SYSID_currentest_date"))  {
      syscategory <- unique(rsf_data[pfcbl_rank==max(pfcbl_rank),pfcbl_category])
      rsf_family_data[SYSCATEGORY != syscategory,
                      SYSID_currentest_date:="{NEVER}"]
      
      setnames(rsf_family_data,
               old="SYSID_currentest_date",
               new=paste0(toupper(syscategory),"_currentest_date"))
      
    }  
  }
  #browser()
  setnames(rsf_family_data,
           old="current_asof_date",
           new="REPORTING_asof_date")
  
  #rsf_family_data[,SYSCATEGORY:=NULL]
  setorder(rsf_family_data,
           SYSID,
           -REPORTING_asof_date)  
  
  if (any("reporting_qdate" %in% SERVER_DASHBOARD_OPTIONS_SYSCOLS,na.rm=T)) {
    rsf_family_data[,
                    REPORTING_qdate:=format_asof_date_label(REPORTING_asof_date)]
  }
  
  selected_indicator_names <- selected_indicators$indicator_name
  selected_col_names <- lapply(paste0("^",selected_indicator_names,"\\."),
                               grep,
                               x=names(rsf_family_data),
                               value=T)
  selected_col_names <- unlist(selected_col_names)
  selected_col_names <- c(names(rsf_family_data)[!names(rsf_family_data) %in% selected_col_names],
                          selected_col_names)
  
  rsf_family_data <- rsf_family_data[,
                                     ..selected_col_names]
  

  if (any(names(rsf_family_data)=="RSFNAME",na.rm=T) &
      all(grepl("^RANK\\d+",rsf_family_data$RSFNAME))) {
    
    ranknames <- rsf_family_data$RSFNAME
    ranks <- as.numeric(gsub("^RANK(\\d+).*$","\\1",ranknames))
    ranknames <- gsub("^RANK\\d+","",ranknames)
    
    ranknames <- paste0("RANK",
                        str_pad(as.character(ranks),
                                width=nchar(as.character(max(ranks))),
                                side="left",
                                pad=0),
                        ranknames)
    rsf_family_data[,RSFNAME:=ranknames]
  }
  
  if (any(duplicated(names(rsf_family_data)))) {
    
    stop(paste0("Duplicated column names: ",
                paste0(names(rsf_family_data)[duplicated(names(rsf_family_data))],collapse=" & ")))
  }
  
  SERVER_DASHBOARD_CURRENT_QUERY(rsf_family_data)
}, ignoreInit=TRUE,priority=-1)
#############################################################################################

#Applies filters: subsets
SERVER_DASHBOARD_DATA_DISPLAY_UPDATE <- eventReactive(c(SERVER_DASHBOARD_CURRENT_QUERY(),
                                                        input$server_dashboard__name_filter,
                                                        SERVER_DASHBOARD_RUN_OPTIONS$format_pivot,
                                                        SERVER_DASHBOARD_RUN_OPTIONS$format_filter,
                                                        SERVER_DASHBOARD_RUN_OPTIONS$flags_filter,
                                                        SERVER_DASHBOARD_RUN_OPTIONS$reporting_filter,
                                                        SERVER_DASHBOARD_RUN_OPTIONS$format_pivot_category), {

  return (list(current_query=SERVER_DASHBOARD_CURRENT_QUERY(),
               name_filter=input$server_dashboard__name_filter,
               format_pivot=SERVER_DASHBOARD_RUN_OPTIONS$format_pivot,
               format_filter=SERVER_DASHBOARD_RUN_OPTIONS$format_filter,
               flags_filter=SERVER_DASHBOARD_RUN_OPTIONS$flags_filter,
               reporting_filter=SERVER_DASHBOARD_RUN_OPTIONS$reporting_filter,
               format_pivot_category=SERVER_DASHBOARD_RUN_OPTIONS$format_pivot_category))
                                                           
}) %>% debounce(250)

#Applies pivot: appearance, columns
SERVER_DASHBOARD_DATA_DISPLAY <- eventReactive(SERVER_DASHBOARD_DATA_DISPLAY_UPDATE(), {
                                                   
 #dashboard_data <- rsf_family_data
 selected_indicators <- SERVER_DASHBOARD_SELECTED_INDICATORS()
 #selected_indicators <<- SERVER_DASHBOARD_SELECTED_INDICATORS()
 #dfilters <<- SERVER_DASHBOARD_DATA_DISPLAY_UPDATE()
 dfilters <- SERVER_DASHBOARD_DATA_DISPLAY_UPDATE()
 
 dcq <- dfilters$current_query
 
 if (empty(dcq)) return (NULL)
 if (empty(selected_indicators)) return (NULL)
 
 format_RAW <- identical(SERVER_DASHBOARD_RUN_OPTIONS$format_raw,TRUE)
 #if (identical(SERVER_DASHBOARD_RUN_OPTIONS$format_raw,TRUE)) return(dcq)
 
 ##dd <<- as.data.table(SERVER_DASHBOARD_DATA())
 ##dcq <<- as.data.frame(dcq)
 ##dashboard_data <- as.data.table(dcq)
 
 ##dfilters <<- dfilters
 ##rsf_indicators<<-RSF_INDICATORS()
 ##selected_indicators <<- SERVER_DASHBOARD_SELECTED_INDICATORS()
 
 if (any(duplicated(names(dcq)))) stop(paste0("Duplicated column names: ",
                                                         paste0(names(dcq)[duplicated(names(dcq))],collapse=" & ")))
 dcq_cols <- names(dcq)
 dashboard_data <- dcq[,..dcq_cols] #we want to create a shallow copy of dcq because we'll be modifying it later, eg, changing .flags from list to logical
 
 rsf_indicators <- RSF_INDICATORS()
 syscategory <- unique(tolower(dashboard_data$SYSCATEGORY))
 
 
 sysidrank <- fcase(syscategory=="global",0,
                    syscategory=="program",1,
                    syscategory=="facility",2,
                    syscategory=="client",3,
                    syscategory=="borrower",4,
                    syscategory=="loan",5,
                    default=NA)
 
 sysidrank <- max(sysidrank,na.rm=T)
 

 pivot <- toupper(dfilters$format_pivot)
 if (!pivot %in% c("DATA","NAME","DATE")) pivot <- "DATA"
 format_pivot_category <- dfilters$format_pivot_category
 
 eufilter <- toupper(dfilters$format_filter)
 ##name filter
 {
   nfilter <- as.numeric(dfilters$name_filter)
   
   if (isTruthy(nfilter)) {
     dashboard_data <- dashboard_data[SYSID %in% nfilter]
   }
 }
 
 #reporting filter
 
 rfilter <- as.logical(SERVER_DASHBOARD_RUN_OPTIONS$reporting_filter)
 if (!is.na(rfilter)) {
   
   dashboard_data[,REPORTING:=TRUE]
  
   dd_dates <- as.character(unique(dashboard_data$REPORTING_asof_date))
   for (d in dd_dates) {
     reporting_dates <- DBPOOL %>% dbGetQuery("
       select distinct on (rep.rsf_pfcbl_id)
         rep.rsf_pfcbl_id,
         rep.is_reporting
       from p_rsf.view_rsf_pfcbl_id_is_reporting rep
       where rep.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
         and rep.reporting_asof_date <= $2::date
       order by rep.rsf_pfcbl_id,rep.reporting_asof_date desc",
       params=list(paste0(unique(dashboard_data$SYSID),collapse=","),
                   d))
     setDT(reporting_dates)
     reporting_dates[,REPORTING_asof_date:=as.Date(d)]

     termination_dates <- DBPOOL %>% dbGetQuery("
     select distinct on (fam.child_rsf_pfcbl_id)
     fam.child_rsf_pfcbl_id as rsf_pfcbl_id,
     (rdc.data_value is not null and ((rdc.data_value)::date) < $2::date) as is_terminated
     from p_rsf.rsf_pfcbl_id_family fam
     inner join p_rsf.indicators ind on ind.data_category = fam.parent_pfcbl_category
     inner join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = fam.parent_rsf_pfcbl_id
                                          and rdc.indicator_id = ind.indicator_id
                                          and rdc.reporting_asof_date <= $2::date
     where fam.child_rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
       and fam.parent_pfcbl_category = 'facility'
       and ind.indicator_sys_category = 'entity_completion_date'
     order by 
     fam.child_rsf_pfcbl_id,
     rdc.data_value desc nulls last",
     params=list(paste0(unique(dashboard_data$SYSID),collapse=","),
                 d))
     
     setDT(termination_dates)
     termination_dates[,REPORTING_asof_date:=as.Date(d)]
     
     dashboard_data[reporting_dates,
                    REPORTING:=i.is_reporting,
                    on=.(SYSID=rsf_pfcbl_id,
                         REPORTING_asof_date)]
     
     dashboard_data[termination_dates[is_terminated==TRUE],
                    REPORTING:=FALSE,
                    on=.(SYSID=rsf_pfcbl_id,
                         REPORTING_asof_date)]
     
     reporting_dates <- NULL
   }
  
   if (rfilter==TRUE) {
     dashboard_data <- dashboard_data[REPORTING==TRUE]
   } else {
     dashboard_data <- dashboard_data[REPORTING==FALSE]
   }
   dashboard_data[,REPORTING:=NULL]
 }
 
 #dd <- dfilters$current_query
 #dashboard_data <- as.data.table(as.data.frame(dd))
 #flag filter -- how flags are filtered is dependent on pivot
 {
   ffilter <- dfilters$flags_filter
   all_flags <- c("critical","warning","error","info")
   if ("any" %in% ffilter) ffilter <- unique(all_flags,
                                             ffilter)
   
   ffilter <- intersect(all_flags,ffilter)
   fcols <- NULL
   if (format_RAW==FALSE) {
     fcols <- grep("\\.flags$",names(dashboard_data),value=T)
   } else {
     fcols <- "flags"
   }
   
   if (length(fcols) > 0 && length(ffilter) > 0) {
     
     
     dashboard_data[,
                    REPORTING_row:=1:.N]
     
     #change the .flags list() column to a TRUE/FALSE logical column on whether the column is in the requested filter
     for (fcol in fcols) {
       has_flags <- sapply(dashboard_data[[fcol]],is.null)==FALSE
       
       if (any(has_flags)) {
         
         rr <- dashboard_data[has_flags,
                              unlist(.SD[[fcol]],recursive=F),
                              by=.(REPORTING_row)]
         
         if (!empty(rr)) {
           
           rrhf <- rr[check_class %in% ffilter,REPORTING_row]
           rrnf <- setdiff(which(has_flags),rrhf)
           if (length(rrnf)>0) has_flags[rrnf] <- FALSE
           #print(paste(fcol,": ",paste0(rrnf,collapse=",")))
         }
       }
       
       set(dashboard_data,
           i=NULL,
           j=fcol,
           value=as.logical(has_flags))
     }
     dashboard_data[,
                    REPORTING_row:=NULL]
   }    
   #If we don't want to apply a flag filter, then omit these columns now to simplify that dataset.
   #Otherwise, we're responsibly for filtering after pivoting.
   if (length(fcols) > 0 && length(ffilter) == 0) {
     keep_cols <- names(dashboard_data)[!(names(dashboard_data) %in% fcols)]
     dashboard_data <- dashboard_data[,..keep_cols]
   }
   
 }
 
 #Convert data cols to character and melt to long (note RAW data is already character values and in long format)
 if (format_RAW==FALSE) {
   display_cols <- grep("^[A-Z]",names(dashboard_data),value=T)
   
   value_cols <- grep("\\.[a-z]+$",names(dashboard_data),value=T)
   
   cols <- c(display_cols,
             value_cols)
   
   if (!all(names(dashboard_data) %in% cols)) stop("Lost column names")
   
   dashboard_data <- dashboard_data[,..cols]
   
   for (col in value_cols) {
     if (class(dashboard_data[[col]])=="character") next;
     set(dashboard_data,
         i=NULL,
         j=col,
         as.character(dashboard_data[[col]]))
  }
  
 
   #Melt to long
   {
     dashboard_data <- melt.data.table(dashboard_data,
                                       id.vars = display_cols,
                                       value.name="VALUE",
                                       value.factor = F,
                                       variable.name="indicator_name_parameter",
                                       variable.factor = F)
     
     inp <- data.table(indicator_name_parameter=unique(dashboard_data$indicator_name_parameter))
     inp[,
         c("INDICATOR","PARAMETER"):=tstrsplit(indicator_name_parameter,"\\.")]
     
     inp[,indicator_basename:=gsub("[@\\.].*$","",INDICATOR)]
     inp[rsf_indicators,
         indicator_pfcbl_rank:=i.indicator_pfcbl_rank,
         on=.(indicator_basename=indicator_name)]
     
     if (format_pivot_category=="parent") {
       inp[,format_pivot_category:=indicator_pfcbl_rank < sysidrank]
       
     } else if (format_pivot_category=="facility") {
       inp[,format_pivot_category:=indicator_pfcbl_rank < 2]
       
     } else if (format_pivot_category=="client") {
       inp[,format_pivot_category:=indicator_pfcbl_rank < 3]
       
     } else if (format_pivot_category=="borrower") {
       inp[,format_pivot_category:=indicator_pfcbl_rank < 4]
     }
     
     dashboard_data[inp,
                    `:=`(INDICATOR=i.INDICATOR,
                         PARAMETER=i.PARAMETER,
                         PIVOTCAT=i.format_pivot_category),
                    on=.(indicator_name_parameter)]
     
     dashboard_data[,indicator_name_parameter:=NULL]
     dashboard_data[,SYSCATEGORY:=NULL]
     
     if (!format_pivot_category %in% c("parent","facility","client","borrower")) {
       dashboard_data[,
                      PIVOTCAT:=FALSE]
     }
   }
 
 } else {
   display_cols <- grep("^[A-Z]",names(dashboard_data),value=T)
   display_cols <- c(display_cols,
                     "indicator_name")
   value_cols <- names(dashboard_data)[!names(dashboard_data) %in% display_cols]
   for (col in value_cols) {
     if (class(dashboard_data[[col]])=="character") next;
     set(dashboard_data,
         i=NULL,
         j=col,
         as.character(dashboard_data[[col]]))
   }
   
   dashboard_data <- melt.data.table(dashboard_data,
                                     id.vars = display_cols,
                                     value.name="VALUE",
                                     value.factor = F,
                                     variable.name="PARAMETER",
                                     variable.factor = F)
   setnames(dashboard_data,
            old="indicator_name",
            new="INDICATOR")
 }
 
 if (!empty(dashboard_data[,.(n=.N),by=.(SYSID,REPORTING_asof_date,INDICATOR,PARAMETER)][n>1])) stop("Duplicated values")
 
 #browser()
 #dashboard_data <- as.data.table(dd);dashboard_data[,SYSCATEGORY:=NULL];pivot <- "DATA"
 {
   #ffilter by pivot
   { 
     if (any(dashboard_data$PARAMETER=="flags",na.rm=T) && 
         length(ffilter) > 0) {
       
       byon <- NULL
       {
         if (pivot=="DATA") byon <- c("SYSID","REPORTING_asof_date")
         else if (pivot=="NAME") byon <- c("REPORTING_asof_date","INDICATOR")
         else if (pivot=="DATE") byon <- c("SYSID","INDICATOR")
       }
       
       #filter will be TRUE if any of the pivoted date columns have any flag
       ffilters <- dashboard_data[PARAMETER=="flags",
                                  .(filter=any(as.logical(VALUE),na.rm=T)),
                                  by=byon]
       
       dashboard_data[,filter:=FALSE]
       dashboard_data[ffilters,
                      filter:=i.filter,
                      on=c(byon)]
       
       dashboard_data <- dashboard_data[filter==TRUE] #when we filter FOR flags, then exclude where all columns have no flags
       dashboard_data[,filter:=NULL]
     }
     
     dashboard_data <- dashboard_data[PARAMETER !="flags"]
   }
   
   #Cast var by pivot:
   cast_var <- NA
   omitcols <- c()
   
   if (pivot=="DATA") {
     if (any(names(dashboard_data)=="PIVOTCAT",na.rm=T)) dashboard_data[,PIVOTCAT:=NULL] #Nothing to pivot as pivot by indicator name is default
     cast_var <- "INDICATOR"
     
   
   
   
   } else if (pivot=="NAME") {

     cast_var <- "SYSID"
     if ("RSFNAME" %in% names(dashboard_data)) {
       cast_var <- "RSFNAME"
       omitcols <- grep("^SYSID$|^SYSNAME$",names(dashboard_data),value=T)
       
     } else if ("SYSNAME" %IN% names(dashboard_data)) {
       cast_var <- "SYSNAME"
       omitcols <- grep("^SYSID$|^RSFNAME$",names(dashboard_data),value=T)
     }

   
   
   
   } else if (pivot=="DATE") {
     
     date_cols <- grep("^[A-Z]+.*date$",names(dashboard_data),value=T)

     if (any(grepl("currentest_date",date_cols))) {
       cast_var <- grep("currentest",date_cols,value=T)
       
     } else if (any(grepl("qdate",date_cols))) {
       cast_var <- grep("qdate",date_cols,value=T)
       omitcols <- date_cols[-which(date_cols==cast_var)]
       
     } else if (any(grepl("asof_date",date_cols))) {
       cast_var <- grep("asof_date",date_cols,value=T)
       omitcols <- date_cols[-which(date_cols==cast_var)]
     }
   }
   
   if (!cast_var %in% names(dashboard_data)) stop(paste0("Cannot find cast column '",cast_var,"'"))
   
   
   #PIVOT on CATEGORY
   #id_vars <- grep("^[A-Z]",names(dashboard_data),value=T)
   if (any(dashboard_data$PIVOTCAT==TRUE) &&
       pivot %in% c("NAME","DATE")) {
     
     pivotcat <- dashboard_data[PIVOTCAT==TRUE]
     dashboard_data <- dashboard_data[PIVOTCAT==FALSE]
     
     dashboard_data[,PIVOTCAT:=NULL]
     pivotcat[,PIVOTCAT:=NULL]
     pivotcat[,(cast_var):=NULL]
     

     
     pivot_id_vars <-  grep("^[A-Z]",names(pivotcat),value=T)
     pivot_id_vars <-  pivot_id_vars[!pivot_id_vars %in% c("INDICATOR","VALUE")]
     pivot_cast_formula <- as.formula(paste0(paste(paste0("`",pivot_id_vars,"`"),collapse="+")," ~ INDICATOR"))

     pivotcat <- dcast.data.table(pivotcat,
                                  pivot_cast_formula,
                                  value.var="VALUE")
     
     pivot_id_vars <- intersect(pivot_id_vars,
                                names(pivotcat))
     
     dashboard_data <- pivotcat[dashboard_data,
                               on=pivot_id_vars]
   }
   
   if (any(names(dashboard_data)=="PIVOTCAT",na.rm=T)) dashboard_data[,PIVOTCAT:=NULL]
   
   
   #PIVOT on CAST_VAR
   {
     if (length(omitcols) > 0) dashboard_data[,(omitcols):=NULL]  
     id_vars <- names(dashboard_data)[-which(names(dashboard_data) %in% c(cast_var,"VALUE"))]
     
     cast_formula <- as.formula(paste0(paste(paste0("`",id_vars,"`"),collapse="+")," ~ ",cast_var))
     
     dashboard_data <- dcast.data.table(dashboard_data,
                                        cast_formula,
                                        value.var="VALUE")
   }
   
 }
 
 for (id_col in id_vars) {
   
   if (id_col %in% c("INDICATOR","PARAMETER")) {
     next; #don't set the parameter itself to NA!
     
   } else if (id_col %in% SERVER_DASHBOARD_SELECTED_INDICATORS()$indicator_name) {
     next; #don't set a pivot data column to NA!  
     
   } else {
     dashboard_data[PARAMETER %in% c("id","updated","flags","checks"),
                    (id_col):=NA]
     
   }
 }

 if (any(eufilter %in% c("ECOLS","EROWS","UCOLS","UROWS"))) {
   
   not.updated <- function(x) { 
     x <- as.logical(x)
     is.na(x) | x==FALSE
   }
   
   #Empty columns
   if (any(eufilter=="ECOLS")) {
     value_cols <- names(dashboard_data)[!names(dashboard_data) %in% id_vars]
     col_data <- dashboard_data[PARAMETER=="value",
                                ..value_cols]
     
     ecols <- sapply(value_cols,
                     function(col) {
                       all(is.na(col_data[[col]]))
                     })
     
     ecols <- names(ecols)[ecols]
     if (length(ecols) > 0) dashboard_data[,
                                           (ecols):=NULL]
   }
   
   #unchanged columns
   if (any(eufilter=="UCOLS")) {
     value_cols <- names(dashboard_data)[!names(dashboard_data) %in% id_vars]
     col_data <- dashboard_data[PARAMETER=="updated",
                                ..value_cols]
     
     
     
     ucols <- sapply(value_cols,
                     function(x) { all(not.updated(col_data[[x]])) })
     
     ucols <- names(ucols)[ucols]
     if (length(ucols) > 0) dashboard_data[,
                                           (ucols):=NULL]
   }
   
   if (any(eufilter %in% c("EROWS","UROWS"))) {
     dashboard_data[,REPORTING_row:=.GRP,
                    by=c(id_vars[!id_vars %in% "PARAMETER"])]
     
     #Empty rows
     if (any(eufilter=="EROWS")) {
       
       value_cols <- names(dashboard_data)[!names(dashboard_data) %in% id_vars] #will include REPORTING_row
       col_data <- dashboard_data[PARAMETER=="value",
                                  ..value_cols]
       
       omit <- col_data[,
                        .(filter=all(is.na(.SD))),
                        by=.(REPORTING_row)
                        ][filter==TRUE,REPORTING_row]
       
       if (length(omit) > 0) dashboard_data <- dashboard_data[!(REPORTING_row %in% omit)]
     }
     
     if (any(eufilter=="UROWS")) {
       value_cols <- names(dashboard_data)[!names(dashboard_data) %in% id_vars] #will include REPORTING_row
       
       col_data <- dashboard_data[PARAMETER=="updated",
                                  ..value_cols]
       
       value_cols <- value_cols[-which(value_cols=="REPORTING_row")]
       
       col_data[,
                (value_cols):=lapply(mget(value_cols),not.updated)]
       
       # omit <- col_data[,
       #                  .(filter=all(.SD==TRUE)),
       #                  by=.(REPORTING_row)
       #                  ][filter==TRUE,REPORTING_row]
       
       omit <- col_data$REPORTING_row[(rowSums(col_data)-col_data$REPORTING_row)==(ncol(col_data)-1)]
       if (length(omit) > 0) dashboard_data <- dashboard_data[!(REPORTING_row %in% omit)]
     }
     
     dashboard_data[,
                    REPORTING_row:=NULL]
   }   
 }

 #SORT Columns/Rows by SELECTED INDICATOR order
 #indicator name is a ROW
 

 #Pivot format may affect if indicator names are in columns or rows or both
 #because @LCU etc will distort the names
 sorted_indicators <- lapply(paste0("^",selected_indicators$indicator_name),
                             grep,
                             x=names(dashboard_data),
                             value=T)
 
 sorted_indicators <- unique(unlist(sorted_indicators))
 
 sorted_indicators <- c(names(dashboard_data)[!(names(dashboard_data) %in% sorted_indicators)],
                        sorted_indicators)
 setcolorder(dashboard_data,
             neworder = sorted_indicators)
 
 
 if (any(names(dashboard_data)=="INDICATOR",na.rm = T)) {
   
   sorted_indicators <- lapply(paste0("^",selected_indicators$indicator_name),
                               grep,
                               x=unique(dashboard_data$INDICATOR),
                               value=T)
   
   sorted_indicators <- unique(unlist(sorted_indicators))
   sorted_indicators <- data.table(INDICATOR=sorted_indicators)
   sorted_indicators[,
                     sort:=1:.N]
   
   dashboard_data[sorted_indicators,
                  sort:=i.sort,
                  on=.(INDICATOR)]
   
   setorder(dashboard_data,
            sort,
            PARAMETER)
   
   dashboard_data[,sort:=NULL]
 }
 
 return (dashboard_data)
})


observeEvent(LOGGEDIN(), {
  
  SERVER_DASHBOARD_RUN_OPTIONS_RESET()
  
},once=TRUE,ignoreInit = FALSE)

observeEvent(input$server_dashboard__reporting_client, {
  
  do_refresh <- FALSE
  if (!setequal(as.numeric(input$server_dashboard__reporting_client),
                SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids)) {
    
    if (!isTruthy(input$server_dashboard__reporting_client)) SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids <- numeric(0)
    else SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids <- as.numeric(input$server_dashboard__reporting_client)
    
    do_refresh <- TRUE
    
  }

  if (!empty(SERVER_DASHBOARD_VALID_ASOF_DATES()) &&
      length(SERVER_DASHBOARD_RUN_ASOF_DATES())==0) {
    SERVER_DASHBOARD_RUN_OPTIONS$asof_dates <- 1
    do_refresh <- TRUE
  }  
  
  if (do_refresh) SERVER_DASHBOARD_REFRESH(SERVER_DASHBOARD_REFRESH()+1)
  
},ignoreNULL = FALSE)

observeEvent(input$server_dashboard__reporting_column_priority, {
  
  selected_ids <- as.numeric(input$server_dashboard__reporting_column_priority)
  
  if (!isTruthy(selected_ids) || empty(SERVER_DASHBOARD_INDICATORS())) {
    SERVER_DASHBOARD_RUN_OPTIONS$indicator_names <- character(0)
  } else {
    
    selected_indicators <- SERVER_DASHBOARD_INDICATORS()[indicator_id %in% selected_ids,
                                                         .(indicator_id,
                                                           indicator_name)]
    selected_indicators <- selected_indicators[match(selected_ids,indicator_id)]
    
    if (!identical(SERVER_DASHBOARD_RUN_OPTIONS$indicator_names,selected_indicators$indicator_name)) {
      #browser()
      SERVER_DASHBOARD_RUN_OPTIONS$indicator_names <- selected_indicators$indicator_name
      #si <- SERVER_DASHBOARD_SELECTED_INDICATORS()
    }
  }
},ignoreNULL = FALSE)

#Exclusively for :expand:special to replace special indicators with actual 
observeEvent(input$server_dashboard__reporting_column_priority, {
  
  req(input$server_dashboard__reporting_column_priority)
  req(SELECTED_PROGRAM_ID())
  
  special_selections <- SERVER_DASHBOARD_SPECIAL_INDICATORS[abs(as.numeric(input$server_dashboard__reporting_column_priority))]
  
  if (any(":expand:indicators" %in% special_selections,na.rm=T)) {
    
    query_cols <- sapply(RSF_INDICATORS()$indicator_name,
                         grep,
                         x=names(SERVER_DASHBOARD_CURRENT_QUERY()),
                         value=F,
                         USE.NAMES = T)
    
    query_cols <- unique(names(query_cols[sapply(query_cols,length)>0]))
    if (length(query_cols) > 0) {
      query_ids <- RSF_INDICATORS()[indicator_name %in% query_cols,indicator_id]
      updateSelectizeInput(session=session,
                           inputId="server_dashboard__reporting_column_priority",
                           selected=query_ids)
    }
  }
},priority=100)  

observeEvent(input$server_dashboard__reporting_column_priority_lookup, {
  
  m <- modalDialog(id="server_dashboard__reporting_column_priority_lookup_modal",
                   div(style="background-color:white;padding:5px;height:325px;",
                     fluidRow(
                       column(12,
                              textInput(inputId="server_dashboard__reporting_column_priority_lookup_search",
                                        label="Keyword Search",
                                        placeholder="Enter key words to lookup and add indicators"))),
                     fluidRow(
                       column(12,
                              style="height:100%;",
                              DT::dataTableOutput(outputId="server_dashboard__reporting_column_priority_lookup_results",
                                                  width="100%")))),
                   
                   title=HTML("Dashboard Indicator Keyword Search"),
                   easyClose = TRUE,
                   footer=modalButton("Close"),
                   size="s")
  showModal(m)
  
})

observeEvent(input$server_dashboard__reporting_asof_date, {
  
  if (!isTruthy(input$server_dashboard__reporting_asof_date)) SERVER_DASHBOARD_RUN_OPTIONS$asof_dates <- character(0)
  else if (!setequal(as.numeric(SERVER_DASHBOARD_RUN_OPTIONS$asof_dates),suppressWarnings(as.numeric(input$server_dashboard__reporting_asof_date)))) {
    SERVER_DASHBOARD_RUN_OPTIONS$asof_dates <- suppressWarnings(as.numeric(input$server_dashboard__reporting_asof_date))
    SERVER_DASHBOARD_REFRESH(SERVER_DASHBOARD_REFRESH()+1)
  }
},ignoreNULL = FALSE)

observeEvent(input$server_dashboard__view_currency, {
  
  
  if (setequal(input$server_dashboard__view_currency,SERVER_DASHBOARD_RUN_OPTIONS$fx_currency)) return (NULL)
  
  if (!isTruthy(SERVER_DASHBOARD_RUN_OPTIONS$fx_currency)) SERVER_DASHBOARD_RUN_OPTIONS$fx_currency <- "LCU"
  else SERVER_DASHBOARD_RUN_OPTIONS$fx_currency <- as.character(input$server_dashboard__view_currency)
  
  SERVER_DASHBOARD_REFRESH(SERVER_DASHBOARD_REFRESH()+1)
  
},ignoreNULL = FALSE)

observeEvent(input$server_dashboard__format_pivot, {
  
  if (setequal(input$server_dashboard__format_pivot,SERVER_DASHBOARD_RUN_OPTIONS$format_pivot)) return (NULL)
  
  if (!isTruthy(SERVER_DASHBOARD_RUN_OPTIONS$format_pivot)) SERVER_DASHBOARD_RUN_OPTIONS$format_pivot <- "DATA"
  else SERVER_DASHBOARD_RUN_OPTIONS$format_pivot <- intersect(toupper(as.character(input$server_dashboard__format_pivot)),
                                                              c("DATA","NAME","DATE"))
},ignoreNULL = FALSE)

observeEvent(input$server_dashboard__flags_filter, {
  
  if (setequal(input$server_dashboard__flags_filter,SERVER_DASHBOARD_RUN_OPTIONS$flags_filter)) return (NULL)
  
  if (!isTruthy(input$server_dashboard__flags_filter)) SERVER_DASHBOARD_RUN_OPTIONS$flags_filter <- ""
  else SERVER_DASHBOARD_RUN_OPTIONS$flags_filter <- intersect(as.character(input$server_dashboard__flags_filter),
                                                              c("any","critical","error","warning","info"))
  
  #SERVER_DASHBOARD_REFRESH(SERVER_DASHBOARD_REFRESH()+1)
  
},ignoreNULL = FALSE)

observeEvent(input$server_dashboard__reporting_filter, {
  
  rfilter <- as.logical(input$server_dashboard__reporting_filter)
  if (is.na(rfilter)) {
    SERVER_DASHBOARD_RUN_OPTIONS$reporting_filter <- ""
  } else if (rfilter==TRUE) {
    SERVER_DASHBOARD_RUN_OPTIONS$reporting_filter <- TRUE
  } else {
    SERVER_DASHBOARD_RUN_OPTIONS$reporting_filter <- FALSE
  }
})

observeEvent(SERVER_DASHBOARD_SELECTED_INDICATORS(), {
  
  SERVER_DASHBOARD_REFRESH(SERVER_DASHBOARD_REFRESH()+1) 
})

observeEvent(LOGGEDIN(), {
  
  if (!LOGGEDIN()) return (NULL)
  
  currency_choices <- GLOBAL_CURRENCIES()
  
  if (!is.null(currency_choices)) {
    currency_choices <- unique(c("USD","EUR",currency_choices))
    currency_choices <- c(LCU="LCU",setNames(currency_choices,currency_choices))
    
    updateSelectizeInput(session=session,
                         inputId="server_dashboard__view_currency",
                         choices=currency_choices,
                         selected="LCU")
  } else {
    showNotification(type="error",
                     h2("An error has occured: Unable to connect to IFC currency database. Jason functionality may be limited or result in errors"))
  }
},ignoreNULL = FALSE,ignoreInit = FALSE)

observeEvent(c(SERVER_DASHBOARD_INDICATORS(),
               SERVER_DASHBOARD_RUN_OPTIONS$indicator_names), {

  dash_indicators <- SERVER_DASHBOARD_INDICATORS()
  if (empty(SERVER_DASHBOARD_INDICATORS())) return (NULL)

  selected_indicator_ids <- ""
  if (isTruthy(SERVER_DASHBOARD_RUN_OPTIONS$indicator_names)) {
    selected_indicator_ids <- dash_indicators[indicator_name %in% SERVER_DASHBOARD_RUN_OPTIONS$indicator_names,
                                              indicator_id]  
  }
  
  
  
  if (!isTruthy(SERVER_DASHBOARD_CHOICE_INDICATORS()) ||
      !all(SERVER_DASHBOARD_RUN_OPTIONS$indicator_names %in% SERVER_DASHBOARD_CHOICE_INDICATORS(),na.rm=T) ||
      (isTruthy(selected_indicator_ids) &&
       !setequal(as.numeric(input$server_dashboard__reporting_column_priority),selected_indicator_ids))) {

    #browser()  
    
    SERVER_DASHBOARD_CHOICE_INDICATORS(dash_indicators$indicator_name)
    updateSelectizeInput(session=session,
                         inputId="server_dashboard__reporting_column_priority",
                         choices=c("",
                                   setNames(dash_indicators$indicator_id,
                                            dash_indicators$indicator_name)),
                         selected=selected_indicator_ids)
    

  }  
},
ignoreNULL = FALSE,
ignoreInit = FALSE,
priority = 9) # #need this observer to fire AFTER observeEvent(SERVER_DASHBOARD_REPORT_SELECTED() which sets SERVER_DASHBOARD_RUN_OPTIONS$indicator_names

observeEvent(SERVER_DASHBOARD_CLIENTS_LIST(), {
  
  clients <- SERVER_DASHBOARD_CLIENTS_LIST()
  
  if (is.null(clients)) {
    updatePickerInput(session=session,
                      inputId="server_dashboard__reporting_client",
                      selected="",
                      choices="")
    
  } else if (empty(clients)) {
    program <- SELECTED_PROGRAM()  
    updatePickerInput(session=session,
                      inputId="server_dashboard__reporting_client",
                      selected=program$rsf_pfcbl_id,
                      choices=setNames(program$rsf_pfcbl_id,
                                       program$program_name))
    
  } else {
    
    clients.choices <- setNames(clients$rsf_pfcbl_id,
                                clients$client_name)
    
    clients.selected <- intersect(clients$rsf_pfcbl_id,
                                  as.numeric(SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids))
  
    updatePickerInput(session=session,
                      inputId="server_dashboard__reporting_client",
                      selected=clients.selected,
                      choices=clients.choices)
    
  }
},ignoreInit = FALSE,ignoreNULL=FALSE)

#Sets reporting dates based on client
observeEvent(SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids, {
  
  selected_clients <- as.numeric(SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids)
  if (!isTruthy(selected_clients)) {
    updatePickerInput(session=session,
                      inputId="server_dashboard__reporting_asof_date",
                      selected="",
                      choices="")
  } else {
    
    valid_dates <- SERVER_DASHBOARD_VALID_ASOF_DATES()
    timeline.choices <- setNames(as.numeric(valid_dates$date_rank),
                                 valid_dates$text_date)
     
    selected_dates <- as.numeric(SERVER_DASHBOARD_RUN_OPTIONS$asof_dates)
    selected_dates <- c(selected_dates[selected_dates >= 0],
                        as.numeric(selected_dates[selected_dates <  0]+1+max(valid_dates$date_rank)))
    
    timeline.selected <- intersect(timeline.choices,
                                   selected_dates)
    
    if (any(is.infinite(suppressWarnings(as.numeric(SERVER_DASHBOARD_RUN_OPTIONS$asof_dates))),na.rm=T)) {
      timeline.selected <- timeline.choices
    }
    
    updatePickerInput(session=session,
                      inputId="server_dashboard__reporting_asof_date",
                      selected=timeline.selected,
                      choices=timeline.choices)
  }
})

#Based on query, sets filter for NAMES
observeEvent(SERVER_DASHBOARD_CURRENT_QUERY(), {
  rsf_data <- SERVER_DASHBOARD_CURRENT_QUERY()
  
  if (identical(SERVER_DASHBOARD_RUN_OPTIONS$format_raw,TRUE)) return(NULL)
  
  if (empty(rsf_data)) {
    updatePickerInput(session=session,
                      inputId="server_dashboard__name_filter",
                      choices="",
                      selected="")
  } else {
    rsf_names <- NULL
    if (any("RSFNAME" %in% names(rsf_data))) rsf_names <- unique(rsf_data[,.(RSFNAME,SYSID)])[order(RSFNAME)]
    else rsf_names <- unique(rsf_data[,.(RSFNAME=SYSNAME,SYSID)])[order(RSFNAME)]
    
    selected_names <- ""
    if (all(SERVER_DASHBOARD_RUN_OPTIONS$name_filter %in% rsf_names$SYSID)) selected_names <- SERVER_DASHBOARD_RUN_OPTIONS$name_filter
    
    updatePickerInput(session=session,
                      inputId="server_dashboard__name_filter",
                      choices=setNames(rsf_names$SYSID,
                                       rsf_names$RSFNAME),
                      selected=selected_names)
  }
})

observeEvent(input$server_dashboard__browser_cell_clicked, {
  
  clicked_cell <- input$server_dashboard__browser_cell_clicked
  
  if (!isTruthy(clicked_cell) || length(clicked_cell) == 0) return (NULL)
  
  clicked_data_id <- SERVER_DASHBOARD_RENDER_DISPLAY(view="id")[clicked_cell$row,clicked_cell$col+1,with=F]
  
  if (empty(clicked_data_id)) return (NULL)
  
  #Has column name: filter-out system cols.
  # if (any(grepl("^[A-Z]",names(clicked_data_id)))) {
  #   clicked_data_id <- NA
  # }
  
  cell_data_id <- as.numeric(unlist(clicked_data_id))
  
  if (isTruthy(cell_data_id)) {
    
    cell_info <- DBPOOL %>% dbGetQuery("
      select 
        rc.reporting_cohort_id,
        rc.reporting_asof_date,
        rc.is_calculated_cohort,
        rc.source_name,
        coalesce(rd.data_value,'{BLANK}') || coalesce((' ' || rd.data_unit),'') as data_value
      from p_rsf.rsf_data rd
      inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
      where data_id = $1::int",
      params=list(cell_data_id))
    
    info <- paste(cell_info$data_value,
                  ifelse(cell_info$is_calculated_cohort==TRUE,"calculated by system","reported"),
                  " for ",
                  format_asof_date_label(cell_info$reporting_asof_date),
                  " in ",
                  cell_info$source_name)
    SERVER_DASHBOARD_CELL_INFO(info)
  } else {
    SERVER_DASHBOARD_CELL_INFO("")
  }
  
}, ignoreInit = TRUE)

observeEvent(input$server_dashboard__reporting_column_priority_lookup_add, {
  add_id <- as.numeric(input$server_dashboard__reporting_column_priority_lookup_add)
  query_ids <- NULL
  
  if (add_id %in% input$server_dashboard__reporting_column_priority) {
    query_ids <- input$server_dashboard__reporting_column_priority[-which(input$server_dashboard__reporting_column_priority==add_id)]
  } else {
    query_ids <- c(input$server_dashboard__reporting_column_priority,add_id)
  }
  
  updateSelectizeInput(session=session,
                       inputId="server_dashboard__reporting_column_priority",
                       selected=query_ids)
})

SERVER_DASHBOARD_RENDER_DISPLAY <- function(display_data=SERVER_DASHBOARD_DATA_DISPLAY(),
                                            view="html") {
  
  if (empty(SERVER_DASHBOARD_DATA_DISPLAY())) return (NULL)
  if (!view %in% c("html","text","value","id")) stop("View must be: html, text, value, id")
  dashboard_data <- display_data[PARAMETER==view]
  dashboard_data[,PARAMETER:=NULL]
  return(dashboard_data)
}

output$server_dashboard__panel_cell_info <- renderUI({
  info <- SERVER_DASHBOARD_CELL_INFO()
  if (!isTruthy(info)) info <- ""
  HTML(info)
})

output$server_dashboard__browser <- DT::renderDataTable({


  emode <- SERVER_DASHBOARD_EDIT_STATUS()
  display_data <- SERVER_DASHBOARD_DATA_DISPLAY()

  syscols <- isolate({ SERVER_DASHBOARD_RUN_OPTIONS$syscols })
  # display_indicator_names <- NULL
  editable <- NULL
  #disabledRows <- NULL
  
  if (empty(display_data)) {

    emptydf <- data.frame(Error="There is no data to display: Please check Reporting Date, Report Filter, Client Selections; and Column Selection filter.")
    run <- isolate({ SERVER_DASHBOARD_RUN_AUTORUN() })
    
    if (!isTruthy(run)) {
      emptydf <- data.frame(Error=paste0(as.character(now()),": ",
                                         "Dashboard set NOT to auto-run.  Finish specifying the report variables and switch the 'run' button to on"))
    }
    
    empty_data <- DT::datatable(emptydf,
                                rownames=FALSE,
                                fillContainer = TRUE,
                                options=list(
                                  dom="t",
                                  autoWidth=TRUE,
                                  paging=FALSE))
    return (empty_data)
  }
  
  dashboard_data <- SERVER_DASHBOARD_RENDER_DISPLAY(view=fcase(emode==TRUE,"text",
                                                               emode==FALSE,"html"),
                                                    display_data=SERVER_DASHBOARD_DATA_DISPLAY())
  # data_view <- NULL
  # if (emode==TRUE) data_view <- "edit"
  # else data_view <- DASH_DATA_OPTIONS$display_timeline_format #REACTIVE
  # 
  # 
  # display_data <- DASH_DATA_RENDER_DISPLAY(display_cell=data_view)


 
  
#browser()
  if (emode == TRUE) {
    
    reportingCols <- as.numeric(which(is.na(sapply(names(dashboard_data),
                                                   SERVER_DASHBOARD_EDIT_COLUMN_IS_EDITABLE)))-1)
    
    #reportingCols <- grep("^reporting_|^indicator_name|^[A-Z]",names(display_data))-1

    #See tp disable rows:
    #https://stackoverflow.com/questions/63808485/shiny-datatable-mode-editable-restrict-specific-columns-and-rows
    # if (any(display_data$reporting_history==TRUE)) {
    #   editable <- list(target = 'cell', disable = list(columns = reportingCols),
    #                    rows = which(display_data$reporting_history==FALSE)) #c(0,1)))
    #
    # } else {
    editable <- list(target = 'cell', disable = list(columns = reportingCols)) #c(0,1)))

    #}
    #editable <- list(target = 'cell')
    # if (any(display_data$reporting_history==TRUE) && any(display_data$reporting_history==FALSE)) {
    #   reporting_history_row <- which(names(display_data)=="reporting_history")-1
    #   disabledRows <- JS(paste("function(row, data, displayNum, displayIndex, dataIndex) {
    # 
    #                                       if (data[",reporting_history_row,"].toString().toLowerCase() =='false') {
    #                                         $(row).addClass('disabledEvents');
    #                                       }
    #                                      }"))
    # }
  }

  row_colors <- c(paste0("rgba(",DASH_DISPLAY_BLUE_C,")"),    #1 1
                  paste0("rgba(",DASH_DISPLAY_BLUE_C,")"),    #3 2
                  paste0("rgba(",DASH_DISPLAY_YELLOW_C,")"),  #2 1
                  paste0("rgba(",DASH_DISPLAY_YELLOW_C,")"))  #4 2

  #means it has history, so add history colors
  # if (any(display_data$reporting_history==TRUE)) {
  #   row_colors <- c(paste0("rgba(",DASH_DISPLAY_BLUE_CH,")"),   #1 1
  #                   paste0("rgba(",DASH_DISPLAY_BLUE_HH,")"),   #3 3
  #                   paste0("rgba(",DASH_DISPLAY_YELLOW_CH,")"), #2 2
  #                   paste0("rgba(",DASH_DISPLAY_YELLOW_HH,")")) #4 4
  # }

  syscols.hide <- names(SERVER_DASHBOARD_OPTIONS_SYSCOLS[!(SERVER_DASHBOARD_OPTIONS_SYSCOLS %in% syscols)])
  if (any(syscols.hide=="SYSID_currentest_date")) {
    SYSID_currentest_date_col <- grep("^[A-Z]+_currentest_date",names(dashboard_data),value=T)
    if (length(SYSID_currentest_date_col)==1) syscols.hide <- c(syscols.hide,SYSID_currentest_date_col)
  }

  hidden_cols <- which(names(dashboard_data) %in% c("REPORTING_group_rank",syscols.hide))-1 #0-based index.
  
  dd <- DT::datatable(dashboard_data,
                      editable = editable,
                      rownames = FALSE,
                      fillContainer=TRUE,
                      selection = fcase(emode==TRUE,'none',
                                        default='single'),
                      class = fcase(emode==TRUE,'cell-border stripe',
                                    default="display"),
                      #extensions = c("KeyTable"),
                      escape = FALSE, #Shouldn't be any HTML escapable text
                      #callback = JS(js),

                      #class = fcase(emode==TRUE,'cell-border stripe',default=NULL),
                      options=list(dom="tirp",
                                   scrollX="auto",
                                   scrollY="70vh",
                                   bSort=F,
                                   paging=TRUE,
                                   ordering=F,
                                   rowCallback = NULL,
                                   pageLength=100,
                                   #colReorder=TRUE,
                                   columnDefs = list(list(visible=FALSE, targets=hidden_cols),
                                                     list(className = 'dt-left', targets = "_all")))
  )

  # if (any(display_data$reporting_history==TRUE) && any(display_data$reporting_history==FALSE)) {
  #   dd <- formatStyle(dd,
  #                     "reporting_history",
  #                     target="row",
  #                     fontWeight = styleRow(rows=which(display_data$reporting_history==FALSE), #why any(FALSE) as browser will crash with numeric(0)
  #                                           values=c("bold")))
  # 
  #   #https://stackoverflow.com/questions/14750078/style-disabled-button-with-css
  #   if (emode==TRUE) {
  #     # dd <- formatStyle(dd,
  #     #                   columns=1:ncol(display_data)-1,
  #     #                   target="cell",
  #     #                   cursor = styleRow(rows=which(display_data$reporting_history==FALSE), #why any(FALSE) as browser will crash with numeric(0)
  #     #                                     values=c("not-allowed")),
  #     #                   `pointer-events` = styleRow(rows=which(display_data$reporting_history==FALSE), #why any(FALSE) as browser will crash with numeric(0)
  #     #                                               values=c("none"))
  #     #                   )
  #     # dd <- formatStyle(dd,
  #     #                   "reporting_history",
  #     #                   target="row",
  #     #                   cursor = styleRow(rows=which(display_data$reporting_history==FALSE), #why any(FALSE) as browser will crash with numeric(0)
  #     #                                     values=c("not-allowed")))
  #   }
  # }

  
  
  dd <- formatStyle(dd,
                    columns=1:ncol(dashboard_data)-1,
                    target="cell",
                    `max-width` = "250px",
                    `white-space`="nowrap",
                    `text-overflow`="ellipsis",
                    `overflow`="hidden")
    
  if (any(names(display_data)=="REPORTING_asof_date")) {
    dd <- formatStyle(dd,
                      columns=c("REPORTING_asof_date"),
                target="cell",
                `max-width` = "150px",
                `white-space`="nowrap",
                `text-overflow`="ellipsis",
                `overflow`="hidden")
    
  }
  
  if (any(names(display_data)=="REPORTING_group_rank")) {
    dd <- formatStyle(dd,
                      columns="REPORTING_group_rank",
                      target = "row",
                      backgroundColor = styleEqual(c(1, 3,
                                                     2, 4),
                                                   row_colors))
  }
  
  if (any(names(display_data)=="SYSNAME")) {
    dd <- formatStyle(dd,
                      columns="SYSNAME",
                target="cell",
                `white-space`="nowrap",
                `overflow`="hidden",
                `text-overflow`="ellipsis",
                #`width` = "250px",
                `max-width` = "350px",
                `direction` = "rtl",
                `text-align` = "left")

  }


  return (dd)
})

SERVER_DASHBOARD_LOOKUP_RESULTS <- eventReactive(c(SERVER_DASHBOARD_INDICATORS(),
                                                   input$server_dashboard__reporting_column_priority_lookup_search), {
                                                     
  keywords <- input$server_dashboard__reporting_column_priority_lookup_search      
  
  
  ind <- RSF_INDICATORS()[,
                          .(indicator_id,
                            indicator_name,
                            definition,
                            labels)]
  
  ind <- rbindlist(list(ind[,unlist(labels,recursive=F),by=.(indicator_id)][label_key != "SYS",.(indicator_id,keywords=label_normalized)],
                        ind[,.(indicator_id,keywords=indicator_name)],
                        ind[,.(indicator_id,keywords=definition)],
                        SERVER_DASHBOARD_INDICATORS()[indicator_id <=0,.(indicator_id,keywords=indicator_name)]))
  
  keywords <- trimws(unlist(strsplit(keywords,split="[^[:alnum:]]")))
  
  matches <- NA
  if (isTruthy(keywords) && any(nchar(keywords) > 0)) {
    
    matches <- sapply(keywords,
                      function(x,indicators) {
                        indicators[grepl(pattern=x,
                                         keywords,
                                         ignore.case = T),
                                   indicator_id]
                      },
                      indicators=ind)
    
    if (length(keywords) > 1) {
      matches <- Reduce(intersect,matches)
    }
    
  
  } else {
    matches <- unique(ind$indicator_id)
  }
  
  ind <- SERVER_DASHBOARD_INDICATORS()[indicator_id %in% matches,
                                       .(indicator_id,
                                         indicator_name)]
  
  setorder(ind,
           indicator_name)
  
  ind[,
      Add:=indicator_id %in% input$server_dashboard__reporting_column_priority]
  
  ind[,
      Add:=paste0("<input type='checkbox' value=",indicator_id," onmousedown='event.stopPropagation();' ",
                  ifelse(Add==TRUE, #"NEW" flags are those who've never had a comment will be auto-selected, else not.
                         "checked=true ",
                         ""),
                  "onclick='Shiny.setInputValue(\"server_dashboard__reporting_column_priority_lookup_add\",",indicator_id,",{priority:\"event\"})' />")]
  ind[,
      `:=`(indicator_id=NULL)]
  
  return (ind[,.(INDICATOR=indicator_name,Add)])
  
})

output$server_dashboard__reporting_column_priority_lookup_results <- DT::renderDataTable({
  
  ind <- SERVER_DASHBOARD_LOOKUP_RESULTS()
  req(ind)
  

  
  dom <- "t"
  if (nrow(ind) > 8) dom <- "tp"
  dd <- DT::datatable(ind,
                      rownames = FALSE,
                      fillContainer=TRUE,
                      class = 'cell-border stripe',
                      colnames=c("INDICATOR","Add"),
                      height = "100%",
                      escape = FALSE,
                      options=list(
                        dom=dom,
                        ordering=FALSE,  
                        paging=TRUE,
                        pageLength=8))
 return (dd)
})

output$action_server_dashboard__download <- downloadHandler(
  filename=function() {
    SERVER_DASHBOARD_DOWNLOAD_FILENAME()
  },
  content=function(file) {


    #dd_dt <- DASH_DATA_DISPLAY()
    export_data <- SERVER_DASHBOARD_RENDER_DISPLAY(view="value",
                                                   display_data=SERVER_DASHBOARD_DATA_DISPLAY())
    if (!isTruthy(export_data)) return (NULL)


    report_note <- c()
    asof_dates <- SERVER_DASHBOARD_RUN_ASOF_DATES()
    opts <- reactiveValuesToList(SERVER_DASHBOARD_RUN_OPTIONS)
    opts$rsf_pfcbl_ids <- NULL
    opts$indicator_ids <- NULL
    opts$indicator_names <- NULL
    opts$asof_dates <- asof_dates
    opts$syscols <- NULL
    opts[grepl("format",names(opts))] <- NULL
     
    report_note <- paste0(sapply(names(opts),function(x) { paste0(x,": ",paste0(as.character(opts[[x]]),collapse=", ")) }),collapse=" & ")

    export_data_flags <- NULL

    savedwb <- withProgress(message=paste0("Generating Excel report.  This may take a few moments...."),value=0.3, {

      excelwb <- DBPOOL %>% export_dashboard_view_to_excel(export_asof_date=max(asof_dates),
                                                           export_data=export_data,
                                                           export_data_flags=export_data_flags,
                                                           exporting_user_id=USER_ID(),
                                                           report_id=NA,
                                                           report_note=report_note)

      savedwb <- openxlsx::saveWorkbook(excelwb,
                                        file,
                                        overwrite=TRUE)
      
      incProgress(amount=1.0,message="Completed")
      savedwb
    })

    savedwb
  },
  contentType="application/xlsx")
