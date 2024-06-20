

DASH_DATA_GET_CLICK <- function(clicked_cell,
                                display_cell=c("html",
                                               "text",
                                               "edit",
                                               "current"),
                                display_for=c("dashboard",
                                              "download"),
                                display_data=DASH_DATA_DISPLAY(),
                                column_data=DASH_DATA_COLUMNS(),
                                rsf_data=DASH_DATA()) {
  
  if (!isTruthy(clicked_cell) || length(clicked_cell) == 0) return (NULL)
  
  rendered_cols <- names(DASH_DATA_RENDER_DISPLAY(display_cell))
  
  clicked_col <- rendered_cols[(clicked_cell$col)+1]
  if (grepl("^reporting_",clicked_col)==TRUE) return(NULL)
  if (clicked_col=="indicator_name") return(NULL)
  
  reporting_cols <- sort(grep("^reporting_",rendered_cols,value=T))
  
  options_FORMAT <- attr(display_data,"options_FORMAT")
  clicked_data <- NULL
  
  if (options_FORMAT=="INDICATORS") {
    
    clicked_col <- indicatorNames_getBasenames(clicked_col)
    id_col <- paste0(clicked_col,".dataid")
    
    reporting_cols <- c(reporting_cols,id_col)
    
    clicked_data <- display_data[clicked_cell$row,..reporting_cols]
    clicked_data[,`:=`(indicator_name=clicked_col,
                       cell_value=as.character(clicked_cell$value))]
    
    setnames(clicked_data,
             old=id_col,
             new="data_id")
    
  } else if (options_FORMAT=="NAMES") {
    
    edit_NAME <- gsub("\\.[a-z]+$","",clicked_col)
    
    id_col <- paste0(edit_NAME,".dataid")
    
    sysid <- unique(column_data[reporting_NAME==edit_NAME,reporting_SYSID])
    
    reporting_cols <- c(reporting_cols,"indicator_name",id_col)
    
    clicked_data <- display_data[clicked_cell$row,..reporting_cols]
    
    setnames(clicked_data,
             old=id_col,
             new="data_id")
    
    clicked_data[,reporting_SYSID:=sysid]
    clicked_data[,cell_value:=as.character(clicked_cell$value)]
    
  } else if (options_FORMAT=="LONG") {
    
    reporting_cols <- c(reporting_cols,
                        "indicator_name",
                        "dataid")
    
    clicked_data <- display_data[clicked_cell$row,..reporting_cols]
    clicked_data[,cell_value:=as.character(clicked_cell$value)]
    
    setnames(clicked_data,
             old="dataid",
             new="data_id")
  } else {
    stop("Unrecognized options_FORMAT")
  }
  
  clicked_data[,data_id:=as.numeric(data_id)]
  clicked_data <- clicked_data[,
                               .(reporting_SYSID,
                                 indicator_name,
                                 reporting_asof_date,
                                 data_asof_date=as.Date(NA),
                                 reporting_intraperiod_rank,
                                 data_id,
                                 cell_value)]
  
  if (!is.na(clicked_data$data_id)) {

    data_date <- rsf_data[data_id == clicked_data$data_id,unique(data_asof_date)]
    clicked_data[,data_asof_date:=data_date]
  }
  return (clicked_data)
  
}

#SETS:
#rsfdash_reporting_asof_date
#rsfdash_view_currency
#resets DASH_DATA_SELECTED_INDICATORS_WRITE
SERVER_DASHBOARD_SPECIAL_INDICATORS <- c(":exclude:reported",
                                         ":exclude:calculated",
                                         ":exclude:unsubscribed",
                                         ":include:loan",
                                         ":include:borrower",
                                         ":include:client",
                                         ":include:facility",
                                         ":include:program",
                                         ":include:IDs",
                                         ":include:NAMES",
                                         ":expand:calculations-shallow",
                                         ":expand:calculations-deep")

SERVER_DASHBOARD_REFRESH <- reactiveVal(0)
SERVER_DASHBOARD_CURRENT_QUERY <- reactiveVal(data.table())
SERVER_DASHBOARD_CURRENT_INDICATORS <- reactiveVal(data.table())
SERVER_DASHBOARD_RUN_AUTORUN <- reactiveVal(FALSE)
SERVER_DASHBOARD_RUN_OPTIONS <- reactiveValues(rsf_pfcbl_ids=numeric(0),
                                               indicator_ids=numeric(0),
                                               indicator_names=character(0),
                                               asof_dates=character(0),
                                               MODE_RAW=FALSE, #will download the native extract, no family.  No browser UI functions.
                                               format_blank="text",
                                               format_unchanged="gray",
                                               format_exceldates=TRUE,
                                               syscols = c("reporting_asof_date", 
                                                            "SYSID",              
                                                            "rsf_full_name"),
                                               display_flags = c("active"),
                                               #aux cols
                                               #REPORTING_data_date, -> option to include ONLY allowed when ONE asof_date is selected (else multiple timeseries would be confusingly reported)
                                               #REPORTING_asof_date, REPORTING_status, REPORTING_
                                               fx_currency="LCU", #FX can not be allowed if FUTURE dates are selected
                                               fx_force_global=TRUE,
                                               fx_concatenate_LCU=TRUE, #when different currency units in same col, merge number+unit
                                               fx_reported_date=FALSE,
                                               fx_audit=FALSE) #TRUE: fx@data reported date  
                                                                       #FALSE: fx$query asof date

SERVER_DASHBOARD_MODE_EDITING <- reactiveVal(FALSE)

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
  
  if (!empty(SERVER_DASHBOARD_REPORT_SELECTED())) {
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
SERVER_DASHBOARD_SELECTED_INDICATORS <- eventReactive(c(SERVER_DASHBOARD_RUN_AUTORUN(),
                                                        SERVER_DASHBOARD_RUN_OPTIONS$indicator_names), {
  
  if (empty(SERVER_DASHBOARD_INDICATORS())) return (NULL)

  selected_dashboard_indicators <- SERVER_DASHBOARD_INDICATORS()[indicator_name %in% SERVER_DASHBOARD_RUN_OPTIONS$indicator_names]
  if (empty(selected_dashboard_indicators)) return (NULL)
  
  if (!SERVER_DASHBOARD_RUN_AUTORUN()) return (NULL)
  
  if (any(selected_dashboard_indicators$indicator_id %in% 0)) {
    report_indicator_names <- unlist(SERVER_DASHBOARD_REPORT_SELECTED$for_indicator_names)
    
    report_location <- which(selected_dashboard_indicators$indicator_id==0)
    exclude_report_names <- selected_dashboard_indicators[1:report_location,indicator_name]
    #If user puts an indicator first, before the report name, filter-out the report's instance of the indicator so that the requsted indicator
    #becomes first in sort. 
    report_indicator_names <- report_indicator_names[!(report_indicator_names %in% exclude_report_names)]
    
    selected_dashboard_indicators <- rbindlist(list(selected_dashboard_indicators,
                                                    selected_dashboard_indicators[indicator_name %in% report_indicator_names]))
  }
  
  
  selected_dashboard_indicators <- selected_dashboard_indicators[grepl("^:report:",indicator_name)==FALSE]
  selected_dashboard_indicators[,n:=.N,
                                by=.(indicator_id)]
  
  selected_dashboard_indicators <- selected_dashboard_indicators[n==1]
  selected_dashboard_indicators[,n:=NULL]
  selected_dashboard_indicators[,
                                sort:=match(indicator_name,SERVER_DASHBOARD_RUN_OPTIONS$indicator_names)]
  
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
           
  print(paste0("selected_dashboard_indicators SORT ORDER: ",
        paste0(selected_dashboard_indicators$indicator_name,collapse=", ")))
  
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
      select 
        prd.valid_reporting_date::text as text_date,
        prd.reporting_sequence_rank,
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
      order by prd.valid_reporting_date desc",
      params=list(program_id,
                  paste0(selected_clients,collapse=",")))
  
  setDT(valid_dates)
  valid_dates[,date_rank:=as.numeric(NA)]
  valid_dates[is_future==FALSE,
              date_rank:=1:.N]
  
  return (valid_dates)  
},
ignoreNULL = FALSE)


SERVER_DASHBOARD_DATA_RUN <- eventReactive(SERVER_DASHBOARD_REFRESH(), {
  
  SERVER_DASHBOARD_REFRESH()  
}) %>% debounce(500)


#Applies filters: subsets
SERVER_DASHBOARD_DATA <-  eventReactive(c(SERVER_DASHBOARD_CURRENT_QUERY(),
                                          input$server_dashboard__name_filter,
                                          input$server_dashboard__flags_filter), {
  
  ffilter <- input$server_dashboard__flags_filter
  nfilter <- as.numeric(input$server_dashboard__name_filter)
  
  current_query <- SERVER_DASHBOARD_CURRENT_QUERY()
  
  if (empty(current_query)) return (NULL)
  
  if (isTruthy(nfilter)) {
    current_query <- current_query[SYSID %in% nfilter]
  }
  
  all_flags <- c("critical","warning","error","info")
  if ("any" %in% ffilter) ffilter <- unique(all_flags,
                                            ffilter)
  
  ffilter <- intersect(all_flags,ffilter)
  fcols <- grep("\\.flags$",names(current_query),value=T)
  if (length(fcols) > 0 && length(ffilter) > 0) {
    
    frows <- lapply(fcols,function(col,ffilter) {
      has_flags <- sapply(current_query[[col]],is.null)==FALSE
      if (!any(has_flags)) return (numeric(0))
      current_query[has_flags,
                    unlist(.SD[[col]],recursive=F),
                    by=.(REPORTING_row)][check_class %in% ffilter,REPORTING_row]
    },
    ffilter=ffilter)
    
    frows <- Reduce(f=union,x=frows)
    current_query <- current_query[REPORTING_row %in% frows]
  }
  cols <- names(current_query)[!(names(current_query) %in% fcols)]
  current_query[,..cols]
  
  return (current_query)
  
})

#Applies pivot: appearance, columns
SERVER_DASHBOARD_DATA_DISPLAY <- eventReactive(c(SERVER_DASHBOARD_DATA(),
                                                 input$server_dashboard__view_display), {
   
   
   #dashboard_data <- rsf_family_data
   dashboard_data <- SERVER_DASHBOARD_DATA()
   pivot <- toupper(input$server_dashboard__view_display)
   if (!pivot %in% c("DATA","NAME","DATE")) pivot <- "DATA"
   
   if (is.null(dashboard_data)) return (NULL)
   
   display_indicators <- SERVER_DASHBOARD_CURRENT_INDICATORS()
   # selected_indicator_ids <- SERVER_DASHBOARD_SELECTED_INDICATORS()
   # display_indicators <- RSF_INDICATORS()[indicator_id %in% selected_indicator_ids,
   #                                        .(indicator_id,
   #                                          indicator_name)]
   # display_indicators <- display_indicators[data.table(indicator_id=selected_indicator_ids,
   #                                                     sort=seq_along(selected_indicator_ids)),
   #                                          on=.(indicator_id)]
   # setorder(display_indicators,
   #          sort)
   
   if (pivot=="DATA") {
     
     #display_cols <- c("REPORTING_asof_date","SYSID","SYSNAME","RSFNAME")
     display_sys_cols <- grep("^[A-Z]",names(dashboard_data),value=T)
     #col_name@LCU.current changes standard pattern
     display_data_cols <- lapply(display_indicators$indicator_name,
                                 function(ind) { grep(paste0("^",ind,".*\\.[a-z]+$"),names(dashboard_data),value=T)})
     display_data_cols <- unique(unlist(display_data_cols))
     display_cols <- c(display_sys_cols,
                       display_data_cols)
     
     dashboard_data <- dashboard_data[,..display_cols]
     dashboard_data[,
                    REPORTING_group_rank:=2-(.GRP%%2),
                    by=.(SYSID)]
   } else if (pivot=="NAME") {
     
     display_cols <- c("REPORTING_asof_date","SYSNAME")
     if ("RSFNAME" %in% names(dashboard_data)) display_cols <- c("REPORTING_asof_date","RSFNAME")
     
     #value_cols <-grep("\\.value$|\\.text$|\\.html$|\\.id$",names(current_query),value=T)
     value_cols <- grep("\\.[a-z]+$",names(dashboard_data),value=T)
     
     cols <- c(display_cols,
               value_cols)
     
     dashboard_data <- dashboard_data[,..cols]
     
     for (col in value_cols) set(dashboard_data,i=NULL,j=col,as.character(dashboard_data[[col]]))
     
     
     dashboard_data <- melt.data.table(dashboard_data,
                                       id.vars = display_cols,
                                       value.name="data_value",
                                       value.factor = F,
                                       variable.name="indicator_name",
                                       variable.factor = F)
     
     dashboard_data<-dcast.data.table(dashboard_data,
                                      REPORTING_asof_date + indicator_name ~ RSFNAME,
                                      value.var="data_value")
     
     dashboard_data[,
                    c("indicator_name","parameter"):=tstrsplit(indicator_name,"\\.")]
     
     setorder(dashboard_data,
              indicator_name,
              -REPORTING_asof_date)
     
     dashboard_data[,
                    REPORTING_row:=1:.N]
     
     dashboard_data[,
                    REPORTING_group_rank:=.GRP,
                    by=.(REPORTING_asof_date,
                         indicator_name)]
     
     setcolorder(dashboard_data,
                 neworder=c("REPORTING_asof_date","indicator_name","parameter"))
     
   }
   
   return (dashboard_data)
 })




#############################################################################################
#Queries database and caches result in SERVER_DASHBOARD_CURRENT_QUERY
observeEvent(SERVER_DASHBOARD_DATA_RUN(), {
  
  selected_client_ids <- SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids                                
  selected_indicators <- SERVER_DASHBOARD_SELECTED_INDICATORS()
  selected_asof_dates <- SERVER_DASHBOARD_RUN_OPTIONS$asof_dates
  
  run <- SERVER_DASHBOARD_RUN_AUTORUN()
  
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
  
  
  if (!isTruthy(run)) {
    return (NULL)
  }
  
  
  
  include.rsf_name <- any("rsf_full_name" %in% SERVER_DASHBOARD_RUN_OPTIONS$syscols)
  include.status <- any(c("reporting_status","reporting_expected") %in% SERVER_DASHBOARD_RUN_OPTIONS$syscols)
  fx_concatenate_LCU <- SERVER_DASHBOARD_RUN_OPTIONS$fx_concatenate_LCU
  sys_cols <- SERVER_DASHBOARD_RUN_OPTIONS$syscols
  
  display_flags <- intersect(c("active","resolved"),
                             tolower(SERVER_DASHBOARD_RUN_OPTIONS$display_flags))
  
  exclude_indicators <- grep("^:exclude:",selected_indicators$indicator_name,value=T)
  
  if (length(exclude_indicators) > 0) {
    
    exclude_indicators <- DBPOOL %>% dbGetQuery("
    select 
      fis.indicator_id,
      fis.indicator_name,
      bool_or(fis.is_subscribed) as is_subscribed,  --is subscribed by any
      bool_and(fis.is_calculated) as is_calculated --is calculated by all
    from p_rsf.rsf_pfcbl_id_family fam 
    inner join p_rsf.view_rsf_program_facility_indicator_subscriptions fis on fis.rsf_pfcbl_id = fam.parent_rsf_pfcbl_id
    where fam.child_rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
      and fam.parent_pfcbl_category = 'facility'
      and fis.indicator_id = any(select unnest(string_to_array($2::text,','))::int)
    group by fis.indicator_id,fis.indicator_name",
    params=list(paste0(selected_client_ids,collapse=","),
                paste0(selected_indicators$indicator_id,collapse=",")))
    
    setDT(exclude_indicators)
    if (":exclude:reported" %in% exclude_indicators) {
      exclude_rank <- selected_indicators[indicator_name==":exclude:reported",sort]
      selected_indicators <- selected_indicators[!(indicator_id %in% exclude_indicators[is_calculated==FALSE,indicator_id]) &
                                                 sort < exclude_rank]
    }

    if (":exclude:calculated" %in% exclude_indicators) {
      exclude_rank <- selected_indicators[indicator_name==":exclude:calculated",sort]
      selected_indicators <- selected_indicators[!(indicator_id %in% exclude_indicators[is_calculated==TRUE,indicator_id]) &
                                                 sort < exclude_rank]
    }
    
    if (":exclude:unsubscribed" %in% exclude_indicators) {
      exclude_rank <- selected_indicators[indicator_name==":exclude:unsubscribed",sort]
      selected_indicators <- selected_indicators[!(indicator_id %in% exclude_indicators[is_calculated==TRUE,indicator_id]) &
                                                 sort < exclude_rank]
    }
  }
  
  expand_indicators <- grep("^:expand:",selected_indicators$indicator_name,value=T)
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
      
      expand_formulas <- expand_formulas[!indicator_id %in% selected_indicators[sort < expand_rank]]
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
      setorder(selected_indicators,
               sort,
               indicator_name)
      selected_indicators[,sort:=1:.N]
      setorder(selected_indicators,
               sort)
    }
  }
  SERVER_DASHBOARD_CURRENT_INDICATORS(selected_indicators)
  
  rsf_data <- NULL  
  withProgress(session=session,
               message="Loading data...", {
                 for (asof in selected_asof_dates) {
                   incProgress(amount=(1/length(selected_asof_dates)),
                               message=paste0("Loading ",asof," data..."))
                   
                   rd <- DBPOOL %>% db_data_get_current(rsf_pfcbl_ids.familytree=selected_client_ids,
                                                        indicator_ids=selected_indicators$indicator_id,
                                                        reporting_current_date=ymd(asof),
                                                        fx_currency=fx_currency,
                                                        include.sys_name=TRUE,
                                                        include.rsf_name=include.rsf_name,
                                                        include.status=include.status,
                                                        include.flags=display_flags,
                                                        fx_force_global=fx_force_global,
                                                        fx_reported_date=fx_reported_date,
                                                        fx_concatenate_LCU=fx_concatenate_LCU)
                   rsf_data <- rbindlist(list(rsf_data,
                                              rd))
                   
                   incProgress(amount=(1/length(selected_asof_dates)))
                 }
               })
  
  if (empty(rsf_data)) {
    return (NULL)
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
    
    if (length(display_flags) > 0) {
      
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
  
  
  if (isTruthy(SERVER_DASHBOARD_RUN_OPTIONS$MODE_RAW)) {
    
    rsf_data[,flags:=NULL] #it's a list object and won't render into Excel.
    
    SERVER_DASHBOARD_CURRENT_QUERY(rsf_data) 
    showNotification(type="message",
                     h3("RAW mode is selected: use Export to download dataset. Dashboard browser is not available in RAW mode."))
    return (NULL)
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
    
    if (any(names(rsf_data)=="DATA_currentest_date"))  {
      rsf_data[,DATA_currentest_date:=as.character(DATA_currentest_date)]
      rsf_data[,
               DATA_currentest_date:=paste0(sort(unique(DATA_currentest_date)),collapse=", "),
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
        
        rsf_data[is.na(value) ==TRUE &
                   updated==FALSE,
                 `:=`(html=fcase(format_blank=="empty","",
                                 default="<div style='color:rgba(220,220,220,0.8);'>{BLANK}</div>"),
                      
                      text=fcase(format_blank=="empty","",
                                 default="{BLANK}"))]
        
        if (format_unchanged=="gray") {
          rsf_data[is.na(value) ==FALSE &
                     updated==FALSE,
                   html:=paste0("<div style='color:rgba(180,180,180,0.8);'>",text,"</div>")]
          
        } else if (format_unchanged=="arrow") {
          rsf_data[is.na(value) ==FALSE &
                     updated==FALSE,
                   html:=paste0("<div style='color:rgba(145,15,60,0.8);font-size:1.3em;' title='",gsub("'","",text)," reported in the past.'>&uarr;</div>")]
          
        } else if (format_unchanged=="empty") {
          rsf_data[is.na(value) ==FALSE &
                     updated==FALSE,
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
  
  rsf_family_data <- db_data_pivot_family(rsf_data,
                                          value.vars=c("id",
                                                       "value",
                                                       "flags",
                                                       "html",
                                                       "text",
                                                       "checks"))
  
  setnames(rsf_family_data,
           old="current_asof_date",
           new="REPORTING_asof_date")
  
  setorder(rsf_family_data,
           SYSID,
           -REPORTING_asof_date)  
  
  rsf_family_data[,
                  REPORTING_row:=1:.N]
  
  
  if (any("reporting_qdate" %in% SERVER_DASHBOARD_OPTIONS_SYSCOLS)) {
    rsf_family_data[,
                    REPORTING_qdate:=format_asof_date_label(REPORTING_asof_date)]
  }
  
  selected_indicator_names <- selected_indicators$indicator_name
  selected_col_names <- lapply(paste0("^",selected_indicator_names),
                               grep,
                               x=names(rsf_family_data),
                               value=T)
  selected_col_names <- unlist(selected_col_names)
  selected_col_names <- c(names(rsf_family_data)[!names(rsf_family_data) %in% selected_col_names],
                          selected_col_names)
  
  rsf_family_data <- rsf_family_data[,
                                     ..selected_col_names]
  
  SERVER_DASHBOARD_CURRENT_QUERY(rsf_family_data)
}, ignoreInit=TRUE,priority=-1)
#############################################################################################

observeEvent(input$server_dashboard__autorun, {
  
  if (!identical(SERVER_DASHBOARD_RUN_AUTORUN(),
                 isTruthy(input$server_dashboard__autorun))) {
    SERVER_DASHBOARD_RUN_AUTORUN(isTruthy(input$server_dashboard__autorun))
  }    
  if (isTruthy(input$server_dashboard__autorun)) SERVER_DASHBOARD_REFRESH(SERVER_DASHBOARD_REFRESH()+1)
},ignoreInit = TRUE,ignoreNULL = FALSE)

observeEvent(input$server_dashboard__reporting_client, {
  
  if (setequal(as.numeric(input$server_dashboard__reporting_client),
               SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids)) {
    return (NULL)
    
  } else {
    if (!isTruthy(input$server_dashboard__reporting_client)) SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids <- numeric(0)
    else SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids <- as.numeric(input$server_dashboard__reporting_client)
    
    SERVER_DASHBOARD_REFRESH(SERVER_DASHBOARD_REFRESH()+1)
    
  }
  
},ignoreNULL = FALSE)

observeEvent(input$server_dashboard__reporting_column_priority, {
  
  selected_ids <- as.numeric(input$server_dashboard__reporting_column_priority)
  if (!isTruthy(selected_ids) || empty(SERVER_DASHBOARD_INDICATORS())) {
    SERVER_DASHBOARD_RUN_OPTIONS$indicator_names <- character(0)
  } else {
    selected_indicators <- SERVER_DASHBOARD_INDICATORS()[indicator_id %in% selected_ids,
                                                         .(indicator_id,
                                                           indicator_name)]
    selected_indicators <- selected_indicators[order(match(indicator_id,selected_ids))]
    
    SERVER_DASHBOARD_RUN_OPTIONS$indicator_names <- selected_indicators$indicator_name
  }
},ignoreNULL = FALSE)

observeEvent(input$server_dashboard__reporting_asof_date, {
  
  if (!isTruthy(input$server_dashboard__reporting_asof_date)) SERVER_DASHBOARD_RUN_OPTIONS$asof_dates <- character(0)
  else SERVER_DASHBOARD_RUN_OPTIONS$asof_dates <- as.character(input$server_dashboard__reporting_asof_date)
  
  SERVER_DASHBOARD_REFRESH(SERVER_DASHBOARD_REFRESH()+1)
  
},ignoreNULL = FALSE)
observeEvent(input$server_dashboard__view_currency, {
  
  if (!isTruthy(SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids)) SERVER_DASHBOARD_RUN_OPTIONS$fx_currency <- "LCU"
  else SERVER_DASHBOARD_RUN_OPTIONS$fx_currency <- as.character(input$server_dashboard__view_currency)
  
  SERVER_DASHBOARD_REFRESH(SERVER_DASHBOARD_REFRESH()+1)
  
},ignoreNULL = FALSE)
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

observeEvent(SERVER_DASHBOARD_INDICATORS(), {

  dash_indicators <- SERVER_DASHBOARD_INDICATORS()
  if (empty(SERVER_DASHBOARD_INDICATORS())) return (NULL)
  selected_indicator_ids <- c()

  if (isTruthy(SERVER_DASHBOARD_RUN_OPTIONS$indicator_names)) {
    selected_indicator_ids <- dash_indicators[indicator_name %in% SERVER_DASHBOARD_RUN_OPTIONS$indicator_names,
                                              indicator_id]  
  }
  
  updateSelectizeInput(session=session,
                       inputId="server_dashboard__reporting_column_priority",
                       choices=setNames(dash_indicators$indicator_id,
                                        dash_indicators$indicator_name),
                       selected=selected_indicator_ids)
  
},
ignoreNULL = FALSE,
ignoreInit = FALSE,
priority = 9) # #need this observer to fire AFTER observeEvent(SERVER_DASHBOARD_REPORT_SELECTED() which sets SERVER_DASHBOARD_RUN_OPTIONS$indicator_names

observeEvent(SELECTED_PROGRAM_CLIENTS_LIST(), {
  
  clients <- SELECTED_PROGRAM_CLIENTS_LIST()
  
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

    timeline.choices <- valid_dates[is_future==FALSE,
                                    text_date]
    
    if (any(valid_dates$is_future)) timeline.choices <- c(`Future Date`="future",
                                                          timeline.choices)
    
    timeline.selected <- intersect(timeline.choices,
                                   as.character(SERVER_DASHBOARD_RUN_OPTIONS$asof_dates))
    
    if ((!isTruthy(timeline.selected) ||
         !any(timeline.selected=="future")) ) {
      timeline.selected <- valid_dates[is_future==FALSE,
                                       max(text_date)]
    }
    
    timeline.choices <- sort(timeline.choices,
                             decreasing = TRUE)
    
    updatePickerInput(session=session,
                      inputId="server_dashboard__reporting_asof_date",
                      selected=timeline.selected,
                      choices=timeline.choices)
  }
})

#Based on query, sets filter for NAMES
observeEvent(SERVER_DASHBOARD_CURRENT_QUERY(), {
  rsf_data <- SERVER_DASHBOARD_CURRENT_QUERY()
  
  if (empty(rsf_data)) {
    updatePickerInput(session=session,
                      inputId="server_dashboard__name_filter",
                      choices="",
                      selected="")
  } else {
    rsf_names <- NULL
    if (any("RSFNAME" %in% names(rsf_data))) rsf_names <- unique(rsf_data[,.(RSFNAME,SYSID)])[order(RSFNAME)]
    else rsf_names <- unique(rsf_data[,.(RSFNAME=SYSNAME,SYSID)])[order(RSFNAME)]
    
    updatePickerInput(session=session,
                      inputId="server_dashboard__name_filter",
                      choices=setNames(rsf_names$SYSID,
                                       rsf_names$RSFNAME),
                      selected="")
  }
})

SERVER_DASHBOARD_RENDER_DISPLAY <- function(display_data=SERVER_DASHBOARD_DATA_DISPLAY(),
                                            pivot=isolate({ toupper(input$server_dashboard__view_display) }),
                                            view="html") {
  
  if (!pivot %in% c("DATA","NAME","DATE")) pivot <- "DATA"
  if (!view %in% c("html","text","value","id")) stop("View must be: html, text, value, id")
  
  dashboard_data <- NULL
  if (pivot=="DATA") {
    cols <- c(grep("\\.",names(display_data),value=T,invert=T),
              grep(paste0("\\.",view,"$"),names(display_data),value=T))
    
    dashboard_data <- display_data[,..cols]
    setnames(dashboard_data,
             old=names(dashboard_data),
             new=gsub(paste0("\\.",view,"$"),"",names(dashboard_data)))
    
  } else if (pivot=="NAME") {
    dashboard_data <- display_data[parameter==view]
    dashboard_data[,parameter:=NULL]
  }
  return(dashboard_data)
}

output$server_dashboard__browser <- DT::renderDataTable({


  emode <- SERVER_DASHBOARD_MODE_EDITING()
  display_data <- SERVER_DASHBOARD_DATA_DISPLAY()

  run <- isolate({ SERVER_DASHBOARD_RUN_AUTORUN() })
  pivot <- isolate({ toupper(input$server_dashboard__view_display) })
  syscols <- isolate({ SERVER_DASHBOARD_RUN_OPTIONS$syscols })
  # display_indicator_names <- NULL
  editable <- NULL
  disabledRows <- NULL
  
  if (empty(display_data)) {
    print(paste0("DT::renderDataTable EMPTY TABLE ",Sys.time()))
    
    emptydf <- data.frame(Error="There is no data to display: Please check Reporting Date, Report Filter, Client Selections; and Column Selection filter.")
    
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
  
  
  
  dashboard_data <- SERVER_DASHBOARD_RENDER_DISPLAY(view="html",
                                                    display_data=SERVER_DASHBOARD_DATA_DISPLAY(),
                                                    pivot=isolate({ toupper(input$server_dashboard__view_display) }))
  # data_view <- NULL
  # if (emode==TRUE) data_view <- "edit"
  # else data_view <- DASH_DATA_OPTIONS$display_timeline_format #REACTIVE
  # 
  # 
  # display_data <- DASH_DATA_RENDER_DISPLAY(display_cell=data_view)


 
  
#browser()
  if (emode == TRUE) {
    reportingCols <- grep("^reporting_|^indicator_name$",names(display_data))-1

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
    if (any(display_data$reporting_history==TRUE) && any(display_data$reporting_history==FALSE)) {
      reporting_history_row <- which(names(display_data)=="reporting_history")-1
      disabledRows <- JS(paste("function(row, data, displayNum, displayIndex, dataIndex) {

                                          if (data[",reporting_history_row,"].toString().toLowerCase() =='false') {
                                            $(row).addClass('disabledEvents');
                                          }
                                         }"))
    }
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
  
  hidden_cols <- which(names(dashboard_data) %in% c("REPORTING_group_rank","REPORTING_row",syscols.hide))-1 #0-based index.

  

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
                                   rowCallback = disabledRows,
                                   pageLength=100,
                                   columnDefs = list(list(visible=FALSE, targets=hidden_cols),
                                                     list(className = 'dt-left', targets = "_all")))
  ) %>% formatStyle("REPORTING_group_rank",
                    target = "row",
                    backgroundColor = styleEqual(c(1, 3,
                                                   2, 4),
                                                 row_colors)) %>%
    formatStyle(columns=1:ncol(dashboard_data)-1,
                target="cell",
                `max-width` = "250px",
                `white-space`="nowrap",
                `text-overflow`="ellipsis",
                `overflow`="hidden") %>%

    #formatStyle(columns=c("reporting_asof_date","reporting_SYSID"),
    formatStyle(columns=c("REPORTING_asof_date"),
                target="cell",
                `max-width` = "150px",
                `white-space`="nowrap",
                `text-overflow`="ellipsis",
                `overflow`="hidden")

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

output$action_server_dashboard__download <- downloadHandler(
  filename=function() {
    #file <- DASH_DATA_EDITING_DEFAULT_FILENAME()
    "RSF Data Export.xlsx"
  },
  content=function(file) {


    #dd_dt <- DASH_DATA_DISPLAY()
    export_data <- dashboard_data <- SERVER_DASHBOARD_RENDER_DISPLAY(view="value",
                                                                     display_data=SERVER_DASHBOARD_DATA_DISPLAY(),
                                                                     pivot=isolate({ toupper(input$server_dashboard__view_display) }))
    if (!isTruthy(export_data)) return (NULL)

    if (isTruthy(SERVER_DASHBOARD_RUN_OPTIONS$format_exceldates)) {
      
      excelOriginOffset <- lubridate::origin - ymd("1900-01-01",tz="UTC") + 2
      
      rsf_family_data[,
                      REPORTING_asof_date:=as.integer(ymd(REPORTING_asof_date)) + as.integer(excelOriginOffet)]
      
      if (any("DATA_currentest_date" %in% names(rsf_family_data)) &&
          all(grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$",rsf_family_data$DATA_currentest_date))) {
        
        rsf_family_data[,
                        DATA_currentest_date:=as.integer(ymd(DATA_currentest_date)) + as.integer(excelOriginOffet)]
      }
    }
    
    report_note <- c()

    opts <- reactiveValuesToList(SERVER_DASHBOARD_RUN_OPTIONS)
    opts$rsf_pfcbl_ids <- NULL
    opts$indicator_ids <- NULL
    opts$pivot <- isolate({ toupper(input$server_dashboard__view_display) })
  
    report_note <- paste0(sapply(names(opts),function(x) { paste0(x,": ",paste0(opts[x],collapse=", ")) }),collapse=" & ")

    export_data_flags <- NULL

    savedwb <- withProgress(message=paste0("Generating Excel report.  This may take a few moments...."),value=0.3, {

      excelwb <- DBPOOL %>% export_dashboard_view_to_excel(export_data=export_data,
                                                           export_data_flags=export_data_flags,
                                                           exporting_user_id=USER_ID(),
                                                           report_id=NA,
                                                           report_note=report_note)

      savedwb <- openxlsx::saveWorkbook(excelwb,file,overwrite=TRUE)
      incProgress(amount=1.0,message="Completed")
      savedwb
    })

    savedwb
  },
  contentType="application/xlsx")


{
# observeEvent(SELECTED_PROGRAM_VALID_REPORTING_DATES(), {
#   
#   date_choices <- character(0)
#   selected <- input$rsfdash_reporting_asof_date
#   program_dates <- SELECTED_PROGRAM_VALID_REPORTING_DATES()
#   program_dates <- as.character(program_dates)
#   
#   if (isTruthy(SELECTED_PROGRAM_VALID_REPORTING_DATES())) date_choices <- program_dates
#   if (!isTruthy(selected) || !selected %in% date_choices) selected <- date_choices[length(date_choices)]
#   
#   updateSelectizeInput(session=session,
#                        inputId="rsfdash_reporting_asof_date",
#                        choices=date_choices,
#                        selected=selected)
# }, priority = 109, ignoreNULL = FALSE,ignoreInit = FALSE)
# 
# #SETS:
# #rsfdash_reporting_client
# observeEvent(DASH_CLIENTS_LIST(), {
#   
#   clients <- DASH_CLIENTS_LIST()
#   client_choices <- character(0)
#   selected <- DASH_DATA_QUERY_RSF_PFCBL_IDS()
#   if (isTruthy(selected)) selected <- selected[selected %in% clients$rsf_pfcbl_id]
#   if (isTruthy(clients)) client_choices <- setNames(clients$rsf_pfcbl_id,clients$client_name)
#   
#   if (!isTruthy(selected) || !any(selected %in% client_choices)) selected <- NULL
#   if (isTruthy(clients) && all(clients$rsf_pfcbl_id==0)) selected <- 0 #Global
#   
#   updatePickerInput(session=session,
#                     inputId="rsfdash_reporting_client",
#                     choices=client_choices,
#                     selected=selected)
#   
# }, priority= 108, ignoreNULL = FALSE)
# 
# #SETS:
# #rsfdash_reporting_report_filter < Note, this may change when program subscribes or unsubscribes from reports
# observeEvent(DASH_REPORTS_INDICATORS_LIST(), {
#   
#   report_indicators <- DASH_REPORTS_INDICATORS_LIST()
#   if (!isTruthy(report_indicators)) {
#     updateSelectizeInput(session=session,
#                          inputId="rsfdash_reporting_report_filter",
#                          choices=character(0),
#                          selected="")
#     
#   } else {
#     reports <- unique(report_indicators[report_id > 0,.(report_id,report_name,is_system)])
#     reports_choices <- setNames(reports$report_id,reports$report_name)
#     
#     updateSelectizeInput(session=session,
#                          inputId="rsfdash_reporting_report_filter",
#                          choices=reports_choices,
#                          selected="")
#   }
# }, priority=107, ignoreNULL = FALSE)
# 
# observeEvent(SELECTED_PROGRAM(), {
#   
#   if (!isTruthy(SELECTED_PROGRAM())) {
#     
#     updateSelectizeInput(session=session,
#                          inputId="rsfdash_reporting_column_priority",
#                          choices=c(),
#                          selected=NULL)
#     
#     updateSelectizeInput(session=session,
#                          inputId="rsfdash_reporting_column_select",
#                          choices=c(),
#                          selected=NULL)
#     
#   } else {
#     
#     program_indicators <- RSF_INDICATORS()[,
#                                            .(indicator_pfcbl_rank,
#                                              indicator_id,
#                                              indicator_name,
#                                              sort_category=data_category,
#                                              is_global=(indicator_pfcbl_rank==0))]
#     setorder(program_indicators,
#              -is_global,
#              indicator_pfcbl_rank)
#     #TODO:
#     #Also trigger observer on changes to indicator subscriptions in setups to update these options
#     priority_choices <- sort(setNames(program_indicators$indicator_id,
#                                       program_indicators$indicator_name))
#     
#     updateSelectizeInput(session=session,
#                          inputId="rsfdash_reporting_column_priority",
#                          choices=priority_choices,
#                          selected=NULL)
#     
# 
#     filter_choices <- program_indicators[,
#                                          .(indicator_members=list(setNames(indicator_id,indicator_name))),
#                                          by=.(sort_category)
#     ][,
#       .(choice=list(setNames(indicator_members,sort_category))),
#       by=.(sort_category)]
#     
#     filter_choices <- unlist(filter_choices$choice,recursive=FALSE)
#     
#     updatePickerInput(session=session,
#                       inputId="rsfdash_reporting_column_select",
#                       choices=filter_choices,
#                       selected=NULL)
#   }
#   
# }, priority= 106, ignoreNULL=FALSE)
# 

# 
# observeEvent(input$rsfdash_reporting_client, {
#   selected_client_ids <- as.numeric(input$rsfdash_reporting_client)
#   if (anyNA(selected_client_ids)) selected_client_ids <- selected_client_ids[!is.na(selected_client_ids)]
#   if (!isTruthy(selected_client_ids)) selected_client_ids <- c()
#   
#   DASH_DATA_QUERY_RSF_PFCBL_IDS(selected_client_ids)
#   
# },priority = 105, ignoreInit=TRUE)
# #A report has been selected from the dropdown list
# #(1) Update rsfdash_reporting_column_select to select the columns that are specified in the selected report
# #(2) Reset the priority filter
# observeEvent(input$rsfdash_reporting_report_filter, {
#   
#   selected_report_id <- as.numeric(input$rsfdash_reporting_report_filter)
#   if (!isTruthy(selected_report_id)) selected_report_id <- DEFAULT_REPORT_ID
#   
#   if (DASH_DATA_OPTIONS$display_report_id==selected_report_id) return (NULL)
#   else DASH_LOAD_DASHBOARD(display_report_id=selected_report_id)
#   
# },priority = 104,ignoreInit=TRUE)
# 
# observeEvent(input$rsfdash_reporting_column_priority, {
#   
#   print(paste0("observeEvent(input$rsfdash_reporting_column_priority: ",paste0(input$rsfdash_reporting_column_priority,collapse=",")))
#   if (!isTruthy(input$rsfdash_reporting_column_priority)) return (NULL)
#   
#   priority_ids <- as.numeric(input$rsfdash_reporting_column_priority)
#   if (anyNA(priority_ids)) priority_ids <- priority_ids[-which(is.na(priority_ids))]
#   unqueried_ids <- setdiff(priority_ids,DASH_DATA_QUERY_INDICATORS_READ())
#   
#   if (length(unqueried_ids) > 0) {
#     selected_ids <- unique(as.numeric(c(priority_ids,input$rsfdash_reporting_column_select)))
#     if (anyNA(selected_ids)) selected_ids <- selected_ids[-which(is.na(selected_ids))]
#     updatePickerInput(session=session,
#                       inputId="rsfdash_reporting_column_select",
#                       selected=selected_ids)
#   }
#   
# }, priority = 103, ignoreNULL = FALSE,ignoreInit=TRUE)
# 
# observeEvent(input$rsfdash_reporting_column_select, {
#   print(paste0("observeEvent(input$rsfdash_reporting_column_select: ",paste0(input$rsfdash_reporting_column_select,collapse=",")))
#   
#   if (!isTruthy(input$rsfdash_reporting_column_select)) {
#     DASH_DATA_QUERY_INDICATORS_WRITE(c())
#     return (NULL)
#   }
#   
#   selected_ids <- as.numeric(input$rsfdash_reporting_column_select)
#   if (anyNA(selected_ids)) selected_ids <- selected_ids[-which(is.na(selected_ids))]
#   
#   selected_ids <- setdiff(selected_ids,DASH_DATA_QUERY_INDICATORS_READ())
#   if (length(selected_ids) > 0) {
#     DASH_DATA_QUERY_INDICATORS_WRITE(unique(c(selected_ids,DASH_DATA_QUERY_INDICATORS_READ())))
#   }
# }, priority = 102, ignoreNULL=FALSE,ignoreInit = TRUE)
# 
# observeEvent(input$rsfdash_flags_filter, {
#   
#   all_flags <- c("critical","error","warning","info")
#   
#   filter_flags <- input$rsfdash_flags_filter
#   
#   if (!isTruthy(filter_flags)) filter_flags <- NA
#   else if (any(filter_flags=="any")) filter_flags <- all_flags
#   else filter_flags <- intersect(filter_flags,all_flags)
#   
#   DASH_DATA_OPTIONS$filter_flags <- filter_flags
#   
# },ignoreNULL = FALSE)
# 
# observeEvent(input$rsfdash_reporting_search, {
#   keywords <- input$rsfdash_reporting_search
#   if (!isTruthy(keywords)) keywords <- as.character(NA)
#   
#   DASH_DATA_OPTIONS$filter_text <- keywords
#   
# },ignoreNULL = FALSE)
# 
# observeEvent(input$rsfdash_filter_indicator_types, {
#   filter_indicator_types <- input$rsfdash_filter_indicator_types
#   if (!isTruthy(filter_indicator_types)) filter_indicator_types <- as.character(NA)
#   else filter_indicator_types <- intersect(toupper(filter_indicator_types),c("USER","CALCULATED","SYSTEM"))
#   
#   DASH_DATA_OPTIONS$filter_indicator_types <- filter_indicator_types
# })
# 
# observeEvent(input$rsfdash_view_currency, {
#   current_currency <- toupper(input$rsfdash_view_currency)
#   
#   if (DASH_DATA_EDITING()==TRUE && current_currency != "LCU") {
#     updateSelectizeInput(session=session,
#                          inputId="rsfdash_view_currency",
#                          selected="LCU")
#     return(showNotification(type="error",
#                             ui=h2("Currency conversion disallowed during data editing. Reverting to default Local Currency view")))
#   }
#   
#   if (!isTruthy(current_currency) ||
#       !grepl("^[A-Z]{3}$",current_currency)  ||
#       current_currency == "LCU") current_currency <- as.character(NA)
#   
#   DASH_DATA_OPTIONS$display_currency <- current_currency
#   
# })
# 
# observeEvent(input$rsfdash_reporting_history, {
#   display_timeline <- as.numeric(input$rsfdash_reporting_history)
#   
#   
#   if (!isTruthy(display_timeline) || !display_timeline %in% c(0,1,2)) display_timeline <- 0 #current
#   if (DASH_DATA_EDITING()==TRUE & (display_timeline != 0) != DASH_DATA_OPTIONS$display_timeline) {
#     updateSelectizeInput(session=session,
#                          inputId="rsfdash_reporting_history",
#                          selected=DASH_DATA_OPTIONS$display_timeline)
#     return(showNotification(type="error",
#                             h3("Timeseries display cannot be changed while editing.")))
#   }
#   DASH_DATA_OPTIONS$display_timeline <- display_timeline != 0
#   DASH_DATA_OPTIONS$display_timeline_format <- fcase(display_timeline==0,"html",
#                                                      display_timeline==1,"html",
#                                                      display_timeline==2,"current",
#                                                      default="html")
# })
# 
# 
# 
# 
# output$rsfdash_panel_cell_info <- renderUI({ 
#   info <- DASH_DATA_CELL_INFO()
#   if (!isTruthy(info)) info <- ""
#   HTML(info) 
# })
# output$rsfdash_title <- renderText({
#   program <- SELECTED_PROGRAM()
#   if (!isTruthy(program)) return ("Please select an RSF Program")
#   else return (paste0(SELECTED_PROGRAM()$program_nickname," Data Browser"))
# })
# 

# 
# 









































###############################

# SERVER_DASHBOARD.INDICATORS_REFRESH <- reactiveVal(1)
# DASH_DATA_QUERY_RSF_PFCBL_IDS <- reactiveVal(c())
# 
# DASH_DATA_QUERY_INDICATORS_WRITE <- reactiveVal(c()) 
# 
# DASH_INDICATORS <- eventReactive(c(SELECTED_PROGRAM(),               #monitors program to ensure reset on program select
#                                    DASH_DATA_QUERY_RSF_PFCBL_IDS(),
#                                    SERVER_DASHBOARD.INDICATORS_REFRESH()),
# {
#  
#   if (!isTruthy(SELECTED_PROGRAM())) return (NULL)
#   
#   
#   ids <- na.omit(as.numeric(DASH_DATA_QUERY_RSF_PFCBL_IDS()))
#   if (!isTruthy(ids)) return (NULL)
#   
#   dash_indicators <- DBPOOL %>% dbGetQuery("
#     select distinct 
#       pis.indicator_id,
#       pis.indicator_name,
#       pis.pfcbl_category,
#       pis.is_calculated,
#       pis.data_type
#     from p_rsf.view_rsf_pfcbl_indicator_subscriptions pis
#     where pis.rsf_pfcbl_id = any(select distinct
#                                  ft.to_family_rsf_pfcbl_id 
#                                  from p_rsf.view_rsf_pfcbl_id_family_tree ft
#                                  where ft.from_rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int))
#       and pis.is_subscribed = true",
#     params=list(paste0(unique(ids),collapse=",")))
#   
#   setDT(dash_indicators)
#   return (dash_indicators)
#   
# },ignoreNULL=FALSE,ignoreInit = TRUE)
# 
# DASH_DATA_QUERY_INDICATORS_READ <- eventReactive(c(input$rsfdash_reporting_column_select,
#                                                    DASH_DATA_QUERY_INDICATORS_WRITE()), { 
#   print(paste0("DASH_DATA_QUERY_INDICATORS_READ ",Sys.time()))                                                
#   ids <- unique(c(input$rsfdash_reporting_column_select,DASH_DATA_QUERY_INDICATORS_WRITE()))
#   if (anyNA(ids)) ids <- ids[-which(is.na(ids))]
#   return (ids)
# }, ignoreNULL=FALSE) %>% debounce(600) #If users are selecting multiple indicators, dont refresh each time!
# 
# DASH_DATA_SELECTED_INDICATORS_READ <- eventReactive(DASH_DATA_QUERY_INDICATORS_READ(), {
#   
#   print(paste0("DASH_DATA_SELECTED_INDICATORS_READ ",Sys.time()))                                                
#   selected_ids <- input$rsfdash_reporting_column_select
#   priority_ids <- input$rsfdash_reporting_column_priority
#   
#   if (!all(priority_ids %in% selected_ids)) {
#     print("WARNING DASH_DATA_SELECTED_INDICATORS_READ: priority_ids requesting indicators not in selected_ids")
#     #return (as.numeric(NA))
#   }
#   
#   ids <- unique(as.numeric(c(selected_ids,priority_ids)))
#   if (anyNA(ids)) ids <- ids[-which(is.na(ids))]
#   
#   if (!isTruthy(ids)) ids <- as.numeric(NA)
#   
#   if (!all(is.na(ids)) && !all(ids %in% DASH_DATA_QUERY_INDICATORS_READ())) {
#     print("WARNING DASH_DATA_SELECTED_INDICATORS_READ: selected ids requested not in indicators ids queried")
#     print(paste0("DASH_DATA_QUERY_INDICATORS_READ: ",paste0(DASH_DATA_QUERY_INDICATORS_READ(),collapse=",")))
#     print(paste0("input$rsfdash_reporting_column_select: ",paste0(input$rsfdash_reporting_column_select,collapse=",")))
#     stop("Arg")
#     #return (as.numeric(NA))
#     #return (as.numeric(NA))
#   }
#   
# 
#   
#   return (ids)
# }, ignoreNULL = FALSE)
# 
# DASH_DATA_KEYWORDS <- reactiveVal(NA)
# DASH_DATA_REFRESH <- reactiveVal(1)
# DASH_DATA_OPTIONS <- reactiveValues(display_format="DEFAULT", #DEFAULT | LONG | INDICATORS | NAMES
#                                     display_currency=NA, #Default/LCU
#                                     display_timeline=FALSE,
#                                     display_timeline_format="html", #html is rendered html vs current is current value
#                                     display_blanks="text",
#                                     display_report_id=DEFAULT_REPORT_ID, #Blank report
#                                     truncate_previous=TRUE,
#                                     truncate_next=TRUE,
#                                     truncate_unchanges=TRUE,
#                                     column_status=TRUE,
#                                     column_sysname=FALSE,
#                                     download_include_flags=TRUE,
#                                     download_format="excel",
#                                     filter_status=NA,
#                                     filter_text=as.character(NA),
#                                     filter_indicator_types=as.character(NA),
#                                     filter_flags=as.character(NA)) #filter columns with flags, of types listed in vector; or no filter if empty
# 
# 
# DASH_DATA_EDITING <- reactiveVal(FALSE)
# DASH_DATA_EDITING_COUNT <- reactiveVal(0) #Counter of number of times user has edited data in this session -- used only for guidance message.
# 
# DASH_DATA_CELL_INFO <- reactiveVal("")
# 
# DASH_DATA_EDITING_NEW_DATA <- reactiveVal(NULL)
# DASH_DATA_EDITING_DEFAULT_FILENAME <- reactive({
#   
#   reporting_date <- suppressWarnings(ymd(input$rsfdash_reporting_asof_date))
#   if (!isTruthy(reporting_date)) {
#     return(NULL)
#   }
#   
#   
#   clients <- as.numeric(DASH_DATA_QUERY_RSF_PFCBL_IDS())
#   if (!isTruthy(clients) || anyNA(clients)) return (NULL)
#   
#   clients_list <- SELECTED_PROGRAM_CLIENTS_LIST()
#   selected_program <- SELECTED_PROGRAM()
#   if (all(clients_list$rsf_pfcbl_id %in% clients) || all(clients == selected_program$rsf_pfcbl_id)) {
#     client_names <- paste0(selected_program$program_nickname," PROGRAM")
#   } else {
#     client_names <- clients_list[rsf_pfcbl_id %in% clients,
#                                  .(client_name=paste0(name," ",id))]
#     
#     client_names <- paste0(sort(client_names$client_name),collapse=" + ")
#     
#   }
#   
#   
#   default_filename <- paste0(client_names," - ",
#                              format_asof_date_label(reporting_date),".xlsx")
#   
# })
# DASH_DATA_EDITING_TIMESERIES_CHOICE <- reactiveVal(NA)
# 
# REFRESH_PROGRAM_REPORTS <- reactiveVal(0)
# DASH_REPORTS_INDICATORS_LIST <- reactive({
#   
#   program <- SELECTED_PROGRAM()
# 
#   if (!isTruthy(program)) return (NULL)
#   refresh <- REFRESH_PROGRAM_REPORTS()
#   subscribed_indicators <- DASH_INDICATORS()
#   if (!isTruthy(subscribed_indicators)) return(NULL)
#   print("reactive: DASH_REPORTS_INDICATORS_LIST: must redo reports")
# 
#     return(NULL)
# 
#   
# })
# 
# DASH_LOAD_DASHBOARD <- function(reporting_asof_date=input$rsfdash_reporting_asof_date,
#                                 rsf_pfcbl_ids=input$rsfdash_reporting_client,
#                                 indicator_ids=c(),
#                                 dashboard_info_msg=NA,
#                                 display_report_id=BLANK_REPORT_ID,
#                                 display_currency="LCU",
#                                 display_timeline=FALSE,
#                                 display_timeline_format=c("html","current"),
#                                 display_flags=TRUE,
#                                 column_status=FALSE,
#                                 column_sysname=TRUE,
#                                 filter_flags=as.character(NA),
#                                 filter_text=as.character(NA),
#                                 filter_indicator_types=as.character(NA), #USER, CALCULATED, SYSTEM (note: user includes calculatables)
#                                 indicator_ids.sorted=FALSE) {
# 
#   rsf_pfcbl_ids <- na.omit(as.numeric(rsf_pfcbl_ids))
#   reporting_asof_date <- as.character(reporting_asof_date)
#   reporting_asof_date <- unique(reporting_asof_date)
#   selected_program <- SELECTED_PROGRAM()  
#   display_timeline_format <- match.arg(display_timeline_format)
#   
#   #if (!isTruthy(display_report_id)) display_report_id <- BLANK_REPORT_ID
#   if (!isTruthy(display_currency)) display_currency <- "LCU"
#   if (is.na(as.logical(display_timeline))) display_timeline <- FALSE 
#   #!isTruthy(display_timeline) || !display_timeline %in% c(0,1,2)) display_timeline <- 0
#   
#   display_timeline_and_format <- fcase(display_timeline==FALSE,0,
#                                        display_timeline==TRUE & display_timeline_format=="html",1,
#                                        display_timeline==TRUE & display_timeline_format=="current",2,
#                                        default=NA)
#   
#   if (!isTruthy(selected_program)) return (FALSE)
#   if (length(reporting_asof_date) != 1) return (FALSE)
# 
#   program_indicators <- DASH_INDICATORS() #db_program_get_indiciators with filter=all
#   program_clients <- SELECTED_PROGRAM_CLIENTS_LIST()
# 
#   #In case loan-level rsf_pfcbl_ids are passed in, then get parent-level  
#   client_rsf_pfcbl_ids <- DBPOOL %>% dbGetQuery("
#                                                 select 
#                                                   ft.rsf_pfcbl_id
#                                                 from p_rsf.get_rsf_pfcbl_id_family_tree(string_to_array($1::text,',')::int[]) ft
#                                                 where ft.pfcbl_rank <= 3",
#                                                 params=list(paste0(rsf_pfcbl_ids,collapse=",")))
#   
#   client_rsf_pfcbl_ids <- unlist(client_rsf_pfcbl_ids)
#   client_rsf_pfcbl_ids <- client_rsf_pfcbl_ids[client_rsf_pfcbl_ids %in% program_clients$rsf_pfcbl_id]
#   
#   report_indicators <- DASH_REPORTS_INDICATORS_LIST()
#   reports <- unique(report_indicators[,.(report_id,report_name)])
#   
#   if (!isTruthy(display_report_id) || !display_report_id %in% reports$report_id) {
#     display_report_id <- BLANK_REPORT_ID
#   } else {
#     indicator_ids <- unique(c(indicator_ids,report_indicators[report_id==display_report_id,indicator_id]))
#   }
#   
#   # if (!isTruthy(indicator_ids)) {
#   #   display_report_id <- ""
#   #   indicator_ids <- report_indicators[report_id==display_report_id,indicator_id]
#   # }
#   
#   if (!isTruthy(program_indicators)) indicator_ids <- c()
#   else {
#     program_indicators <- program_indicators[indicator_id %in% indicator_ids]
#     if (all(program_indicators[,data_category] %in% c("global","program"))) {
#       client_rsf_pfcbl_ids <- selected_program$rsf_pfcbl_id
#     }
#   }
#   
#   #If Global program is selected, always enforce selecting global program
#   if (selected_program$rsf_pfcbl_id==0) client_rsf_pfcbl_ids <- 0
#   
#   removeModal()
#   
#   updateTabItems(session=session,
#                  inputId="sidebarMenu",
#                  selected="dashboard")
# 
#   DASH_DATA_OPTIONS$display_report_id <- display_report_id
#   DASH_DATA_OPTIONS$display_currency <- display_currency
#   DASH_DATA_OPTIONS$display_timeline <- display_timeline
#   DASH_DATA_OPTIONS$display_timeline_format <- display_timeline_format
#   DASH_DATA_OPTIONS$display_flags <- display_flags
#   DASH_DATA_OPTIONS$column_status <- as.logical(column_status)
#   DASH_DATA_OPTIONS$column_sysname <- as.logical(column_sysname)
#   DASH_DATA_OPTIONS$filter_text <- filter_text
#   DASH_DATA_OPTIONS$filter_indicator_types <- filter_indicator_types
#   
#   DASH_DATA_QUERY_RSF_PFCBL_IDS(client_rsf_pfcbl_ids)
#   
#   for (oname in names(options)) DASH_DATA_OPTIONS[[oname]] <- options[[oname]]
# 
#   #This should always be the case, unless program indicators are selected and pushing query to program level.
#   if (any(client_rsf_pfcbl_ids %in% program_clients$rsf_pfcbl_id)) {
#     updatePickerInput(session=session,
#                       inputId="rsfdash_reporting_client",
#                       selected=client_rsf_pfcbl_ids)
#   }
#   
#   updatePickerInput(session=session,
#                     inputId="rsfdash_flags_filter",
#                     selected=filter_flags)
#   
#   updateTextInput(session=session,
#                   inputId="rsfdash_reporting_search",
#                   value = filter_text)
# 
#   updateSelectizeInput(session=session,
#                        inputId="rsfdash_reporting_asof_date",
#                        selected=reporting_asof_date)
#   
#   updateSelectizeInput(session=session,
#                        inputId="rsfdash_reporting_history",
#                        selected=display_timeline_and_format)
#   
#   updateSelectizeInput(session=session,
#                        inputId="rsfdash_view_currency",
#                        selected=display_currency)
#   
#   
#   
#   
#   #Enforce report filter: in case same report is already selected and user adds new variables to it meanwhile
#   #Refresh will also reset priority columns
#   #runjs(paste0('Shiny.setInputValue("rsfdash_reporting_report_filter",',display_report_id,',{priority: "event"})'))
#   
#   DASH_DATA_QUERY_INDICATORS_WRITE(indicator_ids)
# 
#   updateSelectizeInput(session=session,
#                        inputId="rsfdash_reporting_column_priority",
#                        selected="")
#   
#   updateSelectizeInput(session=session,
#                        inputId="rsfdash_reporting_report_filter",
#                        selected=as.character(display_report_id))
#   
#   updateSelectizeInput(session=session,
#                        inputId="rsfdash_reporting_column_select",
#                        selected=indicator_ids)
#   
#   dashboard_info_msg <- trimws(dashboard_info_msg)
#   if (isTruthy(dashboard_info_msg)) DASH_DATA_CELL_INFO(dashboard_info_msg)
# }
# 
# 
# DASH_CLIENTS_LIST <- eventReactive(c(SELECTED_PROGRAM_CLIENTS_LIST(),
#                                      input$rsfdash_reporting_asof_date), {
#   
#   rsf_program_id <- SELECTED_PROGRAM_ID()
#   if (!isTruthy(rsf_program_id)) return (NULL)
#   
#   reporting_date <- suppressWarnings(ymd(input$rsfdash_reporting_asof_date)) #different clients will be available at different times, depending on their inclusion date
#   if (!isTruthy(reporting_date)) return (NULL)
#   
#   clients <- SELECTED_PROGRAM_CLIENTS_LIST()
#   if (!isTruthy(clients)) return (NULL)
#   
#   clients <- clients[created_in_reporting_asof_date <= reporting_date]
#   if (!isTruthy(clients)) return (NULL)
#   
#   if(SYS_PRINT_TIMING) debugtime("reactive: DASH_CLIENTS_LIST")
#   
#   return (clients)
# }, ignoreNULL = FALSE)
# 
# #DASH_RSF_FAMILY <- reactive({
# DASH_RSF_FAMILY <- eventReactive(c(SELECTED_PROGRAM(),                #New program new family
#                                    DASH_DATA_QUERY_RSF_PFCBL_IDS(),    #New client(s) selection, new family
#                                    input$rsfdash_reporting_asof_date, #New reporting date, new history view window
#                                    DASH_DATA_OPTIONS$display_timeline),  #New history selection, new timeseries
# {
#   print(paste0("DASH_RSF_FAMILY ",Sys.time()))                                       
#   program <- SELECTED_PROGRAM()
#   
#   if (empty(program)) return (NULL)
#   if (!isTruthy(DASH_DATA_QUERY_RSF_PFCBL_IDS())) return (NULL)
#   
#   clients_list <- DASH_CLIENTS_LIST()
#   
#   #clients <- as.numeric(input$rsfdash_reporting_client)
#   query_rsf_pfcbl_ids <- unique(c(0,as.numeric(DASH_DATA_QUERY_RSF_PFCBL_IDS()))) #Global always present in query
#   if (anyNA(query_rsf_pfcbl_ids)) query_rsf_pfcbl_ids <- query_rsf_pfcbl_ids[!is.na(query_rsf_pfcbl_ids)]
#   
#   reporting_date <- as.character(input$rsfdash_reporting_asof_date)
#   #refresh <- DASH_DATA_REFRESH() #reactive
#   
#   if (!isTruthy(reporting_date) || !(reporting_date %in% as.character(SELECTED_PROGRAM_VALID_REPORTING_DATES()))) return (NULL)
#   
#   #TODO if program doesn't have any clients then client_list should return facilities
#   #if (!isTruthy(query_rsf_pfcbl_ids) && !empty(clients_list)) return (NULL)
#   #if (empty(clients_list) || !isTruthy(clients)) clients <- program$rsf_pfcbl_id
#   
#   display_timeline <- as.logical(DASH_DATA_OPTIONS$display_timeline)
#   #if (!isTruthy(display_timeline) || !display_timeline %in% c(0,1,2)) display_timeline <- 0
#   
#   history_selection <- display_timeline #!= 0 
#   if (is.null(history_selection) || is.na(history_selection)) history_selection <- FALSE
#   
#   
#   
#   rsf_family <- DBPOOL %>% dbGetQuery("
#                                       select 
#                                     		ids.rsf_pfcbl_id,
#                                     		prd.valid_reporting_date as reporting_asof_date,
#                                     		ids.rsf_program_id,
#                                     		ids.rsf_facility_id,
#                                     		ids.rsf_client_id,
#                                     		ids.rsf_borrower_id,
#                                     		ids.rsf_loan_id,
#                                     		ids.rsf_id,
#                                     		ids.pfcbl_category,
#                                     		ids.pfcbl_category_rank,
#                                     		ids.created_in_reporting_asof_date as start_date,
#                                     		currentest.reporting_date as currentest_date,
#                                     		sn.sys_name,
#                                     		nids.rsf_full_name,
#                                         status.quarter_end_reporting_status as status,
#                                         coalesce(reporting.reported,false) as exists
#                                     	from p_rsf.rsf_pfcbl_ids ids
#                                     	inner join p_rsf.rsf_program_reporting_dates prd on prd.rsf_program_id = ids.rsf_program_id
#                                     	inner join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.rsf_pfcbl_id = ids.rsf_pfcbl_id
#                                     	inner join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = ids.rsf_pfcbl_id
#                                     	inner join lateral p_rsf.get_rsf_pfcbl_id_reporting_status_asof_date(input_rsf_pfcbl_id => ids.rsf_pfcbl_id,
#                                                                                                            input_pfcbl_category => ids.pfcbl_category,
#                                     																																			 input_current_date => prd.valid_reporting_date) as status on true
#                                     	left join lateral(select max(rpr.reporting_asof_date) as reporting_date
#                                     	                  from p_rsf.rsf_pfcbl_reporting rpr
#                                     	                  where rpr.rsf_pfcbl_id = ids.rsf_pfcbl_id) as currentest on true
#                                       left join lateral(select true as reported
#                                     	                  from p_rsf.rsf_pfcbl_reporting rpr
#                                     										where rpr.rsf_pfcbl_id = ids.rsf_pfcbl_id
#                                     										  and rpr.reporting_asof_date = prd.valid_reporting_date) as reporting on true																																	 
#                                     	where ids.rsf_pfcbl_id = any(select ft.rsf_pfcbl_id
#                                     															 from p_rsf.get_rsf_pfcbl_id_family_tree(string_to_array($2::text,',')::int[]) ft)
#                                     		and prd.valid_reporting_date <= $3::date
#                                     		and prd.valid_reporting_date >= ids.created_in_reporting_asof_date
#                                     		and prd.valid_reporting_date >= (select created_in_reporting_asof_date
#                                     																		 from p_rsf.rsf_pfcbl_ids
#                                     																		 where rsf_program_id = $1::int
#                                     																			 and pfcbl_category in ('global','program'))
#                                     		and case 
#                                     			when $4::bool = false then prd.valid_reporting_date = $3::date -- current data
#                                     			else true                                                      -- full change history
#                                     		end",
#                                       params=list(program$rsf_program_id,
#                                                   paste0(query_rsf_pfcbl_ids,collapse=","),
#                                                   reporting_date,
#                                                   history_selection))
#   
#   setDT(rsf_family)
#   rsf_family[,reporting_current_date:=reporting_date]
#   recognized_status <- c("ACTIVE","INACTIVE","PREACTIVE")  #ERROR and NOTEXISTS is also possible from the database function, 
#   if (any(!unique(rsf_family$status) %in% recognized_status)) {
#     stop(paste0("Unrecognized status value: ",
#                 paste0(setdiff(unique(rsf_family$status),recognized_status),collapse=", ")))
#   }
#   
#   print(paste0("DASH_RSF_FAMILY ROWS: ",nrow(rsf_family)))
#   
#   return (rsf_family)
#   
# }, ignoreNULL=FALSE) %>% debounce(250) 
# 
# DASH_DATA <- eventReactive(c(DASH_RSF_FAMILY(),
#                              DASH_DATA_QUERY_INDICATORS_READ(),
#                              DASH_DATA_REFRESH(),
#                              DASH_DATA_OPTIONS$display_currency,
#                              DASH_DATA_OPTIONS$display_flags), 
# {
# 
#   program <- SELECTED_PROGRAM()
#   rsf_family <- DASH_RSF_FAMILY()
#   refresh <- DASH_DATA_REFRESH() #reactive
# 
#   #browser()
#   
#   if (empty(rsf_family)) return (NULL)
#   if (empty(program)) return (NULL)
# 
#   t1<-Sys.time()
#   print(paste0("DASH_DATA ",Sys.time()))
#   
#   all_dates <- SELECTED_PROGRAM_VALID_REPORTING_DATES()
#   reporting_date <- as.character(unique(rsf_family$reporting_current_date))
#   if (!isTruthy(reporting_date) || !(reporting_date %in% as.character(all_dates))) return (NULL)
#   
#   query_indicator_ids <- DASH_DATA_QUERY_INDICATORS_READ()
#   query_indicator_ids <- na.omit(unique(unlist(query_indicator_ids)))
#   
#   
#   rsf_indicators <- DASH_INDICATORS()
#   if (!isTruthy(rsf_indicators)) return (NULL)
#   
#   rsf_indicators <- rsf_indicators[indicator_id %in% query_indicator_ids]
#   if (empty(rsf_indicators)) return (NULL) 
#   
#   query_indicator_ids <- rsf_indicators$indicator_id
#   #No need to pass rsf_pfcbl_ids for entities whose rank will be excluded from requested indicators
# 
#   
#   max_indicator_pfcbl_rank <- max(rsf_indicators$indicator_pfcbl_rank)
#   query_rsf_pfcbl_ids <- rsf_family[pfcbl_category_rank <= max_indicator_pfcbl_rank,rsf_pfcbl_id]
#   
#   if (!any(rsf_indicators$indicator_pfcbl_rank==0) &
#       any(query_rsf_pfcbl_ids==0)) query_rsf_pfcbl_ids <- query_rsf_pfcbl_ids[-which(query_rsf_pfcbl_ids==0)]
#   
#   #In case we're asked to lookup indicators program isn't monitoring
#   #query_indicator_ids <- rsf_indicators$indicator_id
#   
#   #display_timeline <- 
#   history_selection <- as.logical(DASH_DATA_OPTIONS$display_timeline)
#   if (is.null(history_selection) || is.na(history_selection)) history_selection <- FALSE
#   
#   timeEstimate <- length(query_rsf_pfcbl_ids) * length(rsf_indicators$indicator_id)
#   
#   if (history_selection==TRUE) {
#     timeEstimate <- timeEstimate * length(all_dates <= ymd(reporting_date))
#   }
#   
#   print(paste0("DASH_DATA lookup ids: ",length(query_rsf_pfcbl_ids)))
#   print(paste0("DASH_DATA lookup indicators: ",length(rsf_indicators$indicator_id)))
#   print(paste0("DASH_DATA lookup dates: ",length(all_dates <= ymd(reporting_date))))
#   
#   progress <- NULL
#   if (timeEstimate > 50000) {
#     progress <- shiny::Progress$new()
#     on.exit(progress$close())
#     progress$set(message = paste0("Querying ",timeEstimate," data points.  Please wait ..."),
#                  value = 0)
#   }
#    
#   rsf_data <- DBPOOL %>% dbGetQuery("
#                                     select
#                                     	rpd.valid_reporting_date as reporting_asof_date, 
#                                     	cd.reporting_asof_date as data_asof_date, -- origin: actual data reported date
#                                     	ids.rsf_pfcbl_id,
#                                     	cd.data_id,                 
#                                     	ind.indicator_id,
#                                     	cd.data_value,
#                                     	cd.data_unit,
#                                     	cd.data_value_updated,
#                                     	array_to_string(checks.evaluation_ids,',') as evaluation_ids,
#                                     	false as is_intraperiod
#                                     from p_rsf.rsf_pfcbl_ids ids
#                                     inner join p_rsf.rsf_program_reporting_dates rpd on rpd.rsf_program_id = ids.rsf_program_id
#                                     inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
#                                     left join lateral (select
#                                     									    rdc.data_id,
#                                     											rdc.data_value,
#                                     											rdc.data_unit,
#                                     											rdc.reporting_asof_date,
#                                     											rdc.reporting_asof_date = rpd.valid_reporting_date as data_value_updated
#                                     										from p_rsf.rsf_data_current rdc
#                                     										where rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id
#                                     										  and rdc.indicator_id = ind.indicator_id
#                                     											and rdc.reporting_asof_date <= rpd.valid_reporting_date
#                                     									  order by rdc.reporting_asof_date desc
#                                     										limit 1) cd on true
#                                     left join lateral(select array_agg(evaluation_id)::int[] as evaluation_ids
#                                     									from p_rsf.rsf_data_checks checks
#                                     									where checks.data_id = cd.data_id
#                                     										and checks.check_status = 'active'
#                                     										and checks.check_asof_date = rpd.valid_reporting_date
#                                     								 ) as checks on true										
#                                     where rpd.valid_reporting_date >= (select created_in_reporting_asof_date
#                                     																	from p_rsf.rsf_pfcbl_ids
#                                     																	where rsf_program_id = $1::int
#                                     																		and pfcbl_category in ('global','program'))
#                                     	and ids.rsf_pfcbl_id = any(select unnest(string_to_array($2::text,','))::int) -- much faster than a join or in()
#                                     	and ind.indicator_id = any(select unnest(string_to_array($3::text,','))::int)
#                                     	and case 
#                                     				when $5::bool = false then rpd.valid_reporting_date = $4::date -- current data
#                                     				else rpd.valid_reporting_date <= $4::date                 -- full change history
#                                     			end
#                                     ",
#                                     params=list(program$rsf_program_id,
#                                                 paste0(query_rsf_pfcbl_ids,collapse=","),
#                                                 paste0(query_indicator_ids,collapse=","),
#                                                 reporting_date,
#                                                 history_selection))
#   
#   setDT(rsf_data)
#   
#   rsf_data[rsf_family,
#            `:=`(status=i.status,
#                 exists=i.exists),
#            on=.(rsf_pfcbl_id,
#                 reporting_asof_date)]
#   
#   setorder(rsf_data,
#            rsf_pfcbl_id,
#            indicator_id,
#            -reporting_asof_date,
#            is_intraperiod, #FALSE is first
#            -data_id) #Most recent inserts first
#   
#   rsf_data[,reporting_intraperiod_rank:=as.numeric(NA)]
#   
#   rsf_data[is_intraperiod==FALSE,
#            reporting_intraperiod_rank:=0]
#   
#   rsf_data[is_intraperiod==TRUE,
#            reporting_intraperiod_rank:=1:.N, #Because any intraperiod report will have end-of-period reporting, whose rank is zero
#            by=.(rsf_pfcbl_id,
#                 reporting_asof_date,
#                 indicator_id)]
#   
#   if (!empty(setdiff(unique(rsf_data[,.(rsf_pfcbl_id,reporting_asof_date)]),unique(rsf_family[,.(rsf_pfcbl_id,reporting_asof_date)])))) {
#     stop("Mismatched rsf_data and rsf_family")
#   }
#   
#   
#   
#   rsf_data[rsf_indicators,
#            `:=`(indicator_name=i.indicator_name,
#                 data_type=i.data_type),
#            on=.(indicator_id)]
#   
#   
#   evaluation_ids <- rsf_data[!is.na(evaluation_ids),paste0(evaluation_ids,collapse=",")]
#   rsf_data[,flags_exist:=!is.na(evaluation_ids)]
#   rsf_data[,evaluation_ids:=NULL]
#   
#   if (!is.null(progress)) {
#     progress$inc(.50, detail = paste("Looking up flags for requested data..."))
#   }
#   
#   data_flags <- DBPOOL %>% dbGetQuery("
#                                       select 
#                                         rdc.evaluation_id,
#                                         rdc.data_id,
#                                         rdc.check_asof_date,
#                                         rdc.indicator_check_id,
#                                         ic.check_name,
#                                         rdc.check_message,
#                                         rdc.check_status,
#                                         coalesce(icg.overwrite_check_class,ic.check_class) as check_class,
#                                         icg.overwrite_check_class is not null as check_class_overwrite
#                                       from p_rsf.rsf_data_checks rdc
#                                       inner join p_rsf.indicator_checks ic on ic.indicator_check_id = rdc.indicator_check_id
#                                       left join p_rsf.indicator_check_guidance icg on icg.indicator_check_guidance_id = rdc.indicator_check_guidance_id
#                                       where rdc.evaluation_id = any(select unnest(string_to_array($1::text,','))::int)",
#                                       params=list(evaluation_ids))
# 
#   setDT(data_flags)
#   
#   rsf_data[,
#            `:=`(flagstext=as.character(NA),
#                 flagshtml="",
#                 flagscount=0,
#                 flagsclasses=list())]
#   
#   #FLAGS FORMATTING
#   if (!empty(data_flags)) {
#     data_flags[,flag_priority:=fcase(check_class=="critical",1,
#                                     check_class=="error",   2,
#                                     check_class=="warning", 3,
#                                     check_class=="info",    4,
#                                     default=NA)]
#     setorder(data_flags,
#              data_id,
#              check_asof_date,
#              flag_priority)
#     
#     data_flags[,`:=`(checks=paste0(toupper(check_class),
#                                       ": ",check_name,
#                                       ifelse(nchar(check_message)>0,
#                                              paste0("/ ",check_message),
#                                              "")),
#                      check_html="",
#                      check_count=0)]
#     
#     data_flags <- data_flags[,check_html:=mapply(format_html_check_icon,
#                                                  check_class=check_class,
#                                                  title=check_message,
#                                                  css_class="flag-dashboard")]
#     
#     
#     data_flags <- data_flags[,.(checks=paste0(checks,collapse=" | "),
#                                 check_html=paste0(check_html,collapse=" 
#                                                 "),
#                                 check_count=.N),
#                          by=.(data_id,
#                               check_asof_date,
#                               check_class)]
#       
#       
#     data_flags <- data_flags[,
#                          .(check_html=paste0("<div style='float:left;display:inline-block;'>",
#                                              paste0(check_html,collapse=" "),
#                                              "</div>"),
#                            checks=paste0(checks,collapse="\\n"),
#                            check_count=sum(check_count),
#                            check_classes=list(unique(check_class))),
#                          by=.(data_id,
#                               check_asof_date)]
#     #browser()
#     if (history_selection==TRUE) {
#       rsf_data[data_flags,
#                `:=`(flagstext=i.checks,
#                     flagshtml=i.check_html,
#                     flagscount=i.check_count,
#                     flagsclasses=i.check_classes),
#                 on=.(data_id,
#                      reporting_asof_date=check_asof_date)]
#     } else {
#       rsf_data[data_flags,
#                `:=`(flagstext=i.checks,
#                     flagshtml=i.check_html,
#                     flagscount=i.check_count,
#                     flagsclasses=i.check_classes),
#                on=.(data_id,
#                     reporting_asof_date=check_asof_date)] #Yes, but only when we're not reporting historical timeseries
#     }    
#   }
#   
#   #if set to FALSE (don't display flags) will reset flag html/text values to NA and ""
#   #done like this to retain counts and flagsclasses so that filters on flags will still work (even if the flags aren't rendered)
#   display_flags <- as.logical(DASH_DATA_OPTIONS$display_flags)
#   if (any(display_flags==FALSE)) {
#     rsf_data[,
#              `:=`(flagstext=as.character(NA),
#                   flagshtml="",
#                   flagsclasses=list())]
#   }
#   
#   data_flags <- NULL
#   
#   display_currency <- toupper(DASH_DATA_OPTIONS$display_currency)
#   if (!isTruthy(display_currency) || display_currency == "LCU") display_currency <- as.character(NA)
#   
#   if (!is.na(display_currency) &&
#       any(rsf_data$data_type=="currency")) {
#   
#     currency_data <- rsf_data[data_type=="currency"
#                               & data_unit != display_currency, #no need to convert into its native unit; should also eliminate
#                                                                #all indicators that define a non-LCU currency, too.
#                               .(reporting_asof_date,
#                                 indicator_id,
#                                 data_id,
#                                 data_value=as.numeric(data_value),
#                                 from_currency=data_unit)]
#     
#     currency_indicators <- rsf_indicators[unique(currency_data[,.(indicator_id)]),
#                                           .(indicator_id,default_data_unit=data_unit,indicator_name),
#                                           on=.(indicator_id),
#                                           nomatch=NULL]
#     
#     currency_indicators[,indicator_name:=paste0(indicator_name,"@",display_currency)]
#     
#     
#     currency_data[currency_indicators,
#                   indicator_name:=i.indicator_name,
#                   on=.(indicator_id)]
#     
#     
#     if (as.Date(reporting_date) >= (today()-1)) { #-1 because fx data may not be posted yet depending on the time of day "today"
#       showNotification(type="warning",
#                        ui=h3(paste0("Warning: the selected reporting date ",
#                                     as.character(reporting_date),
#                                     " is in the future! FX data does not exist for future dates.  
#                                     FX conversions will be done as of ",as.character(as.Date(today()-1)))))
#     }
#     
#     currency_data[,fx_date:=reporting_asof_date]
#     currency_data[reporting_asof_date > today()-1,
#                   fx_date:=today()-1]
#     
#     fx_data <- unique(currency_data[,.(reporting_asof_date,
#                                        fx_date,
#                                        from_currency)])[,.(rsf_pfcbl_id=0,
#                                                            currency_ratio=paste0(display_currency,"/",from_currency),
#                                                            exchange_rate_date=fx_date)]
#     
#     fx_table <- DBPOOL %>% db_data_get_fx_ratio(fx_table=fx_data,
#                                                 create.indicators = FALSE) #If this is an ad-hoc request for random fx view, don't create/store data
#     
#     currency_data <- currency_data[fx_table,
#                                    .(data_id,
#                                      reporting_asof_date,
#                                      indicator_name,
#                                      data_value=data_value / exchange_rate,
#                                      data_unit=to_currency),
#                                    on=.(fx_date=exchange_rate_date,
#                                         from_currency)]
#     
#     
#     rsf_data[currency_data,
#              `:=`(indicator_name=i.indicator_name,
#                   data_value=as.character(i.data_value),
#                   data_unit=i.data_unit),
#              on=.(data_id,
#                   reporting_asof_date)]
#     
#     currency_indicators <- NULL
#     currency_data <- NULL
#     fx_table <- NULL
#     fx_data <- NULL
#   }
#   
#   
#   if (history_selection==TRUE) {
#     current_rsf_data <- rsf_data[reporting_asof_date==reporting_date & is_intraperiod==FALSE]
#     
#     current_rsf_data[,reporting_history:=FALSE]
#     rsf_data[,reporting_history:=TRUE]
#     
#     rsf_data <- rbindlist(list(current_rsf_data,
#                                rsf_data))
#   } else {
#     rsf_data[,reporting_history:=FALSE]
#   }
#   
#   if (!is.null(progress)) {
#     progress$inc(.85, detail = paste0("Formatting for HTML..."))
#   }
#   
#   #FORMATTING 
#   {
#       
#     rsf_data[,`:=`(text=data_value)]
#     
#     rsf_data[!is.na(text) & data_type %in% c("number","percent"),
#              text:=as.character(
#                format(round(as.numeric(text),3),big.mark = ",",scientific=F))]
#     
#     rsf_data[!is.na(text) & data_type=="currency",
#              text:=as.character(
#                format(
#                  round(as.numeric(text),2),
#                  big.mark = ",",scientific = F))]
#     
#     rsf_data[,edit:=ifelse(is.na(data_value_updated) | data_value_updated==TRUE | reporting_history==FALSE,
#                            text,
#                            as.character(NA))]
#     
#                 
#     #remove currency codes from currency values
#     rsf_data[!is.na(text) & !is.na(data_unit),
#              text:=paste(text,data_unit)]
#     
#     rsf_data[,html:=text]
#     
#     
#     #dt_long[is.na(changed),changed:=exists]
#     
#     { #BLANKS
#       #If its an intentional change then keep default formating (ie, black) else gray-out data that's simpy never been entered.
#       #If it changed to NA then it must have changed from something to nothing; ie, it's been deleted
#       
#       rsf_data[is.na(data_value) & 
#                #exists == TRUE & 
#                #reporting_intraperiod_rank==0 & 
#                (is.na(data_value_updated) | data_value_updated ==FALSE) & 
#                flagscount > 0,
#               `:=`(html="<div style='color:rgba(155,100,50,0.8);'>{MISSING}</div>",
#                    text="{MISSING}")]
#       
#       rsf_data[is.na(data_value) & 
#                #exists == TRUE & 
#                #reporting_intraperiod_rank==0 & 
#                (is.na(data_value_updated) | data_value_updated==FALSE) &  
#                flagscount == 0,
#               `:=`(html="<div style='color:rgba(220,220,220,0.8);'>{BLANK}</div>",
#                    text="{BLANK}")]
#       
#       rsf_data[is.na(data_value) & 
#                data_value_updated==TRUE, #means that I updated it to be blank! (ie, whatever it was before wasn't blank...so deleted)
#               `:=`(html="{DELETED}",
#                    text="{DELETED}")]
# 
#       rsf_data[is.na(data_value),
#                `:=`(html="<div style='color:rgba(220,220,220,0.8);'>{NOTHING CHANGED}</div>",
#                     text="{NOTHING CHANGED}")]
#       
#       #This only makes sense in context of wide data      
#       # rsf_data[is.na(data_value) & 
#       #          exists == TRUE & 
#       #          reporting_intraperiod_rank >0,
#       #         `:=`(html="<div style='color:rgba(220,220,220,0.8);'>{INTRAPERIOD}</div>",
#       #              text="{INTRAPERIOD}")] 
#     }
#     
#     rsf_data[reporting_history==TRUE &
#              exists==TRUE            &  #means entity reported and presumably deliberately reported no change here
#              data_value_updated==FALSE, #FALSE means not NA means not first entry
#             `:=`(html=fcase(status=="ACTIVE","<div style='color:rgba(145,15,60,0.8);font-size:1.3em;' title='Current value reported in the past.'>&uarr;</div>",
#                             status=="INACTIVE","<div style='color:rgba(255,75,60,0.9);font-weight:bold;font-size:1.3em;' title='Retroactive reporting in another column.  This value was reported in the past.'>&uarr;</div>",
#                             #status=="PREACTIVE","<div style='color:rgba(50,50,150,0.8);font-size:1.3em;' title='Preactive reporting in another column before required reporting date.  This value was reported in the past.'>&uarr;</div>",
#                             status=="PREACTIVE","<div style='color:rgba(50,150,50,0.9);font-weight:bold;font-size:1.3em;' title='Preactive reporting in another column before required reporting date.  This value was reported in the past.'>&uarr;</div>",
#                             default="<div style='color:rgba(145,15,60,0.8);font-size:1.3em;' title='Current value reported in the past.'>&uarr;</div>"),
#                  text=as.character(intToUtf8(0x2191)))]
#     
#     #MISSING only when
#     #when cs.system_reporting_period_exists = false and act.quarter_reporting_expected = true
#     # rsf_data[reporting_history==TRUE & 
#     #          exists==FALSE &  
#     #          status=="MISSING",
#     #         `:=`(html="<div style='color:darkred;' title='No data reported'>{MISSING}</div>",
#     #              text="{MISSING}")]
#     
#     #INACTIVE for this case (because status == INACTIVE) and exists==FALSE
#     #when cs.system_reporting_period_exists = false and td.timeseries_end_date < cs.reporting_current_quarter_begin_date
#     rsf_data[reporting_history==TRUE &
#              exists==FALSE &  
#              status=="INACTIVE", 
#              `:=`(html="<div style='color:rgba(9,9,9,.5);' title='Outside reporting period'>{INACTIVE}</div>",
#                   text="{INACTIVE}")]
#     
#   }
#   
#   rsf_data[,html:=paste0("<div>",
#                         flagshtml,
#                         "<div style='float:left;display:inline-block;'>",html,"</div>",
#                         "</div>")]
#   
#   rsf_data[,flagshtml:=NULL]
#   
#   # dt_long[,html:=paste0("<div class='cell-dashboard' onmousedown='event.stopPropagation();' ondblclick=alert('clicked');>",
#   #                                 current.flagshtml,
#   #                                 "<div style='float:left;display:inline-block;'>",current.html,"</div>
#   #                                     </div>")]
#   rsf_data <- rsf_data[,.(reporting_asof_date,
#                           reporting_history,
#                           reporting_intraperiod_rank,
#                           rsf_pfcbl_id,
#                           data_type,
#                           indicator_name, #NOTE: currency conversions will change the name from the standard rsf_indicator names
#                           indicator_id,
#                           data_id,
#                           data_asof_date,
#                           data_value_updated,
#                           data_value,
#                           text,
#                           edit,
#                           html,
#                           flagscount,
#                           flagstext,
#                           flagsclasses)]
#   
# # rsf_data <<- as.data.frame(rsf_data)  
# # clients_list <<- as.data.frame(clients_list)
# # program <<- program
# # clients<<-clients
# # reporting_date <<- reporting_date
# # available_indicators <<- as.data.frame(available_indicators)
# # history_selection <<- history_selection
# #history_selection <- 2
# # browser()
# # 
# # clients_list <- as.data.table(clients_list)
# # available_indicators <- as.data.table(available_indicators)
#   print(paste0("DASH_DATA COMPLETE: ",nrow(rsf_data)," rows in ",round(as.numeric(Sys.time()-t1,"secs"),2),"s"))
#   if (!is.null(progress)) {
#     progress$inc(1, detail = paste0("Completed data lookup in ",round(as.numeric(Sys.time()-t1,"secs"),2),"s"))
#   }
#   
#   return (rsf_data)
# }, ignoreNULL = FALSE) #%>% debounce(500)
# 
# 
# DASH_DATA_COLUMNS <- eventReactive(c(DASH_DATA(),
#                                      DASH_DATA_SELECTED_INDICATORS_READ()), 
# {
#   rsf_data <- DASH_DATA()
#   if(empty(rsf_data)) return (NULL)
# 
#   rsf_family <- DASH_RSF_FAMILY()
#   if (empty(rsf_family)) return (NULL)
#   
#   print(paste0("DASH_DATA_COLUMNS ",Sys.time()))                                       
#                                        #DASH_DATA_QUERY_INDICATORS_READ()
#   selected_indicator_ids <- DASH_DATA_SELECTED_INDICATORS_READ()
#   if (!isTruthy(selected_indicator_ids)) return (NULL)
#   if (!all(DASH_DATA_SELECTED_INDICATORS_READ() %in% DASH_DATA_QUERY_INDICATORS_READ())) {
#     print(paste0("WARNING: Selection/Timing read/query indicator mismatch: ",
#                  paste0(setdiff(DASH_DATA_SELECTED_INDICATORS_READ(),DASH_DATA_QUERY_INDICATORS_READ()),collapse=",")))
#     return(NULL)
#   }
#   
#   
#   
#   program_indicators <- DASH_INDICATORS()
# 
#   #display_timeline <- 
#   history_selection <- as.logical(DASH_DATA_OPTIONS$display_timeline)
#   if (is.null(history_selection) || is.na(history_selection)) history_selection <- FALSE
#  
#   
#   selected_indicators <- program_indicators[indicator_id %in% selected_indicator_ids,
#                                             .(indicator_id,
#                                               indicator_pfcbl_rank,
#                                               indicator_name,
#                                               data_category)]
# 
#   rsf_data <- rsf_data[selected_indicators[,.(indicator_id)],
#                        on=.(indicator_id),
#                        nomatch=NULL]
#   
#   rsf_family <- rsf_family[pfcbl_category_rank <= max(selected_indicators$indicator_pfcbl_rank)]
#   
# 
#   #pfcbl_family
#   {   
#     loans <- unique(rsf_family[pfcbl_category=="loan",.(rsf_pfcbl_id.loan=rsf_pfcbl_id,rsf_borrower_id,rsf_loan_id)])
#     borrowers <- unique(rsf_family[pfcbl_category=="borrower",.(rsf_pfcbl_id.borrower=rsf_pfcbl_id,rsf_client_id,rsf_borrower_id)])
#     clients <- unique(rsf_family[pfcbl_category=="client",.(rsf_pfcbl_id.client=rsf_pfcbl_id,rsf_facility_id,rsf_client_id)])
#     facilities <- unique(rsf_family[pfcbl_category=="facility",.(rsf_pfcbl_id.facility=rsf_pfcbl_id,rsf_program_id,rsf_facility_id)])
#     programs <- unique(rsf_family[pfcbl_category == "program",.(rsf_pfcbl_id.program=rsf_pfcbl_id,rsf_program_id)])
#     
#     
#     pfcbl_family <- loans[borrowers,
#                           on=.(rsf_borrower_id)
#                           ][clients,
#                             on=.(rsf_client_id)
#                           ][facilities,
#                             on=.(rsf_facility_id)
#                           ][programs,
#                             on=.(rsf_program_id)]
#     
#     
#     if (unique(program_indicators$rsf_program_id)==0) {
#       
#       pfcbl_family <- pfcbl_ids[pfcbl_category == "global",.(rsf_pfcbl_id.global=as.integer(rsf_pfcbl_id),
#                                                              rsf_program_id=as.integer(0))]
#       pfcbl_family[,`:=`(rsf_pfcbl_id.program=as.integer(NA),
#                          rsf_pfcbl_id.facility=as.integer(NA),
#                          rsf_pfcbl_id.client=as.integer(NA),
#                          rsf_pfcbl_id.borrower=as.integer(NA),
#                          rsf_pfcbl_id.loan=as.integer(NA),
#                          rsf_facility_id=as.integer(NA),
#                          rsf_client_id=as.integer(NA),
#                          rsf_borrower_id=as.integer(NA),
#                          rsf_loan_id=as.integer(NA))]
#     } else {
#       pfcbl_family[,rsf_pfcbl_id.global:=as.integer(0)]
#     }
#     
#     setcolorder(pfcbl_family,
#                 neworder=c("rsf_pfcbl_id.global",
#                            "rsf_pfcbl_id.program",
#                            "rsf_pfcbl_id.facility",
#                            "rsf_pfcbl_id.client",
#                            "rsf_pfcbl_id.borrower",
#                            "rsf_pfcbl_id.loan",
#                            "rsf_program_id",
#                            "rsf_facility_id",
#                            "rsf_client_id",
#                            "rsf_borrower_id",
#                            "rsf_loan_id"))
#     setorder(pfcbl_family,
#              rsf_program_id,
#              rsf_facility_id,
#              rsf_client_id,
#              rsf_borrower_id,
#              rsf_loan_id)
#     
#     pfcbl_family[,reporting_group:=.GRP,
#                  by=.(rsf_program_id,
#                       rsf_facility_id,
#                       rsf_client_id,
#                       rsf_borrower_id,
#                       rsf_loan_id)]
#   }  
#   
#   #pfcbl_dates
#   {
#     pfcbl_dates <- melt.data.table(pfcbl_family,
#                                    id.vars = "reporting_group",
#                                    measure.vars = c("rsf_pfcbl_id.global",
#                                                     "rsf_pfcbl_id.program",
#                                                     "rsf_pfcbl_id.facility",
#                                                     "rsf_pfcbl_id.client",
#                                                     "rsf_pfcbl_id.borrower",
#                                                     "rsf_pfcbl_id.loan"),
#                                    variable.name = "pfcbl_category",
#                                    value.name = "rsf_pfcbl_id")
#     
#     pfcbl_dates[,pfcbl_category:=gsub("^rsf_pfcbl_id\\.(.*)$","\\1",pfcbl_category)]
#     
#     #filter for selected indicator categories only
#     pfcbl_dates <- pfcbl_dates[unique(selected_indicators[,.(pfcbl_category=data_category)]),
#                                on=.(pfcbl_category),
#                                nomatch=NULL]
#     
#     pfcbl_dates[,pfcbl_category:=NULL]
#     
#     pfcbl_dates <- pfcbl_dates[rsf_family[,
#                                           .(rsf_pfcbl_id,
#                                             pfcbl_category,
#                                             pfcbl_category_rank,
#                                             reporting_asof_date,
#                                             exists,
#                                             status,
#                                             start_date,
#                                             currentest_date)],
#                                .(reporting_group,
#                                  pfcbl_category,
#                                  pfcbl_category_rank,
#                                  reporting_asof_date,
#                                  exists,
#                                  status,
#                                  start_date,
#                                  currentest_date),
#                                on=.(rsf_pfcbl_id),
#                                by=.EACHI,
#                                nomatch=NULL] #nomatch=NA where parents have reporting dates preceeding the filtered entities' start dates (these would be truncated eventually anyway)
#     
#     setorder(pfcbl_dates,
#              reporting_group,
#              rsf_pfcbl_id,
#              reporting_asof_date)
#     
#     #childest rsf_pfcbl_id that will be present in this reporting group to represent the 1:1 parents of the childs.
#     #Should any reporting dates be before the child start date, this will need to be interpreted on read-in, if users supply data updates for non-eligible dates
#     SYSIDs <- unique(pfcbl_dates[pfcbl_category_rank==max(pfcbl_category_rank),
#                                  .(reporting_SYSID=rsf_pfcbl_id,
#                                    pfcbl_category_rank),
#                                  by=.(reporting_group)])
#     
#     pfcbl_dates[SYSIDs,
#                 reporting_SYSID:=i.reporting_SYSID,
#                 on=.(reporting_group)]
#     
#     
#     SYSStatus <- unique(pfcbl_dates[rsf_pfcbl_id==reporting_SYSID,
#                                     .(reporting_SYSID,
#                                       reporting_EXISTS=exists,
#                                       reporting_STATUS=paste0(pfcbl_category,":",status),
#                                       reporting_asof_date)])
#     pfcbl_dates[SYSStatus,
#                 `:=`(reporting_EXISTS=i.reporting_EXISTS,
#                      reporting_STATUS=i.reporting_STATUS),
#                 on=.(reporting_SYSID,
#                      reporting_asof_date)]
#     
#     
#     SYSStatus <- NULL
#     SYSIDs <- NULL
#     pfcbl_dates[,`:=`(exists=NULL,
#                       status=NULL)]
#     
#     pfcbl_dates[,
#                 `:=`(reporting_group_start_date=max(start_date),
#                      reporting_group_currentest_date=min(currentest_date)),
#                 by=.(reporting_group)]
#     
#     pfcbl_dates[,
#                 `:=`(truncate_start=reporting_asof_date < reporting_group_start_date,
#                      truncate_current=reporting_asof_date > reporting_group_currentest_date,
#                      start_date=NULL,
#                      currentest_date=NULL)]
#     
#     pfcbl_dates[,
#                 `:=`(reporting_group_start_date=NULL,
#                      reporting_group_currentest_date=NULL)]
#     
#     pfcbl_dates[,
#                 `:=`(truncate_previous=shift(truncate_start,1,"lag",fill=FALSE)==TRUE & truncate_start==FALSE,
#                      truncate_next=shift(truncate_current,1,"lead",fill=FALSE)==TRUE & truncate_current==FALSE),
#                 by=.(reporting_group,
#                      rsf_pfcbl_id)]
#     
#     if (history_selection==TRUE) {
#       current_pfcbl_dates <- pfcbl_dates[reporting_asof_date==unique(rsf_family$reporting_current_date)]
#       current_pfcbl_dates[,reporting_history:=FALSE]
#       pfcbl_dates[,reporting_history:=TRUE]
#       pfcbl_dates <- rbindlist(list(current_pfcbl_dates,
#                                     pfcbl_dates))
#       current_pfcbl_dates <- NULL
#       
#     } else {
#       pfcbl_dates[,reporting_history:=FALSE]
#     }
#   }
#   #browser()
#   #truncates
#   #pfcbl_dates <- as.data.table(pdates)
#   #input <- list(); input$rsf_dash_reporting_asof_date <- asof_date
#   {
#     selected_asof_date <- ymd(input$rsfdash_reporting_asof_date)
#     action_truncate_start <- DASH_DATA_OPTIONS$truncate_previous
#     action_truncate_current <- DASH_DATA_OPTIONS$truncate_next
#     
#     if (action_truncate_start==TRUE & history_selection==TRUE) {
#       pfcbl_dates <- pfcbl_dates[(reporting_asof_date == selected_asof_date) |
#                                  (truncate_start==FALSE & reporting_history==TRUE) | 
#                                  (reporting_history==FALSE)]
#     }
#     
#     if (action_truncate_current & history_selection==TRUE) {
#       pfcbl_dates <- pfcbl_dates[(reporting_asof_date == selected_asof_date) |
#                                  (truncate_current==FALSE & reporting_history==TRUE) | 
#                                  (reporting_history==FALSE)]
#     }
# 
#     pfcbl_dates[,
#                 `:=`(truncate_start=NULL,
#                      truncate_current=NULL,
#                      pfcbl_category_rank=NULL)]
#   }  
#   
#   #browser()
#   #intraperiod ranks
#   {
#     IRANKS <- rsf_data[,
#                        .(reporting_intraperiods=max(reporting_intraperiod_rank)),
#                        by=.(rsf_pfcbl_id,
#                             reporting_asof_date,
#                             reporting_history)
#                        ][pfcbl_dates[,.(rsf_pfcbl_id,reporting_group,reporting_asof_date,reporting_history)],
#                          on=.(reporting_history,
#                               rsf_pfcbl_id,
#                               reporting_asof_date),
#                          nomatch=NULL
#                          ][,
#                            .(reporting_intraperiod_rank=0:max(reporting_intraperiods)),
#                            by=.(reporting_history,
#                                 reporting_group,
#                                 reporting_asof_date)]
#     
#     pfcbl_dates <- pfcbl_dates[IRANKS,
#                                on=.(reporting_history,
#                                     reporting_group,
#                                     reporting_asof_date)]
#     
#     IRANKS <- NULL
#   }
#   
#   setorder(pfcbl_dates,
#            reporting_group,
#            reporting_history,
#            -reporting_asof_date,
#            reporting_intraperiod_rank)
#   
#   pfcbl_dates[,
#               reporting_rowid:=.GRP,
#               by=.(reporting_group,
#                    reporting_history,
#                    reporting_asof_date,
#                    reporting_intraperiod_rank)]
#   
#   
#  
#   pfcbl_dates[reporting_intraperiod_rank > 0,
#               reporting_STATUS:=paste0(reporting_STATUS,"/INTRAPERIOD")]
#   
#   #for keyword searching  
#   {
#     tags_sysids <- unique(rsf_data[,.(rsf_pfcbl_id)])
#     tags_sysids[,tag:="sysid"]
#     tags_sysids[,keyword:=as.character(rsf_pfcbl_id)]
#     tags_sysids[,data_type:="number"]
#     
#     tags_dt <- program_indicators[indicator_sys_category %in% c("id","name","sysid","rank_id","nickname"),
#                                   .(indicator_id,
#                                     tag=indicator_sys_category)]
#     
#     tags_dt[tag=="rank_id",tag:="rank"]
#     tags_dt[tag=="nickname",tag:="name"]
#     
#     
#     dt_kw <- rsf_data[!is.na(data_value),
#                       .(rsf_pfcbl_id,
#                         data_type,
#                         indicator_id,
#                         text)]
#     
#     dt_kw <- unique(dt_kw)
#     
#     dt_kw[tags_dt,
#           tag:=i.tag,
#           on=.(indicator_id)]
#     dt_kw[,indicator_id:=NULL]
#     tags_dt <- NULL
#     
#     dt_kw[data_type %in% c("number","currency","percent"),
#           text:=gsub(",","",text)]
#     
#     set(dt_kw,i=NULL,j="keywords",value=str_split(dt_kw[["text"]],pattern="[[:space:][:punct:]]+"))
#     
#     dt_kw <- dt_kw[,.(keyword=tolower(unlist(keywords,recursive = FALSE))),
#                    by=.(rsf_pfcbl_id,tag,data_type)]
#     
#     dt_kw <- dt_kw[is.na(keyword)==FALSE & 
#                      nchar(keyword) > 0 &
#                      (nchar(keyword) >= 3 |
#                         data_type %in% c("number","currency","percent") |
#                         !is.na(tag))]
#     
#     dt_kw <- unique(dt_kw)
#     
#     #this function call seems to take a while...so minimize unique observations as much as possible first.
#     dt_kw[,keyword:=stringi::stri_trans_general(str = keyword,
#                                                 id = "Latin-ASCII")]
#     
#     
#     
#     setcolorder(tags_sysids,
#                 neworder=names(dt_kw))
#     
#     dt_kw <- rbindlist(list(dt_kw,
#                             tags_sysids))
#     tags_sysids <- NULL
#     
#     dt_kw <- unique(dt_kw)
#     
#     
#     dt_kw[unique(pfcbl_dates[,.(rsf_pfcbl_id,reporting_group)]),
#           reporting_group:=i.reporting_group,
#           on=.(rsf_pfcbl_id)]
#     dt_kw[,rsf_pfcbl_id:=NULL]
#     dt_kw <- unique(dt_kw)
#     
#     dt_kw <- dt_kw[,
#                    .(reporting_groups=list(c(unique(reporting_group)))),
#                    by=.(keyword,
#                         tag)]
#     
#     setkey(dt_kw,keyword)
#     DASH_DATA_KEYWORDS(dt_kw)
#     
#   } 
#   
#   #browser()
#   INDICATOR_ROWS <- pfcbl_dates[,
#                                 .(reporting_rowid,
#                                    data_category=pfcbl_category)
#                                 ][selected_indicators[,
#                                                       .(indicator_id,data_category)],
#                                   .(reporting_rowid,
#                                     indicator_id),
#                                   on=.(data_category),
#                                   by=.EACHI,
#                                   nomatch=NULL]
#   
#   pfcbl_dates <- pfcbl_dates[INDICATOR_ROWS,
#                              on=.(reporting_rowid,
#                                   pfcbl_category=data_category)]
#   INDICATOR_ROWS <- NULL
#   
#   pfcbl_data <- rsf_data[pfcbl_dates,
#                             
#                             on=.(reporting_history,
#                                  rsf_pfcbl_id,
#                                  indicator_id,
#                                  reporting_asof_date,
#                                  reporting_intraperiod_rank),
#                             nomatch=NA]
#   
#   if (nrow(pfcbl_dates) != nrow(pfcbl_data)) {
#     stop(paste0("Row mismatch: pfcbl_dates=",nrow(pfcbl_dates)," pfcbl_data=",nrow(pfcbl_data)))
#   }
#   
#   action_truncate_unchanges <- DASH_DATA_OPTIONS$truncate_unchanges
#   if (action_truncate_unchanges==TRUE & history_selection==TRUE) {
#     
#     data_changes <- pfcbl_data[reporting_asof_date != selected_asof_date &
#                                reporting_history==TRUE,
#                                .(reporting_rowid,
#                                  reporting_group,
#                                  data_value_updated)]
#     data_changes[is.na(data_value_updated),
#                  data_value_updated:=TRUE]
#     
#     data_changes <- data_changes[,
#                                  .(data_row_unchanged=all(data_value_updated==FALSE)),
#                                  by=.(reporting_rowid,
#                                       reporting_group)]
#     data_changes[,truncate_unchanged:=data_row_unchanged==TRUE & shift(data_row_unchanged,1,"lead",fill=FALSE)==TRUE,
#                  by=.(reporting_group)]
#     
#     data_changes <- data_changes[truncate_unchanged==TRUE]
#     pfcbl_data[,truncate_unchanged:=FALSE]
#     pfcbl_data[data_changes,
#                truncate_unchanged:=i.truncate_unchanged,
#                on=.(reporting_rowid,
#                     reporting_group)]
#     pfcbl_data <- pfcbl_data[truncate_unchanged==FALSE]
#     pfcbl_data[,truncate_unchanged:=NULL]
#     
#     #Reset rowid to ensure its sequential after the truncation
#     pfcbl_data[,
#                reporting_rowid:=.GRP,
#                by=.(reporting_rowid)]
#   }
#   
#   pfcbl_data <- pfcbl_data[,.(reporting_group,
#                                reporting_rowid,
#                                reporting_history,
#                                reporting_asof_date,
#                                reporting_intraperiod_rank,
#                                reporting_SYSID,
#                                reporting_EXISTS,
#                                reporting_STATUS,
#                                truncate_previous,
#                                truncate_next,
#                                rsf_pfcbl_id,
#                                indicator_id,
#                                indicator_name,
#                                dataid=data_id,
#                                current=data_value,
#                                text,
#                                edit,
#                                html,
#                                flagstext,
#                                flagsclasses,
#                                data_value_updated)]
#   
#   
#   #would come in from the join where entity doesn't have indicator_name for that reporting_asof_date
#   #in practice, this should only come in via intraperiod data where the entity reports an intraperiod data update for one or few indicators
#   #and not for its entire set of current indicator values
#   if (anyNA(pfcbl_data$data_id)) {
#     pfcbl_data[is.na(data_id),
#              `:=`(html=fcase(reporting_intraperiod_rank > 0,"<div style='color:rgba(50,50,255,0.9);font-weight:bold;font-size:1.3em;' title='Intraperiod reporting in another column. This value was reported in the past.'>&uarr;</div>",
#                              reporting_STATUS=="ACTIVE","<div style='color:rgba(145,15,60,0.8);font-size:1.3em;' title='Current value reported in the past.'>&uarr;</div>",
#                              reporting_STATUS=="INACTIVE","<div style='color:rgba(255,75,60,0.9);font-weight:bold;font-size:1.3em;' title='Retroactive reporting in another column.  This value was reported in the past.'>&uarr;</div>",
#                              reporting_STATUS=="PREACTIVE","<div style='color:rgba(50,150,50,0.9);font-weight:bold;font-size:1.3em;' title='Preactive reporting in another column before required reporting date.  This value was reported in the past.'>&uarr;</div>",
#                              default="<div style='color:rgba(145,15,60,0.8);font-size:1.3em;' title='Current value reported in the past.'>&uarr;</div>"),
#                   text=as.character(intToUtf8(0x2191)))]
#   }
#   
#   if (any(pfcbl_data$reporting_history==TRUE)) {
# 
#     #notruncate_stop_row means the row(s) BEFORE this row have been truncated
#     #so we don't want to show a "no change" error here, because we don't necessarily know what the originating value is (as it may have been truncated)
#     #however if it's been updated, its its own new value and not a no change value
#     pfcbl_data[data_value_updated==FALSE & reporting_history==TRUE & truncate_previous==TRUE,
#          `:=`(text=ifelse(is.na(current),'{MISSING}',current),
#               html=paste0("<div style='color:lightgray;display:inline-block;'><i>",ifelse(is.na(current),'{MISSING}',current),"</i></div>"))]
# 
#     #notruncate_start_row means the row(s) AFTER this row have been truncated
#     #in which case, we want to add a vertical elipses to indicate the break in the reporting timeline.
#     pfcbl_data[reporting_history==TRUE & truncate_next==TRUE,
#          `:=`(text=paste0(as.character(intToUtf8(0x22EE))," ",current),
#               html=paste0("<div style='color:rgba(145,15,60,0.8);font-size:1.3em;float:left;display:inline-block;' ",
#                           "title='Truncated timeline (full change history not shown)'>&#8942;</div>",
#                           html))]
#    
#   }
#   
#   pfcbl_data[,
#              `:=`(truncate_next=NULL,
#                   truncate_previous=NULL,
#                   data_value_updated=NULL)]
#   
#   {
#       sys_names <- unique(pfcbl_data[,.(reporting_SYSID)])[rsf_family[,.(reporting_SYSID=rsf_pfcbl_id,
#                                                                          sys_name,
#                                                                          rsf_full_name,
#                                                                          pfcbl_category)],
#                                                            on=.(reporting_SYSID),
#                                                            nomatch=NULL]
#     
#       sys_names[,rsf_full_name:=gsub("!|\\."," ",rsf_full_name)] #Because ! and . are used as control characters -- shouldn't happen, but not illegal either, eg a client name "Yup! Co."
#       sys_names[,rsf_full_name:=trimws(gsub("[[:space:]]+"," ",rsf_full_name))]
#       pfcbl_data[sys_names,
#                  `:=`(reporting_NAME=i.rsf_full_name,
#                       reporting_SYSNAME=i.sys_name),
#                    on=.(reporting_SYSID)]
#   }
#   
#   return (pfcbl_data)
#   
# }, ignoreNULL = FALSE) 
# 
# DASH_DATA_DISPLAY <- eventReactive(c(DASH_DATA_COLUMNS(),
#                                      input$rsfdash_reporting_column_priority,
#                                      DASH_DATA_OPTIONS$display_format,
#                                      DASH_DATA_OPTIONS$column_status,
#                                      DASH_DATA_OPTIONS$column_sysname,
#                                      DASH_DATA_OPTIONS$filter_status,
#                                      DASH_DATA_OPTIONS$filter_text,
#                                      DASH_DATA_OPTIONS$filter_indicator_types,
#                                      DASH_DATA_OPTIONS$filter_flags), {
# 
# 
# 
#                                        
#   col_data <- DASH_DATA_COLUMNS()
#   if (empty(col_data)) return (NULL)
# 
#   if (!isTruthy(DASH_DATA_CELL_INFO())) {
#     DASH_DATA_CELL_INFO("Click on a cell for more info")
#   }
#   
#   rsf_family <- DASH_RSF_FAMILY()
#   
#   priority_col_ids <- as.numeric(input$rsfdash_reporting_column_priority) #ordered as user added them and first added is first priority, rank 1
#   if (anyNA(priority_col_ids)) priority_col_ids <- priority_col_ids[-which(is.na(priority_col_ids))]
#   
#   #browser(); 
#   # dd_df <<- as.data.frame(col_data)
#   # priority_col_ids <<- priority_col_ids
#   # col_select <<- as.numeric(input$rsfdash_reporting_column_select)
#   # pii <<- as.data.frame(SELECTED_PROGRAM_INDICATORS_LIST())
#   
#   #means priority cols were added before they were selected
#   #adding a priority call will update the main select and in turn trigger which columns are selected
#   #if there is a set difference between these, then the main DASH_DATA_COLUMNS() is still querying/updating so don't both to re-render now
#   if (length(setdiff(priority_col_ids,as.numeric(input$rsfdash_reporting_column_select))) > 0) {
#     return (NULL)
#   }
#   
#   options_FORMAT <- DASH_DATA_OPTIONS$display_format #NOT reactive!
#   
#   if (!isTruthy(options_FORMAT) || !options_FORMAT %in% c("DEFAULT","LONG","INDICATORS","NAMES")) {
#     options_FORMAT <- "DEFAULT"  
#     DASH_DATA_OPTIONS$display_format <- options_FORMAT
#   }
#   
#   if (options_FORMAT=="DEFAULT") {
#     if (length(unique(col_data$reporting_rowid))==1) options_FORMAT <- "NAMES"
#     else if (rsf_family[rsf_pfcbl_id %in% unique(col_data$reporting_SYSID),min(pfcbl_category_rank)] <= 3) options_FORMAT <- "NAMES"
#     else options_FORMAT <- "INDICATORS"
#   }
#   
#   options_ACTIVE_STATUS <- toupper(DASH_DATA_OPTIONS$filter_status)
#   options_ACTIVE_STATUS <- options_ACTIVE_STATUS[options_ACTIVE_STATUS %in% c("ACTIVE","INACTIVE")]
#   if (isTruthy(options_ACTIVE_STATUS)) {
#     filter_status <- FALSE
#     if (any(options_ACTIVE_STATUS=="ACTIVE")) filter_status <- filter_status | 
#                                                                grepl(":ACTIVE$",col_data$reporting_STATUS,ignore.case = T) |
#                                                                col_data$reporting_EXISTS==TRUE #if inactive but reported something, include
#     
#     if (any(options_ACTIVE_STATUS=="INACTIVE")) filter_status <- filter_status | 
#                                                                  grepl(":INACTIVE$",col_data$reporting_STATUS,ignore.case = T) 
#     
#     col_data <- col_data[filter_status]
#   }
#   
#   indicator_types <- DASH_DATA_OPTIONS$filter_indicator_types
#   if (isTruthy(indicator_types)) {
#     rsf_indicators <- RSF_INDICATORS()
#     filter_types <- FALSE
#     
#     if (any(indicator_types=="SYSTEM")) filter_types <- filter_types | rsf_indicators$is_system==TRUE
#     if (any(indicator_types=="CALCULATED")) filter_types <- filter_types | rsf_indicators$is_calculated==TRUE
#     if (any(indicator_types=="USER")) filter_types <- filter_types | rsf_indicators$is_calculated==FALSE  | rsf_indicators$is_user_calculatable==TRUE
#     
#     rsf_indicator_ids <- rsf_indicators[filter_types,indicator_id]
#     col_data <- col_data[indicator_id %in% rsf_indicator_ids]
#   }
#   
#   keywords <- DASH_DATA_OPTIONS$filter_text #REACTIVE
#   
#   if (isTruthy(keywords) && nchar(keywords)>=3) {
#     dt_kw <- DASH_DATA_KEYWORDS()
#     
#     keywords <- trimws(keywords,whitespace="[ \\t\\r\\n\\v\\h\\s]")
#     
#     search_words <- gsub("([[:alpha:]]\\s*:)[[:space:]]+([^[:space:]])","\\1\\2",keywords)
#     search_words <- gsub("[[:space:]]*,[[:space:]]*","|",search_words)
#     search_words <- gsub("\\[|\\]|\\(|\\)"," ",search_words)
#     search_words <- unlist(str_split(search_words,pattern="[[:space:]]+"))
#     search_words <- search_words[nchar(search_words) > 0]
#     search_words <- tolower(search_words)
#     search_tag_ids <- grep("^(name|id|sysid|rank):.+$",search_words,ignore.case = TRUE)
#     
#     search_tags <- c()
#     if (length(search_tag_ids) > 0) {
#       search_tags <- search_words[search_tag_ids]
#       search_words <- search_words[-search_tag_ids]
#     }
#     
#     matched_reporting_groups <- list()
#     
#     if (length(search_tags)>0) {
#       
#       while (length(search_tags) > 0) {
#         tag <- search_tags[[1]]
#         search_tags <- search_tags[-1]
# 
#         st <- unlist(str_split(tag,":"))
#         st_tag <- st[[1]]
#         st_keywords <- st[[2]]
#         
#         if (st_tag %in% c("rank","sysid")) {
#           st_keywords <- unlist(str_split(st_keywords,pattern="\\|"))
#           st_keywords <- paste0("^",st_keywords,"$",collapse="|")
#           
#         } else if (st_tag == "id") {
#           st_keywords <- unlist(str_split(st_keywords,pattern="\\|"))
#           st_keywords <- paste0("^0*",st_keywords,collapse="|")
#         } 
#         
#         print(paste0("Filtering tag: ",paste0(st,collapse="=")))
#         matches <- unique(unlist(dt_kw[tag==st_tag & grepl(st_keywords,keyword,ignore.case=T),reporting_groups]))
#         
#         if (length(matches)==0 && 
#             st_tag=="id" && 
#             grepl("^\\d+$",st[[2]]) &&
#             grepl("rank:",keywords)==FALSE) search_tags <- c(search_tags,
#                                                              paste0("rank:",st[[2]]))
# 
#         if (length(matches)==0 && 
#             st_tag=="rank" && 
#             grepl("^\\d+$",st[[2]]) &&
#             grepl("sysid:",keywords)==FALSE) search_tags <- c(search_tags,
#                                                               paste0("sysid:",st[[2]]))
#         
#         matched_reporting_groups[[length(matched_reporting_groups)+1]] <- matches
#       }
#     }
#     
#     if (length(search_words)>0) {
#       #word <- search_words[[1]]
#       for (word in search_words) {
#         print(paste0("Filtering word: ",word))
#         matches <- unique(unlist(dt_kw[grepl(word,keyword,ignore.case=T),reporting_groups]))
#         matched_reporting_groups[[length(matched_reporting_groups)+1]] <- matches
#       }
#     }
#     
#     matches <- Reduce(intersect,matched_reporting_groups)
#     print(paste0("Rows matched: ",length(matches)))
#     if (length(matches)==0) return (NA)
#     else {
#       col_data <- col_data[reporting_group %in% matches]
#     }
#   }
#   #browser()
#   all_flags <- c("critical","error","warning","info")
#   filter_flags <- DASH_DATA_OPTIONS$filter_flags
#   if (isTruthy(filter_flags) && all(filter_flags=="all")) filter_flags <- all_flags
#   filter_flags <- intersect(filter_flags,all_flags)
#   
#   #filter_flags <- c("critical","error")
#   if (length(filter_flags) > 0) {
#     col_data[,
#              flag_filter:=FALSE]
#     
#     has_history <- any(col_data$reporting_history==TRUE)
# 
#     flag_data <- col_data[!sapply(col_data$flagsclasses,is.null),
#                           .(dataid,
#                             reporting_rowid,
#                             indicator_id,
#                             reporting_asof_date,
#                             flagsclasses)]
#     
#     flag_data <- flag_data[,.(check_class=as.character(unlist(flagsclasses,recursive=F))),
#                            by=.(dataid,
#                                 reporting_rowid,
#                                 indicator_id,
#                                 reporting_asof_date)]
#     
#     flag_data <- flag_data[check_class %in% filter_flags,
#                            .(flag_count=.N),
#                            by=.(reporting_rowid,
#                                 indicator_id,
#                                 reporting_asof_date)]
#     
#     flagged_indicators <- flag_data[,
#                                     .(flag_count=sum(flag_count)),
#                                       by=.(indicator_id)]
#     
#     setorder(flagged_indicators,
#              -flag_count)
#     
#     #If we're filtering for flags only, then implicitly this is what we're interested in: so bring the columns forward to make more
#     #visible.  But give priority to deliberately prioritized columns
#     priority_col_ids <- unique(c(priority_col_ids,unique(flagged_indicators$indicator_id)))
#     
#     flag_data <- unique(flag_data[,.(reporting_rowid,
#                                      reporting_asof_date)])
#     
#     
#     
#     col_data[flag_data,
#              flag_filter:=TRUE,
#              on=.(reporting_rowid,
#                   reporting_asof_date)]
#     
#     # col_data[!(indicator_id %in% priority_col_ids),
#     #          flag_filter:=TRUE]
#     
#     if (has_history) {
#       col_data <- col_data[reporting_history==FALSE | flag_filter==TRUE]
#     } else {
#       col_data <- col_data[flag_filter==TRUE]
#     }
#     
#     #Filtering for flags...but has none!
#     if (empty(col_data)) {
#       return (NULL)
#     }
#   }
#   
#   #Filtered everything away, evidently.
#   if (empty(col_data)) {
#     return(NULL)
#   }
#   #Priority and sorting preferences
#   #REACTIVE
#   {
#     sorted_indicators <- DASH_INDICATORS()[,.(indicator_id,
#                                                                      indicator_name,
#                                                                      data_category,
#                                                                      section_order=as.numeric(0),
#                                                                      indicator_order=as.numeric(NA),
#                                                                      priority_order=as.numeric(NA))]
#     
#     sorted_indicators <- sorted_indicators[indicator_id %in% unique(col_data$indicator_id)]
#     
#     priority_ids <- data.table(indicator_id=priority_col_ids)
#     priority_ids[,priority_order:=1:.N] #added separately for when priority_col_ids is numeric(0)
# 
#     #if (!empty(priority_ids)) priority_ids[,priority_order:=1:.N]
#     
#     sorted_indicators[priority_ids,
#                       priority_order:=i.priority_order,
#                       on=.(indicator_id)]
#     
#     selected_report_id <- DASH_DATA_OPTIONS$display_report_id
#     report_indicators <- DASH_REPORTS_INDICATORS_LIST()
#   
#   
#     if (isTruthy(selected_report_id) && isTruthy(report_indicators)) {
#       report_indicators <- report_indicators[report_id==selected_report_id,
#                                              .(indicator_id,
#                                                section_order,
#                                                indicator_order)]
#       
#       sorted_indicators[report_indicators,
#                         `:=`(section_order=i.section_order,
#                              indicator_order=i.indicator_order),
#                         on=.(indicator_id)]
#     }
#     
#     setorder(sorted_indicators,
#              priority_order,
#              section_order,
#              indicator_order,
#              na.last = TRUE)
#   
#     sorted_indicators[,sort_rank:=1:.N]
#     
#     #Because when currency_currency is specified, indicator name changes to @USD, eg and indicator names no longer match standard
#     #names that are available in the selection/sort fields.  So merge-in modified names, if any.
#     col_indicators <- unique(col_data[,.(indicator_id,indicator_name)])
#     sorted_indicators[col_indicators,
#                       indicator_name:=i.indicator_name,
#                       on=.(indicator_id)]
#   }
#   
#   #dd_cd <<- as.data.frame(col_data)
#   #s_ind <<- as.data.frame(sorted_indicators)
#   #rf_data <<- as.data.frame(DASH_RSF_FAMILY())
#   #rd <<- as.data.frame(DASH_DATA())
#   
#   #browser()
#   #rsf_family <- as.data.table(rf_data)
#   #col_data <- as.data.table(dd_df)
#   #sorted_indicators <- as.data.table(s_ind)
#   sys_NAMES <- unique(col_data[,.(reporting_SYSID,reporting_NAME)])
#   sys_NAMES <- setNames(sys_NAMES$reporting_SYSID,
#                         sys_NAMES$reporting_NAME)
#   
#   
#   
#   if (options_FORMAT=="INDICATORS") {
#     
#     cast_variables <- c("current","text","edit","html","flagstext","dataid")
# 
#     display_data <- dcast.data.table(col_data,
#                                       reporting_rowid + 
#                                       reporting_group +
#                                       reporting_history +  
#                                       reporting_intraperiod_rank + 
#                                       reporting_asof_date + 
#                                       reporting_SYSID +
#                                       #reporting_NAME +
#                                       reporting_SYSNAME +
#                                       reporting_STATUS ~ 
#                                       indicator_name,
#                                       value.var = cast_variables,
#                                       sep="!")
#     setnames(display_data,
#              old="reporting_SYSNAME",
#              new="reporting_NAME")
#     
#     setnames(display_data,
#              old=names(display_data),
#              new=gsub("^([a-z]+)!(.*$)","\\2.\\1",names(display_data)))
#     
#     column_sort <- unlist(lapply(cast_variables,function(x) paste0(sorted_indicators$indicator_name,".",x)))
#     column_sort <- c("reporting_asof_date",
#                      "reporting_SYSID",
#                      "reporting_STATUS",
#                      "reporting_NAME",
#                      "reporting_rowid",
#                      "reporting_group",
#                      "reporting_history",
#                      "reporting_intraperiod_rank",
#                      column_sort)
#     
#     setcolorder(display_data,column_sort)
#     
#     data_sort_cols <- c("reporting_SYSID")
#     if (!empty(sorted_indicators[!is.na(priority_order)])) {
#       data_sort_cols <- c(data_sort_cols,
#                           sorted_indicators[!is.na(priority_order),paste0(indicator_name,".current")])
#     }
#     
#     sort_dt <- display_data[reporting_history==FALSE & reporting_intraperiod_rank==0,
#                             mget(c("reporting_group",data_sort_cols))]
#     
#     setorderv(sort_dt,cols=data_sort_cols,na.last=TRUE)
#     sort_dt[,data_sort:=1:.N]
#     
#     display_data[sort_dt,
#                  data_sort:=i.data_sort,
#                  on=.(reporting_group)]
#     
#     sort_dt <- NULL
#     
#     setorder(display_data,
#              data_sort,
#              reporting_group,
#              reporting_history,
#              -reporting_asof_date,
#              reporting_intraperiod_rank)
#     
#     setcolorder(display_data,
#                 neworder=c("reporting_asof_date",
#                            "reporting_SYSID",
#                            "reporting_STATUS",
#                            "reporting_rowid",
#                            "reporting_group",
#                            "reporting_history",
#                            "reporting_intraperiod_rank")) #ensure its first
#     
#   } else if (options_FORMAT=="NAMES") {
#     
#     cast_variables <- c("current","text","edit","html","flagstext","dataid")
#     
#     display_data <- dcast.data.table(col_data,
#                                        #reporting_group +
#                                        reporting_history +  
#                                        reporting_intraperiod_rank + 
#                                        reporting_asof_date + 
#                                        indicator_name ~
#                                        reporting_NAME,
#                                      value.var = cast_variables,
#                                      sep="!")
#     
#     setnames(display_data,
#              old=names(display_data),
#              new=gsub("^([a-z]+)!(.*$)","\\2.\\1",names(display_data)))
#     
#     display_data[,
#                  reporting_group:=.GRP,
#                  by=.(indicator_name)]
# 
#     setcolorder(display_data,
#                 sort(names(display_data)))
#     
#     
#     setcolorder(display_data,c("reporting_asof_date",
#                                #"reporting_rowid",
#                                "reporting_group",
#                                "reporting_history",
#                                "reporting_intraperiod_rank",
#                                "indicator_name"))
#     
#     display_data[sorted_indicators,
#                  sort_rank:=i.sort_rank,
#                  on=.(indicator_name)]
#     
#     setorder(display_data,
#              sort_rank,
#              indicator_name,
#              reporting_history,
#              -reporting_asof_date,
#              reporting_intraperiod_rank,
#              na.last=TRUE)
#     
#     # display_data[,
#     #              data_sort:=1:.N]
#     display_data[,
#                  data_sort:=sort_rank]
#         
#     display_data[,sort_rank:=NULL]
#     
#   } else if (options_FORMAT=="LONG") {
#     
#     display_data <- col_data[,
#                                  .(reporting_asof_date,
#                                    reporting_SYSID,
#                                    reporting_STATUS,
#                                    reporting_NAME=reporting_SYSNAME,
#                                    reporting_history,
#                                    reporting_intraperiod_rank,
#                                    indicator_name,
#                                    current,
#                                    text,
#                                    flagstext,
#                                    html,
#                                    edit,
#                                    dataid)]
# 
#     display_data[sorted_indicators,
#                  data_sort:=i.sort_rank,
#                  on=.(indicator_name)]
#     
#     setorder(display_data,
#              reporting_NAME,
#              reporting_history,
#              -reporting_asof_date,
#              reporting_intraperiod_rank,
#              data_sort)
#     
#     display_data[,
#                  reporting_group:=.GRP,
#                  by=.(reporting_SYSID)]
#     
#     display_data <- unique(display_data)
#     
#     setcolorder(display_data,
#                 neworder=c("reporting_asof_date",
#                            "reporting_SYSID",
#                            "reporting_STATUS",
#                            "reporting_NAME",
#                            #"reporting_rowid",
#                            "reporting_group",
#                            "reporting_history",
#                            "reporting_intraperiod_rank",
#                            "indicator_name",
#                            "current",
#                            "text",
#                            "html",
#                            "edit",
#                            "dataid"))
#     
#   } else { stop("Unknown display format") }
#   
#   setattr(display_data,name="options_FORMAT",value=options_FORMAT)
#   setattr(display_data,name="SYSID_NAMES",value=sys_NAMES)
#   
#   #If there's history, then display intraperiod data in the history rows and keep current row as End-of-Period
#   #But if there's no history, the show intraperiod data so that it is not hidden or lost.
#   if (any(display_data$reporting_history==TRUE)) display_data <- display_data[!(reporting_history==FALSE & reporting_intraperiod_rank > 0)]  
#   
#   #Groups are sortder by serorderv()
#   #Current rows alternate 1 and 2
#   display_data[,
#                reporting_group_sort:=((data_sort+1)%%2)+1]
#   
#   #History rows alternate 3 and 4
#   #1 and 3 are grouped; 2 and 4 are grouped
#   display_data[reporting_history==TRUE,
#                reporting_group_sort:=reporting_group_sort+2,
#                by=.(reporting_group,reporting_asof_date)]
#   
#   
#   #NAMES or LONG will lose reporting_rowid and/or change sort orders after dcast
#   display_data[,
#                reporting_rowid:=1:.N]
#   
#   display_data[,data_sort:=NULL]
#   
#   if (any(names(display_data)=="reporting_STATUS") && any(DASH_DATA_OPTIONS$column_status==FALSE)) display_data[,reporting_STATUS:=NULL]
#   if (any(names(display_data)=="reporting_NAME") && any(DASH_DATA_OPTIONS$column_sysname==FALSE)) display_data[,reporting_NAME:=NULL]
# 
#   return (display_data)
# }, ignoreNULL=FALSE)
# 
# DASH_DATA_RENDER_DISPLAY <- function(display_cell=c("html",
#                                                     "text",
#                                                     "flagstext",
#                                                     "edit",
#                                                     "current"),
#                                      display_for=c("dashboard",
#                                                    "download"),
#                                      display_data=DASH_DATA_DISPLAY(),
#                                      display_options=NULL) { #DASH_DATA_OPTIONS) {
# 
#   display_cell <- match.arg(display_cell)
#   display_for <- match.arg(display_for)
#   
#   #display_data <- DASH_DATA_DISPLAY()
# #dd <<- as.data.frame(display_data)
# #display_options <<-  reactiveValuesToList(DASH_DATA_OPTIONS)
# #display_data <- as.data.table(dd)
# #display_cell <- "html"
#   if (empty(display_data)) return (NULL)
#   if (length(names(display_data)) != length(unique(names(display_data)))) stop("Names formatting error has occurred")
#   
#   options_FORMAT <- attr(display_data,"options_FORMAT")
#   
#   display_indicator_names <- NULL
#   render_data <- NULL
#   if (options_FORMAT=="LONG") {
#     display_indicator_names <- c(grep("^reporting_.*$",names(display_data),value=T),
#                                  "indicator_name",
#                                  display_cell)
#     render_data <- display_data[,..display_indicator_names]
#     setnames(render_data,
#              old=grep("^text|html|edit|current$",names(render_data),value=T),
#              new="data_value")
#     
#   } else if (options_FORMAT=="INDICATORS") {
#     display_indicator_names <- c(grep("^reporting_.*$",names(display_data),value=T),
#                                  grep(paste0("\\.",display_cell,"$"),names(display_data),value=T)) 
#     render_data <- display_data[,..display_indicator_names]
#     setnames(render_data,
#              old=names(render_data),
#              new=indicatorNames_getBasenames(names(render_data)))
#     
#   } else if (options_FORMAT=="NAMES") {
#     col_names <- grep(paste0("\\.",display_cell,"$"),names(display_data),value=T)
#     display_indicator_names <- c(grep("^reporting_.*$",names(display_data),value=T),
#                                  "indicator_name",
#                                  col_names) 
#     
#     render_data <- display_data[,..display_indicator_names]
#     setnames(render_data,
#              old=col_names,
#              new=gsub(paste0("\\.",display_cell,"$"),"",col_names))
#   }
#   
# 
#   #just hide in rendering if appropriate, to ensure column sort remains static.
#   #if (is.na(DASH_DATA_OPTIONS$column_status)==FALSE & DASH_DATA_OPTIONS$column_status == FALSE) display_data[,reporting_STATUS:=NULL]
#   #else if (is.na(DASH_DATA_OPTIONS$column_status)==TRUE & length(unique(display_data$reporting_STATUS))==1) display_data[,reporting_STATUS:=NULL]
#   
#   
#   return (render_data)
# }
}