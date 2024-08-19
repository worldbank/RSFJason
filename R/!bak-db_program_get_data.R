db_program_get_data  <- function(pool,
                                 reporting_current_date,  #When - current asof date
                                 indicator_variables,     #What - a named vector of indicator_id values; names, a csv concatenated string of variable attribute requirements
                                 for_rsf_pfcbl_ids,       #Who  - who do we want the data for?
                                 indicator_ids.simplify=TRUE, #When TRUE, will return data columns specified in indicator_ids (and omit sys_reporting pseudo indicators); #Note that rsf_checks_calculate expects these for by() groupings
                                 missing_indicators.use_default=TRUE)
{

  #checks
  {
    # if (length(rsf_program_id) != 1) {
    #   stop("rsf_program_id must be a single value")
    #   return (NULL)
    # }
  
    if (length(reporting_current_date) != 1 || is.Date(as.Date(reporting_current_date))==FALSE) {
      stop("reporting_current_date must be a single date value")
      return (NULL)
    }
    
    if (length(for_rsf_pfcbl_ids)==0) {
      warning("No rsf_pfcbl_ids requested. Returning NULL")
      return (NULL)
    }
    
    #length(0) or NULL is useful because indicator formulas might extract non-existent indicators from poorly defined formulas and then filter those out
    #when feeding the request. So the intention is to request indicators (but they don't exist).  Let the data return basic system indicators instead
    #and let the system calculator discover that expected columns don't exist.
    if (length(indicator_variables)==0 || is.null(indicator_variables)) {
      indicator_variables <- NULL
    } else {
      if (anyNA(indicator_variables)) stop(paste0("Argument indicator_variables cannot be or include NA Request=",paste0(indicator_variables,collapse=",")," Attributes=",unique(names(indicator_variables),collapse=",")))
      if (is.null(names(indicator_variables)) || all(names(indicator_variables)=="")) stop("Argument indicator_variables expects a named vector, whose names request data attributes for respected indicator.  Valid names are: ",paste0(CALCULATIONS_ENVIRONMENT$indicator_ATTRIBUTES,collapse=","))
    }
    
    duplicated_indicators <- which(duplicated(indicator_variables) & duplicated(names(indicator_variables)))
    if (length(duplicated_indicators) > 0) indicator_variables <- indicator_variables[-duplicated_indicators]
    
    for_rsf_pfcbl_ids <- unique(for_rsf_pfcbl_ids)
    if (anyNA(for_rsf_pfcbl_ids)) for_rsf_pfcbl_ids <- for_rsf_pfcbl_ids[-which(is.na(for_rsf_pfcbl_ids))]
    reporting_current_date <- as.Date(reporting_current_date)
  }  
  # db_params <<- as.list(environment())[which(names(as.list(environment())) %in% c("rsf_program_id","reporting_current_date","indicator_variables","for_rsf_pfcbl_ids","indicator_ids.simplify","missing_indicators.use_default"))]
  
  # lapply(names(db_params),function(x) { assign(x,db_params[[x]],envir = globalenv()) })

  
  #SYS_PRINT_TIMING <- TRUE
  t1<-Sys.time()
  
  #MAIN GET INDICATORS, FAMILY, DATA
  {
    #if(SYS_PRINT_TIMING)  debugtime("db_program_get_data","getting data")
    
    #Valid input parameters, ensure unique, etc
    {
      # valid_date <- dbGetQuery(pool,
      #                          "select exists(select *
      #                                                  from p_rsf.rsf_program_reporting_dates prd
      #                                                  where prd.rsf_program_id = $1::int
      #                                                    and prd.valid_reporting_date = $2::date)::bool as valid_date;",
      #                          params=list(rsf_program_id,
      #                                      reporting_current_date))
      # 
      # valid_date <- valid_date$valid_date
      # if (valid_date==FALSE) stop(paste0("Program ID",rsf_program_id," does not have a reporting entry asof ",reporting_current_date))
      
      indicator_ids.keepfor <- NULL #Will be defined based on rsf_data query: if indicator_ids is specified (not NA) then filter-out to keep only requested indicators
      #this has the primary effect of filtering out the sys_rsf_X_reporting indicators that are always pulled in to ensure joins.
      

      requested_vars <- NULL
      #requested_status_vars <- NULL
      
      if (!is.null(indicator_variables)) requested_vars <- unique(trimws(unlist(strsplit(names(indicator_variables),
                                                                                         split=',',
                                                                                         fixed=T))))
     
    }

    #get indicators
    {
      indicators_requested <- unique(as.numeric(indicator_variables))
      if (length(indicator_variables)==0 || all(is.na(indicator_variables))) indicators_requested <- NA
      
      #Deliberate decision NOT to check if entry exists in rsf_program_indicators because non-subscribed indicators could be requested via
      #formulas, checks, globals -- let the consumer function verify validity
      pfcbl_indicators <- dbGetQuery(pool,
                                     "
                                     select
                                      ind.indicator_id,
                                      ind.indicator_name,
                                      ind.data_category,
                                      ind.data_type,
                                      ind.indicator_sys_category,
                                      ind.default_value,
                                      ind.data_unit,
                                      coalesce(ind.is_static_nonreporting,false) as is_static_nonreporting,
                                      coalesce(ind.is_periodic_or_flow_reporting,false) as is_periodic_or_flow_reporting
                                      from p_rsf.indicators ind
                                      where ind.indicator_id = any(select unnest(string_to_array(nullif($1::text,'NA'),','))::int)
                                        and ind.indicator_name ~ E'^rsf_' = false", #don't pull-in any newly created indicators
                                     params=list(paste0(indicators_requested,collapse=",")))
      
      
      setDT(pfcbl_indicators)
      #pfcbl_indicators[,is_reporting_indicator:=FALSE]
      #pfcbl_indicators[indicator_sys_category=='entity_reporting',is_reporting_indicator:=TRUE]
      
      if (any(!indicators_requested %in% pfcbl_indicators$indicator_id) & all(is.na(indicators_requested))==FALSE) {
        
        stop(paste0("Invalid indicator IDs requested for: ",
                    paste0(indicators_requested[which(!indicators_requested %in% pfcbl_indicators$indicator_id)])))
      }
      
      if (length(indicators_requested)==0) {
        indicator_ids.keepfor <- unique(pfcbl_indicators$indicator_id)
      } else {
        indicator_ids.keepfor <- indicators_requested
      }
      
      #Request a non-existent indicator_id
      if (any(!indicator_variables %in% pfcbl_indicators$indicator_id)) indicator_variables <- indicator_variables[-which(!indicator_variables %in% pfcbl_indicators$indicator_id)]
      if (is.null(indicator_variables)) indicator_variables <- numeric(0)
      
      
      query_indicators <- data.table(indicator_id=as.numeric(indicator_variables),
                                     indicator_variable=names(indicator_variables))
      
      if (empty(query_indicators)) {
        query_indicators <- data.table(indicator_id=numeric(0),
                                       indicator_variable=character(0))
      } else  {
        query_indicators <- query_indicators[,.(indicator_variable=unlist(strsplit(indicator_variable,
                                                                                   split=',',
                                                                                   fixed=T),recursive = F)),
                                         by=.(indicator_id)]
      }
      
      query_indicators[,
                       indicator_variable_class:=gsub("^([a-z]+).*","\\1",indicator_variable)]

      #ensure .current or .previous are part of the variable class
      query_indicators[indicator_variable_class %in% c("min","max","sum"),
                       indicator_variable_class:=gsub("^([a-z]+\\.[a-z]+).*","\\1",indicator_variable)]      
      #Issuances:
      #.current and .previous have different query structures and "issuances" isn't a proper class
      query_indicators[indicator_variable_class=="issuances",
                       indicator_variable_class:=indicator_variable]
      
      #.current.reportingcount has a different query structure from .current
      # query_indicators[indicator_variable=="current.reportingcount",
      #                  indicator_variable_class:="current.reportingcount"]
      
      #.current.reportnumber has a different query structure from .current
      #query_indicators[indicator_variable=="current.reportnumber",
      #                 indicator_variable_class:="current.reportnumber"]
      
      query_indicators <- unique(query_indicators)
      query_indicators <- query_indicators[indicator_variable %in% CALCULATIONS_ENVIRONMENT$indicator_ATTRIBUTES]
      
      #static indicators can only meaningfully request .current so enforce this
      query_indicators[,is_static_nonreporting:=FALSE]
      query_indicators[pfcbl_indicators[is_static_nonreporting==TRUE],
                       is_static_nonreporting:=i.is_static_nonreporting,
                       on=.(indicator_id)]
      
      #static non reporting indicators are only meaningful at "current"
      query_indicators <- query_indicators[is_static_nonreporting==FALSE | indicator_variable_class=="current"]
      query_indicators[,is_static_nonreporting:=NULL]
      
      query_indicators <- unique(query_indicators)
    }
    
    #get pfcbl_ids
    {
      #If we request a global indicator, add-in the global rsf_pfcbl_id
      if (any(pfcbl_indicators$data_category=="global") && !any(for_rsf_pfcbl_ids==0)) {
        for_rsf_pfcbl_ids <- c(0,for_rsf_pfcbl_ids)
      }


      pfcbl_ids <- dbGetQuery(pool,
                              "select
                                ids.rsf_pfcbl_id,
                              	ids.rsf_id,
                              	ids.pfcbl_category,
                              	ids.rsf_program_id,
                              	ids.rsf_facility_id,
                              	ids.rsf_client_id,
                              	ids.rsf_borrower_id,
                              	ids.rsf_loan_id,
                              	ids.created_in_reporting_asof_date,
                              	lis.loan_issuance_series_id,
                              	lis.loan_issuance_series_rank
                              from p_rsf.rsf_pfcbl_ids ids
                              left join p_rsf.rsf_loan_issuance_series lis on lis.rsf_pfcbl_id = ids.rsf_pfcbl_id
                              where ids.created_in_reporting_asof_date <= $1::date
                              --and (ids.rsf_program_id = $2 or ids.rsf_program_id = 0)
                              and exists(select * from p_rsf.rsf_pfcbl_id_family fam 
                              						 where ids.rsf_pfcbl_id = fam.parent_rsf_pfcbl_id
                              						   and fam.child_rsf_pfcbl_id = any(select unnest(string_to_array($2::text,','))::int))",
                                params=list(reporting_current_date,
                                            paste0(unique(for_rsf_pfcbl_ids),collapse=",")))
      
      setDT(pfcbl_ids)
      #for_rsf_pfcbl_ids <- c(0,for_rsf_pfcbl_ids)
      
      if (empty(pfcbl_ids)) {
        stop("Empty pfcbl IDs")
      }
      
      
      #pfcbl family
      {
        rsf_program_ids <- unique(pfcbl_ids$rsf_program_id)
        if (anyNA(rsf_program_ids)) { 
          stop("NA rsf_program_id in db_program_get_data")
        }
        if (length(rsf_program_ids[rsf_program_ids > 0]) > 1) {
          stop("Data cannot be requested for multiple programs")
        }
        
        rsf_program_id <- max(rsf_program_ids)
        pfcbl_ids[,rsf_program_id:=max(rsf_program_ids)]
        
        loans <- pfcbl_ids[pfcbl_category=="loan",.(rsf_pfcbl_id.loan=rsf_pfcbl_id,rsf_borrower_id,rsf_loan_id)]
        borrowers <- pfcbl_ids[pfcbl_category=="borrower",.(rsf_pfcbl_id.borrower=rsf_pfcbl_id,rsf_client_id,rsf_borrower_id)]
        clients <- pfcbl_ids[pfcbl_category=="client",.(rsf_pfcbl_id.client=rsf_pfcbl_id,rsf_facility_id,rsf_client_id)]
        facilities <- pfcbl_ids[pfcbl_category=="facility",.(rsf_pfcbl_id.facility=rsf_pfcbl_id,rsf_program_id,rsf_facility_id)]
        programs <- pfcbl_ids[pfcbl_category == "program",.(rsf_pfcbl_id.program=rsf_pfcbl_id,rsf_program_id)]
        #global <- pfcbl_ids[pfcbl_category == "global",.(rsf_pfcbl_id.program=rsf_pfcbl_id,rsf_program_id)]
        
        pfcbl_family <- loans[borrowers,
                              on=.(rsf_borrower_id)
                              ][clients,
                                on=.(rsf_client_id)
                                ][facilities,
                                  on=.(rsf_facility_id)
                                  ][programs,
                                    on=.(rsf_program_id)]
        
        if (rsf_program_id==0) {
          
          pfcbl_family <- pfcbl_ids[pfcbl_category == "global",.(rsf_pfcbl_id.global=as.integer(rsf_pfcbl_id),
                                                                 rsf_program_id=as.integer(0))]
          pfcbl_family[,`:=`(rsf_pfcbl_id.program=as.integer(NA),
                             rsf_pfcbl_id.facility=as.integer(NA),
                             rsf_pfcbl_id.client=as.integer(NA),
                             rsf_pfcbl_id.borrower=as.integer(NA),
                             rsf_pfcbl_id.loan=as.integer(NA),
                             rsf_facility_id=as.integer(NA),
                             rsf_client_id=as.integer(NA),
                             rsf_borrower_id=as.integer(NA),
                             rsf_loan_id=as.integer(NA))]
        } else {
          
          pfcbl_family[,rsf_pfcbl_id.global:=as.integer(0)]
          
        }
        
        setcolorder(pfcbl_family,
                    neworder=c("rsf_pfcbl_id.global",
                               "rsf_pfcbl_id.program",
                               "rsf_pfcbl_id.facility",
                               "rsf_pfcbl_id.client",
                               "rsf_pfcbl_id.borrower",
                               "rsf_pfcbl_id.loan",
                               "rsf_program_id",
                               "rsf_facility_id",
                               "rsf_client_id",
                               "rsf_borrower_id",
                               "rsf_loan_id"))
        setorder(pfcbl_family,
                 rsf_program_id,
                 rsf_facility_id,
                 rsf_client_id,
                 rsf_borrower_id,
                 rsf_loan_id)
        
        
        
        programs <- NULL
        facilities <- NULL
        clients <- NULL
        borrowers <- NULL
        loans <- NULL
        
        
        pfcbl_ids[,
                  `:=`(reporting_indicator_id=as.numeric(NA),
                       reporting_indicator_name=as.character(NA))]
      
        pfcbl_family[,family_id:=1:.N]
        
        #Missing/non-existent entities should also have unique IDs (not NA)
        pfcbl_family[is.na(rsf_facility_id),rsf_facility_id:=-1*(1:.N)]
        pfcbl_family[is.na(rsf_client_id),rsf_client_id:=-1*(1:.N)]
        pfcbl_family[is.na(rsf_borrower_id),rsf_borrower_id:=-1*(1:.N)]
        pfcbl_family[is.na(rsf_loan_id),rsf_loan_id:=-1*(1:.N)]
        
        
        pfcbl_family_long <- melt.data.table(pfcbl_family[,.(family_id,
                                                             rsf_pfcbl_id.global,
                                                             rsf_pfcbl_id.program,
                                                             rsf_pfcbl_id.facility,
                                                             rsf_pfcbl_id.client,
                                                             rsf_pfcbl_id.borrower,
                                                             rsf_pfcbl_id.loan)],
                                             id.vars="family_id",
                                             variable.name="rsf_pfcbl_id.category",
                                             value.name="rsf_pfcbl_id")
        
        #Missing rsf_pfcbl_ids must also be unique
        {
          pfcbl_family_long[is.na(rsf_pfcbl_id),rsf_pfcbl_id:=-1*(1:.N)]
  
          #Must be a global reporting query if program ID is NA
          pfcbl_family[pfcbl_family_long[rsf_pfcbl_id.category=="rsf_pfcbl_id.program",.(rsf_pfcbl_id.program=as.numeric(NA),family_id,rsf_pfcbl_id)],
                       rsf_pfcbl_id.program:=i.rsf_pfcbl_id,
                       on=.(family_id,
                            rsf_pfcbl_id.program)]
            
          pfcbl_family[pfcbl_family_long[rsf_pfcbl_id.category=="rsf_pfcbl_id.facility",.(rsf_pfcbl_id.facility=as.numeric(NA),family_id,rsf_pfcbl_id)],
                       rsf_pfcbl_id.facility:=i.rsf_pfcbl_id,
                       on=.(family_id,
                            rsf_pfcbl_id.facility)]
          
          pfcbl_family[pfcbl_family_long[rsf_pfcbl_id.category=="rsf_pfcbl_id.client",.(rsf_pfcbl_id.client=as.numeric(NA),family_id,rsf_pfcbl_id)],
                       rsf_pfcbl_id.client:=i.rsf_pfcbl_id,
                       on=.(family_id,
                            rsf_pfcbl_id.client)]
          
          pfcbl_family[pfcbl_family_long[rsf_pfcbl_id.category=="rsf_pfcbl_id.borrower",.(rsf_pfcbl_id.borrower=as.numeric(NA),family_id,rsf_pfcbl_id)],
                       rsf_pfcbl_id.borrower:=i.rsf_pfcbl_id,
                       on=.(family_id,
                            rsf_pfcbl_id.borrower)]
          
          pfcbl_family[pfcbl_family_long[rsf_pfcbl_id.category=="rsf_pfcbl_id.loan",.(rsf_pfcbl_id.loan=as.numeric(NA),family_id,rsf_pfcbl_id)],
                       rsf_pfcbl_id.loan:=i.rsf_pfcbl_id,
                       on=.(family_id,
                            rsf_pfcbl_id.loan)]
        }        
        #if(SYS_PRINT_TIMING)  debugtime("db_program_get_data","pfcbl_family created with ",nrow(pfcbl_family)," members")
      }
    }
    
    #get the data
    {
      rsf_data <- data.table(reporting_asof_date=as.Date(numeric(0)),
                             rsf_pfcbl_id=numeric(0),
                             data_id=numeric(0),
                             indicator_id=numeric(0),
                             data_value=character(0),
                             data_unit=character(0),
                             data_class=character(0),
                             data_value_changed=logical(0),
                             data_value_updated=logical(0),
                             data_asof_date=as.Date(numeric(0)),
                             reportnumber=numeric(0))
      
      #issuances.previous aren't date-based so exclude 
      program_previous_date <- as.Date(as.numeric(NA))
      if (any(grepl("previous",query_indicators$indicator_variable)==TRUE &
              grepl("issuances\\.previous",query_indicators$indicator_variable)==FALSE)) {
        
        query_previous_date <- dbGetQuery(pool,"
                                            select 
                                              prd.valid_reporting_date 
                                            from p_rsf.rsf_program_reporting_dates prd
                                            where prd.rsf_program_id = $1::int
                                              and prd.valid_reporting_date < $2::date
                                            order by prd.valid_reporting_date desc
                                            limit 1",
                                          params=list(rsf_program_id,
                                                      reporting_current_date))
        
        if (!empty(query_previous_date)) {
          program_previous_date <- query_previous_date$valid_reporting_date
        } 
      } 
      
      if (any(query_indicators$indicator_variable_class=="current")) {
        
        query_indicator_ids <- query_indicators[indicator_variable_class=="current",unique(indicator_id)]
        query_rsf_data <- dbGetQuery(pool,"
                                      select 
                                      	end_period.reporting_asof_date as data_asof_date,
                                      	ids.rsf_pfcbl_id,
                                      	end_period.data_id,
                                      	ind.indicator_id,
                                      	end_period.data_value,
                                      	coalesce(end_period.data_unit,ind.data_unit) as data_unit,
                                        end_period.reportnumber,
                                        end_period.data_value_changed::bool AND end_period.reporting_asof_date = $3::date as data_value_changed,
                                        end_period.reporting_asof_date = $3::date as data_value_updated
                                      from p_rsf.rsf_pfcbl_ids ids
                                      inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
                                      inner join lateral (select
                                                            rdc.data_id,
                                      											rdc.reporting_asof_date,
                                      											rdc.data_value,
                                      											rdc.data_unit,
                                                            count(*) over() as reportnumber,
                                                            case 
                                                              when lag(rdc.data_id,1) over(order by reporting_asof_date) is NULL 
                                                              then false
                                                              
                                                              -- Any unit change is an unambiguous change
                                                              when rdc.data_unit is distinct from lag(rdc.data_unit,1) over(order by reporting_asof_date)
                                                              then true
                                                            
                                                              -- An update from zero to blank (or blank to zero) will count as an update, but not a change
                                                              -- as numerically these are nothing before and still nothing now.
                                                              when ind.data_type in ('number','percent','currency')
                                                                   AND NULLIF(lag(rdc.data_value,1) over(order by reporting_asof_date),'0') 
                                                                       IS NOT DISTINCT FROM
                                                                       NULLIF(rdc.data_value,'0')
                                                              then false
                                     
                                                              else rdc.data_value is distinct from lag(rdc.data_value,1) over(order by reporting_asof_date)
                                                                   
                                                            end::bool as data_value_changed
                                      										from p_rsf.rsf_data_current rdc
                                      										where rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id
                                      										  and rdc.indicator_id = ind.indicator_id
                                      											and rdc.reporting_asof_date <= $3::date
                                      										order by 
                                      										  rdc.reporting_asof_date desc
                                      										limit 1) end_period on true
                                      where ids.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
                                      	and ind.indicator_id = any(select unnest(string_to_array($2::text,','))::int)",
                               params=list(paste0(unique(pfcbl_ids$rsf_pfcbl_id),collapse=","),
                                           paste0(query_indicator_ids,collapse=","),
                                           reporting_current_date))
  
        setDT(query_rsf_data)

        query_rsf_data[,data_class:="current"]

        query_rsf_data[,
                       reporting_asof_date:=reporting_current_date]

        # if (!empty(query_rsf_data)) {
        #   query_rsf_data[,
        #                  `:=`(reporting_asof_date=reporting_current_date,
        #                       data_value_changed=(fcase(reportnumber==1 & 
        #                                                 reporting_current_date <= data_asof_date,NA, #First reporting isn't a "change" it's a first entry, an NA.
        #                                                 is.same_text()
        #                                                 reporting_current_date == data_asof_date,TRUE,
        #                                                 default=FALSE)),
        #                       data_value_updated=(data_asof_date==reporting_current_date))]
        # }        
        setcolorder(query_rsf_data,
                    neworder=names(rsf_data))
        
        rsf_data <- rbindlist(list(rsf_data,
                                   query_rsf_data))
        
        query_rsf_data <- NULL
      }
      
    
      if (any(query_indicators$indicator_variable_class=="previous")) {
        
        query_indicator_ids <- query_indicators[indicator_variable_class=="previous",unique(indicator_id)]
        
        
        query_rsf_data <- dbGetQuery(pool,"
                                      select 
                                      	end_period.reporting_asof_date as data_asof_date,
                                      	ids.rsf_pfcbl_id,
                                      	end_period.data_id,
                                      	ind.indicator_id,
                                      	end_period.data_value,
                                      	coalesce(end_period.data_unit,ind.data_unit) as data_unit
                                      from p_rsf.rsf_pfcbl_ids ids
                                      inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
                                      inner join lateral (select
                                                            rdc.data_id,
                                      											rdc.reporting_asof_date,
                                      											rdc.data_value,
                                      											rdc.data_unit
                                      										from p_rsf.rsf_data_current rdc
                                      										where rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id
                                      										  and rdc.indicator_id = ind.indicator_id
                                      											and rdc.reporting_asof_date <= $3::date
                                      										order by 
                                      										  rdc.reporting_asof_date desc
                                      										limit 1) end_period on true
                                      where ids.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
                                      	and ind.indicator_id = any(select unnest(string_to_array($2::text,','))::int)",
                                     params=list(paste0(unique(pfcbl_ids$rsf_pfcbl_id),collapse=","),
                                                 paste0(query_indicator_ids,collapse=","),
                                                 program_previous_date))
        
        setDT(query_rsf_data)
        query_rsf_data[,data_class:="previous"]
        
        query_rsf_data[,
                       `:=`(reporting_asof_date=reporting_current_date,
                            data_value_changed=as.logical(NA),
                            data_value_updated=(data_asof_date==program_previous_date), #update meaningful for zero-out flow data
                            reportnumber=as.numeric(NA))]
        
        setcolorder(query_rsf_data,neworder=names(rsf_data))
        rsf_data <- rbindlist(list(rsf_data,
                                   query_rsf_data))
        
        query_rsf_data <- NULL
      }

      if (any(grepl("^(min|max|sum)",query_indicators$indicator_variable_class))) {
        query_mms_indicators <- query_indicators[grepl("^(min|max|sum)",indicator_variable_class)]
        query_mms_indicators_current <- query_mms_indicators[grepl("\\.current$",indicator_variable)]
        query_mms_indicators_previous <- query_mms_indicators[grepl("\\.previous$",indicator_variable)]
        mms_rsf_data <- data.table(data_asof_date=as.Date(numeric(0)),
                                   rsf_pfcbl_id=numeric(0),
                                   data_id=numeric(0),
                                   indicator_id=numeric(0),
                                   data_value=character(0),
                                   data_unit=character(0),
                                   data_class=character(0))
        
        if (!empty(query_mms_indicators_current)) {
          
          if (any(query_mms_indicators_current$indicator_variable_class=="min.current")) {
            query_rsf_data <- dbGetQuery(pool,"
                                        select distinct on (min_period.rsf_pfcbl_id,min_period.indicator_id) 
                                        	min_period.reporting_asof_date as data_asof_date,
                                        	min_period.rsf_pfcbl_id,
                                        	min_period.data_id,
                                        	min_period.indicator_id,
                                        	min_period.data_value,
                                        	min_period.data_unit
                                        from p_rsf.rsf_data_current min_period
                                        where min_period.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
                                        	and min_period.indicator_id = any(select unnest(string_to_array($2::text,','))::int)
                                          and min_period.reporting_asof_date <= $3::date
                                        order by 
                                          min_period.rsf_pfcbl_id,
                                          min_period.indicator_id,
                                          case when isnumeric(min_period.data_value) then (min_period.data_value::numeric) else NULL end asc nulls last,
                                          min_period.data_value asc nulls last,
                                          min_period.reporting_asof_date asc",
                                        params=list(paste0(unique(pfcbl_ids$rsf_pfcbl_id),collapse=","),
                                                    paste0(query_mms_indicators_current[indicator_variable_class=="min.current",unique(indicator_id)],collapse=","),
                                                    reporting_current_date))
          
            setDT(query_rsf_data)
            query_rsf_data[,
                           data_class:="min.current"]
            
            mms_rsf_data <- rbindlist(list(mms_rsf_data,
                                           query_rsf_data))
            query_rsf_data <- NULL
          }
          if (any(query_mms_indicators_current$indicator_variable_class=="max.current")) {
            query_rsf_data <- dbGetQuery(pool,"
                                        select distinct on (max_period.rsf_pfcbl_id,max_period.indicator_id) 
                                        	max_period.reporting_asof_date as data_asof_date,
                                        	max_period.rsf_pfcbl_id,
                                        	max_period.data_id,
                                        	max_period.indicator_id,
                                        	max_period.data_value,
                                        	max_period.data_unit
                                        from p_rsf.rsf_data_current max_period
                                        where max_period.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
                                        	and max_period.indicator_id = any(select unnest(string_to_array($2::text,','))::int)
                                          and max_period.reporting_asof_date <= $3::date
                                        order by 
                                          max_period.rsf_pfcbl_id,
                                          max_period.indicator_id,
                                          case when isnumeric(max_period.data_value) then (max_period.data_value::numeric) else NULL end desc nulls last,
                                          max_period.data_value desc nulls last,
                                          max_period.reporting_asof_date desc",
                                        params=list(paste0(unique(pfcbl_ids$rsf_pfcbl_id),collapse=","),
                                                    paste0(query_mms_indicators_current[indicator_variable_class=="max.current",unique(indicator_id)],collapse=","),
                                                    reporting_current_date))
            
            setDT(query_rsf_data)
            query_rsf_data[,
                           data_class:="max.current"]
            
            mms_rsf_data <- rbindlist(list(mms_rsf_data,
                                           query_rsf_data))
            query_rsf_data <- NULL
          }
          if (any(query_mms_indicators_current$indicator_variable_class=="sum.current")) {
            query_rsf_data <- dbGetQuery(pool,"
                                        select distinct on (sum_period.rsf_pfcbl_id,sum_period.indicator_id)
                                        	sum_period.reporting_asof_date as data_asof_date,
                                        	sum_period.rsf_pfcbl_id,
                                        	sum_period.data_id,
                                        	sum_period.indicator_id,	
                                        	sum(sum_period.data_value::numeric) over(partition by sum_period.rsf_pfcbl_id,sum_period.indicator_id) as data_value,
                                        	sum_period.data_unit
                                        from p_rsf.rsf_data_current sum_period
                                        where sum_period.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
                                        	and sum_period.indicator_id = any(select unnest(string_to_array($2::text,','))::int)
                                          and sum_period.reporting_asof_date <= $3::date
                                          and public.isnumeric(sum_period.data_value) = true
                                        order by 
                                          sum_period.rsf_pfcbl_id,
                                          sum_period.indicator_id,
                                          sum_period.reporting_asof_date desc",
                                        params=list(paste0(unique(pfcbl_ids$rsf_pfcbl_id),collapse=","),
                                                    paste0(query_mms_indicators_current[indicator_variable_class=="sum.current",unique(indicator_id)],collapse=","),
                                                    reporting_current_date))
            
            setDT(query_rsf_data)
            query_rsf_data[,
                           data_class:="sum.current"]
            
            mms_rsf_data <- rbindlist(list(mms_rsf_data,
                                           query_rsf_data))
            query_rsf_data <- NULL
          }
        }
        
        if (!empty(query_mms_indicators_previous)) {
          
          if (any(query_mms_indicators_previous$indicator_variable_class=="min.previous")) {
            query_rsf_data <- dbGetQuery(pool,"
                                        select distinct on (min_period.rsf_pfcbl_id,min_period.indicator_id) 
                                        	min_period.reporting_asof_date as data_asof_date,
                                        	min_period.rsf_pfcbl_id,
                                        	min_period.data_id,
                                        	min_period.indicator_id,
                                        	min_period.data_value,
                                        	min_period.data_unit
                                        from p_rsf.rsf_data_current min_period
                                        where min_period.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
                                        	and min_period.indicator_id = any(select unnest(string_to_array($2::text,','))::int)
                                          and min_period.reporting_asof_date <= $3::date
                                        order by 
                                          min_period.rsf_pfcbl_id,
                                          min_period.indicator_id,
                                          case when isnumeric(min_period.data_value) then (min_period.data_value::numeric) else NULL end asc nulls last,
                                          min_period.data_value asc nulls last,
                                          min_period.reporting_asof_date asc",
                                        params=list(paste0(unique(pfcbl_ids$rsf_pfcbl_id),collapse=","),
                                                    paste0(query_mms_indicators_previous[indicator_variable_class=="min.previous",unique(indicator_id)],collapse=","),
                                                    program_previous_date))
            
            setDT(query_rsf_data)
            query_rsf_data[,
                           data_class:="min.previous"]
            
            mms_rsf_data <- rbindlist(list(mms_rsf_data,
                                           query_rsf_data))
            query_rsf_data <- NULL
          }
          if (any(query_mms_indicators_previous$indicator_variable_class=="max.previous")) {
            query_rsf_data <- dbGetQuery(pool,"
                                        select distinct on (max_period.rsf_pfcbl_id,max_period.indicator_id) 
                                        	max_period.reporting_asof_date as data_asof_date,
                                        	max_period.rsf_pfcbl_id,
                                        	max_period.data_id,
                                        	max_period.indicator_id,
                                        	max_period.data_value,
                                        	max_period.data_unit
                                        from p_rsf.rsf_data_current max_period
                                        where max_period.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
                                        	and max_period.indicator_id = any(select unnest(string_to_array($2::text,','))::int)
                                          and max_period.reporting_asof_date <= $3::date
                                        order by 
                                          max_period.rsf_pfcbl_id,
                                          max_period.indicator_id,
                                          case when isnumeric(max_period.data_value) then (max_period.data_value::numeric) else NULL end desc nulls last,
                                          max_period.data_value desc nulls last ,
                                          max_period.reporting_asof_date desc",
                                        params=list(paste0(unique(pfcbl_ids$rsf_pfcbl_id),collapse=","),
                                                    paste0(query_mms_indicators_previous[indicator_variable_class=="max.previous",unique(indicator_id)],collapse=","),
                                                    program_previous_date))
            
            setDT(query_rsf_data)
            query_rsf_data[,
                           data_class:="max.previous"]
            
            mms_rsf_data <- rbindlist(list(mms_rsf_data,
                                           query_rsf_data))
            query_rsf_data <- NULL
          }
          if (any(query_mms_indicators_previous$indicator_variable_class=="sum.previous")) {
            query_rsf_data <- dbGetQuery(pool,"
                                        select distinct on (sum_period.rsf_pfcbl_id,sum_period.indicator_id)
                                        	sum_period.reporting_asof_date as data_asof_date,
                                        	sum_period.rsf_pfcbl_id,
                                        	sum_period.data_id,
                                        	sum_period.indicator_id,	
                                        	sum(sum_period.data_value::numeric) over(partition by sum_period.rsf_pfcbl_id,sum_period.indicator_id) as data_value,
                                        	sum_period.data_unit
                                        from p_rsf.rsf_data_current sum_period
                                        where sum_period.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
                                        	and sum_period.indicator_id = any(select unnest(string_to_array($2::text,','))::int)
                                          and sum_period.reporting_asof_date <= $3::date
                                          and public.isnumeric(sum_period.data_value) = true
                                        order by 
                                          sum_period.rsf_pfcbl_id,
                                          sum_period.indicator_id,
                                          sum_period.reporting_asof_date desc",
                                        params=list(paste0(unique(pfcbl_ids$rsf_pfcbl_id),collapse=","),
                                                    paste0(query_mms_indicators_previous[indicator_variable_class=="sum.previous",unique(indicator_id)],collapse=","),
                                                    program_previous_date))
            
            setDT(query_rsf_data)
            query_rsf_data[,
                           data_class:="sum.previous"]
            
            mms_rsf_data <- rbindlist(list(mms_rsf_data,
                                           query_rsf_data))
            query_rsf_data <- NULL
          }
        }
        
        mms_rsf_data[,
                     `:=`(reporting_asof_date=reporting_current_date,
                          data_value_changed=as.logical(NA),
                          data_value_updated=as.logical(NA),
                          reportnumber=as.logical(NA))]
        setcolorder(mms_rsf_data,
                    neworder=names(rsf_data))
        rsf_data <- rbindlist(list(rsf_data,
                                   mms_rsf_data))
      }
      #issueances variable_class is not a true base class:
      #issuances.current and issuances.previous have different rules
      if (any(query_indicators$indicator_variable_class=="issuances.current")) {
        query_indicator_ids <- query_indicators[indicator_variable=="issuances.current",unique(indicator_id)]
        query_issuance_ids <- pfcbl_ids[!is.na(loan_issuance_series_id),rsf_pfcbl_id]
        
        
        query_rsf_data <- dbGetQuery(pool,"
                                      select 
                                      	end_period.reporting_asof_date as data_asof_date,
                                      	lis.rsf_pfcbl_id,
                                      	end_period.data_id,
                                      	ind.indicator_id,
                                      	end_period.data_value,
                                      	end_period.data_unit
                                      	
                                      from p_rsf.rsf_loan_issuance_series lis
                                      inner join lateral (select c_lis.rsf_pfcbl_id as current_rsf_pfcbl_id
                                                          from p_rsf.rsf_loan_issuance_series c_lis
                                      										where c_lis.loan_issuance_series_id = lis.loan_issuance_series_id
                                      										order by c_lis.loan_issuance_series_rank desc
                                      										limit 1) current_lis on true
                              				inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = current_lis.current_rsf_pfcbl_id
                                      inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
                                      inner join lateral (select
                                                            rdc.data_id,
                                      											rdc.reporting_asof_date,
                                      											rdc.data_value,
                                      											rdc.data_unit
                                      										from p_rsf.rsf_data_current rdc
                                      										where rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id
                                      										  and rdc.indicator_id = ind.indicator_id
                                      											and rdc.reporting_asof_date <= $3::date
                                      										order by 
                                      										  rdc.reporting_asof_date desc
                                      										limit 1) end_period on true
                                      where lis.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
                                      	and ind.indicator_id = any(select unnest(string_to_array($2::text,','))::int)",
                               params=list(paste0(unique(pfcbl_ids$rsf_pfcbl_id),collapse=","),
                                           paste0(query_indicator_ids,collapse=","),
                                           reporting_current_date))
        
        setDT(query_rsf_data)
        query_rsf_data[,data_class:="issuances.current"]
        
        query_rsf_data[,
                       `:=`(reporting_asof_date=reporting_current_date,
                            data_value_changed=as.logical(NA),
                            data_value_updated=(data_asof_date==reporting_current_date),
                            reportnumber=as.numeric(NA))]
        
        
        setcolorder(query_rsf_data,neworder=names(rsf_data))
        rsf_data <- rbindlist(list(rsf_data,
                                   query_rsf_data))
        
        query_rsf_data <- NULL
      }
      if (any(query_indicators$indicator_variable_class=="issuances.previous")) {
        
        query_indicator_ids <- query_indicators[indicator_variable=="issuances.previous",unique(indicator_id)]
        query_issuance_ids <- pfcbl_ids[!is.na(loan_issuance_series_id),rsf_pfcbl_id]
        
        
        query_rsf_data <- dbGetQuery(pool,"
                                      select 
                                      	end_period.reporting_asof_date as data_asof_date,
                                      	lis.rsf_pfcbl_id,
                                      	end_period.data_id,
                                      	ind.indicator_id,
                                      	end_period.data_value,
                                      	end_period.data_unit
                                      	
                                      from p_rsf.rsf_loan_issuance_series lis
                                      inner join lateral (select c_lis.rsf_pfcbl_id as previous_rsf_pfcbl_id
                                                  from p_rsf.rsf_loan_issuance_series c_lis
                              										where c_lis.loan_issuance_series_id = lis.loan_issuance_series_id
                              										  and c_lis.loan_issuance_series_rank < lis.loan_issuance_series_rank
                              										order by c_lis.loan_issuance_series_rank desc
                              										limit 1) previous_lis on true
                              				inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = previous_lis.previous_rsf_pfcbl_id
                                      inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
                                      inner join lateral (select
                                                            rdc.data_id,
                                      											rdc.reporting_asof_date,
                                      											rdc.data_value,
                                      											rdc.data_unit
                                      										from p_rsf.rsf_data_current rdc
                                      										where rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id
                                      										  and rdc.indicator_id = ind.indicator_id
                                      											and rdc.reporting_asof_date <= $3::date
                                      										order by 
                                      										  rdc.reporting_asof_date desc
                                      										limit 1) end_period on true
                                      where lis.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
                                      	and ind.indicator_id = any(select unnest(string_to_array($2::text,','))::int)",
                                     params=list(paste0(unique(pfcbl_ids$rsf_pfcbl_id),collapse=","),
                                                 paste0(query_indicator_ids,collapse=","),
                                                 reporting_current_date))

        setDT(query_rsf_data)
        query_rsf_data[,data_class:="issuances.previous"]
        
        query_rsf_data[,
                       `:=`(reporting_asof_date=reporting_current_date,
                            data_value_changed=as.logical(NA),
                            data_value_updated=(data_asof_date==reporting_current_date),
                            reportnumber=as.numeric(NA))]
        
        
        setcolorder(query_rsf_data,neworder=names(rsf_data))
        rsf_data <- rbindlist(list(rsf_data,
                                   query_rsf_data))
        
        query_rsf_data <- NULL
      }
      
      
      #First should probably be obsoleted: is it really useful?
      # > lookup entity creation date with sys_entity_reporting.first.reporting_date ?
      if (any(query_indicators$indicator_variable_class=="first")) {
        
        query_indicator_ids <- query_indicators[indicator_variable_class=="first",unique(indicator_id)]
        
        #note: first reporting may be a default/blank entry entered on entity creation and may not strictly reflect the first value reported by the entity
        query_rsf_data <- dbGetQuery(pool,"
                                      select 
                                      	first_period.reporting_asof_date as data_asof_date,
                                      	ids.rsf_pfcbl_id,
                                      	first_period.data_id,
                                      	ind.indicator_id,
                                      	first_period.data_value,
                                      	first_period.data_unit
                                      from p_rsf.rsf_pfcbl_ids ids
                                      inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
                                      inner join lateral (select
                                                            rdc.data_id,
                                      											rdc.reporting_asof_date,
                                      											rdc.data_value,
                                      											rdc.data_unit
                                      										from p_rsf.rsf_data_current rdc
                                      										inner join p_rsf.rsf_data rd on rd.data_id = rdc.data_id
                                      										where rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id
                                      										  and rdc.indicator_id = ind.indicator_id
                                      											and rdc.reporting_asof_date <= $3::date
                                      										order by 
                                                            rdc.data_value is not null desc,
                                      										  rdc.reporting_asof_date asc         -- order by asc instead of desc, because first
                                      										limit 1) first_period on true
                                      where ids.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
                                      	and ind.indicator_id = any(select unnest(string_to_array($2::text,','))::int)",
                                     params=list(paste0(unique(pfcbl_ids$rsf_pfcbl_id),collapse=","),
                                                 paste0(query_indicator_ids,collapse=","),
                                                 reporting_current_date))
        
        setDT(query_rsf_data)
        query_rsf_data[,data_class:="first"]
        
        query_rsf_data[,
                       `:=`(reporting_asof_date=reporting_current_date,
                            data_value_changed=as.logical(NA),
                            data_value_updated=as.logical(TRUE),
                            reportnumber=as.numeric(NA))]
        
        
        setcolorder(query_rsf_data,neworder=names(rsf_data))
        rsf_data <- rbindlist(list(rsf_data,
                                   query_rsf_data))
        
        query_rsf_data <- NULL
      }

      if (any(query_indicators$indicator_variable_class=="all")) {
        
        query_indicator_ids <- query_indicators[indicator_variable_class=="all",unique(indicator_id)]
        query_rsf_data <- dbGetQuery(pool,"
                                     select 
                                      	rdc.reporting_asof_date as data_asof_date,
                                      	rdc.rsf_pfcbl_id,
                                      	rdc.data_id,
                                      	rdc.indicator_id,
                                      	rdc.data_value,
                                      	rdc.data_unit
                                      from p_rsf.rsf_pfcbl_ids ids
                                      inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
                                      inner join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id
                                                                           and rdc.indicator_id = ind.indicator_id
                                                                           and rdc.reporting_asof_date <= $3::date
                                      where ids.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
                                      	and ind.indicator_id = any(select unnest(string_to_array($2::text,','))::int)",
                                     params=list(paste0(unique(pfcbl_ids$rsf_pfcbl_id),collapse=","),
                                                 paste0(query_indicator_ids,collapse=","),
                                                 reporting_current_date))
        
        setDT(query_rsf_data)
        query_rsf_data[,data_class:="all"]
        query_rsf_data[,
                       `:=`(reporting_asof_date=reporting_current_date,
                            data_value_changed=as.logical(NA),
                            data_value_updated=as.logical(TRUE),
                            reportnumber=as.numeric(NA))]
        
        
        setcolorder(query_rsf_data,neworder=names(rsf_data))
        rsf_data <- rbindlist(list(rsf_data,
                                   query_rsf_data))
        
        query_rsf_data <- NULL
      }
    }
    
    #info indicators
    {
      if (any(query_indicators$indicator_variable_class=="info")) {
        
        status_ids <- pfcbl_ids[pfcbl_indicators[query_indicators[indicator_variable_class=="info",
                                                                  .(indicator_id,indicator_variable)],
                                                 .(indicator_id,
                                                   indicator_sys_category,
                                                   data_unit,
                                                   data_class=indicator_variable,
                                                   pfcbl_category=data_category),
                                                 on=.(indicator_id),
                                                 nomatch=NULL],
                                .(rsf_pfcbl_id,
                                  indicator_id,
                                  data_unit,
                                  data_class),
                                on=.(pfcbl_category),
                                nomatch=NULL,
                                by=.EACHI
                                ][,
                                  .(rsf_pfcbl_id,
                                    indicator_id,
                                    data_unit,
                                    data_class)]
        
        if (!empty(status_ids)) {
          if (any(query_indicators$indicator_variable == "info.status")) {
  
            info.status <- status_ids[data_class=="info.status"]
            
            
            status_data <- dbGetQuery(pool,"
                                   select 
                                    ids.rsf_pfcbl_id, 
                                    quarter_end_reporting_status
                                  from p_rsf.rsf_pfcbl_ids ids
                                  left join lateral p_rsf.get_rsf_pfcbl_id_reporting_status_asof_date(ids.rsf_pfcbl_id,
                                                                                                      ids.pfcbl_category::text,
                                                                                                      $2::date) status on true
                                  where ids.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)",
                                     params=list(paste0(unique(info.status$rsf_pfcbl_id),collapse=","),
                                                 reporting_current_date))
            
            setDT(status_data)
            status_data[,reporting_asof_date:=reporting_current_date]
            status_data <- status_data[info.status,
                                       on=.(rsf_pfcbl_id),
                                       nomatch=NULL]
            
            if (!empty(status_data)) {
              status_data <- status_data[,
                                         .(reporting_asof_date,
                                           rsf_pfcbl_id,
                                           data_id=as.numeric(NA),
                                           indicator_id,
                                           data_value=as.character(NA),
                                           data_unit=quarter_end_reporting_status,
                                           data_class,
                                           data_value_changed=as.numeric(NA),
                                           data_value_updated=as.numeric(NA),
                                           data_asof_date=reporting_asof_date,
                                           reportnumber=as.numeric(NA))]
            
              rsf_data <- rbindlist(list(rsf_data,
                                         status_data))
            }
            info.status <- NULL
            status_data <- NULL
          }
          
          if (any(query_indicators$indicator_variable == "info.name")) {
            
            info.name <- status_ids[data_class=="info.name"]
            
            
            status_data <- dbGetQuery(pool,"
                                   select 
                                    nids.rsf_pfcbl_id,
                                    nids.rsf_full_name
                                  from p_rsf.view_current_entity_names_and_ids nids
                                  where nids.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)",
                                      params=list(paste0(unique(info.name$rsf_pfcbl_id),collapse=",")))
            
            setDT(status_data)
            status_data <- status_data[info.name,
                                       on=.(rsf_pfcbl_id),
                                       nomatch=NULL]
            
            if (!empty(status_data)) {
              status_data <- status_data[,
                                         .(reporting_asof_date=reporting_current_date,
                                           rsf_pfcbl_id,
                                           data_id=as.numeric(NA),
                                           indicator_id,
                                           data_value=as.character(NA),
                                           data_unit=rsf_full_name, #using data_unit to carry the value as it's guaranteed character and wont throw NAs introduced by coercion warning
                                           data_class,
                                           data_value_changed=as.numeric(NA),
                                           data_value_updated=as.numeric(NA),
                                           data_asof_date=reporting_current_date,
                                           reportnumber=as.numeric(NA))]
            
              rsf_data <- rbindlist(list(rsf_data,
                                         status_data))
            }
            info.name <- NULL
            status_data <- NULL
          }
          
          if (any(query_indicators$indicator_variable == "info.computationdate")) {
            
            status_data <- status_ids[data_class=="info.computationdate"]
            
            status_data <- status_data[,
                                       .(reporting_asof_date=reporting_current_date,
                                         rsf_pfcbl_id,
                                         data_id=as.numeric(NA),
                                         indicator_id,
                                         data_value=as.character(NA), #because info.reportingdate takes reporting_asof_date as its value
                                         data_unit,
                                         data_class,
                                         data_value_changed=as.numeric(NA),
                                         data_value_updated=as.numeric(FALSE),
                                         data_asof_date=reporting_current_date,
                                         reportnumber=as.numeric(NA))]
            
            rsf_data <- rbindlist(list(rsf_data,
                                       status_data))
            status_data <- NULL
          }
          
          if (any(query_indicators$indicator_variable == "info.createddate")) {
            
            status_data <- status_ids[data_class=="info.createddate"]
            
            status_data[pfcbl_ids,
                        created_in_reporting_asof_date:=i.created_in_reporting_asof_date,
                        on=.(rsf_pfcbl_id)]
            
            status_data <- status_data[,
                                       .(reporting_asof_date=reporting_current_date,
                                         rsf_pfcbl_id,
                                         data_id=as.numeric(NA),
                                         indicator_id,
                                         data_value=as.character(NA), 
                                         data_unit,
                                         data_class,
                                         data_value_changed=as.numeric(NA),
                                         data_value_updated=as.numeric(FALSE),
                                         data_asof_date=created_in_reporting_asof_date,
                                         reportnumber=as.numeric(NA))]
            
            rsf_data <- rbindlist(list(rsf_data,
                                       status_data))
            status_data <- NULL
          }
        }
      }
    }
    #static indicators
    {
      static_indicators <- pfcbl_indicators[is_static_nonreporting==TRUE & indicator_id %in% query_indicators$indicator_id,
                                           .(indicator_id,
                                             indicator_sys_category,
                                             data_unit,
                                             data_class="current",
                                             pfcbl_category=data_category)]
      
      if (any(static_indicators$indicator_sys_category == "issuance_id")) {
        static_data <- pfcbl_ids[static_indicators[indicator_sys_category == "issuance_id",
                                              .(indicator_id,
                                                pfcbl_category,
                                                data_unit,
                                                data_class)],
                             on=.(pfcbl_category),
                             nomatch=NULL]
        
        if (!empty(static_data)) {
          static_data <- static_data[,
                                     .(reporting_asof_date=reporting_current_date,
                                       rsf_pfcbl_id,
                                       data_id=as.numeric(NA),
                                       indicator_id,
                                       data_value=as.character(loan_issuance_series_id),
                                       data_unit=data_unit,
                                       data_class,
                                       data_value_changed=as.numeric(NA),
                                       data_value_updated=as.numeric(NA),
                                       data_asof_date=as.Date(as.numeric(NA)),
                                       reportnumber=as.numeric(NA))]

        }
        static_data <- NULL
      }
      
      if (any(static_indicators$indicator_sys_category == "issuance_rank")) {
        static_data <- pfcbl_ids[static_indicators[indicator_sys_category == "issuance_rank",
                                                 .(indicator_id,
                                                   pfcbl_category,
                                                   data_unit,
                                                   data_class)],
                                on=.(pfcbl_category),
                                nomatch=NULL]
        
        if (!empty(static_data)) {
          
          static_data <- static_data[,
                                    .(reporting_asof_date=reporting_current_date,
                                      rsf_pfcbl_id,
                                      data_id=as.numeric(NA),
                                      indicator_id,
                                      data_value=as.character(loan_issuance_series_rank),
                                      data_unit=data_unit,
                                      data_class,
                                      data_value_changed=as.numeric(NA),
                                      data_value_updated=as.numeric(NA),
                                      data_asof_date=as.Date(as.numeric(NA)),
                                      reportnumber=as.numeric(NA))]

          rsf_data <- rbindlist(list(rsf_data,
                                     static_data))
        }
        static_data <- NULL
      }
    }
    
    #update reporting_current_date and indicator_names etc
    {
      #The very first data entry is neither a change nor non-change, just NA: cannot be changed or updated in reference to non-existent entry before
      #however it is an update
      # rsf_data[pfcbl_ids,
      #          `:=`(data_value_changed=as.numeric(NA),
      #               data_value_updated=TRUE),
      #          on=.(rsf_pfcbl_id,
      #               reporting_asof_date=created_in_reporting_asof_date)] #reporting_asof_date == reporting_current_date, so in relation to now, is
                       
      #FLOW DATA
      #this the first data reported?
      rsf_data[
               data_value_updated==FALSE          & #and nothing submitted this reporting period
               indicator_id %in% pfcbl_indicators[is_periodic_or_flow_reporting==TRUE,
                                                  indicator_id], #and its a flow indicator
               data_value:=as.character(NA)] #then zero it out because last-reported value will not be a valid assumption. No change is valid assumption
      
      rsf_data[,reporting_current_date:=reporting_current_date]
      
      rsf_data[pfcbl_indicators,
               `:=`(indicator_name=i.indicator_name,
                    data_type=i.data_type,
                    data_category=i.data_category),
               on=.(indicator_id)]

      #2023-OCT: Deprecated.
      #Too much time to concatenate it and then str_split currency units.
      #Instead, forcing it to just query the unit values as separate indicator columns!
      
      # rsf_data[data_type %in% c("currency"), #currency_ratio
      #          data_value:=stringr::str_c(data_value,data_unit)] #stringr::str_c is slightly faster than paste0 and it has "contageous NAs"
      #attempts to use units:: package and even custom money object as list or S4, etc are very slow
      #compared to string concatenation and subsequent splitting.
      
      setcolorder(rsf_data,neworder=c("reporting_current_date",
                                      "reporting_asof_date",
                                      "rsf_pfcbl_id",
                                      "data_id",
                                      #"reporting_sequence_rank",
                                      "indicator_id",
                                      "indicator_name",
                                      "data_type",
                                      "data_category",
                                      "data_value",
                                      "data_unit",
                                      "data_class",
                                      "data_asof_date",
                                      "data_value_changed",
                                      "data_value_updated",
                                      "reportnumber"))
      
    }
    #Defaults: requests for indicators that program isn't subscribed to (can happen when chacks require an input variable that program doesn't track--use default)
    {
      #missing_indicators <- indicator_variables[!indicator_variables %in% rsf_data$indicator_id]
      missing_indicators <- query_indicators[!indicator_id %in% rsf_data$indicator_id,indicator_id]
      if (length(missing_indicators) > 0 & missing_indicators.use_default==TRUE) {
        entities <- pfcbl_family_long[,
                                      .(data_category=gsub("^rsf_pfcbl_id\\.(.*)$","\\1",rsf_pfcbl_id.category),
                                        rsf_pfcbl_id)] 
        entities <- unique(entities)
        default_values <- entities[pfcbl_indicators[indicator_id %in% missing_indicators,
                                                   .(indicator_id,
                                                     indicator_name,
                                                     data_type,
                                                     data_category,
                                                     data_value=default_value,
                                                     data_unit)][unique(query_indicators[,.(indicator_id,
                                                                                            data_class=indicator_variable_class)]),
                                                                 on=.(indicator_id),
                                                                 nomatch=NULL],
                                   .(rsf_pfcbl_id,
                                     indicator_id,
                                     indicator_name,
                                     data_type,
                                     data_value,
                                     data_unit,
                                     data_class),
                                   on=.(data_category),
                                   by=.EACHI,
                                   nomatch=NULL]
        
        non_issuance_entities <- pfcbl_ids[is.na(loan_issuance_series_id)==TRUE,.(rsf_pfcbl_id)]
        non_issuance_entities <- rbindlist(list(non_issuance_entities[,.(rsf_pfcbl_id,data_class="issuances.current")],
                                                non_issuance_entities[,.(rsf_pfcbl_id,data_class="issuances.previous")]))
        default_values[non_issuance_entities,
                       data_value:=as.character(NA),
                       on=.(rsf_pfcbl_id,
                            data_class)]
        
        default_values[,reporting_asof_date:=as.Date(reporting_current_date)]
        default_values[,reporting_current_date:=as.Date(reporting_asof_date)]
        default_values[,`:=`(data_id=as.integer(NA),
                               data_asof_date=as.Date(as.numeric(NA)),
                               data_value_changed=as.logical(NA),
                               data_value_updated=as.logical(FALSE),
                               reportnumber=0)]
        
        setcolorder(default_values,
                    neworder=names(rsf_data))
        rsf_data <- rbindlist(list(rsf_data,
                                   default_values))
      }
    }
    
    #Missing data can generate duplicates when missing indicators AND missing entities both occur
    {
      missing_entities <- pfcbl_family_long[rsf_pfcbl_id < 0] 
      if (!empty(missing_entities)) {
        setnames(missing_entities,
                 old="rsf_pfcbl_id.category",
                 new="data_category")
        
        missing_entities[,data_category:=gsub("^rsf_pfcbl_id\\.(.*)$","\\1",data_category)]
        
        
        missing_entities <- missing_entities[pfcbl_indicators[,.(indicator_id,
                                                                 indicator_name,
                                                                 data_type,
                                                                 data_category,
                                                                 data_value=default_value,
                                                                 data_unit)][unique(query_indicators[,.(indicator_id,
                                                                                                        data_class=indicator_variable_class)]),
                                                                             on=.(indicator_id),
                                                                             nomatch=NULL],
                                             .(rsf_pfcbl_id,
                                               indicator_id,
                                               indicator_name,
                                               data_type,
                                               data_value,
                                               data_unit,
                                               data_class),
                                             on=.(data_category),
                                             by=.EACHI,
                                             nomatch=NULL]
        
        missing_entities[,reporting_asof_date:=as.Date(reporting_current_date)]
        missing_entities[,reporting_current_date:=reporting_asof_date]
        missing_entities[,`:=`(data_id=as.integer(NA),
                               data_asof_date=as.Date(as.numeric(NA)),
                               data_value_changed=as.logical(NA),
                               data_value_updated=as.logical(FALSE),
                               reportnumber=0)]
        
        setcolorder(missing_entities,
                    neworder=names(rsf_data))
        
        missing_entities[,duplicated:=FALSE]
        missing_entities[rsf_data,
                         duplicated:=TRUE,
                         on=.(rsf_pfcbl_id,
                              indicator_id,
                              reporting_asof_date,
                              data_id)]
        missing_entities <- missing_entities[duplicated==FALSE]
        missing_entities[,duplicated:=NULL]
        
        rsf_data <- rbindlist(list(rsf_data,
                                   missing_entities))
      }
    }
    
    #finalize rsf_data
    {
      rsf_data[pfcbl_indicators[is.na(default_value) == FALSE,
                                .(indicator_id,
                                  default_value,
                                  joincondition=as.character(NA))],
               `:=`(data_value=i.default_value,
                    data_value_changed=FALSE), #a default value also counts as a non-change, like a first entry.
               on=.(indicator_id,
                    data_value=joincondition)]
      
      setorder(rsf_data,
               reporting_current_date,
               rsf_pfcbl_id,
               indicator_id,
               data_id,
               -reporting_asof_date)
    }
    
    #pfcbl_reporting
    {
      
      #Create family_reporting including reporting.status columns, if applicable
      {
        family_reporting <- pfcbl_family_long[,
                                              .(family_id,
                                                 rsf_pfcbl_id,
                                                 reporting_current_date=reporting_current_date)]

        family_reporting <- unique(family_reporting[,.(family_id,reporting_current_date)])
        
      }

      if (nrow(pfcbl_family) != nrow(family_reporting)) {
        #browser()
        #abandoned_cohorts <- dbGetQuery(pool,"select distinct created_by_reporting_cohort_id from p_rsf.error_check_view_abandoned_rsf_ids")
        print(paste0("rsf_program_id=",rsf_program_id))
        print(paste0("reporting_current_date=",reporting_current_date))
        print(paste0("indicator_variables: ",paste0(paste0(names(indicator_variables),"=",indicator_variables),collapse=", ")))
        print(paste0("for_rsf_pfcbl_ids=",paste0(for_rsf_pfcbl_ids,collapse=",")))
        stop(paste0("nrow(pfcbl_family) * nrow(reporting_dates) != nrow(family_reporting)) : ",nrow(pfcbl_family)," vs ",nrow(family_reporting)," ",
                    "Probable cause: a template upload failed to upload completely and has left partial data. Try deleting Upload# in Datasets listing for #",
                    paste0(abandoned_cohorts,collapse=", "),". Contact SYS Admin to run view_abandoned_rsf_ids_error_check to help resolve."))
      }
    }
    
    #if(SYS_PRINT_TIMING)  debugtime("db_program_get_data","rsf_data setup completed")
    #Data formatting for current vs history
  }
  
  #if(SYS_PRINT_TIMING)  debugtime("db_program_get_data","data formatting start")

  if (!empty(query_indicators)) {
    
    rsf_data_types <- list()
    rsf_data_types[[1]] <- c("number",
                             "percent",
                             "currency_ratio",
                             "currency")
    
    rsf_data_types[[2]] <- c("date")
    rsf_data_types[[3]] <- c("logical")
    
    rsf_data_types[[4]] <- c("text") #text because set to data_value,data_unit character concatenation
    
    #check_types <- which(sapply(rsf_data_types,function(x,dtypes) any(x %in% dtypes),dtypes=unique(pfcbl_indicators$data_type)))
    #rsf_data_types <- rsf_data_types[check_types]
    #i<-1
    #i<-4
    for (i in 1:length(rsf_data_types)) {
      
      current_data_type <- rsf_data_types[[i]]
      if (!any(current_data_type %in% pfcbl_indicators$data_type)) next;
      
      type_rsf_data <- rsf_data[data_type %in% current_data_type]
      if (empty(type_rsf_data)) next;
      
      #Set all the data to the current typed value
      {
        if (any(current_data_type %in% c("number",
                                         "percent",
                                         "currency_ratio",
                                         "currency"))) {

          set(type_rsf_data,i=NULL,j="data_value",value=as.numeric(type_rsf_data$data_value))
          
        } else if (any(current_data_type %in% "logical")) {
          set(type_rsf_data,i=NULL,j="data_value",value=as.logical(type_rsf_data$data_value))
          
        } else if (any(current_data_type %in% "date")) {
          set(type_rsf_data,i=NULL,j="data_value",value=as.Date(type_rsf_data$data_value)) #nope...as.Date to permit formula and check calculations
          
        } else if (any(current_data_type %in% c("text"))) {
          #text does not require anything
          #and currency,currency_ratio were set to text along with its currency unit in data query bloc
          NULL;

        } else {
          stop(paste0("Unrecognized data_type: ",current_data_type))
        }
      }
      
      type_rsf_data_indicator_ids <- type_rsf_data[,unique(indicator_id)]
      #current_indicator_variables <- query_indicators[indicator_id %in% type_rsf_data_indicator_ids]
      
      current_indicators <- query_indicators[indicator_id %in% type_rsf_data_indicator_ids]
      if (empty(current_indicators)) next;
      
      current_indicator_variables <- unique(current_indicators$indicator_variable)
      
      #current_type_data_classes <- type_rsf_data[,unique(data_class)]
      #current_indicator_variables <- data.table(indicator_id=current_indicator_variables,variable_group=names(current_indicator_variables))
      #current_indicator_variable_groups <- current_indicator_variables[,unique(variable_group)]
      #current_variable <- current_indicator_variables[[1]]
      #current_variable <- "sum.previous.unit"
      #current_variable <- "current.reporteddate"
      for (current_variable in current_indicator_variables) {
        
        
        # current_indicator_ids <- unique(current_indicator_variables[variable_group==current_variable_group])
        current_rsf_data <- type_rsf_data[current_indicators[indicator_variable==current_variable,.(indicator_id)],
                                          on=.(indicator_id),
                                          nomatch=NULL]
        
      
        
        current_rsf_entities_and_indicators <- unique(current_rsf_data[,.(rsf_pfcbl_id,indicator_name,reporting_current_date)])
        
        atts_data <- NULL
        #create data attributes and join onto family
        {
  
          if (current_variable=="current") {
            atts_data <- current_rsf_data[data_class=="current",
                                          .(current=data_value),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="current.changed") {
            atts_data <- current_rsf_data[data_class=="current",
                                          .(current.changed=data_value_changed),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="current.updated") {
            atts_data <- current_rsf_data[data_class=="current",
                                          .(current.updated=data_value_updated),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="current.reporteddate") { #reportEDdate
            atts_data <- current_rsf_data[data_class=="current",
                                          .(current.reporteddate=data_asof_date),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="current.unit") {
            atts_data <- current_rsf_data[data_class=="current",
                                          .(current.unit=data_unit),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
  
          } else if (current_variable=="current.id") {  #obsolete
            atts_data <- current_rsf_data[data_class=="current",
                                          .(current.id=data_id),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="current.reportnumber") {
            #special data_class
            atts_data <- current_rsf_data[data_class=="current",
                                          .(current.reportnumber=reportnumber),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="previous") {
            atts_data <- current_rsf_data[data_class=="previous",
                                          .(previous=data_value),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="previous.unit") { 
           atts_data <- current_rsf_data[data_class=="previous",
                                         .(previous.unit=data_unit),
                                         by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="previous.reporteddate") {
            atts_data <- current_rsf_data[data_class=="previous",
                                          .(previous.reporteddate=data_asof_date),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="min.current") {
            atts_data <- current_rsf_data[data_class=="min.current",
                                          .(min.current=data_value),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="min.current.unit") {
            atts_data <- current_rsf_data[data_class=="min.current",
                                          .(min.current.unit=data_unit),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="min.current.reporteddate") {
            atts_data <- current_rsf_data[data_class=="min.current",
                                          .(min.current.reporteddate=data_asof_date),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
          } else if (current_variable=="max.current") {
            atts_data <- current_rsf_data[data_class=="max.current",
                                          .(max.current=data_value),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="max.current.unit") {
            atts_data <- current_rsf_data[data_class=="max.current",
                                          .(max.current.unit=data_unit),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="max.current.reporteddate") {
            atts_data <- current_rsf_data[data_class=="max.current",
                                          .(max.current.reporteddate=data_asof_date),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
          } else if (current_variable=="sum.current") {
            atts_data <- current_rsf_data[data_class=="sum.current",
                                          .(sum.current=data_value),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="sum.current.unit") {
            atts_data <- current_rsf_data[data_class=="sum.current",
                                          .(sum.current.unit=data_unit),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="sum.current.reporteddate") {
            atts_data <- current_rsf_data[data_class=="sum.current",
                                          .(sum.current.reporteddate=data_asof_date),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
          } else if (current_variable=="min.previous") {
            atts_data <- current_rsf_data[data_class=="min.previous",
                                          .(min.previous=data_value),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="min.previous.unit") {
            atts_data <- current_rsf_data[data_class=="min.previous",
                                          .(min.previous.unit=data_unit),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="min.previous.reporteddate") {
            atts_data <- current_rsf_data[data_class=="min.previous",
                                          .(min.previous.reporteddate=data_asof_date),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
          } else if (current_variable=="max.previous") {
            atts_data <- current_rsf_data[data_class=="max.previous",
                                          .(max.previous=data_value),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="max.previous.unit") {
            atts_data <- current_rsf_data[data_class=="max.previous",
                                          .(max.previous.unit=data_unit),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="max.previous.reporteddate") {
            atts_data <- current_rsf_data[data_class=="max.previous",
                                          .(max.previous.reporteddate=data_asof_date),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
          } else if (current_variable=="sum.previous") {
            atts_data <- current_rsf_data[data_class=="sum.previous",
                                          .(sum.previous=data_value),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="sum.previous.unit") {
            atts_data <- current_rsf_data[data_class=="sum.previous",
                                          .(sum.previous.unit=data_unit),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="sum.previous.reporteddate") {
            atts_data <- current_rsf_data[data_class=="sum.previous",
                                          .(sum.previous.reporteddate=data_asof_date),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
          } else if (current_variable=="first") {
            atts_data <- current_rsf_data[data_class=="first",
                                          .(first=data_value),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="first.unit") { 
            atts_data <- current_rsf_data[data_class=="first",
                                          .(first.unit=data_unit),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="first.reporteddate") {
            atts_data <- current_rsf_data[data_class=="first",
                                          .(first.reporteddate=data_asof_date),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
          
         
          } else if (current_variable=="issuances.current") {
            atts_data <- current_rsf_data[data_class=="issuances.current",
                                          .(issuances.current=data_value),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="issuances.current.unit") {
            atts_data <- current_rsf_data[data_class=="issuances.current",
                                          .(issuances.current.unit=data_unit),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="issuances.current.reporteddate") {
            atts_data <- current_rsf_data[data_class=="issuances.current",
                                          .(issuances.current.reporteddate=data_asof_date),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="issuances.previous") {
            atts_data <- current_rsf_data[data_class=="issuances.previous",
                                          .(issuances.previous=data_value),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="issuances.previous.unit") {
            atts_data <- current_rsf_data[data_class=="issuances.previous",
                                          .(issuances.previous.unit=data_unit),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="issuances.previous.reporteddate") {
            atts_data <- current_rsf_data[data_class=="issuances.previous",
                                          .(issuances.previous.reporteddate=data_asof_date),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          }else if (current_variable=="all") {
            atts_data <- current_rsf_data[data_class=="all",
                                          .(all=list(data_value)),
                                          by=.(reporting_current_date,
                                               rsf_pfcbl_id,
                                               indicator_name)]
            
          } else if (current_variable=="all.reporteddates") {
            atts_data <- current_rsf_data[data_class=="all.reporteddates",
                                          .(all.reporteddates=list(data_asof_date)),
                                          by=.(reporting_current_date,
                                               rsf_pfcbl_id,
                                               indicator_name)]
            
          } else if (current_variable=="info.computationdate") {
            atts_data <- current_rsf_data[data_class=="info.computationdate",
                                          .(info.computationdate=reporting_asof_date),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="info.createddate") {
            atts_data <- current_rsf_data[data_class=="info.createddate",
                                          .(info.createddate=data_asof_date),
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="info.status") {
            atts_data <- current_rsf_data[data_class=="info.status",
                                          .(info.status=data_unit), #using data unit to carry value in query above, as if indicator data type is 
                                                                     #numeric, date, etc then casting the text value will throw errors
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else if (current_variable=="info.name") {
            atts_data <- current_rsf_data[data_class=="info.name",
                                          .(info.name=data_unit), #using data unit to carry value in query above, as if indicator data type is 
                                                                  #numeric, date, etc then casting the text value will throw errors
                                          by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            
          } else {
            warning(paste0("Invalid variable attribute: ",current_variable))
            next;
          }
        
          if (!all(nrow(atts_data)==nrow(unique(atts_data[,.(reporting_current_date,rsf_pfcbl_id,indicator_name)])))) {
            atts_data[,n:=.N,
                 by=.(reporting_current_date,rsf_pfcbl_id,indicator_name)]
            print(atts_data[n>1])
            stop(paste0("dim(atts_data) != dim(unique(atts_data)) for ",current_variable," for ",paste0(current_data_type,collapse=" & ")))
          }
          
          atts_data <- atts_data[pfcbl_family_long[current_rsf_entities_and_indicators,
                                                   .(family_id,
                                                     reporting_current_date,
                                                     indicator_name),
                                                   by=.EACHI,
                                                   on=.(rsf_pfcbl_id)],
                                 on=.(reporting_current_date,rsf_pfcbl_id,indicator_name)] #[,mget(atts_names)]
          
          atts_data <- tryCatch({
                        dcast.data.table(atts_data,
                                        family_id + reporting_current_date ~ paste0(indicator_name,".",current_variable),
                                        value.var=c(current_variable),
                                        fun.aggregate = NULL) 
            
            
            # dcast.data.table(atts_data,
            #                  family_id + reporting_current_date ~ paste0(indicator_name,".",current_variable),
            #                  value.var=c(current_variable),
            #                  fun.aggregate = list) 
                       },
                       warning = function(w) { 
                         stop(paste0("dcast.data.table failed match equal dimensions: ",conditionMessage(w))) 
                         NULL
                       },
                       error = function(e) { 
                         stop(paste0("dcast.data.table failed match equal dimensions: ",conditionMessage(e))) 
                         NULL
                       },
                       message = function(m) { 
                         
                         stop(paste0("db_program_get_data() dcast.data.table failed match equal dimensions: ",conditionMessage(m))) 
                       })
          
          atts_names <- grep(paste0("^.*\\.",current_variable,"$"),names(atts_data),value = T)
          family_reporting[atts_data,
                           (atts_names):=mget(paste0("i.",atts_names)),
                           on=.(family_id,reporting_current_date)]
          
          atts_data <- NULL
          atts_names <- NULL
        }
      }  

      current_indicator_variables <- NULL
      current_rsf_entities_and_indicators <- NULL
      #current_indicator_variable_groups <- NULL
      type_rsf_data <- NULL
      
    }
    
  }

  #current.reportingcount has a default value of 1.
  if (any(query_indicators$indicator_variable=="current.reportingcount")) {
    for (reportingrank_col in grep("\\.current.reportingcount",names(family_reporting))) {
      set(family_reporting,
          i=which(is.na(family_reporting[[reportingrank_col]])),
          j=reportingrank_col,
          value=1)
    }
  }
  
  family_col_ids <- names(pfcbl_family)
  
  family_reporting[pfcbl_family,
                   (family_col_ids):=(mget(paste0("i.",family_col_ids))),
                   on=.(family_id)] #adds all the rsf_id and pfcbl_id cols
  
  setnames(family_reporting,old="family_id",new="reporting_group")

  rsf_data <- NULL
  
  #if(SYS_PRINT_TIMING)  debugtime("db_program_get_data","data formatting done")
  
  #Simplify columns and grab-back pfcbl_ids
  {
    requested_data_categories <- pfcbl_indicators[indicator_id %in% indicator_ids.keepfor,unique(data_category)]
    
    if (!any(requested_data_categories=="global")) {
      if (any(names(family_reporting)=="rsf_global_id")) family_reporting[,rsf_global_id:=NULL]
      if (any(names(family_reporting)=="rsf_pfcbl_id.global")) family_reporting[,rsf_pfcbl_id.global:=NULL] #was only added for joins and casts for potentially requested global indicators
                                                   #this is always zero and not meaningful data.
                                                   #unless we've actually requested global indicators, then we'll retain the placeholder.
    } else {
      family_reporting[,rsf_global_id:=0]
      family_reporting[,rsf_pfcbl_id.global:=0]
    }
    
    #If we requested Global then these will have been created as pseudo negative number IDs.  So correct them.
    if (rsf_program_id==0) {
      family_reporting[,rsf_pfcbl_id.program:=0]
      family_reporting[,rsf_program_id:=0]
      family_reporting[,rsf_global_id:=0]
    } else {
      family_reporting <- family_reporting[rsf_program_id >= 0]
    }
    
    # #sys_program_reporting should be (nearly) identical and relevant // invert=TRUE
    # notglobal_reporting <- grep("^sys_global_reporting\\..*",
    #                             names(family_reporting),
    #                             invert = T)
    # 
    # if (any(notglobal_reporting)) family_reporting <- family_reporting[,..notglobal_reporting]

    if (indicator_ids.simplify && !is.null(indicator_ids.keepfor)) {
      
      keepcategories <- unique(pfcbl_indicators[all(is.na(indicator_ids.keepfor)) | indicator_id %in% indicator_ids.keepfor,data_category])
      keepindicators <- unique(c(pfcbl_indicators[all(is.na(indicator_ids.keepfor)) | indicator_id %in% indicator_ids.keepfor,indicator_name]))
      
      if (any(keepcategories=="global") && !any(requested_data_categories=="global")) keepcategories <- keepcategories[-which(keepcategories=="global")]
      
      keep_pfcbl_ids <- paste0("rsf_pfcbl_id.",keepcategories)
      keepcols <- c("reporting_group",
                    "reporting_current_date",
                    paste0("rsf_",keepcategories,"_id"),
                    keep_pfcbl_ids)
      
      keepcols <- c(keepcols,
                    names(family_reporting)[unique(unlist(sapply(paste0("^",keepindicators,"\\..*$"),grep,x=names(family_reporting))))])
            
      missingkeepcols <- which(!keepcols %in% names(family_reporting))
      if (any(missingkeepcols)) keepcols <- keepcols[-missingkeepcols]
      
      family_reporting <- family_reporting[,..keepcols]
      
      if (nrow(family_reporting)==0) return (NA) #Simplified into nothing
      family_reporting <- unique(family_reporting,
                                 by=c("reporting_current_date",
                                      keep_pfcbl_ids))

    } 
    
    #This block should now be obsolete?
    {
      id_cols <- c(grep("^rsf_(global|program|facility|client|borrower|loan)_id$",names(family_reporting)),
                   grep("^rsf_pfcbl_id\\.(global|program|facility|client|borrower|loan)$",names(family_reporting)))
      
      for (id in id_cols) set(family_reporting,i=which(family_reporting[[id]] < 0),j=id,value=as.numeric(NA))
    }
    
    setorder(family_reporting,reporting_group,-reporting_current_date)    
    cnames <- names(family_reporting)
    setcolorder(family_reporting,order(rsf_colranks(cnames),cnames))
    
  }
  
  # calc_time <- round(as.numeric(Sys.time()-t1,"secs"),2)
  # if (calc_time > 5 & SYS_PRINT_TIMING) {
  #   debugtime("db_program_get_data","Long calc time:",calc_time,"s. ",
  #             "Total rows: ",nrow(family_reporting)," x ",ncol(family_reporting)," cols ")
  # }
  
  if(SYS_PRINT_TIMING) debugtime("db_program_get_data","Done!",format(Sys.time()-t1))
  
  return (family_reporting)
}
