
{
  CALCULATIONS_ENVIRONMENT <- new_environment(data=append(list(expr=expr,            #rlang
                                                               sym=sym,              #rlang
                                                               dots_list=dots_list  #rlang
                                                               ),
                                                          append(mget(ls(envir = pkg_env("lubridate")),pkg_env("lubridate")),
                                                                 append(mget(ls(envir = pkg_env("data.table")),pkg_env("data.table")),
                                                                        append(mget(ls(envir = pkg_env("stringr")),pkg_env("stringr")),
                                                                               mget(ls(envir = pkg_env("digest")),pkg_env("digest")))))
  ),   #RSF
  parent=base_env())
  

  #OBSOLETE 
  # #Data values are those variable types that carry the actual data value (ie, not meta data, like "changed").  This is only used by currency lookups
  # #since currency cares about the actual value and not about its metadata.
  # assign(x="indicator_ATTRIBUTES_DATA_VALUES",
  #        envir=CALCULATIONS_ENVIRONMENT,
  #        pos=0,
  #        value=c("current",
  #                "issuances.current",
  #                "issuances.previous",
  #                "previous",
  #                "first",
  #                "all"))
  
  assign(x="indicator_ATTRIBUTES",
         envir=CALCULATIONS_ENVIRONMENT,
         pos=0,
         value=c(
           #.info -- special category for internal and/or exogenously reported data
           #Like the current reporting date (whose lookup value will be triggered by sys_x_reporting)
           #Or system-assigned data, like the entity name or its issuance series information
           "info.status", #removed 20MAR2025 -- wasn't used by any formulas and relies on a separate database function call and is probably not accurate.
           "info.computationdate", #Date at which the computation is running, ie, reporting_current_date
           "info.createddate",
           "info.name",
           
           #.current is the most recently submitted value as-of the requested reporting_current_date
           "current",
           "current.changed", #if first reported, FALSE
                              #current data unit different that lagged data unit: true
                              #is a numeric,percent,currency and changes from 0 to NA or vice-versa: false (not a change)
                              #data value is different from lagged data value: true
           "current.updated",  #updated varies from changed in that updated in that a new data_id is is true, even if values are the same
                               #AND that first entry is true (an update).  Where as first entry is not a change (false).
           "current.reportnumber",
           #removed this.  Not used and not especially useful.  A system flag now checks for multiple reportings.
           #"current.reportingcount",  #effectively the number of intraperiod data points uploaded before the current data point plus 1. Ie, no intraperiod reporting has reportrank==1
           "current.reporteddate",
           "current.unit",
           
           #.previous is the value as-of the previous reporting period.  It is not the previously submitted value, unless user submitted different values over these two periods.
           "previous",
           "previous.reporteddate",
           "previous.unit",
           
           #Aggregate functions (note that "count" is equal to current.reportnumber)
           #Importantly, these are aggregates ACROSS TIME/REPORTING HISTORY
           "min.current",
           "max.current",
           "sum.current",
           "sum.current.reporteddate", #will be the currestest date
           "min.current.reporteddate",
           "max.current.reporteddate",
           "sum.current.unit",
           "min.current.unit",
           "max.current.unit",
           
           
                      
           "min.previous",
           "max.previous",
           "sum.previous",
           "min.previous.reporteddate",
           "max.previous.reporteddate",
           "sum.previous.reporteddate",
           "min.previous.unit",
           "max.previous.unit",
           "sum.previous.unit",
           
           
           #2023-08-04 - deprecated: nobody use, not a good argument to use; "intraperiod", #intermediate data can only exist in the same cohort as the "current" 
           #issuances will place the dataset sorted by reporting_asof_date,issuance_sequance_rank
           #If the client reporting is correct and no anachronistic reporting values are made, this will give a correct sequence of the issuance timeseries reporting
           #However, this assumtion is violated regularly.
           "issuances.current",  #Current reported value in the issuance sequence
           "issuances.current.unit",
           "issuances.current.reporteddate",
           #All but useless, except if a historic issuance reports anachronistically and can check if it is not the current one
           #When the entity is the most recent issuance, then .current and .issuances.current will return the same value
           "issuances.previous", #Previously reported issuance's value in the issuance sequence
           "issuances.previous.unit",
           "issuances.previous.reporteddate",
           #Primarily relevant when a new issuance is generated and we can compare with the closing values of the last issuance
           
           
           #.first is the first value submitted.  First can change if data is uploaded out-of-time and an earlier reporting period precedes an existing one
           #and/or if a modification is made to an historic dataset that causes the existing first to become stale.
           "first",
           "first.reporteddate",
           "first.unit",
           
           #.all is all timeseries data where updated is TRUE 
           #2023-10-10: Limited only to indicator_check_formulas:formula_result_message
           #There are presently zero calculations that actually use this and allowing this feature requires so much overhead for unlisting 
           #lists, etc.
           #GENERATES A LIST VARIABLE
           "all"                #data.table object: all updated and/or multiple DATA VALUES reported for the indicator as-of date
         ))
  
  assign(x="hasmatches",
         envir=CALCULATIONS_ENVIRONMENT,
         pos=0,
         value=function(group_values,
                        member_values,
                        sep="&",
                        mode=c("any","all")) {

           mode <- match.arg(mode)
           
           dt <- data.table(group_values=group_values,
                            member_values=member_values)
           dt[,id:=1:.N]
           
           dt[,group_grouping:=.GRP,
              by=.(group_values)]
           
           dt[,member_grouping:=.GRP,
              by=.(member_values)]
           
           
           groups <- unique(dt[,.(group_values,group_grouping)])
           members <- unique(dt[,.(member_values,member_grouping,group_grouping)])
           
           
           groups[,group_values:=strsplit(x=group_values,split=sep,fixed=T)]
           members[,member_values:=strsplit(x=member_values,split=sep,fixed=T)]
           
           
           groups <- groups[,
                            .(group_value=unlist(group_values,recursive=F)),
                            by=.(group_grouping)]
           
           
           members <- members[,
                              .(member_value=unlist(member_values,recursive=F)),
                              by=.(group_grouping,member_grouping)]
           
           groups[,group_value:=trimws(group_value)]
           members[,member_value:=trimws(member_value)]
           
           if (mode=="all") {
             groups[,size:=.N,
                    by=.(group_grouping)]
             
             members[groups,
                     size:=i.size,
                     on=.(group_grouping)]
           } else {
             members[,size:=1]
           }
           
           members[groups,
                   .(member_grouping,
                     member_value),
                   on=.(group_grouping,
                        member_value=group_value),
                   by=.EACHI]
           
           members[,
                   match:=FALSE]
           members[groups,
                   match:=TRUE,
                   on=.(group_grouping,
                        member_value=group_value)]
           
           members <- members[,
                              .(match=sum(match) >= size),
                              by=.(group_grouping,
                                   member_grouping)]
           
           
           # members[,
           #         match:=matches > 0]
           # 
           # members[,
           #         match:=matches == size]
           
           dt[members,
              match:=i.match,
              on=.(member_grouping,
                   group_grouping)]
           
           dt[is.na(match)==TRUE,
              match:=FALSE]
           
           return(dt$match)
           
         })
  
  
  assign(x="remap",
         envir=CALCULATIONS_ENVIRONMENT,
         pos=0,
         value=function(map_data,
                        mapset_from,
                        mapset_to,
                        sep=",") {
           mapset_from <- tolower(trimws(strsplit(mapset_from,sep,fixed=T)[[1]]))
           mapset_to <- tolower(trimws(strsplit(mapset_to,sep,fixed=T)[[1]]))
           
           if (length(mapset_from) != length(mapset_to)) stop(paste0("remap must have equal-length sets: FROM={",paste0(mapset_from,collapse=","),
                                                              "} TO={",
                                                              paste0(mapset_to,collapse=","),"} (split using sep='",sep,"')"))
           
           map_data <- tolower(trimws(as.character(map_data)))
           matches<-match(map_data,mapset_from,nomatch=NA_character_)
           mapset_to[matches]
           
         })
  
  assign(x="sum",
         envir=CALCULATIONS_ENVIRONMENT,
         value=function(...,na.rm=TRUE) {
           #print("custom sum")
           vals <- list(...)
           vals <- unlist(vals)
           return(base::sum(vals,na.rm=na.rm))
         })
  
  assign(x="min",
         envir=CALCULATIONS_ENVIRONMENT,
         value=function(...,na.rm=TRUE) {
           vals <- list(...)
           
           #sapply(vals,inherits,"Date")
           if (all(rapply(vals,is.Date,how="unlist"))) {
             vals <- unlist(vals)
             vals <- as.Date(vals,origin=lubridate::origin)
           }
           else vals <- unlist(vals)
           
           if (na.rm==TRUE && all(is.na(vals))) return(NA)
           
           val <- base::min(vals,na.rm=na.rm)
           if (length(val)==0) return(NA)
           else return (val)
         })
  
  assign(x="max",
         envir=CALCULATIONS_ENVIRONMENT,
         value=function(...,na.rm=TRUE) {
           vals <- list(...)
           if (all(rapply(vals,is.Date,how="unlist"))) {
             vals <- unlist(vals)
             vals <- as.Date(vals,origin=lubridate::origin)
           }
           else vals <- unlist(vals)
           if (na.rm==TRUE && all(is.na(vals))) return(NA)
           
           val <- base::max(vals,na.rm=na.rm)
           if (length(val)==0) return(NA)
           else return (val)
         })
  
  #parallel mean, excludes NA by default where mean(a,b) = a where all(b)==NA
  {
    assign(x="pmean",
           envir=CALCULATIONS_ENVIRONMENT,
           value=function(...) { stop("pmean function is deprecated in Jason: use 'average()' instead")})
    
    #parallel mean, excludes NA by default where mean(a,b) = a where all(b)==NA
    assign(x="psum",
           envir=CALCULATIONS_ENVIRONMENT,
           value=function(...) { stop("psum function is deprecated in Jason: use 'add()' instead") })
    
    assign(x="pmin",
           envir=CALCULATIONS_ENVIRONMENT,
           value=function(...) { stop("pmean function is deprecated in Jason: use 'least()' instead")})
    
    assign(x="pmax",
           envir=CALCULATIONS_ENVIRONMENT,
           value=function(...) { stop("pmean function is deprecated in Jason: use 'greatest()' instead")})
  }
  
  assign(x="average",
         envir=CALCULATIONS_ENVIRONMENT,
         value=function(...,na.rm=TRUE,na.zero=TRUE) {
           arg <- list(...)

           if (length(unique(sapply(arg,length))) > 1) { 
             stop("All arguments to average must be the same length")
           }
           arg <- as.data.frame(arg,col.names=paste0("X",1:length(arg)))
           x <- NULL
           if (ncol(arg)==1) x <- (base::mean(x=arg$X1,na.rm=na.rm))
           else x <- (base::rowMeans(x=arg,na.rm=na.rm))
           if (length(x)==0) x <- NA
           
           if (na.zero==TRUE & anyNA(x)) {
             x[is.na(x)] <- 0.0
           }
           return (x)
         })
  
  assign(x="add",
         envir=CALCULATIONS_ENVIRONMENT,
         value=function(...,na.rm=TRUE,na.zero=TRUE) {
           arg <- list(...)
           if (length(unique(sapply(arg,length))) > 1) { 
             stop("All arguments to average must be the same length")
           }
           arg <- as.data.frame(arg,col.names=paste0("X",1:length(arg)))
           
           x <- (base::rowSums(x=arg,na.rm=na.rm))
           if (length(x)==0) x <- NA
           
           if (na.zero==TRUE & anyNA(x)) {
             x[is.na(x)] <- 0
           }
           return (x)
         })
  
  assign(x="least",
         envir=CALCULATIONS_ENVIRONMENT,
         value=function(...,na.rm=TRUE) {
           x <- (base::pmin(...,na.rm=na.rm))
           return(x)
         })
  
  assign(x="greatest",
         envir=CALCULATIONS_ENVIRONMENT,
         value=function(...,na.rm=TRUE) {
           x <- (base::pmax(...,na.rm=na.rm))
           return(x)
         })
  
  #values: will return a vector of all ".timeseries" columns and should be used only when a single metric is supplied.
  #by: will return a data.table with columns: reporting_current_date, indicator_name.timeseries values of all indicators supplied.
  assign(x="timeseries",
         envir=CALCULATIONS_ENVIRONMENT,
         value=function(...,
                        by=NULL,
                        values=NULL,
                        
                        #fill will "fill down" NA values into missing timeseries dates
                        #EXCEPT when those timeseries dates are reported as NA
                        #Warning: periodic data expects to zero-out non-reported periods. 
                        #rsf_indicators should be present in the calling stack for this reason: For periodic indicators fill should be FALSE, or NA, or 0 !!
                        fill=TRUE,
                        
                        #Filter_by will filter-out only changed/reported "by" values to yield the same dataset as values=by_variable
                        filter_by=TRUE,
                        
                        rsf_indicators=NULL) {
           #setups
           {
             if (length(fill) != 1 &&
                 length(fill) != length(vars)+1) {
               stop(paste0("Fill values must by a single constant OR a vector of length ",
                           length(vars)+1,
                           " (number of timeseries columns plus one by column)"))
             }
             
             if (is.null(by) && is.null(values)) stop(paste0("Either 'by' or 'values' must be defined (and not both). ",
                                                             "By=indicator will return data for all requested indicators using the 'by' indicators timeseries dates as ",
                                                             "the reference timeline. ",
                                                             "Values=indicator will return the 'timeseries' data for the requested indicator and is a shortcut for ",
                                                             "indicator.all[[1]]$timeseries"))
  
             indicator_parameters <- as.character(match.call(expand.dots = T)[-1])
             all_all <- sapply(indicator_parameters,grepl,pattern="\\.all$|fill$|rsf_indicators$|filter_by$")
             if (!all(all_all)) {
              stop(paste0("Timeseries function takes one or more .all indicator parameter values (and only .all). Bad inputs for: ",paste0(indicator_parameters[!all_all],collapse=", ")))
             }
           }
           
           vars <- list(...)
           
           #If "values" is specified, no other variables allowed
           #This is a returning clause
           if (!is.null(values)) {
             
             if (length(vars) > 0) {
               stop("When timeseries(values=X) is used, only one indicator can be requested.  Did you mean to use timeseries(by=X,...)?")
             }
             
             if (!identical(fill,TRUE) && 
                 !identical(as.logical(fill),as.logical(NA)) &&
                 !identical(fill,FALSE)) {
               stop("Fill is not used when timeseries() is passed a values argument (nothing is filled).  Either leave as default setting or use fill=NA")
             }
             
             if (is.list(values) && all(unlist(lapply(values,
                                                      FUN=function(v) { is.null(v) | is.data.table(v) })))) {
               values <- rbindlist(values)
               
               if (is.null(values) || empty(values)) return (NULL)
               
             } else if (!is.data.table(values)) {
               stop("Timeseries values argument expects to receive an indicator with a '.all' parameter, eg, values=loan_risk_balance.all")
             }
             
             if (!all(c("indicator_name",
                        "timeseries.reporteddate",
                        "reporting_current_date",
                        "timeseries") %in% names(values))) {
               stop("Timeseries arguments require columns with .all specified, eg: loan_risk_balance.all to provide a data.table with columns: timeseries, timeseries.unit, timeseries.reporteddate, timeseries.changed, timeseries.updated")
             }
             
            return(values$timeseries)
          }
           
           if (is.null(by)) stop("by column is required")
          
        
           #a by variable has been submitted
           #and also other variables whose timelines must be aligned.
           if (length(vars) > 0) {
             
             #these are the lengths of the entities, eg, one entity, one list within each column.  It's not the length of timelines for each entity/varaible
             if (!all(length(by) == sapply(vars,length))) {
               stop(paste0("By variable has length of ",
                           length(by),
                           " entities.  Each variable should have the same length but received lengths: ",
                           paste0(sapply(vars,length),collapse=", ")))
             }
           }
           
           #Nothing reported: if "by" is nothing, then it's non-existent timeline means all matching vars that may or may not have been passed are also non-existent
           #we return an empty data.table because we don't know how the receiving function is using the information so give it an expected format.
           if (length(by)==0 || all(sapply(by,is.null))) {
             
             empty_cols <- unlist(lapply(gsub("\\.all$","",indicator_parameters),paste0,
                                         c(".timeseries",".timeseries.unit",".timeseries.reporteddate",".timeseries.changed",".timeseries.updated",".timeseries.reportnumber")))
             empty_dt <- data.table(matrix(nrow=0,ncol=length(empty_cols)))
             setnames(empty_dt,
                      new=empty_cols)
             
             empty_dt[,
                      CALCULATION_DATE:=as.Date(numeric())]
             
             return (empty_dt)
             
           }
           
           if (fill==TRUE && (is.null(rsf_indicators) || empty(rsf_indicators))) {
             
             rsf_indicators <- dynGet("rsf_indicators",ifnotfound = NULL,inherits=T)
             if (empty(rsf_indicators)) {
               rsf_indicators <- (dynGet("calc_env",inherits=T))[["rsf_indicators"]]
               
               if (empty(rsf_indicators)) {
                stop("When timeseries 'by' has fill=TRUE then 'rsf_indicators' must be passed explicitly (or be avilable within the call stack) as periodic_flow type indicators will report NA/Zero for non-reported periods")               
               }
             }
           }
           
           
           entities <- data.table(entity_id=seq_along(by),
                                  by_timeline=TRUE,
                                  ts=by)[,unlist(ts,recursive = F),by=.(entity_id,by_timeline)]
           
           calculation_date <- unique(entities$reporting_current_date)
           
           
           #timeline value may have different class
           entity_vars <- lapply(vars,function(v) {
             data.table(entity_id=seq_along(by),
                        ts=v)[,unlist(ts,recursive = F),by=.(entity_id)]
             })

           timelines <- entities[timeseries.reporteddate <= calculation_date,
                                 .(entity_id,timeseries.reporteddate)]
           
           if (length(entity_vars) > 0) {
             timelines <- unique(rbindlist(list(timelines,
                                                rbindlist(entity_vars)[timeseries.reporteddate <= calculation_date,
                                                                       .(entity_id,timeseries.reporteddate)])))
             # timelines <- unique(rbindlist(list(timelines,
             #                                    rbindlist(lapply(entity_vars,
             #                                                     function(ev) { ev[timeseries.reporteddate <= calculation_date,
             #                                                                       .(entity_id,timeseries.reporteddate)] })))))
           }
           
           timelines <- timelines[,
                                 .(timeline=c(seq(min(timeseries.reporteddate)+1,
                                                  calculation_date+1,
                                                  by="quarters"))-1),
                                 by=.(entity_id)]
           
           # timelines[entities]
           # timelines[,by_timeline:=FALSE]
           # timelines[entities[timeseries.reporteddate <= calculation_date,
           #                    .(timeline=c(seq(min(timeseries.reporteddate)+1,
           #                                     calculation_date+1,
           #                                     by="quarters"))-1),
           #                    by=.(entity_id)],
           #           by_timeline:=TRUE,
           #           on=.(entity_id,timeline)]

           entity_vars[[length(entity_vars)+1]] <- entities
           for (i in 1:length(entity_vars)) {
             
             ent <- entity_vars[[i]]
             ent_name <- ent$indicator_name[[1]]
             
             
             if (!all(c("indicator_name",
                        "timeseries.reporteddate",
                        "reporting_current_date",
                        "timeseries") %in% names(ent))) {
               stop("Timeseries arguments require columns with .all specified, eg: loan_risk_balance.all to provide a data.table with columns: timeseries, timeseries.unit, timeseries.reporteddate, timeseries.changed, timeseries.updated")
             }
             
             ent[,
                 timeline:=timeseries.reporteddate]
             
             ent <- ent[,
                        .SD,
                        .SDcols=c("entity_id","timeline",
                                   grep("^timeseries|^by_timeline$",names(ent),value=T))]
             ent[,
                 reporting_has_NA_value:=is.na(timeseries)]
             
             
             #timelines
             timelines <- ent[timelines,
                              on=.(entity_id,
                                   timeline),
                              nomatch=NA]
             
             #to entire fill down is valid
             setorder(timelines,
                      entity_id,
                      timeline)
             
             if (as.logical(fill) %in% TRUE) {
               
               is_pf <- rsf_indicators[indicator_name==ent_name,is_periodic_or_flow_reporting]
               if (length(is_pf)==0 || is.na(is_pf)) is_pf <- FALSE
               
               timelines <- tidyr::fill(timelines,
                                        timeseries,
                                        timeseries.reportnumber,
                                        timeseries.unit,
                                        .direction="down")
               
               if (is_pf==T) {
                 if (class(ent$timeseries)=="numeric") {
                   timelines[is.na(timeseries.reporteddate),
                             timeseries:=0]
                 } else {
                   timelines[is.na(timeseries.reporteddate),
                             timeseries:=NA]
                 }
               }
             }
             
             timelines[is.na(timeseries.reporteddate),
                       `:=`(timeseries.updated=FALSE,
                            timeseries.changed=FALSE)]
             
             #we don't want fill to fill down NA values that are REPORTED as NA
             timelines[!is.na(timeseries) & reporting_has_NA_value %in% TRUE,
                       timeseries:=NA]
             
             timelines[,reporting_has_NA_value:=NULL]
             setnames(timelines,
                      old=grep("^timeseries",names(ent),value=T),
                      new=paste0(ent_name,".",grep("^timeseries",names(ent),value=T)))
           }
           
           if (filter_by %in% c(TRUE)) {
             timelines <- timelines[by_timeline %in% c(TRUE)]             
           }

           timelines[,
                     `:=`(by_timeline=NULL,
                          entity_id=NULL)]
           
           timelines[,
                      CALCULATION_DATE:=calculation_date]
           
           return(timelines)
           
         
           # if (is.list(by) && all(unlist(lapply(by,is.data.table)))) {
           #   
           #   #new
           #   #means by is a .all variable
           #   #and also means that all "vars" should be .all varables
           #   if (length(by) > 1 && length(vars) > 0) {
           #   }
           #   
           #   if (length(by) > 1 && length(vars) > 0) {
           #     stop(paste0("By variable has a length greater than 1 (multiple elements in this list). ",
           #                 "This is not allowed when passing a list of other variables too."))
           #   }
           #   
           #   by <- rbindlist(by)
           # }
           # 
           # if (!all(c("indicator_name",
           #            "timeseries.reporteddate",
           #            "reporting_current_date",
           #            "timeseries") %in% names(by))) {
           #   stop("Timeseries arguments require columns with .all specified, eg: loan_risk_balance.all to provide a data.table with columns: timeseries, timeseries.unit, timeseries.reporteddate, timeseries.changed, timeseries.updated")
           # }
           # 
           # 
           # timeline <- seq(min(by$timeseries.reporteddate)+1,
           #                 max(by$reporting_current_date)+1,
           #                 by="quarters")
           # 
           # timeline <- data.table(reporting_current_date=ymd(timeline)-1)
           # timeline_origin <- min(timeline$reporting_current_date)
           # vars[[length(vars)+1]] <- by
           # 
           # 
           # for (i in 1:length(vars)) {
           #   
           #   col <- vars[[i]]
           #   if (is.null(names(col)) && !is.null(names(col[[1]]))) col <- col[[1]]
           #   if (!all(c("indicator_name",
           #              "timeseries.reporteddate",
           #              "reporting_current_date",
           #              "timeseries") %in% names(col))) {
           #     stop("Timeseries arguments require columns with .all specified, eg: loan_risk_balance.all to provide a data.table with columns: timeseries, timeseries.unit, timeseries.reporteddate, timeseries.changed, timeseries.updated")
           #   }
           #   
           #   this_fill <- NULL
           #   if (length(fill)==1) { this_fill <- fill 
           #   } else { this_fill <- fill[i] }
           #   
           #   ind_col <- paste0(unique(col$indicator_name),".timeseries")
           #   
           # 
           #   
           #   col <- col[,
           #              .(reporting_current_date=`timeseries.reporteddate`,
           #                timeseries)]
           #   
           #   #column's reporting data pre-dates the "By" reporting dates.  So we carry its value forward.
           #   #Note: this will be incorrect for flow-type data.  But this is a rarely used expert-only function that anyone using should account for themselves.
           #   #perhaps can add a pre-fill
           #   if (!any(col$reporting_current_date==timeline_origin) &&
           #       any(col$reporting_current_date < timeline_origin) &&
           #       as.logical(this_fill) %in% TRUE) {
           #     
           #     col_origin <- col[reporting_current_date < timeline_origin,max(reporting_current_date)]
           #     col <- col[reporting_current_date >= col_origin]
           #     col[reporting_current_date==col_origin,
           #         reporting_current_date:=timeline_origin]
           #   }
           #   col[,has_NA_value:=is.na(timeseries)]
           #   
           #   timeline <- col[timeline,
           #                   on=.(reporting_current_date)]
           #   
           #   
           #   if (as.logical(this_fill) %in% TRUE) {
           #     timeline <- tidyr::fill(timeline,
           #                             timeseries,
           #                             .direction="down")
           #   
           #   } else if (!is.na(this_fill) &&
           #              !identical(this_fill,FALSE)) {
           #     suppressWarnings(timeline[is.na(timeseries),
           #                               timeseries:=this_fill])
           #   }    
           #   
           #   #we don't want fill to fill down NA values that are REPORTED as NA
           #   timeline[!is.na(timeseries) & has_NA_value %in% TRUE,
           #            timeseries:=NA]
           #   
           #   timeline[,has_NA_value:=NULL]
           #   
           #   setnames(timeline,
           #            old="timeseries",
           #            new=ind_col)
           # }
           # return(timeline)
         })

  assign(x="concatenate",
         envir=CALCULATIONS_ENVIRONMENT,
         value=function(...,delimiter=" ") { return(paste(...,sep=delimiter)) })

  #entered as an alias function so checks and calculations can use more intuitive "count" and also avoid very unintutive "unlist" since data values are in [[1]] of list object
  assign(x="count",
         envir=CALCULATIONS_ENVIRONMENT,
         value=function(x,count.false=FALSE,count.missing=FALSE,count.unique=FALSE) { 
           x <- unlist(x)
           
           if (count.missing==FALSE) x <- x[is.na(x)==FALSE]
           if (count.false==FALSE && all(is.logical(x))) x <- x[x==TRUE]
           if (count.unique==TRUE) x <- unique(x)
           return (length(x))
         }) 
  
  assign(x="is.missing",
         envir=CALCULATIONS_ENVIRONMENT,
         value=function(x) { 
           if (length(x)==0) return (TRUE)
           else return (is.na(unlist(x))) 
         })
  
  assign(x="if.missing",
         envir=CALCULATIONS_ENVIRONMENT,
         value=function(a,b) {
           if (length(a)==0) return (b)
           else return(ifelse(is.na(a),b,a))
         })
  

  assign(x="%equal%",
         envir=CALCULATIONS_ENVIRONMENT,
         value=function(e1,e2) {
           mapply(function(a,b) { 
             #if it is actually equal or nearly equal then YES
             if (!is.na(a) & !is.na(suppressWarnings(as.numeric(a)))) a<-as.numeric(a)
             if (!is.na(b) & !is.na(suppressWarnings(as.numeric(b)))) b<-as.numeric(b)
             
             isTRUE(base::all.equal(a,b,check.class=F)) | isTRUE(base::`==`(e1=a,e2=b)) 
           },a=e1,b=e2,USE.NAMES=F)
         }
  )

  assign(x="%unequal%",
         envir=CALCULATIONS_ENVIRONMENT,
         value=function(e1,e2) {
           mapply(function(a,b) { 
             
             if (!is.na(a) & !is.na(suppressWarnings(as.numeric(a)))) a<-as.numeric(a)
             if (!is.na(b) & !is.na(suppressWarnings(as.numeric(b)))) b<-as.numeric(b)
             
             #if it is actually not-equal AND not nearly equal then YES
             (!isTRUE(base::all.equal(a,b,check.class=F))) & isTRUE(base::`!=`(e1=a,e2=b)) 
           },a=e1,b=e2,USE.NAMES=F)
         })
  
  # assign(x="!",
  #        envir=CALCULATIONS_ENVIRONMENT,
  #        value=function(x) {
  #          if (inherits(x,"list")) return (lapply(x,base::`!`))
  #          else return (base::`!`(x))
  #        })
  
  
  
  #limit inputs recognizes as numeric to fixed decimal places, as most long decimals are likely due to calculated results than precision inputs 
  #and can lead to mismatches when say an Excel input is 1.333333333333 and an R calculation is 1.3333333333
  assign(x="MAX_DECIMALS",
         envir=CALCULATIONS_ENVIRONMENT,
         value=14)
  
  #Determins rounding and degree to which system calculator will consider numbers to be materially different and flag changes
  assign(x="SIG_DIGITS",
         envir=CALCULATIONS_ENVIRONMENT,
         value=5)
  
  #Determins rounding and degree to which system calculator will consider numbers to be materially different and flag changes
  assign(x="SYSTEM_CALCULATOR_ACCOUNT",
         envir=CALCULATIONS_ENVIRONMENT,
         value=ACCOUNT_SYS_CALCULATOR$account_id)
  
  get_fx_codes <- function() {
    codes <- dremioQuery(sql=glue("SELECT distinct Currency_Code 
                                 FROM IFCReferenceData.Currency
                      "))
    
    codes <- sort(unlist(codes,use.names = F))
    return(codes)
  }
  
  # #Determins rounding and degree to which system calculator will consider numbers to be materially different and flag changes
  assign(x="VALID_CURRENCIES",
         envir=CALCULATIONS_ENVIRONMENT,
         value=get_fx_codes())
  
  
  get_fx_rate <- function(exchange_rate_date=today(),
                          #Currency code: FROM/TO
                          #Eg, EUR/USD 1.08 -> 1EUR buys 1.08USD 
                          #EG, EUR/USD 1.08 -> 1EUR equals 1.08USD
                          currency_code_ratio="EUR/USD") {
    
    #if (is.null(dremio) || pool::dbIsValid(dremio)==FALSE) stop("DREMIO Database Pool is invalid, failed to connect or failed to start.  Report error to SYS Admin")
    if (is.null(currency_code_ratio) || is.na(currency_code_ratio)) stop(paste0("Currency code is not specified. Failed to look up exchange rate for NA value"))
    if (!grepl("^[[:alpha:]]{3}/[[:alpha:]]{3}$",currency_code_ratio)) stop(paste0("currency code ratio must be in format of: [to currency code]/[from currency code], eg, 'EUR/USD' or 'XOF/NGN'; received: '",currency_code_ratio,"'"))
    
    exchange_rate_date <- as.Date(exchange_rate_date)
    if (!is.Date(exchange_rate_date)) stop("exchange_rate_date must be a valid date")
    if (exchange_rate_date > today()) stop("Exchange rate data unavilable for future days")
    
    {  
      currency_code_ratio <- toupper(currency_code_ratio)
      from_code <- gsub("^([[:alpha:]]{3})/[[:alpha:]]{3}$","\\1",currency_code_ratio)   #NUMERATOR IS FROM
      to_code <- gsub("^[[:alpha:]]{3}/([[:alpha:]]{3})$","\\1",currency_code_ratio)     #DENOMINATOR IS TO
      
      if (from_code %in% c('LCU','LCY') || to_code %in% c('LCU','LCY')) stop(paste0("Local Currency Unit is not a valid Currency Code. ",
                                                                                    "Failed to lookup fx rate for ",currency_code_ratio,". ",
                                                                                    "Ensure LCU base currency is specified for ",as.character(exchange_rate_date)))
      fx <- NULL
      
      if (from_code==to_code) {
        fx <- 1
      } else if (from_code=="USD") {
        
        fx <- dremioQuery(sql=glue("select
                        EXCHANGE_RATE_DATE,
                        EXCHANGE_RATE_PER_USD
                       from ARL.FX_ACTIVE_DAILY2
                       where CURRENCY_CODE = '{to_code}'
                         and EXCHANGE_RATE_DATE <= '{exchange_rate_date}'
                       order by EXCHANGE_RATE_DATE desc, DATETIME_STAMP desc
                       limit 1
                      "))
        
        
        fx <- 1/fx$EXCHANGE_RATE_PER_USD #USD/LCU, eg 14.98ZAR/USD 1.1385EUR/USD
        
      } else if (to_code=="USD") {
        
        fx <-  dremioQuery(sql=glue("select
                        EXCHANGE_RATE_DATE,
                        EXCHANGE_RATE_PER_USD
                       from ARL.FX_ACTIVE_DAILY2
                       where CURRENCY_CODE = '{from_code}'
                         and EXCHANGE_RATE_DATE <= '{exchange_rate_date}'
                       order by EXCHANGE_RATE_DATE desc, DATETIME_STAMP desc
                       limit 1
                      "))
        
        #Ie, To:USD/From:XOF = 1 | To:USD = From:XOF :: 1USD = 595XOF  or USD/XOF = 1/595
        fx <- fx$EXCHANGE_RATE_PER_USD
        
      } else {
        fx <-  dremioQuery(sql=glue("
                       select * from (
                       select
                        EXCHANGE_RATE_DATE,
                        CURRENCY_CODE,
                        EXCHANGE_RATE_PER_USD
                       from ARL.FX_ACTIVE_DAILY2
                       where CURRENCY_CODE = '{to_code}'
                         and EXCHANGE_RATE_DATE <= '{exchange_rate_date}'
                       order by EXCHANGE_RATE_DATE desc, DATETIME_STAMP desc
                       limit 1) from_code
                               
                       union all
                       
                       select * from (
                       select
                        EXCHANGE_RATE_DATE,
                        CURRENCY_CODE,
                        EXCHANGE_RATE_PER_USD
                       from ARL.FX_ACTIVE_DAILY2
                       where CURRENCY_CODE = '{from_code}'
                         and EXCHANGE_RATE_DATE <= '{exchange_rate_date}'
                       order by EXCHANGE_RATE_DATE desc, DATETIME_STAMP desc
                       limit 1) to_code
                      "))
        
        #[FROM/USD] / [TO/USD] -> [TO/FROM]
        fx <- fx[fx$CURRENCY_CODE==from_code,"EXCHANGE_RATE_PER_USD"] / fx[fx$CURRENCY_CODE==to_code,"EXCHANGE_RATE_PER_USD"]
        
      }
      
      if (length(fx) != 1) {
        to_failed <-  dremioQuery(sql=glue("select exists(select * from ARL.FX_ACTIVE_DAILY2  where CURRENCY_CODE = '{to_code}') as to_exists"))
        
        from_failed <-  dremioQuery(sql=glue("select exists(select * from ARL.FX_ACTIVE_DAILY2  where CURRENCY_CODE = '{from_code}') as from_exists"))
        
        if (all(unlist(to_failed)==FALSE)) {
          
          stop(paste0("Failed to lookup fx rate for ",
                      currency_code_ratio,": ",to_code," is not a valid currency code"))
          
        } else if (all(unlist(from_failed)==FALSE)) {
          
          stop(paste0("Failed to lookup fx rate for ",
                      currency_code_ratio,": ",to_code," is not a valid currency code"))
        } else {
          
          stop(paste0("Failed to lookup fx rate for ",
                      currency_code_ratio," or end-of-day rate is currently unavailalbe as-of ",as.character(exchange_rate_date)))
          
        }
        
      }
      
      names(fx) <- currency_code_ratio
    }
    fx
  }
  
  assign(x="get_IFC_FX_rate",
         envir=CALCULATIONS_ENVIRONMENT,
         value=function(exchange_rate_date=today(),
                        currency_code_ratio="EUR/USD") {
           #print(paste0("get_IFC_FX_rate DISABLED FOR TESTING: Returning 100"))
           #return (100)
           
           if (length(exchange_rate_date)==0 || any(is.na(exchange_rate_date))) stop(paste0("MISSING exchange_rate_date value for currency_code_ratio=",currency_code_ratio))
           if (length(currency_code_ratio)==0 || any(is.na(currency_code_ratio))) stop(paste0("MISSING currency_code_ratio value for exchange_rate_date=",exchange_rate_date))
           
           if (length(exchange_rate_date) != length(currency_code_ratio) &&
               length(exchange_rate_date) != 1 &&
               length(currency_code_ratio) != 1) stop(paste0("Exchange rate dates and currency codes must be equal length: ",
                                                             length(exchange_rate_date)," vs ",length(currency_code_ratio)))
           
           mapply(get_fx_rate,
                  exchange_rate_date=exchange_rate_date,
                  currency_code_ratio=currency_code_ratio)
         })
}
