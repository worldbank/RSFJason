rsf_checks_calculate <- function(pool,
                                 rsf_indicators,
                                 rsf_data_wide,
                                 checks,
                                 keep_false_flags=FALSE,
                                 on_fail="sys_checker_failed",
                                 status_message=function(...) {}) #noise useful for test checks functionality and printing status messages
{
  
  if (empty(rsf_data_wide) || is.null(rsf_data_wide) || nrow(rsf_data_wide)==0 || all(is.na(rsf_data_wide))) {
    status_message(class="warning","No data available to calculate checks.")
    return (NULL)
  }


  reporting_current_date <- unique(rsf_data_wide[,reporting_current_date])
  if (length(reporting_current_date) != 1) stop(paste0("reporting_current_date is not unique: ",paste0(reporting_current_date)))
  
  fx_table <- data.table(rsf_pfcbl_id=numeric(0),
                         from_currency=character(0),
                         to_currency=character(0),
                         fx_date=as.Date(numeric(0)),
                         fx_rate=numeric(0),
                         fx_pfcbl_category=character(0),
                         exchange_rate_data_id=numeric(0))
  
  update_fx_table_function <- function(cache_fx) {
    if (!empty(cache_fx)) {
      setcolorder(cache_fx,
                  neworder = names(fx_table))
      
      fx_table <<- rbindlist(list(fx_table,
                                  cache_fx))
    }
  }
  
  all_checks <- list()
  
  #rsf_data_wide[,row_id:=1:.N]
  #i<-1
  for(i in 1:nrow(checks)) {
    
    calc_time <- Sys.time()
    
    check <- checks[i]


    #block: validating inputs
    {
      #if (!is.null(check$is_system) && as.logical(paste0(check$is_system))==TRUE) next;
      parameters <- rbindlist(check$parameters_dt)
      parameters <-parameters[order(fcase(parameter_data_category=="global",0,
                                     parameter_data_category=="program",1,
                                     parameter_data_category=="facility",2,
                                     parameter_data_category=="client",3,
                                     parameter_data_category=="borrower",4,
                                     parameter_data_category=="loan",5))]
      
      check_name <- check$check_name
      check_grouping <- check$grouping
      check_subgrouping <- check$subgrouping
      check_expr <- check$formula  
      check_expr_msg <- check$formula_result_message
      check_for <- check$for_indicator_name
      

      
      {
        if (any(is.null(check_grouping) || is.na(check_grouping) || nchar(check_grouping)==0 || length(check_grouping)==0)) check_grouping <- "none"
        else check_grouping <- tolower(check_grouping)
      }
      if (any(is.null(check_subgrouping) || is.na(check_subgrouping) || nchar(check_subgrouping)==0 || length(check_subgrouping)==0)) check_subgrouping <- NA

      if (!is.na(check_subgrouping) && check_grouping == "none") {
        
          status_message(class="error",paste0(check_name," check grouping is NONE but subgrouping is '",check_subgrouping,"'.  Grouping must be defined to enable sub-grouping. Skipping.\n"))
          status_message(class="info","Note -- to be meaningful, 'grouping' should be defined at the same level or lower than the grouping, eg, if grouping at 'client' then subgrouping indicator should be client, borrower, or loan; but not a facility or program indicator.\n")
        
        next;
      }      
      
      status_message(class="none",paste0(" #",i))
      
      if (is.null(check_expr_msg) || length(check_expr_msg)==0 || is.na(check_expr_msg) || nchar(trimws(check_expr_msg,whitespace="[ \\t\\r\\n\\v\\h\\s]"))==0) check_expr_msg <- NA
      
      #To accommodate line-breaks, etc written into formula form fields
      check_expr <- gsub("[[:cntrl:]]+"," ",check_expr)
      check_subgrouping <- gsub("[[:cntrl:]]+"," ",check_subgrouping)
      
      check_expr_msg <- trimws(gsub("[[:cntrl:]]+"," ",check_expr_msg))
      if (is.na(check_expr_msg) || nchar(check_expr_msg)==0) check_expr_msg <- "'No message: set in admin panel'"
      check_expr_msg <- paste0("paste0(sort(unique(paste0(",check_expr_msg,"))),collapse=\" [AND] \")")
    }
    
    #Block start: checks
    {
      #check_rsf_pfcbl_id <- paste0("rsf_pfcbl_id.",check$check_pfcbl_category)
      #will be ordered
      #check_rsf_pfcbl_id_cols <- paste0("rsf_pfcbl_id.",unique(parameters$parameter_data_category))
      
      check_rsf_pfcbl_id <- paste0("rsf_",check$check_pfcbl_category,"_id")
      #will be ordered
      check_rsf_pfcbl_id_cols <- paste0("rsf_",unique(parameters$parameter_data_category),"_id")
      
      #2023-10-31
      #Changed on Halloween!  Let's hope it's not scary :-D
      check_rsf_group <- switch(check_grouping,
                                loan="rsf_loan_id",
                                borrower="rsf_borrower_id",
                                client="rsf_client_id",
                                facility="rsf_facility_id",
                                program="rsf_program_id",
                                none=check_rsf_pfcbl_id,
                                NA)
      
      if (all(is.na(check_rsf_group))) stop(paste0("Failed to resolve check group using ",check_grouping))

      check_rsf_group <- c("reporting_current_date",check_rsf_group)
      
      check_data_cols <- unique(c(check_rsf_pfcbl_id,
                                  check_rsf_pfcbl_id_cols,
                                  check_rsf_group,
                                  parameters[,parameter_column_name]))
      
      check_data <- rsf_data_wide[,..check_data_cols]
      
     
      
      check_data <- unique(check_data,
                           by=check_rsf_pfcbl_id_cols)
      
      
      setorderv(check_data,
                cols=check_rsf_pfcbl_id_cols)
      
      check_data[,
                 grouping:=.GRP,
                 by=check_rsf_group]
      
      setnames(check_data,
               old=check_rsf_pfcbl_id,
               new="rsf_pfcbl_id")
      
      # setnames(check_data,
      #          old=check_rsf_group,
      #          new="grouping")
      
      
      grouping_cols <- c("grouping","subgrouping")
      
      #when NA set to none in initial setups.
      if (check_grouping=="none") {
        check_data <- check_data[rsf_pfcbl_id %in% unlist(check$check_rsf_pfcbl_ids)]
      
      } else {
      
      #####
      
        #Column names that are equal-to or parent-level from the current grouping-level
        #These columns will be passed to the data.table by() clause and ensure that any aggregate functions will see them only onces and not count/sum repeated rows
        grouped_parameters <- parameters[rsf_indicators[indicator_pfcbl_rank <= check$grouping_rank,
                                                        .(parameter_indicator_id =indicator_id)],
                                         on=.(parameter_indicator_id ),
                                         nomatch=NULL,
                                         parameter_column_name]
        #Should be true, but just in case
        grouped_parameters <- grouped_parameters[grouped_parameters %in% names(check_data)]
        grouping_cols <- c(grouping_cols,grouped_parameters)
      }
      
      ####
    }

    if (!is.na(check$calculate_indicator_currency_unit)) {
      
      
      check_data <- rsf_computation_fx_conversion(pool=pool,
                                                  computation=check,
                                                  comp_data=check_data,
                                                  computation_asof_date=check$check_asof_date,
                                                  fx_table=fx_table,
                                                  update_fx_table_function=update_fx_table_function, 
                                                  add_data_flag_function=NULL, #we don't flag the flags
                                                  add_fx_conversions_function=NULL) #if fx rates change we don't redo checks
    }

    #no, this emerges as a result of actively doing addition and subtraction within the formula.    
    # #Helps undo floating point problems.
    # isn<-which(sapply(check_data,is.numeric))
    # for(i in isn) {
    #   set(check_data,i=NULL,j=i,value=as.numeric(as.character(check_data[[i]])))
    # }
    
    {
      error_mess <- NULL
      computed_results <- tryCatch({


        ###############
        ###CALCULATIONS
        ###############

        
        #for rounding errors.
        #`%equal%` <- function(e1,e2) { mapply(function(a,b) { isTRUE(base::all.equal(a,b)) },a=e1,b=e2) }
        #`%unequal%` <- function(e1,e2) { mapply(function(a,b) { !isTRUE(base::all.equal(a,b)) },a=e1,b=e2) }
        
        check_expr <- gsub("==","%equal%",check_expr)
        check_expr <- gsub("!=","%unequal%",check_expr)
        
        check_expr_msg <- gsub("==","%equal%",check_expr_msg)
        check_expr_msg <- gsub("!=","%unequal%",check_expr_msg)
        
        calc_env <- new_environment(data=list(reporting_current_date=reporting_current_date,
                                              check_expr=check_expr,
                                              check_expr_msg=check_expr_msg,
                                              check_subgrouping=check_subgrouping,
                                              check_data=check_data,
                                              grouping_cols=grouping_cols),
                                    parent=CALCULATIONS_ENVIRONMENT)
        
        #For floating point issues
        #This is a good idea...that doesn't work.  Causes data.table internal methods to fail!
        #Users justneed to use the all.equal() functions within their checks.
        # assign(x="!=",
        #        envir=calc_env,
        #        value=function(e1,e2) { !all.equal(target=e1,current=e2,check.class=F) })
        # 
        # assign(x="==",
        #        envir=calc_env,
        #        value=function(e1,e2) { all.equal(target=e1,current=e2,check.class=F) })
        
        #if.missing <- CALCULATIONS_ENVIRONMENT$if.missing
        calculations <- with(calc_env, {
          
                               check_data[,flag_status:=as.logical(NA)]
                               if (is.na(check_subgrouping)) { 
                                 check_data[,subgrouping:=as.character(NA)]
                               } else {
                                 #paste so multiple indicator column names can be used as subgroup
                                 #will turn NA values into "NA" strings as well
                                 subcols <- sapply(names(check_data),grepl,x=check_subgrouping)
                                 subcols <- names(subcols)[subcols]
                                 blanks <-lapply(check_data[,..subcols],is.na)
                                 all_blanks <- Reduce(`&`,blanks)
                                 any_blanks <- Reduce(`|`,blanks)
                                 
                                 check_data[,subgrouping:=eval(parse(text=paste0("paste0(",check_subgrouping,")")))] 
                                 check_data[(any_blanks==TRUE & is.na(subgrouping)) |
                                            all_blanks==TRUE,
                                            flag_status:=FALSE]
                                 
                                 # check_data[any_blanks==TRUE,
                                 #            flag_status:=FALSE]
                                 # if (any(blanks)) {
                                 #   check_data[,subgrouping_blank:=blanks]
                                 #   check_data[,flag_status:=!all(subgrouping_blank),
                                 #              by=.(grouping_col)]
                                 #   check_data[,subgrouping_blank:=NULL]
                                 # }
                               }
           
                               check_data[is.na(flag_status),
                                          flag_status := as.logical(eval(parse(text=check_expr))),
                                          by=grouping_cols]
                               
                               check_data[is.na(flag_status)==TRUE,
                                          flag_status:=FALSE]
                               
                               ufields <- grep("^rsf_.*_id$",names(check_data),value=T)
                               
                               messages <- unique(check_data[flag_status==TRUE],
                                                  by=c(ufields,"reporting_current_date"))
                               

                               # check_data <- unique(check_data[,
                               #                                 .(rsf_pfcbl_id,
                               #                                   flag_status,
                               #                                   check_message)])

                               #For grouped checks
                               check_data <- check_data[,
                                                        .(flag_status=any(flag_status)),
                                                        by=.(rsf_pfcbl_id)]
                               check_data[is.na(flag_status),flag_status:=FALSE]
                               check_data[,check_message:=as.character(NA)]
                               
                               if (nrow(messages) != 0) {
                                 
                                 # messages[,
                                 #          `:=`(flag_status=NULL,
                                 #               grouping=NULL,
                                 #               subgrouping=NULL)]
                                 # messages[,
                                 #          flag_status:=NULL]
                                 
                                 #Let the message writer do what they want and evaluate it!
                                 #In case the message is asking for ".all"
                                 # has_lists <- sapply(messages,is.list)
                                 # if (any(has_lists)) {
                                 #   has_lists <- names(has_lists)[(has_lists)]
                                 #   for(hl in has_lists) {
                                 #     if (grepl("\\.all",hl)) {
                                 #       set(messages,
                                 #           i=NULL,
                                 #           j=hl,
                                 #           sapply(messages[[hl]],FUN=function(x) { paste0(paste(x$timeseries,x$timeseries.unit),collapse=", ") }))
                                 #       } else {
                                 #       set(messages,
                                 #           i=NULL,
                                 #           j=hl,
                                 #           sapply(messages[[hl]],paste0,collapse=","))
                                 #     }
                                 #   }
                                 # }
                                 
                                 #set all to characters
                                 
                                 
                                 #We have some sort of grouping going on.
                                 # if (nrow(messages) != length(unique(messages$rsf_pfcbl_id))) {
                                 #   
                                 #   
                                 #   mcols <- names(messages)[-which(names(messages) %in% c("rsf_pfcbl_id"))]
                                 #   for (col in mcols) {
                                 #     set(messages,
                                 #         i=NULL,
                                 #         j=col,
                                 #         value=as.character(messages[[col]]))
                                 #   }
                                 #   # messages[,sys_test.current:="test"]
                                 #   # z<-messages[1:100]
                                 #   # z[,sys_test.current:="grouping"]
                                 #   # messages <- rbindlist(list(messages,z))
                                 #   
                                 #   messages <- melt.data.table(messages,
                                 #                               id.vars=c("rsf_pfcbl_id"),
                                 #                               variable.name = "parameter_name",
                                 #                               value.name = "data_value",
                                 #                               value.factor = F,
                                 #                               variable.factor = F)
                                 #   
                                 #   messages <- unique(messages)
                                 #   #messages[rsf_pfcbl_id==16158149]
                                 #   
                                 #   messages[,n:=.N,
                                 #            by=.(rsf_pfcbl_id,
                                 #                 parameter_name)]
                                 #   
                                 #   if (any(messages$n>1)) {
                                 #     
                                 #     messages1 <- messages[n==1,
                                 #                           .(rsf_pfcbl_id,
                                 #                             parameter_name,
                                 #                             data_value)]
                                 #     
                                 #     messagesX <- messages[n>1,
                                 #                           .(rsf_pfcbl_id,
                                 #                             parameter_name,
                                 #                             data_value)]
                                 #     
                                 #     messagesX <- messagesX[,
                                 #                            .(data_value=paste(data_value,collapse=", ")),
                                 #                            by=.(rsf_pfcbl_id,
                                 #                                 parameter_name)]
                                 #     messages <- rbindlist(list(messages1,
                                 #                                messagesX))
                                 #     
                                 #   }
                                 #   
                                 #   messages <- messages[,.(rsf_pfcbl_id,
                                 #                           parameter_name,
                                 #                           data_value)]
                                 #   
                                 #   messages <- dcast.data.table(messages,
                                 #                                formula= rsf_pfcbl_id ~ parameter_name,
                                 #                                value.var="data_value")
                                 #   
                                 # }

                                 
                                 messages <- messages[,
                                                      .(rsf_pfcbl_id,
                                                        flag_status,
                                                        check_message=as.character(eval((parse(text=check_expr_msg))))),
                                                      by=grouping_cols]
                                 
                                 messages <- unique(messages)
                                 messages[,
                                          n:=.N,
                                          by=grouping_cols]
                                 
                                 if (any(messages$n > 1)) {
                                   messages <- messages[,.(check_message=paste0(check_message,collapse=" & ")),
                                                        by=c(grouping_cols,"rsf_pfcbl_id","flag_status")]
                                 }
                                 
                                 check_data[messages,
                                            check_message:=i.check_message,
                                            on=.(rsf_pfcbl_id,
                                                 flag_status)]
                               } 
                               
                               
                              
                               
                               
                               
                               
                               

                               
                               
                               #there's a weird data table bug where errors aren't thrown
                               #check_data[,#flag_status==TRUE,
                              #         check_message := as.character(eval((parse(text=check_expr_msg)))),
                              #         by=.(grouping_col,subgrouping_col)]

                               #check_data[flag_status==FALSE,check_message:=NA]
                               
                               
                               # check_data <- unique(check_data[,
                               #                                 .(rsf_pfcbl_id,
                               #                                   flag_status,   #keeping this for joins later in case keep failed checks is TRUE
                               #                                   check_message)])
                               check_data
                             })
        calculations

      },
      warning = function(war) {
        error_mess <- paste0("Formula warning for ",check$check_name," formula#",
                             check$check_formula_id,": ",as.character(conditionMessage(war)))
        #status_message(class="warning",paste("\nWarning: Formula warning for ",check_name,": ",mess,"\n"))
        check_data[,`:=`(flag_status=TRUE,
                         check_message=error_mess)]
        unique(check_data[,
                       .(rsf_pfcbl_id,
                         flag_status,   #keeping this for joins later in case keep failed checks is TRUE
                         check_message)])
      },
      error = function(err) { 
        error_mess <- paste0("Formula error for ",
                             check$check_name," formula#",
                             check$check_formula_id,": ",as.character(conditionMessage(err)))
        
        #status_message(class="error",paste("\nERROR: Formula error for ",check_name,": ",mess,"\n"))
        check_data[,`:=`(flag_status=TRUE,
                         check_message=error_mess)]
        unique(check_data[,
                          .(rsf_pfcbl_id,
                            flag_status,   #keeping this for joins later in case keep failed checks is TRUE
                            check_message)])
        
      })
      
      computed_results <- computed_results[rsf_pfcbl_id %in% unlist(check$check_rsf_pfcbl_ids)]
      computed_results[,
                    n:=.N,
                    by=.(rsf_pfcbl_id,
                         flag_status)]
      
      if (!empty(computed_results[n>1])) {
        
        computed_results[n>1,
                      `:=`(check_message=paste0("WARNING: multiple results found: should formula explicitly set a grouping category or use an aggregate function in its formula? Check message: ",
                                            paste0(sort(unique(check_message)),collapse=", ")),
                           flag_status=any(flag_status==TRUE)),
                      by=.(rsf_pfcbl_id)]
        
        computed_results <- unique(computed_results)
      }
      
      computed_results[,n:=NULL]

      if (keep_false_flags==FALSE) computed_results <- computed_results[flag_status==TRUE]
      
      if (!empty(computed_results)) {
        computed_results[,
                      `:=`(check_formula_id=check$check_formula_id,
                           check_asof_date=check$check_asof_date)]
        
        computed_results <- computed_results[,.(rsf_pfcbl_id,
                                          check_asof_date,
                                          check_formula_id,
                                          check_message,
                                          flag_status)]
        
        
        all_checks[[length(all_checks)+1]] <- computed_results    
    }
    }
    
    calc_time <- round(as.numeric(Sys.time() - calc_time,format="sec"),2)
    if (calc_time > 1) status_message(class="warning",",\n",check_name," took ",calc_time,"s to calculate\n")
    if(SYS_PRINT_TIMING & calc_time > 0.5)  debugtime("rsf_checks_calculate","long check of ",calc_time," for ",check_name," for ",check_for)  
    #computed_results
  }
  status_message(class="none","\n") #to line-break after #1,2,3,4...etc
  
  
  completed_checks <- rbindlist(all_checks)
  
  if (empty(completed_checks)) return (NULL)
  return(completed_checks)
}
