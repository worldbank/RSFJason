rsf_checks_calculate <- function(pool,
                                 rsf_indicators,
                                 rsf_data_wide,
                                 checks,
                                 on_fail="sys_checker_failed",
                                 status_message=function(...) {}) #noise useful for test checks functionality and printing status messages
{
  #keep_false_flags=FALSE
  
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
  
  check_failed <- function(rsf_pfcbl_id=NA, #will assign check fail message to the family
                           check_asof_date, 
                           check_formula_id,
                           check_message) {

    if (length(check_asof_date) != 1) stop("Check failed check_asof_date must be a unique date")
    if (length(check_formula_id) != 1) stop("Check failed check_formula_id must be a unique date")
    if (length(check_message) != 1) stop("Check failed check_message must be a unique date")
    
    rsf_pfcbl_id <- unique(na.omit(unlist(rsf_pfcbl_id)))
    if (length(rsf_pfcbl_id) == 0) return (NULL)
    
    all_checks[[length(all_checks)+1]] <<- data.table(rsf_pfcbl_id=rsf_pfcbl_id,
                                                      check_asof_date=check_asof_date,
                                                      check_formula_id=check_formula_id,
                                                      check_message=check_message,
                                                      check_variance=as.numeric(NA),
                                                      flag_status=NA)
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
      if (empty(parameters)) {
        # status_message(class="error",
        #                paste0("Check ",check$check_name,"[#",check$check_formula_id,"] has no parameters: calculation will fail: [",check$formula,"]\n"))
        
        check_failed(rsf_pfcbl_id=check$check_rsf_pfcbl_ids,
                     check_asof_date=check$check_asof_date,
                     check_formula_id=check$check_formula_id,
                     check_message=paste0("Check ",check$check_name,"[#",check$check_formula_id,"] has no parameters: calculation will fail: [",check$formula,"]\n"))
        next;
      }
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
      check_variance_formula <- check$variance_formula

      
      {
        if (any(is.null(check_grouping) || is.na(check_grouping) || nchar(check_grouping)==0 || length(check_grouping)==0)) check_grouping <- "none"
        else check_grouping <- tolower(check_grouping)
      }
      if (any(is.null(check_subgrouping) || is.na(check_subgrouping) || nchar(check_subgrouping)==0 || length(check_subgrouping)==0)) check_subgrouping <- NA

      if (!is.na(check_subgrouping) && check_grouping == "none") {

          check_failed(rsf_pfcbl_id=check$check_rsf_pfcbl_ids,
                       check_asof_date=check$check_asof_date,
                       check_formula_id=check$check_formula_id,
                       check_message=paste0("Check ",check_name,"[#",check$check_formula_id,"] check grouping is NONE but subgrouping is '",
                                            check_subgrouping,"'.  Grouping must be defined to enable sub-grouping.\n",
                                            "'grouping' should be defined at the same level or lower than the grouping, eg, if grouping at 'client' then subgrouping indicator should be client, borrower, or loan; but not a facility or program indicator."))

        next;
      }      
      
      status_message(class="none",paste0(" #",i))
      
      if (is.null(check_expr_msg) || length(check_expr_msg)==0 || is.na(check_expr_msg) || nchar(trimws(check_expr_msg,whitespace="[ \\t\\r\\n\\v\\h\\s]"))==0) check_expr_msg <- NA
      
      if (is.na(check_variance_formula) || nchar(check_variance_formula)==0 || tolower(check_variance_formula)=="na") check_variance_formula <- as.character(NA)
      check_variance_formula <- gsub("[[:cntrl:]]+"," ",check_variance_formula)
      #To accommodate line-breaks, etc written into formula form fields
      check_expr <- gsub("[[:cntrl:]]+"," ",check_expr)
      check_subgrouping <- gsub("[[:cntrl:]]+"," ",check_subgrouping)
      
      check_expr_msg <- trimws(gsub("[[:cntrl:]]+"," ",check_expr_msg))
      if (is.na(check_expr_msg) || nchar(check_expr_msg)==0) check_expr_msg <- "'No message: set in admin panel'"
      check_expr_msg <- paste0("paste0(sort(unique(paste0(",check_expr_msg,"))),collapse=\" [AND] \")")
    }
    
    #Block start: checks
    {

      check_rsf_pfcbl_id <- paste0("rsf_",check$check_pfcbl_category,"_id")
      #will be ordered
      check_rsf_pfcbl_id_cols <- paste0("rsf_",unique(parameters$parameter_data_category),"_id")
      
      #2023-10-31
      #Changed on Halloween!  Let's hope it's not scary :-D
      # check_rsf_group <- switch(check_grouping,
      #                           loan="rsf_loan_id",
      #                           borrower="rsf_borrower_id",
      #                           client="rsf_client_id",
      #                           facility="rsf_facility_id",
      #                           program="rsf_program_id",
      #                           none=check_rsf_pfcbl_id,
      #                           NA)
      
      check_rsf_group <- fcase(check_grouping=="loan","rsf_loan_id",
                               check_grouping=="borrower","rsf_borrower_id",
                               check_grouping=="client","rsf_client_id",
                               check_grouping=="facility","rsf_facility_id",
                               check_grouping=="program","rsf_program_id",
                               check_grouping=="none" & check$check_pfcbl_category=="loan","reporting_current_date",
                               default=check_rsf_pfcbl_id)
      
      if (all(is.na(check_rsf_group))) {
        
        check_failed(rsf_pfcbl_id=check$check_rsf_pfcbl_ids,
                     check_asof_date=check$check_asof_date,
                     check_formula_id=check$check_formula_id,
                     check_message=paste0("Check ",check$check_name,"[#",check$check_formula_id,"] Failed to resolve check group using ",check_grouping))
        next;

      }

      check_rsf_group <- unique(c("reporting_current_date",check_rsf_group))
      
      check_data_cols <- unique(c(check_rsf_pfcbl_id,
                                  check_rsf_pfcbl_id_cols,
                                  check_rsf_group,
                                  parameters[,parameter_column_name]))
      
      if (!all(check_data_cols %in% names(rsf_data_wide))) {
        missing_cols <- setdiff(check_data_cols,names(rsf_data_wide))
          
        check_failed(rsf_pfcbl_id=check$check_rsf_pfcbl_ids,
                     check_asof_date=check$check_asof_date,
                     check_formula_id=check$check_formula_id,
                     check_message=paste0("Check ",check$check_name,"[#",check$check_formula_id,"] has missing parameters:\n",
                                          paste0(missing_cols,collapse="\n"),
                                          "\nDo these indicators exist?  Have they been deleted?"))
        next;
      }
      
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
                                                  add_data_flag_function=NULL, #we don't flag these flags
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
                                              grouping_cols=grouping_cols,
                                              check_variance_formula=check_variance_formula),
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
          
          #for formulas to use "CALCULATION_DATE"
          check_data[,CALCULATION_DATE:=reporting_current_date]
          
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
         grouping_cols <- c(grouping_cols,"CALCULATION_DATE")
         
                              setorderv(check_data,
                                        cols=c("rsf_pfcbl_id",grouping_cols))
                               
           
                               check_data[is.na(flag_status),
                                          flag_status := as.logical(eval(parse(text=check_expr))),
                                          by=c(grouping_cols)]
                               
                               check_data[is.na(flag_status)==TRUE,
                                          flag_status:=FALSE]
                               
                               ufields <- grep("^rsf_.*_id$",names(check_data),value=T)
                               
                               messages <- unique(check_data[flag_status==TRUE],
                                                  by=c(ufields,"reporting_current_date","CALCULATION_DATE"))
                               

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
                               
                               check_data[,
                                          check_variance:=as.numeric(NA)]
                               
                               if (nrow(messages) != 0) {
                                 
                                 #it's a grouped formula, but being being applied at the calculation level
                                 #so don't group the flag messages... unless we're deliberately using "concatenate" function intentionally
                                 if (all(messages$rsf_pfcbl_id %in% check_data$rsf_pfcbl_id) &&
                                     !grepl("concatenate\\(",check_expr_msg)) {
                                   
                                   messages <- messages[,
                                                        .(rsf_pfcbl_id,
                                                          flag_status,
                                                          check_message=as.character(eval((parse(text=check_expr_msg)))),
                                                          check_variance=as.numeric(suppressWarnings(
                                                            eval(parse(text=check_variance_formula))
                                                          ))),
                                                        by=c(grouping_cols,"rsf_pfcbl_id")]
                                   
                                 } else {
                                   messages <- messages[,
                                                        .(rsf_pfcbl_id,
                                                          flag_status,
                                                          check_message=as.character(eval((parse(text=check_expr_msg)))),
                                                          check_variance=as.numeric(suppressWarnings(
                                                            eval(parse(text=check_variance_formula))
                                                          ))),
                                                        by=grouping_cols]
                                 }
                                 
                                 messages <- unique(messages)
                                 messages[,
                                          n:=.N,
                                          by=grouping_cols]
                                 
                                 if (any(messages$n > 1)) {
                                   messages <- messages[,
                                                        .(check_message=paste0(check_message,collapse=" & "),
                                                          check_variance=sort(check_variance,na.last=T,decreasing = T)[1]),
                                                        by=c(grouping_cols,"rsf_pfcbl_id","flag_status")]
                                 }
                                 
                                 #round numbers to 2 decimals.
                                 #unless its a highly precise number with preceeding zeros.
                                 
                                 if (any(grepl("\\d",messages$check_message))) {

                                   messages[,
                                            check_message:=gsub("(\\.\\d*?[1-9]\\d)\\d+","\\1",check_message)]
                                   #insert thousands commas into numbers
                                   #but not dates.
                                   #and don't insert commas after decimals (although we should only have two, unelss its highly precise)
                                   #(?<=\\d)(?=(\\d{3})+(?!\\d)(?![-/]))
                                   #(\\d)(?=(\\d{3})+(?!\\d)(?![-/]))
                                   #broken: (?<!\\.\\d{0,10})(?<=\\d{0,5})(?=(\\d{3})+(?![-/\\d]))
                                   #retry: (?:(?<=[[:space:]+=-])\\d+|\\G\\d+?)\\K(?=(\\d{3})+(?!\\d)(?![-/]))
                                   messages[,
                                           check_message:=gsub("(?<=\\d)(?=(\\d{3})+(?!\\d)(?![-/]))", ",", check_message, perl = TRUE)]
                                   
                                   if (any(grepl("\\d+,\\d+",messages$check_message))) {
                                     #(?:(?<=[#\\(\\)])\\d+|\\G\\d+?)\\K,(?=\\d)
                                     messages[,
                                              check_message:=gsub("(?:(?<=[#\\(\\)])\\d+|\\G\\d+?)\\K,(?=\\d)", "", check_message, perl = TRUE)]
                                   }
                                 }
                                 check_data[messages,
                                            `:=`(check_message=i.check_message,
                                                 check_variance=i.check_variance),
                                            on=.(rsf_pfcbl_id,
                                                 flag_status)]
                               } 
                              
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
                         check_message,
                         check_variance=NA)])
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
                            check_message,
                            check_variance=NA)])
        
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
                           check_variance=sort(check_variance,na.last=T,decreasing = T)[1],
                           flag_status=any(flag_status==TRUE)),
                      by=.(rsf_pfcbl_id)]
        
        computed_results <- unique(computed_results)
      }
      
      computed_results[,n:=NULL]

      #if (keep_false_flags==FALSE) computed_results <- computed_results[flag_status==TRUE]
      
      if (!empty(computed_results)) {
        computed_results[,
                      `:=`(check_formula_id=check$check_formula_id,
                           check_asof_date=check$check_asof_date)]
        
        computed_results <- computed_results[,.(rsf_pfcbl_id,
                                          check_asof_date,
                                          check_formula_id,
                                          check_message,
                                          check_variance,
                                          flag_status)]
        
        computed_results[!is.na(check_variance) & (is.infinite(check_variance) | is.nan(check_variance)),check_variance:=1]
        computed_results[!is.na(check_variance),
                         `:=`(check_message=paste0(check_message," (",round(100*abs(abs(check_variance)-1),2),"% variance)"),
                              check_variance=100*abs(abs(check_variance)-1))]
        
        
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
