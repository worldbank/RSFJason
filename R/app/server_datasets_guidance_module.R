server_datasets_guidance_module <- function(id,
                                            guidance_id,
                                            indicator_id,
                                            indicator_check_id,
                                            for_rsf_pfcbl_id,
                                            for_pfcbl_category=NA,
                                            user_id,
                                            INDICATOR_FLAGS_SELECTED_EVALUATION_IDS,
                                            EVENT_GUIDANCE_APPLIED) {
  this_guidance_id <- guidance_id
  this_indicator_id <- indicator_id
  this_indicator_check_id <- indicator_check_id
  this_rsf_pfcbl_id <- for_rsf_pfcbl_id
  this_user_id <- user_id

  this_pfcbl_category <- for_pfcbl_category
  if (!this_pfcbl_category %in% c("global","program","facility")) {
    this_pfcbl_category <- DBPOOL %>% dbGetQuery("
                          select ind.data_category as for_pfcbl_category
												  from p_rsf.indicators ind
												  where ind.indicator_id = $1::int",
                                          params=list(this_indicator_id))
    this_pfcbl_category <- this_pfcbl_category$for_pfcbl_category
    if (!this_pfcbl_category %in% c("global","program","facility")) this_pfcbl_category <- "facility"
  }
  
  if (!isTruthy(this_pfcbl_category)) this_pfcbl_category <- "facility"
  
  registeredObservers <- list()
  register <- function(ob) {
    registeredObservers[[length(registeredObservers)+1]] <<- ob
  }

  destroyModule <- function() {
    if (length(registeredObservers) > 0) {
      #remove_shiny_inputs(id,input) #remove input first, before destroying observers
      
      for (i in length(registeredObservers):1) { #Reverse order so NULL assignment doesn't adjust index length in the middle of loop
        registeredObservers[[i]]$destroy()
        registeredObservers[[i]] <<- NULL
      }
      output[["new_guidance_name"]] <- NULL
      
      this_guidance_id <<- NULL
      this_indicator_id <<- NULL
      this_indicator_check_id <<- NULL
      this_rsf_pfcbl_id <<- NULL
      this_user_id <<- NULL
      this_pfcbl_category <<- NULL
    }
  }
  
  moduleServer(id,function(input,output,session) {
    
    register(observeEvent(input$guidance_apply, {
      guidance <- input$guidance_text
      resolving <- fcase(input$guidance_resolving=="RESOLVE",TRUE,
                         default=FALSE)
      ignoring <- as.character(input$guidance_resolving)=="IGNORE"
      tolerance <- as.character(input$tolerance_variance)
      
      if (!isTruthy(tolerance)) tolerance <- "0" 
      
      if (grepl("%$",tolerance)) {
        tolerance <- gsub("%$","",tolerance)
        tolerance <- as.numeric(tolerance)
        tolerance <- tolerance / 100
      }
      
      tolerance <- as.numeric(tolerance)
      
      overwrite_class <- input$guidance_class
      glevel <- tolower(input$guidance_set_program_level)
      
      #if (!isTruthy(reporting_cohort_id)) return(NULL)
      if (!isTruthy(guidance)) guidance <- NA
      if (!isTruthy(resolving)) resolving <- FALSE #If its FALSE isTruthy will by FALSE, too
      if (!isTruthy(overwrite_class) || !overwrite_class %in% c("critical","error","warning","info")) overwrite_class <- as.character(NA)
      if (!isTruthy(glevel)) glevel <- "facility"
      if (!isTruthy(tolerance)) tolerance <- 0
      if (tolerance < 0) tolerance <- abs(tolerance)

      #cannot change once created
      if (isTruthy(as.numeric(this_guidance_id))) {
        pfcbl_category <- this_pfcbl_category
      } else {
        pfcbl_category <- glevel
        if (glevel=="facility" && this_pfcbl_category == "program") pfcbl_category <- "program"
      }
      
      check <- DBPOOL %>% dbGetQuery("select 
                                      check_class,
                                      coalesce(variance_tolerance_allowed,false)::bool as variance_tolerance_allowed
                                      from p_rsf.indicator_checks ic
                                      where ic.indicator_check_id = $1::int",
                                     params=list(this_indicator_check_id))
      check_class <- check$check_class
      
      if (is.na(check_class)) check_class <- "none"
      if ((ignoring==TRUE | resolving == TRUE) & check_class %in% c("critical")) {
        return(showNotification(type="error",
                                h2("Auto-resolving guidance is not allowed for CRITICAL ERRORS.  Select User Review for this guidance.")))
      }
      if (check$variance_tolerance_allowed==FALSE) {
        tolerance <- 0
      }
      
      show_modal_spinner(spin = "circle",
                         color = "green",
                         text = "Applying Updates...",
                         session = shiny::getDefaultReactiveDomain())
      
      guidance_id <- DBPOOL %>% db_program_facility_checks_add_update_guidance(guidance_id=this_guidance_id,
                                                                              indicator_id=this_indicator_id,
                                                                              indicator_check_id=this_indicator_check_id,
                                                                              for_rsf_pfcbl_id=this_rsf_pfcbl_id,
                                                                              user_id=this_user_id,
                                                                              guidance=guidance,
                                                                              auto_resolve=resolving,
                                                                              ignoring=ignoring,
                                                                              overwrite_check_class=overwrite_class,
                                                                              tolerance_variance=tolerance,
                                                                              pfcbl_category=pfcbl_category)
      
      apply_to <- na.omit(as.numeric(INDICATOR_FLAGS_SELECTED_EVALUATION_IDS()))
      if (length(apply_to) > 0) {
          DBPOOL %>% dbExecute("with current_evaluations as (
                                	select
                                		rdc.evaluation_id,
                                		rdc.indicator_check_guidance_id,
                                		icg.guidance,
                                		rdc.check_status_comment,
                                		coalesce(icg.guidance = rdc.check_status_comment,false) OR rdc.check_status_comment is NULL as update_guidance_comment
                                	from p_rsf.rsf_data_checks rdc
                                	left join p_rsf.indicator_check_guidance icg on icg.indicator_check_guidance_id = rdc.indicator_check_guidance_id
                                	where rdc.evaluation_id = any(select unnest(string_to_array($2::text,','))::int)
                                )
                                update p_rsf.rsf_data_checks rdc
                                   set indicator_check_guidance_id = icg.indicator_check_guidance_id,
                                			 check_status_comment = case when ce.update_guidance_comment = true then icg.guidance else rdc.check_status_comment end
                                from current_evaluations ce,p_rsf.indicator_check_guidance icg 
                                 where rdc.evaluation_id = ce.evaluation_id
                                   and icg.indicator_check_guidance_id = $1::int",
                              params=list(guidance_id,
                                          paste0(apply_to,collapse=",")))
      }
      
      EVENT_GUIDANCE_APPLIED(EVENT_GUIDANCE_APPLIED()+1)
    }, ignoreInit = TRUE))
    
    output$new_guidance_name <- renderUI({

      glevel <- tolower(input$guidance_set_program_level)
      
      guidance_name <- "New Facility Guidance"
      if (glevel=="global") guidance_name <- "New Universal Gudance"
      else if (glevel=="program") guidance_name <- "New RSF Program Guidance"
      
      HTML(paste0("<div>",
                  format_html_indicator(indicator_name=guidance_name,
                                        data_category=glevel,
                                        data_type="",
                                        is_system=FALSE,
                                        is_calculated=FALSE,
                                        options_group_name=NA),
                  "</div>"))
    })
    
    output$level_description <- renderText({
      
      glevel <- tolower(input$guidance_set_program_level)
      resolving <- tolower(input$guidance_resolving)
      overwrite <- tolower(input$guidance_class)
      
      level <- fcase(glevel=="global",
                   "Universal Guidance will be applied to all RSF Programs and all RSF Facilities. Universal Guidance is not recommended.",
                   glevel=="program",
                   "All Facilities under this RSF Program receive this guidance (except for any facilities with different facility-specific guidance).",
                   glevel=="facility",
                   "This guidance will be applied at the individual facility level; this guidance may be applied to multiple different facilities.",
                   default="Level error")
      
      resolve <- fcase(resolving=="review","User Review ensures flags are active and must be resolved intentionally.",
                       resolving=="resolve","Flags will be auto-resolved by the system, but will be retained for logging or audit purposes.",
                       resolving=="ignore","Flags will be fully ignored and not be retained in the database (this is not recommended)",
                       default="Resolution Error")
      
      class <- fcase(isTruthy(overwrite)==FALSE,"Flags will retain their default error classification.",
                     default=paste0("Flags will be re-classified as ",toupper(overwrite)))
      
      return (paste0(level," ",resolve," ",class))
      
    })
  })
  
  return(environment())
}