server_datasets_indicatorflags_module <- function(id,
                                                  evaluation_id,
                                                  check_statuses,
                                                  INDICATOR_FLAGS_SELECTED_EVALUATION_IDS,
                                                  #STATUS_SELECT,
                                                  FLAGS_SELECTED) {
  
  this_evaluation_id <- evaluation_id
  
  registeredObservers <- list()
  register <- function(ob) {
    registeredObservers[[length(registeredObservers)+1]] <<- ob
  }
  #is_ignorable <- ignorable
  moduleServer(id,function(input,output,session) {
    #print(paste0("Id=",id," ignorable=",is_ignorable," ",Sys.time()))
    module_status <- check_statuses
    
    destroyModule <- function() {

      if (length(registeredObservers) > 0) {
        for (i in length(registeredObservers):1) { #Reverse order so NULL assignment doesn't adjust index length in the middle of loop
          registeredObservers[[i]]$destroy()
          registeredObservers[[i]] <- NULL
        }
        this_evaluation_id <<- NULL
      }      
    }
    
    register(observeEvent(INDICATOR_FLAGS_SELECTED_EVALUATION_IDS(), {
      if (all(is.na(INDICATOR_FLAGS_SELECTED_EVALUATION_IDS()))) {
        destroyModule()
      }
    },ignoreInit=TRUE))
    
    register(observeEvent(FLAGS_SELECTED(), {

      selected <- as.logical(input$include_exclude)
      update_selected <- NA
      if (is.na(selected)) return (NULL)
      if (!isTruthy(FLAGS_SELECTED())) return(NULL)
      
      if (FLAGS_SELECTED()=="all") update_selected <- TRUE
      else if (FLAGS_SELECTED()=="none") update_selected <- FALSE
      else if (FLAGS_SELECTED()=="active" && any(module_status=="active")) update_selected <- TRUE
      else if (FLAGS_SELECTED()=="active" && !any(module_status=="active")) update_selected <- FALSE
      else if (FLAGS_SELECTED()=="resolved" && any(module_status=="resolved")) update_selected <- TRUE
      else if (FLAGS_SELECTED()=="resolved" && !any(module_status=="resolved")) update_selected <- FALSE
      else if (FLAGS_SELECTED()=="new" && any(module_status=="new")) update_selected <- TRUE
      else if (FLAGS_SELECTED()=="new" && !any(module_status=="new")) update_selected <- FALSE
     
      #print(paste0(id," ",FLAGS_SELECTED()," ",update_selected," ",selected))
      
      if (selected != update_selected) updateCheckboxInput(session=session,
                                                           inputId="include_exclude",
                                                           value=update_selected)
    },
    ignoreInit = TRUE))
    
    register(observeEvent(input$include_exclude, {
      
      #print(paste0("register(observeEvent(input$include_exclude id==",id," ",Sys.time()))
      include_exclude <- as.logical(input$include_exclude)
      if (is.null(include_exclude) || is.na(include_exclude)) return (NULL)

      #status <- STATUS_SELECT()
      
      current_indicator_flags <- INDICATOR_FLAGS_SELECTED_EVALUATION_IDS()
      update_indicator_flags <- current_indicator_flags
      if (all(is.na(update_indicator_flags))) update_indicator_flags <- c()
      
      if (include_exclude==FALSE && any(update_indicator_flags==this_evaluation_id)) {
        update_indicator_flags <- update_indicator_flags[-which(update_indicator_flags==this_evaluation_id)]
      } else if (include_exclude==TRUE && !any(update_indicator_flags==this_evaluation_id)) {
        update_indicator_flags <- c(update_indicator_flags,this_evaluation_id)
      }
      
      if (length(update_indicator_flags)==0) update_indicator_flags <- c()
      
      #print(paste0("INCLUDIUNG INDICATOR FLAGS=",paste0(indicator_flags,collapse=",")))
      if (!setequal(current_indicator_flags,update_indicator_flags)) {
        #x <- union(setdiff(current_indicator_flags,update_indicator_flags),setdiff(update_indicator_flags,current_indicator_flags))
        #print(paste0("INCLUDIUNG INDICATOR FLAGS ",Sys.time(),": ",paste0(x,collapse=",")))
        INDICATOR_FLAGS_SELECTED_EVALUATION_IDS(update_indicator_flags)
        
      }
    }, ignoreInit = TRUE))
  })
}