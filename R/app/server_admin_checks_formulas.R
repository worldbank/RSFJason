

server_admin_checks_formulas.module_ui <- function(id,
                                                   formula,
                                                   rsf_indicators,
                                                   start.collapsed=TRUE) {
  ns <- NS(id)
  
  formula_label <- div(style="display:flex;flex-direction:row;",
                       div(tags$label("Formula")),
                       div(name="formulaHelper",style="padding-left:12px;vertical-align:bottom;",
                           selectizeInput(inputId=ns("formulaHelper"),
                                          label=NULL,
                                          width="800px",
                                          choices=c("",sort(rsf_indicators$indicator_name)),
                                          selected="",
                                          options=list(placeholder="Formula variables selection helper..."))))
  
  formula_comments <- formula$formula_comments
  if (!isTruthy(formula_comments)) formula_comments <- ""
  c_rows <- ceiling(nchar(formula_comments)/100)
  c_rows <- max(c_rows,(1+length(strsplit(formula_comments,"[[:cntrl:]]")[[1]])))
  
  formula_eq <- formula$formula
  if (!isTruthy(formula_eq)) formula_eq <- ""
  f_rows <- ceiling(nchar(formula_eq)/100)
  f_rows <- max(f_rows,(1+length(strsplit(formula_eq,"[[:cntrl:]]")[[1]])))
  
  formula_rmsg <- formula$formula_result_message
  if (!isTruthy(formula_rmsg)) formula_rmsg <- ""
  r_rows <- ceiling(nchar(formula_rmsg)/100)
  r_rows <- max(r_rows,(1+length(strsplit(formula_rmsg,"[[:cntrl:]]")[[1]])))
  
  ui <- div(name="formula_box",id=ns("ui"),
            box(title=uiOutput(outputId=ns("box_formula_title")),collapsible=TRUE,collapsed=start.collapsed,width=12,
                fluidRow(column(4,style='padding:0 3px 0 15px;',
                                textInput(inputId=ns("check_formula_title"),
                                          label="Formula Title",
                                          width="100%",
                                          value=formula$check_formula_title,
                                          placeholder="Title to describe the formula")),
                         column(3,style='padding:0 3px 0 3px;',
                                selectizeInput(inputId=ns("formula_fx_date"),
                                               label="FX Date Method",
                                               choices=c(`Computation Date`="calculation",
                                                         `Each Parameter's Date`="parameter",
                                                         `Changed FX Rate Date`='fx',
                                                         `Not applicable`=""),
                                               selected=formula$formula_fx_date)),                         
                         column(2,style='padding:0 3px 0 3px;',
                                selectizeInput(inputId=ns("auto_resolve"),
                                               width="100%",
                                               label="Auto-Resolve",
                                               choices=c(`Yes`=TRUE,`No`=FALSE),
                                               selected=formula$auto_resolve,
                                               options=list(placeholder="Select..."))),
                         column(3,align="right",style='float:right;',
                                actionButton(inputId=ns("formula_test"),
                                             label="Test Formula",
                                             icon=icon("flask"),
                                             class="btn-primary"))
                ),
                fluidRow(column(12,style='padding:0px 15px 0 15px;',
                                textAreaInput(inputId=ns("formula"),
                                              label=formula_label,
                                              rows=f_rows,
                                              value=formula_eq,
                                              placeholder="Please enter a formula..."))
                ),
                
                fluidRow(column(12,style='padding:0px 15px 0 15px;',
                                textAreaInput(inputId=ns("formula_result_message"),
                                              label="Formula Flag Message",
                                              rows=r_rows,
                                              value=formula_rmsg,
                                              placeholder='Please enter a "quoted" flag message text; or formula to compute a flag message...'))),

                fluidRow(column(12,style='padding:0px 15px 0 15px;',
                                textAreaInput(inputId=ns("formula_comments"),
                                              label=NULL,
                                              width="100%",
                                              value=formula_comments,
                                              placeholder="Formula description, comments, notes..."))
                ),
                
                fluidRow(column(12,style='padding-top:5px;',
                                actionButton(inputId=ns("formula_delete"),label="Delete Formula",icon=icon("trash"),class="btn btn-danger")))
            )
  )
  
  return (ui)
}

server_admin_checks_formulas.module_server <- function(id, 
                                                       module_id,
                                                       pool,
                                                       SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS,
                                                       SERVER_ADMIN_CHECKS.MODULES) {
  ns <- NS(id)
  modules <- isolate({ SERVER_ADMIN_CHECKS.MODULES() })
  #print(paste0("server_admin_checks_formulas.module_server: modules count=",length(modules)))
  moduleServer(id,function(input,output, session) { 
    
    this.id <- reactiveVal(module_id)
    
    o1 <- observeEvent(sapply(names(SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS()),function(x) input[[x]]), { 
      
      selected_formulas <- SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS()
      inputs <- reactiveValuesToList(input)
      which_formula <- which(selected_formulas$module_id==this.id())
      
      if (!isTruthy(which_formula)) return (NULL)
      
      any_changes <- FALSE
      for (item in names(inputs)) {
        
        if (!item %in% names(selected_formulas)) next;
        if (is.null(inputs[[item]])) next; #it's being initialized.  Else set() to NULL will delete it from the data table.
        
        cur_val <- as.character(selected_formulas[which_formula][[item]])
        this_val <- as.character(inputs[[item]])
        
        if (!isTruthy(this_val)) this_val <- as.character(NA)
        if (!isTruthy(cur_val)) cur_val <- as.character(NA)
        
        if (!identical(cur_val,this_val)) {
          set(selected_formulas,
              i=which_formula,
              j=item,
              value=inputs[[item]])
          
          selected_formulas[which_formula,
                            edited:=TRUE]
          any_changes <- TRUE
        }
      }
      if (any_changes) {
        SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS(selected_formulas)
      }        
      
      
    }, ignoreInit = TRUE, priority=2)
    
    o2 <- observeEvent(input$formulaHelper, {
      if (isTruthy(input$formulaHelper)) {
        formula <- input$formula
        if (!isTruthy(formula)) formula <- ""
        
        updateTextAreaInput(session=session,
                            inputId="formula",
                            value=paste0(formula," ",input$formulaHelper))
        
        updateSelectizeInput(session=session,
                             inputId="formulaHelper",
                             selected="")
      }
    })
    
    o3 <- observeEvent(input$formula_test, {
      
      formula <- SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS()[module_id == this.id()]
      if (any(formula$edited==TRUE) || any(formula$check_formula_id <= 0)) {
        showNotification(type="warning",
                         ui=h3("Formula's changes must be saved before test can be performed.  Click Save button below."))
        
      } else {
        shinyjs::runjs(paste0("Shiny.setInputValue('server_admin_checks_review__test',",formula$check_formula_id,",{priority:'event'})"))
      }
    })
    
    o4 <- observeEvent(input$formula_delete, {
      
      check_formula_id <- SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS()[module_id == this.id(),check_formula_id]
      if (!isTruthy(check_formula_id)) return (NULL)
      
      used <- dbGetQuery(pool,"
          select sn.sys_name
          from p_rsf.rsf_program_facility_checks pfc
          inner join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.rsf_pfcbl_id = pfc.rsf_pfcbl_id
          where pfc.check_formula_id = $1::int
            and pfc.is_subscribed = true",
          params=list(check_formula_id))
      
      setDT(used)
      if (!empty(used)) {
        
        usedby <- tagList()
        for (i in 1:nrow(used)) {
          usedby[[length(usedby)+1]] <- div(used[i,sys_name])
        }
        showNotification(type="error",
                         ui=div(h3("Failed to delete formula: '",input$check_formula_title,"' because it is actively used by:"),
                                div(style="display:flex;flex-direction:row",usedby),
                                h4("Go to: Setup Tab and remove this check from these entities, before it can be deleted.")),
                         duration=20)
        
      } else {
        
        deleted <- NULL
        if (check_formula_id > 0) { #It's not yet been saved, perhaps added in error?
          
          deleted <- dbGetQuery(pool,
                                "delete from p_rsf.indicator_check_formulas
                                 where check_formula_id = $1::int
                                 returning check_formula_id",
                                params=list(check_formula_id))
          
          deleted <- isTruthy(as.numeric(unlist(deleted)))
          
        } else {
          deleted <- TRUE
        }
        
        if (deleted==TRUE) {
          
          formulas <- SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS()
          formulas <- formulas[module_id != this.id()]
          SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS(formulas)
          
          removeUI(selector=paste0("#",ns("ui")))
          
        } else {
          showNotification(type="error",
                           ui=h3("Failed to delete this formula."))
        }
        
        
      }
      
      
    })
    
    output$box_formula_title <- renderUI({

      index <- which(SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS()$module_id==this.id())
      title <- input$check_formula_title
      
      if (!isTruthy(title)) title <- SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS()[index]$check_formula_title
      if (!isTruthy(title)) title <- "Missing Formula Title"
      formula_id <- SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS()[index]$check_formula_id
      title <- paste0("<div style='font-size:14px;'><b>",index," Formula #",formula_id,": ",title,"</b></div>")
      
      return(HTML(title))
    })
    
    modules[[length(modules)+1]] <- o1
    modules[[length(modules)+1]] <- o2
    modules[[length(modules)+1]] <- o3
    modules[[length(modules)+1]] <- o4
    
    SERVER_ADMIN_CHECKS.MODULES(modules)
  })
}
