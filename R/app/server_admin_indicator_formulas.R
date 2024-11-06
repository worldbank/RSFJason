

server_admin_indicator_formulas.module_ui_indicator_formula <- function(id,
                                                                        formula,
                                                                        rsf_indicators,
                                                                        start.collapsed=TRUE) {
  ns <- NS(id)
  
  is_system <- formula$is_system #Users can view system indicators (and their formulas) but not edit!
  
  formula_label <- div(style="display:flex;flex-direction:row;",
                       div(tags$label("Formula")),
                       div(name="formulaHelper",style="padding-left:12px;vertical-align:bottom;",
                          selectizeInput(inputId=ns("formulaHelper"),
                                         label=NULL,
                                         width="800px",
                                         choices=c("",sort(rsf_indicators$indicator_name)),
                                         selected="",
                                         options=list(placeholder="Formula variables selection helper..."))))
  
  formula_notes <- formula$formula_notes
  if (!isTruthy(formula_notes)) formula_notes <- ""
  
  #To know how large to size the default text area
  formula_eq <- formula$formula
  if (!isTruthy(formula_eq)) formula_eq <- ""
  f_rows <- ceiling(nchar(formula_eq)/100)
  f_rows <- max(f_rows,(1+length(strsplit(formula_eq,"[[:cntrl:]]")[[1]])))
  
  #program_subscriptions <- tagList(fluidRow(column(12,div(HTML(program_subscriptions)))))
  
  ui <- div(name="formula_box",id=ns("ui"),
            box(title=uiOutput(outputId=ns("box_formula_title")),collapsible=TRUE,collapsed=start.collapsed,width=12,
                fluidRow(column(4,style='padding:0 3px 0 15px;',
                                textInput(inputId=ns("formula_title"),
                                          label="Formula Title",
                                          width="100%",
                                          value=formula$formula_title,
                                          placeholder="Title to describe the formula")),
                         column(2,style='padding:0 3px 0 3px;',
                                selectizeInput(inputId=ns("formula_overwrite"),
                                               label="Overwrite",
                                               choices=c(`Allow`="allow",
                                                         `If Missing`="missing",
                                                         `If Unchanged`="unchanged",
                                                         `Deny`="deny"),
                                               selected=formula$formula_overwrite)),
                         column(3,style='padding:0 3px 0 3px;',
                                selectizeInput(inputId=ns("formula_fx_date"),
                                               label="FX Date Method",
                                               choices=c(`Computation Date`="calculation",
                                                         `Each Parameter's Date`="parameter",
                                                         `Changed FX Rate Date`='fx',
                                                         `Not applicable`=""),
                                               selected=formula$formula_fx_date)),
                
                         column(3,align="right",style='float:right;',
                                actionButton(inputId=ns("formula_test"),
                                             label="Test Formula",
                                             icon=icon("flask"),
                                             class="btn-primary"))
                ),
                fluidRow(column(12,style='padding:0px 15px 0 15px;',
                                enabled(state=!is_system,
                                        textAreaInput(inputId=ns("formula"),
                                                      label=formula_label,
                                                      rows=f_rows,
                                                      value=formula_eq,
                                                      placeholder="Please enter a formula...")))
                ),
                
                fluidRow(column(6,style='padding:0 3px 0 15px;',
                                enabled(state=!is_system,
                                        textInput(inputId=ns("formula_sort"),
                                                  label="Sorting & Grouping",
                                                  width="100%",
                                                  value=formula$formula_sort,
                                                  placeholder="Advanced: Group/Sorting"))),
                         
                         column(4,style='padding:0px 3px 0 3px;',
                                enabled(state=!is_system,
                                        textInput(inputId=ns("formula_unit_set_by_indicator_name"),
                                                  label="Calculation Unit Definition",
                                                  width="100%",
                                                  value=formula$formula_unit_set_by_indicator_name,
                                                  placeholder="Advanced: Indicator Name"))),
                         
                         column(2,style=';padding:0px 15px 0 3px;text-align:center',
                                tags$label("Primary Formula"),
                                switchInput(inputId=ns("is_primary_default"),
                                            label=NULL,
                                            value=formula$is_primary_default,
                                            onLabel="YES",
                                            offLabel="NO",
                                            size="small")),
                         
                ),
                
                fluidRow(column(12,style='padding:0px 15px 0 15px;',
                                textAreaInput(inputId=ns("formula_notes"),
                                              label=NULL,
                                              width="100%",
                                              value=formula_notes,
                                              placeholder="Formula description, comments, notes..."))
                ),
                
                fluidRow(align="left",
                         style='padding-bottom:5px;',
                         column(12,div(tags$label("Programs monitoring this Indicator Formula"),"Todo"))),
                
                fluidRow(column(12,style='padding-top:5px;',
                                enabled(state=(!is_system),
                                        actionButton(inputId=ns("formula_delete"),
                                                     label="Delete Formula",
                                                     icon=icon("flask"),
                                                     class="btn-danger"))))
            )
  )
  
  return (ui)
}

server_admin_indicator_formulas.module_session_indicator_formula <- function(id, 
                                                                             module_id,
                                                                             pool,
                                                                             SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS,
                                                                             SERVER_ADMIN_INDICATORS.MODULES) 
{
  ns <- NS(id)
  modules <- isolate({ SERVER_ADMIN_INDICATORS.MODULES() })
  moduleServer(id,function(input,output, session) { 
  
    this.id <- reactiveVal(module_id)

    o1 <- observeEvent(sapply(names(SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS()),function(x) input[[x]]), { 
      
      selected_formulas <- SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS()
      inputs <- reactiveValuesToList(input)
      which_formula <- which(selected_formulas$module_id==this.id())
      
      if (!isTruthy(which_formula)) return (NULL)
      #browser()
      any_changes <- FALSE
      for (item in names(inputs)) {
        
        if (!item %in% names(selected_formulas)) next;
        if (is.null(inputs[[item]])) next; #it's being initialized.  Else set() to NULL will delete it from the data table.
        
        cur_val <- as.character(selected_formulas[which_formula][[item]])
        this_val <- as.character(inputs[[item]])
        
        if (!isTruthy(this_val)) this_val <- as.character(NA)
        
        if (!identical(cur_val,this_val)) {
          set(selected_formulas,
              i=which_formula,
              j=item,
              value=inputs[[item]])
          
          selected_formulas[which_formula,
                            edited:=TRUE]
          any_changes <- TRUE
          #print(paste0("For: ",item," edited #",which_formula," from: ",cur_val," to: ",this_val))
        }
      }
      if (any_changes) {
        SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS(selected_formulas)
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
    
    o3 <- observeEvent(input$is_primary_default, {
      default <- as.logical(input$is_primary_default)
      if (is.na(default)) default <- FALSE

      if (default==FALSE) {
        shinyjs::addClass(id="ui",
                          class="customformula")
      } else {
        shinyjs::removeClass(id="ui",
                             class="customformula")

      }
      if (default==TRUE &&
          any(SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS()[module_id != this.id(),is_primary_default])) {

        updateModules <- SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS()[module_id != this.id() & is_primary_default==TRUE,module_id]
        for (module in updateModules) {
          
          moduleName <- paste0("RSF-",module,"-is_primary_default")
          shinyjs::runjs(paste0("Shiny.setInputValue('",moduleName,"','false',{priority:'event'})"))
          
        }
        
      
      } else if (default==FALSE) {
        #So that if another formula sets this formula's primary to FALSE (in the above block)
        #Then this one will set its own button to off to match the value that was submitted.
        #In the case of user clicking the formula's own button, this is redundant.
        updateSwitchInput(session=session,
                          inputId = "is_primary_default",
                          value=FALSE)
      }
      
      #Is this a quasi-calculation formula?
      if (!any(SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS()$is_primary_default==TRUE)) {
        shinyjs::html(id="server_admin_indicators__quasi_calculation",
                      asis=TRUE,
                      html="<div style='padding:-3px 15px 0px 15px;'><b>Attention</b>: This is a <u>quasi-calculated indicator</u>.  
                            System will calculate this indicator <u>only</u> for Programs or Facilities that explicitly monitor a formula.  
                            Otherwise, manually reported data is expected.  Change this behavior by setting a primary formula.</div>")
      } else {
        shinyjs::html(id="server_admin_indicators__quasi_calculation",
                      asis=TRUE,
                      html="")
      }
    },ignoreInit = FALSE, priority = 1) #Priority to manage the save first to ensure updates are taken on correct data.
    
    o4 <- observeEvent(input$formula_test, {
      
      formula <- SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS()[module_id == this.id()]
      if (any(formula$edited==TRUE) || any(formula$formula_id <= 0)) {
        showNotification(type="warning",
                         ui=h3("Formula's changes must be saved before test can be performed.  Click Save button below."))
        
      } else {
        shinyjs::runjs(paste0("Shiny.setInputValue('action_indicator_review',",formula$formula_id,",{priority:'event'})"))
      }
    })
    
    o5 <- observeEvent(input$formula_delete, {
      
      formula_id <- SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS()[module_id == this.id(),formula_id]
      #print(paste0("Deleting formula_id=",formula_id))
      #RSF-SERVER_ADMIN_INDICATORS.module-1-ui
      #browser()
      if (!isTruthy(formula_id)) return (NULL)
      
      used <- dbGetQuery(pool,"
        select sn.sys_name
        from p_rsf.rsf_program_facility_indicators pfi
        inner join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.rsf_pfcbl_id = pfi.rsf_pfcbl_id
        where pfi.formula_id = $1::int
          and pfi.is_subscribed = true
          and exists(select * from p_rsf.rsf_data rd
        	           where rd.indicator_id = pfi.indicator_id
        						   and rd.rsf_pfcbl_id = any(select ft.to_family_rsf_pfcbl_id
        							                           from p_rsf.view_rsf_pfcbl_id_family_tree ft
        																				 where ft.from_rsf_pfcbl_id = pfi.rsf_pfcbl_id))",
        params=list(formula_id))
      
      setDT(used)
      if (!empty(used)) {
        
        usedby <- tagList()
        for (i in 1:nrow(used)) {
          usedby[[length(usedby)+1]] <- div(used[i,sys_name])
        }
        showNotification(type="error",
                         ui=div(h3("Failed to delete formula: '",input$formula_title,"' because it is actively used by:"),
                                div(style="display:flex;flex-direction:row",usedby),
                                h4("Go to: Setup Tab and remove this formula from these entities, before it can be deleted.")),
                         duration=20)
      
      } else {

        deleted <- NULL
        if (formula_id > 0) { #It's not yet been saved, perhaps added in error?
        
          deleted <- dbGetQuery(pool,
                                "delete from p_rsf.indicator_formulas
                                 where formula_id = $1::int
                                   and not exists(select * from p_rsf.indicator_formulas indf
                                                  inner join p_rsf.indicators ind on ind.indicator_id = indf.indicator_id
                                                  where indf.formula_id = $1::int
                                                    and ind.is_system = true)
                                 returning formula_id",
                                params=list(formula_id))
          
          deleted <- isTruthy(as.numeric(unlist(deleted)))
        
        } else {
          deleted <- TRUE
        }
        
        if (deleted==TRUE) {
          
          formulas <- SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS()
          formulas <- formulas[module_id != this.id()]
          SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS(formulas)
          
          # print(paste0("Deleting: ",paste0("#",ns("ui"))))
          # removeUI(session=session,
          #          selector=paste0("#",ns("ui")))
          # 
          # removeUI(session=session,
          #          selector=gsub("\\.","\\\\.",paste0("#",ns("ui"))))

          #Jquery requires escaped periods
          removeUI(selector=paste0("#",ns("ui")))
          
        } else {
          showNotification(type="error",
                           ui=h3("Failed to delete this formula."))
        }
      
        
      }
      
      
    })
    
    output$box_formula_title <- renderUI({
      #print(this.id())
      #print(SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS())
      index <- which(SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS()$module_id==this.id())
      title <- input$formula_title
      primary <- as.logical(input$is_primary_default)
      if (!isTruthy(primary)) primary <- FALSE
      
      if (!isTruthy(title)) title <- "Missing Formula Title"
      
      if (primary==FALSE) {
        title <- paste0(title," [Program- Facility-specific Formula]")  
      } else {
        title <- paste0(title," [Primary Default Formula]")  
      }
      indicator_name <- SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS()[index]$indicator_name
      if (!isTruthy(indicator_name)) indicator_name <- "ERROR: {MISSING} indicator not found"
      
      formula_id <- SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS()[index]$formula_id
      title <- paste0("<div style='font-size:14px;'><b>",index," Formula #",formula_id,": ",title,"</b></div>")
      
      return(HTML(title))
    })
    
    modules[[length(modules)+1]] <- o1
    modules[[length(modules)+1]] <- o2
    modules[[length(modules)+1]] <- o3
    modules[[length(modules)+1]] <- o4
    modules[[length(modules)+1]] <- o5
    
    SERVER_ADMIN_INDICATORS.MODULES(modules)
  })
}