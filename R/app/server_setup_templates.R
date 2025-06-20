
server_setup_template__header_module_server <- function(id,
                                                        pool,
                                                        RSF_INDICATORS,
                                                        RSF_INDICATOR_FORMULAS,
                                                        RSF_CHECK_FORMULAS,
                                                        SERVER_SETUP_TEMPLATES__ACTIVE_HEADER_MODULES) {
  
  ns <- NS(id)
  modules <- isolate({ SERVER_SETUP_TEMPLATES__ACTIVE_HEADER_MODULES() })

  moduleServer(id,
               function(input,output, session) {
                 
    this.id <- reactiveVal(as.numeric(gsub("^.*headerModule(\\d+)$","\\1",id)))
    
    if (!isTruthy(this.id())) stop(paste0("Failed to resolve header module ID: ",id))
    
    
    
    
    
    
    o1 <- observeEvent(input$template_header_sheet_name, {
     
     if (isTruthy(input$template_header_sheet_name)) {
       tryCatch({
         dbExecute(pool,"
            update p_rsf.rsf_program_facility_template_headers fth
            set template_header_sheet_name = NULLIF($1::text,'')
            where fth.header_id = $2::int",
            params=list(as.character(input$template_header_sheet_name),
                        this.id()))
       },
       warning = function(w) { showNotification(ui=h3("Failed to save: ",conditionMessage(w))) },
       error = function(e) { showNotification(ui=h3("Failed to save: ",conditionMessage(e))) })
     }
    },ignoreInit=TRUE)

    o2 <- observeEvent(input$template_header_text, {
      if (isTruthy(input$template_header_text)) {
        tryCatch({
        dbExecute(pool,"
          update p_rsf.rsf_program_facility_template_headers fth
          set template_header = NULLIF($1::text,'')
          where fth.header_id = $2::int",
                  params=list(as.character(input$template_header_text),
                              this.id()))
        },
        warning = function(w) { showNotification(ui=h3("Failed to save: ",conditionMessage(w))) },
        error = function(e) { showNotification(ui=h3("Failed to save: ",conditionMessage(e))) })
      }      
      
    },ignoreInit=TRUE)
    
    o3<- observeEvent(input$template_header_action, {
      
      # choices=c(`Default`="default",  #Matched: No action 
      #           `Ignore`="ignore",    #Matched: Do nothing
      #           `Enable`="remap",     #Matched: map to indicator (and monitor it)
      #           `Disable`="unmap",  #Matched: map to indicator (and disable it)
      #           
      #           `Set Check`="check",       #Subscribe to check
      #           `Set Formula`="calculate", #Set/Subscribe to Formula/Indicator
      #           `Set Value`="parse"        #Match the content and parse {} into values for {indicator} and subscribe to it.
      #           
      action <- input$template_header_action
      if (!isTruthy(action)) {
        return(showNotification(type="error",
                         ui=h3("Action must be selected")))
      }
      tryCatch({
        dbExecute(pool,"
          update p_rsf.rsf_program_facility_template_headers fth
          set action = $1::text,
              map_indicator_id = NULL,
              map_formula_id = NULL,
              map_check_formula_id = NULL
          where fth.header_id = $2::int",
                  params=list(action,
                              this.id()))
  
        if (action %in% c("default","ignore","parse")) {
          
          updateSelectizeInput(session=session,
                            inputId="template_header_target_name",
                            choices="",
                            selected="")
          
        
        } else if (action %in% c("remap","unmap")) {
          
          updateSelectizeInput(session=session,
                            inputId="template_header_target_name",
                            choices=c("",do.call(setNames,
                                                 RSF_INDICATORS()[order(indicator_name),
                                                                .(object=indicator_id,nm=indicator_name)])),
                            selected="")
        
        }  else if (action %in% c("check")) {
          
          updateSelectizeInput(session=session,
                            inputId="template_header_target_name",
                            choices=c("",do.call(setNames,
                                                 RSF_CHECK_FORMULAS()[order(check_name_formula_title),
                                                                  .(object=check_formula_id,nm=check_name_formula_title)])))
        
        }  else if (action %in% c("calculate")) {
          
          updateSelectizeInput(session=session,
                            inputId="template_header_target_name",
                            choices=c("",do.call(setNames,
                                                 RSF_INDICATOR_FORMULAS()[order(indicator_name_formula_title),
                                                                  .(object=formula_id,nm=indicator_name_formula_title)])))
                            
        
        } else {
          showNotification(type="error",
                           ui=h3("Unknown action:",action))
          
        }
      },
      warning = function(w) { showNotification(ui=h3("Failed to save: ",conditionMessage(w))) },
      error = function(e) { showNotification(ui=h3("Failed to save: ",conditionMessage(e))) })  
      
    },ignoreInit=TRUE)
    

    o4 <- observeEvent(input$template_header_target_name, {
      
      action <- input$template_header_action
      if (!isTruthy(action)) {
        return(showNotification(type="error",
                                ui=h3("Action must be selected")))
      }

      tryCatch({
        if (action %in% c("default","ignore","parse")) {
          dbExecute(pool,"
          update p_rsf.rsf_program_facility_template_headers fth
          set map_indicator_id = NULL,
              map_formula_id = NULL,
              map_check_formula_id = NULL
          where fth.header_id = $1::int",
                    params=list(this.id()))
          
        
        
        } else if (action %in% c("remap","unmap")) {
          
  
          dbExecute(pool,"
          update p_rsf.rsf_program_facility_template_headers fth
          set map_indicator_id = $2,
              map_formula_id = NULL,
              map_check_formula_id = NULL
          where fth.header_id = $1::int",
                    params=list(this.id(),
                                as.numeric(input$template_header_target_name)))
        
        
        } else if (action %in% c("check")) {
          
          dbExecute(pool,"
          update p_rsf.rsf_program_facility_template_headers fth
          set map_indicator_id = NULL,
              map_formula_id = NULL,
              map_check_formula_id = $2::int
          where fth.header_id = $1::int",
                    params=list(this.id(),
                                as.numeric(input$template_header_target_name)))
          
        
        } else if (action %in% c("calculate")) {
          
          dbExecute(pool,"
          update p_rsf.rsf_program_facility_template_headers fth
          set map_indicator_id = NULL,
              map_formula_id = $2::int,
              map_check_formula_id = NULL
          where fth.header_id = $1::int",
                    params=list(this.id(),
                                as.numeric(input$template_header_target_name)))
        }
      },
      warning = function(w) { showNotification(ui=h3("Failed to save: ",conditionMessage(w))) },
      error = function(e) { showNotification(ui=h3("Failed to save: ",conditionMessage(e))) })      
    },ignoreInit=TRUE)

    o5 <- observeEvent(input$template_header_comment, {
      
      tryCatch({
        dbExecute(pool,"
          update p_rsf.rsf_program_facility_template_headers fth
          set comment = NULLIF($1::text,'')
          where fth.header_id = $2::int",
                  params=list(as.character(input$template_header_comment),
                              this.id()))
      },
      warning = function(w) { showNotification(ui=h3("Failed to save: ",conditionMessage(w))) },
      error = function(e) { showNotification(ui=h3("Failed to save: ",conditionMessage(e))) })
      
    },ignoreInit=TRUE)
    
    o6 <- observeEvent(input$template_delete_header, {
    
       tryCatch({
         dbExecute(pool,"
          delete from p_rsf.rsf_program_facility_template_headers fth
          where fth.header_id = $1::int",
         params=list(this.id()))
         
         removeUI(selector=paste0("#",ns("ui")))
       },
       warning = function(w) { showNotification(ui=h3("Failed to save: ",conditionMessage(w))) },
       error = function(e) { showNotification(ui=h3("Failed to save: ",conditionMessage(e))) })
      
       #SERVER_SETUP_TEMPLATES__LOAD_HEADERS(SERVER_SETUP_TEMPLATES__LOAD_HEADERS()+1)
     },ignoreInit=TRUE)
     
    modules[[length(modules)+1]] <- o1
    modules[[length(modules)+1]] <- o2
    modules[[length(modules)+1]] <- o3
    modules[[length(modules)+1]] <- o4
    modules[[length(modules)+1]] <- o5
    modules[[length(modules)+1]] <- o6
    
    SERVER_SETUP_TEMPLATES__ACTIVE_HEADER_MODULES(modules)
  })
}

server_setup_template__header_module_ui <- function(id,
                                                    header,
                                                    template,
                                                    rsf_indicators,
                                                    rsf_indicator_formulas,
                                                    rsf_check_formulas,
                                                    is_static=TRUE) {
  ns <- NS(id)
  
  is_system <- template$is_system #Users can view system templates!
  
  template_sheet  <- div(style="display:flex;flex-flow:row nowrap;min-width:150px;width:200px;padding:0 0 0 2px;white-space:nowrap;",
                         textAreaInput(inputId=ns("template_header_sheet_name"),
                                   label=NULL,
                                   value=header$template_header_sheet_name,
                                   width="100%",
                                   height=paste0(34+(min(1,floor(nchar(header$template_header_sheet_name)/30),na.rm=T)*38),"px"),
                                   placeholder="Anywhere"))
  

  
    header_template <- div(style="display:flex;flex-flow:row nowrap;min-width:450px;padding:0 0 0 2px;flex-grow:1;white-space:nowrap;",
                           textAreaInput(inputId=ns("template_header_text"),
                                     label=NULL,
                                     value=header$template_header,
                                     height=paste0(34+(min(1,floor(nchar(header$template_header)/90),na.rm=T)*38),"px"),
                                     width="100%",
                                     placeholder="Enter template column to match"))
   
    header_action <- div(style="display:flex;flex-flow:row nowrap;min-width:175px;width:175px;padding:0 0 0 2px;white-space:nowrap;",
                         selectizeInput(inputId=ns("template_header_action"),
                                        label=NULL,
                                        width="100%",
                                        choices=c(`Default`="default",  #Matched: No action 
                                                  `Ignore`="ignore",    #Matched: Do nothing
                                                  `Enable`="remap",     #Matched: map to indicator (and monitor it)
                                                  `Disable`="unmap",  #Matched: map to indicator (and disable it)
                                                  
                                                  `Set Check`="check",       #Subscribe to check
                                                  `Set Formula`="calculate", #Set/Subscribe to Formula/Indicator
                                                  `Set Value`="parse"        #Match the content and parse {} into values for {indicator} and subscribe to it.
                                        ),
                                        selected=header$action))
    
    
    header_mapping <- NULL
    if (header$action %in% c("default","ignore","parse")) {
      header_mapping <- div(style="display:flex;flex-flow:row nowrap;min-width:400px;padding:0 0 0 2px;white-space:nowrap;",
                            selectizeInput(inputId=ns("template_header_target_name"),
                                           label=NULL,
                                           width="100%",
                                           choices="",
                                           selected="",
                                           options=list(placeholder="Header not mapped")))
      
    } else if (header$action %in% c("remap","unmap")) {

      header_mapping <- div(style="display:flex;flex-flow:row nowrap;min-width:400px;padding:0 0 0 2px;white-space:nowrap;",
                            selectizeInput(inputId=ns("template_header_target_name"),
                                           label=NULL,
                                           width="100%",
                                           choices=c("",do.call(setNames,
                                                                rsf_indicators[order(indicator_name),
                                                                               .(object=indicator_id,nm=indicator_name)])),
                                           selected=header$map_indicator_id,
                                           options=list(placeholder="Header not mapped")))
    } else if (header$action %in% c("check")) {
      
      header_mapping <- div(style="display:flex;flex-flow:row nowrap;min-width:400px;padding:0 0 0 2px;white-space:nowrap;",
                            selectizeInput(inputId=ns("template_header_target_name"),
                                           label=NULL,
                                           width="100%",
                                           choices=c("",do.call(setNames,
                                                                RSF_CHECK_FORMULAS()[order(check_name_formula_title),
                                                                                     .(object=check_formula_id,nm=check_name_formula_title)])),
                                           selected=header$map_formula_check_id,
                                           options=list(placeholder="Header not mapped")))
    }  else if (header$action %in% c("calculate")) {
      
      header_mapping <- div(style="display:flex;flex-flow:row nowrap;min-width:400px;padding:0 0 0 2px;white-space:nowrap;",
                            selectizeInput(inputId=ns("template_header_target_name"),
                                           label=NULL,
                                           width="100%",
                                           choices=c("",do.call(setNames,
                                                           RSF_INDICATOR_FORMULAS()[order(indicator_name_formula_title),
                                                                                    .(object=formula_id,nm=indicator_name_formula_title)])),
                                           selected=header$map_formula_id,
                                           options=list(placeholder="Header not mapped")))
    }      
      

    header_comment <- div(style="display:flex;flex-flow:row nowrap;min-width:150px;padding:0 0 0 2px;white-space:nowrap;",
                          textAreaInput(inputId=ns("template_header_comment"),
                                    label=NULL,
                                    value=header$comment,
                                    width="100%",
                                    height=paste0(34+(min(1,floor(nchar(header$comment)/30),na.rm=T)*38),"px"),
                                    placeholder="Comments"))
    
    
  header_remove  <- div(style="display:flex;flex-flow:row nowrap;width:40px;padding:0 0 0 2px;",
                                     
                        actionButton(inputId=ns("template_delete_header"),
                                     label=NULL,
                                     #onclick=paste0("Shiny.setInputValue(`server_setup_templates__header_delete`,",header$header_id,",{priority:`event`})"),
                                     icon=icon("trash"),
                                     class="btn-danger"))
  
  header_edit  <- div(style="display:flex;flex-flow:row nowrap;width:40px;padding:0 0 0 2px;",
                        
                        actionButton(inputId=ns("template_edit_header"),
                                     label=NULL,
                                     onclick=paste0("Shiny.setInputValue(`server_setup_templates__header_edit`,",header$header_id,",{priority:`event`})"),
                                     icon=icon("pen-to-square"),
                                     class="btn-primary"))
  
  action <- header_remove
  
  if (is_static==TRUE) {
    template_sheet <- disabled(template_sheet)
    header_template <- disabled(header_template)
    header_action <- disabled(header_action)
    header_mapping <- disabled(header_mapping)
    header_comment <- disabled(header_comment)
    action <- header_edit
  }
  
  ui <- div(name="template_header_box",
            #style=switch(!is_label_header,"height:36px","''"),
            id=ns("ui"),
            div(style="display:flex;flex-flow:row nowrap;justify-content:left;align-items:start;",
                template_sheet,
                header_template,
                header_action,
                header_mapping,
                header_comment,
                action))
  
  return (ui)
}

RSF_TEMPLATES <- eventReactive(LOGGEDIN(), {
  
  if (!isTruthy(LOGGEDIN())) return (NULL)
  
  #only non-system templates because system templates will all be downloaded with valid headers
  #And presumably not headers to ignore as they would not be uploaded in the first place and not downloaded irrelevently in the second place.
  templates <- DBPOOL %>% dbGetQuery("
    select
      template_id,
      template_name,
      template_key,
      description,
      is_reportable,
      template_has_static_row_ids,
      is_setup_template,
      file_extension,
      is_system
    from p_rsf.reporting_templates
    where is_system = false
    order by template_name;")
  
  setDT(templates)
  return (templates)
},ignoreNULL = FALSE)

SERVER_SETUP_TEMPLATES__ACTIVE_HEADER_MODULES <- reactiveVal(list())
SERVER_SETUP_TEMPLATES__ACTIVE_HEADERS <- reactiveVal(c())

observeEvent(RSF_TEMPLATES(), {
  
  templates <- RSF_TEMPLATES()
  if (empty(templates)) return (NULL)
  
  choices <- c(`None`="",
               setNames(templates$template_id,
                        templates$template_name))
  
  updateSelectizeInput(session=session,
                       inputId="ui_setup__template_selected",
                       choices=choices,
                       selected="")
})

SERVER_SETUP_TEMPLATES__SELECTED_TEMPLATE_DATA <- eventReactive(SERVER_SETUP_TEMPLATES__SELECTED_TEMPLATE(), {
  
  template <- SERVER_SETUP_TEMPLATES__SELECTED_TEMPLATE()
  
  if (empty(template)) return (NULL)
  
  tdata <- DBPOOL %>% dbGetQuery("
  select * 
  from
  p_rsf.view_rsf_program_facility_template_headers fth
  where fth.template_id = $1::int
    and fth.rsf_pfcbl_id = any(select ft.to_family_rsf_pfcbl_id
                             from p_rsf.view_rsf_pfcbl_id_family_tree ft
                             where ft.from_rsf_pfcbl_id = $2::int
                               and ft.to_pfcbl_rank <= ft.from_pfcbl_rank)",
  params=list(template$template_id,
              template$selected_rsf_pfcbl_id))
  
  return (tdata)
  
},ignoreNULL = FALSE)


SERVER_SETUP_TEMPLATES__SELECTED_TEMPLATE <- eventReactive(c(input$ui_setup__templates_program_facilities,
                                                             input$ui_setup__template_selected), 
{
  
  selected_template_id <- as.numeric(input$ui_setup__template_selected)
  selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__templates_program_facilities)
  
  if (!isTruthy(selected_template_id)) return (NULL)
  if (!isTruthy(selected_rsf_pfcbl_id)) return (NULL)
  if (empty(RSF_TEMPLATES())) return (NULL)
  
  selected_rsf_pfcbl_name <- (DBPOOL %>% dbGetQuery("select pfcbl_name from p_rsf.view_rsf_pfcbl_id_current_sys_names where rsf_pfcbl_id = $1::int",selected_rsf_pfcbl_id))$pfcbl_name
  selected_rsf_pfcbl_name <- gsub("^[a-z]:","",selected_rsf_pfcbl_name)
  
  template <- (RSF_TEMPLATES()[template_id==selected_template_id])
  template[,
           selected_rsf_pfcbl_id:=selected_rsf_pfcbl_id]
  
  template[,
           selected_rsf_pfcbl_name:=selected_rsf_pfcbl_name]
  
  return (template)
})

SERVER_SETUP_TEMPLATES__HEADER_ACTIONS <- eventReactive(SERVER_SETUP_TEMPLATES__SELECTED_TEMPLATE(), {
  
  
  selected_template <- SERVER_SETUP_TEMPLATES__SELECTED_TEMPLATE()
  
  if (empty(RSF_TEMPLATES())) return (NULL)
  if (empty(selected_template)) return (NULL)
  
  {
    
    headers <- DBPOOL %>% dbGetQuery("
    select
      fth.rsf_pfcbl_id,
      fth.template_id,
      fth.action,
      fth.template_header,
      fth.remap_header,
      fth.template_header_sheet_name,
      fth.template_header_encounter_index,
      fth.header_id,
      fth.map_indicator_id,
      fth.map_formula_id,
      fth.map_check_formula_id,
      coalesce(fth.comment,'') as comment
    from p_rsf.rsf_program_facility_template_headers fth
    where fth.rsf_pfcbl_id = $1::int
      and fth.template_id = $2::int",
    params=list(selected_template$selected_rsf_pfcbl_id,
                selected_template$template_id))
  
    setDT(headers)
    
    headers[template_header_encounter_index > 0,
            template_header:=paste0(template_header," [",(template_header_encounter_index),"]")]
    
    headers[,
            module_id:=paste0("headerModule",header_id)]
    
    setorder(headers,
             template_header,
             template_header_sheet_name,
             template_header_encounter_index)
    return(headers)
  }
}, ignoreNULL = FALSE, ignoreInit = TRUE)

observeEvent(SERVER_SETUP_TEMPLATES__SELECTED_TEMPLATE(), {

  active_headers <- SERVER_SETUP_TEMPLATES__ACTIVE_HEADERS()
  if (length(active_headers) > 0) {
    for (h in active_headers) {
      id <- paste0("headerModule",h,"-ui")
      removeUI(session=session,
               selector=paste0("#",ns(id)))
    }
  }
  SERVER_SETUP_TEMPLATES__ACTIVE_HEADERS(c())
  Shiny.destroyList(SERVER_SETUP_TEMPLATES__ACTIVE_HEADER_MODULES)
  
},ignoreInit=TRUE,ignoreNULL=FALSE)


observeEvent(input$server_setup_templates__header_edit, {
  edit_header_id <- as.numeric(input$server_setup_templates__header_edit)
  headers <- SERVER_SETUP_TEMPLATES__HEADER_ACTIONS()
  selected_template <- SERVER_SETUP_TEMPLATES__SELECTED_TEMPLATE()
  
  if (empty(selected_template)) return (NULL)
  #if (!delete_header_id %in% headers$header_id) return (NULL)
  
  id <- paste0("headerModule",edit_header_id,"-ui")
  
  removeUI(session=session,
           selector=paste0("#",ns(id)))
  
  if (edit_header_id %in% headers$header_id) {
    header <- headers[header_id==edit_header_id]
    ui <- server_setup_template__header_module_ui(id=ns(header$module_id),
                                                  header=header,
                                                  template=SERVER_SETUP_TEMPLATES__SELECTED_TEMPLATE(),
                                                  rsf_indicators=RSF_INDICATORS(),
                                                  rsf_indicator_formulas=RSF_INDICATOR_FORMULAS(),
                                                  rsf_check_formulas=RSF_CHECK_FORMULAS(),
                                                  is_static=FALSE)
    
    SERVER_SETUP_TEMPLATES__ACTIVE_HEADERS(c(SERVER_SETUP_TEMPLATES__ACTIVE_HEADERS(),edit_header_id))
    
    server_setup_template__header_module_server(id=ns(paste0("headerModule",edit_header_id)),
                                                pool=DBPOOL,
                                                RSF_INDICATORS=RSF_INDICATORS,
                                                RSF_INDICATOR_FORMULAS=RSF_INDICATOR_FORMULAS,
                                                RSF_CHECK_FORMULAS=RSF_CHECK_FORMULAS,
                                                SERVER_SETUP_TEMPLATES__ACTIVE_HEADER_MODULES=SERVER_SETUP_TEMPLATES__ACTIVE_HEADER_MODULES)
      
    insertUI(session=session,
             selector="#ui_setup__templates_mapping_labels",
             where="afterEnd",
             ui=ui)
  }
  
},ignoreInit=TRUE)

observeEvent(input$server_setup_templates__add, {

  selected_template <- SERVER_SETUP_TEMPLATES__SELECTED_TEMPLATE()
  
  if (empty(selected_template)) return (NULL)

  header <- DBPOOL %>% dbGetQuery("
    insert into p_rsf.rsf_program_facility_template_headers(rsf_pfcbl_id,
                                                            template_id,
                                                            rsf_program_id,
                                                            rsf_facility_id,
                                                            template_header_sheet_name,
                                                            template_header,
                                                            template_header_encounter_index,
                                                            action,
                                                            remap_header,
                                                            comment)
    select 
      ids.rsf_pfcbl_id,
      $2::int as template_id,
      ids.rsf_program_id,
      ids.rsf_facility_id,
      '' as template_header_sheet_name,
      '' as template_header_indicator_name,
      0 as template_header_encounter_index,
      'default' as template_header_action,
      NULL as template_remap_header_indicator_name,
      NULL as comment
    from p_rsf.rsf_pfcbl_ids ids
    where ids.rsf_pfcbl_id = $1::int
    returning 
      rsf_pfcbl_id,
      template_id,
      action,
      template_header,
      remap_header,
      template_header_sheet_name,
      template_header_encounter_index,
      header_id,
      comment",
    params=list(selected_template$selected_rsf_pfcbl_id,
                selected_template$template_id))
  
  setDT(header)
  header[,module_id:=paste0("headerModule",header_id)]
  
  #ui_setup__templates_mapping_labels
  #SERVER_SETUP_TEMPLATES__LOAD_HEADERS(SERVER_SETUP_TEMPLATES__LOAD_HEADERS()+1)

  ui <- server_setup_template__header_module_ui(id=ns(header$module_id),
                                          header=header,
                                          template=SERVER_SETUP_TEMPLATES__SELECTED_TEMPLATE(),
                                          rsf_indicators=RSF_INDICATORS(),
                                          rsf_indicator_formulas=RSF_INDICATOR_FORMULAS(),
                                          rsf_check_formulas=RSF_CHECK_FORMULAS(),
                                          is_static=FALSE)
  
  SERVER_SETUP_TEMPLATES__ACTIVE_HEADERS(c(SERVER_SETUP_TEMPLATES__ACTIVE_HEADERS(),header$header_id))
  
  server_setup_template__header_module_server(id=ns(header$module_id),
                                              pool=DBPOOL,
                                              RSF_INDICATORS = RSF_INDICATORS,
                                              RSF_INDICATOR_FORMULAS=RSF_INDICATOR_FORMULAS,
                                              RSF_CHECK_FORMULAS=RSF_CHECK_FORMULAS,
                                              SERVER_SETUP_TEMPLATES__ACTIVE_HEADER_MODULES=SERVER_SETUP_TEMPLATES__ACTIVE_HEADER_MODULES)
  
  insertUI(session=session,
           selector="#ui_setup__templates_mapping_labels",
           where="afterEnd",
           ui=ui)
})

observeEvent(input$ui_setup__template_headers_upload, {
  
  upload <- input$ui_setup__template_headers_upload
  if (!isTruthy(upload)) return (NULL)
  
  filename <- upload$name
  datapath <- upload$datapath
  
  if (!file_ext(filename) %in% c("xlsx")) {
    return (showNotification(h1("Error: file must be Excel format .xlsx only (not .xls, .xlsxm or .xlsxb)"),
                             closeButton = TRUE,
                             duration=8,
                             type="error"))
  }
  
  exceldata <- openxlsx::read.xlsx(xlsxFile=datapath,
                                 sheet=1)
  
  names(exceldata) <- tolower(names(exceldata))
  
  if (!setequal(names(exceldata),
                c("rsf_pfcbl_id",
                  "template_id",
                  "sysname",
                  "template_name",
                  "headerid",
                  "template_header_sheet_name",
                  "template_header","action","comment","map_indicator_id","indicator_name","map_formula_id","calculation_formula","map_check_formula_id","check_formula"))) {
    return (showNotification(h1("Error: file column names expected: ",
                                "rsf_pfcbl_id,template_id,SYSNAME,template_name,HEADERID,template_header_sheet_name,template_header,action,comment,map_indicator_id,indicator_name,map_formula_id,calculation_formula,map_check_formula_id,check_formula"),
                             closeButton = TRUE,
                             duration=8,
                             type="error"))
  }
  
  #conn <- poolCheckout(DBPOOL)
  #dbBegin(conn)
  #dbRollback(conn)
  poolWithTransaction(pool=DBPOOL,function(conn) { 
    
    dbExecute(conn,"
      create  table _temp_headers(
        rsf_pfcbl_id int,
        template_id int,
        sysname text,
        template_name text,
        headerid int,
        template_header_sheet_name text,
        template_header text,
        action text,
        comment text,
        map_indicator_id int,
        indicator_name text,
        map_formula_id int,
        calculation_formula text,
        map_check_formula_id int,
        check_formula text) on commit drop
    ")
    
    dbAppendTable(conn,
                  name="_temp_headers",
                  value=exceldata)
    
    nx <- dbExecute(conn,"
      delete from _temp_headers th
      where not exists(select * from p_rsf.reporting_templates rt where rt.template_id = th.template_id and rt.template_Name = th.template_name)
    ")
    
    nx <- dbExecute(conn,"
      update _temp_headers th
      set rsf_pfcbl_id = nids.rsf_pfcbl_id
      from p_rsf.rsf_data_current_names_and_ids nids
      where nids.sys_name = th.sysname
        and nids.rsf_pfcbl_id is distinct from th.rsf_pfcbl_id")
    nx <- dbExecute(conn,"
      delete from _temp_headers th
      where not exists(select * from p_rsf.rsf_pfcbl_ids ids where ids.rsf_pfcbl_id = th.rsf_pfcbl_id)
    ")
    
    nx <- dbExecute(conn,"
      delete from _temp_headers th
      where th.map_indicator_id is not null 
      and not exists(select * from p_rsf.indicators ind where ind.indicator_id = th.map_indicator_id)
    ")
    
    nx <- dbExecute(conn,"
      delete from _temp_headers th
      where th.map_formula_id is not null 
      and not exists(select * from p_rsf.indicator_formulas indf where indf.formula_id = th.map_formula_id)
    ")
    
    nx <- dbExecute(conn,"
      delete from _temp_headers th
      where th.map_check_formula_id is not null 
      and not exists(select * from p_rsf.indicator_check_formulas icf where icf.check_formula_id = th.map_check_formula_id)
    ")
    
   nx <- dbExecute(conn,"
    delete from _temp_headers th
    where ((th.map_indicator_id is not null)::int + (th.map_formula_id is not null)::int  + (th.map_check_formula_id is not null)::int) > 1   
   ")  
   
   nx <- dbExecute(conn,"
     delete from _temp_headers th
     where th.action is null 
        or th.action not in ('default'::text,
                             'ignore'::text,
                             'remap'::text,
                             'unmap'::text,
                             'check'::text,
                             'calculate'::text,
                             'parse'::text)")
   
   dbExecute(conn,"
    with headers as (                                
      select 
        th.rsf_pfcbl_id,
        th.template_id,
        ids.rsf_program_id,
        ids.rsf_facility_id,
        th.headerid as header_id,
        th.template_header_sheet_name,
        th.template_header,
        th.action,
        th.comment,
        th.map_indicator_id,
        th.map_formula_id,
        th.map_check_formula_id
      from _temp_headers th
      inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = th.rsf_pfcbl_id
      
      except
      
      select  
        fth.rsf_pfcbl_id,
        fth.template_id,
        fth.rsf_program_id,
        fth.rsf_facility_id,
        fth.header_id,
        fth.template_header_sheet_name,
        fth.template_header,
        fth.action,
        fth.comment,
        fth.map_indicator_id,
        fth.map_formula_id,
        fth.map_check_formula_id
      from p_rsf.rsf_program_facility_template_headers fth
    )
    insert into p_rsf.rsf_program_facility_template_headers(rsf_pfcbl_id,
                                                            template_id,
                                                            rsf_program_id,
                                                            rsf_facility_id,
                                                            header_id,
                                                            template_header_sheet_name,
                                                            template_header,
                                                            action,
                                                            comment,
                                                            map_indicator_id,
                                                            map_formula_id,
                                                            map_check_formula_id)
      select
        hea.rsf_pfcbl_id,
        hea.template_id,
        hea.rsf_program_id,
        hea.rsf_facility_id,
        hea.header_id,
        hea.template_header_sheet_name,
        hea.template_header,
        hea.action,
        hea.comment,
        hea.map_indicator_id,
        hea.map_formula_id,
        hea.map_check_formula_id
      from headers hea
      on conflict(header_id)
      do update
      set rsf_pfcbl_id = EXCLUDED.rsf_pfcbl_id,
          rsf_program_id = EXCLUDED.rsf_program_id,
          rsf_facility_id = EXCLUDED.rsf_facility_id,
          template_header_sheet_name = EXCLUDED.template_header_sheet_name,
          template_header = EXCLUDED.template_header,
          action = EXCLUDED.action,
          comment = EXCLUDED.comment,
          map_indicator_id = EXCLUDED.map_indicator_id,
          map_formula_id = EXCLUDED.map_formula_id,
          map_check_formula_id = EXCLUDED.map_check_formula_id         
  ")
   
  })
})
  
  
output$ui_setup__templates_add_mapping_UI <- renderUI({
  selected_template <- SERVER_SETUP_TEMPLATES__SELECTED_TEMPLATE()
  
  ui <- NULL
  if (empty(selected_template)) {
    ui <- div(p("Please select a template to add actions for specific template headers"))
  } else {
    ui <- div(
      actionButton(inputId="server_setup_templates__add",
                   label="Add Header Action",
                   class="btn-primary",
                   icon=icon("plus")))
  }
  return (ui)
})

output$ui_setup__templates_mapping_UI <- renderUI({
  
  headers <- SERVER_SETUP_TEMPLATES__HEADER_ACTIONS()
  req(SERVER_SETUP_TEMPLATES__SELECTED_TEMPLATE())
  
  
  ui <- NULL
  if (!empty(headers)) {
    
    headers_ui <- tagList()
    
    for (h in 1:nrow(headers)) {
      header <- headers[h]
      headers_ui[[length(headers_ui)+1]] <- server_setup_template__header_module_ui(id=ns(header$module_id),
                                                                                    header=header,
                                                                                    template=SERVER_SETUP_TEMPLATES__SELECTED_TEMPLATE(),
                                                                                    rsf_indicators=RSF_INDICATORS(),
                                                                                    is_static=TRUE)
    }
    ui <- headers_ui
    
    # ui <- div(
    #  
    #   fluidRow(
    #     column(12,
    #            div(headers_ui))),
      
      # fluidRow(style="padding-top:15px;",
      #   column(12,
      #          align="right",
      #          
      #          actionButton(inputId="server_setup_templates__save",
      #                       label="Save Header Actions",
      #                       class="btn-success",
      #                       icon=icon("save")))))
  
  
  } else {
    ui <- 
      fluidRow(
        column(12,
               div(id="template_header_container_nothing_message",
                   p("Template has no custom header actions.  Click 'Add Header Action' to create new entry"))))
  }
  
  return (ui)
})

output$ui_setup__template_headers_download <- downloadHandler(
  filename = function() {
    paste0(SERVER_SETUP_TEMPLATES__SELECTED_TEMPLATE()$selected_rsf_pfcbl_name," ",
           SERVER_SETUP_TEMPLATES__SELECTED_TEMPLATE()$template_name," HEADERS.xlsx")
  },
  content = function(filename) {
    withProgress(message="Downloading file",value=0.5, {
      
      tdata <- SERVER_SETUP_TEMPLATES__SELECTED_TEMPLATE_DATA()

      excelwb <- openxlsx::createWorkbook()
      addWorksheet(excelwb,
                   sheetName="Template Headers")
      
      writeDataTable(excelwb,sheet=1,x=tdata)

      openxlsx::saveWorkbook(excelwb,filename)
    })
  },
  contentType = "application/xlsx")