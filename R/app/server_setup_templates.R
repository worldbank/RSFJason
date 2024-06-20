#Pseudo module.  Not enough is happening to merit a real module.
server_setup_template__header_module_ui <- function(id,
                                                    header,
                                                    template,
                                                    rsf_indicators,
                                                    is_label_header=FALSE) {
  ns <- NS(id)
  
  is_system <- template$is_system #Users can view system templates!
  
  template_sheet  <- div(style="display:flex;flex-flow:row nowrap;width:120px;padding:0 0 0 2px;white-space:nowrap;",
                         textInput(inputId=ns("template_header_sheet_name"),
                                   label=switch(is_label_header,
                                                "Sheet Name",
                                                NULL),
                                   value=header$template_header_sheet_name,
                                   width="100%",
                                   placeholder="All Sheets"))
  
  header_template <- div(style="display:flex;flex-flow:row nowrap;min-width:450px;padding:0 0 0 2px;flex-grow:1;white-space:nowrap;",
                         textInput(inputId=ns("template_header_indicator_name"),
                                   label=switch(is_label_header,
                                                "Template Column Title",
                                                NULL),
                                   value=header$template_header,
                                   width="100%",
                                   placeholder="Enter template column to be remapped"))
  
  header_action <- div(style="display:flex;flex-flow:row nowrap;width:120px;padding:0 0 0 2px;white-space:nowrap;",
                       selectizeInput(inputId=ns("template_header_action"),
                                      label=switch(is_label_header,
                                                   "Action",
                                                   NULL),
                                      width="100%",
                                      choices=c(`Default`="default",
                                                `Re-Map`="remap",
                                                `Ignore`="ignore"),
                                      selected=header$action))
  
  header_mapping <- div(style="display:flex;flex-flow:row nowrap;min-width:450px;padding:0 0 0 2px;white-space:nowrap;",
                        selectizeInput(inputId=ns("template_remap_header_indicator_name"),
                                       label=switch(is_label_header,
                                                    "Re-Map to Indicator",
                                                    NULL),
                                       width="100%",
                                       choices=c("",sort(rsf_indicators$indicator_name)),
                                       selected=header$remap_header,
                                       options=list(placeholder="Header not remapped")))
  #browser()
  header_remove  <- div(style=paste0("display:flex;flex-flow:row nowrap;width:40px;padding:0 0 0 2px;",
                                     ifelse(is_label_header,
                                            "padding-top:25px;",
                                            "")),
                        actionButton(inputId=ns("template_header_delete"),
                                     label=NULL,
                                     onclick=paste0("Shiny.setInputValue(`server_setup_templates__header_delete`,",header$header_id,",{priority:`event`})"),
                                     icon=icon("trash")))
  
  ui <- div(name="template_header_box",
            id=ns("ui"),
            div(style="display:flex;flex-flow:row nowrap;justify-content:left;align-items:start;",
                template_sheet,
                header_template,
                header_action,
                header_mapping,
                header_remove))
  
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

SERVER_SETUP_TEMPLATES__LOAD_HEADERS <- reactiveVal(0)

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

SERVER_SETUP_TEMPLATES__SELECTED_TEMPLATE <- eventReactive(c(input$ui_setup__templates_program_facilities,
                                                             input$ui_setup__template_selected), 
{
  
  selected_template_id <- as.numeric(input$ui_setup__template_selected)
  selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__templates_program_facilities)
  
  if (!isTruthy(selected_template_id)) return (NULL)
  if (!isTruthy(selected_rsf_pfcbl_id)) return (NULL)
  if (empty(RSF_TEMPLATES())) return (NULL)
  
  template <- (RSF_TEMPLATES()[template_id==selected_template_id])
  template[,
           selected_rsf_pfcbl_id:=selected_rsf_pfcbl_id]
  
  return (template)
})

SERVER_SETUP_TEMPLATES__HEADER_ACTIONS <- eventReactive(SERVER_SETUP_TEMPLATES__LOAD_HEADERS(), {
  
  
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
      fth.header_id
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
  
  #if (!empty(SERVER_SETUP_TEMPLATES__SELECTED_TEMPLATE())) SERVER_SETUP_TEMPLATES__LOAD_HEADERS(SERVER_SETUP_TEMPLATES__LOAD_HEADERS()+1)
  SERVER_SETUP_TEMPLATES__LOAD_HEADERS(SERVER_SETUP_TEMPLATES__LOAD_HEADERS()+1)
  
},ignoreInit=TRUE,ignoreNULL=FALSE)

observeEvent(input$server_setup_templates__header_delete,{
  delete_header_id <- as.numeric(input$server_setup_templates__header_delete)
  headers <- SERVER_SETUP_TEMPLATES__HEADER_ACTIONS()
  selected_template <- SERVER_SETUP_TEMPLATES__SELECTED_TEMPLATE()
  
  if (empty(selected_template)) return (NULL)
  if (!delete_header_id %in% headers$header_id) return (NULL)
  
  DBPOOL %>% dbExecute("
    delete from p_rsf.rsf_program_facility_template_headers fth
    where fth.header_id = $1::int
      and fth.rsf_pfcbl_id = $2::int
      and fth.template_id = $3::int",
    params=list(delete_header_id,
                selected_template$selected_rsf_pfcbl_id,
                selected_template$template_id))

  SERVER_SETUP_TEMPLATES__LOAD_HEADERS(SERVER_SETUP_TEMPLATES__LOAD_HEADERS()+1)
},ignoreInit=TRUE)

observeEvent(input$server_setup_templates__save, {

  headers <-  SERVER_SETUP_TEMPLATES__HEADER_ACTIONS()
  selected_template <- SERVER_SETUP_TEMPLATES__SELECTED_TEMPLATE()
  
  
  if (empty(selected_template)) return (NULL)
  if (empty(headers)) return (NULL)
  
  withProgress(value=0.5,
               message="Saving template header actions...",
               {
    headerInputs <- unique(grep("^RSF-headerModule",names(input),value=TRUE))
    headerValues <- lapply(headerInputs,function(j,x) { x[[j]] },x=input)
    names(headerValues) <- headerInputs
    
    nulls <- sapply(headerValues,is.null)
    if (any(nulls)) headerInputs <- headerInputs[!nulls]
    if (length(headerInputs)==0) return (NULL)
    
    save_headers <- data.table(input_name=names(headerValues),
                               input_value=trimws(headerValues))
    save_headers[,
                 `:=`(header_id=as.numeric(gsub("^RSF-headerModule(\\d+)-.*$","\\1",input_name)),
                      header_name=gsub("^RSF-headerModule\\d+-(.*)$","\\1",input_name))]
    save_headers <- dcast.data.table(save_headers,
                                     header_id ~ header_name,
                                     value.var="input_value")
    
    #Because this is a pseudo module, the inputs stick around to ensure the header_ids we're saving are associated only with the header ID's we've loaded.
    save_headers <- save_headers[header_id %in% headers$header_id]
    
    if (empty(save_headers)) {
      return(showNotification(type="error",
                              ui=h3("No valid header actions to be saved.")))
    }
    
    save_headers[,
                 template_header_encounter_index:=0]
    
    
    save_headers[grepl("\\[\\d+\\]$",template_header_indicator_name),
                 `:=`(template_header_encounter_index=as.numeric(gsub("\\[(\\d+)\\]$","\\1",template_header_indicator_name)),
                      template_header_indicator_name=trimws(gsub("\\[\\d+\\]$","",template_header_indicator_name)))]
    
    save_headers[,
                 normalized_header_indicator_name:=normalizeLabel(template_header_indicator_name)]
    
    setorder(save_headers,
             template_header_sheet_name,
             template_header_indicator_name,
             template_header_sheet_name,
             template_header_indicator_name,
             template_header_encounter_index,
             header_id)
    
    save_headers[,
                 template_header_encounter_index:=(1:.N)-ifelse(.N > 1,0,1),
                 by=.(template_header_sheet_name,
                      normalized_header_indicator_name)]
    
    if (any(nchar(save_headers$template_header_indicator_name)==0)) {
      return(showNotification(type="error",
                              ui=h3("Template header name cannot be missing or blank and should match a specific text found in this template")))
    }
    #conn <- poolCheckout(DBPOOL)
    #dbBegin(conn);
    #dbRollback(conn);poolReturn(conn)
    
    poolWithTransaction(DBPOOL,function(conn) {
      dbExecute(conn,"
        create temp table _temp_headers(header_id int,
                                        template_header_sheet_name text,
                                        template_header_indicator_name text,
                                        template_header_action text,
                                        template_header_encounter_index int,
                                        template_remap_header_indicator_name text)
        on commit drop;")
      
      dbAppendTable(conn,
                    name="_temp_headers",
                    value=save_headers[,.(header_id,
                                          template_header_sheet_name,
                                          template_header_indicator_name,
                                          template_header_action,
                                          template_header_encounter_index,
                                          template_remap_header_indicator_name)])

      dbExecute(conn,"
        update _temp_headers th
        set template_remap_header_indicator_name = NULL
        where th.template_header_action in ('default','ignore')")
      
      dbExecute(conn,"
        update _temp_headers th
        set template_remap_header_indicator_name = NULL,
            template_header_action = 'default'
        where th.template_header_action is not distinct from 'remap'
          and not exists(select * from p_rsf.indicators ind 
                         where ind.indicator_name = th.template_remap_header_indicator_name)")
      
      dbExecute(conn,"
        update p_rsf.rsf_program_facility_template_headers fth
        set template_header_sheet_name = th.template_header_sheet_name,
            template_header = th.template_header_indicator_name,
            template_header_encounter_index = th.template_header_encounter_index,
            action = th.template_header_action,
            remap_header = th.template_remap_header_indicator_name
        from _temp_headers th
        where th.header_id = fth.header_id
          and fth.rsf_pfcbl_id = $1::int
          and fth.template_id = $2::int",
        params=list(selected_template$selected_rsf_pfcbl_id,
                    selected_template$template_id))
    })
  })
  SERVER_SETUP_TEMPLATES__LOAD_HEADERS(SERVER_SETUP_TEMPLATES__LOAD_HEADERS()+1)
})

observeEvent(input$server_setup_templates__add, {

  selected_template <- SERVER_SETUP_TEMPLATES__SELECTED_TEMPLATE()
  
  if (empty(selected_template)) return (NULL)

  DBPOOL %>% dbExecute("
    insert into p_rsf.rsf_program_facility_template_headers(rsf_pfcbl_id,
                                                            template_id,
                                                            rsf_program_id,
                                                            rsf_facility_id,
                                                            template_header_sheet_name,
                                                            template_header,
                                                            template_header_encounter_index,
                                                            action,
                                                            remap_header)
    select 
      ids.rsf_pfcbl_id,
      $2::int as template_id,
      ids.rsf_program_id,
      ids.rsf_facility_id,
      '' as template_header_sheet_name,
      '' as template_header_indicator_name,
      0 as template_header_encounter_index,
      'default' as template_header_action,
      NULL as template_remap_header_indicator_name
    from p_rsf.rsf_pfcbl_ids ids
    where ids.rsf_pfcbl_id = $1::int",
    params=list(selected_template$selected_rsf_pfcbl_id,
                selected_template$template_id))
  SERVER_SETUP_TEMPLATES__LOAD_HEADERS(SERVER_SETUP_TEMPLATES__LOAD_HEADERS()+1)
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
                                                                                    is_label_header=(h==1))
    }
    
    ui <- div(
     
      fluidRow(
        column(12,
               div(headers_ui))),
      
      fluidRow(style="padding-top:15px;",
        column(12,
               align="right",
               
               actionButton(inputId="server_setup_templates__save",
                            label="Save Header Actions",
                            class="btn-success",
                            icon=icon("save")))))
  
  
  } else {
    ui <- 
      fluidRow(
        column(12,
               div(id="template_header_container_nothing_message",
                   p("Template has no custom header actions.  Click 'Add Header Action' to create new entry"))))
  }
  
  return (ui)
})