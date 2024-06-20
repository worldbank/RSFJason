REFRESH_SYSTEM_OPTIONS <- reactiveVal(0)

SYSTEM_OPTIONS_MODULES <- reactiveVal(list())
SELECTED_SYSTEM_OPTION_DATA <- reactiveVal(NA)

SYSTEM_ALL_OPTIONS <- reactive({
  if (!LOGGEDIN()) return (NULL)
  REFRESH_SYSTEM_OPTIONS()
  OPTION_CREATE_NEW()
  Shiny.destroyList(SYSTEM_OPTIONS_MODULES)
  options <- DBPOOL %>% db_rsf_get_options_groups()
  setDT(options)
  print("SYSTEM_ALL_OPTIONS <- reactive({")
  
  return (options)

})


OPTION_CREATE_NEW <- eventReactive(input$admin_system_create_option, { 

  if (!LOGGEDIN()) return (NA)
  if (!isTruthy(input$admin_system_create_option)) return (NA)
  
  new_option <- DBPOOL %>% db_rsf_options_create()
  if (all(is.na(new_option))) return (NA)
  
  return (new_option$options_group_id)
},ignoreNULL = FALSE, ignoreInit = TRUE)

SYSTEM_ALL_LABEL_KEYS <- eventReactive(SYSTEM_ALL_OPTIONS(), {
  
  if (!LOGGEDIN()) return (NULL)
  
  keys <- DBPOOL %>% dbGetQuery("select label_key,label_key_name,key_type from p_rsf.label_keys
                                order by key_type='language' desc, key_type='client' desc, key_type = 'ifc' desc")
  
  setDT(keys)
  if (!isTruthy(keys)) stop("System label keys not found.")
  return (keys)
}, ignoreNULL = FALSE)

SYSTEM_ALL_LABEL_KEYS_LIST <- eventReactive(SYSTEM_ALL_LABEL_KEYS(), {

  system_label_keys <- SYSTEM_ALL_LABEL_KEYS()

  if (!isTruthy(system_label_keys)) return (NULL)

  system_label_keys$key_type <- toupper(system_label_keys$key_type)
  system_label_keys <- split(system_label_keys,by="key_type")
  system_label_keys <- lapply(system_label_keys,function(x) {
    options <- setNames(x$label_key,x$label_key_name)
    options
  })

  return (system_label_keys)
}, ignoreNULL = FALSE)

SELECTED_SYSTEM_OPTION <- eventReactive(c(SYSTEM_ALL_OPTIONS(),
                                          input$admin_system_selected_option), {
  
  selected_id <- as.numeric(input$admin_system_selected_option)
  
  if (!LOGGEDIN()) return (NULL)
  if (!isTruthy(selected_id)) return (NULL)
  
  if (SYS_PRINT_TIMING) debugtime("eventReactive: SELECTED_SYSTEM_OPTION")
  
  options <- SYSTEM_ALL_OPTIONS()
  if (!isTruthy(options)) return (NULL)

  options_group <- options[options_group_id==selected_id]
  if (empty(options_group)) return (NULL)
  
  if (nrow(options_group) != 1) stop("Options group has more than 1 row with current selected_id")
  
  return (options_group)  
}, ignoreNULL=FALSE)


observeEvent(OPTION_CREATE_NEW(), {
  
  new_option_id <- OPTION_CREATE_NEW()
  if (!isTruthy(new_option_id)) return (NULL)
  all_options <- SYSTEM_ALL_OPTIONS()

  choices <- setNames(all_options$options_group_id,all_options$options_group_name)
  updateSelectizeInput(session=session,inputId="admin_system_selected_option",choices=choices,selected=new_option_id)
  
}, ignoreInit = FALSE, priority = 90)

observeEvent(SYSTEM_ALL_OPTIONS(), {
  
  all_options <- SYSTEM_ALL_OPTIONS()
  if (!isTruthy(all_options)) return (NULL)
  if (SYS_PRINT_TIMING) debugtime("observeEvent: SYSTEM_ALL_OPTIONS")
  
  options_group <- SELECTED_SYSTEM_OPTION()
  
  selected_id <- NULL
  if (!isTruthy(options_group)) selected_id <- numeric(0)
  else selected_id <- options_group$options_group_id
  
  choices <- setNames(all_options$options_group_id,all_options$options_group_name)
  
  updateSelectizeInput(session=session,inputId="admin_system_selected_option",choices=choices,selected=selected_id)
  
}, ignoreInit = FALSE, priority = 100)

observeEvent(SELECTED_SYSTEM_OPTION(), {
  sso <- SELECTED_SYSTEM_OPTION()
  
  if (empty(sso)) return (NULL)
  
  if (SYS_PRINT_TIMING) debugtime("observeEvent: SELECTED_SYSTEM_OPTION")
  
    
    options_data <- as.data.frame(sso$labels[[1]])

    # print("options data")
    # print(options_data)
    # print(class(options_data))

    SELECTED_SYSTEM_OPTION_DATA(options_data)    
    label_ids <- unique(options_data$label_id)
    for (lID in label_ids) {
      
      current_label <- options_data[options_data$label_id==lID,]
      current_label_keys <- unique(current_label$label_key)
      current_options_group_id <- unique(current_label$options_group_id)
      
      id <- ns(paste0("OPT",current_options_group_id,"_LAB",lID)) #ns from server.R adds "RSF-"
      print(paste0("Initializing options key Server ",id))
      module_session_system_options_keys(id=id,label_id=lID,label_keys=current_label_keys,SELECTED_SYSTEM_OPTION_DATA)
    }
}) 

observeEvent(input$admin_system_edit_options_delete, {

  if (!LOGGEDIN()) return (NULL)
  
  option_data <- SELECTED_SYSTEM_OPTION_DATA()
  warning("Data count was removed from get query.  Deprecated.")
  if (!isTruthy(option_data$data_count) || option_data$data_count==0) {
    withProgress({
      incProgress(amount=0.5)
      DBPOOL %>% db_rsf_options_delete(options_group_id=option_data$options_group_id)
      REFRESH_SYSTEM_OPTIONS(REFRESH_SYSTEM_OPTIONS()+1)
    })
    print("Deleted")
    
  } else {
    showNotification(type="error","Delete failed. This option has active data and can only be removed after data is deleted.")
    return (NULL)
  }
})
  
observeEvent(input$admin_system_edit_options_save, {

  if (!LOGGEDIN()) return (NULL)
  print("SAVING OPTION DATA")

  new_options_name <- input$admin_system_edit_options_name
  new_definition <- input$admin_system_edit_options_definition
  new_data_type <- tolower(input$admin_system_edit_options_type)
  
  all_options <- SYSTEM_ALL_OPTIONS()

  option_data <- SELECTED_SYSTEM_OPTION_DATA()
  
  option_data <- as.data.table(option_data)
  option_data[is.na(primary_label),primary_label:=""]
  option_data[is.na(secondary_labels),secondary_labels:=""]
  option_data[is.na(options_group_key),options_group_key:=""]

  options_keys <- unique(option_data[,.(options_group_id,options_group_key,label_id)])
  options_keys <- options_keys[,.(n=.N,options_group_key=toupper(unique(options_group_key))),by=.(options_group_id,options_group_key)]
  
  if (!isTruthy(new_options_name) || nchar(new_options_name) < 5 || nchar(new_options_name) > 50) {
    showNotification(type="error",h2("Save Failed. Choice group name must be between 5-50 characters long."))
    return (NULL)
  } else if (any(all_options$options_group_name[all_options$options_group_id != unique(option_data$options_group_id)] == new_options_name)) {
    showNotification(type="error",h2("Save Failed. Name '",new_options_name,"' is already used.  Names must be unique."))
    return (NULL)
  }

  if (!isTruthy(new_data_type)) {
    showNotification(type="error",h2("Save Failed. Data type must be seleted: text, number, date, logical; this is the type of data in the choices value field."))
    return (NULL)
  } else if (!new_data_type %in% c("text","number","date","logical")) {
    showNotification(type="error",h2("Save Failed. Data type must be one of: text, number, date, logical; this is the type of data in the choices value field."))
    return (NULL)
  } else {
    all_keys <- options_keys[!is.na(options_group_key) & options_group_key != "",unique(options_group_key)]
    if (new_data_type == "text") {
      if (any(!is.na(suppressWarnings(as.numeric(all_keys))))) {
        showNotification(type="error",h2("Save Failed. Selected data type is text, but choice values has numbers."))
        return (NULL)
      } else if (any(!is.na(suppressWarnings(as.logical(all_keys))))) {
        showNotification(type="error",h2("Save Failed. Selected data type is text, but choice values has logical TRUE/FALSE values."))
        return (NULL)
      } else if (any(!is.na(suppressWarnings(ymd(all_keys))))) {
        showNotification(type="error",h2("Save Failed. Selected data type is text, but choice values has date values."))
        return (NULL)
      }
    } else if (new_data_type == "number" & any(is.na(suppressWarnings(as.numeric(all_keys))))) {
      showNotification(type="error",h2("Save Failed. Selected data type is number, but choice values has values that are not numbers."))
      return (NULL)
    } else if (new_data_type == "logical" & any(is.na(suppressWarnings(as.logical(all_keys))))) {
      showNotification(type="error",h2("Save Failed. Selected data type is logical; choice values may only be either TRUE, FALSE or {BLANK}"))
      return (NULL)
    } else if (new_data_type == "date" & any(is.na(suppressWarnings(ymd(all_keys))))) {
      showNotification(type="error",h2("Save Failed. Selected data type is date; all choice values must be dates in YYYY-MM-DD format."))
      return (NULL)
    }
  }
  
  option_keys_label <- unique(option_data[,.(options_group_id,label_id,options_group_key,label_key)])[,.(n=.N),by=.(options_group_id,label_id,options_group_key,label_key)]

  label_count <- option_data[,.(label=unique(c(primary_label,secondary_labels))),by=.(options_group_id,options_group_key,label_id)][,.(n=.N),by=.(label)][label != ""]
  label_blank <- option_data[primary_label=="" & secondary_labels != ""]
  deletes <- option_data[primary_label=="" & secondary_labels == ""]
  saves <- option_data[primary_label != ""]
  
  if (nrow(options_keys[n>1])>0) {
    showNotification(type="error",h2("Save Failed. Each choice must be unique. Repeats: ",paste0(options_keys[n>1,unique(ifelse(options_group_key=="","{BLANK}",options_group_key))],collapse=", ")))
    return (NULL)
  } else if (nrow(option_keys_label[n>1])>0) {
    showNotification(type="error",h2("Save Failed. Each choice labels must be 'for' only one type. Repeats: ",paste0(option_keys_label[n>1,unique(label_key)],collapse=", ")))
    return (NULL)
  } else if (nrow(label_count[n>1])>0) {
    showNotification(type="error",h2("Save Failed. Each label must be unique for each choice. Repeats: ",paste0(label_count[n>1,unique(ifelse(label=="","{BLANK}",label))],collapse=", ")))
    return (NULL)
  } else if (nrow(label_blank)>0) {
    showNotification(type="error",h2("Save Failed. Primary label cannot be blank if alternative label has a value.  Label will be deleted when both Primary and Alternatives are all blank"))
    return (NULL)
  } else if (any(grepl("[^A-Za-z0-9_\\.-]",unique(option_data$options_group_key)))) {
    showNotification(type="error",h2("Save Failed. Punctuation is not permitted in choice codes.  Issues: ",paste0(unique(grep("[^[:alnum:]_\\.-]",unique(option_data$options_group_key),value=T)))))
    return (NULL)
  } else if (any(nchar(unique(option_data$options_group_key)) > 8)) {
    showNotification(type="error",h2("Save Failed. Choices codes are limited to 8 characters.  Issues: ",paste0(unique(option_data$options_group_key)[nchar(unique(option_data$options_group_key))>8])))
    return (NULL)
  }

  tryCatch({
    saves$action <- "save"
    deletes$action <- "delete"
    upload_data <- rbind(saves,deletes)
    upload_data$options_group_name  <- new_options_name
    upload_data$options_group_definition <- new_definition
    upload_data$options_group_data_type <- new_data_type
    
    withProgress(message="Saving...",
    {
      incProgress(amount=0.5)
      DBPOOL %>% db_rsf_options_update(upload_data)
    })
    print("Saved")
    
    Shiny.destroyList(SYSTEM_OPTIONS_MODULES)
    REFRESH_SYSTEM_OPTIONS(REFRESH_SYSTEM_OPTIONS()+1)
  },
  error = function(err) {
    msg <- conditionMessage(err)
    showNotification(type="error",h2("Save failed. ",msg))
  },
  warning = function(war) {
    msg <- conditionMessage(war)
    showNotification(type="error",h2("Save failed. ",msg))
  })

}, ignoreInit=TRUE)


observeEvent(input$admin_system_edit_options_keys_add, {

  if (!LOGGEDIN()) return (NULL)
  
  ssod <- SELECTED_SYSTEM_OPTION_DATA()
  keys_list <- SYSTEM_ALL_LABEL_KEYS()
  
  new_label_id <- 0
  if (isTruthy(ssod)) new_label_id <- nrow(ssod)
  new_label_id <- (new_label_id + 1) * -1
  
  new_option_key <- data.frame(options_group_id=unique(ssod$options_group_id),
                               options_group_key=NA,
                               data_count=0,
                               label_id=new_label_id)
  
  new_option_data <- data.frame(options_group_id=unique(ssod$options_group_id),
                                options_group_key=NA,
                                #data_count=0,
                                label_id=new_label_id,
                                label_key="EN",
                                label_key_name="English",
                                key_type="Language",
                                primary_label=NA,
                                secondary_labels=NA,
                                secondary_label_rank=1)
  
  new_id <- ns(paste0("OPT",new_option_data$options_group_id,"_LAB",new_option_data$label_id))

  new_key_ui <- module_ui_system_options_key(id=new_id,
                                             options_key=new_option_key,
                                             options_key_labels=new_option_data,
                                             keys_list=keys_list)

  location <- paste0("#location_system_edit_options_keys")
  insertUI(selector=location,where="afterBegin",ui=new_key_ui)

  if (!isTruthy(ssod) || all(is.na(ssod))) ssod <- new_option_data
  else ssod <- rbind(ssod,new_option_data)
  
  SELECTED_SYSTEM_OPTION_DATA(ssod)
  module_session_system_options_keys(id=new_id,label_id=new_option_data$label_id,label_keys=new_option_data$label_key,SELECTED_SYSTEM_OPTION_DATA)
  
}, ignoreInit = TRUE)

output$admin_system_display_options_html_name <- renderUI({
  
  if (!LOGGEDIN()) return ("")
  
  option <- SELECTED_SYSTEM_OPTION()
  
  if (!isTruthy(option)) return("")
  
  html <- format_html_option(option$options_group_name)
  return (HTML(html))
})

output$admin_system_edit_options <- renderUI({

  if (!LOGGEDIN()) return ("")
  
  print("RENDERING admin_system_edit_options")
  
  option <- SELECTED_SYSTEM_OPTION()
  
  if (!isTruthy(option)) return (h2("Please select an options group to edit"))

  #print(option)
  #print(option$labels)
  option_data <- rbindlist(option$labels)
  #option_data[is.na(label_id),`:=`(label_id=-1 * 1:.N, label_key="EN", secondary_label_rank=1)] #for newly created groups, database will left-join NAs.
  
  keys_list <- SYSTEM_ALL_LABEL_KEYS_LIST()
  
  
  options_name <- option$options_group_name
  
  options_name_label <- paste0("Options name may use only: upper and lower-case letters and numbers; and underscore.")
  secondary_labels <- paste0("One label per field.",
                             "Enter alternate phrases, (mis)spellings or aliases associated with the primary label.",
                             "These include permutations found in template files, including misspellings and typos, which the system will recognize and associate with this indicator.",
                             "Deleting an entry could prevent the system from reading a template correctly. Modify with care.")
  
  definition <- option$definition

  ui_options_keys <- tagList()

  data_count <- sum(option$data_count,na.rm = T)
  if (length(data_count)==0) data_count <- 0
  
  option_keys <- unique(option_data[,.(options_group_id,options_group_key,label_id,data_count)])
  label_ids <- option_keys$label_id

  if (!is.null(label_ids) && length(label_ids) > 0 && !all(is.na(label_ids))) {

    for (lID in label_ids) {      
      if (is.na(lID)) next;
      id <- ns(paste0("OPT",option$options_group_id,"_LAB",lID))
      current_group_key <- option_keys[option_keys$label_id==lID,]
      current_key_labels <- option_data[option_data$label_id==lID,.(label_id,options_group_id,data_count,label_key,primary_label,secondary_labels,secondary_label_rank)]

      ui <- module_ui_system_options_key(id=id,
                                         options_key=current_group_key,
                                         options_key_labels=current_key_labels,
                                         keys_list=keys_list)
      
      spacer <- NULL
      if (lID != label_ids[length(label_ids)]) spacer <- fluidRow(column(1,""),column(10,style="border-top:solid lightgray 1px;padding-bottom:10px;",""),column(1,""))
      ui_options_keys <- tagList(ui_options_keys,ui,spacer)
    }
  }
  
  option_name <- option$options_group_name
  option_type <- option$options_group_data_type

  option_name_label <- paste0("Options group name may use only: upper and lower-case letters and numbers; and underscore punctiation.")
  
  definition <- option$definition
  if (is.na(definition)) definition <- ""
  
  data_type_choices <- c(Text="text",Number="number",Date="date",Logical="logical")
  
  limited <- NULL
  editable <- data_count == 0
  if (data_count > 0) limited <- fluidRow(style="padding-bottom:5px;",
                                      column(12,div(style='padding_left:15px;',
                                          icon("exclamation-triangle",class="icon-orange"),
                                          paste0("Option has ",data_count," data points saved. Editing options that affect existing data are disabled."))))

  ui <- div(id="location_edit_options",style='width:955px;background-color:gainsboro;padding:10px;margin: 0 auto;',
            fluidRow(align="center",style="padding-top:5px;padding-bottom:5px;",column(12,uiOutput(outputId="admin_system_display_options_html_name"))),
            limited,
            fluidRow(column(9,textInput(inputId="admin_system_edit_options_name",
                                         width="100%",
                                         label = tags$label("Choices Group Name",style='margin:0px;',tags$i(class='fas fa-question icon-question',title=option_name_label)),
                                         value=option_name)),
                     column(3,enabled(state=editable,selectizeInput(inputId="admin_system_edit_options_type",
                                             width="100%",
                                             label="Values Data Type",
                                             choices=data_type_choices,
                                             selected=option_type)))
            ),
            fluidRow(align="left",
                     column(12,
                            textAreaInput(inputId="admin_system_edit_options_definition",rows="1",width="937px",label="Definition",value=definition))),
            fluidRow(align="left",style="padding-top:5px;padding-bottom:10px;",
                     column(12,style="padding-left:15px;",
                            div(actionButton(inputId="admin_system_edit_options_keys_add",label="Add Choice",icon=icon("puzzle-piece"),class="btn-primary btn-sm")),
                     )
            ),
            fluidRow(align="left",column(12,div(id="location_system_edit_options_keys",ui_options_keys))),
            fluidRow(style='padding-top:10px;',div(style='border-top:solid gray 1px;margin-left:10px;margin-right:10px;',
                     column(6,align="left",style="padding-top:5px;",
                                       enabled(state=editable,
                                               actionButton(inputId="admin_system_edit_options_delete",class="btn-danger",label="Delete",icon=icon("minus-circle")))
                     ),
                     column(6,align="right",style="padding-top:5px;",
                            actionButton(inputId="admin_system_edit_options_save",class="btn-success",label="Save",icon=icon("save")))))
  )
  
  return (ui)
})