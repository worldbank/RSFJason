
module_session_system_options_key_labels <- function(id,label_id,label_key,slabel_count,SELECTED_SYSTEM_OPTION_DATA) {
  
  
  moduleServer(id,
   function(input,output, session) {
     
     #print(paste0("new label server ",session$ns("")," id=",id))
     this.label_key <- reactiveVal(label_key)
     this.secondary_label_count <- reactiveVal(slabel_count)
     
     o1 <- observeEvent(input$label_secondary_add, {
       #print("input$label_secondary_add")
       
       option_data_cols <- c("options_group_id","options_group_key","label_id","label_key","primary_label")
       option_data <- SELECTED_SYSTEM_OPTION_DATA()
       option_data <- option_data[,c(option_data_cols,"secondary_labels","secondary_label_rank")]
       
       new_rank <- max(option_data$secondary_label_rank)+1
       new_sl_data <- unique(option_data[option_data$label_id==label_id & option_data$label_key==input$label_key,
                                         option_data_cols])
       
       new_sl_data$secondary_labels <- NA
       new_sl_data$secondary_label_rank <- new_rank
       new_sl_data <- new_sl_data[,names(option_data)]
       
       new_sl_ui <- fluidRow(column(12,style="padding-left:0px;width:315px;",class="options-groups-labels",
                                    textInput(inputId=session$ns(paste0("secondary_label",new_sl_data$secondary_label_rank)),
                                              label=NULL,
                                              width="100%",
                                              value="",
                                              placeholder = "No alternative defined")))
       
       location <- paste0("#",session$ns("location_secondary_labels"))
       #print(paste0("Adding to: ",location))
       
       insertUI(selector=location,where="beforeEnd",ui=new_sl_ui)
       option_data <- rbind(option_data,new_sl_data)
       SELECTED_SYSTEM_OPTION_DATA(option_data)
       this.secondary_label_count(this.secondary_label_count()+1)
       
     }, ignoreInit=TRUE)
     
     #o2 <- observeEvent(sapply(1:this.secondary_label_count(),function(x) input[[paste0("secondary_label",x)]]),{ 
     o2 <- observeEvent(sapply(grep("^secondary_label",names(input),value=T),function(x) input[[x]]), {
       

       option_data <- SELECTED_SYSTEM_OPTION_DATA()      
       ranks <- option_data$secondary_label_rank[option_data$label_id==label_id & option_data$label_key==this.label_key()]
       
       if (length(ranks) != this.secondary_label_count()) {
         return(showNotification(type="error",h2("Please save changes before making this change")))
         #stop("Label ranks don't match label count") #This shouldn't happen?
       }
       
       labs <- sapply(grep("^secondary_label",names(input),value=T),function(x) input[[x]])
       
       for(i in 1:length(labs)) {
         #r <- ranks[i]
         slabel <- labs[i]
         
         if (!isTruthy(slabel)) next;
         
         rank <- option_data$secondary_label_rank[option_data$label_id==label_id & option_data$label_key==this.label_key()][i]
         current_val <- option_data$secondary_labels[option_data$label_id==label_id & option_data$label_key==this.label_key() & option_data$secondary_label_rank==rank]
         if (isTruthy(current_val) && current_val==slabel) next
         else option_data$secondary_labels[option_data$label_id==label_id & option_data$label_key==this.label_key() & option_data$secondary_label_rank==rank] <- slabel
       }
       SELECTED_SYSTEM_OPTION_DATA(option_data)
     }, ignoreInit=TRUE)
     
     o3 <- observeEvent(input$primary_label, {
       #print("input$primary_label")
       new_pl <- trimws(input$primary_label,whitespace="[ \\t\\r\\n\\v\\h\\s]")
       if (!isTruthy(new_pl)) new_pl <- NA
       
       ssod <- SELECTED_SYSTEM_OPTION_DATA()
       ssod$primary_label[ssod$label_id==label_id & ssod$label_key==this.label_key()] <- new_pl
       
       SELECTED_SYSTEM_OPTION_DATA(ssod)
       
     }, ignoreInit=TRUE)
     
     o4 <- observeEvent(input$label_key, {
       #print("input$label_key")
       all_labels <-  SYSTEM_ALL_LABEL_KEYS()
       if (!isTruthy(this.label_key()) || !isTruthy(input$label_key)) return (NULL)
       if (input$label_key == this.label_key()) return (NULL) #No change
       if (!(input$label_key %in% all_labels$label_key)) return (NULL) #Unallowed entry
       
       ssod <- SELECTED_SYSTEM_OPTION_DATA()
       exists <-  ssod$label_key[ssod$label_id==label_id & ssod$label_key==input$label_key]
       
       if (length(exists) > 0) {
         showNotification(h2("Selection for ",all_labels$label_key_name[all_labels$label_key==input$label_key]," already exists.  Change disallowed."),type="error")
         updateSelectizeInput(session=session,inputId="label_key",selected=this.label_key())
         return (NULL)
         
       } else if (this.label_key()=="EN") {
         showNotification(h2("Change disallowed. English label is required and cannot be changed. Select blue plus button to add a new language label."),type="error")
         updateSelectizeInput(session=session,inputId="label_key",selected=this.label_key())
         return (NULL)
       }
       
       ssod$label_key[ssod$label_id==label_id & ssod$label_key==this.label_key()] <- input$label_key
       SELECTED_SYSTEM_OPTION_DATA(ssod)
       this.label_key(input$label_key)
     }, ignoreInit = TRUE)
     
     modules <- SYSTEM_OPTIONS_MODULES()
     modules[[length(modules)+1]] <- o1
     modules[[length(modules)+1]] <- o2
     modules[[length(modules)+1]] <- o3
     modules[[length(modules)+1]] <- o4
     SYSTEM_OPTIONS_MODULES(modules)
   })
}


module_session_system_options_keys <- function(id, label_id, label_keys, SELECTED_SYSTEM_OPTION_DATA) {
  
  moduleServer(id,
               function(input,output,session) {
                 
                 #print(paste0("New module_session_system_options_keys: ",session$ns("!")))
                 for (i in 1:length(label_keys)) {
                   id <- paste0("LKEY",i)
                   #print(paste0("module_session_system_options_keys - labels for id=",id))
                   ssod <- SELECTED_SYSTEM_OPTION_DATA()
                   slabels <- ssod$secondary_labels[ssod$label_id==label_id & ssod$label_key==label_keys[i]]
                   #print(paste0("initializing secondary labels: ",slabels))
                   slabel_count <- length(slabels)
                   module_session_system_options_key_labels(id=id,label_id=label_id,label_key=label_keys[i],slabel_count=slabel_count,SELECTED_SYSTEM_OPTION_DATA)
                   
                 }
                 
                 o1 <- observeEvent(input$options_key,{
                   
                   #print(paste0("options_key=",input$options_key,"; label_id=",label_id))
                   data <- SELECTED_SYSTEM_OPTION_DATA()
                   data$options_group_key[data$label_id==label_id] <- input$options_key
                   SELECTED_SYSTEM_OPTION_DATA(data)
                   
                 }, ignoreInit=TRUE)
                 
                 o2 <- observeEvent(input$label_key_add, {
                   
                   keys_dt <- SYSTEM_ALL_LABEL_KEYS()
                   keys_dt[,key_type:=toupper(key_type)]
                   all_keys <- keys_dt$label_key
                   
                   #This is to give selectizeMenu options groupsings by key_type category
                   keys_list_select_options <- split(keys_dt,by=c("key_type"))
                   keys_list_select_options <- lapply(keys_list_select_options,function(x) {
                     options <- setNames(x$label_key,x$label_key_name)
                     options
                   })
                   
#                   print(paste0("Label key add: ",input$label_key_add, " in ns=",session$ns("")," "))
                   
                   option_data <- SELECTED_SYSTEM_OPTION_DATA()
                   
                   this.option_data <- option_data[option_data$label_id==label_id,]
                   
                   new_label_key <- all_keys[-which(all_keys %in% unique(this.option_data$label_key))][1]
                   new_label_key_id <- paste0("LKEY",(length(unique(this.option_data$label_key))+1))
                   
                   if (!isTruthy(new_label_key)) return (showNotification(type="error",h2("All label options have already been selected. No more may be added."))) #User has added all possible keys already...unexpected
                   
                   options_group_id <- unique(this.option_data$options_group_id)
                   
                   new_option_data <- unique(this.option_data[,c("options_group_id","options_group_key","label_id")])
                   
                   first_label_key_default <- keys_dt[label_key==new_label_key][1]
                   
                   new_option_data$label_key <- first_label_key_default$label_key
                   new_option_data$label_key_name <- first_label_key_default$label_key_name
                   new_option_data$key_type <- first_label_key_default$key_type
                   
                   new_option_data$primary_label <- NA
                   new_option_data$secondary_labels <- NA
                   new_option_data$secondary_label_rank <- 1
                   
                   new_option_data <- new_option_data[,names(option_data)]
                   option_data <- rbind(option_data,new_option_data)
                   
                   SELECTED_SYSTEM_OPTION_DATA(option_data)
                   
                   location <- paste0("#",session$ns("section"))
                   
                   new_ui <- module_ui_system_options_key_labels(id=session$ns(new_label_key_id),
                                                                 options_key_labels=new_option_data,
                                                                 system_label_keys=keys_list_select_options,
                                                                 useLabels=FALSE)
                   
                   insertUI(selector=location,where="beforeEnd",ui=new_ui)
                   
                   module_session_system_options_key_labels(id=new_label_key_id,
                                                            label_id=new_option_data$label_id,
                                                            label_key=new_label_key,
                                                            slabel_count=1,
                                                            SELECTED_SYSTEM_OPTION_DATA)
                   
                 }, ignoreInit=TRUE)
                 
                 modules <- SYSTEM_OPTIONS_MODULES()
                 modules[[length(modules)+1]] <- o1
                 modules[[length(modules)+1]] <- o2
                 SYSTEM_OPTIONS_MODULES(modules)
               })
}

module_ui_system_options_key_labels <- function(id,options_key_labels,system_label_keys,useLabels=FALSE) {
  ns <- NS(id)
  
  #print(paste0("Drawing labels: ",ns("")))
  #print(options_key_labels)
  
  system_label_keys <- SYSTEM_ALL_LABEL_KEYS_LIST()
  
  lkey_label <- "For"
  if (!useLabels) lkey_label <- NULL
  
  plabel_label <- "Primary Label"
  if (!useLabels) plabel_label <- NULL
  
  slabel_label <- "Alternative Labels"
  if (!useLabels) slabel_label <- NULL
  
  lkey <- unique(options_key_labels$label_key)
  if (length(lkey) != 1) stop("Multiple label keys exist")
  if (!all(isTruthy(lkey))) lkey <- "EN"
  
  plabel <- unique(options_key_labels$primary_label)
  if (length(plabel) != 1) stop("Multiple primary labels exist") #This shouldn't happen as it's coming from the database due to an unnest with secondary label array
  if (!isTruthy(plabel)) plabel <- ""
  
  slabel_ranks <- options_key_labels$secondary_label_rank
  
  lkey_ui <- column(2,class="options-groups-labels",selectizeInput(inputId=ns("label_key"),
                                                                   width="160px",
                                                                   label=lkey_label,
                                                                   choices=system_label_keys,
                                                                   selected=lkey))
  
  plabel_ui <-  column(4,class="options-groups-labels",style="padding-left:0px;width:315px;",textInput(inputId=ns("primary_label"),
                                                                                                       label=plabel_label,
                                                                                                       width="100%",
                                                                                                       value=plabel,
                                                                                                       placeholder="Required. Leave blank to delete on save."))
  
  slabels_list <- list()
  if (all(is.na(options_key_labels$secondary_labels))) {
    slabels_list[[length(slabels_list)+1]] <- fluidRow(column(12,style="padding-left:0px;width:315px;",class="options-groups-labels",
                                                              textInput(inputId=ns("secondary_label1"),label=slabel_label,width="100%",value="",placeholder = "No alternative defined")))
  } else {
    for (r in slabel_ranks) {
      slabel <- options_key_labels[options_key_labels$secondary_label_rank==r,]
      if (r > 1) slabel_label <- NULL
      slabels_list[[length(slabels_list)+1]] <- fluidRow(column(12,style="padding-left:0px;width:315px;",class="options-groups-labels",
                                                                textInput(inputId=ns(paste0("secondary_label",r)),label=slabel_label,width="100%",value=slabel$secondary_labels,placeholder = "No alternative defined")))
    }
  }
  
  slabels_ui <- column(4,id=ns("location_secondary_labels"),slabels_list)
  slabel_add <- column(2,style=glue("width:20px; float:left; margin-left:25px; {ifelse(useLabels,'margin-top:31px;','margin-top:5px;')}"),
                       actionButton(ns("label_secondary_add"),label=NULL,title="Add Alternative Label",icon=icon("plus"),class="btn-primary btn-circle", style="font-size:10px; width:20px; height:20px; padding: 0px 5px; line-height:0px;"))
  
  spacer <- "padding-bottom:10px"
  if (!useLabels) spacer <- "padding-bottom:10px"
  
  ui <- fluidRow(style=spacer,lkey_ui,plabel_ui,slabels_ui,slabel_add)
  return(ui)
}


module_ui_system_options_key <- function(id,options_key,options_key_labels,keys_list) {
  ns <- NS(id)
  
  #print(paste0("Drawing options key ",ns("")))
  
  ui_list <- list()
  
  label_id <- options_key$label_id
  label_keys <- unique(options_key_labels$label_key)
  data_count <- unique(options_key_labels$data_count)
  
  if (is.na(data_count) || length(data_count)==0) data_count <- 0
  editable <- data_count == 0
  
  okey_ui <- column(2,style='width:110px;padding:0 0px 0 15px;display:inline-block;',
                    enabled(state=editable,
                            textInput(inputId=ns("options_key"),label="Choice Value",width="100%",value=toupper(options_key$options_group_key),placeholder="{BLANK}")))
  
  lkey_ui_add <- column(1,style="width:40px;margin-left:5px;margin-right:3px;",
                        actionButton(inputId=ns("label_key_add"),label=NULL,icon=icon("plus"),class="btn btn-primary btn-sm",style="margin-top:27px;"))
  
  labels_list <- list()
  for (i in 1:length(label_keys)) {
    id <- paste0("LKEY",i)
    
    lkey <- options_key_labels[options_key_labels$label_key==label_keys[i],]
    labels_ui <- module_ui_system_options_key_labels(id=ns(id),
                                                     options_key_labels=lkey,
                                                     system_label_keys = keys_list,
                                                     useLabels=(i==1))
    labels_list[[length(labels_list)+1]] <- labels_ui
  }
  
  ui_list[[length(ui_list)+1]] <- fluidRow(okey_ui,lkey_ui_add,column(10,id=ns("section"),labels_list))
  
  ui <- tagList(ui_list)
  
  return (ui)
}
