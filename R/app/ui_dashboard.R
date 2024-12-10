div(style="background-color:white;padding:10px;",
    tabsetPanel(id="tabset_dashboard",
                
#################
###TAB DASHBOARD
#################
tabPanel("Dashboard",value="dashboard",

  div(style="width:100%;padding:10px;background-color:white;",
    #div(align="left",style="width:100%;",
      div(align="left",
             style="width:100%;display:flex;flex-flow:row wrap",
             
             div(style="display:flex;flex-grow:1;flex-flow:row nowrap;margin-right:20px;max-width:650px;",
                    div(style="min-width:200px;flex:1;vertical-align:top;margin-right:5px;", #style="min-width:150px",
                        div(class="picker",
                            pickerInput(inputId="server_dashboard__reporting_client",
                                         label="IFC Clients",
                                         multiple=TRUE,
                                         width="100%",
                                         choices=list(),
                                         selected=NULL,
                                         options = list(`actions-box` = TRUE,title="Select clients...")))),
                 div(style="width:200px;vertical-align:top;margin-right:5px;", #style="min-width:150px",
                     pickerInput(inputId="server_dashboard__reporting_asof_date",
                                 label="Reporting Dates",
                                 multiple=TRUE,
                                 width="100%",
                                 choices=list(),
                                 selected=NULL,
                                 options = list(`actions-box` = TRUE,title="Select timeline...")))
              ),
             
             div(style="display:flex;flex-grow:2;flex-flow:row nowrap;justify-content:center;margin-right:10px;margin-left:10px;",
                 div(style="width:115px;vertical-align:top;margin-right:5px;",
                     selectizeInput(inputId = "server_dashboard__reporting_filter",
                                    label="Portfolio",
                                    multiple=FALSE,
                                    choices=c(`Everything`="",
                                              `Reporting`=TRUE,
                                              `Inactive`=FALSE),
                                    selected="",
                                    options = list(placeholder="No Filter"),
                                    width="100%")),
                 
                 div(class="picker",style="margin-right:5px;vertical-align:top;",
                   pickerInput(inputId = "server_dashboard__flags_filter",
                               label="Flags",
                               multiple=TRUE,
                               choices=list(`Any Flag`="any",
                                            Critical="critical",
                                            Errors="error",
                                            Warnings="warning",
                                            Info="info"),
                               selected=NULL,
                               options = list(title="Filter Flags..."))),
               
               
               div(style="min-width:200px;max-width:300px;flex-grow:2;vertical-align:top;margin-right:5px;",
                   pickerInput(inputId = "server_dashboard__name_filter",
                               label="Names",
                               multiple=TRUE,
                               choices=c(),
                               selected=NULL,
                               options = pickerOptions(title="Filter Names...",
                                                       actionsBox=TRUE,
                                                       liveSearch=TRUE,
                                                       size=15,
                                                       virtualScroll=50,
                                                       width="css-width"),
                             width="100%"))
             ),
             div(style="vertical-align:top;display:flex;flex-flow:row nowrap;flex-grow:3;justify-content:flex-end;margin-top:24px;",
                      div(id="panel_actions_viewing",
                          style="vertical-align:top;display:flex-flow:row nowrap;text-align:right;",
                          downloadButton(outputId="action_server_dashboard__download",
                                         label="Export",
                                         icon=icon("file-excel"),
                                         class="btn-success"),
                          
                          actionButton(inputId="action_server_dashboard_reports__save_as",
                                       label="Save",
                                       icon=icon("save"),
                                       class="btn-success"),
                          
                          actionButton(inputId="action_server_dashboard__edit",
                                        label="Edit",
                                        icon=icon("edit"),
                                        class="btn-primary")),
                     
                        hidden(div(id="panel_actions_editing",
                          style="vertical-align:top;display:flex-flow:row nowrap;text-align:right;",
                          actionButton(inputId="action_server_dashboard__edit_save",
                                       label="Save & Close",
                                       icon=icon("edit"),
                                       class="btn-success"),
                          actionButton(inputId="action_server_dashboard__edit_discard",
                                       label="Discard",
                                       icon=icon("cancel"),
                                       class="btn-danger")))
             )
      )           
    ),
  
    div(align="left",
             style="width:100%;",
             div(align="left",
                    style="width:100%;display:flex;flex-flow:row nowrap",
                    
        div(style="display:flex;flex-flow:row wrap;flex-grow:1;margin-right:5px;",
             
             div(style="min-width:100px;width:100%;margin-right:5px;",
               selectizeInput(inputId="server_dashboard__reporting_column_priority",
                              label=div("Column View & Sort Order ",
                                        actionLink(inputId="server_dashboard__reporting_column_priority_lookup",
                                                   label=NULL,
                                                   icon=icon("magnifying-glass-plus"),
                                                   style="padding-left:5px;")),
                              choices="",
                              width="100%",
                              multiple=TRUE,
                              selected="",
                              options=list(placeholder="Select data columns...",
                                           plugins=list("drag_drop"))))),
        
        div(style="display:flex;flex-flow:row nowrap;flex-shrink:1;",
            
            div(style="width:72px;margin-right:5px;margin-top:25px;",
                selectizeInput(inputId="server_dashboard__view_currency",
                               label=NULL,
                               width="100%",
                               multiple=FALSE,
                               choices=c(LCU="LCU",USD="USD",EUR="EUR"),
                               selected = "LCU")),
            div(style="width:78px;margin-right:5px;margin-top:25px;",
                selectizeInput(inputId="server_dashboard__format_pivot",
                               label=NULL,
                               width="100%",
                               multiple=FALSE,
                               choices=c("",DATA="DATA",NAME="NAME",DATE="DATE",NONE="NONE"),
                               selected = "",
                               options=list(placeholder="Pivot"))),
            
            div(style="vertical-align:top;padding-top:24px;",
                actionButton(inputId="server_dashboard__view_options",
                             label="",
                             icon=icon("screwdriver-wrench"),
                             class="btn-primary")),
            div(style="vertical-align:top;padding-top:24px;padding-left:20px;",
                switchInput(inputId="server_dashboard__autorun",
                            label="Run",
                            value=TRUE,
                            onLabel="YES",
                            onStatus= "success",
                            offLabel="NO",
                            offStatus = "danger",
                            inline=TRUE,
                            size="small"))
              
             
             
        )
      )
    ),
  #options: view flags, all, warning, alerts, etc
  #view history changes
  #editing mode?
  #filter bars?
  #indicator group headers
   
   fluidRow(column(width=12,
                   div(id="server_dashboard__panel_info",
                       style="background-color:skyblue;margin:2px;padding:2px;border-top:solid black 1px;border-bottom:solid black 1px;",
                              uiOutput(outputId="server_dashboard__panel_cell_info",inline = TRUE)),
                   div(id="server_dashboard__panel_browser",
                     DT::dataTableOutput(outputId="server_dashboard__browser")))
  
            )
  #)
), #end tabPanel
#################
###TAB REPORTS
#################
tabPanel("Reports",value="reports",
  div(style="width:100%;padding:10px;background-color:white;",
      
          div(align="left",
              style="width:100%;display:flex;flex-flow:row nowrap;flex-grow:1;margin-right:5px;",
              
              div(style="min-width:250px;width:100%;margin-right:5px;",
                  textInput(inputId="server_dashboard_reports__search",
                            label="Search",
                            value="",
                            placeholder="Search reports...")
              ),
              div(style="min-width:200px;margin-right:5px;",
                  selectizeInput(inputId="server_dashboard_reports__owner",
                                 label="Report Owner",
                                 width="100%",
                                 multiple=FALSE,
                                 choices="",
                                 selected = "")),
          ),
          div(align="left",
              style="width:100%;display:flex;flex-flow:row nowrap;flex-grow:1;margin-right:5px;",
              id="server_dashboard__panel_reports",
                  DT::dataTableOutput(outputId="server_dashboard_reports__list",
                                      width="100%")
          )
      
      
  )
),

####################
tabPanel("Export Templates",value="templates",
         fluidRow(style='padding-top:10px;padding-bottom:10px;',
                  column(10,align="left",style='display:inline-block;',
                         div(style="display:flex;flex-flow:row nowrap;",
                             div(selectizeInput(inputId="server_dashboard_exports__selected_template",
                                                label="Select Export Template",
                                                choices=character(0),
                                                selected=NULL,
                                                width="450px",
                                                multiple=FALSE,
                                                options=list(placeholder="Select a Template to View or Launch"))),
                             div(style="padding-left:15px;",
                                 textInput(inputId="server_dashboard_exports__search",
                                           label="Search Indicators",
                                           value="",
                                           width="200px",
                                           placeholder="Enter keywords")),
                             div(style="padding-left:5px;padding-top:25px;",
                                 actionButton(inputId="server_dashboard_exports__clear",
                                              label=NULL,
                                              icon=icon("xmark"),
                                              class="btn-default")),
                             div(style="justify-self:start;flex-grow:1;padding-left:10px;padding-top:25px;",
                                 actionButton(inputId="server_dashboard_exports__create_export_template",
                                              label="Create New Template",
                                              class="btn-primary",
                                              icon=icon("puzzle-piece")))))
         ),
         fluidRow(column(12,align="center",
                         panel(
                           heading="Export Template",
                           footer=NULL,
                           div(align="left",
                               uiOutput(outputId="server_dashboard_exports__template_ui"))
                         ))
         )
         
)

##############end templates
) #end tabsetPanel
) #end div
