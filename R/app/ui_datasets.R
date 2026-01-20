#panel(style="min-width:1000px;",
div(style="background-color:white;padding:10px;",

  div(style="float:right;position:relative;padding-left:50px;height:34px;",
      actionButton(inputId="action_template_upload_new",label="Upload Template",class="btn-primary",icon=icon("upload"))
  ),
  
  div(style="position:relative;float:right;display:inline-block;white-space:nowrap;height:34px;",
         div(style="display:inline-block",shiny::tags$label("Search Datasets")),
         
         div(style=" display:inline-block;",selectInput(input="dataset_review_filter_facility",
                                                   label=NULL,
                                                   choices=NULL,
                                                   selected=NULL,
                                                   multiple=FALSE,
                                                   selectize=FALSE,
                                                   #options=list(placeholder="Client Filter..."),
                                                   width="150px")),
         div(style=" display:inline-block;",textInput(input="dataset_review_filter",
                                                   label=NULL,
                                                   placeholder="Enter key words...",
                                                   value="recent 15",
                                                   width="150px")),
         div(style="display:inline-block;position:relative;top:-2px;",
             actionButton(inputId="action_dataset_review_filter_clear",label=NULL,icon=icon("window-close"),
                          class="btn btn-outline-secondary btn-sm"))
  ),
  
  #heading="Manage Datasets and Template Uploads",
  #footer=NULL,
  tabsetPanel(id="datasetsTabset",
              
              #######################
              tabPanel("Uploads List",value="list",
                       fluidRow(column(width=12,
                                       div(style="padding-top:5px;min-width:1200px",
                                       DT::dataTableOutput(outputId="list_reporting_cohorts",width="100%"))))),
              
              #########################
              tabPanel("Dataset Review",value="review",
                       fluidRow(id="dataset_collections_select",
                                style="padding-top:15px;",
                                
                         column(10,
                                selectizeInput(inputId="cohort_collection_selected_id",
                                      label="Datasets Collection",
                                      choices=NULL,
                                      selected=NULL,
                                      options=list(placeholder="Select a dataset for review"),
                                      width="100%")),
                         column(2,align="right",style="padding-top:25px;",
                                downloadButton(outputId="datasets_review_download_source_action",
                                               label="Download Template",
                                               class="btn btn-success",
                                               icon=icon("download")))
                       ),
                       shinyjs::hidden(
                        fluidRow(id="dataset_review_header",
                          column(12,
                                 fluidRow(style="background-color:cadetblue;",
                                          column(12,
                                                 div(style="width:100%;text-align:center;color:white;font-size:20px;",
                                                 uiOutput(outputId="datasets_review_title")))),
                                 ##FILTERS
                                 fluidRow(style="padding-top:5px;background-color:whitesmoke;",
                                          
                                   div(align="left",
                                       style="width:100%;display:flex;flex-flow:row wrap;margin-left:10px;margin-right:10px;",

                                       div(style="display:flex;min-width:120px;flex-shrink:1;flex-flow:row nowrap;margin-right:10px;",
                                           name="flag_options_filters",
                                           selectizeInput(inputId="cohort_view_flagged_data",
                                                                multiple=TRUE,
                                                                label="Flags View",
                                                                width="100%",
                                                                choices="",
                                                                selected="",
                                                                options=list(placeholder="All Data"))),
                                       
                                       div(style="display:flex;min-width:200px;flex-shrink:2;flex-flow:row nowrap;margin-right:10px;",
                                           name="flag_options_filters",
                                           selectizeInput(inputId="cohort_view_flag_classes",
                                                          multiple=TRUE,
                                                          label="Flags Severity",
                                                          width="100%",
                                                          choices="",
                                                          selected="",
                                                          options=list(placeholder="Any Flags",
                                                                       render = I("{
                                                                                      item: function(item, escape) { return '<div>' + item.label + '</div>'; },
                                                                                      option: function(item, escape) { return '<div>' + item.label + '</div>'; }
                                                                                     }")))
                                       ),
                                       
                                       div(style="display:flex;min-width:200px;flex-shrink:2;flex-flow:row nowrap;margin-right:10px;",
                                           name="flag_options_filters",
                                           selectizeInput(inputId="cohort_view_flag_types",
                                                          multiple=TRUE,
                                                          label="Types: Flags & Data",
                                                          width="100%",
                                                          choices="",
                                                          selected="",
                                                          options=list(placeholder="Any Flags",
                                                                       render = I("{
                                                                                      item: function(item, escape) { return '<div>' + item.label + '</div>'; },
                                                                                      option: function(item, escape) { return '<div>' + item.label + '</div>'; }
                                                                                     }")))
                                       ),
                                       
                                       div(style="display:flex;min-width:250px;flex-shrink:2;flex-flow:row nowrap;margin-right:10px;",
                                           name="flag_options_filters",
                                           selectizeInput(inputId="cohort_view_flag_indicator_classifications",
                                                          multiple=FALSE,
                                                          label="Priority Indicator Flags",
                                                          width="100%",
                                                          choices="",
                                                          selected="",
                                                          options=list(placeholder="Nothing flagged"))
                                       ),
                                       
                                       div(style="display:flex;text-align:center;flex-grow:1;flex-flow:row nowrap;",
                                           div(" ")
                                       ),
                                       div(style="display:flex;min-width:120px;text-align:center;flex-flow:row nowrap;margin-right:10px;",
                                         div(style="display:flex;text-align:center;flex-flow:column nowrap;",
                                                     div(tags$label(
                                                       div(style="display:flex;flex-flow:row nowrap",
                                                           "Active Flags: ",
                                                           textOutput(outputId="cohort_view_reported_flags_active_total",inline=TRUE)))),
                                                     div(style='text-align:center',
                                                             htmlOutput(outputId="cohort_view_html_flags_active",inline=TRUE)))
                                       ),
                                       div(style="display:flex;min-width:120px;text-align:center;flex-flow:row nowrap;margin-right:10px;",
                                         div(style="display:flex;text-align:center;flex-flow:column nowrap;",
                                             div(tags$label(
                                               div(style="display:flex;flex-flow:row nowrap",
                                                   "Resolved Flags: ",
                                                   textOutput(outputId="cohort_view_reported_flags_resolved_total",inline=TRUE)))),
                                            div(style='text-align:center',
                                                htmlOutput(outputId="cohort_view_html_flags_resolved",inline=TRUE)))
                                       ),
                                       
                                       div(style="display:flex;min-width:120px;text-align:center;flex-flow:row nowrap;margin-right:10px;",
                                           div(style="margin-top:25px;",
                                               downloadButton(outputId="datasets_review_download_flags_action",
                                                          label="Export Flags",
                                                          class="btn btn-success",
                                                          icon=icon("file-export")))
                                       ),
                                       
                                       div(style="display:flex;min-width:120px;text-align:center;flex-flow:row nowrap;margin-right:10px;",
                                          div(style="margin-top:25px;",
                                              actionButton(inputId='cohort_action_dashboard',
                                                            label="Dashboard",
                                                            class='btn btn-primary',
                                                            icon=icon("eye")))
                                       )
                                 )),
                                 ##############FLAGS REVIEW
                                 
                                 fluidRow(column(12,style="padding-left:0px;padding-right:0px;",
                                                 div(id="datasets_review_box",
                                                     fluidRow(column(12,style="padding-bottom:10px;",
                                                                     DT::dataTableOutput(outputId="datasets_review_flags_summary",width = "100%")
                                                     ))
                                                 )
                                          )
                                 )
                                 
                                )
                            ))
                       ))
)
