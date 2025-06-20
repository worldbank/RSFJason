    #panel(
    #  heading="Manage Program Indicator Subscriptions and Assign Indicator Checks",
    #  footer=NULL,
div(style="background-color:white;padding:10px;",
      div(style="float:right;",
          div(style="width:100%;display:flex;flex-flow:row nowrap;margin-left:10px;margin-right:10px;",
            div(downloadButton(outputId="program_download_archive",
                           label=textOutput(outputId="server_setup_download_archive_title",inline=T),
                           class="btn-success",
                           icon=icon("download"))),
            div(style="padding-left:10px;",
                downloadButton(outputId="program_download_setup",
                               label=textOutput(outputId="server_setup_download_setup_title",inline=T),
                               class="btn-success",
                               icon=icon("download"))),
            div(style="padding-left:10px;",
                downloadButton(outputId="program_download_backup",
                               label=textOutput(outputId="server_setup_download_backup_title",inline=T),
                               class="btn-primary",
                               icon=icon("download"))),
            )),
      tabsetPanel(id="tabset_setup_program",
                  
#################
###TAB INDICATORS
#################
                  tabPanel("Setup Indicators",
                           fluidRow(column(10,
                                    div(style="display:flex;flex-direction:row;padding-top:5px;",
                                      div(style="padding-left:20px",
                                          selectizeInput(inputId="ui_setup__indicator_program_facilities",
                                                     label="RSF Program Facilities",
                                                     choices=c(),
                                                     selected="",
                                                     width="280px")),
                                      
                                      hidden(div(style="padding-left:20px",
                                                 id="ui_setup__indicator_setup_filter_ui",
                                          selectizeInput(inputId="ui_setup__indicator_setup_filter",
                                                         label="RSA Setup",
                                                         choices=c(`No`=FALSE,
                                                                   `Setup`=TRUE),
                                                         options=list(placeholder="No"),
                                                         selected=FALSE,
                                                         width="80px"))),
                                      
                                      div(style="padding-left:20px",
                                          selectizeInput(inputId="ui_setup__indicator_monitoring_filter",
                                                         label="Monitoring Filter",
                                                         choices=c(`No Filter`="all",
                                                                   `Monitored`="subscribed",
                                                                   `Not Monitored`="unsubscribed",
                                                                   `Never Reported`="unreported",
                                                                   `Auto-Monitoring`="auto",
                                                                   `Manual-Monitoring`="setup"),
                                                         options=list(placeholder="No Filter"),
                                                         selected="",
                                                         width="150px")),
                                      
                                      
                                      
                                      div(style="padding-left:20px",
                                          selectizeInput(inputId="ui_setup__indicator_calculated_filter",
                                                         label="Calculated Filter",
                                                         choices=c(`No Filter`="all",
                                                                   `Calculated`="calculated",
                                                                   `Multiple Formulas`="formulas",
                                                                   `Reported`="reported"),
                                                         options=list(placeholder="No Filter"),
                                                         selected="",
                                                         multiple=TRUE,
                                                         width="150px")),
                                      
                                      div(style="padding-left:20px",
                                          selectizeInput(inputId="ui_setup__indicator_category_filter",
                                                         label="Category Filter",
                                                         choices=c(`No Filter`="all",
                                                                   `Program`="program",
                                                                   `Facility`="facility",
                                                                   `Client`="client",
                                                                   `Borrower`="borrower",
                                                                   `Loan`="loan"),
                                                         selected="",
                                                         options=list(placeholder="No Filter"),
                                                         width="125px")),
                                      div(style="padding-left:20px;flex-grow:1;",
                                          textInput(inputId="ui_setup__indicator_search_filter",
                                                    label="Search Filter",
                                                    value="",
                                                    placeholder="Keyword search",
                                                    width="250px"))
                                      
                                    )),
                                    column(2,
                                           align="right",style='padding-top:10px;padding-left:10px;padding-right:10px;',
                                           div(style="text-align:left;float:right;",
                                               actionButton(inputId="ui_setup__indicators_recalculate",
                                                            label="Recalculate"))),
                           ),
                           fluidRow(column(12,
                                           div("To toggle indicator subscriptions: check it and click on the crossing-arrows button.  To edit sort ordering or monitoring comments: double-click in the cell to make edits."))),
                           fluidRow(column(12,
                                           DT::dataTableOutput(outputId="ui_setup__indicators_monitored_table")))),
#############
###TAB CHECKS
#############
  tabPanel("Setup Checks",
           fluidRow(column(10,
                           div(style="display:flex;flex-direction:row;padding-top:5px;",
                               div(style="padding-left:20px",
                                   selectizeInput(inputId="ui_setup__checks_program_facilities",
                                                  label="RSF Program Facilities",
                                                  choices=c(),
                                                  selected="",
                                                  width="280px")),
                               
                               div(style="padding-left:20px",
                                   selectizeInput(inputId="ui_setup__checks_monitoring_filter",
                                                  label="Monitoring Filter",
                                                  choices=c(`No Filter`="all",
                                                            `Monitored`="subscribed",
                                                            `Not Monitored`="unsubscribed",
                                                            `Never Flagged`="unreported"),
                                                  options=list(placeholder="No Filter"),
                                                  selected="",
                                                  width="140px")),
                               
                               div(style="padding-left:20px",
                                   selectizeInput(inputId="ui_setup__checks_category_filter",
                                                  label="Category Filter",
                                                  choices=c(`No Filter`="all",
                                                            `Program`="program",
                                                            `Facility`="facility",
                                                            `Client`="client",
                                                            `Borrower`="borrower",
                                                            `Loan`="loan"),
                                                  selected="",
                                                  options=list(placeholder="No Filter"),
                                                  width="125px")),
                               
                               
                               div(style="padding-left:20px",
                                   selectizeInput(inputId="ui_setup__checks_type_filter",
                                                  label="Type Filter",
                                                  choices=c(`No Filter`="",
                                                            `<i class='fa-solid fa-square' style='color:limegreen'></i> Contract Compliance`="contract",
                                                            `<i class='fa-solid fa-square' style='color:skyblue'></i> Business Rules`="business",
                                                            `<i class='fa-solid fa-square' style='color:violet'></i> Data Validity`="data",
                                                            `<i class='fa-solid fa-square' style='color:pink'></i> Unclassified`="none"),
                                                  selected="",
                                                  options=list(placeholder="Any Flags",
                                                               render = I("{
                                                                            item: function(item, escape) { return '<div>' + item.label + '</div>'; },
                                                                            option: function(item, escape) { return '<div>' + item.label + '</div>'; }
                                                                           }")),
                                                  width="175px")),
                               
                               div(style="padding-left:20px;flex-grow:1;",
                                   textInput(inputId="ui_setup__checks_search_filter",
                                             label="Search Filter",
                                             value="",
                                             placeholder="Keyword search",
                                             width="250px"))
                           )),
                    column(2,
                           align="right",style='padding-top:10px;padding-left:10px;padding-right:10px;',
                           div(style="text-align:left;float:right;",
                               actionButton(inputId="ui_setup__checks_recheck",
                                            label="Recheck"))),
           ),
           fluidRow(column(12,
                           
                           DT::dataTableOutput(outputId="ui_setup__checks_monitored_table")))),


################
###TAB TEMPLATES
################

tabPanel("Setup Templates",
         fluidRow(column(12,
                         div(style="display:flex;flex-direction:row;padding-top:5px;",
                             div(style="padding-left:20px",
                                 selectizeInput(inputId="ui_setup__templates_program_facilities",
                                                label="RSF Program Facilities",
                                                choices=c(),
                                                selected="",
                                                width="280px")),
                             
                             div(style="padding-left:20px",
                                 selectizeInput(inputId="ui_setup__template_selected",
                                                label="Templates Listing",
                                                choices="",
                                                options=list(placeholder="Nothing selected"),
                                                selected="",
                                                width="250px")),
                             
                             div(style="padding-left:40px;padding-top:25px;",
                                 downloadButton(outputId="ui_setup__template_headers_download",
                                                label="Download Headers",
                                                width="250px")),
                             
                             div(style="padding-left:20px",
                                 fileInput(inputId="ui_setup__template_headers_upload",
                                           label="Upload Headers",
                                           accept="xlsx",
                                           width="250px"))
                         ))
         ),
         fluidRow(style="margin-bottom:15px;",
                  column(12,
                         uiOutput(outputId="ui_setup__templates_add_mapping_UI"))),

         fluidRow(column(12,
                         
                          div(name="template_header_box",
                              id="ui_setup__templates_mapping_labels",
                              div(style="display:flex;flex-flow:row nowrap;justify-content:left;align-items:start;",
                                div(style="display:flex;flex-flow:row nowrap;min-width:100px;width:200px;padding:0 0 0 2px;white-space:nowrap;",
                                    tags$label("Location")),
                                div(style="display:flex;flex-flow:row nowrap;min-width:450px;padding:0 0 0 2px;flex-grow:1;white-space:nowrap;",
                                    tags$label("Template Header or Match Value")),
                                div(style="display:flex;flex-flow:row nowrap;min-width:175px;width:175px;padding:0 0 0 2px;white-space:nowrap;",
                                    tags$label("Action")),
                                div(style="display:flex;flex-flow:row nowrap;min-width:400px;padding:0 0 0 2px;white-space:nowrap;",
                                    tags$label("Target")),
                                div(style="display:flex;flex-flow:row nowrap;min-width:150px;padding:0 0 0 2px;white-space:nowrap;",
                                    tags$label("Comment")),
                                div(style=paste0("display:flex;flex-flow:row nowrap;width:40px;padding:0 0 0 2px;"),"")
                              )
                          ),
                          uiOutput(outputId="ui_setup__templates_mapping_UI")))),

#################
###TAB CREATE NEW
#################

                  tabPanel("Create New...",
                           fluidRow(column(12,align='left',style='padding-top:10px;padding-bottom:10px;',
                                      div(style="width:100%;display:flex;flex-flow:row wrap",
                                           div(style="width:150px",
                                               selectizeInput(inputId="setup_program_create_what",
                                                        label="Create New...",
                                                        choices=c("",
                                                                  Program="program"),
                                                        selected="",
                                                        width="100%",
                                                        options=list(placeholder="Select what...")
                                                        )),
                                           div(style="min-width:200px;flex-grow:1;margin-left:10px;",
                                               hidden(selectizeInput(inputId="setup_program_create_selected_facility",
                                                                     label="For Facility",
                                                                     choices=c(""),
                                                                     width="100%",
                                                                     selected="",
                                                                     options=list(placeholder="Select Facility...")))),
                                           
                                          div(style="min-width:200px;flex-grow:1;margin-left:10px;",
                                              hidden(selectizeInput(inputId="setup_program_create_selected_client",
                                                                     label="For Client",
                                                                     choices=c(""),
                                                                     width="100%",
                                                                     selected="",
                                                                     options=list(placeholder="Select Client...")))),
                                           
                                          div(style="min-width:200px;flex-grow:1;margin-left:10px;",
                                              hidden(selectizeInput(inputId="setup_program_create_selected_borrower",
                                                                     label="For Borrower",
                                                                     choices=c(""),
                                                                     width="100%",
                                                                     selected="",
                                                                     options=list(placeholder="Select Borrower..."))))))
                            ),
                            fluidRow(column(12,align="center",
                                            panel(style="width:100%;",
                                              heading=textOutput(outputId="setup_program_create_what_text"),
                                              footer=NULL,
                                              div(align="left",style="display:flex;justify-content:center;flex-flow:column;width:75%;",
                                                  uiOutput(outputId="setup_program_create_ui"))))
                            )
              )
        )
      )
