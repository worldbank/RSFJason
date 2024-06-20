
    # panel(
    #   heading="Create and Manage Indicator Definitions, Calculations and Checks",
    #   footer=NULL,
    div(style="background-color:white;padding:10px;",
      tabsetPanel(id="tabset_admin_system",
                  tabPanel("Indicators",
                           fluidRow(style='padding-top:10px;padding-bottom:10px;',
                                    column(10,align="left",style='display:inline-block;',
                                      div(style="display:flex;flex-flow:row nowrap;",
                                          div(selectizeInput(inputId="admin_system_selected_indicator",
                                                     label="Edit Indicator",
                                                     choices=character(0),
                                                     selected=NULL,
                                                     width="450px",
                                                     multiple=FALSE,
                                                     options=list(placeholder="Select Indicator to Edit"))),
                                          div(style="padding-left:15px;",
                                              textInput(inputId="server_admin_indicators__search",
                                                           label="Search Indicators",
                                                           value="",
                                                           width="200px",
                                                           placeholder="Enter keywords")),
                                          div(style="padding-left:5px;padding-top:25px;",
                                            actionButton(inputId="server_admin_indicators__clear",
                                                         label=NULL,
                                                         icon=icon("xmark"),
                                                         class="btn-default")),
                                          div(style="justify-self:start;flex-grow:1;padding-left:10px;padding-top:25px;",
                                              actionButton(inputId="server_admin_indicators__create_indicator",
                                                     label="Create New Indicator",
                                                     class="btn-primary",
                                                     icon=icon("puzzle-piece"))))),
                                    
                                    column(2,downloadButton(outputId="admin_system_download_indicators",
                                                          label="Download Indicators",
                                                          class="btn-success",
                                                          icon=icon("download")))
                           ),
                           fluidRow(column(12,align="center",
                                           panel(
                                             heading="Edit Indicator",
                                             footer=NULL,
                                             div(align="left",
                                                 uiOutput(outputId="admin_system_edit_indicator"))
                                           ))
                           )

                  ),
                  tabPanel("Checks",
                           fluidRow(style='padding-top:10px;padding-bottom:10px;',
                                    column(10,align="left",style='display:inline-block;',
                                           div(style="display:flex;flex-flow:row nowrap;",
                                               div(selectizeInput(inputId="server_admin_checks__selected_check",
                                                                  label="Edit Check",
                                                                  choices=character(0),
                                                                  selected=NULL,
                                                                  width="450px",
                                                                  multiple=FALSE,
                                                                  options=list(placeholder="Select Check to Edit"))),
                                               div(style="padding-left:15px;",
                                                   textInput(inputId="server_admin_checks__search",
                                                             label="Search Checks",
                                                             value="",
                                                             width="200px",
                                                             placeholder="Enter keywords")),
                                               div(style="padding-left:5px;padding-top:25px;",
                                                   actionButton(inputId="server_admin_checks__clear",
                                                                label=NULL,
                                                                icon=icon("xmark"),
                                                                class="btn-default")),
                                           div(style="justify-self:start;flex-grow:1;padding-left:10px;padding-top:25px;",
                                               actionButton(inputId="server_admin_checks__create_check",
                                                            label="Create New Check",
                                                            class="btn-primary",
                                                            icon=icon("puzzle-piece"))))),
                                    
                                    column(2,downloadButton(outputId="server_admin_checks__download_checks",
                                                            label="Download Checks",
                                                            class="btn-success",
                                                            icon=icon("download")))
        
                           ),
                           fluidRow(column(12,align="center",
                                           panel(
                                             heading="Edit Check",
                                             footer=NULL,
                                             div(align="left",
                                                 uiOutput(outputId="server_admin_checks__edit_check_UI"))
                                           ))
                           )
                           
                  ),
                  tabPanel("Choices Groups",
                           fluidRow(style='padding-top:10px;padding-bottom:10px;',
                                    column(12,align="left",style='display:inline-block;',
                                           div(style="display:inline-block;vertical-align:middle;",
                                               selectizeInput(inputId="admin_system_selected_option",
                                                              label="Edit Choices",
                                                              choices=character(0),
                                                              selected=NULL,
                                                              width="500px",
                                                              multiple=FALSE,
                                                              options=list(placeholder="Select Group to Edit"))),
                                           div(style="display:inline-block;vertical-align:middle;padding-left:10px;padding-top:4px;",
                                               actionButton(inputId="admin_system_create_option",
                                                            label="Create New Choice Group",
                                                            class="btn-primary",
                                                            icon=icon("puzzle-piece"))))
                           ),
                           fluidRow(column(12,align="center",
                                           panel(
                                             heading="Edit Choice",
                                             footer=NULL,
                                             div(align="left",
                                                 uiOutput(outputId="admin_system_edit_options"))
                                           ))
                           )
                  )
      )
    )



