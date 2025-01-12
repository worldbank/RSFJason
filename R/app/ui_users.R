
    div(style="background-color:white;padding:10px;",
       fluidRow(style='padding-top:10px;padding-bottom:10px;',
                column(10,align="left",style='display:inline-block;',
                  div(style="display:flex;flex-flow:row nowrap;",
                      div(selectizeInput(inputId="server_admin_users__filter_username",
                                 label="Filter User",
                                 choices="",
                                 selected="",
                                 width="450px",
                                 multiple=TRUE,
                                 options=list(placeholder="Username Filter"))),
                      div(style="padding-left:15px;",
                          selectizeInput(inputId="server_admin_users__filter_facility",
                                         label="Filter Facility",
                                         choices="",
                                         selected="",
                                         width="450px",
                                         multiple=FALSE,
                                         options=list(placeholder="Facility Filter"))),
                      div(style="padding-left:5px;padding-top:25px;",
                        actionButton(inputId="server_admin_users__clear",
                                     label=NULL,
                                     icon=icon("xmark"),
                                     class="btn-default")),
                      
                      div(style="padding-left:25px;padding-top:25px;",
                          actionButton(inputId="server_admin_users__permissions",
                                       label="Set Permissions",
                                       class="btn-success",
                                       icon=icon("user-plus"))),
                      
                      div(style="justify-self:start;flex-grow:1;padding-left:10px;padding-top:25px;",
                          actionButton(inputId="server_admin_users__create_user",
                                 label="Create New User Account",
                                 class="btn-primary",
                                 icon=icon("puzzle-piece")))))
       ),
       fluidRow(column(12,align="center",
                       
                         div(align="left",
                             DT::dataTableOutput(outputId="server_admin_users__users_table"))
                       )
       )

    )



