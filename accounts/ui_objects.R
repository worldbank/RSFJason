sidebar_width <- 250
LOCATION_SKIN  <- switch(LOCATION,
                         SSA_DEV="red",
                         SSA_PROD="purple",
                         Jason_DEV="yellow",
                         Jason_PROD="blue",
                         "green")

#tags$link(rel = "stylesheet", type = "text/css", href = "research.css"),
#https://stackoverflow.com/questions/37572035/trouble-with-reactivity-when-binding-unbinding-datatable
#                       tags$head(tags$script('Shiny.addCustomMessageHandler("cookies", function(arg) {  Shiny.onInputChange("cookie_report", document.cookie); });'),
#                                 tags$script('assertBinding = function(container_id,binding_id) {
#               if (typeof Shiny.shinyapp.$bindings[binding_id]=="undefined") Shiny.bindAll($(document.getElementById(container_id)))
#               else {
#                 
#                 Shiny.unbindAll($(document.getElementById(container_id)));
#                 Shiny.shinyapp.$bindings[binding_id] = undefined;
#                 Shiny.bindAll($(document.getElementById(container_id)));
#               }
# //console.log("Skipping binding request to "+container_id+" for existing binding: "+binding_id)
#             }'
#           )),
# useShinyjs(),

accounts_NS <- NS("accounts_server")

ui_htmlHead <- tagList(
  tags$head(tags$script(paste0('Shiny.addCustomMessageHandler("cookies", function(arg) {  Shiny.setInputValue("',accounts_NS("cookie_report"),'", document.cookie, {priority: "event"}); });'))),
  #tags$link(rel = "stylesheet", type = "text/css", href = "research.css"),
  tags$style(".shiny-notification { position:fixed; top: calc(50%); left: calc(25%); width:800px; }
            
            .shiny-input-container:not(.shiny-input-container-inline)
            {
              width: 100%;
            }
            
            .modal-dialog {
                display: inline-block;
                text-align: left;
                vertical-align: middle;
                <!-- width: 900px !important; -->
                width: 900px !important;
              }

              .modal-lg {
                width: 85% !important;
              }

              .modal {
                text-align: center;
                padding: 0!important;
                background-color: rgba(0, 0, 0, 0.4);
              }

              .modal-content { 
                border-radius:10px;
                border:solid rgba(122, 24, 122, 0.2) 2px
              }
              .modal:before {
                content: '';
                display: inline-block;
                height: 100%;
                vertical-align: middle;
                margin-right: -4px;
              }
              .modal-header { background-color: rgba(122, 24, 122, 0.6);
                color:white;
              }
              .modal-title { font-weight:bold; color:white; font-size:22px; }
  
              .pointer:hover { cursor:pointer; }

              .fas[data-count]{
                  position:relative;
              }

              .fas[data-count]:after{
                  position: absolute;
                  
                  left: 6px;
                  top: -7px;
                  content: attr(data-count);
                  border-radius: 12px;
                  color: black;
                  background: rgba(255,255,255,0.2);
                  text-align: center;
                  padding:3px 3px 3px 3px;
                  min-width: 2em;
                  font-size:8px;
                  font-family:sans-serif;
                  font-weight:bold;
              }
              .fas[data-count]:hover:after{
                  font-size:13px;
                  background: rgba(255,255,255,0.85);
                  border:solid white 2px;
                  left:-5px;
                  top: -3px;
                  
              }


              .fa-history { margin-left:15px; color:navy; }
              .fas.fa-history[data-count]:after{
                  position: absolute;
                  left: -10px;
                  top: 3px;
                  content: attr(data-count);
                  color: black;
                  text-align: center;
                  padding:3px 3px 3px 3px;
                  font-size:8px;
                  font-family:sans-serif;
                  font-weight:bold;
              }
              .fas.fa-history[data-count]:hover:after{ font-size:8px;
                  background: transparent;
                  border:none;
                  left:-10px;
              }

              .fas.fa-eye[data-count]:after{
                  left: 15px;
                  font-size:10px;

              }
              
              .fas.fa-eye[data-count=\"!\"]:after{
                  font-weight:800;
                  color:crimson;
                  border:solid red 2px;
                  left: 15px;
                  font-size:10px;
              }

              .fas.fa-eye[data-count]:hover:after{ 
                  left:15px;
                  top:-9px;
                  border:solid crimson 2px;
                  font-size:10px;
              }




              .fas.fa-flag[data-count]:after{
                  left: 10px;
                  font-size:8px;
              }
              
              .fas.fa-flag[data-count=\"!\"]:after{
                  font-weight:800;
                  color:crimson;
                  border:solid red 2px;
                  left: 10px;
                  font-size:8px;
              }

              .fas.fa-flag[data-count]:hover:after{ 
                  left:10px;
                  top:-9px;
                  border:solid crimson 2px;
                  font-size:8px;
              }

              button:disabled { background-color:gainsboro; }
              .btn-blue { background-color:skyblue;font-weight:bold; }
              .btn-blue:hover { background-color:aliceblue; }

              .picker .btn-default { background-color:white; }

              .check_mark {
                width:min-content;
                margin-top:2px;
                border-top-right-radius:12px;
                border-bottom-right-radius:12px;
                border:solid 1px white;
                vertical-align:middle;
                text-align:right;
              }
            
              .check_bubble {
                width:min-content;
                padding:2px 4px 2px 4px;
                margin-left:9px;
                border-top-right-radius:12px;
                border-bottom-right-radius:12px;
                border-left:solid 1px white;
                text-align:center;
                vertical-align:center;
                font-size:12px;
                font-family:sans-serif;
                font-weight:bold;
                color:white;
                background:white;
              }
            
              .indicator_bubble {
                width:min-content;
                padding:2px 4px 2px 4px;
                margin-top:2px;
                border-radius:12px;
                border:solid 1px white;
                text-align:center;
                vertical-align:center;
                font-size:12px;
                font-family:sans-serif;
                font-weight:bold;
                color:white;
                background:white;
              }
              
              .icon-loan { color: #336600; }
              .icon-borrower { color: #CC6600; }
              .icon-client { color: #990000; }
              .icon-facility { color: #660066; }
              .icon-program { color: #000066; }
              .icon-global { color: #00ffea; }

            
              .indicator_bubble.indicator { background-color: #cc0000; }
              .indicator_bubble.indicator.loan { background-color: #336600; }
              .indicator_bubble.indicator.borrower { background-color: #CC6600; }
              .indicator_bubble.indicator.client { background-color: #990000; }
              .indicator_bubble.indicator.facility { background-color: #660066; }
              .indicator_bubble.indicator.program { background-color: #000066; }
              .indicator_bubble.indicator.global { background-color: #00ffea; color:black; }
              .indicator_bubble.indicator.system { cursor:not-allowed; }
              .indicator_bubble.indicator.formula { background-color: #d4af37; color:black; }
              .indicator_bubble.indicator.customformula { background-color: #ffdf00; color:black; }
              .indicator_bubble.indicator.noformula { background-color: #B2FF33; color:black; }
              .indicator_bubble.unsubscribed { background-color:gainsboro; color:slategray; }
              .indicator_bubble.indicator.unsubscribed { background-color:gainsboro;  color:slategray; }
            
              .check_bubble.critical { background-color: FireBrick; }
              .check_bubble.error { background-color: red; }
              .check_bubble.warning { background-color: orange; }
              .check_bubble.info { background-color: blue; }
              .check_bubble.system { cursor:not-allowed; }
              .check_bubble.unsubscribed { background-color:gainsboro; color:slategray; }
              .check_mark:has(.unsubscribed){ background:none !important; background-color:slategray !important; }
              
              .icon-question { 
                  width:12px;height:12px;
                  border-radius:12px;
                  text-align:center;
                  vertical-align:top;
                  padding-top:1px;
                  font-size:9px;
                  color:white;
                  background:forestgreen;
                  
              }
            
              .icon-action { width:25px;height:25px;color:#000066;padding-top:5px; }
              .icon-action:hover { color:blue; }

              .icon-trash { width:25px;height:25px; }
              .icon-trash:hover { color:limegreen; }

              .icon-edit { width:25px;height:25px; }
              .icon-edit:hover { color:forestgreen; }
             
             .icon-view { width:25px;height:25px;color:rgba(0,0,0,0.4); }
             .icon-view:hover { color:skyblue; }
            
             .icon-excel { width:25px;height:25px;color:rgba(0,0,0,0.4); }
             .icon-excel:hover { color:forestgreen; }
             
              .icon-green { width:20px;height:20px;color:forestgreen; }
              
              .icon-blue { width:20px;height:20px;color:blue; }
              .icon-orange { width:20px;height:20px;color:orange; }
              .icon-red { width:20px;height:20px;color:red; }

                
              .icon-info { color:blue;margin-left:1px; }
              .icon-reporting { color:teal;margin-left:2px;margin-right:2px; }
              .icon-warning { color:orange;margin-left:3px; }
              .icon-error { color:red;margin-left:3px; }
              .icon-critical { color:FireBrick;margin-left:3px; }
              .icon-disabled { color:gray; }
              
              .flag-dashboard { font-size:0.85em; vertical-align:top; opacity:0.4}
              .flag-dashboard:hover { opacity:0.85 }

              <!-- table.dataTable.hover tbody td:hover, table.dataTable.display tbody td:hover { background-color: rgba(0,0,0,0.1); } -->
              .cell-dashboard { height: 1em; }
              <!--.cell-dashboard:hover { cursor:zoom-in; }-->

              .circle {
                  background-color:#fff;
                  border:1px solid red;    
                  height:100px;
                  border-radius:50%;
                  -moz-border-radius:50%;
                  -webkit-border-radius:50%;
                  width:100px;
              }

              .panel-border-editing {
                border:5px solid orange;
              }

    .disabledEvents {
            cursor: not-allowed;
    }
    .disabledEvents:active {
        pointer-events:none !important;
    }
    
   
             #dep-cohort_view_sources label { margin: 0px; }
             [name=cohort_filters] .form-group {
                  margin-bottom: 0px !important;
             }


             [name=Xcohort_filters] .selectize-input {
              font-size: 15px;
              line-height: 15px;
              min-height:0px;
             }
             

             [name=Xcohort_filters] .selectize-dropdown {
             font-size: 15px;
             line-height: 15px;
             }
             [name=Xcohort_filters] .selectize-control { margin-bottom:0px; height:30px}
             [name=Xcohort_filters] .selectize-control.multi .selectize-input.has-items { padding:3px 10px 1px 5px; }


             #datasets_review_box .box-title {
              font-weight:600;
              font-size:20px;
              margin-top:2px;
              width:100%;
             }
             
             #datasets_review_box .btn-box-tool {
              padding-top:1px;
              padding-bottom:1px;
              font-size:12px;
              font-weight:400;
              border-radius:12px;
              border:solid black 1px;
              color:black;
              background-color:#0080009e;
             }
             
             #datasets_review_box .box-header { 
              background-color:rgba(171,215,230,0.3);
              padding-top:3px;
              padding-bottom:3px;
             }
             

             li.dropdown-header {
               font-size:1.10em;
               font-weight:bold;
               color:#337ab7;
               background-color: rgba(9,9,9,0.2);
             }

             .options-groups-labels .form-group,.form-control.selectize-control { margin-bottom:0px; }
             .dropdown-header-orange { background-color:orange !important; }

             [name=formulaHelper] .selectize-input {
              font-size: 11px;
              line-height: 15px;
              color:navy;
              min-height:30px;
              vertical-align: bottom;
              border:none;
              padding-left:25px;
             }
             [name=formulaHelper] .selectize-control { margin-bottom:0px;}
             [name=formulaHelper] .selectize-dropdown {
             font-size: 11px;
             color:navy;
             line-height: 11px;
             }

             [name=formulaHelper] .form-group { margin-bottom: 0px !important; }
             [name=formulaHelper] .selectize-control.single .selectize-input:after {left:5px;}


             [name=formula_box] .box-header {
              
              background-color: #d4af37; color:black;
              <!-- background-color:rgba(250,250,200,0.5); --> 
             }
             
             [name=formula_box].customformula .box-header {
              
              background-color: #ffdf00; color:black;
             }

             #datasets_review_flags_summary .dataTables_scrollBody {
              max-height:inherit;
              height:inherit!important;
             }
    
             #server_datasets_review_flags_dataset .dataTables_scrollBody {
              max-height:inherit;
              height:inherit!important;
             }
    
              #server_dashboard__reporting_column_priority_lookup_results .dataTables_scrollBody {
              max-height:inherit;
              height:inherit!important;
             }
            "),
  useShinyjs())

ui_header_OUT <- dashboardHeader(title=div(ifelse(grepl("DEV",LOCATION)==TRUE,"DEV! ",""),
                                           "RSF/ Jason"),
                                 titleWidth = sidebar_width)


ui_header_IN <- dashboardHeader(title=div(ifelse(grepl("DEV",LOCATION)==TRUE,"DEV! ",""),
                                          textOutput(outputId="dashboard_title",inline=TRUE)),
                                 titleWidth = sidebar_width,
                                 #tags$li(class = 'dropdown',titleWidth = sidebar_width),
                                 tags$li(class = 'dropdown',
                                         div(style="margin-right:5px;color:white;font-size:22px;font-weight:bold;vertical-align:middle;padding-top:8px;display:inline-block",
                                             div(style="padding-right:10px;display:inline-block",textOutput(outputId=accounts_NS("login_who"))),
                                             div(style="padding-right:5px;display:inline-block",
                                                 actionButton(inputId=accounts_NS("login_logout_action"),
                                                              label="Exit",icon=icon("sign-out-alt"),class="btn btn-primary")))))

ui_sidebar_OUT <- dashboardSidebar(
  width = sidebar_width,
  collapsed = TRUE,
  disable = TRUE)



ui_sidebar_IN <- dashboardSidebar(
  width = sidebar_width,
  collapsed = FALSE,
  disable = FALSE,
  sidebarMenu(id="sidebarMenu",
    div(style="padding-left:5px;display: inline-block;width:245px;",
        selectizeInput(inputId="select_rsf_program_id",label=NULL,choices=NULL,multiple=FALSE,width="245px")),
    
    # menuItem("Reporting",  
    #          href="https://qlik.worldbank.org/hub/stream/aaec8d41-5201-43ab-809f-3063750dfafd",
    #          icon = icon("arrow-up-right-from-square")),
    
    
    div(id="ui_reporting",
        style="padding-left:25px;width=245px;padding-top:10px;",
        menuItem("Reporting", tabName = "dashboard", icon = icon("chart-area"))),
    
    div(id="ui_datasets",
        style="padding-left:25px;width=245px;padding-top:10px;",
        menuItem("Datasets",  tabName = "datasets", icon = icon("table-list"))),
        
    div(id="ui_setup",
        style="padding-left:25px;width=245px;padding-top:10px;",
        menuItem("RSF Setup", tabName = "setup", icon = icon("sliders"))),
    
    div(id="ui_system",
        style="padding-left:25px;width=245px;padding-top:10px;",
        menuItem("System", tabName = "system", icon = icon("cogs"))),
    
    hidden(
    div(id="ui_users",
        style="padding-left:25px;width=245px;padding-top:10px;",
        menuItem("Admin", tabName = "users", icon = icon("user"))))
    
    # HTML('<li id="users_ui" style="vasibility:hidden">
    #       <a href="#shiny-tab-users" data-toggle="tab" data-value="users">
    #         <i class="far fa-user" role="presentation" aria-label="user icon"></i>
    #         <span>Admin</span>
    #         </a>
    #       </li>')
  ))




ui_body_OUT <- dashboardBody(id="dashboardBody",
  div(style="width:400px;margin:auto;text-align:left;margin-top:10%;",
      wellPanel(id="location-login_panel",
                div(id=accounts_NS("login_rememberme_ui"),
                                   style="width:100%;height:25px;display:flex;flex-direction:row;flex-wrap:nowrap;position:relative;top:15px;",
                    div(style="display:flex;flex-grow:1;justify-content:flex-end;padding-top:10px",tags$label("Remember Sign-in")),
                    div(style="display:flex;flex-shrink:1;padding-left:5px;",checkboxInput(inputId=accounts_NS("login_rememberme"),
                                  label=NULL,
                                  value=TRUE))),
      
                textInput(inputId=accounts_NS("user_login"),label="Username"),
                passwordInput(inputId=accounts_NS("login_password"),label="Password"),
                hidden(div(id=accounts_NS("login_change_reset"),style="color:black;padding-top:5px;",
                           p("Your password has been reset"),
                           "Check your email for a temporary password")),
                       div(id=accounts_NS("login_button"),
                           div(style="width:100%;display:inline-block;padding-top:10px;",
                               div(column(6, actionButton(inputId=accounts_NS("login_action"),
                                                           label="Login",icon=icon("sign-in-alt"),class="btn btn-success")),
                                   column(6, actionButton(inputId=accounts_NS("forgot_password"),
                                                           label="Forgot password",icon=icon("fas fa-question"), class="btn btn-success"), align = "right")
                                ))),
                
                hidden(div(id=accounts_NS("login_failed1"),style="color:red;","Login Attempt ",
                           textOutput(outputId=accounts_NS("login_attempts"),
                                      inline=TRUE)," Failed.")),
                hidden(div(id=accounts_NS("login_failed2"),style="color:red;","Wrong login")),
                hidden(div(id=accounts_NS("login_failed3"),style="color:red;","You have no permission")),
                hidden(div(id=accounts_NS("login_failed4"),style="color:red;","Your password was reseted. Check your email")),
                hidden(div(id=accounts_NS("reset_password"),style="color:black;","Get Temporary Password ", 
                           actionButton(inputId=accounts_NS("reset_password_button"),
                                        label="Send email",icon=icon("envelope"),class="btn btn-success"))),
                hidden(div(id=accounts_NS("login_change_password"),style="padding-top:10px;border-top:double black 4px;",
                           passwordInput(inputId=accounts_NS("login_new_password1"),
                                         label="New Password"),
                           passwordInput(inputId=accounts_NS("login_new_password2"),
                                         label="Verify Password"),
                           actionButton(inputId=accounts_NS("login_change_action"),
                                        label="Login",icon=icon("sign-in-alt"),class="btn btn-success"),
                           hidden(div(id=accounts_NS("login_change_failed"),style="color:red;padding-top:5px;",
                                      textOutput(outputId=accounts_NS("login_change_failed_message"),inline=TRUE))))))))

ui_body_IN <- dashboardBody(id="dashboardBody",
                      fluidRow(
                        column(12,
                               fluidPage(
                                 tabItems(
                                   tabItem(tabName = "dashboard",source("./R/app/ui_dashboard.R",local=TRUE)$value),
                                   tabItem(tabName = "datasets",source("./R/app/ui_datasets.R",local=TRUE)$value),
                                   
                                   tabItem(tabName = "setup",source("./R/app/ui_setup.R",local=TRUE)$value),
                                   
                                   tabItem(tabName = "system",source("./R/app/ui_admin.R",local=TRUE)$value),
                                   
                                   tabItem(tabName = "users",source("./R/app/ui_users.R",local=TRUE)$value)
                                 )
                               )
                        )
                      ))