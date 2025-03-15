SERVER_ADMIN_USERS_LIST.REFRESH <- reactiveVal(0)
SERVER_ADMIN_USERS_LIST <- eventReactive(c(SERVER_ADMIN_USERS_LIST.REFRESH(),
                                           LOGGEDIN()), {
  
  if (!LOGGEDIN()) return (NULL)

  user_data <- DBPOOL %>% dbGetQuery("
    select 
    vai.account_id,
    vai.users_name,
    vai.users_login,
    per.rsf_pfcbl_id,
    per.sys_name,
    coalesce(nids.pfcbl_category || ':' || nids.rsf_full_name,per.sys_name,'NOTHING') as rsf_name, -- sys_name may be SYSTEM when nids is null
    per.granted,
    role.role_name,
    custom.permissions_granted,
    case when per.sys_name is NULL and per.granted is null then 'ACCOUNT EXISTS'
         else coalesce(NULLIF(trim(concat(role.role_name,' ',custom.permissions_granted)),''),'NONE') end as permissions_text
    from p_rsf.view_account_info vai
    left join users.permissions per on per.account_id = vai.account_id
                                   and (exists(select * from users.view_rsf_pfcbl_id_family_permissions_granted fpg 
                                               where fpg.rsf_pfcbl_id = per.rsf_pfcbl_id
                                                 and fpg.account_id = $1::text
                                                 and fpg.permission_name = 'ADMIN')
                                        OR 
                                        -- I am a SYSTEM admin
                                        exists(select * from users.view_account_permissions_granted apg
                                               where apg.rsf_pfcbl_id is NULL
                                                 and apg.account_id = $1::text
                                                 and apg.permission_name = 'ADMIN'))
    left join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = per.rsf_pfcbl_id
    left join lateral (select roles.role_name,roles.role_permissions
                       from users.roles
    									 where roles.role_permissions & per.granted = roles.role_permissions
    									 order by roles.role_permissions desc
    									 limit 1) as role on true
    left join lateral (select 'CUSTOM: ' || array_to_string(array_agg(permission_name order by permission_value asc),', ') permissions_granted 
                       from users.permission_types	pt
                       where pt.permission_value & (per.granted & ~ coalesce(role.role_permissions,0)) = pt.permission_value) as custom on true
    where vai.is_system_account = false
      -- I have ADMIN rsf_pfcbl_id permissions
      
    order by 
      vai.users_name,
      per.rsf_pfcbl_id is NULL desc,
      per.rsf_pfcbl_id is not distinct from 0 desc,
      per.sys_name
  ",params=list(USER_ID()))
  setDT(user_data)
  return(user_data)
  
},ignoreInit = FALSE, ignoreNULL = FALSE)

SERVER_ADMIN_USERS_LIST_FILTERED <- eventReactive(c(SERVER_ADMIN_USERS_LIST(),
                                                    input$server_admin_users__filter_facility,
                                                    input$server_admin_users__filter_username), {

 ulist <- SERVER_ADMIN_USERS_LIST()                                                      
 if (isTruthy(input$server_admin_users__filter_username)) {
   ulist <- ulist[users_name %in% input$server_admin_users__filter_username]
 }
 
 if (isTruthy(input$server_admin_users__filter_facility)) {
   ulist <- ulist[rsf_name %in% input$server_admin_users__filter_facility]
 }

 
 return (ulist)
 
},ignoreNULL=FALSE)

SERVER_ADMIN_USERS_ENTITIES_LIST <- eventReactive(c(USER_PROGRAMS(),
                                                    SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST()), {
  
  if (!LOGGEDIN()) return (NULL)
  
  rsf_pfcbl_ids <- NULL
  if (!isTruthy(SELECTED_PROGRAM_ID())) {
    rsf_pfcbl_ids <- USER_PROGRAMS()$rsf_pfcbl_id
  } else {
    rsf_pfcbl_ids <- SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST()$rsf_pfcbl_id
  }
  
  entities <- DBPOOL %>% dbGetQuery("
    select x.account_id,x.rsf_pfcbl_id,x.sys_name,x.rsf_name from (
    select 
      fpg.account_id,
      fpg.rsf_pfcbl_id,
      sn.sys_name,
      nids.pfcbl_category || ':' || nids.rsf_full_name as rsf_name
    from users.view_rsf_pfcbl_id_family_permissions_granted fpg 
    inner join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.rsf_pfcbl_id = fpg.rsf_pfcbl_id
    inner join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = fpg.rsf_pfcbl_id
    where account_id =  $1::text 
    and fpg.rsf_pfcbl_id = any(select unnest(string_to_array($2::text,','))::int)
    and fpg.permission_name = 'ADMIN'
    
    union all
    
    select 
      apg.account_id,
      apg.rsf_pfcbl_id,
      apg.sys_name,
      apg.sys_name as rsf_name
    from users.view_account_permissions_granted apg 
    where account_id =  $1::text 
      and apg.rsf_pfcbl_id is null
      and apg.permission_name = 'ADMIN'
    ) x
    order by 
      x.rsf_pfcbl_id is NULL desc,
      x.rsf_pfcbl_id is not distinct from 0 desc,
      x.sys_name",
    params=list(USER_ID(),
                paste0(rsf_pfcbl_ids,collapse=",")))
  
  setDT(entities)
  

  return (entities)
})

observeEvent(LOGGEDIN(), {
  
  if (!LOGGEDIN()) {
    hideElement(id="ui_users")
    USER_ACCOUNT$JASON <- list()
  } else {
    
    sys_permissions <- DBPOOL %>% dbGetQuery("
    select
      apg.permission_name,
      apg.permission_value
    from users.view_account_permissions_granted apg
    where apg.account_id = $1::text
      and apg.rsf_pfcbl_id is NULL
      and apg.sys_name = 'SYSTEM'",
    params=list(USER_ID()))
    
    USER_ACCOUNT$JASON <- as.list(sys_permissions)
    
    any_admin <- DBPOOL %>% dbGetQuery("
      select exists(select * from users.permissions per 
                    where per.account_id = $1::text
                    and (per.granted::int & (select permission_value from users.permission_types where permission_name = 'ADMIN')::int) > 0)::bool as is_admin
    ",params=list(USER_ID()))
    
    if (any_admin$is_admin==TRUE) {
      showElement(id="ui_users")
      
    } else {
      hideElement(id="ui_users")
      
    }
  }
},ignoreInit = FALSE,ignoreNULL = FALSE)

observeEvent(input$server_admin_users__clear, {
  
  updateSelectizeInput(session=session,
                       inputId = "server_admin_users__filter_facility",
                       selected="")
  
  updateSelectizeInput(session=session,
                       inputId = "server_admin_users__filter_username",
                       selected="")
})

observeEvent(SERVER_ADMIN_USERS_LIST(), {
  
  updateSelectizeInput(session=session,
                       inputId = "server_admin_users__filter_username",
                       choices=c("",unique(SERVER_ADMIN_USERS_LIST()$users_name)),
                       selected=input$server_admin_users__filter_username)
},ignoreNULL=FALSE)

observeEvent(SERVER_ADMIN_USERS_ENTITIES_LIST(), {
  updateSelectizeInput(session=session,
                       inputId = "server_admin_users__filter_facility",
                       choices=c("",unique(SERVER_ADMIN_USERS_ENTITIES_LIST()$rsf_name)),
                       selected=input$server_admin_users__filter_facility)
},ignoreNULL=FALSE)

observeEvent(input$server_admin_users__permissions_set, { 
  role <- input$server_admin_users__permissions_role
  custom <- input$server_admin_users__permissions_custom
  
  who <- SERVER_ADMIN_USERS_LIST()[users_name %in% input$server_admin_users__filter_username]
  entity <- SERVER_ADMIN_USERS_ENTITIES_LIST()[rsf_name %in% input$server_admin_users__filter_facility]
  
  if (!isTruthy(role) && !isTruthy(custom)) {
    return(showNotification(type="error",
                            ui=h3("Either Role-based permissions OR Custom permissions (or both) must be selected.  To delete/remove permissions, select role 'NONE'")))
  }
  
  if (empty(who)) {
    return(showNotification(type="error",
                            ui=h3("Please select the name(s) of the individuals for whom to set permissions in the 'filter names' selection field")))  
  }
  
  if (empty(entity)) {
    return(showNotification(type="error",
                            ui=h3("Please selet the program or facility for which to set permissions for these users in the 'filter facility' selection field")))  
  }
  
  if (USER_ID() %in% who$account_id) {
    return(showNotification(type="error",
                            ui=h3("You may not modify your own permissions: Request changes by another system Manager")))
  }
  
  
  DBPOOL %>% dbExecute("
    with entity as (
    	  select 
          fpg.rsf_pfcbl_id,
          sn.sys_name
        from users.view_rsf_pfcbl_id_family_permissions_granted fpg 
        inner join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.rsf_pfcbl_id = fpg.rsf_pfcbl_id
        where account_id =  $1::text
        and fpg.rsf_pfcbl_id = $2::int
        and fpg.permission_name = 'ADMIN'
        
        union all
        
        select 
          apg.rsf_pfcbl_id,
          apg.sys_name
        from users.view_account_permissions_granted apg 
        where account_id = $1::text
          and apg.rsf_pfcbl_id is null
          and $2::int is NULL
          and apg.permission_name = 'ADMIN'
    ),
    permissions as (
      select bit_or(per.permission_value)  as permission_value
    	from (
    		select roles.role_permissions as permission_value
    		from users.roles 
    		where roles.role_name = NULLIF($4::text,'NA')
    		
    		union 
    		
    		select permission_types.permission_value
    		from users.permission_types
    		where permission_types.permission_name = any(select unnest(string_to_array($5::text,','))::text)
    
        union 
    
        select 0 as permission_value
    	) per
    )
    insert into users.permissions(account_id,rsf_pfcbl_id,sys_name,granted,notes)
    select 
    vai.account_id,
    entity.rsf_pfcbl_id,
    entity.sys_name,
    coalesce(permissions.permission_value,0) as granted,
    concat('Permissions set by ',
           (select users_name from  p_rsf.view_account_info where account_id = $1),
    			 ' on ',
    			 (select timeofday()::date)) as notes
    from p_rsf.view_account_info vai
    cross join entity
    cross join permissions
    where vai.account_id = any(select unnest(string_to_array($3,','))::text)
    on conflict(account_id,sys_name)
    do update
    set granted = excluded.granted,
        notes = excluded.notes;",
  params=list(USER_ID(),
              entity$rsf_pfcbl_id,
              paste0(who$account_id,collapse=","),
              role,
              paste0(custom,collapse=",")))
  
  SERVER_ADMIN_USERS_LIST.REFRESH(SERVER_ADMIN_USERS_LIST.REFRESH()+1)
  removeModal()
  
})

observeEvent(input$server_admin_users__permissions, { 
  
  who <- SERVER_ADMIN_USERS_LIST()[users_name %in% input$server_admin_users__filter_username]
  entity <- SERVER_ADMIN_USERS_ENTITIES_LIST()[rsf_name %in% input$server_admin_users__filter_facility]
  
  if (empty(who)) {
    return(showNotification(type="error",
                            ui=h3("Please select the name(s) of the individuals for whom to set permissions in the 'filter names' selection field")))  
  }
  
  if (empty(entity)) {
    return(showNotification(type="error",
                            ui=h3("Please selet the program or facility for which to set permissions for these users in the 'filter facility' selection field")))  
  }

  if (USER_ID() %in% who$account_id) {
    return(showNotification(type="error",
                            ui=h3("You may not modify your own permissions: Request changes by another system Manager")))
  }
  
  roles <- DBPOOL %>% dbGetQuery("select role_name from users.roles order by role_permissions asc")
  custom <- DBPOOL %>% dbGetQuery("select permission_name from users.permission_types order by permission_value asc")
  
  m <- modalDialog(id="server_admin_users__permissions_modal",
                   div(align="center",
                       div(style="background-color:white;padding:5px;height:200px;width:300px;",
                           align="left",
                           fluidRow(
                             column(12,
                                    selectizeInput(inputId="server_admin_users__permissions_role",
                                                   label="Set Role-Based Permissions",
                                                   choices=c("NONE",roles),
                                                   selected="",
                                                   multiple=FALSE))),
                           fluidRow(
                             column(12,
                                    selectizeInput(inputId="server_admin_users__permissions_custom",
                                                   label="Set Custom Permissions",
                                                   choices=c("",custom),
                                                   selected="",
                                                   multiple=FALSE)))
                       )),
                   
                   title=HTML("Set Permissions for Selected Users"),
                   easyClose = FALSE,
                   footer=div(style="display:inline-block;width:100%;",
                              div(style="display:inline-block;float:left;",
                                  modalButton("Cancel")),
                              div(style="display:inline-block;float:right;",
                                  actionButton(inputId="server_admin_users__permissions_set",
                                               label="Submit",
                                               class="btn-success"))),
                   size="s")
  showModal(m)
    
})

observeEvent(input$server_admin_users__create_user, {
  

  any_admin <- DBPOOL %>% dbGetQuery("
      select exists(select * from users.permissions per 
                    where per.account_id = $1::text
                    and (per.granted::int & (select permission_value from users.permission_types where permission_name = 'ADMIN')::int) > 0)::bool as is_admin
    ",params=list(USER_ID()))
  
  if (!isTruthy(any_admin$is_admin)) return (NULL)
  
  global_roles <- DBPOOL %>% dbGetQuery("
  select arg.role_name
    from users.view_account_roles_granted arg
    where arg.rsf_pfcbl_id =0 
      and arg.account_id = $1::text
    order by arg.role_permissions asc",
  params=list(USER_ID()))
  
  global_roles <- global_roles$role_name
  global_selected <- "NONE"
  if (length(global_roles) > 0) {
    global_selected <- global_roles[1]
  }
  global_roles <- c("NONE",global_roles)
  
  m <- modalDialog(id="server_admin_users__create_user_modal",
                   div(align="center",
                   div(style="background-color:white;padding:5px;height:275px;width:450px;",
                       align="left",
                       fluidRow(
                         column(12,
                                textInput(inputId="server_admin_users__create_user_name",
                                          label="User Name",
                                          placeholder="User's First & Last Name"))),
                       fluidRow(
                         column(12,
                                textInput(inputId="server_admin_users__create_user_email",
                                          label="Email Address",
                                          placeholder="Email is account login"))),
                       fluidRow(
                         column(12,
                                selectizeInput(inputId="server_admin_users__create_user_global_role",
                                               label="Global Permissions",
                                               choices=global_roles,
                                               selected=global_selected,
                                               multiple=FALSE)))
                       )),
                   
                   title=HTML("Create New RSF Jason User"),
                   easyClose = FALSE,
                   footer=div(style="display:inline-block;width:100%;",
                              div(style="display:inline-block;float:left;",
                                  modalButton("Cancel")),
                              div(style="display:inline-block;float:right;",
                                  actionButton(inputId="server_admin_users__create_action",
                                               label="Submit",
                                               class="btn-success"))),
                   size="s")
  showModal(m)
})

observeEvent(input$server_admin_users__create_action, {
  
  name <- trimws(input$server_admin_users__create_user_name)
  email <- trimws(input$server_admin_users__create_user_email)
  grole <- input$server_admin_users__create_user_global_role
  
  if (!isTruthy(name) || 
      !isTruthy(email)) {
    return(showNotification(type="error",
                            ui=h3("Name and email address must be completed")))
  }
  
  new_account_id <- tryCatch({
    new_account_id <- DBPOOL_APPLICATIONS %>% dbGetQuery("
    select * 
    from arlapplications.accounts_create(v_application_hashid => $1::text,
                                         v_request_by_account_id => $2::text,
                                         v_name => $3::text,
                                         v_login => $4::text)",
                                                         
    params=list(RSF_MANAGEMENT_APPLICATION_ID,
                USER_ID(),
                name,
                email))
    
    new_account_id <- unlist(new_account_id)
    
    reset_code <- db_user_reset_password(pool=DBPOOL_APPLICATIONS,
                                         application_hashid=RSF_MANAGEMENT_APPLICATION_ID,
                                         sysadmin_id=ACCOUNT_SYS_ADMIN$account_id,
                                         username=email)
    
    email <- div(p(paste0("Dear ",tools::toTitleCase(name),",")),
                 p("Your ",tags$a(href="https://datanalytics-int.worldbank.org/rsf-prod/","RSF Jason")," account has been created."),
                 p("You username is your email: ",email),
                 p("Please use the link above to login for the first time and complete setting up your account with this temporary password: ",reset_code$reset_password),
                 p("First Time Account Creation Steps"),
                 p("1: Click the ",tags$a(href="https://datanalytics-int.worldbank.org/rsf-prod/","RSF Jason")," link"),
                 p("2: Enter your email address '",email,"' in the username field."),
                 p("3: Enter your temporary password '",reset_code$reset_password,"' in the password field"),
                 p("4: Click the Login button -> You will be redirected to change your temporary password"),
                 p("5: Enter your temporary password '",reset_code$reset_password,"' in the tempoerary password field."),
                 p("6: Enter your own personalized (memoarable and secure) password in the New Password field."),
                 p("7: Again re-enter your own personalized password in the Verify Password field"),
                 p("8: Click the Login button -> You will now be logged into the Jason system"))
    
    user_send_email(pool=DBPOOL_APPLICATIONS,
                    to=reset_code$login_email,
                    subject="RSF JASON | password reset",
                    html=email)
    
    if (isTruthy(grole)) {
      
      DBPOOL %>% dbExecute("
        insert into users.permissions(account_id,rsf_pfcbl_id,sys_name,granted,notes)
        select 
          $1::text as account_id,
          sn.rsf_pfcbl_id,
          sn.sys_name,
          roles.role_permissions,
          'Granted at account creation by ' || $3::text
        from
        p_rsf.view_rsf_pfcbl_id_current_sys_names sn, 
        users.roles
        where sn.rsf_pfcbl_id = 0
          and roles.role_name = $2::text",
      params=list(new_account_id,
                  toupper(grole),
                  USER_NAME()))
      
    }
  },
  error = function(e) {
    showNotification(type="error",
                     ui=h3(conditionMessage(e)))
    NULL
  },
  warning = function(w) {
    showNotification(type="error",
                     ui=h3(conditionMessage(w)))
    NULL
  })
  
  SERVER_ADMIN_USERS_LIST.REFRESH(SERVER_ADMIN_USERS_LIST.REFRESH()+1)
  removeModal()
  
})

output$server_admin_users__users_table <- DT::renderDataTable({
  
  req(SERVER_ADMIN_USERS_LIST_FILTERED())

  udata <- SERVER_ADMIN_USERS_LIST_FILTERED()
  udata <- udata[,.(`Name`=users_name,
                    `Email`=users_login,
                    `For`=rsf_name,
                    `Permissions`=permissions_text)]
  
  dd <- DT::datatable(udata,
                      editable = FALSE,
                      rownames = FALSE,
                      fillContainer=TRUE,
                      selection = 'single',
                      class = "display",
                      escape = FALSE, #Shouldn't be any HTML escapable text
                      options=list(dom="tirp",
                                   scrollX="auto",
                                   scrollY="70vh",
                                   bSort=F,
                                   paging=TRUE,
                                   ordering=F,
                                   rowCallback = NULL,
                                   pageLength=100,
                                   columnDefs = list(list(className = 'dt-left', targets = "_all")))
  )
  return (dd)
})