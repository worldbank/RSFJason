#rsf_program_id may be NA where template encodes program_id (such as system reports); but traditional templates require a user-defined input value
template_process <- function(pool,
                             template,
                             status_message=function(...) {}) {
  t1 <- Sys.time()
  status_message(class="info",paste0("Parsing template: ",template$template_source," for reporting date ",template$reporting_asof_date,"\n"))
  
  if (!any(names(template)=="reporting_import")) {
    stop("Template must define a reporting_import (this will be created in a call first to template_parse_file")
  }
 
  #RSF ID vs PFCBL ID template matching
  {
    #Set defined rsf_pfcbl_ids
    {
     
      template_match_data <- unique(template$template_data[!is.na(data_category), #can arise from non-matched indicators
                                                           .(reporting_template_row_group,
                                                             pfcbl_category=data_category)])
      template_match_data[,
                          `:=`(rsf_pfcbl_id=as.numeric(NA),
                               parent_rsf_pfcbl_id=as.numeric(NA),
                               match_action=as.character(NA),
                               match_issues=as.character(NA),
                               matched_by=as.character(NA))]
                               
                               
      
      pfcbl_ranks <- dbGetQuery(pool,"select pfcbl_category,pfcbl_rank from p_rsf.rsf_pfcbl_categories")
      setDT(pfcbl_ranks)
      template_match_data[pfcbl_ranks,
                          pfcbl_rank:=i.pfcbl_rank,
                          on=.(pfcbl_category)]
      
      required_categories <- unique(template_match_data[,.(pfcbl_category,
                                                           pfcbl_rank)])

      # Any QR template will need to include the inclusion rank of loans it is adding or reporting on.  Since all facilities have one parent, if we are given the 
      # inclusion rank, we can reverse lookup all parent IDs without querying each's ID number and determining if it's parent is new or not.
      loan_inclusion_ranks <- template$template_data[indicator_id==template$rsf_indicators[indicator_sys_category=="rank_id" & 
                                                                                           data_category=="loan",
                                                                                           indicator_id],
                                                     .(reporting_template_row_group,
                                                       rank_id=suppressWarnings(as.numeric(data_value)))][!is.na(rank_id)]
      if (!empty(loan_inclusion_ranks)) {
        
        #removed this:
        #uploading the IDs takes time and most templates include all ranks, so will always need to download all before the given date 
        # and nids.rank_id = any(select unnest(string_to_array($3::text,','))::text)",
        pfcbl_inclusion_ranks <- dbGetQuery(pool,"
          select distinct -- distinct necessary for loans that change their IDs over time: may have differt nids entries by reporting_asof_date, but this info is discarded; hence distinct
            nids.rank_id,
            ids.rsf_program_id,
            ids.rsf_facility_id,
            ids.rsf_client_id,
            ids.rsf_borrower_id,
            ids.rsf_loan_id
          from p_rsf.view_rsf_pfcbl_id_family_tree ft
          inner join p_rsf.rsf_data_current_names_and_ids nids on nids.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
          inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
          where ft.from_rsf_pfcbl_id = $1::int
            and ft.to_pfcbl_category = 'loan'
            and nids.reporting_asof_date <= $2::date",
           
        params=list(template$reporting_import$import_rsf_pfcbl_id,
                    template$reporting_import$reporting_asof_date))
        
        setDT(pfcbl_inclusion_ranks)
        
        multi_ranks <- pfcbl_inclusion_ranks[,n:=.N,by=.(rank_id)][n>1]
        if (!empty(multi_ranks)) {
          
          
          multi_ranks <- dbGetQuery(pool,"
          select 
            nids.rank_id,
            nids.sys_name,
            nids.reporting_asof_date::text
          from p_rsf.view_rsf_pfcbl_id_family_tree ft
          inner join p_rsf.rsf_data_current_names_and_ids nids on nids.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
          inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
          where ft.from_rsf_pfcbl_id = $1::int
            and ft.to_pfcbl_category = 'loan'
            and nids.reporting_asof_date <= $2::date
            and nids.rank_id = any(select unnest(string_to_array($3::text,','))::text)",
            params=list(template$reporting_import$import_rsf_pfcbl_id,
                        template$reporting_import$reporting_asof_date,
                        paste0(unique(multi_ranks$rank_id),collapse=",")))
          setorder(multi_ranks,
                   rank_id,
                   reporting_asof_date)
          
          message <- paste0(paste0("Rank #",multi_ranks$rank_id," assigned in [",multi_ranks$reporting_asof_date,"] TO: ",multi_ranks$sys_name),collapse="\n ")
          stop(paste0("Inclusion rank ID has been assigned to multiple different loans. \n",
          "\n",message,"\n\nThis must be corrected in historical datasets. Delete old QRs and re-uploaded corrected QRs with unique ranks per included facility"))
        }
        
        pfcbl_inclusion_ranks[,n:=NULL]

        pfcbl_inclusion_ranks[,rank_id:=as.numeric(rank_id)]
        
        pfcbl_inclusion_ranks <- loan_inclusion_ranks[pfcbl_inclusion_ranks,
                                                     on=.(rank_id),
                                                     nomatch=NULL]
        
        pfcbl_inclusion_ranks <- template_match_data[,
                                                    .(reporting_template_row_group,pfcbl_category)
                                                    ][pfcbl_inclusion_ranks,
                                                      on=.(reporting_template_row_group),
                                                      nomatch=NULL]
        pfcbl_inclusion_ranks[,
                             `:=`(rsf_pfcbl_id=fcase(pfcbl_category=="loan",rsf_loan_id,
                                                     pfcbl_category=="borrower",rsf_borrower_id,
                                                     pfcbl_category=="client",rsf_client_id,
                                                     pfcbl_category=="facility",rsf_facility_id,
                                                     pfcbl_category=="program",rsf_program_id,
                                                     default=NA),
                                  parent_rsf_pfcbl_id=fcase(pfcbl_category=="loan",rsf_borrower_id,
                                                            pfcbl_category=="borrower",rsf_client_id,
                                                            pfcbl_category=="client",rsf_facility_id,
                                                            pfcbl_category=="facility",rsf_program_id,
                                                            default=NA))]
        
        pfcbl_inclusion_ranks <- pfcbl_inclusion_ranks[,
                                                       .(reporting_template_row_group,
                                                        pfcbl_category,
                                                        rsf_pfcbl_id,
                                                        parent_rsf_pfcbl_id)]
        
        template_match_data[pfcbl_inclusion_ranks,
                            `:=`(rsf_pfcbl_id=i.rsf_pfcbl_id,
                                 parent_rsf_pfcbl_id=i.parent_rsf_pfcbl_id,
                                 match_action="update",
                                 matched_by="defined"),
                            on=.(reporting_template_row_group,
                                 pfcbl_category)]
        
      }
      
      if (!empty(template_match_data[is.na(rsf_pfcbl_id) & pfcbl_rank <= template$reporting_import$import_pfcbl_rank])) {
        import_family_tree <- dbGetQuery(pool,"
          select 
            ft.to_family_rsf_pfcbl_id as rsf_pfcbl_id,
            ft.to_pfcbl_category as pfcbl_category,
            ft.to_pfcbl_rank as pfcbl_rank,
            lag(ft.to_family_rsf_pfcbl_id) over(order by ft.to_pfcbl_rank) as parent_rsf_pfcbl_id
          from p_rsf.view_rsf_pfcbl_id_family_tree ft
          where ft.from_rsf_pfcbl_id = $1::int 
            and ft.pfcbl_hierarchy <> 'child'",
          params=list(template$reporting_import$import_rsf_pfcbl_id))
        setDT(import_family_tree)
        import_family_tree[,joincondition:=as.numeric(NA)]

        template_match_data[import_family_tree,
                            `:=`(rsf_pfcbl_id=i.rsf_pfcbl_id,
                                 parent_rsf_pfcbl_id=i.parent_rsf_pfcbl_id,
                                 match_action="update",
                                 matched_by="defined"),
                            on=.(rsf_pfcbl_id=joincondition,
                                 pfcbl_category)]
        
      }
      
      #Since there is a very reliable relationship that one facility has one (and only one) client, and in the absence of client ID/Name info, assume
      #that the client rsf_pfcbl_id is the facility's only child client entity
      if (!empty(template_match_data[is.na(rsf_pfcbl_id) & pfcbl_category=="client" & template$reporting_import$import_pfcbl_category=="facility"]) &&
          empty(template$template_data[data_category=="client" & indicator_sys_category %in% c("id","name")])) {

        import_family_tree <- dbGetQuery(pool,"
          select 
            ft.to_family_rsf_pfcbl_id as rsf_pfcbl_id,
            ft.to_pfcbl_category as pfcbl_category,
            ft.to_pfcbl_rank as pfcbl_rank,
            ft.from_rsf_pfcbl_id as parent_rsf_pfcbl_id
          from p_rsf.view_rsf_pfcbl_id_family_tree ft
          where ft.from_rsf_pfcbl_id = $1::int 
            and ft.to_pfcbl_category = 'client'",
        params=list(template$reporting_import$import_rsf_pfcbl_id))
        setDT(import_family_tree)
        
        if (nrow(import_family_tree) != 1) stop("No client ID or Name parameters in template data and this facility has multiple clients")
        
        import_family_tree[,joincondition:=as.numeric(NA)]
        
        template_match_data[import_family_tree,
                            `:=`(rsf_pfcbl_id=i.rsf_pfcbl_id,
                                 parent_rsf_pfcbl_id=i.parent_rsf_pfcbl_id,
                                 match_action="update",
                                 matched_by="defined"),
                            on=.(rsf_pfcbl_id=joincondition,
                                 pfcbl_category)]
        
      }      
      if (!empty(template_match_data[is.na(parent_rsf_pfcbl_id) & pfcbl_rank == (template$reporting_import$import_pfcbl_rank+1)])) {

        
        template_match_data[template$reporting_import[,.(parent_rsf_pfcbl_id=import_rsf_pfcbl_id,
                                                         joincondition=as.numeric(NA),
                                                         pfcbl_rank=import_pfcbl_rank+1)],
                            `:=`(parent_rsf_pfcbl_id=i.parent_rsf_pfcbl_id),
                            on=.(rsf_pfcbl_id=joincondition,
                                 pfcbl_rank)]
        
      }
      #March 2026: Removed because child-to-parent IDs from the inclusion rank is superior to guessing at parent-to-child where only one child exists
      #and due to error creating new entities when program has only one facilitiy, that creatined 2nd facility failed.  So instead of fixing or adding cases, just removed.
    
      #Template QRs might specifiy the facility ID versus the client ID
      #While the system enables a one-to-many relationship for facilities to have multiple clients, IFC business does not do this on the
      #investment side (it does on the advisory side).  But since RSFs are an IS/Upstream product, almost certainly a facility ID will have
      #only one client ID and therefore the client ID may be inferred, hence pfcbl_members=1
      #Note that if template defines facility_id and doesn't upload any client-level indicators then this could create an issue.
      #Presently, all QR templates upload client-level data
      # defined_ids <- dbGetQuery(pool,"
      #                           with defined_ids as (
      #                             select 
      #                               ft.to_family_rsf_pfcbl_id as rsf_pfcbl_id,
      #                             	ft.to_pfcbl_category as pfcbl_category,
      #                             	ft.to_pfcbl_rank,
      #                             	count(*) over(partition by ft.to_pfcbl_category) as pfcbl_members
      #                             from p_rsf.view_rsf_pfcbl_id_family_tree ft 
      #                             where ft.from_rsf_pfcbl_id = $1::int
      #                             )
      #                             select 
      #                             	ids.rsf_pfcbl_id,
      #                             	ids.pfcbl_category,
      #                             	case when ids.pfcbl_category = 'program' then 0
      #                             	     when ids.pfcbl_category = 'facility' then ids.rsf_program_id
      #                             	     when ids.pfcbl_category = 'client' then ids.rsf_facility_id
      #                             	     when ids.pfcbl_category = 'borrower' then ids.rsf_client_id
      #                             	     when ids.pfcbl_category = 'loan' then ids.rsf_borrower_id
      #                             	     else NULL::int end as parent_rsf_pfcbl_id 
      #                             from defined_ids di
      #                             inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = di.rsf_pfcbl_id
      #                             where di.pfcbl_members = 1
      #                               and di.pfcbl_category = any(select unnest(string_to_array($2::text,','))::text)
      #                               and di.pfcbl_category in ('global','program','facility','client')",
      #                           params=list(template$reporting_import$import_rsf_pfcbl_id,
      #                                       paste0(required_categories$pfcbl_category,collapse=",")))
      # setDT(defined_ids)
      # 
      # if (!empty(defined_ids)) {
      #   defined_ids[,joincondition:=as.numeric(NA)]
      #   template_match_data[defined_ids,
      #                       `:=`(rsf_pfcbl_id=i.rsf_pfcbl_id,
      #                            parent_rsf_pfcbl_id=i.parent_rsf_pfcbl_id,
      #                            match_action="update",
      #                            matched_by="defined"),
      #                       on=.(pfcbl_category,
      #                            rsf_pfcbl_id=joincondition)]
      # }
      
      bad_matches <- template_match_data[!is.na(rsf_pfcbl_id)][,.(n=length(unique(rsf_pfcbl_id))),by=.(reporting_template_row_group,pfcbl_category)][n>1]
      if (!empty(bad_matches)) {
        stop("Bad matches:\n ",paste0("For ",paste0(bad_matches$pfcbl_category," at ",bad_matches$reporting_template_row_group),collapse=" \n"))
      }
      
      template_match_data[,row_num:=as.numeric(gsub("^(\\d+).*$","\\1",reporting_template_row_group))]
      
      setcolorder(template_match_data,
                  neworder=c("reporting_template_row_group",
                             "parent_rsf_pfcbl_id",
                             "rsf_pfcbl_id",
                             "pfcbl_category",
                             "pfcbl_rank",
                             "match_action",
                             "match_issues",
                             "matched_by"))
      
      setorder(template_match_data,
               pfcbl_rank,
               row_num)
      
      template_match_data[,row_num:=NULL]
      
      template$match_results <- template_match_data
      template_match_data <- NULL
    }
    
    if (any(names(template$template_data)=="SYSID",na.rm=T)) {
      template_data_ids <- na.omit(template$template_data[SYSID>0,unique(SYSID)])
      
      if (length(template_data_ids) > 0 &&
          !all(template$reporting_import$import_rsf_pfcbl_id %in% template_data_ids)) {
        
        #This is an error check -- we want it to be empty.
        family_match = dbGetQuery(pool,"
                                  select unnest($2::int[]) as rsf_pfcbl_id
                                  
                                  except
                                  
                                  select ft.to_family_rsf_pfcbl_id
                                  from p_rsf.view_rsf_pfcbl_id_family_tree ft 
                                  where ft.from_rsf_pfcbl_id = $1::int
                                    and ft.pfcbl_hierarchy <> 'parent'",
                                  params=list(template$reporting_import$import_rsf_pfcbl_id,
                                              dbMakeIntArray(template_data_ids)))
        
        if (!empty(family_match)) {
          stop(paste0("Malformed template: for pfcbl_id templates SYSIDs must all be members of the RSF_REPORTING_ENTITY: but the follow SYSIDs are not child entites: ",
                      paste0(unlist(family_match),collapse=", ")))
        }
      }
      
      if (anyNA(template$template_data$SYSID)) {
        stop("NA values are not allowed in template_data$SYSID: New entities must be explicitly defined using a negative number as the SYSID")
      } else if (any(template$template_data$SYSID < 0,na.rm=T)) {
        template$match_results[unique(template$template_data[SYSID < 0,
                                                             .(reporting_template_row_group,
                                                               rsf_pfcbl_id=as.numeric(NA),  #join on undefined ONLY
                                                               SYSID,
                                                               pfcbl_category=data_category)]),
                               `:=`(rsf_pfcbl_id=SYSID,
                                    match_action="new",
                                    matched_by="Negative SYSID"),
                               on=.(reporting_template_row_group,
                                    pfcbl_category,
                                    rsf_pfcbl_id)]
      }
      
      template <- template_set_data_match_pfcbl_ids(pool=pool,
                                                    template=template) #function also adds to template: template$match_results
    }
    
    
    if (anyNA(template$match_results$rsf_pfcbl_id) | any(template$match_results$rsf_pfcbl_id < 0,na.rm=T)) {
      status_message(class="none","Matching IDs and creating new entries\n")
      
      template <- template_set_data_match_rsf_ids(pool=pool,
                                                  template=template) #function also adds to template: template$match_results
      
    }
    
    if (anyNA(template$match_results$rsf_pfcbl_id) | any(template$match_results$rsf_pfcbl_id < 0,na.rm=T)) {
      stop("Failed to match or create entity IDs")
    }
    
    
    #template <- readRDS("template.RDS")
    #saveRDS(template,"template.RDS")
    #lobstr::obj_size(template) #OBA: 1.16GB
    template$template_data[template$match_results,
                           rsf_pfcbl_id:=i.rsf_pfcbl_id,
                           on=.(reporting_template_row_group,
                                data_category=pfcbl_category)]
    
    #unmatched indicators will of course have NA rsf_pfcbl_ids since we can't match on the data_category of an unknown indicator.
    #these will be filtered out and flagged later.  Here, check that known indicators have known rsf_pfcbl_ids
    if(anyNA(template$template_data[!is.na(indicator_id),unique(rsf_pfcbl_id)])) stop("Failed to match rsf_pfcbl_id to reporting_template_row_group and data_category")
    
    names(template)[which(names(template)=="template_data")] <- "pfcbl_data"
    
  }
  
  #################
  #Setup templates! Enter Name and ID information first to create SYSNAME and load subscription settings before upload and indicator checks.
  #################
  
  {
    if (!is.null(template$template_settings$template_is_setup) && 
        template$template_settings$template_is_setup==TRUE) {
      
      status_message(class="info",paste0("Setup template:\n"))
      
      # setup_rsf_program_id <- dbGetQuery(pool,"select rsf_program_id from p_rsf.rsf_pfcbl_ids ids where ids.rsf_pfcbl_id = $1::int",
      #                                    params=list(template$reporting_import$import_rsf_pfcbl_id))
      # 
      # setup_rsf_program_id <- unlist(setup_rsf_program_id,use.names = F)
      
      
      if (any(template$match_results$match_action=="new",na.rm=T)) {
        status_message(class="info",paste0(" - Setup SYSNAMES\n"))  
        
        create_sysnames <- template$pfcbl_data[rsf_pfcbl_id %in% template$match_results[match_action=="new",rsf_pfcbl_id] &
                                               indicator_id  %in% template$rsf_indicators[indicator_sys_category %in% c("name","nickname","id","rank_id","tranche_id"),indicator_id]]
        
        create_sysnames <- db_add_update_data_user(pool=pool,
                                                   import_id=template$reporting_import$import_id,
                                                   upload_data=create_sysnames,
                                                   upload_user_id=template$reporting_user_id,
                                                   rsf_indicators=template$rsf_indicators)
      }
    }
    
  }
  
  #This helps to resolve reporting flags onto correct pfcbl_categories/entities for indicator_not_found which we don't know how to map because we don't know what indicator
  #it is to know what pfcbl_category to map it to.
  if (!any(is.na(template$match_results$rsf_pfcbl_id),na.rm=T) &&
      length(unique(template$match_results$rsf_pfcbl_id))==1 &&
      anyNA(template$pfcbl_data$rsf_pfcbl_id)) { 
    
    template$pfcbl_data[is.na(rsf_pfcbl_id),
                        rsf_pfcbl_id:=unique(template$match_results$rsf_pfcbl_id)]
    
  }
  #now that we know our rsf_pfcbl_ids we can unnest the data_flags and manage these separately.
  #now we know the rsf_pfcbl_id and it's associated with the specific data_rank that's been asigned
  pfcbl_data_flags <- {
      pfcbl_data_flags <- template$pfcbl_data[,
                                              unlist(data_flags_new,recursive = F),
                                              by=.(rsf_pfcbl_id,
                                                   indicator_id,
                                                   reporting_asof_date,
                                                   reporting_template_row_group,
                                                   reporting_template_data_rank)]
    
      if (empty(pfcbl_data_flags)) {
        pfcbl_data_flags[,
                         `:=`(check_name=character(),
                              check_message=character())]
      }
      pfcbl_data_flags <- unique(pfcbl_data_flags[,.(rsf_pfcbl_id,
                                                     indicator_id,
                                                     reporting_asof_date,
                                                     reporting_template_row_group,
                                                     reporting_template_data_rank,
                                                     check_name,
                                                     check_message)])
      if (any(names(template$pfcbl_data)=="data_flags_new")) template$pfcbl_data[,data_flags_new:=NULL]
    pfcbl_data_flags
  }
      
  #parse indicators
  #some templates will filter these out on read-in.  Others won't so double check.
  {
    
    #Note this includes ALL subscriptions across the family tree and will include global/program/facility/borrower, etc indicators
    indicator_subscriptions <- dbGetQuery(pool,"
      select 
        fis.rsf_pfcbl_id,
        fis.indicator_id,
        fis.indicator_name,
        fis.is_subscribed,
        fis.is_unsubscribed,
        fis.formula_id,
        fis.is_calculated,
        fis.data_type,
        fis.data_category,
        fis.default_unit as default_data_unit,
        fis.formula_calculation_unit,
        fis.unit_fx_indicator_id
      from p_rsf.view_rsf_setup_indicator_subscriptions fis
      where fis.rsf_pfcbl_id = $1::int",
    params=list(template$reporting_import$import_rsf_pfcbl_id))
    
    setDT(indicator_subscriptions)
    
    template$rsf_indicator_subscriptions <- indicator_subscriptions
    
    template$pfcbl_data[indicator_subscriptions,
                        is_unsubscribed:=i.is_unsubscribed,
                        on=.(indicator_id)]
    
    #if it's being uploaded, but neither subscribed nor unsubscribed, then we should subscribe: this happens in the data trigger
    #indicator_subscriptions[indicator_id %in% unique(template$pfcbl_data$indicator_id)][is_subscribed==FALSE & is_unsubscribed==FALSE]
    
    #see if unexpected formula/constant flags should be _removed_ on account of facility specific calculations setups
    #these could have been added in parse_data_formats()
    if (!empty(pfcbl_data_flags)) {
      pfcbl_data_flags[indicator_subscriptions,
                       `:=`(is_calculated=i.is_calculated,
                            unit_fx_indicator_id=i.unit_fx_indicator_id),
                       on=.(indicator_id)]
      
      pfcbl_data_flags[,
                       omit:=FALSE]
      
      pfcbl_data_flags[check_name=="sys_flag_unexpected_formula" & is_calculated==TRUE,
                       omit:=TRUE]
      
      pfcbl_data_flags[check_name=="sys_flag_unexpected_constant" & is_calculated==FALSE,
                       omit:=TRUE]
      
      #p_rsf.view_rsf_setup_indicator_subscriptions should now tag unit_fx_indicator_id set metrics as is_calculated=T
      #pfcbl_data_flags[check_name=="sys_flag_unexpected_formula" & is_calculated==TRUE & !is.na(unit_fx_indicator_id)]
      #pfcbl_data_flags[omit==T]
      pfcbl_data_flags <- pfcbl_data_flags[omit==FALSE]
      pfcbl_data_flags[,
                       `:=`(omit=NULL,
                            is_calculated=NULL)]
    }    
    
    
    template$pfcbl_data[,omit:=FALSE]
    template$pfcbl_data[is.na(indicator_id) |
                        is_unsubscribed==TRUE |
                        indicator_id %in% template$rsf_indicators[is_system==TRUE & #ie, omit system indicators (unless user is reporting on "is_active" status)
                                                                  indicator_sys_category != "is_active", #special system indicator: allows manual overwrite by users
                                                                  indicator_id],
                        omit:=TRUE]
    
    bad_indicators <- template$pfcbl_data[omit==TRUE]
    
    template$pfcbl_data <- template$pfcbl_data[omit==FALSE]
    template$pfcbl_data[,omit:=NULL]
    
    #This should be obsolete with the new header management?
    if (!empty(bad_indicators)) {
      
      bad_indicators[,
                     `:=`(row_num=gsub("[^[:digit:]]+","",reporting_template_row_group),
                          sheet_name=gsub("[[:digit:]]+","",reporting_template_row_group))]
      
      bad_headers <- bad_indicators[is.na(indicator_id)==TRUE]
      
      bad_indicators <- bad_indicators[is.na(indicator_id)==FALSE]
      #no need to flag if somehow a system indicator is being reported (it's probably via a system extract?)
      #just omit it.
      bad_indicators <- bad_indicators[!(indicator_id %in% template$rsf_indicators[is_system==TRUE,indicator_id])]

      if (!empty(bad_headers)) {
      
        for (ind in unique(bad_headers$indicator_name)) {
          status_message(class="error",paste0("Unknown Indicator: '",ind,"' does not exist.  Ignored.\n"))
        }
        
        
        bad_headers <- bad_headers[,
                                   .(message=paste0('[IN SHEET ',sheet_name,'] ',
                                                    ifelse(is.na(indicator_name),"Unknown Header",
                                                           paste0('"',indicator_name,'"'))," = ",
                                                    paste0(paste0('"',head(data_value,100),'"'),
                                                           collapse=' & '))),
                                   by=.(sheet_name,indicator_name,indicator_id,rsf_pfcbl_id)]
        
        bad_headers[,reporting_asof_date:=template$reporting_import$reporting_asof_date]

        template$pfcbl_reporting_flags <- rbindlist(list(template$pfcbl_reporting_flags,
                                                         bad_headers[,.(rsf_pfcbl_id,
                                                                        indicator_id,
                                                                        reporting_asof_date=template$reporting_import$reporting_asof_date,
                                                                        check_name="sys_flag_indicator_not_found",
                                                                        check_message=paste0(message," \n",
                                                                                             "Fix in Jason: Setup Templates > ",template$template_name,"; or correct Excel header"))]))
      }

      bad_headers <- NULL
      
      unsubscribed_indicators <- bad_indicators[is_unsubscribed==TRUE,
                                                .(rsf_pfcbl_id,
                                                  indicator_id,
                                                  reporting_asof_date,
                                                  indicator_name,
                                                  data_value,
                                                  data_unit)]
      
      bad_indicators <- bad_indicators[is.na(is_unsubscribed) | is_unsubscribed==FALSE]
      if (!empty(bad_indicators)) {
        stop("Failed to process unrecognized indicators")
      }
      
      if (!empty(unsubscribed_indicators)) {
        unsubscribed_indicators[,
                                message:=paste0(indicator_name," is explicitly NOT MONITORED in RSF Program/Facility setup. Data NOT SAVED: ",
                                               ifelse(is.na(data_value),
                                                      "{MISSING}",
                                                      data_value),
                                               ifelse(is.na(data_unit),
                                                      "",
                                                      paste0(" ",data_unit)))]
        template$pfcbl_reporting_flags <- rbindlist(list(template$pfcbl_reporting_flags,
                                                         unsubscribed_indicators[,.(rsf_pfcbl_id,
                                                                                    indicator_id, #=as.numeric(NA), reverted to use this indicator_id, it creates some unexpected data in rsf_data_flags
                                                                                                  #with data_id to sys_X_reporting and the indicator_id to another entity/indicator, but it's ok.
                                                                                                  #as the forein key is on data_id
                                                                                    #indicator_id, #cannot put it on this indicator ID because this data point has been omitted and will never upload
                                                                                                  #must tag sys_X_reporting indicator!
                                                                                    reporting_asof_date,
                                                                                    check_name="sys_flag_indicator_not_monitored",
                                                                                    check_message=message)]))
      }
      unsubscribed_indicators <- NULL
    }
  }
  
  
  #Duplicates
  #Check manually reported currency ratios
  #This happens here and not in parse_data_formats because mannually reported ratios usually report a generic LCU fx rate, eg, USD/LCU
  #In which case, we need to know the entity and what it's LCU value is to process this.
  {
    #inverted fx ratios
    {
      #check if fx currency units are inverted and correct: eg, USD/EUR but user should have entered EUR/USD
      #do this before duplicates checks.
      #only facilities can report fx ratios.
      #Ratios variable is used below so whether empty or not empty, ensure same column names exist.
      ratios <- template$pfcbl_data[indicator_id %in% template$rsf_indicators[data_type=="currency_ratio",indicator_id]]
      
      
      if (!empty(ratios)) {
        
        ratios[,
               entity_local_currency_unit:=as.character(NA)]
        
        lcu <- template$pfcbl_data[indicator_id %in% template$rsf_indicators[indicator_sys_category=="entity_local_currency_unit",indicator_id]]
        ratios[lcu,
               entity_local_currency_unit:=i.data_value,
               on=.(rsf_pfcbl_id)]
        #Note that parse_data_formats() will invert LCU/USD to USD/LCU if it's reported contrary to the defult.
        if (anyNA(ratios$entity_local_currency_unit)) {
          lcu <- dbGetQuery(pool,"
                  select distinct on (lcu.for_rsf_pfcbl_id)
                  lcu.for_rsf_pfcbl_id as rsf_pfcbl_id,
                  lcu.data_unit_value
                  from p_rsf.rsf_data_current_lcu lcu
                  where for_rsf_pfcbl_id = any($1::int[])
                    and lcu.reporting_asof_date <= $2::date
                  order by 
                  lcu.for_rsf_pfcbl_id,
                  lcu.reporting_asof_date desc",
                            params=list(dbMakeIntArray(ratios[is.na(entity_local_currency_unit)==TRUE,unique(rsf_pfcbl_id)]),
                                        template$reporting_import$reporting_asof_date))
          setDT(lcu)
          ratios[lcu,
                 entity_local_currency_unit:=i.data_unit_value,
                 on=.(rsf_pfcbl_id)]
        }
        
        ratios <- ratios[,
                         .(rsf_pfcbl_id,
                           indicator_id,
                           reporting_asof_date,
                           entity_local_currency_unit,
                           data_value=as.numeric(data_value),
                           data_unit)]
        ratios[,
               `:=`(from_currency=gsub("^([A-Z]{3}).*$","\\1",data_unit),
                    to_currency=gsub("^.*([A-Z]{3})$","\\1",data_unit))]
        
        ratios[from_currency %in% c("LCU","LCY"),
               from_currency:=entity_local_currency_unit]
        
        ratios[to_currency %in% c("LCU","LCY"),
               to_currency:=entity_local_currency_unit]
        
        fx_lookup <- tryCatch({ 
                     db_data_get_fx_ratio(pool=pool,
                                          fx_lookup=ratios[,
                                                           .(rsf_pfcbl_id,
                                                             exchange_rate_date=reporting_asof_date,
                                                             to_currency,
                                                             from_currency)],
                                          create.indicators=TRUE, #Yes, create because these will be queried otherwise later for the same indicator when variance is checked.
                                          force.global=TRUE) #the whole point is to check against global rates
        },
        warning=function(w) {
          status_message(paste0("Failed to get FX Ratios: ",conditionMessage(w)))
          stop(paste0("Failed to get FX Ratios: ",conditionMessage(w)))
        },
        error=function(e) {
          status_message(paste0("Failed to get FX Ratios: ",conditionMessage(e)))
          stop(paste0("Failed to get FX Ratios: ",conditionMessage(e)))
        })
        
        ratios[fx_lookup,
               global_fx_rate:=as.numeric(i.exchange_rate),
               on=.(rsf_pfcbl_id,
                    reporting_asof_date=exchange_rate_date,
                    from_currency,
                    to_currency)]
        
        ratios[,inverse_reported:=mapply(function(x,g) {
          
          #fx rate of 1 means to currency equals from currency, ie, not an fx but a unity
          if (g==1) return (FALSE)
          
          which(abs(c(g,1/g) - x) == min(abs(x - c(g,1/g)))) == 2 #1 will be closest to position 1, not the inverse; 2 closest to invest
        },x=data_value,g=global_fx_rate)]
        
        invratios <- ratios[inverse_reported==TRUE]
        if (!empty(invratios)) {
          
          invratios[,
                    inverted_data_unit:=paste0(gsub("^.*([A-Z]{3})$","\\1",data_unit),"/",
                                               gsub("^([A-Z]{3}).*$","\\1",data_unit))]
          
          invratios[,
                    message:=paste0("Inverted currency ratio reported {",data_unit," ",data_value,"} ",
                                    "FX rate auto-corrected to: {",inverted_data_unit," ",data_value,"} ")]
          
          template$pfcbl_reporting_flags <- rbindlist(list(template$pfcbl_reporting_flags,
                                                           invratios[,
                                                                     .(rsf_pfcbl_id,
                                                                       indicator_id,
                                                                       reporting_asof_date,
                                                                       check_name="sys_flag_data_unit_auto_correction",
                                                                       check_message=message)]))
          ratios[invratios,
                 `:=`(data_unit=i.inverted_data_unit,
                      from_currency=to_currency,
                      to_currency=from_currency),
                 on=.(rsf_pfcbl_id,
                      indicator_id,
                      reporting_asof_date)]
          
          template$pfcbl_data[invratios,
                              data_unit:=i.inverted_data_unit,
                              on=.(rsf_pfcbl_id,
                                   indicator_id,
                                   reporting_asof_date)]
        }
        
        ratios <- unique(ratios[,.(rsf_pfcbl_id,
                                   indicator_id,
                                   reporting_asof_date,
                                   entity_local_currency_unit,
                                   data_value,
                                   data_unit,
                                   from_currency,
                                   to_currency,
                                   global_fx_rate)])
      
      
      } else {
        ratios <- ratios[,.(rsf_pfcbl_id,
                             indicator_id,
                             reporting_asof_date,
                             entity_local_currency_unit=character(0),
                             data_value,
                             data_unit,
                             from_currency=character(0),
                             to_currency=character(0),
                             global_fx_rate=logical(0))]
      }
    }
    
    #SET LCU where defined
    #If we're uploading the currency unit in this report, then it will change to this in the data base insert triggers.  So do it now: both to avoid the work and also to reconcile
    #unit_fx parameters that may also be present in this upload.
    #(and where it is not defined...presumably it must have been defined historically and therefore be present in the database)
    {
      currency_indicator_ids <- template$rsf_indicators[grepl("currency_unit",indicator_sys_category) & indicator_id %in% unique(template$pfcbl_data$indicator_id),
                                                        .(indicator_id,data_category)]
      
      template$pfcbl_data[template$rsf_indicators,
                          data_type:=i.data_type,
                          on=.(indicator_id)]
      
      reported_local_currency_units <- template$pfcbl_data[currency_indicator_ids,
                                                           .(reporting_template_row_group,
                                                             data_category,
                                                             indicator_id,
                                                             reporting_asof_date,
                                                             data_unit=data_value,
                                                             data_type="currency"),
                                                           on=.(indicator_id,
                                                                data_category)]
      
      template$pfcbl_data[reported_local_currency_units,
                          data_unit:="LCU",
                          on=.(reporting_template_row_group,
                               data_category,
                               reporting_asof_date,
                               data_unit,
                               data_type)]
    }

    #Remap FX defined indicators to the reported indicator IDs (based on the governing indicator_id having been mapped by template column name mapping and by the reported data's unit equal to the
    #defined unit of the fx_defined_indicator.
    #Note that when there is ambiguity with are defined indicator's unit is equal to the reporting entity's LCU currency, this will map (incorrectly/unintentionally) from the governing unit
    #reporting in LCU to a defiend currency metric.  This will be reconciled in the data upload as any time a defined currency unit is reported, it's required that the governing metric be present
    #in the reported data too.
    
    {
      
      template$pfcbl_data[template$rsf_indicator_subscriptions[!is.na(unit_fx_indicator_id) & 
                                                                 is_unsubscribed != TRUE,
                                                               .(fx_defined_indicator_id=indicator_id,
                                                                 fx_defined_indicator_name=indicator_name,
                                                                 data_unit=default_data_unit,
                                                                 governing_indicator_id=unit_fx_indicator_id)],
                          `:=`(indicator_id=i.fx_defined_indicator_id,
                               indicator_name=i.fx_defined_indicator_name),
                          on=.(indicator_id=governing_indicator_id,
                               data_unit)]
    }
    
    #duplicates per row should fail
    {
      #duplicates due to currency FX will already have been removed
      #now remove duplicates due to rounding issues
      template$pfcbl_data <- unique(template$pfcbl_data)
      
      template$pfcbl_data[,
                          ambiguous:=.N,
                          by=.(rsf_pfcbl_id,
                               indicator_id,
                               reporting_asof_date,
                               reporting_template_row_group)]
      
      if (any(template$pfcbl_data$ambiguous > 1)) {
        
        #template$pfcbl_data[redundancies > 1]
        template$pfcbl_data[ambiguous>1,duplicate_value:=ifelse(!is.na(data_value) & !is.na(suppressWarnings(as.numeric(data_value))),
                                                     as.character(round(suppressWarnings(as.numeric(data_value)),CALCULATIONS_ENVIRONMENT$SIG_DIGITS)),
                                                     data_value)]
        
        template$pfcbl_data[ambiguous>1 & !is.na(duplicate_value) & !is.na(data_unit),
                            duplicate_value:=paste0(data_value," ",data_unit)]
        
        template$pfcbl_data[ambiguous>1 & is.na(duplicate_value) & !is.na(data_unit),
                            duplicate_value:=paste0("{BLANK ",data_unit,"}")]
  
        template$pfcbl_data[ambiguous>1 & is.na(duplicate_value) & is.na(data_unit),
                            duplicate_value:="{MISSING}"]
  
        template$pfcbl_data[,omit:=FALSE]
        template$pfcbl_data[ambiguous>1,
                            omit:=(length(unique(duplicate_value))==1) & (1:.N)>1,
                            by=.(rsf_pfcbl_id,
                                 indicator_id,
                                 reporting_asof_date,
                                 reporting_template_row_group)] #duplicates by reporting_template_row_group mean the column itself is repeated multiple times in different ways
                                                                #within the same row group, keep the first redundancy and omit the following duplicates (due to rounding)
        
        #template$pfcbl_data[omit==T]
        template$pfcbl_data <- template$pfcbl_data[omit==FALSE]
        template$pfcbl_data[,
                            omit:=NULL]
        
        #so if we've passed currency reconciliation
        #and passed rounding reconciliation
        #then we really do have errors...
        template$pfcbl_data[,
                            ambiguous:=length(unique(duplicate_value)),
                            by=.(rsf_pfcbl_id,
                                 indicator_id,
                                 reporting_asof_date,
                                 reporting_template_row_group)]
        
        ambiguous_data <- template$pfcbl_data[ambiguous>1]
        if (!empty(ambiguous_data)) {
          
          ambiguous_data <- ambiguous_data[,
                                           .(duplicate_value,
                                             data_rank=seq_along(reporting_template_data_rank)),
                                           by=.(reporting_template_row_group,
                                                rsf_pfcbl_id,
                                                reporting_asof_date,
                                                indicator_id,
                                                indicator_name)]
          
          if (!empty(template$template_headers)) {
              
            headers <- template$template_headers[indicator_id %in% ambiguous_data$indicator_id,
                                                 .(labels=paste0(data_source_index," [",paste0(unique(label),collapse=" & "),"]")),
                                                 by=.(indicator_id,data_source_index)]
            headers[,
                    data_rank:=seq_along(unique(data_source_index)),
                    by=.(indicator_id)]
            
            ambiguous_data <- ambiguous_data[headers,
                                             on=.(indicator_id,
                                                  data_rank)]
          } else {
            ambiguous_data[,labels:=indicator_name]
          }
            
          ambiguous_data <- unique(ambiguous_data[,
                                .(indicator_name,rsf_pfcbl_id,reporting_asof_date,indicator_id,labels,duplicate_value)])
          
          ambiguous_data <- ambiguous_data[,
                                       .(
                                         check_name="sys_reporting_data_discarded",
                                         check_message=paste0("Ambiguous reporting: same indicator repeated on different rows with different value\n",
                                                        "Warning: The FIRST VALUE {",duplicate_value[1],"} will be saved for '",indicator_name,"'\n",
                                                        "And DISCARD ",paste0(duplicate_value[-1],collapse=" & "),"\n. This may result in errors.\n",
                                                        "Recommended: fix the template and/or ensure the header labels are correct. Or remap these labels in Template Setup.\n",
                                                        paste0(indicator_name," is ",duplicate_value," on ",labels,collapse=" \n"))),
                                       by=.(rsf_pfcbl_id,
                                            indicator_id,
                                            indicator_name,
                                            reporting_asof_date)]
          ambiguous_data[,indicator_name:=NULL]
          setcolorder(ambiguous_data,
                      neworder=names(template$pfcbl_reporting_flags))
          
            template$pfcbl_reporting_flags <- rbindlist(list(template$pfcbl_reporting_flags,
                                                             ambiguous_data))
            template$pfcbl_data[,omit:=FALSE]
            template$pfcbl_data[ambiguous_data,
                                ambiguous:=1,
                                on=.(rsf_pfcbl_id,indicator_id,reporting_asof_date)]
            template$pfcbl_data[ambiguous==1,ambiguous:=1:.N,
                                by=.(rsf_pfcbl_id,indicator_id,reporting_asof_date)]
            template$pfcbl_data[ambiguous>1,
                                omit:=TRUE]
            template$pfcbl_data <- template$pfcbl_data[omit==FALSE]
            template$pfcbl_data[,omit:=NULL]
        }
        ambiguous_data <- NULL          
        
        template$pfcbl_data[,
                            duplicate_value:=NULL]
      }
      template$pfcbl_data[,ambiguous:=NULL]
      
      template$pfcbl_data <- unique(template$pfcbl_data)
    }
    
    
    #Futures
    {
      futures <- template$pfcbl_data[indicator_sys_category=="entity_creation_date" & !is.na(data_value)
      ][ymd(data_value) > (today()-1) | 
          ymd(data_value) > reporting_asof_date]
      if (!empty(futures)) {
        
        #sys_reporting_future_date
        #max_future <- futures[data_value==max(data_value),paste0(unique(indicator_name)," ",unique(as.character(data_value)))]
        
        futures <- futures[,
                           .(rsf_pfcbl_id,
                             indicator_id,
                             reporting_asof_date,
                             check_name="sys_reporting_future_date",
                             check_message=paste0(toupper(data_category)," is reported to have been created in the future on ",
                                                  ymd(data_value),": ",
                                                  ymd(data_value) - reporting_asof_date," DAYS _after_ the reporting date ",reporting_asof_date,
                                                  ". Reporting cannot be based on future contractual commitments or predicted data."))]
        template$pfcbl_reporting_flags <- rbindlist(list(template$pfcbl_reporting_flags,
                                                         futures))
     }
      futures <- NULL
    }
  }
  
  #match action redundancies
  {
    
    {
      if (any(!template$match_results$match_action %in% c("unchanged","update","new"))) {
        stop(paste0("Invalid match results: ",paste0(unique(template$match_results$match_action),collapse=",")))
      }

      if (!empty(template$match_results[,
                                        .(any_all_new=any(match_action=="new") & !all(match_action=="new")),
                                        by=.(rsf_pfcbl_id)][any_all_new==TRUE])) {
        stop(paste0("If any entities are new, then all reporting rows must be new"))
      }

    }
    
    template <- template_set_redundancy_reporting(pool=pool,
                                                  indicator_subscriptions=indicator_subscriptions,
                                                  template=template)

    if (!empty(template$pfcbl_data)) {
      template$pfcbl_data[,n:=.N,
                          by=.(rsf_pfcbl_id,
                               indicator_id,
                               reporting_asof_date)]
      
      if (any(template$pfcbl_data$n>1,na.rm=T)) {
        
        #Unless it's for identical values.
        #Which happens if template LIST values equal SUMMARY values, which come up as redundancies across different sheets.
        template$pfcbl_data[n>1,
                    `:=`(x=length(unique(paste0(data_value,"",data_unit))),
                         i=(1:.N)[order(is.na(data_submitted),is.na(data_unit),is.na(data_value),nchar(data_submitted),nchar(data_value))]),
                    by=.(rsf_pfcbl_id,
                         indicator_id,
                         reporting_asof_date)]
        
        template$pfcbl_data[,omit:=FALSE]
        template$pfcbl_data[(n>1 & x==1 & i>1),
                            omit:=TRUE]
        
        template$pfcbl_data <- template$pfcbl_data[omit==FALSE]
        template$pfcbl_data[,
                            `:=`(x=NULL,
                                 i=NULL,
                                 omit=NULL)]
        
        template$pfcbl_data[,n:=.N,
                            by=.(rsf_pfcbl_id,
                                 indicator_id,
                                 reporting_asof_date)]
        
        
        if (any(template$pfcbl_data$n>1,na.rm=T)) { stop("Template contains duplicated data across different indicators or data sheets that cannot be removed") }
        template$pfcbl_data[,n:=NULL]
      }
    
      #If redundancies data ranks have been removed 
      if (!any(names(template$pfcbl_data)=="reporting_template_data_rank",na.rm=T)) stop("Failed to find column 'reporting_template_data_rank' in template$pfcbl_data")
  
      #if no data, then no data falgs.    
      pfcbl_data_flags <- pfcbl_data_flags[reporting_template_data_rank %in% template$pfcbl_data$reporting_template_data_rank]
      pfcbl_data_flags[,reporting_template_data_rank:=NULL]
      pfcbl_data_flags <- unique(pfcbl_data_flags)
      template$pfcbl_data_flags <- pfcbl_data_flags
    }
  }

  
  #validate formatting and column requirements
  {
    
    keep_cols <- c(
                   "rsf_pfcbl_id",
                   "indicator_id",
                   "reporting_asof_date",
                   "data_value",
                   "data_unit",
                   "data_submitted",
                   "reporting_template_row_group"
                   
                   #"reporting_rsf_pfcbl_id",
                   #"reporting_chronology_rank"
                   )
    
    remove_cols <- names(template$pfcbl_data)[!names(template$pfcbl_data) %in% keep_cols]
    if (length(remove_cols)) template$pfcbl_data[,(remove_cols):=NULL]
    
    # setcolorder(template$pfcbl_data,
    #             neworder=keep_cols)
    # 
    if (!all(unique(template$match_results$match_action) %in% c("new",         #identified as new based on input ID fields not already existing
                                                                "unchanged",   #matched hasvalues shows data is unchanged
                                                                "update"))) {
      stop(paste0("Match results match_action must be 1 of: new, unchanged, update, revert, omit.  Found: ",paste0(unique(template$match_results$match_action),collapse=", ")))
    }
    
    if (is.null(template$pfcbl_data)) {
      template$pfcbl_data <- data.table(reporting_template_row_group=character(0),
                                        reporting_asof_date=as.Date(numeric(0)),
                                        rsf_pfcbl_id=numeric(0),
                                        indicator_id=numeric(0),
                                        data_value=character(0),
                                        data_unit=character(0),
                                        data_submitted=character(0))
    }
    
    if (!all(c("reporting_template_row_group",
               "reporting_asof_date",
               "rsf_pfcbl_id",
               "indicator_id",
               "data_value",
               "data_unit",
               "data_submitted") %in% names(template$pfcbl_data))) {
      stop(paste0("Template pfcbl_data must define: reporting_template_row_group, reporting_asof_date, rsf_pfcbl_id, indicator_id, indicator_name, data_value, data_type, data_unit, data_flags, data_submitted"))
    } else {
      setcolorder(template$pfcbl_data,neworder = c("reporting_template_row_group",
                                                   "rsf_pfcbl_id",
                                                   "indicator_id",
                                                   "reporting_asof_date",
                                                   "data_value",
                                                   "data_unit",
                                                   "data_submitted"))
    }
  }
  
  template$process_time <- as.numeric(Sys.time()-t1,"secs")
  if(SYS_PRINT_TIMING) debugtime("template_process"," Process time: ",format(Sys.time()-t1))
  return (template)
}