#rsf_program_id may be NA where template encodes program_id (such as system reports); but traditional templates require a user-defined input value
template_process <- function(pool,
                             template,
                             status_message=function(...) {}) {
  t1 <- Sys.time()
  status_message(class="info",paste0("Parsing template: ",template$template_source," for reporting date ",template$reporting_asof_date,"\n"))
  
  if (!any(names(template)=="reporting_cohort")) {
    stop("Template must define a reporting_cohort (this will be created in a call first to template_parse_file")
  }
  
  #tp <- copy(template)
  #Indicator and data validations
  
  #cohorts can be created and uploaded at client, facility, program or global levels
  #but settings are at global/program and facility levels.  So get the closest one
  rsf_facilityist_id <- { 
    rsf_facilityist_id <- dbGetQuery(pool,"
      select to_family_rsf_pfcbl_id as rsf_pfcbl_id
      from p_rsf.view_rsf_pfcbl_id_family_tree ft 
      where ft.from_rsf_pfcbl_id = $1::int 
        and ft.to_pfcbl_rank <= ft.from_pfcbl_rank
        and ft.to_pfcbl_rank <= 2 -- facility
      order by ft.to_pfcbl_rank desc
      limit 1",
      params=list(template$reporting_cohort$reporting_rsf_pfcbl_id))
   rsf_facilityist_id$rsf_pfcbl_id
  }
  
  #Upload input file for archiving and logging before any data are inserted.
  {
    status_message(class="info","\nSaving backup:",basename(template$template_file),"\n"); 
    
    t2 <- Sys.time()
    uploaded <- db_cohort_upload_file(pool=pool,
                                      file_path=template$template_file,
                                      reporting_cohort_id=template$reporting_cohort$reporting_cohort_id)
    
    if (!uploaded) {
      stop(paste0("Failed to save file ",paste0("'",basename(template$template_file),"'")))
    }
    template$backup_time <- Sys.time()-t2
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

      #Template QRs might specifiy the facility ID versus the client ID
      #While the system enables a one-to-many relationship for facilities to have multiple clients, IFC business does not do this on the
      #investment side (it does on the advisory side).  But since RSFs are an IS/Upstream product, almost certainly a facility ID will have
      #only one client ID and therefore the client ID may be inferred, hence pfcbl_members=1
      #Note that if template defines facility_id and doesn't upload any client-level indicators then this could create an issue.
      #Presently, all QR templates upload client-level data
      defined_ids <- dbGetQuery(pool,"
                                with defined_ids as (
                                  select 
                                    ft.to_family_rsf_pfcbl_id as rsf_pfcbl_id,
                                  	ft.to_pfcbl_category as pfcbl_category,
                                  	ft.to_pfcbl_rank,
                                  	count(*) over(partition by ft.to_pfcbl_category) as pfcbl_members
                                  from p_rsf.view_rsf_pfcbl_id_family_tree ft 
                                  where ft.from_rsf_pfcbl_id = $1::int
                                  )
                                  select 
                                  	ids.rsf_pfcbl_id,
                                  	ids.pfcbl_category,
                                  	ids.parent_rsf_pfcbl_id 
                                  from defined_ids di
                                  inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = di.rsf_pfcbl_id
                                  where di.pfcbl_members = 1
                                    and di.pfcbl_category = any(select unnest(string_to_array($2::text,','))::text)
                                    and di.pfcbl_category in ('global','program','facility','client')",
                                params=list(template$reporting_cohort$reporting_rsf_pfcbl_id,
                                            paste0(required_categories$pfcbl_category,collapse=",")))
      setDT(defined_ids)

      if (!empty(defined_ids)) {
        
          template_match_data[defined_ids,
                              `:=`(rsf_pfcbl_id=i.rsf_pfcbl_id,
                                   parent_rsf_pfcbl_id=i.parent_rsf_pfcbl_id,
                                   match_action="update",
                                   matched_by="defined"),
                              on=.(pfcbl_category)]
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
    
    if (template$template_ids_method=="rsf_id") {
      status_message(class="none","Matching IDs and creating new entries\n")
      
      template <- template_set_data_match_rsf_ids(pool=pool,
                                                  template=template) #function also adds to template: template$match_results

    
    } 
    
    else if (template$template_ids_method=="pfcbl_id") {
      template_data_ids <- na.omit(template$template_data[SYSID>0,unique(SYSID)])
      #browser()
      if (length(template_data_ids) > 0 &&
          !all(template$reporting_cohort$reporting_rsf_pfcbl_id %in% template_data_ids)) {
        family_match = dbGetQuery(pool,"
                                  select fam.parent_rsf_pfcbl_id,count(distinct child_rsf_pfcbl_id) as members
                                  from p_rsf.rsf_pfcbl_id_family fam
                                  where fam.child_rsf_pfcbl_id = any(select unnest(string_to_array($2,','))::int)
                                    and fam.parent_rsf_pfcbl_id = $1::int
                                  group by fam.parent_rsf_pfcbl_id",
                                  params=list(template$reporting_cohort$reporting_rsf_pfcbl_id,
                                              paste0(template_data_ids,collapse=",")))
        
        if (empty(family_match) || family_match$members != length(template_data_ids)) {
          stop(paste0("Malformed template: for pfcbl_id templates SYSIDs must all be members of the RSF_REPORTING_ENTITY: expected ",length(template_data_ids)," found ",family_match$members))
        }
      }
      
      if (anyNA(template$template_data$SYSID)) {
        stop("NA values are not allowed in template_data$SYSID: New entities must be explicitly defined using a negative number as the SYSID")
      } else if (any(template$template_data$SYSID < 0,na.rm=T)) {
        template$match_results[unique(template$template_data[SYSID < 0,
                                                             .(reporting_template_row_group,
                                                               pfcbl_category=data_category)]),
                               match_action:="new",
                               on=.(reporting_template_row_group,
                                    pfcbl_category)]
      }
      
      template <- template_set_data_match_pfcbl_ids(pool=pool,
                                                    template=template) #function also adds to template: template$match_results
    
      #Not Found!  So create it, matching rsf_id by input data
      if (anyNA(template$match_results$rsf_pfcbl_id)) {
        template <- template_set_data_match_rsf_ids(pool=pool,
                                                    template=template) #function also adds to template: template$match_results
      }
    }
    
    else {
      stop("Template must define rsf_id vs pfcbl_id matching")
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
    #also pulls in program-facility ID to ensure facility-level uploads
    
    # indicator_subscriptions <- dbGetQuery(pool,"
    #                                       select
    #                                       pis.rsf_pfcbl_id,
    #                                       pis.rsf_program_id,
    #                                       pis.rsf_facility_id,
    #                                       pis.indicator_id,
    #                                       pis.is_unsubscribed,
    #                                       pis.rsf_program_id,
    #                                       pis.has_program_entry,
    #                                       pis.formula_id,
    #                                       pis.is_calculated
    #                                       from p_rsf.view_rsf_pfcbl_indicator_subscriptions pis
    #                                       where pis.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)",
    #                                       params=list(paste0(unique(template$match_results$rsf_pfcbl_id),collapse=",")))
    # 
    indicator_subscriptions <- dbGetQuery(pool,"
      select 
        fis.rsf_pfcbl_id,
        fis.indicator_id,
        fis.is_subscribed,
        fis.is_unsubscribed,
        fis.formula_id,
        fis.is_calculated,
        fis.pfcbl_category_setting,
        fis.data_type,
        fis.data_category,
        fis.data_unit,
        fis.formula_calculation_unit
      from p_rsf.view_rsf_program_facility_indicator_subscriptions fis
      where fis.rsf_pfcbl_id = $1::int",
    params=list(rsf_facilityist_id))
    setDT(indicator_subscriptions)
    
    template$pfcbl_data[indicator_subscriptions,
                        is_unsubscribed:=i.is_unsubscribed,
                        on=.(indicator_id)]
    
    indicator_subscriptions[indicator_id %in% unique(template$pfcbl_data$indicator_id)][indicator_id==157392]
    indicator_subscriptions[indicator_id %in% unique(template$pfcbl_data$indicator_id)][pfcbl_category_setting=="global"]
    
    #if it's being uploaded, but neither subscribed nor unsubscribed, then we should subscribe
    indicator_subscriptions[indicator_id %in% unique(template$pfcbl_data$indicator_id)][is_subscribed==FALSE & is_unsubscribed==FALSE]
    
    #see if unexpected formula/constant flags should be _removed_ on account of facility specific calculations setups
    #these could have been added in parse_data_formats()
    if (!empty(pfcbl_data_flags)) {
      pfcbl_data_flags[indicator_subscriptions,
                       is_calculated:=i.is_calculated,
                       on=.(rsf_pfcbl_id,
                            indicator_id)]
      
      pfcbl_data_flags[,
                       omit:=FALSE]
      
      pfcbl_data_flags[check_name=="sys_flag_unexpected_formula" & is_calculated==TRUE,
                       omit:=TRUE]
      
      pfcbl_data_flags[check_name=="sys_flag_unexpected_constant" & is_calculated==FALSE,
                       omit:=TRUE]
      
      pfcbl_data_flags <- pfcbl_data_flags[omit==FALSE]
      pfcbl_data_flags[,
                       `:=`(omit=NULL,
                            is_calculated=NULL)]
      
      # data_flags <- data_flags[,
      #                          .(data_flags_new=list(.SD)),
      #                          by=.(rsf_pfcbl_id,
      #                               indicator_id,
      #                               reporting_asof_date,
      #                               reporting_template_row_group,
      #                               reporting_template_data_rank),
      #                          .SDcols=c("check_name","check_message")]
      
      #Reset the flags and re-apply those after checks
      # template$pfcbl_data[,
      #                     data_flags_new:=NULL]
      # 
      # template$pfcbl_data[data_flags,
      #                     data_flags_new:=i.data_flags_new,
      #                     on=.(rsf_pfcbl_id,
      #                          indicator_id,
      #                          reporting_asof_date,
      #                          reporting_template_row_group,
      #                          reporting_template_data_rank)]
      
    
    }    
    
    
    template$pfcbl_data[,omit:=FALSE]
    template$pfcbl_data[is.na(indicator_id) |
                        is_unsubscribed==TRUE |
                        indicator_id %in% template$rsf_indicators[is_system==TRUE,indicator_id],
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
                                                    paste0(paste0('"',labels_submitted,'"'),
                                                           collapse=' & '))),
                                   by=.(sheet_name)]
        bad_headers <- paste0("Column header not defined recognized.\n  Recommended: either define an indicator or an alias in System Admin; OR, enter these columns in Template Ignore Colmns in Program Setup.\n ",
                              paste0(bad_headers$message,collapse=" "))
        
        template$pfcbl_reporting_flags <- rbindlist(list(template$pfcbl_reporting_flags,
                                                         data.table(rsf_pfcbl_id=as.numeric(NA),
                                                                    indicator_id=as.numeric(NA),
                                                                    reporting_asof_date=template$reporting_cohort$reporting_asof_date,
                                                                    check_name="sys_flag_indicator_not_found",
                                                                    check_message=bad_headers)))
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
                                                                                    indicator_id=as.numeric(NA),
                                                                                    #indicator_id, #cannot put it on this indicator ID because this data point has been omitted and will never upload
                                                                                                  #must tag sys_X_reporting indicator!
                                                                                    reporting_asof_date,
                                                                                    check_name="sys_flag_indicator_not_monitored",
                                                                                    check_message=message)]))
      }
      unsubscribed_indicators <- NULL
    }
  }
  
  #misalignments 
  {
    #misliagnments should never happen... But they could potentially result from accidental or malicious activities.  Eg, user might copy and paste an uploads pfcbl_id
    #in one row associated with a borrower and borrower-level indicator to a different row associated with a facility-level indicator.
    #data_integrity_key generally is designed to allow users some flexibility in moving data around, or even the possibility to add a new indicator that's within the existing
    #set in the template for an existing rsf_pfcbl_id entity in the template without violating the key. But also opens the door for some issues such as misalignment.
    #Database "instead" insert trigger should deny (and throw error) any misalignments as well--it would be a big problem.
    template <- template_remove_data_category_misalignments(template=template)
  
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
                  where for_rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int) 
                    and lcu.reporting_asof_date <= $2::date
                  order by 
                  lcu.for_rsf_pfcbl_id,
                  lcu.reporting_asof_date desc",
                            params=list(paste0(ratios[is.na(entity_local_currency_unit)==TRUE,unique(rsf_pfcbl_id)],collapse=","),
                                        template$reporting_cohort$reporting_asof_date))
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
        
        fx_lookup <- db_data_get_fx_ratio(pool=pool,
                                          fx_lookup=ratios[,
                                                           .(rsf_pfcbl_id,
                                                             exchange_rate_date=reporting_asof_date,
                                                             to_currency,
                                                             from_currency)],
                                          create.indicators=TRUE, #Yes, create because these will be queried otherwise later for the same indicator when variance is checked.
                                          force.global=TRUE) #the whole point is to check against global rates
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
    
    #check for currency data duplicates
    #and if multiple culumns are reported in different currencies!
    {
      currency_data <- template$pfcbl_data[indicator_id %in% template$rsf_indicators[data_type=="currency",indicator_id]]
      
      #should happen, but just in case
      currency_data[is.na(data_unit),
                    data_unit:='LCU']
      
      currency_data[,
                    redundancies:=length(unique(paste0(ifelse(is.na(data_value),"0",data_value),data_unit))),
                    by=.(rsf_pfcbl_id,
                         indicator_id,
                         reporting_asof_date,
                         reporting_template_row_group)]
      
      currency_data <- currency_data[redundancies>1][order(indicator_id)]
      if (!empty(currency_data)) {
        
        currency_indicator_ids <- template$rsf_indicators[grepl("currency_unit",indicator_sys_category) & indicator_id %in% unique(template$pfcbl_data$indicator_id),
                                                 .(indicator_id,data_category)]
        
        #if the template is uploading its local currency unit, then no need to download it
        currency_units <- template$pfcbl_data[indicator_id %in% currency_indicator_ids$indicator_id,
                                              .(rsf_pfcbl_id,indicator_id,reporting_asof_date,data_category,local_currency_unit=data_value)]
        currency_data[,
                      `:=`(local_currency_unit=as.character(NA),       #LCU of the entity (generally loan or facility)
                           defined_currency_unit=as.character(NA),     #If this metric is defined as _always_ reporting in a specific currency, eg USD (rare)
                           formula_calculation_unit=as.character(NA))] #Setting-specific for formulas to return values converted into this currency for this facility.
        
        #Priority 3: it is assumed to be the local currency
        currency_data[currency_units,
                      local_currency_unit:=i.local_currency_unit,
                      on=.(rsf_pfcbl_id,
                           reporting_asof_date)]
        
        #Priority 2: it is setup to calculate in this currency
        currency_data[indicator_subscriptions[data_type=="currency" & is.na(formula_calculation_unit)==FALSE],
                      formula_calculation_unit:=i.formula_calculation_unit,
                      on=.(indicator_id)]
        
        #Priority 1: it must be this currency by definition of the metric
        currency_data[template$rsf_indicators[data_type=="currency" & data_unit != "LCU"],
                      defined_currency_unit:=i.data_unit,
                      on=.(indicator_id)]
        
        #So if the Template uploads metric_x as 100EUR and elsewhere metric_x as 120USD: if, forexample, it is set to have a formula calculation unit as EUR
        #then the metric_x 100EUR will be considered the value of record and 120USD will be the redundancy, even if the facility's base value is USD.
        
        #we want everyone's local currency unit to know which is our reference data versus our redundant data. (reference is reported data in the LCU value)
        missing_lcu <- currency_data[is.na(local_currency_unit)]
        if (!empty(missing_lcu)) {
          lcu <- lapply(unique(as.character(missing_lcu$reporting_asof_date)),
                        FUN=function(asof_date,mlcu) {
                          lcu <- dbGetQuery(pool,"
                     select distinct on (lcu.for_rsf_pfcbl_id)
                       lcu.for_rsf_pfcbl_id as rsf_pfcbl_id,
    									 lcu.data_unit_value,
    									 lcu.reporting_asof_date as lcu_current_date
    								 from p_rsf.rsf_data_current_lcu lcu
    								 where lcu.for_rsf_pfcbl_id = any(select unnest(string_to_array($2::text,','))::int)
    									 and lcu.reporting_asof_date <= $1::date
    								 order by lcu.for_rsf_pfcbl_id,lcu.reporting_asof_date desc",
                                            params=list(asof_date,
                                                        paste0(unique(mlcu$rsf_pfcbl_id),collapse=",")))
                          lcu$reporting_asof_date <- as.Date(asof_date)
                          lcu
                        },mlcu=missing_lcu)
          lcu <- rbindlist(lcu)
          lcu[,
              joincondition:=as.character(NA)]
          currency_data[lcu,
                        local_currency_unit:=i.data_unit_value,
                        on=.(rsf_pfcbl_id,
                             reporting_asof_date)]
          
          if (!empty(currency_data[is.na(local_currency_unit)])) {
            stop("Redundant data reported and failed to resolve local currency units")
          }
        }
        
        currency_data[data_unit=="LCU",
                      data_unit:=local_currency_unit]
        currency_data[,
                      unit_ratio:=paste0(data_unit,"/",local_currency_unit)]
        currency_data[,
                      fx_ratio:=as.numeric(NA)]
        currency_data[data_unit==local_currency_unit,
                      fx_ratio:=1]
        
        ratios[,joincondition:=as.numeric(NA)]
        
        fx_ratios <- rbindlist(list(ratios[,.(reporting_asof_date,data_value=as.numeric(data_value),data_unit)],
                                    ratios[,.(reporting_asof_date,
                                              data_value=1/as.numeric(data_value),
                                              data_unit=paste0(to_currency,"/",from_currency))]))
        currency_data[fx_ratios,
                      fx_ratio:=i.data_value,
                      on=.(reporting_asof_date,
                           unit_ratio=data_unit)]
        
        if (anyNA(currency_data$fx_ratio)) {
          stop("Soren TODO: lookup fx ratios that are not reported in dataset for comparing redundant reported indicators")
        }
        
        currency_data[,
                      fx_value:=round(as.numeric(data_value)*fx_ratio,CALCULATIONS_ENVIRONMENT$SIG_DIGITS)]
        
        currency_data[,
                      unredundancies:=length(unique(fx_value)),
                      by=.(rsf_pfcbl_id,
                           indicator_id,
                           reporting_asof_date,
                           reporting_template_row_group)] #redundancy not restricted to reporting_template_row_group
        
        #not redundant at CURRENT FX rates
        if (any(currency_data$unredundancies==1)) {
          equal_currency_data <- currency_data[unredundancies==1]
          
          equal_currency_data[,
                              redundancy_priority:=fcase(data_unit==defined_currency_unit,1,
                                                         data_unit==formula_calculation_unit,2,
                                                         data_unit==local_currency_unit,3,
                                                         default=4)]
          equal_currency_data[,
                              is_calculated:=FALSE]
          
          equal_currency_data[indicator_subscriptions[is_calculated==TRUE],
                        is_calculated:=TRUE,
                        on=.(indicator_id)]
          
          #given group .by below, will put indicators _in the same row group_ in the order first of refernece_data and then in order of data_id (order of appearance)
          setorder(equal_currency_data,
                   is_calculated,   #reported data is priority over calculated data as uncalculated is 0/FALSE and calcualted is 1/TRUE
                   redundancy_priority,
                   reporting_template_data_rank)
          
          equal_currency_data[,
                              omit:=(1:.N)>1,
                              by=.(rsf_pfcbl_id,
                                   indicator_id,
                                   reporting_asof_date,
                                   reporting_template_row_group)]
          
          redundancies <- equal_currency_data[,
                                    .(message=paste0("Multiple values reported ",paste0("{",data_submitted,"}",collapse=" & "),
                                                     " are equal in current-FX terms. Saving {",data_submitted[omit==FALSE],"} and discarding redundancies")),
                                    by=.(rsf_pfcbl_id,
                                         indicator_id,
                                         reporting_asof_date,
                                         reporting_template_row_group,
                                         indicator_name)]
          
          template$pfcbl_reporting_flags <- rbindlist(list(template$pfcbl_reporting_flags,
                                                           redundancies[,.(rsf_pfcbl_id,
                                                                           indicator_id,
                                                                           reporting_asof_date,
                                                                           check_name="sys_flag_multiple_data_points_reported",
                                                                           check_message=message)]))
          #currency_data[omit==F]
          #omitting it because it's identical, so no need to flag
          template$pfcbl_data <- template$pfcbl_data[!(reporting_template_data_rank %in% equal_currency_data[omit==TRUE,reporting_template_data_rank])]  
          currency_data <- currency_data[unredundancies > 1] #these are not equal at CURRENT FX rates
          equal_currency_data <- NULL
        }
        
        #TODO: Try again by testing fx rate at parameter's last update FX rate date.
      }
      currency_data <- NULL
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
                                .(indicator_name,labels,duplicate_value)])
          
          ambiguous_data <- ambiguous_data[,
                                       .(message=paste0(indicator_name," is ",duplicate_value," on ",labels,collapse=" \n")),
                                       by=.(indicator_name)]
          
            stop(paste0("\nAMBIGUOUS REPORTING: Repeat metrics should report the same value, but different values are reported:\n",
                        "Verify that the labels in the template are correctly entered?\n\n",
                        paste0(ambiguous_data$message,collapse=" \n")))  
        }
        ambiguous_data <- NULL          
        
        template$pfcbl_data[,
                            duplicate_value:=NULL]
      }
      template$pfcbl_data[,ambiguous:=NULL]
      
      template$pfcbl_data <- unique(template$pfcbl_data)
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
    
    if (empty(template$pfcbl_data)) {
      print("Template has no new data.  Returning NULL")
      return (NULL)
    }
    
    template <- template_set_redundancy_reporting(pool=pool,
                                                  indicator_subscriptions=indicator_subscriptions,
                                                  template=template)

    #Facility ranks
    {
      if (template$reporting_cohort$pfcbl_category_rank < 2 &
          length(unique(template$pfcbl_data$rsf_pfcbl_id)) > 1) 
      {

        facilities <- dbGetQuery(pool,"
          select
            ids.rsf_pfcbl_id,
            coalesce(ids.rsf_facility_id,ids.rsf_program_id) as reporting_rsf_pfcbl_id
            
          from p_rsf.rsf_pfcbl_ids ids
          where ids.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)",
          params=list(paste0(unique(template$pfcbl_data$rsf_pfcbl_id),collapse=",")))
          
        setDT(facilities)
        
        template$pfcbl_data[facilities,
                            reporting_rsf_pfcbl_id:=i.reporting_rsf_pfcbl_id,
                            on=.(rsf_pfcbl_id)]
      } else {
      
        template$pfcbl_data[,
                            reporting_rsf_pfcbl_id:=template$reporting_cohort$reporting_rsf_pfcbl_id]
      }
    }    
    
    setorder(template$pfcbl_data,
             reporting_rsf_pfcbl_id,
             reporting_asof_date)
    
    template$pfcbl_data[,
                        reporting_chronology_rank:=(.GRP)-1,
                        by=.(reporting_rsf_pfcbl_id,
                             reporting_asof_date)]
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
                   "reporting_template_row_group",
                   
                   "reporting_rsf_pfcbl_id",
                   "reporting_chronology_rank"
                   )
    
    remove_cols <- names(template$pfcbl_data)[!names(template$pfcbl_data) %in% keep_cols]
    template$pfcbl_data[,(remove_cols):=NULL]
    setcolorder(template$pfcbl_data,
                neworder=keep_cols)
    
    if (!all(unique(template$match_results$match_action) %in% c("new",         #identified as new based on input ID fields not already existing
                                                                "unchanged",   #matched hasvalues shows data is unchanged
                                                                "update"))) {
      stop(paste0("Match results match_action must be 1 of: new, unchanged, update, revert, omit.  Found: ",paste0(unique(template$match_results$match_action),collapse=", ")))
    }
    
    
    if (is.null(template$pfcbl_data) || 
        !all(c("reporting_template_row_group",
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
  
  #change row names as used in database
  {
    setnames(template$pfcbl_data,
             old="reporting_template_row_group",
             new="data_source_row_id")
    
    setnames(pfcbl_data_flags,
             old="reporting_template_row_group",
             new="data_source_row_id")
    
    #If redundancies data ranks have been removed 
    pfcbl_data_flags <- pfcbl_data_flags[reporting_template_data_rank %in% template$pfcbl_data$reporting_template_data_rank]
    pfcbl_data_flags[,reporting_template_data_rank:=NULL]
    pfcbl_data_flags <- unique(pfcbl_data_flags)
    template$pfcbl_data_flags <- pfcbl_data_flags
    
    # if (!is.null(template$pfcbl_data_flags)) {
    #   setnames(template$pfcbl_data_flags,
    #          old="reporting_template_row_group",
    #          new="data_source_row_id")
    # }
  }
  
  template$process_time <- as.numeric(Sys.time()-t1,"secs")
  if(SYS_PRINT_TIMING) debugtime("template_process"," Process time: ",format(Sys.time()-t1))
  return (template)
}