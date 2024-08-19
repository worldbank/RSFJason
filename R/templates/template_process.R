#rsf_program_id may be NA where template encodes program_id (such as system reports); but traditional templates require a user-defined input value
template_process <- function(pool,
                             template,
                             status_message=function(...) {}) {
  t1 <- Sys.time()
  #tp <- copy(template)
  #Indicator and data validations
  {
    #Check for valid fields created in processing
    {
      
      status_message(class="info",paste0("Parsing template: ",template$template_source," for reporting date ",template$reporting_asof_date,"\n"))
      rsf_program_id <- template$rsf_program_id
  
      if (!all(c("rsf_program_id",
                 "rsf_indicators",
                 "reporting_cohort",
                 "reporting_asof_date",
                 "template_ids_method",
                 "template_data") %in% names(template))) stop("Templates must return: rsf_program_id, rsf_indicators,  reporting_cohort, reporting_asof_date, template_source_reference, template_ids_method and template_data")
      
      
      if (length(setdiff(c("reporting_asof_date",
                           "indicator_name",
                           "reporting_submitted_data_unit",
                           "reporting_submitted_data_value",
                           "reporting_submitted_data_formula",
                           "reporting_template_row_group"),
                         names(template$template_data)))>0) {
        
        stop("Template Data must define: reporting_asof_date,indicator_name,reporting_submitted_data_unit,reporting_submitted_data_value,reporting_submitted_data_formula,reporting_template_row_group,<optional>labels_submitted")
      }
      
      if (!template$template_ids_method %in% c("rsf_id","pfcbl_id")) stop("Template IDs method must define either 'rsf_id' or 'pfcbl_id'")
  
      if (template$template_ids_method=="pfcbl_id" && !any(names(template$template_data)=="SYSID")) {
        stop("Templates defining pfcbl_id lookup must define SYSID column in template_data")
      }
      
      if (is.null(template$reporting_asof_date) || is.null(template$template_data$reporting_asof_date) || length(template$reporting_asof_date) != 1 || 
          !all(template$reporting_asof_date %in% template$template_data$reporting_asof_date)) stop("Template's reporting_asof_date must uniquely match template's data program_id")
    }
  
    #Check we're uploading for a valid date for the program
    #Check is made in template_process instead of parse_files since file could be passed an NA rsf_program_id for a program_create
    #Here, we know the template has a specific program ID
    {
      #NOTE: A cohort with a "today" reporting date will have a valid reporting_asof_date.  That cohort MAY ALSO have FUTURE reporting_asof_dates in its
      #template_data in which case, cohort triggers will insert those future dates as valid dates.
      #However, a template with a "tomorrow" reporting date will not be allowed.
      #Ie, I can know today what future requirements will be (because they're contracted to be so).
      #But I cannot report future data from a future perspective.
      valid_reporting_dates <- dbGetQuery(pool,"
                                          select 
                                            prd.valid_reporting_date::date,
                                            rp.reporting_period
                                          from p_rsf.rsf_programs rp
                                          inner join lateral p_rsf.rsf_program_generate_reporting_dates(rp.rsf_program_id,now()::date) as prd on true
                                          where rp.rsf_program_id = $1::int
                                            and prd.valid_reporting_date <= $2::date
                                          order 
                                            by prd.valid_reporting_date desc",
                                          params=list(template$rsf_program_id,
                                                      template$reporting_asof_date))
      
      valid_dates <- valid_reporting_dates$valid_reporting_date
  
      if (length(valid_dates)==0 ||
          !template$reporting_asof_date %in% valid_dates) {
        
        #Previous denied reporting into the future.  However, sometimes future dates are known, such as pre-defined contractual obligations or changes
        #and therefore should allow
        if (template$reporting_asof_date > max(valid_dates)) {
          stop(paste0("Future reporting not allowed: reporting date ",
                      as.character(template$reporting_asof_date)," is ",
                      as.numeric(template$reporting_asof_date - today(tzone="GMT"))," days into the future from today's date ",
                      as.character(today(tzone="GMT"))))
        }
        
        stop(paste0("Template reporting date ",as.character(template$reporting_asof_date),
                    " is not a valid date for this program"))
      }
      
      if (template$reporting_asof_date != template$reporting_cohort$reporting_asof_date) {
        stop(paste0("Mismatch error: template$reporting_asof_date = ",
                    as.character(template$reporting_asof_date)," and reporting_cohort$reporting_asof_date = ",
                    as.character(template$reporting_cohort$reporting_asof_date)))
      }
      
      template$reporting_period <- c(lubridate::floor_date(template$reporting_asof_date,
                                                           unique(valid_reporting_dates$reporting_period)),
                                     template$reporting_asof_date)
    }
    
    
    # (1) Parse indicators, ensure they're valid for this program
    # (2) Parse indicator formats and flag/omit invalid data formats and options 
    #     !important: this must be done before hashvalues and further controls to ensure comparisons are being made against normalized data (eg, trimmed, etc)
    #     especially before matching IDs using things like name where normalized name is trimmed and upper case and only useful to compare using standardized formatting
    # (3) Get program settings to control further processing behavior
    {
      
      status_message("Parsing template data.\n")
      
      if (empty(template$template_data)) stop("Template contains no data after omitting unrecognized/unsubscribed indicators.  Ensure program is subscribed to indicators and labels are properly defined.")
  
      #if unknown indicator name is submitted, NA indicator_id. This will be filtered later after rsf_pfcbl_ids are identified along with relevant subscriptions
      template$template_data[template$rsf_indicators,
                             `:=`(indicator_id=i.indicator_id,
                                  indicator_sys_category=i.indicator_sys_category,
                                  data_category=i.data_category),
                             on=.(indicator_name)]
      
      template$template_data <- unique(template$template_data)
      
      #duplicates per row should fail
      {
        template_duplicates <- unique(template$template_data[,
                                                             .(reporting_template_row_group,
                                                               indicator_name,
                                                               reporting_submitted_data_value)])
        
        template_duplicates[,n:=.N,
                            by=.(reporting_template_row_group,
                                        indicator_name)]
        
        template_duplicates <- template_duplicates[n>1]
        if (!empty(template_duplicates)) {
          #unlikely error check -- adding values more than doubles the calculation time.  So let's take the time just if the error exists in the first place after doing a faster calc
          template_duplicates <-  template$template_data[,.(n=length(unique(reporting_submitted_data_value)),
                                                            values=paste0(paste0("Value#",1:.N," Reported under: '",indicator_name,"' = {",reporting_submitted_data_value,"}"),collapse=" AND ")),
                                                         by=.(reporting_template_row_group,indicator_name)][n>1]
          
          for (i in 1:nrow(template_duplicates)) {
            redundancy <- template_duplicates[i]
            status_message(class="info",redundancy$indicator_name," is specified ",redundancy$n," times with following values: ",redundancy$values,"\n")
          }
          msg <- paste0(template_duplicates[,paste0(indicator_name," is specified ",n," times with different values: ",values)],collapse=" [AND ALSO] ")
          stop(paste0("Template has the same indicator multiple times with different values. Template must be corrected: delete repeated indicators and/or ensure duplicates report the same value: ",msg))
        }
        template_duplicates <- NULL
      }      
      
      #Instances where templates repeat indicators on multiple rows for convenience sake of the user but use different labels, aliases of labels or language combination of
      #labels. So merge these all together.
      
      #labels submitted no longer purposeful
      # unique_cols <- names(template$template_data)[-which(names(template$template_data) %in% c("labels_submitted","reporting_submitted_data_formula"))]
      # if (!(all(is.na(template$template_data$labels_submitted)) && all(is.na(template$template_data$reporting_submitted_data_formula)))) {
      #   if (!any(names(template$template_data)=="labels_submitted")) {
      #     template$template_data[,labels_submitted:=indicator_name]
      #   }
      #   
      #   template$template_data <- template$template_data[,.(labels_submitted=paste0(labels_submitted,collapse=" || "),
      #                                                       
      #                                                       #don't want <NA> formulas to turn into "NA" when there's a duplicated entry
      #                                                       reporting_submitted_data_formula=ifelse(all(is.na(reporting_submitted_data_formula)),
      #                                                                                               as.character(NA),
      #                                                                                               paste0(reporting_submitted_data_formula,collapse=" [AND ALSO] "))),
      #                                                     by=c(unique_cols)]
      # }    
      
      template$template_data <- parse_data_formats(template_data=template$template_data,
                                                   rsf_indicators=template$rsf_indicators)
      
      futures <- template$template_data[indicator_sys_category=="entity_creation_date" & !is.na(data_value)][ymd(data_value) > reporting_asof_date]
      if (!empty(futures)) {
        max_future <- futures[data_value==max(data_value),paste0(unique(indicator_name)," ",unique(as.character(data_value)))]
        futures <- futures[,
                .(message=paste0("Future creation date on ",.N,
                              " rows starting from {",min(as.character(data_value)),"} on ",reporting_template_row_group[[1]],
                              " to {",max(as.character(data_value)),"} on ",reporting_template_row_group[[.N]])),
                by=.(indicator_name)]
        
       
        for (i in 1:nrow(futures)) status_message(class="error",
                                                  paste0("Creation date is in the future: template reporting date is ",
                                                         template$reporting_asof_date,
                                                         " but ",futures[i,indicator_name]," has: ",futures[i,message],"\n"))
        
        stop(paste0("Template failed to parse: entities are reported to have been created in the future.  Correct reporting date or correct the creation date to the past: ",max_future," cannot be greater than template reporting date ",template$reporting_asof_date))
      }
      futures <- NULL
      
    } 
  
    #Validate template_reporting_row_group: ensures max/min and sorting of reporting_template_row_group are as expected
    #Starts with a row number, upper case dataset name and/or sheet name
    {
      if (any(grepl("^[[:digit:]]+[A-Z_-]+$",template$template_data$reporting_template_row_group)==FALSE)) {
        first <- template$template_data$reporting_template_row_group[which(!grepl("^[[:digit:]]+[A-Z_-]+$",template$template_data$reporting_template_row_group))[[1]]]
        
        stop(paste0("template_data$reporting_template_row_group expects format: ROW-numberROW-name. Where ROW-name is Letters or _ or - (not numbers or other punctuation).  First malformed row group is: ",first))
      }
    }
    
    duplicates <- template$template_data[,
                                         .(n=.N),
                                         by=.(reporting_template_row_group,
                                              indicator_name,
                                              reporting_asof_date)][n>1]
    if (!empty(duplicates)) {
      stop(paste0("Duplicated entries for: ",
                  paste(paste(duplicates$reporting_template_row_group," ",duplicates$indicator_name," ",duplicates$reporting_asof_date)),
                  collapse=" \n"))
    }
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
      template_data_ids <- na.omit(template$template_data[,unique(SYSID)])
      if (!all(template$reporting_cohort$reporting_rsf_pfcbl_id %in% template_data_ids)) {
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
  
  
  #parse indicators
  #some templates will filter these out on read-in.  Others won't so double check.
  {
    indicator_subscriptions <- dbGetQuery(pool,"
                                          select 
                                          pis.rsf_pfcbl_id,
                                          pis.indicator_id,
                                          pis.is_unsubscribed,
                                          pis.rsf_program_id,
                                          pis.has_program_entry,
                                          pis.formula_id,
                                          pis.is_calculated
                                          from p_rsf.view_rsf_pfcbl_indicator_subscriptions pis
                                          where pis.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)",
                                          params=list(paste0(unique(template$match_results$rsf_pfcbl_id),collapse=",")))
    setDT(indicator_subscriptions)
    
    template$pfcbl_data[indicator_subscriptions,
                        is_unsubscribed:=i.is_unsubscribed,
                        on=.(rsf_pfcbl_id,
                             indicator_id)]
    
    #indicator_subscriptions <- NULL
    data_flags <- template$pfcbl_data[,
                                      unlist(data_flags_new,recursive=F),
                                      by=.(rsf_pfcbl_id,
                                           indicator_id,
                                           reporting_asof_date,
                                           reporting_template_row_group)]
    
    if (!empty(data_flags)) {
      data_flags[indicator_subscriptions,
                 is_calculated:=i.is_calculated,
                 on=.(rsf_pfcbl_id,
                      indicator_id)]
      data_flags[,
                 omit:=FALSE]
      
      data_flags[check_name=="sys_flag_unexpected_formula" & is_calculated==TRUE,
                 omit:=TRUE]
      
      data_flags[check_name=="sys_flag_unexpected_constant" & is_calculated==FALSE,
                 omit:=TRUE]
      
      data_flags <- data_flags[omit==FALSE]
      data_flags <- data_flags[,
                               .(data_flags_new=list(.SD)),
                               by=.(rsf_pfcbl_id,
                                    indicator_id,
                                    reporting_asof_date,
                                    reporting_template_row_group),
                               .SDcols=c("check_name","check_message")]
      
      #Reset the flags and re-apply those after checks
      template$pfcbl_data[,
                          data_flags_new:=NULL]
      
      template$pfcbl_data[data_flags,
                          data_flags_new:=i.data_flags_new,
                          on=.(rsf_pfcbl_id,
                               indicator_id,
                               reporting_asof_date,
                               reporting_template_row_group)]
      
    
    }    
    data_flags <- NULL
    
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
        # 
        # bad_headers <- unique(bad_headers[,
        #                                   .(indicator_name,sheet_name)])
        # bad_headers[,id:=1:.N]
        # bad_headers[,
        #             .(labels_submitted=strsplit(labels_submitted,split="||",fixed=T)),
        #             by=.(id,sheet_name)]
        # bad_headers[,
        #             labels_normalized:=normalizeLabel(labels_submitted)]
        # 
        # template_ignore_headers <- dbGetQuery(pool,"
        #                                     select distinct on (pft.template_id,ignore)
        #                                       pft.template_id,
        #                                       pft.rsf_pfcbl_id,
        #                                       ignore as ignore_header,
        #                                       nids.pfcbl_category || ':' || nids.rsf_full_name as ignore_name
        #                                     from p_rsf.view_rsf_pfcbl_id_family_tree ft
        #                                     inner join p_rsf.rsf_program_facility_templates pft on pft.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
        #                                     inner join lateral unnest(ignore_headers_list) as ignore on true
        #                                     inner join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = pft.rsf_pfcbl_id
        #                                     where ft.from_rsf_pfcbl_id = $1::int
        #                                       and ft.to_pfcbl_rank <= ft.from_pfcbl_rank 
        #                                       and pft.template_id = $2::int
        #                                     order by pft.template_id,ignore,ft.to_pfcbl_rank",
        #                                     params=list(template$reporting_cohort$reporting_rsf_pfcbl_id,
        #                                                 template$template_id))
        # setDT(template_ignore_headers)
        # bad_headers[,ignore:=FALSE]
        # if (!empty(template_ignore_headers)) {
        #   template_ignore_headers[,
        #                           ignore_header:=normalizeLabel(ignore_header)]
        #   
        #   template_ignore_headers <- bad_headers[template_ignore_headers,
        #                                          on=.(labels_normalized=ignore_header)]
        #   
        #   if (!empty(template_ignore_headers)) {
        #     bad_headers[id %in% template_ignore_headers$id,
        #                 ignore:=TRUE]
        #     
        #     for (ind in unique(template_ignore_headers$labels_submitted)) {
        #       status_message(class="info",paste0("Ignored Header: '",ind,"'\n"))
        #     }
        #     
        #     template_ignore_headers <- template_ignore_headers[,
        #                                                        .(message=paste0('[',ignore_name,'] ',
        #                                                                         paste0(paste0('"',labels_submitted,'"'),
        #                                                                                collapse=" & "))),
        #                                                        by=.(ignore_name)]
        #     template_ignore_headers <- paste0(template$template_name," SETUP is set to IGNORE HEADERS for ",
        #                                       paste0(template_ignore_headers$message,collapse=" "))
        #     
        #     template$pfcbl_reporting_flags <- rbindlist(list(template$pfcbl_reporting_flags,
        #                                                      data.table(rsf_pfcbl_id=as.numeric(NA),
        #                                                                 indicator_id=as.numeric(NA),
        #                                                                 reporting_asof_date=template$reporting_cohort$reporting_asof_date,
        #                                                                 check_name="sys_flag_indicator_ignored",
        #                                                                 check_message=template_ignore_headers)))
        #   }            
        # }
        # 
        # bad_headers <- bad_headers[ignore==FALSE]
        
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
                                                                                    indicator_id,
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
  
  {
    keep_cols <- c("reporting_template_row_group",
                   "rsf_pfcbl_id",
                   "indicator_id",
                   "reporting_asof_date",
                   "data_value",
                   "data_unit",
                   "data_flags_new",
                   "data_submitted")
    
    remove_cols <- names(template$pfcbl_data)[!names(template$pfcbl_data) %in% keep_cols]
    template$pfcbl_data[,(remove_cols):=NULL]
    setcolorder(template$pfcbl_data,
                neworder=keep_cols)
  }
  
  #With rsf_pfcbl_ids and reporting_cohort created, merge-in and flags that have been raised thus far.
  {

    pfcbl_data_flags <- template$pfcbl_data[,
                                            unlist(data_flags_new,recursive = F),
                                            by=.(reporting_template_row_group,
                                                 rsf_pfcbl_id,
                                                 indicator_id,
                                                 reporting_asof_date)]
    
    if (!empty(pfcbl_data_flags)) {
      template$pfcbl_data_flags <- pfcbl_data_flags[,.(reporting_template_row_group,
                                                       rsf_pfcbl_id,
                                                       indicator_id,
                                                       reporting_asof_date,
                                                       check_name,
                                                       check_message)]
    } else {
      template$pfcbl_data_flags <- NULL #so there's a reference entry when template_upload requests
    }
    
    if (any(names(template$pfcbl_data)=="data_flags_new")) template$pfcbl_data[,data_flags_new:=NULL]
  }
  
  #validate formatting and column requirements
  {

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
                                                  template=template)

    template$pfcbl_data[,reporting_chronology_rank:=frank(template$pfcbl_data,
                                                          reporting_asof_date,
                                                          ties.method = "dense")-1] #zero-based index
    
    base_rank <- template$pfcbl_data[reporting_asof_date==template$reporting_cohort$reporting_asof_date,
                                     unique(reporting_chronology_rank)]
    
    #Past are negative numbers.
    #Current is zero
    #Future are positive numbers
    template$pfcbl_data[,
                        reporting_chronology_rank:=reporting_chronology_rank-base_rank]
  }

  #change row names as used in database
  {
    setnames(template$pfcbl_data,
             old="reporting_template_row_group",
             new="data_source_row_id")
    
    if (!is.null(template$pfcbl_data_flags)) {
      setnames(template$pfcbl_data_flags,
             old="reporting_template_row_group",
             new="data_source_row_id")
    }
  }
  
  #Check manually reported currency ratios
  #This happens here and not in parse_data_formats because mannually reported ratios usually report a generic LCU fx rate, eg, USD/LCU
  #In which case, we need to know the entity and what it's LCU value is to process this.
  {
    #only facilities can report fx ratios.
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
      
      ratios <- ratios[inverse_reported==TRUE]
      if (!empty(ratios)) {
        
        ratios[,
               inverted_data_unit:=paste0(gsub("^.*([A-Z]{3})$","\\1",data_unit),"/",
                                          gsub("^([A-Z]{3}).*$","\\1",data_unit))]
        
        ratios[,
               message:=paste0("Inverted currency ratio reported {",data_unit," ",data_value,"} ",
                               "FX rate auto-corrected to: {",inverted_data_unit," ",data_value,"} ")]
        
        template$pfcbl_reporting_flags <- rbindlist(list(template$pfcbl_reporting_flags,
                                                         ratios[,
                                                                .(rsf_pfcbl_id,
                                                                  indicator_id,
                                                                  reporting_asof_date,
                                                                  check_name="sys_flag_data_unit_auto_correction",
                                                                  check_message=message)]))
        template$pfcbl_data[ratios,
                            data_unit:=i.inverted_data_unit,
                            on=.(rsf_pfcbl_id,
                                 indicator_id,
                                 reporting_asof_date)]
        
        # ratios[template$rsf_indicators,
        #        indicator_name:=i.indicator_name,
        #        on=.(indicator_id)]
        # ratios <- ratios[template$pfcbl_data[,
        #                            .(rsf_pfcbl_id,
        #                              indicator_id,
        #                              reporting_asof_date,
        #                              data_source_row_id,
        #                              data_submitted,
        #                              data_unit_submitted=data_unit)],
        #        on=.(rsf_pfcbl_id,
        #             indicator_id,
        #             reporting_asof_date),
        #        nomatch=NULL]
        # 
        # ratios[template$rsf_indicators,
        #        indicator_name:=i.indicator_name,
        #        on=.(indicator_id)]
        # 
        # ratios[,
        #        inverted_data_unit:=paste0(gsub("^.*([A-Z]{3})$","\\1",data_unit),"/",
        #                                   gsub("^([A-Z]{3}).*$","\\1",data_unit))]
        # ratios[,
        #        message:=paste0("Inverted currency ratio reported for '",indicator_name,"' {",data_unit,"=",data_submitted,"} ",
        #                        "FX rate should be corrected as: {",inverted_data_unit," ",data_value,"} ",
        #                        " OR {",data_unit," ",as.character((1/data_value)),"}")]
        
        #stop(paste0(ratios$message,collapse=" AND ALSO \n"))
      }
    }
  }
  template$process_time <- as.numeric(Sys.time()-t1,"secs")
  if(SYS_PRINT_TIMING) debugtime("template_process"," Process time: ",format(Sys.time()-t1))
  return (template)
}