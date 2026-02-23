#TODO:
#Efficiency gain: don't check each reporting_template_row_id: aggregate these into each unique ID-type and check only those (except for static reporting templates where the row ID is an ID value)
#hashvalues should be set before this, based on wide data
#possible efficiency gain by filtering query against creation date range(s) of entities that are being passed-in?
template_set_data_match_rsf_ids <- function(pool,
                                            template)
{
  t1 <- Sys.time()

  #setups
  {
    #if(SYS_PRINT_TIMING) debugtime("template_set_data_match_rsf_ids")
    
    import_id <- as.numeric(template$reporting_import$import_id)
    if (is.null(import_id) || all(is.na(import_id))) stop(paste0("Bad import_id: ",import_id))

    if (length(unique(template$reporting_asof_date))>1) stop("template_set_data_match_rsf_ids() only supports data with one reporting as-of date.  Templates with multiple chronologies must use template_set_data_match_pfcbl_ids()") 
    
    rsf_indicators <- template$rsf_indicators
    template_data <- template$template_data
    
    template_match_data <- template$match_results
    
    template_match_data[,recognized_entity_number:=as.numeric(NA)]
  }
  
  
 
  
  #For data archive restores: we know who the entity WILL BE over time, but hasn't been created yet.  So restore the who-am-i relationship across data rows
  #where those rows may not have ID data values for data updates that are submitted over time.
  template_data_sys_names <- NULL
  if (any(names(template$template_data)=="SYSNAME")) {
    
    template_data_sys_names <- unique(template$template_data[,.(reporting_template_row_group,data_category,SYSNAME)])
    template_data_sys_names[,sysname_tree:=strsplit(SYSNAME,
                                  split=">",
                                  fixed=T)]
    set(template_data_sys_names,
        i=NULL,
        j="pfcbl_category",
        value=sapply(template_data_sys_names$sysname_tree,function(x) gsub("\\s?([a-z]+):.*$","\\1",x[length(x)])))
    
    template_data_sys_names[,
          recognized_entity_number:=.GRP,
          by=.(SYSNAME,
               data_category)]
    
    template_match_data[template_data_sys_names,
                        recognized_entity_number:=i.recognized_entity_number,
                        on=.(reporting_template_row_group,
                             pfcbl_category)]
  }
  
  #Main lookup matching
  {
    current_rsf_id_rank <- 0 #0=Global, which is already known by definition as Global.
                             #1=Program, and nothing can be submitted with undefined rsf_program_id
    #max means we've matched all ranks below this one
    #if (!empty(template_match_data[!is.na(rsf_id)])) current_rsf_id_rank <- max(template_match_data[!is.na(rsf_id),pfcbl_rank])
    
    repeat {

      #setup current ranks and match data filters
      {
        #get first required lookup category, from parent-est to child-est
        ranks <- template_match_data[is.na(rsf_pfcbl_id) | rsf_pfcbl_id <= 0,
                                     #is.na(match_action), #because match action can be "new" and therefore blank rsf_pfcbl_id
                                     unique(pfcbl_rank)]
        
        #above is what's used because template_match_data for rsf_id matching is built through rbindlist for each pfcbl category
        #where as pfcbl _id matching gives a full list of match data ex-ante and so we can look at still-to-be-matched ranks that cannot
        #be done with sequential matching
        #ranks <- template_match_data[is.na(rsf_pfcbl_id),unique(pfcbl_rank)]
        if (length(ranks)==0) break;
        if (min(ranks) < current_rsf_id_rank) {
          stop("current_rsf_id_rank > min(ranks) in template_match_data")
        }
        
        current_rsf_id_rank <- min(ranks)
        template_db_current_match_data <-NULL
      }      
      

      #get current match data and categories
      {
        
        current_match_data <- template_match_data[pfcbl_rank==current_rsf_id_rank]
        current_data_category <- template_match_data[pfcbl_rank==current_rsf_id_rank,unique(pfcbl_category)]
        current_id_category <- fcase(current_data_category=="loan","rank_id",
                                     current_data_category=="borrower","id",
                                     current_data_category=="client","id",
                                     current_data_category=="facility","id",
                                     current_data_category=="program","name", #not all programs have IFC project ID numbers (but some do! eg, SLGP)
                                     current_data_category=="global","name",
                                     default=NA)
        
        template_current_data_all <- template_data[indicator_sys_category == current_id_category &
                                                     data_category==current_data_category,
                                                   .(reporting_template_row_group,
                                                     pfcbl_category=data_category,
                                                     indicator_sys_category,
                                                     data_value=data_value,
                                                     data_submitted,
                                                     indicator_id)]
        current_match_data[template_current_data_all,
                           id_value:=i.data_value,
                           on=.(reporting_template_row_group,
                                pfcbl_category)]
        
        new_ids_already_defined <- current_match_data[,
          .(has_defined_ids=any(!is.na(rsf_pfcbl_id)),
            has_undefined_ids=anyNA(rsf_pfcbl_id)),
          by=.(id_value,
               pfcbl_category)
         ][has_defined_ids==TRUE & has_undefined_ids==TRUE,
           unique(id_value)]
        
        if (length(new_ids_already_defined)) {
          defined_ids <- current_match_data[id_value %in% new_ids_already_defined]
          defined_ids[,
                      single_definition:=length(unique(rsf_pfcbl_id[!is.na(rsf_pfcbl_id)]))==1,
                      by=.(id_value,
                           pfcbl_category)]
          defined_ids <- defined_ids[single_definition==TRUE & !is.na(rsf_pfcbl_id)]
          
          if (!empty(defined_ids)) {
            defined_ids[,joincondition:=as.numeric(NA)]
            
            current_match_data[defined_ids,
                               `:=`(parent_rsf_pfcbl_id=i.parent_rsf_pfcbl_id,
                                    rsf_pfcbl_id=i.rsf_pfcbl_id,
                                    match_action="update",
                                    matched_by="predefined"),
                               on=.(id_value,
                                    pfcbl_category,
                                    rsf_pfcbl_id=joincondition)]
            
            # defined_ids <- current_match_data[!is.na(rsf_pfcbl_id) & id_value %in% defined_ids$id_value & matched_by=="predefined"]
            # defined_ids[,joincondition:=as.numeric(NA)]
            # 
            # template_match_data[defined_ids,
            #                     `:=`(parent_rsf_pfcbl_id=i.parent_rsf_pfcbl_id,
            #                          rsf_pfcbl_id=i.rsf_pfcbl_id,
            #                          match_action="update",
            #                          matched_by="predefined"),
            #                     on=.(reporting_template_row_group,
            #                          pfcbl_category,
            #                          rsf_pfcbl_id=joincondition)]
            
          }
        }
        
        current_match_data <- current_match_data[is.na(rsf_pfcbl_id) | rsf_pfcbl_id <= 0 | (!is.na(matched_by) & matched_by=="predefined")]
        
        parent_data_category <- fcase(current_rsf_id_rank==0,as.character(NA),
                                      current_rsf_id_rank==1,"global",
                                      current_rsf_id_rank==2,"program",
                                      current_rsf_id_rank==3,"facility",
                                      current_rsf_id_rank==4,"client",
                                      current_rsf_id_rank==5,"borrower",
                                      default=NULL)
        
        parent_rsf_pfcbl_id_col <- paste0("rsf_",parent_data_category,"_id")
        rsf_id_col <- paste0("rsf_",current_data_category,"_id")
        
        #Because most IFC QR templates include the CLIENT ID and not facility ID
        #We need to lookup the facility ID from the client
        #But this is also used for JASON templates that can look-up the parent chain if for example just a loan data point is uploaded that it can look-up the loan's borrower
        if (anyNA(current_match_data$parent_rsf_pfcbl_id)) {
          
          #If the parent ID is undefined...
          #And if the parent ID only has one child at the current data category, then we can set it.
          parent_ids <- dbGetQuery(pool,"
                                    select 
                                      ft.to_family_rsf_pfcbl_id as rsf_pfcbl_id
                                    from p_rsf.view_rsf_pfcbl_id_family_tree ft
                                    where ft.from_rsf_pfcbl_id = $1::int
                                      and ft.pfcbl_hierarchy = 'child'
                                      and ft.to_pfcbl_category = $2::text",
                                   params=list(template$reporting_import$import_rsf_pfcbl_id,
                                               parent_data_category))
          if (nrow(parent_ids)==1) {
            
            current_match_data[is.na(parent_rsf_pfcbl_id),
                               parent_rsf_pfcbl_id:=parent_ids$rsf_pfcbl_id]
          
          
          } else if (template$reporting_import$import_pfcbl_category=="program" && current_data_category=="facility") {
            current_match_data[is.na(parent_rsf_pfcbl_id),
                               parent_rsf_pfcbl_id:=template$reporting_import$import_rsf_pfcbl_id]
          } else if (template$reporting_import$import_pfcbl_category=="facility" && current_data_category=="client") {
            current_match_data[is.na(parent_rsf_pfcbl_id),
                               parent_rsf_pfcbl_id:=template$reporting_import$import_rsf_pfcbl_id]
          } else if (!is.null(template_data_sys_names)) {
            parent_sys_names <- current_match_data[is.na(parent_rsf_pfcbl_id),
                               .(reporting_template_row_group,
                                 pfcbl_category,
                                 recognized_entity_number)
                               ][template_data_sys_names,
                                 on=.(reporting_template_row_group,
                                      recognized_entity_number,
                                      pfcbl_category),
                                 nomatch=NULL
                                 ][,
                                   .(reporting_template_row_group,
                                     recognized_entity_number,
                                     pfcbl_category,
                                     parent_sys_name=sapply(sysname_tree,function(x) trimws(paste0(x[-length(x)],collapse=">"))))]
            
            parent_ids <- template_data_sys_names[pfcbl_category==parent_data_category,
                                                  .(reporting_template_row_group,
                                                    pfcbl_category,
                                                    SYSNAME)
                                                  ][template_match_data[,.(reporting_template_row_group,
                                                                          pfcbl_category,
                                                                          rsf_pfcbl_id)],
                                                    on=.(reporting_template_row_group,
                                                         pfcbl_category),
                                                    nomatch=NULL
                                                    ][,
                                                      .(parent_rsf_pfcbl_id=rsf_pfcbl_id,
                                                        parent_sys_name=SYSNAME,
                                                        joincondition=as.numeric(NA))]
            parent_ids <- unique(parent_ids)
            
            parent_ids <- parent_sys_names[parent_ids,
                                           on=.(parent_sys_name),
                                           nomatch=NULL][!is.na(parent_rsf_pfcbl_id)]
            
            current_match_data[parent_ids,
                               parent_rsf_pfcbl_id:=i.parent_rsf_pfcbl_id,
                               on=.(parent_rsf_pfcbl_id=joincondition,
                                    reporting_template_row_group,
                                    pfcbl_category)]
            
          
          } else if (!empty(template$match_SYSNAMES) && any(names(template_data)=="SYSID")) {
              
              current_SYSIDS <- unique(template_data[data_category==current_data_category,
                                                     .(reporting_template_row_group,
                                                       SYSID)])
              
              current_SYSIDS <- current_SYSIDS[unique(template$match_SYSNAMES[,.(SYSID,parent_SYSID)]),
                                               on=.(SYSID),
                                               nomatch=NULL]
              
              current_SYSIDS <- current_SYSIDS[,
                                               .(reporting_template_row_groups=list(reporting_template_row_group)),
                                               by=.(SYSID,
                                                    parent_SYSID)]
              
              
              parent_SYSIDS <- unique(template_data[data_category==parent_data_category,
                                                    .(reporting_template_row_group=reporting_template_row_group,
                                                      parent_SYSID=SYSID)])
              parent_SYSIDS <- unique(parent_SYSIDS[template_match_data[pfcbl_category==parent_data_category,
                                                                 .(reporting_template_row_group,
                                                                   rsf_pfcbl_id)],
                                             .(parent_SYSID,
                                               parent_rsf_pfcbl_id=rsf_pfcbl_id),
                                             on=.(reporting_template_row_group),
                                             nomatch=NULL])
              
              current_SYSIDS <- current_SYSIDS[parent_SYSIDS,
                                               on=.(parent_SYSID),
                                               nomatch=NULL
                                               ][,.(reporting_template_row_group=unlist(reporting_template_row_groups,recursive=F)),
                                                 by=.(SYSID,
                                                      parent_rsf_pfcbl_id)]
              current_SYSIDS[,joincondition:=as.numeric(NA)]
              
              current_match_data[current_SYSIDS,
                                 parent_rsf_pfcbl_id:=i.parent_rsf_pfcbl_id,
                                 on=.(parent_rsf_pfcbl_id=joincondition,
                                      reporting_template_row_group)]
              parent_SYSIDS <- NULL
              current_SYSIDS <- NULL
              
            
            
          
          } else {
              
              bad_data <- unique(template$template_data[data_category==current_data_category,
                                                 .(reporting_template_row_group,
                                                   indicator_name,
                                                   data_submitted)])
              message <- paste0("System is uploading the following ",toupper(current_data_category)," level data: \n",
                                paste0("  ",paste0(bad_data$reporting_template_row_group," tab: ",bad_data$indicator_name," = '",bad_data$data_submitted,"'"),collapse=" \n"))
              
              message <- paste0(message," \n",
                                "However new ",toupper(current_data_category),"s cannot be created because system cannot match any ",toupper(parent_data_category),"s. \n",
                                "Verify your dataset is correct and that all ",toupper(parent_data_category),"s and ",toupper(current_data_category),"s have relevent ID and/or Name data columns to lookup or create")
              
              stop(message)
              
            
          }
          
        }
        
        if (anyNA(current_match_data$parent_rsf_pfcbl_id)) {
          stop(paste0("NA values for parent_rsf_pfcbl_id column for ",current_data_category," on rows: ",
                      paste0(current_match_data[is.na(parent_rsf_pfcbl_id),unique(reporting_template_row_group)],collapse=", ")))
        }
        
        #Just for lonans
        #issuance_rank_indicator <- rsf_indicators[data_category==current_data_category & indicator_sys_category=="issuance_rank"]
        
      }
      
      #to enable early-exit if fully matched
      repeat {    
          
        
        #template_current_lookup_data -- relevant lookup data available in the submitted template to match against db_lookup_data
        {
          

          template_current_data <- template_current_data_all[current_match_data[is.na(match_action), #may alraedy be marked new coming in
                                                                       .(reporting_template_row_group,
                                                                         parent_rsf_pfcbl_id,
                                                                         pfcbl_category)],
                                                  on=.(reporting_template_row_group,
                                                       pfcbl_category),
                                                  nomatch=NULL]
          
          if (empty(template_current_data)) break;
          
          
          #duplicates?
          #This can happen when client templates repeat the same columns (why on earth?).  But Equity BCDC redundantly reports the internal ID number for some reason?
          #If the data is the same, ignore.
          #Otherwise, we can't have two differnet IDs for the same entity.
          {
            duplicates <- template_current_data[,
                                                .(duplicated=.N,
                                                  uduplicates=length(unique(data_submitted))),
                                                  by=names(template_current_data)][duplicated>1][order(reporting_template_row_group,indicator_sys_category)]
            
            #if there are both duplicates and unique duplicates, it's a problem.  Otherwise we just remove duplicates.
            if (!empty(duplicates[uduplicates>1])) {
              
              duplicates <- duplicates[uduplicates > 1,
                                       .(message=paste0(reporting_template_row_group," ambiguous ",toupper(indicator_sys_category)," as: ",paste0("{",data_submitted,"}",collapse=" & "),"\n")),
                                       by=.(reporting_template_row_group,indicator_sys_category)]
              stop(paste0("Template reports ambiguous values in multiple columns:\n",
                          paste0(duplicates$message,collapse=" \n")))
              
            } else {
              template_current_data <- unique(template_current_data)
            }
            duplicates <- NULL
            
            
          }
          
          #If somehow template_process() did not setup match_data correctly
          if (!all(current_match_data$reporting_template_row_group %in% template_current_data$reporting_template_row_group)) {
            
            no_id_data_rows <- current_match_data$reporting_template_row_group[!current_match_data$reporting_template_row_group %in% template_current_data$reporting_template_row_group]
            
            recognized_ids_with_data <- current_match_data[reporting_template_row_group %in% template_current_data$reporting_template_row_group,recognized_entity_number]
            no_id_data_rows <- current_match_data[reporting_template_row_group %in% no_id_data_rows &
                                                  !recognized_entity_number %in% recognized_ids_with_data,
                                                  reporting_template_row_group]
            
            if (length(no_id_data_rows) > 0) {
              #no_ids <- which(!(current_match_data$reporting_template_row_group %in% template_current_data$reporting_template_row_group))
              valid_indicators <- template$rsf_indicators[data_category==current_data_category &
                                          indicator_sys_category %in% indicator_sys_category_match_indicators$indicator_sys_category,
                                          indicator_name]
              
              stop(paste0("No Identifier indicators submitted to lookup ",toupper(current_data_category)," on: ",
                          paste0(no_id_data_rows,collapse=" & "),". ",
                          "Dataset expected to provide at least one of: ",
                          paste0(valid_indicators,collapse=" AND/OR ")))
            }
          }

          template_current_lookup_data <- template_current_data[,
                                                                .(parent_rsf_pfcbl_id,
                                                                  reporting_template_row_group,
                                                                  indicator_id,
                                                                  indicator_sys_category,
                                                                  data_value)]

          if (nrow(template_current_lookup_data) != nrow(unique(template_current_lookup_data))) {
            
            stop(paste0("Duplicated data in template_current_lookup_data(",nrow(template_current_lookup_data),") vs template_current_data(",nrow(template_current_data),")"))
          }
          
          #if an ID value has ">>" it is a trigger for indicating a changed/modified/updated ID
          #where it is necessary to report the OLD ID that it is being changed FROM and the new ID that it is being changed TO: FROM >> TO
          #So the system can lookup FROM and then change TO
          #Will result in BOTH the OLD and the NEW IDs being part of the lookup
          modified_ids <- template_current_data[grepl(">>",data_submitted)]
          if (!empty(modified_ids)) {
            modified_ids[,
                         data_value:=strsplit(data_value,split=">>")]
            modified_ids <- modified_ids[,
                                         .(data_value=unlist(data_value,recursive=F)),
                                         by=.(parent_rsf_pfcbl_id,
                                              reporting_template_row_group,
                                              indicator_id,
                                              indicator_sys_category)]
            
            modified_ids[,
                         data_value:=normalizeSyscategory_id(data_value)]
            modified_ids <- modified_ids[!is.na(data_value)]
            
            template_current_lookup_data <- rbindlist(list(template_current_lookup_data,
                                                           modified_ids))
            
            template_current_lookup_data <- unique(template_current_lookup_data)
            
          }
        } 
        
        #create template_db_match_data
        {
         
          #filter missing values and aggregate repeated rows (eg, one borrower reports two new inclusions)
          {
            
            #happens through parse_data            
            #leading zeros will get matched as id_modified and this is really not very modified enough to count.  So omit zero-padding here.
            template_current_lookup_data[(is.na(data_value)    |
                                          data_value=="" |
                                          data_value=="{MISSING}" |
                                          grepl("^[[:punct:][:blank:][:cntrl:]]+$",data_value) |
                                          grepl("^MISSING",data_value,ignore.case=T) |
                                          grepl("^ERROR|^#ERROR",data_value,ignore.case=T) |
                                          grepl("^N/A$|^NA$|^#NA$",data_value,ignore.case=T)),
                                         `:=`(missing_values=TRUE,
                                              data_value="{MISSING}")]

            if (anyNA(template_current_data$data_value)) {
              stop(paste0(template_current_data[is.na(data_value),paste0(pfcbl_category," on ",reporting_template_row_group," has {MISSING} ",indicator_sys_category)],collapse=" \n"))
            }
            
            template_current_lookup_data <- unique(template_current_lookup_data)

            template_current_lookup_data <- template_current_lookup_data[,
                                                                         .(reporting_template_row_groups=paste0(reporting_template_row_group,
                                                                                                                collapse=",")),
                                                                         by=.(parent_rsf_pfcbl_id,
                                                                              data_value)]
          }
          

          #Query database          
          {
            t2 <- Sys.time()
            #conn <- poolCheckout(pool)
            #dbBegin(conn)
            #dbRollback(conn)
            #dbExecute(conn,"drop table _temp_match")
            
            # lobstr::obj_size(template_current_lookup_data[,.(parent_rsf_pfcbl_id,indicator_id,indicator_sys_category,data_value,reporting_template_row_groups)])
            
            template_db_current_match_data <- poolWithTransaction(pool,function(conn) {
              
              
              dbExecute(conn,"
                        create temp table _temp_match(parent_rsf_pfcbl_id int,
                                                      data_value text,
                                                      reporting_template_row_groups text)
                        on commit drop;")
              
              #t3 <- Sys.time()
              dbAppendTable(conn,
                            name="_temp_match",
                            value=template_current_lookup_data[,
                                                               .(parent_rsf_pfcbl_id,
                                                                 data_value,
                                                                 reporting_template_row_groups)])
              
              dbExecute(conn,"analyze _temp_match")
             
              dbExecute(conn,"alter table _temp_match add primary key(parent_rsf_pfcbl_id,
                                                                      data_value, 
                                                                      reporting_template_row_groups)")
              
              
              #glue matches on parent_rsf_pfcbl_id_col eg, if we're borrower match on rsf_client_id col
              #NOTE: p_rsf.rsf_data_current_names_and_ids MUST have column names identical to INDICATOR_SYS_CATEGORY, eg, "id" and "rank_id"
              matches <- dbGetQuery(conn,glue("
                                    select
                                    	tmd.parent_rsf_pfcbl_id,
                                    	tmd.reporting_template_row_groups,
                                    	ids.rsf_pfcbl_id as matched_rsf_pfcbl_id
                                    from p_rsf.rsf_pfcbl_ids ids
                                    inner join p_rsf.rsf_data_current_names_and_ids nids on nids.rsf_pfcbl_id = ids.rsf_pfcbl_id
                                    inner join _temp_match tmd on tmd.parent_rsf_pfcbl_id = ids.{parent_rsf_pfcbl_id_col}
                                    													and tmd.data_value is not distinct from nids.{current_id_category}
                                    where ids.created_in_reporting_asof_date <= $1::date
                                    	and ids.pfcbl_category = $2::text"),
                                    params=list(template$reporting_asof_date,
                                                current_data_category))

              
              return (matches)
              
            })
          
            setDT(template_db_current_match_data)
          }
          
          {
            
            if(SYS_PRINT_TIMING) debugtime("template_set_data_match_rsf_ids",paste0("db_current_match_data for ",current_data_category),format(Sys.time()-t2))
            
            template_current_lookup_data[,data_value:=NULL]
            unmatched_data <- fsetdiff(template_current_lookup_data[,
                                                 .(parent_rsf_pfcbl_id,
                                                   reporting_template_row_groups)],
                                      template_db_current_match_data[,
                                                                     .(parent_rsf_pfcbl_id,
                                                                       reporting_template_row_groups)])


            #when creating new entities with db_create_new_rsf_ids, some minimal level of ID information must be within the dataset (so that we can look it up later!)
            #so if we see a new data_source_row_id (because the template has static rows), then does that new row have an ID value in it?
            #if not, then don't score a new row as a new possible new entity
            current_match_data_new <- current_match_data[!is.na(rsf_pfcbl_id) & rsf_pfcbl_id < 0,rsf_pfcbl_id]
            if (length(current_match_data_new) > 0) {
              current_match_data_new <- abs(min(current_match_data_new))
            } else { 
              current_match_data_new <- 0
            }
            
            unmatched_data[,
                           `:=`(matched_rsf_pfcbl_id=-(.I+current_match_data_new))]
            
            template_current_lookup_data <- rbindlist(list(template_db_current_match_data,
                                                           unmatched_data))
            
            template_current_lookup_data <- template_current_lookup_data[,
                                                                         .(reporting_template_row_group=as.character(
                                                                           unlist(
                                                                             strsplit(reporting_template_row_groups,
                                                                                      split=",",
                                                                                      fixed=T),
                                                                             recursive=F))),
                                                                         by=.(parent_rsf_pfcbl_id,
                                                                              matched_rsf_pfcbl_id)]
            
            #In the unlikely event we matched NEW and EXISTING (such as through an ID modification, for example, where OLD is matched and NEW is not)
            #Then max() will return the actual ID (positive number) before the NEW ID trigger (negative number)
            template_current_lookup_data[,
                                         matched_rsf_pfcbl_id:=max(matched_rsf_pfcbl_id,na.rm=T),
                                         by=.(parent_rsf_pfcbl_id,
                                              reporting_template_row_group)]
            
            #It is also possible that we matched an OLD entry by defined IDs outside this function (eg, by loan inclusion rank) and that an identical borrower ID has been inserted that has matched with
            #a new inclusion.  So match the new inclusions to the old defined ID.  
            #This may also constitue a reporting error...but hopefully critrical errors will be raised by matching loan IDs across different borrower portfolios, if the client did make an error
            
            template_current_lookup_data[,joincondition:=as.numeric(NA)]
            current_match_data[template_current_lookup_data,
                               `:=`(rsf_pfcbl_id=i.matched_rsf_pfcbl_id,
                                    match_action=ifelse(matched_rsf_pfcbl_id < 0,
                                                        "new",
                                                        "update"),
                                    matched_by=ifelse(matched_rsf_pfcbl_id < 0,
                                                      "nomatches",
                                                      current_id_category)),
                               on=.(parent_rsf_pfcbl_id,
                                    rsf_pfcbl_id=joincondition,   #in case it's already defined somehow
                                    reporting_template_row_group)]
            
          }
          
          if (!empty(current_match_data[is.na(match_action)])) {
            stop(paste0("Failed to match or determine as new for: ",
                        current_match_data[is.na(match_action),paste0(pfcbl_category," on ",reporting_template_row_group)]))
          }
          
          break;
        }
        
      }
      
      #NEW IDS
      {
        if (!empty(current_match_data[rsf_pfcbl_id < 0])) {
          
        
         
          
          #pfcbl_id can pre-assign new groups on template read-in without need to group by data-values across rows (pfcbl templates 
          #will also read-in "long" data and therefore not have parent-child relationships on individual rows)
          {
            create_new_undefined <- current_match_data[rsf_pfcbl_id < 0,
                                                       .(reporting_template_row_group,
                                                         unique_new_entity_row_id=-rsf_pfcbl_id,
                                                         parent_rsf_pfcbl_id)]
            creation_dates <- template_data[indicator_sys_category == current_id_category &
                                            data_category == current_data_category &
                                            reporting_template_row_group %in% create_new_undefined$reporting_template_row_group,
                                            .(reporting_template_row_group,
                                              reporting_asof_date)]
            
            create_new_undefined[creation_dates,
                                 creation_asof_date:=i.reporting_asof_date,
                                 on=.(reporting_template_row_group)]
            
            create_new_undefined <- create_new_undefined[,
                                                         .(reporting_template_row_groups=list(unique(reporting_template_row_group)),
                                                           creation_asof_date=min(creation_asof_date,na.rm=T)),
                                                         by=.(unique_new_entity_row_id,
                                                              parent_rsf_pfcbl_id)]
           
            
            setorder(create_new_undefined,
                     unique_new_entity_row_id,
                     creation_asof_date,
                     parent_rsf_pfcbl_id)
           
              

            
            
          }
          
        
          create_new_undefined[,
                     unique_new_entity_row_id:=1:.N]
          
          create_new_undefined[,count:=sapply(reporting_template_row_groups,length)]
         
            
          if (sum(create_new_undefined$count) != nrow(current_match_data[match_action=="new"])) {
            stop(paste0("Expected to create ",nrow(current_match_data[match_action=="new"])," new ",current_data_category,"s. ",
                        "But match data resulted in ",sum(create_new_undefined$count)," entities"))
          }
            

          new_pfcbl_ids <- db_create_new_rsf_ids(pool=pool,
                                                 reporting_import=template$reporting_import,
                                                 parent_pfcbl_category=parent_data_category,
                                                 for_pfcbl_category=current_data_category,
                                                 new_ids=create_new_undefined[,
                                                                    .(unique_new_entity_row_id,
                                                                      parent_rsf_pfcbl_id,
                                                                      creation_asof_date)])
          create_new_undefined[new_pfcbl_ids,
                     `:=`(rsf_id=i.new_rsf_pfcbl_id,
                          rsf_pfcbl_id=i.new_rsf_pfcbl_id),
                     on=.(unique_new_entity_row_id,
                          parent_rsf_pfcbl_id)]
          
          create_new_undefined <- create_new_undefined[,
                                   .(reporting_template_row_group=unlist(reporting_template_row_groups,recursive=F)),
                                   by=.(unique_new_entity_row_id,
                                        parent_rsf_pfcbl_id,
                                        rsf_pfcbl_id,
                                        rsf_id)]
          
          current_match_data[create_new_undefined,
                             `:=`(rsf_pfcbl_id=i.rsf_pfcbl_id),
                             on=.(reporting_template_row_group,
                                  parent_rsf_pfcbl_id)]
          new_pfcbl_ids <- NULL
          create_new_undefined <- NULL
       }
      }
      
      #join new rsf_ids into relevant data tables
      {
        
        template_match_data[current_match_data,
                            `:=`(parent_rsf_pfcbl_id=i.parent_rsf_pfcbl_id,
                                 rsf_pfcbl_id=i.rsf_pfcbl_id,
                                 match_action=i.match_action,
                                 match_issues=i.match_issues,
                                 matched_by=i.matched_by),
                            on=.(reporting_template_row_group,
                                 pfcbl_category)]
        
        
        template_match_data[current_match_data[,
                                               .(rsf_pfcbl_id,
                                                 reporting_template_row_group,
                                                 child_rank=pfcbl_rank+1,
                                                 joincondition=as.numeric(NA))],
                            parent_rsf_pfcbl_id:=i.rsf_pfcbl_id,
                            on=.(parent_rsf_pfcbl_id=joincondition,
                                 reporting_template_row_group,
                                 pfcbl_rank=child_rank)]

        if (!empty(template_match_data[pfcbl_category==current_data_category & (is.na(rsf_pfcbl_id)==TRUE | rsf_pfcbl_id <= 0)])) {
          
          stop("Template has missing rsf_pfcbl_ids")
        }
      }    
      
    } #NEXT ENTITY
  }
  
  if (!all(template_match_data$match_action %in% c("new","update"))) {
    stop("template_match_data match actions not defined as: new or update")
  } 
  
  match_action_issues <- template_match_data[is.na(match_issues)==FALSE,
                                             .(reporting_template_row_group,
                                              rsf_pfcbl_id,
                                              match_action,
                                              match_issues)]
  
  if (!empty(match_action_issues)) {

    match_action_issues[,check_message:=paste0("ROW ",reporting_template_row_group," Action: ",match_action,"/ ",match_issues)]
    match_action_issues[,check_message:=paste0(check_message,collapse=" AND ALSO "),
                        by=.(rsf_pfcbl_id)]
    match_action_issues[,
                        `:=`(reporting_asof_date=template$reporting_asof_date,
                             indicator_id=as.numeric(NA),
                             check_name="sys_flag_template_match_actions")]

    setcolorder(match_action_issues,
                neworder=c("rsf_pfcbl_id","indicator_id","reporting_asof_date","check_name","check_message"))
    
    template$pfcbl_reporting_flags <- rbindlist(list(template$pfcbl_reporting_flags,
                                                     match_action_issues[,.(rsf_pfcbl_id,
                                                                            indicator_id,
                                                                            reporting_asof_date,
                                                                            check_name,
                                                                            check_message
                                                                            )]))
    template$pfcbl_reporting_flags <- unique(template$pfcbl_reporting_flags)

  }
  
  #template$template_data <- template_data
  template$match_results <- template_match_data
  
  if(SYS_PRINT_TIMING) debugtime("template_set_data_match_rsf_ids","Done!",format(Sys.time()-t1))
  
  return (template)
}

