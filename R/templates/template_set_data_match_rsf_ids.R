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
    
    reporting_cohort_id <- as.numeric(template$reporting_cohort$reporting_cohort_id)
    if (is.null(reporting_cohort_id) || all(is.na(reporting_cohort_id))) stop(paste0("Bad reporting_cohort_id: ",reporting_cohort_id))
    
    if (is.null(template$template_settings$template_has_static_row_ids) ||
        is.na(template$template_settings$template_has_static_row_ids)) stop("Undefined template setting: template_has_static_row_ids")
   
    if (length(unique(template$reporting_asof_date))>1) stop("template_set_data_match_rsf_ids() only supports data with one reporting as-of date.  Templates with multiple chronologies must use template_set_data_match_pfcbl_ids()") 
    
    rsf_indicators <- template$rsf_indicators
    template_data <- template$template_data
    
    
    template_has_static_row_ids <- template$template_settings$template_has_static_row_ids
    
    #These are the only indicator category types that are used for looking-up and matching entities (aside from hashvalue)
    indicator_sys_category_match_indicators <- data.table(indicator_sys_category=c("id",        #*should* be unique -- affected by loan renewals
                                                                                   "rank_id",   #*should* be unique -- affected by loan renrewals
                                                                                   "name",      #not guaranteed to be unique, may not be
                                                                                   "entity_creation_date", #Entity create date 
                                                                                   
                                                                                   "data_source_row_id"),     #pseudo-category
                                                          #hashvalue itself may not be unique in the database
                                                          should_be_unique=c(TRUE,
                                                                             TRUE,
                                                                             FALSE,
                                                                             FALSE,
                                                                             FALSE), #known NOT to be unique for renewals, for example
                                                          #If new entity exists on multiple rows (eg, a borrower entering the portfolio with multiple loans issued in the same QR),
                                                          #use these categories.  Ie, we don't want to use the row_id as a unique category, as we know it will be different and fail to group;
                                                          #but even the hashvalue should not be used because the formating could be applied to the name and cause different hashvalues for same-named entities
                                                          use_create_new=c(TRUE,
                                                                           TRUE,
                                                                           TRUE,
                                                                           TRUE,
                                                                           FALSE),
                                                          score=c(4, #ID:            best, highest quality match
                                                                  3, #rank_id:       (for loans) excellent match (when compliant and not having copy-paste errors!!)
                                                                  2, #name:          is a mediocre match: should be pretty unique (for borrowers) but typos and change a lot due to data entry issues
                                                                  2, #creation_date: Not too much happens on exactly the same date across multiple entities within the same parent (but can); more frquently, clients report an identical default date for borrowers
                                                                  4)) 

    
    
  }
  
  
  template_match_data <- template$match_results
  
  template_match_data[,recognized_entity_number:=as.numeric(NA)]
  
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
        ranks <- template_match_data[is.na(rsf_pfcbl_id),
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
      
#if (current_rsf_id_rank==5) stop("test")
      #get current match data and categories
      {
        
        current_match_data <- template_match_data[pfcbl_rank==current_rsf_id_rank]
        current_match_data <- current_match_data[is.na(rsf_pfcbl_id)]
        
        
        current_data_category <- template_match_data[pfcbl_rank==current_rsf_id_rank,unique(pfcbl_category)]
        
        parent_data_category <- dbGetQuery(pool,"
                                           select rpc.pfcbl_category
                                           from p_rsf.rsf_pfcbl_categories rpc
                                           where pfcbl_rank < $1
                                           order by pfcbl_rank desc
                                           limit 1",
                                           params=list(current_rsf_id_rank))
        parent_data_category <- parent_data_category$pfcbl_category
        
        parent_rsf_pfcbl_id_col <- paste0("rsf_",parent_data_category,"_id")
        rsf_id_col <- paste0("rsf_",current_data_category,"_id")
        
        if (anyNA(current_match_data$parent_rsf_pfcbl_id)) {
          
          #If the parent ID is undefined...
          #And if the parent ID only has one child at the current data category, then we can set it.
          parent_ids <- dbGetQuery(pool,"
                                    select 
                                      fam.child_rsf_pfcbl_id as rsf_pfcbl_id
                                    from p_rsf.rsf_pfcbl_id_family fam
                                    where fam.parent_rsf_pfcbl_id = $1::int
                                      and fam.child_pfcbl_category = $2::text",
                                   params=list(template$reporting_cohort$reporting_rsf_pfcbl_id,
                                               parent_data_category))
          if (nrow(parent_ids)==1) {
            
            current_match_data[is.na(parent_rsf_pfcbl_id),
                               parent_rsf_pfcbl_id:=parent_ids$rsf_pfcbl_id]
          
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
        issuance_rank_indicator <- rsf_indicators[data_category==current_data_category & indicator_sys_category=="issuance_rank"]
        
      }
      
      #to enable early-exit if fully matched
      repeat {    
          
        
        #template_current_lookup_data -- relevant lookup data available in the submitted template to match against db_lookup_data
        {

          template_current_data <- template_data[indicator_sys_category %in% indicator_sys_category_match_indicators$indicator_sys_category & 
                                                 data_category==current_data_category,
                                                 .(reporting_template_row_group,
                                                   pfcbl_category=data_category,
                                                   indicator_sys_category,
                                                   data_value=data_value,
                                                   data_submitted,
                                                   indicator_id)
                                                 ][current_match_data[is.na(match_action), #may alraedy be marked new coming in
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
          
          #template_current_lookup_data[,reporting_template_id:=template$template_id]
          template_current_data[is.na(data_value),
                                data_value:=data_submitted] #so NAs don't get compared as <NA> but as "NA" or "{MISSING}"
          
          template_current_lookup_data <- template_current_data[,
                                                                .(parent_rsf_pfcbl_id,
                                                                  reporting_template_row_group,
                                                                  indicator_id,
                                                                  indicator_sys_category,
                                                                  data_value)]
          
         
          if (nrow(template_current_lookup_data) != nrow(unique(template_current_lookup_data))) {
           
            stop(paste0("Duplicated data in template_current_lookup_data(",nrow(template_current_lookup_data),") vs template_current_data(",nrow(template_current_data),")"))
          }
        } 
        
        #create template_db_match_data
        {
         
          #create pseudo columns: normalized_id and data_source_row_id
          {
            if (template_has_static_row_ids==TRUE) {
              
              row_source_ids <- unique(template_current_lookup_data[,
                                                                   .(parent_rsf_pfcbl_id,
                                                                     reporting_template_row_group,
                                                                     indicator_id=as.numeric(NA),
                                                                     indicator_sys_category="data_source_row_id",
                                                                     data_value=reporting_template_row_group)])
              
              template_current_lookup_data <- rbindlist(list(template_current_lookup_data,
                                                             row_source_ids))
              row_source_ids <- NULL
            }
            
            template_current_lookup_data[,missing_values:=FALSE]
            template_current_lookup_data[(is.na(data_value)    |
                                          data_value=="" |
                                          data_value=="{MISSING}" |
                                          grepl("^[[:punct:][:blank:][:cntrl:]]+$",data_value) |
                                          grepl("^MISSING",data_value,ignore.case=T) |
                                          grepl("^ERROR|^#ERROR",data_value,ignore.case=T) |
                                          grepl("^N/A$|^NA$|^#NA$",data_value,ignore.case=T)),
                                         `:=`(missing_values=TRUE,
                                              data_value="{MISSING}")]

            #happens through parse_data            
            # #leading zeros will get matched as id_modified and this is really not very modified enough to count.  So omit zero-padding here.
            # template_current_lookup_data[indicator_sys_category=="id",
            #                              data_value:=gsub("^0+","",data_value)]
            
            #Fully omitted because we don't want remaining NA matches to count toward NEW entity matches
            template_current_lookup_data <- template_current_lookup_data[missing_values==FALSE]
            template_current_lookup_data <- unique(template_current_lookup_data)
            template_current_lookup_data[,missing_values:=NULL]
            
            template_current_lookup_data <- template_current_lookup_data[,
                                                                         .(reporting_template_row_groups=paste0(reporting_template_row_group,
                                                                                                                collapse=",")),
                                                                         by=.(parent_rsf_pfcbl_id,
                                                                              indicator_id,
                                                                              indicator_sys_category,
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
                                                      indicator_id int,
                                                      indicator_sys_category text,
                                                      data_value text,
                                                      reporting_template_row_groups text)
                        on commit drop;")
              
              t3 <- Sys.time()
              dbAppendTable(conn,
                            name="_temp_match",
                            value=template_current_lookup_data[,
                                                               .(parent_rsf_pfcbl_id,
                                                                 indicator_id,
                                                                 indicator_sys_category,
                                                                 data_value,
                                                                 reporting_template_row_groups)])
              
              dbExecute(conn,"analyze _temp_match")
              
              if(SYS_PRINT_TIMING) debugtime("template_set_data_match_rsf_ids",
                                             paste0("upload data for ",current_data_category," ",nrow(template_current_lookup_data))," rows in ",
                                             format(Sys.time()-t3))
              
              
              dbExecute(conn,"alter table _temp_match add primary key(parent_rsf_pfcbl_id,
                                                                      indicator_sys_category,
                                                                      data_value, 
                                                                      reporting_template_row_groups)")
              
              
           
              matches <- dbGetQuery(conn,"
                                    select
                                    	tmd.parent_rsf_pfcbl_id,
                                    	tmd.reporting_template_row_groups,
                                    	tmd.indicator_id,
                                    	tmd.indicator_sys_category,
                                    	ids.rsf_pfcbl_id as matched_rsf_pfcbl_id
                                    from p_rsf.rsf_pfcbl_ids ids
                                    inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
                                    inner join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id
                                                                         and rdc.indicator_id = ind.indicator_id
                                    inner join _temp_match tmd on tmd.parent_rsf_pfcbl_id = ids.parent_rsf_pfcbl_id
                                    													and tmd.indicator_id = rdc.indicator_id
                                    													and tmd.data_value is not distinct from rdc.data_value
                                    where ids.created_in_reporting_asof_date <= $1::date
                                    	and ids.parent_rsf_pfcbl_id = any((select distinct tm.parent_rsf_pfcbl_id from _temp_match tm))
                                    	and ind.indicator_id = any((select unnest(string_to_array($2,','))::int))
                                    	and rdc.data_value is not null",
                                    params=list(template$reporting_asof_date,
                                                #na.omit because data_source_row_id will have NA indicator_id
                                                paste0(na.omit(unique(template_current_lookup_data$indicator_id)),collapse=",")))
              setDT(matches)
              
              if (template_has_static_row_ids) {
                row_ids <- dbGetQuery(conn,"
                                      select 
                                        tm.parent_rsf_pfcbl_id,
                                      	tm.reporting_template_row_groups,
                                      	tm.indicator_id,
                                      	tm.indicator_sys_category,
                                      	tri.rsf_pfcbl_id as matched_rsf_pfcbl_id
                                      from _temp_match tm 
                                      inner join p_rsf.rsf_pfcbl_reporting_template_row_ids tri on tri.parent_rsf_pfcbl_id = tm.parent_rsf_pfcbl_id
                                                                                               and tri.data_source_row_id = tm.data_value
                                                                                               and tri.template_id = $1::int
                                      inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = tri.rsf_pfcbl_id
                                      where ids.pfcbl_category = $2::text",
                                      params=list(template$template_id,
                                                  current_data_category))
                setDT(row_ids)
                matches <- rbindlist(list(matches,
                                          row_ids))
                row_ids <- NULL
              }
              
              return (matches)
              
            })
          
            setDT(template_db_current_match_data)

            if(SYS_PRINT_TIMING) debugtime("template_set_data_match_rsf_ids",paste0("db_current_match_data for ",current_data_category),format(Sys.time()-t2))
            
            template_current_lookup_data[,data_value:=NULL]
            unmatched_data <- fsetdiff(template_current_lookup_data[,
                                                 .(parent_rsf_pfcbl_id,
                                                   reporting_template_row_groups,
                                                   indicator_id,
                                                   indicator_sys_category)],
                                      template_db_current_match_data[,
                                                                     .(parent_rsf_pfcbl_id,
                                                                       reporting_template_row_groups,
                                                                       indicator_id,
                                                                       indicator_sys_category)])


            #when creating new entities with db_create_new_rsf_ids, some minimal level of ID information must be within the dataset (so that we can look it up later!)
            #so if we see a new data_source_row_id (because the template has static rows), then does that new row have an ID value in it?
            #if not, then don't score a new row as a new possible new entity
            unmatched_data[,
                           `:=`(matched_rsf_pfcbl_id=as.numeric(NA))]
  
            unmatched_data[,newable:=TRUE]
            unmatched_data[indicator_sys_category=="data_source_row_id",
                           newable:=reporting_template_row_groups %in% template_current_lookup_data[indicator_sys_category=="id",
                                                                                                    unique(reporting_template_row_groups)] ]
            
            unmatched_data <- unmatched_data[newable==TRUE]
            unmatched_data[,newable:=NULL]
            
            

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
                                                                              indicator_id,
                                                                              indicator_sys_category,
                                                                              matched_rsf_pfcbl_id)]
            
            #Filter out recognized entities that have been matched
            #Filter here so they can go through disambiguation matching in case lookup returns multiple matches.
            #They'll be re-assosciated with the best match later.
            # if (!empty(template_match_data[pfcbl_category==current_data_category & 
            #                                is.na(recognized_entity_number) == FALSE])) {
            #   
            #   unmatched_recognized_rows <- template_current_lookup_data[,
            #                                                             .(nomatches=all(is.na(matched_rsf_pfcbl_id))),
            #                                                             by=.(reporting_template_row_group)
            #                                                             ][nomatches==TRUE
            #                                                               ][template_match_data[!is.na(recognized_entity_number),
            #                                                                                     .(reporting_template_row_group,
            #                                                                                       recognized_entity_number)],
            #                                                                 on=.(reporting_template_row_group),
            #                                                                 nomatch=NULL]
            #   
            #   #Filter-out unmatched rows that are recogized but have not submitted and ID information.  As these will be re-associated with those rows that
            #   #did submit that ID information.
            #   no_id_data_rows <- current_match_data[!reporting_template_row_group %in% template_current_data$reporting_template_row_group,
            #                                         .(reporting_template_row_group,
            #                                           recognized_entity_number)]
            #   
            #   #current_match_data[recognized_entity_number==2551]
            #   #template_current_lookup_data[reporting_template_row_group %in% c("9012RSFDATA","9013RSFDATA")]
            #   
            #   unmatched_recognized_rows[!reporting_template_row_group %in% no_id_data_rows]
            #   
            #   recognized_ids_with_data <- current_match_data[reporting_template_row_group %in% template_current_data$reporting_template_row_group,recognized_entity_number]
            #   
            #   # matched_recognized_rows <- template_match_data[pfcbl_category==current_data_category & 
            #   #                                                is.na(recognized_entity_number) == FALSE,
            #   #                                                 .(reporting_template_row_group,
            #   #                                                   recognized_entity_number)
            #   #                                                 ][template_current_lookup_data[,.(reporting_template_row_group,
            #   #                                                                                   matched_rsf_pfcbl_id)],
            #   #                                                   on=.(reporting_template_row_group),
            #   #                                                   nomatch=NULL][,
            #   #                                                          .(hasmatches=any(!is.na(matched_rsf_pfcbl_id))),
            #   #                                                          by=.(recognized_entity_number)
            #   #                                                          ][hasmatches==TRUE]
            #   
            #   #has to match something if it'll be matched.  Else they're all new!... and new entities will group by recognition otherwise.
            #   unmatched_recognized_rows <- unmatched_recognized_rows[recognized_entity_number %in% matched_recognized_rows$recognized_entity_number]
            # 
            #   
            #   template_current_lookup_data <- template_current_lookup_data[!(reporting_template_row_group %in% unmatched_recognized_rows$reporting_template_row_group)]
            # }
            
            
            #for entities (borrowers) that change their name and/or other info AND ALSO add a new entry on a new row,
            #then the heretofore unreported name change isn't available to match in the database.  So here, if we dcast all sys categories
            #and they're all the same on multiple rows, and some rows match existing reported rows -- and some rows match new (ie, NA), then
            #we can infer because all their ID info is the same, that this is "new" row, should match the same row.
            if (template_has_static_row_ids) {
              multiple_row_entries <- template_current_data[,.(parent_rsf_pfcbl_id,reporting_template_row_group,data_value,indicator_sys_category)]
              multiple_row_entries <- dcast.data.table(multiple_row_entries,
                                                       parent_rsf_pfcbl_id+reporting_template_row_group ~ indicator_sys_category,
                                                       value.var = "data_value")
              
              by_cols <- names(multiple_row_entries)[-which(names(multiple_row_entries) %in% c("parent_rsf_pfcbl_id","reporting_template_row_group"))]
              multiple_row_entries[,
                                   `:=`(n=.N,
                                        g=.GRP),
                                   by=by_cols]
              multiple_row_entries <- multiple_row_entries[n>1]
              
              
              matched_rows <- template_current_lookup_data[!is.na(matched_rsf_pfcbl_id) & indicator_sys_category=="data_source_row_id"]
              multiple_row_entries[matched_rows,
                                   `:=`(matched_rsf_pfcbl_id=i.matched_rsf_pfcbl_id),
                                   on=.(reporting_template_row_group,
                                        parent_rsf_pfcbl_id)]
              
              multiple_row_entries_matched <- multiple_row_entries[!is.na(matched_rsf_pfcbl_id),
                                                                   .(g,
                                                                     matched_rsf_pfcbl_id)]
              
              multiple_row_entries_matched <- unique(multiple_row_entries_matched)
              multiple_row_entries_matched[,
                                           matches:=.N,
                                           by=.(g)]
              
              multiple_row_entries_matched <- multiple_row_entries_matched[matches==1]
              multiple_row_entries_unmatched <- multiple_row_entries[is.na(matched_rsf_pfcbl_id),
                                                                     .(parent_rsf_pfcbl_id,
                                                                       reporting_template_row_group,
                                                                       g)]
              multiple_row_entries_unmatched <- multiple_row_entries_unmatched[multiple_row_entries_matched,
                                                                               on=.(g),
                                                                               nomatch=NULL]
              
              multiple_row_entries_unmatched[,indicator_sys_category:="data_source_row_id"]
              template_current_lookup_data[multiple_row_entries_unmatched,
                                           `:=`(matched_rsf_pfcbl_id=i.matched_rsf_pfcbl_id),
                                           on=.(reporting_template_row_group,
                                                parent_rsf_pfcbl_id,
                                                indicator_sys_category)]
            }
          }
          
         
          template_current_lookup_data[,actioned:=FALSE]
        }
        
        #ACTION NEW RSF IDS: Any rows where no identifying information can be matched
        {
          #Not found in database means: not already matched (no rsf_pfcbl_id) and also not matched "with confidence" using a reported data point
          
          new_entities <- template_current_lookup_data[,
                                                       .(new_entity=all(is.na(matched_rsf_pfcbl_id))),
                                                       by=.(reporting_template_row_group)][new_entity==TRUE]
          
          current_match_data[reporting_template_row_group %in% unique(new_entities$reporting_template_row_group),
                             `:=`(rsf_pfcbl_id=as.numeric(NA),
                                  match_action="new",
                                  matched_by="nomatches")]
          
          template_current_lookup_data[reporting_template_row_group %in% current_match_data[!is.na(match_action),reporting_template_row_group],
                                       actioned:=TRUE]
          
          template_current_lookup_data <- template_current_lookup_data[actioned==FALSE]
          
          if (!empty(issuance_rank_indicator)) {
            
            #entries that have a NEW rank_id AND a new entity_creation_date
            new_entries <- template_current_lookup_data[indicator_sys_category %in% c("rank_id","entity_creation_date"),
                                                          .(new_entry=all(is.na(matched_rsf_pfcbl_id))),
                                                          by=.(reporting_template_row_group)][new_entry==TRUE]
            
            #entries that matched ID
            #protocol will have clients add a sequence ID to end of ID, eg 12345#1, making these unique and not matching
            #but client may not do that and just repeat the same ID for the new issuance
            id_matches <- template_current_lookup_data[indicator_sys_category == "id" &
                                                       is.na(matched_rsf_pfcbl_id)==FALSE,
                                                         .(existing_id=length(unique(matched_rsf_pfcbl_id))==1),
                                                         by=.(reporting_template_row_group)][existing_id==TRUE]
            
            #When rank_id and/or creation_date are new but ID matches an existing entry, then it will be counted as a new entity (that is
            #an issuance of the loan series that matches the ID).
            #This block will skip the ambiguous matching, which would presumable match "new" to NA rank_id and entity_creation_date and
            #"update" to the matched loan ID.  The result would be based on how the scoring preferences are tuned.  This is a better hueuristic
            #for issuances in this situation
            issuance_matches <- new_entries[id_matches,
                                            on=.(reporting_template_row_group),
                                            nomatch=NULL]
            
            if (!empty(issuance_matches)) {
              current_match_data[is.na(match_action) &
                                 reporting_template_row_group %in% unique(issuance_matches$reporting_template_row_group),
                                 `:=`(rsf_pfcbl_id=as.numeric(NA),
                                      match_action="new",
                                      matched_by="issuance")]
              
              template_current_lookup_data[reporting_template_row_group %in% current_match_data[!is.na(match_action),reporting_template_row_group],
                                           actioned:=TRUE]
              
              template_current_lookup_data <- template_current_lookup_data[actioned==FALSE]
              
            }
            issuance_matches <- NULL
            id_matches <- NULL
            new_entries <- NULL
          }

          
          #EXIT early if all rsf_ids are matched
          if (empty(current_match_data[is.na(match_action)])) break; 
          
        }
        
        #Here: (1) we've matched hashvalues (identical information as previously submitted)
        #      (2) we've matched new IDs (nothing matched anything previously submitted)
        #Therefore, somethings match somethings previously submitted...
        
        #BLOCK: Identify unambiguous rsf_ids for update
        {
          setorder(template_current_lookup_data,
                   matched_rsf_pfcbl_id,
                   indicator_sys_category)
          
          unambiguous_match_data <- template_current_lookup_data[,
                                                                 .(unambiguous_match=length(unique(matched_rsf_pfcbl_id))==1 &
                                                                                     any(is.na(matched_rsf_pfcbl_id))==FALSE,
                                                                   matched_by=paste0("Unambiguous Match with ",
                                                                                     .N," values: ",
                                                                                     paste0(indicator_sys_category,
                                                                                            collapse=" & ")),
                                                                   matched_rsf_pfcbl_id=matched_rsf_pfcbl_id[1]),
                                                                 by=.(reporting_template_row_group,
                                                                      parent_rsf_pfcbl_id)][unambiguous_match==TRUE]
          #Unambiguous match data
          #All identifying information submitted matches only one RSF_ID
          if (!empty(unambiguous_match_data)) {
            
            current_match_data[unambiguous_match_data,
                               `:=`(rsf_pfcbl_id=i.matched_rsf_pfcbl_id,
                                    match_action="update",
                                    matched_by=i.matched_by),
                               on=.(reporting_template_row_group,
                                    parent_rsf_pfcbl_id)]
            
            
            unambiguous_match_data <- NULL
            
            template_current_lookup_data[reporting_template_row_group %in% current_match_data[!is.na(match_action),reporting_template_row_group],
                                         actioned:=TRUE]
            
            template_current_lookup_data <- template_current_lookup_data[actioned==FALSE]
          }
          
          
          #EXIT early if all matches are unambiguous
          if (empty(current_match_data[is.na(match_action)])) break; 
          
          template_current_lookup_data[current_match_data[!is.na(match_action),
                                                           .(reporting_template_row_group)],
                                         actioned:=TRUE,
                                         on=.(reporting_template_row_group)]
          
          template_current_lookup_data <- template_current_lookup_data[actioned==FALSE]
          
        }
        
        #BLOCK: Identify quasi-unambiguous rsf_ids for update (all except for create date, which should be semi-unique but may be generic and therefore spammy)
        {
          
          qunambiguous_match_data <- template_current_lookup_data[indicator_sys_category != "entity_creation_date",
                                                                 .(qunambiguous_match=length(unique(matched_rsf_pfcbl_id))==1 & 
                                                                                      .N >=2,
                                                                                      #& any(is.na(matched_rsf_pfcbl_id))==FALSE,
                                                                   matched_by=paste0("Quasi-unambiguous Match with ",
                                                                                     .N," values: ",
                                                                                     paste0(indicator_sys_category,
                                                                                            collapse=" & ")),
                                                                   matched_rsf_pfcbl_id=matched_rsf_pfcbl_id[1]),
                                                                 by=.(reporting_template_row_group,
                                                                      parent_rsf_pfcbl_id)][qunambiguous_match==TRUE]
          
        
          
          #Unambiguous match data
          #All identifying information submitted matches only one RSF_ID
          if (!empty(qunambiguous_match_data)) {
            
            current_match_data[qunambiguous_match_data,
                               `:=`(rsf_pfcbl_id=i.matched_rsf_pfcbl_id,
                                    match_action=ifelse(is.na(i.matched_rsf_pfcbl_id),
                                                              "new",
                                                              "update"),
                                    matched_by=i.matched_by),
                               on=.(reporting_template_row_group,
                                    parent_rsf_pfcbl_id)]
            
            
            qunambiguous_match_data <- NULL
            
            template_current_lookup_data[reporting_template_row_group %in% current_match_data[!is.na(match_action),reporting_template_row_group],
                                         actioned:=TRUE]
            
            template_current_lookup_data <- template_current_lookup_data[actioned==FALSE]
          }
          
          
          #EXIT early if all matches are unambiguous
          if (empty(current_match_data[is.na(match_action)])) break; 
          
          template_current_lookup_data[current_match_data[!is.na(match_action),
                                                            .(reporting_template_row_group)],
                                         actioned:=TRUE,
                                         on=.(reporting_template_row_group)]
          
          template_current_lookup_data <- template_current_lookup_data[actioned==FALSE]
          
        }
        
        #Ambiguous match data
        {
          
          #Slightly redundant, but ambiguous match data is by definition what is left over from previous matches
          ambiguous_match_data <- template_current_lookup_data[current_match_data[is.na(match_action),
                                                                                    .(reporting_template_row_group,
                                                                                      parent_rsf_pfcbl_id)],
                                                                 on=.(reporting_template_row_group,
                                                                      parent_rsf_pfcbl_id),
                                                                 nomatch=NULL
                                                               ][,
                                                                 .(matched_rsf_pfcbl_id,
                                                                   reporting_template_row_group,
                                                                   indicator_sys_category,
                                                                   parent_rsf_pfcbl_id,
                                                                   indicator_id,
                                                                   actioned)] #basically omits data_value, which can have redundant
                                                                              #matches due to id_normalized matching the same ID for compound_ids
          
          if (!empty(ambiguous_match_data)) {
            
            ambiguous_match_data <- unique(ambiguous_match_data)
            ambiguous_match_data[indicator_sys_category_match_indicators,
                                 score:=i.score,
                                 on=.(indicator_sys_category)]
            
            if (any(is.na(ambiguous_match_data$score))) {
              stop(paste0("Failed to score the following indicator_sys_categories: ",
                          paste0(ambiguous_match_data[is.na(score),unique(indicator_sys_category)],collapse=", ")))
            }
            
            ambiguous_match_data <- unique(ambiguous_match_data)
            ambiguous_match_data[template_current_data,
                                 data_value:=i.data_value,
                                 on=.(reporting_template_row_group,
                                      indicator_id,
                                      parent_rsf_pfcbl_id)]
            ambiguous_match_data[indicator_sys_category=="data_source_row_id",
                                 data_value:=reporting_template_row_group]
            
            ambiguous_match_data_scores <- ambiguous_match_data[,.(matched_by=paste0(ifelse(is.na(matched_rsf_pfcbl_id),
                                                                                     paste0("NEW ",toupper(current_data_category)),
                                                                                     paste0("MATCHES SYSID#",matched_rsf_pfcbl_id)),
                                                                                     " for ",paste0(
                                                                                       paste0("{",indicator_sys_category,
                                                                                              "=",
                                                                                              data_value,"}"),
                                                                                                    collapse=" & ")),
                                                                   match_count=.N,
                                                                   score_total=sum(score)),
                                                             by=.(reporting_template_row_group,
                                                                  parent_rsf_pfcbl_id,
                                                                  matched_rsf_pfcbl_id)]
            
            ambiguous_match_data_scores[,
                                        `:=`(reporting_template_row_group_score_total=sum(score_total),
                                             reporting_template_row_group_score_median=median(score_total)),
                                        by=.(reporting_template_row_group,
                                             parent_rsf_pfcbl_id)]
            
            ambiguous_match_data_scores[,match_score_confidence:=score_total/reporting_template_row_group_score_total]
            
            setorder(ambiguous_match_data_scores,
                     reporting_template_row_group,
                     score_total)
            
            ambiguous_match_data_scores[,selected:=FALSE]
            ambiguous_match_data_scores[,selected:=score_total==max(score_total),
                                        by=.(reporting_template_row_group,
                                             parent_rsf_pfcbl_id)]
            
            ambiguous_match_data_scores[,n:=sum(selected),
                                        by=.(reporting_template_row_group,
                                             parent_rsf_pfcbl_id)]
            
            #Between multiple ambiguous IDs or NEW vs UPDATE, select UPDATE (na.rm)
            ambiguous_match_data_scores[selected==TRUE & n>1,
                                        selected:=matched_rsf_pfcbl_id==min(matched_rsf_pfcbl_id,na.rm=TRUE),
                                        by=.(reporting_template_row_group)]
            
            ambiguous_match_data_scores[is.na(selected),
                                        selected:=FALSE]
            
            selected_ambiguous_match_data <- ambiguous_match_data_scores[selected==TRUE]
            
            selected_ambiguous_match_data[is.na(matched_rsf_pfcbl_id),
                                          match_issues:=paste0(" CREATE NEW ",toupper(current_data_category)," on ROW ",reporting_template_row_group)]
            
            selected_ambiguous_match_data[!is.na(matched_rsf_pfcbl_id),
                                          match_issues:=matched_by]
            
            unselected_ambiguous_match_data <- ambiguous_match_data_scores[selected==FALSE]
            
            unselected_ambiguous_match_data <- unselected_ambiguous_match_data[score_total>=reporting_template_row_group_score_median |
                                                                                 match_score_confidence > 0.3]
            
            unselected_info <- unselected_ambiguous_match_data[,.(current_info=paste0(unique(matched_by),collapse=" AND ALSO ")),
                                                               by=.(parent_rsf_pfcbl_id,
                                                                    reporting_template_row_group)]
            
            unselected_info[,current_info:=gsub("MATCHES ","",current_info)]
            
            selected_ambiguous_match_data[,unselected_info:=as.character(NA)]
            selected_ambiguous_match_data[unselected_info,
                                          unselected_info:=paste0(" -- with uncertainty because partial-match with: ",i.current_info),
                                          on=.(reporting_template_row_group,
                                               parent_rsf_pfcbl_id)]
            
            selected_ambiguous_match_data[is.na(unselected_info),
                                          match_issues:=as.character(NA)]
            
            selected_ambiguous_match_data[!is.na(unselected_info),
                                          match_issues:=paste0(match_issues,unselected_info)]
            
            current_match_data[selected_ambiguous_match_data[is.na(matched_rsf_pfcbl_id)==FALSE],
                                  `:=`(rsf_pfcbl_id=i.matched_rsf_pfcbl_id,
                                       match_action="update",
                                       matched_by=i.matched_by,
                                       match_issues=i.match_issues),
                                  on=.(reporting_template_row_group,
                                       parent_rsf_pfcbl_id)]
            
            new_ids <- selected_ambiguous_match_data[is.na(matched_rsf_pfcbl_id)==TRUE,
                                                     .(reporting_template_row_group,
                                                       parent_rsf_pfcbl_id,
                                                       matched_by,
                                                       match_issues)]
          
            if (!empty(new_ids)) {

              current_match_data[unique(new_ids),
                                 `:=`(rsf_id=as.numeric(NA),
                                      rsf_pfcbl_id=as.numeric(NA),
                                      match_action="new",
                                      matched_by=i.matched_by,
                                      match_issues=i.match_issues),
                                 on=.(reporting_template_row_group,
                                      parent_rsf_pfcbl_id)]
            }  
            
          }
        }
        
        
        recognized_actions <- current_match_data[!is.na(match_action) & !is.na(recognized_entity_number),
                                                 .(rsf_pfcbl_id,
                                                   match_action,
                                                   recognized_entity_number)]
        
        if (!empty(recognized_actions)) {
          recognized_actions[,n:=nrow(unique(.SD)),by=.(recognized_entity_number)]
          recognized_actions <- recognized_actions[n==1]
          recognized_actions[,n:=NULL]
          
          recognized_actions <- current_match_data[is.na(match_action) &
                                                   !is.na(recognized_entity_number),
                                                   .(reporting_template_row_group,
                                                     recognized_entity_number)
                                ][recognized_actions,
                                  on=.(recognized_entity_number)]
          
          current_match_data[recognized_actions,
                             `:=`(rsf_pfcbl_id=i.rsf_pfcbl_id,
                                  match_action=i.match_action,
                                  matched_by='Recognized entity'),
                             on=.(reporting_template_row_group,
                                  recognized_entity_number)]
        }
        
        if (!empty(current_match_data[is.na(match_action)])) {
          bad_actions <- current_match_data[is.na(match_action)]
          bad_data <- template_data[reporting_template_row_group %in% bad_actions$reporting_template_row_group
                                    & data_category == current_data_category
                                    & indicator_sys_category %in% indicator_sys_category_match_indicators$indicator_sys_category,
                        .(reporting_template_row_group,
                          indicator_name,
                          data_submitted,
                          data_flags_new)]
          bad_data[,flags:=lapply(data_flags_new,function(dt) {
            if (!empty(dt)) {
              paste0(paste0("[",dt$check_name,": ",dt$check_message),"]",collapse=" AND ALSO")
            } else { "" }
          })]
          bad_data[,error:=paste0(indicator_name,"=",data_submitted," ",flags)]
          bad_data <- bad_data[,.(error=paste0(error,collapse = ";  ")),
                               by=.(reporting_template_row_group)]
          bad_data <- bad_data[,paste0("Error on row ",reporting_template_row_group,": ",error)]
          bad_data <- paste0(bad_data,collapse=" + + + + ")
          stop(paste0("Failed to match all IDs for '",current_data_category,"' on rows ",
                      paste0(current_match_data[is.na(match_action),unique(reporting_template_row_group)],collapse=","),
                      ". Ambiguities in the reporting prevented entity from being neither Uniquely Recognized nor New.  Verify on these rows that ",current_data_category," IDs are correct: ",
                      bad_data))
        }
        
        break;
      }
      
      #NEW IDS
      {
        if (!empty(current_match_data[is.na(rsf_pfcbl_id)])) {
          
          if (!all(current_match_data[is.na(rsf_pfcbl_id),unique(match_action)]=="new")) {
            stop("Failed to match rsf_pfcbl_id and match_action specifies: ",paste0(current_match_data[is.na(rsf_pfcbl_id),unique(match_action)],collapse = " & "))            
          }
          
          if (current_data_category=="program") {
                                    
            stop(paste0("Attempted to create new ",toupper(current_data_category)," within ID matching: however ",current_data_category,
                        " IDs should be created through other setup processes"))
          }

          create_new_defined <- NULL
          create_new_undefined <- NULL
          
          #pfcbl_id can pre-assign new groups on template read-in without need to group by data-values across rows (pfcbl templates 
          #will also read-in "long" data and therefore not have parent-child relationships on individual rows)
          {
            #This is semi-obsoleted and replaced by recognized_entity_number
            #But as it works and as it is used by setup template, it's left in.
            if (template$template_ids_method=="pfcbl_id" && any(names(template_data)=="SYSID") && !empty(template_data[SYSID < 0])) {
              
              #for defined, they'are already defined...we don't care if there are redundancies or in any way aboud the data_value values.
              
              create_new_defined <- template_data[SYSID < 0 &
                                                  data_category == current_data_category,
                                                  .(unique_new_entity_row_id=-SYSID,
                                                    reporting_template_row_group)
                                                  ][current_match_data[is.na(rsf_pfcbl_id),
                                                                       .(reporting_template_row_group,
                                                                         parent_rsf_pfcbl_id)],
                                                    on=.(reporting_template_row_group),
                                                    nomatch=NULL]
              
              create_new_defined <- create_new_defined[,
                                                       .(reporting_template_row_groups=list(unique(reporting_template_row_group))),
                                                       by=.(unique_new_entity_row_id,
                                                            parent_rsf_pfcbl_id)]
              
              #pfcbl_id templates may (more likely) have multiple reporting periods within the same template
              #where entities need to be created, ENSURE THEY ARE CREATED IN THE CORRECT TIMELINE
              creation_asof_dates <- template_data[SYSID < 0 &
                                                   data_category == current_data_category &
                                                   #Not everyone has a creation date, eg, facilities
                                                   #Everyone must report an ID, but sometimes it's blank and reported later, eg, loans
                                                   indicator_sys_category %in% c("entity_creation_date","id"),
                                                   .(unique_new_entity_row_id=-SYSID,
                                                     reporting_asof_date)
                                                   ][,
                                                     .(creation_asof_date=min(reporting_asof_date,na.rm=T)),
                                                     by=.(unique_new_entity_row_id)]
              
              create_new_defined[creation_asof_dates,
                                 creation_asof_date:=i.creation_asof_date,
                                 on=.(unique_new_entity_row_id)]
              
              create_new_defined[is.na(creation_asof_date),
                                 creation_asof_date:=template$reporting_cohort$reporting_asof_date]
              
              setorder(create_new_defined,
                       unique_new_entity_row_id,
                       creation_asof_date,
                       parent_rsf_pfcbl_id)
            } 
            
            else {
              
              creation_data <- template_data[data_category==current_data_category & 
                                             indicator_sys_category %in% indicator_sys_category_match_indicators[use_create_new==TRUE,
                                                                                                                 indicator_sys_category],
                                             .(reporting_template_row_group,
                                               for_pfcbl_category=data_category,
                                               indicator_sys_category,
                                               data_value,
                                               reporting_asof_date)]
  
              {
                duplicates <- creation_data[,
                                            .(duplicated=.N),
                                            by=names(creation_data)][duplicated>1]
                
                #if there are both duplicates and unique duplicates, it's a problem.  Otherwise we just remove duplicates.
                if (!empty(duplicates)) {
                  
                  duplicates <- duplicates[,.(uduplicates=length(unique(data_value))),
                                           by=names(duplicates)][uduplicates>1]
                  
                  if (!empty(duplicates)) {
                    duplicates <- duplicates[,
                                             .(message=paste0(reporting_template_row_group," ambiguous ",toupper(indicator_sys_category)," as: ",paste0("{",data_value,"}",collapse=" & "),"\n")),
                                             by=.(reporting_template_row_group,indicator_sys_category)]
                    stop(paste0("Template reports ambiguous values in multiple columns:\n",
                                paste0(duplicates$message,collapse=" \n")))
                  } else {
                    creation_data <- unique(creation_data)
                  }
                }
                duplicates <- NULL
              }
              
              
              creation_data <- current_match_data[is.na(rsf_pfcbl_id),
                                               .(reporting_template_row_group,
                                                 parent_rsf_pfcbl_id, 
                                                 parent_pfcbl_category=parent_data_category, 
                                                 for_pfcbl_category=pfcbl_category,
                                                 recognized_entity_number)
                                                ][creation_data,
                                                    on=.(reporting_template_row_group,
                                                         for_pfcbl_category),
                                                    nomatch=NULL]
              
              if (!empty(creation_data)) {
                
                setorder(creation_data,
                         reporting_template_row_group,
                         parent_rsf_pfcbl_id,
                         parent_pfcbl_category,
                         for_pfcbl_category,
                         indicator_sys_category,
                         data_value)
                
                if (any(creation_data$parent_pfcbl_category != parent_data_category)) {
                  stop(paste0("Mismatch between create_new_undefined$parent_pfcbl_category and ",parent_data_category))
                }
                
                if (any(creation_data$for_pfcbl_category != current_data_category)) {
                  stop(paste0("Mismatch between create_new_undefined$for_pfcbl_category and ",current_data_category))
                }
             
                #Because parent entities may be new and also repeated across multiple rows.
                #Eg, a new borrower this quarter has taken two new loans.  Ensure that only one new borrower is created.
                #(hopefully) that borrower's ID value information, name and ID, etc, will be the same across all the template rows.
                #If so, dcast will enable grouping by submitted values and only one borrower will be created for the two new loans.
                #If not, presumably checks will throw critical errors 
                create_new_undefined <- dcast.data.table(creation_data,
                                                         reporting_template_row_group + 
                                                         parent_rsf_pfcbl_id + 
                                                         parent_pfcbl_category + 
                                                         recognized_entity_number +
                                                         for_pfcbl_category ~ 
                                                         indicator_sys_category,
                                                         value.var="data_value")
                
                if (!any(names(create_new_undefined)=="id")) {
                  stop(paste0("Attempted to create new ",current_data_category," but could not resolve ID value in the data template.  ID is a minimally required field."))
                }
                
                #NOTE: !important!
                #cannot filter-out NA ID values.
                #Actually, many loans will start with NA id values if comitted before an ID is internally assigned.            
                # if (anyNA(create_new$id)) {
                #   stop(paste0("Attemptd to create new ",current_data_category," but ID is {MISSING} for the following rows: ",
                #               paste0(create_new[is.na(id),reporting_template_row_group],collapse=",")))
                # }
                
                id_cols <- names(create_new_undefined)[-which(names(create_new_undefined) %in% c("reporting_template_row_group",
                                                                                                 "parent_rsf_pfcbl_id",
                                                                                                 "parent_pfcbl_category",
                                                                                                 "for_pfcbl_category",
                                                                                                 "recognized_entity_number"))]
                
                
                #This check basically inserted as a result of database unique index violations on rsf_data_current_names_and_ids
                #Where, eg, borrowers with same and and ID have different establishment dates.  Previously, this was a check.  But with a table and index for
                #names and IDs, this can now result in an insert violation.  So it's controlled here.
                #But new borrowers should be unique by NAME and by ID
                #And new loans should be unique by ID and Rank: since clients that re-report same ID for revolvers, each revolver may have the same ID but a different
                #setup date and, accordingly, a different rank. And for clients that report on tranche, the tranche ID will be unique but also generate a unique inclusion rank.
                name_id_cols <- id_cols[which(id_cols %in% c("id","name","rank_id"))]
                duplicate_name_ids <- create_new_undefined[,
                                                           n:=nrow(unique(.SD)),
                                                           by=name_id_cols,
                                                           .SDcols=id_cols][n>1]
                if (!empty(duplicate_name_ids)) {
                  setorderv(duplicate_name_ids,
                            cols=unique(c(name_id_cols,id_cols,"reporting_template_row_group")))
                  
                  duplicate_name_ids <- duplicate_name_ids[,.SD,
                                                           .SDcols = c("reporting_template_row_group",id_cols)]
                  
                  stop(paste0("Failed to create NEW ",toupper(current_data_category)," due to inconsistent values for: ",paste0(id_cols,collapse=", "),"\n\n",
                              paste0(apply(duplicate_name_ids,1,paste,collapse=" "),collapse=" \n")),
                       "\n\nThese values can change over time...but when creating a new ",toupper(current_data_category)," these must all be the same as-of ",
                       template$reporting_asof_date)
                }
                
                duplicate_name_ids <- NULL
                create_new_undefined[,
                                     n:=NULL]
                
                create_new_undefined[,row_number:=as.numeric(gsub("^([[:digit:]]+).*$","\\1",reporting_template_row_group))]
                
                #Ensures new entity IDs are ordered by the reporting row in which they appear in the upload
                setorder(create_new_undefined,
                         row_number)
                
                if (any(!is.na(create_new_undefined$recognized_entity_number))) {
                  
                  first_cols <- c("recognized_entity_number",id_cols)
                  
                  first_recognized_entity <- create_new_undefined[!is.na(recognized_entity_number)
                                             ][unique(creation_data[,.(reporting_template_row_group,reporting_asof_date)]),
                                               on=.(reporting_template_row_group)]
                  setorder(first_recognized_entity,
                           reporting_asof_date,
                           -row_number) #within the reporting dates, we want the last (latest) row first as "currentest data"
                  
                  first_recognized_entity[,
                                          n:=1:.N,
                                          by=.(recognized_entity_number)]
                  
                  first_recognized_entity <- first_recognized_entity[n==1,
                                                                     ..first_cols]
                  
                  first_recognized_entity <- first_recognized_entity[current_match_data[,
                                                                                        .(reporting_template_row_group,
                                                                                          parent_rsf_pfcbl_id, 
                                                                                          parent_pfcbl_category=parent_data_category, 
                                                                                          for_pfcbl_category=pfcbl_category,
                                                                                          recognized_entity_number)],
                                          on=.(recognized_entity_number)]
                  
                  #just to allow merge, removed subsequently
                  first_recognized_entity[,row_number:=.I]
                  
                  setcolorder(first_recognized_entity,
                              neworder=names(create_new_undefined))
                  
                  
                  create_new_undefined <- create_new_undefined[!(recognized_entity_number %in% unique(first_recognized_entity$recognized_entity_number))]
                  create_new_undefined <- rbindlist(list(create_new_undefined,
                                                         first_recognized_entity))
                  
                }
                
                create_new_undefined[,row_number:=NULL]
                
                #all indicator_sys_category files are NA for the reporting row
                missing_ids <- which(sapply(as.data.frame(t(create_new_undefined[,..id_cols])),function(x) all(is.na(x))))
                if (length(missing_ids) > 0) {
                  bad_rows <- create_new_undefined[missing_ids,reporting_template_row_group]
                  stop(paste0("Failed to create new ",toTitleCase(current_data_category)," entities because ",
                              paste0(id_cols,collapse=", "),
                              " columnas are ALL {MISSING} on ROWS ",
                              paste0(bad_rows,collapse=" & "),
                              ". At least one non-missing ",current_data_category," ID value must be reported on these rows."))
                }
                
                group_cols <- names(create_new_undefined)[-which(names(create_new_undefined)=="reporting_template_row_group")]
                create_new_undefined[,
                                     unique_new_entity_row_id:=.GRP,
                                     by=group_cols]
                
                
                #Now we now which entities are reported across multiple rows, we can look back to see if there are multiple creation dates, too.
                creation_data <- creation_data[,.(creation_asof_date=min(reporting_asof_date,na.rm=T)),
                                               by=.(reporting_template_row_group,
                                                    parent_rsf_pfcbl_id)]
                creation_data[create_new_undefined,
                              unique_new_entity_row_id:=i.unique_new_entity_row_id,
                              on=.(reporting_template_row_group,
                                   parent_rsf_pfcbl_id)]
                
                creation_data <- creation_data[,
                                               .(creation_asof_date=min(creation_asof_date,na.rm=T)),
                                               by=.(unique_new_entity_row_id,
                                                    parent_rsf_pfcbl_id)]
                
                create_new_undefined <- create_new_undefined[,
                                                             .(reporting_template_row_groups=list(reporting_template_row_group)),
                                                             by=.(unique_new_entity_row_id,
                                                                  parent_rsf_pfcbl_id)]
                
                create_new_undefined[creation_data,
                                     creation_asof_date:=i.creation_asof_date,
                                     on=.(unique_new_entity_row_id,
                                          parent_rsf_pfcbl_id)]
                
                create_new_undefined[is.na(creation_asof_date),
                                     creation_asof_date:=template$reporting_cohort$reporting_asof_date]
                
                setorder(create_new_undefined,
                         unique_new_entity_row_id,
                         creation_asof_date,
                         parent_rsf_pfcbl_id)
              }
            }
          }
          
          #Should never have opportunity to have differently defined new entities, since template will either be pfcbl_id or rsf_id method
          #just a convenience here
          create_new <- rbindlist(list(create_new_defined,
                                       create_new_undefined))       
          
          create_new[,
                     unique_new_entity_row_id:=1:.N]
          
          create_new[,count:=sapply(reporting_template_row_groups,length)]
         
            
          if (sum(create_new$count) != nrow(current_match_data[match_action=="new"])) {
            stop(paste0("Expected to create ",nrow(current_match_data[match_action=="new"])," new ",current_data_category,"s. ",
                        "But match data resulted in ",sum(create_new$count)," entities"))
          }
            

          new_pfcbl_ids <- db_create_new_rsf_ids(pool=pool,
                                                 reporting_cohort=template$reporting_cohort,
                                                 parent_pfcbl_category=parent_data_category,
                                                 for_pfcbl_category=current_data_category,
                                                 new_ids=create_new[,
                                                                    .(unique_new_entity_row_id,
                                                                      parent_rsf_pfcbl_id,
                                                                      creation_asof_date)])
          create_new[new_pfcbl_ids,
                     `:=`(rsf_id=i.new_rsf_pfcbl_id,
                          rsf_pfcbl_id=i.new_rsf_pfcbl_id),
                     on=.(unique_new_entity_row_id,
                          parent_rsf_pfcbl_id)]
          
          create_new <- create_new[,
                                   .(reporting_template_row_group=unlist(reporting_template_row_groups,recursive=F)),
                                   by=.(unique_new_entity_row_id,
                                        parent_rsf_pfcbl_id,
                                        rsf_pfcbl_id,
                                        rsf_id)]
          
          current_match_data[create_new,
                             `:=`(rsf_id=i.rsf_id,
                                  rsf_pfcbl_id=i.rsf_pfcbl_id),
                             on=.(reporting_template_row_group,
                                  parent_rsf_pfcbl_id)]
          new_pfcbl_ids <- NULL
          create_new <- NULL
       }
      }
      
      #if template has static row IDs and any IDs for this category are unmatched,
      #then entity must have reported on a new row ID (probably because it's either a newly created entity OR its parent is repeated, ie an
      #existing borrower takes a new loan)
      if (template_has_static_row_ids==TRUE) {
        
        matched_row_ids <- template_db_current_match_data[indicator_sys_category=="data_source_row_id",
                                       .(reporting_template_row_group=as.character(
                                         unlist(
                                           strsplit(reporting_template_row_groups,
                                                    split=",",
                                                    fixed=T),
                                           recursive=F))),
                                       by=.(parent_rsf_pfcbl_id,
                                            indicator_id,
                                            indicator_sys_category,
                                            matched_rsf_pfcbl_id)
                                       ][,.(rsf_pfcbl_id=matched_rsf_pfcbl_id,
                                            parent_rsf_pfcbl_id,
                                            reporting_template_row_group)]
        matched_row_ids[,exists:=TRUE]
        
        new_row_ids <- matched_row_ids[current_match_data,
                                       on=.(rsf_pfcbl_id,
                                            parent_rsf_pfcbl_id,
                                            reporting_template_row_group)
                                       ][is.na(exists)==TRUE & 
                                         is.na(rsf_pfcbl_id)==FALSE &
                                         is.na(reporting_template_row_group)==FALSE,
                                         .(rsf_pfcbl_id,
                                           reporting_template_row_group)]
        new_row_ids <- unique(new_row_ids)
        # first reporting_cohort_id to register this data_source_row_id will take credit for the reporting line 
        # note: if an old cohort is deleted and newer ones still exist, this data_source_row_id may no longer be
        # registered...but deleting data out of time will have much bigger impacts on (mis)calculated data!
        #conn <- poolCheckout(pool)
        #poolReturn(conn)
        #dbBegin(conn)
        #dbRollback(conn)
        poolWithTransaction(pool,function(conn) {
          
          dbExecute(conn,"
                    create temp table _new_row_ids(rsf_pfcbl_id int,
                                                   reporting_template_row_group text)
                    on commit drop;")
          
          dbAppendTable(conn,
                        name="_new_row_ids",
                        value=new_row_ids)
          
          dbExecute(conn,"
                    insert into p_rsf.rsf_pfcbl_reporting_template_row_ids(rsf_pfcbl_id,
                																													 reporting_cohort_id,
                																													 data_source_row_id,
                																													 template_id,
                																													 parent_rsf_pfcbl_id)
                  	select
                  		nri.rsf_pfcbl_id,
                  		$1::int,
                  		nri.reporting_template_row_group as data_source_row_id,
                  		$2::int,
                  	  ids.parent_rsf_pfcbl_id
                  	from _new_row_ids nri
                  	inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = nri.rsf_pfcbl_id
                		on conflict 
                		do nothing; -- first reporting cohort per data_source_row_id per template to be registered
                    ",
                    params=list(template$reporting_cohort$reporting_cohort_id,
                                template$template_id))
          
        })
        
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
        
        
        #This should hopefully now also be obsolete!?
        template_match_data[,
                            n:=.N,
                            by=.(reporting_template_row_group,
                                        pfcbl_category)]
        
        if (any(template_match_data$n > 1)) {
          new_updates <- template_match_data[n>1,.(new_update=all(match_action %in% c("new","update")),
                                                   matched_by=paste0(na.omit(unique(fcase(match_action=="update",matched_by,
                                                                            default=NA)),
                                                                     collapse=" AND ALSO "))),
                                             by=.(reporting_template_row_group,
                                                  pfcbl_category)][new_update==TRUE]
          
          if (!empty(new_updates)) {
            new_updates[,message:=paste0(toTitleCase(pfcbl_category)," specified as NEW matched existing ",pfcbl_category,". ",matched_by)]
            message <- paste0(new_updates$message,collapse=" & ")
            stop("Failed to create NEW entity because an existing entity already exists with the same identifiers: ",
                 message,
                 ". If you want to update information for the existing entity, do so via the Dashboard and not through 'Create New'. ",
                 " If you are certain that you want to create a new entity, ensure that it has unique identifying information, name, id number, etc")
          }
          
          multiples <- template_match_data[n>1,.(message=paste0(na.omit(unique(paste0(toupper(match_action)," ",toupper(pfcbl_category),": Because ",matched_by)),
                                                                   collapse=" AND ALSO "))),
                                             by=.(reporting_template_row_group,
                                                  pfcbl_category)]
          message <- paste0(multiples$message,collapse=" & ")
          stop(paste0("Template match data and current match data matched the same reporting_template_row_group and pfcbl_category and identified different match actions: ",
                      message))
        }
        
        template_match_data[,n:=NULL]
        

        if (!empty(template_match_data[pfcbl_category==current_data_category & is.na(rsf_pfcbl_id)==TRUE])) {
          
          stop("Template has missing rsf_pfcbl_ids")
        }
      }    
      
    } #NEXT ENTITY
    
  }
  
  if (!all(template_match_data$match_action %in% c("new","update","unchanged"))) {
    stop("template_match_data match actions not defined as: new, update or unchanged")
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

