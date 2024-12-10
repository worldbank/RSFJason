db_create_new_rsf_pfcbl_ids <- function(pool,
                                  reporting_cohort,
                                  parent_pfcbl_category,
                                  for_pfcbl_category,
                                  new_ids)
{
  
  #useful article on explain (analyze,buffers) and Postgres performance
  #https://postgres.ai/blog/20220106-explain-analyze-needs-buffers-to-improve-the-postgres-query-optimization-process
  {
    t1 <- Sys.time()  
    #if(SYS_PRINT_TIMING) debugtime("db_create_new_rsf_pfcbl_ids","start")
    # rc <<- as.data.frame(reporting_cohort)
    # ni <<- as.data.frame(new_ids)
    # 
    # reporting_cohort <- as.data.table(rc)
    # new_ids <- as.data.table(ni)
    if (length(parent_pfcbl_category) != 1 || all(is.na(parent_pfcbl_category))) {
      stop("One parent_pfcbl_category must be provided")
    }
    
    if (length(for_pfcbl_category) != 1 || all(is.na(for_pfcbl_category))) {
      stop("One for_pfcbl_category must be provided")
    }
    
    #new_row_id required because reporting_template_row_group MAY not be unique when one entity is being created across multiple reporting rows
    #For example: one new borrower is added on MULTIPLE rows because in the same first reporting quarter it takes MULTIPLE loans, ie,
    #in 2020Q4 Borrower ACME is on row20 and row21 with loans 123A and 123B, each loan is uniquely new per row; but only one borrower should be created!
    #Update: 2023-12-13: Removed requirement for reporting_template_row_group since only unique_new_entity_row_id is relevant and ensuring that it's 
    #sordered in the appropriate create-order of template_reporting_row_id can be the onus of the calling function.
    new_cols <- c("unique_new_entity_row_id",
                  "parent_rsf_pfcbl_id",
                  "creation_asof_date")
    
    if (!all(new_cols %in% names(new_ids))) stop("new_rsf_pfcbl_ids must define columns: parent_rsf_pfcbl_id, unique_new_entity_row_id")
    new_rsf_pfcbl_ids <- new_ids[,..new_cols]
    
    #That the Excel/CSV row order rank will correspond with the rsf_pfcbl_id rank, ie, those with higher row numbers are presumably
    #added to the portfolio later and are newer, and so the database will maintain than rank ordering.
    # new_ids[,row_number:=as.numeric(gsub("^([[:digit:]]+).*$","\\1",reporting_template_row_group))]
    # 
    # #Ensures new entity IDs are ordered by the reporting row in which they appear in the upload
    # setorder(new_ids,
    #          row_number)
    # new_ids[,row_number:=NULL]
    # 
    # group_by_cols <- names(new_ids)[-which(names(new_ids) == "reporting_template_row_group")]
    # 
    # 
    # new_rsf_pfcbl_ids <- new_ids[,
    #                        .(reporting_template_row_groups=paste0(unique(reporting_template_row_group),collapse=",")),
    #                        by=group_by_cols]
    
    
    #if (!empty(new_rsf_pfcbl_ids[,.(n=.N),by=.(reporting_template_row_groups)][n>1])) stop("unique_new_entity_row_id row_id is not unique across ")
  
    setorder(new_rsf_pfcbl_ids,
             creation_asof_date,
             unique_new_entity_row_id) #Important: row_id sort will mean first-to-appear in dataset will receive first next val ID in database and rsf_ids will be chronological, a last-resort for any sorting/disambiguation needs
    
    # new_rsf_pfcbl_ids <- new_rsf_pfcbl_ids[,.(unique_new_entity_row_id,
    #                               parent_rsf_pfcbl_id,
    #                               reporting_template_row_groups)]
  
    #for_pfcbl_category <- unique(new_rsf_pfcbl_ids[,for_pfcbl_category])
    for_pfcbl_category_col <- paste0("rsf_",for_pfcbl_category,"_id")
    parent_pfcbl_category_col <- paste0("rsf_",parent_pfcbl_category,"_id")
    
    if (length(for_pfcbl_category) != 1) stop(paste0("Only one for_pfcbl_category allowed per function call.  Received: ",paste0(for_pfcbl_category,collapse=" & ")))
    if (length(for_pfcbl_category_col) != 1) stop(paste0("Only one for_pfcbl_category allowed per function call.  Received: ",paste0(for_pfcbl_category_col,collapse=" & ")))
    if (length(parent_pfcbl_category_col) != 1) stop(paste0("Only one for_pfcbl_category allowed per function call.  Received: ",paste0(parent_pfcbl_category_col,collapse=" & ")))
    
    rsf_table_name <- switch(for_pfcbl_category,
                             program="rsf_programs",
                             facility="rsf_facilities",
                             client="rsf_clients",
                             borrower="rsf_borrowers",
                             loan="rsf_loans",
                             NA)
    
    # rsf_sequence_name <- switch(for_pfcbl_category,
    #                             program="nextval('p_rsf.rsf_programs_program_id_seq'::regclass)",
    #                             facility="nextval('p_rsf.rsf_program_facilities_facility_id_seq'::regclass)",
    #                             client="nextval('p_rsf.rsf_facility_clients_rsf_client_id_seq'::regclass)",
    #                             borrower="nextval('p_rsf.rsf_client_borrowers_rsf_borrower_id_seq'::regclass)",
    #                             loan="nextval('p_rsf.rsf_borrower_loans_rsf_loan_id_seq'::regclass)")
    
    
    if (any(as.character(new_ids$creation_asof_date) != as.character(reporting_cohort$reporting_asof_date))) {
      if (anyNA(new_ids$creation_asof_date)) stop("NA creation_asof_date passed to db_create_new_rsf_pfcbl_ids")
      message(paste0("Call to create new ",for_pfcbl_category," outside of current reporting timeline ",as.character(reporting_cohort$reporting_asof_date)))
    }
    
    new_ids[,
            creation_group:=.GRP,
            by=.(creation_asof_date)]
    
  }
  
  
  #lobstr::obj_size(new_rsf_pfcbl_ids)
  pfcbl_ids <- NULL
  {
    creation_groups <- sort(unique(new_ids$creation_group))
    for (g in creation_groups) {
      
      id_group <- new_ids[creation_group==g]
      id_group_creation_date <- unique(id_group$creation_asof_date)
      setorder(id_group,
               unique_new_entity_row_id)
      {
        #create new entities as a child group of the primary reporting group
        parent_reporting_cohort_id <- NULL
        if (id_group_creation_date==reporting_cohort$reporting_asof_date) {
          parent_reporting_cohort_id <- reporting_cohort$reporting_cohort_id
        } 
        
        #else we're being prompted to create outside the reporting timeline and we should create or lookup a linked cohort
        else {
          #Does a linked cohort alraedy exist?
          linked_cohort <- dbGetQuery(pool,"
                                         select 
                                          lc.reporting_cohort_id,
                                          lc.reporting_asof_date,
                                          lc.source_reference,
                                          lc.source_name
                                         from p_rsf.reporting_cohorts lc
                                         where lc.linked_reporting_cohort_id = $1::int
                                           and lc.reporting_asof_date = $2::date",
                                        params=list(reporting_cohort$reporting_cohort_id,
                                                    as.character(id_group_creation_date))) 
          
          if (empty(linked_cohort)) {
            linked_cohort <- db_cohort_create(pool,
                                              rsf_program_id=               reporting_cohort$rsf_program_id,
                                              reporting_user_id=            reporting_cohort$reporting_user_id,
                                              reporting_asof_date=          id_group_creation_date,
                                              data_asof_date=               id_group_creation_date,
                                              cohort_pfcbl_id=              reporting_cohort$reporting_rsf_pfcbl_id,
                                              from_reporting_template_id=   reporting_cohort$from_reporting_template_id,
                                              source_reference=             reporting_cohort$source_reference,
                                              source_name=paste0(format_asof_date_label(id_group_creation_date)," {SOURCE} ",
                                                                  reporting_cohort$source_name), 
                                              source_note=paste0("Chronology reporting for ",as.character(id_group_creation_date),
                                                                  " from template '",reporting_cohort$source_name,"' with ",
                                                                  as.character(reporting_cohort$reporting_asof_date)," reporting date"),
                                              reporting_pfcbl_categories=NA,
                                              fail_on_check_class = "none",
                                              fail_on_incomplete_cohorts = FALSE,
                                              linked_reporting_cohort_id=reporting_cohort$reporting_cohort_id)
          }
          parent_reporting_cohort_id <- linked_cohort$reporting_cohort_id
        }

        creation_cohort <- dbGetQuery(pool,"
          insert into p_rsf.reporting_cohorts(rsf_program_id,
                                              reporting_rsf_pfcbl_id,
                                              reporting_asof_date,
                                              reporting_user_id,
                                              reporting_time,
                                              data_asof_date,
                                              from_reporting_template_id,
                                              source_reference,
                                              source_name,
                                              source_note,
                                              parent_reporting_cohort_id,
                                              is_reported_cohort,
                                              is_calculated_cohort,
                                              is_redundancy_cohort)
            select 
              prc.rsf_program_id,
              prc.reporting_rsf_pfcbl_id as reporting_rsf_pfcbl_id,
              prc.reporting_asof_date as reporting_asof_date,
              $1::text as reporting_user_id,
              TIMEOFDAY()::timestamptz as reporting_time,
              prc.data_asof_date,
              prc.from_reporting_template_id,
              concat($1::text,' create/ ',prc.source_reference) as source_rference,
              prc.source_name,
              concat('SYSTEM COHORT - CREATE NEW ENTITIES for ',$1::text) as source_note,
              $2::int as parent_reporting_cohort_id,
              false as is_reported_cohort,
              false as is_calculated_cohort,
              false as is_redundancy_cohort
            from p_rsf.reporting_cohorts prc
            where prc.reporting_cohort_id = $2::int
            returning reporting_cohort_id,
                      reporting_asof_date",
         params=list(for_pfcbl_category,
                     parent_reporting_cohort_id))
      }
      
      if (empty(creation_cohort)) {
        stop("Failed to lookup or create creation_cohort_id")
      }
      
      #conn <- poolCheckout(pool); 
      #dbBegin(conn);
      #dbRollback(conn)
      #
      new_pfcbl_ids <- poolWithTransaction(pool,function(conn) {
        
       
        dbExecute(conn,"create temp table _temp_new_rsf_pfcbl_ids(unique_new_entity_row_id int,
                                                            parent_rsf_pfcbl_id int,
                                                            new_rsf_pfcbl_id int,
                                                            new_rsf_pfcbl_id int)
                        ON COMMIT DROP")
        
        dbAppendTable(conn=conn,
                      name="_temp_new_rsf_pfcbl_ids",
                      value=id_group[,.(unique_new_entity_row_id,
                                        parent_rsf_pfcbl_id)])
        
        #dbExecute(conn,"alter table _temp_new_rsf_pfcbl_ids add primary key(unique_new_entity_row_id)")
        dbExecute(conn,"analyze _temp_new_rsf_pfcbl_ids")
        
        #if(SYS_PRINT_TIMING) debugtime("db_create_new_rsf_pfcbl_ids","_temp_new_rsf_pfcbl_ids created",as.numeric(Sys.time()-t1,"secs"))
    
        #3.0ms
        
        dbExecute(conn,"
                  update _temp_new_rsf_pfcbl_ids tnri
                  set new_rsf_pfcbl_id = nextval('p_rsf.rsf_pfcbl_ids_rsf_pfcbl_id_seq'::regclass)::int")
        
        #x <- dbGetQuery(conn,"select * from _temp_new_rsf_pfcbl_ids");x
        #if(SYS_PRINT_TIMING) debugtime("db_create_new_rsf_pfcbl_ids","_temp_new_rsf_pfcbl_ids keys and new IDs",as.numeric(Sys.time()-t1,"secs"))
        #5.2ms
        # x<-dbGetQuery(conn,"select distinct
        # ids.rsf_id as parent_rsf_id,
        # nri.new_rsf_pfcbl_id
        # from _temp_new_rsf_pfcbl_ids nri
        # inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = nri.parent_rsf_pfcbl_id")
        # 
        
        #not useful for loans and borrowers...maybe not even for clients?
        if (for_pfcbl_category %in% c("program","facility","client")) {
          dbExecute(conn,
                    glue("insert into p_rsf.{rsf_table_name}({parent_pfcbl_category_col},{for_pfcbl_category_col})
                         select distinct
                          ids.rsf_pfcbl_id as parent_rsf_id,
                          nri.new_rsf_pfcbl_id
                         from _temp_new_rsf_pfcbl_ids nri
                        inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = nri.parent_rsf_pfcbl_id"))
        }
        
        dbExecute(conn,"alter table _temp_new_rsf_pfcbl_ids add primary key(new_rsf_pfcbl_id)")
        #dbExecute(conn,"create unique index _temp_new_rsf_pfcbl_ids_rsf_udx on _temp_new_rsf_pfcbl_ids(new_rsf_pfcbl_id);")
        #25ms
        
        t2 <- Sys.time()
        
        dbExecute(conn,"
                  insert into p_rsf.rsf_pfcbl_ids(rsf_pfcbl_id,
                                                  rsf_program_id,
                                                  rsf_facility_id,
                                                  rsf_client_id,
                                                  rsf_borrower_id,
                                                  rsf_loan_id,
                                                  rsf_id,
                                                  pfcbl_category,
                                                  pfcbl_category_rank,
                                                  parent_rsf_pfcbl_id,
                                                  created_by_reporting_cohort_id,
                                                  created_in_reporting_asof_date)
        
                  select 
                    ordered_ids.rsf_id as rsf_pfcbl_id
                    ordered_ids.rsf_program_id,
                    ordered_ids.rsf_facility_id,
                    ordered_ids.rsf_client_id,
                    ordered_ids.rsf_borrower_id,
                    ordered_ids.rsf_loan_id,
                    ordered_ids.rsf_id,
                    ordered_ids.pfcbl_category,
                    ordered_ids.pfcbl_category_rank,
                    ordered_ids.parent_rsf_pfcbl_id,
                    ordered_ids.created_by_reporting_cohort_id,
                    ordered_ids.created_in_reporting_asof_date
                  from 
                  (
                  	 select 
                  		 case when rpc.pfcbl_category = 'program' 
                  		      then nids.new_rsf_pfcbl_id
                  					else pids.rsf_program_id
                  		 end as rsf_program_id,
                  		 
                  		 case when rpc.pfcbl_category = 'facility' 
                  		      then nids.new_rsf_pfcbl_id
                  					else pids.rsf_facility_id
                  		 end as rsf_facility_id,
                  		 
                  		 case when rpc.pfcbl_category = 'client' 
                  		      then nids.new_rsf_pfcbl_id
                  					else pids.rsf_client_id
                  		 end as rsf_client_id,
                  		 
                  		 case when rpc.pfcbl_category = 'borrower' 
                  		      then nids.new_rsf_pfcbl_id
                  					else pids.rsf_borrower_id
                  		 end as rsf_borrower_id,
                  		 
                  		 case when rpc.pfcbl_category = 'loan' 
                  		      then nids.new_rsf_pfcbl_id
                  					else pids.rsf_loan_id
                  		 end as rsf_loan_id,		 
                  		 
                       nids.new_rsf_pfcbl_id as rsf_id,
                  		 rpc.pfcbl_category,
                  		 rpc.pfcbl_rank as pfcbl_category_rank,
                  		 coalesce(pids.rsf_pfcbl_id,0) as parent_rsf_pfcbl_id,
                  		 $1::int as created_by_reporting_cohort_id,
                  		 $2::date as created_in_reporting_asof_date
                  
                  	from _temp_new_rsf_pfcbl_ids nids	 
                  	inner join p_rsf.rsf_pfcbl_categories rpc on rpc.pfcbl_category = $3::text
                  	-- left join is only relevant for new programs, else all other categories will have a parent
                    left join p_rsf.rsf_pfcbl_ids pids on pids.rsf_pfcbl_id = nids.parent_rsf_pfcbl_id
                  	order by 
                  	nids.new_rsf_pfcbl_id,
                  	nids.parent_rsf_pfcbl_id
                  	
                  ) as ordered_ids ",
                  params=list(creation_cohort$reporting_cohort_id,
                              creation_cohort$reporting_asof_date,
                              for_pfcbl_category))
        

        
        new_pfcbl_ids <- dbGetQuery(conn,"select distinct
                                                    nri.unique_new_entity_row_id,
                                                    nri.parent_rsf_pfcbl_id,
                                                    nri.new_rsf_pfcbl_id,
                                                    ids.rsf_pfcbl_id as new_rsf_pfcbl_id
                                                  from _temp_new_rsf_pfcbl_ids nri
                                                  inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = nri.new_rsf_pfcbl_id
                                                                                    and ids.pfcbl_category = $2::text
                                                  where ids.created_by_reporting_cohort_id = $1::int",
                                    params=list(creation_cohort$reporting_cohort_id,
                                                for_pfcbl_category))

        if(SYS_PRINT_TIMING) debugtime("db_create_new_rsf_pfcbl_ids",nrow(new_pfcbl_ids)," new ",toupper(for_pfcbl_category)," IDs created in database in ",format(Sys.time()-t2))                                
        #dbExecute(conn,"drop table _temp_new_rsf_pfcbl_ids")
                  
                  
                  
        # new_pfcbl_ids[["reporting_template_row_groups"]] <- lapply(new_pfcbl_ids[["reporting_template_row_groups"]],
        #                                                            function(x) as.character(strsplit(x,
        #                                                                                              split=',',
        #                                                                                              fixed=T)[[1]]))
                  
        new_pfcbl_ids
      })
      
      pfcbl_ids <- rbindlist(list(pfcbl_ids,
                                  new_pfcbl_ids))
      new_pfcbl_ids <- NULL
    }
  }  

  
#  if(SYS_PRINT_TIMING) debugtime("db_create_new_rsf_pfcbl_ids","completed database actions in ",format(Sys.time()-t1))

  
  if(SYS_PRINT_TIMING) debugtime("db_create_new_rsf_pfcbl_ids","Done!",as.numeric(Sys.time()-t1,"secs"))
  
  return (pfcbl_ids)
}