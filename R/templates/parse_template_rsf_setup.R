parse_template_rsf_setup <- function(pool,
                                     template,
                                     template_file,
                                     reporting_user_id,
                                     rsf_indicators,
                                     omit.missing=TRUE) {
  
  template_FORMAT <- NULL
  
  #template$setup_data
  {
    #openxlsx has some bug where it can't read some types of workbooks with pivot tables
    #https://github.com/ycphs/openxlsx/issues/124
    excelwb <- tryCatch({
      openxlsx::loadWorkbook(template_file)
    },
    error = function(e) { 
      stop(conditionMessage(e))
    },
    warning = function(w) { 
      message(paste0("Notice: Pivot tables can cause errors in loadWorkdbook.  Loading with suppressWarnings because: ",conditionMessage(w)))
      suppressWarnings(openxlsx::loadWorkbook(template_file))
    })
    
    tables <- lapply(excelwb$sheet_names,
                     FUN=function(sn,wb) {
                       tbl <- openxlsx::getTables(wb=wb,
                                        sheet=sn)
                       data.table(sheet=sn,
                                  table_name=tbl,
                                  ref=names(tbl))
                     },
                     wb=excelwb)
    tables <- rbindlist(tables)
    tables <- tables[table_name != "rsf_template_data"] #parsed already through load template
    tables <- tables[grepl("^rsf_",table_name) == TRUE] #must start with rsf_
    
    table_data <- list()
    if (!empty(tables)) {
      for (i in 1:nrow(tables)) {
        tab <- tables[i]
        table_data[[tab$sheet]] <- rsf_reports_excel_read_rsf_data(excelwb=excelwb,
                                                                         table_name=tab$table_name,
                                                                         sheet_name=tab$sheet)
      }
      template$setup_data <- table_data
    }
  }
  
  #setups 
  {
    if (all(c("SYSNAME","INDID","reporting_asof_date") %in% names(template$template_data))) {
      template_FORMAT <- "LONG"
    } else if (any(names(template$template_data)=="SYSNAME")) {
      template_FORMAT <- "WIDE"
    } else {
      stop("Failed to resovle template_FORMAT is WIDE vs LONG")
    }
    
    if (is.na(template$rsf_program_id) && template_FORMAT == "WIDE") {
      stop("Program ID must be defined to WIDE format setup template")
    }
    
    if (!any(names(template$template_data) %in% c("INDID","indicator_name"))) {
      stop("Template is expected to define columns 'INDID' and/or 'indicator_name'")
    }
    
    tdata <- template$template_data
    if (any(names(tdata)=="INDID")) {
      tdata[,
            INDID:=as.numeric(INDID)]
    }
    
    if (!any(names(tdata)=="INDID")) {
      tdata[rsf_indicators,
            INDID:=i.indicator_id,
            on=.(indicator_name)]
    } else if (!any(names(tdata)=="indicator_name")) {
      
      tdata[rsf_indicators,
            indicator_name:=i.indicator_name,
            on=.(INDID=indicator_id)]
    } else if (anyNA(tdata$INDID)) {

      tdata[rsf_indicators[,
                           .(indicator_id,
                             indicator_name,
                             joincondition=as.numeric(NA))],
            INDID:=i.indicator_id,
            on=.(indicator_name,
                 INDID=joincondition)]
    }
    
    setnames(tdata,
             old="INDID",
             new="indicator_id")
    
    bad_indicators <- fsetdiff(tdata[,.(indicator_id,indicator_name)],
                              rsf_indicators[,.(indicator_id,indicator_name)])

    bad_indicators <- as.data.frame(bad_indicators)
    setDT(bad_indicators)
    
    bad_indicators[rsf_indicators,
                   current_name:=i.indicator_name,
                   on=.(indicator_id)]
    
    bad_indicators[,similarity:=mapply(FUN=function(a,b) { 
      a<-unlist(strsplit(a,split="_",fixed=T))
      b<-unlist(strsplit(b,split="_",fixed=T))
      n <- min(length(a),length(b))
      n <- length(intersect(a,b))/n
      if (all(is.na(n)) || length(n)==0) n <- 0
      n;
    },a=indicator_name,b=current_name)]
    
    auto_correct <- bad_indicators[similarity >= 0.7]
    tdata[auto_correct,
          indicator_name:=i.current_name,
          on=.(indicator_id)]
    
    bad_indicators <- bad_indicators[similarity < 0.7]

    if (!empty(bad_indicators)) {
      bad_indicators[,
                     message:=paste0(indicator_name," (",
                                     indicator_id,
                                     ifelse(is.na(current_name),
                                            " is undefined/deleted",
                                            paste0(" is now '",current_name,"'")),")")]
      
      stop(paste0("The setup file defines the following indicators, which are NOT defined in the database.\n",
                  "Perhaps the indicator names have been changed (or deleted) since the file was generated?\n",
                  "If so, review the current master list and revise the upload file accordingly.\n",
                  "Bad indicators: ",
                  paste0(bad_indicators$message,collapse=", \n")))
    }
  }  
  
  #Formula checks
  {
    formulaSheet <- template$template_data_formula_sheet
    formulaSheet <-  formulaSheet[,
                                  .(has_formulas=any(!is.na(.SD))),
                                  by=.(reporting_template_row_group)]
    formulaSheet <- formulaSheet[has_formulas==TRUE]
    if (!empty(formulaSheet)) {
      stop(paste0("Formulas are not allowed in setup templates. See errors on rows: ",
                  paste0(formulaSheet$reporting_template_row_group,collapse=", ")))
    }
  }  
  
  
  
  {
    sys_names <- unique(tdata[,.(SYSNAME)])
    sys_names <- sys_names[,
                           name_elements:=strsplit(SYSNAME,
                                                   split=">",
                                                   fixed=T)]
    #sys_names[,name_members:=sapply(name_elements,length)]
    sys_names[,
              program_name:=trimws(sapply(name_elements,'[[',1))]
    
    prog_names <- unique(sys_names$program_name)
    if (length(prog_names) != 1) {
      stop(paste0("Multiple program names not allowed: ",paste0(prog_names,collapse=", ")))
    }
    
    
    
    exists <- dbGetQuery(pool,"
      select
        ids.rsf_program_id,
        ids.rsf_pfcbl_id
      from p_rsf.rsf_pfcbl_ids ids
      where ids.rsf_pfcbl_id = (select p_rsf.get_rsf_pfcbl_id_by_sys_name($1::text) as rsf_pfcbl_id)
        and ids.pfcbl_category = 'program'
    ",params=list(prog_names))

    #We expect it to be empty, but possible user has re-uploaded file, so don't re-create
    if (!empty(exists)) {
      
      if (!is.na(template$rsf_program_id)) {
        if (!identical(as.integer(template$rsf_program_id),as.integer(exists$rsf_program_id))) {
          stop(paste0("Template upload specified PROGRAM ID#",template$rsf_program_id," but data is for ",prog_names," and PROGRAM ID#",exists$rsf_program_id))
        }
      }
      
      template$cohort_pfcbl_id <- as.numeric(exists$rsf_pfcbl_id)
      template$rsf_program_id <- as.numeric(exists$rsf_program_id)
      
    } else {
      
      required <- rsf_indicators[data_category=="program" & is_setup=="required",
                                 .(indicator_id,
                                   indicator_name,
                                   indicator_sys_category)]
      
      if (!all(required$indicator_id %in% tdata$indicator_id) || empty(tdata)) {
        stop(paste0(prog_names," {CREATE} requested but no creation data supplied. Required: ",
                    paste0(required$indicator_name,collapse=" & ")))
      }

      pdata <- tdata[required[,.(indicator_id,indicator_sys_category)],
                             on=.(indicator_id),
                             nomatch=NULL][,.(SYSNAME,
                                              reporting_asof_date,
                                              value,
                                              indicator_sys_category)]

      setorder(pdata,
               SYSNAME,
               reporting_asof_date,
               indicator_sys_category)
      
      pdata[,
            first:=1:.N,
            by=.(SYSNAME,indicator_sys_category)]
      
      pdata <- pdata[first==1]
      
      inception_date <- pdata[indicator_sys_category=="entity_creation_date",value] 
      name <- pdata[indicator_sys_category=="name",value]
      nickname <- pdata[indicator_sys_category=="nickname",value]
      lcu <- pdata[indicator_sys_category=="entity_local_currency_unit",value]
      
      if (grepl("^\\d+$",as.character(inception_date))) {
        inception_date <- openxlsx::convertToDate(inception_date,origin=openxlsx::getDateOrigin(template_file))
      }
      
      inception_date <- as.Date(inception_date)
      
      
      
      new_program <- db_program_create(pool=pool,
                                       program_name=name,
                                       program_nickname=nickname,
                                       program_inception_date=inception_date,
                                       program_lcu=lcu,
                                       reporting_user_id=reporting_user_id,
                                       program_reporting_frequency="quarter",
                                       source_name=basename(template_file))

      template$cohort_pfcbl_id <- as.numeric(new_program$rsf_pfcbl_id)
      template$rsf_program_id <- as.numeric(new_program$rsf_program_id)
      template$linked_reporting_cohort_id <- as.numeric(new_program$linked_reporting_cohort_id)
    }
  }
  
  #WIDE TO LONG
  {
    # if (template_FORMAT=="LONG") {
    #   name_family <- unique(tdata[,.(SYSNAME)])
    #   name_family[,names_list:=strsplit(SYSNAME,
    #                                            split=">",
    #                                            fixed=T)]
    #   name_family[,names_members:=sapply(names_list,length)]
    #   name_family[,
    #               `:=`(parent_pfcbl_name=as.character(NA),
    #                    pfcbl_name=as.character(NA))]
    #   name_family[names_members==1,pfcbl_name:=trimws(SYSNAME)]
    # 
    #   name_family[names_members >1,
    #               `:=`(pfcbl_name=trimws(mapply(FUN='[[',names_list,names_members)),
    #                    parent_pfcbl_name=trimws(mapply(FUN='[[',names_list,names_members-1)))]
    #   name_family[,
    #               pfcbl_category:=gsub(":.*$","",pfcbl_name)]
    # 
    #   
    #   pfcbl_ranks <- dbGetQuery(pool,"select pfcbl_category,pfcbl_rank from p_rsf.rsf_pfcbl_categories")
    #   setDT(pfcbl_ranks)
    #   name_family[pfcbl_ranks,
    #               pfcbl_rank:=i.pfcbl_rank,
    #               on=.(pfcbl_category)]
    #   name_data <- name_family[pfcbl_rank==min(name_family$pfcbl_rank),
    #                            .(parent_pfcbl_name,
    #                              pfcbl_name)]
    # } 
  }

  #lookup the SYSNAME but also if the ID indicator and value are submitted, then also look up by ID
  lookups <- rbindlist(list(tdata[rsf_indicators[indicator_sys_category=="id",.(indicator_id)],
                       .(sys_name=SYSNAME,
                         indicator_id,
                         data_value=value),
                       on=.(indicator_id),
                       nomatch=NULL][!is.na(data_value)],
                 tdata[,.(sys_name=unique(SYSNAME),
                          indicator_id=as.numeric(NA),
                          data_value=as.character(NA))]))[,
                                                          .(data_value=ifelse(all(is.na(data_value)),
                                                                              as.character(NA),
                                                                              max(data_value,na.rm=T)),
                                                            indicator_id=ifelse(all(is.na(indicator_id)),
                                                                                as.numeric(NA),
                                                                                max(indicator_id,na.rm = T))),
                                                          
                                                          by=.(sys_name)]
  
  
  lookups[,data_value:=normalizeSyscategory_id(data_value)]
  
  #not necessary as indicator ID will pick up data_category.  inefficient anyway!
  # lookups[,pfcbl_category:=sapply(strsplit(lookups$sys_name,
  #                                          split=">",
  #                                          fixed=T),function(x) gsub("^[[:space:]]*([a-z]+):.*$","\\1",x[[length(x)]]))]
  
  #conn <- poolCheckout(pool)
  #dbBegin(conn)
  #dbRollback(conn)
  existing_ids <- poolWithTransaction(pool,function(conn) {
    dbExecute(conn,"create temp table _temp_sysnames(sys_name text,
                                                     indicator_id int,
                                                     data_value text)
              on commit drop;")
    
    dbAppendTable(conn,
                  name="_temp_sysnames",
                  value=lookups[,.(sys_name,
                                   indicator_id,
                                   data_value)])  #data_value equals the "id" value and indicator it's category's ID indicator
    
    dbExecute(conn,"analyze _temp_sysnames")
    
    dbGetQuery(conn,"
               select 
                found.rsf_pfcbl_id,
                found.sys_name
               from (
                 select 
                   p_rsf.get_rsf_pfcbl_id_by_sys_name(sn.sys_name) as rsf_pfcbl_id,
                   sn.sys_name
                 from (select distinct tsn.sys_name from _temp_sysnames tsn) sn
               
                 union 
                 
                 select
                  nai.rsf_pfcbl_id,
                  tsn.sys_name
                 from _temp_sysnames tsn
                 inner join p_rsf.indicators ind on ind.indicator_id = tsn.indicator_id
                 inner join p_rsf.rsf_data_current_names_and_ids nai on nai.id = tsn.data_value
                                                                    and nai.pfcbl_category = ind.data_category
               ) as found
               inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = found.rsf_pfcbl_id
               where ids.rsf_program_id = $1::int",
               params=list(template$rsf_program_id))
  })
  setDT(existing_ids)
  
  existing_ids[,n:=.N,
               by=.(sys_name)]
  
  if (any(existing_ids$n>1)) {
    stop("Multiple existing IDs matched")
  }
  
  tdata[,SYSID:=as.numeric(NA)]
  tdata[existing_ids,
        SYSID:=i.rsf_pfcbl_id,
        on=.(SYSNAME=sys_name)]

  if (anyNA(tdata$SYSID)) {
    
    # sys_names <- unique(tdata[,.(SYSNAME)])
    # sys_names <- sys_names[,
    #                        name_elements:=strsplit(SYSNAME,
    #                                                split=">",
    #                                                fixed=T)]
    # 
    # sys_names[,
    #           program_name:=trimws(sapply(name_elements,'[[',1))]
    # 
    #A negative value will prompt template_process to assign "new" action to each of these entities
    #However, parent-child relationships must also be established.
    tdata[is.na(SYSID),
          SYSID:=-.GRP,
          by=.(SYSNAME)]
    
    sys_ids <- unique(tdata[,.(SYSNAME,SYSID)])
    sys_names[,element_length:=sapply(name_elements,length)]
    
    pnames <- sys_names[,
                        .(parent_SYSNAME=paste0(trimws(unlist(name_elements,recursive=T))[-element_length],collapse=" > ")),
                           by=.(SYSNAME)]
    pnames[nchar(parent_SYSNAME)==0,parent_SYSNAME:='global:GLOBAL']
    
    pnames[sys_ids,
           SYSID:=i.SYSID,
           on=.(SYSNAME)]
    
    pnames[sys_ids,
           parent_SYSID:=i.SYSID,
           on=.(parent_SYSNAME=SYSNAME)]
    
    pids <- unique(pnames[,.(parent_SYSNAME)])[sys_ids,
                                               on=.(parent_SYSNAME=SYSNAME)]
    pnames[pids,
           parent_SYSID:=i.SYSID,
           on=.(parent_SYSNAME)]

    template$match_SYSNAMES <- pnames
  }  
  
  #omit missing will remove data values equal to "{MISSING}" which are blank default values generated on system export to signal where 
  #facility data might need to be entered.  So if a user downloads the setup file and leaves these values as MISSING then we can assume these entries
  #don't exist.
  if (omit.missing==TRUE) {
    tdata <- tdata[grepl("^\\{MISSING\\}$",value,ignore.case = TRUE)==FALSE]
  }
  
  tdata[,reporting_submitted_data_formula:=as.character(NA)]
  template$template_data <- tdata[,.(SYSID,
                                     reporting_template_row_group,
                                     reporting_asof_date,
                                     indicator_name,
                                     reporting_submitted_data_value=value,
                                     reporting_submitted_data_unit=unit,
                                     reporting_submitted_data_formula)]
  
  template$template_ids_method <- "pfcbl_id"
  template$template_source_reference <- "RSF PROGRAM UPDATE"
  
  return (template)
}
