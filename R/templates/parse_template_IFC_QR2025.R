

parse_template_IFC_QR2025 <- function(pool,
                                      template_file,
                                      template_lookup=db_export_get_template(pool=pool,template_name="IFC-QR-TEMPLATE2025"),
                                      rsf_indicators=db_indicators_get_labels(pool),
                                      return.insert_flags=NULL, #To insert and return current flags tab based on current QR of template_file in system: this is a DATA TABLE of flags exported by UI
                                      return.next_date=FALSE,    #To automatically create zero-version of next QR based on current QR in system
                                      reporting_user_id,
                                      status_message,
                                      CALCULATIONS_ENVIRONMENT=CALCULATIONS_ENVIRONMENT) 
  
{
  
  clean_up_template <- function(excelwb) {
    
    qreport_sheet_num <- grep("QReport$",excelwb$sheet_names,ignore.case=T)
    qreport_data <- excelwb$to_df(sheet=qreport_sheet_num,col_names = F)
    
    last_not_blank <- nrow(qreport_data)
    if (any( (qreport_data_blank_rows <- rowSums(!is.na(qreport_data))==0)[-(1:QREPORT_startrow)],na.rm=T)) {
      last_not_blank <- Position(isFALSE,qreport_data_blank_rows,right=TRUE)
      
      #allow 5 blank rows
      if (last_not_blank+5 < nrow(qreport_data) && last_not_blank >QREPORT_startrow) {
        
        excelwb$clean_sheet(sheet = qreport_sheet_num, 
                            dims = paste0("A",(last_not_blank+5),":",openxlsx2::int2col(ncol(qreport_data)),nrow(qreport_data)))
        
        
      }
    }
    
    excelwb$add_data(sheet=qreport_sheet_num,
                     x=rep("",times=(nrow(qreport_data)+1-QREPORT_startrow)),
                     dims=paste0("A",QREPORT_startrow,":A",last_not_blank))
    
    #remove conditional styles and reset styles on Qreport
    {
      
      #Remove conditional formatting applied to arbitrary cells    
      if (length(excelwb$worksheets[[qreport_sheet_num]]$conditionalFormatting) &&
          length(removecf <- grep("\\s+",trimws(excelwb$worksheets[[qreport_sheet_num]]$conditionalFormatting$sqref)))) {
        excelwb$worksheets[[qreport_sheet_num]]$conditionalFormatting <- excelwb$worksheets[[qreport_sheet_num]]$conditionalFormatting[-removecf,]
      }
      
      #any conditional formual on a row with double-digit row number (ie, more than row 6 start row; simpler than parsing each digit value)
      if (length(excelwb$worksheets[[qreport_sheet_num]]$conditionalFormatting) && 
          length(removecf <- grep("\\d{2,}",excelwb$worksheets[[qreport_sheet_num]]$conditionalFormatting$sqref))) {
        excelwb$worksheets[[qreport_sheet_num]]$conditionalFormatting <- excelwb$worksheets[[qreport_sheet_num]]$conditionalFormatting[-removecf,]
      }
      
      #openxlsx2::wb_save(excelwb,file="C:/Temp/OTP Test2.xlsx",overwrite=T)
      excelwb$add_fill(sheet=qreport_sheet_num,
                       dims=paste0("A",QREPORT_startrow,":",openxlsx2::int2col(ncol(qreport_data)),last_not_blank),
                       color=NULL)
      
      excelwb$add_font(sheet=qreport_sheet_num,
                       dims=paste0("A",QREPORT_startrow,":",openxlsx2::int2col(ncol(qreport_data)),last_not_blank),
                       update=NULL) 
      
      #Using add border as a multi-dimensional range causes an infinite loop inside openxlsx! ???
      for (excelcol in names(qreport_data)) {
        excelwb$add_border(sheet=qreport_sheet_num,
                           dims=paste0(excelcol,QREPORT_startrow,":",excelcol,last_not_blank),
                           update=NULL)
      }
      
      # target_dims <- openxlsx2::wb_dims(
      #   x = excelwb$to_df(sheet=qreport_sheet_num,col_names = F),
      #   from_row = QREPORT_startrow,
      #   from_col = 1
      # )
      
      
      excelwb$add_data(sheet=qreport_sheet_num,
                       x="🏴 ",
                       dims="A5")
      
      excelwb$add_cell_style(sheet=qreport_sheet_num,
                             dims="A5",
                             horizontal = "center")
      
      #Color of the flag
      excelwb$add_font(sheet=qreport_sheet_num,
                       dims="A5",
                       color=wb_color("gray"))
    }
    
    #Remove custom XML
    if (length(excelwb$customXml) ||
        any(grepl("customXml",excelwb$Content_Types)) || 
        any(grepl("customXml",excelwb$workbook.xml.rels))) {
      
      message("Excel file has customXml that will be removed")
      excelwb$Content_Types <- grep("customXml",excelwb$Content_Types,value=T,invert = T)
      excelwb$workbook.xml.rels <- grep("customXml",excelwb$workbook.xml.rels,value=T,invert = T)
      excelwb$customXml <- NULL
    }
    
    
    
    defined_names_xml <- rbindlist(lapply(lapply(excelwb$workbook$definedNames,xml2::read_xml),function(x) as.data.frame(as.list(xml2::xml_attrs(x)))),fill=T)
    if (is.null(defined_names_xml$hidden)) {
      defined_names_xml[,hidden:=""]
    }
    defined_names_xml[,is_hidden:=!is.na(suppressWarnings(as.numeric(hidden))==1)]
    hidden_dn <- which(defined_names_xml$is_hidden)
    if (length(hidden_dn)) {
      for (i in rev(hidden_dn)) {
        message(paste0("Excel file has hidden named ranges that will be removed: ",defined_names_xml$name[i])) 
        excelwb$workbook$definedNames <- excelwb$workbook$definedNames[-i]
      }      
    }
    
    
    errors <- fsetdiff(nregions_table[is_hidden==F,.(name,value,sheets,coords)],
                       as.data.table(openxlsx2::wb_get_named_regions(excelwb))[,.(name,value,sheets,coords)])
    if(!empty(errors)) {
      stop(paste0("Defined name ranges have gone missing!\n",
                  paste(capture.output(print(errors)), collapse = "\n")))
    }
    # if (any(nregions_table$is_hidden,na.rm=T)) {
    # #Remove Hidden Named Ranges
    #   remove_hidden <- nregions_table[is_hidden==T]
    #   for (i in nrow(remove_hidden):1) {
    #     rh <- remove_hidden[i]
    #     target_sheet <- if (is.na(rh$sheets) || rh$sheets == "") { NULL } else { rh$sheets }
    #     message(paste0("Excel file has hidden named ranges that will be removed: ",rh$name," on ",ifelse(is.null(target_sheet),"GLOBAL",target_sheet)))  
    #     excelwb$remove_named_region(
    #       sheet = target_sheet,
    #       name = rh$name
    #     )
    #   }
    #}
    # if (any(nchar( (nr<-excelwb$get_named_regions())$hidden) )) {
    #   
    #   for (i in 1:nrow(nr)) {
    #     rng <- nr[i,]
    #     hnr <- suppressWarnings(as.numeric(rng$hidden))
    #     if (length(hnr) > 0 && !is.na(hnr) && hnr==1) {
    #       
    #       if (any(rng$sheets==excelwb$sheet_names)) {
    #        
    #         excelwb$remove_named_region(sheet=rng$sheets,
    #                                     name=rng$name)
    #       }
    #     }
    #   }
    # }
    
    #Remove existing comments (Legacy comments create issues)
    if (length(excelwb$comments)) {
      
      comments <- rbindlist(lapply(seq_along(excelwb$sheet_names),function(s) { x <- excelwb$get_comment(sheet=s); if (length(x)) cbind(x[,c("ref","cmmt_id")],sheet=s) }))
      #threads <- rbindlist(lapply(seq_along(excelwb$sheet_names),function(s) { x <- excelwb$get_thread(sheet=s); if (length(x)) cbind(x,sheet=s) }))
      
      
      
      for (nc in 1:nrow(comments)) excelwb$remove_comment(sheet=comments[nc]$sheet,dims=comments[nc]$ref)
      #for (nc in 1:nrow(threads)) excelwb$remove_comment(sheet=threads[nc]$sheet,dims=threads[nc]$ref)
      #if (!empty(comments)) { invisible(pmap(comments[,.(sheet,dims=ref)],excelwb$remove_comment)) }
      
      for (i in seq_along(excelwb$vml_rels)) {
        if (nchar(excelwb$vml_rels[i])==0) excelwb$vml_rels[[i]] <- NULL
      }
    }
    if (length(excelwb$threadComments)) {
      for (i in seq_along(excelwb$threadComments)) excelwb$threadComments[[i]] <- ""
    }
    
    snames <- wb_get_sheet_names(excelwb)
    for(sname in snames) {
      size <- dim(excelwb$to_df(sheet=sname))
      excelwb$set_sheet_visibility(sheet=sname,value="visible")
      
      if (length(size)) {
        excelwb$remove_row_heights(sheet=sname,
                                   rows=1:size[1])
      }
      
      ratt <- excelwb[["worksheets"]][[which(sname==snames)]][["sheet_data"]][["row_attr"]]
      if (any(nchar(ratt$hidden)>0,na.rm=T)) {
        excelwb[["worksheets"]][[which(sname==snames)]][["sheet_data"]][["row_attr"]][which(nchar(ratt$hidden)>0),"hidden"] <- ""
      }
      
      if (any(nchar(ratt$ht)>0,na.rm=T)) {
        excelwb[["worksheets"]][[which(sname==snames)]][["sheet_data"]][["row_attr"]][which(nchar(ratt$ht)>0),"ht"] <- ""
      }
      
      excelwb$remove_hyperlink(sheet=sname)
    }
    return (excelwb)
  }
  
  #Read file
  {
    if (!file.exists(template_file)) stop(paste0("Uh oh! File '",template_file,"' doesn't exist!"))
    if (!file_ext(template_file) %in% "xlsx") stop("Only .xlsx files using Excel-365 versions or later may use this template")
    if ((file.size(template_file) / 1024^2) > 35) {
      stop(paste0("Uh oh! This file is ",round(file.info(template_file)$size / 1024^2,2),"MB! This exceeds the 35MB limit (and it surely also exceeds IFC and client email server limits, which will prevent you from communicating this data with the client). ",
                  "Most likely, your file unnecessarily is large and Excel can reduce the size considerably (like deleting unused formats).  Ask Copilot Excel how to ",
                  "reduce this file size and re-save it or try going to: \n",
                  "- Review tab \n",
                  "- Check Performance button \n",
                  "- Optimize Workbook \n",
                  "> Then also see if formats are filled-out for the entire workbook--and if so, highlight unused cells and 'clear formats'",
                  "However, if all the data here is truly necessary for upload, re-save as a .csv the quarterly report file and use Jason's csv file upload method instead (",
                  "the qreport headers need to be on row 1 of the .csv file and the project ID number needs to be in the file name of the .csv file"))
    }
    
    
    #!very important!
    #Especially to ensure that we do not read-in checks from a file that is being downloaded!
    #We only want to read-in checks for uploaded files that are being reported into the system.
    RETURN.regular <- is.null(return.insert_flags) && !isTRUE(return.next_date)
    
    ####
    #openxlsx has some bug where it can't read some types of workbooks with pivot tables
    #https://github.com/ycphs/openxlsx/issues/124
    
    excelwb <- suppressWarnings(openxlsx2::wb_load(template_file))
    
    
    sheetCURRENTFLAGS <- "Current Flags"
    nregions_table <- openxlsx2::wb_get_named_regions(excelwb)
    setDT(nregions_table)
    
    #If there are no hidden names the column name is not present at all
    if (is.null(nregions_table$hidden)) {
      nregions_table[,hidden:=""]
    }
    nregions_table[,is_hidden := suppressWarnings(as.numeric(hidden)) == 1]
    nregions_table[is.na(is_hidden), is_hidden := FALSE]
    
    nregions_table[,data_value:=list()]
    #128
    for (i in 1:nrow(nregions_table)) {
      
      nr <- nregions_table[i,]
      
      if (!grepl("\\d",nr$coords)) next; #_xlnm.Print_Area has coords of D:XFD and this crashes R Studio -- wb_to_df is evidently building a bazillion cell matrix?
      if (nr$is_hidden) next;
      
      coords <- nr$coords
      if (!grepl("^[A-Z]+[0-9]*(:[A-Z]+[0-9]*)?$",coords)) next;
      
      size <- openxlsx2::dims_to_rowcol(nr$coords)
      if (all(sapply(size,length)>1)) next; #we're not interested in multi-dimensional ranges; tables, etc. Purely reading-in vectors or cells.
      

      x <- tryCatch({
        
        openxlsx2::wb_to_df(excelwb,
                            sheet=nr$sheets,
                            named_region=nr$name,
                            col_names=F,
                            convert=F,
                            detect_dates = F)
 
        # x <- openxlsx2::wb_to_df(excelwb,named_region=n)
        # 
        # if (nrow(x)==0) {
        #   dval <- names(x)
        # 
        # } else if (ncol(x)==1) {
        #   
        #   x <- na.omit(c(names(x),unlist(x)))
        #   if (any(grepl(",",x))) x <- gsub(","," ",x)
        #   x <- paste0(x,collapse=",")
        #   x <- gsub("[[:space:]]{2,}"," ",x)
        #   dval <- x
        # }
      },
      error=function(e) { data.frame() },
      warning=function(w) { data.frame() })
      
      if (empty(x)) next;

      dval <- as.character(unlist(x))
      dval <- na.omit(dval)
      if (!length(dval)) dval <- as.character(NA)
      # if (any(grepl(",",dval))) dval <- gsub(","," ",dval)
      # dval <- paste0(dval,collapse=",")
      # dval <- gsub("[[:space:]]{2,}"," ",dval)
      # dval

      nregions_table[i,
                     data_value:=list(as.character(dval))]
    }
    

    
    
    snames <- openxlsx2::wb_get_sheet_names(excelwb)
 
    # {
    # # 3. Read formulas for all sheets using lapply
    #   for (snum in seq_along(snames)) {
    #     
    #     cc_data <- excelwb$worksheets[[snum]]$sheet_data$cc
    #     if (is.null(cc_data) || empty(cc_data)) next;
    #     
    #     if ("f_attr" %in% names(cc_data)) {
    #       # Clear strings containing t="array" or related flags
    #       legacy_rows <- grepl('t="array"', cc_data$f_attr)
    #       excelwb$worksheets[[snum]]$sheet_data$cc$f_attr[legacy_rows] <- NA_character_
    #     }
    #     
    #     if ("f_t" %in% names(cc_data)) {
    #       # Clear the explicit formula type flag if present
    #       legacy_type_rows <- cc_data$f_t == "array" & !is.na(cc_data$f_t)
    #       excelwb$worksheets[[snum]]$sheet_data$cc$f_t[legacy_type_rows] <- NA_character_
    #     }
    #   }
    # wb_save(excelwb,"C:/Temp/modified.xlsx",overwrite=T)  
    # excelwb_sheet_data <- lapply(seq_along(snames), function(snum) {
    #   sheet_cells <- excelwb$worksheets[[snum]]$sheet_data$cc
    #   #formulas <- sheet_cells[!is.na(sheet_cells$f) & nchar(sheet_cells$f) > 0,]
    #   sheet_cells[!is.na(sheet_cells$f) & nchar(sheet_cells$f) > 0,]
    #   
    # })
    # }
    
  } 
  
  #Read datasets
  {
    reporting_flags <- data.table(rsf_pfcbl_id=numeric(0),
                                  indicator_id=numeric(0),
                                  reporting_asof_date=as.Date(numeric(0)),
                                  check_name=character(0),
                                  check_message=character(0))
    
    reporting_asof_date <- {
      
      data_qdd <- names(openxlsx2::wb_to_df(excelwb, named_region = "Data_QDD"))
      
      if (!length(data_qdd)==1) {
        if (length(data_qdd)==0) stop("Failed to find defined name: Data_QDD specifying the Template's Quarterly Determination Date, suggesting this template is corrupted.")
        else stop(paste0("Template defines multiple defined names for 'Data_QDD' with different values: ",paste0(data_qdd,collapse=" & "),", suggesting that this template is corrupted."))
      }
      
      data_qdd <- as.Date(data_qdd)
      
      if (data_qdd >= today()) {
        stop(paste0("Invalid reporting QDD date: ",data_qdd," is in the FUTURE!  This is not possible. Verify the QDD date is correct?"))
      }
      
      data_qdd
    }
   
    #Load the data sheets (and their formulas)  
    {
      template_headers <- data.table(label=character(0),
                                     label_formula=character(0),
                                     data_source_index=character(0),
                                     indicator_id=numeric(0))
      
      
      #Summary tab
      {
        summarySheet <- grep("Summary$",snames,value=T,ignore.case = T)
        if (length(summarySheet) != 1) {
          stop("Failed to find 'Summary' Sheet in Template")
        }
        
        summary_sheet <- openxlsx2::read_xlsx(excelwb,sheet=summarySheet,row_names=F,col_names=F,detect_dates=T)
        setDT(summary_sheet)
        
        summary_formula_matrix <- openxlsx2::wb_to_df(excelwb,
                                                      sheet=summarySheet,
                                                      row_names=F,
                                                      col_names=F,
                                                      show_formula=T)
        setDT(summary_formula_matrix)
        
        if (!all(dim(summary_formula_matrix) == dim(summary_sheet))) {
          stop("Summary sheet data and formulas are mismatched")
        }
        
        summary_formula_matrix[summary_formula_matrix==summary_sheet] <-NA
        
        setDT(summary_sheet)
        summary_sheet[,original_row_num:=.I]
      
        POSITION_current_terms <- grep("Current Terms",summary_sheet$E,ignore.case = T)
        POSITION_current_qdd <- grep("Current QDD",summary_sheet$E,ignore.case = T)
        
        if (length(POSITION_current_terms)==0) { POSITION_current_terms <- 5 } #Default is row 5 for Facility Errors
        if (length(POSITION_current_qdd)==0) { POSITION_current_qdd <- 7 } #Default is row 5 for Client Errors
      }
      
      #QReport
      {
        dataSheet <- grep("QReport$",snames,value=T,ignore.case = T)
        if (length(dataSheet) != 1) {
          stop("Failed to find 'QReport' Sheet in Template")
        }
        
        data_sheet <- openxlsx2::read_xlsx(excelwb,sheet=dataSheet,row_names=F,col_names=F,detect_dates=T)
        setDT(data_sheet)
        
        for (col in names(data_sheet)) {
          if (is.character(data_sheet[[col]])) next;
          set(data_sheet, j = col, value = as.character(data_sheet[[col]]))
        }
        
        QREPORT_data_dims <- dim(data_sheet)
        
        data_formula_matrix <- openxlsx2::wb_to_df(excelwb,sheet=dataSheet,row_names=F,col_names=F,show_formula=T)
        setDT(data_formula_matrix)
        
        for (col in names(data_formula_matrix)) {
          if (is.character(data_formula_matrix[[col]])) next;
          set(data_formula_matrix, j = col, value = as.character(data_formula_matrix[[col]]))
        }
        
        if (!all(dim(data_formula_matrix) == dim(data_sheet))) {
          stop("QReport data sheet data and formulas are mismatched")
        }
        
        data_formula_matrix[data_sheet==data_formula_matrix] <-NA
        
        # data_sheet <- template_excel_read_sheet(excelwb=excelwb,
        #                                         sheetName=dataSheet)
        # 
        # data_formula_matrix <- openxlsx_get_formulas(excelwb=excelwb,
        #                                              sheetName=dataSheet,
        #                                              truncate_predata_rows = TRUE)
        setDT(data_sheet)
      }
      
      #Import Named Ranges into summary-level data
      {
       
        
        lsheet <- grep("lists|template",snames,ignore.case=T,value=T)
        if (length(lsheet) != 1) { stop("Failed to find Template sheet (formally Lists)") }
        
        list_sheet <- nregions_table[sheets==lsheet,.(name,data_value)]
        list_sheet <- list_sheet[grepl("^Template_",name)==F] #template defined names are inherently excluded as not relevant for Jason (they're for the template!)
        list_sheet[,original_row_num:=.I]
      }
      
      #Current Flags
      {
        #unique(nregions_table$sheets)
        
        if (any(openxlsx2::wb_get_tables(excelwb)=="rsf_current_flags",na.rm=T) &&
            RETURN.regular==TRUE) {
          
          current_flags <- wb_read(excelwb,named_region="rsf_current_flags")
          setDT(current_flags)

          #legacy formatting
          if (any(names(current_flags)=="comment",na.rm=T)) setnames(current_flags,old="comment",new="IFC Comments")
          #Expected: FLAGID,DATE,NAME,type,class,CHECK,STATUS,MESSAGE,IFC Comments,CLIENT Comments
          
          if (!empty(current_flags)) {
          
            setnames(current_flags,
                     old=names(current_flags),
                     new=tolower(gsub("\\s+","_",names(current_flags))))

            if (!all(c("flagid","status","ifc_comments") %in% names(current_flags))) {
              status_message(class="error",
                             "Failed to read-in CURRENT FLAGS due to missing/unrecognized headers")
              
              current_flags <- NULL
              
            
            } else {
              
             
              # current_flags <- current_flags[,
              #                                ..current_flag_headers]
              
              if (!any(grepl("client_comments",names(current_flags),ignore.case=T))) {
                
                current_flags[,
                              client_comments:=""]
              }
              
              current_flags[grepl("N/A",ifc_comments,ignore.case = T) | 
                            grepl("^NA$",ifc_comments,ignore.case = T) | 
                            is.na(ifc_comments),
                            ifc_comments:=""]

              current_flags[grepl("N/A",client_comments,ignore.case = T) | 
                            grepl("^NA$",client_comments,ignore.case = T) | 
                            is.na(client_comments),
                            client_comments:=""]

                            
              current_flags[,
                            ifc_comments:=trimws(ifc_comments)]

              current_flags[,
                            client_comments:=trimws(client_comments)]
              
              current_flags <- current_flags[nchar(ifc_comments) > 0 | nchar(client_comments) > 0]
              
              current_flags[,status:=tolower(status)]
              current_flags[,status:=fcase(status=="closed","resolved",
                                           status=="resolved","resolved",
                                           default="active")]
              
              
              
              
              current_flags[,evaluation_id:=suppressWarnings(as.numeric(gsub("^.*#(\\d+)$","\\1",flagid)))]
              current_flags <- current_flags[!is.na(evaluation_id)]
              
            }
          }
          
          if (!empty(current_flags)) {
            
            ids_notfound <- db_checks_report_comments(pool=pool,
                                                      current_flags=current_flags,
                                                      reporting_user_id=reporting_user_id)
            
            if (length(ids_notfound)) {
              current_flags <- current_flags[evaluation_id %in% ids_notfound]
              current_flags[,c("indicator_name","check_formula") := tstrsplit(check,":")]
              
              updateids <- poolWithTransaction(pool,function(conn) {
                
                dbExecute(conn,"create temp table _temp_flags(evaluation_id int,
                                                              check_asof_date date,
                                                              pfcbl_name text,
                                                              indicator_name text,
                                                              check_formula text,
                                                              check_message text)
                              on commit drop")
                
                dbAppendTable(conn,
                              name="_temp_flags",
                              value=current_flags[,.(evaluation_id,
                                                     check_asof_date=date,
                                                     pfcbl_name=name,
                                                     indicator_name,
                                                     check_formula,
                                                     check_message=message)])
                
                dbGetQuery(conn,"
                select 
                  lookup.missing_id,
                  coalesce(rdc.evaluation_id,dca.archive_id) as current_id
                from (
                  select 
                  tf.evaluation_id as missing_id,
                  nids.rsf_pfcbl_id,
                  ind.indicator_id,
                  tf.check_asof_date,
                  coalesce(icf.indicator_check_id,ic.indicator_check_id) as indicator_check_id,
                  icf.check_formula_id,
                  tf.check_message
                  from _temp_flags tf
                  inner join p_rsf.indicators ind on ind.indicator_name = tf.indicator_name
                  inner join p_rsf.rsf_data_current_names_and_ids nids on nids.pfcbl_name like '%' || tf.pfcbl_name
                  left join p_rsf.indicator_checks ic on ic.check_name = trim(tf.check_formula)                  
                  left join p_rsf.indicator_check_formulas icf on icf.check_formula_title = trim(tf.check_formula)
                ) lookup
                  left join p_rsf.rsf_data_checks rdc on rdc.rsf_pfcbl_id = lookup.rsf_pfcbl_id
                                                      and rdc.check_asof_date = lookup.check_asof_date
                                                      and rdc.indicator_check_id = lookup.indicator_check_id                                                      
                                                      and rdc.check_formula_id is not distinct from lookup.check_formula_id
                                                      and rdc.check_message is not distinct from lookup.check_message
                  left join p_rsf.rsf_data_checks_archive dca on dca.rsf_pfcbl_id = lookup.rsf_pfcbl_id
                                                             and dca.check_asof_date = lookup.check_asof_date
                                                             and dca.indicator_check_id = lookup.indicator_check_id                                                      
                                                             and dca.check_formula_id is not distinct from lookup.check_formula_id
                                                             and dca.check_message is not distinct from lookup.check_message                                                      
                           
                ")
              })
              
              setDT(updateids)
              updateids <- updateids[!is.na(current_id)]
              
              if (!empty(updateids)) {
                current_flags[updateids,
                              evaluation_id:=i.current_id,
                              on=.(evaluation_id=missing_id)]
                ids_notfound <- db_checks_report_comments(pool=pool,
                                                          current_flags=current_flags[evaluation_id %in% updateids$current_id],
                                                          reporting_user_id=reporting_user_id)
              }
            }
          }
        }
      }
    }
    
    #will also omit the first rows above the Facility ID
    rsf_pfcbl_id.facility <- {
      
      
      #ifcpid <- names(openxlsx2::wb_to_df(excelwb, named_region = "IFC_ProjectID"))
      #ifcpid <- nregions_table[range_name=="",range_value]
      ifcpid <- unlist(nregions_table[grepl("IFC_ProjectID",name,ignore.case=T),data_value])
      ifcpn <-  unlist(nregions_table[grepl("IFC_ClientName",name,ignore.case=T),data_value])
      project_id <- as.numeric(gsub("[^[:digit:]]+","",ifcpid))
      

      if (length(project_id)==0 || is.na(project_id)) {
        stop(paste0("Failed to identify IFC Project ID number from defined name RSA_IFCProjectID.  Read-in value: ",project_id," from: ",ifcpid))
      }
      
      rsf_pfcbl_id <- dbGetQuery(pool,"
                                      select distinct
                                      cni.rsf_pfcbl_id
                                      from p_rsf.rsf_data_current_names_and_ids cni
                                      where id = $1::text
                                        and cni.pfcbl_category = 'facility'",
                                  params=list(project_id))
      
      
      if (nrow(rsf_pfcbl_id) != 1) {
        
        rsf_pfcbl_id <- dbGetQuery(pool,"
          select distinct
            rd.rsf_pfcbl_id
          from p_rsf.rsf_data rd
          inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = rd.rsf_pfcbl_id
          inner join p_rsf.indicators ind on ind.indicator_id = rd.indicator_id
                                         and ind.indicator_sys_category in ('id','name')
          where (rd.data_value = trim($1::text) or rd.data_value = trim($2::text))
            
            and ids.pfcbl_category = 'facility'",
                                   params=list(ifcpid,ifcpn))
        
        if (nrow(rsf_pfcbl_id) != 1) {
          stop(paste0("Failed to uniquely match IFC Project ID from Project ID defined in template: '",ifcpid," ",ifcpn,"'.  Has this IFC Facility been setup?"))
        }
      }
      
      rsf_pfcbl_id$rsf_pfcbl_id #need this here to return it from the anonamous function
    }
    
    summary_sheet_ID_row <- {
      
      rid <- nregions_table[grepl("IFC_ProjectID",name,ignore.case=T),coords]
      
      
      if (length(rid) !=1) stop(paste0("IFC_ProjectID defined name is defined multiple times for row(s): ",paste0(rid,collapse=" & "),". Please review defined name manager for duplicates"))
      as.numeric(openxlsx2::dims_to_rowcol(rid)$row)
    }
    
    #labels, including facility-specific label mappings
    {
      #why did I do this?...Maybe to turn it into a setting or something later on?  For IFC QR template the headers are such a complete mess, just leave always on.  No use to if() check.
      save_headers <- TRUE
      rsf_labels <- db_indicators_get_header_actions(pool=pool,
                                                     template_id=template_lookup$template_id,
                                                     rsf_pfcbl_id=rsf_pfcbl_id.facility,
                                                     rsf_indicators=rsf_indicators,
                                                     formatting.function=normalizeLabel)
    }
  }
  
  data.list <- {
    #new
    {
      
      label_matches <- mapply(labelMatches,
             find_sections=tolower(rsf_labels$template_section_lookup),
             find_labels=tolower(rsf_labels$template_label_lookup),
             match_id=rsf_labels$label_header_id,
             match_postion=rsf_labels$template_header_position,
             MoreArgs=list(search_sections=rep(x="template",times=length(list_sheet$name)),
                           search_labels=normalizeLabel(list_sheet$name)),
             USE.NAMES = F)
      
      label_matches <- rbindlist(label_matches)
      
      label_matches <- label_matches[rsf_labels[,.(label_header_id,action,map_indicator_id,map_formula_id,map_check_formula_id)],
                                     on=.(match_id=label_header_id),
                                     nomatch=NULL]
      
      label_matches <- label_matches[,
                                     .(header_ids=list(unique(match_id))),
                                     by=.(original_row_num=match_rows,action,map_indicator_id,map_formula_id,map_check_formula_id)]
      
      
      list_sheet <- label_matches[list_sheet,
                                  on=.(original_row_num)]  
      

    }
    
    #Label errors/mismatching
    {
      list_sheet[,
                  ignore:=anyNA(action)==FALSE & all(action=="ignore"),
                  by=.(original_row_num)]

      list_sheet <- list_sheet[ignore==FALSE]
      
      
      list_sheet[rsf_indicators,
                 `:=`(indicator_name=i.indicator_name,
                      data_category=i.data_category),
                 on=.(map_indicator_id=indicator_id)]
      
      {
        bad_categories <- list_sheet[!is.na(data_category) & !data_category %in% c("client","facility")]
        
        if (!empty(bad_categories)) {
          bad_categories[,
                         check_message:=paste0("Template Tab defined name '",name,"' maps to a ",
                                               toupper(data_category)," level metric '",indicator_name,"' however the LISTS sheet can only have Client and Facility setup data ",
                                               " (lists of acceptable input that can be used in drop-down lists, as defined by the RSA or template). ",
                                               "Since this data cannot be for any specific ",toupper(data_category)," it is being IGNORED.  Consider going to ",
                                               "JASON -> RSF Setup -> Setup Templates -> ",template_lookup$template_name," to remap ",
                                               "this specific label to the appropriate client or facility level metric; or set to ignore it entirely this field is for Excel use only")]
          
          status_message(type="error",paste0(bad_categories$check_message,collapse="\n\n"))
          
          bad_categories <- bad_categories[,.(rsf_pfcbl_id=NA,
                                              indicator_id=NA,
                                              reporting_asof_date=reporting_asof_date,
                                              check_name="sys_reporting_data_discarded",
                                              check_message)]
          
          reporting_flags <- rbindlist(list(reporting_flags,
                                            bad_categories))
          
          list_sheet <- list_sheet[is.na(data_category) | data_category %in% c("client","facility")]
          
        }
      }
      
      #Will fail because its ambiguous: this shouldn't be possible for defined names? Unless copy-paste errors copy-in multiple defined names??
      {
        list_sheet[,
                      mismatch:=anyNA(action)==FALSE & length(unique(map_indicator_id))>1,
                      by=.(original_row_num)]
        
        mismatch_labels <- list_sheet[mismatch==TRUE]
        if (!empty(mismatch_labels)) {
          
          
          mismatch_labels[,
                          message:=paste0("List Sheet defined name \"",name,"\" maps to \"",indicator_name,"\"")]
          
          setorder(mismatch_labels,
                   original_row_num)
          
          message <- paste0(mismatch_labels$message,collapse=" \n")
          stop(paste0("Mismatched Column Labels:\n",
                      "Correct the column name(s) in Summary Tab \n",
                      "Or if this is a Template Requirement map these columns in JASON -> RSF Setup -> Setup Templates -> ",template_lookup$template_name," \n\n",
                      message))
        }
      }    
      
      #Unfound: will asign to entity_reporting
      {
        list_sheet[,
                      notfound:=all(is.na(action)),
                      by=.(original_row_num)]
        
        unfound_labels <- list_sheet[notfound==TRUE]
        
        if (!empty(unfound_labels)) {
          
          setorder(unfound_labels,
                   original_row_num)
          
          unfound_labels[,
                         `:=`(rsf_pfcbl_id=rsf_pfcbl_id.facility,
                              indicator_id=as.numeric(NA), #will be auto-assigned to reporting indicator
                              reporting_asof_date=reporting_asof_date,
                              check_name="sys_flag_indicator_not_found",
                              check_message=paste0("List Tab defined name \"",name,"\""))]
          
          unfound_labels <- unfound_labels[,.(rsf_pfcbl_id,
                                              indicator_id,
                                              reporting_asof_date,
                                              check_name,
                                              check_message)]
          
          reporting_flags <- rbindlist(list(reporting_flags,
                                            unfound_labels))
        }
      }

      list_sheet <- list_sheet[is.na(action)==FALSE]
      
      if (save_headers) {
        template_headers <- rbindlist(list(template_headers,
                                           list_sheet[!is.na(map_indicator_id),
                                                         .(label=name,
                                                           label_formula=as.character(NA),
                                                           data_source_index=paste0("Template Sheet defined name ",name),
                                                           indicator_id=map_indicator_id)]))
      }
      
      
      list_sheet <- list_sheet[is.na(map_indicator_id)==FALSE,
                                     .(indicator_name,
                                       data_unit=as.character(NA),
                                       data_value,
                                       original_row_num)]
    }
    
    {
      #list_sheet <- list_sheet unique(list_sheet,by=.(original_row_num,indicator_name))
      
      list_sheet[,
                 `:=`(reporting_submitted_data_unit=data_unit,
                      reporting_submitted_data_value=sapply(list_sheet$data_value,paste0,collapse=","))]
      # setnames(list_sheet,
      #          old=c("data_unit","data_value"),
      #          new=c("reporting_submitted_data_unit",
      #                "reporting_submitted_data_value"))
      # 
      #list_sheet <- unique(list_sheet)
      
      list_sheet[rsf_indicators[!is.na(data_unit),
                                   .(indicator_name,data_unit,joincondition=as.character(NA))],
                    reporting_submitted_data_unit := i.data_unit,
                    on=.(indicator_name,
                         reporting_submitted_data_unit=joincondition)]
      list_sheet[,
                 reporting_submitted_data_formula:=as.numeric(NA)]
      
      list_sheet[rsf_indicators,
                 indicator_sys_category:=i.indicator_sys_category,
                 on=.(indicator_name)]
      
      #list_sheet
      #TEMPLATE product headers:
      #Allowed Loan Type             -- product name: "products_eligible"
      #Funded vs Unfunded	           -- YES/NO
      #Amoritizing/Term Type	       -- YES/NO
      #Revolver/Demand Type	         -- YES/NO
      #Commitments at Risk (Billing) -- YES/NO
      #RSA "Type" Classification     -- open text, usually TYPe1 and TYPe2

      if (any(list_sheet$indicator_sys_category=="products_eligible",na.rm=T)) {
        products <- unlist(list_sheet[indicator_sys_category=="products_eligible",data_value])
      
        #Funded/Unfunded
        {
          if (any(list_sheet$indicator_sys_category=="products_funded",na.rm=T)) {
            list_pr <- list_sheet[indicator_sys_category=="products_funded"]
            tfproducts <- unlist(list_pr[,data_value])
            if (length(tfproducts) != length(products) && !all(tfproducts %in% products)) {
              stop(paste0("Eligible products are: [",paste0(products,collapse=","),"] and Funded Products are: [",paste0(tfproducts,collapse=","),"]. These lists must have equal lengths"))
            }
            is_true <- sapply(tfproducts,FUN=function(p) {
              p <- superTrim(p)
              any(p==superTrim(products),na.rm = T) |
              any(p==superTrim(c("Yes","Oui","Si","True","Sim","Ja","Da")),na.rm=T)
            })
            
            list_sheet[indicator_sys_category=="products_funded",
                       reporting_submitted_data_value:=paste0(products[is_true],collapse=",")]
            
            #There shouldn't be! as the list defines funded products
            if (!any(list_sheet$indicator_sys_category=="products_unfunded",na.rm=T)) {
              
              list_pr[,
                      `:=`(data_value=list(products[!is_true]),
                           indicator_sys_category="products_unfunded",
                           indicator_name=rsf_indicators[indicator_sys_category=="products_unfunded",indicator_name],
                           reporting_submitted_data_value=paste0(products[!is_true],collapse=","))]
              
              list_sheet <- rbindlist(list(list_sheet,
                                           list_pr))
            }
              
          }
          
          # if (any(list_sheet$indicator_sys_category=="products_unfunded",na.rm=T)) {
          #   list_pr <- list_sheet[indicator_sys_category=="products_unfunded"]
          #   tfproducts <- unlist(list_pr[,data_value])
          #   if (length(tfproducts) != length(products) && !all(tfproducts %in% products)) {
          #     stop(paste0("Eligible products are: [",paste0(products,collapse=","),"] and Ununded Products are: [",paste0(tfproducts,collapse=","),"]. These lists must have equal lengths"))
          #   }
          #   
          #   is_true <- sapply(tfproducts,FUN=function(p) {
          #     p <- superTrim(p)
          #     any(p==superTrim(products),na.rm = T) |
          #     any(p==superTrim(c("Yes","Oui","Si","True","Sim","Ja","Da")),na.rm=T)
          #   })
          #   
          #   list_sheet[indicator_sys_category=="products_unfunded",
          #              reporting_submitted_data_value:=paste0(products[is_true],collapse=",")]
          #   
          #   if (!any(list_sheet$indicator_sys_category=="products_funded",na.rm=T)) {
          #     
          #     list_pr[,
          #             `:=`(data_value=paste0(products[!is_true],collapse=","),
          #                  indicator_sys_category="products_funded",
          #                  indicator_name=rsf_indicators[indicator_sys_category=="products_funded",indicator_name],
          #                  reporting_submitted_data_value=paste0(products[!is_true],collapse=","))]
          #     
          #     list_sheet <- rbindlist(list(list_sheet,
          #                                  list_pr))
          #   }
          #   
          # }
        }
        
        #Amortizing/Revolving
        {
          if (any(list_sheet$indicator_sys_category=="products_amortizing",na.rm=T)) {
            list_pr <- list_sheet[indicator_sys_category=="products_amortizing"]
            tfproducts <- unlist(list_pr[,data_value])
            if (length(tfproducts) != length(products) && !all(tfproducts %in% products)) {
              stop(paste0("Eligible products are: [",paste0(products,collapse=","),"] and Amortizing Products are: [",paste0(tfproducts,collapse=","),"]. These lists must have equal lengths"))
            }
            
            is_true <- sapply(tfproducts,FUN=function(p) {
              p <- superTrim(p)
              any(p==superTrim(products),na.rm = T) |
              any(p==superTrim(c("Yes","Oui","Si","True","Sim","Ja","Da")),na.rm=T)
            })
            
            list_sheet[indicator_sys_category=="products_amortizing",
                       reporting_submitted_data_value:=paste0(products[is_true],collapse=",")]
            
            
            # if (!any(list_sheet$indicator_sys_category=="products_revolving",na.rm=T)) {
            #   
            #   list_pr[,
            #           `:=`(data_value=paste0(products[!is_true],collapse=","),
            #                indicator_sys_category="products_revolving",
            #                indicator_name=rsf_indicators[indicator_sys_category=="products_revolving",indicator_name],
            #                reporting_submitted_data_value=paste0(products[!is_true],collapse=","))]
            #   
            #   list_sheet <- rbindlist(list(list_sheet,
            #                                list_pr))
            # }
            
          }
          
          if (any(list_sheet$indicator_sys_category=="products_revolving",na.rm=T)) {
            list_pr <- list_sheet[indicator_sys_category=="products_revolving"]
            tfproducts <- unlist(list_pr[,data_value])
            if (length(tfproducts) != length(products) && !all(tfproducts %in% products)) {
              stop(paste0("Eligible products are: [",paste0(products,collapse=","),"] and Revolving Products are: [",paste0(tfproducts,collapse=","),"]. These lists must have equal lengths"))
            }
            
            is_true <- sapply(tfproducts,FUN=function(p) {
              p <- superTrim(p)
              any(p==superTrim(products),na.rm = T) |
                any(p==superTrim(c("Yes","Oui","Si","True","Sim","Ja","Da")),na.rm=T)
            })
            
            list_sheet[indicator_sys_category=="products_revolving",
                       reporting_submitted_data_value:=paste0(products[is_true],collapse=",")]
            
            # if (!any(list_sheet$indicator_sys_category=="products_amortizing",na.rm=T)) {
            #   
            #   list_pr[,
            #           `:=`(data_value=paste0(products[!is_true],collapse=","),
            #                indicator_sys_category="products_amortizing",
            #                indicator_name=rsf_indicators[indicator_sys_category=="products_amortizing",indicator_name],
            #                reporting_submitted_data_value=paste0(products[!is_true],collapse=","))]
            #   
            #   list_sheet <- rbindlist(list(list_sheet,
            #                                list_pr))
            # }
            
          }
        }
        
        
        #atrisk
        if (any(list_sheet$indicator_sys_category=="products_undrawn_atrisk",na.rm=T)) {
          list_pr <- list_sheet[indicator_sys_category=="products_undrawn_atrisk"]
          tfproducts <- unlist(list_pr[,data_value])
          if (length(tfproducts) != length(products) && !all(tfproducts %in% products)) {
            stop(paste0("Eligible products are: [",paste0(products,collapse=","),"] and Undrawn Principal At-Risk Products are: [",paste0(tfproducts,collapse=","),"]. These lists must have equal lengths"))
          }
          
          is_true <- sapply(tfproducts,FUN=function(p) {
            p <- superTrim(p)
            any(p==superTrim(products),na.rm = T) |
              any(p==superTrim(c("Yes","Oui","Si","True","Sim","Ja","Da")),na.rm=T)
          })
          
          list_sheet[indicator_sys_category=="products_undrawn_atrisk",
                     reporting_submitted_data_value:=paste0(products[is_true],collapse=",")]
          
        }
      }
      
      list_sheet[,
                 reporting_template_row_group:='1TEMPLATE']
      
      list_sheet[,
                 reporting_template_data_rank:=1:.N] #1:.N instead of original row number since new data may be added in products
        
        
        
        
      }
    
    
    list_sheet <- list_sheet[,
                             .(reporting_template_row_group,
                               reporting_template_data_rank,
                               indicator_name,
                               reporting_submitted_data_value,
                               reporting_submitted_data_unit,
                               reporting_submitted_data_formula)]
    
    list_sheet[is.na(reporting_submitted_data_value),
               reporting_submitted_data_value:="N/A"]  
    
    list_sheet
  }
  
  data.summary <- {
  
    #new
    {
      label_matches <- lapply(summary_sheet,
                 FUN=function(x,find_sections,find_labels,match_id,match_postion) {
                   mapply(labelMatches,
                          find_sections=find_sections,
                          find_labels=find_labels,
                          match_id=match_id,
                          match_postion=match_postion,
                          MoreArgs=list(search_sections=rep(x="summary",times=length(x)),
                                        search_labels=normalizeLabel(x)),
                          USE.NAMES = F)
                   
                 },
                 find_sections=tolower(rsf_labels$template_section_lookup),
                 find_labels=tolower(rsf_labels$template_label_lookup),
                 match_id=rsf_labels$label_header_id,
                 match_postion=rsf_labels$template_header_position)
      
      label_cols <- sapply(label_matches,
                           function(x) length(unlist(x)),
                           USE.NAMES = TRUE)
      
      if (length(label_cols[label_cols > 0]) == 0) stop("Failed to indicator labels in the template Summary sheet: Expected on Column B")
      
      #Here, 2 refers to 2nd column, ie Colunm B.
      if (which.max(label_cols) != 2) stop("Indicators are expected in Column B (and Column C may be used for multi-language labeling).  However, labels appear to be entered primarily elsewhere?")
      
      
      label_cols <- label_cols[which(names(label_cols) %in% c("B","C"))] #only columns where labels are allowed
      
      #Ignore if no labels
      if (label_cols[2]==0) label_cols <- label_cols[-2]
      if (label_cols[1]==0) label_cols <- label_cols[-1]
      
      #label_cols <- label_cols[which(names(label_cols) %in% c("B"))] #only check B (the defined names are separately checked)
      label_cols_index <- which(names(label_matches) %in% names(label_cols))
      
      
      label_cols_names <- names(label_matches)[label_cols_index]
      label_matches <- label_matches[label_cols_index]
      
      #match_id is label_header_id
      label_matches <- rbindlist(lapply(which(label_cols>0),
                                        function(header_row,label_matches) { 
        x <- rbindlist(label_matches[[header_row]])
        x[,header_row:=header_row]
        return(x)
      },label_matches=label_matches))
      
      
      #match preference by exact index (compared to parse_template_IFC_QR2018) this is simpler and doesn't get into the template position matching with double && headers.
      {
        
        label_matches[,selected_exact_index_match:=FALSE]
        label_matches[rsf_labels[is.na(template_header_section_index)==FALSE,
                                 .(label_header_id,
                                   match_rows=suppressWarnings(as.numeric(template_header_section_index)))], #must be a numeric row to match SUMMARY row
                      selected_exact_index_match:=TRUE,
                      on=.(match_id=label_header_id,
                           match_rows)]
        
        #if we matched the header_id but DID NOT match the exact match row ID, then omit this header entirely because its ONLY purpose is to match that label on that row number
        #and cannot introduce amibuity on other rows.
        label_matches[selected_exact_index_match==FALSE & match_id %in% rsf_labels[is.na(template_header_section_index)==FALSE,label_header_id],
                      selected_exact_index_match:=NA]
        
        label_matches <- label_matches[!is.na(selected_exact_index_match)]
        
        label_matches[,selected_preference:=frank(.SD,-selected_exact_index_match,ties.method="dense"),
                      by=.(match_rows)]
        
        label_matches[,
                      selected:=selected_preference==min(selected_preference),
                      by=.(match_rows)]
        
        label_matches <- label_matches[selected==TRUE]
        label_matches[,
                      `:=`(selected=NULL,
                           selected_preference=NULL,
                           selected_exact_index_match=NULL)]
      }
      
      label_matches <- label_matches[rsf_labels[,.(label_header_id,action,map_indicator_id,map_formula_id,map_check_formula_id)],
                                     on=.(match_id=label_header_id),
                                     nomatch=NULL]
      label_matches <- label_matches[,
                                     .(header_ids=list(unique(match_id)),
                                       header_cols=list(header_row)),
                                     by=.(original_row_num=match_rows,
                                          action,
                                          map_indicator_id,
                                          map_formula_id,
                                          map_check_formula_id)]
      
      #If col "C" has been omitted due to being blank or hidden, etc it will be pulled back in here, but not have label matches included
      #And therefore the label matches have been matched only against B.
      data_cols <- names(summary_sheet)[c(2,3,4,5)] #Cols B,C,D,E
      data_cols <- c(data_cols,"original_row_num")
      
      formula_cols <- names(summary_formula_matrix)[c(2,3,4,5)]
      summary_formula_matrix <- summary_formula_matrix[,..formula_cols] #df not dt
      
      summary_sheet <- summary_sheet[,
                                     ..data_cols]
      
      #NOTE!
      #Column defined_name expects,by default, to have template's Defined Names
      #As included in the Template sheet.
      #However, users may delete these entirely (all NA) if not used/useful after setup
      #OR 
      #They may use this column for bi-lingual templates
      summary_sheet <- setnames(summary_sheet,
                                old=names(summary_sheet),
                                new=c("label",
                                      "defined_name",
                                      "data_unit",
                                      "data_value",
                                      "original_row_num"))
      
      
      #If defined name is blank entirely then it will be read-in as logical/numeric NA values and won't "melt" with character values.
      set(summary_sheet,i=NULL,j="defined_name",value=as.character(summary_sheet$defined_name))
      set(summary_sheet,i=NULL,j="label",value=as.character(summary_sheet$label))
      
      summary_sheet[,data_column_num:=4]
      
      summary_sheet[,data_unit:=superTrim(data_unit,
                                          to.lower.case=FALSE,
                                          empty.is.NA=TRUE)]
      
      summary_sheet[,data_value:=superTrim(data_value,
                                           to.lower.case=FALSE,
                                           empty.is.NA=TRUE)]
  
      
      all_blanks <- which(sapply(as.data.frame(is.na(t(summary_sheet[,.(label,defined_name,data_unit,data_value)]))),all))
      
      all_blanks <- which(sapply(as.data.frame(is.na(t(summary_sheet[,.(label,defined_name,data_unit,data_value)]))),all))
      if (any(all_blanks)) summary_sheet <- summary_sheet[-all_blanks]
      
      no_labels <- which(sapply(as.data.frame(is.na(t(summary_sheet[,.(label,defined_name)]))),all))
      if (any(no_labels)) summary_sheet <- summary_sheet[-no_labels]
      
      #Identified section headers
      one_label_no_data <- which(sapply(as.data.frame(is.na(t(summary_sheet[,.(label,defined_name)]))),any) &
                                 sapply(as.data.frame(is.na(t(summary_sheet[,.(data_unit,data_value)]))),all))
      
      #Exclude...UNLESS we want to do something with this header label and it's matched a mapping action
      one_label_no_data <- one_label_no_data[!(summary_sheet[one_label_no_data,original_row_num %in% label_matches$original_row_num])]
      if (any(one_label_no_data,na.rm = T)) summary_sheet <- summary_sheet[-one_label_no_data]
      
      #Data values are reported in the data units column (and data "values" are themselves blank.  Ie, we've imported only units.  So interpret as values without units)
      summary_sheet[is.na(data_value) & 
                    !is.na(data_unit),
                    `:=`(data_value=data_unit,
                         data_unit=as.character(NA),
                         data_column_num=3)]
  
      #label column comes before defined_name column 
      #and therefore labels will be header_row=1
      #and defined_names will be header_row=2
      summary_sheet <- melt.data.table(summary_sheet,
                                        id=c("original_row_num","data_column_num","data_value","data_unit"), 
                                        variable.factor = F,
                                        variable.name="header_row",
                                        value.factor=F,
                                        value.name="label")
      #"header_row" will contain "label" (the typed label) and "defined_name" (if one is available)
      #and this will allow comparing the defined names and the labels they are supposed to match with.
      summary_sheet[,
                    header_row:=seq_along(unique(header_row)),
                    by=.(original_row_num)]
      summary_sheet[,header_row:=as.numeric(header_row)] #were previously column names, character data type
      
      
      summary_sheet[,label_normalized:=normalizeLabel(label)]
      
      summary_sheet[is.na(label),
                    label_normalized:=normalizeLabel("NA")]
     
      setorder(summary_sheet,
               original_row_num,
               header_row,
               na.last = TRUE)
      
      #a defind_name label is not required
      #The template uses this functionality to help users match-up where defined names may not match-up with labels they're using for formulas
      summary_sheet <- summary_sheet[!(is.na(label) & header_row==2)]
      
      summary_sheet <- label_matches[summary_sheet,
                                     on=.(original_row_num)]  
    }
    

    #Label errors/mismatching
    {
      
      summary_sheet[,
                     ignore:=anyNA(action)==FALSE & all(action=="ignore"),
                     by=.(original_row_num)]

      #Information before the Project ID is not structured the same and has no discrete labels.
      summary_sheet[original_row_num <= summary_sheet_ID_row &
                    original_row_num > 0,
                    ignore:=TRUE]
      
      
      #stop_row <- summary_sheet[map_indicator_id %in% rsf_indicators[indicator_sys_category=="template_read_stop",indicator_id],original_row_num]
      
      #For defined (but unmatched!) stop row definitions.
      #It will be unmatched when the template header is set to "" or NA to simply specifiy that, eg, "SUMMARY:100" is the stop row for the template.
      stop_row <- rsf_labels[is.na(template_header_section_index)==FALSE &
                             sapply(template_section_lookup,grepl,x="summary",ignore.case=T) & #for summary tab
                             superTrim(label) %in% c("","na") &                                #for missing/NA labels
                             map_indicator_id %in% rsf_indicators[indicator_sys_category=="template_read_stop",indicator_id],
                             suppressWarnings(as.numeric(template_header_section_index))]
      stop_row <- na.omit(stop_row)
      if (length(stop_row) > 0) {
        stop_row <- max(stop_row) #max stop row is used for generic/missing/NA header.
        summary_sheet[original_row_num > stop_row,
                      ignore:=TRUE]
      }
      
      #changed to label_matches versus summary_sheet because the stop row could have matched a row that was removed already (like if stop row matched label is in column A)
      stop_row <- label_matches[map_indicator_id %in% rsf_indicators[indicator_sys_category=="template_read_stop",indicator_id],original_row_num]
      if (length(stop_row) > 0) {
        stop_row <- min(stop_row)
        summary_sheet[original_row_num >= stop_row,
                      ignore:=TRUE]
      }
      
      #summary_sheet[ignore==TRUE]
      summary_sheet <- summary_sheet[ignore==FALSE]
      
      
      summary_sheet[rsf_indicators,
                    indicator_name:=i.indicator_name,
                    on=.(map_indicator_id=indicator_id)]
      
      #Will fail because its ambiguous
      {
        summary_sheet[,
                       mismatch:=anyNA(action)==FALSE & length(unique(map_indicator_id))>1,
                       by=.(original_row_num)]
        
        mismatch_labels <- summary_sheet[mismatch==TRUE]
        if (!empty(mismatch_labels)) {
          
         
          mismatch_labels[,
                          message:=paste0("Summary Sheet Row ",original_row_num," column ",header_row," \"",label,"\" maps to \"",indicator_name,"\"")]
          setorder(mismatch_labels,
                   original_row_num,
                   header_row)
          
          message <- paste0(mismatch_labels$message,collapse=" \n")
          stop(paste0("Mismatched Column Labels:\n",
                      "Correct the column name(s) in Summary Tab \n",
                      "Or if this is a Template Requirement map these columns in JASON -> RSF Setup -> Setup Templates -> ",template_lookup$template_name," \n\n",
                      message))
        }
      }    
      
      #Unfound: will asign to entity_reporting
      {
        summary_sheet[,
                       notfound:=all(is.na(action)) & !any(label_normalized=="na"),
                       by=.(original_row_num)]
        
        unfound_labels <- summary_sheet[notfound==TRUE]
        
        if (!empty(unfound_labels)) {
          
          setorder(unfound_labels,
                   original_row_num,
                   header_row)
          
          unfound_labels[,
                         `:=`(rsf_pfcbl_id=rsf_pfcbl_id.facility,
                              indicator_id=as.numeric(NA), #will be auto-assigned to reporting indicator
                              reporting_asof_date=reporting_asof_date,
                              check_name="sys_flag_indicator_not_found",
                              check_message=paste0("Summary Tab Row ",original_row_num," Column ",openxlsx2::int2col(header_row+1)," \"",label,"\""))]
          
          unfound_labels <- unfound_labels[,.(rsf_pfcbl_id,
                                              indicator_id,
                                              reporting_asof_date,
                                              check_name,
                                              check_message)]
          
          reporting_flags <- rbindlist(list(reporting_flags,
                                            unfound_labels))
        }
      }
      
      #summary_sheet[is.na(action)==T][!(notfound|untranslated)] #Should yield an empty table: ie, no action, but represented by an error message.
      summary_sheet <- summary_sheet[is.na(action)==FALSE]
      
      #special management for currency ratios and if the currency code is being reported in the label
      {
        currency_reporting <- summary_sheet[rsf_indicators[data_type=="currency_ratio",.(indicator_id,default_data_unit=data_unit)],
                                            on=.(map_indicator_id=indicator_id),
                                            nomatch=NULL]
        
        currency_reporting[,
                           label_data_unit:=as.character(NA)]
        
        currency_reporting[grepl("[a-zA-z]{3}/[a-zA-Z]{3}",label),
                           label_data_unit:=toupper(gsub("^.*([a-zA-z]{3}/[a-zA-Z]{3}).*$","\\1",label))]
        
        currency_reporting[,
                           n:=length(na.omit(unique(label_data_unit))),
                           by=.(original_row_num)]
    
        ambiguous_units <- currency_reporting[n > 1]
        if (!empty(ambiguous_units)) {
          
          stop(paste0(unique(paste0("Ambiguous fx ratios reported for ",
                             ambiguous_units$indicator_name,
                             " on Summary tab row ",ambiguous_units$original_row_num,": ",
                             paste0("\"",ambiguous_units$label,"\" reports ",
                                    "\"",ambiguous_units$label_data_unit,"\"",
                                    collapse=" vs ")),collapse=" AND ALSO \n")))
        }
        currency_reporting <- unique(currency_reporting[(is.na(data_unit) |
                                                           grepl("[a-zA-z]{3}/[a-zA-Z]{3}",data_unit)==FALSE) &
                                                          is.na(label_data_unit)==FALSE,
                                                        .(original_row_num,
                                                          map_indicator_id,
                                                          label_data_unit)])
        summary_sheet[currency_reporting,
                      data_unit:=i.label_data_unit,
                      on=.(original_row_num,
                           map_indicator_id)]
      }
      
      
      #currency unit reporting in label
      {
        currency_unit_reporting <- summary_sheet[rsf_indicators[data_type=="currency",.(indicator_id,default_data_unit=data_unit)],
                                            on=.(map_indicator_id=indicator_id),
                                            nomatch=NULL]
        
        unit_labs <- sapply(c(CALCULATIONS_ENVIRONMENT$VALID_CURRENCIES,"LCY","LCU"),
                            FUN=function(x,labs) { 
                              stringr::str_detect(string=toupper(superTrim(labs)),
                                                  pattern=paste0("^",x,"[[:space:]]+|[[:space:]]+",x,"[[:space:]]+|[[:space:]]+",x,"$"))
                 
                      },labs=currency_unit_reporting$label_normalized,
                      USE.NAMES=T)
        
        unit_labs <- as.data.table(unit_labs)
        unit_rows <- which(rowSums(unit_labs)>0) #if it's >1 it means multiple currencies matched; which is an error.
        
        if (length(unit_rows) > 0) {
          unit_values <- names(unit_labs)[sapply(as.data.frame(t(unit_labs[unit_rows])),which.max)] #which.max will return the first currency, if multiple happen to be entered.
                                                                                                    #which will either be a correct guess...or return errors elsewhere.
          set(currency_unit_reporting,
              i=unit_rows,
              j="label_unit",
              unit_values)
          
          currency_unit_reporting <- currency_unit_reporting[(is.na(data_unit) & !is.na(label_unit)) |
                                                             (data_unit %in% c("LCU","LCY") & !label_unit %in% c("LCU","LCY"))]
          
          if (!empty(currency_unit_reporting)) {
            summary_sheet[currency_unit_reporting,
                          data_unit := i.label_unit,
                          on=.(original_row_num)]
            
            currency_unit_reporting <- currency_unit_reporting[,
                                                               .(rsf_pfcbl_id=NA,
                                                                 indicator_id=map_indicator_id,
                                                                 reporting_asof_date=reporting_asof_date,
                                                                 check_name="sys_flag_data_unit_auto_correction",
                                                                 check_message=paste("Currency unit {",label_unit,"} inferred from Label {",label,"}"))]
            
            reporting_flags <- rbindlist(list(reporting_flags,
                                              currency_unit_reporting))
          }
        }        
      }
      
     
    }
    
    
    #If the data unit is a formula, it's probably just lazy data entry.  Make the unit equal to another cell's unit instead of retyping.
    #Unlikley that the unit is actually a calculated result.
    summary_sheet[,
                  data_formula:=mapply(function(i,j,x) { 
                    if (i <= 0 || j <=0) { as.character(NA) #because List data is added as negative original row number
                    } else { x[i,j] }
                  },
                  i=original_row_num,
                  j=data_column_num,
                  MoreArgs=list(x=as.matrix(summary_formula_matrix)),
                  SIMPLIFY = TRUE)]
    
    if (save_headers) {
      template_headers <- rbindlist(list(template_headers,
                                         summary_sheet[!is.na(map_indicator_id),
                                                       .(label,
                                                         label_formula=data_formula,
                                                         data_source_index=paste0("SUMMARY ROW-",original_row_num),
                                                         indicator_id=map_indicator_id)]))
    }
    
    
    summary_sheet <- summary_sheet[is.na(map_indicator_id)==FALSE,
                                   .(indicator_name,
                                     data_unit,
                                     data_value,
                                     data_formula,
                                     original_row_num,
                                     data_column_num)]
    
    summary_sheet <- unique(summary_sheet)
    
    
    summary_sheet[,
                  reporting_template_row_group:='1SUMMARY']
    summary_sheet[,
                  reporting_template_data_rank:=original_row_num]
    
    setnames(summary_sheet,
             old=c("data_unit","data_value","data_formula"),
             new=c("reporting_submitted_data_unit",
                   "reporting_submitted_data_value",
                   "reporting_submitted_data_formula"))
    
    summary_sheet <- unique(summary_sheet)
    
    summary_sheet[rsf_indicators[!is.na(data_unit),
                              .(indicator_name,data_unit,joincondition=as.character(NA))],
               reporting_submitted_data_unit := i.data_unit,
               on=.(indicator_name,
                    reporting_submitted_data_unit=joincondition)]
    
    summary_sheet <- summary_sheet[,
                                   .(reporting_submitted_data_formula=ifelse(all(is.na(reporting_submitted_data_formula)),
                                                                             as.character(NA),
                                                                             paste0(na.omit(reporting_submitted_data_formula),collapse=","))),
                                   by=.(reporting_template_row_group,
                                        reporting_template_data_rank,
                                        indicator_name,
                                        reporting_submitted_data_value,
                                        reporting_submitted_data_unit)]
    
    summary_sheet <- summary_sheet[,
                                   .(reporting_template_row_group,
                                     reporting_template_data_rank,
                                     indicator_name,
                                     reporting_submitted_data_value,
                                     reporting_submitted_data_unit,
                                     reporting_submitted_data_formula)]
    
    if (!any(summary_sheet$indicator_name %in% rsf_indicators[indicator_sys_category=="template_file" & data_category=="facility",indicator_name])) {
      
      next_rank <- max(suppressWarnings(as.numeric(summary_sheet[reporting_template_row_group=="1SUMMARY"]$reporting_template_data_rank)),na.rm=T)
      if (length(next_rank)==0 || is.infinite(next_rank)) { next_rank <- "1" 
      } else { next_rank <- as.character(next_rank+1) }
      
      tfdata <- data.table(reporting_template_row_group="1SUMMARY",
                           reporting_template_data_rank=next_rank,
                           indicator_name=rsf_indicators[indicator_sys_category=="template_file" & data_category=="facility",indicator_name],
                           reporting_submitted_data_value=basename(template_file),
                           reporting_submitted_data_unit=as.character(NA),
                           reporting_submitted_data_formula=as.character(NA))
      
      summary_sheet <- rbindlist(list(summary_sheet,
                                      tfdata))
      
    }
    
    summary_sheet
    
  }

  data.quarterly <- {
    
    {
      QREPORT_startrow <- nregions_table[grepl("Template_QReport_StartRow",name,ignore.case=T),data_value]

      if (!length(QREPORT_startrow) || 
          suppressWarnings(is.na(as.numeric(QREPORT_startrow)))) {
        if (all(grepl("^#",unlist(QREPORT_startrow$data_value)))) {
          stop(paste0("Template_QReport_StartRow defined name on ",QREPORT_startrow$value,
                      " is an error value: '",QREPORT_startrow$data_value,"' confirm that template has not been corrupted.  Re-enter the formula if needed to force it to recompute"))
        } else {
          stop("Failed to find defined name range Template_qreport_startrow in template. Has template been modified or corrupted?  Ensure automatic calculations are turned ON and saved")
        }
      } 
      
      QREPORT_startrow <- as.numeric(QREPORT_startrow)
      
      label_matches <- lapply(as.data.frame(t(data_sheet[1:(QREPORT_startrow-1)])),
                              FUN=function(x,find_sections,find_labels,match_id,match_postion) {
                                
                                mapply(labelMatches,
                                       find_sections=find_sections,
                                       find_labels=find_labels,
                                       match_id=match_id,
                                       match_postion=match_postion,
                                       MoreArgs=list(search_sections=rep(x="qreport",times=length(x)), #lower case because of tolower() and normalizeLabel
                                                     search_labels=normalizeLabel(x)),
                                       USE.NAMES = F)
                                
                              },
                              find_sections=tolower(rsf_labels$template_section_lookup),
                              find_labels=tolower(rsf_labels$template_label_lookup),
                              match_id=rsf_labels$label_header_id,
                              match_postion=rsf_labels$template_header_position)
      
      label_rows <- sapply(label_matches,
                           function(x) length(unlist(x)),
                           USE.NAMES = TRUE)
      
      if (length(label_rows[label_rows > 0]) ==0) stop(paste0("Failed to find any QReport indicator labels in the template (which are expected to be found on QReport sheet row ",(QREPORT_startrow-1),")"))

      #Teams can enter bi-lingual headers on other rows, if they like
      if (which.max(label_rows) != (QREPORT_startrow-1)) stop(paste0("Template defined name Template_qreport_startrow specifies QReport headers start on ROW ",(QREPORT_startrow-1),
                                                                     ". But most headers are on ROW ",which.max(label_rows),
                                                                     ". Has the template been modified or corrupted? Otherwise, ensure headers are on ROW ",QREPORT_startrow-1," and facility ",
                                                                     "data starts ",QREPORT_startrow,".  If this is a bi-lingual template, non-English headers may be added in rows 2 or 3 and/or modify the default headers and section titles on these rows.  However, the english headers are expected to be on Row ",(QREPORT_startrow-1)," and the QR data is expected to start on Row ",(QREPORT_startrow)))
      
      
      label_rows <- label_rows[label_rows > 0]
      label_rows_index <- which(names(label_matches) %in% names(label_rows))
      label_matches <- label_matches[names(label_rows)]
      
      data_labels <- data_sheet[label_rows_index]
      label_cols_index <- which(!sapply(as.data.frame(is.na(data_labels)),all))
      
      if (!(QREPORT_startrow == max(label_rows_index)+1)) {
        stop(paste0("QREPORT_startrow defined by named region Template_qreport_startrow equals ",
                    QREPORT_startrow,
                    " but max(labels_rows_index)+1 equals ",max(label_rows_index)+1)," and these should be equal")
      }
      
      data_rows <- seq(from=max(label_rows_index)+1,
                       to=nrow(data_sheet))
      
      na_data <- rowSums(is.na(data_sheet))==ncol(data_sheet)
      
      if (any(na_data)) {
        na_ratio <- (sum(rowSums(is.na(data_sheet))==ncol(data_sheet)) / nrow(data_sheet))
        #What is the ratio of fully NA rows to total number of rows?
        #If it is high, it implies that the Excel may have a lot of junk data -- such as formats applied to nothing.
        #This takes a lot of time to read in (eg, if user has formatted to end of possible Excel limit, this is 1 million+ rows that we'll be parsing)
        #Do we have more than 1000 rows defined and more than 85% blank?
        if (nrow(data_sheet) > 1000 &
            na_ratio > 0.85) {
          status_message(class="warning",
                         "Sheet QReport defines ",nrow(data_sheet)," rows and ",round(nrow(data_sheet) * na_ratio)," are entirely blank! \n",
                         "This takes a long time to parse and then discard blank rows. \n",
                         "Ensure that Excel formats are not filled-in until the last row and no columns have filled-down blanks or other defaults that generate unnecessary empty rows. ",
                         "To clean up and improve upload speed: \n",
                         "  - click on the first blank data row number \n",
                         "  - do Ctrl+Shift+Down Arrow to select all empty rows to the end of the sheet \n",
                         "  - Right-click mouse and select 'Delete' \n",
                         "  - Save As an updated file name")
          
          
          # status_message(class="warning",
          #                "System will attempt to auto-cleanup empty rows.  By ignoring information after row ",)
          
        }
      }
      
      #Empty submitted blank data sheet!
      if ( (max(label_rows_index)==nrow(data_sheet)) ||
           (max(label_rows_index)+1 > nrow(data_sheet)) ) data_rows <- 0
      
      data_sheet <- data_sheet[data_rows]
      data_sheet_cols <- names(data_sheet)
      data_formulas <- data_formula_matrix[data_rows,..data_sheet_cols]
      
      setnames(data_formulas,
               old=names(data_formulas),
               new=names(data_sheet))
      
      label_matches <- rbindlist(lapply(seq_along(label_rows),function(header_row,label_matches) { 
        x <- rbindlist(label_matches[[header_row]])
        x[,header_row:=header_row]
        return(x)
        },label_matches=label_matches))
      
     
      
      label_matches <- unique(label_matches)
      
      label_matches <- label_matches[rsf_labels[,.(label_header_id,action,map_indicator_id,map_formula_id,map_check_formula_id)],
                                     on=.(match_id=label_header_id),
                                     nomatch=NULL]
      
      label_matches <- label_matches[,
                                     .(header_ids=list(unique(match_id))),
                                     .(original_col_num=openxlsx2::int2col(match_rows),header_row,
                                       action,map_indicator_id,map_formula_id,map_check_formula_id)]
    }

    
    #data labels
    {

      no_data_reported <- which(sapply(data_sheet,function(x) all(is.na(x)),USE.NAMES = T))
      no_data_reported <- data.table(rn=names(no_data_reported),original_col_num=openxlsx2::int2col(no_data_reported))
      
      data_cols_names <- names(label_cols_index)
      
      data_labels <- data_labels[,
                                 ..data_cols_names]
      
      data_labels <- as.data.table(as.data.frame(t(data_labels)),
                                   keep.rownames = T)
      
      setnames(data_labels,
               old=c("V1"),
               new=c("label"))

      data_labels <- melt.data.table(data_labels,
                           id="rn",
                           variable.factor = F,
                           value.factor = F,
                           variable.name = "header_row",
                           value.name="label")

      setnames(data_labels,
               old="rn",
               new="original_col_num")      
      
      
      
      # data_labels[,
      #              original_col:=as.numeric(openxlsx2::col2int(rn))]
      
      #defined names aren't matched for QReport template
      #Maybe that should change?
      data_labels[,
                    header_row:=seq_along(unique(header_row)),
                    by=.(original_col_num)]
      
      data_labels[,header_row:=as.numeric(header_row)] #were previously column names, character data type
      
      
      data_labels <- label_matches[data_labels,
                                     on=.(original_col_num,header_row),
                                   nomatch=NA]  
      
      data_labels[no_data_reported,
                  no_data:=TRUE,
                  on=.(original_col_num)]
      
      data_labels[,
                  no_data:=any(no_data,na.rm=T),
                  by=.(original_col_num)]
      
      data_labels[rsf_indicators,
                  indicator_name:=i.indicator_name,
                  on=.(map_indicator_id=indicator_id)]
      
      #Modify labels for error messages
      data_labels[,
                    label:=trimws(label)]
      
      data_labels[,
                  ignore:=FALSE]
      data_labels[ignore==FALSE,
                  ignore:=anyNA(action)==FALSE & all(action=="ignore"),
                  by=.(original_col_num)]
      
      data_labels[ignore==FALSE,
                  ignore:=all(is.na(action)) & all(no_data),
                  by=.(original_col_num)]
      
      
      data_labels[,semi_missing:=any(is.na(label)) & !all(is.na(label)),
                  by=.(original_col_num)]
      
      
      data_labels[is.na(label) & semi_missing==TRUE,
                  ignore:=TRUE]
      
      data_labels[,semi_missing:=NULL]
      #data_labels[ignore==T]
      data_labels <- data_labels[ignore==FALSE]
      
      
      
      #Did we match an indicator and also its unit_fx_indicator_id? Reconcile.
      {
        #unit_fx_indicator_id is the base indicator in LCU
        unit_fx_indicators <- rsf_indicators[!is.na(unit_fx_indicator_id),.(unit_fx_indicator_id,indicator_id,data_unit,indicator_name)]
        data_labels[,
                    `:=`(unit_fx_defined=NA,
                         ignore=NA)]
        
        data_labels[unit_fx_indicators,
                    unit_fx_defined:=mapply(grepl,pattern=i.data_unit,x=label,MoreArgs=list(ignore.case=T)),
                    on=.(map_indicator_id=indicator_id)]
        
        data_labels[,
                    ignore:=is.na(unit_fx_defined) & any(!is.na(unit_fx_defined),na.rm=T),
                    by=.(original_col_num)]
        
        data_labels <- data_labels[ignore==FALSE]
        data_labels[,unit_fx_defined:=NULL]
      }
      
      #Will fail because its ambiguous
      {
        data_labels[,
                      mismatch:=anyNA(action)==FALSE & length(unique(map_indicator_id))>1,
                      by=.(original_col_num)]
        
        mismatch_labels <- data_labels[mismatch==TRUE]
        if (!empty(mismatch_labels)) {
          
          
          mismatch_labels[,
                          message:=paste0("QReport Sheet row ",header_row," column ",original_col_num," \"",label,"\" maps to \"",indicator_name,"\"")]
          
          setorder(mismatch_labels,
                   original_col_num,
                   header_row)
          
          message <- paste0(mismatch_labels$message,collapse=" \n")
          stop(paste0("Mismatched Column Labels:\n",
                      "Correct the column name(s) in QReport Tab \n",
                      "Or if this is a Template Requirement map these columns in JASON -> RSF Setup -> Setup Templates -> ",template_lookup$template_name," \n\n",
                      message))
        }
      }    
      
      #Unfound: will asign to entity_reporting
      {
        data_labels[,
                      notfound:=all(is.na(action)),
                      by=.(original_col_num)]
        
        unfound_labels <- data_labels[notfound==TRUE]
        
        if (!empty(unfound_labels)) {
          
          setorder(unfound_labels,
                   original_col_num,
                   header_row)
          
          unfound_labels[,
                         `:=`(rsf_pfcbl_id=rsf_pfcbl_id.facility,
                              indicator_id=as.numeric(NA), #will be auto-assigned to reporting indicator
                              reporting_asof_date=reporting_asof_date,
                              check_name="sys_flag_indicator_not_found",
                              check_message=paste0("QReport Column ",original_col_num," Row ",header_row,"  \"",label,"\""))]
          
          unfound_labels <- unfound_labels[,.(rsf_pfcbl_id,
                                              indicator_id,
                                              reporting_asof_date,
                                              check_name,
                                              check_message)]
          
          reporting_flags <- rbindlist(list(reporting_flags,
                                            unfound_labels))
        }
      }
      
      #currency unit reporting in label
      {
        data_labels[,label_unit:=as.character(NA)]
        
        currency_unit_reporting <- data_labels[rsf_indicators[data_type=="currency",.(indicator_id,default_data_unit=data_unit)],
                                               on=.(map_indicator_id=indicator_id),
                                               nomatch=NULL]
        
        unit_labs <- sapply(c(CALCULATIONS_ENVIRONMENT$VALID_CURRENCIES,"LCY","LCU"),
                            FUN=function(x,labs) { 
                              unit_pattern <- paste0(c(paste0("^",x,"[[:space:]]+"),                  #EUR blah blah...
                                                       paste0("in[[:space:]]+",x,"[[:space:]]+"),     #blah blah in EUR blah blah...
                                                       paste0("[[:space:]]+",x,"[[:space:]]+based"),  #blah blah EUR based blah blah...
                                                       paste0("[[:space:]]+",x,"[[:space:]]+total"),  #blah blah EUR total blah blah...
                                                       paste0("[[:space:]]+",x,"$")),                 #...blah blah EUR
                                                     collapse="|")
                              
                              stringr::str_detect(string=toupper(superTrim(labs)),
                                                  pattern=unit_pattern)
                              
                            },labs=normalizeLabel(currency_unit_reporting$label), #all punctuation removed, so "blah blah (EUR)" normalizes as "blah blah EUR"
                            USE.NAMES=T)
        
        unit_labs <- as.data.table(unit_labs)
        unit_rows <- which(rowSums(unit_labs)>0) #if it's >1 it means multiple currencies matched; which is an error.
        
        if (length(unit_rows) > 0) {
          unit_values <- names(unit_labs)[sapply(as.data.frame(t(unit_labs[unit_rows])),which.max)] #which.max will return the first currency, if multiple happen to be entered.
          #which will either be a correct guess...or return errors elsewhere.
          set(currency_unit_reporting,
              i=unit_rows,
              j="label_unit",
              unit_values)
          
          currency_unit_reporting <- currency_unit_reporting[!is.na(label_unit)]
          currency_unit_reporting[,unit_mismatch:=length(na.omit(unique(label_unit)))>1,
                                  by=.(original_col_num)]
          
          multi_units  <- currency_unit_reporting[unit_mismatch==TRUE]
          if (!empty(multi_units)) {
            
              setorder(multi_units,
                       original_col_num,
                       header_row)
              
              multi_units <- multi_units[,
                                                .(rsf_pfcbl_id=NA,
                                                  reporting_asof_date=reporting_asof_date,
                                                  indicator_id=map_indicator_id,
                                                  check_name="sys_flag_data_invalid_unit",
                                                  check_message=paste0("QReport column ",original_col_num," defines different currency units across two labels: ",
                                                                      paste0("{",label,"}",collapse=" AND "),". ",
                                                                      "Value {",label_unit[length(label_unit)],"} will be used for this data")),
                                    by=.(original_col_num)]
              
              
              
              reporting_flags <- rbindlist(list(reporting_flags,
                                                unique(multi_units[,.(rsf_pfcbl_id,
                                                                      indicator_id,
                                                                      reporting_asof_date,
                                                               
                                                               check_name,
                                                               check_message)])))
            
          }
          
          if (!empty(currency_unit_reporting)) {
            currency_unit_reporting <- currency_unit_reporting[,
                                                               .(label_unit=label_unit[length(label_unit)],
                                                                 label=label[length(label_unit)]),
                                                               by=.(original_col_num,map_indicator_id)]
            data_labels[currency_unit_reporting,
                          label_unit := i.label_unit,
                          on=.(original_col_num)]
            
            
            currency_unit_reporting <- currency_unit_reporting[,
                                                               .(rsf_pfcbl_id=NA,
                                                                 indicator_id=map_indicator_id,
                                                                 reporting_asof_date=reporting_asof_date,
                                                                 check_name="sys_flag_data_unit_auto_correction",
                                                                 check_message=paste0("Currency unit {",label_unit,"} inferred from Label {",label,"}"))]
            
            
            reporting_flags <- rbindlist(list(reporting_flags,
                                              currency_unit_reporting))
          }
        }        
        
        
        
      }
      
      if (save_headers) {
        template_headers <- rbindlist(list(template_headers,
                                           unique(data_labels[!is.na(map_indicator_id),
                                                         .(label,
                                                           label_formula=as.character(NA),
                                                           data_source_index=paste0("QReport COL-",original_col_num),
                                                           indicator_id=map_indicator_id)])))
      }
      
      data_labels <- data_labels[,
                                 ignore:=is.na(action) | action=="ignore"]
      
      #Unfound: will asign to entity_reporting
      {
        mismatched_labels <- data_labels[is.na(action)]
        
        if (!empty(mismatched_labels)) {
          mismatched_labels[data_labels[ignore==FALSE,.(indicator_name=paste0(indicator_name,collapse=" & ")),by=.(original_col_num)],
                         matched_indicator:=i.indicator_name,
                         on=.(original_col_num)]
          
          setorder(mismatched_labels,
                   original_col_num,
                   header_row)
          
          mismatched_labels[,
                         `:=`(rsf_pfcbl_id=rsf_pfcbl_id.facility,
                              indicator_id=as.numeric(NA), #will be auto-assigned to reporting indicator
                              reporting_asof_date=reporting_asof_date,
                              check_name="sys_flag_indicator_ignored",
                              check_message=paste0("Ignored \"",label,"\" on QReport Column ",original_col_num," because matched \"",matched_indicator,"\": should an alternative label be added in system indicators?"))]
          
          mismatched_labels <- mismatched_labels[,.(rsf_pfcbl_id,
                                              indicator_id,
                                              reporting_asof_date,
                                              check_name,
                                              check_message)]
          
          reporting_flags <- rbindlist(list(reporting_flags,
                                            mismatched_labels))
        }
      }
      
      data_labels <- data_labels[ignore==FALSE,
                                 .(original_col_num,
                                   action,
                                   map_indicator_id,
                                   map_formula_id,
                                   map_check_formula_id,
                                   indicator_name,
                                   label_unit)]
      
      data_labels <- unique(data_labels)
      
  
    }
    
    
    #data_cols_index <- which(names(data_sheet) %in% data_cols_names)
    # rename <- unique(data_labels[,.(rn,original_col_num)])
    # setnames(data_sheet,
    #          old=rename$rn,
    #          new=rename$original_col_num)

    # setnames(data_formulas,
    #          old=rename$rn,
    #          new=rename$original_col_num)
    # 
    data_cols_names <- data_labels$original_col_num
    data_cols_names <- data_cols_names[order(nchar(data_cols_names),data_cols_names)]
    data_sheet <- data_sheet[,
                             ..data_cols_names]
    
    data_formulas <- data_formulas[,
                                   ..data_cols_names]
    
    #1:.N will be offset equal to QREPORT_startrow
    data_sheet[,
               reporting_template_row_group:=paste0(1:.N,"QREPORT")] #Fundamental to keep the original row number/order intact since QR template is columnar data.
    
    data_formulas[,
                  reporting_template_row_group:=paste0(1:.N,"QREPORT")]
    

    data_sheet <- melt.data.table(data_sheet,
                                  id="reporting_template_row_group",
                                  variable.name="original_col_num",
                                  variable.factor=FALSE,
                                  value.factor = FALSE,
                                  value.name="reporting_submitted_data_value")
    
    data_formulas <- melt.data.table(data_formulas,
                                     id="reporting_template_row_group",
                                     variable.name="original_col_num",
                                     variable.factor=FALSE,
                                     value.factor = FALSE,
                                     value.name="reporting_submitted_data_formula")
    
    data_sheet[,
               reporting_submitted_data_unit:=as.character(NA)]
    
    data_sheet[data_formulas,
               reporting_submitted_data_formula:=i.reporting_submitted_data_formula,
               on=.(reporting_template_row_group,
                    original_col_num)]
    
    data_sheet[data_labels,
               `:=`(indicator_name=i.indicator_name,
                    reporting_submitted_data_unit=i.label_unit),
               on=.(original_col_num)]
    
    data_sheet[rsf_indicators[!is.na(data_unit),
                              .(indicator_name,data_unit,joincondition=as.character(NA))],
               reporting_submitted_data_unit := i.data_unit,
               on=.(indicator_name,
                    reporting_submitted_data_unit=joincondition)]
    
    #empty rows (of the excel sheet), not columns
    empty_rows <- data_sheet[,
                             .(blank=all(is.na(reporting_submitted_data_value) | nchar(superTrim(reporting_submitted_data_value))==0)),
                             by=.(reporting_template_row_group)
                             ][blank==T,reporting_template_row_group]
    
    data_sheet <- data_sheet[!(reporting_template_row_group %in% empty_rows)]
    
    
    empty_ids <- data_sheet[rsf_indicators[,.(indicator_sys_category,indicator_name)],
                            on=.(indicator_name)
                            ][!is.na(indicator_sys_category),
                              .(noids=all(is.na(reporting_submitted_data_value) | nchar(superTrim(reporting_submitted_data_value))==0)),
                              by=.(reporting_template_row_group)
                              ][noids==TRUE,
                                reporting_template_row_group]
    
    empty_ids <- empty_ids[!is.na(empty_ids)]
    if (length(empty_ids)) {
      unexpected_blanks <- data_sheet[reporting_template_row_group %in% empty_ids & 
                                        (!is.na(reporting_submitted_data_value) | 
                                           nchar(superTrim(reporting_submitted_data_value)) > 0)]
      
      unexpected_blanks <- unexpected_blanks[,.(check_message=paste0(reporting_template_row_group," not uploaded because ID values eixst, unexpectedly reports data elsewhere: ",
                                                                     paste0(
                                                                       paste0(" '",indicator_name,"' = '",reporting_submitted_data_value,"' "),
                                                                       collapse=" & "))),
                                             by=.(reporting_template_row_group)]
      
      unexpected_blanks <- unexpected_blanks[,
                                             .(rsf_pfcbl_id=rsf_pfcbl_id.facility,
                                               indicator_id=as.numeric(NA),
                                               reporting_asof_date=reporting_asof_date,
                                               check_name="sys_reporting_data_discarded",
                                               check_message)]
      
      reporting_flags <- rbindlist(list(reporting_flags,
                                        unexpected_blanks))
      
      data_sheet <- data_sheet[!(reporting_template_row_group %in% empty_ids)]
      
    }
    #Where users have done a formula fill-down that just generically creates data without any real intention of reporting anything.
    calculation_indicators <- rsf_indicators[,
                                             .(indicator_name,
                                               indicator_id,
                                               calculated=is_calculated==TRUE & is_user_calculatable==FALSE)]
    
    blank_fill_down_rows <- data_sheet[calculation_indicators,
                                       on=.(indicator_name),
                                       nomatch=NULL
                                       ][,
                                         .(blank_fill_down=all(is.na(reporting_submitted_data_value) | 
                                                               !is.na(reporting_submitted_data_formula) |
                                                               calculated==TRUE)),
                                         by=.(reporting_template_row_group)
                                         ][blank_fill_down==TRUE,
                                           reporting_template_row_group]
    
    #sys_flag_unexpected_formula
    if (length(blank_fill_down_rows) > 0) {
     
      unexpected_formulas <- data_sheet[reporting_template_row_group %in% blank_fill_down_rows &
                                        is.na(reporting_submitted_data_formula)==FALSE,
                                        .(reporting_template_row_group,
                                          indicator_name)]
      
      unexpected_formulas <- unexpected_formulas[,
                                                 .(message=paste0(indicator_name,collapse=", ")),
                                                 by=.(reporting_template_row_group)]
      
      unexpected_formulas <- unexpected_formulas[,
                                                .(rsf_pfcbl_id=rsf_pfcbl_id.facility,
                                                  indicator_id=as.numeric(NA),
                                                  reporting_asof_date=reporting_asof_date,
                                                  check_name="sys_flag_unexpected_formula",
                                                  check_message=paste0("System skipped import for rows ",
                                                                          paste0(reporting_template_row_group,collapse=", "),
                                                                          " because all blank reported data and only formula-calculated data for columns: ",
                                                                          message,". Are these data unexpected fill-down rows? ",
                                                                          "Delete these rows to resolve this error (or submit actual data, not just formulas)")),
                                                by=.(message)]
      
      unexpected_formulas <- unexpected_formulas[,
                                                 .(rsf_pfcbl_id,
                                                   indicator_id,
                                                   reporting_asof_date,
                                                   check_name,
                                                   check_message)]
   
      unexpected_constants <- data_sheet[reporting_template_row_group %in% blank_fill_down_rows &
                                         indicator_name %in% calculation_indicators[calculated==TRUE,indicator_name] &
                                         is.na(reporting_submitted_data_formula)==TRUE &
                                         is.na(reporting_submitted_data_value)==FALSE,
                                        .(reporting_template_row_group,
                                          indicator_name)]
      
      unexpected_constants <- unexpected_constants[,
                                                 .(message=paste0(indicator_name,collapse=", ")),
                                                 by=.(reporting_template_row_group)]
      
      unexpected_constants <- unexpected_constants[,
                                                 .(rsf_pfcbl_id=rsf_pfcbl_id.facility,
                                                   indicator_id=as.numeric(NA),
                                                   reporting_asof_date=reporting_asof_date,
                                                   check_name="sys_flag_unexpected_constant",
                                                   check_message=paste0("System skipped import for rows ",
                                                                        paste0(reporting_template_row_group,collapse=", "),
                                                                        " because all blank reported data and only unexpected constant data for columns: ",
                                                                        message,". Are these data unexpected fill-down rows? ",
                                                                        "Delete these rows to resolve this error (or submit actual data, not just formulas)")),
                                                 by=.(message)]
      
      unexpected_constants <- unexpected_constants[,
                                                   .(rsf_pfcbl_id,
                                                     indicator_id,
                                                     reporting_asof_date,
                                                     check_name,
                                                     check_message)]
      
      reporting_flags <- rbindlist(list(reporting_flags,
                                        unexpected_formulas,
                                        unexpected_constants))
      
      data_sheet <- data_sheet[!(reporting_template_row_group %in% blank_fill_down_rows)]
    }
    
    
    
    ######
  
    
    data_sheet <- data_sheet[,
                             .(reporting_template_row_group,
                               reporting_template_data_rank=original_col_num, #will be same as original_col_num as it's already sorted there.
                               indicator_name,
                               reporting_submitted_data_value,
                               reporting_submitted_data_unit,
                               reporting_submitted_data_formula)]
    
    data_sheet
  }
  
  template_data <- rbindlist(list(list_sheet,
                                  data.summary,
                                  data.quarterly))
  
  template_data[,reporting_asof_date:=reporting_asof_date]

  
  
  
  if (!is.null(return.insert_flags)) {

    excelwb <- clean_up_template(excelwb=excelwb)
    
    {
      
      excelwb$add_dxfs_style(name="checkStatusReview",
                             font_color = wb_color(hex = "#006100"),
                             bg_fill = wb_color(hex = "#C6EFCE"))
      
      excelwb$add_dxfs_style(name="checkStatusError",
                             font_color = wb_color(hex = "#FFC7CE"),
                             bg_fill = wb_color(hex = "#9C0006"))
      
      excelwb$add_dxfs_style(name="checkStatusClosed",
                             font_color = wb_color(hex = "#333333"),
                             bg_fill = wb_color(hex = "#E1E1E1"))
      
      excelwb$add_dxfs_style(name="checkClassCritical",
                             font_color = wb_color(hex = "#F4CCCC"),
                             bg_fill = wb_color(hex = "#800000"))
      
      excelwb$add_dxfs_style(name="checkClassError",
                             font_color = wb_color(hex = "#FCE5CD"),
                             bg_fill = wb_color(hex = "#CC0000"))
      
      excelwb$add_dxfs_style(name="checkClassWarning",
                             font_color = wb_color(hex = "#FFF2CC"),
                             bg_fill = wb_color(hex = "#ED7D31"))
      
      excelwb$add_dxfs_style(name="checkClassInfo",
                             font_color = wb_color(hex = "#D9EAD3"),
                             bg_fill = wb_color(hex = "#1155CC"))

    }
    
    if (nrow(return.insert_flags) > 0 && 
        !all(return.insert_flags$check_asof_date==reporting_asof_date)) {
      stop(paste0("Cannot insert flags for check_asof_date eqaul to ",
                  paste0(unique(return.insert_flags$check_asof_date),collapse=" and "),
                  " when template data is for ",reporting_asof_date))
    }
    
    if (!all(
        (expected<-c("evaluation_id",
        "entity_name",
        "rsf_pfcbl_id",
        "pfcbl_category_rank",
        "check_rank",
        "indicator_id",
        "indicator_name",
        "formula_title",
        "check_formula_id",
        "indicator_check_id",
        "check_asof_date",
        "check_name",
        "check_type",
        "check_class",
        "check_formula_title")) %in% names(return.insert_flags))) {
          stop("Flags submitted with return.insert_flags should submit a data.table with these columns defined: ",
               paste0(expected,collapse=','),
               " and is missing: ",
               paste0(setdiff(expected,names(return.insert_flags)),collapse=","))
    }
    
    add_flag <- function(wb,
                         sheet,
                         flag,
                         styleflag=FALSE,
                         pid=wb$get_person(name="IFC Risk Sharing")$id) {

      if (is.character(sheet)) stop("Numeric sheet number is required for add_flag")
      
      check_class <- toupper(flag$check_class)
      
      #write comments?
      
        # pid <- wb$get_person(name=check_class)$id
        # if (!length(pid)) {
        #   wb$add_person(name=check_class)
        #   pid <- wb$get_person(name=check_class)$id
        # }
        # 
        
        has_comment <- length(wb$get_comment(sheet=sheet,dims=flag$ref)) > 0
        has_thread <- tryCatch({ NROW(wb$get_thread(sheet=sheet,dims=flag$ref)) },error=function(e) { 0 }) > 0
  
        if (has_comment) {
          wb$remove_comment(sheet=sheet,dims=flag$ref)
          has_comment <- FALSE
        }
        
        # if (has_comment && !has_thread) {
        #   wb$remove_comment(sheet=sheet,dims=flag$ref)
        #   has_comment <- FALSE
        # }
  
#        if (has_thread) {
        # 
        #   wb$add_thread(sheet=sheet,
        #                 dims=flag$ref,
        #                 person_id=pid,
        #                 comment=NULL)
        #   has_thread <- FALSE
        # }
  
        # if (has_thread==FALSE && flag$ref_n > 1) {
        #   has_thread <- TRUE
        #   wb$add_thread(sheet=sheet,
        #                 dims=flag$ref,
        #                 person_id=wb$get_person(name="IFC Risk Sharing")$id,
        #                 reply=FALSE,
        #                 resolve=FALSE,
        #                 comment="Multiple flags are assigned to this cell")
        # 
        # }
  
        #comment <- paste0(flag$check_message,"\n[",gsub("_"," ",flag$check_name),"]\n[ID:",flag$evaluation_id,"]")
  
        comment <- flag$check_message
        
        #print(paste0(flag$ref," ",comment))
        # #ensure XML control characters are escaped#fixed in github
        comment <- gsub("<","&lt;",comment)
        comment <- gsub(">","&gt;",comment)
        commnet <- gsub("'","&apos;",comment)
        comment <- gsub("&","&amp;",comment)
        comment <- gsub('"',"&quot;",comment)
        comment <- gsub("[[:cntrl:]]+"," ",comment)
        
        #comment <- "Test comment"
        wb$add_thread(sheet=sheet,
                      dims=flag$ref,
                      person_id=pid,
                      reply=has_thread,
                      resolve=ifelse(flag$check_status=="active",FALSE,TRUE),
                      comment=comment)
  
    
    
      if (styleflag==TRUE) {
        
        # wb$add_fill(sheet=sheet,
        #             dims=flag$ref,
        #             color=wb_color(name=flag$check_color))
        # 
        # wb$add_font(sheet=sheet,
        #             dims=flag$ref,
        #             color=wb_color(name=flag$check_font))
        
      }
    }
    
    # #Remove custom XML
    # if (length(excelwb$customXml) ||
    #     any(grepl("customXml",excelwb$Content_Types)) || 
    #     any(grepl("customXml",excelwb$workbook.xml.rels))) {
    #   
    #   message("Excel file has customXml that will be removed")
    #   excelwb$Content_Types <- grep("customXml",excelwb$Content_Types,value=T,invert = T)
    #   excelwb$workbook.xml.rels <- grep("customXml",excelwb$workbook.xml.rels,value=T,invert = T)
    #   excelwb$customXml <- NULL
    # }
    # 
    # 
    # #Remove Hidden Named Ranges
    # if (any(nchar( (nr<-excelwb$get_named_regions())$hidden) )) {
    #   
    #   for (i in 1:nrow(nr)) {
    #     rng <- nr[i,]
    #     hnr <- suppressWarnings(as.numeric(rng$hidden))
    #     if (length(hnr) > 0 && !is.na(hnr) && hnr==1) {
    #       
    #       if (any(rng$sheets==excelwb$sheet_names)) {
    #         message(paste0("Excel file has hidden named ranges that will be removed: ",rng$name))  
    #         excelwb$remove_named_region(sheet=rng$sheets,
    #                                     name=rng$name)
    #       }
    #     }
    #   }
    # }
    # 
    # #Remove existing comments (Legacy comments create issues)
    # if (length(excelwb$comments)) {
    #   
    #   comments <- rbindlist(lapply(seq_along(excelwb$sheet_names),function(s) { x <- excelwb$get_comment(sheet=s); if (length(x)) cbind(x[,c("ref","cmmt_id")],sheet=s) }))
    #   #threads <- rbindlist(lapply(seq_along(excelwb$sheet_names),function(s) { x <- excelwb$get_thread(sheet=s); if (length(x)) cbind(x,sheet=s) }))
    #   
    #   
    #   
    #   for (nc in 1:nrow(comments)) excelwb$remove_comment(sheet=comments[nc]$sheet,dims=comments[nc]$ref)
    #   #for (nc in 1:nrow(threads)) excelwb$remove_comment(sheet=threads[nc]$sheet,dims=threads[nc]$ref)
    #   #if (!empty(comments)) { invisible(pmap(comments[,.(sheet,dims=ref)],excelwb$remove_comment)) }
    #   
    #   for (i in seq_along(excelwb$vml_rels)) {
    #     if (nchar(excelwb$vml_rels[i])==0) excelwb$vml_rels[[i]] <- NULL
    #   }
    # }
    # if (length(excelwb$threadComments)) {
    #   for (i in seq_along(excelwb$threadComments)) excelwb$threadComments[[i]] <- ""
    # }
     
    
    #openxlsx2::wb_save(excelwb,file="C:/Temp/test1.xlsx",overwrite=T)
    #summarySheet: set above
    #dataSheet: set above
    
    if (empty(excelwb$get_person(name="IFC Risk Sharing"))) { excelwb$add_person(name="IFC Risk Sharing") }
    
    return.insert_flags[rsf_indicators,
                 pfcbl_category:=data_category,
                 on=.(indicator_id)]
    
    flag_data <- template_data[grepl("Summary$|QReport$",reporting_template_row_group,ignore.case = T)]
    flag_data[,sheet_name:=fcase(grepl("Summary$",reporting_template_row_group,ignore.case=T),"summary",
                                 grepl("QReport$",reporting_template_row_group,ignore.case=T),"qreport",
                                 default=NA)]
    
    flag_data[rsf_indicators,
              indicator_id:=i.indicator_id,
              on=.(indicator_name)]
    
    flag_data[,omit:=FALSE]
    flag_data[sheet_name=="summary",
              omit:=(.N:1)>1,
              by=.(indicator_id)]
    
    flag_data <- flag_data[omit==F]
    flag_data[,omit:=NULL]
    
    return.insert_flags[,
                        check_rank:=fcase(check_class=="critical",4,
                               check_class=="error",3,
                               check_class=="warning",2,
                               check_class=="info",1,
                               default=5)]
    
    return.insert_flags[,
                        `:=`(check_color=fcase(check_status=="resolved","#E1E1E1",
                                               check_class=="critical","#800000",
                                               check_class=="error","#CC0000",
                                               check_class=="warning","#ED7D31",
                                               check_class=="info","#1155CC",
                                               default="gray"),
                             
                             check_font=fcase(check_status=="resolved","#333333",
                                              check_class=="critical","#F4CCCC",
                                              check_class=="error","#FCE5CD",
                                              check_class=="warning","#FFF2CC",
                                              check_class=="info","#D9EAD3",
                                              default="gray"))]
 
    # return.insert_flags[,
    #         check_color:=fcase(check_status=="resolved","lightgray",
    #                            check_class=="critical","firebrick",
    #                            check_class=="error","red",
    #                            check_class=="warning","orange",
    #                            check_class=="info","blue",
    #                            default="gray")]
    
    #because there's only one indicator per error
    summary <- flag_data[sheet_name=="summary"
                         ][return.insert_flags[pfcbl_category %in% c("facility","client")],
                           on=.(indicator_id),
                           nomatch = NA]
    
    if (!empty(summary)) {
      
      summary[is.na(reporting_template_data_rank) & pfcbl_category=="facility",
              reporting_template_data_rank:=POSITION_current_terms]
      
      summary[is.na(reporting_template_data_rank) & pfcbl_category=="client",
              reporting_template_data_rank:=POSITION_current_qdd]
      
      #All data is on column E for this template
      summary[,ref:=paste0("E",reporting_template_data_rank)]
      summary[,ref_n:=.N,by=.(ref)]
      summary <- summary[,
                         .(ref,
                           ref_n,
                           evaluation_id,
                           check_name,
                           check_class,
                           check_type,
                           check_formula_title,
                           check_status,
                           check_status_comment,
                           check_message,
                           check_rank,
                           check_color,
                           check_font)]
      # summary[,check_rank:=fcase(check_class=="critical",4,
      #                            check_class=="error",3,
      #                            check_class=="warning",2,
      #                            check_class=="info",1,
      #                            default=5)]
      
      setorder(summary,-check_rank)
      sheet_num <- grep("Summary$",excelwb$sheet_names,ignore.case=T)
      sflags <- summary[order(check_rank,decreasing = T),
                        .(check_rank=check_rank[1],
                          check_class=check_class[1],
                          check_color=check_color[1],
                          check_font=check_font[1],
                          check_message=paste0(paste0(toupper(check_class),": ",check_message),collapse=" \n\n")),
                          by=.(ref)]
      
      for (f in 1:nrow(sflags)) {
        
        add_flag(wb=excelwb,
                 sheet=sheet_num,
                 flag=sflags[f],
                 styleflag = F)
      }
    } else { summary[,ref:=NA] }
    
    #excelwb2 <- excelwb
    #excelwb <- excelwb2
    #openxlsx2::wb_save(excelwb,file="C:/Temp/OTP Test2.xlsx",overwrite=T)
    
    #wb_save(excelwb,file="c:/temp/test2.xlsx",overwrite = T)
    
    {
      rsf_pfcbl_ids <- dbGetQuery(pool,"
        select
          ft.from_rsf_pfcbl_id as rsf_pfcbl_id,
          max(cni.rank_id) as inclusion_rank
        
        from p_rsf.view_rsf_pfcbl_id_family_tree ft
        inner join p_rsf.rsf_data_current_names_and_ids cni on cni.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
        where cni.reporting_asof_date <= $2::date
        and ft.pfcbl_hierarchy <> 'parent'
        and ft.to_pfcbl_category = 'loan'
        
        and ft.from_rsf_pfcbl_id = any($1::int[])
        group by ft.from_rsf_pfcbl_id",
        params=list(dbMakeIntArray(return.insert_flags[pfcbl_category %in% c("borrower","loan"),unique(rsf_pfcbl_id)]),
                    as.character(reporting_asof_date)))
      
      setDT(rsf_pfcbl_ids)
      
      flag_ranks <- flag_data[sheet_name=="qreport" & indicator_id == rsf_indicators[data_category=="loan" & indicator_sys_category=="rank_id",indicator_id],
                              .(reporting_template_row_group,inclusion_rank=reporting_submitted_data_value)]
      
      flag_ranks <- flag_ranks[rsf_pfcbl_ids,
                               on=.(inclusion_rank),
                               nomatch=NULL]
  
      if (!empty(flag_ranks[,.(n=.N),by=.(rsf_pfcbl_id)][n>1])) {
        stop("Multiple rsf_pfcbl_ids per reporting_template_row_group")
      }
    }
    
    qflags <- return.insert_flags[flag_ranks,
                                   on=.(rsf_pfcbl_id),
                                   nomatch=NULL]
    
    qflags[!indicator_id %in% unique(flag_data$indicator_id),
           indicator_id:=NA]
    
    
    qflags[is.na(indicator_id) & pfcbl_category=="loan",
                        indicator_id:=rsf_indicators[data_category=="loan" & indicator_sys_category=="rank_id",indicator_id]]
    
    qflags[is.na(indicator_id) & pfcbl_category=="borrower",
                        indicator_id:=rsf_indicators[data_category=="borrower" & indicator_sys_category=="id",indicator_id]]
    
    qreport <- flag_data[sheet_name=="qreport" &
                         reporting_template_row_group %in% unique(qflags$reporting_template_row_group)
                         ][qflags[pfcbl_category %in% c("loan","borrower")],
                           on=.(reporting_template_row_group,
                                reporting_asof_date=check_asof_date,
                                indicator_id),
                           nomatch=NA]
    
    sheet_num <- grep("QReport$",excelwb$sheet_names,ignore.case=T)
    qreport_data <- wb_to_df(excelwb,sheet=sheet_num,col_names = F)
    # 
    # if (any( (qreport_data_blank_rows <- rowSums(!is.na(qreport_data))==0)[-(1:QREPORT_startrow)],na.rm=T)) {
    #   last_not_blank <- Position(isFALSE,qreport_data_blank_rows,right=TRUE)
    #   
    #   #allow 5 blank rows
    #   if (last_not_blank+5 < nrow(qreport_data) && last_not_blank >QREPORT_startrow) {
    #     
    #     excelwb <- wb_clean_sheet(excelwb, 
    #                               sheet = sheet_num, 
    #                               dims = paste0("A",(last_not_blank+5),":",openxlsx2::int2col(ncol(qreport_data)),nrow(qreport_data)))
    #     
    #     qreport_data <- qreport_data[1:(last_not_blank+5),]                
    #   }
    # }
    # 
    # #remove conditional styles and reset styles on Qreport
    # {
    # 
    #   #Remove conditional formatting applied to arbitrary cells    
    #   if (length(removecf <- grep("\\s+",trimws(excelwb$worksheets[[sheet_num]]$conditionalFormatting$sqref)))) {
    #     excelwb$worksheets[[sheet_num]]$conditionalFormatting <- excelwb$worksheets[[sheet_num]]$conditionalFormatting[-removecf,]
    #   }
    #   
    #   #any conditional formual on a row with double-digit row number (ie, more than row 6 start row; simpler than parsing each digit value)
    #   if (length(removecf <- grep("\\d{2,}",excelwb$worksheets[[sheet_num]]$conditionalFormatting$sqref))) {
    #     excelwb$worksheets[[sheet_num]]$conditionalFormatting <- excelwb$worksheets[[sheet_num]]$conditionalFormatting[-removecf,]
    #   }
    #   
    #   #openxlsx2::wb_save(excelwb,file="C:/Temp/OTP Test2.xlsx",overwrite=T)
    #   excelwb <- wb_add_fill(wb=excelwb,
    #                          sheet=sheet_num,
    #                          dims=paste0("A",QREPORT_startrow,":",openxlsx2::int2col(ncol(qreport_data)),nrow(qreport_data)),
    #                          color=NULL)
    #   
    #   excelwb <- wb_add_font(wb=excelwb,
    #                          sheet=sheet_num,
    #                          dims=paste0("A",QREPORT_startrow,":",openxlsx2::int2col(ncol(qreport_data)),nrow(qreport_data)),
    #                          update=NULL) 
    #   
    #   excelwb <- wb_add_border(wb=excelwb,
    #                            sheet=sheet_num,
    #                            dims=paste0("A",QREPORT_startrow,":",openxlsx2::int2col(ncol(qreport_data)),nrow(qreport_data)),
    #                            update=NULL)
    #   
    #   excelwb <- wb_add_data(wb=excelwb,
    #                          sheet=sheet_num,
    #                          x="🏴 ",
    #                          dims="A5")
    #   
    #   excelwb <- wb_add_cell_style(wb=excelwb,
    #                                sheet=sheet_num,
    #                                dims="A5",
    #                                horizontal = "center")
    # 
    #   #Color of the flag
    #   excelwb <- wb_add_font(wb=excelwb,
    #                          sheet=sheet_num,
    #                          dims="A5",
    #                          color=wb_color(name=fcase(any(qreport$check_class=="critical",na.rm=T),"#800000",
    #                                                    any(qreport$check_class=="error",na.rm=T),"#CC0000",
    #                                                    any(qreport$check_class=="warning",na.rm=T),"#ED7D31",
    #                                                    any(qreport$check_class=="info",na.rm=T),"#1155CC",
    #                                                    default="gray")))
    # }
    
    #wb_save(excelwb,file="c:/temp/test2.xlsx",overwrite = T)
    
    #openxlsx2::wb_save(excelwb,file="C:/Temp/OTP Test2.xlsx",overwrite=T)
    #Reset flags to blanks
    # excelwb <- wb_add_data(wb=excelwb,
    #                        sheet=sheet_num,
    #                        x=rep("",times=(nrow(qreport_data)+1-QREPORT_startrow)),
    #                        dims=paste0("A",QREPORT_startrow,":A",nrow(qreport_data)))
    # 
    excelwb <- wb_add_cell_style(wb=excelwb,
                                 sheet=sheet_num,
                                 dims=paste0("A",QREPORT_startrow,":A",nrow(qreport_data)),
                                 horizontal = "center")

    excelwb <- wb_add_filter(wb=excelwb,
                             sheet=sheet_num,
                             rows=(QREPORT_startrow-1),
                             cols=1:ncol(qreport_data))
    
    #openxlsx2::wb_save(excelwb,file="C:/Temp/NBS Test2.xlsx",overwrite=T)
    #wb_save(excelwb,file="c:/temp/test3.xlsx",overwrite = T)
    
    if (!empty(qreport)) {
      
      qreport[is.na(reporting_template_data_rank),
              reporting_template_data_rank:="B"] #B is inclusion rank ID column

      #minus 1 because start_row should be first row where data exists; and that should also be reporting_template_row_group line 1
      qreport[,ref_row:=as.numeric(gsub("[[:alpha:]]+","",reporting_template_row_group))+QREPORT_startrow-1]
      if (anyNA(qreport$ref_row)) {
        stop(paste0("Failed to insert flags due to missing reference row for reporting_template_row_group: ",
             paste0(qreport[is.na(ref_row),unique(reporting_template_row_group)],collapse=", ")))
      }

     
      #All data is on column E for this template
      qreport[,ref:=paste0(reporting_template_data_rank,ref_row)]
      qreport[,ref_n:=.N,by=.(ref)]
      qreport <- qreport[,.(ref,ref_n,evaluation_id,check_name,check_class,check_type,check_formula_title,check_status,check_status_comment,check_message,
                            check_rank,
                            check_font,
                            check_color)]
      
      
      
      
      setorder(qreport,check_rank)
      
      qsummary <- qreport[check_status=="active",
                          .(ref_row=paste0("A",gsub("[A-Z]","",ref)),
                            check_rank)][,.(row_flag_count=.N,
                                            row_flag_rank=sort(check_rank,decreasing=T)[1]),
                                         by=.(ref_row)]
      setorder(qsummary,ref_row)
      
      qsummary[,
               check_color:=fcase(row_flag_rank==4,"#800000",
                                  row_flag_rank==3,"#CC0000",
                                  row_flag_rank==2,"#ED7D31",
                                  row_flag_rank==1,"#1155CC",
                                  default="gray")]
      
      
      if (!empty(qsummary)) {
        for (f in 1:nrow(qsummary)) {
          excelwb$add_data(sheet=sheet_num,
                           x=qsummary[f,row_flag_count],
                           dims=qsummary[f,ref_row])
          excelwb$add_font(sheet=sheet_num,
                           dims=qsummary[f,ref_row],
                           bold=TRUE,
                           color=wb_color(name=qsummary[f,check_color]))
        }
      }
      
      #wb_save(excelwb,file="c:/temp/test4.xlsx",overwrite = T)
      #ACK 1
      if (!empty(qreport)) {
        
        qflags <- qreport[order(check_rank,decreasing = T),
                          .(check_rank=check_rank[1],
                            check_class=check_class[1],
                            check_color=check_color[1],
                            check_font=check_font[1],
                            check_message=paste0(paste0(toupper(check_class),": ",check_message),collapse=" \n\n")),
                          by=.(ref)]
        
        for (f in 1:nrow(qflags)) {

          add_flag(wb=excelwb,
                   sheet=sheet_num,
                   flag=qflags[f],
                   styleflag=TRUE)
        }
      }
    } else { qreport[,ref:=NA] }
    
    #ok for save flags.
    #wb_save(excelwb,file="c:/temp/test.xlsx",overwrite = T)
    #wb_save(excelwb,file="c:/temp/test5.xlsx",overwrite = T)
    
    refs <- rbindlist(list(summary[,.(ref,evaluation_id,sheet=grep("Summary$",excelwb$sheet_names,value=T,ignore.case=T))],
                           qreport[,.(ref,evaluation_id,sheet=grep("QReport$",excelwb$sheet_names,value=T,ignore.case=T))]))
    
    refs[,ref_n:=.N,by=.(ref,sheet)]
    
    #refs[,sheet:=paste0("'",sheet,"'")]
    refs_flags <- refs[return.insert_flags,
                       on=.(evaluation_id),
                       nomatch=NULL]
    
    refs_flags[,dims:=paste0("B",5+(1:.N))]
    
    #excelwb2 <- excelwb
    #wb_save(excelwb,file="c:/temp/test2.xlsx",overwrite = T)
    #ok
    sheetSUMMARY <- grep("Summary",excelwb$sheet_names,value=T,ignore.case=T)
    if (length(sheetSUMMARY)) {
      sumdf <- excelwb$to_df(sheet=sheetSUMMARY,col_names=F,detect_dates = F)
      rsf_report <- NULL
      if (any(names(sumdf)=="B")) { rsf_report <- grep("RSF Quarterly report",sumdf$B,ignore.case = T) }
      if (length(rsf_report) && rsf_report[1] < 10) {
        rsf_report <- rsf_report[1]
        tojason <- excelwb$to_df(sheet=sheetSUMMARY,col_names=F,detect_dates = F,cols="B",rows=rsf_report,show_formula=T)
        if (!grepl("hyperlink",tojason,ignore.case = T)) {
          excelwb$add_formula(sheet=sheetSUMMARY,
                              dims=paste0("B",rsf_report),
                              x=paste0('HYPERLINK("https://datanalytics-int.worldbank.org/rsf-prod/?',rsf_pfcbl_id.facility,'","RSF Quarterly Report")'))
        }
      }
    }
    
    if (any(excelwb$sheet_names==sheetCURRENTFLAGS)) {
      
      excelwb$clean_sheet(sheetCURRENTFLAGS)
      
      existing_tables <- excelwb$get_tables(sheet=sheetCURRENTFLAGS)
      if (length(existing_tables)) {
        existing_tables <- existing_tables$tab_name
        if (length(existing_tables)) {
          for (tn in existing_tables) {
            message(paste0(tn," already exists: removing from Current Flags"))
            excelwb$remove_tables(sheet=sheetCURRENTFLAGS,table=tn)
          }
        }
      }
      
      excelwb$worksheets[[which(excelwb$sheet_names==sheetCURRENTFLAGS)]][["sheetPr"]] <- xml_node_create(
        "sheetPr", 
        xml_children = xml_node_create(
          "tabColor", 
          xml_attributes = c(rgb = "FFFF0000")
        )
      )
      
      #somehow this seems to cause problems, possible it removes the wrong index or doens't upadte index correctly and results in user-defined formulas crapping out
      #excelwb$remove_worksheet(sheet=sheetCURRENTFLAGS)
    
    } else {
      
      excelwb$add_worksheet(sheet=sheetCURRENTFLAGS,
                            zoom=80,
                            tab_color=wb_color("red"))
    }
    
    #wb_save(excelwb,file="c:/temp/test6.xlsx",overwrite = T)
    #Unhide hidden sheets and collapsed rows
    # snames <- wb_get_sheet_names(excelwb)
    # for(sname in snames) {
    #   size <- dim(wb_to_df(excelwb,sheet=sname))
    #   excelwb <- wb_set_sheet_visibility(excelwb,sheet=sname,value="visible")
    # 
    #   if (length(size)) {
    #     excelwb$remove_row_heights(sheet=sname,
    #                                rows=1:size[1])
    #   }
    #   # excelwb$set_row_heights(sheet=sname,
    #   #                         rows=1:size[1],
    #   #                         heights=150,
    #   #                         hidden=FALSE,
    #   #                         hide_blanks=FALSE)
    #   # 
    #   
    #   ratt <- excelwb[["worksheets"]][[which(sname==snames)]][["sheet_data"]][["row_attr"]]
    #   if (any(nchar(ratt$hidden)>0,na.rm=T)) {
    #     excelwb[["worksheets"]][[which(sname==snames)]][["sheet_data"]][["row_attr"]][which(nchar(ratt$hidden)>0),"hidden"] <- ""
    #   }
    #   
    #   if (any(nchar(ratt$ht)>0,na.rm=T)) {
    #     excelwb[["worksheets"]][[which(sname==snames)]][["sheet_data"]][["row_attr"]][which(nchar(ratt$ht)>0),"ht"] <- ""
    #   }
    #   
    #   excelwb$remove_hyperlink(sheet=sname)
    # }

    #wb_save(excelwb,file="c:/temp/test7.xlsx",overwrite = T)
    
    #wb_save(excelwb,file="c:/temp/test4.xlsx",overwrite = T)
    #ok
    if (empty(refs_flags)) {
      
     
      excelwb$add_data_table(sheet=sheetCURRENTFLAGS,
                             table_name="rsf_current_flags",
                             col_names=TRUE,
                             dims="B5",
                             x=refs_flags[,
                                          .(FLAGID=NA,           #B
                                            DATE=reporting_asof_date,
                                            NAME="[NONE]",
                                            type=NA,
                                            class=NA,
                                            
                                            CHECK="[NONE]",
                                            STATUS=NA,
                                            MESSAGE="There are no checks for this dataset",
                                            `IFC Comments`="",
                                            `CLIENT Comments`="")])
    
    
    
    } else {
    
      excelwb$add_data_table(sheet=sheetCURRENTFLAGS,
                             table_name="rsf_current_flags",
                             col_names=TRUE,
                             dims="B5",
                             x=refs_flags[,
                                         .(FLAGID=evaluation_id,           #B
                                           DATE=check_asof_date,           #C
                                           NAME=entity_name,               #D
                                           type=check_type,                #E
                                           class=check_class,              #F 
                                           CHECK=paste0(indicator_name,": ",ifelse(is.na(check_formula_title),check_name, #system checks only have a check_name
                                                                                   check_formula_title)),
                                           STATUS=fcase(check_status=="active","Review",
                                                        check_status=="resolved","Closed",
                                                        TRUE,check_status),
                                           MESSAGE=check_message,          #G  
                                           `IFC Comments`=ifelse(is.na(check_status_comment),"",check_status_comment),
                                           `CLIENT Comments`=ifelse(is.na(check_reporting_comment),"",check_reporting_comment))])
      
      refs_flags[,target_cf:=paste0("'Current Flags'!K",5+(1:.N))] #offset to row 5
      
      pwalk(refs_flags[,.(ref,dims,evaluation_id,sheet,target_cf,check_color,check_font)],
            function(ref,dims,evaluation_id,sheet,target_cf,check_color,check_font) { 
        
        excelwb$add_formula(sheet=sheetCURRENTFLAGS,
                            dims=dims,
                            x=paste0('HYPERLINK("#',paste0("'",sheet,"'"),'!',ref,'", "Go to #',evaluation_id,'")'))

        excelwb$add_hyperlink(sheet=sheet,
                              dims=ref,
                              target=target_cf,
                              tooltip="Click enter comments for this flag",
                              is_external=F,
                              col_names=F)
        
        if (grepl("Qreport",sheet,ignore.case = T)) {
          excelwb$add_fill(sheet=sheet,
                      dims=ref,
                      color=wb_color(name=check_color))
          
          excelwb$add_font(sheet=sheet,
                      dims=ref,
                      color=wb_color(name=check_font))
        }
      })
      
      #FORMATTING and CONDITIONAL FORMATTING
      {
        excelwb$add_cell_style(sheet=sheetCURRENTFLAGS,
                               dims=paste0("G6:G",length(refs_flags$dims)+5),
                               wrap_text = TRUE) 
        
        excelwb$add_cell_style(sheet=sheetCURRENTFLAGS,
                               dims=paste0("H6:H",length(refs_flags$dims)+5),
                               wrap_text = TRUE) 
  
        excelwb$add_cell_style(sheet=sheetCURRENTFLAGS,
                               dims=paste0("I6:I",length(refs_flags$dims)+5),
                               wrap_text = TRUE) 
        
        excelwb$add_cell_style(sheet=sheetCURRENTFLAGS,
                               dims=paste0("J6:J",length(refs_flags$dims)+5),
                               wrap_text = TRUE) 
  
        
        excelwb$add_font(sheet=sheetCURRENTFLAGS,
                         dims=paste0("B6:B",length(refs_flags$dims)+5),
                         size=9,
                         color = wb_color(hex = "#0000FF"), 
                         underline = "single")
        
        excelwb$add_data_validation(sheet=sheetCURRENTFLAGS,
                         dims=paste0("H6:H",length(refs_flags$dims)+5),
                         type="list",
                         value='"Review,Closed,Past Reporting Error"',
                         allow_blank=FALSE)
        
        excelwb$add_conditional_formatting(sheet=sheetCURRENTFLAGS,
                                           dims=paste0("H6:H",length(refs_flags$dims)+5),
                                           type = "containsText",
                                           rule="Review",
                                           #formula = '',
                                           style = "checkStatusReview")
        
        excelwb$add_conditional_formatting(sheet=sheetCURRENTFLAGS,
                                           dims=paste0("H6:H",length(refs_flags$dims)+5),
                                           type = "containsText",
                                           rule = "Closed",
                                           style = "checkStatusClosed")
        
        excelwb$add_conditional_formatting(sheet=sheetCURRENTFLAGS,
                                           dims=paste0("H6:H",length(refs_flags$dims)+5),
                                           type = "containsText",
                                           rule = "Past Reporting Error",
                                           style = "checkStatusError")
        
        
        
        excelwb$add_conditional_formatting(sheet=sheetCURRENTFLAGS,
                                           dims=paste0("F6:F",length(refs_flags$dims)+5),
                                           type = "containsText",
                                           rule = "critical",
                                           style = "checkClassCritical")
  
        excelwb$add_conditional_formatting(sheet=sheetCURRENTFLAGS,
                                           dims=paste0("F6:F",length(refs_flags$dims)+5),
                                           type = "containsText",
                                           rule = "error",
                                           style = "checkClassError")
  
        excelwb$add_conditional_formatting(sheet=sheetCURRENTFLAGS,
                                           dims=paste0("F6:F",length(refs_flags$dims)+5),
                                           type = "containsText",
                                           rule = "warning",
                                           style = "checkClassWarning")
  
        excelwb$add_conditional_formatting(sheet=sheetCURRENTFLAGS,
                                           dims=paste0("F6:F",length(refs_flags$dims)+5),
                                           type = "containsText",
                                           rule = "info",
                                           style = "checkClassInfo")
      
      }
    }
    
    #excelwb2 <- excelwb
    #wb_save(excelwb,file="c:/temp/cs1.xlsx",overwrite = T)
    excelwb$add_data(sheet=sheetCURRENTFLAGS,
                     x=paste0("RSF FLAG REPORT Generated on: ",today()),
                     dims="A1")
    
    excelwb$add_formula(sheet=sheetCURRENTFLAGS,
                        dims="A1",
                        x=paste0('HYPERLINK("https://datanalytics-int.worldbank.org/rsf-prod/?',rsf_pfcbl_id.facility,'","RSF FLAG REPORT Generated on: ',today(),'")'))
    
    excelwb$set_col_widths(sheet=sheetCURRENTFLAGS,
                           cols=1+c(1,2,3,4,5,6,7,8,9,10), #Col B is +1 offset
                           widths = c(12, #Flag ID/Link
                                      9, #check date 
                                      35, #entity name
                                      17, #check type
                                      10, #check class
                                      45, #check 
                                      9,  #status
                                      70, #Message
                                      70, #Comment
                                      70))#Response
    
    excelwb$set_active_sheet(sheetCURRENTFLAGS)
    excelwb$set_selected(sheet=sheetCURRENTFLAGS)
    excelwb$set_sheetview(sheet=sheetCURRENTFLAGS,
                         top_left_cell = "A1")
    
    excelwb$freeze_pane(sheet=sheetCURRENTFLAGS,
                        first_active_row=6,
                        first_active_col="C")
    
    #wb_save(excelwb,file="c:/temp/test4.xlsx",overwrite = T)
    #browser()
    return (excelwb)
  }
  
  
  if (isTRUE(return.next_date)) {
    
    excelwb <- clean_up_template(excelwb=excelwb)
    if (any(excelwb$sheet_names==sheetCURRENTFLAGS)) {
      
      excelwb$clean_sheet(sheetCURRENTFLAGS)
      
      existing_tables <- excelwb$get_tables(sheet=sheetCURRENTFLAGS)$tab_name
      if (length(existing_tables)) {
        for (tn in existing_tables) {
          message(paste0(tn," already exists: removing from Current Flags"))
          excelwb$remove_tables(sheet=sheetCURRENTFLAGS,table=tn)
        }
      }
      
      excelwb$worksheets[[which(excelwb$sheet_names==sheetCURRENTFLAGS)]][["sheetPr"]] <- xml_node_create(
        "sheetPr", 
        xml_children = xml_node_create(
          "tabColor", 
          xml_attributes = c(rgb = "FFC0C0C0")
        )
      )
      
    } 
    
    #Reset quarterly data
    {
      quaterly_cols_periodic <- data.quarterly[indicator_name %in% rsf_indicators[is_periodic_or_flow_reporting==T,indicator_name],
                                               unique(reporting_template_data_rank)]
      qdata.ranks <- excelwb$to_df(sheet=dataSheet,
                                   start_row=QREPORT_startrow,
                                   cols="B")
      qdata.lastrow <- as.numeric(row.names(qdata.ranks)[Position(x=qdata.ranks[[1]],function(r) !is.na(r),right=T)])
      
      if (length(qdata.lastrow) && qdata.lastrow >= QREPORT_startrow) {
      
        for (qc in quaterly_cols_periodic) {
          excelwb$add_data(sheet=dataSheet,
                           x=rep(NA,times=qdata.lastrow-QREPORT_startrow+1),
                           dims=paste0(qc,QREPORT_startrow,":",qc,qdata.lastrow),
                           na="")
        }
      }
      
      #Testing...Just to check
      # qdata.periodic <- excelwb$to_df(sheet=dataSheet,
      #                                 start_row = QREPORT_startrow-1,
      #                                 cols=quaterly_cols_periodic)
      # setDT(qdata.periodic)
    }
    
    # ..excelwb2 <- copy(excelwb)
    #excelwb <- excelwb2 
    #Migrate history in Summary Data
    {
      SUMMARY_history_startrow <- nregions_table[grepl("Template_Summary_HistoryStartRow",name,ignore.case=T)]
      summary_history_data <- excelwb$to_df(sheet=summarySheet,
                                            start_row = as.numeric(SUMMARY_history_startrow$data_value)+1,
                                            start_col = "E",
                                            col_names=F,
                                            convert=FALSE)
      #row.names(summary_history_data)
      #dims <- openxlsx::col2int(names(summary_history_data))+1
      for (r in 1:nrow(summary_history_data)) {
        
        current_row <- unlist(summary_history_data[r,],use.names = F)
        allblanks <- Position(x=current_row,f=function(p) !is.na(p),right=T)
        if (any(allblanks,na.rm=T)) current_row <- current_row[1:allblanks]
        if (!length(current_row)) next;
        dimStart <- paste0("F",row.names(summary_history_data[r,]))
        dimEnd <- paste0(int2col(5+length(current_row)),row.names(summary_history_data[r,]))
        dim <- paste0(dimStart,":",dimEnd)
        
        
        if (all(is.na(current_row))) {
          current_row <- ""
        } else if (all(!is.na(suppressWarnings(as.numeric(na.omit(current_row)))),na.rm=T)) {
          current_row <- as.numeric(current_row)
        } else if (all(!is.na(suppressWarnings(lubridate::as_date(na.omit(current_row)))),na.rm=T)) {
          current_row <- as.Date(current_row)
        } else {
          current_row <- as.character(current_row)
        }
        source_style <- excelwb$get_cell_style(sheet = summarySheet, dims = dimStart)
        excelwb$add_data(sheet=summarySheet,
                         x=current_row,
                         dims=dim,
                         col_names=F,
                         row_names=F,
                         na="")
        
        excelwb$set_cell_style(sheet=summarySheet,
                               dims=dim,
                               style=source_style)
      }
        
  
    }
    
    #Zero-out any defined FX rates and the delivery date    
    {
      summary_fx_data <- data.summary[indicator_name %in% rsf_indicators[data_type=="currency_ratio",indicator_name]]
      sdata.fx_cols <- sort(suppressWarnings(as.numeric(summary_fx_data$reporting_template_data_rank)))
      
      if (length(na.omit(sdata.fx_cols))) {
        excelwb$add_data(sheet=summarySheet,
                         x=rep(0,times=length(sdata.fx_cols)),
                         dims=paste0("E",sdata.fx_cols[1],":E",sdata.fx_cols[length(sdata.fx_cols)]),
                         col_names=F,
                         row_names=F,
                         na="")
      }
      # Testing...Just to check
      # 
      # sdata.fx <- excelwb$to_df(sheet=summarySheet,
      #                           rows = as.numeric(summary_fx_data$reporting_template_data_rank),
      #                           cols="E")
      # setDT(qdata.periodic)
    }
    
    #Update the QDD date to the next interval
    {
      #excelwb$to_df(sheet="Template",named_region = "List_QDD")
      nr_list_qdd <- nregions_table[grepl("List_QDD",name,ignore.case=T)]
      nr_data_qdd <- nregions_table[grepl("Data_QDD",name,ignore.case=T)]
      
      if (empty(nr_list_qdd)) stop("Failed to find List_QDD defined name (should be in Templates sheet)")
      if (empty(nr_data_qdd)) stop("Failed to find Data_QDD defined name (should be in Summary sheet, assigned to QDD drop down menu)")
      
      sdata.qdds <- openxlsx2::convert_date(
        unlist(nr_list_qdd$data_value))
      
      sdata.qdd <- openxlsx2::convert_date(unlist(nr_data_qdd$data_value))
      
      qdd_now <- which(sdata.qdds==sdata.qdd)
      if (!length(qdd_now)) {
        stop(paste0("Failed to identify current Data_QDD='",sdata.qdd,"' in List_QDD='",paste0(sdata.qdds,collapse=", ")))
      }
      
      if (qdd_now==length(sdata.qdds)) {
        stop(paste0("Current Data_QDD='",sdata.qdd,"' which is the last QDD defined in List_QDD in this Template. If the RSA has been extended, the template and its termination date must be extended"))
      }
      
      qdd_next <- sdata.qdds[qdd_now+1]
      qdd_next <- openxlsx2::convert_to_excel_date(data.frame(qdd=qdd_next))$qdd
      
      excelwb$add_data(sheet=summarySheet,
                       x=qdd_next,
                       dims=nr_data_qdd$coords,
                       col_names=F,
                       row_names=F,
                       na="")
      
      excelwb$set_active_sheet(summarySheet)
      excelwb$set_selected(sheet=summarySheet)
      excelwb$set_sheetview(sheet=summarySheet,
                            top_left_cell = "A1")
    }
    
    return(excelwb)
  }  
  
  
  
  #excelwb$save("C:/Temp/newqr5.xlsx",overwrite=T)
  template <- list(cohort_pfcbl_id=rsf_pfcbl_id.facility,
                   reporting_asof_date=reporting_asof_date,
                   template_data=template_data,
                   pfcbl_reporting_flags=reporting_flags,
                   template_headers=unique(template_headers))

  status_message(class="info","Success: Completed Parsing File:\n")
  return (template)
}

