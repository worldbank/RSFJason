#report_title <- "RSF Data"
#client_names <- "
#test_dt <- copy(dd_dt)
#dd_dt <- as.data.table(as.data.frame(test_dt)[1:100,])
#dd_dt$reporting_group <- 1:.N
export_dashboard_view_to_excel <- function(pool,
                                           export_asof_date,
                                           export_data,
                                           export_data_flags=NULL,
                                           exporting_user_id,
                                           report_id,
                                           report_note="None",
                                           options=list(useDropDowns=FALSE,
                                                        useTextFormat=TRUE,
                                                        writeFlags=FALSE),
                                           USE_TEMPLATE_NAME = "PFCBL-DASHBOARD-TEMPLATE",
                                           EXPORT_SHEET_NAME = "RSF DATA") {
  
  # ed <<- as.data.table(export_data)
  # exporting_user_id <<- exporting_user_id
  # report_id <<- report_id
  # report_note <<- report_note
  # export_asof_date <<- export_asof_date
  # export_data <<- export_data
  # opts <<- options
  # export_data <- as.data.table(as.data.frame(ed))
  #browser()
  
  #dd_dt <- as.data.table(as.data.frame(dd_dt))
  #options <- opts
  #SHEET_NAME <- "RSF DATA"
  SHEET_NAME_LISTS <- "Lists"
  START_ROW <- 10
  
  
  TEMPLATE <- db_export_get_template(pool,USE_TEMPLATE_NAME)
  
  options_FORMAT <- attr(export_data,"options_FORMAT")
  SYSID_NAMES <- attr(export_data,"SYSID_NAMES")
  
  if (is.null(SYSID_NAMES) &&
      any(names(export_data)=="SYSNAME") &&
      any(names(export_data)=="SYSID")) {
    SYSID_NAMES <- unique(export_data[,.(SYSNAME,SYSID)])
  }
  
  if (is.null(SYSID_NAMES)) {
    snames <- dbGetQuery(pool,"
      select
      nids.rsf_pfcbl_id,
      nids.rsf_full_name
      from p_rsf.view_current_entity_names_and_ids nids
      where nids.rsf_name = any(select unnest(string_to_array($1::text,','))::text)
        or nids.rsf_full_name = any(select unnest(string_to_array($1::text,','))::text)",
      params=list(paste0(names(export_data),collapse=",")))
    
    setDT(snames)
    SYSID_NAMES <- unique(snames[,.(SYSNAME=rsf_full_name,SYSID=rsf_pfcbl_id)])
    
  }
  SYSID_NAMES <- setNames(SYSID_NAMES$SYSID,
                          SYSID_NAMES$SYSNAME)
  #print("Generating excel report")
  # #options:
  # useDropDowns <- options$useDropDowns
  # useTextFormat <- options$useTextFormat
  # writeFlags <- options$writeFlags
  # 
  # if (is.null(useDropDowns)) useDropDowns <- FALSE
  # if (is.null(useTextFormat)) useTextFormat <- FALSE
  # if (is.null(writeFlags)) writeFlags <- FALSE
  
  #https://stackoverflow.com/questions/56068262/openxlsx-protect-sheet-but-allow-entering-of-values


  # indicator_data_cols <- indicatorNames_getByAttribute(ind_names=names(export_data),attribute="current")
  # if (useTextFormat==TRUE) {
  #   if (!any(grepl("\\.text$",names(dd_dt)))) stop("Export options specify useTextFormat=TRUE but current.text column names are not provided in export dataset")
  #   indicator_data_cols <- indicatorNames_getByAttribute(ind_names=names(dd_dt),attribute="text")
  # }
  export_data[,reporting_rowid:=1:.N]
  
  export_names <- NULL
  
  if (any(names(export_data)=="indicator_name")) {
    export_names <- data.table(column_name=export_data$indicator_name)
  } else if (any(names(export_data)=="INDICATOR")) {
    export_names <- data.table(column_name=export_data$INDICATOR)
  } else {
    export_names <- data.table(column_name=names(export_data))
  }
  
  export_names[,indicator_name:=indicatorNames_getBasenames(column_name)]
  export_names[,indicator_name:=gsub("@[A-Z]{3}$","",indicator_name)] #to omit any currency conversions

  dd_indicators <- dbGetQuery(pool,"
                              select 
                                ind.indicator_id,
                                ind.indicator_name,
                                ind.data_type
                              from p_rsf.indicators ind
                              where ind.indicator_name = any(select unnest(string_to_array($1::text,','))::text)",
                              params=list(paste0(export_names$indicator_name,collapse=",")))
  setDT(dd_indicators)
  
  export_names <- export_names[dd_indicators,
                               `:=`(indicator_id=i.indicator_id,
                                    data_type=i.data_type),
                               on=.(indicator_name)]
  
  exporting_asof_date <- as.character(max(unique(export_asof_date)))
  
  export_cohort <- db_export_create(pool,
                                    exporting_user_id=exporting_user_id,
                                    exporting_asof_date=exporting_asof_date,
                                    exporting_pfcbl_ids=unname(SYSID_NAMES),
                                    exporting_indicator_ids=export_names[!is.na(indicator_id),indicator_id],
                                    template_id=TEMPLATE$template_id,
                                    export_name="RSF Dashboard")

  if (empty(export_cohort)) stop("Failed to create export cohort")
  
  
  # reporting_timeline <- export_data$reporting_history
  # has_history <- any(reporting_timeline)
  # 
  # omit_cols <- which(names(export_data) %in% c("reporting_group",
  #                                              "reporting_group_sort",
  #                                              "reporting_intraperiod_rank",
  #                                              "reporting_history"))
  # 
  # if (length(omit_cols) > 0) {
  #   export_cols <- names(export_data)
  #   export_cols <- export_cols[-omit_cols]
  #   export_data <- export_data[,..export_cols]
  # }
  # 
  # report_cols <- grep("^reporting_.*$|^indicator_name$",names(export_data))
  # data_cols <- (1:length(names(export_data)))[-report_cols]
  # 
  # setnames(export_data,
  #          old=names(export_data)[report_cols],
  #          new=toupper(gsub("^reporting_","",names(export_data)[report_cols])))
  # 
  # setnames(export_data,
  #          old="ASOF_DATE",
  #          new="REPORTING_DATE")
  
  
  if (any(names(export_data)=="REPORTING_asof_date")) {
    export_data[,REPORTING_asof_date:=as.character(REPORTING_asof_date)]
  }
  
  col_types <- sapply(names(export_data),function(col_name) {
    
    col_type <- "TEXT"
    ind_type <- "system"
    if (any(col_name %in% export_names$column_name)) {
      ind_type <- export_names[column_name==col_name,data_type]
      col_type <- fcase(ind_type=="currency","NUMBER",
                        ind_type=="date","DATE",
                        ind_type=="percent","PERCENTAGE",
                        ind_type=="number","NUMBER",
                        ind_type=="text","TEXT",
                        ind_type=="currency_ratio","TEXT",
                        ind_type=="logical","TEXT",
                        default="TEXT")      
    }
    
    #print(paste0(col_name," ",ind_type))
    if (col_name=="REPORTING_asof_date") {
      is_date <- grepl("\\d{4}-\\d{1,2}-\\d{1,2}",export_data[[col_name]]) | nchar(export_data[[col_name]])==0 | is.na(export_data[[col_name]])
      is_num <- grepl("\\d+",export_data[[col_name]])
      if (all(is_date)) col_type <- "DATE"
      else if (all(is_num)) col_type <- "NUMBER"
    }
    
    if (col_type %in% c("NUMBER","PERCENTAGE","CURRENCY")) {

      #Because @LCU columns may include, eg "10000 XOF" and "1533 UAH" in the columns and aren't numbers, despite the type.
      valid_numbers <- all(is.na(export_data[[col_name]]) | !is.na(suppressWarnings(as.numeric(export_data[[col_name]]))))
      if (valid_numbers) {
        
        if (!grepl("date|id",col_name)) {
          if (ind_type=="currency") col_type <- "0.00"
          else if (col_type=="NUMBER" && !any(grepl("\\.",export_data[[col_name]]))) col_type <- "#,##0"
        } 
        
        if (grepl("id",col_name)) {
          col_type <- "0"
        }
      

        set(export_data,
            i=NULL,
            j=col_name,
            value=suppressWarnings(as.numeric(export_data[[col_name]])))
      }
    }
    
    if (col_type %in% c("DATE")) {
      is_date <- grepl("\\d{4}-\\d{1,2}-\\d{1,2}",export_data[[col_name]]) | nchar(export_data[[col_name]])==0 | is.na(export_data[[col_name]])
      if (all(is_date)) {
        set(export_data,
            i=NULL,
            j=col_name,
            value=ymd(export_data[[col_name]]))
      }
    }
    col_type
  })
  
  #sapply(export_data,class)
  excelwb <-  rsf_reports_create_excel_sheet(sheet_name="RSF_DATA",
                                             sheet_data_table_name="RSF_TEMPLATE_DATA",
                                             sheet_data=export_data,
                                             
                                             program_name=export_cohort$program_name,
                                             program_image="IFC-CMCO_Horizontal_RGB_TransparentBG-high.png",
                                             
                                             template_key=TEMPLATE$template_key,     #required
                                             report_key=export_cohort$reporting_key, #optional, but when included, will concatenate template-key with ':'
                                             data_integrity_key=export_cohort$data_integrity_key, #Quasi-optional.   
                                             
                                             reporting_entity=export_cohort$exporting_entity_name,
                                             reporting_asof_date=export_cohort$exporting_asof_date,
                                             reporting_user=format_name_abbreviation(export_cohort$exporting_users_name),
                                             reporting_time=export_cohort$exporting_time,
                                             reporting_notes=report_note,
                                             protect=NA)
  

  
  #print("Creating Excel Workbook")
  #excelwb <- openxlsx::createWorkbook()
  #addWorksheet(excelwb,"RSF Data")

  for (colnum in seq_along(col_types)) {
    
    col_type <- col_types[[colnum]]
    addStyle(excelwb,
             sheet="RSF_DATA",
             style=createStyle(numFmt=col_type),
             rows=(START_ROW+1):(START_ROW+nrow(export_data)),
             cols=colnum,
             gridExpand = TRUE)
    
  }

  #savedwb <- openxlsx::saveWorkbook(excelwb,"test3.xlsx",overwrite=TRUE)
  
  # if (useDropDowns==TRUE) {
  #   # print("Creating drop downs")
  #   # stop("useDropDowns==TRUE feature not yet implemented")
  #   # t1 <- Sys.time()
  #   # ops_indicators <- dbGetQuery(pool,"select ind.indicator_id,ind.indicator_name,ind.indicator_options_group_id,ind.indicator_options_group_allows_blanks,iog.options_group_name
  #   #                                    from p_rsf.indicators ind
  #   #                                    inner join p_rsf.indicator_options_groups iog on iog.options_group_id = ind.indicator_options_group_id
  #   #                                    where array[indicator_name] && string_to_array($1,',')::varchar[]
  #   #                                    and indicator_options_group_id is not null",
  #   #                                    params=list(paste0(indicator_basenames,collapse=',')))
  #   # setDT(ops_indicators)
  #   # if (!empty(ops_indicators)) {
  #   #   options <- dbGetQuery(pool,"select distinct topic,location,phrase_id,section as options_group_name,primary_phrase
  #   #                               from p_rsf.view_rsf_labels_options
  #   #                               where array[section] && string_to_array($1,',')::text[]
  #   #                               and language = 'english'
  #   #                               order by options_group_name,phrase_id",
  #   #                               params=list(paste0(unique(ops_indicators$options_group_name),collapse=',')))
  #   #   options <- as.data.table(options)
  #   #   
  #   #   #https://stackoverflow.com/questions/29898269/possible-to-write-excel-formulas-or-data-validation-using-r/55191118#55191118
  #   #   addWorksheet(excelwb,SHEET_NAME_LISTS,visible=FALSE)
  #   #   
  #   #   options_names <- unique(ops_indicators$options_group_name)
  #   #   for (op_index in 1:length(options_names)) {
  #   #     op <- options_names[op_index]
  #   #     indicators <- ops_indicators[options_group_name==op,indicator_name]
  #   #     op_values <- options[options_group_name==op,.(phrase_id=as.character(phrase_id),primary_phrase=as.character(primary_phrase))]
  #   #     for (ind in indicators) {
  #   #       
  #   #       #rdata[,..ind][op_values,on=.(borrower_industry=phrase_id)]
  #   #       translated_values <- op_values[rdata[,.(phrase_id=as.character(unlist(.SD)),original=as.character(unlist(.SD))),.SDcols=ind],on=.(phrase_id)]
  #   #       translated_values[is.na(primary_phrase),primary_phrase:=original]
  #   #       translated_values <- translated_values$primary_phrase
  #   #       if (length(translated_values) != nrow(rdata)) stop("Error in translations: translated length doesn't match original length")
  #   #       set(rdata,i=NULL,j=ind,value=translated_values)
  #   #     }
  #   #     op_df <- as.data.frame(dots_list(!!op:=op_values$primary_phrase))
  #   #     writeData(excelwb, sheet = 2, x = op_df, startCol = op_index)
  #   #     
  #   #     op_cols <- match(indicators,names(rdata))
  #   #     cellRef <- paste0(getCellRefs(data.frame(row=2,col=op_index)),":",getCellRefs(data.frame(row=nrow(op_df)+1,col=op_index)))
  #   #     cellRef <- gsub("([A-Z]+)","$\\1",cellRef)
  #   #     cellRef <- gsub("([0-9]+)","$\\1",cellRef)
  #   #     cellRef <- paste0("'",SHEET_NAME_LISTS,"'!",cellRef)
  #   #     
  #   #     for (col_num in op_cols) {
  #   #       row_nums <- which(dd_dt$reporting_group_rank==-1)+START_ROW #for some reason adding in bulk causes auto-fill down the entire table
  #   #       for (row_num in row_nums) dataValidation(excelwb, sheet=1, col = col_num, rows = row_num, type = "list", value = cellRef)
  #   #     }
  #   #   }
  #   # }
  #   # print(paste0("Dropdowns created: ",(Sys.time()-t1)))
  # }
  
  #print("Writing data")  
  #openxlsx::writeData(excelwb,sheet=1,x=rheaders,colNames=FALSE,rowNames=FALSE)

  #createNamedRegion(excelwb,sheet=1,cols=3,rows=1,name="REPORT_KEY")
  #createNamedRegion(excelwb,sheet=1,cols=3,rows=2,name="DATA_INTEGRITY_KEY")
  #createNamedRegion(excelwb,sheet=1,cols=3,rows=3,name="REPORTING_DATE")

  
  #openxlsx::writeData(excelwb, sheet=1, x=program_info$name, colNames=FALSE, rowNames=FALSE, startRow = 1, startCol = 7) #right-aligned
  #openxlsx::insertImage(excelwb,sheet=1,startRow=4,startCol=8,file="IFC-CMCO_Horizontal_RGB_TransparentBG-high.png",units="px",width=3060/4,height=777/4) #don't understand the measures, but dims are 244x62  
  # 
  # openxlsx::writeDataTable(excelwb, 
  #                          sheet=1, 
  #                          x=export_data,
  #                          colNames=TRUE, 
  #                          rowNames=FALSE, 
  #                          startRow = START_ROW, 
  #                          startCol = 1,
  #                          tableStyle="none",
  #                          tableName="rsf_template_data")
  
  #currently disabled
  # if (FALSE & !empty(export_data_flags)) {
  #   
  #   #print("Generating flags")
  #   #stop("Write flags currently disabled")
  #   #t1 <- Sys.time()
  #   flagStyle <- createStyle(fontSize=9)
  #   
  #   for (cname in (names(export_data)[data_cols])) {
  #     col_flags <- export_data_flags[[cname]]
  #     col_flags[nchar(col_flags)==0] <- as.character(NA)
  #     
  #     if (all(is.na(col_flags))) next;
  #     
  #     col_num <- which(names(export_data)==cname)
  #     for (row_num in 1:length(col_flags)) {
  #       flagtext <- col_flags[[row_num]]
  #       if (is.na(flagtext)) next
  #       
  #       comment <- createComment(author="System",
  #                                comment=flagtext,
  #                                visible=FALSE,
  #                                style=flagStyle,
  #                                height=4,
  #                                width=3)
  #       
  #       writeComment(excelwb,
  #                    sheet=1,
  #                    col = col_num,
  #                    row = START_ROW+row_num,
  #                    comment = comment)
  #     }
  #   }
  # }
  
  # print("Applying styles")
  # t1 <- Sys.time()
  # 
  # 
  # if (any(names(export_data)=="REPORTING_DATE"))   setColWidths(excelwb,sheet=1,cols=which(names(export_data)=="REPORTING_DATE"),widths=c(12))
  # if (any(names(export_data)=="SYSID"))   setColWidths(excelwb,sheet=1,cols=which(names(export_data)=="SYSID"),widths=c(8))
  # if (any(names(export_data)=="STATUS"))   setColWidths(excelwb,sheet=1,cols=which(names(export_data)=="STATUS"),widths=c(12))
  # if (any(names(export_data)=="ROWID"))   setColWidths(excelwb,sheet=1,cols=which(names(export_data)=="ROWID"),widths=c(4))
  # if (any(names(export_data)=="NAME"))   setColWidths(excelwb,sheet=1,cols=which(names(export_data)=="NAME"),widths=c(20))
  # 
  # setColWidths(excelwb,sheet=1,cols=(1:ncol(export_data))[report_cols],widths=18)
  # 
  
  
  # #PREHEADER DATA ROW, Data Table name
  # {
  #   mergeCells(excelwb,sheet=1,rows=START_ROW-1,cols=1:ncol(export_data))
  #   openxlsx::writeData(excelwb, sheet=1, x="RSF Data Table", colNames=FALSE, rowNames=FALSE, startRow =START_ROW-1, startCol = 1)
  #   #Data Header background style: Blue
  #   addStyle(excelwb,
  #            sheet=1,
  #            createStyle(fontSize = 14, fontColour = "#FFFFFF", halign = "left", fgFill = "#4F81BD", border="TopBottom", borderColour = "#4F81BD"),
  #            rows=START_ROW-1,
  #            cols=1:ncol(export_data),
  #            gridExpand = TRUE)
  # }  
  # 
  # #Data headers style: black font, gray background
  # {
  #   addStyle(excelwb,
  #            sheet=1,
  #            createStyle(fontSize = 11, fontColour = "black", textDecoration="bold", halign = "left", fgFill = "gainsboro", border="Bottom", borderColour = "#4F81BD", borderStyle="thin"),
  #            rows=START_ROW,
  #            cols=1:ncol(export_data),
  #            gridExpand = TRUE)
  # }

# if (FALSE) {
#   if (has_history==FALSE) {
#     evenGroups <- export_data$ROWID%%2==0 #name changed from reporting_group to `Row#`
#     oddGroups <- export_data$ROWID%%2==1
#     
#     tableEvenStyle <- createStyle(fontSize=10, fgFill = rgba2hex(DASH_DISPLAY_BLUE_C))
#     tableOddStyle <- createStyle(fontSize=10, fgFill = rgba2hex(DASH_DISPLAY_YELLOW_C))
#   
#     addStyle(excelwb,
#              sheet=1,
#              tableEvenStyle,
#              rows=which(evenGroups)+START_ROW,
#              cols=1:ncol(export_data),
#              gridExpand = TRUE)
#   
#     addStyle(excelwb,
#              sheet=1,
#              tableOddStyle,
#              rows=which(oddGroups)+START_ROW,
#              cols=1:ncol(export_data),
#              gridExpand = TRUE)
#     
#     addStyle(excelwb, 
#              sheet=1, 
#              style = createStyle(locked = FALSE), 
#              rows = (1:nrow(export_data))+START_ROW,
#              cols = data_cols,
#              gridExpand = TRUE, 
#              stack=TRUE)
# 
#   
#   
#   } else {
#     sys_id_group=frank(export_data,"SYSID",ties.method = "dense")
#     
#     evenGroupCurrent <- sys_id_group%%2==0 & reporting_timeline == FALSE
#     evenGroupHistory <- sys_id_group%%2==0 & reporting_timeline == TRUE
#     
#     oddGroupCurrent <- sys_id_group%%2==1 & reporting_timeline == FALSE
#     oddGroupHistory <- sys_id_group%%2==1 & reporting_timeline == TRUE
# 
#     tableEvenStyleCurrent <- createStyle(fontSize=10, fgFill = rgba2hex(DASH_DISPLAY_BLUE_CH)) 
#     tableEvenStyleHistory <- createStyle(fontSize=10, fgFill = rgba2hex(DASH_DISPLAY_BLUE_HH))
#     
#     tableOddStyleCurrent <- createStyle(fontSize=10, fgFill = rgba2hex(DASH_DISPLAY_YELLOW_CH))
#     tableOddStyleHistory <- createStyle(fontSize=10, fgFill = rgba2hex(DASH_DISPLAY_YELLOW_HH))
#     
#     # tableEvenStyleCurrent <- createStyle(fontSize=10, fgFill = "#D7FFD7") #Sea green
#     # tableEvenStyleHistory <- createStyle(fontSize=10, fgFill = "#EFFDEF")
#     # 
#     # tableOddStyleCurrent <- createStyle(fontSize=10, fgFill = "#e0f0fa") #Light Blue
#     # tableOddStyleHistory <- createStyle(fontSize=10, fgFill = "#F2F7FB")
#     
#     historyBorderStyle <- createStyle(border="bottom", borderColour = "white",borderStyle = "thin")
#     currentBorderStyle <- createStyle(border="top", borderColour = "dimgrey",borderStyle = "thin")
#     
#     addStyle(excelwb,
#              sheet=1,
#              tableEvenStyleCurrent,
#              rows=which(evenGroupCurrent)+START_ROW,
#              cols=1:ncol(export_data),
#              gridExpand = TRUE)
# 
#     addStyle(excelwb,
#              sheet=1,
#              tableEvenStyleHistory,
#              rows=which(evenGroupHistory)+START_ROW,
#              cols=1:ncol(export_data),
#              gridExpand = TRUE)
#     
#     addStyle(excelwb,
#              sheet=1,
#              tableOddStyleCurrent,
#              rows=which(oddGroupCurrent)+START_ROW,
#              cols=1:ncol(export_data),
#              gridExpand = TRUE)
#     
#     addStyle(excelwb,
#              sheet=1,
#              tableOddStyleHistory,
#              rows=which(oddGroupHistory)+START_ROW,
#              cols=1:ncol(export_data),
#              gridExpand = TRUE)
#     
#     addStyle(excelwb,
#              sheet=1,
#              historyBorderStyle,
#              rows=which(oddGroupHistory | evenGroupHistory)+START_ROW,
#              cols=1:ncol(export_data),
#              gridExpand = TRUE,
#              stack = TRUE)
# 
#     addStyle(excelwb,
#              sheet=1,
#              currentBorderStyle,
#              rows=which(oddGroupCurrent | evenGroupCurrent)+START_ROW,
#              cols=1:ncol(export_data),
#              gridExpand = TRUE,
#              stack = TRUE)
#     
#     #first 4 cols are sys cols
#     addStyle(excelwb, 
#              sheet=1, 
#              style = createStyle(locked = FALSE), 
#              rows = which(evenGroupCurrent | oddGroupCurrent)+START_ROW, 
#              cols = data_cols,
#              gridExpand=TRUE,
#              stack=TRUE)
#     
#   }
#   addStyle(excelwb,
#            sheet=1, 
#            style = createStyle(locked = FALSE), 
#            rows = nrow(export_data):(nrow(export_data)+100)+START_ROW, #Unlocks the last 100 rows (for modifying/adding new rows)
#            cols = data_cols, 
#            gridExpand=TRUE, 
#            stack=TRUE)
#   
#   #openxlsx::saveWorkbook(excelwb,file="test2.xlsx",overwrite=TRUE)
#   
#   
#   if (useTextFormat==TRUE) {
#     arrowStyle <- createStyle(fontColour = "#910f3c", fontSize = 10)
#     voidStyle <- createStyle(fontColour = "#a6a6a6")
#     missingStyle <- createStyle(fontColour = "#9b6432")
#     blankStyle <- createStyle(fontColour = "#dcdcdc")
#     
#     for (col_index in data_cols) {
#       arrowRows <- which(export_data[[col_index]] %in% c(as.character(intToUtf8(0x2191)),as.character(intToUtf8(0x2195))))
#       addStyle(excelwb,sheet=1,style=arrowStyle,rows=arrowRows+START_ROW,cols=col_index,stack=TRUE)
#       
#       voidRows <- which(export_data[[col_index]]=="{VOID}")
#       addStyle(excelwb,sheet=1,style=voidStyle,rows=voidRows+START_ROW,cols=col_index,stack=TRUE)
# 
#       missingRows <- which(export_data[[col_index]]=="{MISSING}")
#       addStyle(excelwb,sheet=1,style=missingStyle,rows=missingRows+START_ROW,cols=col_index,stack=TRUE)
#       
#       blankRows <- which(export_data[[col_index]]=="{BLANK}")
#       addStyle(excelwb,sheet=1,style=blankStyle,rows=blankRows+START_ROW,cols=col_index,stack=TRUE)
#       
#     }
#   }
# 
#   showGridLines(excelwb, sheet=1, showGridLines = FALSE)
#   
#   print(paste0("Styles created: ",(Sys.time()-t1)))
#   
#   print("Protecting sheets")
#   
#   sheet_names <- openxlsx::sheets(excelwb)
#   if (SHEET_NAME_LISTS %in% sheet_names) {
#     protectWorksheet(excelwb, 
#                      sheet=SHEET_NAME_LISTS,
#                      protect = TRUE,
#                      password="IFCRSF")
#   }
# }
  
  # protectWorksheet(excelwb, 
  #                  sheet=EXPORT_SHEET_NAME,
  #                  protect = TRUE,
  #                  password="IFCRSF",
  #                  lockSelectingLockedCells=FALSE,
  #                  lockSelectingUnlockedCells=FALSE,
  #                  lockFormattingCells = FALSE, 
  #                  lockFormattingColumns = FALSE, 
  #                  lockInsertingColumns = TRUE,
  #                  lockDeletingColumns = TRUE,
  #                  lockInsertingRows=FALSE,
  #                  lockDeletingRows=TRUE,
  #                  lockSorting = FALSE,
  #                  lockAutoFilter = FALSE)
  
  #This line allows specified cells to be unlocked so that users can enter values.
  
  
  #print("Completed excelwb")
  #print(Sys.time()-t0)
  #saveWorkbook(excelwb,file="./dashboard_test.xlsx",overwrite=TRUE)
  
  return(excelwb)  
}
