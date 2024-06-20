rsf_reports_excel_read_rsf_data <- function(excelwb,
                                            table_name="rsf_template_data",
                                            sheet_name=NULL) {
  
  if (is.null(sheet_name)) sheet_name <- excelwb$sheet_names[1]
  
  sheetNum <- which(tolower(excelwb$sheet_names)==tolower(sheet_name))
  if (length(sheetNum)==0) {
    stop(paste0("Failed to find sheet name ",sheet_name))
  }
  
  tables <- tolower(openxlsx::getTables(excelwb,sheetNum))
  if (!any(tables==table_name)) {
    return (NULL)
  }
  
  tables <- tables[which(tables==table_name)]
  if (length(tables) != 1) stop("Found multiple named entities 'rsf_template_date' and only one is allowed")
  range <- names(tables)
  
  #report_data <- lapply(names(tables),function(range) {
    
    rows <- as.numeric(str_extract_all(range,"\\d+")[[1]]) #table reader is included in rows, so tdata is offset by 1.
    
    
    #AS create entities template allows adding new rows, its possible users would add rows beyond the last line of the table instead of inserting
    #a new line into the table and expanding the table size.  So simply auto-expand to capture last row and read it in.
    max_rows <- max(excelwb$worksheets[[sheetNum]]$sheet_data$rows)
    
    if (max_rows > max(rows)) {
      rows[2] <- max_rows
    }
    
    rows <- seq(from=rows[1],to=rows[2],by=1)
    
    cols <- sapply(strsplit(range,":")[[1]],convertFromExcelRef,USE.NAMES = F)
    cols <- seq(from=cols[1],to=cols[2],by=1)
    max_cols <- max(excelwb$worksheets[[sheetNum]]$sheet_data$cols)
    
    if (max_cols > max(cols) && table_name=="rsf_template_data") {
      stop(paste0("Excel Data Table 'rsf_template_data' defines ",max(cols)," columns in the range ",range,".  This sheet has data outside the data table in column ",max_cols," that cannot br read-in. Data beyond column ",max(cols)," is not allowed."))
    }
    
    tdata <- readWorkbook(excelwb,sheet=sheetNum,startRow=rows[1],rows=rows,cols=cols,colNames=T)
    
   # tdata 
  #})
  setDT(tdata)
  
  setattr(tdata,
          name="excelColumns",
          value=cols)
  setattr(tdata,
          name="excelRows",
          value=rows)
  
  cols <- names(tdata)
  cols <- cols[grepl("^SYS",cols)==FALSE]
  cols <- cols[grepl("^reporting_",cols,ignore.case = T)==FALSE]
  for (col in cols) set(tdata,
                        j=col,
                        value=as.character(tdata[[col]]))
  
  if (empty(tdata)) return(NULL)
  else return (tdata)
}
