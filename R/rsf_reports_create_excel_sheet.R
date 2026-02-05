rsf_reports_create_excel_sheet <- function(excelwb=NULL,
                                           sheet_name="RSF_DATA",
                                           sheet_data_table_name="RSF_TEMPLATE_DATA",
                                           sheet_data,
                                           
                                           header_name,
                                           header_image="IFC-CMCO_Horizontal_RGB_TransparentBG-high.png",
                                           
                                           template_key,              #required
                                           report_key=NA,             #optional, but when included, will concatenate template-key with ':'
                                           data_integrity_key="NONE", #Quasi-optional.   
                                           
                                           reporting_entity,
                                           reporting_asof_date,
                                           reporting_user,
                                           reporting_time=as.character(format.Date(now(),"%Y%b%d %Hh%M")),
                                           reporting_notes="",
                                           protect="IFCRSF",
                                           protect_full=FALSE) #protect just headers or full sheet
{


  
  START_ROW <- 10 #start row for data table
  header_image_scaling <- 4
  if (length(template_key) != 1 || all(is.na(template_key))) {
    stop("A valid template key must be provided")
  }
  named_region_prefix <- "RSF"
  if (!sheet_data_table_name %in% "RSF_TEMPLATE_DATA") named_region_prefix <- paste0(sheet_data_table_name,"_")
  
  named_region_prefix <- gsub("\\s+|_+","_",named_region_prefix)
  named_region_prefix <- paste0(named_region_prefix,"_")
  
  if (all(is.na(reporting_asof_date))) stop("Excel header requires reporting_asof_date")

  if (all(is.na(data_integrity_key)) || length(data_integrity_key) != 1) data_integrity_key <- "NONE"
  
  #if (merge_system_cols == TRUE && program_start_col < 5) stop("Excel header program start column must by 5 or greater")
  #if (merge_system_cols == FALSE && program_start_col < 4) stop("Excel header program start column must by 5 or greater")
  if (empty(sheet_data) || all(is.na(sheet_data))) {
    stop("Sheet data is missing or NA")
  }
  
  sheet_cols <- ncol(sheet_data)
  
  if (sheet_cols < 5) {
    sheet_cols <- 5
  }

  reporting_asof_date <- as.character(reporting_asof_date)
  
  if (is.null(excelwb)) {
    excelwb <- openxlsx::createWorkbook()
  }

  if (any(grepl(sheet_name,excelwb$sheet_names,ignore.case = T))) {
    stop(paste0("Cannot create Excel sheet '",sheet_name," in workbook: sheet already exists"))
  }

  addWorksheet(excelwb,
               sheetName=sheet_name,
               gridLines=FALSE,
               tabColour="royalblue") #colours()
  
  sheet <- which(excelwb$sheet_names==sheet_name)
  
  header <- matrix(data=NA,7,ncol=sheet_cols)
  
  if (!is.na(report_key)) {
    #header[1,c(1,2)] <- c("Report Key",paste0(toupper(report_key),":",toupper(template_key)))
    header[1,1] <- c(paste0(toupper(report_key),":",toupper(template_key)))
  } else {
    #header[1,c(1,2)] <- c("Template Key",toupper(template_key))
    header[1,1] <- c(toupper(template_key))
  }
  #header[2,c(1,2)] <- c("Data Integrity Key",data_integrity_key)
  header[2,1] <- c(data_integrity_key)
  
  header[3,c(1,2)] <- c("Entity",reporting_entity)
  header[4,c(1,2)] <- c("Data As-Of Date",reporting_asof_date)
  header[5,c(1,2)] <- c("Created By",reporting_user)
  header[6,c(1,2)] <- c("Notes",reporting_notes)
  header[7,c(1,2)] <- c("Time",reporting_time)
  
  header[1,3] <- header_name
  
  openxlsx::writeData(excelwb,sheet=sheet,x=header,colNames=FALSE,rowNames=FALSE,startCol=1,startRow=1)
  
  # createNamedRegion(excelwb,sheet=sheet,rows=1,cols=2,name=paste0(named_region_prefix,"REPORT_KEY"))
  # createNamedRegion(excelwb,sheet=sheet,rows=2,cols=2,name=paste0(named_region_prefix,"DATA_INTEGRITY_KEY"))
  createNamedRegion(excelwb,sheet=sheet,rows=1,cols=1,name=paste0(named_region_prefix,"REPORT_KEY"))
  createNamedRegion(excelwb,sheet=sheet,rows=2,cols=1,name=paste0(named_region_prefix,"DATA_INTEGRITY_KEY"))
  
  createNamedRegion(excelwb,sheet=sheet,rows=3,cols=2,name=paste0(named_region_prefix,"REPORTING_ENTITY"))
  createNamedRegion(excelwb,sheet=sheet,rows=4,cols=2,name=paste0(named_region_prefix,"REPORTING_DATE"))

  
  tableSysStyle  <- createStyle(fontSize = 11, fontColour = "black", halign = "left", fgFill = "gainsboro", textDecoration = "bold")
  tableProgStyle <- createStyle(fontSize = 16, fontColour = "white", textDecoration="bold", halign = "left", valign="center")
  

  #Keys and system data
  {
    tableKeysStyle <- createStyle(fontSize = 11, fontColour = "dimgrey", halign = "left", fgFill = "dimgrey")
    addStyle(wb=excelwb,
             sheet=sheet,
             tableKeysStyle,
             rows=1:2,
             cols=1:sheet_cols,
             gridExpand = TRUE)
    
    setRowHeights(
      wb=excelwb,
      sheet=sheet,
      rows=1:2,
      heights=27)
    
    #White border under keys table
    addStyle(excelwb,sheet=sheet,createStyle(border="bottom", borderColour = "white",borderStyle = "thin"),rows=2,cols=1:sheet_cols,stack=TRUE)
    
    #Big white program title  
    addStyle(excelwb,
             sheet=sheet,
             tableProgStyle,
             rows=1:2,
             cols=3:sheet_cols,
             gridExpand = TRUE,
             stack=TRUE)
    
    mergeCells(wb=excelwb,
               sheet=sheet,
               cols=3:sheet_cols,
               rows=1:2)
    
    if (!is.na(header_image)) {
      openxlsx::insertImage(excelwb,
                             sheet=sheet,
                             startRow=1,
                             startCol=1,
                             file=header_image,units="px",width=3060/header_image_scaling,height=777/header_image_scaling)

      if (length(excelwb$drawings[[sheet]]) > 0) {
        #offset image from edge by approx 5px (50,000 is 5 px??)
        excelwb$drawings[[sheet]][[1]] <- gsub("\\d+</xdr:colOff>","50000</xdr:colOff>",excelwb$drawings[[sheet]][[1]])
        excelwb$drawings[[sheet]][[1]] <- gsub("\\d+</xdr:rowOff>","50000</xdr:rowOff>",excelwb$drawings[[sheet]][[1]])
      }
    }
  }  
  
  #medium grey report info table
  addStyle(excelwb,sheet=sheet,tableSysStyle,rows=3:7,cols=1:sheet_cols,gridExpand = TRUE)

 
  
 
  
  openxlsx::writeDataTable(excelwb, 
                           sheet=sheet, 
                           x=sheet_data,
                           colNames=TRUE,
                           rowNames=FALSE,
                           startRow = START_ROW,
                           startCol = 1,
                           tableStyle="none",
                           tableName=sheet_data_table_name)
  
  setColWidths(excelwb,
               sheet=sheet,
               cols=1:ncol(sheet_data),
               widths=c(30))
  
  
  
  
  #PREHEADER DATA ROW, Data Table name
  {
    
    #Text title of the data table, eg, "RSF TEMPLATE DATA"
    openxlsx::writeData(excelwb,
                        sheet=sheet,
                        x=toupper(gsub("[[:punct:][:space:]]+"," ",sheet_data_table_name)), 
                        colNames=FALSE, 
                        rowNames=FALSE, 
                        startRow =START_ROW-1, 
                        startCol = 1)
    
    #Data Header background style: Blue
    addStyle(excelwb,
             sheet=sheet,
             createStyle(fontSize = 14, fontColour = "#FFFFFF", halign = "left", fgFill = "#4F81BD", border="TopBottom", borderColour = "#4F81BD"),
             rows=START_ROW-1,
             cols=1:sheet_cols,
             gridExpand = TRUE)
  }  
  
  #Data headers style: black font, gray background
  {
    addStyle(excelwb,
             sheet=sheet,
             createStyle(fontSize = 11, fontColour = "black", textDecoration="bold", halign = "left", fgFill = "gainsboro", border="Bottom", borderColour = "#4F81BD", borderStyle="thin"),
             rows=START_ROW,
             cols=1:sheet_cols,
             gridExpand = TRUE)
  }

  #sets the active cell to A9, the top of the datatable
  excelwb$worksheets[[sheet]]$sheetViews <-  '<sheetViews>  
  <sheetView tabSelected="1" workbookViewId="0" zoomScale="100">  
    <selection activeCell="A9" sqref="A9"/>  
    </sheetView>  
  </sheetViews>'
  
  protect <- trimws(protect)
  if (is.null(protect)==FALSE && all(is.na(protect))==FALSE & nchar(protect) >= 3) {
    
    protectWorksheet(excelwb,
                     sheet=sheet,
                     protect = TRUE,
                     password=protect,
                     lockSelectingLockedCells=FALSE,
                     lockSelectingUnlockedCells=FALSE,
                     lockFormattingCells = FALSE,
                     lockFormattingColumns = FALSE,
                     lockInsertingRows=FALSE,
                     lockSorting = FALSE,
                     lockAutoFilter = FALSE,
                     lockInsertingColumns = TRUE,
                     lockDeletingColumns = TRUE,
                     lockDeletingRows=TRUE)
    
    if (protect_full==FALSE) {

      n_rows <- max((excelwb$worksheets[[sheet]])$sheet_data$rows)
      
      addStyle(wb=excelwb, 
               sheet=sheet,
               style = createStyle(locked = FALSE),
               rows=START_ROW:n_rows, 
               cols=1:sheet_cols,
               gridExpand = T)
      
    }
  }  
  return(excelwb)
}