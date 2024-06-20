rsf_reports_excel_header <- function(excelwb=NULL,
                                     sheet_name="RSF_DATA",
                                     sheet_cols=11,
                                     program_name,
                                     program_image="IFC-CMCO_Horizontal_RGB_TransparentBG-high.png",
                                     # program_image_scaling=4,
                                     # program_image_start_col=8,
                                     # program_start_col=7,
                                     # merge_system_cols=TRUE,
                                     template_key,              #required
                                     report_key=NA,             #optional, but when included, will concatenate template-key with ':'
                                     data_integrity_key="NONE", #Quasi-optional.   
                                     reporting_entity,
                                     reporting_asof_date,
                                     reporting_user,
                                     reporting_time=as.character(now()),
                                     reporting_notes="")
{
  
  stop("Deprecated: use rsf_reports_create_excel_sheet() instead")
  program_image_scaling <- 4
  if (length(template_key) != 1 || all(is.na(template_key))) {
    stop("A valid template key must be provided")
  }
  
  if (all(is.na(reporting_asof_date))) stop("Excel header requires reporting_asof_date")

  if (all(is.na(data_integrity_key)) || length(data_integrity_key) != 1) data_integrity_key <- "NONE"
  
  #if (merge_system_cols == TRUE && program_start_col < 5) stop("Excel header program start column must by 5 or greater")
  #if (merge_system_cols == FALSE && program_start_col < 4) stop("Excel header program start column must by 5 or greater")
  
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
    header[1,c(1,2)] <- c("Report Key",paste0(toupper(report_key),":",toupper(template_key)))
  } else {
    header[1,c(1,2)] <- c("Template Key",toupper(template_key))
  }
  
  header[2,c(1,2)] <- c("Data Integrity Key",data_integrity_key)
  header[3,c(1,2)] <- c("Reporting Entity",reporting_entity)
  header[4,c(1,2)] <- c("Reporting As-Of Date",reporting_asof_date)
  header[5,c(1,2)] <- c("Created By",reporting_user)
  header[6,c(1,2)] <- c("Notes",reporting_notes)
  header[7,c(1,2)] <- c("Export Time",reporting_time)
  
  header[1,4] <- program_name
  
  if (sheet==1) {
    createNamedRegion(excelwb,sheet=sheet,rows=1,cols=2,name="RSF_REPORT_KEY")
    createNamedRegion(excelwb,sheet=sheet,rows=2,cols=2,name="RSF_DATA_INTEGRITY_KEY")
    
    createNamedRegion(excelwb,sheet=sheet,rows=3,cols=2,name="RSF_REPORTING_ENTITY")
    createNamedRegion(excelwb,sheet=sheet,rows=4,cols=2,name="RSF_REPORTING_DATE")
    
  }
  
  
  
  openxlsx::writeData(excelwb,sheet=sheet,x=header,colNames=FALSE,rowNames=FALSE,startCol=1,startRow=1)
  tableSysRows <- 1:7

  
  tableKeysStyle <- createStyle(fontSize = 11, fontColour = "black", halign = "left", fgFill = "dimgrey")
  tableSysStyle  <- createStyle(fontSize = 11, fontColour = "black", halign = "left", fgFill = "gainsboro", textDecoration = "bold")
  tableProgStyle <- createStyle(fontSize = 16, fontColour = "white", textDecoration="bold", halign = "left", valign="center")
  
  
  # if (merge_system_cols==TRUE) {
  #   for (r in unique(c(tableKeysRows,tableSysRows))) {
  #     mergeCells(excelwb,sheet=sheet,rows=r,cols=1:2)
  #     mergeCells(excelwb,sheet=sheet,rows=r,cols=3:(program_start_col-1))
  #   }
  #   mergeCells(excelwb,sheet=sheet,rows=tableKeysRows,cols=program_start_col:sheet_cols)
  # }
  
  #Light grey keys table
  addStyle(excelwb,sheet=sheet,tableKeysStyle,rows=1:2,cols=1:sheet_cols,gridExpand = TRUE)
  
  #White border under keys table
  addStyle(excelwb,sheet=sheet,createStyle(border="bottom", borderColour = "white",borderStyle = "thin"),rows=2,cols=1:sheet_cols,stack=TRUE)
  
  #medium grey report info table
  addStyle(excelwb,sheet=sheet,tableSysStyle,rows=3:7,cols=1:sheet_cols,gridExpand = TRUE)

  #Big white program title  
  addStyle(excelwb,sheet=sheet,tableProgStyle,rows=1:2,cols=4:sheet_cols,gridExpand = TRUE,stack=TRUE)
  
  if (!is.na(program_image)) openxlsx::insertImage(excelwb,
                                                   sheet=sheet,
                                                   startRow=4,
                                                   startCol=4,
                                                   file=program_image,units="px",width=3060/program_image_scaling,height=777/program_image_scaling)

  #saveWorkbook(excelwb,file="test_headers4.xlsx",overwrite=TRUE)
  
  return(excelwb)
}