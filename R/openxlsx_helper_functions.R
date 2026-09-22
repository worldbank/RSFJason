openxlsx_get_formulas <- function(excelwb,
                                  sheetName,
                                  truncate_predata_rows=TRUE) {
  
  sheetNum <- NULL

  if (!inherits(excelwb,"Workbook")) {
    stop(paste0("Expected an openxlsx::Workbook object and instead received '",excelwb,"'."))
  }
  
  if (!is.na(suppressWarnings(as.numeric(sheetName)))) {
    sheetNum <- sheetName
  } else {
    sheetNum <- which(excelwb$sheet_names==sheetName)
  }
  
  excelwb$worksheets[[sheetNum]]$order_sheetdata()
  
  formulas <- data.table(xlsxIndex=seq_along(excelwb$worksheets[[sheetNum]]$sheet_data$f),
                         formulaXML=excelwb$worksheets[[sheetNum]]$sheet_data$f,
                         formulaRow=excelwb$worksheets[[sheetNum]]$sheet_data$rows,
                         formulaCol=excelwb$worksheets[[sheetNum]]$sheet_data$cols)
  
  formulas <- formulas[!is.na(formulaXML)]
  
  formulas[grepl("<f.*ref=.*>",formulaXML),ref:=gsub("<f.*ref=[\"']([[:alnum:]:]+)[\"'].*>$","\\1",formulaXML)]
  formulas[grepl("<f.*t=.*>",formulaXML),shared:=gsub("<f.*t=[\"']([[:alnum:]]+)[\"'].*>$","\\1",formulaXML)]
  formulas[grepl("<f.*si=.*>",formulaXML),si:=as.numeric(gsub("<f.*si=[\"']([[:alnum:]]+)[\"'].*>$","\\1",formulaXML))]
  formulas[!is.na(formulaXML),formula:=gsub("^<f.*>(.*)</f>$","\\1",formulaXML)]
  formulas[,fill_method:=as.character(NA)]
  formulas[!is.na(formulaXML),fill_method:=""]
  
  formulas[!is.na(ref) & shared=="shared",
           fill_method:="origin"]
  
  formulas[is.na(shared) & !is.na(formulaXML),
           `:=`(fill_method="none",
                formula=paste0(formula," @",openxlsx::int2col(formulaCol),":",formulaRow,""))]
  
  formulas[formulas[fill_method=="origin",.(si,formulaCol,formulaRow,formula)],
           `:=`(formula=ifelse(fill_method=="origin",formula,
                               paste0(i.formula," @",openxlsx::int2col(formulaCol),":",formulaRow," (fill-down from ",openxlsx::int2col(i.formulaCol),":",i.formulaRow,")")),
                fill_method=ifelse(fill_method=="origin","origin",
                                   "down")),
           on=.(si,formulaCol)]
  
  formulas[formulas[fill_method=="origin",.(si,formulaCol,formulaRow,formula)],
           `:=`(formula=ifelse(fill_method=="origin",formula,
                               paste0(i.formula," @",openxlsx::int2col(formulaCol),":",formulaRow," (fill-right from ",openxlsx::int2col(i.formulaCol),":",i.formulaRow,")")),
                fill_method=ifelse(fill_method=="origin","origin",
                                   "right")),
           on=.(si,formulaRow)]

  formulas[fill_method=="origin",
           formula:=paste0(formula," @",openxlsx::int2col(formulaCol),":",formulaRow,"")]
  
  formulas[!is.na(formula),
           formula:=paste0("=",formula)]
  
  formulaSheet <- matrix(nrow=max(excelwb$worksheets[[sheetNum]]$sheet_data$rows),
                         ncol=max(excelwb$worksheets[[sheetNum]]$sheet_data$cols))
  
  # for (i in formulas$xlsxIndex) formulaSheet[excelwb$worksheets[[sheetNum]]$sheet_data$rows[i],
  #                                            excelwb$worksheets[[sheetNum]]$sheet_data$cols[i]] <- formulas[xlsxIndex==i,formula]
  formulaSheet <- as.data.frame(formulaSheet)
  formulaSheet <- data.frame(lapply(formulaSheet,as.character))
  
  if (!empty(formulas)) {
    formulaSheet[as.matrix(formulas[,.(formulaRow,formulaCol)])] <- formulas$formula
  }

  if (truncate_predata_rows==TRUE) {

    firstDataPoint <- which.min(is.na(excelwb$worksheets[[sheetNum]]$sheet_data$v))
    firstDataRow <- excelwb$worksheets[[sheetNum]]$sheet_data$rows[firstDataPoint]
    if (firstDataRow != 1) {
      formulaSheet <- formulaSheet[firstDataRow:max(excelwb$worksheets[[sheetNum]]$sheet_data$rows),]
    }
    
    lastDataCol <- max(excelwb$worksheets[[sheetNum]]$sheet_data$cols[which(!is.na(excelwb$worksheets[[sheetNum]]$sheet_data$v))])
    if (lastDataCol < ncol(formulaSheet)) {
      formulaSheet <- formulaSheet[,1:lastDataCol]
    }
  }
  return (formulaSheet)
}

#for open
openxlsx_getNamedRegionsTable <- function(excelwb) {
  nregions <- openxlsx::getNamedRegions(excelwb)
  nregions_locations <- attr(nregions,"position")
  nregions_allowed <- grep("^[A-Z]+[0-9]+$|^[A-Z]+[0-9]+:[A-Z]+[0-9]+$",nregions_locations)
  nregions_locations <- nregions_locations[nregions_allowed]
  nregions_sheets <- attr(nregions,"sheet")[nregions_allowed]
  nregions <- nregions[nregions_allowed]
  
  nregion_data <- mapply(SIMPLIFY=F,FUN=function(sheet,cell,nr) {
    
    nregion_cols <- convertFromExcelRef(nregions_locations)
    cols <- NULL
    rows <- NULL
    
    if (grepl("^_xl|\\.wvu\\.",nr)) { return (NULL); }
    
    if (grepl(":",cell)) {
      cell <- strsplit(cell,":",fixed=T)[[1]]
      cols <- seq(from=convertFromExcelRef(cell[[1]]),
                  to=convertFromExcelRef(cell[[2]]),
                  by=1)
      
      rows <- seq(from=as.numeric(gsub("[^0-9]","",cell[[1]])),
                  to=as.numeric(gsub("[^0-9]","",cell[[2]])),
                  by=1)
    } else {
      cols <- convertFromExcelRef(cell)
      rows <- as.numeric(gsub("[^0-9]","",cell))
    }
    
    #if (nr=="Data_PeriodEndDate") { browser() }
    
    val <- suppressWarnings(openxlsx::readWorkbook(xlsxFile=excelwb,
                                                   sheet=sheet,
                                                   rows=rows,
                                                   cols=cols,
                                                   colNames=F,
                                                   rowNames=F,
                                                   skipEmptyRows = T,
                                                   skipEmptyCols = T))
    
    value <- NA
    
    if (length(val)==0) val <- NA
    
    
    range_val <- NULL    
    if (!is.data.frame(val)) {
      range_val <- as.data.frame(val)
    } else {
      range_val <- val
    }
    
    if (all(dim(val)==1)) {
      value <- unlist(unique(val))
    }
    
    if (nrow(range_val)==1 || ncol(range_val)==1) {
      range_val <- unlist(range_val,use.names = F)
      range_val <- list(c(range_val))
    }
    #browser()
    dt <- data.table(sheet=sheet,
                     range_name=nr,
                     range_value=as.character(value),
                     range_list=list(),
                     range_rows=list(rows),
                     range_cols=list(cols))
    
    dt[,range_list:=list(range_val)]
    
    
    #dt
    return(dt)
  },
  sheet=nregions_sheets,
  cell=nregions_locations,
  nr=nregions)
  
  nregion_data <- rbindlist(nregion_data)
  return (nregion_data)
}