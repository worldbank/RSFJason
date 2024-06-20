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