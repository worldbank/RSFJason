template_excel_read_sheet <- function(excelwb,
                                      sheetName) {
  
  excelSheet <- tryCatch({ openxlsx::read.xlsx(xlsxFile=excelwb,sheet=sheetName,colNames=FALSE, skipEmptyRows=FALSE,skipEmptyCols=FALSE,detectDates=TRUE) },
                         error = function(err) {
                           print(conditionMessage(err))
                           openxlsx::read.xlsx(xlsxFile=excelwb,sheet=sheetName,colNames=FALSE, skipEmptyRows=FALSE,skipEmptyCols=FALSE,detectDates=FALSE)
                         },
                         warning = function(war) {
                           #will throw an NA introduced by coersion due to detectDates, but this can cause text info to be eliminated.  Eg, in summary sheet, XOF and % get lost
                           openxlsx::read.xlsx(xlsxFile=excelwb,sheet=sheetName,colNames=FALSE, skipEmptyRows=FALSE,skipEmptyCols=FALSE,detectDates=FALSE)
                         })
}