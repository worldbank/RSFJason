#cohort.flags.only: when TRUE will select data only with non-NULL flags.
#                   when FALSE will select flagged data and also actual data points saved by cohort, whether flagged or unflagged
#this.cohort.only: when FALSE will include related/sibling rsf_pfcbl_ids according to children under cohort's overall reporting_id
#                  when TRUE will include only data points that match the reporting_cohort_id of the cohort (which may mean if its generated historically, data may have increased data_sequence_ranks)
export_reporting_cohort_to_excel <- function(pool,
                                             reporting_cohort_id,
                                             exporting_user_id,
                                             cohort_data=db_cohort_get_data(pool=pool,
                                                                            reporting_cohort_id=reporting_cohort_id)) {
                                             #flags.only=TRUE,
                                             #indicator_type_filters=NA,
                                             #indicator_category_filters=NA) {
  #Hard-coded in database.
  USE_TEMPLATE_NAME <- "PFCBL-COHORT-TEMPLATE"
  SHEET_NAME <- "RSF DATA"

  if (empty(cohort_data)) return(NULL)
  
  freport <- cohort_data
  # freport <- db_cohort_get_data(pool=pool,
  #                               reporting_cohort_id=reporting_cohort_id,
  #                               view=view) 
  # 
  freport[,n:=frank(freport,cols="rsf_pfcbl_id",ties.method = "dense")]
  
  #if (flags.only==TRUE) freport <- freport[has_data_flags_active == TRUE & data_sequence_rank==1]
  
  if(SYS_PRINT_TIMING)  debugtime("export_reporting_cohort_to_excel",reset=TRUE)

  TEMPLATE <- db_export_get_template(pool,USE_TEMPLATE_NAME)
  
  rsf_program_id <- unique(freport$rsf_program_id)
  rsf_program_name <- dbGetQuery(pool,
                                "select rsf_name
                                 from p_rsf.view_current_entity_names_and_ids nids
                                 where nids.rsf_program_id = $1::int 
                                   and nids.pfcbl_category in ('global','program')",
                                 params=list(rsf_program_id))
  rsf_program_name <- unlist(rsf_program_name,use.names = F)
  
  reporting_asof_date <- as.character(unique(freport[["reporting_asof_date"]]))
  reporting_time <- unique(freport[["reporting_time"]])
  
  integrity_key <- rsf_reports_data_integrity_key(reporting_asof_date=reporting_asof_date,
                                                  rsf_pfcbl_ids = freport$rsf_pfcbl_id)

  #Export this cohort dataset "under" the reporting rsf_pfcbl_id of that cohort.
  exporting_pfcbl_id <- dbGetQuery(pool,"select reporting_rsf_pfcbl_id from p_rsf.reporting_cohorts where reporting_cohort_id = $1::int",
                                   params=list(reporting_cohort_id))
  exporting_pfcbl_id <- unlist(exporting_pfcbl_id)
  
  export_cohort <- db_export_create(pool=pool,
                                    exporting_user_id=exporting_user_id,
                                    exporting_asof_date=reporting_asof_date,
                                    exporting_pfcbl_ids=exporting_pfcbl_id,
                                    template_id=TEMPLATE$template_id,
                                    export_name=NA)
  
  
  if(SYS_PRINT_TIMING)  debugtime("export_reporting_cohort_to_excel","export cohort created with db_export_create")
  
  report_key <- export_cohort$reporting_key
  
  source_name <- unique(freport[["source_name"]])
  source_reference <- unique(freport[["source_reference"]])
  source_note <- unique(freport[["source_note"]])

  
  excelwb <- rsf_reports_excel_header(sheet_name=SHEET_NAME,
                                       sheet_cols=11,
                                       program_name=rsf_program_name,
                                       program_image="IFC-CMCO_Horizontal_RGB_TransparentBG-high.png",
                                       program_image_scaling=4,
                                       program_image_start_col=8,
                                       program_start_col=7,
                                       report_key=report_key,
                                       data_integrity_key=integrity_key,
                                       reporting_asof_date=reporting_asof_date,
                                       `Source Name`=source_name,
                                       `Source Reference`=source_reference,
                                       `Source Note`=source_note,
                                       `Source Upload Time`=as.character(reporting_time))
  
  if(SYS_PRINT_TIMING)  debugtime("export_reporting_cohort_to_excel","header created with rsf_reports_excel_header()")
  
  START_ROW <- 9
  freport[,`:=`(source_name=NULL,source_reference=NULL,source_note=NULL,reporting_time=NULL,reporting_asof_date=NULL)]

  report <- list()
  
  spacer <- data.frame(V1=NA,V2=NA,V3=NA,V4=NA,V5=NA,V6=NA,V7=NA,V8=NA,V9=NA,V10=NA,V11=NA,V12=NA,V13=0)
  
  
  excel_tables <- list()
  
  #####Created in server_datasets.R but repurposed here.  Function would be better
  cohort_flags <- rbindlist(freport$flags_dt)
  setorder(cohort_flags,data_id,flag_priority,check_name)
  
  if (!empty(cohort_flags)) {
    freport[cohort_flags[check_class=="critical",.(flags_text=gsub("'","&apos;",paste0(flag_text,collapse=" \n"))),
                             by=.(data_id)],
                `:=`(flags_title_critical=i.flags_text),
                on=.(data_id)]
    
    freport[cohort_flags[check_class=="error",.(flags_text=gsub("'","&apos;",paste0(flag_text,collapse=" \n"))),
                         by=.(data_id)],
            `:=`(flags_title_error=i.flags_text),
            on=.(data_id)]    

    freport[cohort_flags[check_class=="warning",.(flags_text=gsub("'","&apos;",paste0(flag_text,collapse=" \n"))),
                         by=.(data_id)],
            `:=`(flags_title_warning=i.flags_text),
            on=.(data_id)]    

    freport[cohort_flags[check_class=="info",.(flags_text=gsub("'","&apos;",paste0(flag_text,collapse=" \n"))),
                         by=.(data_id)],
            `:=`(flags_title_info=i.flags_text),
            on=.(data_id)]
    
    active_flags <- cohort_flags[check_status=="active"]
    
    freport[active_flags[,.(count_active_flags_total=.N),by=.(data_id)],
            count_active_flags_total:=i.count_active_flags_total,
            on=.(data_id)]

    freport[active_flags[check_class=="critical",.(count_active_flags_critical=.N),by=.(data_id)],
            count_active_flags_critical:=i.count_active_flags_critical,
            on=.(data_id)]
    
    freport[active_flags[check_class=="error",.(count_active_flags_error=.N),by=.(data_id)],
            count_active_flags_error:=i.count_active_flags_error,
            on=.(data_id)]

    freport[active_flags[check_class=="warning",.(count_active_flags_warning=.N),by=.(data_id)],
            count_active_flags_warning:=i.count_active_flags_warning,
            on=.(data_id)]
    
    freport[active_flags[check_class=="info",.(count_active_flags_info=.N),by=.(data_id)],
            count_active_flags_info:=i.count_active_flags_info,
            on=.(data_id)]
    
    freport[is.na(count_active_flags_total),count_active_flags_total:=0]

    freport[is.na(count_active_flags_critical),count_active_flags_critical:=0]
    freport[is.na(count_active_flags_error),count_active_flags_error:=0]
    freport[is.na(count_active_flags_warning),count_active_flags_warning:=0]
    freport[is.na(count_active_flags_info),count_active_flags_info:=0]
    
  } else {
    freport[,`:=`(flags_title_critical=NA,
                  count_active_flags_critical=0,
                  flags_title_error=NA,
                  count_active_flags_error=0,
                  flags_title_warning=NA,
                  count_active_flags_warning=0,
                  flags_title_info=NA,
                  count_active_flags_info=0)]
  }
  freport[,flags_text:=paste0(na.omit(c(flags_title_critical,flags_title_error,flags_title_warning,flags_title_info)),collapse="\\n"),
          by=.(data_id)]
  ######
  tab_num <- 1
  categories <- c("program","facility","client","borrower","loan")
  
  for (category in categories) {
    creport <- freport[data_category==category,.(SYSID=rsf_pfcbl_id,
                                                 Type=indicator_type,
                                                 entity_name,
                                                 entity_id,
                                                 Indicator=indicator_name,
                                                 Value=data_value,
                                                 Flags=flags_text,
                                                 Errors=count_active_flags_critical+count_active_flags_error,
                                                 Warnings=count_active_flags_warning,
                                                 Info=count_active_flags_info,
                                                 Total=count_active_flags_total,
                                                 n,
                                                 data_sequence_rank)]
    
    if (nrow(creport)==0) next;
    theader <- ""
    if (category=="program") { names(creport)[which(names(creport) %in% c("entity_name","entity_id"))] <- c("Program Name"," - "); theader <- "RSF Program Data"; }
    if (category=="facility") { names(creport)[which(names(creport) %in% c("entity_name","entity_id"))] <- c("Facility Name","IFC Project ID"); theader <- "IFC Facility Data"; }
    if (category=="client") { names(creport)[which(names(creport) %in% c("entity_name","entity_id"))] <- c("Client Name","IFC Partner ID"); theader <- "Client Data"; }
    if (category=="borrower") { names(creport)[which(names(creport) %in% c("entity_name","entity_id"))] <- c("Borrower Name","Borrower ID"); theader <- "Borrower Data"; } 
    if (category=="loan") { names(creport)[which(names(creport) %in% c("entity_name","entity_id"))] <- c("Inclusion Rank","Internal Loan ID"); theader <- "Lending Data"; }
  
    theader <- data.frame(V1=paste0("Table ",tab_num),
                          V2=theader,
                          V3=NA,V4=NA,V5=NA,V6=NA,V7=NA,V8=NA,V9=NA,V10=NA,V11=NA,V12=NA,V13=0)

    excel_tables[[length(excel_tables)+1]] <- list(theader=theader,tdata=creport)
    
    creport <- rbindlist(list(spacer,theader,as.data.frame(t(data.frame(names(creport)))),creport),use.names = F)
  
    report <- rbind(report,creport)
    tab_num <- tab_num + 1
    
  }
  
  report <- data.table(report)
  header <- as.data.table(matrix(data=NA,nrow=START_ROW-2,ncol=ncol(report)))
  report <- rbindlist(list(header,report))
  
  {
    tableHStyle <- createStyle(fontSize = 14, fontColour = "#FFFFFF", halign = "center", fgFill = "#4F81BD", border="TopBottom", borderColour = "#4F81BD")
    tableHRows <- grep("^Table\\s\\d+$",report[[1]]) #+ START_ROW
  
    tableTHStyle <- createStyle(fontSize = 11, fontColour = "black", textDecoration="bold", halign = "left", fgFill = "gainsboro", border="Bottom", borderColour = "#4F81BD", borderStyle="thin")
    tableTHRows <- tableHRows+1

    tableSysCalcRows <- grep("^SYSTEM$|^CALCULATED$",report[[2]]) #+ START_ROW
    
    tableEvenStyle <- createStyle(fontSize=10, fgFill = "white")
    tableOddStyle <- createStyle(fontSize=10, fgFill = "#e0f0fa")
    tableTRows <- grepl("^\\d+$",report[[1]]) #+ START_ROW
    tableOddRows <-  suppressWarnings(as.numeric(report[["V12"]])) %% 2==1 #n field
    tableEvenRows <- which(tableTRows & !tableOddRows)
    tableOddRows <- which(tableTRows & tableOddRows)
    tableHistoricRows <- which(suppressWarnings(as.numeric(report[["V13"]]) > 1)) ##V13=data_sequence_rank
    
    start_row <- START_ROW
    for (i in 1:length(excel_tables)) {
      etable <- excel_tables[[i]]
      theader <- etable$theader
      tdata <- etable$tdata

      openxlsx::writeData(excelwb,sheet=1,x=theader[,1:11],colNames=FALSE,rowNames=FALSE, startRow=start_row,startCol=1)
      start_row <- start_row + nrow(theader)
      
      openxlsx::writeDataTable(excelwb, sheet=1, x=tdata[,1:11], colNames=TRUE, rowNames=FALSE, startRow = start_row, startCol = 1,tableStyle="none",tableName=gsub("\\s+","",paste0(theader[1,1],'_',theader[1,2])))
      
      start_row <- start_row + nrow(tdata) + 2
    }
    
    
    setColWidths(excelwb,sheet=1,cols=c(1,2,3,4,5,6,7,8,9,10,11),widths=c(10,10,24,15,40,15,50,8,10.5,7,7.5))
    for (r in (tableHRows)) mergeCells(excelwb,sheet=1,rows=r,cols=2:11)
    
    addStyle(excelwb,sheet=1,tableHStyle,rows=tableHRows,cols=1:11,gridExpand = TRUE)
    addStyle(excelwb,sheet=1,tableTHStyle,rows=tableTHRows,cols=1:11,gridExpand = TRUE)
    
    addStyle(excelwb,sheet=1,tableEvenStyle,rows=tableEvenRows,cols=1:11,gridExpand = TRUE)
    addStyle(excelwb,sheet=1,tableOddStyle,rows=tableOddRows,cols=1:11,gridExpand = TRUE)
    addStyle(excelwb,sheet=1,createStyle(border="bottom",borderStyle="thin",borderColour = "gray"),cols=1:11,rows=c(tableOddRows,tableEvenRows),gridExpand=TRUE,stack=TRUE)
    
    addStyle(excelwb,sheet=1,createStyle(fontColour = "gray"),tableSysCalcRows,cols=c(2,6),gridExpand=TRUE,stack=TRUE)
    
    addStyle(excelwb,sheet=1,createStyle(halign = "left"),rows=1:nrow(report),cols=1,gridExpand=TRUE,stack=TRUE)
    addStyle(excelwb,sheet=1,createStyle(halign = "center"),rows=c(1:nrow(report))[-tableTHRows],cols=c(8,9,10,11),gridExpand=TRUE,stack=TRUE) #Center flag counts
    addStyle(excelwb,sheet=1,createStyle(textDecoration = "strikeout",fontColour="#968c5a"),rows=tableHistoricRows,cols=6:11,gridExpand = TRUE,stack = TRUE)
    addStyle(excelwb,sheet=1,createStyle(fontColour = "forestgreen"),rows=tableTHRows,cols=6,gridExpand=TRUE,stack=TRUE)
    addStyle(excelwb,sheet=1,createStyle(fontColour = "red"),rows=tableTHRows,cols=8,gridExpand=TRUE,stack=TRUE)
    addStyle(excelwb,sheet=1,createStyle(fontColour = "orange"),rows=tableTHRows,cols=9,gridExpand=TRUE,stack=TRUE)
    addStyle(excelwb,sheet=1,createStyle(fontColour = "blue"),rows=tableTHRows,cols=10,gridExpand=TRUE,stack=TRUE)
    
    showGridLines(excelwb, sheet=1, showGridLines = FALSE)
    
  }

  if(SYS_PRINT_TIMING)  debugtime("export_reporting_cohort_to_excel","Excel formatted.  Done!")
  
  return (excelwb)
}