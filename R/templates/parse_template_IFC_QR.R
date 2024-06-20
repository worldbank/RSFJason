

parse_template_IFC_QR <- function(pool,
                                  template_lookup,
                                  rsf_program_id,
                                  template_file,
                                  rsf_indicators,
                                  status_message) 
{
  ####
  #openxlsx has some bug where it can't read some types of workbooks with pivot tables
  #https://github.com/ycphs/openxlsx/issues/124
  excelwb <- tryCatch({
    openxlsx::loadWorkbook(template_file)
  },
  error = function(e) { 
    stop(conditionMessage(e))
  },
  warning = function(w) { 
    print(conditionMessage(w))
    suppressWarnings(openxlsx::loadWorkbook(template_file))
  })

  reporting_flags <- data.table(rsf_pfcbl_id=numeric(0),
                                indicator_id=numeric(0),
                                reporting_asof_date=as.Date(numeric(0)),
                                check_name=character(0),
                                check_message=character(0))
  
  reporting_asof_date <- {
    snames <- openxlsx::getSheetNames(file=template_file)
    nregions <- openxlsx::getNamedRegions(excelwb)
  
    summarySheet <- grep("summary",snames,ignore.case=TRUE,value=T)
    dataSheet <-grep("QReport",snames,ignore.case=TRUE,value=T)
  
    #Note: sheets with external references links can create real issues and also generate sheet names that aren't in the workbook.  Eg, "[1]1. Summary"
    
    nregions_date <- which(nregions %in% c("S_DET","S_QDD"))
    nregions_locations <- attr(nregions,"position")[nregions_date]
    nregions_sheets <- attr(nregions,"sheet")[nregions_date]
    nregion_cols <- convertFromExcelRef(nregions_locations)
    nregion_rows <- as.numeric(gsub("[^0-9.]","",nregions_locations))
    nregion_coords <- data.frame(col=nregion_cols,row=nregion_rows,sheet=nregions_sheets)
    
    if (any(!nregion_coords$sheet %in% snames)) {
      status_message(class="error","Workbook contains references to Worksheets that do not exists. Check for external links and use Break Links if these exist.\n")
      status_message(class="error","Important: Workbook may contain links in the 'Name Manager'.  These must be manually deleted: type Ctrl+F3 (or Ctrl+Fn+F3) or navigate to Formulas -> Name Manager\n")
      status_message(class="info","Attempting to fix sheet....If fix is successful, script will proceed. Otherwise, break links is required.\n")
      
  
      nregion_coords <- nregion_coords[nregion_coords$sheet %in% snames,]
    }
    
    dates <- sapply(1:nrow(nregion_coords),
                    function(i) openxlsx::readWorkbook(xlsxFile=excelwb,
                                                       sheet=nregion_coords$sheet[i],
                                                       rows=nregion_coords$row[i],
                                                       cols=nregion_coords$col[i]))
    dates <- names(dates)
    
    
    reporting_asof_date <- suppressWarnings(na.omit(as.numeric(dates)))
    
    if (is.null(reporting_asof_date) || length(reporting_asof_date) !=1) {
      status_message(class="error","Error: Unable to read namedRegion S_DET or S_QDD.  Verify that workbook is not using links to external files in its name regions and/or that links are to Remote Files on Sharepoint that require sign-in to update.  Workbook should not contian external links.")
      status_message(class="info",paste0("Check sheet=",unique(nregions_sheets)," Cells=",paste0(nregions_locations,collapse=",")))
      stop("Error: Unable to read namedRegion S_DET or S_QDD.  Verify that workbook is not using links to external files in its name regions and/or that links are to Remote Files on Sharepoint that require sign-in to update.  Workbook should not contian external links.")
    }
  
    reporting_asof_date <- openxlsx::convertToDate(x=reporting_asof_date,
                                                   origin=as.Date(openxlsx::getDateOrigin(template_file)))
    
    reporting_asof_date <- unique(reporting_asof_date)
    if (length(reporting_asof_date) != 1) {
      status_message(class="error",paste0("Error: unable to read unique template reporting date from named regions S_DET and S_QDD.\n"))
      status_message(class="info",paste0("Check sheet=",unique(nregions_sheets)," Cells=",paste0(nregions_locations,collapse=",")," and Named Regions: ensure names S_DET and S_QDD are specified only once, not multiple times."))
      stop("Error: unable to read unique template reporting date from named regions S_DET and S_QDD.")
    }
    reporting_asof_date
  }
 
  #Load the data sheets (and their formulas)  
  {
    {
      summary_sheet <- template_excel_read_sheet(excelwb=excelwb,
                                                 sheetName=summarySheet)
    
      summary_formula_matrix <- openxlsx_get_formulas(excelwb=excelwb,
                                                      sheetName=summarySheet,
                                                      truncate_predata_rows = TRUE)
      setDT(summary_sheet)
      summary_sheet[,original_row_num:=.I]
    }
    
    {
      data_sheet <- template_excel_read_sheet(excelwb=excelwb,
                                              sheetName=dataSheet)
      
      data_formula_matrix <- openxlsx_get_formulas(excelwb=excelwb,
                                                   sheetName=dataSheet,
                                                   truncate_predata_rows = TRUE)
      setDT(data_sheet)
    }
  }
  
  rsf_pfcbl_id.facility <- {
    
    
    id_expr <- "^.*#[[:space:]]*(\\d+).*$|^.*[[:space:]]+(\\d+)[[:punct:]]?$"
    name_id_expr <- "^[[:punct:]]?([[:alpha:][:space:]-]+)[[:punct:][:space:]]+(\\d+).*$"
    project_name <- NULL
    project_id <- NULL
    id_data <- NULL
    
    for (r in 1:nrow(summary_sheet)) {
      id_cols <- which(grepl(name_id_expr,summary_sheet[r]) &  grepl(id_expr,summary_sheet[r]))
      if (length(id_cols)==2) {
        id_cols <- names(summary_sheet)[id_cols]
        id_data <- summary_sheet[r,..id_cols]
        project_name <- trimws(gsub(name_id_expr,"\\1",id_data),whitespace="[ \\t\\r\\n\\v\\h\\s]")
        project_id <- suppressWarnings(as.numeric(gsub(name_id_expr,"\\2",id_data)))  
        break;
      } else {
        next;
      }
    }
    project_name <- unique(project_name)
    project_id <- as.numeric(unique(project_id))
    
    if (length(project_name) != 1 ||
        length(project_id) != 1) {
      if (is.null(id_data)) stop("Failed to find Project Name and ID in Summary tab.  Ensure this information is present and properly formatted.")
      else stop(paste0("Ambiguous project ID and name found on Summary tab: found: ",paste0(id_data,collapse=" [AND] ")))
    }

    rsf_pfcbl_id <- dbGetQuery(pool,"
                                        select distinct
                                        ids.rsf_pfcbl_id
                                    from p_rsf.rsf_pfcbl_ids ids
                                    inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
                                    inner join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id
                                                                         and rdc.indicator_id = ind.indicator_id
                                    where ids.rsf_program_id = $1::int
                                      and ids.pfcbl_category = 'facility'
                                      and ind.indicator_sys_category = 'id'
                                      and rdc.data_value = $2::text
                                    ",
                                params=list(rsf_program_id,
                                            project_id))
    if (nrow(rsf_pfcbl_id) != 1) {

        stop(paste0("Failed to uniquely match IFC Project ID number using value specified by template: ",
                    project_name," #",project_id," for specified RSF Program ID #",rsf_program_id))
    }
    
    #We know that name and ID info is at the top and we've now parsed it, so discard and focus on what's next
    summary_sheet <- summary_sheet[-(1:r)]
    as.numeric(rsf_pfcbl_id$rsf_pfcbl_id)
  }
  
  #rsf_indicators <- db_indicators_get_labels(pool)
  #labels, including facility-specific label mappings
  {
    rsf_labels <- rbindlist(rsf_indicators$labels)
    rsf_labels <- unique(rsf_labels[,.(indicator_id,indicator_header_id,label_normalized)])
    
    rsf_labels[,
               `:=`(template_header_sheet_name=as.character(NA),
                    action="remap",
                    template_header_encounter_index=0,
                    template_header_position=as.numeric(NA))]
    
    header_actions <- dbGetQuery(pool,"
      select
        tha.rsf_pfcbl_id,
        tha.template_id,
        tha.header_id,
        tha.indicator_header_id,
        tha.template_header,
        tha.template_header_sheet_name,
        tha.template_header_encounter_index,
        tha.action,
        tha.remap_header,
        tha.remap_indicator_id,
        tha.action_level
      from p_rsf.view_rsf_program_facility_template_header_actions tha
      where tha.rsf_pfcbl_id = $1::int
        and tha.template_id = $2::int
      order by header_id desc",
      params=list(rsf_pfcbl_id.facility,
                  template_lookup$template_id))
    
    setDT(header_actions)
    
    header_actions[,template_header_position:=as.numeric(NA)]
    
    #default allows facilities to overwrite program-level setups, for example.
    header_actions <- header_actions[action != "default"]
    
    if (any(grepl("&&",header_actions$template_header))) {
      grouped_header_actions <- header_actions[grepl("&&",template_header),
                                               .(ungrouped_header=unlist(strsplit(template_header,split="[[:space:]]+&&[[:space:]]+",fixed=F),recursive=F)),
                                               by=.(indicator_header_id)]
      
      header_actions <- grouped_header_actions[header_actions,
                                               on=.(indicator_header_id),
                                               nomatch=NA]
      header_actions[!is.na(ungrouped_header),
                     template_header:=ungrouped_header]
      
      header_actions[!is.na(ungrouped_header),
                     template_header_position:=1:.N,
                     by=.(indicator_header_id)]
      header_actions[,
                     ungrouped_header:=NULL]
    }
    
    header_actions[,
                   label_normalized:=normalizeLabel(template_header)]
    
    # header_actions[label_normalized=="na",
    #                label_normalized:=as.character(NA)]
    
    setnames(header_actions,
             old="remap_indicator_id",
             new="indicator_id")
    
    header_actions <- header_actions[,
                                     .SD,
                                     .SDcols = names(rsf_labels)]
    
    rsf_labels <- rsf_labels[!(label_normalized %in% header_actions[is.na(template_header_sheet_name),label_normalized])]
    rsf_labels <- rbindlist(list(rsf_labels,
                                 header_actions))
    rsf_labels[,joincondition:=as.character(NA)]
    setorder(rsf_labels,
             indicator_header_id,
             template_header_position,
             template_header_encounter_index,
             na.last = TRUE)
  }
  
  data.summary <- {
    
    #Basic filtering and normalizng
    {  
      label_matches <- sapply(summary_sheet,
                           function(dcol) {
                             which(normalizeLabel(dcol) %in% rsf_labels$label_normalized)
                           },USE.NAMES = TRUE)
      
      label_cols <- sapply(label_matches,
                           length,
                           USE.NAMES = TRUE)
      
      if (length(label_cols[label_cols > 0]) < 2) stop("Failed to find bi-lingual indicator labels in the template (ie, two columns of labels in the Summary tab)")
      
      label_cols <- sort(label_cols,decreasing =TRUE)[1:2]
      label_cols_index <- which(names(label_matches) %in% names(label_cols))
      
      if (label_cols_index[2]-label_cols_index[1] != 1) {
        stop(paste0("Label columns are not adjacent.  Template seems to have labels in columns ",paste0(label_cols_index,collapse=" and ")," instead of next to each other?"))
      }
      
      label_cols_names <- names(label_matches)[label_cols_index]
      label_matches <- label_matches[label_cols_index]
      
      data_cols <- names(summary_sheet)[seq(from=min(label_cols_index),length.out=4)]
      data_cols <- c(data_cols,"original_row_num")
      
      formula_cols <- names(summary_formula_matrix)[seq(from=min(label_cols_index),length.out=4)]
      summary_formula_matrix <- summary_formula_matrix[,formula_cols] #df not dt
      
      summary_sheet <- summary_sheet[,
                                     ..data_cols]
      
      summary_sheet <- setnames(summary_sheet,
                                old=names(summary_sheet),
                                new=c("label1",
                                      "label2",
                                      "data_unit",
                                      "data_value",
                                      "original_row_num"))
     
      summary_sheet[,data_column_num:=4]
      
      summary_sheet[,data_unit:=superTrim(data_unit,
                                          to.lower.case=FALSE,
                                          empty.is.NA=TRUE)]
      
      summary_sheet[,data_value:=superTrim(data_value,
                                           to.lower.case=FALSE,
                                           empty.is.NA=TRUE)]
  
      all_blanks <- which(sapply(as.data.frame(is.na(t(summary_sheet))),all))
      if (any(all_blanks)) summary_sheet <- summary_sheet[-all_blanks]
      
      no_labels <- which(sapply(as.data.frame(is.na(t(summary_sheet[,.(label1,label2)]))),all))
      if (any(no_labels)) summary_sheet <- summary_sheet[-no_labels]
      
      #data unit OR data value in case values are put in units column
      one_label_no_data <- which(sapply(as.data.frame(is.na(t(summary_sheet[,.(label1,label2)]))),any) &
                                 sapply(as.data.frame(is.na(t(summary_sheet[,.(data_unit,data_value)]))),all))
      if (any(one_label_no_data)) summary_sheet <- summary_sheet[-one_label_no_data]
      
      #Data values are reported in the data units column (and data "values" are themselves blank.  Ie, we've imported only units.  So interpret as values without units)
      summary_sheet[is.na(data_value) & 
                    !is.na(data_unit),
                    `:=`(data_value=data_unit,
                         data_unit=as.character(NA),
                         data_column_num=3)]
  
      summary_sheet <- melt.data.table(summary_sheet,
                                        id=c("original_row_num","data_column_num","data_value","data_unit"),
                                        variable.factor = F,
                                        variable.name="label_position",
                                        value.factor=F,
                                        value.name="label")
      
      summary_sheet[,
                     label_position:=as.numeric(gsub("label","",label_position))]
      
      summary_sheet[,label_normalized:=normalizeLabel(label)]
      
      summary_sheet[is.na(label),
                    label_normalized:=normalizeLabel("NA")]
      
      summary_sheet[,
                     `:=`(indicator_id=as.numeric(NA),
                          action=as.character(NA))]
      
      summary_sheet[,
                     header_encounter_index:=(1:.N)-ifelse(.N > 1,0,1),
                     by=.(label_normalized,
                          label_position)]
      
      summary_sheet[is.na(label_normalized),
                    header_encounter_index:=0]
      
      setorder(summary_sheet,
               original_row_num,
               label_position,
               header_encounter_index,
               na.last = TRUE)
    }
    
    #label matching!!
    {
      #match by exact position (all matches) and also exact encounter index
      {
      position_matches <- summary_sheet[is.na(action) &
                                          header_encounter_index > 0,
                                        .(label_normalized=paste0(label_normalized,collapse=" && ")),
                                        by=.(original_row_num,header_encounter_index)
                          ][rsf_labels[grepl("Summary",template_header_sheet_name,ignore.case=T) & 
                                         is.na(template_header_position)==FALSE &
                                         template_header_encounter_index > 0,
                                       .(label_normalized=paste0(label_normalized,collapse=" && ")),
                                       by=.(action,
                                            indicator_id,
                                            indicator_header_id,
                                            template_header_encounter_index)],
                            on=.(label_normalized,header_encounter_index=template_header_encounter_index),
                            nomatch=NULL
                            ][,
                              .(action,
                                indicator_id,
                                exact_match=nrow(unique(.SD))==1),
                              by=.(original_row_num),
                              .SDcols=c("action","indicator_id")
                              ][exact_match==TRUE]
      
      summary_sheet[position_matches,
                     `:=`(indicator_id=i.indicator_id,
                          action=i.action),
                     on=.(original_row_num)]
      }    
      
      #now without encounter index
      {
      position_matches <- summary_sheet[is.na(action),
                                        .(label_normalized=paste0(label_normalized,collapse=" && ")),
                                        by=.(original_row_num)
                          ][rsf_labels[grepl("Summary",template_header_sheet_name,ignore.case=T) & 
                                       is.na(template_header_position)==FALSE,
                                       .(label_normalized=paste0(label_normalized,collapse=" && ")),
                                       by=.(action,
                                            indicator_id,
                                            indicator_header_id)],
                            on=.(label_normalized),
                            nomatch=NULL
                            ][,
                              .(action,
                                indicator_id,
                                exact_match=nrow(unique(.SD))==1),
                              by=.(original_row_num),
                              .SDcols=c("action","indicator_id")
                              ][exact_match==TRUE]
      
      
      summary_sheet[position_matches,
                    `:=`(indicator_id=i.indicator_id,
                         action=i.action),
                    on=.(original_row_num)]
      }
  
      #now only with without encounter index==0
      {
        position_matches <- summary_sheet[is.na(action),
                                          .(label_normalized=paste0(label_normalized,collapse=" && ")),
                                          by=.(original_row_num)
        ][rsf_labels[grepl("Summary",template_header_sheet_name,ignore.case=T) & 
                       is.na(template_header_position)==FALSE &
                       template_header_encounter_index == 0,
                     .(label_normalized=paste0(label_normalized,collapse=" && ")),
                     by=.(action,
                          indicator_id,
                          indicator_header_id)],
          on=.(label_normalized),
          nomatch=NULL
        ][,
          .(action,
            indicator_id,
            exact_match=nrow(unique(.SD))==1),
          by=.(original_row_num),
          .SDcols=c("action","indicator_id")
        ][exact_match==TRUE]
        
        summary_sheet[position_matches,
                      `:=`(indicator_id=i.indicator_id,
                           action=i.action),
                      on=.(original_row_num)]
      }
      
      #now by implied unit
      {
        unit_matches <- summary_sheet[is.na(action),
                                      .(label_normalized=normalizeLabel(paste0(label_normalized," ",data_unit)),
                                        original_row_num)
        ][rsf_labels[,
                     .(label_normalized,
                       action,
                       indicator_id,
                       indicator_header_id)],
          on=.(label_normalized),
          nomatch=NULL
        ][,
          .(action,
            indicator_id,
            exact_match=nrow(unique(.SD))==1),
          by=.(original_row_num),
          .SDcols=c("action","indicator_id")
        ][exact_match==TRUE,
          unique(.SD)] #Exact matches by BOTH columns because there is only one unit column that should apply for both text labels
        
        summary_sheet[unit_matches,
                      `:=`(indicator_id=i.indicator_id,
                           action=i.action),
                      on=.(original_row_num)]
      }
      
      {
      #By sheet name and encounter index
      summary_sheet[rsf_labels[grepl("Summary",template_header_sheet_name,ignore.case=T) &
                               is.na(template_header_position)==TRUE], #because position info has already been matched
                     `:=`(indicator_id=i.indicator_id,
                          action=i.action),
                     on=.(label_normalized,
                          header_encounter_index=template_header_encounter_index,
                          action=joincondition)]
  
      #By default and encounter index
      summary_sheet[rsf_labels[is.na(template_header_position)==TRUE],
                     `:=`(indicator_id=i.indicator_id,
                          action=i.action),
                     on=.(label_normalized,
                          header_encounter_index=template_header_encounter_index,
                          action=joincondition)]
      
      summary_sheet[!is.na(data_unit),
                    label_normalized_unit:=paste0(label_normalized,data_unit)]
      
      summary_sheet[rsf_labels[template_header_encounter_index==0 &
                               is.na(template_header_position)==TRUE],
                    `:=`(indicator_id=i.indicator_id,
                         action=i.action),
                    on=.(label_normalized_unit=label_normalized,
                         action=joincondition)]
      
      summary_sheet[,
                    label_normalized_unit:=NULL]
      
      #By default without index (sometimes indicators can be repeated)
      #zero encounter index will be the first/default
      summary_sheet[rsf_labels[template_header_encounter_index==0 &
                               is.na(template_header_position)==TRUE],
                    `:=`(indicator_id=i.indicator_id,
                         action=i.action),
                    on=.(label_normalized,
                         action=joincondition)]
      
      summary_sheet[rsf_indicators,
                     indicator_name:=i.indicator_name,
                     on=.(indicator_id)]
      
      #Modify labels for error messages
      summary_sheet[,
                     label:=trimws(label)]
      
      summary_sheet[header_encounter_index > 0,
                     label:=paste0(label," [",header_encounter_index,"]")]
      }    
  
    }
    ########################################
    
    #Label errors/mismatching
    {
      summary_sheet[,
                     ignore:=anyNA(action)==FALSE & all(action=="ignore"),
                     by=.(original_row_num)]
      
      #summary_sheet[ignore==TRUE]
      summary_sheet <- summary_sheet[ignore==FALSE]
      
      #Will fail because its ambiguous
      {
        summary_sheet[,
                       mismatch:=anyNA(indicator_id)==FALSE & length(unique(indicator_id))>1,
                       by=.(original_row_num)]
        
        mismatch_labels <- summary_sheet[mismatch==TRUE]
        if (!empty(mismatch_labels)) {
          
         
          mismatch_labels[,
                          message:=paste0("Summary Sheet Row ",original_row_num," column ",label_position," \"",label,"\" maps to \"",indicator_name,"\"")]
          setorder(mismatch_labels,
                   original_row_num,
                   label_position)
          
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
                       notfound:=all(is.na(indicator_id)) & !any(label_normalized=="na"),
                       by=.(original_row_num)]
        unfound_labels <- summary_sheet[notfound==TRUE]
        
        if (!empty(unfound_labels)) {
          
          setorder(unfound_labels,
                   original_row_num,
                   label_position)
          
          unfound_labels[,
                         `:=`(rsf_pfcbl_id=rsf_pfcbl_id.facility,
                              indicator_id=as.numeric(NA), #will be auto-assigned to reporting indicator
                              reporting_asof_date=reporting_asof_date,
                              check_name="sys_flag_indicator_not_found",
                              check_message=paste0("Summary Tab Column ",label_position," Row ",original_row_num," \"",label,"\""))]
          
          unfound_labels <- unfound_labels[,.(rsf_pfcbl_id,
                                              indicator_id,
                                              reporting_asof_date,
                                              check_name,
                                              check_message)]
          
          reporting_flags <- rbindlist(list(reporting_flags,
                                            unfound_labels))
        }
      }
      
      #Untranslated: will assign to the indicator that did have the match
      {
        summary_sheet[,
                       untranslated:=anyNA(indicator_id) & !all(is.na(indicator_id)),
                       by=.(original_row_num)]
        
        untranslated_labels <- summary_sheet[untranslated==TRUE]
        untranslated_labels <- untranslated_labels[is.na(indicator_id) & (is.na(action) | action != "ignore")]
        
        if (!empty(untranslated_labels)) {
          setorder(untranslated_labels,
                   original_row_num,
                   label_position)
          
          untranslated_labels[summary_sheet[is.na(indicator_id)==FALSE],
                              matched_message:=paste0(i.indicator_name," matched using label #",i.label_position," '",i.label,"'"),
                              on=.(original_row_num)]
          
          untranslated_labels[summary_sheet[is.na(indicator_id)==TRUE],
                              matched_message:=paste0(matched_message," but failed to identify label #",i.label_position," '",i.label,"'"),
                              on=.(original_row_num)]
          
          untranslated_labels[nchar(label_normalized)==0,
                              label:="{BLANK}"]
          
          untranslated_labels[,
                              `:=`(rsf_pfcbl_id=rsf_pfcbl_id.facility,
                                   reporting_asof_date=reporting_asof_date,
                                   check_name="sys_flag_indicator_not_found",
                                   check_message=paste0("Summary Tab Column ",label_position," Row ",original_row_num," \"",label,"\" (",matched_message,")"))]
          
          untranslated_labels <- untranslated_labels[is.na(indicator_id),
                                                     .(rsf_pfcbl_id,
                                                        indicator_id,
                                                        reporting_asof_date,
                                                        check_name,
                                                        check_message)]
          
          reporting_flags <- rbindlist(list(reporting_flags,
                                            untranslated_labels))
        }
      }
      
      summary_sheet <- summary_sheet[is.na(indicator_id)==FALSE]
      
      
      
      summary_sheet[,
                     ambiguous:= nrow(unique(.SD)) > 1,
                     by=.(indicator_id),
                     .SDcols = c("data_value","data_unit")]
     
      ambiguities <- summary_sheet[ambiguous==TRUE]
      if (!empty(ambiguities)) {
        setorder(ambiguities,
                 indicator_name,
                 original_row_num,
                 label_position)
        
        #Sometimes the ambiguity is caused by some rows reporting data units and others leaving them blank.  Determine if/where this is the case
        ambiguities[,
                    ambiguous:= length(na.omit(unique(data_value))) > 1 |
                                length(na.omit(unique(data_unit))) > 1,
                    by=.(indicator_id)]
        
        
        
        unambiguous_units <- ambiguities[ambiguous==FALSE,
                                         .(data_value=sort(data_value,na.last=TRUE)[1],
                                           data_unit=sort(data_unit,na.last=TRUE)[1]),
                                         by=.(indicator_id)]
        
        ambiguities <- ambiguities[ambiguous==TRUE]
        
        if (!empty(unambiguous_units)) {
          summary_sheet[unambiguous_units,
                        `:=`(data_value=i.data_value,
                             data_unit=i.data_unit),
                        on=.(indicator_id)]
        }
        
        if (!empty(ambiguities)) {
          ambiguities[,
                      data_unit:=ifelse(is.na(data_unit) | nchar(data_unit)==0,
                                        "",
                                        paste0(" ",toupper(data_unit)))]
          
          ambiguities <- ambiguities[,
                                     .(label=paste0(label,collapse=" && "),
                                       data_value=paste0(unique(paste0(as.character(data_value),data_unit)),collapse=" && ")),
                                     by=.(original_row_num,indicator_name)]
          
          ambiguities <- ambiguities[,
                      .(message=paste0("AMBIGUOUS REPORTING: ",
                                       paste0(
                                         paste0("Summary tab row ",original_row_num," ",indicator_name," (",label,") IS ",data_value),
                                         collapse=" [AND] "))),
                      by=.(indicator_name)]
        
          stop(paste0(ambiguities$message,collapse=" \n"))  
        }
      }
  
      #special management for currency ratios and if the currency code is being reported in the label
      {
        currency_reporting <- summary_sheet[rsf_indicators[data_type=="currency_ratio",.(indicator_id,default_data_unit=data_unit)],
                                            on=.(indicator_id),
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
                                                          indicator_id,
                                                          label_data_unit)])
        summary_sheet[currency_reporting,
                      data_unit:=i.label_data_unit,
                      on=.(original_row_num,
                           indicator_id)]
      }
      
      summary_sheet <- summary_sheet[is.na(indicator_id)==FALSE,
                                       .(indicator_name,
                                         data_unit,
                                         data_value,
                                         original_row_num,
                                         data_column_num)]
    }
    
    summary_sheet <- unique(summary_sheet)
    
    #If the data unit is a formula, it's probably just lazy data entry.  Make the unit equal to another cell's unit instead of retyping.
    #Unlikley that the unit is actually a calculated result.
    summary_sheet[,
                  data_formula:=mapply(function(i,j,x) { x[i,j] },
                                       i=original_row_num,
                                       j=data_column_num,
                                       MoreArgs=list(x=summary_formula_matrix),
                                       SIMPLIFY = TRUE)]
    
    summary_sheet[,
                  reporting_template_row_group:='1SUMMARY']
    
    setnames(summary_sheet,
             old=c("data_unit","data_value","data_formula"),
             new=c("reporting_submitted_data_unit",
                   "reporting_submitted_data_value",
                   "reporting_submitted_data_formula"))
    
    summary_sheet <- summary_sheet[,
                                   .(reporting_template_row_group,
                                     indicator_name,
                                     reporting_submitted_data_value,
                                     reporting_submitted_data_unit,
                                     reporting_submitted_data_formula)]
    summary_sheet
    
  }

  data.quarterly <- {
    
    label_matches <- sapply(as.data.frame(t(data_sheet[1:30])), #headers should always be in the top 10ish rows...30 way more than enough search
                            function(dcol) {
                              which(normalizeLabel(dcol) %in% rsf_labels$label_normalized)
                            },USE.NAMES = TRUE)
    
    label_rows <- sapply(label_matches,
                         length,
                         USE.NAMES = TRUE)
    
    if (length(label_rows[label_rows > 0]) < 2) stop("Failed to find bi-lingual indicator labels in the template (ie, two columns of labels in the QReport tab)")
    
    label_rows <- sort(label_rows,decreasing =TRUE)[1:2]
    label_rows_index <- which(names(label_matches) %in% names(label_rows))
    
    data_labels <- data_sheet[label_rows_index]
    label_cols_index <- which(!sapply(as.data.frame(is.na(data_labels)),all))

    data_rows <- seq(from=max(label_rows_index)+1,
                     to=nrow(data_sheet))
    
    data_sheet <- data_sheet[data_rows]
    data_formulas <- as.data.table(data_formula_matrix[data_rows,
                                                       1:ncol(data_sheet)])
    setnames(data_formulas,
             old=names(data_formulas),
             new=names(data_sheet))
    
    #data labels
    {
      data_cols_names <- names(label_cols_index)
      
      data_labels <- data_labels[,
                                 ..data_cols_names]
      
      data_labels <- as.data.table(as.data.frame(t(data_labels)),
                                   keep.rownames = T)
      
      setnames(data_labels,
               old=c("V1","V2"),
               new=c("label1","label2"))

      data_labels <- melt.data.table(data_labels,
                           id="rn",
                           variable.factor = F,
                           value.factor = F,
                           variable.name = "label_position",
                           value.name="label")
      
      data_labels[,
                   original_col_num:=openxlsx::int2col(as.numeric(gsub("\\D","",rn)))]
      
      data_labels[,
                   label_position:=as.numeric(gsub("label","",label_position))]
      
      data_labels[,label_normalized:=normalizeLabel(label)]
      
      data_labels[is.na(label),
                  label_normalized:=normalizeLabel("NA")]
      
      data_labels[,
                    `:=`(indicator_id=as.numeric(NA),
                         action=as.character(NA))]
      
      data_labels[,
                    header_encounter_index:=(1:.N)-ifelse(.N > 1,0,1),
                    by=.(label_normalized,
                         label_position)]
      
      
      {
        #match by exact position (all matches) and also exact encounter index
        position_matches <- data_labels[is.na(action) &
                                        header_encounter_index > 0,
                                      .(label_normalized=paste0(label_normalized,collapse=" && ")),
                                        by=.(original_col_num,header_encounter_index)
                            ][rsf_labels[grepl("QReport",template_header_sheet_name,ignore.case=T) & 
                                           is.na(template_header_position)==FALSE &
                                           template_header_encounter_index > 0,
                                         .(label_normalized=paste0(label_normalized,collapse=" && ")),
                                         by=.(action,
                                              indicator_id,
                                              indicator_header_id,
                                              template_header_encounter_index)],
                              on=.(label_normalized,header_encounter_index=template_header_encounter_index),
                              nomatch=NULL
                            ][,
                              .(action,
                                indicator_id,
                                exact_match=.N==1),
                              by=.(original_col_num)
                            ][exact_match==TRUE]
        
        data_labels[position_matches,
                      `:=`(indicator_id=i.indicator_id,
                           action=i.action),
                      on=.(original_col_num)]
        
        #now without encounter index
        position_matches <- data_labels[is.na(action),
                                        .(label_normalized=paste0(label_normalized,collapse=" && ")),
                                          by=.(original_col_num)
                            ][rsf_labels[grepl("QReport",template_header_sheet_name,ignore.case=T) & 
                                           is.na(template_header_position)==FALSE,
                                         .(label_normalized=paste0(label_normalized,collapse=" && ")),
                                         by=.(action,
                                              indicator_id,
                                              indicator_header_id)],
                              on=.(label_normalized),
                              nomatch=NULL
                            ][,
                              .(action,
                                indicator_id,
                                exact_match=.N==1),
                              by=.(original_col_num)
                            ][exact_match==TRUE]
        
        data_labels[position_matches,
                    `:=`(indicator_id=i.indicator_id,
                         action=i.action),
                    on=.(original_col_num)]
        
        
        #By sheet name and encounter index
        data_labels[rsf_labels[grepl("QReport",template_header_sheet_name,ignore.case=T) &
                               is.na(template_header_position)==TRUE],
                      `:=`(indicator_id=i.indicator_id,
                           action=i.action),
                      on=.(label_normalized,
                           header_encounter_index=template_header_encounter_index,
                           action=joincondition)]
        
        #By default and encounter index
        data_labels[rsf_labels[is.na(template_header_position)==TRUE],
                      `:=`(indicator_id=i.indicator_id,
                           action=i.action),
                      on=.(label_normalized,
                           header_encounter_index=template_header_encounter_index,
                           action=joincondition)]
        
        #By default without index (sometimes indicators can be repeated)
        #zero encounter index will be the first/default
        data_labels[rsf_labels[template_header_encounter_index==0 &
                               is.na(template_header_position)==TRUE],
                      `:=`(indicator_id=i.indicator_id,
                           action=i.action),
                      on=.(label_normalized,
                           action=joincondition)]
      }
      
      data_labels[rsf_indicators,
                  indicator_name:=i.indicator_name,
                  on=.(indicator_id)]
      
      #Modify labels for error messages
      data_labels[,
                    label:=trimws(label)]
      
      data_labels[header_encounter_index > 0,
                  label:=paste0(label," [",header_encounter_index,"]")]
      

      data_labels[,
                  ignore:=anyNA(action)==FALSE & all(action=="ignore"),
                  by=.(original_col_num)]
      
      data_labels <- data_labels[ignore==FALSE]
      
      #Will fail because its ambiguous
      {
        data_labels[,
                      mismatch:=anyNA(indicator_id)==FALSE & length(unique(indicator_id))>1,
                      by=.(original_col_num)]
        
        mismatch_labels <- data_labels[mismatch==TRUE]
        if (!empty(mismatch_labels)) {
          
          
          mismatch_labels[,
                          message:=paste0("QReport Sheet row ",label_position," column ",original_col_num," \"",label,"\" maps to \"",indicator_name,"\"")]
          
          setorder(mismatch_labels,
                   original_col_num,
                   label_position)
          
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
                      notfound:=all(is.na(indicator_id)),
                      by=.(original_col_num)]
        
        unfound_labels <- data_labels[notfound==TRUE]
        
        if (!empty(unfound_labels)) {
          
          setorder(unfound_labels,
                   original_col_num,
                   label_position)
          
          unfound_labels[,
                         `:=`(rsf_pfcbl_id=rsf_pfcbl_id.facility,
                              indicator_id=as.numeric(NA), #will be auto-assigned to reporting indicator
                              reporting_asof_date=reporting_asof_date,
                              check_name="sys_flag_indicator_not_found",
                              check_message=paste0("QReport Tab row ",label_position," column ",original_col_num," \"",label,"\""))]
          
          unfound_labels <- unfound_labels[,.(rsf_pfcbl_id,
                                              indicator_id,
                                              reporting_asof_date,
                                              check_name,
                                              check_message)]
          
          reporting_flags <- rbindlist(list(reporting_flags,
                                            unfound_labels))
        }
      }
      
      #Untranslated: will assign to the indicator that did have the match
      {
        data_labels[,
                      untranslated:=anyNA(indicator_id) & !all(is.na(indicator_id)),
                      by=.(original_col_num)]
        
        untranslated_labels <- data_labels[untranslated==TRUE]
        untranslated_labels <- untranslated_labels[is.na(indicator_id) & (is.na(action) | action != "ignore")]
        
        if (!empty(untranslated_labels)) {
          setorder(untranslated_labels,
                   original_col_num,
                   label_position)
          
          
          untranslated_labels[data_labels[is.na(indicator_id)==FALSE],
                              matched_message:=paste0(i.indicator_name," matched label #",label_position),
                              on=.(original_col_num)]
          
          untranslated_labels[,
                              `:=`(rsf_pfcbl_id=rsf_pfcbl_id.facility,
                                   reporting_asof_date=reporting_asof_date,
                                   check_name="sys_flag_indicator_not_found",
                                   check_message=paste0("QReport Tab row ",label_position," column ",original_col_num," \"",label,"\" (",matched_message,")"))]
          
          untranslated_labels <- untranslated_labels[is.na(indicator_id),
                                                     .(rsf_pfcbl_id,
                                                       indicator_id,
                                                       reporting_asof_date,
                                                       check_name,
                                                       check_message)]
          
          
          reporting_flags <- rbindlist(list(reporting_flags,
                                            untranslated_labels))
        }
      }
      
      #data_labels <- data_labels[is.na(indicator_id)==FALSE]
      
      
      data_labels <- data_labels[is.na(indicator_id)==FALSE,
                                 .(rn,
                                   indicator_name)]
      
      data_labels <- unique(data_labels)
      
  
    }
    
    data_cols_names <- data_labels$rn
    data_cols_index <- which(names(data_sheet) %in% data_cols_names)
    
    data_sheet <- data_sheet[,
                             ..data_cols_names]
    
    data_formulas <- data_formulas[,
                                   ..data_cols_names]
    
    setnames(data_sheet,
             old=data_cols_names,
             new=data_labels$indicator_name)
    
    setnames(data_formulas,
             old=data_cols_names,
             new=data_labels$indicator_name)
    
    
    data_sheet[,
               reporting_template_row_group:=paste0(1:.N,"QREPORT")] #Fundamental to keep the original row number/order intact since QR template is columnar data.
    
    data_formulas[,
                  reporting_template_row_group:=paste0(1:.N,"QREPORT")]
    

    data_sheet <- melt.data.table(data_sheet,
                                  id="reporting_template_row_group",
                                  variable.name="indicator_name",
                                  variable.factor=FALSE,
                                  value.factor = FALSE,
                                  value.name="reporting_submitted_data_value")
    
    data_formulas <- melt.data.table(data_formulas,
                                     id="reporting_template_row_group",
                                     variable.name="indicator_name",
                                     variable.factor=FALSE,
                                     value.factor = FALSE,
                                     value.name="reporting_submitted_data_formula")
    
    data_sheet[data_formulas,
               reporting_submitted_data_formula:=i.reporting_submitted_data_formula,
               on=.(reporting_template_row_group,
                    indicator_name)]
    
    empty_rows <- data_sheet[,
                            .(blank=all(is.na(reporting_submitted_data_value) | nchar(reporting_submitted_data_value)==0)),
                            by=.(reporting_template_row_group)][blank==T,reporting_template_row_group]
    
    data_sheet <- data_sheet[!(reporting_template_row_group %in% empty_rows)]
    
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
    
    duplicates <- data_sheet[,
                             .(n=length(unique(reporting_submitted_data_value)),
                               values=list(reporting_submitted_data_value)),
                             by=.(indicator_name,
                                  reporting_template_row_group)][n>1]
    if (!empty(duplicates)) {
      
      duplicates <- duplicates[,
                              .(data_value=unlist(values,recursive=F)),
                              by=.(indicator_name,
                                   reporting_template_row_group)]
      
      duplicates[,
                 encounter_number:=1:.N,
                 by=.(indicator_name,
                      reporting_template_row_group)]
      
      data_labels[,
                  encounter_number:=1:.N,
                  by=.(indicator_name)]
      
      duplicates <- duplicates[data_labels,
                               on=.(indicator_name,encounter_number),
                               nomatch=NULL]
      duplicates[,
                 colnum:=openxlsx::int2col(as.numeric(gsub("\\D","",rn)))]
      
      messages <- duplicates[,
                     .(message=paste0(indicator_name," is repeated ",.N," times with different values on row ",
                                      reporting_template_row_group,": ",
                                paste0("column ",colnum," is {",data_value,"}",collapse=" AND "))),
                     by=.(indicator_name,reporting_template_row_group)]
      messages <- messages$message
      if (length(messages) > 20) {
        messages[21] <- paste0("Truncated...Top 20 errors reported of ",length(messages)," errors found")
        messages <- messages[1:21]
      }
      
      for (m in messages) {
        status_message(class="error",
                       m,"\n")
      }
      
      duplicates <- duplicates[,
                 .(cols=paste0(unique(colnum),collapse=" and ")),
                 by=.(indicator_name)]
      stop(paste0("On tab QReport indicators repeated multiple times under different labels and reporting different values: ",
                  paste0(paste0(duplicates$indicator_name," matched for columns ",duplicates$cols),
                         collapse=" AND ALSO ")),". Either correct the column labels for the correct indicator; or if repeated columns are intentional, ensure the data is identical across rows; or if a column is for notes or non-reporting purposes, tell Jason to ignore the irrelevant column inder Setup Template")
    }
    
    ######
    data_sheet[,
               reporting_submitted_data_unit:=as.character(NA)]
    
    data_sheet <- data_sheet[,
                             .(reporting_template_row_group,
                               indicator_name,
                               reporting_submitted_data_value,
                               reporting_submitted_data_unit,
                               reporting_submitted_data_formula)]
    
    data_sheet
  }
  
  
  
  
  template_data <- rbindlist(list(data.summary,
                                  data.quarterly))
  
  template_data[,reporting_asof_date:=reporting_asof_date]
  
  
  template <- list(cohort_pfcbl_id=rsf_pfcbl_id.facility,
                   reporting_asof_date=reporting_asof_date,
                   template_data=template_data,
                   pfcbl_reporting_flags=reporting_flags)

  status_message(class="info","Success: Completed Parsing File:\n")
  return (template)
}
