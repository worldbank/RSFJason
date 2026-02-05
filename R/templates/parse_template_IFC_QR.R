

parse_template_IFC_QR <- function(pool,
                                  template_lookup,
                                  template_file,
                                  rsf_indicators,
                                  status_message,
                                  CALCULATIONS_ENVIRONMENT=CALCULATIONS_ENVIRONMENT) 
{
 
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
      template_headers <- NULL
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
    
    #will also omit the first rows above the Facility ID
    rsf_pfcbl_id.facility <- {
      
      
      id_expr <- "^.*#[[:space:]]*(\\d+).*$|^.*[[:space:]]+(\\d+)[[:punct:]]?$"
      name_id_expr <- "^[[:punct:]]?([[:alpha:][:space:]'-]+)[[:punct:][:space:]]+(\\d+).*$"
      project_name <- NULL
      project_id <- NULL
      id_data <- NULL
      
      for (summary_sheet_ID_row in 1:nrow(summary_sheet)) {
        id_cols <- which(grepl(name_id_expr,summary_sheet[summary_sheet_ID_row]) &  grepl(id_expr,summary_sheet[summary_sheet_ID_row]))
        if (length(id_cols)==2) {
          id_cols <- names(summary_sheet)[id_cols]
          id_data <- summary_sheet[summary_sheet_ID_row,
                                   ..id_cols]
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
                                      cni.rsf_pfcbl_id
                                      from p_rsf.rsf_data_current_names_and_ids cni
                                      where id = $1::text
                                        and cni.pfcbl_category = 'facility'",
                                 params=list(project_id))
      if (nrow(rsf_pfcbl_id) != 1) {
        stop(paste0("Failed to uniquely match IFC Project ID from Project ID defined in template: '",project_id,"'.  Has this IFC Facility been setup?"))
      }

      #We know that name and ID info is at the top and we've now parsed it, so discard and focus on what's next
      summary_sheet_ID_row
      #use to omit later
      #summary_sheet <- summary_sheet[-(1:r)]
      rsf_pfcbl_id$rsf_pfcbl_id #need this here to return it from the anonamous function
    }
    
    
    #labels, including facility-specific label mappings
    {
      #why did I do this?...Maybe to turn it into a setting or something later on?  For IFC QR template the headers are such a complete mess, just leave always on.  No use to if() check.
      save_headers <- TRUE
      rsf_labels <- db_indicators_get_header_actions(pool=pool,
                                                     template_id=template_lookup$template_id,
                                                     rsf_pfcbl_id=rsf_pfcbl_id.facility,
                                                     rsf_indicators=rsf_indicators,
                                                     detection="full",
                                                     normalize=T)
      
      stop_actions <- rsf_labels[is.na(stop)==FALSE & action=="ignore"]
      #This is now updated
      
      # stop_read_actions <- rsf_labels[rsf_indicators[indicator_sys_category=="template_read_stop",.(indicator_id)],
      #                                on=.(map_indicator_id=indicator_id),
      #                                nomatch=NULL]
      if (F) {
      rsf_labels <- rbindlist(rsf_indicators$labels)
      
      #use superTrim() over label_normalized
      rsf_labels <- unique(rsf_labels[,.(map_indicator_id=indicator_id,label_key,label=superTrim(label))])
      
      #rsf labels only map to indicators and are the default matching
      rsf_labels[,
                 `:=`(template_header_section_name=as.character(NA),
                      template_section_lookup=as.character(NA),
                      template_label_lookup=paste0('^"?',str_escape(label),'"?$'), #ignore quoted headers
                      action="default",
                      template_header_position=as.numeric(NA),
                      map_formula_id=as.numeric(NA),
                      calculation_formula=as.character(NA),
                      map_check_formula_id=as.numeric(NA),
                      check_formula=as.character(NA))]
      
      header_actions <- db_indicators_get_header_actions(pool=pool,
                                                         template_id=template_lookup$template_id,
                                                         rsf_pfcbl_id=rsf_pfcbl_id.facility)
      
     
      
      setDT(header_actions)
      
      header_actions[,label_key:="SYS"]
      header_actions[,label:=superTrim(template_header)] #for this template, use trimmed, not normalized (as parsing values are used and therefore don't normalize {} delimiter!)
      
      header_actions[,
                     label:=normalizeLabel(template_header)] #normalizedLabel calls superTrim(trim.punct=F)
      
      # header_actions[label_normalized=="na",
      #                label_normalized:=as.character(NA)]
  
      setnames(header_actions,
               old="map_indicator_id",
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
    }
  }
  
  data.summary <- {
  
    
    remap_lcu <- Filter(length,
                        lapply(summary_sheet,
                        grep,
                        pattern="LCU|LCY",
                        value=T))

    #The LCU "base currency" should define what the LCU value is in the Summary sheet
    if (length(remap_lcu)) {
      bcu <- rsf_indicators[data_category=="facility" &
                            (grepl("ifc.*maximum.*risk",indicator_name,ignore.case=T) | indicator_sys_category=="entity_local_currency_unit")]
      
      if (!empty(bcu)) {
        if (!empty(bcu[indicator_sys_category=="entity_local_currency_unit"])) {
          bcu <- bcu[indicator_sys_category=="entity_local_currency_unit"]
        }

        label_matches <- lapply(summary_sheet[,.SD,.SDcols=c(names(remap_lcu))],
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
                                find_sections=tolower(rsf_labels[map_indicator_id==bcu$indicator_id]$template_section_lookup),
                                find_labels=tolower(rsf_labels[map_indicator_id==bcu$indicator_id]$template_label_lookup),
                                match_id=rsf_labels[map_indicator_id==bcu$indicator_id]$label_header_id,
                                match_postion=rsf_labels[map_indicator_id==bcu$indicator_id]$template_header_position)
        
        srows <- unique(unlist(lapply(label_matches,lapply,`[[`,"match_rows"),recursive=T))
          
        lcu <- unique(unlist(Filter(length,
                      sapply(paste0("^",CALCULATIONS_ENVIRONMENT$VALID_CURRENCIES,"$"),
                             grep,x=unlist(summary_sheet[srows]),value=T,USE.NAMES = F))))
        
        if (length(lcu)==0) {
          
          for (n in names(remap_lcu)) {
            print(n)
            summary_sheet[,(n):=gsub("LCU|LCY",lcu,get(n),ignore.case=F)]
          }
        
          for (n in unlist(remap_lcu)) {
            rl <- gsub("LCU|LCY",lcu,n)
            status_message("warning",
                           paste0("Correcting Local Currency reference FROM [",n,"] TO [",rl,"]\n"))
            reporting_flags <- rbindlist(list(reporting_flags,
                                              data.table(rsf_pfcbl_id=NA,
                                                         indicator_id=NA,
                                                         check_name="sys_flag_data_format_auto_correction",
                                                         check_message=paste0("Correcting Local Currency reference FROM [",n,"] TO [",rl,"]\n"))))
          }
      }
    }
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

      if (length(label_cols[label_cols > 0]) < 2) stop("Failed to find bi-lingual indicator labels in the template (ie, two columns of labels in the Summary tab)")
      
      label_cols <- sort(label_cols,decreasing =TRUE)[1:2]
      label_cols_index <- which(names(label_matches) %in% names(label_cols))
      
      if (label_cols_index[2]-label_cols_index[1] != 1) {
        stop(paste0("Label columns are not adjacent.  Template seems to have labels in columns ",paste0(label_cols_index,collapse=" and ")," instead of next to each other?"))
      }
      
      label_cols_names <- names(label_matches)[label_cols_index]
      label_matches <- label_matches[label_cols_index]
      
      #match_id is label_header_id
      label_matches <- rbindlist(lapply(seq_along(label_cols_index),function(header_row,label_matches) { 
        x <- rbindlist(label_matches[[header_row]])
        x[,header_row:=header_row]
        return(x)
      },label_matches=label_matches))
      
      label_matches[,
                    `:=`(positions_matched = .N,
                         all_positions_matched= all(is.na(match_position)) | all(header_row %in% match_position)),
                    by=.(match_id,match_rows)]
      
      label_matches[rsf_labels[is.na(template_header_position)==F][,.(positions_expected=max(template_header_position)),by=.(label_header_id)],
                    all_positions_matched:=all_positions_matched & i.positions_expected==positions_matched,
                    on=.(match_id=label_header_id)]
      
      label_matches <- label_matches[all_positions_matched==T]
      
      label_matches[,
                     selected:=positions_matched==max(positions_matched),
                     by=.(match_rows)]
      
      label_matches <- label_matches[selected==TRUE]
      label_matches[,selected:=NULL]
      
      label_matches <- label_matches[rsf_labels[,.(label_header_id,action,map_indicator_id,map_formula_id,map_check_formula_id)],
                                     on=.(match_id=label_header_id),
                                     nomatch=NULL]
      label_matches <- label_matches[,
                                     .(header_ids=list(unique(match_id))),
                                     by=.(original_row_num=match_rows,action,map_indicator_id,map_formula_id,map_check_formula_id,header_row)]
      
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
      #should filter out, eg
      #Key RSA terms
      #Quarterly situation
      #etc.
      one_label_no_data <- which(sapply(as.data.frame(is.na(t(summary_sheet[,.(label1,label2)]))),any) &
                                 sapply(as.data.frame(is.na(t(summary_sheet[,.(data_unit,data_value)]))),all))
      
      #Except unless we want to do something with this header label and it's matched a mapping action
      one_label_no_data <- one_label_no_data[!(summary_sheet[one_label_no_data,original_row_num %in% label_matches$original_row_num])]
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
                                        variable.name="header_row",
                                        value.factor=F,
                                        value.name="label")
      
      summary_sheet[,
                    header_row:=as.numeric(gsub("label","",header_row))]
      
      summary_sheet[,label_normalized:=normalizeLabel(label)]
      
      summary_sheet[is.na(label),
                    label_normalized:=normalizeLabel("NA")]
     
      setorder(summary_sheet,
               original_row_num,
               header_row,
               na.last = TRUE)
      
      
    }
    
    {
      summary_sheet <- label_matches[summary_sheet,
                                     on=.(original_row_num,
                                          header_row)]  
    }
    
   
    
    #Label errors/mismatching
    {
      summary_sheet <- summary_sheet[original_row_num > summary_sheet_ID_row]
      summary_sheet[,
                     ignore:=anyNA(action)==FALSE & all(action=="ignore"),
                     by=.(original_row_num)]

      stop_row <- na.omit(stop_actions[grepl("summary",template_header_section_name,ignore.case=T) & is.na(stop)==FALSE,stop])
      if (length(stop_row) > 0) {
        stop_row <- max(stop_row)
        summary_sheet[original_row_num > stop_row,
                      ignore:=TRUE]
      }
      
      stop_row <- summary_sheet[map_indicator_id %in% rsf_indicators[indicator_sys_category=="template_read_stop",indicator_id],original_row_num]
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
                              check_message=paste0("Summary Tab Row ",original_row_num," Column ",header_row," \"",label,"\""))]
          
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
                       untranslated:=anyNA(action) & !all(is.na(action)),
                       by=.(original_row_num)]
        
        untranslated_labels <- summary_sheet[untranslated==TRUE]
        untranslated_labels <- untranslated_labels[is.na(action) | action != "ignore"]
        
        if (!empty(untranslated_labels)) {
          setorder(untranslated_labels,
                   original_row_num,
                   header_row)
          
          untranslated_labels[summary_sheet[is.na(map_indicator_id)==FALSE],
                              matched_message:=paste0(i.indicator_name," matched using label #",i.header_row," '",i.label,"'"),
                              on=.(original_row_num)]
          
          untranslated_labels[summary_sheet[is.na(map_indicator_id)==TRUE],
                              matched_message:=paste0(matched_message," but failed to identify label #",i.header_row," '",i.label,"'"),
                              on=.(original_row_num)]
          
          untranslated_labels[,
                              `:=`(rsf_pfcbl_id=rsf_pfcbl_id.facility,
                                   reporting_asof_date=reporting_asof_date,
                                   check_name="sys_flag_missing_data",
                                   check_message=paste0("Summary Tab Column ",header_row," Row ",original_row_num," \"",label,"\" (",matched_message,")"))]
          
          untranslated_labels <- untranslated_labels[is.na(map_indicator_id),
                                                     .(rsf_pfcbl_id,
                                                        indicator_id=map_indicator_id,
                                                        reporting_asof_date,
                                                        check_name,
                                                        check_message)]
          
          reporting_flags <- rbindlist(list(reporting_flags,
                                            untranslated_labels))
        }
      }
      
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
      
      if (save_headers) {
        template_headers <- rbindlist(list(template_headers,
                                           summary_sheet[!is.na(map_indicator_id),
                                                         .(label,
                                                           data_source_index=paste0("SUMMARY ROW-",original_row_num),
                                                           indicator_id=map_indicator_id)]))
      }
      
      
      summary_sheet <- summary_sheet[is.na(map_indicator_id)==FALSE,
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
    
    summary_sheet
    
  }

  data.quarterly <- {
    
    {
      label_matches <- lapply(as.data.frame(t(data_sheet[1:30])),
                              FUN=function(x,find_sections,find_labels,match_id,match_postion) {
                                
                                mapply(labelMatches,
                                       find_sections=find_sections,
                                       find_labels=find_labels,
                                       match_id=match_id,
                                       match_postion=match_postion,
                                       MoreArgs=list(search_sections=rep(x="qreport",times=length(x)),
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
      
      if (length(label_rows[label_rows > 0]) < 2) stop("Failed to find bi-lingual indicator labels in the template (ie, two columns of labels in the QReport tab)")
      
      label_rows <- sort(label_rows,decreasing =TRUE)[1:2]
      label_rows <- label_rows[order(names(label_rows))] #top two may result in a mis-sorting of which are first and second labels.  So ensure they are re-sorted.
      label_rows_index <- which(names(label_matches) %in% names(label_rows))
      label_matches <- label_matches[names(label_rows)]
      
      data_labels <- data_sheet[label_rows_index]
      label_cols_index <- which(!sapply(as.data.frame(is.na(data_labels)),all))
      
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
      data_formulas <- as.data.table(data_formula_matrix[data_rows,
                                                         1:ncol(data_sheet)])
      setnames(data_formulas,
               old=names(data_formulas),
               new=names(data_sheet))
      
      label_matches <- rbindlist(lapply(seq_along(label_rows),function(header_row,label_matches) { 
        x <- rbindlist(label_matches[[header_row]])
        x[,header_row:=header_row]
        return(x)
        },label_matches=label_matches))
      
      label_matches[,
                    `:=`(positions_matched = .N,
                         all_positions_matched= all(is.na(match_position)) | all(header_row %in% match_position)),
                    by=.(match_id,match_rows)]
      
      label_matches[rsf_labels[is.na(template_header_position)==F][,.(positions_expected=max(template_header_position)),by=.(label_header_id)],
                    all_positions_matched:=all_positions_matched & i.positions_expected==positions_matched,
                    on=.(match_id=label_header_id)]
      
      
      label_matches <- label_matches[all_positions_matched==T]
      label_matches[,
                    selected:=positions_matched==max(positions_matched),
                    by=.(match_rows)]
      label_matches <- label_matches[selected==TRUE]
      label_matches[,selected:=NULL]
      
      label_matches <- unique(label_matches)
      
      label_matches <- label_matches[rsf_labels[,.(label_header_id,action,map_indicator_id,map_formula_id,map_check_formula_id)],
                                     on=.(match_id=label_header_id),
                                     nomatch=NULL]
      
      label_matches <- label_matches[,
                                     .(header_ids=list(unique(match_id))),
                                     .(original_col_num=openxlsx::int2col(match_rows),header_row,
                                       action,map_indicator_id,map_formula_id,map_check_formula_id)]
    }

    
    #data labels
    {

      no_data_reported <- which(sapply(data_sheet,function(x) all(is.na(x)),USE.NAMES = T))
      no_data_reported <- data.table(rn=names(no_data_reported),original_col_num=openxlsx::int2col(no_data_reported))
      
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
                           variable.name = "header_row",
                           value.name="label")
      
      data_labels[,
                   original_col_num:=openxlsx::int2col(as.numeric(gsub("\\D","",rn)))]
      
      data_labels[,
                  header_row:=as.numeric(gsub("label","",header_row))]
      
      
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
      
      data_labels[,ignore:=FALSE]
      data_labels[ignore==FALSE,
                  ignore:=anyNA(action)==FALSE & all(action=="ignore"),
                  by=.(original_col_num)]
      
      data_labels[ignore==FALSE,
                  ignore:=all(is.na(action)) & all(no_data),
                  by=.(original_col_num)]
      
      data_labels <- data_labels[ignore==FALSE]
      
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
                              check_message=paste0("QReport Tab column ",original_col_num," row ",header_row,"  \"",label,"\""))]
          
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
                      untranslated:=anyNA(action) & !all(is.na(action)),
                      by=.(original_col_num)]
        
        untranslated_labels <- data_labels[untranslated==TRUE]
        untranslated_labels <- untranslated_labels[is.na(action) | action != "ignore"]
        
        if (!empty(untranslated_labels)) {
          setorder(untranslated_labels,
                   original_col_num,
                   header_row)
          
          
          untranslated_labels[data_labels[is.na(map_indicator_id)==FALSE],
                              matched_message:=paste0(i.indicator_name," matched label #",header_row),
                              on=.(original_col_num)]
          
          untranslated_labels[,
                              `:=`(rsf_pfcbl_id=rsf_pfcbl_id.facility,
                                   reporting_asof_date=reporting_asof_date,
                                   check_name="sys_flag_missing_data",
                                   check_message=paste0("QReport Tab, headers #",header_row," column ",original_col_num," \"",label,"\" (",matched_message,")"))]
          
          untranslated_labels <- untranslated_labels[is.na(map_indicator_id),
                                                     .(rsf_pfcbl_id,
                                                       indicator_id=map_indicator_id,
                                                       reporting_asof_date,
                                                       check_name,
                                                       check_message)]
          
          
          reporting_flags <- rbindlist(list(reporting_flags,
                                            untranslated_labels))
        }
      }
      
      #data_labels <- data_labels[is.na(indicator_id)==FALSE]
      
      
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
                                                           data_source_index=paste0("QReport COL-",original_col_num),
                                                           indicator_id=map_indicator_id)])))
      }
      
      
      data_labels <- data_labels[is.na(action)==FALSE,
                                 .(original_col_num,
                                   rn,
                                   action,
                                   map_indicator_id,
                                   map_formula_id,
                                   map_check_formula_id,
                                   indicator_name,
                                   label_unit)]
      
      data_labels <- unique(data_labels)
      
  
    }
    
    
    #data_cols_index <- which(names(data_sheet) %in% data_cols_names)
    rename <- unique(data_labels[,.(rn,original_col_num)])
    setnames(data_sheet,
             old=rename$rn,
             new=rename$original_col_num)

    setnames(data_formulas,
             old=rename$rn,
             new=rename$original_col_num)
    
    data_cols_names <- rename$original_col_num
    
    data_sheet <- data_sheet[,
                             ..data_cols_names]
    
    data_formulas <- data_formulas[,
                                   ..data_cols_names]
    
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
                             .(blank=all(is.na(reporting_submitted_data_value) | nchar(reporting_submitted_data_value)==0)),
                             by=.(reporting_template_row_group)
                             ][blank==T,reporting_template_row_group]
    
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
  
  
  
  
  template_data <- rbindlist(list(data.summary,
                                  data.quarterly))
  
  template_data[,reporting_asof_date:=reporting_asof_date]
  
  #This is moved to template_process in a more generalized manor.
  # duplicates <- template_data[,
  #                          .(n=length(unique(reporting_submitted_data_value)),
  #                            values=list(reporting_submitted_data_value)),
  #                          by=.(indicator_name,
  #                               reporting_template_row_group)][n>1]
  # if (!empty(duplicates)) {
  #   
  #   duplicates <- duplicates[,
  #                            .(data_value=unlist(values,recursive=F)),
  #                            by=.(indicator_name,
  #                                 reporting_template_row_group)]
  #   
  #   duplicates[,
  #              encounter_number:=1:.N,
  #              by=.(indicator_name,
  #                   reporting_template_row_group)]
  #   
  #   data_labels[,
  #               encounter_number:=1:.N,
  #               by=.(indicator_name)]
  #   
  #   duplicates <- duplicates[data_labels,
  #                            on=.(indicator_name,encounter_number),
  #                            nomatch=NULL]
  #   duplicates[,
  #              colnum:=openxlsx::int2col(as.numeric(gsub("\\D","",rn)))]
  #   
  #   messages <- duplicates[,
  #                          .(message=paste0(indicator_name," is repeated ",.N," times with different values on row ",
  #                                           reporting_template_row_group,": ",
  #                                           paste0("column ",colnum," is {",data_value,"}",collapse=" AND "))),
  #                          by=.(indicator_name,reporting_template_row_group)]
  #   messages <- messages$message
  #   if (length(messages) > 20) {
  #     messages[21] <- paste0("Truncated...Top 20 errors reported of ",length(messages)," errors found")
  #     messages <- messages[1:21]
  #   }
  #   
  #   for (m in messages) {
  #     status_message(class="error",
  #                    m,"\n")
  #   }
  #   
  #   duplicates <- duplicates[,
  #                            .(cols=paste0(unique(colnum),collapse=" and ")),
  #                            by=.(indicator_name)]
  #   stop(paste0("On tab QReport indicators repeated multiple times under different labels and reporting different values: ",
  #               paste0(paste0(duplicates$indicator_name," matched for columns ",duplicates$cols),
  #                      collapse=" AND ALSO ")),". Either correct the column labels for the correct indicator; or if repeated columns are intentional, ensure the data is identical across rows; or if a column is for notes or non-reporting purposes, tell Jason to ignore the irrelevant column inder Setup Template")
  # }
  
  template <- list(cohort_pfcbl_id=rsf_pfcbl_id.facility,
                   reporting_asof_date=reporting_asof_date,
                   template_data=template_data,
                   pfcbl_reporting_flags=reporting_flags,
                   template_headers=unique(template_headers))

  status_message(class="info","Success: Completed Parsing File:\n")
  return (template)
}
