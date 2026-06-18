

parse_template_IFC_QR2018 <- function(pool,
                                      template_file,
                                      template_lookup=db_export_get_template(pool=pool,template_name="IFC-QR-TEMPLATE2018"),
                                      rsf_indicators=db_indicators_get_labels(pool),
                                      return.insert_flags=NULL, #To insert and return current flags tab based on current QR of template_file in system: this is a DATA TABLE of flags exported by UI
                                      return.next_date=FALSE,    #To automatically create zero-version of next QR based on current QR in system
                                      reporting_user_id,
                                      status_message,
                                      CALCULATIONS_ENVIRONMENT=CALCULATIONS_ENVIRONMENT) 
{
 
  {
    if (!file.exists(template_file)) stop(paste0("Uh oh! File '",template_file,"' doesn't exist!"))
    if (!file_ext(template_file) %in% "xlsx") stop("Only .xlsx files using Excel-365 versions or later may use this template")
    if ((file.info(template_file)$size / 1024^2) > 350) {
      stop(paste0("Uh oh! This file is ",round(file.info(template_file)$size / 1024^2,2),"MB! This exceeds the 35MB limit (and it surely also exceeds IFC and client email server limits, which will prevent you from communicating this data with the client). ",
                  "Most likely, your file unnecessarily large and Excel can reduce the size considerably (like deleting unused formats).  Ask Copilot Excel how to ",
                  "reduce this file size and re-save it or try going to: \n",
                  "- Review tab \n",
                  "- Check Performance button \n",
                  "- Optimize Workbook \n",
                  "However, if all the data here is truly necessary for upload, re-save as a .csv the quarterly report file and use Jason's csv file upload method instead (",
                  "the qreport headers need to be on row 1 of the .csv file and the project ID number needs to be in the file name of the .csv file"))
    }
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
  
    #effort to migrate from openxlsx to openxlsx2 ... slowly.
    excelwb2 <- suppressWarnings(openxlsx2::wb_load(template_file))
    
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
    
      nregions_all <- data.table(name=nregions,
                                 sheet=attr(nregions,"sheet"),
                                 position=attr(nregions,"position"))
      
      nregions_all[,is_system:=grepl("\\.wvu|^_xlnm",name)]
      nregions_all <- nregions_all[is_system==FALSE]
      #Note: sheets with external references links can create real issues and also generate sheet names that aren't in the workbook.  Eg, "[1]1. Summary"
      
      nregions_date <- which(nregions %in% c("S_DET","S_QDD"))
      nregions_locations <- attr(nregions,"position")[nregions_date]
      nregions_sheets <- attr(nregions,"sheet")[nregions_date]
      nregion_cols <- convertFromExcelRef(nregions_locations)
      nregion_rows <- as.numeric(gsub("[^0-9.]","",nregions_locations))
      nregion_coords <- data.frame(col=nregion_cols,row=nregion_rows,sheet=nregions_sheets)
      

      bad_names <- nregions_all[grepl("#REF",nregions_all$name,ignore.case=T) | (!sheet %in% snames & !grepl("[()]",sheet))]
      if (!empty(bad_names)) {
          status_message("\nTemplate has errors in the DEFINED NAMES (type Ctrl+F3 or Ctrl+Fn+F3) or navigate to name manager to review\n")
        for (i in 1:nrow(bad_names)) {
          nr <- bad_names[i]
          status_message(paste0("Defined name #REF error '",nr$name,"' ",nr$sheet,"!",nr$position),"\n")
          
        }
      
      }
      
      nregions_all[,n:=.N,by=.(name)]
      dup_names <- nregions_all[n>1]
      if (!empty(dup_names)) {
        status_message("\nTemplate has duplicated/ambiguous DEFINED NAMES (type Ctrl+F3 or Ctrl+Fn+F3) or navigate to name manager to review\n")
        setorder(dup_names,name,sheet,position)
        for (i in 1:nrow(dup_names)) {
          nr <- dup_names[i]
          status_message(paste0("Defined name duplicated x",nr$n," error '",nr$name,"' ",nr$sheet,"!",nr$position),"\n")
          
        }
      }
      
      if (!empty(bad_names) | !empty(dup_names)) {
        Sys.sleep(2)
      }
      
      # if (any(!nregion_coords$sheet %in% snames)) {
      #   status_message(class="error","Workbook contains references to Worksheets that do not exists. Check for external links and use Break Links if these exist.\n")
      #   status_message(class="error","Important: Workbook may contain links in the 'Name Manager'.  These must be manually deleted: type Ctrl+F3 (or Ctrl+Fn+F3) or navigate to Formulas -> Name Manager\n")
      #   status_message(class="info","Attempting to fix sheet....If fix is successful, script will proceed. Otherwise, break links is required.\n")
      #   
      # 
      #   
      # }
      # 
      nregion_coords <- nregion_coords[nregion_coords$sheet %in% snames,]
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
      
      if (reporting_asof_date >= today()) {
        stop(paste0("Invalid reporting QDD date: ",reporting_asof_date," is in the FUTURE!  This is not possible. Verify the QDD date is correct?"))
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
      
      #2018 template doesn't use named variables, so read-in more standard
      {
        
        
        listSheetName <- grep("lists|template",snames,ignore.case=T,value=T)
        if (length(listSheetName) != 1) { stop("Failed to find Template sheet (formally Lists)") }
        
        # list_sheet <- nregions_table[sheets==listSheetName,.(name,data_value)]
        # list_sheet <- list_sheet[grepl("^Template_",name)==F] #template defined names are inherently excluded as not relevant for Jason (they're for the template!)
        list_sheet <- excelwb2$to_df(sheet=listSheetName,
                                     col_names=F,
                                     skip_empty_cols=T,
                                     skip_empty_rows = T)
        list_sheet[["original_row_num"]] <- row.names(list_sheet)
        setDT(list_sheet)
        
        
        header_row <- which.max(sapply(data.frame(t(!is.na(list_sheet))),sum))
        list_sheet <- list_sheet[header_row:nrow(list_sheet),]
        
       
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
                                                     formatting.function=normalizeLabel)
      
      
    }
  }
  
  data.lists <- { 
    
    
    # useless_cols <- sapply(list_sheet[header_row],function(hr) any(sapply(useless_cols,grepl,x=hr,ignore.case=T)))
    # useful_cols <- names(useless_cols[!useless_cols])
    # list_sheet <- list_sheet[,..useful_cols]

    #Many spreadsheets "Stack" lists across rows in a checkerboard of overlapping lists.
    #Here, see if any column data is separated by 3 (or more) consecutive NA values and split into separate columns.
    #Return a list of data.frames that use the first row as the column header.
    lists_data <- data.table(reporting_template_row_group=character(0),
                             reporting_template_data_rank=character(0),
                             indicator_name=character(0),
                             reporting_submitted_data_value=character(0),
                             reporting_submitted_data_unit=character(0),
                             reporting_submitted_data_formula=character(0))
    
    if (!empty(list_sheet)) {
      
      lists_data <- lapply(names(list_sheet),function(cn,ls) {
        if (cn=="original_row_num") return (NULL)
        
        col <- ls[[cn]]
        orn <- ls[["original_row_num"]]
        nas <- rle(is.na(col))
        nas$values <- nas$values & nas$lengths>=3
        split_ids <- cumsum(inverse.rle(nas))
        result <- split(col[!inverse.rle(nas)], split_ids[!inverse.rle(nas)])
        resultrow <- split(orn[!inverse.rle(nas)], split_ids[!inverse.rle(nas)])
        
        mapply(FUN=function(r,rr,cn) {

          result_label <- r[1]
          r <- r[-1]
          r <- r[!is.na(r)]
          
          r <- gsub(","," ",r)
          r <- gsub("\\s+"," ",r)
          r <- paste0(r,collapse=",")
          
          df <- data.frame(original_row_num=rr[[1]],
                           original_col_num=cn,
                           data_value=r,
                           label=result_label)
          df
        },
        r=result,
        rr=resultrow,
        MoreArgs = list(cn=cn),
        SIMPLIFY = F)
        
        
        # #do.call(cbind,result,resultrow)
        # result_label <- result[1]
        # result <- result[-1]
        # result <- result[!is.na(result)]
        # 
        # result <- gsub(","," ",result)
        # result <- gsub("\\s+"," ",result)
        # result <- paste0(result,collapse=",")
        # 
        # df <- data.frame(original_row_num=resultrow[[1]],
        #                  original_col_num=cn,
        #                  data_value=result,
        #                  label=result_label)
        # df
        # df <- data.frame(paste0(result[-1],collapse=","),row=min(result_row),col=cn)
        # 
       
        
        # mapply(FUN=function(a,b,c) { 
        #   unique(cbind(do.call(data.frame,args=list(data=a,row=min(b))),col=cn)) #min(b) because its' the position of where the table header starts
        # },a=result,b=resultrow,SIMPLIFY = F)
        
      },ls=list_sheet)
      
      lists_data <- unlist(lists_data,recursive=F)
      lists_data <- rbindlist(lists_data)
      
      
      #These are mostly ubiquitious in these templtes -- just ignoring in full without specific template setup.
      useless_list_labels <- c("QDDs",
                               "FR",
                               "row",
                               "col",
                               "sheet",
                               "Translation of other specific cells",
                               "FR",
                               "Yes/No",
                               "sector",
                               "Reason for partial inclusion","interest type")
      
      #sapply(lists_data$label,function(lab) any(sapply(useless_list_labels,grepl,x=lab,ignore.case=T)))
      lists_data <- lists_data[!sapply(label,function(lab) any(sapply(useless_list_labels,grepl,x=lab,ignore.case=T)))]
      lists_data[,list_id:=1:.N] #must be set after filtering so list_id matches row number
    }    
    
    #Matches
    {
      label_matches <- mapply(labelMatches,
                              find_sections=tolower(rsf_labels$template_section_lookup),
                              find_labels=tolower(rsf_labels$template_label_lookup),
                              match_id=rsf_labels$label_header_id,
                              match_postion=rsf_labels$template_header_position,
                              MoreArgs=list(search_sections=rep(x="lists",times=length(lists_data$label)),
                                            search_labels=normalizeLabel(lists_data$label)),
                              USE.NAMES = F,
                              SIMPLIFY = F)
      
      label_matches <- rbindlist(label_matches)
      
      label_matches <- label_matches[rsf_labels[,.(label_header_id,action,map_indicator_id,map_formula_id,map_check_formula_id,header_id)],
                                     on=.(match_id=label_header_id),
                                     nomatch=NULL]
      
      #changed all references from: header_ids=list(unique(match_id)) TO header_ids=list(unique(header_id)) 
      
      label_matches <- label_matches[,
                                     .(header_ids=list(unique(header_id))),
                                     by=.(list_id=match_rows,action,map_indicator_id,map_formula_id,map_check_formula_id)]
      
      
      lists_data <- label_matches[lists_data,
                                  on=.(list_id)]  
      
      #lists_data[rsf_indicators[,.(indicator_name,data_category,indicator_id)],on=.(map_indicator_id=indicator_id),nomatch=NULL]
    }
    
    #Label errors/mismatching
    {
      lists_data[,
                 ignore:=anyNA(action)==FALSE & all(action=="ignore"),
                 by=.(list_id)]
      
      lists_data <- lists_data[ignore==FALSE]
      
      lists_data[rsf_indicators,
                 `:=`(indicator_name=i.indicator_name,
                      data_category=i.data_category),
                 on=.(map_indicator_id=indicator_id)]
      
      {
        bad_categories <- lists_data[!is.na(data_category) & !data_category %in% c("client","facility")]
        
        if (!empty(bad_categories)) {
          
          bad_categories[,
                         check_message:=paste0("LISTS sheet ",original_col_num,original_row_num," '",
                         label,"' maps to a ",
                         toupper(data_category)," level metric '",indicator_name,"' however the LISTS sheet can only have Client and Facility setup data ",
                         " (lists of acceptable input that can be used in drop-down lists, as defined by the RSA or template). ",
                         "Since this data cannot be for any specific ",toupper(data_category)," it is being IGNORED.  Consider going to ",
                         "JASON -> RSF Setup -> Setup Templates -> ",template_lookup$template_name," to remap ",
                         "this specific label to the appropriate client or facility level metric; or set to ignore it entirely this field is for Excel use only")]
          
          status_message(type="error",paste0(bad_categories$check_message,collapse="\n\n"))
          
          bad_categories <- bad_categories[,.(rsf_pfcbl_id=NA,
                                              indicator_id=NA,
                                              reporting_asof_date=reporting_asof_date,
                                              check_name="sys_reporting_data_discarded",
                                              check_message)]
          
          reporting_flags <- rbindlist(list(reporting_flags,
                                            bad_categories))
          
          lists_data <- lists_data[is.na(data_category) | data_category %in% c("client","facility")]
          
        }
      }
      #Will fail because its ambiguous: this shouldn't be possible for defined names? Unless copy-paste errors copy-in multiple defined names??
      {
        lists_data[,
                   mismatch:=anyNA(action)==FALSE & length(unique(map_indicator_id))>1,
                   by=.(list_id)]
        
        mismatch_labels <- lists_data[mismatch==TRUE]
        if (!empty(mismatch_labels)) {
          
          
          mismatch_labels[,
                          message:=paste0("List Sheet table/list name \"",label,"\" maps to \"",indicator_name,"\"")]
          
          setorder(mismatch_labels,
                   list_id)
          
          message <- paste0(mismatch_labels$message,collapse=" \n")
          stop(paste0("Mismatched Column Labels:\n",
                      "Correct the column name(s) in Lists Tab \n",
                      "Or if this is a Template Requirement map these columns in JASON -> RSF Setup -> Setup Templates -> ",template_lookup$template_name," \n\n",
                      message))
        }
      }    
      
      #Unfound: will asign to entity_reporting
      {
        lists_data[,
                   notfound:=all(is.na(action)),
                   by=.(list_id)]
        
        unfound_labels <- lists_data[notfound==TRUE]
        
        if (!empty(unfound_labels)) {
          
          setorder(unfound_labels,
                   list_id)
          
          unfound_labels[,
                         `:=`(rsf_pfcbl_id=rsf_pfcbl_id.facility,
                              indicator_id=as.numeric(NA), #will be auto-assigned to reporting indicator
                              reporting_asof_date=reporting_asof_date,
                              check_name="sys_flag_indicator_not_found",
                              check_message=paste0("Lists Sheet for list with header name \"",label,"\" on ",original_col_num,original_row_num))]
          
          unfound_labels <- unfound_labels[,.(rsf_pfcbl_id,
                                              indicator_id,
                                              reporting_asof_date,
                                              check_name,
                                              check_message)]
          
          reporting_flags <- rbindlist(list(reporting_flags,
                                            unfound_labels))
        }
      }
      
      lists_data <- lists_data[is.na(action)==FALSE]
      
      if (save_headers) {
        template_headers <- rbindlist(list(template_headers,
                                           lists_data[!is.na(map_indicator_id),
                                                      .(label=label,
                                                        data_source_index=paste0("Lists Sheet ",original_col_num,original_row_num,": ",label),
                                                        indicator_id=map_indicator_id)]))
      }
      
      
      lists_data <- lists_data[is.na(map_indicator_id)==FALSE,
                               .(indicator_name,
                                 data_unit=as.character(NA),
                                 data_value,
                                 original_row_num)]
    }
    
    {

      lists_data[,
                 `:=`(reporting_submitted_data_unit=data_unit,
                      reporting_submitted_data_value=data_value)]
      
      lists_data[rsf_indicators[!is.na(data_unit),
                                .(indicator_name,data_unit,joincondition=as.character(NA))],
                 reporting_submitted_data_unit := i.data_unit,
                 on=.(indicator_name,
                      reporting_submitted_data_unit=joincondition)]
      lists_data[,
                 reporting_submitted_data_formula:=as.numeric(NA)]
      
      lists_data[rsf_indicators,
                 indicator_sys_category:=i.indicator_sys_category,
                 on=.(indicator_name)]
      
      if (any(lists_data$indicator_sys_category=="products_eligible",na.rm=T)) {
        products <- unlist(lists_data[indicator_sys_category=="products_eligible",data_value])
        
        #Copied from IFC_QR2025...because I was optimistic but none of these obsolete templates track product metadata/classifications.
      
      }
      
      lists_data[,
                 reporting_template_row_group:='1LISTS']
      
      lists_data[,
                 reporting_template_data_rank:=1:.N] #1:.N instead of original row number since new data may be added in products
      
      
      
      
    }
    
    lists_data[,.(reporting_template_row_group,
                  reporting_template_data_rank,
                  indicator_name,
                  reporting_submitted_data_value,
                  reporting_submitted_data_unit,
                  reporting_submitted_data_formula=as.character(NA))]
  }
  
  
  data.summary <- {
  
    summarySheetIndex <- grep("summary",excelwb2$sheet_names,ignore.case = T)
    
    #A real pain!
    #But with tehse 2018 version templates, columns are in English (and French) and then people copy-paste templates
    #and then the client doesn't speak French and they hide the column (or vice versa) and then they change the columns to something else entirely
    #and then one language says the data is X and the other says the data is Y and then Jason throws an error and people complain that Jason isn't working.
    #so, now we're going to test which is a hidden or nearly-hidden column and pick that in the case of an ambiguity.
    summarySheetXML <- 
      as.data.table(
        dplyr::bind_rows(
          lapply(
            lapply(
              (excelwb2$worksheets[[summarySheetIndex]])$cols_attr,
                xml2::read_xml),
            xml2::xml_attrs)))
    
    summarySheetXML[,excelCol:=openxlsx2::int2col(1:.N)]
    if (!any(names(summarySheetXML)=="hidden")) summarySheetXML[,hidden:=as.character(NA)]
    
    
    #This is a legacy issue of indicators generically defining the Base Currency Unit to "LCU" currency FX without specifing what exactly.
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
                                         USE.NAMES = F,
                                         SIMPLIFY=F)
                                  
                                },
                                find_sections=tolower(rsf_labels[map_indicator_id==bcu$indicator_id]$template_section_lookup),
                                find_labels=tolower(rsf_labels[map_indicator_id==bcu$indicator_id]$template_label_lookup),
                                match_id=rsf_labels[map_indicator_id==bcu$indicator_id]$label_header_id,
                                match_postion=rsf_labels[map_indicator_id==bcu$indicator_id]$template_header_position)
        
        srows <- unique(unlist(lapply(label_matches,lapply,`[[`,"match_rows"),recursive=T))
          
        lcu <- unique(unlist(Filter(length,
                      sapply(paste0("^",CALCULATIONS_ENVIRONMENT$VALID_CURRENCIES,"$"),
                             grep,x=unlist(summary_sheet[srows]),value=T,USE.NAMES = F))))
        
        if (length(lcu)==1) {
          
          for (n in unique(names(remap_lcu))) {
            
            summary_sheet[,(n):=gsub("LCU|LCY",lcu,get(n),ignore.case=F)]
          }
        
          for (n in unique(unlist(remap_lcu))) {
            rl <- gsub("LCU|LCY",lcu,n)
            status_message(class="warning",
                           paste0("Correcting Local Currency reference FROM [",n,"] TO [",rl,"]\n"))
            reporting_flags <- rbindlist(list(reporting_flags,
                                              data.table(rsf_pfcbl_id=NA,
                                                         indicator_id=NA,
                                                         reporting_asof_date=NA,
                                                         check_name="sys_flag_data_format_auto_correction",
                                                         check_message=paste0("Correcting Local Currency reference FROM [",n,"] TO [",rl,"]\n"))))
          }
        }
      }
    }
    
    
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
                          USE.NAMES = F,
                          SIMPLIFY=F)
                   
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
      label_cols_excelnames <- openxlsx2::int2col(label_cols_index)
      
      if (label_cols_index[2]-label_cols_index[1] != 1) {
        stop(paste0("Label columns are not adjacent.  Template seems to have labels in columns ",paste0(label_cols_excelnames,collapse=" and ")," instead of next to each other?"))
      }
      
      label_cols_names <- names(label_matches)[label_cols_index]
      label_matches <- label_matches[label_cols_index]
      
      #match_id is label_header_id
      label_matches <- rbindlist(lapply(seq_along(label_cols_index),function(header_row,label_matches) { 
        x <- rbindlist(label_matches[[header_row]])
        x[,header_row:=header_row]
        return(x)
      },label_matches=label_matches))
      
      #match preference by exact index
      {
        label_matches[,selected_exact_index_match:=FALSE]
        label_matches[rsf_labels[is.na(template_header_section_index)==FALSE,
                                 .(label_header_id,
                                   match_rows=suppressWarnings(as.numeric(template_header_section_index)))], #must be a numeric row to match SUMMARY row
                      selected_exact_index_match:=TRUE,
                      on=.(match_id=label_header_id,
                           match_rows)]
      
        #if we matched the header_id but DID NOT match the exact match row ID, then omit this header entirely because its ONLY purpose is to match that label on that row number
        #and cannot introduce amibuity on other rows.
        label_matches[selected_exact_index_match==FALSE & match_id %in% rsf_labels[is.na(template_header_section_index)==FALSE,label_header_id],
                      selected_exact_index_match:=NA]
        
        label_matches <- label_matches[!is.na(selected_exact_index_match)]
      }
      
      #match preference by number of positions matched for && columns
      {
        label_matches[,
                      `:=`(positions_matched = .N,
                           all_positions_matched= all(is.na(match_position)) | all(header_row %in% match_position)),
                      by=.(match_id,match_rows)]
        
        label_matches[rsf_labels[is.na(template_header_position)==F][,.(positions_expected=max(template_header_position)),by=.(label_header_id)],
                      all_positions_matched:=all_positions_matched & i.positions_expected==positions_matched,
                      on=.(match_id=label_header_id)]
  
        #if the label specifies "this && that" and the template headers are "this && yes" then this header is an anti-match despite the partial match
        label_matches <- label_matches[all_positions_matched==T]
        
        #if there's a double header, eg, "this && that" and the header matches both, then this is a BETTER match than an ambiguous header_id that matches only 1 label
        label_matches[,
                      selected_most_positions_matched:=positions_matched==max(positions_matched),
                      by=.(match_rows)]

        
        label_matches[,
                      `:=`(positions_matched=NULL,
                           all_positions_matched=NULL)]
        
        #label_matches <- label_matches[selected_most_positions_matched==TRUE]
        
      }
      
      #Best match
      {
        label_matches[,selected_preference:=frank(.SD,-selected_exact_index_match,-selected_most_positions_matched,ties.method="dense"),
                      by=.(match_rows)]
        
        label_matches[,
                      selected:=selected_preference==min(selected_preference),
                      by=.(match_rows)]
        
        label_matches <- label_matches[selected==TRUE]
        label_matches[,
                      `:=`(selected=NULL,
                           selected_preference=NULL,
                           selected_most_positions_matched=NULL,
                           selected_exact_index_match=NULL)]
        
      }
      
      label_matches <- label_matches[rsf_labels[,.(label_header_id,action,map_indicator_id,map_formula_id,map_check_formula_id,header_id)],
                                     on=.(match_id=label_header_id),
                                     nomatch=NULL]
      #changed from header_ids=list(unique(match_id)) TO header_ids=list(unique(header_id)) 
      label_matches <- label_matches[,
                                     .(header_ids=list(unique(header_id))),
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

      
      #For defined (but unmatched!) stop row definitions.
      #It will be unmatched when the template header is set to "" or NA to simply specifiy that, eg, "SUMMARY:100" is the stop row for the template.
      stop_row <- rsf_labels[is.na(template_header_section_index)==FALSE &
                             sapply(template_section_lookup,grepl,x="summary",ignore.case=T) & #for summary tab
                             superTrim(label) %in% c("","na") &                                #for missing/NA labels
                             map_indicator_id %in% rsf_indicators[indicator_sys_category=="template_read_stop",indicator_id],
                             suppressWarnings(as.numeric(template_header_section_index))]
      
      stop_row <- na.omit(stop_row)
      if (length(stop_row) > 0) {
        stop_row <- max(stop_row) #max stop row is used for generic/missing/NA header.
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
      
      #Did we match an indicator and also its unit_fx_indicator_id? Reconcile.
      {
        #unit_fx_indicator_id is the base indicator in LCU
        unit_fx_indicators <- rsf_indicators[!is.na(unit_fx_indicator_id),.(unit_fx_indicator_id,indicator_id,data_unit,indicator_name)]
        summary_sheet[,
                    `:=`(unit_fx_defined=NA,
                         ignore=NA)]

        summary_sheet[unit_fx_indicators,
                    unit_fx_defined:=mapply(grepl,pattern=i.data_unit,x=label,MoreArgs=list(ignore.case=T)),
                    on=.(map_indicator_id=indicator_id)]

        summary_sheet[,
                    ignore:=is.na(unit_fx_defined) & any(!is.na(unit_fx_defined),na.rm=T),
                    by=.(original_row_num)]

        summary_sheet <- summary_sheet[ignore==FALSE]
        summary_sheet[,unit_fx_defined:=NULL]
      }
      
      #Will fail because its ambiguous
      {
        summary_sheet[,
                       mismatch:=anyNA(action)==FALSE & length(unique(na.omit(map_indicator_id)))>1,
                       by=.(original_row_num)]
        
        mismatch_labels <- summary_sheet[mismatch==TRUE]
        if (!empty(mismatch_labels)) {
          
          mismatch_labels[data.frame(excelCol=label_cols_excelnames,header_row=seq_along(label_cols_excelnames)),
                          excelCol:=i.excelCol,
                          on=.(header_row)]
          
          mismatch_labels[,
                          message:=paste0("Summary Sheet Row ",original_row_num," column ",excelCol," \"",label,"\" maps to \"",indicator_name,"\"")]
          setorder(mismatch_labels,
                   original_row_num,
                   header_row)
          
          mismatch_labels <- mismatch_labels[summarySheetXML,
                                             on=.(excelCol),
                                             nomatch=NULL]
          
          mismatch_labels[,hidden:=suppressWarnings(as.numeric(hidden))>0]
          mismatch_labels[,width:=suppressWarnings(as.numeric(width))]
          mismatch_labels[is.na(hidden),hidden:=FALSE]
          mismatch_labels[is.na(width),width:=0]
          
          
          mismatch_labels <- mismatch_labels[,
                          selected:=fcase(any(hidden) & !all(hidden),as.integer(header_row[which.max(!hidden)]),
                                          any(length(unique(width))>1),as.integer(header_row[which.max(width)]),
                                          default=as.integer(1)),
                          
                          by=.(original_row_num)]
          
          mismatch_labels <- mismatch_labels[,
                          .(rsf_pfcbl_id=as.numeric(NA),
                            indicator_id=map_indicator_id[selected],
                            reporting_asof_date=reporting_asof_date,
                            check_name="sys_reporting_data_discarded",
                            check_message=paste0("Excel template defines AMBIGUOUS labels on columns ",paste0(excelCol,collapse=" and "),
                                          " for value {",unique(data_value)," ",unique(data_unit),"} on: \n",
                                          paste0(message,collapse=" AND\n "),
                                          ". System is auto-selecting column ",excelCol[selected]," ",
                            fcase(any(hidden)," because the other column is hidden (so you probably have a typo or mis-entered label in your template?)",
                                  length(unique(width))>1," it is the widest width column and therefore assumed to be the focus and primary label?",
                                  default=" it is the first label and therefore assumed to be the focus and primary label?"),
                            ". If this assumption is correct, you must fix your template column labels (and the results uploaded for '",
                            indicator_name[selected],"'={",unique(data_value)," ",unique(data_unit),"} will likely result in errors")),
                          by=.(original_row_num,selected)]
          
          for (ml in mismatch_labels$check_message) { status_message(class="error",ml,"\n") }
          
          summary_sheet[mismatch_labels[,.(mismatch=TRUE,
                                           selected,
                                           original_row_num)],
                        mismatch:=!(header_row==selected),
                        on=.(mismatch,
                             original_row_num)]
          
          summary_sheet <- summary_sheet[mismatch==FALSE]
          
          mismatch_labels <- mismatch_labels[,.(rsf_pfcbl_id,
                                              indicator_id,
                                              reporting_asof_date,
                                              check_name,
                                              check_message)]
          
          reporting_flags <- rbindlist(list(reporting_flags,
                                            mismatch_labels))
          
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
      
      #Unfound: will asign to entity_reporting
      {
        summary_sheet[,
                      mixed:=length(unique(action))>1 & length(unique(map_indicator_id)) > 1 & anyNA(map_indicator_id),
                      by=.(original_row_num)]
        mixed_labels <- summary_sheet[mixed==TRUE]
        
        if (!empty(mixed_labels)) {
          
          setorder(mixed_labels,
                   original_row_num,
                   map_indicator_id,
                   na.last=T)
          
          mixed_labels <- mixed_labels[,.(rsf_pfcbl_id=rsf_pfcbl_id.facility,
                              indicator_id=map_indicator_id[1],
                              reporting_asof_date=reporting_asof_date,
                              check_name="sys_flag_indicator_ignored",
                              check_message=paste0("Summary Tab Row ",original_row_num,
                                                   paste0(paste0(" Column ",header_row," matches \"",label,"\" to ",
                                                                 ifelse(action=="ignore",paste0("IGNORE (template header ",paste0(unlist(header_ids),collapse=", "),")"),
                                                                        paste0(indicator_name," (",action,")"))),collapse=" AND "))),
                       by=.(original_row_num)]
          
          mixed_labels <- mixed_labels[,.(rsf_pfcbl_id,
                                              indicator_id,
                                              reporting_asof_date,
                                              check_name,
                                              check_message)]
          
          reporting_flags <- rbindlist(list(reporting_flags,
                                            mixed_labels))
        }
      }
      
      summary_sheet <- summary_sheet[is.na(action)==FALSE]
      summary_sheet <- summary_sheet[(action=="ignore")==FALSE]
      
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
    
    if (!any(summary_sheet$indicator_name %in% rsf_indicators[indicator_sys_category=="template_file" & data_category=="facility",indicator_name])) {
      
      next_rank <- max(suppressWarnings(as.numeric(summary_sheet[reporting_template_row_group=="1SUMMARY"]$reporting_template_data_rank)),na.rm=T)
      if (length(next_rank)==0 || is.infinite(next_rank)) { next_rank <- "1" 
      } else { next_rank <- as.character(next_rank+1) }
      
      tfdata <- data.table(reporting_template_row_group="1SUMMARY",
                           reporting_template_data_rank=next_rank,
                           indicator_name=rsf_indicators[indicator_sys_category=="template_file" & data_category=="facility",indicator_name],
                           reporting_submitted_data_value=basename(template_file),
                           reporting_submitted_data_unit=as.character(NA),
                           reporting_submitted_data_formula=as.character(NA))
      
      summary_sheet <- rbindlist(list(summary_sheet,
                                      tfdata))
    }
    
    summary_sheet
    
  }

  data.quarterly <- {
    
    
    
    qreportSheetIndex <- grep("qreport",excelwb2$sheet_names,ignore.case = T)
    
    #A real pain!
    #But with tehse 2018 version templates, columns are in English (and French) and then people copy-paste templates
    #and then the client doesn't speak French and they hide the column (or vice versa) and then they change the columns to something else entirely
    #and then one language says the data is X and the other says the data is Y and then Jason throws an error and people complain that Jason isn't working.
    #so, now we're going to test which is a hidden or nearly-hidden column and pick that in the case of an ambiguity.
    qreportSheetXML <- data.table((excelwb2$worksheets[[qreportSheetIndex]])$sheet_data$row_attr)
    
    qreportSheetXML[,excelCol:=openxlsx2::int2col(1:.N)]
    if (!any(names(qreportSheetXML)=="hidden")) qreportSheetXML[,hidden:=as.character(NA)]
    if (!any(names(qreportSheetXML)=="collapsed")) qreportSheetXML[,collapsed:=as.character(NA)]
    
    qreportSheetXML[,hidden:=as.logical(suppressWarnings(as.numeric(hidden))>0)]
    qreportSheetXML[is.na(hidden),hidden:=FALSE]
    #qreportSheetXML[,collapsed:=as.logical(suppressWarnings(as.numeric(collapsed))>0)]
    #qreportSheetXML[is.na(collapsed),collapsed:=FALSE]
    qreportSheetXML[,ht:=suppressWarnings(as.numeric(ht))]
    qreportSheetXML[is.na(ht),ht:=0]
    qreportSheetXML[,original_row_num:=as.numeric(r)]
    
    #openxlsx skipped frist row(s) so we have an offset 
    if (nrow(data_formula_matrix > 0) &&
        as.numeric(row.names(data_formula_matrix))[1] != 1) {
      
      first_row <-  as.numeric(row.names(data_formula_matrix))[1]
      
      qreportSheetXML <- qreportSheetXML[original_row_num >= first_row]
      qreportSheetXML <- qreportSheetXML[,original_row_num:=1:.N]
      
    }
    # dim(qreportSheetXML)
    # dim(data_formula_matrix)
    # dim(data_sheet)
    #Because openxlsx skips the first blank row (usually)
    
    
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
                                       USE.NAMES = F,
                                       SIMPLIFY=F)
                                
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
      
      label_matches <- label_matches[rsf_labels[,.(label_header_id,action,map_indicator_id,map_formula_id,map_check_formula_id,header_id)],
                                     on=.(match_id=label_header_id),
                                     nomatch=NULL]
      #chaned from header_ids=list(unique(match_id)) TO header_ids=list(unique(header_id)) 
      label_matches <- label_matches[,
                                     .(header_ids=list(unique(header_id))),
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
      
      #data_labels[,original_row_num:=label_rows_index]
      # melted_data_labels <- melt.data.table(data=data_labels,
      #                 id="original_row_num",
      #                 variable.factor = F,
      #                 value.factor = F,
      #                 variable.name="header_row",
      #                 value.name="label")

      #I don't know why I did it this way... just keep it for now...      
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
      
      data_labels[data.table(original_row_num=label_rows_index,
                             header_row=seq_along(label_rows_index)),
                  original_row_num:=i.original_row_num,
                  on=.(header_row)]
      
      
      
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
      
      #Did we match an indicator and also its unit_fx_indicator_id? Reconcile.
      {
        #unit_fx_indicator_id is the base indicator in LCU
        unit_fx_indicators <- rsf_indicators[!is.na(unit_fx_indicator_id),.(unit_fx_indicator_id,indicator_id,data_unit,indicator_name)]
        data_labels[,
                    `:=`(unit_fx_defined=NA,
                         ignore=NA)]
        
        data_labels[unit_fx_indicators,
                    unit_fx_defined:=mapply(grepl,pattern=i.data_unit,x=label,MoreArgs=list(ignore.case=T)),
                    on=.(map_indicator_id=indicator_id)]
        
        data_labels[,
                    ignore:=is.na(unit_fx_defined) & any(!is.na(unit_fx_defined),na.rm=T),
                    by=.(original_col_num)]
        
        data_labels <- data_labels[ignore==FALSE]
        data_labels[,unit_fx_defined:=NULL]
      }
      
      #Will fail because its ambiguous
      {
        data_labels[,
                      mismatch:=anyNA(action)==FALSE & length(unique(map_indicator_id))>1,
                      by=.(original_col_num)]
        
        mismatch_labels <- data_labels[mismatch==TRUE]
        if (!empty(mismatch_labels)) {
          
          
          mismatch_labels <- mismatch_labels[qreportSheetXML,
                          on=.(original_row_num),
                          nomatch=NULL]
          
          mismatch_labels <- mismatch_labels[,
                                             selected:=(fcase(any(hidden) & !all(hidden),as.integer(header_row[which.max(!hidden)]),
                                                             any(length(unique(ht))>1),as.integer(header_row[which.max(ht)]),
                                                             default=as.integer(1))),
                                             
                                             by=.(original_col_num)][order(original_col_num,original_row_num)]
          
          
          
          mismatch_labels[,n:=.N,
                          by=.(original_col_num,label,header_row,selected)]
          
          #one label matches multiple indicators: should be impossible, can happen due to FX unit issues (which are resolved above)
          #but just in case someone creates an indicator alias or other mapping that causes too much confusion!
          #then just pick one.
          if (any(mismatch_labels$n>1)) {
            mismatch_labels[n>1,
                            omit:=map_indicator_id==max(map_indicator_id),
                            by=.(original_col_num,label,header_row,selected)]
            mismatch_labels <- mismatch_labels[omit==F]
          }
          
          data_labels[,ignore:=FALSE]
          data_labels[mismatch_labels[header_row != selected],
                      ignore:=TRUE,
                      on=.(map_indicator_id,
                           original_col_num,
                           header_row)]
          #data_labels[ignore==T]
          
          data_labels <- data_labels[ignore==F]
          
          mismatch_labels[,check_message:=paste0("QReport Column ",original_col_num," label ",
                                                 paste0(unique(paste0("'",label,"' (row ",original_row_num,")")),collapse=" AND "),
                                                  " match: ",paste0("'",unique(indicator_name),"'",collapse=" OR "),
                                                 " Importing COL",original_col_num," into ",paste0(unique(indicator_name[header_row==selected]),collapse=", ")," because ",
                                                 fcase(any(hidden,na.rm=T),
                                                       paste0(" because ROW ",paste0(unique(header_row[header_row != selected]),collapse=" & "),
                                                       " is hidden (so you probably have a typo or mis-entered label in your template?)"),
                                                       length(unique(ht))>1," it is the highest height row and therefore assumed to be the focus and primary label?",
                                                       default=" it is the first label row and therefore assumed to be the focus and primary label?"),
                                                 " If this assumption is not correct, you must fix your template column labels (and the results uploaded for COLUMN ",
                                                 original_col_num," will likely result in errors)"),
                          by=.(original_col_num)]
          
          mismatch_labels <- mismatch_labels[header_row==selected,
                                             .(rsf_pfcbl_id=as.numeric(NA),
                                               indicator_id=map_indicator_id,
                                               reporting_asof_date=reporting_asof_date,
                                               check_name="sys_flag_indicator_not_found",
                                               check_message)]
                                               
          mismatch_labels <- unique(mismatch_labels)  
          reporting_flags <- rbindlist(list(reporting_flags,
                                            mismatch_labels))
          # # mismatch_labels <- mismatch_labels[,
          #                                    .(rsf_pfcbl_id=as.numeric(NA),
          #                                      indicator_id=map_indicator_id[selected],
          #                                      reporting_asof_date=reporting_asof_date,
          #                                      check_name="sys_reporting_data_discarded",
          # #                                      check_message=paste0("Excel template defines AMBIGUOUS labels on ",original_col_num,paste0(unique(original_row_num),collapse=" and "),
          # #                                                           ": \n",
          # #                                                           paste0(
          # #                                                             unique(paste0("ROW ",original_row_num," is '",label,"' and maps to ",indicator_name)),collapse=" AND\n "),
          # #                                                           ". System is auto-selecting ROW ",original_row_num[selected]," ",
          # #                                                           fcase(any(hidden)," because the other row is hidden (so you probably have a typo or mis-entered label in your template?)",
          # #                                                                 length(unique(ht))>1," it is the highest height row and therefore assumed to be the focus and primary label?",
          # #                                                                 default=" it is the first label row and therefore assumed to be the focus and primary label?"),
          # #                                                           ". If this assumption is correct, you must fix your template column labels (and the results uploaded for COLUMN ",
          # #                                                           original_col_num," will likely result in errors)")),
          # #                                    by=.(original_col_num,selected)]
          
          for (ml in mismatch_labels$check_message) { status_message(class="error",ml,"\n") }
          
          
          # mismatch_labels[,
          #                 message:=paste0("QReport Sheet row ",header_row," column ",original_col_num," \"",label,"\" maps to \"",indicator_name,"\"")]
          # 
          # setorder(mismatch_labels,
          #          original_col_num,
          #          header_row)
          # 
          # message <- paste0(mismatch_labels$message,collapse=" \n")
          # stop(paste0("Mismatched Column Labels:\n",
          #             "Correct the column name(s) in QReport Tab \n",
          #             "Or if this is a Template Requirement map these columns in JASON -> RSF Setup -> Setup Templates -> ",template_lookup$template_name," \n\n",
          #             message))
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
  
  
  
  
  template_data <- rbindlist(list(data.lists,
                                  data.summary,
                                  data.quarterly))
  
  template_data[,reporting_asof_date:=reporting_asof_date]
  
  template <- list(cohort_pfcbl_id=rsf_pfcbl_id.facility,
                   reporting_asof_date=reporting_asof_date,
                   template_data=template_data,
                   pfcbl_reporting_flags=reporting_flags,
                   template_headers=unique(template_headers))

  status_message(class="info","Success: Completed Parsing File:\n")
  return (template)
}
