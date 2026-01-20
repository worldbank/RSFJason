

parse_template_IFC_QR2025 <- function(pool,
                                      template_file,
                                      template_lookup=db_export_get_template(pool=pool,template_name="IFC-QR-TEMPLATE2025"),
                                      rsf_indicators=db_indicators_get_labels(pool),
                                      status_message,
                                      CALCULATIONS_ENVIRONMENT=CALCULATIONS_ENVIRONMENT) 
{
  #Read file
  {
    if (!file_ext(template_file) %in% "xlsx") stop("Only .xlsx files using Excel-365 versions or later may use this template")
    ####
    #openxlsx has some bug where it can't read some types of workbooks with pivot tables
    #https://github.com/ycphs/openxlsx/issues/124
    excelwb <- tryCatch({
      openxlsx2::wb_load(template_file)
    },
    error = function(e) { 
      stop(conditionMessage(e))
    },
    warning = function(w) { 
      suppressWarnings(openxlsx2::wb_load(template_file))
    })
    
   
    nregions_table <- openxlsx2::wb_get_named_regions(excelwb)
    setDT(nregions_table)
    nregions_table <- nregions_table[nchar(hidden)==0]
    
    snames <- openxlsx2::wb_get_sheet_names(excelwb)
    
    
  } 
  
  #Read datasets
  {
    reporting_flags <- data.table(rsf_pfcbl_id=numeric(0),
                                  indicator_id=numeric(0),
                                  reporting_asof_date=as.Date(numeric(0)),
                                  check_name=character(0),
                                  check_message=character(0))
    
    reporting_asof_date <- {
      
      data_qdd <- names(openxlsx2::wb_to_df(excelwb, named_region = "Data_QDD"))
      
      if (!length(data_qdd)==1) {
        if (length(data_qdd)==0) stop("Failed to find defined name: Data_QDD specifying the Template's Quarterly Determination Date, suggesting this template is corrupted.")
        else stop(paste0("Template defines multiple defined names for 'Data_QDD' with different values: ",paste0(data_qdd,collapse=" & "),", suggesting that this template is corrupted."))
      }
      
      data_qdd <- as.Date(data_qdd)
      data_qdd
    }
   
    #Load the data sheets (and their formulas)  
    {
      template_headers <- NULL
      
      #Summary tab
      {
        summarySheet <- grep("Summary$",snames,value=T,ignore.case = T)
        if (length(summarySheet) != 1) {
          stop("Failed to find 'Summary' Sheet in Template")
        }
        
        summary_sheet <- openxlsx2::read_xlsx(excelwb,sheet=summarySheet,row_names=F,col_names=F,detect_dates=T)
        setDT(summary_sheet)
        
        summary_formula_matrix <- openxlsx2::wb_to_df(excelwb,sheet=summarySheet,row_names=F,col_names=F,show_formula=T)
        setDT(summary_formula_matrix)
        
        if (!all(dim(summary_formula_matrix) == dim(summary_sheet))) {
          stop("Summary sheet data and formulas are mismatched")
        }
        
        summary_formula_matrix[summary_formula_matrix==summary_sheet] <-NA
        
        # summary_sheet <- template_excel_read_sheet(excelwb=excelwb,
        #                                            sheetName=summarySheet)
        # 
        # summary_formula_matrix <- openxlsx_get_formulas(excelwb=excelwb,
        #                                                 sheetName=summarySheet,
        #                                                 truncate_predata_rows = TRUE)
        setDT(summary_sheet)
        summary_sheet[,original_row_num:=.I]
      
        
      }
      
      #QReport
      {
        dataSheet <- grep("QReport$",snames,value=T,ignore.case = T)
        if (length(dataSheet) != 1) {
          stop("Failed to find 'QReport' Sheet in Template")
        }
        
        
        data_sheet <- openxlsx2::read_xlsx(excelwb,sheet=dataSheet,row_names=F,col_names=F,detect_dates=T)
        setDT(data_sheet)
        
        data_formula_matrix <- openxlsx2::wb_to_df(excelwb,sheet=dataSheet,row_names=F,col_names=F,show_formula=T)
        setDT(data_formula_matrix)
        
        if (!all(dim(data_formula_matrix) == dim(data_sheet))) {
          stop("QReport data sheet data and formulas are mismatched")
        }
        
        data_formula_matrix[data_sheet==data_formula_matrix] <-NA
        
        # data_sheet <- template_excel_read_sheet(excelwb=excelwb,
        #                                         sheetName=dataSheet)
        # 
        # data_formula_matrix <- openxlsx_get_formulas(excelwb=excelwb,
        #                                              sheetName=dataSheet,
        #                                              truncate_predata_rows = TRUE)
        setDT(data_sheet)
      }
      
      
      #Import Named Ranges into summary-level data
      {
        nregions_table[,data_value:=as.character(NA)]
        
        for (i in 1:nrow(nregions_table)) {

            n <- nregions_table[i,name]
            dval <- as.character(NA)
            tryCatch({
              x <- openxlsx2::wb_to_df(excelwb,named_region=n)
              
              if (nrow(x)==0) {
                dval <- names(x)

              } else if (ncol(x)==1) {
                
                x <- na.omit(c(names(x),unlist(x)))
                if (any(grepl(",",x))) x <- gsub(","," ",x)
                x <- paste0(x,collapse=",")
                x <- gsub("[[:space:]]{2,}"," ",x)
                dval <- x
              }
            },
            error=function(e) { },
            warning=function(w) { })
            nregions_table[i,
                     data_value:=dval]
        }
        
        list_sheet <- nregions_table[sheets=="Lists",.(name,data_value)]
        list_sheet <- list_sheet[grepl("^Template_",name)==F]
        list_sheet[,original_row_num:=.I]
      }
    }
    
    #will also omit the first rows above the Facility ID
    rsf_pfcbl_id.facility <- {
      
      
      ifcpid <- names(openxlsx2::wb_to_df(excelwb, named_region = "IFC_ProjectID"))
      #ifcpid <- nregions_table[range_name=="",range_value]
      project_id <- as.numeric(gsub("[^[:digit:]]+","",ifcpid))
      
      if (length(project_id)==0 || is.na(project_id)) {
        stop(paste0("Failed to identify IFC Project ID number from defined name RSA_IFCProjectID.  Read-in value: ",project_id," from: ",ifcpid))
      }
      rsf_pfcbl_id <- dbGetQuery(pool,"
                                      select distinct
                                      cni.rsf_pfcbl_id
                                      from p_rsf.rsf_data_current_names_and_ids cni
                                      where id = $1::text
                                        and cni.pfcbl_category = 'facility'",
                                  params=list(project_id))
      if (nrow(rsf_pfcbl_id) != 1) {
          stop(paste0("Failed to uniquely match IFC Project ID from Project ID defined in template: '",ifcpid,"'.  Has this IFC Facility been setup?"))
      }
      rsf_pfcbl_id$rsf_pfcbl_id #need this here to return it from the anonamous function
    }
    
    summary_sheet_ID_row <- {
      
      rid <- nregions_table[name=="IFC_ProjectID",coords]
      
      
      if (length(rid) !=1) stop(paste0("IFC_ProjectID defined name is defined multiple times for row(s): ",paste0(rid,collapse=" & "),". Please review defined name manager for duplicates"))
      as.numeric(openxlsx2::dims_to_rowcol(rid)$row)
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
    }
  }
  
  data.list <- {
    #new
    {
      
      label_matches <- mapply(labelMatches,
             find_sections=tolower(rsf_labels$template_section_lookup),
             find_labels=tolower(rsf_labels$template_label_lookup),
             match_id=rsf_labels$label_header_id,
             match_postion=rsf_labels$template_header_position,
             MoreArgs=list(search_sections=rep(x="lists",times=length(list_sheet$name)),
                           search_labels=normalizeLabel(list_sheet$name)),
             USE.NAMES = F)
      
      label_matches <- rbindlist(label_matches)
      
      label_matches <- label_matches[rsf_labels[,.(label_header_id,action,map_indicator_id,map_formula_id,map_check_formula_id)],
                                     on=.(match_id=label_header_id),
                                     nomatch=NULL]
      
      label_matches <- label_matches[,
                                     .(header_ids=list(unique(match_id))),
                                     by=.(original_row_num=match_rows,action,map_indicator_id,map_formula_id,map_check_formula_id)]
      
      
      list_sheet <- label_matches[list_sheet,
                                  on=.(original_row_num)]  
      

    }
    
    #Label errors/mismatching
    {
      list_sheet[,
                  ignore:=anyNA(action)==FALSE & all(action=="ignore"),
                  by=.(original_row_num)]

      list_sheet <- list_sheet[ignore==FALSE]
      
      list_sheet[rsf_indicators,
                    indicator_name:=i.indicator_name,
                    on=.(map_indicator_id=indicator_id)]
      
      #Will fail because its ambiguous: this shouldn't be possible for defined names? Unless copy-paste errors copy-in multiple defined names??
      {
        list_sheet[,
                      mismatch:=anyNA(action)==FALSE & length(unique(map_indicator_id))>1,
                      by=.(original_row_num)]
        
        mismatch_labels <- list_sheet[mismatch==TRUE]
        if (!empty(mismatch_labels)) {
          
          
          mismatch_labels[,
                          message:=paste0("List Sheet defined name \"",name,"\" maps to \"",indicator_name,"\"")]
          
          setorder(mismatch_labels,
                   original_row_num)
          
          message <- paste0(mismatch_labels$message,collapse=" \n")
          stop(paste0("Mismatched Column Labels:\n",
                      "Correct the column name(s) in Summary Tab \n",
                      "Or if this is a Template Requirement map these columns in JASON -> RSF Setup -> Setup Templates -> ",template_lookup$template_name," \n\n",
                      message))
        }
      }    
      
      #Unfound: will asign to entity_reporting
      {
        list_sheet[,
                      notfound:=all(is.na(action)),
                      by=.(original_row_num)]
        
        unfound_labels <- list_sheet[notfound==TRUE]
        
        if (!empty(unfound_labels)) {
          
          setorder(unfound_labels,
                   original_row_num)
          
          unfound_labels[,
                         `:=`(rsf_pfcbl_id=rsf_pfcbl_id.facility,
                              indicator_id=as.numeric(NA), #will be auto-assigned to reporting indicator
                              reporting_asof_date=reporting_asof_date,
                              check_name="sys_flag_indicator_not_found",
                              check_message=paste0("List Tab defined name \"",name,"\""))]
          
          unfound_labels <- unfound_labels[,.(rsf_pfcbl_id,
                                              indicator_id,
                                              reporting_asof_date,
                                              check_name,
                                              check_message)]
          
          reporting_flags <- rbindlist(list(reporting_flags,
                                            unfound_labels))
        }
      }

      list_sheet <- list_sheet[is.na(action)==FALSE]
      
      if (save_headers) {
        template_headers <- rbindlist(list(template_headers,
                                           list_sheet[!is.na(map_indicator_id),
                                                         .(label=name,
                                                           data_source_index=paste0("Lists Sheet defined name ",name),
                                                           indicator_id=map_indicator_id)]))
      }
      
      
      list_sheet <- list_sheet[is.na(map_indicator_id)==FALSE,
                                     .(indicator_name,
                                       data_unit=as.character(NA),
                                       data_value,
                                       original_row_num)]
    }
    
    {
      list_sheet <- unique(list_sheet)
      
      setnames(list_sheet,
               old=c("data_unit","data_value"),
               new=c("reporting_submitted_data_unit",
                     "reporting_submitted_data_value"))
      
      list_sheet <- unique(list_sheet)
      
      list_sheet[rsf_indicators[!is.na(data_unit),
                                   .(indicator_name,data_unit,joincondition=as.character(NA))],
                    reporting_submitted_data_unit := i.data_unit,
                    on=.(indicator_name,
                         reporting_submitted_data_unit=joincondition)]
      list_sheet[,
                 reporting_submitted_data_formula:=as.numeric(NA)]
      
      list_sheet[rsf_indicators,
                 indicator_sys_category:=i.indicator_sys_category,
                 on=.(indicator_name)]
      
      if (any(list_sheet$indicator_sys_category=="products_eligible",na.rm=T)) {
        products <- unlist(str_split(list_sheet[indicator_sys_category=="products_eligible",
                                                reporting_submitted_data_value],","))
      
        #Funded/Unfunded
        {
          if (any(list_sheet$indicator_sys_category=="products_funded",na.rm=T)) {
            list_pr <- list_sheet[indicator_sys_category=="products_funded"]
            tfproducts <- unlist(str_split(list_pr[,reporting_submitted_data_value],","))
            if (length(tfproducts) != length(products) && !all(tfproducts %in% products)) {
              stop(paste0("Eligible products are: [",paste0(products,collapse=","),"] and Funded Products are: [",paste0(tfproducts,collapse=","),"]. These lists must have equal lengths"))
            }
            is_true <- sapply(tfproducts,FUN=function(p) {
              p <- superTrim(p)
              any(p==superTrim(products),na.rm = T) |
              any(p==superTrim(c("Yes","Oui","Si","True","Sim","Ja","Da")),na.rm=T)
            })
            
            list_sheet[indicator_sys_category=="products_funded",
                       reporting_submitted_data_value:=paste0(products[is_true],collapse=",")]
            
            if (!any(list_sheet$indicator_sys_category=="products_unfunded",na.rm=T)) {
              
              list_pr[,
                      `:=`(indicator_sys_category="products_unfunded",
                           indicator_name=rsf_indicators[indicator_sys_category=="products_unfunded",indicator_name],
                           reporting_submitted_data_value=paste0(products[!is_true],collapse=","))]
              
              list_sheet <- rbindlist(list(list_sheet,
                                           list_pr))
            }
              
          }
          
          if (any(list_sheet$indicator_sys_category=="products_unfunded",na.rm=T)) {
            list_pr <- list_sheet[indicator_sys_category=="products_unfunded"]
            tfproducts <- unlist(str_split(list_pr[,reporting_submitted_data_value],","))
            if (length(tfproducts) != length(products) && !all(tfproducts %in% products)) {
              stop(paste0("Eligible products are: [",paste0(products,collapse=","),"] and Ununded Products are: [",paste0(tfproducts,collapse=","),"]. These lists must have equal lengths"))
            }
            
            is_true <- sapply(tfproducts,FUN=function(p) {
              p <- superTrim(p)
              any(p==superTrim(products),na.rm = T) |
              any(p==superTrim(c("Yes","Oui","Si","True","Sim","Ja","Da")),na.rm=T)
            })
            
            list_sheet[indicator_sys_category=="products_unfunded",
                       reporting_submitted_data_value:=paste0(products[is_true],collapse=",")]
            
            if (!any(list_sheet$indicator_sys_category=="products_funded",na.rm=T)) {
              
              list_pr[,
                      `:=`(indicator_sys_category="products_funded",
                           indicator_name=rsf_indicators[indicator_sys_category=="products_funded",indicator_name],
                           reporting_submitted_data_value=paste0(products[!is_true],collapse=","))]
              
              list_sheet <- rbindlist(list(list_sheet,
                                           list_pr))
            }
            
          }
        }
        
        #Amortizing/Revolving
        {
          if (any(list_sheet$indicator_sys_category=="products_amortizing",na.rm=T)) {
            list_pr <- list_sheet[indicator_sys_category=="products_amortizing"]
            tfproducts <- unlist(str_split(list_pr[,reporting_submitted_data_value],","))
            if (length(tfproducts) != length(products) && !all(tfproducts %in% products)) {
              stop(paste0("Eligible products are: [",paste0(products,collapse=","),"] and Amortizing Products are: [",paste0(tfproducts,collapse=","),"]. These lists must have equal lengths"))
            }
            
            is_true <- sapply(tfproducts,FUN=function(p) {
              p <- superTrim(p)
              any(p==superTrim(products),na.rm = T) |
              any(p==superTrim(c("Yes","Oui","Si","True","Sim","Ja","Da")),na.rm=T)
            })
            
            list_sheet[indicator_sys_category=="products_amortizing",
                       reporting_submitted_data_value:=paste0(products[is_true],collapse=",")]
            
            if (!any(list_sheet$indicator_sys_category=="products_revolving",na.rm=T)) {
              
              list_pr[,
                      `:=`(indicator_sys_category="products_revolving",
                           indicator_name=rsf_indicators[indicator_sys_category=="products_revolving",indicator_name],
                           reporting_submitted_data_value=paste0(products[!is_true],collapse=","))]
              
              list_sheet <- rbindlist(list(list_sheet,
                                           list_pr))
            }
            
          }
          
          if (any(list_sheet$indicator_sys_category=="products_revolving",na.rm=T)) {
            list_pr <- list_sheet[indicator_sys_category=="products_revolving"]
            tfproducts <- unlist(str_split(list_pr[,reporting_submitted_data_value],","))
            if (length(tfproducts) != length(products) && !all(tfproducts %in% products)) {
              stop(paste0("Eligible products are: [",paste0(products,collapse=","),"] and Revolving Products are: [",paste0(tfproducts,collapse=","),"]. These lists must have equal lengths"))
            }
            
            is_true <- sapply(tfproducts,FUN=function(p) {
              p <- superTrim(p)
              any(p==superTrim(products),na.rm = T) |
                any(p==superTrim(c("Yes","Oui","Si","True","Sim","Ja","Da")),na.rm=T)
            })
            
            list_sheet[indicator_sys_category=="products_revolving",
                       reporting_submitted_data_value:=paste0(products[is_true],collapse=",")]
            
            if (!any(list_sheet$indicator_sys_category=="products_amortizing",na.rm=T)) {
              
              list_pr[,
                      `:=`(indicator_sys_category="products_amortizing",
                           indicator_name=rsf_indicators[indicator_sys_category=="products_amortizing",indicator_name],
                           reporting_submitted_data_value=paste0(products[!is_true],collapse=","))]
              
              list_sheet <- rbindlist(list(list_sheet,
                                           list_pr))
            }
            
          }
        }

        #Denied/Allowed
        {
          if (any(list_sheet$indicator_sys_category=="products_undrawn_denied",na.rm=T)) {
            list_pr <- list_sheet[indicator_sys_category=="products_undrawn_denied"]
            tfproducts <- unlist(str_split(list_pr[,reporting_submitted_data_value],","))
            if (length(tfproducts) != length(products) && !all(tfproducts %in% products)) {
              stop(paste0("Eligible products are: [",paste0(products,collapse=","),"] and Undrawn Principal Denied Products are: [",paste0(tfproducts,collapse=","),"]. These lists must have equal lengths"))
            }
            
            is_true <- sapply(tfproducts,FUN=function(p) {
              p <- superTrim(p)
              any(p==superTrim(products),na.rm = T) |
              any(p==superTrim(c("Yes","Oui","Si","True","Sim","Ja","Da")),na.rm=T)
            })
            
            list_sheet[indicator_sys_category=="products_undrawn_denied",
                       reporting_submitted_data_value:=paste0(products[is_true],collapse=",")]
            
            if (!any(list_sheet$indicator_sys_category=="products_undrawn_allowed",na.rm=T)) {
              
              list_pr[,
                      `:=`(indicator_sys_category="products_undrawn_allowed",
                           indicator_name=rsf_indicators[indicator_sys_category=="products_undrawn_allowed",indicator_name],
                           reporting_submitted_data_value=paste0(products[!is_true],collapse=","))]
              
              list_sheet <- rbindlist(list(list_sheet,
                                           list_pr))
            }
            
          }
          
          if (any(list_sheet$indicator_sys_category=="products_undrawn_allowed",na.rm=T)) {
            list_pr <- list_sheet[indicator_sys_category=="products_undrawn_allowed"]
            tfproducts <- unlist(str_split(list_pr[,reporting_submitted_data_value],","))
            if (length(tfproducts) != length(products) && !all(tfproducts %in% products)) {
              stop(paste0("Eligible products are: [",paste0(products,collapse=","),"] and Revolving Products are: [",paste0(tfproducts,collapse=","),"]. These lists must have equal lengths"))
            }
            
            is_true <- sapply(tfproducts,FUN=function(p) {
              p <- superTrim(p)
              any(p==superTrim(products),na.rm = T) |
                any(p==superTrim(c("Yes","Oui","Si","True","Sim","Ja","Da")),na.rm=T)
            })
            
            list_sheet[indicator_sys_category=="products_undrawn_allowed",
                       reporting_submitted_data_value:=paste0(products[is_true],collapse=",")]
            
            if (!any(list_sheet$indicator_sys_category=="products_undrawn_denied",na.rm=T)) {
              
              list_pr[,
                      `:=`(indicator_sys_category="products_undrawn_denied",
                           indicator_name=rsf_indicators[indicator_sys_category=="products_undrawn_denied",indicator_name],
                           reporting_submitted_data_value=paste0(products[!is_true],collapse=","))]
              
              list_sheet <- rbindlist(list(list_sheet,
                                           list_pr))
            }
            
          }
        }
        
        #atrisk
        if (any(list_sheet$indicator_sys_category=="products_undrawn_atrisk",na.rm=T)) {
          list_pr <- list_sheet[indicator_sys_category=="products_undrawn_atrisk"]
          tfproducts <- unlist(str_split(list_pr[,reporting_submitted_data_value],","))
          if (length(tfproducts) != length(products) && !all(tfproducts %in% products)) {
            stop(paste0("Eligible products are: [",paste0(products,collapse=","),"] and Undrawn Principal At-Risk Products are: [",paste0(tfproducts,collapse=","),"]. These lists must have equal lengths"))
          }
          
          is_true <- sapply(tfproducts,FUN=function(p) {
            p <- superTrim(p)
            any(p==superTrim(products),na.rm = T) |
              any(p==superTrim(c("Yes","Oui","Si","True","Sim","Ja","Da")),na.rm=T)
          })
          
          list_sheet[indicator_sys_category=="products_undrawn_atrisk",
                     reporting_submitted_data_value:=paste0(products[is_true],collapse=",")]
          
        }
      }
      
      list_sheet[,
                 reporting_template_row_group:='1LISTS']
      
      list_sheet[,
                 reporting_template_data_rank:=1:.N] #1:.N instead of original row number since new data may be added in products
        
        
        
        
      }
    
    
    list_sheet <- list_sheet[,
                             .(reporting_template_row_group,
                               reporting_template_data_rank,
                               indicator_name,
                               reporting_submitted_data_value,
                               reporting_submitted_data_unit,
                               reporting_submitted_data_formula)]
    
    list_sheet[is.na(reporting_submitted_data_value),
               reporting_submitted_data_value:="N/A"]  
    
    list_sheet
  }
  
  data.summary <- {
  
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

      if (length(label_cols[label_cols > 0]) == 0) stop("Failed to indicator labels in the template Summary sheet: Expected on Column B")
      if (which.max(label_cols) != 2) stop("Indicators are expected in Column B (and Column A may be used for multi-language labeling).  However, labels appear to be entered primarily elsewhere?")
      
      label_cols <- label_cols[2]
      label_cols_index <- which(names(label_matches) %in% names(label_cols))
      
      
      label_cols_names <- names(label_matches)[label_cols_index]
      label_matches <- label_matches[label_cols_index]
      
      #match_id is label_header_id
      label_matches <- rbindlist(lapply(seq_along(label_cols_index),function(header_row,label_matches) { 
        x <- rbindlist(label_matches[[header_row]])
        x[,header_row:=header_row]
        return(x)
      },label_matches=label_matches))
      
      label_matches <- label_matches[rsf_labels[,.(label_header_id,action,map_indicator_id,map_formula_id,map_check_formula_id)],
                                     on=.(match_id=label_header_id),
                                     nomatch=NULL]
      label_matches <- label_matches[,
                                     .(header_ids=list(unique(match_id))),
                                     by=.(original_row_num=match_rows,action,map_indicator_id,map_formula_id,map_check_formula_id,header_row)]
      
      data_cols <- names(summary_sheet)[seq(from=min(label_cols_index),length.out=4)]
      data_cols <- c(data_cols,"original_row_num")
      
      formula_cols <- names(summary_formula_matrix)[seq(from=min(label_cols_index),length.out=4)]
      summary_formula_matrix <- summary_formula_matrix[,..formula_cols] #df not dt
      
      summary_sheet <- summary_sheet[,
                                     ..data_cols]
      
      summary_sheet <- setnames(summary_sheet,
                                old=names(summary_sheet),
                                new=c("label",
                                      "defined_name",
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
  
      
      all_blanks <- which(sapply(as.data.frame(is.na(t(summary_sheet[,.(label,defined_name,data_unit,data_value)]))),all))
      if (any(all_blanks)) summary_sheet <- summary_sheet[-all_blanks]
      
      no_labels <- which(sapply(as.data.frame(is.na(t(summary_sheet[,.(label,defined_name)]))),all))
      if (any(no_labels)) summary_sheet <- summary_sheet[-no_labels]
      
      #Identified section headers
      one_label_no_data <- which(sapply(as.data.frame(is.na(t(summary_sheet[,.(label,defined_name)]))),any) &
                                 sapply(as.data.frame(is.na(t(summary_sheet[,.(data_unit,data_value)]))),all))
      
      #Exclude...UNLESS we want to do something with this header label and it's matched a mapping action
      one_label_no_data <- one_label_no_data[!(summary_sheet[one_label_no_data,original_row_num %in% label_matches$original_row_num])]
      if (any(one_label_no_data,na.rm = T)) summary_sheet <- summary_sheet[-one_label_no_data]
      
      #Data values are reported in the data units column (and data "values" are themselves blank.  Ie, we've imported only units.  So interpret as values without units)
      summary_sheet[is.na(data_value) & 
                    !is.na(data_unit),
                    `:=`(data_value=data_unit,
                         data_unit=as.character(NA),
                         data_column_num=3)]
  
      #label column comes before defined_name column 
      #and therefore labels will be header_row=1
      #and defined_names will be header_row=2
      summary_sheet <- melt.data.table(summary_sheet,
                                        id=c("original_row_num","data_column_num","data_value","data_unit"), 
                                        variable.factor = F,
                                        variable.name="header_row",
                                        value.factor=F,
                                        value.name="label")
      #"header_row" will contain "label" (the typed label) and "defined_name" (if one is available)
      #and this will allow comparing the defined names and the labels they are supposed to match with.
      summary_sheet[,
                    header_row:=seq_along(unique(header_row)),
                    by=.(original_row_num)]
      summary_sheet[,header_row:=as.numeric(header_row)] #were previously column names, character data type
      
      
      summary_sheet[,label_normalized:=normalizeLabel(label)]
      
      summary_sheet[is.na(label),
                    label_normalized:=normalizeLabel("NA")]
     
      setorder(summary_sheet,
               original_row_num,
               header_row,
               na.last = TRUE)
      
      #a defind_name label is not required
      #The template uses this functionality to help users match-up where defined names may not match-up with labels they're using for formulas
      summary_sheet <- summary_sheet[!(is.na(label) & header_row==2)]
    }
    
    {
      summary_sheet <- label_matches[summary_sheet,
                                     on=.(original_row_num,
                                          header_row)]  
    }
    
    #Label errors/mismatching
    {
      
      summary_sheet[,
                     ignore:=anyNA(action)==FALSE & all(action=="ignore"),
                     by=.(original_row_num)]

      #Information before the Project ID is not structured the same and has no discrete labels.
      summary_sheet[original_row_num <= summary_sheet_ID_row &
                    original_row_num > 0,
                    ignore:=TRUE]
      
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
      
      #summary_sheet[is.na(action)==T][!(notfound|untranslated)] #Should yield an empty table: ie, no action, but represented by an error message.
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
                  data_formula:=mapply(function(i,j,x) { 
                    if (i <= 0 || j <=0) { as.character(NA) #because List data is added as negative original row number
                    } else { x[i,j] }
                  },
                   i=original_row_num,
                   j=data_column_num,
                   MoreArgs=list(x=as.matrix(summary_formula_matrix)),
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
      qreport_startrow <- as.numeric(names(openxlsx2::wb_to_df(excelwb, named_region = "Template_QReport_StartRow")))
      
      if (length(qreport_startrow)==0 || is.na(qreport_startrow)) stop("Failed to find defined name range Template_QReport_StartRow in template. Has template been modified or corrupted?")
      
      label_matches <- lapply(as.data.frame(t(data_sheet[1:(qreport_startrow-1)])),
                              FUN=function(x,find_sections,find_labels,match_id,match_postion) {
                                
                                mapply(labelMatches,
                                       find_sections=find_sections,
                                       find_labels=find_labels,
                                       match_id=match_id,
                                       match_postion=match_postion,
                                       MoreArgs=list(search_sections=rep(x="qreport",times=length(x)), #lower case because of tolower() and normalizeLabel
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
      
      if (length(label_rows[label_rows > 0]) ==0) stop(paste0("Failed to find any QReport indicator labels in the template (which are expected to be found on QReport sheet row ",(qreport_startrow-1),")"))

      #Teams can enter bi-lingual headers on other rows, if they like
      if (which.max(label_rows) != (qreport_startrow-1)) stop(paste0("Template defined name Template_QReport_StartRow specifies QReport headers start on ROW ",(qreport_startrow-1),
                                                                     ". But most headers are on ROW ",which.max(label_rows),
                                                                     ". Has the template been modified or corrupted? Otherwise, ensure headers are on ROW ",qreport_startrow-1," and facility ",
                                                                     "data starts ",qreport_startrow))
      
      
      label_rows <- label_rows[label_rows > 0]
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
      data_sheet_cols <- names(data_sheet)
      data_formulas <- data_formula_matrix[data_rows,..data_sheet_cols]
      
      setnames(data_formulas,
               old=names(data_formulas),
               new=names(data_sheet))
      
      label_matches <- rbindlist(lapply(seq_along(label_rows),function(header_row,label_matches) { 
        x <- rbindlist(label_matches[[header_row]])
        x[,header_row:=header_row]
        return(x)
        },label_matches=label_matches))
      
     
      
      label_matches <- unique(label_matches)
      
      label_matches <- label_matches[rsf_labels[,.(label_header_id,action,map_indicator_id,map_formula_id,map_check_formula_id)],
                                     on=.(match_id=label_header_id),
                                     nomatch=NULL]
      
      label_matches <- label_matches[,
                                     .(header_ids=list(unique(match_id))),
                                     .(original_col_num=openxlsx2::int2col(match_rows),header_row,
                                       action,map_indicator_id,map_formula_id,map_check_formula_id)]
    }

    
    #data labels
    {

      no_data_reported <- which(sapply(data_sheet,function(x) all(is.na(x)),USE.NAMES = T))
      no_data_reported <- data.table(rn=names(no_data_reported),original_col_num=openxlsx2::int2col(no_data_reported))
      
      data_cols_names <- names(label_cols_index)
      
      data_labels <- data_labels[,
                                 ..data_cols_names]
      
      data_labels <- as.data.table(as.data.frame(t(data_labels)),
                                   keep.rownames = T)
      
      setnames(data_labels,
               old=c("V1"),
               new=c("label"))

      data_labels <- melt.data.table(data_labels,
                           id="rn",
                           variable.factor = F,
                           value.factor = F,
                           variable.name = "header_row",
                           value.name="label")

      setnames(data_labels,
               old="rn",
               new="original_col_num")      
      
      
      
      # data_labels[,
      #              original_col:=as.numeric(openxlsx2::col2int(rn))]
      
      #defined names aren't matched for QReport template
      #Maybe that should change?
      data_labels[,
                    header_row:=seq_along(unique(header_row)),
                    by=.(original_col_num)]
      
      data_labels[,header_row:=as.numeric(header_row)] #were previously column names, character data type
      
      
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
      
      data_labels[,
                  ignore:=FALSE]
      data_labels[ignore==FALSE,
                  ignore:=anyNA(action)==FALSE & all(action=="ignore"),
                  by=.(original_col_num)]
      
      data_labels[ignore==FALSE,
                  ignore:=all(is.na(action)) & all(no_data),
                  by=.(original_col_num)]
      
      
      data_labels[,semi_missing:=any(is.na(label)) & !all(is.na(label)),
                  by=.(original_col_num)]
      
      
      data_labels[is.na(label) & semi_missing==TRUE,
                  ignore:=TRUE]
      
      data_labels[,semi_missing:=NULL]
      #data_labels[ignore==T]
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
                              check_message=paste0("QReport Column ",original_col_num," Row ",header_row,"  \"",label,"\""))]
          
          unfound_labels <- unfound_labels[,.(rsf_pfcbl_id,
                                              indicator_id,
                                              reporting_asof_date,
                                              check_name,
                                              check_message)]
          
          reporting_flags <- rbindlist(list(reporting_flags,
                                            unfound_labels))
        }
      }
      
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
      
      data_labels <- data_labels[,
                                 ignore:=is.na(action) | action=="ignore"]
      
      #Unfound: will asign to entity_reporting
      {
        mismatched_labels <- data_labels[is.na(action)]
        
        if (!empty(mismatched_labels)) {
          mismatched_labels[data_labels[ignore==FALSE,.(indicator_name=paste0(indicator_name,collapse=" & ")),by=.(original_col_num)],
                         matched_indicator:=i.indicator_name,
                         on=.(original_col_num)]
          
          setorder(mismatched_labels,
                   original_col_num,
                   header_row)
          
          mismatched_labels[,
                         `:=`(rsf_pfcbl_id=rsf_pfcbl_id.facility,
                              indicator_id=as.numeric(NA), #will be auto-assigned to reporting indicator
                              reporting_asof_date=reporting_asof_date,
                              check_name="sys_flag_indicator_ignored",
                              check_message=paste0("Ignored \"",label,"\" on QReport Column ",original_col_num," because matched \"",matched_indicator,"\""))]
          
          mismatched_labels <- mismatched_labels[,.(rsf_pfcbl_id,
                                              indicator_id,
                                              reporting_asof_date,
                                              check_name,
                                              check_message)]
          
          reporting_flags <- rbindlist(list(reporting_flags,
                                            mismatched_labels))
        }
      }
      
      data_labels <- data_labels[ignore==FALSE,
                                 .(original_col_num,
                                   action,
                                   map_indicator_id,
                                   map_formula_id,
                                   map_check_formula_id,
                                   indicator_name,
                                   label_unit)]
      
      data_labels <- unique(data_labels)
      
  
    }
    
    
    #data_cols_index <- which(names(data_sheet) %in% data_cols_names)
    # rename <- unique(data_labels[,.(rn,original_col_num)])
    # setnames(data_sheet,
    #          old=rename$rn,
    #          new=rename$original_col_num)

    # setnames(data_formulas,
    #          old=rename$rn,
    #          new=rename$original_col_num)
    # 
    data_cols_names <- data_labels$original_col_num
    data_cols_names <- data_cols_names[order(nchar(data_cols_names),data_cols_names)]
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
  
  template_data <- rbindlist(list(list_sheet,
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
