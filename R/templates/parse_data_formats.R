#TODO:
#Efficiency gains: just check each unique data point per indicator, not the full list of columns.  Then re-join errors or corrections.
#Efficienct gains: don't check where formats are valid, eg, where as.numeric(X) != NA, skip those

parse_data_formats <- function(template_data, #parses the dataset instead of the template as it's used elsewhere external to templates, ie, in calculations
                               rsf_indicators,
                               sys_indicator_format_check=FALSE,
                               DATA_DEFINED_BLANKS=c("N/A","NA","N.A.","N A","N.A",
                                                     "","''",'""',"-",
                                                     "NULL",
                                                     "ERROR","#ERROR","#REF",
                                                     "{MISSING}","{VOID}","{BLANK}","{DELETED}","{NA}"),
                               DATA_DEFINED_LOCAL_CURRENCY_UNITS=c("LCU",
                                                                   "LCY"),
                               DATA_MAX_DECIMALS=CALCULATIONS_ENVIRONMENT$MAX_DECIMALS,
                               DATA_SIG_DIGITS=CALCULATIONS_ENVIRONMENT$SIG_DIGITS,
                               VALID_CURRENCIES=CALCULATIONS_ENVIRONMENT$VALID_CURRENCIES) {
  
  # ld <<- as.data.frame(template_data)
  # ri <<- as.data.frame(rsf_indicators)
  t1 <- Sys.time()

  #template_data <- as.data.table(ld)
  #rsf_indicators <- as.data.table(ri)
  
  #SETUPS
  {
    NON_NUMERIC_DATA_TYPES <- c("text","logical","date")
    NUMERIC_DATA_TYPES <- c("percent","currency","number","currency_ratio")
    
    
    if (!inherits(template_data,"data.table")) stop("Input data must be type data.table")
    
    if (!all(c("indicator_id",
               "reporting_submitted_data_value",
               "reporting_submitted_data_unit") %in% names(template_data))) stop("Input data must define 'indicator_id' and 'reporting_submitted_data_value' and 'reporting_submitted_data_unit'")
    
    if (any(c("data_value","data_unit") %in% names(template_data))) stop("Input data must NOT define data_value or data_unit: these are output results of parse_data_formats().  Rename columns as reporting_submitted_data_value and reporting_submitted_data_unit")
    
    if (empty(template_data)) stop("Failed to parse data formats: dataset contains no data")
    
    if (is.null(template_data$reporting_submitted_data_formula)) template_data[,reporting_submitted_data_formula:=as.character(NA)]
    
    indicator_data_flags <- data.table(parse_id=numeric(0),
                                       check_name=character(0),
                                       check_message=character(0))
  
    template_data[,data_id:=1:.N]
    if (!all(c("data_category","indicator_sys_category") %in% names(template_data))) {
      template_data[rsf_indicators,
                    `:=`(data_category=i.data_category,
                         indicator_sys_category=i.indicator_sys_category),
                    on=.(indicator_id)]
    }
    
    indicator_data <- template_data[,
                                    .(reporting_template_row_group,
                                      indicator_id,
                                      indicator_name,
                                      indicator_sys_category,
                                      data_category,
                                      data_id,
                                      reporting_submitted_data_unit,
                                      reporting_submitted_data_value,
                                      reporting_submitted_data_formula)]
    
    indicator_data <- indicator_data[,
                                     .(data_ids=list(c(data_id))),
                                     by=.(indicator_id,
                                          indicator_name,
                                          indicator_sys_category,
                                          data_category,
                                          reporting_submitted_data_unit,
                                          reporting_submitted_data_value,
                                          reporting_submitted_data_formula)]
    indicator_data[,`:=`(parse_id=1:.N,
                         data_value=as.character(NA),
                         parsed=FALSE)]
    
    reference_indicators <- rsf_indicators[unique(indicator_data[,.(indicator_id)]),
                                             .(indicator_id,
                                               indicator_name,
                                               indicator_sys_category,
                                               is_system,
                                               data_type,
                                               data_unit,
                                               options_group_id,
                                               options_group,
                                               options_group_name,
                                               indicator_options_group_allows_blanks,
                                               indicator_options_group_allows_multiples,
                                               default_value),
                                             on=.(indicator_id),
                                             nomatch=NULL]
  
    #units not really fully implemented yet.  Mostly intended for fx
    #submitted units should not be part of options groups
  
    indicator_data[reference_indicators,
                    `:=`(data_type=i.data_type,
                         default_data_unit=i.data_unit),
                    on=.(indicator_id)]
    
    indicator_data[is.na(reporting_submitted_data_unit) & 
                   !is.na(default_data_unit),
                   reporting_submitted_data_unit:=default_data_unit]
    
    indicator_data[,reporting_submitted_data_unit:=toupper(gsub("[[:space:]]+","",superTrim(reporting_submitted_data_unit)))]
    indicator_data[,data_unit:=reporting_submitted_data_unit]
    
    #A data_unit indicator type should submit its unit classification as its data value (not as its data unit)
    #So if they were submitted in the inverse, switch them.
    indicator_data[indicator_id %in% rsf_indicators[is_data_unit==TRUE,indicator_id] &
                   (is.na(reporting_submitted_data_value) | nchar(reporting_submitted_data_value)==0) &
                   (!is.na(reporting_submitted_data_unit) & nchar(reporting_submitted_data_unit) > 0),
                   `:=`(reporting_submitted_data_value=reporting_submitted_data_unit,
                        reporting_submitted_data_unit=as.character(NA))]
    
    #If the value is actually a data unit, it should comply with the same data unit formatting criteria (ie, be uppercase)
    indicator_data[indicator_id %in% rsf_indicators[is_data_unit==TRUE,indicator_id],
                   reporting_submitted_data_value:=toupper(gsub("[[:space:]]+","",superTrim(reporting_submitted_data_value)))]
    
    ###data_value_has_default_unit
    #The default data unit for the indicator is reported as a value within the reported data:
    #In this case, omit it with replacing the default unit with ""
    #Because these can result in double-inserting units by the parsing below
    {
      indicator_data_units <- indicator_data[!is.na(data_unit) & !is.na(reporting_submitted_data_value),unique(data_unit)]
      indicator_data_units <- paste0("[[:space:]]*",indicator_data_units,"[[:space:]]*$")

      for (unit_rxp in indicator_data_units) {
        indicator_data[!is.na(data_unit) &
                       !is.na(reporting_submitted_data_value) &
                       grepl(unit_rxp,data_value,ignore.case=T),
                       reporting_submitted_data_value:=gsub(unit_rxp,"",data_value,
                                                            ignore.case = T)]
      }
    }  
    
    indicator_data[,data_unit_validated:=is.na(data_unit)]
    
    
    #PARSE/VALID indicator default values (should happen in UI, but just to ensure are normalized here where used in comparisons)
    {
      recognized_blanks <- data.table(defined_blank=na.omit(unique(superTrim(c(DATA_DEFINED_BLANKS,'NA')))))
      
      reference_indicators[!is.na(default_value) 
                           & nchar(superTrim(default_value))==0,
                           default_value:=as.character(NA)]
      
      #hold off on this because Transport 2-letter sector code options group value is "NA", so don't want to make it missing--user can never submit it.
      #reference_indicators[superTrim(default_value) %in% recognized_blanks$defined_blank,default_value:=as.character(NA)]
      
      reference_indicators[is.na(indicator_options_group_allows_blanks),
                           indicator_options_group_allows_blanks:=FALSE]
      
      reference_indicators[is.na(indicator_options_group_allows_multiples),
                           indicator_options_group_allows_multiples:=FALSE]
    }  
    
    #System data, if it happens to be passed in: these will ultimately be removed so just mark as parsed
    #And bad indicators (ie, missing indicator_id)
    {
      if (sys_indicator_format_check==FALSE & any(reference_indicators$is_system==TRUE)) {
        indicator_data[reference_indicators[is_system==TRUE,.(indicator_id)],
                       `:=`(parsed=TRUE,
                            data_unit_validated=TRUE,
                            data_value=reporting_submitted_data_value),
                       on=.(indicator_id)]
        reference_indicators <- reference_indicators[is_system==FALSE]
      }
      
      if (anyNA(indicator_data$indicator_id)) {
        indicator_data[is.na(indicator_id),
                       `:=`(parsed=TRUE,
                            data_unit_validated=TRUE,
                            data_value=reporting_submitted_data_value)]
      }
    }
  }
  
    
  #PARSE OPTIONS GROUPS INPUT DATA
  {
    #If we have any options groups to parse
    if (any(!is.na(reference_indicators$options_group_id))) {
      and_or_replacement <- "^([^[:punct:][:space:]]+)[[:space:]]*([[:punct:]])[[:space:]]*([^[:punct:][:space:]]+)$"
      multiples_delimiters <- "[,&]"
      
      #unique "by" because data.table can't apply unique() to list types (ie, the options group labels)
      reference_options_groups_labels <- unique(reference_indicators[!is.na(options_group_id),
                                                                    .(options_group_id,
                                                                      options_group_name,
                                                                      options_group)],
                                               by="options_group_id")
      
      reference_options_groups_labels <- reference_options_groups_labels[,
                                                                         unlist(options_group,recursive=F),
                                                                         by=.(options_group_id,
                                                                              options_group_name)]
      setnames(reference_options_groups_labels,
               old=c("label","options_group_key"),
               new=c("valid_value","key_value"))
      
      reference_options_groups_labels <- reference_options_groups_labels[,.(options_group_id,
                                                                            options_group_name,
                                                                            key_value,
                                                                            valid_value)]
      reference_options_groups_labels <- unique(reference_options_groups_labels)
      
      #reference_options_groups_labels <- rbindlist(reference_options_groups_labels$options_group)
      
      #reference_options_groups_labels <- rbindlist(reference_options_groups_labels$labels) #Just need primary and secondary label(s), especially if redundant across langauges
      
      # reference_options_groups_labels <- rbindlist(list(reference_options_groups_labels[,.(options_group_id,key_value=options_group_key,valid_value=primary_label)],
      #                                                   reference_options_groups_labels[!is.na(secondary_labels),.(options_group_id,key_value=options_group_key,valid_value=secondary_labels)],
      #                                                   reference_options_groups_labels[,.(options_group_id,key_value=options_group_key,valid_value=options_group_key)])) #input is allowed to directly match the key 
    
      reference_options_groups_labels[,valid_value:=superTrim(valid_value)] #to lower case and punctuation formatting
      reference_options_groups_labels[superTrim(key_value)=="",
                                      key_value:=NA]
      
      reference_options_groups_labels[,valid_value:=gsub(and_or_replacement,"\\1\\2\\3",valid_value)]  
      
      reference_options_groups_labels <- unique(reference_options_groups_labels)
      
      #Eg, see options_group on sector codes, that TELCOM's sector code is NA, so if data submits a value that uses this options group, then "NA" input should resolve to "TELCOM" and not to {MISSING}
      reference_options_groups_labels[,valid_blank:=FALSE]
      reference_options_groups_labels[recognized_blanks,
                                      valid_blank:=is.na(key_value),
                                      on=.(valid_value=defined_blank)]
    
      options_data <- indicator_data[,.(parse_id,
                                        indicator_id,
                                        reporting_submitted_data_value,
                                        reporting_submitted_data_unit) #options data should NOT be able to submit units!  Write/overwrite with indicator
                                     ][reference_indicators[,.(indicator_id,
                                                               indicator_name,
                                                               options_group_id,
                                                               indicator_options_group_allows_blanks,
                                                               indicator_options_group_allows_multiples,
                                                               default_value,
                                                               default_unit=data_unit)],
                                     on=.(indicator_id)
                                     ][!is.na(options_group_id),
                                       .(parse_id,
                                         indicator_name,
                                         options_group_id,
                                         indicator_options_group_allows_blanks,
                                         indicator_options_group_allows_multiples,
                                         default_value,
                                         default_unit,
                                         reporting_submitted_data_value,
                                         reporting_submitted_data_unit)]
                                     
    
      options_data[,normalized_submitted_data_value:=superTrim(reporting_submitted_data_value)]
      options_data[,normalized_submitted_data_value:=gsub(and_or_replacement,"\\1\\2\\3",normalized_submitted_data_value)]
      
      options_data[,
                   `:=`(matched=FALSE,
                        data_value=as.character(NA))
                   ]
      
      options_data[reference_options_groups_labels,
                   `:=`(matched=TRUE,
                        data_value=i.key_value,
                        valid_blank=i.valid_blank),
                   on=.(options_group_id,
                        normalized_submitted_data_value=valid_value)]
      
      #OPTIONS GROUPS THAT ALL MULTIPLE INPUTS (delimited by & or ,)
      {
        options_data_multiples <- options_data[matched==FALSE 
                                               & indicator_options_group_allows_multiples==TRUE 
                                               & grepl(multiples_delimiters,normalized_submitted_data_value)]
        
        if (!empty(options_data_multiples)) {
          options_data_multiples <- options_data_multiples[,.(normalized_submitted_data_value=unlist(
                                                                                                  strsplit(
                                                                                                    normalized_submitted_data_value,split=multiples_delimiters
                                                                                                  ),
                                                                                                recursive=F)),
                                 by=.(parse_id,
                                      indicator_name,
                                      options_group_id,
                                      matched,
                                      data_value)]
          
          options_data_multiples[,normalized_submitted_data_value:=superTrim(gsub(and_or_replacement,"\\1\\2\\3",normalized_submitted_data_value))]
        
          options_data_multiples[reference_options_groups_labels,
                                 `:=`(matched=TRUE,
                                      data_value=i.key_value),
                                 on=.(options_group_id,
                                      normalized_submitted_data_value=valid_value)]
          
        
          bad_matches <- options_data_multiples[matched==FALSE]
          if (!empty(bad_matches)) {
            bad_matches_flags <- bad_matches[,.(parse_id,
                                                check_name="sys_flag_data_invalid_value",
                                                check_message=paste0(indicator_name,
                                                                    " allows multi-select choices but value {",toupper(normalized_submitted_data_value),"} is invalid and ignored"))]
            indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                                   bad_matches_flags))
            
            bad_matches_flags <- NULL
          }
          bad_matches <- NULL
        
          options_data_multiples <- options_data_multiples[matched==TRUE][order(parse_id,data_value),.(data_value=paste0(data_value,collapse=" & ")),by=.(parse_id,matched)]
          
          options_data[options_data_multiples,
                       `:=`(matched=i.matched,
                            data_value=i.data_value),
                       on=.(parse_id)]
        }        
  
      }
      
      #(1) We didn't match it, so data_value is <NA>
      #(2) But NAs are allowed for this indicator's options group, so no problem...
      #(3) But if we're blank and we have a non-blank default value, then use the default value and raise info flag.
      
      #(1) We didn't match it, so data_value is <NA>
      #(2) And NAs are not allowed for this indicator's options group.
      #(3) If we're blank and we have a non-blank default value, then use the default value
      #(4) But if we're NA and NAs aren't allowed and we don't have a defualt, then we have a problem
      
      bad_matches <- options_data[matched==FALSE]
      options_data[matched==FALSE,
                   `:=`(data_value=default_value,
                        data_unit=default_unit)]
      
      if (!empty(bad_matches)) {
        
        if (!empty(bad_matches[!is.na(default_value)])) {
          
          bad_matches_flags <- bad_matches[!is.na(default_value),
                                           .(parse_id,
                                             check_name="sys_flag_data_missing_value_using_default",
                                             check_message=paste0(indicator_name,": ",
                                                                "from {",ifelse(is.na(reporting_submitted_data_value),'MISSING',reporting_submitted_data_value),"}",
                                                                " to ",
                                                                "{",default_value,"}"))]
          
          indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                                 bad_matches_flags))
          bad_matches_flags <- NULL
        }
        
        if (!empty(bad_matches[is.na(default_value)
                               & indicator_options_group_allows_blanks==FALSE])) {
        
          bad_matches_flags <- bad_matches[is.na(default_value) & indicator_options_group_allows_blanks==FALSE,
                                           .(parse_id,
                                             check_name="sys_flag_data_invalid_value",
                                             check_message=paste0(indicator_name,": ",
                                                              "invalid value {",ifelse(is.na(reporting_submitted_data_value),'MISSING',reporting_submitted_data_value),"}",
                                                              " and {MISSING} values are not allowed for this indicator"))]
          
          indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                                 bad_matches_flags))
          bad_matches_flags <- NULL
        }
        
      }
      bad_matches <- NULL
      
      bad_units <- options_data[!is.na(data_unit) & 
                                (is.na(default_unit) | default_unit != data_unit)]
      
      if (!empty(bad_units)) {
        
        bad_units_flags <- bad_units[,
                                     .(parse_id,
                                       check_name="sys_flag_data_invalid_unit",
                                       check_message=paste0(indicator_name,": ",
                                                            "invalid unit {",reporting_submitted_data_unit,"}",
                                                            " using indicator-defined unit {",ifelse(is.na(defulat_unit),"NONE",default_unit),"}"))]
        
        indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                               bad_units_flags))
        bad_units_flags <- NULL
      }
      
      bad_units <- NULL
      
      indicator_data[options_data,
                     `:=`(data_value=i.data_value,
                          data_unit=i.data_unit,
                          data_unit_validated=TRUE,
                          parsed=TRUE),
                     on=.(parse_id)]
    }
  }
  
  #REMAINING UNPARSED "REGULAR" DATA (non-System, non-Options)
  {
    regular_data <- indicator_data[parsed==FALSE,
                                   .(parse_id,
                                     indicator_id,
                                     reporting_submitted_data_value,
                                     reporting_submitted_data_unit,
                                     data_value,
                                     data_unit,
                                     parsed,
                                     data_unit_validated)
                                   ][reference_indicators[,.(indicator_id,
                                                             indicator_name,
                                                             indicator_sys_category,
                                                             data_type,
                                                             default_value,
                                                             default_unit=data_unit)],
                                     on=.(indicator_id),
                                     nomatch=NULL]

    
    if (!empty(regular_data)) {
      
      regular_data[,`:=`(normalized_submitted_data_value=superTrim(reporting_submitted_data_value),
                         data_value=trimws(reporting_submitted_data_value,whitespace="[ \\t\\r\\n\\v\\h\\s[:space:]]"))]
      
      #User has put data value and data unit as the same, so assume it's the value they're reporting and have duplicated.
      regular_data[superTrim(reporting_submitted_data_value)==superTrim(reporting_submitted_data_unit),
                   data_unit:=default_unit]
      
      regular_data[nchar(normalized_submitted_data_value)==0,
                   `:=`(parsed=TRUE,
                        data_value=as.character(NA),
                        data_unit=default_unit)]
      
      regular_data[recognized_blanks,
                   `:=`(parsed=TRUE,
                        data_value=as.character(NA),
                        data_unit=default_unit),
                   on=.(normalized_submitted_data_value=defined_blank)]
      
      default_data <- regular_data[(is.na(data_value) & !is.na(default_value))]
      
      regular_data[(is.na(data_value) & !is.na(default_value)),
                   `:=`(data_value=default_value,
                        data_unit=default_unit)]

      if (!empty(default_data)) {
          
        default_data_flags <- default_data[,.(parse_id,
                                              check_name="sys_flag_data_missing_value_using_default",
                                             check_message=paste0(indicator_name,": ",
                                                              "from {",ifelse(is.na(data_value),'MISSING',reporting_submitted_data_value),"}",
                                                              " to ",
                                                              "{",ifelse(is.na(default_value),'MISSING',default_value),
                                                                  ifelse(is.na(default_unit),'',default_unit),
                                                              "}"))]
          
        indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                               default_data_flags))
        default_data_flags <- NULL
      }
      default_data <- NULL
      
      #FORMATTING BY DATA TYPE
      reference_data_types <- regular_data[,unique(data_type)]
      
      
      
      
      #Numerics for decimal separator 
      if (any(reference_data_types %in% c("number","currency","currency_ratio","percent"))) {
        
        sci_notation <- grep("\\d+e-\\d+$",regular_data$data_value,ignore.case = T,perl=T)
        if (length(sci_notation) > 0) {
          sci_notation <- sci_notation[!is.na(suppressWarnings(as.numeric(regular_data$data_value[sci_notation])))]
          if (length(sci_notation) > 0) {
            set(regular_data,
                i=sci_notation,
                j="data_value",
                value=as.character(suppressWarnings(as.numeric(regular_data$data_value[sci_notation]))))
          }
        }
        
        #Units are included (assumes any unit has at least one :alpha: letter), ie, any submited number that has a letter, has submitted units
        words_data_numerics <- regular_data$data_type %in% c("number","currency","currency_ratio","percent") &
                               is.na(regular_data$data_value)==FALSE &
                               grepl("[[:alpha:]]",regular_data$data_value,perl=T)
        
        if (any(words_data_numerics)) {
          words_data_numerics <- words_data_numerics & grepl("zero|one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|teen|first|second|third|fifth|ninth",
                                                             regular_data$data_value,perl=T,ignore.case = T)
          regular_data[words_data_numerics,
                       data_value:=sapply(data_value,words_to_numbers)]
        }
        
        #Units are included (assumes any unit has at least one :alpha: letter), ie, any submited number that has a letter, has submitted units
        units_data_numerics <- regular_data$data_type %in% c("number","currency","currency_ratio","percent") &
                               is.na(regular_data$data_value)==FALSE &
                               grepl("[[:alpha:]]",regular_data$data_value,perl=T)
        
        if (any(units_data_numerics)) {
          #eg, USD or USD/EUR
          prefix_units <- units_data_numerics & grepl("^[[:alpha:]/]+[[:space:]]*[[:digit:],\\.+-]+$",regular_data$data_value,perl=T)
          
          #Eg, EUR5,000 will make unform: 5,000EUR
          if (any(prefix_units)) {
            regular_data[prefix_units,
                         data_value:=gsub("^([[:alpha:]/]+)[[:space:]]*(.*)$","\\2\\1",data_value,perl=T)]
          }

          regular_data[units_data_numerics,
                       `:=`(data_value=trimws(gsub("^([^[:alpha:]]+).*$","\\1",data_value,perl=T)),
                            data_unit=trimws(toupper(gsub("^[^[:alpha:]]+(.*)$","\\1",data_value,perl=T))))]
        }
        #Numeric data where user has submitted placeholder, eg, "1,000" to parse these out.  But since users might enter data
        #in either US or EU standards and interchange 1,000.00 vs 2.000.000,03 try to suss this out and convert to 1000.00 and 2000000.03, etc
        regular_data_numerics <- regular_data[data_type %in% c("number","currency","currency_ratio","percent")
                                              & is.na(data_value)==FALSE
                                              & grepl("[,\\.]",data_value,perl=T)==TRUE,
                                              .(parse_id,
                                                indicator_name,
                                                reporting_submitted_data_value,
                                                data_value)]
        
        regular_data_numerics[,thousands_sep:=as.character(NA)]
        
        #if both , and . are used, then the first one must be thousands sep and the second one must be decimal sep
        regular_data_numerics[is.na(thousands_sep) & grepl(",\\d{3}\\.",data_value,perl=T),thousands_sep:="comma"]
        regular_data_numerics[is.na(thousands_sep) & grepl("\\.\\d{3},",data_value,perl=T),thousands_sep:="period"]
        
        #if multiple thousands-seps are used, then that must be the thousands sep
        regular_data_numerics[is.na(thousands_sep) & grepl(",\\d{3},",data_value,perl=T),thousands_sep:="comma"]   
        regular_data_numerics[is.na(thousands_sep) & grepl("\\.\\d{3}\\.",data_value,perl=T),thousands_sep:="period"]   #if multiple thousands-seps are used, then that must be the units sep 
        
        #A bit of a guess work, but a pretty good guess that if , or . is followed by 000 that it's a thousands sep and not specifying a decimal that is precisely .000
        #Assume that period separator is less common, so require that at least one period was previously identified
        any_periods <- any(regular_data_numerics$thousands_sep=="period")
        if (is.na(any_periods)) any_periods <- FALSE

        any_commas <- any(regular_data_numerics$thousands_sep=="comma")
        if (is.na(any_commas)) any_commas <- FALSE
        
        #comma comes first and is less strict since it is a stronger assumption
        regular_data_numerics[is.na(thousands_sep) &
                                grepl(",000$",data_value,perl=T) & 
                                !any_periods,
                              thousands_sep:="comma"]
        
        regular_data_numerics[is.na(thousands_sep) &
                              grepl("\\.000$",data_value,perl=T) &
                              !any_commas,
                              thousands_sep:="period"]
        
        if (anyNA(regular_data_numerics$thousands_sep)) {
          unambiguous_sep <- unique(regular_data_numerics[!is.na(thousands_sep),thousands_sep])
          if (length(unambiguous_sep)==1) {
            regular_data_numerics[is.na(thousands_sep),
                                  thousands_sep:=unambiguous_sep]
          }
        }
        
        has_subs <- !is.na(regular_data_numerics$thousands_sep)     &
                    regular_data_numerics$thousands_sep == "comma"  &
                    grepl(",",regular_data_numerics$data_value,perl=T)
        
        regular_data_numerics[has_subs,
                              `:=`(data_value=gsub(",","",data_value,perl=T),
                                   thousands_sep=as.character(NA))]
        
        subs_flags <- regular_data_numerics[which(has_subs),
                                            .(parse_id,
                                              check_name="sys_flag_data_format_auto_correction",
                                              check_message=paste0(indicator_name,": ",
                                                                   "from {",reporting_submitted_data_value,"} ",
                                                                   "to {",ifelse(is.na(data_value),"MISSING",data_value),"}"))]
        
        indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                               subs_flags))
        
        has_subs <- !is.na(regular_data_numerics$thousands_sep)     &
                    regular_data_numerics$thousands_sep == "period" &
                    grepl("[\\.,]",regular_data_numerics$data_value,perl=T)
        
        regular_data_numerics[has_subs,
                              `:=`(data_value=gsub(",","\\.",
                                                   gsub("\\.","",data_value,perl=T),perl=T), #place both thousands separator "." with "" and also decimal sep of "," with "."
                                   thousands_sep=as.character(NA))]
        
        subs_flags <- regular_data_numerics[which(has_subs),
                                            .(parse_id,
                                              check_name="sys_flag_data_format_auto_correction",
                                              check_message=paste0(indicator_name,": ",
                                                                   "from {",reporting_submitted_data_value,"} ",
                                                                   "to {",ifelse(is.na(data_value),"MISSING",data_value),"}"))]
        
        has_zeros <- grepl("\\.$|\\.0+",regular_data_numerics$data_value,perl=T)
        if (any(has_zeros)) {
          regular_data_numerics[has_zeros,
                                data_value:=gsub("\\.$|\\.0+$","",data_value,perl=T)]
        }
        
        has_zeros <- grepl("\\.[1-9]+0+$",regular_data_numerics$data_value,perl=T)
        if (any(has_zeros)) {
          regular_data_numerics[has_zeros,
                                data_value:=gsub("(\\.[1-9]+)0+$","\\1",data_value,perl=T)]
        }
        
        regular_data[regular_data_numerics,
                     data_value:=i.data_value,
                     on=.(parse_id)]
        
        indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                               subs_flags))
        
        regular_data_numerics <- NULL
      }
      #SPECIAL FORMATTING FOR SYS CATEGORIES: Names and IDs
      reference_sys_categories <- regular_data[!is.na(indicator_sys_category),unique(indicator_sys_category)]
      if (length(reference_sys_categories) > 0) {
        
        regular_data_names <- regular_data[indicator_sys_category %in% c("name","nickname")]
        if (!empty(regular_data_names)) {

          regular_data_names[,data_value:=normalizeSyscategory_name(data_value)]
          bad_names <- regular_data_names[is.na(data_value) | data_value != reporting_submitted_data_value]
          if (!empty(bad_names)) {
            
            bad_names_flags <- bad_names[,.(parse_id,
                                            check_name="sys_flag_data_format_name_normalized",
                                            check_message=paste0(indicator_name,": ",
                                                             "from {",reporting_submitted_data_value,"} ",
                                                             "to {",ifelse(is.na(data_value),"MISSING",data_value),"}"))]
            
            indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                                   bad_names_flags))
            bad_names_flags <- NULL
          }
          bad_names <- NULL
          regular_data[regular_data_names,
                       data_value:=i.data_value,
                       on=.(parse_id)]
        }
        regular_data_names <- NULL
        
        regular_data_ids <- regular_data[indicator_sys_category == "id"]
        if (!empty(regular_data_ids)) {
          
          regular_data_ids[,data_value:=normalizeSyscategory_id(data_value)]
          
          bad_ids <- regular_data_ids[is.na(data_value) | data_value != reporting_submitted_data_value]
          if (!empty(bad_ids)) {
            
            bad_id_flags <- bad_ids[,.(parse_id,
                                       check_name="sys_flag_data_format_id_normalized",
                                       check_message=paste0(indicator_name,": ",
                                                            "from {",reporting_submitted_data_value,"} ",
                                                            "to {",ifelse(is.na(data_value),"MISSING",data_value),"}"))]
            
            indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                                   bad_id_flags))
            bad_id_flags <- NULL
          }
          bad_ids <- NULL
          regular_data[regular_data_ids,
                       data_value:=i.data_value,
                       on=.(parse_id)]
        }
        regular_data_ids <- NULL
      }
      
      if (any(reference_data_types=="number")) {
        
        regular_data_numbers <- regular_data[data_type=="number"]
        
        has_subs <- grepl("[[:space:]]+|^-$",regular_data_numbers$data_value,perl=T)
        if (any(has_subs)) {

          numbers <- gsub("[[:space:]]+","",regular_data_numbers[which(has_subs),data_value],perl=T) #Some clients have entered values of: "300 000 000" and "500 000 000" for example
          numbers <- gsub("^-$","0",numbers,perl=T) #accounting uses "-" as "0"
          set(regular_data_numbers,i=which(has_subs),j="data_value",value=numbers)
          
          subs_flags <- regular_data_numbers[which(has_subs),
                                             .(parse_id,
                                               check_name="sys_flag_data_format_auto_correction",
                                               check_message=paste0(indicator_name,": ",
                                                                  "from {",reporting_submitted_data_value,"} ",
                                                                  "to {",ifelse(is.na(data_value),"MISSING",data_value),"}"))]
          
          indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                                 subs_flags))
          subs_flags <- NULL
        }
        
        missings <- is.na(regular_data_numbers$data_value)
        numbers <- suppressWarnings(as.numeric(regular_data_numbers$data_value))
        set(regular_data_numbers,i=NULL,j="data_value",value=numbers)
        
        bad_numbers <- !missings & is.na(numbers)
        if (any(bad_numbers)) {
          bad_flags <- regular_data_numbers[bad_numbers,
                                             .(parse_id,
                                               check_name="sys_flag_data_format_invalid_number",
                                               check_message=paste0(indicator_name,": ",
                                                                  "from {",reporting_submitted_data_value,"} ",
                                                                  "to {MISSING}"))]
          
          indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                                 bad_flags))
          bad_flags <- NULL
        }
        
        
        regular_data[regular_data_numbers,
                     `:=`(parsed=TRUE,
                          data_value=i.data_value),
                     on=.(parse_id)]
        regular_data_numbers <- NULL
      }
      
      if (any(reference_data_types=="currency")) {
        
        #TODO: fx issues and currency data type
        #if (is.null(type_unit)) stop(paste("SLGP Schema Error: ",indicator$indicator_name," is type CURRENCY but does not define 'type_unit' currency code (eg, LCU, USD, EUR, etc)"),"\n",sep="")

        regular_data_currency <- regular_data[data_type=="currency"]
        
        has_subs <- grepl("[[:space:]]+|^-$",regular_data_currency$data_value,perl=T)
        if (any(has_subs)) {
          
          money <- gsub("[[:space:]]+","",regular_data_currency[which(has_subs),data_value],perl=T) #Some clients have entered values of: "300 000 000" and "500 000 000" for example
          money <- gsub("^-$","0",money,perl=T) #accounting uses "-" as "0"
          set(regular_data_currency,i=which(has_subs),j="data_value",value=money)
          
          subs_flags <- regular_data_currency[which(has_subs),
                                             .(parse_id,
                                               check_name="sys_flag_data_format_auto_correction",
                                               check_message=paste0(indicator_name,": ",
                                                                  "from {",reporting_submitted_data_value,"} ",
                                                                  "to {",ifelse(is.na(data_value),"MISSING",data_value),"}"))]
          
          indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                                 subs_flags))
          subs_flags <- NULL
        }
        
        missings <- is.na(regular_data_currency$data_value)
        money <- suppressWarnings(as.numeric(regular_data_currency$data_value))
        set(regular_data_currency,i=NULL,j="data_value",value=money)
        
        bad_money <- !missings & is.na(money)
        if (any(bad_money)) {
          bad_flags <- regular_data_currency[bad_money,
                                            .(parse_id,
                                              check_name="sys_flag_data_format_invalid_money",
                                              check_message=paste0(indicator_name,": ",
                                                                 "from {",reporting_submitted_data_value,"} ",
                                                                 "to {MISSING}"))]
          
          indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                                 bad_flags))
          bad_flags <- NULL
        }
        
        regular_data[regular_data_currency,
                     `:=`(parsed=TRUE,
                          data_value=i.data_value),
                     on=.(parse_id)]
        regular_data_currency <- NULL
      }
      
      if (any(reference_data_types=="currency_ratio")) {
        
        #TODO: fx issues and currency data type
        #if (is.null(type_unit)) stop(paste("SLGP Schema Error: ",indicator$indicator_name," is type CURRENCY but does not define 'type_unit' currency code (eg, LCU, USD, EUR, etc)"),"\n",sep="")
        
        regular_data_currencyfx <- regular_data[data_type=="currency_ratio"]
        
        #currency_ratio should follow same rules as money, only units are different
        has_subs <- grepl("[[:space:]]+|^-$",regular_data_currencyfx$data_value,perl=T)
        if (any(has_subs)) {
          
          money <- gsub("[[:space:]]+","",regular_data_currencyfx[which(has_subs),data_value],perl=T) #Some clients have entered values of: "300 000 000" and "500 000 000" for example
          money <- gsub("^-$","0",money) #accounting uses "-" as "0"
          set(regular_data_currencyfx,i=which(has_subs),j="data_value",value=money)
          
          subs_flags <- regular_data_currencyfx[which(has_subs),
                                              .(parse_id,
                                                check_name="sys_flag_data_format_auto_correction",
                                                check_message=paste0(indicator_name,": ",
                                                                    "from {",reporting_submitted_data_value,"} ",
                                                                    "to {",ifelse(is.na(data_value),"MISSING",data_value),"}"))]
          
          indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                                 subs_flags))
          subs_flags <- NULL
        }
        
        missings <- is.na(regular_data_currencyfx$data_value)
        money <- suppressWarnings(as.numeric(regular_data_currencyfx$data_value))
        set(regular_data_currencyfx,i=NULL,j="data_value",value=money)
        
        bad_money <- !missings & is.na(money)
        if (any(bad_money)) {
          bad_flags <- regular_data_currencyfx[bad_money,
                                             .(parse_id,
                                               check_name="sys_flag_data_format_invalid_money",
                                               check_message=paste0(indicator_name,": ",
                                                                   "from {",reporting_submitted_data_value,"} ",
                                                                   "to {MISSING}"))]
          
          indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                                 bad_flags))
          bad_flags <- NULL
        }
        
        regular_data[regular_data_currencyfx,
                     `:=`(parsed=TRUE,
                          data_value=i.data_value),
                     on=.(parse_id)]
        regular_data_currencyfx <- NULL
      }
      
      if (any(reference_data_types=="percent")) {
        
        regular_data_pct <- regular_data[data_type=="percent"]
        
        has_subs <- grepl("[[:space:]]+|^-$",regular_data_pct$data_value,perl=T)
        if (any(has_subs)) {
          
          pcts <- gsub("[[:space:]]+","",regular_data_pct[which(has_subs),data_value],perl=T) #Some clients have entered values of: "300 000 000" and "500 000 000" for example
          pcts <- gsub("^-$","0",pcts,perl=T) #accounting uses "-" as "0"
          set(regular_data_pct,i=which(has_subs),j="data_value",value=pcts)
          
          subs_flags <- regular_data_pct[which(has_subs),
                                              .(parse_id,
                                                check_name="sys_flag_data_format_auto_correction",
                                                check_message=paste0(indicator_name,": ",
                                                                   "from {",reporting_submitted_data_value,"} ",
                                                                   "to {",ifelse(is.na(data_value),"MISSING",data_value),"}"))]
          
          indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                                 subs_flags))
          subs_flags <- NULL
        }
        
        
        has_percents <- grepl("^\\d*\\.?\\d*%$",regular_data_pct$data_value,perl=T)
        if (any(has_percents)) {
          
          pcts <- regular_data_pct[which(has_percents),data_value]
          
          pcts <- as.numeric(gsub("^(\\d*\\.?\\d*)%$","\\1",pcts,perl=T)) / 100
          set(regular_data_pct,i=which(has_percents),j="data_value",value=pcts)
          
          subs_flags <- regular_data_pct[which(has_percents),
                                         .(parse_id,
                                           check_name="sys_flag_data_format_auto_correction",
                                           check_message=paste0(indicator_name,": ",
                                                            "from {",reporting_submitted_data_value,"} ",
                                                            "to {",ifelse(is.na(data_value),"MISSING",data_value),"}"))]
          
          indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                                 subs_flags))
          subs_flags <- NULL
        }

        has_percents <- grepl("^\\d*\\.?\\d*percent$",regular_data_pct$data_value,perl=T)
        if (any(has_percents)) {
          
          pcts <- regular_data_pct[which(has_percents),data_value]
          
          pcts <- as.numeric(gsub("^(\\d*\\.?\\d*)percent$","\\1",pcts,perl=T)) / 100
          set(regular_data_pct,i=which(has_percents),j="data_value",value=pcts)
          
          subs_flags <- regular_data_pct[which(has_percents),
                                         .(parse_id,
                                           check_name="sys_flag_data_format_auto_correction",
                                           check_message=paste0(indicator_name,": ",
                                                                "from {",reporting_submitted_data_value,"} ",
                                                                "to {",ifelse(is.na(data_value),"MISSING",data_value),"}"))]
          
          indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                                 subs_flags))
          subs_flags <- NULL
        }
        
        missings <- is.na(regular_data_pct$data_value)        
        pcts <- suppressWarnings(as.numeric(regular_data_pct$data_value))
        set(regular_data_pct,i=NULL,j="data_value",value=pcts)
        
        bad_percents <- !missings & is.na(pcts)
        if (any(bad_percents)) {
          
          subs_flags <- regular_data_pct[which(bad_percents),
                                         .(parse_id,
                                           check_name="sys_flag_data_format_invalid_percent",
                                           check_message=paste0(indicator_name,": ",
                                                              "from {",reporting_submitted_data_value,"} ",
                                                              "to {MISSING}"))]
          
          indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                                 subs_flags))
          subs_flags <- NULL
        }
        
        above_100 <- regular_data_pct[!is.na(data_value) & 
                                      suppressWarnings(as.numeric(data_value)) > 1]
        if (!empty(above_100)) {
          above_flags <- above_100[,
                                   .(parse_id,
                                     check_name="sys_flag_data_format_warning",
                                     check_message=paste0("More than one-hundred percent warning: ",indicator_name," reports ",
                                                          (as.numeric(data_value)*100),"%",
                                                          "(Should reported value be \"",(as.numeric(data_value)/100),"\" instead of \"",data_value,"\"?)"))]
          
          indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                                 above_flags))
          above_flags <- NULL
        }
        above_100 <- NULL
        
        regular_data[regular_data_pct,
                     `:=`(parsed=TRUE,
                          data_value=i.data_value),
                     on=.(parse_id)]
        regular_data_pct <- NULL   
      }
      
      if (any(reference_data_types=="logical")) {

        regular_data_tf <- regular_data[data_type=="logical"]
        tf <- regular_data_tf$data_value
        trues <- grepl("^1$|^t$|^true$|^yes$|^oui$",as.character(tf),ignore.case = TRUE,perl=T)
        falses <- grepl("^0$|^f$|^false$|^no$|^non$",as.character(tf),ignore.case = TRUE,perl=T)
        if (any(trues)) tf[trues] <- TRUE
        if (any(falses)) tf[falses] <- FALSE
        
        missings <- is.na(tf)
        tf <- suppressWarnings(as.logical(tf))
        set(regular_data_tf,i=NULL,j="data_value",value=tf)
        
        bad_logic <- !missings & is.na(tf)
        if (any(bad_logic)) {
          
          bad_flags <- regular_data_tf[bad_logic,
                                             .(parse_id,
                                               check_name="sys_flag_data_format_invalid_logical",
                                               check_message=paste0(indicator_name,": ",
                                                                "from {",reporting_submitted_data_value,"} ",
                                                                "to {MISSING}"))]
          
          indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                                 bad_flags))
          bad_flags <- NULL
        }
        regular_data[regular_data_tf,
                     `:=`(parsed=TRUE,
                          data_value=i.data_value),
                     on=.(parse_id)]
        regular_data_tf <- NULL
      }
      
      if (any(reference_data_types=="date")) {
        
        regular_data_dates <- regular_data[data_type=="date"]
        
        parse_dates <- function(excel_values) {
          
          parsed_dates <- rep(NA_character_,length(excel_values))
          unparsed_dates <- 1:length(excel_values)
          
          which_na_dates <- c(which(superTrim(excel_values) %in% recognized_blanks$defined_blank),
                              which(is.na(excel_values)))
          
          if (length(which_na_dates) > 0) {
            unparsed_dates[which_na_dates] <- NA
          }
          if (all(is.na(unparsed_dates))) return (parsed_dates)
          
          which_excel_dates <- which(!is.na(suppressWarnings(as.numeric(excel_values))))
          if (length(which_excel_dates) > 0) {
            parsed_dates[which_excel_dates] <- as.character(
                                                as.Date(
                                                  as.numeric(excel_values[which_excel_dates]), 
                                                  origin = "1899-12-30")) #it's numeric, read-in as integrate days in Excel format
            
            unparsed_dates[which_excel_dates] <- NA
          }
          if (all(is.na(unparsed_dates))) return (parsed_dates)
          
          which_as_dates <- which(!is.na(suppressWarnings(as.Date(excel_values,optional=TRUE))))
          if (length(which_as_dates) > 0) {
            parsed_dates[which_as_dates] <- as.character(as.Date(excel_values[which_as_dates]))
            unparsed_dates[which_as_dates] <- NA
          }
          
          if (all(is.na(unparsed_dates))) return (parsed_dates)
          which_ymd_dates <- which(!is.na(suppressWarnings(ymd(excel_values))))
          if (length(which_ymd_dates) > 0) {
            parsed_dates[which_ymd_dates] <- as.character(ymd(excel_values[which_ymd_dates]))
            unparsed_dates[which_ymd_dates] <- NA
          }
          if (all(is.na(unparsed_dates))) return (parsed_dates)

          which_dmy_dates <- which(!is.na(suppressWarnings(dmy(excel_values))))
          if (length(which_dmy_dates) > 0) {
            parsed_dates[which_dmy_dates] <- as.character(dmy(excel_values[which_dmy_dates]))
            unparsed_dates[which_dmy_dates] <- NA
          }
          if (all(is.na(unparsed_dates))) return (parsed_dates)
          
          which_mdy_dates <- which(!is.na(suppressWarnings(mdy(excel_values))))
          if (length(which_mdy_dates) > 0) {
            parsed_dates[which_mdy_dates] <- as.character(mdy(excel_values[which_mdy_dates]))
            unparsed_dates[which_mdy_dates] <- NA
          }
          if (all(is.na(unparsed_dates))) return (parsed_dates)
          
          return (parsed_dates)
        }
        
        missings <- is.na(regular_data_dates$data_value)
        dates <- parse_dates(excel_values=regular_data_dates$data_value)
        
        bad_dates <- is.na(dates) | is.na(suppressWarnings(ymd(dates)))
        if (any(bad_dates)) {
          unit_dates <- parse_dates(excel_values=regular_data_dates$data_unit[bad_dates])
          auto_correct <- c()
          if (any(!is.na(unit_dates))) {
            
            auto_correct <- which(bad_dates)[!is.na(unit_dates)]
            
            if (length(auto_correct) > 0) {
              dates[auto_correct] <- unit_dates[!is.na(unit_dates)]
                
              regular_data[regular_data_dates[auto_correct],
                           `:=`(data_unit=as.character(NA)),
                           on=.(parse_id)]
              
              bad_flag_units <- regular_data_dates[auto_correct,
                                              .(parse_id,
                                                check_name="sys_flag_data_unit_auto_correction",
                                                check_message=paste0(indicator_name,": ",
                                                                     "from {",reporting_submitted_data_unit,"} ",
                                                                     "to {date} because date value was reported in the data unit column"))]
              
              
              bad_flag_values <- regular_data_dates[auto_correct,
                                              .(parse_id,
                                                check_name="sys_flag_data_format_auto_correction",
                                                check_message=paste0(indicator_name,": ",
                                                                     "from {",reporting_submitted_data_value,"} ",
                                                                     "to {",reporting_submitted_data_unit,"} because date value was reported in the data unit column"))]
              indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                                     bad_flag_units,
                                                     bad_flag_values))
            }
          }
          # if (length(auto_correct)>0) {
          #   dates[bad_dates==TRUE & auto_correct==FALSE] <- as.character(NA)
          # }
        }
        
        
        
        set(regular_data_dates,i=NULL,j="data_value",value=dates)
        
        bad_dates <- !missings & is.na(dates)
        if (any(bad_dates)) {
          bad_flags <- regular_data_dates[bad_dates,
                                            .(parse_id,
                                              check_name="sys_flag_data_format_invalid_date",
                                              check_message=paste0(indicator_name,": ",
                                                               "from {",reporting_submitted_data_value,"} ",
                                                               "to {MISSING}"))]
          
          indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                                 bad_flags))
          bad_flags <- NULL
        }
        
        #Date is more than 15 years different from today
        odd_dates <- abs(year(suppressWarnings(ymd(dates))) - year(today())) > 100
        odd_dates[is.na(odd_dates)] <- FALSE
        if (any(odd_dates)) {
          bad_flags <- regular_data_dates[odd_dates,
                                          .(parse_id,
                                            check_name="sys_flag_data_format_warning",
                                            check_message=paste0(indicator_name,": ",
                                                                 "Reported date {",reporting_submitted_data_value,"} resolved to {",data_value,"} ",
                                                                 "is more than 100 years from today"))]
          
          indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                                 bad_flags))
          bad_flags <- NULL
        }
          
        regular_data[regular_data_dates,
                     `:=`(parsed=TRUE,
                          data_value=i.data_value),
                     on=.(parse_id)]
        regular_data_dates <- NULL
      }

      if (any(reference_data_types=="text")) {
        regular_data_text <- regular_data[data_type=="text"]
        regular_data[regular_data_text,
                     `:=`(parsed=TRUE,
                          data_value=i.data_value),
                     on=.(parse_id)]
        regular_data_text <- NULL
        
      }
      
      #Numerics for rounding
      if (any(reference_data_types %in% c("number","currency","currency_ratio","percent"))) {
        regular_data_numerics <- regular_data[data_type %in% c("number","currency","currency_ratio","percent")
                                              & is.na(data_value)==FALSE]
        
        #Excel and R have different numbers of max decimals (14 vs 15, I think?), which can make effectively identical numbers uncomparable; so round both to 14 places.
        #these are compared in update data into the database, notably where system calculator will calculate an item and see if it's meaningfully changed and/or if the
        #user has submitted updated reported data (even if the resultant data_value stays the same)
        #ok.... this should now be addressesd by the database function data_value_is_meaningfully_different()
        #it will only allow user data inserts when data submitted unmeaningfully changes in the same reporting asof date.  This also avoids issues previously experienced
        #where user will be submitting "N/A" for a given field one reporting date; and then in another, reformat the template to submit blank "" instead and trigger database
        #to see a different reported value, even though the recorded data_value remains NA in both cases.  This should also apply to rounding and numbers with lots of decimals
        #that get rounded differently across reporting periods.
        # numeric_input_data <- suppressWarnings(as.numeric(regular_data_numerics$reporting_submitted_data_value))
        # numeric_input_data <- round(
        #                         numeric_input_data,
        #                         digits=DATA_MAX_DECIMALS)
        # set(regular_data_numerics,i=which(!is.na(numeric_input_data)),j="reporting_submitted_data_value",value=as.character(numeric_input_data[which(!is.na(numeric_input_data))]))
        
        numeric_values_data <- as.numeric(regular_data_numerics$data_value)
        
        numeric_values_data[is.nan(numeric_values_data)] <- as.numeric(NA)
        numeric_values_data[is.infinite(numeric_values_data)] <- as.numeric(NA)
        
        numeric_values_data <- round(
                                numeric_values_data,
                                digits=DATA_MAX_DECIMALS)

        numeric_values_data_decimals <- numeric_values_data - trunc(numeric_values_data)
        effectively_whole_numbers <- is.na(numeric_values_data_decimals)==FALSE & 
                                     ((1-numeric_values_data_decimals) < (1/10^(DATA_SIG_DIGITS+1)))
        
        numeric_values_data[effectively_whole_numbers] <- round(numeric_values_data[effectively_whole_numbers],0)
        
        set(regular_data_numerics,i=NULL,j="data_value",value=as.character(numeric_values_data))
        
        regular_data[regular_data_numerics,
                     `:=`(parsed=TRUE,
                          data_value=i.data_value),
                     on=.(parse_id)]
        
        regular_data_numerics <- NULL
        
      }

      indicator_data[regular_data,
                     `:=`(data_value=i.data_value,
                          data_unit=i.data_unit,
                          parsed=i.parsed),
                     on=.(parse_id)]
    }
    
    regular_data <- NULL
  }
  
  #Check unexpected formulas or constants
  {
    #Calculated fields that are reported as constants
    unexpected_constats <- indicator_data[rsf_indicators[is_calculated==TRUE & is_system==FALSE,.(indicator_id)],
                                          on=.(indicator_id),
                                          nomatch=NULL][,`:=`(blank_formulas=sum(is.na(reporting_submitted_data_formula)),
                                                              reported_count=.N),
                                                        by=.(indicator_id)][is.na(reporting_submitted_data_formula)==TRUE & blank_formulas < reported_count]
    
    if (!empty(unexpected_constats)) {
      bad_flags <- unexpected_constats[,.(parse_id,
                                          check_name="sys_flag_unexpected_constant",
                                          check_message=paste0(indicator_name,
                                                              " is a calculated field: constant value of {",
                                                              reporting_submitted_data_value,
                                                              "} reported instead of calculation."))]
      
      indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                             bad_flags))
      
      bad_flags <- NULL
    }
    unexpected_constats <- NULL

    unexpected_formulas <- indicator_data[!is.na(reporting_submitted_data_formula)
                                          ][rsf_indicators[is_calculated==FALSE & is_system==FALSE,.(indicator_id)],
                                            on=.(indicator_id),
                                            nomatch=NULL]
    
    if (!empty(unexpected_formulas)) {
      bad_flags <- unexpected_formulas[,.(parse_id,
                                          check_name="sys_flag_unexpected_formula",
                                          check_message=paste0(indicator_name,
                                                              " is a non-calculated field reporting value {",
                                                              reporting_submitted_data_value,
                                                              "} using formula {",reporting_submitted_data_formula,"}"))]
      
      indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                             bad_flags))
      
      bad_flags <- NULL
    }
    
  }
  
  #Data Units
  {
    indicator_data[data_unit_validated==FALSE 
                   & is.same_text(data_unit,default_data_unit),
                   data_unit_validated:=TRUE]
    
    parse_units <- indicator_data[is.na(indicator_id)==FALSE & 
                                  data_unit_validated==FALSE]
    
    parse_units[,convert_to_default:=as.numeric(NA)]
    parse_units[,unit_error:=FALSE]
    
    if (!empty(parse_units)) {
      parse_units_data_types <- parse_units[,unique(data_type)]

      #currently only recognized number units are time metrics: DAYS, MONTHS, YEARS
      if (any(parse_units_data_types=="number")) {
        
        parse_units[data_type=="number" & 
                      data_unit=="#",
                    `:=`(data_unit=NA,
                         data_unit_validated=TRUE)]
        
        #pluralize
        if (!empty(parse_units[data_type=="number" & data_unit %in% c("YEAR","MONTH","DAY")])) {
          parse_units[data_type=="number" & data_unit=="DAY",data_unit:="DAYS"]
          parse_units[data_type=="number" & data_unit=="MONTH",data_unit:="MONTHS"]
          parse_units[data_type=="number" & data_unit=="YEAR",data_unit:="YEARS"]
        }
        
        #periodacity to pluralize
        if (!empty(parse_units[data_type=="number" & data_unit %in% c("YEARLY","MONTHLY","DAYLY")])) {
          parse_units[data_type=="number" & data_unit=="DAYLY",data_unit:="DAYS"]
          parse_units[data_type=="number" & data_unit=="MONTHLY",data_unit:="MONTHS"]
          parse_units[data_type=="number" & data_unit=="YEARLY",data_unit:="YEARS"]
        }
        
        parse_units[data_type=="number" & data_unit=="YEARS" & default_data_unit=="MONTHS",convert_to_default:=12]
        parse_units[data_type=="number" & data_unit=="MONTHS" & default_data_unit=="YEARS",convert_to_default:=1/12]
        parse_units[data_type=="number" & data_unit=="DAYS" & default_data_unit=="YEARS",convert_to_default:=1/365]
        parse_units[data_type=="number" & data_unit=="YEARS" & default_data_unit=="DAYS",convert_to_default:=365]
        
        parse_units[data_type=="number" & data_unit=="MONTHS" & default_data_unit=="DAYS",unit_error:=TRUE]
        parse_units[data_type=="number" & data_unit=="DAYS" & default_data_unit=="MONTHS",unit_error:=TRUE]
        
      }
      
      if (any(parse_units_data_types=="percent")) { 
        parse_units[data_type=="percent" & 
                    data_unit=="%",
                    `:=`(data_unit=NA,
                         data_unit_validated=TRUE)]
      }
      
      if (any(parse_units_data_types=="date")) { 
        parse_units[data_type=="date" & 
                      data_unit=="DATE",
                    `:=`(data_unit=NA,
                         data_unit_validated=TRUE)]
      }
      
      if (any(parse_units_data_types=="currency")) {
        
        #if it's LCY then normalize to LCU
        parse_units[data_type=="currency" & 
                    data_unit %in% DATA_DEFINED_LOCAL_CURRENCY_UNITS & 
                    data_unit != "LCU",
                    data_unit:="LCU"]
        
        parse_units[data_type=="currency" & 
                    (data_unit %in% VALID_CURRENCIES) == FALSE,
                    #grepl("^[A-Z]{3}$",data_unit)==FALSE,
                    unit_error:=TRUE]
        
        #indicators that are defined currency units, eg, defined as USD or EUR, then the input value cannot specify a different currency unit.
        parse_units[data_type=="currency" & 
                      unit_error==FALSE &
                      default_data_unit != "LCU" &
                      data_unit != default_data_unit,
                    unit_error:=TRUE]
        
        #When indicator is defined in LCU and submitted data supplies the actual currency unit, this is accepted.
        parse_units[data_type=="currency" & 
                    default_data_unit=="LCU" & 
                    data_unit != "LCU" &
                    unit_error==FALSE,
                    data_unit_validated:=TRUE]
        
      }
      
      if (any(parse_units_data_types=="currency_ratio")) {
        parse_units[data_type=="currency_ratio" & grepl("LCY",data_unit),
                    data_unit:=gsub("LCY","LCU",data_unit)]
        parse_units[data_type=="currency_ratio" & grepl("^[A-Z]{3}/[A-Z]{3}$",data_unit)==FALSE,
                    unit_error:=TRUE]
        
        parse_units[,
                    `:=`(data_unit_n=as.character(NA),
                         data_unit_d=as.character(NA),
                         default_data_unit_n=as.character(NA),
                         default_data_unit_d=as.character(NA))]

        parse_units[data_type=="currency_ratio" & unit_error==FALSE,
                    c("data_unit_n","data_unit_d"):=tstrsplit(data_unit,"/",fixed=TRUE)]
        
        parse_units[data_type=="currency_ratio" & unit_error==FALSE,
                    c("default_data_unit_n","default_data_unit_d"):=tstrsplit(default_data_unit,"/",fixed=TRUE)]
        
        parse_units[data_type=="currency_ratio" & 
                    unit_error==FALSE &
                    default_data_unit_n=="LCU" &
                    data_unit_d==default_data_unit_d,
                    data_unit_validated:=TRUE]
        
        parse_units[data_type=="currency_ratio" & 
                    unit_error==FALSE &
                    default_data_unit_d=="LCU" &
                    data_unit_n==default_data_unit_n,
                    data_unit_validated:=TRUE]
        
        parse_units[data_type=="currency_ratio" &
                    unit_error==FALSE &
                    data_unit_validated==FALSE &
                    data_unit_d==default_data_unit_n &
                    data_unit_n==default_data_unit_d &
                    is.numeric(suppressWarnings(as.numeric(data_value)))==TRUE,
                    convert_to_default:=1/(as.numeric(data_value)*as.numeric(data_value))] #The value by which to multiple to invert to convert to 1/x is 1/x^2
      }
      
      
      
      convert_units <- parse_units[!is.na(convert_to_default)]
      if (!empty(convert_units)) {
        
        convert_units[,`:=`(data_value=as.character(as.numeric(data_value)*convert_to_default),
                            data_unit=default_data_unit)]
        
        indicator_data[convert_units[,.(parse_id,
                                        data_value,
                                        data_unit)],
                       `:=`(data_value=i.data_value,
                            data_unit=i.data_unit,
                            data_unit_validated=TRUE),
                       on=.(parse_id)]
        
        parse_units[convert_units[,.(parse_id,
                                     data_value,
                                     data_unit)],
                       `:=`(data_value=i.data_value,
                            data_unit=i.data_unit,
                            data_unit_validated=TRUE),
                       on=.(parse_id)]

        convert_units_flags <- convert_units[,
                                     .(parse_id,
                                       check_name="sys_flag_data_unit_auto_correction",
                                       check_message=paste0(indicator_name,": ",
                                                            "corrected and converted from {",reporting_submitted_data_value," ",reporting_submitted_data_unit,"}",
                                                            " to {",data_value," ",data_unit,"}"))]
        
        indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                               convert_units_flags))
        
        convert_units_flags <- NULL
      }
      convert_units <- NULL
      
      parse_units[data_unit_validated==FALSE & is.same_text(data_unit,default_data_unit),  #Eg for DAY -> DAYS corrections, units will now match
                  data_unit_validated:=TRUE]
      
      indicator_data[parse_units[data_unit_validated==TRUE,.(parse_id,data_unit)],
                     `:=`(data_unit=i.data_unit,
                          data_unit_validated=TRUE),
                     on=.(parse_id)]
      
      bad_units <- parse_units[data_unit_validated==FALSE &
                               unit_error==FALSE]
      
      if (!empty(bad_units)) {
        
        indicator_data[bad_units[,.(parse_id,default_data_unit)],
                       `:=`(data_unit=i.default_data_unit,
                            data_unit_validated=TRUE),
                       on=.(parse_id)]
        
        bad_units_flags <- bad_units[!is.na(default_data_unit), #Because if they submit something irrelevant the flag is not so useful to say it's invalid.  Just ignore.
                                     .(parse_id,
                                       check_name="sys_flag_data_invalid_unit",
                                       check_message=paste0(indicator_name,": Data reported: ",reporting_submitted_data_value," has ",
                                                            "invalid unit {",reporting_submitted_data_unit,"}",
                                                            " using indicator-defined unit {",ifelse(is.na(default_data_unit),"NONE",default_data_unit),"}"))]
        
        indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                               bad_units_flags))
        bad_units_flags <- NULL
      }
      
      bad_units <- parse_units[data_unit_validated==FALSE &
                               unit_error==TRUE]
      
      if (!empty(bad_units)) {
        
        bad_units_good_currency <- bad_units[data_type=="currency" & 
                                             data_unit %in% VALID_CURRENCIES]
        
        if (!empty(bad_units_good_currency)) {

          bad_units[,good_currency:=FALSE]
          bad_units[bad_units_good_currency,
                    good_currency:=TRUE,
                    on=.(parse_id)]
          bad_units <- bad_units[good_currency==FALSE]
          bad_units[,good_currency:=NULL]
          
          indicator_data[bad_units_good_currency[,.(parse_id,data_value,default_data_unit)],
                         `:=`(data_unit_validated=TRUE),
                         on=.(parse_id)]
          
          bad_units_good_currency_flags <- bad_units_good_currency[,
                                           .(parse_id,
                                             check_name="sys_flag_data_invalid_value",
                                             check_message=paste0(indicator_name,": ",
                                                                  "requires {",default_data_unit,"} but {",data_value," ",
                                                                  data_unit,"} was reported. This may result in serious errors, as calculations will interpret numbers as {",data_value," ",
                                                                  default_data_unit,"}"))]
          
          indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                                 bad_units_good_currency_flags))
          
        }
        
        if (!empty(bad_units)) {
          indicator_data[bad_units[,.(parse_id,data_value,default_data_unit)],
                         `:=`(data_value=i.data_value,
                              data_unit=i.default_data_unit,
                              data_unit_validated=TRUE),
                         on=.(parse_id)]
          
          bad_units_flags <- bad_units[,
                                       .(parse_id,
                                         check_name="sys_flag_data_invalid_value",
                                         check_message=paste0(indicator_name,": ",
                                                              "unable to auto-correct units: {",reporting_submitted_data_value,reporting_submitted_data_unit,"} ",
                                                              "interpreting using default units as {",ifelse(is.na(data_value),"MISSING",data_value),
                                                                              ifelse(is.na(default_data_unit),"",paste0(" ",default_data_unit)),"}"))]
          
          indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                                 bad_units_flags))
          bad_units_flags <- NULL
        }
      }
      bad_units <- NULL
      
    }
    
    parse_units <- NULL
    
    parse_data_units <- indicator_data[is.na(indicator_id)==FALSE & 
                                       indicator_id %in% rsf_indicators[is_data_unit==TRUE,indicator_id]]
    parse_data_units[,convert:=as.logical(NA)]
    parse_data_units[,unit_error:=FALSE]
    if (!empty(parse_data_units)) {

      parse_data_units[rsf_indicators,
                       default_data_unit:=i.default_value,
                       on=.(indicator_id)]
      
      #only currency_unit is_data_unit indicators are currently defined      
      parse_data_units[is.na(convert) &
                       (is.na(indicator_sys_category) |
                        grepl("currency_unit$",indicator_sys_category)==FALSE),
                       `:=`(data_value=default_data_unit,
                            unit_error=TRUE,
                            convert=TRUE)]

      #LCY isn't allowed but as it's widely used, simply convert without error message
      parse_data_units[is.na(convert) &
                       data_value %in% DATA_DEFINED_LOCAL_CURRENCY_UNITS[-which(DATA_DEFINED_LOCAL_CURRENCY_UNITS=="LCU")],
                       `:=`(data_value="LCU",
                            unit_error=FALSE, #not merit a flag
                            convert=TRUE)]

      #any reference to "dollar" is USD by default      
      parse_data_units[is.na(convert) &
                         tolower(data_value) == "dollar",
                       `:=`(data_value="USD",
                            unit_error=FALSE, #not merit a flag
                            convert=TRUE)]

      #any reference to "dollar" is USD by default      
      parse_data_units[is.na(convert) &
                         tolower(data_value) == "euro",
                       `:=`(data_value="EUR",
                            unit_error=FALSE, #not merit a flag
                            convert=TRUE)]
      
      parse_data_units[is.na(convert) &
                       !(data_value %in% VALID_CURRENCIES),
                       `:=`(data_value=default_data_unit,
                            unit_error=TRUE,
                            convert=TRUE)]
      
      bad_units <- parse_data_units[unit_error==TRUE,
                                   .(parse_id,
                                     check_name="sys_flag_data_invalid_unit",
                                     check_message=paste0(indicator_name," ",
                                                          "is a defined unit but {",reporting_submitted_data_value ,"} is not valid. ",
                                                          "Default will be used {",data_value,"}"))]
      
      indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                             bad_units))
      
      indicator_data[parse_data_units[convert==TRUE],
                     data_value:=i.data_value,
                     on=.(parse_id)]
      
    }
  }

  
  if (any(!indicator_data$parsed)) stop("Failed to fully parse data")
  if (any(!indicator_data$data_unit_validated)) stop("Failed to fully parse data")
  
  
  
  #new revision to data flags
  {
    #data_flags_new might be defined via calculate already.
    if (is.null(template_data[["data_flags_new"]])) template_data[,data_flags_new:=list()]
    
    if (!empty(indicator_data_flags)) {
      #browser()
      indicator_data_flags[indicator_data,
                           data_ids:=i.data_ids,
                           on=.(parse_id)]
      
      indicator_data_flags <- indicator_data_flags[,
                                                   .(data_id=unlist(data_ids)),
                                                   by=.(parse_id,check_name,check_message)]
      
      preexisting_flags <- template_data[,unlist(data_flags_new,recursive=F),
                                          by=.(data_id)]
       
      if (empty(preexisting_flags)) preexisting_flags <- NULL
      
      indicator_data_flags <- rbindlist(list(indicator_data_flags,
                                              preexisting_flags))
       
       
      data_flags_new_dt <- indicator_data_flags[,
                                                .(data_flags_new=list(.SD)),
                                                by=.(data_id),
                                                .SDcols=c("check_name","check_message")]
      
      
      template_data[data_flags_new_dt,
                    data_flags_new:=i.data_flags_new,
                    on=.(data_id)]
      
      data_flags_new_dt <- NULL
    }    
  }


  
  
  
  #Parse data_submitted -- database doesn't allow NULL/NA data_submitted values
  #Also helps normalize Excel fields filled with space(s)
  indicator_data[is.na(reporting_submitted_data_value),
                 data_submitted:="{MISSING}"]
  
  indicator_data[nchar(superTrim(reporting_submitted_data_value))==0,
                 data_submitted:="{NOTHING}"]
  
  

  indicator_data <- indicator_data[,
                                   .(data_id=unlist(data_ids,recursive=F)),
                                   by=.(parse_id,
                                        data_value,
                                        data_unit)] 
  
  
  template_data[indicator_data,
                `:=`(data_value=i.data_value,
                     data_unit=i.data_unit),
                on=.(data_id)]
  
  #merging these is for error reporting and logging
  template_data[!is.na(reporting_submitted_data_unit) &
                !is.na(reporting_submitted_data_value) &
                !is.na(data_value),
                reporting_submitted_data_value:=paste0(reporting_submitted_data_value," ",reporting_submitted_data_unit)]
  
  template_data[,
                `:=`(reporting_submitted_data_formula=NULL,
                     reporting_submitted_data_unit=NULL,
                     data_id=NULL)]
  
  setnames(template_data,
           old=c("reporting_submitted_data_value"),
           new=c("data_submitted"))
  
  indicator_data <- NULL
  
  #if the parsed value is effectively the same as the reported value then no need to upload and save both.
  template_data[(is.na(data_value) & nchar(data_submitted)==0) |
                data_value==data_submitted,
                data_submitted:=NA]
  
  if(SYS_PRINT_TIMING) debugtime("parse_data_formats","Done!",as.numeric(Sys.time()-t1,"secs"))
  
  return (template_data)
}
