db_indicators_get_header_actions <- function(pool,
                                             template_id,
                                             rsf_pfcbl_id,
                                             rsf_indicators,
                                             # "full" will add pattern ^string$ and "partial" will add pattern ^.*string.*$
                                             # in other words, match the full header or any partial match.  For templates (like RSA) that have
                                             # discrete terms that are a specific header section, a partial match can help identify similar equivalent 
                                             # patterns of the same term. Whereas for QR template, each section is a list of complete headers that should be fully matched
                                             formatting.function=superTrim,
                                             formatting.strip=NULL) #regexp to strip-out, ie, gsub to "" 
{
  
  trimFunc <- NULL
  
  #superTrim calls trim.punct by default as true
  #normalizeLabel does a few more stuff, like remove accents; but not trim punct
  if (is.null(formatting.function)) {
    trimFunc <- normalizeLabel
  } else {
    trimFunc <- formatting.function
  }
  
  header_actions <- dbGetQuery(pool,"
      select tha.*,indf.indicator_id as formula_indicator_id
        from p_rsf.view_rsf_setup_template_header_actions tha
        left join p_rsf.indicator_formulas indf on indf.formula_id = map_formula_id
      where tha.rsf_pfcbl_id = $1::int
        and tha.template_id = $2::int
      order by header_id desc",
                               params=list(rsf_pfcbl_id,
                                           template_id))
  
  setDT(header_actions)
  header_actions[is.na(map_indicator_id) & !is.na(formula_indicator_id),
                 map_indicator_id := formula_indicator_id]
  
  header_actions[,template_header_position:=as.numeric(NA)]
  
  #what fx are we reporting in that we could possibly include as a custom template header
  currency_of_interest <- dbGetQuery(pool,"
    select fx_unit 
    from (
      select distinct formula_calculation_unit as fx_unit
      from p_rsf.view_rsf_setup_indicator_subscriptions sis
      where sis.rsf_pfcbl_id = $1::int
        and data_type='currency'
        and is_subscribed is true
      
      union 
      
      select distinct rdc.data_unit as fx_unit
      from p_rsf.rsf_pfcbl_ids ids
      inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
      inner join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id in (ids.rsf_facility_id,ids.rsf_client_id)
                                           and rdc.indicator_id = ind.indicator_id
      where ids.rsf_pfcbl_id = $1::int
       and ind.data_type = 'currency'
    ) fx_cu
    where fx_cu.fx_unit is not null and fx_cu.fx_unit <> 'LCU'
  ",params=list(rsf_pfcbl_id))
  currency_of_interest <- unlist(currency_of_interest,use.names = F)
  # header_actions[,stop:=as.numeric(NA)]
  # header_actions[grepl("^:AFTER:(ROW|COL)\\d+$",template_header),
  #                stop:=as.numeric(gsub("^:AFTER:(ROW|COL)(\\d+)$","\\2",template_header))]
  # 
  # stop_actions <- header_actions[is.na(stop)==FALSE & action=="ignore"]
  
  #default allows facilities to overwrite program-level setups, for example.
  #header_actions <- header_actions[action != "default"]
  
  if (any(grepl("&&",header_actions$template_header))) {
    grouped_header_actions <- header_actions[grepl("&&",template_header),
                                             .(ungrouped_header=unlist(strsplit(template_header,split="[[:space:]]+&&[[:space:]]+",fixed=F),recursive=F)),
                                             by=.(header_id)]
    
    header_actions <- grouped_header_actions[header_actions,
                                             on=.(header_id),
                                             nomatch=NA]
    header_actions[!is.na(ungrouped_header),
                   template_header:=ungrouped_header]
    
    header_actions[!is.na(ungrouped_header),
                   template_header_position:=1:.N,
                   by=.(header_id)]
    header_actions[,
                   ungrouped_header:=NULL]
  }
  
 
  rsf_labels <- rbindlist(rsf_indicators$labels)
  
  
  #use superTrim() over label_normalized
  #all rsf_labels are aliases for enable/map_indicator_id (that's what the labels are for!)
  rsf_labels <- unique(rsf_labels[,.(map_indicator_id=indicator_id,label_key,label=trimFunc(label))])
  
  currency_labels <- rsf_labels[map_indicator_id %in% rsf_indicators[data_type=="currency" & data_unit=="LCU",indicator_id] & label_key != 'SYS']
  
  label_has_unit <- sapply(currency_labels$label,function(l,currency_of_interest) {
    any(sapply(paste0("\\b",currency_of_interest,"\\b"),grepl,x=l,ignore.case=T,USE.NAMES = F))
  },currency_of_interest=currency_of_interest,USE.NAMES = F)
  
  currency_labels <- currency_labels[-which(label_has_unit)]

  currency_labels <- currency_labels[,.(fx_label=paste0(label," ",tolower(currency_of_interest))),by=.(map_indicator_id,label_key,label)]
  
  rsf_labels <- rbindlist(list(rsf_labels,
                               currency_labels[,.(map_indicator_id,label_key,label=fx_label)]))
  
  rsf_labels <- unique(rsf_labels)
  #rsf labels only map to indicators and are the default matching
  rsf_labels[,
             `:=`(header_id=-.I,
                  template_header_section_name=as.character(NA),
                  template_header_section_index=as.character(NA),
                  template_section_lookup=as.character(NA),
                  template_label_lookup=paste0('^"?',str_escape(label),'"?$'), #ignore quoted headers
                  action="default",
                  template_header_position=as.numeric(NA),
                  map_formula_id=as.numeric(NA),
                  calculation_formula=as.character(NA),
                  map_check_formula_id=as.numeric(NA),
                  check_formula=as.character(NA))]
  
  
  header_actions[,label_key:="SYS"]
 
  
  
  #except, for titles that are purely "*"
  #and where JASON control codes inside {} have been inserted
  
  #Primarily designed to strip-out bullets.  This is done before the str_escape, else bullets are no longer formatted as anticipated
  if (!is.null(formatting.strip)) {
    if (length(formatting.strip) != 1) {
      formatting.strip <- paste0(formatting.strip,collapse="|")
    }
    header_actions[grepl(formatting.strip,template_header),
                   template_header:=gsub(formatting.strip,"",template_header)]
  }

  #TESTING
  #if (any(grepl("\\{",header_actions$template_header ))) { browser() }

  
  header_actions[,label:=trimFunc(template_header)] #for this template, use trimmed, not normalized (as parsing values are used and therefore don't normalize {} delimiter!)
  header_actions[,template_label_lookup:=template_header]
  {
    
    #{indicators inside brackets#units}
    header_actions[!is.na(template_header_section_name) & 
                   grepl("\\{[^\\}]+\\}",template_label_lookup),
                   template_label_lookup:=gsub("\\{[^\\}]+\\}",
                                               ".*",
                                               template_label_lookup)]
    
    #words with the trim.punct of superTrim
    #the label entries might not all match puctuation exactly; or user might wrap-up into one line instead of two.  So try to allow flexibility.
    
    #header_actions[,template_label_lookup:=str_escape(label)]
    
    
  }
  
  
  
  #For the label "template_label_lookup"
  header_actions[,template_label_lookup:=paste0("^",str_escape(trimFunc(template_label_lookup)),"$")]
  
  #...but if we had {system} stuff that are replaced with ".*" str_escape will escape those to literal \\.\\* so undo that!
  header_actions[grepl("(\\\\\\.\\\\\\*)",template_label_lookup),
                 template_label_lookup:=gsub("([[:space:]]*\\\\\\.\\\\\\*[[:space:]]*)",".*",template_label_lookup)]
  
  # header_actions[,
  #                template_label_lookup:=gsub("\\\\*[\\.,!;:?][\\n\\s]*(?!\\*)","[\\\\.,!;:\\\\?\\\\s\\\\n]*",template_label_lookup,perl=T)]
  # 
  
  #when we insert .* for content inside {} 
  #if the text is, eg: This is a value:30
  #and the parse is,   This is a value:{XXX}
  #Then the result is: This is a value:.*
  #So if we have a literal "*" with punctuation and (maybe) a space
  #then replace it with a literal regexp expression [[:space:]\\\\.,!;:\\\\?]*
  #to match zero-plus spaces or puncrutations preceeding a literal * so that we can match :.*
  #Match literal *
  header_actions[,
                 template_label_lookup:=gsub("\\\\*[.,!;:?][[:space:]]*(?!\\*)","[[:space:].,!;:?]*",template_label_lookup,perl=T)]
  
  #For the "section"
  #adds .* AFTER the header section name, so any "Summary..."
  header_actions[,template_section_lookup:= paste0("^",str_escape(trimFunc(template_header_section_name)),"$")]
  
  

  #if there's a header action whose label is identical to a regular indicator, it means that the header actions are setup to overwrite the default.  So omit these
  #and where there is a match, the header will match any presence found in the document
  #however, for those where template_header_position is not NA, it means there was a concatenated label && in the template header and we DONT want to omit any where one/two of the
  #independent headers match as they must be matched in pairs
  rsf_labels <- rsf_labels[!label %in% header_actions[is.na(template_header_position),label]]
  
  
  header_actions <- header_actions[,
                                   .SD,
                                   .SDcols = names(rsf_labels)]
  
  #this is obsolete as template header section should not be NA
  rsf_labels <- rsf_labels[!(label %in% header_actions[is.na(template_header_section_name),label])]
  
  header_actions <- rbindlist(list(rsf_labels,
                               header_actions))
  
  header_actions <- unique(header_actions)
  
  setorder(header_actions,
           header_id,
           template_header_position, #This is for headers that have multi-language headers with "&&" where position 1 is first and position 2 is second
           na.last = TRUE)
  
  header_actions[,label_header_id:=.GRP,
                 by=.(header_id)]
  
 return (header_actions)
}
 