db_indicators_get_header_actions <- function(pool,
                                             template_id,
                                             rsf_pfcbl_id,
                                             rsf_indicators,
                                             detection=c("full","partial"), 
                                             # "full" will add pattern ^string$ and "partial" will add pattern ^.*string.*$
                                             # in other words, match the full header or any partial match.  For templates (like RSA) that have
                                             # discrete terms that are a specific header section, a partial match can help identify similar equivalent 
                                             # patterns of the same term. Whereas for QR template, each section is a list of complete headers that should be fully matched
                                             normalize=F) 
  {
  
  trimFunc <- NULL

  detection <- match.arg(detection)
  
  #superTrim calls trim.punct by default as true
  #normalizeLabel does a few more stuff, like remove accents; but not trim punct
  if (normalize==T) {
    trimFunc <- normalizeLabel
  } else {
    trimFunc <- superTrim
  }
  
  header_actions <- dbGetQuery(pool,"
      select tha.*,indf.indicator_id as formula_indicator_id
        from p_rsf.view_rsf_program_facility_template_header_actions tha
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
  header_actions[,stop:=as.numeric(NA)]
  header_actions[grepl("^:AFTER:(ROW|COL)\\d+$",template_header),
                 stop:=as.numeric(gsub("^:AFTER:(ROW|COL)(\\d+)$","\\2",template_header))]
  stop_actions <- header_actions[is.na(stop)==FALSE & action=="ignore"]
  
  #default allows facilities to overwrite program-level setups, for example.
  header_actions <- header_actions[action != "default"]
  
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
  
  #rsf labels only map to indicators and are the default matching
  rsf_labels[,
             `:=`(header_id=-.I,
                  template_header_section_name=as.character(NA),
                  template_section_lookup=as.character(NA),
                  template_label_lookup=paste0('^"?',str_escape(label),'"?$'), #ignore quoted headers
                  action="default",
                  template_header_position=as.numeric(NA),
                  map_formula_id=as.numeric(NA),
                  calculation_formula=as.character(NA),
                  map_check_formula_id=as.numeric(NA),
                  check_formula=as.character(NA),
                  stop=NA)]
  
  
  header_actions[,label_key:="SYS"]
  header_actions[,label:=trimFunc(template_header)] #for this template, use trimmed, not normalized (as parsing values are used and therefore don't normalize {} delimiter!)
  
  header_actions[,template_label_lookup:=str_escape(label)]
  
  #except, for titles that are purely "*"
  #and where JASON control codes inside {} have been inserted
  header_actions[,template_label_lookup:=gsub("\\\\\\{[^\\}]+\\\\\\}",
                                              ".*",
                                              template_label_lookup)]
  
  header_actions[,template_label_lookup:=gsub("^([[:punct:]]+|\\\\\\*)$",
                                              ".*",
                                              template_label_lookup)]
  
  #to ignore an entire section.
  header_actions[!is.na(template_header_section_name) &
                 grepl("^\\{IGNORE\\}$",template_header,ignore.case=T),
                 template_label_lookup:=".*"]

  if (detection=="full") {
    
    header_actions[,template_label_lookup:=paste0("^",str_escape(trimFunc(template_label_lookup)),"$")]
    
  } else {
    
    header_actions[,template_label_lookup:=paste0("^.*",str_escape(trimFunc(template_label_lookup)),".*$")]
  }
  
  #adds .* AFTER the header section name, so any "Summary..."
  header_actions[,template_section_lookup:= paste0("^",str_escape(trimFunc(template_header_section_name)),".*$")]
  
  #Is this still necessary?
  # header_actions[,template_section_lookup:= paste0("^",
  #                                                  gsub(":(all|any)$|:$","",
  #                                                       x=str_escape(
  #                                                         trimFunc(
  #                                                           template_header_section_name)),
  #                                                       ignore.case=T),
  #                                                  ".*",
  #                                                  "$")]
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
  
  
  # setnames(header_actions,
  #          old="remap_indicator_id",
  #          new="indicator_id")
  
 return (header_actions)
}