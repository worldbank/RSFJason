db_indicators_get_header_actions <- function(pool,
                                             template_id,
                                             rsf_pfcbl_id) {
  header_actions <- dbGetQuery(pool,"
      select tha.*
        from p_rsf.view_rsf_program_facility_template_header_actions tha
      where tha.rsf_pfcbl_id = $1::int
        and tha.template_id = $2::int
      order by header_id desc",
                               params=list(rsf_pfcbl_id,
                                           template_id))
  
  setDT(header_actions)
  
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
  
 
  # setnames(header_actions,
  #          old="remap_indicator_id",
  #          new="indicator_id")
  
 return (header_actions)
}