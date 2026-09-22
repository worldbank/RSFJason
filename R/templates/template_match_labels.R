

template_match_labels <- function(find_sections=NA, #match any section if NA, section may match regular expression, notably :ALL will be .*$
                         find_labels=NA,   #match any section if NA, template_label must always have an exact "normalized" match (also may match content)
                         search_sections=NA,
                         search_labels=NA,
                         match_id=NA,
                         match_postion=NA) {
  
  if (!is.na(find_sections) && nchar(find_sections)==0) find_sections <- NA
  if (!is.na(find_labels) && nchar(find_labels)==0) find_labels <- NA
  if (length(search_sections) != length(search_labels)) stop(paste0("seach_sections and search_labels should be equal length vectors to ensure which() returns equivalent indexes: ",
                                                                    length(search_sections)," vs ",length(search_labels)))
  
  #JUN2026: didn'tknow why "Lists" wasn't working and needed to be "lists" -- spent a while on this!
  find_sections <- tolower(find_sections)
  search_sections <- tolower(search_sections)
  search_labels <- tolower(search_labels)
  find_labels <- tolower(find_labels)
  
  if (all(is.na(find_sections)) && all(is.na(find_labels))) return (NULL)
  
  
  
  # add_start_stop <- function(str) {
  #   none <- grep("^\\^",str,invert = T)
  #   if (any(none)) str[none] <- paste0("^",str[none])
  #   
  #   none <- grep("\\$$",str,invert = T)
  #   if (any(none)) str[none] <- paste0(str[none],"$")
  #   
  #   return (str)
  # }
  
  if (!is.na(find_sections) && !is.na(find_labels)) {
    
    matches <- intersect(which(stringr::str_detect(string=search_sections,pattern=find_sections)),
                         which(stringr::str_detect(string=search_labels,pattern=find_labels)))
    
  } else if (!is.na(find_sections)) {
    
    matches <- which(stringr::str_detect(string=search_sections,pattern=find_sections))
    
  } else {
    
    matches <- which(stringr::str_detect(string=search_labels,pattern=find_labels))
    
  }
  
  if (length(matches)==0) { return(NULL) 
  } else { 
    
    return (list(match_id=match_id,
                 match_position=match_postion,
                 match_rows=unlist(matches)))
  }
}


