parse_template_RSA <- function(pool,
                               template_id,
                               rsf_facility_id, #only makes sense at facility level to parse an RSA
                               template_file,
                               rsf_indicators=db_indicators_get_labels(pool=pool),
                               rsf_indicator_formulas=db_indicators_get_formulas(pool=pool),
                               rsf_check_formulas=db_checks_get_formulas(pool=pool),
                               reporting_user_id,
                               status_message) 
{
  #rsf_program_id <- 286031
  # rsf_facility_id <- 286031
  #template_id <- 11
  # 
  # 
  # template_file <- paste0(c("C:",
  #                           "Users","SHeitmann","WBG","Dev","RSAs","SLGP - NBS","IFC_Bank of Samoa - Risk Sharing Agreement - Executed (06.16.2023).pdf"),
  #                         collapse="/")
  # 
  # template_file <- paste0(c("C:",
  #                           "Users","SHeitmann","WBG","Dev","RSAs","SLGP - NBS","IFC_Bank of Samoa - Risk Sharing Agreement - Executed (06.16.2023)2.pdf"),
  #                         collapse="/")
  # 
  # template_file <- paste0(c("C:",
  #                    "Users","SHeitmann","WBG","Dev","RSAs","CEU - RBUA","IFC_RBUA SME RSF - RSA signed and dated version.pdf"),
  #                    collapse="/")
  
  
  #file.exists(template_file)
  #setups 
  {
    text <- pdftools::pdf_text(template_file)
      
      #image_encoding <- strsplit(text,split="(?!\\n)[[:cntrl:]]",perl=T)
      #word_encoding <- strsplit(text[[1]],split="\\w+")[[1]]
  
      
      image_encoding <- lapply(text,stringr::str_extract_all,pattern="(?!\\n)[[:cntrl:]]")
      word_encoding <- lapply(text,stringr::str_extract_all,pattern="\\w+")
  
      
      ratios <- mapply(FUN=function(a,b) { length(unlist(a)) > length(unlist(b)) },image_encoding,word_encoding)
      
      #more than 25% of the pages have more control characters than words...so it's probably corrupted or scanned image o OCR text
    if (mean(ratios) > .25) {
      stop("This pdf appears to have been scanned in and pdf text interpreted from scanned images -- it is unrelaible and may have corrupted text.  Can you use Cntrl-F to search for words? Try to upload the Execution version instead of the signed, scanned version.")
      #text <- pdftools::pdf_ocr_text(pdf=template_file)
    }
  
    reporting_flags <- data.table(rsf_pfcbl_id=numeric(0),
                                  indicator_id=numeric(0),
                                  reporting_asof_date=as.Date(numeric(0)),
                                  check_name=character(0),
                                  check_message=character(0))
  }
  
  #Create the Pages with chapters, sections and bullets
  {
    pages <- lapply(seq_along(text),
                    FUN=function(p,text) {
  #p<-77
  #p<-55                    
                      content <- text[[p]]
                      
                      if (is.na(content) || nchar(content)==0) return(NULL)
                      
                      content_lines <- unlist(strsplit(content,
                                                       split="\\n"))
                                                       #split="\\n|[^[:alnum:][:punct:][:cntrl:][:space:]]"))
                      content_lines <- data.table(page=p,line=content_lines)
                      content_lines[,
                                    first_char:=sapply(line,
                                                       FUN=regexpr,
                                                       pattern="[[:alpha:]]")]
                      content_lines[,
                                    prev_start:=shift(first_char,n=1,type="lag")]
                      content_lines[,line_num:=.I]
                      
                      content_lines[,start:=TRUE]
                      #content_lines[grepl("^[[:space:]]",line) & first_char==prev_start,
                      #              start:=FALSE]
  
                      content_lines[!is.na(line) & nchar(line) > 0 & first_char==prev_start,
                                    start:=FALSE]
                      
                      #zero+ sapces followed by a non-printing character -- a bullet mark
                      content_lines[grepl("^[[:space:]]*[^[:alnum:][:punct:][:cntrl:][:space:]]",line),
                                    start:=TRUE]
  
                      #zero+ sapces followed by a lower-case "o" and 1+ spaces: a cirlce bullet is read-in as an "o"
                      content_lines[grepl("^[[:space:]]*o[[:space:]]+",line),
                                    start:=TRUE]
                      
                      content_lines[start==FALSE,
                                    line_num:=NA]
                      
                      content_lines <- tidyr::fill(content_lines,line_num)
                      content_lines <- content_lines[,
                                    .(line=paste0(line,collapse=" ")),
                                    by=.(page,line_num)]
                      
                      content_lines[,text:=sapply(line,
                                            FUN=function(x) {
                                              #remove non-printing characters
                                              x<-gsub("[^[:alnum:][:punct:][:cntrl:][:space:]]"," ",x)
                                              
                                              #remove pseudo bullets
                                              x<-gsub("^[[:space:]]*o[[:space:]]+"," ",x)
                                              x<-gsub("[[:space:]]{2,}"," ",x)
                                              trimws(x)
                                            })]
                      
                      content_lines <- content_lines[!(is.na(line) | nchar(line)==0),
                                                     .(page,text)]
                      return (content_lines)
  #old
                      # {
                      #   #line break with new line starting with 3+ spaces or tabs:
                      #   #treat as a section/paragraph break
                      #   content <- gsub(pattern="\\n[[:space:]\\t]{3,}",
                      #                   replacement="\n\n",
                      #                   x=content)
                      #   
                      #   #content <- gsub("[[:cntrl:]][^[:cntrl]]"," ",content)
                      #   paragraphs <- unlist(strsplit(content,
                      #                        split="\\n{2,}|[^[:alnum:][:punct:][:cntrl:][:space:]]"))
                      #   
                      #   if (any(grepl("\\n(?=[^$])",paragraphs,perl=T),na.rm=T)) {
                      # 
                      #     #paragraphs <- gsub("\\n(?=[^$])","\n\n",paragraphs,perl=T)
                      # 
                      #     paragraphs <- unlist(strsplit(paragraphs,
                      #                                   split="\\n(?=[^$])",
                      #                                   perl=T))
                      # 
                      #   }
                      #   
                      #   paragraphs <- lapply(paragraphs,
                      #                        gsub,
                      #                        pattern="\\n",
                      #                        replacement=" ")
                      #   
                      #   paragraphs <- lapply(paragraphs,
                      #                        FUN=function(x) {
                      #                          x<-gsub("[[:space:]]{2,}"," ",x)
                      #                          trimws(x)
                      #                        })
                      #   paragraphs <- unlist(paragraphs)
                      #   
                      #   if (length(paragraphs)==0) paragraphs <- "" #page is fully blank or rendered as image
                      #   
                      #   data.table(page=p,
                      #              paragraph=seq_along(paragraphs),
                      #              text=paragraphs)
                      # }
                      
                    },text=text)
    
    pages <- rbindlist(pages)
    pages[,
          `:=`(chapter=as.character(NA),
               chapter_title=as.character(NA),
               section=as.character(NA),
               section_title=as.character(NA),
               bullet_title=as.character(NA),
               index=.I)]
    
    pages[grepl("^INVESTMENT NUMBER [0-9]+$",text,ignore.case = T),
          `:=`(chapter="COVERPAGE",
               chapter_title=text)]
  
    pages[grepl("^TABLE OF CONTENTS.*$",text,ignore.case = T) & page==2,
          `:=`(chapter="TABLE OF CONTENTS",
               chapter_title="TABLE OF CONTENTS")]
    
    
    pages[grepl("^TABLE OF CONTENTS$",text,ignore.case = T),
          chapter:=toupper(text)]
    
    pages[grepl("^ARTICLE [IV]+$",text,ignore.case = T),
          chapter:=toupper(text)]
    
    # pages[grepl("^ARTICLE [IV]+[[:space:][:graph:]]+$",text,ignore.case = T),
    #       `:=`(chapter=gsub("^(ARTICLE [IV]+)[[:space:][:graph:]]+$","\\1",text),
    #            chapter_title=gsub("^ARTICLE [IV]+([[:space:][:graph:]]+)$","\\1",text))]
    
    pages[grepl("^ANNEX [A-Z]+$",text,ignore.case = T),
          chapter:=toupper(text)]
    
    # pages[grepl("^ARTICLE [A-Z]+[[:space:][:graph:]]+$",text,ignore.case = T),
    #       `:=`(chapter=gsub("^(ARTICLE [A-Z]+)[[:space:][:graph:]]+$","\\1",text),
    #            chapter_title=gsub("^ARTICLE [A-Z]+([[:space:][:graph:]]+)$","\\1",text))]
    
    pages[grepl("^SCHEDULE [0-9]+$",text,ignore.case = T),
          chapter:=toupper(text)]

    # pages[grepl("^SCHEDULE [0-9]+[[:space:][:graph:]]+$",text,ignore.case = T),
    #       `:=`(chapter=gsub("^(SCHEDULE [0-9]+)[[:space:][:graph:]]+$","\\1",text),
    #            chapter_title=gsub("^SCHEDULE [0-9]+([[:space:][:graph:]]+)$","\\1",text))]

        
    
    chapter_titles <- pages[index %in% pages[!is.na(chapter) & is.na(chapter_title),index+1],.(text,index=index-1)]
    pages[chapter_titles,
          chapter_title:=toupper(i.text),
          on=.(index)]
    
    pages <- tidyr::fill(pages,
                         chapter,
                         chapter_title,
                         .direction="down")
  
    #Valid RSA "chapter" headers
    #incorporate into TEMPLATE:HEADERS field
    {
      RSA_chapters <- unique(pages[,.(chapter,chapter_title)])
      
      recognized_RSA_chapters <- data.table(chapter=c('TABLE OF CONTENTS','ARTICLE I','ARTICLE II','ARTICLE III','ARTICLE IV','ARTICLE V','ARTICLE VI','ANNEX A','ANNEX B','SCHEDULE 1','SCHEDULE 2','SCHEDULE 3','SCHEDULE 4','SCHEDULE 5','SCHEDULE 6','SCHEDULE 7','SCHEDULE 8','SCHEDULE 9','SCHEDULE 10','SCHEDULE 11','SCHEDULE 12'),
                                            chapter_title=c('TABLE OF CONTENTS',
                                                            'DEFINITIONS AND INTERPRETATION',                     #!!
                                                            'RISK SHARING',                                       #!!
                                                            'REPRESENTATIONS AND WARRANTIES',
                                                            'PARTICULAR COVENANTS',
                                                            'TERMINATION',
                                                            'MISCELLANEOUS',                                      #??
                                                            'EXCLUSION LIST',
                                                            'SANCTIONABLE PRACTICES',
                                                            'ELIGIBILITY AND PORTFOLIO CRITERIA',                 #!!
                                                            'FORM OF PAYMENT DEMAND',
                                                            'FORM OF LETTER TO BENEFICIARY\'S AUDITORS',
                                                            'FORM OF CERTIFICATE OF INCUMBENCY AND AUTHORITY',
                                                            'FORM OF E&S PERFORMANCE REPORT',
                                                            'INFORMATION TO BE INCLUDED IN THE QUARTERLY REPORT', #!!
                                                            'FORM OF DEVELOPMENT IMPACT PORTFOLIO REPORT',
                                                            'FORM OF LOCAL COUNSEL’S LEGAL OPINION',
                                                            'FORM OF SERVICE OF PROCESS LETTER',
                                                            'FORM OF EXTENSION REQUEST',
                                                            'FORM OF EXTENSION CONFIRMATION',
                                                            'E&S ACTION PLAN'))
      
      unrecognized <- fsetdiff(RSA_chapters[chapter!="COVERPAGE"],
                               recognized_RSA_chapters)
      
      if (!empty(unrecognized)) stop("Unrecognized chapters")
    }
    
    
    #For definitions, set each definition as a "Section"
    pages[chapter_title=="DEFINITIONS AND INTERPRETATION" & 
          grepl('^".+"[[:space:]]+.*$',text,ignore.case = T),
          `:=`(section=gsub('^"([^"]+)"[[:space:]]+.*$',"\\1",text),
               section_title=gsub('^"([^"]+)"[[:space:]]+.*$',"\\1",text))]
    
    
    #For eligibility, set each sub-header as section
    pages[chapter_title=="ELIGIBILITY AND PORTFOLIO CRITERIA" & 
            grepl("^[A-Z]{1,3}\\..*CRITERIA$",text,ignore.case = T),
          `:=`(section=gsub("^([A-Z]{1,3})\\.[[:space:]]*[A-Z[:space:]]+CRITERIA$","\\1",text),
               section_title=gsub("^[A-Z]{1,3}\\.[[:space:]]*([A-Z[:space:]]+CRITERIA)$","\\1",text))]
    
    #pages[chapter_title=="INFORMATION TO BE INCLUDED IN THE QUARTERLY REPORT"]
    #incorporate into TEMPLATE:INFORMATION TO BE INCLUDED IN THE QUARTERLY REPORT:HEADERS field
    #View(pages[chapter_title=="INFORMATION TO BE INCLUDED IN THE QUARTERLY REPORT",text])
    qr_headers <- c("Basic Facility Information",
                    "Facility information",
                    "Facility Pricing",
                    "Facility Timeline",
                    "Facility Cashflow Information",
                    "Facility Performance Information for Current Quarter",
                    "Other")
    
    pages[chapter_title=="INFORMATION TO BE INCLUDED IN THE QUARTERLY REPORT" &
          normalizeLabel(text) %in% normalizeLabel(qr_headers),
          `:=`(section=text,
               section_title=text)]
    
    #"Section 1.0.1 meaningful from ARTICLE II forward
    #mindex1 <- pages[chapter_title=="DEFINITIONS AND INTERPRETATION",min(index)]
    mindex2 <- pages[chapter_title=='RISK SHARING',min(index)]
    
    pages[is.na(section) & 
          grepl("^Section\\s+[0-9\\.]+",text,ignore.case = T),
          `:=`(section=gsub("^(Section\\s+[0-9]+\\.[0-9]+).*$","\\1",text,ignore.case=T),
               section_title=gsub("^Section\\s+[0-9]+\\.[0-9]+\\.([^\\.]+).*$","\\1",text,ignore.case=T))]
    
    
    
    pages[,
          c("section","section_title"):=tidyr::fill(.SD,section,section_title),
          by=.(chapter_title),
          .SDcols = c("section","section_title")]
    
    pages[index>=mindex2 & 
          !is.na(section) &
          grepl("^\\([a-z]\\)",text),
          bullet_title:=gsub("^(\\([a-z]\\)).*$","\\1",text,ignore.case=T)]
  
    pages[index>=mindex2 & 
            !is.na(section) &
            grepl("^\\([ivx]+\\)",text),
          bullet_title:=gsub("^(\\([ivx]+\\)).*$","\\1",text,ignore.case=T)]
  
    pages[chapter_title=="INFORMATION TO BE INCLUDED IN THE QUARTERLY REPORT" & !is.na(section_title),
          bullet_title:=1:.N,
          by=.(section_title)]
    
    pages[,
          `:=`(section=trimws(section),
               section_title=trimws(section_title),
               bullet_title=trimws(bullet_title))]
    
    pages[index>=mindex2,
          bullet_title:=tidyr::fill(.SD,bullet_title),
          by=.(chapter_title,section_title),
          .SDcols = c("bullet_title")]
    
    #page numbers
    pages <- pages[grepl("^\\d+$",text)==FALSE]
   
    pages[!is.na(bullet_title),
          bullet_rank:=1:.N,
          by=.(chapter_title,section_title)]
    
    pages[bullet_rank==1,
          bullet_type:=fcase(grepl("\\([a-h]\\)",bullet_title),"letters",
                             grepl("\\(i+\\)",bullet_title),"numerals",
                             grepl("\\(?\\d+\\)?\\.?",bullet_title),"numbers",
                             default="unknown")]
    
    pages[!is.na(bullet_title),
         bullet_type:=tidyr::fill(.SD,bullet_type),
         by=.(chapter,section),
         .SDcols = c("bullet_type")]
    pages[,bullet_title:=gsub("[\\(\\)]","",bullet_title)]
    
    bullet_sequence <- function(bsequence,btype) {
      
      bs <- tolower(bsequence)
      btype <- unique(btype)
      if (length(btype) != 1) stop(paste0("Bullet type mismatch: ",paste(paste(bsequence," ",btype),collapse=", ")))
      for (i in seq_along(bs)) {
        
        if (i==1) next;
        if (bs[i]==bs[i-1]) next;
        
        if (btype=="letters" && bs[i]==letters[which(letters==bs[i-1])+1]) next;
        if (btype=="numerals" && as.numeric(as.roman(bs[i]))== (as.numeric(as.roman(bs[i-1]))+1)) next;
        if (btype=="numbers" && as.numeric(bs[i])==(as.numeric(bs[i-1])+1)) next;
        
        bs[i] <- bs[i-1]
      }
      return (bs)
    }
    
    pages[!is.na(bullet_title),
          bullet_title:=bullet_sequence(bullet_title,bullet_type),
          by=.(chapter,section)]
}
  
  #Create RSA
  {
    
    rsa <- pages[!(normalizeLabel(text) %in% c(normalizeLabel(chapter),
                                               normalizeLabel(chapter_title),
                                               normalizeLabel(section),
                                               normalizeLabel(section_title),
                                               normalizeLabel(paste0(chapter," ",chapter_title)),
                                               normalizeLabel(paste0(section," ",section_title)))),
                 .(text=paste0(text,collapse="\n "),
                    pages=paste0(unique(page),collapse=",")),
                 by=.(chapter,chapter_title,section,section_title,bullet_title)]
    rsa[,
        full_title:=superTrim(paste0(
          ifelse(is.na(chapter_title),
                 "NONE",
                 chapter_title),":",
          ifelse(is.na(section_title),
                 "NONE",
                 section_title),
          ifelse(is.na(bullet_title),
                 "",
                 paste0(":",bullet_title))))]
    
    rsa[,
        section_title:=superTrim(section_title)]
    
    rsa[,
        chapter_title:=superTrim(chapter_title)]
    
    #Effectively "empty rows"
    # rsa <- rsa[(normalizeLabel(text) %in% c(normalizeLabel(chapter),
    #                                          normalizeLabel(chapter_title),
    #                                          normalizeLabel(section),
    #                                          normalizeLabel(section_title),
    #                                          normalizeLabel(paste0(chapter," ",chapter_title)),
    #                                          normalizeLabel(paste0(section," ",section_title))))]
    
    {
      coverpage <- rsa[chapter=="COVERPAGE"] #not superTrim()
      project_id <- as.numeric(gsub("INVESTMENT NUMBER (\\d+)$","\\1",coverpage$chapter_title,ignore.case=T))
      
      #resolve reporting_asof_date
      {
        signing_date <- NULL
        reporting_asof_date <- NULL
        
        if (grepl("dated",coverpage$text,ignore.case = T)) {
          date_text <- gsub("[[:cntrl:]]+"," ",coverpage$text)
          
          if (grepl(paste0("(",paste0(c(month.name,month.abb),collapse="|"),")[[:space:]]+\\d{1,2}\\s*Dated _+,\\s*\\d{4}"),date_text,ignore.case=T)) {
            signing_date <- gsub(paste0("^.*(",paste0(c(month.name,month.abb),collapse="|"),")[[:space:]]+(\\d{1,2})[[:space:]]+Dated .*,\\s*(\\d{4}).*$"),
                                 "\\1 \\2, \\3",
                                 date_text,
                                 ignore.case=T)
            signing_date <- mdy(signing_date)
            if (is.na(signing_date)) {
              stop(paste0("Signing date in M-D-Y format appears to be stated in the COVERPAGE: however, this cannot be resolved.  Try re-naming the file-name by including the RSA signing date in YYYY-MM-DD format at the end of the filename.  For example: ",
                          '"',paste0(file_path_sans_ext(basename(template_file))," - signed ",as.character(today()),".",file_ext(template_file)),'"'))
            }
          }
        }
        
        if (is.null(signing_date)) {
          if (grepl("\\d{4}-\\d{1,2}-\\d{1,2}",basename(template_file))) {
            signing_date <- gsub("^.*(\\d{4}-\\d{1,2}-\\d{1,2}).*$","\\1",basename(template_file))
            signing_date <- ymd(signing_date)
            if (is.na(signing_date)) {
              stop(paste0("RSA signing date appears in the filename.  But is it in format YYYY-MM-DD?"))
            }
          } else {
            stop(paste0("Failed to resolve the signing date for this RSA. Include this information in the filename in YYYY-MM-DD format. For example: ",
                 '"',paste0(file_path_sans_ext(basename(template_file))," - signed ",as.character(today()),".",file_ext(template_file)),'"'))
          }
        }
        
        reporting_asof_date <- ceiling_date(signing_date,"quarter")-1
      }
      
      rsa <- rsa[!normalizeLabel(chapter) %in% normalizeLabel(c("COVERPAGE","TABLE OF CONTENTS"))]
      
      # #qr <- rsa[chapter_title==superTrim("INFORMATION TO BE INCLUDED IN THE QUARTERLY REPORT")]
      # qr <- rsa[chapter_title==superTrim("INFORMATION TO BE INCLUDED IN THE QUARTERLY REPORT"),
      #           .(text=unlist(strsplit(text,split="[[:cntrl:]]"))),
      #           by=.(chapter,chapter_title,section,section_title,bullet_title,full_title,pages)]
      # 
      # rsa <- rsa[!chapter_title==superTrim("INFORMATION TO BE INCLUDED IN THE QUARTERLY REPORT")]
      # 
      # #some bullet points are read-in as letter "o"
      # qr[,text:=trimws(gsub("^[[:space:]]?o[[:space:]]+","",text))]
      # 
      # setcolorder(qr,neworder=names(rsa))
      # rsa <- rbindlist(list(rsa,qr))
      
    }
  }
  
  {
    
    rsf_labels <- rbindlist(rsf_indicators$labels)
  
    #use superTrim() over label_normalized
    rsf_labels <- unique(rsf_labels[,.(indicator_id,indicator_header_id,label_key,label=superTrim(label))])
    
    rsf_labels[,
               `:=`(template_header_section_name=as.character(NA),
                    template_section_lookup=as.character(NA),
                    template_label_lookup=paste0('^"?',str_escape(label),'"?$'), #ignore quoted headers
                    action="default",
                    template_header_encounter_index=0,
                    template_header_position=as.numeric(NA))]
    
    header_actions <- db_indicators_get_header_actions(pool=pool,
                                                       template_id=11,
                                                       rsf_pfcbl_id=rsf_facility_id)
    header_actions[,label_key:="SYS"]
    header_actions[,label:=superTrim(template_header)] #for this template, use trimmed, not normalized (as parsing values are used and therefore don't normalize {} delimiter!)
  
    #headers not wrapped in quotes
    #escape regex control codes
    header_actions[,template_label_lookup:=str_escape(label)]
    
    #except, for titles that are purely "*"
    #and where JASON control codes inside {} have been inserted
    header_actions[,template_label_lookup:=gsub("\\\\\\{[^\\}]+\\\\\\}",
                                                ".*",
                                                template_label_lookup)]
  
    header_actions[,template_label_lookup:=gsub("^([[:punct:]]+|\\\\\\*)$",
                                                ".*",
                                                template_label_lookup)]
  
    header_actions[is.na(template_label_lookup) | nchar(template_label_lookup)==0,
                   template_label_lookup:=".*"]
    
    header_actions[,template_section_lookup:= paste0("^",
                                                 gsub(":(all|any)$|:$","",
                                                      x=superTrim(template_header_section_name),
                                                      ignore.case=T),
                                                 ".*",
                                                 "$")]
    header_actions <- header_actions[,
                                     .SD,
                                     .SDcols = names(rsf_labels)]
    
    rsf_labels <- rsf_labels[!(label %in% header_actions[is.na(template_header_section_name),label])]
    rsf_labels <- rbindlist(list(rsf_labels,
                                 header_actions))
    
    setorder(rsf_labels,
             indicator_header_id,
             template_header_position,
             template_header_encounter_index,
             na.last = TRUE)
  
    
    rsa[,action:=as.character(NA)]
    
    
    labelMatches <- function(find_sections=NA, #match any section if NA, section may match regular expression, notably :ALL will be .*$
                             find_labels=NA,   #match any section if NA, template_label must always have an exact "normalized" match (also may match content)
                             search_sections=NA,
                             search_labels=NA,
                             match_id=NA,
                             fuzzy=TRUE) {
  
      if (!is.na(find_sections) && nchar(find_sections)==0) find_sections <- NA
      if (!is.na(find_labels) && nchar(find_labels)==0) find_labels <- NA
      
      if (is.na(find_sections) && is.na(find_labels)) return (NULL)
      add_start_stop <- function(str) {
        none <- grep("^\\^",str,invert = T)
        if (any(none)) str[none] <- paste0("^",str[none])
  
        none <- grep("\\$$",str,invert = T)
        if (any(none)) str[none] <- paste0(str[none],"$")
        
        return (str)
      }
      
      if (!is.na(find_sections) && !is.na(find_labels)) {
        # if (fuzzy==FALSE) {
        #   find_sections <- add_start_stop(find_sections)
        #   find_labels <- add_start_stop(find_labels)
        #   # find_sections <- paste0("^",find_sections,"$")
        #   # find_labels <- paste0("^",find_labels,"$")
        # }
        matches <- intersect(grep(find_sections,x=search_sections,ignore.case=T),
                             grep(find_labels,x=search_labels,ignore.case=T))
      } else if (!is.na(find_sections)) {
        # if (fuzzy==FALSE) {
        #   #find_sections <- paste0("^",find_sections,"$")
        #   find_sections <- add_start_stop(find_sections)
        # }
        
        matches <- grep(find_sections,x=search_sections,ignore.case=T)
      } else {
        # if (fuzzy==FALSE) {
        #   #find_labels <- paste0("^",find_labels,"$")
        #   find_labels <- add_start_stop(find_labels)
        # }
        
        matches <- grep(find_labels,x=search_labels,ignore.case=T)
      }
      
      if (length(matches)==0) { return(NULL) 
      } else { 
        return (list(match_id=match_id,
                     match_rows=unlist(matches)))
      }
    }
    
    #rsa[,text_original:=text]
    rsa[,text:=superTrim(text)]
    #Header section is "fuzzy" and labels are "fuzzy" (which will identify partial sections)
    m1 <-mapply(labelMatches,
                find_sections=rsf_labels$template_section_lookup,
                find_labels=rsf_labels$template_label_lookup,
                match_id=rsf_labels$indicator_header_id,
                MoreArgs=list(search_sections=rsa$full_title,
                              search_labels=rsa$text, #just superTrim above to avoid re-doing elsewhere or matches for parse action
                              #search_labels=superTrim(rsa$text),
                              fuzzy=T),
                USE.NAMES = F)
    
    # #header section isn't fuzzy and exactly matches the chapter_title; and labels fully match the rsa section's text
    # m2<-mapply(labelMatches,
    #           find_sections=superTrim(rsf_labels$template_header_section_name),
    #           find_labels=rsf_labels$template_label_lookup,
    #           match_id=rsf_labels$indicator_header_id,
    #           MoreArgs=list(search_sections=superTrim(rsa$chapter_title),
    #                         search_labels=superTrim(rsa$text),
    #                         fuzzy=F),
    #           USE.NAMES = F);
    # 
    # #header section isn't fuzzy and exactly matches the chapter_title:section_title; and labels fully match the rsa section's text
    # m3<-mapply(labelMatches,
    #           find_sections=superTrim(rsf_labels$template_header_section_name),
    #           find_labels=rsf_labels$template_label_lookup,
    #           match_id=rsf_labels$indicator_header_id,
    #           MoreArgs=list(search_sections=superTrim(paste0(rsa$chapter_title,":",rsa$section_title)),
    #                         search_labels=superTrim(rsa$text),
    #                         fuzzy=F),
    #           USE.NAMES = F);
    # 
    # 
    # m4<-mapply(labelMatches,
    #            find_sections=superTrim(rsf_labels$template_header_section_name),
    #            find_labels=normalizeLabel(rsf_labels$label),
    #            match_id=rsf_labels$indicator_header_id,
    #            MoreArgs=list(search_sections=superTrim(paste0(rsa$chapter_title,":",rsa$section_title)),
    #                          search_labels=normalizeLabel(rsa$text),
    #                          fuzzy=F),
    #            USE.NAMES = F);
    
    # header_matches <- rbindlist(list(rbindlist(m1),
    #                                  rbindlist(m2),
    #                                  rbindlist(m3),
    #                                  rbindlist(m4)))
    
    header_matches <- unique(rbindlist(m1))
    
    header_matches[,n:=.N,
                   by=.(match_rows)]
    
    if (any(header_matches$n>1)) stop("Multiple header match actions found -- tbd only an issue for remap to different indicators, okay to remap and parse")
    
    set(rsa,
        i=header_matches$match_rows,
        j="indicator_header_id",
        value=header_matches$match_id)

    #For RSA template, only "parse" settings are expected to extract setup data
    #Otherwise, actions are expected to subscribe to indicators, formula definitions and checks.
    rsa[,
        `:=`(action=as.character(NA),
             indicator_id=as.numeric(NA),
             indicator_formula_id=as.numeric(NA),
             check_formula_id=as.numeric(NA))]
    
    rsa[rsf_labels,
        `:=`(action=i.action,
             indicator_id=i.indicator_id),
        on=.(indicator_header_id)]
    
   
}

  #default
  #ignore
  #remap
  #unmap
  #check
  #calculate
  #parse
  
  rsa <- rsa[!action %in% "ignore"]
  
  #Unfound: will asign to entity_reporting
  {
    unfound_labels <- rsa[is.na(action)==TRUE]
    rsa <- rsa[is.na(action)==FALSE]
    
    if (!empty(unfound_labels)) {
      
      unfound_labels[,
                     `:=`(rsf_pfcbl_id=rsf_facility_id,
                          indicator_id=as.numeric(NA), #will be auto-assigned to reporting indicator
                          reporting_asof_date=reporting_asof_date,
                          check_name="sys_flag_indicator_not_found",
                          check_message=paste0(full_title," pdf page ",pages,":: ",text))]
      
      unfound_labels <- unfound_labels[,.(rsf_pfcbl_id,
                                          indicator_id,
                                          reporting_asof_date,
                                          check_name,
                                          check_message)]
      
      reporting_flags <- rbindlist(list(reporting_flags,
                                        unfound_labels))
    }
  }

  rsa[rsf_indicators,
      indicator_name:=i.indicator_name,
      on=.(indicator_id)]
  
  rsa[rsf_indicator_formulas,
      `:=`(indicator_id=i.indicator_id,
           indicator_name_formula_title=i.indicator_name_formula_title),
      on=.(indicator_formula_id=formula_id)]

  rsa[rsf_check_formulas,
      check_name_formula_title:=i.check_name_formula_title,
      on=.(check_formula_id=check_formula_id)]
  
  mapped_labels <- rsa[action=="remap" | action=="default"]
  if (!empty(mapped_labels)) {
    mapped_labels <- mapped_labels[,
                                   .(rsf_pfcbl_id=rsf_facility_id,
                                     indicator_id, #will be auto-assigned to reporting indicator
                                     reporting_asof_date=reporting_asof_date,
                                     check_name='sys_flag_template_match_actions',
                                     check_message=paste0(indicator_name," mapped from PDF page ",pages," [",full_title,"]: ",text))]
    
    reporting_flags <- rbindlist(list(reporting_flags,
                                      mapped_labels))
  }
  
  mapped_labels <- rsa[action=="calculate"]
  if (!empty(mapped_labels)) {
    mapped_labels <- mapped_labels[,
                                   .(rsf_pfcbl_id=rsf_facility_id,
                                     indicator_id, #will be auto-assigned to reporting indicator
                                     reporting_asof_date=reporting_asof_date,
                                     check_name='sys_flag_template_match_actions',
                                     check_message=paste0(indicator_name_formula_title," mapped from PDF page ",pages," [",full_title,"]: ",text))]
    
    reporting_flags <- rbindlist(list(reporting_flags,
                                      mapped_labels))
  }
  
  mapped_labels <- rsa[action=="check"]
  if (!empty(mapped_labels)) {
    
    mapped_labels <- mapped_labels[,
                                   .(rsf_pfcbl_id=rsf_facility_id,
                                     indicator_id=as.numeric(NA), #will be auto-assigned to reporting indicator
                                     reporting_asof_date=reporting_asof_date,
                                     check_name='sys_flag_template_match_actions',
                                     check_message=paste0(check_name_formula_title," mapped from PDF page ",pages," [",full_title,"]: ",text))]
    
    reporting_flags <- rbindlist(list(reporting_flags,
                                      mapped_labels))
  }
  
  
  #PARSING: The only area for data collection within the RSA template
  {
    parsing <- rsa[action=="parse"]
    if (empty(parsing)) parsing <- NULL
    
    rsa <- rsa[action != "parse"]
    
    if (!empty(parsing)) {
      parsing[header_actions[action=="parse"],
              `:=`(parse_label=i.label,
                   parse_label_lookup=i.template_label_lookup),
              on=.(action,
                   indicator_header_id)]
    
      #testing    
      #parsing[,parse_label:=("\"ifc maximum risk amount\" us\\${facility_ifc_maximum_risk_amount#usd} as may be reduced from time to time in accordance with section {test_indicator}\\(b\\) \\(costs\\)")]
      #parsing[,parse_label_lookup:=("\"ifc maximum risk amount\" us\\$.* as may be reduced from time to time in accordance with section .*\\(b\\) \\(costs\\)")]
      # 
      parsing[,
              parse_label_lookup:=gsub(pattern="\\.\\*","(.*)",parse_label_lookup)]
      
      parsing[,
              parse_keys:=lapply(parse_label,FUN=stringr::str_extract_all,pattern="\\{[^\\}]+\\}")]
      
      parsing[,
              parse_values:=mapply(FUN=function(pattern,string) {
                                     values <- stringr::str_match_all(pattern=pattern,
                                                                      string=string)
                                     values <- unlist(values)
                                     
                                     if (values[[1]] != string) { stop(paste0("Parse values expects first value ",values[[1]]," == ",string)) }
                                     
                                     values <- values[-1] #pattern will match the entire string, as well as the capture group(s).  
                                                          #So omit the entire string, as it will be returned at index 1
                                     values
                                     
                                   },
                                   pattern=parse_label_lookup,
                                   string=text,
                                   SIMPLIFY = F,
                                   USE.NAMES = F)]
      
      parsing[,id:=1:.N]
      parsing[,.(id,
                 full_title,
                 parse_label,
                 text,
                 pages,
                 parse_keys)]
      
      pkv <- mapply(FUN=function(a,b) {
    
        length(a) == length(b)
      },a=parsing$parse_keys,b=parsing$parse_values)
      
      if (!all(pkv)) stop(paste0("Failed to parse equal sets for parse keys and values: 
      ",paste0(parsing[which(!pkv),text],collapse="\n AND ALSO \n")))
      
      parsing <- parsing[,
                         .(indicator_name=unlist(parse_keys,recursive = F),
                            data_value=unlist(parse_values,recursive=F)),
                         by=.(id,parse_label,full_title,text,pages)]
      parsing[,data_unit:=as.character(NA)]
      parsing[,
              indicator_name:=gsub("[{}]","",indicator_name)]
      
      parsing[grepl("#.*$",indicator_name),
              `:=`(data_unit=gsub("^.*#(.*)$","\\1",indicator_name),
                   indicator_name=gsub("^(.*)#.*$","\\1",indicator_name))]
      
      
      #The parse lookup will have gone through superTrim() and so upper-case letters in the indicator_name may be transformed.
      parsing[rsf_indicators[,
                             .(indicator_id,
                               indicator_name=superTrim(indicator_name))],
              indicator_id:=i.indicator_id,
              on=.(indicator_name)]
      
      parsing[rsf_indicators,
              indicator_name:=i.indicator_name,
              on=.(indicator_id)]
    
      {
        unfound_labels <- parsing[is.na(indicator_id)]
        parsing <- parsing[is.na(indicator_id)==FALSE]
        
        if (!empty(unfound_labels)) {
          
          unfound_labels[,
                         `:=`(rsf_pfcbl_id=rsf_facility_id,
                              indicator_id=as.numeric(NA), #will be auto-assigned to reporting indicator
                              reporting_asof_date=reporting_asof_date,
                              check_name="sys_flag_indicator_not_found",
                              check_message=paste0("Failed to map '",indicator_name,"' for header: ",parse_label," USING [",full_title,"] pdf page ",pages,":: ",text))]
          
          unfound_labels <- unfound_labels[,.(rsf_pfcbl_id,
                                              indicator_id,
                                              reporting_asof_date,
                                              check_name,
                                              check_message)]
          
          unfound_labels[,check_message:=gsub(pattern="\\\\","",check_message)]
          
          reporting_flags <- rbindlist(list(reporting_flags,
                                            unfound_labels))
        }
      } 
      
      parsed_labels <- parsing[,
                               .(rsf_pfcbl_id=rsf_facility_id,
                                 indicator_id, #will be auto-assigned to reporting indicator
                                 reporting_asof_date=reporting_asof_date,
                                 check_name='sys_flag_template_match_actions',
                                 check_message=paste0(indicator_name,"[",
                                                      ifelse(is.na(data_value),
                                                             "{NOTHING}",
                                                             data_value),
                                                      ifelse(is.na(data_unit),
                                                             "",
                                                             paste0(" ",toupper(data_unit))),
                                                      "] parsed from PDF page ",pages," [",full_title,"]: ",text))]
      
    
      
      reporting_flags <- rbindlist(list(reporting_flags,
                                        parsed_labels))
      
      
      parsing[,
              reporting_asof_date:=reporting_asof_date]
      
      parsing[,SYSID:=rsf_facility_id]
      
      pfcbl_category <- rsf_indicators[indicator_id %in% parsing[,indicator_id],unique(data_category)]
      if (!all(pfcbl_category %in% c("facility"))) {
        stop("RSA template parsing can only map to facility-level indicators.  Double check the template header mapping settings.")
      }
    
    }
  }
 
  
  #Only "PARSING" will generate data
  #other actions generate actions
  template_data <- parsing[!is.na(indicator_name),
                           .(reporting_template_row_group=paste0(1:.N,"RSA"),
                             SYSID,
                             reporting_asof_date,
                             indicator_id,
                             indicator_name,
                             reporting_submitted_data_value=data_value,
                             reporting_submitted_data_unit=data_unit,
                             reporting_submitted_data_formula=as.character(NA))]
  
  
  actions <- rbindlist(list(rsa[,.(action,indicator_id,indicator_formula_id,check_formula_id,comments=paste0(full_title," page ",pages,":: ",text))],
                            parsing[!is.na(indicator_id),
                                    .(action="remap",
                                      indicator_id,
                                      indicator_formula_id=as.numeric(NA),
                                      check_formula_id=as.numeric(NA),
                                      comments=paste0(full_title," page ",pages,":: ",text))]))
  
  
  actions <- actions[!(action=="ignore")] #ignore means ignore... do nothing
  actions <- actions[!(action=="parse")] #parse should already have been managed and re-combined to remap above.
  
  #conn <- poolCheckout(pool)
  #dbBegin(conn)
  #dbRollback(conn)
  act <- actions[!is.na(indicator_id) & action %in% c("default","remap")] #default is only meaningful for indicators that are mapped via template names
  if (!empty(act)) {
    poolWithTransaction(pool, function(conn) { 
      dbExecute(conn,"create temp table _act(indicator_id int,comments text) on commit drop;")  
      dbAppendTable(conn,
                    name="_act",
                    value=act[,.(indicator_id,comments)])
      
      dbExecute(conn,"
        insert into p_rsf.rsf_program_facility_indicators(rsf_pfcbl_id,
                                                          indicator_id,
                                                          formula_id,
                                                          rsf_program_id,
                                                          rsf_facility_id,
                                                          is_subscribed,
                                                          is_auto_subscribed,
                                                          subscription_comments,
                                                          comments_user_id)
       select
         ids.rsf_pfcbl_id,
         act.indicator_id,
         indf.formula_id,
         ids.rsf_program_id,
         ids.rsf_facility_id,
         true as is_subscribed,
         false as is_auto_subscribed,
         act.comments as subscription_comments,
         'SYSTEM: ' || $2::text
       from p_rsf.rsf_pfcbl_ids ids
       cross join _act act
       left join p_rsf.indicator_formulas indf on indf.indicator_id = act.indicator_id
                                              and indf.is_primary_default = true
       where ids.rsf_pfcbl_id = $1::int
         and not exists(select * from  p_rsf.rsf_program_facility_indicators pfi
                        where pfi.rsf_pfcbl_id = $1::int
                          and pfi.indicator_id = act.indicator_id
                          and pfi.is_subscribed is true
                          and pfi.is_auto_subscribed is false)
       on conflict(rsf_pfcbl_id,indicator_id)
       do update
       set 
        formula_id = EXCLUDED.formula_id,
        is_subscribed = EXCLUDED.is_subscribed,
        is_auto_subscribed = EXCLUDED.is_auto_subscribed,
        subscription_comments = concat(rsf_program_facility_indicators.subscription_comments || ' \n' || (now()::date)::text || ': ',EXCLUDED.subscription_comments),
        comments_user_id = EXCLUDED.comments_user_id
           
       ",
      params=list(rsf_facility_id,
                  reporting_user_id))
    })
  }
  
  act <- actions[!is.na(indicator_id) & action %in% c("unmap")] #default is only meaningful for indicators that are mapped via template names
  if (!empty(act)) {
    poolWithTransaction(pool, function(conn) { 
      dbExecute(conn,"create temp table _act(indicator_id int,comments text) on commit drop;")  
      dbAppendTable(conn,
                    name="_act",
                    value=act[,.(indicator_id,comments)])
      
      dbExecute(conn,"
        insert into p_rsf.rsf_program_facility_indicators(rsf_pfcbl_id,
                                                          indicator_id,
                                                          formula_id,
                                                          rsf_program_id,
                                                          rsf_facility_id,
                                                          is_subscribed,
                                                          is_auto_subscribed,
                                                          subscription_comments,
                                                          comments_user_id)
       select
         ids.rsf_pfcbl_id,
         act.indicator_id,
         indf.formula_id,
         ids.rsf_program_id,
         ids.rsf_facility_id,
         false as is_subscribed,
         false as is_auto_subscribed,
         act.comments as subscription_comments,
         'SYSTEM Unsubscribed: ' || $2::text
       from p_rsf.rsf_pfcbl_ids ids
       cross join _act act
       left join p_rsf.indicator_formulas indf on indf.indicator_id = act.indicator_id
                                              and indf.is_primary_default = true
       where ids.rsf_pfcbl_id = $1::int
         and not exists(select * from  p_rsf.rsf_program_facility_indicators pfi
                        where pfi.rsf_pfcbl_id = $1::int
                          and pfi.indicator_id = act.indicator_id
                          and pfi.is_subscribed is false
                          and pfi.is_auto_subscribed is false)
       on conflict(rsf_pfcbl_id,indicator_id)
       do update
       set 
        formula_id = EXCLUDED.formula_id,
        is_subscribed = EXCLUDED.is_subscribed,
        is_auto_subscribed = EXCLUDED.is_auto_subscribed,
        subscription_comments = concat(rsf_program_facility_indicators.subscription_comments || ' \n' || (now()::date)::text || ': ',EXCLUDED.subscription_comments),
        comments_user_id = EXCLUDED.comments_user_id
       ",
                params=list(rsf_facility_id,
                            reporting_user_id))
    })
  }
  
  act <- actions[!is.na(indicator_id) & action %in% c("check")] #default is only meaningful for indicators that are mapped via template names
  if (!empty(act)) {
    poolWithTransaction(pool, function(conn) { 
      dbExecute(conn,"create temp table _act(check_formula_id int,comments text) on commit drop;")  
      dbAppendTable(conn,
                    name="_act",
                    value=act[,.(check_formula_id,comments)])
      
      dbExecute(conn,"
        insert into p_rsf.rsf_program_facility_checks(rsf_pfcbl_id,
                                                      check_formula_id,
                                                      indicator_check_id,
                                                      rsf_program_id,
                                                      rsf_facility_id,
                                                      is_subscribed,
                                                      is_auto_subscribed,
                                                      subscription_comments,
                                                      comments_user_id)
       select
         ids.rsf_pfcbl_id,
         act.check_formula_id,
         icf.indicator_check_id,
         ids.rsf_program_id,
         ids.rsf_facility_id,
         true as is_subscribed,
         false as is_auto_subscribed,
         act.comments as subscription_comments,
         'SYSTEM Subscribed: ' || $2::text
       from p_rsf.rsf_pfcbl_ids ids
       cross join _act act
       inner join p_rsf.indicator_check_formulas icf on icf.check_formula_id = act.check_formula_id
       where ids.rsf_pfcbl_id = $1::int
         and not exists(select * from  p_rsf.rsf_program_facility_checks pfc
                        where pfc.rsf_pfcbl_id = $1::int
                          and pfc.check_formula_id = act.check_formula_id
                          and pfc.indicator_check_id = icf.indicator_check_id
                          and pfc.is_subscribed is false
                          and pfc.is_auto_subscribed is false)
       on conflict(rsf_pfcbl_id,check_formula_id)
       do update
       set 
        indicator_check_id = EXCLUDED.indicator_check_id,
        is_subscribed = EXCLUDED.is_subscribed,
        is_auto_subscribed = EXCLUDED.is_auto_subscribed,
        subscription_comments = concat(rsf_program_facility_checks.subscription_comments || ' \n' || (now()::date)::text || ': ',EXCLUDED.subscription_comments),
        comments_user_id = EXCLUDED.comments_user_id
       ",
                params=list(rsf_facility_id,
                            reporting_user_id))
    })
  }
  
  act <- actions[!is.na(indicator_id) & action %in% c("calculate")] #default is only meaningful for indicators that are mapped via template names
  
  if (!empty(act)) {
    poolWithTransaction(pool, function(conn) { 
      dbExecute(conn,"create temp table _act(formula_id int,comments text) on commit drop;")  
      dbAppendTable(conn,
                    name="_act",
                    value=act[,.(formula_id=indicator_formula_id,comments)])
      
      dbExecute(conn,"
        insert into p_rsf.rsf_program_facility_indicators(rsf_pfcbl_id,
                                                          indicator_id,
                                                          formula_id,
                                                          rsf_program_id,
                                                          rsf_facility_id,
                                                          is_subscribed,
                                                          is_auto_subscribed,
                                                          subscription_comments,
                                                          comments_user_id)
       select
         ids.rsf_pfcbl_id,
         indf.indicator_id,
         act.formula_id,
         ids.rsf_program_id,
         ids.rsf_facility_id,
         true as is_subscribed,
         false as is_auto_subscribed,
         act.comments as subscription_comments,
         'SYSTEM: ' || $2::text
       from p_rsf.rsf_pfcbl_ids ids
       cross join _act act
       inner join p_rsf.indicator_formulas indf on indf.formula_id = act.formula_id -- could be a NULL formula, but header can't map to null
       where ids.rsf_pfcbl_id = $1::int
         and not exists(select * from  p_rsf.rsf_program_facility_indicators pfi
                        where pfi.rsf_pfcbl_id = $1::int
                          and pfi.indicator_id = indf.indicator_id
                          and pfi.formula_id is not distinct from act.formula_id
                          and pfi.is_subscribed is true
                          and pfi.is_auto_subscribed is false)
       on conflict(rsf_pfcbl_id,indicator_id)
       do update
       set 
        is_subscribed = EXCLUDED.is_subscribed,
        is_auto_subscribed = EXCLUDED.is_auto_subscribed,
        subscription_comments = concat(rsf_program_facility_indicators.subscription_comments || ' \n' || (now()::date)::text || ': ',EXCLUDED.subscription_comments),
        comments_user_id = EXCLUDED.comments_user_id
           
       ",
                params=list(rsf_facility_id,
                            reporting_user_id))
    })
  }
  
  
  
  template <- list(cohort_pfcbl_id=rsf_facility_id,
                   reporting_asof_date=reporting_asof_date,
                   template_data=template_data,
                   pfcbl_reporting_flags=reporting_flags)
  
                   #only the IFC QR template merits saving headers
                   #template_headers=unique(template_headers))
  
  status_message(class="info","Success: Completed Parsing File:\n")
  return (template)
}


