parse_template_RSA <- function(pool,
                               template_id,
                               for_rsf_pfcbl_id, #only makes sense at facility level to parse an RSA; and at project level for programs-as-projects (Like SLGP)
                               template_file,
                               rsf_indicators=db_indicators_get_labels(pool=pool),
                               rsf_indicator_formulas=db_indicators_get_formulas(pool=pool),
                               rsf_check_formulas=db_checks_get_formulas(pool=pool),
                               reporting_user_id,
                               status_message) 
{
 
# reporting_user_id <- SOREN_ACCOUNT
# for_rsf_pfcbl_id <- 586864
# for_rsf_pfcbl_id <- 586864
# template_id <- 11
# template_file <- "C:/Users/SHeitmann/OneDrive - WBG/IFC Risk Sharing - Documents/PROJECTS/49649 - RBUA SME RSF/IFC Legal Agreements/IFC_RBUA SME RSF - RSA signed and dated version.pdf"
  
  #file.exists(template_file)
  
  if (!tolower(file_ext(template_file)) %in% c("txt","pdf")) {
    stop("Only .txt and .pdf files are allowed for parse_template_RSA() and only .txt documents are processed as settings and data uploads; whereas .pdf RSA documents are parsed and inputted into 'Setup Agreement' for review before submitting")
  }
  
  save_headers <- TRUE
  template_headers <- data.table(rsf_pfcbl_id=numeric(0),
                                 indiator_id=numeric(0),
                                 label=character(0),
                                 data_source_index=character(0))
  
  rsf_labels <- db_indicators_get_header_actions(pool=pool,
                                                 template_id=template_id,
                                                 rsf_pfcbl_id=for_rsf_pfcbl_id,
                                                 rsf_indicators=rsf_indicators,
                                                 formatting.strip=regexp_get_bullets(),
                                                 formatting.function=superTrim) #important: function is normalizeLabels() will omit {system parse#unit} stuff
  
  section_labels <- rsf_labels[action=="section"]
  rsf_labels <- rsf_labels[action != "section"]
  
  reporting_flags <- data.table(rsf_pfcbl_id=numeric(0),
                                indicator_id=numeric(0),
                                reporting_asof_date=as.Date(numeric(0)),
                                check_name=character(0),
                                check_message=character(0))
  
  #when these controllabels are entered into brackets, eg {...} or {...LCU...} it's ignored
  #variety is to enable specificity about what data is there
  ignore.controllabels <- c("...etc...","...","...BCU...","...LCU...","...LCY...","LCU","LCY") 
  
  
  sections_rsa <- c("TERMS","DETERMINATION","COSTS","TERMINATION","CRITERIA","REPORTING","OTHER")
  sections_sys <- c("KEY","SYSNAME","ASOFDATE","USERID","ASOFDATE","TIMESTAMP",
                    "SYSTEM") #Jason system command action mapping
  
  text <- {
    #setups 
    if ((tolower(file_ext(template_file))=="txt")) {
      text <- readLines(con=template_file)
      text

    
    } else if ((tolower(file_ext(template_file))=="pdf")) {
      
        pdfpages <- {
          text <- pdftools::pdf_text(template_file)
          
          #image_encoding <- strsplit(text,split="(?!\\n)[[:cntrl:]]",perl=T)
          #word_encoding <- strsplit(text[[1]],split="\\w+")[[1]]
          
          
          image_encoding <- lapply(text,stringr::str_extract_all,pattern="(?!\\n)[[:cntrl:]]")
          word_encoding <- lapply(text,stringr::str_extract_all,pattern="\\w+")
          
          
          ratios <- mapply(FUN=function(a,b) { length(unlist(a)) > length(unlist(b)) },image_encoding,word_encoding)
          
          #more than 25% of the pdfpages have more control characters than words...so it's probably corrupted or scanned image o OCR text
          if (mean(ratios) > .25) {
            stop("This pdf appears to have been scanned in and pdf text interpreted from scanned images -- it is unrelaible and may have corrupted text.  Can you use Cntrl-F to search for words? Try to upload the Execution version instead of the signed, scanned version.")
            #text <- pdftools::pdf_ocr_text(pdf=template_file)
          }
          
          
          
          
          
          pdfpages <- rbindlist(lapply(seq_along(text),
                                       FUN=function(p,text) {
                                         
                                         content <- text[[p]]
                                         content_lines <- unlist(strsplit(content,split="\n"))  
                                         if (is.na(content) || nchar(content)==0) return(NULL)
                                         content_lines <- trimws(content_lines)
                                         data.table(page=p,line=content_lines) },
                                       text=text))
          #get rid of page numbers
          pdfpages <- pdfpages[!grepl("^\\d+$",line)]
          
          #get rid footer doc references
          pdfpages <- pdfpages[!grepl("^[[:alpha:]]+\\d+/\\d+_?\\d+$",line)]  
          
          #get rid of Official Use Only
          pdfpages <- pdfpages[!grepl("^Official Use Only$",line,ignore.case = T)]
          
          #get rid of Official Use Only
          pdfpages <- pdfpages[!grepl("^Confidential$",line,ignore.case = T)]
          pdfpages[,page:=1:.N]
          pdfpages
        }
        
        toc_start <- grep("TABLE OF CONTENTS",pdfpages$line,ignore.case=T)[[1]]
        rsa_start <- NULL
        if (!length(toc_start)) {
          stop("Failed to find section labeled 'TABLE OF CONTENTS'.  Is this a valid RSA?  All legal agreements should have a TABLE OF CONTENTS section in the first few pdfpages")
        }
        
        extractSectionHeader <- function(x) {
          #Extract the header information.  Get rid of line and referencec punctionation.
          #Eg,  "Section 6.02. Notices ................................................................................................ 29"
          header <- gsub("(?<=[[:alnum:]])\\s*\\.*\\s*\\d+$","",x,perl=T)
          #header <- gsub("^[^[:alnum:]]*(?=[[:alnum:]])","",header,perl=T) #get rid of any junk that might be ahead of the header, like tabs or spaces or sub-heading dashes
          
          #in case multiple spaces or other word-break characters are rendered by pdf. (including commas, semi-colons just for consistency)
          #but don't omit when it's between two numbers, eg, Section 6.12 will keep the "." or the & in between E&S
          #header <- gsub("\\W+(?![[:alnum:]])"," ",header,perl=T)
          header <- gsub("[[:cntrl:]]+"," ",header)
          header <- gsub("\\s{2,}"," ",header,perl=T) #get rid of multiple spaces
          header <- trimws(header)
          header <- tolower(header) #for consistency
          
          
          header
        }
        
        sectionHeaders <- c()
        for (lnum in toc_start:length(pdfpages$line)) {
          line <- pdfpages$line[lnum]
          
          header <- extractSectionHeader(line)
          
          
          
          #continue adding header lines until we get to the first recognized header. Meaning, we've read all the Table of Contents and we know we've finished reading all the headers
          #because now we're encounting the actual content whose headers have already been enumerated in the TOC
          if (any(header %in% sectionHeaders)) {
            rsa_start <- lnum-1
            break;
          }
          
          
          
          if (nchar(header)==0) next; #if rendered into "" then it wasn't a real header
          if (!grepl("\\d+$",line)) next; #all lines in the Table of Contents end with a page number: so don't add it do sectionHeaders vector
          
          sectionHeaders <- c(sectionHeaders,header)
        }
        
        preface <- pdfpages[1:rsa_start]
        pdfpages <- pdfpages[(rsa_start+1):nrow(pdfpages)]
        
        pdfpages[,as.sectionHeader:=sapply(line,extractSectionHeader)]
        
        for (x in unique(sectionHeaders)) {
          g <- grep(paste0("^",x,"[[:punct:]]*$"),pdfpages$as.sectionHeader,ignore.case = T)
          if (length(g)==1) {
            #print(paste0(g," ",x," MATCHES ",pdfpages$as.sectionHeader[g]))
            set(pdfpages,i=g,j="section",value=x)
          }
        }
        
        #pdfpages[!is.na(section) & grepl("2\\.",section)][,.(paste0(chapter," ",section))]
        #section chapters
        {
          pdfpages[!is.na(section) & !grepl("\\d[[:punct:]]",section),
                   `:=`(chapter=section,
                        section="")]
          
          pdfpages[!is.na(chapter),
                   cgroup:=.GRP,
                   by=.(chapter)]
          
          setnafill(pdfpages,
                    type="locf",
                    cols="cgroup")
          
          pdfpages[pdfpages[!is.na(chapter),.(cgroup,chapter)],
                   chapter:=i.chapter,
                   on=.(cgroup)]
          
          pdfpages[!is.na(section),
                   sgroup:=.GRP,
                   by=.(cgroup,section)]
          
          
          setnafill(pdfpages,
                    type="locf",
                    cols="sgroup")
          
          pdfpages[pdfpages[!is.na(section),.(cgroup,sgroup,section)],
                   section:=i.section,
                   on=.(cgroup,sgroup)]
          
          
          

          setorder(pdfpages,
                   cgroup,
                   sgroup)
          
          #all the "Forms of..." are not actionable for portfolio monitoring.
          pdfpages <- pdfpages[,
                               .(chapter,
                                 section,
                                 text=trimws(line),
                                 sheet=as.character(NA))]
        }
        
        for (i in 1:nrow(section_labels)) {
          lookup <- section_labels[i]$template_label_lookup #has been put through superTrim
          g <- grepl(lookup,superTrim(paste0(pdfpages$chapter," ",pdfpages$section)),ignore.case=T) & is.na(pdfpages$sheet)
          if (length(g)) {
            set(pdfpages,
                i=which(g),
                j="sheet",
                value=toupper(superTrim(section_labels[i]$template_header_section_name)))
          }
          
          g <- grepl(lookup,superTrim(pdfpages$section),ignore.case=T) & is.na(pdfpages$sheet)
          if (length(g)) {
            set(pdfpages,
                i=which(g),
                j="sheet",
                value=toupper(superTrim(section_labels[i]$template_header_section_name)))
          }
          
          g <- grepl(lookup,superTrim(pdfpages$chapter),ignore.case=T) & is.na(pdfpages$sheet)
          if (length(g)) {
            set(pdfpages,
                i=which(g),
                j="sheet",
                value=toupper(superTrim(section_labels[i]$template_header_section_name)))
          }
          
          
        }
        
        pdfpages[is.na(sheet),
                 sheet:="OTHER"]
        
        
        
        #Force new sections to be interpreted as new paragraphs.
        # pdfpages[nchar(text)>0,
        #          new_s:=(1:.N)==1,
        #          by=.(chapter,section)]
        # 
        # pdfpages[new_s==1,
        #          text:=paste0("\n\n",text)]
        # 
        # pdfpages[,new_s:=NULL]
        
        if (any(pdfpages$sheet=="IGNORE",na.rm=T)) {
          pdfpages <- pdfpages[!(sheet == "IGNORE")]
        }
        
        # pdfpages[,rown:=as.double(1:.N)]
        # newsections <- pdfpages[,.(rown=min(rown)-0.1,
        #                            text="\n"),
        #                         by=.(chapter,section,sheet)]
        # 
        # pdfpages <- rbindlist(list(newsections[,.(chapter,section,sheet,text,rown)],
        #                            pdfpages[,.(chapter,section,sheet,text,rown)]))
        # 
        # setorder(pdfpages,
        #          sheet,
        #          rown)
        
        pdfother <- pdfpages[sheet=="OTHER"]
        pdfpages <- pdfpages[sheet!="OTHER"]
        
        # 
        # pdfpages <- pdfpages[,.(text=paste0(text,collapse="\n\n")),by=.(sheet)]

        otherheaders <- toupper(unique(c(ifelse(nchar(pdfother$section)==0,
                                                pdfother$chapter,
                                                paste0(pdfother$chapter," ",pdfother$section)))))
        
        #if the header has been accounted for, don't add it as a special line
        #NOTE: anything assigned to "section" will have been removed...  So any labels that are being deliberately assigned to section will not be present
        knownheaders <- otherheaders[sapply(otherheaders,
                               function(header,stl) {
                                 any(grepl(paste0("^",superTrim(header),"$"),stl))
                               },stl=unique(c(superTrim(section_labels[tolower(template_header_section_name) %in% c(NA,"","other"),label]),
                                              superTrim(rsf_labels[tolower(template_header_section_name) %in% c(NA,"","other"),label]))))]
        
        
        otherheaders <- otherheaders[!(otherheaders %in% knownheaders)]
        
        remove_known <- which(sapply(pdfother$text,
               function(txt,stl) {
                 any(grepl(paste0("^",str_escape(superTrim(txt)),"$"),stl))
               },stl=str_escape(superTrim(rsf_labels[action %in% c("ignore","default") & tolower(template_header_section_name) %in% c(NA,"","other"),label]))))
        
        pdfother <- pdfother[-remove_known]
        
        if (!length(otherheaders)) {
          otherheaders <- NULL
        } else {
          otherheaders <- data.table(chapter="Missing Headers",
                                     section="",
                                     sheet="OTHER",
                                     text=c("[UNRECOGNIZED HEADERS: START]",otherheaders,"[UNRECOGNIZED HEADERS: END]"))
        }
        
        pdfpages <- rbindlist(list(pdfpages[,.(chapter,section,sheet,text)],
                                   otherheaders[,.(chapter,section,sheet,text)],
                                   pdfother[,.(chapter,section,sheet,text)]))
        
        pdfpages <- pdfpages[nchar(text)>0]
        
        {
          pdfterms <- NULL
          if (any(pdfpages$sheet=="TERMS",na.rm=T))  {
            
            
            pdfcontent <- sapply(split(pdfpages[sheet=="TERMS"],by=c("section","chapter","sheet")),
                               function(dt) {
                                 parse_text_to_paragraphs(content_lines=dt$text,
                                                          output="text",
                                                          paragraph.bullets=FALSE,
                                                          paragraph.custom=c('^"[A-Z].*$'))
                               })
            
            pdfcontent <- paste0(pdfcontent,collapse="\n\n")

            pdfterms <- c("<TERMS>\n",pdfcontent)
          }
          
          pdfdetermination <- NULL
          if (any(pdfpages$sheet=="DETERMINATION",na.rm=T))  {
            
            pdfcontent <- sapply(split(pdfpages[sheet=="DETERMINATION"],by=c("section","chapter","sheet")),
                                 function(dt) {
                                   parse_text_to_paragraphs(content_lines=dt$text,
                                                            output="text",
                                                            paragraph.bullets=TRUE)
                                 })
            
            pdfcontent <- paste0(pdfcontent,collapse="\n\n")
            
            pdfdetermination <- c("<DETERMINATION>\n",pdfcontent)
          }

          pdfcosts <- NULL
          if (any(pdfpages$sheet=="COSTS",na.rm=T))  {
            
            pdfcontent <- sapply(split(pdfpages[sheet=="COSTS"],by=c("section","chapter","sheet")),
                                 function(dt) {
                                   parse_text_to_paragraphs(content_lines=dt$text,
                                                            output="text",
                                                            paragraph.bullets=TRUE)
                                 })
            
            pdfcontent <- paste0(pdfcontent,collapse="\n\n")
            
            pdfcosts <- c("<COSTS>\n",pdfcontent)
          }
          
          pdftermination <- NULL
          if (any(pdfpages$sheet=="TERMINATION",na.rm=T))  {
            
            pdfcontent <- sapply(split(pdfpages[sheet=="TERMINATION"],by=c("section","chapter","sheet")),
                                 function(dt) {
                                   parse_text_to_paragraphs(content_lines=dt$text,
                                                            output="text",
                                                            paragraph.bullets=TRUE)
                                 })
            
            pdfcontent <- paste0(pdfcontent,collapse="\n\n")
            
            pdftermination <- c("<TERMINATION>\n",pdfcontent)
          }
          
          pdfcriteria <- NULL
          if (any(pdfpages$sheet=="CRITERIA",na.rm=T))  {
            
            pdfcontent <- sapply(split(pdfpages[sheet=="CRITERIA"],by=c("section","chapter","sheet")),
                                 function(dt) {
                                   parse_text_to_paragraphs(content_lines=dt$text,
                                                            output="text",
                                                            paragraph.bullets=TRUE)
                                 })
            
            pdfcontent <- paste0(pdfcontent,collapse="\n\n")
            
            pdfcriteria <- c("<CRITERIA>\n",pdfcontent)
          }
          
          pdfreporting <- NULL
          if (any(pdfpages$sheet=="REPORTING",na.rm=T))  {
            
            pdfcontent <- sapply(split(pdfpages[sheet=="REPORTING"],by=c("section","chapter","sheet")),
                                 function(dt) {
                                   parse_text_to_paragraphs(content_lines=dt$text,
                                                            output="text",
                                                            paragraph.bullets=TRUE,
                                                            paragraph.custom=c("^Comments, if any$",
                                                                               "^Facility information$"))
                                 })
            
            pdfcontent <- paste0(pdfcontent,collapse="\n\n")
            
            pdfreporting <- c("<REPORTING>\n",pdfcontent)
          }
          
          pdfother <- NULL
          if (any(pdfpages$sheet=="OTHER",na.rm=T))  {
            
            pdfcontent <- sapply(split(pdfpages[sheet=="OTHER"],by=c("section","chapter","sheet")),
                                 function(dt) {
                                   parse_text_to_paragraphs(content_lines=dt$text,
                                                            output="text",
                                                            paragraph.bullets=FALSE)
                                 })
            
            pdfcontent <- paste0(pdfcontent,collapse="\n\n")

            pdfother <- c("<OTHER>\n",pdfcontent)
            
          }
          
        }

        pdfpages <- paste0(c(pdfterms,pdfdetermination,pdfcosts,pdftermination,pdfcriteria,pdfreporting,pdfother),collapse="\n\n")
        pdfpages <- gsub("\\n{3,}","\n\n",pdfpages)
        
        #RSA formatting rules on artithmetic:
        # {
        #   for (arithmetic in c("plus","minus","divided by","multiplied by","times","sum of")) {
        #     if (any(grepl(paste0("\\n{2,}",arithmetic,"[[:punct:]]*\\n{2,}"),pdfpages,ignore.case=T),na.rm=T)) {
        #       pdfpages <- gsub(paste0("\\n{2,}(",arithmetic,"[[:punct:]]*)\\n{2,}"),"\n\\1\n",pdfpages,ignore.case=T)
        #     }
        #   }
        # 
        #   #blag blah sum of: is generally kept on the same line followed by what to sum-up that PDFs generate extra spaces thereafter.
        #   if (any(grepl("sum of[[:punct:]]*\\n{2,}",pdfpages,ignore.case=T),na.rm=T)) {
        #     pdfpages <- gsub("(sum of[[:punct:]]*)\\n{2,}","\\1\n",pdfpages,ignore.case=T)
        #   }
        # }
        
        pdfpages <- trimws(unlist(strsplit(pdfpages,split="\n")))
        
        
    } else {
      stop("RSA files must be text or pdf files created and submitted through Jason")
    }
  }
  
  pages <- {
    
    #The whole point of the text file is that it is manually formatted (or previously auto formatted to manual satisfaction) -- so no additional formatting here, save un-bulleting 

    pages <- data.table(text=text)
    #pages <- pages[nchar(text) > 0]
    pages[grepl("^<[A-Z]+>$",text),
          section:=gsub("^<([A-Z]+)>$","\\1",text)]
    
    pages[is.na(section) & grepl("^<[A-Z]+>.+$",text),
          `:=`(section=gsub("^<([A-Z]+)>.+$","\\1",text),
               text=gsub("^<[A-Z]+>(.+)$","\\1",text))]
    
    pages[!is.na(section),
          sGRP:=.GRP,
          by=.(section)]
    
    setnafill(pages,
              type="locf",
              cols="sGRP")
    
    pages[pages[!is.na(section),.(sGRP,section)],
          section:=i.section,
          on=.(sGRP)]
    
    pages <- pages[!(paste0("<",section,">")==text)]
    
    pages[grepl("^[[:cntrl:]]+$",text) | nchar(text)==0,
          paragraph:=1:.N,
          by=.(section)]
    
    setnafill(pages,
              type="locf",
              cols="paragraph")
    
    pages <- pages[!(grepl("^[[:cntrl:]]+$",text) | nchar(text)==0)] #omit text that is "" or just \n blank links
    
    pages[is.na(paragraph),
          paragraph:=1]
    
    regexp_bullets <- paste0(paste0(regexp_get_bullets(),"[[:space:]]*"),collapse="|")
    pages[grepl(regexp_bullets,text),
          text:=gsub(regexp_bullets,"",text)]
    
    pages <- pages[,
                   .(text=paste0(text,collapse="\n")),
                   by=.(section,paragraph)]
    
    #since set set bullets to empty, we may have created new empties.
    pages <- pages[!(grepl("^[[:cntrl:][:space:]]+$",text) | nchar(text)==0)] #omit text that is "" or just \n blank links
    
    pages
    #unique(pages$section)
   
    # text <- parse_text_to_paragraphs(content_lines=text,
    #                                  output="text",
    #                                  paragraph.bullets = F,
    #                                  paragraph.custom="^<[A-Z]+>$") #because should be coming in as auto or manually formatted: new paragraph on empty line
    # cat(text)
  }
  
  reporting_asof_date <- {
    asof_date <- ymd(pages[section=="ASOFDATE",text])
    
    #pdf document will always use creation date for as-of date.
    if (!length(asof_date) || all(is.na(asof_date))) {
        created_date <- dbGetQuery(pool,"
          select
            ids.created_in_reporting_asof_date::text
          from p_rsf.rsf_pfcbl_ids ids
          where ids.rsf_pfcbl_id = $1::int",
          params=list(for_rsf_pfcbl_id))
        asof_date <- ymd(created_date$created_in_reporting_asof_date)
    }
    asof_date
  }
  
  #reporting import and cohort
  #set metadata
  {
    reporting_import <- db_reporting_import_create(pool=pool,
                                                   import_rsf_pfcbl_id=for_rsf_pfcbl_id,
                                                   import_user_id=reporting_user_id,
                                                   reporting_asof_date=reporting_asof_date,
                                                   template_id=template_id,
                                                   file_path=template_file,
                                                   import_comments="RSA Setup",
                                                   auto_delete_old_versions=FALSE) 
    
    auto_subscribed_by_reporting_cohort_id <- unlist(dbGetQuery(pool,"
      insert into p_rsf.reporting_cohorts(import_id,
                                          reporting_asof_date,
                                          reporting_rsf_pfcbl_id,
                                          reporting_user_id,
                                          reporting_time,
                                          is_calculated_cohort,
                                          is_reported_cohort,
                                          data_asof_date,
                                          reporting_type)
      select 
        ri.import_id,
        ri.reporting_asof_date,
        ri.import_rsf_pfcbl_id,
        ri.import_user_id,
        timeofday()::timestamptz,
        false,
        false,
        ri.reporting_asof_date,
        0
      from p_rsf.reporting_imports ri
      where ri.import_id = $1::int
      returning reporting_cohort_id",
                                                                params=list(reporting_import$import_id)))
    
    meta_pages <- pages[section %in% sections_rsa,
                        .(text=paste0(text,collapse="\n\n")),
                        by=.(section)]
    
    #meta_pages[,text:=paste0("Section: ",section)]
    meta_pages <- dcast.data.table(meta_pages,
                                   formula = . ~ section,
                                   value.var="text")
    meta_pages[,`.`:=NULL]
    meta_pages <- as.list(meta_pages)
    
    meta_pages <- jsonlite::toJSON(list(RSA=meta_pages),auto_unbox = T)
    
    
    dbExecute(pool,"
      update p_rsf.reporting_imports ri
      set metadata = ri.metadata || ($2)::jsonb
      where ri.import_id = $1::int",
              params=list(reporting_import$import_id,
                          meta_pages))
  }
  
  if (tolower(file_ext(template_file))=="pdf") {
    
    return (list(reporting_import=reporting_import, #This is unusual.
                 cohort_pfcbl_id=for_rsf_pfcbl_id,
                 reporting_asof_date=reporting_asof_date,
                 template_data=data.table(SYSID=as.numeric(0),
                                          reporting_asof_date=as.Date(as.numeric(0)),
                                          indicator_id=as.numeric(0),
                                          indicator_name=as.character(0),
                                          reporting_submitted_data_value=as.character(0),
                                          reporting_submitted_data_unit=as,character(0),
                                          reporting_submitted_data_formula=as.character(NA),
                                          reporting_template_row_group=as.character(0),
                                          labels_submitted=as.character(0)),
                 pfcbl_reporting_flags=reporting_flags,
                 template_headers=pages[,
                                        .(rsf_pfcbl_id=for_rsf_pfcbl_id,
                                          indicator_id=as.numeric(NA),
                                          label=text,
                                          data_source_index=paste0(section,":",paragraph))]))
  }
  
  #pdf documents merely SET the meta data (a manual review is ALWAYS expected before submitting)
  #txt documents are pulled from Jason and (hopefully) reviewed
  
  
  rsa <- {
    
    label_matches <- mapply(labelMatches,
                            find_sections=tolower(rsf_labels$template_section_lookup),
                            find_labels=tolower(rsf_labels$template_label_lookup),
                            match_id=rsf_labels$label_header_id,
                            match_postion=rsf_labels$template_header_position,
                            MoreArgs=list(search_sections=superTrim(pages$section),
                                          search_labels=superTrim(pages$text)),
                            USE.NAMES = F)
    
    label_matches <- rbindlist(label_matches)
    
    label_matches <- label_matches[rsf_labels[,.(label_header_id,action,map_indicator_id,map_formula_id,map_check_formula_id)],
                                   on=.(match_id=label_header_id),
                                   nomatch=NULL]
    
    label_matches <- label_matches[,
                                   .(header_ids=list(unique(match_id))),
                                   by=.(matched_row_num=match_rows,
                                        action,
                                        map_indicator_id,
                                        map_formula_id,
                                        map_check_formula_id)]
    
    #match_id is the match_row
    pages[,row_num:=1:.N]
    rsa <- label_matches[pages,
                         on=.(matched_row_num=row_num)]
    
    rsa
    
  } 
  
 

  #default
  #ignore
  #remap
  #unmap
  #check
  #calculate
  #parse
  
  template_headers <- rsa[,
                          .(label_header_id=unlist(header_ids,recursive = F)),
                          by=.(section,
                               paragraph,
                               text,
                               action,
                               map_indicator_id,
                               map_formula_id,
                               map_check_formula_id)
                          ][,
                            .(rsf_pfcbl_id=for_rsf_pfcbl_id,
                              indicator_id=label_header_id,
                              label=text,
                              data_source_index=paste0(section,":",paragraph,"#",action,"::",map_indicator_id,"_",map_formula_id,"_",map_check_formula_id))]
  
  template_headers <- unique(template_headers)
  
  sys_text <- rsa[section %in% sections_sys]
  rsa <- rsa[!action %in% "ignore"]
  rsa <- rsa[!section %in% sections_sys]
  
  #conflicts
  {
    conflicts <- rsa[,.(n=length(unique(unlist(header_ids))),
                        header_ids),
                     by=.(matched_row_num,action,map_indicator_id,map_formula_id,map_check_formula_id)][n>1]
    
    #eg, "country" {facility_eligible_client_countries}
    #and "country" means {facility_eligible_client_countries}
    #will parse "Means Ukraine" and "Ukraine" 
    #But we want to match and parse the one that matches more actual text.  And less of what could be covered within the .* of the brackets.
    if (!empty(conflicts)) {
      conflicts[,n:=1:.N] #n recycled as row ID
      conflicts <- conflicts[,
                             .(label_header_id=unlist(header_ids,recursive=F)),
                             by=.(matched_row_num,n,action,map_indicator_id,map_formula_id,map_check_formula_id)]
      
      conflicts <- conflicts[rsf_labels[,.(label_header_id,template_label_lookup)],
                             on=.(label_header_id),
                             nomatch=NULL]
      conflicts[,
                nmatch:=nchar(template_label_lookup)]
      conflicts[,
                nactions:= as.numeric(!is.na(map_indicator_id)) + as.numeric(!is.na(map_formula_id)) + as.numeric(!is.na(map_check_formula_id)) + fifelse(action=="default",yes=0,no=1,na=0)]
      
      setorder(conflicts,
               matched_row_num,
               n,
               -nactions,
               -nmatch) #most match is first
      
      #this one is the most match and there is a clear "most" (ie, not two of the same nchar count)
      conflicts[,
                selected:=(nmatch==nmatch[1]) & (sum(nmatch==nmatch[1]))==1,
                by=.(matched_row_num,n,action,map_indicator_id,map_formula_id,map_check_formula_id)]
      
      conflicts <- conflicts[selected==TRUE]
      rsa[conflicts,
          header_ids:=list(i.label_header_id),
          on=.(matched_row_num,
               action,map_indicator_id,map_formula_id,map_check_formula_id)]
    }
  }
  
  #Unfound: will asign to entity_reporting
  {
    unfound_labels <- rsa[is.na(action)==TRUE]
    rsa <- rsa[is.na(action)==FALSE]
    
    if (!empty(unfound_labels)) {
      
      unfound_labels[,
                     `:=`(rsf_pfcbl_id=for_rsf_pfcbl_id,
                          indicator_id=as.numeric(NA), #will be auto-assigned to reporting indicator
                          reporting_asof_date=reporting_asof_date,
                          check_name="sys_flag_indicator_not_found",
                          check_message=paste0(toupper(section)," paragraph ",paragraph,": [",trimws(text),"]"))]

      unfound_labels <- unfound_labels[,.(rsf_pfcbl_id,
                                          indicator_id,
                                          reporting_asof_date,
                                          check_name,
                                          check_message)]
      
      reporting_flags <- rbindlist(list(reporting_flags,
                                        unfound_labels))
    }
  }

  #Remap/Enable matched labels to indicators.
  {
    rsa[rsf_indicators,
        indicator_name:=i.indicator_name,
        on=.(map_indicator_id=indicator_id)]
    
    rsa[rsf_indicator_formulas,
        `:=`(indicator_id=i.indicator_id,
             indicator_name_formula_title=i.indicator_name_formula_title),
        on=.(map_formula_id=formula_id)]
  
    rsa[rsf_check_formulas,
        check_name_formula_title:=i.check_name_formula_title,
        on=.(map_check_formula_id=check_formula_id)]
    
    mapped_labels <- rsa[action=="remap" | action=="default"]
    if (!empty(mapped_labels)) {
      mapped_actions <- mapped_labels[!is.na(map_indicator_id),
                                     .(rsf_pfcbl_id=for_rsf_pfcbl_id,
                                       indicator_id=map_indicator_id,
                                       reporting_asof_date=reporting_asof_date,
                                       check_name='sys_flag_template_match_actions',
                                       check_message=paste0(indicator_name," mapped from section ",toupper(section)," paragraph ",paragraph,": [",text,"]"))]
      
      reporting_flags <- rbindlist(list(reporting_flags,
                                        mapped_actions))
      
      mapped_actions <- mapped_labels[is.na(map_indicator_id),
                                     .(rsf_pfcbl_id=for_rsf_pfcbl_id,
                                       indicator_id=NA, #will be auto-assigned to reporting indicator
                                       reporting_asof_date=reporting_asof_date,
                                       check_name='sys_flag_indicator_not_found',
                                       check_message=paste0("No mapping found for DEFAULT assignment for header ",toupper(section)," paragraph ",paragraph,": [",text,"] (should action be set to Ignore?)"))]
      
      reporting_flags <- rbindlist(list(reporting_flags,
                                        mapped_actions))
    }
    
    mapped_labels <- rsa[action=="calculate"]
    if (!empty(mapped_labels)) {
      mapped_labels <- mapped_labels[,
                                     .(rsf_pfcbl_id=for_rsf_pfcbl_id,
                                       indicator_id=map_indicator_id, #will be auto-assigned to reporting indicator
                                       reporting_asof_date=reporting_asof_date,
                                       check_name='sys_flag_template_match_actions',
                                       check_message=paste0(indicator_name_formula_title," mapped from section ",toupper(section)," paragraph ",paragraph,": [",text,"]"))]
      
      reporting_flags <- rbindlist(list(reporting_flags,
                                        mapped_labels))
    }
    
    mapped_labels <- rsa[action=="check"]
    if (!empty(mapped_labels)) {
      
      mapped_labels <- mapped_labels[,
                                     .(rsf_pfcbl_id=for_rsf_pfcbl_id,
                                       indicator_id=as.numeric(NA), #will be auto-assigned to reporting indicator
                                       reporting_asof_date=reporting_asof_date,
                                       check_name='sys_flag_template_match_actions',
                                       check_message=paste0(check_name_formula_title," mapped from section ",toupper(section)," paragraph ",paragraph,": [",text,"]"))]
      
      reporting_flags <- rbindlist(list(reporting_flags,
                                        mapped_labels))
    }
  }  
  
  #PARSING: The only area for data collection within the RSA template
  #Will parse when explicitly set to parse (meaning, parse and do nothing else)
  #Or, if any {} encoding exists, it will auto-parse it, having presumably already taken the specified alternative action.
  {
    #parsing <- rsa[action=="parse"]
    

    parsing <- rsa[,.(label_header_id=unlist(header_ids,recursive=F)),
                   by=c(grep("header_ids",names(rsa),invert=T,value=T))]

    #parsing are all those explicitly labeled as "parse"
    #and all those that have a {.*} stuff in brackets
    parsing <- parsing[rsf_labels[action=="parse" | grepl("\\{.*\\}",label),
                              .(parse_label=label,
                                parse_label_lookup=template_label_lookup,
                                label_header_id)],
                on=.(label_header_id),
                nomatch=NULL]
    
    if (empty(parsing)) parsing <- NULL
    
    rsa <- rsa[action != "parse"]
    
    if (!empty(parsing)) {
      
      parsing[,
              parse_label_lookup:=gsub(pattern="\\.\\*","(.*)",parse_label_lookup)]
      
      parsing[,
              parse_keys:=lapply(parse_label,
                                 FUN=function(pattern,string) {
                                   values <- stringr::str_extract_all(pattern=pattern,
                                                                      string=string)
                                   unlist(values)
                                 },
              pattern="\\{[^\\}]+\\}")]
      
      parsing[,
              parse_values:=mapply(FUN=function(pattern,string) {
                                     values <- stringr::str_match_all(pattern=pattern,
                                                                      string=string)
                                     values <- unlist(values)
                                     if (!length(values)) stop(paste0("Parse failed to match pattern [",pattern,"] for string [",string,"]"))
                                     if (all(is.na(string))) stop(paste0("NA text to search for pattern=",pattern))
                                     if (values[[1]] != string) { stop(paste0("Parse values expects first value ",values[[1]]," == ",string)) }
                                     
                                     values <- values[-1] #pattern will match the entire string, as well as the capture group(s).  
                                                          #So omit the entire string, as it will be returned at index 1
                                     values
                                     
                                   },
                                   pattern=parse_label_lookup,
                                   string=superTrim(text),
                                   SIMPLIFY = F,
                                   USE.NAMES = F)]
      
      parsing[,id:=1:.N]
      
      pkv <- mapply(FUN=function(a,b) {
    
        length(a) == length(b)
      },a=parsing$parse_keys,b=parsing$parse_values)
      
      if (!all(pkv)) stop(paste0("Failed to parse equal sets for parse keys and values: 
      ",paste0(parsing[which(!pkv),text],collapse="\n AND ALSO \n")))
      
      parsing <- parsing[,
                         .(indicator_name=unlist(parse_keys,recursive = F),
                            data_value=unlist(parse_values,recursive=F)),
                         by=.(id,parse_label,section,paragraph,text)]
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
      
      #where values are intentionally set to ignore but to allow the parsing to accommodate various text permutations
      parsing <- parsing[!(is.na(indicator_id) & data_unit %in% c("text","number","percent","ignore","date"))]
      parsing <- parsing[!tolower(indicator_name) %in% tolower(ignore.controllabels)]
      
      parsing[!(is.na(indicator_id) & data_unit %in% c("percent")) & !grepl("%|percent",data_value) & data_unit %in% c("%") & !is.na(suppressWarnings(as.numeric(data_value))),
              data_value:=as.character(as.numeric(data_value)/100)]
      
      {
        unfound_labels <- parsing[is.na(indicator_id)]
        parsing <- parsing[is.na(indicator_id)==FALSE]
        
        if (!empty(unfound_labels)) {
          
          unfound_labels[,
                         `:=`(rsf_pfcbl_id=for_rsf_pfcbl_id,
                              indicator_id=as.numeric(NA), #will be auto-assigned to reporting indicator
                              reporting_asof_date=reporting_asof_date,
                              check_name="sys_flag_indicator_not_found",
                              check_message=paste0("Failed to map '",indicator_name,"' for header: ",parse_label," USING [",toupper(section)," paragraph ",paragraph,": [",trimws(text),"]"))]

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
                               .(rsf_pfcbl_id=for_rsf_pfcbl_id,
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
                                                      "] parsed from section ",toupper(section)," paragraph ",paragraph," [",text,"]"))]
      
    
      
      reporting_flags <- rbindlist(list(reporting_flags,
                                        parsed_labels))
      
      
      parsing[,
              reporting_asof_date:=reporting_asof_date]
      
      parsing[,SYSID:=for_rsf_pfcbl_id]
      
      #we cannot parse and enter data for hierarchal data.  But we can (and need) to be able to specify loan, borrower, etc level indicators for facility-level indicator subscriptions.
      #It's possible some of these could be subscribed via a parse instruction.
      pfcbl_category <- rsf_indicators[indicator_id %in% parsing[,indicator_id],unique(data_category)]
      remap_hierarchies <- {
        if (!all(pfcbl_category %in% c("facility","program"))) {
  
  
          remap_hierarchies <-parsing[indicator_id %in% rsf_indicators[!data_category %in% c("facility","program"),indicator_id],
                                      .(action="remap",
                                        indicator_id,
                                        indicator_formula_id=as.numeric(NA),
                                        check_formula_id=as.numeric(NA),
                                        comments=paste0(toupper(section)," paragraph ",paragraph,":: ",text))]
          
          parsing <- parsing[!(indicator_id %in% rsf_indicators[!data_category %in% c("facility","program"),indicator_id])]
  
          remap_hierarchies
  
        } else {
          NULL
        }
      }
    
    }
  }
 
  #Its possible (expected) that the same metric could parse the same or multiple values.  For LIST text metrics, this is necessary.  And for multi-select values.
  #Concatenate these into a csv list.
  parsing[,n:=.N,
          by=.(SYSID,
               reporting_asof_date,
               indicator_id,
               indicator_name)]
  
  if (!empty(parsing[n>1])) {
    
    parsing[(indicator_name %in% rsf_indicators[indicator_options_group_allows_multiples==TRUE,indicator_name]) |
            (grepl("list",indicator_name,ignore.case=T) & indicator_name %in% rsf_indicators[data_type=="text",indicator_name]),
            multiples:=TRUE]
    
    parsing[n > 1 & multiples==TRUE,
            data_value:=paste0(sort(unique(superTrim(data_value))),collapse=" & "),
            by=.(SYSID,
                 reporting_asof_date,
                 indicator_id,
                 indicator_name,
                 data_unit)]
    
    parsing[,multiples:=NULL]
    parsing <- unique(parsing)
    
  }
  
  parsing[,n:=NULL]
  parsing[,data_value:=superTrim(data_value)]
  
  
  
  existing_data <- dbGetQuery(pool,"
    select
      rdc.rsf_pfcbl_id,
      rdc.indicator_id,
      rdc.reporting_asof_date,
      p_rsf.rsf_data_value_unit(rdc.data_value,rdc.data_unit) as existing_value,
      ri.file_name,
      p_rsf.data_value_is_meaningfully_different(input_rsf_pfcbl_id => rdc.rsf_pfcbl_id,
                                                 input_indicator_id => rdc.indicator_id,
                                                 input_reporting_asof_date => rdc.reporting_asof_date,
                                                 input_data_value => rdc.data_value,
                                                 input_data_unit => rdc.data_unit,
                                                 is_user_reporting => true) as different
    from p_rsf.rsf_data_current rdc
    inner join p_rsf.rsf_data rd on rd.data_id = rdc.data_id
    inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
    inner join p_rsf.reporting_imports ri on ri.import_id = rc.import_id
    where rdc.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
      and rdc.indicator_id = any(select unnest(string_to_array($2::text,','))::int)
      and rdc.reporting_asof_date = any(select unnest(string_to_array($3::text,','))::date)
      and rc.is_calculated_cohort is false",
    params=list(paste0(unique(parsing$SYSID),collapse=","),
                paste0(unique(parsing$indicator_id),collapse=","),
                paste0(unique(parsing$reporting_asof_date,collapse=",")))) 
  
  if (!empty(existing_data)) {
    
    setDT(existing_data)
    existing_data[,file_name:=gsub(".gz","",file_name)]
    parsing[,omit:=FALSE]
    parsing[existing_data,
            omit:=TRUE,
            on=.(SYSID=rsf_pfcbl_id,
                 indicator_id,
                 reporting_asof_date)]
    
    existing_data <- existing_data[parsing,
                  on=.(rsf_pfcbl_id=SYSID,
                       indicator_id,
                       reporting_asof_date),
                  nomatch=NULL]
    existing_data <- existing_data[different==TRUE]
    existing_data <- unique(existing_data[,.(rsf_pfcbl_id,
                                             indicator_id=NA, #consolidate under sys_reporting
                                             reporting_asof_date,
                                             check_name="sys_flag_indicator_ignored",
                                             check_message=paste0("RSA template will not overwrite data: ",
                                                                  "Parsed value ",indicator_name,"[",
                                                                  fcase(!is.na(data_value) & !is.na(data_unit),paste0(data_value," ",data_unit),
                                                                        is.na(data_value),data_unit,
                                                                        is.na(data_unit),data_value,
                                                                        default="MISSING"),
                                                                  "] IGNORED because of existing value [",existing_value,"] ",
                                                                  "reported in '",file_name,"'"))])
    
    reporting_flags <- rbindlist(list(reporting_flags,
                                      existing_data))
    
    parsing <- parsing[omit==FALSE]
  }                              
  #Only "PARSING" will generate data
  #other actions generate actions
  template_data <- unique(parsing[!is.na(indicator_name),
                                 .(SYSID,
                                   reporting_asof_date,
                                   indicator_id,
                                   indicator_name,
                                   reporting_submitted_data_value=data_value,
                                   reporting_submitted_data_unit=data_unit,
                                   reporting_submitted_data_formula=as.character(NA))])
  
  template_data[,reporting_template_row_group:=paste0(1:.N,"RSA")]
  
  
  actions <- rbindlist(list(rsa[,.(action,
                                   indicator_id=map_indicator_id,
                                   indicator_formula_id=map_formula_id,
                                   check_formula_id=map_check_formula_id,
                                   comments=paste0(toupper(section)," paragraph ",paragraph,":: ",text))],
                            parsing[!is.na(indicator_id),
                                    .(action="remap",
                                      indicator_id,
                                      indicator_formula_id=as.numeric(NA),
                                      check_formula_id=as.numeric(NA),
                                      comments=paste0(toupper(section)," paragraph ",paragraph,":: ",text))],
                            remap_hierarchies))
  
  
  actions <- actions[!(action=="ignore")] #ignore means ignore... do nothing
  actions <- actions[!(action=="parse")] #parse should already have been managed and re-combined to remap above.
  actions <- unique(actions)
  
  #conn <- poolCheckout(pool)
  #dbBegin(conn)
  #dbRollback(conn)
  
  
  act <- actions[!is.na(indicator_id) & action %in% c("default","remap")] #default is only meaningful for indicators that are mapped via template names
  if (!empty(act)) {
    poolWithTransaction(pool, function(conn) { 
      dbExecute(conn,"create temp table _act(indicator_id int,comments text) on commit drop;")  
      dbAppendTable(conn,
                    name="_act",
                    value=act[,.(indicator_id,comments)][,.(comments=paste0(comments,collapse="\n AND \n")),by=.(indicator_id)])
      
      dbExecute(conn,"
        insert into p_rsf.rsf_setup_indicators(rsf_pfcbl_id,
                                              indicator_id,
                                              formula_id,
                                              rsf_program_id,
                                              rsf_facility_id,
                                              is_subscribed,
                                              is_auto_subscribed,
                                              subscription_comments,
                                              comments_user_id,
                                              auto_subscribed_by_reporting_cohort_id)
       select
         ids.rsf_pfcbl_id,
         act.indicator_id,
         indf.formula_id,
         ids.rsf_program_id,
         ids.rsf_facility_id,
         true as is_subscribed,
         true as is_auto_subscribed,
         act.comments as subscription_comments,
         'SYSTEM: ' || $2::text,
         $3::int as auto_subscribed_by_reporting_cohort_id
       from p_rsf.rsf_pfcbl_ids ids
       cross join _act act
       left join p_rsf.indicator_formulas indf on indf.indicator_id = act.indicator_id
                                              and indf.is_primary_default = true
       where ids.rsf_pfcbl_id = $1::int
         and not exists(select * from  p_rsf.rsf_setup_indicators pfi
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
        subscription_comments = concat(rsf_setup_indicators.subscription_comments || ' \n' || (now()::date)::text || ': ',EXCLUDED.subscription_comments),
        comments_user_id = EXCLUDED.comments_user_id,
        auto_subscribed_by_reporting_cohort_id = EXCLUDED.auto_subscribed_by_reporting_cohort_id",
      params=list(for_rsf_pfcbl_id,
                  reporting_user_id,
                  auto_subscribed_by_reporting_cohort_id))
    })
  }
  
  act <- actions[!is.na(indicator_id) & action %in% c("unmap")] #default is only meaningful for indicators that are mapped via template names
  if (!empty(act)) {
    poolWithTransaction(pool, function(conn) { 
      dbExecute(conn,"create temp table _act(indicator_id int,comments text) on commit drop;")  
      dbAppendTable(conn,
                    name="_act",
                    value=act[,.(indicator_id,comments)][,.(comments=paste0(comments,collapse="\n AND \n")),by=.(indicator_id)])
      
      dbExecute(conn,"
        insert into p_rsf.rsf_setup_indicators(rsf_pfcbl_id,
                                              indicator_id,
                                              formula_id,
                                              rsf_program_id,
                                              rsf_facility_id,
                                              is_subscribed,
                                              is_auto_subscribed,
                                              subscription_comments,
                                              comments_user_id,
                                              auto_subscribed_by_reporting_cohort_id)
       select
         ids.rsf_pfcbl_id,
         act.indicator_id,
         indf.formula_id,
         ids.rsf_program_id,
         ids.rsf_facility_id,
         false as is_subscribed,
         true as is_auto_subscribed,
         act.comments as subscription_comments,
         'SYSTEM Unsubscribed: ' || $2::text,
         $3::int as auto_subscribed_by_reporting_cohort_id
       from p_rsf.rsf_pfcbl_ids ids
       cross join _act act
       left join p_rsf.indicator_formulas indf on indf.indicator_id = act.indicator_id
                                              and indf.is_primary_default = true
       where ids.rsf_pfcbl_id = $1::int
         and not exists(select * from  p_rsf.rsf_setup_indicators pfi
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
        subscription_comments = concat(rsf_setup_indicators.subscription_comments || ' \n' || (now()::date)::text || ': ',EXCLUDED.subscription_comments),
        comments_user_id = EXCLUDED.comments_user_id,
        auto_subscribed_by_reporting_cohort_id = EXCLUDED.auto_subscribed_by_reporting_cohort_id
       ",
                params=list(for_rsf_pfcbl_id,
                            reporting_user_id,
                            auto_subscribed_by_reporting_cohort_id))
    })
  }
  
  act <- actions[!is.na(check_formula_id) & action %in% c("check")] #default is only meaningful for indicators that are mapped via template names
  if (!empty(act)) {
    poolWithTransaction(pool, function(conn) { 
      dbExecute(conn,"create temp table _act(check_formula_id int,comments text) on commit drop;")  
      dbAppendTable(conn,
                    name="_act",
                    value=act[,.(check_formula_id,comments)][,.(comments=paste0(comments,collapse="\n AND \n")),by=.(check_formula_id)])
      
      dbExecute(conn,"
        insert into p_rsf.rsf_setup_checks(rsf_pfcbl_id,
                                                      check_formula_id,
                                                      indicator_check_id,
                                                      rsf_program_id,
                                                      rsf_facility_id,
                                                      is_subscribed,
                                                      is_auto_subscribed,
                                                      subscription_comments,
                                                      comments_user_id,
                                                      auto_subscribed_by_reporting_cohort_id)
       select
         ids.rsf_pfcbl_id,
         act.check_formula_id,
         icf.indicator_check_id,
         ids.rsf_program_id,
         ids.rsf_facility_id,
         true as is_subscribed,
         true as is_auto_subscribed,
         act.comments as subscription_comments,
         'SYSTEM Subscribed: ' || $2::text,
         $3::int as auto_subscribed_by_reporting_cohort_id
       from p_rsf.rsf_pfcbl_ids ids
       cross join _act act
       inner join p_rsf.indicator_check_formulas icf on icf.check_formula_id = act.check_formula_id
       where ids.rsf_pfcbl_id = $1::int
         and not exists(select * from  p_rsf.rsf_setup_checks pfc
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
        subscription_comments = concat(rsf_setup_checks.subscription_comments || ' \n' || (now()::date)::text || ': ',EXCLUDED.subscription_comments),
        comments_user_id = EXCLUDED.comments_user_id,
        auto_subscribed_by_reporting_cohort_id = EXCLUDED.auto_subscribed_by_reporting_cohort_id",
                params=list(for_rsf_pfcbl_id,
                            reporting_user_id,
                            auto_subscribed_by_reporting_cohort_id))
    })
  }
  
  act <- actions[!is.na(indicator_formula_id) & action %in% c("calculate")] #default is only meaningful for indicators that are mapped via template names
  if (!empty(act)) {
    poolWithTransaction(pool, function(conn) { 
      dbExecute(conn,"create temp table _act(formula_id int,comments text) on commit drop;")  
      dbAppendTable(conn,
                    name="_act",
                    value=act[,.(formula_id=indicator_formula_id,comments)][,.(comments=paste0(comments,collapse="\n AND \n")),by=.(formula_id)])
      
      dbExecute(conn,"
        insert into p_rsf.rsf_setup_indicators(rsf_pfcbl_id,
                                                          indicator_id,
                                                          formula_id,
                                                          rsf_program_id,
                                                          rsf_facility_id,
                                                          is_subscribed,
                                                          is_auto_subscribed,
                                                          subscription_comments,
                                                          comments_user_id,
                                                          auto_subscribed_by_reporting_cohort_id)
       select
         ids.rsf_pfcbl_id,
         indf.indicator_id,
         act.formula_id,
         ids.rsf_program_id,
         ids.rsf_facility_id,
         true as is_subscribed,
         true as is_auto_subscribed,
         act.comments as subscription_comments,
         'SYSTEM: ' || $2::text,
         $3::int as auto_subscribed_by_reporting_cohort_id
       from p_rsf.rsf_pfcbl_ids ids
       cross join _act act
       inner join p_rsf.indicator_formulas indf on indf.formula_id = act.formula_id -- could be a NULL formula, but header can't map to null
       where ids.rsf_pfcbl_id = $1::int
         and not exists(select * from  p_rsf.rsf_setup_indicators pfi
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
        subscription_comments = concat(rsf_setup_indicators.subscription_comments || ' \n' || (now()::date)::text || ': ',EXCLUDED.subscription_comments),
        comments_user_id = EXCLUDED.comments_user_id,
        auto_subscribed_by_reporting_cohort_id = EXCLUDED.auto_subscribed_by_reporting_cohort_id
           
       ",
                params=list(for_rsf_pfcbl_id,
                            reporting_user_id,
                            auto_subscribed_by_reporting_cohort_id))
    })
  }
  
                             
  #These are essentially triggers:
  #IF this indicator_id/indicator_name is present in the facility's actions registry (presumably as a result of the actions we've just taken)
  #Then register this action.
  #Useful if an indicator input exists, then to setup a default/non-default calculation
  #Or also if an indicator exists, then ensure we're running a relevant check for it
  #However, it's useful 
  # act <- header_actions[rsf_indicators[,.(indicator_name=superTrim(indicator_name),indicator_id)],
  #                       .(rsf_pfcbl_id=for_rsf_pfcbl_id,
  #                         indicator_id=i.indicator_id,
  #                         action,
  #                         map_indicator_id,
  #                         map_formula_id,
  #                         map_check_formula_id),
  #                         on=.(template_label_lookup=indicator_name),
  #                         nomatch=NULL]
  # if (!empty(act)) {
  # status_message(class="warning","TBD to implement triggered subscriptions...is this still needed?")  
  # }
  setorder(reporting_flags,
           rsf_pfcbl_id,
           reporting_asof_date,
           check_name,
           check_message)
  
  template <- list(reporting_import=reporting_import, #This is unusual.
                   cohort_pfcbl_id=for_rsf_pfcbl_id,
                   reporting_asof_date=reporting_asof_date,
                   template_data=template_data,
                   pfcbl_reporting_flags=reporting_flags,
                   template_headers=template_headers)
  
                   #only the IFC QR template merits saving headers
                   #template_headers=unique(template_headers))
  
  status_message(class="info","Success: Completed Parsing File:\n")
  return (template)
}


