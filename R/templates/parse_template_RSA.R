rsa <- "C:/Users/SHeitmann/WBG/Dev/RSAs/CEU - RBUA/IFC_RBUA SME RSF - RSA signed and dated version.pdf"

pdftools::pdf_info(rsa)
file.exists(rsa)


text <- pdftools::pdf_text(rsa)

{
  pages <- lapply(seq_along(text),
                  FUN=function(p,text) {
                    content <- text[[p]]
                    
                    content <- gsub(pattern="\\n[[:space:]\\t]{3,}",
                                    replacement="\n\n",
                                    x=content)
                    
                    #content <- gsub("[[:cntrl:]][^[:cntrl]]"," ",content)
                    paragraphs <- unlist(strsplit(content,
                                           split="\\n{2,}"))
                    
                    paragraphs <- lapply(paragraphs,
                                         gsub,
                                         pattern="\\n",
                                         replacement=" ")
                    
                    paragraphs <- lapply(paragraphs,
                                         FUN=function(x) {
                                           x<-gsub("[[:space:]]{2,}"," ",x)
                                           trimws(x)
                                         })
                    paragraphs <- unlist(paragraphs)
                    
                    if (length(paragraphs)==0) paragraphs <- "" #page is fully blank or rendered as image
                    
                    data.table(page=p,
                               paragraph=seq_along(paragraphs),
                               text=paragraphs)
                               
                  },text=text)
  
  pages <- rbindlist(pages)
  pages[,
        `:=`(chapter=as.character(NA),
             chapter_title=as.character(NA),
             section=as.character(NA),
             section_title=as.character(NA),
             sub_section=as.character(NA),
             index=.I)]
  
  pages[grepl("^INVESTMENT NUMBER [0-9]+$",text,ignore.case = T),
        `:=`(chapter="INVESTMENT PROJECT",
             chapter_title=text)]
  
  pages[grepl("^ARTICLE [IV]+$",text,ignore.case = T),
        chapter:=toupper(text)]
  
  pages[grepl("^ANNEX [A-Z]+$",text,ignore.case = T),
        chapter:=toupper(text)]
  
  pages[grepl("^SCHEDULE [0-9]+$",text,ignore.case = T),
        chapter:=toupper(text)]
  
  pages[grepl("^TABLE OF CONTENTS$",text,ignore.case = T),
        chapter:=toupper(text)]
  
  chapter_titles <- pages[index %in% pages[!is.na(chapter),index+1],.(text,index=index-1)]
  pages[chapter_titles,
        chapter_title:=i.text,
        on=.(index)]
  
  pages <- tidyr::fill(pages,
                       chapter,
                       chapter_title,
                       .direction="down")
  

  #For definitions, set each definition as a "Section"
  pages[chapter=="ARTICLE I" & 
        grepl('^".+"[[:space:]]+.*$',text,ignore.case = T),
        `:=`(section=gsub('^"(.+)"[[:space:]]+.*$',"\\1",text),
             section_title=gsub('^"(.+)"[[:space:]]+.*$',"\\1",text))]
  
  #"Section 1.0.1 meaningful from ARTICLE II forward
  mindex1 <- pages[chapter=="ARTICLE I",min(index)]
  mindex2 <- pages[chapter=="ARTICLE II",min(index)]
  
  pages[index >= mindex2 & 
        is.na(section) & 
          grepl("^Section\\s+[0-9\\.]+",text,ignore.case = T),
        `:=`(section=gsub("^(Section\\s+[0-9\\.]+).*$","\\1",text),
             section_title=gsub("^Section\\s+[0-9\\.]+","",text))]
  
  pages[index>=mindex1,
        c("section","section_title"):=tidyr::fill(.SD,section,section_title),
        by=.(chapter),
        .SDcols = c("section","section_title")]
  
  pages[index>=mindex1 & 
        !is.na(section) &
        grepl("^\\([a-z]\\)",text),
        sub_section:=gsub("^(\\([a-z]\\)).*$","\\1",text)]

  pages[index>=mindex1 & 
          !is.na(section) &
          grepl("^\\([ivx]+\\)",text),
        sub_section:=gsub("^(\\([ivx]+\\)).*$","\\1",text)]
  
 pages[,
       `:=`(section=trimws(section),
            section_title=trimws(section_title))]
 
        
}

View(pages[section_title=="Reporting Requirements"])

pages[!is.na(sub_section)]

c("section","section_title"):=tidyr::fill(.SD,section,section_title),
by=.(chapter),
.SDcols = c("section","section_title")]
View(pages[!is.na(section)])



grepl()

pages
pages[!is.na(chapter)]
pages[1]
