
parse_text_to_paragraphs <- function(content_lines,
                                     output=c("text","table"),
                                     paragraph.bullets=FALSE, #nested bullets are not paragraphs; only primary bullets
                                     paragraph.custom=c() #manual regexp to wrap
                                     ) {
    
  output <- match.arg(output)
  #content_lines <- readLines("C:/Users/SHeitmann/OneDrive-New/WBG/IFC Risk Sharing - Documents/Apps/Jason/Dev/RSFJason/facilityOTPSMELEASING51197-2026-02-18.txt")
  bullet_rxp <- regexp_get_bullets()
  
  content_lines <- data.table(ln=1:length(content_lines),line=trimws(content_lines))
  
  content_lines[,
                junk:=grepl("^\\d+$",line) |
                      grepl("^[A-Z]+[0-9]+[[:punct:]][0-9]+[[:punct:]][0-9]+$",line)]
  
  content_lines <- content_lines[junk==F]
  content_lines[,junk:=NULL]
  
  content_lines[,
                line:=gsub("[^[:punct:][:cntrl:][:alnum:][:space:]]","+",line)] #basically these are non-printing characters, which get generated from copy-pasting bullets.
  
  content_lines[,
                prev_line:=shift(line,n=1,type="lag",fill="")]
  
  content_lines[,
                title_line:=grepl("^section|^annex|^article|^schedule|^<[A-Z]+>.*$",line,ignore.case=T)==TRUE & 
                  grepl("of$|in$|by$|including$|to$",prev_line,ignore.case=T)==FALSE]
  
  #If the line only contains capital letters and no lower-case letters, its a title/header line
  content_lines[,
                title_line:=grepl("[A-Z]{5,}",line)==TRUE & grepl("[a-z]",line)==FALSE] #5+ capital letters in a row implies a word, and not interpreteing a "A." as a title
  
  content_lines[,prev_line:=NULL]
  
  #text starts with any kind of punctuation or control character (unless it starts with parenthetical text)
  content_lines[,
                new_line:=grepl("^[[:punct:][:cntrl:]]",line) &
                          !grepl("^\\([^\\)]{4,}\\)",line)]

  
  #If there is only a word or two, then it's an intentionally short line
  content_lines[,
                emphasis_line:=grepl("^[a-z]+$|^[a-z]+\\s+[a-z]$",line)] #one or two words only, eg, "plus" "minus" "divided by" all are emphasised by having their own line
  
  
  #if the line neds in ; : or . (but not an abbreviation), then it's assumed the line is an intnetional end and not a wrap.
  content_lines[,
                end_line:=grepl("[;:\\.]$",line) &
                          !grepl("[A-Z]\\.[A-Z]\\.$",line)] #eg, line ends with U.S.A. 
  

  
  
  
  #If the last line is an end line, then this line is a "next line" ie, an intentional new line.
  content_lines[,
                next_line:=shift(end_line,n=1,type="lag",fill=TRUE)]
  
  #Bullet point all by itself (with no content)
  content_lines[,
                bullet_point:=grepl(paste0(bullet_rxp,collapse="|"),line,ignore.case=F)] #case is manually entered to determine bullet style: upper vs lower matters
  
  #Bullet point and a line of content all together
  content_lines[,
                bullet_line:=grepl(paste0(paste0(bullet_rxp,"\\s+[[:alpha:]]+.*$"),collapse="|"),line)]
  
  content_lines[bullet_line==TRUE,
                bullet_point:=TRUE]

  #This line is a regular line that follows an "empty" bullet point (ie, clipboard pasted bullets on their own lines and the bullet's text on the line below)
  #So if the previous line is an empty bullet point, and this line isn't a new bullet; nor a title line, then read as the last line's bullet.
  content_lines[,
                bullet_line:=bullet_point==FALSE & title_line==FALSE & next_line==FALSE & shift(bullet_point,n=1,type="lag",fill=FALSE)==TRUE]
  
  #If we've previously determined its a new line but now determined it's actually a wrapped bullet, then remove the new line classification
  content_lines[bullet_line==TRUE & bullet_point==FALSE,
                new_line:=FALSE]
  
  
  #an intentionally blank line is an umabiguous new line
  content_lines[nchar(line)==0,
                `:=`(new_line=TRUE,
                     end_line=TRUE,
                     next_line=TRUE,
                     bullet_point=FALSE,
                     bullet_line=FALSE,
                     title_line=FALSE)]
  
  #Line before a title line is an end line (this isn't used, but for completeness)
  content_lines[shift(title_line,n=1,type="lead",fill=TRUE),
                `:=`(end_line=TRUE)]
  
  
  content_lines[bullet_point==TRUE & bullet_line==FALSE & shift(title_line,n=1,type="lead",fill=FALSE)==TRUE,
                `:=`(bullet_point=FALSE,
                     new_line=TRUE,
                     next_line=TRUE)]
  
  
  #If we're updating end line, also re-update next_line
  content_lines[,
                next_line:=next_line | shift(end_line,n=1,type="lag",fill=TRUE)]
  
  content_lines[,
                new_p:=title_line==TRUE |
                      (next_line==TRUE & new_line==TRUE & bullet_point==FALSE)] #don't start paragraphs with bullets (although this isn't true in the legal document)
  
  if (length(paragraph.custom)) {
    for (rxp in paragraph.custom) {
      content_lines[grepl(rxp,line),
                    new_p:=TRUE]      
    }
  }
  
  #The line following the title line is a new paragraph
  content_lines[shift(title_line,n=1,type="lag",fill=TRUE),
                new_p := TRUE]
  
  #not a bullet and line is all in title case and has at least two words (ie, one space) and doesn't end in punctuation
  content_lines[new_p==FALSE & bullet_point==FALSE & bullet_line==FALSE & grepl("\\b[a-z]",line)==FALSE & grepl(" ",line)==TRUE & grepl("[[:punct:][:cntrl:]]$",line)==FALSE,
                new_p:=TRUE]
  
  content_lines[new_p==TRUE,
                p_group:=ln]
  
  setnafill(content_lines,
            type="locf",
            cols="p_group")
  
  content_lines[,pbullet:=FALSE]
  
  # content_paragraphs <- split(content_lines,
  #                             by="p_group")
  
  parse_paragraph <- function(i,paragraphs) {
    para <- paragraphs[[i]]
  
    if (paragraph.bullets & any(para$bullet_point)) {
      primary_bullet <- para$line[min(which(para$bullet_point))]
      bullet_style <- names(which.max(sapply(bullet_rxp,grepl,
                                             x=primary_bullet)))
      if (length(bullet_style)) {
        para[,
             pbullet:=bullet_point & grepl(bullet_style,line,ignore.case=F)]   
        
        para[pbullet==TRUE,
             pbullet_txt:=gsub(paste0("^(",bullet_style,").*$"),"\\1",line)]   
        para[pbullet==TRUE,
             pbullet_txt:=gsub("[\\(\\)\\.]","",pbullet_txt)]
        
        primary_bullet <- para$pbullet_txt[min(which(para$bullet_point))]
        para[pbullet==TRUE,
             pbullet_num:=as.numeric(NA)]
        
        #primary bullets are numbers
        if (!is.na(suppressWarnings(as.numeric(primary_bullet)))) {
          para[,
               pbullet_num:=suppressWarnings(as.numeric(pbullet_txt))]
          
        #primary bullets are roman numerals: this elseif comes first before letters because roman numerals are letters; but as they start at "i" its very unlikely that a non-roman
        #numeral bulletted list would start at i,j,k ... rather, it would start a,b,c
        } else if (!is.na(suppressWarnings(as.numeric(as.roman(primary_bullet))))) {
          para[,
               pbullet_num:=suppressWarnings(as.numeric(as.roman(pbullet_txt)))]
        } else if (any(tolower(primary_bullet)==letters,na.rm=T)) {

          para[,
               pbullet_num:=sapply(tolower(pbullet_txt),function(l) { n<-which(l==letters); ifelse(!length(n),NA,n) })]
        }
        
        #this is done sequentially because we should eventually pick-up the non-nested bullet as the next in the sequence.  Whereas looking at the whole vector at once will only see the immediate previous value
        #minus first index.
        for (b in which(para$pbullet==TRUE & !is.na(suppressWarnings(as.numeric(para$pbullet_num))))[-1]) {
          
          #because we're re-valuing if it's a pbullet in the loop, such that the next row in the loop may not identify this current row, after re-valuing
          bn <- which(para$pbullet==TRUE & !is.na(suppressWarnings(as.numeric(para$pbullet_num))))
          bn <- bn[bn<=b]
          bn <- rev(bn)[1:2]
          if (length(bn) != 2) next;
          
          bn <- para[bn,pbullet_num]
          if (bn[1] != bn[2]+1) { 
            set(para,i=b,j="pbullet_num",value=NA)
            set(para,i=b,j="pbullet",value=FALSE)
          }
        }
      }
    }
    
    if (paragraph.bullets & any(para$pbullet==TRUE,na.rm=T)) {
      para[pbullet==TRUE,
           line:=paste0("\n",line)]
    }
    
    para[new_line==TRUE | bullet_point==TRUE | title_line==TRUE | next_line==TRUE | new_p==TRUE,
         line:=paste0("\n",line)]
    
    para[bullet_point==TRUE & bullet_line==FALSE,
         line:=paste0(line," ")] #to add a space after the bullet point punctuation when the next line is merged together.
    para <- paste0(paragraphs[[i]]$line,collapse=" ")
    para <- gsub("^[[:cntrl:]]+|[[:cntrl:]]+$","",para)
    para <- gsub(" {2,}"," ",para)
    data.frame(p=i,text=para)
  }
  
  content_paragraphs <- split(content_lines,
                              by="p_group")
  
  content_paragraphs <- lapply(seq_along(content_paragraphs),
                               FUN=parse_paragraph,
                               paragraphs=content_paragraphs)

  if (output=="table") {
    return (rbindlist(content_paragraphs))
  } else if (output=="text") {
    return (paste0(sapply(content_paragraphs,'[[','text'),collapse="\n\n"))
  } else {
    #testing
    x<-paste0(paste0(sapply(content_paragraphs,'[[','p')," - ",
                     sapply(content_paragraphs,'[[','text')),collapse="\n\n")
    cat(x)
  }

}