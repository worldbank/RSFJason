#for UI to start enabled based on value of "state"
enabled <- function(state,...) { 
  if (state) return(tagList(...))
  else return (disabled(...))
}


regexp_get_bullets <- function() { 
  c("^[a-z]\\.",       #eg, a.
    "^[A-Z]\\.",       #eg, A.
    "^\\(\\d+\\)",     #eg, (1)
    "^\\([a-z]\\)",      #eg, (a)
    "^\\([A-Z]\\)",      #eg, (A)
    "^\\([ivx]+\\)",     #eg, (iv)
    "^\\([IXV]+\\)",     #eg, (IV)
    "^[ivx]+\\.",    #eg, iii.
    "^[IXV]+\\.",    #eg, III.
    "^o$",           #eg, "o"
    "^o\\.",         #eg, "o."
    "^o\\s",         #eg, "o "
    "^\\*",
    "^\\+",
    "^\\-\\s")            #eg, - bullet
}

indicatorNames_getBasenames <- function(ind_names) {
  pattern <- "^([a-zA-Z0-9_@]+)\\.[a-z.]+$" #The '@' symbol only used in dashboard view for currency conversions
  gsub(pattern,"\\1",ind_names)
}

indicatorNames_extractIndicatorVariables <- function(indicator_formula) {
  
  regexprs_indicator_variable <- "(sys_)?(global|program|facility|client|borrower|loan)_[a-zA-Z0-9_]+\\.[a-z\\.]+"
  stringr::str_extract_all(indicator_formula,regexprs_indicator_variable)
}

indicatorNames_getAttributes <- function(ind_names) {
  pattern <- "^[a-zA-Z0-9_]+\\.([a-z.]+)$"
  ind_names[!grepl(pattern,ind_names)] <- NA
  gsub(pattern,"\\1",ind_names)
}

normalizeSystemName <- function(x) {
  x <- gsub("%","pct",x)  #no % in names -- this could conflict with dataase like 'acme%' sql
  x <- gsub("[^A-Za-z0-9_]+","_",x) #non letters and numbers and underscores
  x <- gsub("^_+|_+$","",x)
  return (x)
}

normalizeIndicatorName <- function(x,
                                   category,
                                   data_unit=NA,
                                   is_system=FALSE,
                                   data_type=NA) {
  
  category <- tolower(category)
  if (is.na(x) || is.null(x) || x=="" || !category %in% c("loan","borrower","client","facility","program","global")) return("indicator_name_error")
  
  x <- normalizeSystemName(x)
  if (is_system==TRUE) if (!grepl(paste0("^sys_",category,".*$"),x)) x <- paste0("sys_",category,"_",x)
  if (is_system==FALSE) if (!grepl(paste0("^",category,".*$"),x)) x <- paste0(category,"_",x)

  if (!is.na(data_unit)) data_unit <- toupper(trimws(gsub("\\s+","",data_unit)))
  else data_unit <- ""
  
  if (!is.na(data_type) && data_type=="currency") {
    if (grepl("^[A-Z]{3}$",data_unit)==FALSE) data_unit <- "LCU"; #Error!
    
  } else if (!is.na(data_type) && data_type=="currency_ratio") {
    if (grepl("^[A-Z]{3}/[A-Z]{3}$",data_unit)==FALSE) data_unit <- "USD/LCU" #Error!
    else if (length(unique(unlist(strsplit(data_unit,"/"))))==1) data_unit <- "USD/LCU" #Error: cant have LCU/LCU or USD/USD exchange rate indicator
    else if (grepl("^LCU/[A-Z]{3}$",data_unit)==TRUE) data_unit <- gsub("^LCU/([A-Z]{3})$","\\1/LCU",data_unit) #only allow LCU data units in denominator
  }

  data_unit <- gsub("[^[:alnum:]]","_",data_unit)
  
  if (nchar(data_unit) > 0 
      && toupper(data_unit) != "LCU"
      && grepl(paste0(data_unit,"$"),x)==FALSE) { #LCU units do not force appending LCU to the name, else it would be SPAMMY for all currency indicators to end LCU
      x <- paste0(x,"_",data_unit)
  }
  
  x <- gsub("_+","_",x)
  x <- gsub("_+$","",x)
  x <- gsub("^_+","",x)
  
  return (x)
}

normalizeSyscategory_name <- function(x) {
  x <- gsub("[[:cntrl:]]+"," ",x,perl=T) #remove line breaks
  x <- gsub("\\s+"," ",x,perl=T)
  x <- trimws(x,whitespace="[ \\t\\r\\n\\v\\h\\s]")
  x[grepl("^[[:punct:][:blank:]]+$",x,perl=T)] <- NA #Names that are only punctuation are invalid, consider as blanks
  
  x[!is.na(x) & nchar(x)==0] <- NA
  x <- toupper(x)
  return (x)
}

normalizeSyscategory_id <- function(x) {
  x <- paste0(x)
  nas <- is.na(x) | x=="" | grepl("na|n/a|n\\a|n.a|n.a.",x,ignore.case = T)
  x <- gsub("[[:cntrl:]]+","-",x,perl=T)                  #remove line breaks, eg Excel headers on multiple lines (replace with '-' at least temporarily)
  x <- gsub("[\\+/\\\\;,]+","-",x,perl=T)                 #convert + \ ; and , to -
  x <- gsub("\\((\\d+)\\)$","#\\1",x,perl=T)              #converts parenthetcial numbersm eg (2) to #2 to indicate sequenced IDs
  x <- gsub("\\(([[:alpha:]{1,2}])\\)$",":\\1",x,perl=T)  #converts parenthetcial letter(s) eg (A) or (AC) to :A or :AC, used to indicate related IDs but treated as distinctly separate IDs
  x <- gsub("\\(([[:alpha:]{1,2}])\\)#?(\\d+)$",":\\1#\\2",x,perl=T)  #converts parenthetcial letter(s) eg (A) or (AC) to :A or :AC, used to indicate related IDs but treated as distinctly separate IDs
  x <- gsub("\\(([[:alpha:]]{1,2})#?(\\d+)\\)$",":\\1#\\2",x,perl=T)  #converts parenthetcial letter(s) eg (A) or (AC) to :A or :AC, used to indicate related IDs but treated as distinctly separate IDs
  x <- gsub("([\\.:#_-])\\s+","\\1",x,perl=T)             #allow "." ":" and "-" delimeters, but truncate surrounding whitespace
  x <- gsub("\\s+([\\.:#_-])","\\1",x,perl=T)             #allow "." ":" and "-" delimeters, but truncate surrounding whitespace
  x <- gsub("-+","-",x,perl=T)                            #if -- have been inserted, convert to just one -
  x <- toupper(trimws(x,whitespace="[ \\t\\r\\n\\v\\h\\s]"))                         #upper and trim (trim before using end-of-line $ )
  x <- gsub("[[:punct:]]+$","",x,perl=T)                  #disallow ID to end in punctuations
  x <- gsub("[\\.:_-]+(#\\d+)$","\\1",x,perl=T)           #if ID ends in a punctuation and also has a sequence number
  x <- gsub("^[[:punct:]]+","",x,perl=T)                  #disallow IDs that start with a punctuation.
  x <- gsub("^0+","",x,perl=T)                            #removing zero-padded IDs.
  
  #only allowed ONE # or :
  multi_hashes <- grepl("#.*#",x,perl=T)
  if (any(multi_hashes)) {
    repeat {
      x[multi_hashes] <- sub("#","-",x[multi_hashes])
      multi_hashes <- grepl("#.*#",x,perl=T)
      if (!any(multi_hashes)) break;
    }
  }
  
  multi_colons <- grepl(":.*:",x)
  if (any(multi_colons)) {
    repeat {
      x[multi_colons] <- sub(":","-",x[multi_colons])
      multi_colons <- grepl(":.*:",x)
      if (!any(multi_colons)) break;
    }
  }

  x[nas] <- NA                                    #digest will hash NA
  
  return (x)
}

normalizeLabel <- function(x) {
  #stringi::stri_trans_general(normalizePunctuation(superTrim(x)),"Latin-ASCII")
  #x <- gsub("^[[:punct:]]+","",x) #Remove any labels that start with punctuation
  #x <- gsub("[\\*\\.:,-]+$","",x)  #Labels ending in . * - : # ,
  x <- stringi::stri_trans_general(x,"Latin-ASCII")
  x <- gsub("'","",x,perl=T)                   #get rid of "'s"
  x <- gsub("N/A","na",x,ignore.case = TRUE,perl=T)
  x <- gsub("[[:space:]]+&[[:space:]]+"," and ",x,ignore.case = TRUE,perl=T)
  x <- gsub("[^%#&@$<>\\/[:alnum:]]"," ",x,perl=T) #Starting with punctuation
  x <- gsub("[[:space:]]*([[:punct:]])[[:space:]]*","\\1",x)
  
  superTrim(x,trim.punct=FALSE)
  # x[is.na(x) | nchar(x)==0 | x=="na"] <- "{BLANK}"
  # return(x)
}

superTrim <- function(x,
                      to.lower.case=TRUE,
                      trim.punct=TRUE,
                      empty.is.NA=FALSE,
                      trim.each.line=TRUE) {
  
  x <- as.character(x)
  nas <- is.na(x)
  
  
  if (any(grepl("\n",x))) {
    lbreaks <- grep("\n",x)
    x[lbreaks] <- sapply(
                  lapply(
                    strsplit(x=x[lbreaks],split="\n",fixed=T),
                    superTrim),
                  paste0,collapse="\n")
  }
  
  x <- gsub("\\\\+","/",x,perl=T)
  x <- gsub("[[:cntrl:]]+"," ",x,perl=T)      #remove line breaks, eg Excel headers on multiple lines
  x <- gsub("\\s+"," ",x,perl=T)              #remove multipe spaces
  x <- gsub("\\s?(/)\\s?","\\1",x,perl=T)     #spaces in between or sign "/" eg, "Yes/ No" -> "Yes/No"
  x <- gsub("\\s+([,\\?;:!%])","\\1",x,perl=T) #spaces followed by ? eg, "What ?" -> "What?" or "yes ;"
  x <- gsub("\\s+(\\.(?!\\*))","\\1",x,perl=T) #spaces followed by . unless its .* for search strings
  x <- trimws(x,whitespace="[ \\t\\r\\n\\v\\h\\s]")
  x[grepl("^[\"' ]+$",x,perl=T)] <- ""        #The whole string is spaces or quotes, such as literal "" or ''
  if (to.lower.case==TRUE) x <- tolower(x)
  if (trim.punct==TRUE) {                     #remove trialing punctuation, eg "Yes!" -> "Yes" or "Yes." -> "Yes" 
    x <- gsub("[\\.,!;:\\\\/%#?_+-]$","",x)
  }
  if (empty.is.NA==TRUE) {
    blanks <- which(nchar(x)==0)
    if (length(blanks) > 0) x[blanks] <- as.character(NA)
  }
  
  x[nas] <- as.character(NA)
  
  return (x)
}

letters2numbers <- function(x){
  #from: https://stackoverflow.com/questions/34537243/convert-excel-column-names-to-numbers
  # letters encoding
  encoding <- setNames(seq_along(LETTERS), LETTERS)
  
  # uppercase
  x <- toupper(x)
  
  # convert string to a list of vectors of single letters
  x <- strsplit(x, split = "")
  
  # convert each letter to the corresponding number
  # calculate the column number
  # return a numeric vector
  sapply(x, function(xs) sum(encoding[xs] * 26^((length(xs)-1):0)))
  
}

format_asof_date_label <- function(asof_date) {
  stopifnot(is.Date(asof_date))
  
  paste0(year(asof_date),
         'Q',lubridate::quarter(asof_date),
         '-',as.character(lubridate::month(asof_date,label=T)))
}




format_html_option <- function(options_group_name) {
  html <- glue("<div class='indicator_bubble' style='vertical-align:middle;white-space:nowrap;padding-right:10px;display:inline-block;background-color:#3db83b'>
                              <div style='display:inline-block;vertical-align:middle;'><i class='fa fa-sitemap'></i></div>
                              <div style='display:inline-block;vertical-align:middle;'>{options_group_name}</div>
                              </div>")
  return (html)
}

format_html_check_icon <- function(check_class,is_reporting_flag=FALSE,title=check_class,css_class="") {
  
  icon_class <- switch(check_class,
                       critical='fas fa-fire',
                       error='fas fa-times-circle',
                       warning='fas fa-exclamation-triangle',
                       info='fas fa-info-circle',
                       "")
  
  reporting_icon <- ""
  if (is_reporting_flag==TRUE) reporting_icon <- "<i class='fas fa-file-upload icon-reporting' style='display:inline-block;'></i>"
  
  html <- paste0(reporting_icon,"<i class='",icon_class," icon-",check_class," ",css_class,"' style='display:inline-block;' title=\"",title,"\"></i>")
  return (html)
}

format_html_check <- function(check_name,
                              check_class,
                              check_type=NA,
                              is_subscribed=TRUE,
                              is_system=FALSE,
                              user_subscription=NULL,
                              id="") {
  
  htmlid <- ifelse(nchar(id)>1,paste0("id='",id,"'"),"")
  htmluserid <- ifelse(nchar(id)>1,paste0("id='",id,"subscription'"),"")
  
  check_name <- gsub("[[:punct:]]"," ",check_name) #easier to read in html bubble
  check_name <- sub("(loan|borrower|client|facility|program|global)","\\1:",check_name,ignore.case = T)
  check_name <- tools::toTitleCase(check_name)
  check_name <- gsub("^sys","SYS",check_name,ignore.case = T)
  
  check_type_color <- fcase(check_type=="data_audit","violet",
                            check_type=="data_validity","violet",
                            check_type=="contract_terms","limegreen",
                            check_type=="contract_criteria","limegreen",
                            check_type=="business_integrity","skyblue",
                            check_type=="business_monitoring","skyblue",
                            check_type=="none","pink",
                            default="gray")
  
  
  icon_class <- switch(check_class,
                       critical='fas fa-fire',
                       error='fas fa-times-circle',
                       warning='fas fa-exclamation-triangle',
                       info='fas fa-info-circle',
                       "")
  
  unsubscribed <- ifelse(is_subscribed==FALSE," unsubscribed","")
  user_subscribed <- fcase(is.null(user_subscription),"",
                           user_subscription==TRUE,
                           paste0("<i ",htmluserid," class='fa-regular fa-circle-check' style='color:white;font-size:12px;'></i>"),
                           user_subscription==FALSE,
                           paste0("<i ",htmluserid," class='fa-regular fa-circle-xmark' style='color:white;font-size:12px;'></i>"),
                           default=paste0("<i ",htmluserid," class='fa-regular fa-circle-question' style='color:white;font-size:12px;'></i>"))
  
  
  
  #user_subscribed <- "<i name='userSubscription' class='fa-regular fa-circle-check' style='color:green'></i>"
  html <- glue("<div class='check_mark' style='display:flex;flex-flow:row nowrap;white-space:nowrap;background:linear-gradient(to right,{check_type_color},white)'>
                  <div {htmlid} class='check_bubble {check_class}{unsubscribed}' style='display:flex;flex-flow:row nowrap;justify-content:flex-end;align-items: center;'>
                    <div style='color:rgba(250,250,250,.5);display:flex;flex-flow:row nowrap;margin-left:5px;'><i class='{icon_class}'></i></div>
                    <div style='margin-left:5px;margin-right:5px;display:flex;flex-flow:row nowrap;'>{check_name}</div>
                    {user_subscribed}
                  </div>
                </div>")
  
  return (html)
}

format_html_indicator <- function(indicator_name,
                                  data_category,
                                  data_type,
                                  is_system,
                                  is_calculated,
                                  options_group_name=NA,
                                  is_subscribed=TRUE,
                                  user_subscription=NULL,
                                  id="") {
  
  data_category <- tolower(data_category)
  data_type <- tolower(data_type)
  is_calculated <- toupper(as.character(is_calculated))
  is_system <- as.logical(is_system) #should only be passed-in by system
  htmluserid <- ifelse(nchar(id)>1,paste0("id='",id,"subscription'"),"")
  html_attributes <- "<div style='display:inline-block;white-space:nowrap;color:rgba(250,250,250,.5);vertical-align:middle;'>"
  if (data_type=="number")        html_attributes <- paste(html_attributes,"<i class='fa fa-hashtag' title='Data type: number' style='padding-left:5px;'></i>")
  else if (data_type=="currency") html_attributes <- paste(html_attributes,"<i class='fa fa-coins' title='Data type: currency' style='padding-left:5px;'></i>")
  else if (data_type=="currency_ratio") html_attributes <- paste(html_attributes,"<i class='fa fa-coins' title='Data type: currency' style='padding-left:5px;'></i>")
  else if (data_type=="percent")  html_attributes <- paste(html_attributes,"<i class='fa fa-percent' title='Data type: percent' style='padding-left:5px;'></i>")
  else if (data_type=="date")     html_attributes <- paste(html_attributes,"<i class='fa fa-calendar-alt' title='Data type: date' style='padding-left:5px;'></i>")
  else if (data_type=="text")     html_attributes <- paste(html_attributes,"<i class='fa fa-paragraph' title='Data type: text' style='padding-left:5px;'></i>")
  else if (data_type=="logical")  html_attributes <- paste(html_attributes,"<i class='fa fa-grip-lines-vertical' title='Data type: true-false' style='padding-left:5px;'></i>")
  
  if (is_system==TRUE) html_attributes <- paste(html_attributes,"<i class='fa fa-cogs' title='System Indicator' style='padding-left:5px;'></i>")
  if (toupper(is_calculated)=="TRUE") html_attributes <- paste(html_attributes,"<i class='fa fa-calculator' title='Calculated Indicator' style='padding-left:5px;'></i>")
  if (is.na(options_group_name)==FALSE) html_attributes <- paste(html_attributes,"<i class='fa fa-sitemap' title='Assigned choices type: ",options_group_name,"' style='padding-left:5px;'></i>")
  
  user_subscribed <- fcase(is.null(user_subscription),"",
                           user_subscription==TRUE,
                           paste0("<i ",htmluserid," class='fa-regular fa-circle-check' style='color:white;font-size:12px;'></i>"),
                           user_subscription==FALSE,
                           paste0("<i ",htmluserid," class='fa-regular fa-circle-xmark' style='color:white;font-size:12px;'></i>"),
                           default=paste0("<i ",htmluserid," class='fa-regular fa-circle-question' style='color:white;font-size:12px;'></i>"))
  
  
  html_attributes <- paste(html_attributes,"</div>")
  
  html_status <- "<div style='display:inline-block;vertical-align:middle'>"
  #if (is_hidden==TRUE)      html_status <- paste(html_status,"<i class='fa fa-user-secret' style='color:purple;padding-left:5px;' title='Indicator is hidden: will not appear in searches or downloads; but will be saved on uploads'></i>")
  #if (is_deprecated==TRUE)  html_status <- paste(html_status,"<i class='fa fa-ban' style='color:red;padding-left:5px;' title='Indicator is deprecated and no longer used by this RSF Program (however old data reported when inidcator was previously used is retained)'></i>")
  html_status <- paste(html_status,"</div>")
  
  indicator_html <- paste0("<div style='display:inline-block;vertical-align:middle;'>",html_status,"
                              <div ",ifelse(isTruthy(id),
                                            paste0("id='",id,"'"),
                                            ""),
                           "class='indicator_bubble indicator ",data_category," ",ifelse(is_subscribed==FALSE,"unsubscribed","")," ",ifelse(is_system,"disabled",""),"' style='vertical-align:middle;white-space:nowrap;display:inline-block;'>
                                       ",html_attributes,"
                                       <div ",
                           ifelse(isTruthy(id),
                                  paste0("id='",id,"-text'"),
                                  "")
                           ," style='display:inline-block;vertical-align:middle;font-size:12px;font-weight:bold;padding-left:5px;padding-right:10px;'>",indicator_name,"</div>
                               ",user_subscribed,"
                                       </div>
                                       </div>")
  
  return (indicator_html)
}


# 
# in calculations environment for replaceing == and !=
# `%equal%` <- function(e1,e2) { 
#    mapply(function(a,b) { isTRUE(base::all.equal(a,b,check.class=F)) },a=e1,b=e2,USE.NAMES=F)
# }
# 
# 
# `%unequal%` <- function(e1,e2) { 
#          mapply(function(a,b) { !isTRUE(base::all.equal(a,b,check.class=F)) },a=e1,b=e2,USE.NAMES=F)
# }
# 


is.same_text <- function(a,b) { 
  x<-(is.na(a) & is.na(b)) | (as.character(a)==as.character(b)) 
  ifelse(is.na(x),FALSE,x)
}

#all forms of nothing, with zero being a numeric nothing
is.nothing <- function(x) {
  (is.null(x) || length(x)==0 || all(x==0))
}

is.same_number <- function(a,b,tolerance=1/10^CALCULATIONS_ENVIRONMENT$SIG_DIGITS) { 
  
  
  x <- mapply(function(a,b) { 
    if (!is.na(a) & !is.na(suppressWarnings(as.numeric(a)))) a<-as.numeric(a)
    if (!is.na(b) & !is.na(suppressWarnings(as.numeric(b)))) b<-as.numeric(b)
    
    #isTRUE(base::all.equal(a,b,check.class=F,tolerance=tolerance)) 
    isTRUE(base::all.equal(a,b,check.class=F)) | isTRUE(base::`==`(e1=a,e2=b))  | all(c(is.nothing(a),is.nothing(b)))
    
  },a=a,b=b,USE.NAMES=F)
  x[is.na(x)] <- FALSE
  
  if (is.nothing(x)) {
    
    return (FALSE)
    
  } else {
    
    return (x)
    
  }
}

format_name_abbreviation <- function(person_name) {
  
  re <- "^\\w{1}|\\w+-?\\w+$"
  
  name <- unlist(regmatches(person_name, gregexpr(re, person_name)))
  if (is.null(name) || all(is.na(name)) || length(name) != 2) name <- person_name
  else name <- paste0(name[1],".",name[2])
  return(name)
}

#See: https://stackoverflow.com/questions/60977641/r-function-for-rgba-to-hex-color-conversion
rgba2rgb <- function(color_RGBA,background_RGB=col2rgb("white")) {
  
  # get alpha
  if (length(color_RGBA)==1) color_RGBA <- as.numeric(trimws(unlist(str_split(color_RGBA,","))))
  if (length(color_RGBA) != 4) stop("color_RGBA should have 4 elements")
  
  alpha=color_RGBA[4]
  
  # get new color  
  new_col=matrix(c(
    (1 - alpha) * background_RGB[1] + alpha * color_RGBA[1],
    (1 - alpha) * background_RGB[2] + alpha * color_RGBA[2],
    (1 - alpha) * background_RGB[3] + alpha * color_RGBA[3]),
    nrow=3,ncol=1,dimnames=list(c("red","green","blue"))
  )
  return(new_col)
}

rgb2hex <- function(x) { rgb(x[1], x[2], x[3], maxColorValue = 255) }
rgba2hex <- function(color_RGBA,background_RGB) {
  rgb2hex(rgba2rgb(color_RGBA = color_RGBA))
}

words_to_numbers <- function(s) {
  s <- stringr::str_to_lower(s)
  for (i in rev(c(0:19,20,30,40,50,60,70,80,90,100,1000))) {
    s <- stringr::str_replace_all(s, words(i), as.character(i))
    s <- stringr::str_replace_all(s, ordinal(i), as.character(i))
  }
  #s
  
  n <- suppressWarnings(as.numeric(s))
  
  #if, eg, "thirty three" is passed as an argument, 's' will be "30 3"
  #correct format should be "thirty-three"
  #this is fine for small numbers.  For "one million" this will result in NA, for better or worse?
  if (anyNA(n)) {
    nn <- which(is.na(n))
    x <- strsplit(s[nn],"[\\s-]+")
    x <- sapply(x,FUN=function(nums) { sum(suppressWarnings(as.numeric(nums))) })
    n[nn] <- x
  }
  return (n)
}

