rsf_hash <- function(...) {
  params <- unlist(lapply(list(...),as.character)) #unlist will convert to base character and thereby convert dates to their integer-days and then interger as string
                                                   #and thereby cause inconsistancies when hashing date data that is passes in, eg as.Date('2016-08-01') vs '2016-08-01'
                                                   #so first call as.character on each object for format consistency and then hash
  if (all(is.na(params))) return (NA)
  
  digestion <- paste0(params,collapse="|")
  hash <- digest(digestion,algo="sha256")

  return (hash)
}

enabled <- function(state,...) { 
  if (state) return(tagList(...))
  else return (disabled(...))
}

#todo: move to separate funcion
rsf_colranks <- function(cols) {
  ranks <- rep(4,length(cols))
  ranks[grepl("^reporting",cols)]  <- 1
  ranks[grepl("^rsf",cols)]        <- 3
  ranks[grepl("^rsf_pfcbl",cols)]  <- 2
  ranks[grepl("^sys",cols)]        <- 4
  
  g <- grepl("^(rsf_|sys_|.*\\.)?global",cols) | grepl("^global\\..*$",cols)
  p <- grepl("^(rsf_|sys_|.*\\.)?program",cols) | grepl("^program\\..*$",cols)
  f <- grepl("^(rsf_|sys_|.*\\.)?facility",cols) | grepl("^facility\\..*$",cols)
  c <- grepl("^(rsf_|sys_|.*\\.)?client",cols) | grepl("^client\\..*$",cols)
  b <- grepl("^(rsf_|sys_|.*\\.)?borrower",cols) | grepl("^borrower\\..*$",cols)
  l <- grepl("^(rsf_|sys_|.*\\.)?loan",cols) | grepl("^loan\\..*$",cols)
  
  ranks[g] <- ranks[g] + 0.0
  ranks[p] <- ranks[p] + 0.1
  ranks[f] <- ranks[f] + 0.2
  ranks[c] <- ranks[c] + 0.3
  ranks[b] <- ranks[b] + 0.4
  ranks[l] <- ranks[l] + 0.5
  
  return (ranks)
}

#indicatorNames_namePattern <- function() { "[a-zA-Z0-9_]+" }

indicatorNames_isIndicator <- function(ind_names) {
  grepl("^(sys_)?(global|program|facility|client|borrower|loan)_[a-zA-Z0-9_]+\\.?.*$",ind_names)
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

parentIndicatorCategory <- function(indicator_category) {
  sapply(indicator_category,
         switch,
         global="global",
         program="program",
         facility="program",
         client="facility",
         borrower="client",
         loan="borrower",
         NA)
}

rankIndicatorCategories <- function(indicator_category) { 
  sapply(indicator_category,
         switch,
         global=0,
         program=1,
         facility=2,
         client=3,
         borrower=4,
         loan=5,
         NA)  
}

indicatorNameResolveCategory <- function(indicator_names) {
  category_names <- grepl("^(sys_|rsf_)?(global|program|facility|client|borrower|loan)_.*$",indicator_names)
  categories <- gsub("^(sys_|rsf_)?([a-z]+)_.*$","\\2",indicator_names)
  categories[categories=="" | !category_names] <- NA
  return (categories)
}


get_rsf_id_from_category <- function(category) { 
  switch(category,
         loan="rsf_loan_id",
         borrower="rsf_borrower_id",
         client="rsf_client_id",
         facility="rsf_facility_id",
         program="rsf_program_id",
         global="rsf_program_id",
         NA) 
}       


flag_text <- function(check_class,check_name,check_message,check_status,evaluation_asof_date) {
  paste0(ifelse(check_status=="active","",paste0("{",toupper(check_status),"} ")),
         toupper(check_class),": ",check_name,ifelse(nchar(check_message)>0,paste0("/ ",check_message),"")," \U2208 ",evaluation_asof_date)
}

normalizeSystemName <- function(x) {
  x <- gsub("%","pct",x)
  x <- gsub("[^A-Za-z0-9_]+","_",x)
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
  x[grepl("^[[:punct:][:blank:]]+$",x,perl=T)] <- NA
  x[!is.na(x) & nchar(x)==0] <- NA
  x <- toupper(x)
  return (x)
}


validSyscategory_id <- function(x) {

  !is.na(x) & grepl("^[A-Za-z0-9\\.:_-]+(\\s?[#\\(]{1}\\d+\\)?)?$",x)==TRUE
  
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


#read-in skips blank start rows
normalizePunctuation <- function(x) {
  
  x <- gsub("1st","first",x,perl=T,ignore.case = T)             #1st -> first
  x <- gsub("N/A","na",x,ignore.case = TRUE,perl=T)             #1st -> first
  x <- gsub("#","num",x,perl=T)                 #pound sign to "num"
  x <- gsub("'","",x,perl=T)                   #get rid of "'s"
  x <- gsub("([[:digit:]])\\s+","\\1",x,perl=T) #remove spaces after digits
  x <- gsub("[[:punct:]]"," ",x,perl=T)         #remove punctuation
  x <- gsub("\\s+"," ",x,perl=T)                #remove mlutiple spaces
  x <- gsub("(\\s+)$|^(\\s+)","",x,perl=T)      #remove leading and trailing spaces
  return (x)
}

normalizeLabel <- function(x) {
  #stringi::stri_trans_general(normalizePunctuation(superTrim(x)),"Latin-ASCII")
  #x <- gsub("^[[:punct:]]+","",x) #Remove any labels that start with punctuation
  #x <- gsub("[\\*\\.:,-]+$","",x)  #Labels ending in . * - : # ,
  x <- gsub("'","",x,perl=T)                   #get rid of "'s"
  x <- gsub("N/A","na",x,ignore.case = TRUE,perl=T)
  x <- gsub("[[:space:]]+&[[:space:]]+"," and ",x,ignore.case = TRUE,perl=T)
  x <- gsub("[^%#&@$<>\\/[:alnum:]]"," ",x,perl=T)
  x <- gsub("[[:space:]]*([[:punct:]])[[:space:]]*","\\1",x)
  stringi::stri_trans_general(superTrim(x),"Latin-ASCII")
}

superTrim <- function(x,
                      to.lower.case=TRUE,
                      empty.is.NA=FALSE) {
  
  x <- as.character(x)
  nas <- is.na(x)
  x <- gsub("\\\\+","/",x,perl=T)
  x <- gsub("[[:cntrl:]]+"," ",x,perl=T) #remove line breaks, eg Excel headers on multiple lines
  x <- gsub("\\s+"," ",x,perl=T)              #remove multipe spaces
  x <- gsub("\\s?(/)\\s?","\\1",x,perl=T)     #spaces in between or sign "/" eg, "Yes/ No" -> "Yes/No"
  x <- gsub("\\s+([,\\?\\.])","\\1",x,perl=T) #spaces followed by ? eg, "What ?" -> "What?"
  x <- trimws(x,whitespace="[ \\t\\r\\n\\v\\h\\s]")
  x[grepl("^[\"' ]+$",x,perl=T)] <- ""  #The whole stirng is spaces or quotes, such as literal "" or ''
  if (to.lower.case==TRUE) x <- tolower(x)
  if (empty.is.NA==TRUE) {
    blanks <- which(nchar(x)==0)
    if (length(blanks) > 0) x[blanks] <- as.character(NA)
  }
  
  x[nas] <- as.character(NA)
  
  return (x)
}

if.na <- function(x,replacement) {
  if (length(x)==0) return(x)
  x[is.na(x)] <- replacement
  return (x)
}

if.null <- function(x,replacement) {
  
  if (length(x)==0) return(x)
  nulls <- sapply(x,is.null)
  if (any(nulls)) x[nulls] <- replacement
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
