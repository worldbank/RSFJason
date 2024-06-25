db_data_pivot_family <- function(rsf_data,
                                 value.vars=c("value",
                                              "updated",
                                              "flags"),
                                 indicators.format=TRUE)
                                 # current_date.format=c("SYSID",
                                 #                       "MAX",
                                 #                       "ALL")) #if true, format columns as date, number, text, etc
{

  if (!all(value.vars %in% names(rsf_data))) {
    stop(paste0("Columns not found in value.vars for rsf_data: ",paste0(setdiff(value.vars,names(rsf_data)),collapse=", ")))
  }
  
  if (!all(c("value") %in% c(value.vars,names(rsf_data)))) {
    stop(paste0("Columns required rsf_data: 'value'"))
  }
  
  if (!all(c("pfcbl_rank",
             "rsf_pfcbl_id",
             "current_asof_date",
             "parent_rsf_pfcbl_id",
             "pfcbl_category",
             "indicator_name",
             "data_type") %in% names(rsf_data))) {
    stop(paste0("Required col names: pfcbl_rank,current_asof_date,parent_rsf_pfcbl_id,pfcbl_category,sys_name,rsf_full_name,reporting_status,reporting_expected"))
  }
  
  #current_date.format <- match.arg(current_date.format)
  
  ranks <- sort(unique(rsf_data$pfcbl_rank))
  
  data_family <- unique(rsf_data[pfcbl_rank==ranks[1],
                                 .(current_asof_date,
                                   SYSID=parent_rsf_pfcbl_id,
                                   SYSCATEGORY=pfcbl_category)])
  
  # data_family[,
  #             `:=`(ALL_rsf_pfcbl_ids=list())]
  
  
  cols <- c("current_asof_date",
            "rsf_pfcbl_id",
            "pfcbl_category",
            "parent_rsf_pfcbl_id",
            "indicator_name",
            "data_type",
            value.vars)
  
  for (r in ranks) {
    
    rdata <- rsf_data[pfcbl_rank==r,
                      ..cols]

    
    
    if (!all(unique(rdata$parent_rsf_pfcbl_id) %in% data_family$SYSID)) {
      stop("Child entity cannot find parent_rsf_pfcbl_id in data_family")
    }
    

    wdata <- dcast.data.table(rdata,
                              current_asof_date + rsf_pfcbl_id + parent_rsf_pfcbl_id + pfcbl_category ~ indicator_name,
                              value.var=value.vars,
                              sep="!")

    wnames <- grep("!",names(wdata),value=T)
    setnames(wdata,
             old=wnames,
             new=gsub("^([a-z]+)!(.*)$","\\2.\\1",wnames))
    
    data_types <- unique(rdata[,
                               .(indicator_name,
                                 data_type)])
    
    for (i in 1:nrow(data_types)) {
      col_name <- data_types[i,indicator_name]
      col_name <- paste0(col_name,".value")
      col_type <- data_types[i,data_type]
      
      if (indicators.format==FALSE) col_type <- "text"
      
      if (col_type %in% c("number","currency","percent")) { set(wdata,j=col_name,value=as.numeric(wdata[[col_name]]))
      } else if (col_type == "date") { set(wdata,j=col_name,value=as.Date(wdata[[col_name]]))
      } else if (col_type == "logical") { set(wdata,j=col_name,value=as.logical(wdata[[col_name]]))
      } else if (col_type %in% c("text","currency_ratio")) { set(wdata,j=col_name,value=as.character(wdata[[col_name]]))
      } else { set(wdata,j=col_name,value=as.character(wdata[[col_name]])) }
    }
    
    data_family <- data_family[wdata,
                               on=.(current_asof_date,
                                    SYSID=parent_rsf_pfcbl_id)]
    
    data_family[!is.na(rsf_pfcbl_id),
                `:=`(SYSID=rsf_pfcbl_id,
                     SYSCATEGORY=pfcbl_category)]
    
    data_family[,
                `:=`(rsf_pfcbl_id=NULL,
                     pfcbl_category=NULL)]  
  }
  
  # data_family[,
  #             SYSCATEGORY:=NULL]
  
  mergecols <- grep("^[A-Z]",names(rsf_data),value=T) #merge cols are those that start with an UPPER CASE letter (indicators all start with lower-case, so we know its a system or reporting column)
  if (length(mergecols)>0) {

    mergedata <- unique(rsf_data[rsf_pfcbl_id %in% unique(data_family$SYSID),
                                 .SD,
                                 .SDcols=c("rsf_pfcbl_id","current_asof_date",mergecols)])
    mergedata[,n:=.N,
              by=.(rsf_pfcbl_id,
                   current_asof_date)]
    
    if (any(mergedata$n > 1)) {
      stop(paste0("Attempt to merge rsf_data into rsf_data_family failed due to duplicated values using: ",
                  paste0(mergecols,collapse=", ")))
    }
    
    data_family[rsf_data,
                (mergecols):=(mget(paste0("i.",mergecols))),
                on=.(SYSID=rsf_pfcbl_id,
                     current_asof_date)]
    
  }
  
  return (data_family)
}
