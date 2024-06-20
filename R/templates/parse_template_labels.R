parse_template_labels <- function(rsf_indicators,
                                  template_labels,
                                  lookup_cols=c(),
                                  label_key_priority=NA,
                                  label_key_filter=NA) {
  
  if (!is.data.table(template_labels)) stop("Template labels must be a data.table")
  if (!("id" %in% names(template_labels))) stop("template labels must define column 'id' with a unique ID value")
  if (empty(rsf_indicators)) stop("rsf_indicators is empty()")
  
  indicator_labels <- rsf_indicators[,
                                     unlist(labels,recursive = F),
                                     by=.(indicator_name)]
  
  indicator_dictionary <- indicator_labels[,
                                           .(indicator_id,
                                             indicator_name,
                                             label_key,
                                             label)]
  
  # indicator_dictionary <- rbindlist(list(indicator_dictionary,
  #                                        indicator_labels[!is.na(secondary_labels),
  #                                                         .(indicator_id,indicator_name,label_key,label=secondary_labels)]))
  
  indicator_dictionary[,label_normalized:=normalizeLabel(label)]
  indicator_dictionary <- unique(indicator_dictionary)
  indicator_dictionary <- indicator_dictionary[!is.na(label_normalized)]
  
  if (!all(is.na(label_key_filter))) {
    indicator_dictionary <- indicator_dictionary[!(label_key %in% label_key_filter) & !(indicator_name %in% label_key_filter)]
  }
  
  lookup_cols_id <- c("id",lookup_cols)

  lookups <- template_labels[,..lookup_cols_id]
  lookups <- melt.data.table(lookups,id.vars = "id",variable.factor = F,variable.name = "label_type",value.factor = F,value.name="label")
  lookups[,label_normalized:=normalizeLabel(label)]
  lookups <- unique(lookups)
  lookups[,indicator_name:=NA_character_]

  #Exacty system indicator name is used.  
  lookups[indicator_dictionary,
          indicator_name:=i.indicator_name,
          on=.(label=indicator_name)]
  
  #First match by filter(s) to resolve any ambiguity reporting, eg a template reuses the same label in different sections
  #allows call to parse_template_labels to disambiguate by prefering labels in the identigied section
  label_key_priority <- unique(label_key_priority)
  if (!all(is.na(label_key_priority))) {
    lookups[indicator_dictionary[label_key %in% label_key_priority],
              indicator_name:=ifelse(is.na(indicator_name),i.indicator_name,indicator_name),
              on=.(label_normalized)]
  }
  
  lookups[indicator_dictionary,
          indicator_name:=ifelse(is.na(indicator_name),i.indicator_name,indicator_name),
          on=.(label_normalized)]

  lookups <- lookups[is.na(indicator_name)==FALSE]
  lookups[,n:=length(unique(indicator_name)),by=.(id)]
  if (!empty(lookups[n>1])) {
    err <- lookups[n>1,.(msg=paste0("'",label,"' matches '",indicator_name,"'",collapse=" BUT ")),by=.(id)]
    msg <- err[,paste0(msg,collapse=" AND ALSO ")]
    stop(paste0("Template labels match multiple different indicators: ",msg,". Verify that Template has correctly entered labels in different languages; also verify that System Indicator Admin has correctly entered labels for these indicators and language labels are not swapped."))
  }
  lookups <- lookups[n==1]
  lookups <- lookups[,.(labels_submitted=paste0(unique(sort(label)),collapse=" || ")),
                     by=.(id,indicator_name)]

  return (lookups)
}