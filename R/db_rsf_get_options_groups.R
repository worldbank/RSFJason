db_rsf_get_options_groups <- function(pool) {
  
  options_groups <- poolWithTransaction(pool,function(conn) {
    
    
    options_groups <- dbGetQuery(conn,"select 
                                      iog.options_group_id,
                                      iog.options_group_name,
                                      iog.options_group_definition as definition,
                                      iog.options_group_data_type
                                      from p_rsf.indicator_options_groups iog")
    
    if (all(is.na(options_groups)) || empty(options_groups)) return (NA)
    
    options_groups <- as.data.table(options_groups)
    options_labels <- dbGetQuery(conn,"select 
                                      iog.options_group_id,
                                      iogk.options_group_key,
                                      coalesce(iogk.label_id,-1) as label_id,
                                      coalesce(la.label_key,'EN') as label_key,
                                      lk.label_key_name,
                                      initcap(key_type) as key_type,
                                      la.primary_label,
                                      sl as secondary_labels
                                      from p_rsf.indicator_options_groups iog
                                      left join p_rsf.indicator_options_group_keys iogk on iogk.options_group_id = iog.options_group_id
                                      left join p_rsf.labels la on la.label_id = iogk.label_id
                                      left join p_rsf.label_keys lk on lk.label_key = la.label_key
                                      left join lateral unnest(secondary_labels) sl on true
                                      order by 
                                      iog.options_group_id,
                                      iogk.options_group_key,
                                      (case when lk.key_type = 'language' then  1
                                      			when lk.key_type = 'client' then 2
                                      			when lk.key_type = 'ifc' then 3
                                      			else 4 end) asc,
                                      lk.label_key = 'EN' desc,
                                      lk.label_key_name,
                                      la.primary_label,sl")
    
    options_labels <- as.data.table(options_labels)
    options_labels[order(secondary_labels),secondary_label_rank:=1:.N,by=.(options_group_id,options_group_key,label_id,label_key)]
    options_labels <- options_labels[,.(labels=list(.SD)),by=.(options_group_id),
                                     .SDcols=c("options_group_id",
                                               "options_group_key",
                                               "label_id",
                                               "label_key",
                                               "label_key_name",
                                               "key_type",
                                               "primary_label",
                                               "secondary_labels",
                                               "secondary_label_rank")]
    
    options_groups[options_labels,labels:=i.labels,on=.(options_group_id)]
    
    setorder(options_groups,options_group_name)
    options_groups
  })  
  return (options_groups)
}