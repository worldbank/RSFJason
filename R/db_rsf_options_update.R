db_rsf_options_update <- function(pool,
                                  options_data) {

  setDT(options_data)
  
  options_data <- unique(options_data[,.(options_group_id,options_group_name,options_group_data_type,options_group_definition,
                                         options_group_key,label_id,label_key,primary_label,secondary_labels,action)])
  #browser()
  USE_TEMP <- TRUE
  #USE_TEMP <- FALSE; conn <- poolCheckout(DBPOOL)
  saved <- FALSE
  saved <- poolWithTransaction(pool,function(conn) {
    saved <- tryCatch({
      dbWriteTable(conn,"_temp_options_updates",value=options_data,overwrite=TRUE,temporary=USE_TEMP,field.types=c(options_group_id="int",
                                                                                                                   options_group_name="text",
                                                                                                                   options_group_data_type="text",
                                                                                                                   options_group_definition="text",
                                                                                                                   options_group_key="text",
                                                                                                                   label_id="int",
                                                                                                                   label_key="varchar",
                                                                                                                   primary_label="text",
                                                                                                                   secondary_labels="text",
                                                                                                                   action="varchar"))
      dbExecute(conn,"update p_rsf.indicator_options_groups iog
                      set options_group_name = tou.options_group_name,
                          options_group_data_type = lower(tou.options_group_data_type),
                          options_group_definition = NULLIF(tou.options_group_definition,'NA')
                      from (select distinct options_group_id,options_group_name,options_group_data_type,options_group_definition from _temp_options_updates) tou
                      where tou.options_group_id = iog.options_group_id;")
      
      dbExecute(conn,"delete from p_rsf.labels
                      where exists(select * from _temp_options_updates tou 
                                   where tou.action = 'delete'
                                     and tou.label_id = labels.label_id
                                     and tou.label_key = labels.label_key)")
      
      dbExecute(conn,"delete from p_rsf.indicator_options_group_keys iogk
                      where not exists(select * from p_rsf.labels where labels.label_id = iogk.label_id);")
      
      dbExecute(conn,"delete from _temp_options_updates where action = 'delete';")
      
      dbExecute(conn,"with new_options_keys as (
                      	insert into p_rsf.indicator_options_group_keys(options_group_id,options_group_key)
                      	select distinct options_group_id,options_group_key from _temp_options_updates tou where label_id < 0
                      	and not exists(select * from p_rsf.indicator_options_group_keys iogk 
                      	               where iogk.options_group_id = tou.options_group_id 
                      								   and iogk.options_group_key = tou.options_group_key)
                      	returning options_group_id,options_group_key,label_id
                      )
                      update _temp_options_updates tou
                      set label_id = nok.label_id
                      from new_options_keys nok
                      where nok.options_group_id = tou.options_group_id
                      	and nok.options_group_key = tou.options_group_key")
      
      dbExecute(conn,"delete from p_rsf.labels lab 
                      where exists (select * from _temp_options_updates tou where 'options-' || tou.options_group_id = lab.label_id_group)
                      and not exists(select * from _temp_options_updates tou where tou.label_id = lab.label_id)")
      
      dbExecute(conn,"update p_rsf.indicator_options_group_keys iogk
                      set options_group_key = tou.options_group_key
                      from _temp_options_updates tou
                      where tou.label_id = iogk.label_id
                        and tou.options_group_id = iogk.options_group_id
                        and tou.options_group_key <> iogk.options_group_key")
      
      dbExecute(conn,"insert into p_rsf.labels(label_id,label_key,primary_label,secondary_labels,label_id_group)
                      select label_id,label_key,primary_label,array_remove(array_agg(distinct secondary_labels),'') as secondary_labels,'options-' || tou.options_group_id
                      from _temp_options_updates tou
                      group by label_id,label_key,primary_label,tou.options_group_id
                      on conflict(label_id,label_key) do update
                      set primary_label = EXCLUDED.primary_label,
                          secondary_labels = EXCLUDED.secondary_labels")
      
      dbExecute(conn,"delete from p_rsf.label_ids lids
                      where exists (select * from _temp_options_updates tou where 'options-' || tou.options_group_id = lids.label_id_group)
                      and not exists(select * from p_rsf.labels lab where lab.label_id = lids.label_id)")
      
      dbExecute(conn,"drop table if exists _temp_options_updates;")      
      TRUE
    },
    error = function(e) {
      print(conditionMessage(e))
      print("db_rsf_options_update: ERROR ROLLING BACK TRANSACTION")
      stop(e)
    },
    warning = function(w) {
      print(conditionMessage(w))
      print("db_rsf_options_update: WARNING ROLLING BACK TRANSACTION")
      stop(w)
    })    
  })
  
  return (saved)
}