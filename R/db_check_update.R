db_check_update <- function(pool,
                            check_data,
                            check_formulas,
                            is_system_check=FALSE) {
  
  #conn <- poolCheckout(pool)
  #dbBegin(conn)
  #dbRollback(conn)
  new_ids <- c()
  if (length(unique(check_data$indicator_check_id)) != 1) stop("Only one check can be updated at a time.")
  
  #Users have limited ability to modify system checks
  if (is_system_check==TRUE) {
    dbExecute(pool,"
      update p_rsf.indicator_checks ic
      set check_class = $1::text,
          check_type = $2::text,
          definition = coalesce($3::text,'No definition')
      where ic.indicator_check_id = $4::int",
      params=list(check_data$check_class,
                  check_data$check_type,
                  check_data$definition,
                  check_data$indicator_check_id))
  } else {
    check_data[!is.na(grouping) &
               !is.na(subgrouping) &
               grouping==check_pfcbl_category,
               grouping:=as.character(NA)]
    
    dbExecute(pool,"
      update p_rsf.indicator_checks ic
      set check_name = $1::text,
          check_class = $2::text,
          check_pfcbl_category = $3::text,
          check_type = $4::text,
          grouping = nullif($5::text,''),
          subgrouping = nullif($6::text,''),
          definition = coalesce($7::text,'No definition'),
          auto_subscribe = coalesce($8::bool,false)
      where ic.indicator_check_id = $9::int",
      params=list(check_data$check_name,
                  check_data$check_class,
                  check_data$check_pfcbl_category,
                  check_data$check_type,
                  check_data$grouping,
                  check_data$subgrouping,
                  check_data$definition,
                  check_data$check_auto_subscribe,
                  check_data$indicator_check_id))
      
    check_formulas <- check_formulas[edited==TRUE,
                                     .(indicator_check_id,
                                       check_formula_id,
                                       check_formula_title,
                                       formula,
                                       formula_result_message,
                                       formula_fx_date,
                                       formula_comments,
                                       auto_resolve)]
    
    new_ids <- poolWithTransaction(pool,function(conn) {
      new_ids <- c()
      dbExecute(conn,"
        create temp table _temp_formulas(indicator_check_id int,
                                         check_formula_id int,
                                         check_formula_title text,
                                         formula text,
                                         formula_result_message text,
                                         formula_fx_date text,
                                         
                                         formula_comments text,
                                         auto_resolve bool default false)
        on commit drop;")
      
      dbAppendTable(conn,
                    name="_temp_formulas",
                    value=check_formulas)
      
      if (any(check_formulas$check_formula_id < 0)) {
        new_ids <- dbGetQuery(conn,"
          update _temp_formulas tf
          set check_formula_id = nextval('p_rsf.indicator_check_formulas_check_formula_id_seq'::regclass)
          where not exists(select * from p_rsf.indicator_check_formulas icf
                           where icf.check_formula_id = tf.check_formula_id)
          returning check_formula_id
        ")
        new_ids <- as.numeric(unlist(new_ids))
      }
      
      dbExecute(conn,"
        insert into p_rsf.indicator_check_formulas(indicator_check_id,
                                                   check_formula_id,
                                                   check_formula_title,
                                                   check_pfcbl_category,
                                                   formula,
                                                   formula_result_message,
                                                   formula_fx_date,
                                                   formula_comments,
                                                   auto_resolve)
        select
          tf.indicator_check_id,
          tf.check_formula_id,
          tf.check_formula_title,
          ic.check_pfcbl_category,
          nullif(tf.formula,''),
          nullif(tf.formula_result_message,''),
          coalesce(nullif(tf.formula_fx_date,''),'calculation'),
          nullif(tf.formula_comments,''),
          coalesce(tf.auto_resolve,false)
        from _temp_formulas tf
        inner join p_rsf.indicator_checks ic on ic.indicator_check_id = tf.indicator_check_id
        on conflict(check_formula_id)
        do update
        set check_formula_title = excluded.check_formula_title,
            formula = excluded.formula,
            formula_result_message = excluded.formula_result_message,
            formula_fx_date = excluded.formula_fx_date,
            formula_comments = excluded.formula_comments,
            auto_resolve = excluded.auto_resolve;")
      
      return (new_ids)
    })
  } 
  return (new_ids)
}