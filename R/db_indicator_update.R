db_indicator_update <- function(pool,
                                indicator,
                                labels,
                                formulas,
                                user_id) {
  
  if (length(unique(indicator$indicator_id)) != 1) stop("Only one indicator may be updated at a time.")

  lables <- unique(labels[,.(label_id,
                             label_key,
                             is_primary,
                             label)])
  
  labels[,label:=gsub("[[:cntrl:][:space:]]+"," ",label)]
  labels[,label:=trimws(label)]
  labels <- labels[is.na(label)==FALSE]
  labels <- labels[nchar(label) > 3]
  
  
  
  #labels
  #conn <- poolCheckout(pool)
  #dbBegin(conn)
  #dbRollback(conn)
  poolWithTransaction(pool,function(conn) {
    
    dbExecute(conn,"create temp table _temp_labels(label_id int,
                                                   label_key text,
                                                   is_primary bool,
                                                   label text)
              on commit drop;")
    
    dbAppendTable(conn,
                  name="_temp_labels",
                  value=labels[,.(label_id,
                                  label_key,
                                  is_primary,
                                  label)])
    
    #see for regexp escape discussion
    #https://stackoverflow.com/questions/5144036/escape-function-for-regular-expression-or-like-patterns/45741630#45741630
    #Regex replace to add escape characters too much and really we don't want identical matching we want to match the substance of the label so as to also include
    #very similar ones (ie, if they end one with a period or not)
    duplicate <- dbGetQuery(conn,"
      select distinct 
        vil.indicator_id,
        vil.indicator_name,
        vil.label
      from p_rsf.view_indicator_labels vil
      where vil.indicator_id <> $1::int
        and exists(select * from _temp_labels tl
           where regexp_replace(tl.label, '[^%#&@$<>\\/[:alnum:]]', '', 'g') ~* 
					       ('^' ||  regexp_replace(vil.label, '[^%#&@$<>\\/[:alnum:]]', '', 'g') || '$'))",
      
      
      params=list(indicator$indicator_id))
    
    if (!empty(duplicate)) {
      stop(paste0("Indicator names (and aliases must be unique).  Update indicator failed because: ",
                  paste0(paste0(duplicate$label," conflicts with ",duplicate$indicator_name),collapse = " AND ALSO ")))
    }
    
    dbExecute(conn,"
      insert into p_rsf.labels(label_id,
                               label_key,
                               primary_label,
                               secondary_labels,
                               label_id_group)
      select 
        tl.label_id,
        tl.label_key,
        (array_agg(distinct label order by label) filter(where is_primary = true))[1],
        array_agg(distinct label order by label) filter(where is_primary = false),
        'indicators'
      from _temp_labels tl
      group by
        tl.label_id,
        tl.label_key
      on conflict(label_id,label_key)
      do update
      set primary_label = EXCLUDED.primary_label,
          secondary_labels = EXCLUDED.secondary_labels")
  })
  
  #2: Update indicators.  Various checks require update before formulas to ensure consistency, ie, with defaults not being calculated.
  
  if (indicator$data_type=="currency_ratio") {
    if (empty(formulas) || nrow(formulas)==0) stop("currency ratio indicators must have a calculated formula")
    
    dbExecute(pool,"
              update p_rsf.indicators 
              set is_calculated = true
              where indicator_id = $1::int",
              params=list(indicator$indicator_id))

  }
  
  dbExecute(pool,"
    update p_rsf.indicators
    set indicator_name = $1,
       data_category = $2,
       data_type = $3,
       data_unit = NULLIF($4,'NA'),
       default_value = NULLIF($5,'NA'),
       definition = NULLIF($6,'NA'),
       indicator_options_group_id = NULLIF($7::text,'NA')::int,
       indicator_options_group_allows_blanks = NULLIF($8,'NA')::bool,
       indicator_options_group_allows_multiples = NULLIF($9,'NA')::bool,
       is_periodic_or_flow_reporting = coalesce($10::bool,false),
       default_subscription = coalesce(NULLIF($11::text,'NA')::bool,true)::bool,
       sort_preference = NULLIF($12::text,'NA')::int2,
       modified_by_user_id = $13::text,
       modification_time = (timeofday())::timestamptz
    where indicator_id = $14::int
    ",params=list(indicator$indicator_name,
                  indicator$data_category,
                  indicator$data_type,
                  indicator$data_unit,
                  indicator$default_value,
                  indicator$definition,
                  indicator$options_group_id,
                  indicator$options_group_allows_blanks,
                  indicator$options_group_allows_multiples,
                  indicator$data_frequency,
                  indicator$default_subscription,
                  indicator$sort_preference,
                  user_id,
                  indicator$indicator_id))
  
  #conn <- poolCheckout(pool)
  #dbBegin(conn)
  #dbRollback(conn)
  poolWithTransaction(pool,function(conn) {

    dbExecute(conn,"
      create temp table _temp_formulas(indicator_id int,
                                       formula_id int,
                                       formula text,
                                       formula_sort text,
                                       formula_overwrite text,
                                       formula_fx_date text,
                                       formula_title text,
                                       formula_notes text,
                                       formula_labels text,
                                       formula_unit_set_by_indicator_name text,
                                       is_primary_default bool)
      on commit drop;")
    
    dbAppendTable(conn,
                  name="_temp_formulas",
                  value=formulas[,
                                 .(indicator_id,
                                   formula_id,
                                   formula,
                                   formula_sort,
                                   formula_overwrite,
                                   formula_fx_date,
                                   formula_title,
                                   formula_notes,
                                   formula_unit_set_by_indicator_name,
                                   is_primary_default)])
    
    if (any(formulas$formula_id < 0)) {
      dbExecute(conn,"
        update _temp_formulas tf
        set formula_id = nextval('p_rsf.indicator_formulas_formula_id_seq'::regclass)
        where not exists(select * from p_rsf.indicator_formulas indf
                         where indf.formula_id = tf.formula_id)
      ")
    }
    
    dbExecute(conn,"
      insert into p_rsf.indicator_formulas(formula_id,
                                           indicator_id,
                                           formula,
                                           formula_sort,
                                           overwrite,
                                           formula_fx_date,
                                           formula_title,
                                           formula_notes,
                                           formula_unit_set_by_indicator_id,
                                           is_primary_default,
                                           modified_by_user_id)
      select
        tf.formula_id,
        tf.indicator_id,
        NULLIF(tf.formula,''),
        NULLIF(tf.formula_sort,''),
        coalesce(NULLIF(tf.formula_overwrite,''),'allow'),
        coalesce(NULLIF(tf.formula_fx_date,''),'calculation'),
        coalesce(NULLIF(tf.formula_title,''),'Missing Formula Title'),
        NULLIF(tf.formula_notes,''),
        ind.indicator_id as formula_unit_set_by_indicator_id,
        coalesce(tf.is_primary_default,false),
        $1::text
      from _temp_formulas tf
      left join p_rsf.indicators ind on ind.indicator_name = tf.formula_unit_set_by_indicator_name
      on conflict(formula_id)
      do update
      set indicator_id = excluded.indicator_id,
          formula = excluded.formula,
          formula_sort = excluded.formula_sort,
          overwrite = excluded.overwrite,
          formula_fx_date = excluded.formula_fx_date,
          formula_title = excluded.formula_title,
          formula_notes = excluded.formula_notes,
          formula_unit_set_by_indicator_id = excluded.formula_unit_set_by_indicator_id,
          is_primary_default = excluded.is_primary_default,
          modified_by_user_id = excluded.modified_by_user_id;",
      params=list(user_id))

    #This was a bad idea, just created a lot of UI stuff -- moved formula definition matching into header actions    
    # dbExecute(conn,"
    # insert into p_rsf.labels(label_id,label_key,primary_label,label_id_group)
    # select 
    #   lid.label_id,
    #   'RSA' as label_key,
    #   tf.formula_labels,
    #   lid.label_id_group
    # from p_rsf.label_ids lid
    # inner join p_rsf.indicator_formulas indf on indf.label_id = lid.label_id
    # inner join _temp_formulas tf on tf.formula_id = indf.formula_id
    # on conflict(label_id,label_key) 
    # do update
    # set primary_label = EXCLUDED.primary_label")
  })
    
  return (TRUE)
}