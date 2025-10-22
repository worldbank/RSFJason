db_program_toggle_indicator_subscription <- function(pool,
                                                     rsf_program_id,
                                                     rsf_pfcbl_id,
                                                     indicator_id) {
  
  
  status <- dbGetQuery(pool,"
            insert into p_rsf.rsf_program_facility_indicators(rsf_pfcbl_id,
                                                              indicator_id,
                                                              formula_id,
                                                              rsf_program_id,
                                                              rsf_facility_id,
                                                              is_subscribed,
                                                              is_auto_subscribed)
            select 
              ids.rsf_pfcbl_id,
              fis.indicator_id,
              fis.formula_id,
              ids.rsf_program_id,
              ids.rsf_facility_id,
              not is_subscribed as is_subscribed,
              false as is_auto_subscribed
            from p_rsf.view_rsf_program_facility_indicator_subscriptions fis
            inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = fis.rsf_pfcbl_id
            where ids.rsf_pfcbl_id = $1::int
              and ids.rsf_program_id = $2::int
              and fis.indicator_id = $3
              and fis.is_system = false
              and fis.setting_allowed is true
            on conflict (rsf_pfcbl_id,indicator_id)
            do update
            set is_subscribed = EXCLUDED.is_subscribed,
                is_auto_subscribed = EXCLUDED.is_auto_subscribed
            returning is_subscribed",
            params=list(rsf_pfcbl_id,
                        rsf_program_id,
                        indicator_id))
  
  if (empty(status)) {
    status <- dbGetQuery(pool,"
    select is_subscribed or is_auto_subscribed or is_system
    from p_rsf.view_rsf_program_facility_indicator_subscriptions 
    where rsf_pfcbl_id = $1::int 
      and indicator_id = $2::int",
    params=list(rsf_pfcbl_id,
                indicator_id))
  }
  status <- unlist(status)
  return (status)
}