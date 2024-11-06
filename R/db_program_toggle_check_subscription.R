db_program_toggle_check_subscription <- function(pool,
                                                 rsf_pfcbl_id,
                                                 check_formula_id) {

  subscription_status <- dbGetQuery(pool,"
    insert into p_rsf.rsf_program_facility_checks(rsf_pfcbl_id,
                                                  check_formula_id,
                                                  indicator_check_id,
                                                  rsf_program_id,
                                                  rsf_facility_id,
                                                  is_subscribed,
                                                  is_auto_subscribed)
    select
      fcs.rsf_pfcbl_id,
      fcs.check_formula_id,
      fcs.indicator_check_id,
      fcs.rsf_program_id,
      fcs.rsf_facility_id,
      not sub.is_subscribed as is_subscribed,
      false as is_auto_subscribed
    from p_rsf.view_rsf_program_facility_check_subscribable fcs
    inner join p_rsf.view_rsf_program_facility_check_subscriptions sub on sub.rsf_pfcbl_id = fcs.rsf_pfcbl_id
                                                                      and sub.check_formula_id = fcs.check_formula_id
    where fcs.rsf_pfcbl_id = $1::int
      and fcs.check_formula_id = $2::int
      --and fcs.is_subscribable = true -- now trigger will auto subscribe
    on conflict(rsf_pfcbl_id,check_formula_id)
    do update
    set is_subscribed = EXCLUDED.is_subscribed,
        indicator_check_id = EXCLUDED.indicator_check_id,
        is_auto_subscribed = EXCLUDED.is_auto_subscribed
    returning rsf_pfcbl_id,is_subscribed;",
    params=list(rsf_pfcbl_id,
                check_formula_id))
  
  if (empty(subscription_status)) { 
    subscription_status <- as.logical(NA)
  } else {
    subscription_status <- subscription_status$is_subscribed
  }

  return (subscription_status)
}