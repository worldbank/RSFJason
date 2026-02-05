db_program_toggle_check_subscription <- function(pool,
                                                 rsf_pfcbl_id,
                                                 check_formula_id) {

  subscription_status <- dbGetQuery(pool,"
    with status as (
      select 
        ids.rsf_pfcbl_id,
        ids.rsf_program_id,
        ids.rsf_facility_id,
        $2::int as check_formula_id,
        coalesce(sub.is_subscribed,false) as is_subscribed
      from p_rsf.rsf_pfcbl_ids ids
      left join p_rsf.view_rsf_setup_check_subscriptions sub on sub.rsf_pfcbl_id = ids.rsf_pfcbl_id
                                                            and sub.check_formula_id = $2::int
      where ids.rsf_pfcbl_id = $1::int   
    )
    insert into p_rsf.rsf_setup_checks(rsf_pfcbl_id,
                                                  check_formula_id,
                                                  indicator_check_id,
                                                  rsf_program_id,
                                                  rsf_facility_id,
                                                  is_subscribed,
                                                  is_auto_subscribed,
                                                  auto_subscribed_by_reporting_cohort_id)
    select
      status.rsf_pfcbl_id,
      status.check_formula_id,
      icf.indicator_check_id,
      status.rsf_program_id,
      status.rsf_facility_id,
      (not status.is_subscribed) as is_subscribed,
      false as is_auto_subscribed,
      NULL as auto_subscribed_by_reporting_cohort_id
    from status
    inner join p_rsf.indicator_check_formulas icf on icf.check_formula_id = status.check_formula_id
    on conflict(rsf_pfcbl_id,check_formula_id)
    do update
    set is_subscribed = EXCLUDED.is_subscribed,
        indicator_check_id = EXCLUDED.indicator_check_id,
        is_auto_subscribed = EXCLUDED.is_auto_subscribed,
        auto_subscribed_by_reporting_cohort_id = EXCLUDED.auto_subscribed_by_reporting_cohort_id
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