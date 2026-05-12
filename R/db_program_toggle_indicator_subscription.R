db_program_toggle_indicator_subscription <- function(pool,
                                                     rsf_program_id,
                                                     rsf_pfcbl_id,
                                                     indicator_id,
                                                     user_id) {
  
  
  status <- dbGetQuery(pool,"
            insert into p_rsf.rsf_setup_indicators(rsf_pfcbl_id,
                                                   indicator_id,
                                                   formula_id,
                                                   rsf_program_id,
                                                   rsf_facility_id,
                                                   is_subscribed,
                                                   is_auto_subscribed,
                                                   auto_subscribed_by_reporting_cohort_id,
                                                   subscription_comments,
                                                   comments_user_id)
            select 
              ids.rsf_pfcbl_id,
              sis.indicator_id,
              sis.formula_id,
              ids.rsf_program_id,
              ids.rsf_facility_id,
              NOT sis.is_subscribed as is_subscribed,
              false as is_auto_subscribed,
              NULL as auto_subscribed_by_reporting_cohort_id,
              case when NOT sis.is_subscribed is false then concat('Unsubscribed by ',coalesce(vai.users_name,'UNKNOWN'),' on ',now()::date)
                   else concat('Subscribed by ',vai.users_name,' on ',now()::date) end as subscription_comments,
              $3::text as comments_user_id
            from p_rsf.view_rsf_setup_indicator_subscriptions sis
            inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = sis.rsf_pfcbl_id
            left join p_rsf.view_account_info vai on vai.account_id = $3::text
            where sis.rsf_pfcbl_id = $1::int
              and sis.indicator_id = $2::int
              and sis.is_system_indicator is false
              and (not sis.is_subscribed = true or sis.is_system_indicator = false)
            on conflict (rsf_pfcbl_id,indicator_id)
            do update
            set is_subscribed = EXCLUDED.is_subscribed,
                is_auto_subscribed = EXCLUDED.is_auto_subscribed,
                auto_subscribed_by_reporting_cohort_id = EXCLUDED.auto_subscribed_by_reporting_cohort_id,
                subscription_comments = concat(rsf_setup_indicators.subscription_comments,'\n',EXCLUDED.subscription_comments),
                comments_user_id = EXCLUDED.comments_user_id
            returning is_subscribed",
            params=list(rsf_pfcbl_id,
                        indicator_id,
                        user_id))
  
  if (empty(status)) {
    status <- dbGetQuery(pool,"
    select is_subscribed
    from p_rsf.view_rsf_setup_indicator_subscriptions
    where rsf_pfcbl_id = $1::int 
      and indicator_id = $2::int",
    params=list(rsf_pfcbl_id,
                indicator_id))
  }
  status <- unlist(status)
  return (status)
}