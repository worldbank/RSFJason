
db_program_facility_checks_add_update_guidance <- function(pool,
                                                           guidance_id,
                                                           indicator_id,
                                                           indicator_check_id,
                                                           for_rsf_pfcbl_id,
                                                           user_id,
                                                           guidance,
                                                           auto_resolve=FALSE,
                                                           ignoring=FALSE,
                                                           overwrite_check_class=NA,
                                                           tolerance_variance=0,
                                                           pfcbl_category)
{
  #browser()
  
  # guidance_id <<- guidance_id
  # indicator_id <<- indicator_id 
  # indicator_check_id <<- indicator_check_id
  # for_rsf_pfcbl_id <<- for_rsf_pfcbl_id
  # user_id <<- user_id
  # guidance <<- guidance
  # auto_resolve <<- auto_resolve
  # ignoring <<- ignoring
  # overwrite_check_class <<- overwrite_check_class
  # tolerance_variance <<- tolerance_variance
  # pfcbl_category <<- pfcbl_category
  
  current_guidance <- guidance
  if (length(guidance)==0 || is.na(guidance) || nchar(guidance)==0) guidance <- NA
  if (length(pfcbl_category) == 0 || !pfcbl_category %in% c("global","program","facility")) stop ("Pfcbl category must be either program or facility")
  
  if (pfcbl_category=="global") {
    for_rsf_pfcbl_id <- 0
  }
  
  if (is.na(suppressWarnings(as.numeric(tolerance_variance)))==TRUE) {
    tolerance_variance <- 0
  } else {
    tolerance_variance <- as.numeric(tolerance_variance)
  }
  
  if (ignoring==TRUE) auto_resolve <- FALSE
  
  #Blank guidance on a "new"/blank row means to delete any current guidance
  if (is.na(guidance) && is.na(guidance_id)) {
    dbExecute(pool,
      "with guidance as MATERIALIZED (
         select indicator_check_guidance_id
         from p_rsf.indicator_check_guidance icg
         where icg.indicator_check_id = $1
           and icg.for_indicator_id = $2
      )
      delete from p_rsf.rsf_program_facility_check_guidance pfcg
      where pfcg.rsf_pfcbl_id = any(select ft.to_family_rsf_pfcbl_id
                                    from p_rsf.view_rsf_pfcbl_id_family_tree ft
                                    where ft.from_rsf_pfcbl_id = $3::int)
        and exists(select * from guidance gu
                   where gu.indicator_check_guidance_id = pfcg.indicator_check_guidance_id))",
      params=list(indicator_check_id,
                  indicator_id,
                  for_rsf_pfcbl_id))
    return (NA)
  }
  
  #Blank guidance on an existing guidance_id means to delete the subscription.
  if (is.na(guidance) && !is.na(guidance_id)) {
    dbExecute(pool,
              "with guidance as MATERIALIZED (
                select 
                  $1::int as indicator_check_guidance_id,
                  ids.rsf_program_id,
                  ids.rsf_facility_id
                from p_rsf.rsf_pfcbl_ids ids 
                where ids.rsf_pfcbl_id = $2::int
              )
              delete from p_rsf.rsf_program_facility_check_guidance pfcg
              where pfcg.indicator_check_guidance_id = $1::int
                and pfcg.rsf_pfcbl_id = any(select ft.to_family_rsf_pfcbl_id
                                            from p_rsf.view_rsf_pfcbl_id_family_tree ft 
                                            where ft.from_rsf_pfcbl_id = $2:int)
                           where pfcg.rsf_pfcbl_id = ft.rsf_pfcbl_id)
              ",
              params=list(guidance_id,
                          for_rsf_pfcbl_id))
    
    dbExecute(pool,
              "delete from p_rsf.indicator_check_guidance icg
              where icg.indicator_check_guidance_id = $1::int
                and not exists(select * from p_rsf.rsf_program_facility_check_guidance pfcg
                               where pfcg.indicator_check_guidance_id = icg.indicator_check_guidance_id)",
              params=list(guidance_id))
    
    return (guidance_id)
  }
  
  duplicate_guidance <- NULL
  
  #New guidance on new guidance row
  if (is.na(guidance_id)) {
    new_id <- dbGetQuery(pool,
                         "select icg.indicator_check_guidance_id
                         from p_rsf.indicator_check_guidance icg
                         where icg.indicator_check_id = $1::int
                           and icg.for_indicator_id = $2::int
                           and normalizeLabel(icg.guidance) is not distinct from normalizeLabel($3::text)",
                         params=list(indicator_check_id,
                                     indicator_id,
                                     guidance))
    
    if (!empty(new_id)) {
      guidance_id <- as.numeric(unlist(new_id))
    } else {
      new_id <- dbGetQuery(pool,
                           "insert into p_rsf.indicator_check_guidance(indicator_check_id,
                                                                       for_indicator_id,
                                                                       guidance,
                                                                       is_resolving_guidance,
                                                                       is_ignoring_guidance,
                                                                       for_pfcbl_category,
                                                                       user_id,
                                                                       overwrite_check_class,
                                                                       variance_threshold,
                                                                       update_time)
                           select 
                            $1::int,
                            $2::int,
                            $3::text,
                            $4::bool,
                            $5::bool,
                            $6::text,
                            $7::text,
                            NULLIF($8::text,'NA')::text,
                            round(coalesce($9::numeric,0),3),
                            timeofday()::timestamptz
                            returning indicator_check_guidance_id;",
                           params=list(indicator_check_id,
                                       indicator_id,
                                       guidance,
                                       auto_resolve,
                                       ignoring,
                                       pfcbl_category,
                                       user_id,
                                       paste0(overwrite_check_class),
                                       tolerance_variance))
      
      guidance_id <- as.numeric(unlist(new_id))
      if (length(guidance_id) != 1 || is.na(guidance_id)) stop("Failed to insert guidance: verify that identical guidance comment does not already exist?")
    }    
  } 
  
  {
    exists <- dbGetQuery(pool,
                         "select exists(select * from p_rsf.indicator_check_guidance icg
                                      where icg.indicator_check_guidance_id = $1::int
                                        and icg.for_indicator_id = $2::int
                                        and icg.indicator_check_id = $3::int) as guidance_exists",
                         params=list(guidance_id,
                                     indicator_id,
                                     indicator_check_id))
    exists <- as.logical(unlist(exists))
    
    if (!exists) {
      stop(paste0("Failed to identify guidance with indicator_check_guidance_id=",guidance_id,
                  " AND for_indicator_id=",indicator_id,
                  " AND indicator_check_id=",indicator_check_id))
    }
    
    duplicate_guidance <- dbGetQuery(pool,
                                        "select 
                                          icg.indicator_check_guidance_id,
                                          icg.for_pfcbl_category,
                                          $4::int as migrate_guidance_id
                                         from p_rsf.indicator_check_guidance icg
                                         where icg.indicator_check_id = $1::int
                                           and icg.for_indicator_id = $2::int
                                           and normalizeLabel(icg.guidance) = normalizeLabel($3::text)
                                           and icg.indicator_check_guidance_id <> $4::int",
                                        params=list(indicator_check_id,
                                                    indicator_id,
                                                    guidance,
                                                    guidance_id))
    
    #Means user is creating an identical guidance to one that already exists.
    #So let's delete this one and migrate to the one that already exists.
    if (!empty(duplicate_guidance)) {

      guidance_id <- duplicate_guidance$indicator_check_guidance_id
      pfcbl_category <- duplicate_guidance$for_pfcbl_category
    }
    
    current_guidance <- dbGetQuery(pool,
                                  "select 
                                    icg.guidance,
                                    icg.for_pfcbl_category
                                   from p_rsf.indicator_check_guidance icg
                                   where icg.indicator_check_guidance_id = $1::int",
                                   params=list(guidance_id))
    
    current_guidance_pfcbl_category <- current_guidance$for_pfcbl_category
    current_guidance <- current_guidance$guidance
    
    dbExecute(pool,"
              update p_rsf.indicator_check_guidance icg
              set 
                guidance = $1::text,
                is_resolving_guidance = $2::bool,
                is_ignoring_guidance = $6::bool,
                user_id = $3::text,
                overwrite_check_class = NULLIF($4::text,'NA')::text,
                variance_threshold = round(coalesce($7::numeric,0),3),
                update_time = timeofday()::timestamptz
              where icg.indicator_check_guidance_id = $5::int
                and (icg.guidance <> $1::text
                     or
                     icg.is_resolving_guidance <> $2::bool
                     or 
                     icg.overwrite_check_class is distinct from NULLIF($4::text,'NA')::text)",
              params=list(guidance,
                          auto_resolve,
                          user_id,
                          paste0(overwrite_check_class),
                          guidance_id,
                          ignoring,
                          tolerance_variance))
  }

  #conn <- poolCheckout(pool)
  #dbBegin(conn)
  #dbRollback(conn)
  rsf_pfcbl_id <- poolWithTransaction(pool,function(conn) {
    rsf_pfcbl_id <- dbGetQuery(conn,
                               "
                                select ft.to_family_rsf_pfcbl_id as rsf_pfcbl_id
                                from p_rsf.view_rsf_pfcbl_id_family_tree ft
                                where ft.from_rsf_pfcbl_id = $1::int
                                  and ft.to_pfcbl_category = $2",
                                params=list(for_rsf_pfcbl_id,
                                            current_guidance_pfcbl_category))
    
    rsf_pfcbl_id <- as.numeric(unlist(rsf_pfcbl_id))

    if (!empty(duplicate_guidance)) {
      dbExecute(conn,"
                update p_rsf.rsf_program_facility_check_guidance
                set indicator_check_guidance_id = $1::int
                where indicator_check_guidance_id = $2::int",
                params=list(guidance_id,
                            duplicate_guidance$migrate_guidance_id))
      
      dbExecute(conn,"
                update p_rsf.rsf_data_checks rdc
                set indicator_check_guidance_id = $1::int
                where indicator_check_guidance_id = $2::int",
                params=list(guidance_id,
                            duplicate_guidance$migrate_guidance_id))
      
      dbExecute(conn,"
                delete from p_rsf.indicator_check_guidance icg 
                where icg.indicator_check_guidance_id = $1::int",
                params=list(duplicate_guidance$migrate_guidance_id))
    }
    
    #Once set guidance cannot change from facility to program or vice versa, this is obsolete
    #In case the updated guidance_id went from a facility to program-level indicator,
    #then delete the individual facilities subscribed to this guidance, since they inherit the subscription at the program level
    #(and if it went from program to facility, then the above-selected rsf_pfcbl_id will be for the facility and it will subscribe)
    # dbExecute(conn,"
    #           delete
    #           from p_rsf.rsf_program_facility_check_guidance pfcg
    #           where pfcg.indicator_check_guidance_id = $1::int
    #             and exists(select * from p_rsf.indicator_check_guidance icg 
    #                        where icg.indicator_check_guidance_id = pfcg.indicator_check_guidance_id
    #           							 and pfcg.rsf_facility_id is not null and icg.for_pfcbl_category = 'program')",
    #           params=list(guidance_id))
    
    #delete existing subscriptions
    dbExecute(conn,"
              with current_subscriptions as (
                select 
                  pfcg.rsf_pfcbl_id,
                  icg.indicator_check_guidance_id
                from p_rsf.rsf_program_facility_check_guidance pfcg
                inner join p_rsf.indicator_check_guidance icg on icg.indicator_check_guidance_id = pfcg.indicator_check_guidance_id
                where pfcg.rsf_pfcbl_id = $1::int
                  and icg.for_indicator_id = $2::int
                  and icg.indicator_check_id = $3::int
              )
              delete from p_rsf.rsf_program_facility_check_guidance pfcg
              where exists(select * from current_subscriptions cs
                           where cs.rsf_pfcbl_id = pfcg.rsf_pfcbl_id
                             and cs.indicator_check_guidance_id = pfcg.indicator_check_guidance_id)",
              params=list(rsf_pfcbl_id,
                          indicator_id,
                          indicator_check_id))
    
    dbExecute(conn,"
              insert into p_rsf.rsf_program_facility_check_guidance(rsf_pfcbl_id,
                                                                    indicator_check_guidance_id,
                                                                    rsf_program_id,
                                                                    rsf_facility_id,
                                                                    applied_by_user_id,
                                                                    application_time)
              select
                ids.rsf_pfcbl_id,
                $1::int,
                ids.rsf_program_id,
                ids.rsf_facility_id,
                $2::text as applied_by_user_id,
                timeofday()::timestamptz as application_time
              from p_rsf.rsf_pfcbl_ids ids
              where ids.rsf_pfcbl_id = $3::int",
              params=list(guidance_id,
                          user_id,
                          rsf_pfcbl_id))
    return (rsf_pfcbl_id)
  })
  
  dbExecute(pool,"
                            with evaluations as MATERIALIZED (
                              select 
                                rdc.evaluation_id
                              from p_rsf.indicator_check_guidance icg
                              inner join p_rsf.rsf_program_facility_check_guidance pfcg on pfcg.indicator_check_guidance_id = icg.indicator_check_guidance_id
                              inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = pfcg.rsf_pfcbl_id
                              inner join p_rsf.rsf_data rd on rd.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
                                                          and rd.indicator_id = icg.for_indicator_id
                              inner join p_rsf.rsf_data_checks rdc on rdc.data_id = rd.data_id
                              																	  and rdc.indicator_check_id = icg.indicator_check_id
                              where rdc.check_ignore = false
                                and icg.indicator_check_guidance_id = $1::int
                                and (
                                      (rdc.check_status = 'active' AND rdc.indicator_check_guidance_id = icg.indicator_check_guidance_id)
                                      OR 
                                      (rdc.check_status = 'active' AND check_status_user_id is NULL)
                                      OR 
                                      (rdc.check_status_comment is not distinct from NULLIF($2::text,'NA')))
                                
                            )
                            update p_rsf.rsf_data_checks rdc
                            set
                              check_status = case when $3::bool = true then 'resolved' else 'active' end,
                              check_status_comment = $4::text,
                              check_status_user_id = $5::text,
                              indicator_check_guidance_id = $1::int
                            from evaluations ev
                            where ev.evaluation_id = rdc.evaluation_id
                            ",
                            params=list(guidance_id,
                                        current_guidance,
                                        auto_resolve,
                                        guidance,
                                        user_id))
  

  return(guidance_id)
}