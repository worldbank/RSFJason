db_export_create <- function(pool,
                             exporting_user_id,
                             exporting_asof_date,
                             exporting_pfcbl_ids,
                             exporting_indicator_ids=NULL, #NULL means users can add/remove columns and still upload with a viable integrity key
                             template_id,
                             report_id=as.numeric(NA),
                             dashboard_parameters=as.character(NA),
                             exporting_title=as.character(NA))
{
  
  exporting_pfcbl_ids <- unique(as.numeric(exporting_pfcbl_ids))
  exporting_indicator_ids <- unique(as.numeric(exporting_indicator_ids))
  
  if (anyNA(exporting_pfcbl_ids)) stop("exporting_pfcbl_ids contains NAs")
  if (anyNA(exporting_indicator_ids)) stop("exporting_indicator_ids contains NAs")
  if (length(exporting_asof_date) != 1) stop("exporting_asof_date must have length 1.  If export has multiple chronologies, pass the max() reporting date")

  if (length(exporting_indicator_ids)==0) exporting_indicator_ids <- NULL
  
  
  account_info <- dbGetQuery(pool,"select * from p_rsf.view_account_info where account_id = $1::text",
                             params=list(exporting_user_id))
  
  if (empty(account_info)) stop(paste0("Failed to locate account info for user ID: ",exporting_user_id))
  
  data_integrity_key <- rsf_reports_data_integrity_key(reporting_asof_date=exporting_asof_date,
                                                       rsf_pfcbl_ids=exporting_pfcbl_ids,
                                                       indicator_ids=exporting_indicator_ids)
  
  #conn <- poolCheckout(pool)
  #dbBegin(conn)
  #dbRollback(conn)
  exporting_cohort <- poolWithTransaction(pool,function(conn) {

    exporting_parent <- dbGetQuery(conn,"
                                   with rank_members as materialized (
                                    select 
                                  	to_pfcbl_rank,
                                  	array_agg(distinct to_family_rsf_pfcbl_id) as rsf_pfcbl_ids,
                                  	count(distinct to_family_rsf_pfcbl_id) as members 
                                  	from p_rsf.view_rsf_pfcbl_id_family_tree ft
                                  	where ft.from_rsf_pfcbl_id = any($1::int[])
                                  		and to_pfcbl_rank <= 2
                                  		--and to_pfcbl_category <> 'global'
                                  	group by to_pfcbl_rank
                                  ),
                                  parent as materialized (
                                    select 
                                    unnest(rsf_pfcbl_ids) as rsf_pfcbl_id
                                    from rank_members rm
                                    where rm.members = 1
                                    order by to_pfcbl_rank desc
                                    limit 1
                                  )
                                  select
                                    ids.rsf_pf_id as exporting_rsf_pf_id,
                                    ids.pfcbl_category,
                                    nids.sys_name
                                  from p_rsf.rsf_pfcbl_ids ids
                                  inner join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = ids.rsf_pfcbl_id
                                  where ids.rsf_pfcbl_id = (select rsf_pfcbl_id from parent)",
                                  params=list(dbMakeIntArray(exporting_pfcbl_ids)))
 
    
    #nothing here.  Probably due to multiple programs being selected in an export.  Otherwise, this shouldn't happen!
    # if (empty(exporting_parent)) {
    #   exporting_parent  <- dbGetQuery(conn,"
    #     select
    #       ids.rsf_program_id,
    #       ids.rsf_pfcbl_id as exporting_rsf_pfcbl_id,
    #       ids.pfcbl_category
    #     from p_rsf.rsf_pfcbl_ids ids
    #     where ids.rsf_pfcbl_id = 0
    #   ")
    # }
    
    exporting_cohort <- dbGetQuery(conn,"
      insert into p_rsf.dashboard_exports(exporting_rsf_pf_id,
                                          exporting_user_id,
                                          export_name,
                                          generated_with_report_id,
                                          for_reporting_template_id,
                                          data_integrity_key,
                                          exporting_asof_date,
                                          dashboard_parameters,
                                          exporting_title)
                        select
                         $1::int exporting_rsf_pf_id,
                         $2::text as exporting_user_id,
                         $3::text as export_name,
                         $4::int as generated_with_report_id,
                         $5::int as template_id,
                         $6::text as data_integrity_key,
                         $7::date as export_asof_date,
                         $8::jsonb as dashboard_parameters,
                         $9::text as exporting_title
                        returning 
                          export_id,
                          export_name,
                          exporting_rsf_pf_id,
                          exporting_asof_date,
                          reporting_key,
                          data_integrity_key,
                          exporting_time",
                     params=list(exporting_parent$exporting_rsf_pf_id,
                                 exporting_user_id,
                                 exporting_parent$sys_name,
                                 report_id,
                                 template_id,
                                 data_integrity_key,
                                 exporting_asof_date,
                                 jsonlite::toJSON(dashboard_parameters),
                                 exporting_title))

    return(exporting_cohort)
  })

  # program_info <- dbGetQuery(pool,
  #                           "select
  #                             coalesce(nickname,name,pfcbl_name) as name
  #                            from p_rsf.rsf_data_current_names_and_ids nai
  #                           where nai.rsf_pfcbl_id = $1
  #                             and nai.reporting_asof_date <= $2::date
  #                           order by nai.reporting_asof_date desc
  #                           limit 1",
  #                           params=list(exporting_cohort$rsf_program_id,
  #                                       exporting_asof_date))
  
  sys_name <- dbGetQuery(pool,
                         "select
                            nai.sys_name,
                            nai.pfcbl_name
                          from p_rsf.rsf_data_current_names_and_ids nai
                          where nai.rsf_pfcbl_id = $1::int
                            and nai.reporting_asof_date <= $2::date
                          order by nai.reporting_asof_date desc
                          limit 1",
                         params=list(exporting_cohort$exporting_rsf_pf_id,
                                     exporting_asof_date))
  setDT(exporting_cohort)
  
  exporting_cohort[,program_name:=paste0("RSF Jason Extract #",export_id)]
  exporting_cohort[,exporting_entity_name:=sys_name$sys_name]
  exporting_cohort[,exporting_pfcbl_name:=sys_name$pfcbl_name]
  exporting_cohort[,exporting_users_name:=account_info$users_name]
  
  return (exporting_cohort)
}
