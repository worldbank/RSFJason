db_rsf_reports_list <- function(pool,
                                rsf_program_id,
                                account_user_id=NA) {
  print("db_rsf_reports_list must be entirely redone")
  
  # reports <- dbGetQuery(pool,"with report_indicators as (
  #                             	select
  #                             	$1::int as rsf_program_id,
  #                             	re.report_id,
  #                             	re.report_name,
  #                             	re.is_system,
  #                             	ri.indicator_id,
  #                             	ri.section_name,
  #                             	ri.section_order,
  #                             	ri.indicator_order
  #                             	from p_rsf.reports re
  #                             	left join p_rsf.report_indicators ri on ri.report_id = re.report_id
  #                             	where re.is_system=true
  #                             		 or (
  #                             				 (
  #                             				  coalesce(cardinality(re.rsf_program_id_subscriptions),0)=0 
  #                             				  or 
  #                             				  re.rsf_program_id_subscriptions && array[$1::int])
  #                             				 
  #                             				 and
  #                             				 
  #                             				 (re.created_by_user_id = NULLIF($2::text,'NA')
  #                             				  or 
  #                             				  re.is_public=true
  #                             				 )
  #                             				)
  #                             )
  #                             select 
  #                             ri.rsf_program_id,
  #                             ri.report_id,
  #                             case when ri.is_system then 'SYS: ' || ri.report_name else ri.report_name end as report_name,
  #                             ri.is_system,
  #                             ri.indicator_id,
  #                             ind.indicator_name,
  #                             ind.data_category,
  #                             ri.section_name,
  #                             ri.section_order,
  #                             ri.indicator_order
  #                             from report_indicators ri
  #                             left join p_rsf.indicators ind on ind.indicator_id = ri.indicator_id
  #                             where ind.indicator_id is NULL
  #                                or exists(select * from p_rsf.rsf_program_indicators rpi 
  #                             	           where rpi.rsf_program_id = ri.rsf_program_id 
  #                             							 and rpi.indicator_id = ri.indicator_id
  #                             							 and rpi.is_deprecated = false
  #                             							 and rpi.is_hidden = false)
  #                             order by 
  #                               ri.is_system asc,
  #                               ri.report_id,
  #                               ri.section_order,
  #                               ri.indicator_order",
  #                             params=list(rsf_program_id,
  #                                         account_user_id))
  # 
  # setDT(reports)
  # reports[,report_data_categories:=paste0(sort(c(unique(data_category))),collapse=","),
  #         by=.(report_id)]
  reports <- NULL
  return (reports)
}
