db_rsf_options_create <- function(pool) {
  
  new_option <- dbGetQuery(pool,"insert into p_rsf.indicator_options_groups(options_group_name)
                                select 
                                  'rsf_new_choices_group' || coalesce(max(substring(newind.options_group_name from '^rsf_new_choices_group([[:digit:]]+)$')::int),0)+1 as options_group_name
                                from p_rsf.indicator_options_groups newind
                                where newind.options_group_name ~ E'^rsf_new_choices_group[[:digit:]]+$'
                                having count(*) < 5 -- limit of pending new indicator checks that can be created
                                  returning options_group_id,options_group_name")
  print("new option created")
  print(new_option)
  return (new_option)
}