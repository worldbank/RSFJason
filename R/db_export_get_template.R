db_export_get_template <- function(pool,
                                   template_name) {
  
  TEMPLATE <- dbGetQuery(pool,
                         "select template_id,template_name,template_key,is_reportable,template_has_static_row_ids
                               from p_rsf.reporting_templates 
                               where template_name = $1
                                  or template_key ~* $1",params=list(template_name))
  
  if (empty(TEMPLATE)) stop(paste0("Failed to find template '",template_name,"'"))
 
  return (TEMPLATE) 
}