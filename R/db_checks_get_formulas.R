db_checks_get_formulas <- function(pool) {
  formulas <- dbGetQuery(pool,"
  select 
    ic.indicator_check_id,
    icf.check_formula_id,
    ic.check_name,
    icf.check_formula_title
  from p_rsf.indicator_check_formulas icf
  inner join p_rsf.indicator_checks ic on ic.indicator_check_id = icf.indicator_check_id")
  
  setDT(formulas)
  formulas[,check_name_formula_title:=paste0(check_name,"(",check_formula_title,")")]
  return (formulas)
}