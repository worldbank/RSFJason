db_indicators_get_formulas <- function(pool) {
  formulas <- dbGetQuery(pool,"
    select 
      ind.indicator_id,
      indf.formula_id,
      ind.indicator_name,
      indf.formula_title
     from p_rsf.indicator_formulas indf
    inner join p_rsf.indicators ind on ind.indicator_id = indf.indicator_id")
  
  setDT(formulas)
  formulas[,indicator_name_formula_title:=paste0(indicator_name,"(",formula_title,")")]
  return (formulas)
}