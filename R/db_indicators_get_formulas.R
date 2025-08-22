db_indicators_get_formulas <- function(pool) {
  formulas <- dbGetQuery(pool,"
    select 
      ind.indicator_id,
      indf.formula_id,
      concat(indf.formula,(' {{SORTING BY}} ' || indf.formula_sort)) as formula,
      indf.overwrite,
      indf.formula_fx_date,
      indf.formula_title,
      indf.formula_notes,
      indf.is_primary_default,
      ind.indicator_name
    from p_rsf.indicator_formulas indf
    inner join p_rsf.indicators ind on ind.indicator_id = indf.indicator_id")
  
  setDT(formulas)
  formulas[,indicator_name_formula_title:=paste0(indicator_name,"(",formula_title,")")]
  return (formulas)
}
