# for_indicator_ids <- 448
# for_rsf_program_ids <- c(56)
# recalculation_start_date <- NA_character_
# DBPOOL %>% rsf_calculations_recalculate(for_indicator_ids=for_indicator_ids,
#                                        for_rsf_program_id=for_rsf_program_ids,
#                                        recalculation_start_date=NA)

rsf_calculations_recalculate <- function(pool,
                                         for_rsf_program_ids=NA,
                                         for_indicator_ids=NA,
                                         recalculation_start_date=NA)
{
  
  if (all(is.na(for_rsf_program_ids))) {
    rsf_program_ids <- dbGetQuery(pool,"select rsf_program_id from p_rsf.rsf_programs")
    rsf_program_ids <- unlist(rsf_program_ids)
  }
  
  clients <- dbGetQuery(pool,"
                        select rsf_pfcbl_id
                        from p_rsf.rsf_pfcbl_ids
                        where pfcbl_category = 'client'
                        and rsf_program_id = any(select unnest(string_to_array($1::text,','))::int)",
                        params=list(paste0(rsf_program_ids,collapse=",")))
  
  if (!isTruthy(clients)) return (NULL)
  clients <- clients[,.(rsf_program_id,
                        rsf_pfcbl_id,
                        client_name)]
  
  rsf_indicators <- db_indicators_get_labels(pool=pool)
  
  if (SELECTED_PROGRAM_ID() != 0) {
    clients <- rbindlist(list(data.table(rsf_program_id=0,
                                         rsf_pfcbl_id=0,
                                         client_name="GLOBAL"),
                              clients))
  }
  
  for (i in 1:nrow(clients)) {
    client <- clients[i]
    withProgress(value=((i-1)/nrow(clients)),
                 message=paste0("Recalculating ",client$client_name,": "), 
                 {
                   progress_status_message <- function(class,...) {
                     dots <- list(...)
                     dots <- paste0(unlist(dots),collapse=" ")
                     incProgress(amount=0,
                                 message=paste0("Recalculating ",client$client_name,": ",dots))
                   }
                   DBPOOL %>% rsf_program_calculate(rsf_program_id=client$rsf_program_id,
                                                    rsf_indicators=rsf_indicators,
                                                    calculate_pfcbl_ids.familytree=client$rsf_pfcbl_id,
                                                    status_message=progress_status_message)
                 })
  }
}