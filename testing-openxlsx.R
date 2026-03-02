

Sys.setenv(PATH="c:\\rtools44\\x86_64-w64-mingw32.static.posix\\bin;c:\\rtools44\\usr\\bin;C:\\Program Files\\R\\R-4.5.2\\bin\\x64;C:\\WINDOWS\\system32;C:\\WINDOWS;C:\\WINDOWS\\System32\\Wbem;C:\\WINDOWS\\System32\\WindowsPowerShell\\v1.0\\;C:\\WINDOWS\\System32\\OpenSSH\\;C:\\Program Files\\dotnet\\;C:\\Users\\SHeitmann\\AppData\\Local\\Microsoft\\WindowsApps;C:\\Users\\SHeitmann\\AppData\\Local\\GitHubDesktop\\bin;C:\\Program Files\\RStudio\\resources\\app\\bin\\quarto\\bin;C:\\Program Files\\RStudio\\resources\\app\\bin\\postback")
Sys.getenv("PATH")
#remotes::install_github("JanMarvin/openxlsx2@main")
#remotes::install_github("JanMarvin/openxlsx2@71b01a55a712b7c3bbc7d6e0a415d26bc67f5f76")

options(warn = 1)
options(error = NULL)
options(error = recover)

return.insert_flags <- readRDS("testing_flags.RDS")

x <- parse_template_IFC_QR2025(pool=pool,
                               template_file=template_file,
                               return.insert_flags = return.insert_flags,
                               template_lookup=db_export_get_template(pool=pool,template_name="IFC-QR-TEMPLATE2025"),
                               rsf_indicators=db_indicators_get_labels(pool),
                               return.next_date=NULL,
                               status_message=status_message,
                               CALCULATIONS_ENVIRONMENT=CALCULATIONS_ENVIRONMENT)
x$save("C:/Temp/comments.xlsx")

template_file <- "C:/Temp/comments2.xlsx" #removed comments

excelwb$save("C:/Temp/comments1.xlsx",overwrite = T)
excelwb$save("C:/Temp/comments1-custom.xlsx",overwrite = T)
excelwb$save("C:/Temp/comments1-qreport.xlsx",overwrite = T)
excelwb$save("C:/Temp/comments1-summary.xlsx",overwrite = T)
###########

library(profvis)
library(openxlsx2)
wb <- wb_workbook(creator="John Doe")
wb <- wb_add_worksheet(wb)
wb <- wb_add_worksheet(wb)
wb <- wb_add_person(wb,name="John Doe")
pid <- wb$get_person("John Doe")$id;

timing <- c()
for (i in 1:500) {
  t1 <- Sys.time()
  wb$add_thread(sheet=1,dims=paste0("A",i),
                person_id=pid,
                comment=paste0("Comment ",i))
  timing <- c(timing,as.numeric(Sys.time() - t1,"secs"))
}

p <- profvis({ 
for (i in 1:500) {

  wb$add_thread(sheet=2,dims=paste0("A",i),
                person_id=pid,
                comment=paste0("Comment ",i))
}
})
  
plot(timing)



wb <- wb_add_thread(wb,sheet=1,dims="A1",person_id=pid,comment='<![CDATA[Five is < Twenty]]>')
wb <- wb_add_thread(wb,sheet=1,dims="A10",person_id=pid,comment='Five is < Twenty')
wb <- wb_add_thread(wb,sheet=1,dims="A1",person_id=pid,comment=gsub("<","&lt;",'Five is < Twenty'))

wb <- wb_add_thread(wb,sheet=1,dims="A1",person_id=pid,comment='Reply 1',reply=T)
wb <- wb_add_thread(wb,sheet=1,dims="A1",person_id=pid,comment='Reply 2',reply=T)
wb <- wb_add_thread(wb,sheet=1,dims="A1",person_id=pid,comment='Reply 3',reply=T)
wb <- wb_add_thread(wb,sheet=1,dims="A1",person_id=pid,comment='Reply 4',reply=T)
wb <- wb_add_thread(wb,sheet=1,dims="A1",person_id=pid,comment='Reply 5',reply=T)
wb <- wb_add_thread(wb,sheet=1,dims="A1",person_id=pid,comment='Reply 6',reply=T)
wb <- wb_add_thread(wb,sheet=1,dims="A1",person_id=pid,comment='Reply 7',reply=T)
wb <- wb_add_thread(wb,sheet=1,dims="A1",person_id=pid,comment='Reply 8',reply=T)

wb_get_thread(wb,1,"A1")
wb_save(wb,file="C:/Temp/example.xlsx",overwrite=T)


wb <- openxlsx2::wb_load(template_file)


wb <- wb_add_person(wb,name="John Doe")
pid <- wb$get_person("John Doe")$id;

for (f in 1:nrow(summary)) {
  flag <- summary[f]
  #if (flag$ref=="E7") next;
  #if (f>8) break;
  #if (f>10) break;
  #if (flag$ref_n>1) next;
  #cmt <- paste0(toupper(flag$check_class),":",flag$check_name,"\n",flag$check_message,"\n\n[ID:",flag$evaluation_id,"]")
  cmt <- paste0("comment ",flag$evaluation_id)
  c_reply <- tryCatch({ gc <- wb$get_comment(sheet=sheet,dims=flag$ref); !(length(gc)==0) },error=function(e) { FALSE })
  
  
  #wb$threadComments
    wb$add_thread(sheet=sheet,
                   dims=flag$ref,
                   person_id=pid,
                   reply=c_reply, 
                   #resolve=ifelse(flag$check_status=="active",FALSE,TRUE),
                   resolve=FALSE,
                   comment=cmt)
}

wb$save(file="C:/Temp/example.xlsx",overwrite=T)

debugonce(wb$save)
wb$save(file="C:/Temp/example.xlsx",overwrite=T)



excelwb <- openxlsx2::wb_load(template_file)

wb <- openxlsx2::wb_load("C:/Temp/Example_legacynote.xlsx")
comments <- wb$get_comment(sheet=1)
for (i in 1:nrow(comments)) {
  wb$remove_comment(sheet=1,dims=comments[i,"ref"])
}
wb$save("C:/Temp/test.xlsx",overwrite=T)

excelwb <- openxlsx2::wb_load("C:/Temp/Example_legacynote.xlsx")

if (length(excelwb$comments)) {

comments <- rbindlist(lapply(seq_along(excelwb$sheet_names),function(s) { x <- excelwb$get_comment(sheet=s); if (length(x)) cbind(x[,c("ref","cmmt_id")],sheet=s) }))
if (!empty(comments)) { invisible(pmap(comments[,.(sheet,dims=ref)],excelwb$remove_comment)) }

}

for (i in seq_along(excelwb$vml_rels)) {
  if (nchar(excelwb$vml_rels[i])==0) excelwb$vml_rels[[i]] <- NULL
}
excelwb$worksheets_rels[[1]]

excelwb$save("C:/Temp/nocomments.xlsx",overwrite=T)

debugonce(excelwb$save)
debugonce(excelwb$remove_comment)

excelwb$save("C:/Temp/test.xlsx",overwrite=T)
