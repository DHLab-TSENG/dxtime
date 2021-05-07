plotSurv <- function(DataFile,ID,ICD,date,DataFile_personal,label,var,icdcount=2){
  dataCol <- c(deparse(substitute(ID)),deparse(substitute(ICD)),deparse(substitute(date)))
  DataFile <- DataFile[,dataCol,with = FALSE]
  dataCol <- c(deparse(substitute(ID)),deparse(substitute(label)))
  DataFile_personal <- DataFile_personal[,dataCol,with = FALSE]
  DataFile <- merge(DataFile,DataFile_personal,all.x = T)
  cutdata <- cutWindow(DataFile, ID, ICD, date, predictwindow=0 , icdcount=icdcount)


  COXccs_summary <- list()

  ##取該window的數據
  cutdata <- cutdata[,-c("window_N")]
  tranName <- data.table(oriname=names(cutdata[,3:ncol(cutdata)]))
  tranName <- merge(tranName,unique(ccstable[,.(CCS_CATEGORY,CCS_CATEGORY_DESCRIPTION)]),by.x = "oriname",by.y = "CCS_CATEGORY",all.x = TRUE)
  setnames(cutdata,names(cutdata[,3:ncol(cutdata)]),tranName$CCS_CATEGORY_DESCRIPTION)
  DataFile <- DataFile[, c("predfirstdate", "indexdate") := list(min(date), max(date)), by=ID]
  ##原始資料取futime數據
  DataFile_case <- DataFile[,c("ID","futat","futime") := list(ID,label,indexdate-predfirstdate),][,c("ID","futat","futime"),]
  DataFile_case <- unique(DataFile_case)
  DataFile_case <- merge(DataFile_case,cutdata,by="ID",all.x=T)
  ##決定以mul/single進行特徵選取
  string2 <- paste0("`",var,"`")
  f1 <- as.formula(paste("Surv(DataFile_case$futime, DataFile_case$futat) ~ DataFile_case$", string2))
  surv <- survfit(f1, data=DataFile_case)
  return(ggsurv(surv))

}
