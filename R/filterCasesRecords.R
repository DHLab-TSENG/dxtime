filterCasesRecords <- function(DataFile,ID,ICD,date,align="right",predictwindow,followindow,followindow_limit = NULL,count_limit = NULL){

  DataFile <- as.data.table(DataFile)
  dataCol <- c(deparse(substitute(ID)),deparse(substitute(ICD)),deparse(substitute(date)))
  DataFile <- DataFile[, dataCol,with = FALSE]
  DataFile <- DataFile[, c("firstdate", "indexdate") := list(min(date), max(date)), by=ID]
  DataFile <- as.data.table(DataFile)
  #
  if(is.null(followindow_limit)){followindow_limit <- 0}
  if(is.null(count_limit)){count_limit <- 0}
  #資料長度
  DataFile[,recordperiod := unclass(indexdate-firstdate),]
  DataFile[,predictdate := indexdate-predictwindow,]
  DataFile[,followperiod := unclass(predictdate-firstdate),]

  ###########Step1 資料長度至少五年，且五年內資料至少兩筆
  ###CAD
  if(align=="left"){
    DataFile_rec <- DataFile[unclass(indexdate-firstdate)+1 > length & unclass(date-firstdate) <= length , n := .N, by=ID][n > count_limit, cri := T]
  }else if(align=="right"){
    DataFile_pat <- DataFile[,n:= .N,by=ID][followperiod >= followindow_limit & n >= count_limit, cri := T]
    DataFile_pat[is.na(DataFile_pat$cri)] <- F
    DataFile_pat <- unique(DataFile_pat[,c("ID","firstdate","indexdate","followperiod","n","cri")])#符合條件者cri=T
  }
  DataFile_pat <- as.data.table(DataFile_pat)
  DataFile <- DataFile[DataFile$ID %in% DataFile_pat[cri==T,]$ID,]
  setDT(DataFile)
  DataFile[,predfirstdate := predictdate - followindow,]
  DataFile_rec <- DataFile[date <= predictdate & date >= predfirstdate,c("ID","ICD","date")]##如何實作%in%
  return(list(DataFile_pat = DataFile_pat, DataFile_rec=DataFile_rec))
}
