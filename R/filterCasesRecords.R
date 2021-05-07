filterCasesRecords <- function(DataFile,ID,ICD,date,predictWindow,folloWindow,followedDataLength_limit = 0,align="right",countICD_limit = 0){

  DataFile <- as.data.table(DataFile)
  dataCol <- c(deparse(substitute(ID)),deparse(substitute(ICD)),deparse(substitute(date)))
  DataFile <- DataFile[, dataCol,with = FALSE]
  DataFile <- DataFile[, c("firstDate", "indexDate") := list(min(date), max(date)), by=ID]
  DataFile <- as.data.table(DataFile)
  #
  # if(is.null(folloWindow_limit)){folloWindow_limit <- 0}
  # if(is.null(countICD_limit)){countICD_limit <- 0}
  #資料長度
  DataFile[,recordperiod := unclass(indexDate-firstDate),]
  DataFile[,predictDate := indexDate-predictWindow,]
  DataFile[,followedDataLength := unclass(predictDate-firstDate),]
  DataFile[,predfirstdate := predictDate - folloWindow,]
  ###########Step1 資料長度至少五年，且五年內資料至少兩筆
  ###CAD
  DataFile_pat <- DataFile[,countICD:= .N,by=ID][followedDataLength >= followedDataLength_limit & countICD >= countICD_limit, cri := T]
  DataFile_pat[is.na(DataFile_pat$cri),c("cri")] <- F
  DataFile_pat <- unique(DataFile_pat[,c("ID","firstDate","predictDate","indexDate","followedDataLength","countICD","cri")])#符合條件者cri=T
  DataFile_pat <- as.data.table(DataFile_pat)
  DataFile <- DataFile[DataFile$ID %in% DataFile_pat[cri==T,]$ID,]
  setDT(DataFile)
  if(align=="left"){
    DataFile_rec <- DataFile[date <= firstDate + folloWindow & date <= predictDate,c("ID","ICD","date")]
  }else if(align=="right"){
    DataFile[,predfirstdate := predictDate - folloWindow,]
    DataFile_rec <- DataFile[date <= predictDate & date >= predfirstdate,c("ID","ICD","date")]##如何實作%in%

  }
  DataFile_pat$followedDataLength <- (as.numeric(DataFile_pat$followedDataLength))
  return(list(Patient = DataFile_pat, Record=DataFile_rec))
}
