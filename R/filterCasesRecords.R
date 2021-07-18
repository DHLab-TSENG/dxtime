filterCasesRecords <- function(DataFile,idColName,icdColName,dateColName,predictGap,exposurePeriod,includePeriodAtLeast = 0,align="right",countICDAtLeast = 0){

  DataFile <- as.data.table(DataFile)
  dataCol <- c(deparse(substitute(idColName)),deparse(substitute(icdColName)),deparse(substitute(dateColName)))
  DataFile <- DataFile[, dataCol,with = FALSE]
  names(DataFile) <- c("ID","ICD","date")
  DataFile <- DataFile[, c("firstDate", "indexDate") := list(min(date), max(date)), by=ID]
  DataFile <- as.data.table(DataFile)
  #
  # if(is.null(folloWindow_limit)){folloWindow_limit <- 0}
  # if(is.null(countICDAtLeast)){countICDAtLeast <- 0}
  #資料長度
  DataFile[,recordperiod := unclass(indexDate-firstDate),]
  DataFile[,predictDate := indexDate-predictGap,]
  DataFile[,includePeriod := unclass(predictDate-firstDate),]
  DataFile[,predfirstdate := predictDate - exposurePeriod,]
  ###########Step1 資料長度至少五年，且五年內資料至少兩筆
  ###CAD
  DataFile_pat <- DataFile[,countICD:= .N,by=ID][includePeriod >= includePeriodAtLeast & countICD >= countICDAtLeast, cri := T]
  DataFile_pat[is.na(DataFile_pat$cri),c("cri")] <- F
  DataFile_pat <- unique(DataFile_pat[,c("ID","firstDate","predictDate","indexDate","includePeriod","countICD","cri")])#符合條件者cri=T
  DataFile_pat <- as.data.table(DataFile_pat)
  DataFile <- DataFile[DataFile$ID %in% DataFile_pat[cri==T,]$ID,]
  setDT(DataFile)
  if(align=="left"){
    DataFile_rec <- DataFile[date <= firstDate + exposurePeriod & date <= predictDate,c("ID","ICD","date")]
  }else if(align=="right"){
    DataFile[,predfirstdate := predictDate - exposurePeriod,]
    DataFile_rec <- DataFile[date <= predictDate & date >= predfirstdate,c("ID","ICD","date")]##如何實作%in%

  }
  DataFile_pat$includePeriod <- (as.numeric(DataFile_pat$includePeriod))
  return(list(Patient = DataFile_pat, Record=DataFile_rec))
}
