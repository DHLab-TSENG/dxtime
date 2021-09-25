plotSurv <- function(DataFile_cutData,DataFile_personal,idColName,labelColName,dataLengthColName,ccsDescription){
  dataCol <- c(deparse(substitute(idColName)),deparse(substitute(labelColName)),deparse(substitute(dataLengthColName)))
  DataFile_personal <- DataFile_personal[,dataCol,with = FALSE]
  names(DataFile_personal) <- c("ID","label","dataLength")
  DataFile_personal$dataLength <- as.numeric(DataFile_personal$dataLength)

  #取該window的數據
  cutdata <- DataFile_cutData[,(ncol(DataFile_cutData)-283):ncol(DataFile_cutData)]
  setnames(cutdata,names(cutdata)[2:ncol(cutdata)],ccstable$CCS_CATEGORY_DESCRIPTION)
  ##原始資料取futime數據
  personal <- DataFile_personal[,c("ID","label","dataLength"),]
  personal <- unique(personal)
  personal <- merge(personal,cutdata,by="ID",all.x=T)
  string2 <- paste0("`",ccsDescription,"`")
  f1 <- as.formula(paste("Surv(dataLength,label) ~ personal$", string2))
  surv <- survfit(f1, data=personal)
  P <- ggsurv(surv) +
    ggplot2::guides(linetype = FALSE) +
    ggplot2::scale_colour_discrete(
    name   = ccsDescription,
    labels = c('F', 'T')
  )
  return(P)

}
