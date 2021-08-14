matchCases <- function(DataFile,idColName,labelColName,matchedVariableColName = c(),method = "pscore",ratio=5L){
  DataFile <- as.data.table(DataFile)

  if(method=="pscore"){
    dataCol <- c(deparse(substitute(idColName)),deparse(substitute(labelColName)),matchedVariableColName)
    #dataCol <- c(deparse(substitute(ID)),deparse(substitute(label)),matchedVariableColName)

    DataFile <- DataFile[, dataCol,with = FALSE]
    names(DataFile) <- c("ID","label",matchedVariableColName)
    paste2 <- function(x, y, sep = "+") paste(x, y, sep = sep)
    matchedvector <- reduce(matchedVariableColName,paste2)
    str <- paste0(deparse(substitute(label)),"~",matchedvector)
    str <- as.formula(str)
    match <- matchit(str, data = DataFile, method="nearest",ratio=ratio,distance ="glm")#####
    m.data <- match.data(match,distance ="pscore")
    m.data <- m.data[,-c("weights"),]
    return(match = m.data)
  }else if(method=="direct"){
    dataCol <- c(deparse(substitute(idColName)),deparse(substitute(labelColName)),matchedVariableColName)
    DataFile <- DataFile[, dataCol,with = FALSE]
    names(DataFile) <- c("ID","label",matchedVariableColName)

    patformatch <- DataFile[,dataCol,with = FALSE]
    class <- unique(patformatch[,matchedVariableColName,with = FALSE])
    class[,subclass := c(1:nrow(class))]
    patformatch <- merge(patformatch,class,all.x = T,by = matchedVariableColName)
    patformatch[,n:= .N,by=c("label","subclass")]
    class_case <- unique(patformatch[label==1,.(label,subclass,n),])
    class_case[,matchcontrol_n := n*ratio]
    class_control <- unique(patformatch[label==0,.(label,subclass,n),])
    class_control <- merge(class_control,class_case[,.(subclass,matchcontrol_n)],all.x=T,by="subclass")
    class_control[is.na(matchcontrol_n),"matchcontrol_n"] <- 0

    controlBeChoosed <- NULL
    for(i in 1:nrow(class_control)){
      if(class_control[i]$n < class_control[i]$matchcontrol_n){
        controlN <- c(1:class_control[i]$n)
      }else{
        set.seed(5)
        controlN <- sample(1:class_control[i]$n,class_control[i]$matchcontrol_n)
      }
      control <- patformatch[label==0 & subclass==class_control[i]$subclass][controlN]
      controlBeChoosed <- rbind(controlBeChoosed,control)
    }
    patmatchresult <- rbind(controlBeChoosed,patformatch[label==1])
    patmatchresult <- patmatchresult[,c(dataCol,"subclass"),with = FALSE]
    return(match = patmatchresult)
  }

}
