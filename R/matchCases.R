matchCases <- function(DataFile,ID,label,matchedVariable = c()){

  DataFile <- as.data.table(DataFile)
  dataCol <- c(deparse(substitute(ID)),deparse(substitute(label)),matchedVariable)
  DataFile <- DataFile[, dataCol,with = FALSE]
  paste2 <- function(x, y, sep = "+") paste(x, y, sep = sep)
  matchedvector <- reduce(matchedVariable,paste2)
  str <- paste0(deparse(substitute(label)),"~",matchedvector)
  str <- as.formula(str)
  match <- matchit(str, data = DataFile, method="nearest",ratio=5,distance ="logit")#####
  m.data <- match.data(match,distance ="pscore")
  m.data <- m.data[,-c("weights","subclass"),]
  return(match = m.data)
}
