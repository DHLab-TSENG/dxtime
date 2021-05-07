cutWindow <- function(DataFile,ID,ICD,date,getPeriodage=F,binaryAge=F,ageLayer = 45,predictWindow,window=NULL,N=NULL,countICD_toccs=2,BIRTHDAY=NULL){

  library(dxpr)
   #####建function，ccswide_summary和ccswide_followindow
  rep <- function(x,y){
    if(y==0){
      x <- 0
    }else{}
    return(x)
  }
  ###
  if(is.null(N)){N <- 1}
  ###
  DataFile <- as.data.table(DataFile)
  if(getPeriodage){
    dataCol <- c(deparse(substitute(ID)),deparse(substitute(ICD)),deparse(substitute(date)),deparse(substitute(BIRTHDAY)))
  }else{
    dataCol <- c(deparse(substitute(ID)),deparse(substitute(ICD)),deparse(substitute(date)))
  }
  DataFile <- DataFile[,dataCol,with=FALSE]
  DataFile <- DataFile[,indexdate := max(date),by=ID][,predfirstdate := min(date),by=ID][,predictdate := indexdate-predictWindow,]


  #判斷單個window，該病患有無得病
  CCSWide_summary <- data.table()
  for(i in 1:N){
    if(i==1){
      #判斷整個followwindow中，該病患有無得病
      DataFile_followindow <- DataFile[predfirstdate <= date & date <= predictdate,]
      if(nrow(DataFile_followindow) > 0){

        CCSLong_followindow <- icdDxToCCS(dxDataFile = DataFile_followindow,
                                          idColName = ID,
                                          icdColName = ICD,
                                          dateColName = date,
                                          isDescription = FALSE,
                                          icd10usingDate = "1996-01-01")

        CCSWide_followindow <- groupedDataLongToWide(dxDataFile = CCSLong_followindow$groupedDT,
                                                     idColName = ID,
                                                     categoryColName = CCS_CATEGORY,
                                                     dateColName = Date,
                                                     numericOrBinary = B,
                                                     count = countICD_toccs)


        n <- as.numeric(length(CCSWide_followindow))
        CCSWide_followindow <- as.data.table(CCSWide_followindow)[, c(2:n) := lapply(.SD, function(x) ifelse(x == FALSE, 0L, 1L)), .SDcols = 2:n]
        #othercode
        otherccs <- unique(ccsDxICD9$CCS_CATEGORY)[!unique(ccsDxICD9$CCS_CATEGORY) %in% colnames(CCSWide_followindow)]
        CCSWide_followindow[,c(otherccs):=NA]
        #otherpt
        Otherpt_followindow <- c(unique(DataFile[! DataFile$ID %in% CCSWide_followindow$ID,.(ID)]))
        OtherPt_table_followindow <- data.table(matrix(nrow=lengths(Otherpt_followindow),ncol=284))
        names(OtherPt_table_followindow) <- c(c("ID"),c(unique(ccsDxICD9$CCS_CATEGORY)))
        OtherPt_table_followindow$ID <- Otherpt_followindow
        CCSWide_followindow <- rbind(OtherPt_table_followindow,CCSWide_followindow)
        CCSWide_followindow[is.na(CCSWide_followindow)] <- 0L
        if(is.null(window)){
          window_N<- 1
          CCSWide_followindow <- cbind(window_N, CCSWide_followindow)  ##如果window為NA，代表沒有要切分window，總表直接為T=1
          #計算indexage
          if(getPeriodage){
            age <- DataFile[,c("ID","indexdate","BIRTHDAY"),] %>% unique()
            periodAge <- round(age[,((indexdate-predictWindow-i*window)-BIRTHDAY)/365.25,],digits = 2)
            CCSWide_followindow <- cbind(periodAge,CCSWide_followindow)
            if(binaryAge){
              CCSWide_followindow[,periodAge:=lapply(.SD,function(x)ifelse(x>ageLayer,1,0)),.SDcols="periodAge"]
            }
          }
          return(CCSWide_followindow)
        }else{
          window_N<- 0
          CCSWide_followindow <- cbind(window_N, CCSWide_followindow)  ##如果window不為NA，代表要切分window，此總表設定為T=0
          periodAge <- NA
          CCSWide_followindow <- cbind(periodAge,CCSWide_followindow)
        }
      }
    }
    if(!is.null(window)){
      DataFile_window <- DataFile[predictdate-window*i <= date & date <= predictdate-window*(i-1),]
      if(nrow(DataFile_window) > 0){
        CCSLong <- icdDxToCCS(dxDataFile = DataFile_window,
                              idColName = ID,
                              icdColName = ICD,
                              dateColName = date,
                              isDescription = FALSE,
                              icd10usingDate = "1996-01-01")

        CCSWide <- groupedDataLongToWide(dxDataFile = CCSLong$groupedDT,
                                         idColName = ID,
                                         categoryColName = CCS_CATEGORY,
                                         dateColName = Date,
                                         numericOrBinary = B,
                                         count = 1)


        n <- as.numeric(length(CCSWide))
        CCSWide <- as.data.table(CCSWide)[, c(2:n) := lapply(.SD, function(x) ifelse(x == FALSE, 0L, 1L)), .SDcols = 2:n]
        #othercode
        otherccs <- unique(ccsDxICD9$CCS_CATEGORY)[!unique(ccsDxICD9$CCS_CATEGORY) %in% colnames(CCSWide)]
        CCSWide[,c(otherccs):=NA]
        #otherpt
        Otherpt <- c(unique(DataFile[! DataFile$ID %in% CCSWide$ID,.(ID)]))
        OtherPt_table <- data.table(matrix(nrow=lengths(Otherpt),ncol=284))
        names(OtherPt_table) <- c(c("ID"),unique(c(ccsDxICD9$CCS_CATEGORY)))
        OtherPt_table$ID <- Otherpt
        CCSWide <- rbind(OtherPt_table,CCSWide)
        CCSWide[is.na(CCSWide)] <- 0L
      }else{
        warning(paste('window',N,'is empty'))
        Otherpt <- c(unique(DataFile[! DataFile$ID %in% CCSWide$ID,.(ID)]))
        OtherPt_table <- data.table(matrix(nrow=lengths(Otherpt),ncol=284))
        names(OtherPt_table) <- c(c("ID"),c(ccsDxICD9$CCS_CATEGORY %>% unique()))
        OtherPt_table$ID <- Otherpt
        CCSWide <- rbind(CCSWide,OtherPt_table)
        CCSWide[is.na(CCSWide)] <- 0L
      }
      window_N<- i
      CCSWide <- cbind(window_N, CCSWide)
      #依照ID排序
      CCSWide <- CCSWide[order(ID)]
      CCSWide_followindow <- CCSWide_followindow[order(ID)]
      #如果在predwindow中沒生病，在此Window中也不應該生病
      ccslist <-
        map2_dbl(unlist(CCSWide[,3:ncol(CCSWide)]),
                 unlist(CCSWide_followindow[,4:ncol(CCSWide_followindow)]),
                 rep)
      CCSWide_test <- CCSWide
      CCSWide_test[,3:ncol(CCSWide)] <- data.table(matrix(ccslist,nrow(CCSWide)))
      #計算periodAge
      if(getPeriodage){
        age <- unique(DataFile[,c("ID","indexdate","BIRTHDAY")])
        periodAge <- round(age[,((indexdate-predictWindow-i*window)-BIRTHDAY)/365.25,],digits = 2)
        CCSWide_test <- cbind(periodAge,CCSWide_test)
      }
      #合併
      CCSWide_summary <- rbind(CCSWide_summary,CCSWide_test)
    }
  }
  if(getPeriodage){
    CCSWide_summary$periodAge <- as.character(CCSWide_summary$periodAge)
    if(binaryAge){
      CCSWide_summary[,periodAge:=lapply(.SD,function(x)ifelse(x>ageLayer,1,0)),.SDcols="periodAge"]
    }
  }else{
    CCSWide_summary$periodAge <- NULL
    CCSWide_followindow$periodAge <- NULL
  }

  CCSWide_summary <- rbind(CCSWide_summary,CCSWide_followindow)
  if(! is.null(CCSWide_summary$periodAge)){
    CCSWide_summary <- cbind(CCSWide_summary[,c("window_N")],CCSWide_summary[,-c("window_N")])

  }
  return(CCSWide_summary)
}
