cutWindow <- function(DataFile,idColName,icdColName,dateColName,birthdayColName=NULL,binaryAge=F,ageLayer = 45,ifgroup = TRUE,countICD_toCCS=2,predictGap,window=NULL,N=NULL){

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
  if(deparse(substitute(birthdayColName)) != "NULL"){
    dataCol <- c(deparse(substitute(idColName)),deparse(substitute(icdColName)),deparse(substitute(dateColName)),deparse(substitute(birthdayColName)))
    DataFile <- DataFile[,dataCol,with=FALSE]
    names(DataFile) <- c("ID","ICD","date","birthday")
  }else{
    dataCol <- c(deparse(substitute(idColName)),deparse(substitute(icdColName)),deparse(substitute(dateColName)))
    #dataCol <- c(deparse(substitute(ID)),deparse(substitute(ICD)),deparse(substitute(date)))
    DataFile <- DataFile[,dataCol,with=FALSE]
    names(DataFile) <- c("ID","ICD","date")
  }
  #DataFile <- DataFile[,dataCol,with=FALSE]
  DataFile <- DataFile[,indexdate := max(date),by=ID][,predfirstdate := min(date),by=ID][,predictdate := indexdate-predictGap,]
  #直接長表轉寬表
  icdtable <- paste0("ICD_",unique(DataFile$ICD))
  ICDWide_summary <- data.table()
  if(ifgroup == FALSE){
    for(i in 1:N){
      DataFile_window <- DataFile[predictdate-window*i < date & date <= predictdate-window*(i-1),]
      DataFile_followindow <- DataFile_window
      DataFile_followindow$values <- 1
      DataFile_followindow$ICD <- paste0("ICD_",DataFile_followindow$ICD)
      DataFile_followindow <- unique(DataFile_followindow[,.(ID,ICD,values)])
      ICDWide <- spread(DataFile_followindow,key=ICD,value=values)
      ICDWide <- cbind(ICDWide[,1],ICDWide[,lapply(.SD,function(x){ifelse(is.na(x),0L,1L)}),.SDcols=2:ncol(ICDWide)])
      #othercode
      othericd <- icdtable[!icdtable %in% colnames(ICDWide)]
      ICDWide[,c(othericd):=NA]

      #otherpt
      Otherpt <- c(unique(DataFile[! DataFile$ID %in% ICDWide$ID,.(ID)]))
      OtherPt_table <- data.table(matrix(nrow=lengths(Otherpt),ncol=1+length(icdtable)))
      names(OtherPt_table) <- c(c("ID"),icdtable)
      OtherPt_table$ID <- Otherpt
      ICDWide <- rbind(OtherPt_table,ICDWide)
      ICDWide[is.na(ICDWide)] <- 0L
      #加上window次序
      window_N <- i
      ICDWide <- cbind(window_N, ICDWide)
      #依照ID排序
      ICDWide <- ICDWide[order(ID)]
      #計算periodAge
      if(deparse(substitute(birthdayColName)) != "NULL"){
        age <- unique(DataFile[,c("ID","indexdate","birthday")])
        periodAge <- round(age[,((indexdate-predictGap-i*window)-birthday)/365.25,],digits = 2)
        ICDWide <- cbind(periodAge,ICDWide)
      }
      #合併
      ICDWide_summary <- rbind(ICDWide_summary,ICDWide)
    }
    if(deparse(substitute(birthdayColName)) != "NULL"){
      ICDWide_summary$periodAge <- as.character(ICDWide_summary$periodAge)
      if(binaryAge){
        ICDWide_summary[,periodAge:=lapply(.SD,function(x)ifelse(x>ageLayer,1,0)),.SDcols="periodAge"]
      }
    }else{
      ICDWide_summary$periodAge <- NULL
      ICDWide_summary$periodAge <- NULL
    }

    #CCSWide_summary <- rbind(CCSWide_summary,CCSWide_followindow)
    if(deparse(substitute(birthdayColName)) != "NULL"){
      ICDWide_summary <- cbind(ICDWide_summary[,c("window_N")],ICDWide_summary[,-c("window_N")])
    }
    ICDWide_summary$window_N <- as.character(ICDWide_summary$window_N)
    ICDWide_summary[window_N==0,window_N:="all"]
    return(ICDWide_summary)

  }else{
  #group為CCS
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
                                            icd10usingDate = "2016-01-01")

          CCSWide_followindow <- groupedDataLongToWide(dxDataFile = CCSLong_followindow$groupedDT,
                                                       idColName = ID,
                                                       categoryColName = CCS_CATEGORY,
                                                       dateColName = Date,
                                                       numericOrBinary = B,
                                                       count = countICD_toCCS)
          names(CCSWide_followindow)[2:ncol(CCSWide_followindow)] <- paste0("ccs_",names(CCSWide_followindow)[2:ncol(CCSWide_followindow)])

          n <- as.numeric(length(CCSWide_followindow))
          CCSWide_followindow <- as.data.table(CCSWide_followindow)[, c(2:n) := lapply(.SD, function(x) ifelse(x == FALSE, 0L, 1L)), .SDcols = 2:n]
          #othercode
          otherccs <- unique(ccstable$CCS_CATEGORY)[!unique(ccstable$CCS_CATEGORY) %in% colnames(CCSWide_followindow)]
          CCSWide_followindow[,c(otherccs):=NA]
          #CCSWide_followindow <-as.logical(CCSWide_followindow[,c(2:ncol(CCSWide_followindow)),])
          #otherpt
          Otherpt_followindow <- c(unique(DataFile[! DataFile$ID %in% CCSWide_followindow$ID,.(ID)]))
          OtherPt_table_followindow <- data.table(matrix(nrow=lengths(Otherpt_followindow),ncol=284))
          names(OtherPt_table_followindow) <- c(c("ID"),c(unique(ccstable$CCS_CATEGORY)))
          OtherPt_table_followindow$ID <- Otherpt_followindow
          CCSWide_followindow <- rbind(OtherPt_table_followindow,CCSWide_followindow)
          CCSWide_followindow[is.na(CCSWide_followindow)] <- 0L
          if(is.null(window)){
            window_N<- 1
            CCSWide_followindow <- cbind(window_N, CCSWide_followindow)  ##如果window為NA，代表沒有要切分window，總表直接為T=1

            if(deparse(substitute(birthdayColName)) != "NULL"){
              periodAge <- NA
              CCSWide_followindow <- cbind(periodAge,CCSWide_followindow)
              if(binaryAge){
                CCSWide_followindow[,periodAge:=lapply(.SD,function(x)ifelse(x>ageLayer,1,0)),.SDcols="periodAge"]
              }
            }
            #CCSWide_followindow$window_N <- as.character(CCSWide_followindow$window_N)
            #CCSWide_followindow[window_N==0,window_N:="all"]
            return(CCSWide_followindow)
          }else{
            window_N <- 0
            CCSWide_followindow <- cbind(window_N, CCSWide_followindow)  ##如果window不為NA，代表要切分window，此總表設定為T=0
            if(deparse(substitute(birthdayColName)) != "NULL"){
               periodAge <- NA
              CCSWide_followindow <- cbind(periodAge,CCSWide_followindow)
              if(binaryAge){
                CCSWide_followindow[,periodAge:=lapply(.SD,function(x)ifelse(x>ageLayer,1,0)),.SDcols="periodAge"]
              }
            }else{
              periodAge <- NA
              CCSWide_followindow <- cbind(periodAge,CCSWide_followindow)
            }

          }
        }
      }
      #各window切分
      if(!is.null(window)){
        DataFile_window <- DataFile[predictdate-window*i < date & date <= predictdate-window*(i-1),]
        if(nrow(DataFile_window) > 0){
          CCSLong <- icdDxToCCS(dxDataFile = DataFile_window,
                                idColName = ID,
                                icdColName = ICD,
                                dateColName = date,
                                isDescription = FALSE,
                                icd10usingDate = "2016-01-01")

          CCSWide <- groupedDataLongToWide(dxDataFile = CCSLong$groupedDT,
                                           idColName = ID,
                                           categoryColName = CCS_CATEGORY,
                                           dateColName = Date,
                                           numericOrBinary = B,
                                           count = 1)
          names(CCSWide)[2:ncol(CCSWide)] <- paste0("ccs_",names(CCSWide)[2:ncol(CCSWide)])


          n <- as.numeric(length(CCSWide))
          CCSWide <- as.data.table(CCSWide)[, c(2:n) := lapply(.SD, function(x) ifelse(x == FALSE, 0L, 1L)), .SDcols = 2:n]
          #othercode
          otherccs <- unique(ccstable$CCS_CATEGORY)[!unique(ccstable$CCS_CATEGORY) %in% colnames(CCSWide)]
          CCSWide[,c(otherccs):=NA]
          #otherpt
          Otherpt <- c(unique(DataFile[! DataFile$ID %in% CCSWide$ID,.(ID)]))
          OtherPt_table <- data.table(matrix(nrow=lengths(Otherpt),ncol=284))
          names(OtherPt_table) <- c(c("ID"),unique(c(ccstable$CCS_CATEGORY)))
          OtherPt_table$ID <- Otherpt
          CCSWide <- rbind(OtherPt_table,CCSWide)
          CCSWide[is.na(CCSWide)] <- 0L
        }else{
          warning(paste('window',N,'is empty'))
          Otherpt <- c(unique(DataFile[! DataFile$ID %in% CCSWide$ID,.(ID)]))
          OtherPt_table <- data.table(matrix(nrow=lengths(Otherpt),ncol=284))
          names(OtherPt_table) <- c(c("ID"),c(ccstable$CCS_CATEGORY %>% unique()))
          OtherPt_table$ID <- Otherpt
          CCSWide <- rbind(CCSWide,OtherPt_table)
          CCSWide[is.na(CCSWide)] <- 0L
        }
        window_N <- i
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
        if(deparse(substitute(birthdayColName)) != "NULL"){
          age <- unique(DataFile[,c("ID","indexdate","birthday")])
          periodAge <- round(age[,((indexdate-predictGap-i*window)-birthday)/365.25,],digits = 2)
          CCSWide_test <- cbind(periodAge,CCSWide_test)
        }
        #合併
        CCSWide_summary <- rbind(CCSWide_summary,CCSWide_test)
      }
    }
    if(deparse(substitute(birthdayColName)) != "NULL"){
      CCSWide_summary$periodAge <- as.character(CCSWide_summary$periodAge)
      if(binaryAge){
        CCSWide_summary[,periodAge:=lapply(.SD,function(x)ifelse(x>ageLayer,1,0)),.SDcols="periodAge"]
      }
    }else{
      CCSWide_summary$periodAge <- NULL
      CCSWide_followindow$periodAge <- NULL
    }

    CCSWide_summary <- rbind(CCSWide_summary,CCSWide_followindow)
    if(deparse(substitute(birthdayColName)) != "NULL"){
      CCSWide_summary <- cbind(CCSWide_summary[,c("window_N")],CCSWide_summary[,-c("window_N")])

    }
    CCSWide_summary$window_N <- as.character(CCSWide_summary$window_N)
    CCSWide_summary[window_N==0,window_N:="all"]
    return(CCSWide_summary)
  }



}
