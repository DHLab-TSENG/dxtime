analWindow_Cox <- function(DataFile_cutData,DataFile_feature,DataFile_personal,idColName,labelColName,dataLengthColName,predictorColName=NULL,isDescription=F,testN=100,method="weighting"){
  #DataFile_cutData <- DataFile_cutData[window_N!="all"]

  DataFile_cutData$window_N <- as.integer(DataFile_cutData$window_N)
  if(is.na(DataFile_cutData$window_N)){
    DataFile_cutData$window_N <- 0
    N=1
  }else{
    N <- max(DataFile_cutData$window_N)
  }


  if(length(DataFile_feature) != 1 & length(DataFile_feature) != max(DataFile_cutData$window_N)){
    stop("error feature length")
  }
  #N <- max(DataFile_cutData$window_N)
  cutdata <- as.data.table(DataFile_cutData)
  ccscategory <- unique(c(ccstable$CCS_CATEGORY))

  cutdata_dataCol <- c(deparse(substitute(window_N)),deparse(substitute(ID)),deparse(substitute(periodAge)),ccscategory)
  cutdata <- cutdata[, cutdata_dataCol,with=FALSE]
  names(cutdata) <- c("window_N","ID","periodAge",ccscategory)


  test_timeseriesauc_summary <- c()
  test_windowauc_summary <- NULL
  #如果用同樣的feature跳message
  if(length(DataFile_feature)==1){
    message("same features in all windows")
  }

  #
  DataFile_personal <- as.data.table(DataFile_personal)
  personal_dataCol <- c(deparse(substitute(idColName)),deparse(substitute(labelColName)),deparse(substitute(dataLengthColName)),predictorColName)
  #personal_dataCol <- c(deparse(substitute(ID)),deparse(substitute(label)),deparse(substitute(dataLength)),predictorColName)
  DataFile_personal <- DataFile_personal[, personal_dataCol, with=FALSE]
  names(DataFile_personal) <- c("ID","label","dataLength",predictorColName)

  DataFile_personal$futat <- DataFile_personal$label
  DataFile_personal$futat <- as.numeric(DataFile_personal$futat)
  DataFile_personal$dataLength <- as.numeric(DataFile_personal$dataLength)

  set.seed(10)
  for(j in 1:testN){
    #切test train
    test_indices  <- sample(1:length(unique(cutdata$ID)),length(unique(cutdata$ID))/5)

    #coxph
    model <- list()
    COXccs <- list()
    testdata <- list()
    traindata <- list()

    for(i in 1:N){
      if(length(DataFile_feature)==1){
        COXccs_top <- DataFile_feature[[1]][selected==1, "CCS_CATEGORY"]
      }else if(length(DataFile_feature)==N){
        COXccs_top <- DataFile_feature[[i]][selected==1, "CCS_CATEGORY"]
      }
      setDT(COXccs_top)
      if(N==1){
        tcutdata_test <- cutdata[test_indices]
        tcutdata_train <- cutdata[-test_indices]
      }else{
        tcutdata_test <- cutdata[window_N==i][test_indices]
        tcutdata_train <- cutdata[window_N==i][-test_indices]
      }
      string <- paste(as.character(COXccs_top$CCS_CATEGORY), collapse = "+")
      paste2 <- function(x, y, sep = "+") paste(x, y, sep = sep)
      if(string == ""){
        string2 <- "`SEX`+`periodAge`"
      }else{
        if(length(grep("periodAge",names(DataFile_cutData)))>0){
          if(! is.null(predictorColName)){
            predictorvector <- reduce(predictorColName,paste2)
            predictorvector <- paste0(predictorvector,"+`periodAge`")
            string2 <- paste0(string,"+",predictorvector,"")
          }else{
            string2 <- paste0(string,"+`periodAge`","")
          }

        }else{
          if(! is.null(predictorColName)){
            predictorvector <- reduce(predictorColName,paste2)
            string2 <- paste0(string,"+",predictorvector,"")
          }else{
            string2 <- paste0(string,"")
          }

        }
      }


      finaldata <- merge(tcutdata_train,DataFile_personal,by="ID",all.x=T)
      finaldata_test <- merge(tcutdata_test,DataFile_personal,by="ID",all.x=T)

      f1 <- as.formula(paste("Surv(dataLength, futat) ~ ", string2))
      ova.fit <- coxph(f1, data = finaldata)
      ova.fit

      COXccs_window <- summary(ova.fit)
      temp <- cbind(COXccs_window$conf.int, "Pr(>|z|)" =c(COXccs_window$coefficients[,5]))
      temp <- round(temp,3)
      options(scipen = 999)
      temp <- cbind("CCS_CATEGORY"=c(rownames(temp)), temp)
      COXccs_window <- as.data.table(temp)
      COXccs_window$`exp(-coef)` <- NULL
      #####加入文字敘述及數量
      CCSWide_num <- setDT(tcutdata_train)[,lapply(tcutdata_train[,3:ncol(tcutdata_train)],sum),]
      #CCSWide_num <- gather(CCSWide_num,key=CCS_CATEGORY,value=count)
      CCSWide_num <- melt(setDT(CCSWide_num), na.rm = TRUE)[,.(CCS_CATEGORY=variable,caseCount=value)]
      CCSWide_num$CCS_CATEGORY <- paste0(CCSWide_num$CCS_CATEGORY)
      COXccs_window <- merge(COXccs_window,CCSWide_num,by="CCS_CATEGORY",all.x=T)

      if(isDescription==T){
        COXccs_des <- unique(ccstable[,c("CCS_CATEGORY","CCS_CATEGORY_DESCRIPTION","CCS_LVL_2_LABEL","CCS_LVL_1_LABEL"),])
        #COXccs_des$CCS_CATEGORY <- paste0("`",COXccs_des$CCS_CATEGORY,"`")
        COXccs_window <- merge(COXccs_window,COXccs_des,by="CCS_CATEGORY",all.x=T)
      }

      setDT(COXccs_window)
      setnames(COXccs_window,"CCS_CATEGORY","features")
      #COXccs_window[,features:= lapply(.SD,function(x)ifelse(grepl("[0-9]+",x),paste0("ccs_",gsub("`","",x)),x)),.SDcols="features"]
      model[[i]] <- ova.fit
      COXccs[[i]] <- COXccs_window
      traindata[[i]] <- tcutdata_train
      testdata[[i]] <- tcutdata_test

    }


    #model evaluate
    votesummary <- NULL
    windowauc <- c()
    for(i in 1:N){
      windowcut_window <- merge(testdata[[i]], DataFile_personal, all.x = T,by="ID")
      personal_test <-  DataFile_personal[ID %in% windowcut_window$ID]
      forROC <- data.frame(personal_test[,.(ID,label)],pre=predict(model[[i]], newdata = windowcut_window[,3:ncol(windowcut_window)], type="risk"))
      rocobj <- roc(forROC$label,forROC$pre)
      # ##
      # value <- as.numeric(auc(forROC$label,forROC$pre))
      # assign(paste("window", i),value)
      # assign(paste("window",i),assign("N",0))

      ##
      windowauc[i] <- as.numeric(auc(forROC$label,forROC$pre))
      ROC <- coords(rocobj, x="best", input="threshold", best.method="youden")
      if(method=="vote"){
        threshold <- ROC$threshold
        predictvalue <- ifelse(forROC$pre>threshold,1,0)
      }else if(method=="weighting"){
        predictvalue <- forROC$pre
      }
      votesummary <- cbind(votesummary,predictvalue)
    }
    test_windowauc_summary <- cbind(test_windowauc_summary,windowauc)
    votesummary <- as.data.table(votesummary)
    if(method=="vote"){
      votesummary[,finalpredict:= ifelse(rowSums(.SD)>N/2,1,0),]
    }else if(method=="weighting"){
      getPredict <- function(data,x){
        sum = 0
        for(i in 1:length(data)){
          sum <- sum+data[i]*(1/i)
        }
        return(sum)
      }
      votesummary$finalpredict <- apply(votesummary, 1, getPredict)
    }
    finalrocobj <- roc(personal_test$label,votesummary$finalpredict)
    auc <- as.numeric(auc(personal_test$label,votesummary$finalpredict))
    test_timeseriesauc_summary[j] <- auc
  }



  test_windowauc_summary <- as.data.table(test_windowauc_summary)
  test_windowauc_summary$mean <- apply(test_windowauc_summary, 1, mean)
  test_windowauc_summary <- as.data.table(test_windowauc_summary)
  test_timeseriesauc_summary[j+1] <- mean(test_timeseriesauc_summary[1:j])
  test_windowauc_summary_list <- list()
  for(i in 1:N){
    assign(paste0("window",i),as.numeric(test_windowauc_summary[i,]))
    test_windowauc_summary_list[[i]] <- get(paste0("window",i))
  }
  windowname <- paste0("window", 1:N)
  names(test_windowauc_summary_list) <- windowname

  list.Cox <- list(model = model, summarytable = COXccs,evaluation_test=test_timeseriesauc_summary,evaluation_test_window=test_windowauc_summary_list)
  return(list.Cox)
}
