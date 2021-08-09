selectFeature <- function(DataFile_cutData,DataFile_personal,idColName,labelColName,dataLengthColName,caseCountRate_limit=0.001,type="single",isDescription=F,method="allWindow",pvalue=0.05){

  if(method=="allWindow"){
    N <- 1
  }else if(method=="perWindow"){
    DataFile_cutData <- DataFile_cutData[window_N!="all"]
    DataFile_cutData$window_N <- as.integer(DataFile_cutData$window_N)
    N <- max(DataFile_cutData$window_N)
  }
  cutdata <- as.data.table(DataFile_cutData)
  ccscategory <- unique(c(ccstable$CCS_CATEGORY))
  dataCol <- c(deparse(substitute(ID)),deparse(substitute(window_N)),ccscategory)
  cutdata <- cutdata[,dataCol,with = FALSE]
  #cutdata <- cutWindow(DataFile, ID, ICD, date, predictGap=predictGap , window=window , N=N , countICD_toccs=countICD_toccs)

  DataFile_personal <- as.data.table(DataFile_personal)
  dataCol <- c(deparse(substitute(idColName)),deparse(substitute(labelColName)),deparse(substitute(dataLengthColName)))
  #dataCol <- c(deparse(substitute(ID)),deparse(substitute(label)),deparse(substitute(dataLength)))
  personal <- DataFile_personal[,dataCol, with=FALSE]
  names(personal) <- c("ID","futat","futime")
  casecount_limit <- caseCountRate_limit*length(unique(personal$ID))

  COXccs_summary <- list()
  for(i in 1:N){
    ##取該window的數據
    if(N==1){
      cutdata_bywindow <- cutdata[window_N == "all",-c("window_N")]
    }else{
      cutdata_bywindow <- cutdata[window_N == i,-c("window_N")]
    }
    personal_bywindow <- merge(personal,cutdata_bywindow,by="ID",all.x=T)
    if(type=="mul"){
      string <- paste(colnames(personal_bywindow)[4:(ncol(personal_bywindow))], collapse = "`+`")
      string2 <- paste0("`", string, "`")
      f1 <- as.formula(paste("Surv(futime, futat) ~ ", string2))
      ova.fit <- coxph(f1, data = personal_bywindow)
      ova.fit
      COXccs <- summary(ova.fit)
      COXccs <- as.data.table(COXccs$coefficients, COXccs$conf.int)
      setDT(COXccs)
    }else if(type=="single"){
      COXccs <- data.table()
      for(j in 4:ncol(personal_bywindow)){
        string2 <- paste0(colnames(personal_bywindow)[j])
        f1 <- as.formula(paste("Surv(futime, futat) ~ ", string2))
        ova.fit_sg <- coxph(f1,data = personal_bywindow)
        COXccs_sgtemp <- summary(ova.fit_sg)
        COXccs <- rbind(COXccs,as.data.table(COXccs_sgtemp$coefficients, COXccs_sgtemp$conf.int))
        setDT(COXccs)
      }
    }
    ##計算casecount數量
    setnames(COXccs,"rn","CCS_CATEGORY")
    CCSWide_num <- setDT(cutdata_bywindow)[,lapply(cutdata_bywindow[,2:ncol(cutdata_bywindow)],sum),]
    #CCSWide_num <- gather(CCSWide_num,key=rn,value=count)
    CCSWide_num <- melt(setDT(CCSWide_num), na.rm = TRUE)[,.(CCS_CATEGORY=variable,count=value)]
    CCSWide_num$CCS_CATEGORY <- paste0(CCSWide_num$CCS_CATEGORY)
    COXccs <- merge(COXccs,CCSWide_num,by="CCS_CATEGORY",all.x=T)
    ##決定是否增加文字說明
    if(isDescription==T){
      COXccs_des <- unique(ccstable[,c("CCS_CATEGORY","CCS_CATEGORY_DESCRIPTION","CCS_LVL_1_LABEL","CCS_LVL_2_LABEL"),])
      #COXccs_des <- setnames(COXccs_des,"CCS_CATEGORY","CCS_CATEGORY")
      COXccs_des$CCS_CATEGORY <- paste0(COXccs_des$CCS_CATEGORY)
      COXccs <- merge(COXccs,COXccs_des,by="CCS_CATEGORY",all.x=T)
      COXccs <- COXccs[, c("CCS_CATEGORY","exp(coef)","Pr(>|z|)","count","CCS_CATEGORY_DESCRIPTION","CCS_LVL_1_LABEL","CCS_LVL_2_LABEL")]
    }else{
      COXccs <- COXccs[, c("CCS_CATEGORY","exp(coef)","Pr(>|z|)","count")]
    }
    COXccs <- COXccs[count>0]
    COXccs <- COXccs[`Pr(>|z|)` < pvalue & count > casecount_limit , c("selected") := TRUE]
    COXccs[is.na(COXccs$selected),"selected"] <- FALSE
    setnames(COXccs,c("exp(coef)","count"),c("HR","caseCount"))
    COXccs$`Pr(>|z|)` <- round(COXccs$`Pr(>|z|)`,3)
    COXccs$HR <- round(COXccs$HR,3)
    COXccs_summary[[i]] <- COXccs
    options(scipen=999)
  }
  return(COXccs_summary)
}
