selectFeature <- function(DataFile,ID,ICD,date,label,casecount_limit,type="single",isDescription=F,window,predictwindow,N=NULL,icdcount){
  if(is.null(N)){N <- 1}
  DataFile <- as.data.table(DataFile)

  dataCol <- c(deparse(substitute(ID)),deparse(substitute(ICD)),deparse(substitute(date)),deparse(substitute(label)))

  DataFile <- DataFile[,dataCol,with = FALSE]
  cutdata <- cutWindow(DataFile, ID, ICD, date, predictwindow=predictwindow , window=window , N=N , icdcount=icdcount)


  COXccs_summary <- list()
  for(i in 1:N){
    ##取該window的數據
    cutdata_bywindow <- cutdata[window_N == i,-c("window_N")]
    DataFile <- DataFile[, c("predfirstdate", "indexdate") := list(min(date), max(date)), by=ID]
    ##原始資料取futime數據
    DataFile_case <- DataFile[,c("ID","futat","futime") := list(ID,label,indexdate-predfirstdate),][,c("ID","futat","futime"),]
    DataFile_case <- unique(DataFile_case)
    DataFile_case <- merge(DataFile_case,cutdata_bywindow,by="ID",all.x=T)
    ##決定以mul/single進行特徵選取
    if(type=="mul"){
      #string <- paste(colnames(temp)[4:(ncol(CCSWide)+2)], collapse = "`+`")########從這
      string <- paste(colnames(DataFile_case)[4:(ncol(DataFile_case))], collapse = "`+`")##因為變數太多會跑不動，所以先到50
      string2 <- paste0("`", string, "`")
      f1 <- as.formula(paste("Surv(futime, futat) ~ ", string2))###從這0715 a.m.12:27
      ova.fit <- coxph(f1, data = DataFile_case)#Error: no function to return from, jumping to top level
      ova.fit
      COXccs <- summary(ova.fit)
      COXccs <- as.data.table(COXccs$coefficients, COXccs$conf.int)
      setDT(COXccs)
    }else if(type=="single"){
      ###(單)
      COXccs <- data.table()
      for(j in 4:ncol(DataFile_case)){
        string2 <- paste0("`", colnames(DataFile_case)[j], "`")
        f1 <- as.formula(paste("Surv(futime, futat) ~ ", string2))
        ova.fit_sg <- coxph(f1,data = DataFile_case)
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
    CCSWide_num$CCS_CATEGORY <- paste0("`",CCSWide_num$CCS_CATEGORY,"`")
    COXccs <- merge(COXccs,CCSWide_num,by="CCS_CATEGORY",all.x=T)
    ##決定是否增加文字說明
    if(isDescription==T){
      COXccs_des <- unique(ccsDxICD10[,c("CCS_CATEGORY","CCS_CATEGORY_DESCRIPTION","CCS_LVL_2_LABEL","CCS_LVL_1_LABEL"),])
      COXccs_des <- rename(COXccs_des,"CCS_CATEGORY" = "CCS_CATEGORY")
      COXccs_des$CCS_CATEGORY <- paste0("`",COXccs_des$CCS_CATEGORY,"`")
      COXccs <- merge(COXccs,COXccs_des,by="CCS_CATEGORY",all.x=T)
      COXccs <- COXccs[, c("CCS_CATEGORY","exp(coef)","Pr(>|z|)","count","CCS_CATEGORY_DESCRIPTION","CCS_LVL_2_LABEL","CCS_LVL_1_LABEL")]
    }else{
      COXccs <- COXccs[, c("CCS_CATEGORY","exp(coef)","Pr(>|z|)","count")]
    }
    COXccs <- COXccs[count>0]
    COXccs <- COXccs[`Pr(>|z|)` < 0.05 & count > casecount_limit , c("selected") := TRUE]
    COXccs[is.na(COXccs$selected),"selected"] <- FALSE
    setnames(COXccs,c("exp(coef)","count"),c("HR","caseCount"))
    COXccs_summary[[i]] <- COXccs

  }
  return(COXccs_summary)
}
