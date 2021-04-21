analyWindow_COX <- function(DataFile_cutdata,window_N,ID,DataFile_feature,DataFile_personal,label,futime,predictor=c(),periodage=NULL,isDescription=F,method,N){

  cutdata <- as.data.table(DataFile_cutdata)
  ccscategory <- unique(c(ccsDxICD9$CCS_CATEGORY))
  dataCol <- c(deparse(substitute(window_N)),deparse(substitute(ID)),ccscategory)
  cutdata <- cutdata[, dataCol,with=FALSE]

  DataFile_personal <- as.data.table(DataFile_personal)

  dataCol <- c(deparse(substitute(ID)),deparse(substitute(label)),deparse(substitute(futime)),predictor)

  DataFile_personal <- DataFile_personal[, dataCol, with=FALSE]


  if(! is.na(method)){

    model <- list()
    COXccs <- list()
    for(i in 1:3){
      if(method=="one window"){
        COXccs_top <- DataFile_feature[[1]][selected==1, "CCS_CATEGORY"]
      }else if(method=="per window"){
        COXccs_top <- DataFile_feature[[i]][selected==1, "CCS_CATEGORY"]
      }
      setDT(COXccs_top)
      tcutdata <- cutdata[window_N==i,]

      string <- paste(as.character(COXccs_top$CCS_CATEGORY), collapse = "+")
      paste2 <- function(x, y, sep = "+") paste(x, y, sep = sep)
      if(string == ""){

        string2 <- "`SEX`+`periodage`"
      }else{

        if(!is.null(periodage)){
          predictorvector <- reduce(predictor,paste2)
          predictorvector <- paste0(predictorvector,"+`periodage`")
          string2 <- paste0(string,predictorvector,"")
        }else{
          predictorvector <- reduce(predictor,paste2)
          string2 <- paste0(string,"+",predictorvector,"")
        }
      }

      DataFile_personal$futat <- DataFile_personal$label
      DataFile_personal$futat <- as.numeric(DataFile_personal$futat)
      DataFile_personal$futime <- as.numeric(DataFile_personal$futime)

      finaldata <- merge(tcutdata,DataFile_personal,by="ID",all.x=T)
      f1 <- as.formula(paste("Surv(futime, futat) ~ ", string2))
      ova.fit <- coxph(f1, data = finaldata)
      ova.fit

      COXccs_window <- summary(ova.fit)
      temp <- cbind(COXccs_window$conf.int, "Pr(>|z|)" =c(COXccs_window$coefficients[,5]))
      options(scipen = 999)
      temp <- cbind("CCS_CATEGORY"=c(rownames(temp)), temp)
      COXccs_window <- as.data.table(temp)
      #####加入文字敘述及數量
      CCSWide_num <- setDT(tcutdata)[,lapply(tcutdata[,3:ncol(tcutdata)],sum),]
      #CCSWide_num <- gather(CCSWide_num,key=CCS_CATEGORY,value=count)
      CCSWide_num <- melt(setDT(CCSWide_num), na.rm = TRUE)[,.(CCS_CATEGORY=variable,count=value)]
      CCSWide_num$CCS_CATEGORY <- paste0("`",CCSWide_num$CCS_CATEGORY,"`")
      COXccs_window <- merge(COXccs_window,CCSWide_num,by="CCS_CATEGORY",all.x=T)
      if(isDescription==T){
        COXccs_des <- unique(ccsDxICD10[,c("CCS_CATEGORY","CCS_CATEGORY_DESCRIPTION","CCS_LVL_2_LABEL","CCS_LVL_1_LABEL"),])
        COXccs_des$rn <- paste0("`",COXccs_des$CCS_CATEGORY,"`")
        COXccs_window <- merge(COXccs_window,COXccs_des,by="CCS_CATEGORY",all.x=T)
      }
      setDT(COXccs_window)
      model[[i]] <- ova.fit
      COXccs[[i]] <- COXccs_window

    }
  }
  list.COX <- list(model = model, model_table = COXccs)
  return(list.COX)
}
