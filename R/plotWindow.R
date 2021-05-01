plotWindow <- function(DataFile_cutdata,window_N,method="top",topN=10,LVL_1_LABEL="Neoplasms"){
  #visbylabel <- merge(windowcut,pat_dm[,.(ID,label)],by="ID",all.x = T)
  DataFile_cutdata <- as.data.table(DataFile_cutdata)
  ccscategory <- unique(ccstable$CCS_CATEGORY)
  DataFile_cutdata <- DataFile_cutdata[,c("window_N",ccscategory),with=FALSE]
  melt.df_bylabel <- melt(DataFile_cutdata, id=c("window_N"))
  visbylabel_count <- unique(melt.df_bylabel[,count:=sum(value),by=c("window_N","variable")][,.(window_N,variable,count)])
  visbylabel_count$variable <- as.character(visbylabel_count$variable)
  visbylabel_count$window_N <- as.character(visbylabel_count$window_N)
  visbylabel_count <- merge(visbylabel_count,unique(ccstable[,.(CCS_CATEGORY,CCS_CATEGORY_DESCRIPTION)]),by.x="variable",by.y="CCS_CATEGORY",all.x = TRUE)

  if(method == "top"){
    variable_top10 <- visbylabel_count[order(-count)][window_N==0 , c("variable")][1:topN]
    setDT(visbylabel_count)
    ccsplot <- ggplot(visbylabel_count[window_N!=0 & (variable %in% variable_top10$variable)],aes(x=CCS_CATEGORY_DESCRIPTION,y=window_N,fill=count))+ geom_tile(colour="white",size=0.05) +
      scale_fill_gradient(low = "white",high = "steelblue") +
      theme(axis.text.x = element_text(size=rel(0.8),angle=80,hjust=1))

  }else if(method == "ccslevel"){
    variable_level <- ccstable[CCS_LVL_1_LABEL==LVL_1_LABEL,c("CCS_CATEGORY")]
    setDT(variable_level)
    ccsplot <- ggplot(visbylabel_count[window_N!=0 & (variable %in% variable_level$CCS_CATEGORY)],aes(x=CCS_CATEGORY_DESCRIPTION,y=window_N,fill=count))+ geom_tile(colour="white",size=0.05)+
      scale_fill_gradient(low = "white",high = "steelblue")+
      theme(axis.text.x = element_text(size=rel(0.8),angle=80,hjust=1))

  }else{
    setDT(visbylabel_count)
    ccsplot <- ggplot(visbylabel_count[window_N!=0],aes(x=variable,y=window_N,fill=count))+geom_tile(colour="white",size=0.05)+scale_fill_gradient(low = "white",high = "steelblue") + theme(axis.text.x = element_text(size=rel(0.8),angle=80,hjust=1)) + guides(color = guide_legend(title="case count"))

  }

  #visbylabel_count <- visbylabel_count[head(count,10)]
    return(ccsplot)
}
