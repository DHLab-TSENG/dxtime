plotWindow <- function(DataFile_cutData,method="top",topN=10,LVL_1_LABEL="Neoplasms"){
  #visbylabel <- merge(windowcut,pat_dm[,.(ID,label)],by="ID",all.x = T)
  DataFile_cutData <- as.data.table(DataFile_cutData)
  ccscategory <- unique(ccstable$CCS_CATEGORY)
  DataFile_cutData <- DataFile_cutData[,c("window_N",ccscategory),with=FALSE]
  melt.df_bylabel <- melt(DataFile_cutData, id=c("window_N"))
  visbylabel_count <- unique(melt.df_bylabel[,count:=sum(value),by=c("window_N","variable")][,.(window_N,variable,count)])
  visbylabel_count$variable <- as.character(visbylabel_count$variable)
  visbylabel_count$window_N <- as.character(visbylabel_count$window_N)
  visbylabel_count <- merge(visbylabel_count,unique(ccstable[,.(CCS_CATEGORY,CCS_CATEGORY_DESCRIPTION)]),by.x="variable",by.y="CCS_CATEGORY",all.x = TRUE)
  visbylabel_count$count <- as.integer(visbylabel_count$count)
  if(method == "top"){
    variable_top10 <- visbylabel_count[order(-count)][window_N=="all" , c("variable")][1:topN]
    setDT(visbylabel_count)
    visbylabel_count <- visbylabel_count[window_N!="all"]
    visbylabel_count$window_N <- as.numeric(visbylabel_count$window_N)
    ccsplot <- ggplot(visbylabel_count[window_N!="all" & (variable %in% variable_top10$variable)],aes(x=window_N,y=CCS_CATEGORY_DESCRIPTION))+
      geom_tile(aes(fill=count),colour="white",size=0.05) +
      scale_fill_gradient(low = "white",high = "steelblue") +
      theme(axis.text.x = element_text(hjust=0.3,vjust = 1)) +
      labs(fill = "case count") +
      theme(panel.background = element_rect(fill="white"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())+
      ggtitle(paste0("Top",topN," CCS"))


  }else if(method == "ccslevel"){
    variable_level <- ccstable[CCS_LVL_1_LABEL==LVL_1_LABEL,c("CCS_CATEGORY")]
    setDT(variable_level)
    visbylabel_count <- visbylabel_count[window_N!="all"]
    visbylabel_count$window_N <- as.numeric(visbylabel_count$window_N)
    ccsplot <- ggplot(visbylabel_count[window_N!="all" & (variable %in% variable_level$CCS_CATEGORY)],aes(x=window_N,y=CCS_CATEGORY_DESCRIPTION))+
      geom_tile(aes(fill=count),colour="white",size=0.05)+
      scale_fill_gradient(low = "white",high = "steelblue")+
      theme(axis.text.x = element_text(hjust=0.3,vjust = 1))+
      labs(title="case count") +
      theme(panel.background = element_rect(fill="white"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())+
      ggtitle(paste0(LVL_1_LABEL))

  }else{
    setDT(visbylabel_count)
    ccsplot <- ggplot(visbylabel_count[window_N!="all"] ,aes(x=window_N,y=CCS_CATEGORY_DESCRIPTION))+
      scale_fill_gradient(low = "white",high = "steelblue") +
      theme(axis.text.x = element_text(hjust=0.3,vjust = 1)) +
      labs(title="case count") +
      theme(panel.background = element_rect(fill="white"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  }
  #visbylabel_count <- visbylabel_count[head(count,10)]
    return(ccsplot)
}
