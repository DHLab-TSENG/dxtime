plotWindow <- function(DataFile_cutdata,window_N){
  #visbylabel <- merge(windowcut,pat_dm[,.(ID,label)],by="ID",all.x = T)
  DataFile_cutdata <- windowcut
  DataFile_cutdata <- as.data.table(DataFile_cutdata)
  ccscategory <- unique(ccsDxICD9$CCS_CATEGORY)
  DataFile_cutdata <- DataFile_cutdata[,c("window_N",ccscategory),with=FALSE]
  melt.df_bylabel <- melt(DataFile_cutdata, id=c("window_N"))
  visbylabel_count <- unique(melt.df_bylabel[,count:=sum(value),by=c("window_N","variable")][,.(window_N,variable,count)])
  visbylabel_count$window_N <- as.character(visbylabel_count$window_N)
  setDT(visbylabel_count)
  ccsplot <- ggplot(visbylabel_count[window_N!=0],aes(x=variable,y=window_N,fill=count))+geom_tile(colour="white",size=0.05)+scale_fill_gradient(low = "white",high = "steelblue")+theme(axis.text.x=element_text(size=rel(0.2), angle=90))
  return(ccsplot)
}
