plotHR <- function(DataFile,pvalue=0.05){
  #DataFile[[]] <- DataFile[[1]]
  DataFile$CCS_CATEGORY <- gsub(pattern = '`',replacement = '',x = DataFile$CCS_CATEGORY)
  DataFile <- DataFile[caseCount != 0]
  ccstable <- ccstable
  #ccstable[,features:= lapply(.SD,function(x)ifelse(grepl("[0-9]+",x),paste0("ccs_",x),x)),.SDcols="CCS_CATEGORY"]

  if(is.null(DataFile$CCS_CATEGORY_DESCRIPTION)){
    DataFile <- merge(DataFile,unique(ccstable[,.(CCS_CATEGORY,CCS_CATEGORY_DESCRIPTION)]),all.x = T,by.x="features",by.y="CCS_CATEGORY")
  }


  DataFile[is.na(CCS_CATEGORY_DESCRIPTION),CCS_CATEGORY_DESCRIPTION:= features]
  df <- data.frame(boxLabels = DataFile$CCS_CATEGORY_DESCRIPTION,
                   yAxis = length(DataFile$CCS_CATEGORY_DESCRIPTION):1,
                   boxHRs = as.numeric(DataFile$`exp(coef)`),
                   boxCILow = as.numeric(DataFile$`lower .95`),
                   boxCIHigh = as.numeric(DataFile$`upper .95`),
                   pvalue = as.numeric(DataFile$`Pr(>|z|)`))
  setDT(df)
  pvalue_limit = pvalue
  label = paste("Model p <",pvalue_limit)
  p <- ggplot(df[pvalue < pvalue_limit], aes(x = boxLabels, y = boxHRs)) +
    geom_errorbar(aes(ymin = boxCILow, ymax = boxCIHigh),width = 0.5,cex = 0.9,color="black") +
    geom_point(size = 1, color = "orange") +
    geom_hline(yintercept =1,linetype = 2) +
    #geom_pointrange(aes(col = boxLabels),size=0.3,color = "blue") +
    coord_flip() +
    theme(legend.position="none",
          axis.text.y = element_text(size=rel(0.8),angle=0,hjust=1),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_line(color = "grey80", size = 0.5)) +
    annotate(geom = "text", y =Inf, x = -Inf, label =label,vjust = -1 ,hjust= 1.5,size = 3.5) +
    ylab("Hazard ratio") +
    xlab("CCScategory") +
    ggtitle("HR (95% CI)")

  return(p)
}
