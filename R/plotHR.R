plotHR <- function(DataFile,pvalue){

  DataFile$CCS_CATEGORY <- gsub(pattern = '`',replacement = '',x = DataFile$CCS_CATEGORY)
  DataFile <- merge(DataFile,unique(ccstable[,.(CCS_CATEGORY,CCS_CATEGORY_DESCRIPTION)]),all.x = T)
  df <- data.frame(boxLabels = DataFile$CCS_CATEGORY_DESCRIPTION,
                   yAxis = length(DataFile$CCS_CATEGORY_DESCRIPRION):1,
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
    #geom_pointrange(aes(col = boxLabels),size=0.3,color = "blue") +
    coord_flip() +
    theme(legend.position="none",
          axis.text.y = element_text(size=rel(0.5),angle=0,hjust=1),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_line(color = "grey80", size = 0.5)) +
    annotate(geom = "text", y =Inf, x = -Inf, label =label,vjust = -1 ,hjust= 1.5,size = 3.5) +
    ylab("Hazard ratio") +
    xlab("CCScategory")
  return(p)
}
