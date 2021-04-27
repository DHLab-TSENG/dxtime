plotHR <- function(DataFile,pvalue){
  df <- data.frame(boxLabels = DataFile$CCS_CATEGORY,
                   yAxis = length(DataFile$CCS_CATEGORY):1,
                   boxOdds = as.numeric(DataFile$`exp(coef)`),
                   boxCILow = as.numeric(DataFile$`lower .95`),
                   boxCIHigh = as.numeric(DataFile$`upper .95`),
                   pvalue = as.numeric(DataFile$`Pr(>|z|)`))
  setDT(df)
  pvalue_limit = pvalue
  label = paste("Model p <",pvalue_limit)
  p <- ggplot(df[pvalue < pvalue_limit], aes(x = boxLabels, y = boxOdds, ymin = boxCILow, ymax = boxCIHigh)) +
    geom_pointrange(aes(col = boxLabels), size = 0.2,color = "blue")+
    geom_hline(yintercept =1,linetype = 2)+
    geom_errorbar(aes(ymin = boxCILow, ymax = boxCIHigh, col = boxLabels),width = 0.5,cex = 0.9)+
    coord_flip() +
    annotate(geom = "text", y =30, x = 1, label =label, size = 3.5, hjust = 0) +
    ylab("Hazard ratio") +
    xlab("") +
    theme(legend.position="none",panel.background = element_rect(fill = "white", color = "black"),	panel.grid = element_line(color = "grey80", size = 0.5)) +
    geom_point(size = 1, color = "black")
  return(p)
}
