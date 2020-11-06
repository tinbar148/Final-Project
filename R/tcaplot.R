#' tcaplot
#' 
#' Makes a TCA plot for a chosen gene from the cleaned dataset produced using dataclean.R.
#' @param finalexp the cleaned rnaseq dataset 
#' @param gname the chosen gene expression data for analysis
#' @keywords TCA plot, expression analysis
#' @examples 
#' tcaplot()

#choose gene for analysis
gname <- "Mrpl15"

tcaplot <- function(finalexp, gname){
  #run PCA analysis on one gene 
  PCAresult <<- prcomp(finalexp, scale. = T)
  plot(PCAresult$x)
  
  #set color gradient and parameters
  ncolors <- 100
  cRamp <- colorRampPalette(c("seagreen", "yellow", "red"))
  cols <- c("gray", cRamp(ncolors))
  gfinalexp <- finalexp[,gname]
  gfinalexp <- ceiling((finalexp/max(gfinalexp))*ncolors)
  gfinalexp <- gfinalexp + 1
  gfinalexp.col <- cols[gfinalexp]
  pcs <<- c(1,2)
  
  #produce final plot
  plot(PCAresult$x[,pcs[1]], PCAresult$x[,pcs[2]], col = gfinalexp.col, pch = 19, main = gname, xlab = paste("PC", pcs[1]), ylab = paste("PC", pcs[2]))
}
