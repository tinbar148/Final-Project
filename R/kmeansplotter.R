#' kmeansplotter
#' 
#' Makes a k-means plot based on the TCA analysis run in tcaplot.R.
#' @param finalexp the cleaned rnaseq dataset 
#' @keywords k-means plot, expression analysis
#' @examples 
#' kmeansplotter()

#plot data as a k-means plot
kmeansplotter <- function(finalexp){
  #set number of clusters
  k <- 3
  #set parameters for kmeans method and number of iterations
  kmeansresult <- kmeans(finalexp, k, iter.max = 1000, nstart = 10, algorithm = c("Hartigan-Wong"), trace = F)
  k.clusters <- kmeansresult$cluster
  m <- as.factor(k.clusters)
  #plot
  plot(PCAresult$x[,pcs[1]], PCAresult$x[,pcs[2]], col = m, pch = 16, main = "K-means", xlab = paste("PC", pcs[1]), ylab = paste("PC", pcs[2]))
  m.values <- as.factor (levels(m))
  #make legend for different clusters
  legend(legend = m.values, "topright", pch = 16, col = m.values)
}