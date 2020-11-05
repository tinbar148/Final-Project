#' dataclean
#' 
#' Takes in an rnaseq dataset and cleans it so that gene expression data can be analyzed in subsequent functions.
#' @param data the imported rnaseq dataset (excel file)
#' @keywords clean, dataset
#' @examples 
#' dataclean()

#make a function for reading in data and cleaning
dataclean <- function(data){
  #transpose data to flip rows and columns
  data_transpose <- as.data.frame(t(as.matrix(data)))
  
  #change row and column names so that gene names are treated as column names
  rownames(data_transpose) <- NULL
  colnames(data_transpose) <- data_transpose[1,] 
  
  #eliminate columns without gene expression data
  non.data.col <- 5
  
  #create a new data frame with only gene expression data for use in next two functions
  geneexp <- data_transpose[2:nrow(data_transpose), (non.data.col+1):ncol(data_transpose)]
  
  #remove empty data columns and return to numeric format
  geneexp <- geneexp[, colSums(geneexp != 0) > 0]
  geneexp <- apply(geneexp, 2, as.numeric)
  finalexp <<- geneexp
}

