#' Generates a matrix of spearman coefficients
#'
#' Generates a matrix of spearman coefficients
#' Recommend plotting using a heatmap
#'
#' library(gplots)
#' library(RColorBrewer)
#' heatmap.2(coeffs,Rowv=T,Colv=T,symm=T,key=T,trace="none",col=brewer.pal(9,"Purples"))
#'
#' @param df A data frame, each column being a set of data to correlate
#' @return A matrix of spearman coefficients
#' @keywords Spearman
#' @export
spear.matrix <- function(df){
	spearij <- function(i,j,data) {cor.test(data[,i],data[,j],method="spear",exact=F,alternative="t")$estimate}
	spear <- Vectorize(spearij,vectorize.args=list("i","j"))
	n <- ncol(df)
	coeffs <- outer(1:n,1:n,spear,data=df)
	colnames(coeffs) <- colnames(df)
	rownames(coeffs) <- colnames(df)
	return(coeffs)
}

#' Performs a Jaccard comparison between two bed files
#'
#' Generates Jaccard simlirity of two bed files ( a measure of similarity, less sensitive to noise than Spearman, Pearson, etc.
#' Requires bedtools to be installed and in environment PATH
#'
#' @param a first bed file
#' @param b second bed file
#' @return A vector containing Jaccard distances
#' @keywords chip-seq jaccard
#' @export
do.jaccard <- function(a,b){
	cmd <- paste0("bedtools jaccard -a ", a, " -b ", b)
	stdout <- system(cmd, T)
	split <- strsplit(stdout, "\t")
	output <- as.numeric(split[[2]])
	names(output) <- split[[1]]
	return(output)
}

