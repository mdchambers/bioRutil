
#' Gets all genes from UCSC.dm3.ensGene genome
#'
#' Returns all genes as a GeneList
#'
#' @keywords genes
#' @export
getDmelGenes <- function(){
	library(TxDb.Dmelanogaster.UCSC.dm3.ensGene)
	txdb <- TxDb.Dmelanogaster.UCSC.dm3.ensGene
	gn <- sort(genes(txdb))
	return(gn)
}

#' Gets flanking regions of given genes
#'
#' Returns the upstream sequences given a list of genes
#'
#' @param genes A GRanges object of genes of interest (e.g. as produced by getDmelGenes)
#' @param len	Length of upstream sequence to retrieve
#' @return A DNAStringSet contining upstream sequences
#' @keywords genes
#' @export
getUpstreamSeq <- function(genes, len){
	ups <- flank(genes, width=len)
	library(BSgenome.Dmelanogaster.UCSC.dm3)
	genome <- BSgenome.Dmelanogaster.UCSC.dm3
	return( getSeq(genome, ups))
}

#' Clusters genes by expression using k-means
#'
#' Returns a list of data.frames split by cluster
#'
#' @param df	A data.frame containing DE values, one gene per line (gene as rowname)
#' @param clusters	Number of clusters to generate
#' @keywords genes, clustering
#' @return A list of data.frames
#' @export
clusterDEList <- function(df, clusters){
	# Perform clustering
	k <- kmeans(df, clusters)

	# df$gene <- rownames(df)
	# Transfer cluster number to DF of dgro logFCs
	df$cluster <- as.factor(k$cluster)
	df.split <- split(df, df$cluster)
	return(df.split)
}
