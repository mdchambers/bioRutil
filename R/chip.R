#' Reads a MACS2 .xls file into a GRanges, preserving metadata
#'
#' Reads a MACS2 .xls file into a GRAnges object with metadata
#'
#' @param file
#' @return g A GRange
#' @keywords MACS2 chip GRanges
#' @export
import.macs2.xls <- function(file){
	xls <- read.table(file, header=T)
	GenomicRanges::GRanges(seqnames=xls$chr, 
			ranges=IRanges::IRanges(start=xls$start, end=xls$end), 
			summit=xls$abs_summit, 
			pileup=xls$pileup, 
			pval=xls$X.log10.pvalue., 
			qval=xls$X.log10.qvalue., 
			enrich=xls$fold_enrichment, 
			name=xls$name)
}