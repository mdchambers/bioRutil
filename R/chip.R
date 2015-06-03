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


#' Import a MACS2 NarrowPeak file as a GRanges object
#'
#' Creates a GRanges object from the NarrowPeak file, taking into account switch from zero-base to one-base nomenclature. Adds and labels data as metadata.
#'
#' @param file Path to a NarrowPeak file for import
#' @return A GRanges object
#' @keywords MACS2 chip GRanges
#' @export
import.macs2.narrowpeak <- function(file){
	np <- read.table(file, header=F)
	gr <- GenomicRanges::GRanges(seqnames=np$V1,
		ranges=IRanges::IRanges(start=np$V2+1, end=np$V3),
		score=np$V5,
		name=np$V4,
		fc=np$V7,
		log10p=np$V8,
		log10q=np$V9,
		relsummit=np$V10,
		summit=np$V2+np$V10+1)
	names(gr) <- np$V4
	gr
}