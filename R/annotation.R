#' Returns a mapping data_frame from org.Dm.eg.db
#'
#' Mapping data.frame of FBgn <-> entrez <-> Gene Description
#'
#' @return A mapping data_frame
#' @keywords annotation
#' @export
fbToCommon <- function(){
	# require(org.Dm.eg.db)
	fb <- unlist(AnnotationDbi::as.list(org.Dm.eg.db::org.Dm.egFLYBASE2EG))
	fb.df <- dplyr::data_frame(fly = names(fb), entrez = as.vector(fb))

	gn <- unlist(AnnotationDbi::as.list(org.Dm.eg.db::org.Dm.egGENENAME))
	gn.df <- dplyr::data_frame(entrez = names(gn), common = as.vector(gn))

	map.df <- dplyr::left_join(fb.df, gn.df, by="entrez")
	return(map.df)
}