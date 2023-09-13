#' Compiles .bib files
#'
#' Takes filepaths where .bib files are stored and compiles them
#'
#' @param PATH1 path to .bib directory
#' @param PATH2 path to .bib directory
#' @param PATH3 path to .bib directory
#' @param PATH4 path to .bib directory
#'
#' @return a thesis.bib file stored in the bib/ directory (should be in the aggiedown project)
#'
#' @import readtext
#'
#' @export

auto_bib_file <- function(PATH1, PATH2, PATH3, PATH4){
  bib0 <- readtext::readtext(paste0(PATH1,'04_refs.bib'))$text
  bib1 <- readtext::readtext(paste0(PATH2,'04_refs.bib'))$text
  bib2 <- readtext::readtext(paste0(PATH3,'04_refs.bib'))$text
  bib3 <- readtext::readtext(paste0(PATH4,'04_refs.bib'))$text

  bib <- paste(bib0, bib1, bib2, bib3, collapse = "\n")

  write.table(bib, 'bib/thesis.bib', row.names = F, col.names = F, quote = F)
}
