#' @importFrom shiny addResourcePath

.onLoad <- function(libname, pkgname) {
  # Create link to logo
  shiny::addResourcePath("SeqExpressionAnalyser", system.file("www", package = "SeqExpressionAnalyser"))

}
