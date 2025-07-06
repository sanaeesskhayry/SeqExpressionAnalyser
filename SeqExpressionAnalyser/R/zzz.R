#' @importFrom shiny addResourcePath

.onLoad <- function(libname, pkgname) {
  # Create link to logo
  shiny::addResourcePath("SeqExpressionAnalyser", system.file("www", package = "SeqExpressionAnalyser"))

}
.onAttach <- function(libname, pkgname) {
  pkgVersion <- packageDescription("SeqExpressionAnalyser", fields = "Version")
  msg <- paste0("Welcome to SeqExpressionAnalyser v", pkgVersion, "\n")
  packageStartupMessage(msg)

}
