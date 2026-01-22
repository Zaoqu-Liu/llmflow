#' @importFrom utils capture.output head tail str packageVersion
#' @importFrom stats na.omit
NULL

.onAttach <- function(libname, pkgname) {
  msg <- paste0(
    "llmflow ", utils::packageVersion("llmflow"), "\n",
    "For LLM communication, please load ellmer: library(ellmer)"
  )
  packageStartupMessage(msg)
}
