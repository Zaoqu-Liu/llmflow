#' Extract Examples from a Package Function
#'
#' This function extracts and cleans the examples section from a specific
#' function's documentation in an R package. It uses the `tools` package
#' to access the Rd database and extracts examples using `tools::Rd2ex()`.
#' The output is cleaned to remove metadata headers and formatting artifacts.
#'
#' @param package_name A character string specifying the name of the package
#' @param function_name A character string specifying the name of the function
#'
#' @return A character string containing the cleaned examples code, or `NA`
#'   if no examples are found or an error occurs
#'
#' @examples
#' \dontrun{
#' # Extract examples from ggplot2's geom_point function
#' examples <- extract_function_examples("ggplot2", "geom_point")
#' cat(examples)
#' }
#'
#' @export
extract_function_examples <- function(package_name, function_name) {
  tryCatch(
    {
      # Locate the Rd database file for the package
      rdbfile <- file.path(find.package(package_name), "help", package_name)

      # Fetch the documentation for the specific function
      rdb <- tools:::fetchRdDB(rdbfile, key = function_name)
      if (is.null(rdb)) {
        return(NA)
      }

      # Extract examples using tools::Rd2ex
      examples_text <- capture.output(tools::Rd2ex(rdb))
      if (length(examples_text) == 0) {
        return(NA)
      }

      # Clean examples: remove header information, keep only actual code
      # Look for the "### ** Examples" line
      examples_start <- which(grepl("### \\*\\* Examples", examples_text))
      if (length(examples_start) > 0) {
        # Start from the line after "### ** Examples"
        clean_examples <- examples_text[(examples_start[1] + 1):length(examples_text)]

        # Remove leading empty lines
        while (length(clean_examples) > 0 && clean_examples[1] == "") {
          clean_examples <- clean_examples[-1]
        }
        return(paste(clean_examples, collapse = "\n"))
      } else {
        # If standard format not found, return all (remove obvious headers)
        # Remove lines starting with ###
        clean_examples <- examples_text[!grepl("^###", examples_text)]
        return(paste(clean_examples, collapse = "\n"))
      }
    },
    error = function(e) {
      return(NA)
    }
  )
}
