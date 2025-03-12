#' Label Tags
#'
#' A helper function to convert tag variable names into labels. Can be used for
#' ggplot axis labels, tables, or display.
#'
#' @param x A character vector of variable names to convert.
#' @param label_data A data frame with `tag` and `label` columns.
#' If `NULL`, the function will use the default data stored in the package.
#' @param capitalize A string specifying how to capitalize labels.
#' Options are `"none"`, `"first"`, `"title"`. Default is `"none"`.
#' @param wrap An integer specifying the width at which to wrap text.
#' Default is `Inf` (no wrapping).
#' @param return_type A string specifying the output format.
#' Options are `"vector"` (default) or `"named"`.
#'
#' @return A character vector or named vector of converted labels.
#'
#' @examples
#' # Example 1: Direct conversion
#' label_tags(c("practices_a_la_carte", "adaptive_content"))
#'
#' # Example 2: Rename ggplot axis labels
#' ggplot(mpg, aes(class, hwy)) +
#'   geom_col() +
#'   scale_x_discrete(labels = label_tags)
#'
#' # Example 3: Rename dataframe columns
#' df <- data.frame(a = 1:5, b = 6:10)
#' names(df) <- label_tags(names(df), return_type = "named")
#'
#' @export
label_tags <- function(x, label_data = NULL, capitalize = "none", wrap = Inf, return_type = c("vector", "named")) {
  return_type <- match.arg(return_type)

  # Load default label data if none provided
  if (is.null(label_data)) {
    file_path <- system.file("extdata", "tag-labels.csv", package = "canopyexplorer")
    if (file_path == "") {
      stop("Label data file not found. Ensure 'tag-labels.csv' is stored in 'inst/extdata/'.")
    }
    label_data <- read.csv(file_path, stringsAsFactors = FALSE)
  }

  # Check for missing tag/label columns
  if (!all(c("tag", "label") %in% names(label_data))) {
    stop("Input data must have columns named 'tag' and 'label'.")
  }

  # Match labels
  labels <- label_data$label[match(x, label_data$tag)]

  # Replace missing labels with original variable names
  labels[is.na(labels)] <- x[is.na(labels)]

  # Capitalize if specified
  if (capitalize == "title") {
    labels <- stringr::str_to_title(labels)
  } else if (capitalize == "first") {
    labels <- stringr::str_to_sentence(labels)
  }

  # Wrap if specified
  if (is.finite(wrap)) {
    labels <- stringr::str_wrap(labels, width = wrap)
  }

  # Return as named vector if specified
  if (return_type == "named") {
    names(labels) <- x
  }

  return(labels)
}
