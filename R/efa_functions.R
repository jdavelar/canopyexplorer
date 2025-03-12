#' Correlation of Tag Variables
#'
#' Generates a correlation matrix for tag variables in a dataset.
#'
#' @param data A data frame containing tag variables.
#' @param prefix A string specifying the prefix of tag columns (default is `"practices_"`).
#' @param output A string specifying the desired output: `"dataframe"`, `"table"`, or `"plot"`.
#' @param use A string specifying how to handle missing values. Default is `"pairwise.complete.obs"`.
#' @param method A string specifying the correlation method (`"pearson"`, `"spearman"`, `"kendall"`). Default is `"pearson"`.
#'
#' @return A correlation matrix (data frame) or a table/plot (if specified).
#'
#' @examples
#' test_data <- data.frame(
#'   practices_a = c(1, 0, 1, 1, 0),
#'   practices_b = c(0, 1, 1, 0, 1),
#'   practices_c = c(1, 1, 0, 1, 1)
#' )
#'
#' # Return correlation dataframe
#' tag_correlations(test_data)
#'
#' # Display as table
#' tag_correlations(test_data, output = "table")
#'
#' # Display as plot
#' tag_correlations(test_data, output = "plot")
#'
#' @export
tag_correlations <- function(
    data,
    prefix = "practices_",
    output = c("dataframe", "table", "plot"),
    use = "pairwise.complete.obs",
    method = "pearson"
) {
  output <- match.arg(output)

  # Identify relevant columns based on prefix
  tag_cols <- grep(paste0("^", prefix), names(data), value = TRUE)

  if (length(tag_cols) < 2) {
    stop("Not enough tag variables found with the specified prefix.")
  }

  # Subset the data
  tag_data <- data[tag_cols]

  # Ensure numeric columns
  if (any(!sapply(tag_data, is.numeric))) {
    stop("Tag variables must be numeric (0/1). Please convert before running this function.")
  }

  # Calculate correlation matrix
  cor_matrix <- cor(tag_data, use = use, method = method)

  # Return correlation matrix if requested
  if (output == "dataframe") {
    return(as.data.frame(cor_matrix))
  }

  # Return table if requested
  if (output == "table") {

    # Convert to long format
    cor_long <- as.data.frame(as.table(cor_matrix))
    colnames(cor_long) <- c("var1", "var2", "correlation")

    # Remove diagonal (self-correlations)
    cor_long <- cor_long[cor_long$var1 != cor_long$var2, ]

    # Apply labels using label_tags()
    labels <- label_tags(unique(c(cor_long$var1, cor_long$var2)), return_type = "named")
    cor_long$var1 <- labels[cor_long$var1]
    cor_long$var2 <- labels[cor_long$var2]

    # Round for cleaner display
    cor_long$correlation <- round(cor_long$correlation, 2)

    # Display using DT
    if (requireNamespace("DT", quietly = TRUE)) {
      return(DT::datatable(
        cor_long,
        options = list(pageLength = 10)
      ))
    }

    # Fallback to knitr or gt
    else if (requireNamespace("knitr", quietly = TRUE)) {
      return(knitr::kable(cor_long, digits = 2))
    }

    else if (requireNamespace("gt", quietly = TRUE)) {
      return(gt::gt(cor_long))
    }

    else {
      stop("Either `DT`, `knitr`, or `gt` must be installed to display a table.")
    }
  }

  # Return plot if requested
  if (output == "plot") {
    if (requireNamespace("ggplot2", quietly = TRUE)) {

      # Convert to long format
      cor_long <- as.data.frame(as.table(cor_matrix))
      colnames(cor_long) <- c("var1", "var2", "correlation")

      # Use labels from label_tags()
      labels <- label_tags(unique(c(cor_long$var1, cor_long$var2)), return_type = "named")
      cor_long$var1 <- labels[cor_long$var1]
      cor_long$var2 <- labels[cor_long$var2]

      # Create plot
      plot <- ggplot2::ggplot(cor_long, ggplot2::aes(x = var1, y = var2, fill = correlation)) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank()
        ) +
        ggplot2::labs(title = "Correlation Matrix", subtitle = "Direct correlation values")

      print(plot)

    } else {
      stop("The `ggplot2` package must be installed to display a plot.")
    }
  }
}


#' Exploratory Factor Analysis (EFA) for Tag Correlations
#'
#' This function conducts an exploratory factor analysis (EFA) based on a correlation
#' matrix from the `correlate_tags()` function. The function returns a table of factor
#' loadings for the specified number of factors.
#'
#' The function assumes that the correlation matrix reflects shared variance between
#' items. Results are presented as either an interactive table (via `DT`) or a data frame.
#'
#' @param cor_matrix A correlation matrix (from `correlate_tags()`)
#' @param n_factors Number of factors to extract. Default is 4.
#' @param output Output format. Options:
#' \itemize{
#'   \item `"table"` (default) – Displays an interactive table using `DT::datatable`.
#'   \item `"dataframe"` – Returns the factor loadings as a data frame.
#' }
#'
#' @return A table or data frame of factor loadings, with labels applied.
#'
#' @seealso \code{\link{correlate_tags}} for creating the correlation matrix.
#'
#' @examples
#' cor_matrix <- correlate_tags(test_data, output = "dataframe")
#' tag_efa(cor_matrix, n_factors = 4)
#'
#' # Return as data frame
#' factor_results <- tag_efa(cor_matrix, n_factors = 4, output = "dataframe")
#'
#' @export
tag_efa <- function(cor_matrix, n_factors = 4, output = "table") {
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("The `psych` package must be installed to perform factor analysis.")
  }

  # Run factor analysis
  efa_result <- tryCatch({
    psych::fa(cor_matrix, nfactors = n_factors, rotate = "varimax")
  }, error = function(e) {
    stop("Factor analysis failed: ", e$message)
  })

  # Extract loadings
  loadings <- as.data.frame(unclass(efa_result$loadings))

  # Add row labels using label_tags()
  labels <- label_tags(rownames(loadings), return_type = "named")
  rownames(loadings) <- labels

  # Display as table using DT
  if (output == "table") {
    if (requireNamespace("DT", quietly = TRUE)) {
      return(DT::datatable(
        round(loadings, 2),
        options = list(pageLength = 10)
      ))
    } else {
      warning("`DT` package not found; returning data frame instead.")
      return(loadings)
    }
  }

  # Return as dataframe
  if (output == "dataframe") {
    return(loadings)
  }
}
