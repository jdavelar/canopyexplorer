#' Generate Quick Summaries
#'
#' This function generates a quick summary table or barplot for a specified variable
#' (numeric or categorical). It can also handle grouping by a second variable to
#' create summaries by category.
#'
#' @param data A dataframe containing the variables to summarize.
#' @param var A character string specifying the variable to summarize.
#' @param by An optional character string specifying a grouping variable.
#' @param output A character string indicating the type of output. Must be one of
#'   `"table"` (default) or `"plot"`.
#'
#' @return Depending on the `output` argument:
#'   - `"table"`: Returns a summary table (as a `data.frame`).
#'   - `"plot"`: Returns a ggplot object showing the summary (barplot for categorical, histogram for numeric).
#'
#' @examples
#' # Summary of a single variable
#' quick_summary(dat_2024, var = "leader_race")
#'
#' # Summary by a grouping variable
#' quick_summary(dat_2024, var = "leader_race", by = "school_type")
#'
#' # Generate a barplot for a categorical variable
#' quick_summary(dat_2024, var = "leader_race", output = "plot")
#'
#' # Generate a histogram for a numeric variable
#' quick_summary(dat_2024, var = "pct_bipoc", output = "plot")
#'
#' @export
quick_summary <- function(
    data,
    var,
    by = NULL,
    output = c("table", "plot")) {
  # Extract CRPE color from the palette
  fill_color <- suppressWarnings(crpe_palette("crpe_blues")$palette(1)[1])

  output <- match.arg(output)

  if (!var %in% names(data)) {
    stop(paste("Variable", var, "not found in the dataset."))
  }

  if (!is.null(by) && !by %in% names(data)) {
    stop(paste("Grouping variable", by, "not found in the dataset."))
  }

  # Handle numeric vs categorical separately
  if (is.numeric(data[[var]])) {
    if (output == "table") {
      if (is.null(by)) {
        # Summary stats for single numeric variable
        summary_table <- data %>%
          dplyr::summarise(
            Mean = mean(.data[[var]], na.rm = TRUE),
            Median = median(.data[[var]], na.rm = TRUE),
            SD = sd(.data[[var]], na.rm = TRUE),
            Min = min(.data[[var]], na.rm = TRUE),
            Max = max(.data[[var]], na.rm = TRUE),
            Count = sum(!is.na(.data[[var]]))
          )
      } else {
        # Summary stats grouped by categorical variable
        summary_table <- data %>%
          dplyr::group_by(.data[[by]]) %>%
          dplyr::summarise(
            Mean = mean(.data[[var]], na.rm = TRUE),
            Median = median(.data[[var]], na.rm = TRUE),
            SD = sd(.data[[var]], na.rm = TRUE),
            Min = min(.data[[var]], na.rm = TRUE),
            Max = max(.data[[var]], na.rm = TRUE),
            Count = sum(!is.na(.data[[var]]))
          ) %>%
          dplyr::ungroup()
      }
      return(summary_table)
    }

    if (output == "plot") {
      if (is.null(by)) {
        # Histogram for single numeric variable (20 bins + dashed line for mean)
        plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[var]])) +
          ggplot2::geom_histogram(
            bins = 20,
            fill = fill_color, # CRPE color applied here
            color = "white"
          ) +
          ggplot2::geom_vline(
            xintercept = mean(data[[var]], na.rm = TRUE),
            linetype = "dashed",
            color = "red"
          ) +
          ggplot2::labs(
            x = var,
            y = "Count",
            title = paste("Distribution of", var)
          ) +
          theme_crpe()
      } else {
        # Boxplot for numeric variable grouped by categorical variable
        plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[by]], y = .data[[var]])) +
          ggplot2::geom_boxplot(fill = fill_color) + # CRPE color applied here
          ggplot2::labs(
            x = by,
            y = var,
            title = paste("Distribution of", var, "by", by)
          ) +
          theme_crpe()
      }
      return(plot)
    }
  } else {
    # Categorical variable handling
    if (output == "table") {
      if (is.null(by)) {
        # Frequency table for single categorical variable
        summary_table <- data %>%
          dplyr::count(.data[[var]]) %>%
          dplyr::mutate(
            Percent = round((n / sum(n)) * 100, 1)
          ) %>%
          dplyr::rename(Count = n)
      } else {
        # Grouped frequency table
        summary_table <- data %>%
          dplyr::count(.data[[by]], .data[[var]]) %>%
          dplyr::group_by(.data[[by]]) %>%
          dplyr::mutate(
            Percent = round((n / sum(n)) * 100, 1)
          ) %>%
          dplyr::rename(Count = n) %>%
          dplyr::ungroup()
      }
      return(summary_table)
    }

    if (output == "plot") {
      if (is.null(by)) {
        # Bar plot for single categorical variable
        plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[var]])) +
          ggplot2::geom_bar(fill = fill_color) +
          ggplot2::labs(
            x = var,
            y = "Count",
            title = paste("Distribution of", var)
          ) +
          theme_crpe()
      } else {
        # Grouped bar plot for categorical variable
        plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[var]], fill = .data[[by]])) +
          ggplot2::geom_bar(position = "dodge") +
          ggplot2::labs(
            x = var,
            y = "Count",
            fill = by,
            title = paste("Distribution of", var, "by", by)
          ) +
          theme_crpe()
      }
      return(plot)
    }
  }
}
