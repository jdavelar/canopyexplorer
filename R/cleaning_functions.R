#' Cleaning function for grade data
#'
#' This function takes columns indicating the grade levels served by a school and creates
#' binary indicators for elementary, middle, and high school levels.
#'
#' @param data A data frame containing the grade data.
#' @param elementary_pattern A regular expression or vector of column names for elementary grades.
#' @param middle_pattern A regular expression or vector of column names for middle grades.
#' @param high_pattern A regular expression or vector of column names for high school grades.
#'
#' @details
#' - If the user does not specify patterns, the function defaults to searching for common
#'   grade-level patterns used in Canopy data.
#' - The function returns binary indicators (`1`/`0`) for each level.
#'
#' @return A data frame with `grades_elementary`, `grades_middle`, and `grades_high` columns added.
#'
#' @seealso \code{\link{load_canopy_data}} for loading Canopy datasets,
#' \code{\link{clean_practices}} for transforming columns documenting practices,
#' \code{\link{clean_locale}} for creating a column summarizing school geographic locale
#'
#' @examples
#' test_data <- data.frame(
#'   grades_k = c(1, 0, 0),
#'   grades_1 = c(1, 0, 1),
#'   grades_6 = c(0, 1, 0),
#'   grades_9 = c(0, 0, 1)
#' )
#'
#' clean_grades(test_data)
#'
#' @export

clean_grades <- function(data, prefix = "") {
  # Ensure it's a data frame
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }

  # Identify columns with the specified prefix + 'grade'
  grade_cols <- grep(paste0("^", prefix, ".*grade"), colnames(data), value = TRUE)

  # Harmonize Yes/No and numeric values
  data <- data %>%
    mutate(across(all_of(grade_cols), ~ {
      if (is.numeric(.)) {
        . # Keep numeric values unchanged
      } else {
        case_when(
          tolower(.) == "yes" ~ 1L,
          tolower(.) == "no" ~ 0L,
          . %in% c("", "Not reported") ~ NA_integer_,
          suppressWarnings(!is.na(as.numeric(.))) ~ as.integer(.),
          TRUE ~ NA_integer_
        )
      }
    }))

  # Extract numeric values or 'k'
  grade_levels <- stringr::str_extract(grade_cols, "(\\d+|k)")

  # Convert 'k' to 0, otherwise convert to numeric
  grade_levels <- ifelse(grade_levels == "k", 0, suppressWarnings(as.numeric(grade_levels)))

  # If no valid grades are detected, issue a warning
  if (all(is.na(grade_levels))) {
    warning("No valid grade columns detected. Check column names.")
  }

  # Group columns based on extracted grade level (ignore NAs when grouping)
  elementary_cols <- grade_cols[grade_levels %in% c(0, 1, 2, 3, 4, 5)]
  middle_cols <- grade_cols[grade_levels %in% c(6, 7, 8)]
  high_cols <- grade_cols[grade_levels %in% c(9, 10, 11, 12)]

  # Add binary indicators (converted to integer)
  data <- data %>%
    rowwise() %>%
    mutate(
      grades_elementary = as.integer(any(c_across(all_of(elementary_cols)) == 1)),
      grades_middle = as.integer(any(c_across(all_of(middle_cols)) == 1)),
      grades_high = as.integer(any(c_across(all_of(high_cols)) == 1))
    ) %>%
    ungroup()

  return(data)
}


#' Cleaning function for Canopy school data
#'
#' This function takes three numeric binary columns (0/1) indicating whether a
#' school serves urban, suburban, and rural locales, and collapses them into
#' a single categorical column labeled `locale`.
#'
#' The function assumes that the input binary columns are coded as `0` or `1`.
#' If multiple binary indicators are `1`, the function will classify the school
#' as `"Multiple"`. If none of the indicators are `1`, the value will be `NA`.
#'
#' @param data A data frame.
#' @param urban A string indicating the name of the column that specifies if a
#' school serves students in urban locales.
#' @param rural A string indicating the name of the column that specifies if a
#' school serves students in rural locales.
#' @param suburban A string indicating the name of the column that specifies if a
#' school serves students in suburban locales.
#'
#' @details
#' - If any binary column is not coded as `0/1`, a warning will be issued and
#'   the value will be coerced to `NA`.
#' - If multiple indicators are set to `1`, the function will return `"Multiple"`.
#' - If all indicators are `0` or `NA`, the function will return `NA`.
#'
#' @return A data frame with a new `locale` column summarizing the locale type.
#' Possible values are `"Urban"`, `"Suburban"`, `"Rural"`, `"Multiple"`, or `NA`.
#'
#' @seealso \code{\link{load_canopy_data}} for loading Canopy datasets,
#' \code{\link{clean_practices}} for transforming columns documenting practices,
#' \code{\link{clean_grades}} for creating a column summarizing school level
#'
#' @note If a `locale` column already exists in the input data, it will be
#' overwritten.
#'
#' @examples
#' # Example data
#' test_data <- data.frame(
#'   urban = c(1, 0, 0, 1, 0),
#'   rural = c(0, 1, 0, 1, 0),
#'   suburban = c(0, 0, 1, 0, 1)
#' )
#'
#' clean_locale(test_data, "urban", "rural", "suburban")
#'
#' @export

clean_locale <- function(data, urban, rural, suburban) {
  # Check input types
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }

  # Check that the columns exist
  cols <- as.character(c(urban, rural, suburban))
  names(data) <- trimws(names(data))

  missing_cols <- cols[!cols %in% names(data)]
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }

  # Check that the columns are numeric and binary (0/1)
  for (col in cols) {
    if (!is.numeric(data[[col]]) && !is.integer(data[[col]])) {
      stop(paste("Column", col, "must be numeric or integer."))
    }
    if (!all(data[[col]] %in% c(0, 1, NA))) {
      stop(paste("Column", col, "must contain only binary values (0/1) or NA."))
    }
  }

  # Create the locale column
  dat <- data %>%
    mutate(locale = case_when(
      .[[urban]] == 1 & .[[rural]] == 0 & .[[suburban]] == 0 ~ "Urban",
      .[[suburban]] == 1 & .[[rural]] == 0 & .[[urban]] == 0 ~ "Suburban",
      .[[rural]] == 1 & .[[urban]] == 0 & .[[suburban]] == 0 ~ "Rural",
      rowSums(as.data.frame(select(., all_of(cols))) == 1, na.rm = TRUE) > 1 ~ "Multiple",
      TRUE ~ NA_character_
    ))

  return(dat)
}


#' Cleaning function for tag data
#'
#' A cleaning function that takes the original values for all tags with a
#' specified prefix and returns 3 cleaned versions of the tag:
#' \itemize{
#'   \item `"practices_*"` — A binary variable indicating if a school selected a tag.
#'   \item `"core_*"` — A binary variable indicating a school selected a tag and
#'   indicated the practice is core to their school's design.
#'   \item `"time_*"` — A variable indicating the length of time the core practice
#'   has been in use at a schoolwide level.
#' }
#'
#' @param data A data frame containing the tag data.
#' @param prefix A character string indicating the prefix for the tag variables,
#' default `"practices_"`.
#'
#' @details
#' - This function creates three new columns for each original tag:
#'   - `"practices_*"` is `1` if the practice was selected, `0` otherwise.
#'   - `"core_*"` is `1` if the practice was marked as core to the school's design.
#'   - `"time_*"` reflects the length of time the core practice has been in place.
#' - If the original tag column contains `NA`, the output will be `NA`.
#'
#' @return A data frame with the new `practices_*`, `core_*`, and `time_*` columns
#' added.
#'
#' @seealso \code{\link{load_canopy_data}} for loading Canopy datasets,
#' \code{\link{clean_locale}} for creating a column summarizing school geographic locale,
#' \code{\link{clean_grades}} for creating a column summarizing school level
#'
#' @note If output columns already exist, they will be overwritten.
#'
#' @examples
#' test_data <- data.frame(
#'   practices_math = c("1-2 years", "Less than a year", "Not sure", NA, "5+ years"),
#'   practices_reading = c("3-4 years", "5+ years", "Less than a year", "1", "Not sure")
#' )
#'
#' clean_practices(test_data, prefix = "practices_")
#'
#' @export

clean_practices <- function(data, prefix = "practices_") {
  # Check input types
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }

  if (!is.character(prefix) || length(prefix) != 1) {
    stop("`prefix` must be a single character string.")
  }

  # Select columns with the specified prefix
  original_names <- names(data)[grepl(paste0("^", prefix), names(data))]

  if (length(original_names) == 0) {
    stop(paste("No columns found with prefix:", prefix))
  }

  for (col in original_names) {
    base <- sub(prefix, "", col)

    # Create core_* column
    data[[paste0("core_", base)]] <- dplyr::case_when(
      data[[col]] %in% c("Less than a year", "1-2 years", "3-4 years", "5+ years", "Not sure") ~ 1,
      TRUE ~ 0
    )

    # Create time_* column (set non-core to NA instead of "0" for consistency)
    data[[paste0("time_", base)]] <- dplyr::case_when(
      data[[col]] %in% c("Less than a year", "1-2 years", "3-4 years", "5+ years", "Not sure") ~ as.character(data[[col]]),
      TRUE ~ NA_character_
    )

    # Create practices_* column
    data[[paste0(prefix, base)]] <- dplyr::case_when(
      data[[col]] %in% c("Less than a year", "1-2 years", "3-4 years", "5+ years", "Not sure", "1") ~ 1,
      is.na(data[[col]]) ~ NA,
      TRUE ~ 0
    )
  }

  return(data)
}
