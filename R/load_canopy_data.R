#' Load Canopy datasets
#'
#' This is a function to load publicly-available Canopy datasets. These datasets
#' can also be found and manually downloaded at canopyschools.org.
#'
#' @param year specifies which year's data you would like to load. Options
#' currently include 2019, 2021a, 2021b, 2022, 2023, or 2024. Note: there is no
#' data set for 2020 due to COVID. 2021a represents the typical spring school
#' survey, and 2021b represents a follow-up survey that Canopy conducted to
#' track rapid changes during COVID-era school responses.
#'
#' @details
#' - Data structure may vary slightly between years due to changes in survey
#'   design and reporting conventions.
#'
#' @return A data frame containing the selected Canopy dataset.
#'
#' @export
load_canopy_data <- function(year) {
  valid_years <- c("2019", "2021a", "2021b", "2022", "2023", "2024")

  if (!year %in% valid_years) {
    stop(paste("Invalid year. Please choose from:", paste(valid_years, collapse = ", ")))
  }

  # Build file path based on selected year
  file_name <- paste0("canopy_", year, ".csv")
  file_path <- system.file("extdata", file_name, package = "canopyexplorer")

  # Check if file path exists
  if (file_path == "") {
    stop(paste("Data file for", year, "not found."))
  }

  # Load data
  data <- read.csv(file_path)
  return(data)
}
