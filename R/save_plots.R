#' Save ggplot objects for CRPE Branding
#'
#' This wrapper function saves a ggplot object as either (1) a PNG file,
#' (2) an SVG file, or (3) an EPS file. The function creates subdirectories
#' for each format automatically if they do not already exist.
#'
#' @param plot A ggplot object to save.
#' @param file A character string specifying the desired file name (without extension).
#' @param dir A character string specifying the directory to save the file in.
#' If `NULL`, the function will default to the current working directory.
#' @param fig_width A numeric value specifying the width of the plot in inches. Default is 9.
#' @param fig_height A numeric value specifying the height of the plot in inches. Default is 7.
#' @param formats A character vector specifying the file format(s) to save.
#' Options are `"png"`, `"svg"`, and `"eps"`. Default is `c("png", "svg", "eps")`.
#'
#' @details
#' - PNG files are saved at 96 dpi for consistent on-screen and print quality.
#' - Subdirectories are automatically created within `dir` for each file format.
#' - The function will attempt to save each file type separately and will return
#'   a warning if a format fails to save.
#'
#' @examples
#' library(ggplot2)
#' plot <- ggplot(mpg, aes(x = class, fill = drv)) +
#'   geom_bar() +
#'   theme_minimal()
#'
#' # Save as PNG and SVG
#' save_plots(plot, file = "example_plot", formats = c("png", "svg"))
#'
#' @export
save_plots <- function(
    plot, file, dir = NULL,
    fig_width = 9, fig_height = 7, formats = c("png", "svg", "eps")
) {
  # Use default working directory if 'dir' is not specified
  dir <- dir %||% getwd()

  # Directories for each format
  png_dir <- file.path(dir, "png")
  svg_dir <- file.path(dir, "svg")
  eps_dir <- file.path(dir, "eps")

  # Ensure directories exist
  if ("png" %in% formats) dir.create(png_dir, recursive = TRUE, showWarnings = FALSE)
  if ("svg" %in% formats) dir.create(svg_dir, recursive = TRUE, showWarnings = FALSE)
  if ("eps" %in% formats) dir.create(eps_dir, recursive = TRUE, showWarnings = FALSE)

  # Save as PNG
  if ("png" %in% formats) {
    png_file <- file.path(png_dir, paste0(file, ".png"))
    tryCatch({
      png(filename = png_file, width = fig_width * 96, height = fig_height * 96, res = 96) # 96 dpi
      print(plot)  # Print plot to file
      dev.off()    # Close the file
      message(sprintf("Successfully saved PNG file to %s", png_file))
    }, error = function(e) {
      message("Failed to save PNG file: ", e$message)
    })
  }

  # Save as SVG
  if ("svg" %in% formats) {
    svg_file <- file.path(svg_dir, paste0(file, ".svg"))
    tryCatch({
      svg(filename = svg_file, width = fig_width, height = fig_height)
      print(plot)
      dev.off()
      message(sprintf("Successfully saved SVG file to %s", svg_file))
    }, error = function(e) {
      message("Failed to save SVG file: ", e$message)
    })
  }

  # Save as EPS
  if ("eps" %in% formats) {
    eps_file <- file.path(eps_dir, paste0(file, ".eps"))
    tryCatch({
      ggsave(filename = eps_file, plot = plot, device = "eps", width = fig_width, height = fig_height)
      message(sprintf("Successfully saved EPS file to %s", eps_file))
    }, error = function(e) {
      message("Failed to save EPS file: ", e$message)
    })
  }
}
