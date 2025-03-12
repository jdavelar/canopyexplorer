#' Standard Transcend save of plots/data
#'
#' This is a ggsave wrapper that will save (1) a PNG file, (2) a SVG file,
#' and (3) optionally a CSV file with the data from the plot so it can
#' be easily referenced.
#'
#' @param plot a ggplot object to save
#' @param file the file name
#' @param dir the directory to save in
#' @param fig_width width in inches, default 9
#' @param fig_height height in inches, default 7
#' @param write_data boolean indicating whether or not to write a CSV with data,
#' default is TRUE.
ggsave_transcend = function(
    plot, file, dir = here("images"),
    fig_width = 9, fig_height = 7, write_data = TRUE,
    exts = c("png", "svg")
) {
  for (ext in exts) {
    ggsave(filename = sprintf("%s/%s.%s", dir, file, ext),
           plot = plot,
           width = fig_width, height = fig_height)
  }
  if(write_data) write_csv(plot$data, file = sprintf("%s/%s_data.csv", dir, file))
}
