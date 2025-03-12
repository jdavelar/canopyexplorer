#' Transcend style theme for ggplot objects
#'
#' This is a ggplot2 theme that applies Transcend branding style,
#' including expanded margins, specified font, and removed minor axis lines.
#' It should be used for bar plots, scatter plots, and line plots.
#'
#' @return A ggplot2 theme object.
#'
#' @examples
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   theme_transcend()
#'
#' @export
theme_transcend <- function() {
  # Font check
  if (!all(c("Open Sans", "Bebas Neue") %in% systemfonts::system_fonts()$family)) {
    warning(
      "Please install the fonts 'Open Sans' and 'Bebas Neue' from Google Fonts.\n",
      "You may need to restart your R session after installation."
    )
  }

  ggthemes::theme_gdocs(base_size = 14, base_family = "Open Sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(family = "Bebas Neue", color = "black"),
      plot.background = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = "black"),
      axis.title = ggplot2::element_text(colour = "black"),
      panel.border = ggplot2::element_rect(colour = "#4D4D4F"),
      strip.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
      plot.margin = ggplot2::margin(10, 24, 10, 10, "pt"),
      panel.grid.major.x = ggplot2::element_line(color = "grey80"),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey80"),
      panel.grid.minor.y = ggplot2::element_blank()
    )
}


#' Transcend sparse style theme for ggplot objects
#'
#' This is a simplified ggplot2 theme that applies Transcend branding style while
#' removing all axis lines. It is intended for minimalist plots.
#'
#' @return A ggplot2 theme object.
#'
#' @examples
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   theme_transcend_sparse()
#'
#' @export
theme_transcend_sparse <- function() {
  if (!all(c("Open Sans", "Bebas Neue") %in% systemfonts::system_fonts()$family)) {
    warning(
      "Please install the fonts 'Open Sans' and 'Bebas Neue' from Google Fonts.\n",
      "You may need to restart your R session after installation."
    )
  }

  ggthemes::theme_few(base_size = 12, base_family = "Open Sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(family = "Bebas Neue", colour = "black"),
      axis.text = ggplot2::element_text(colour = "black")
    )
}


#' Transcend color palettes for ggplot objects
#'
#' A wrapper function to make Transcend color palettes compatible with ggplot
#' objects when using categorical data. Supports both fill and color aesthetics.
#'
#' @param palette_name A character string indicating the color palette to use.
#' Options include:
#' \itemize{
#'  \item `"transcend_colors"`
#'  \item `"transcend_expanded"`
#'  \item `"transcend_grays"`
#'  \item `"transcend_na"`
#' }
#' @param type A character string indicating whether to apply the palette to the
#' fill or color aesthetic. Options are `"fill"` or `"color"`. Default is `"fill"`.
#' @param secondary_palette Optional. A character string for a secondary palette
#' if the primary palette runs out of colors.
#'
#' @return A ggplot2 scale object.
#'
#' @examples
#' ggplot(mpg, aes(class, fill = drv)) +
#'   geom_bar() +
#'   transcend_palette("transcend_colors", type = "fill", secondary_palette = "transcend_expanded")
#'
#' @export
transcend_palette <- function(palette_name, type = c("fill", "color"), secondary_palette = NULL) {
  type <- match.arg(type)

  transcend_palettes <- list(
    transcend_colors = c("#1A4C81", "#59C3B4", "#EF464B", "#ADE0EE"),
    transcend_grays = c("#4D4D4F", "#9D9FA2", "#D1D3D4"),
    transcend_expanded = c("#BC2582", "#FFA630", "#FFDE42", "#99C24D", "#218380", "#D3B7D7"),
    transcend_na = c("#9D9FA2")
  )

  if (!palette_name %in% names(transcend_palettes)) {
    stop("Palette not found. Please choose from: ", paste(names(transcend_palettes), collapse = ", "))
  }

  primary_colors <- transcend_palettes[[palette_name]]

  if (!is.null(secondary_palette) && secondary_palette %in% names(transcend_palettes)) {
    secondary_colors <- transcend_palettes[[secondary_palette]]
    combined_palette <- c(primary_colors, secondary_colors)
  } else {
    combined_palette <- rep(primary_colors, length.out = 10)
    warning("More categories than colors — recycling primary palette.")
  }

  if (type == "fill") {
    return(ggplot2::scale_fill_manual(values = combined_palette))
  } else {
    return(ggplot2::scale_color_manual(values = combined_palette))
  }
}
