#' CRPE style theme for ggplot objects
#'
#' This is a ggplot2 theme that applies CRPE branding to a plot, including
#' custom fonts, expanded margins, and removal of minor axis lines and padding.
#' It is designed for bar plots, scatter plots, and line plots.
#'
#' @param expand_x Logical. Whether x-axis padding should be removed when
#' using continuous variables. Cannot be used with discrete variables.
#' Default is `FALSE`.
#' @param expand_y Logical. Whether y-axis padding should be removed when
#' using continuous variables. Cannot be used with discrete variables.
#' Default is `FALSE`.
#' @param percent_labels Logical. Whether the y-axis should display percentage
#' labels (rounded to the nearest whole number). Default is `FALSE`.
#' @param x_var A vector representing the x-axis data. Only used to validate
#' whether `expand_x` is applicable.
#' @param y_var A vector representing the y-axis data. Only used to validate
#' whether `expand_y` is applicable.
#'
#' @return A ggplot2 theme object.
#'
#' @examples
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   theme_crpe(expand_x = TRUE, expand_y = TRUE)
#'
#' @export
theme_crpe <- function(percent_labels = FALSE) {
  # Base theme
  base_theme <- ggthemes::theme_gdocs(base_size = 14, base_family = "Open Sans") +
    theme(
      plot.title = element_text(family = "Helvetica Bold", color = "black"),
      plot.background = element_blank(),
      axis.text = element_text(colour = "black"),
      axis.title = element_text(colour = "black"),
      panel.border = element_rect(colour = "#4D4D4F"),
      strip.text = element_text(size = rel(0.8)),
      plot.margin = margin(10, 24, 10, 10, "pt")
    )

  # Conditional y-axis percent labels
  if (percent_labels) {
    base_theme <- base_theme + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  }

  return(base_theme)
}

#' Minimal CRPE style theme for ggplot objects
#'
#' This is a simplified ggplot2 theme that applies CRPE branding while
#' removing all axis lines and simplifying the plot's visual appearance.
#' It is intended for minimalist plots where gridlines and borders
#' are unnecessary.
#'
#' @return A ggplot2 theme object.
#'
#' @examples
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   theme_crpe_sparse()
#'
#' @export
theme_crpe_sparse = function() {
  ggthemes::theme_few(base_size = 14, base_family = "Open Sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(family = "Helvetica Bold", color = "black"),
      axis.text = ggplot2::element_text(colour = "black")
    )
}

#' Expand axes for ggplot objects
#'
#' This function removes padding from the x and/or y axes in ggplot objects
#' when using continuous variables. It is designed to be used in combination
#' with `theme_crpe()` to adjust the axis expansion behavior.
#'
#' @param expand_x Logical. If `TRUE`, removes padding from the x-axis when using
#' continuous variables. Cannot be used with discrete variables. Default is `FALSE`.
#' @param expand_y Logical. If `TRUE`, removes padding from the y-axis when using
#' continuous variables. Cannot be used with discrete variables. Default is `FALSE`.
#'
#' @return A list of ggplot scale objects that can be added to a ggplot object.
#'
#' @examples
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   theme_crpe() +
#'   expand_crpe_axes(expand_x = TRUE, expand_y = TRUE)
#'
#' @seealso \code{\link{theme_crpe}} for the CRPE theme.
#'
#' @export
expand_axes <- function(expand_x = FALSE, expand_y = FALSE) {
  expansion <- list()

  if (expand_x) {
    expansion <- append(expansion, scale_x_continuous(expand = c(0, 0)))
  }

  if (expand_y) {
    expansion <- append(expansion, scale_y_continuous(expand = c(0, 0)))
  }

  return(expansion)
}


#' CRPE color palettes for ggplot objects
#'
#' A wrapper function to make CRPE color palettes compatible with ggplot
#' objects when using categorical data. Supports both fill and color aesthetics.
#' Allows for a secondary palette if the primary palette does not have enough colors.
#'
#' @param palette_name A character string indicating the primary color palette to use.
#' Options include:
#' \itemize{
#'  \item `"crpe_blues"`
#'  \item `"crpe_oranges"`
#'  \item `"crpe_purples"`
#'  \item `"crpe_grays"`
#' }
#' @param type A character string indicating whether to apply the palette to the
#' fill or color aesthetic. Options are `"fill"` or `"color"`. Default is `"fill"`.
#' @param secondary_palette A character string indicating the secondary palette to use
#' if the primary palette runs out of colors. If `NULL`, the primary palette will be
#' recycled. Options are the same as `palette_name`.
#'
#' @return A ggplot2 scale object.
#'
#' @examples
#' # Use for fill aesthetic with overflow handled by secondary palette
#' ggplot(mpg, aes(class, fill = drv)) +
#'   geom_bar() +
#'   crpe_palette("crpe_blues", type = "fill", secondary_palette = "crpe_oranges")
#'
#' # Use for color aesthetic
#' ggplot(mpg, aes(displ, hwy, color = drv)) +
#'   geom_point() +
#'   crpe_palette("crpe_oranges", type = "color")
#'
#' @export
crpe_palette <- function(palette_name, type = c("fill", "color"), secondary_palette = NULL) {
  type <- match.arg(type)

  crpe_palettes <- list(
    crpe_blues = c("#0178BA", "#62A3D0", "#A1C9E6"),
    crpe_oranges = c("#E38C34", "#F5C37D", "#F5C37D"),
    crpe_grays = c("#D9D9D9", "#E6E6E6", "#F2F2F2"),
    crpe_purples = c("#9467BD", "#B08CD3", "#C8B3E2")
  )

  # Validate primary palette
  if (!palette_name %in% names(crpe_palettes)) {
    stop("Palette not found. Please choose from: ", paste(names(crpe_palettes), collapse = ", "))
  }

  primary_colors <- crpe_palettes[[palette_name]]

  # Handle overflow with a secondary palette
  if (!is.null(secondary_palette)) {
    if (!secondary_palette %in% names(crpe_palettes)) {
      stop("Secondary palette not found. Please choose from: ", paste(names(crpe_palettes), collapse = ", "))
    }
    secondary_colors <- crpe_palettes[[secondary_palette]]
    combined_palette <- c(primary_colors, secondary_colors)
  } else {
    combined_palette <- rep(primary_colors, length.out = 10) # Extend primary palette up to 10 values
    warning("More categories than colors in the palette — primary palette will be recycled.")
  }

  # Return the correct scale
  if (type == "fill") {
    return(ggplot2::scale_fill_manual(values = combined_palette))
  } else {
    return(ggplot2::scale_color_manual(values = combined_palette))
  }
}
