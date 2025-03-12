theme_transcend = googleway::theme_gdocs(base_size = 14, base_family = "Open Sans") +
  ggplot2::theme(
    plot.title = element_text(family = "Bebas Neue", color = "black"),
    plot.background = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.title = element_text(colour = "black"),
    panel.border = element_rect(colour = transcend_grays[1]),
    strip.text = element_text(size = rel(0.8)),
    plot.margin = margin(10, 24, 10, 10, "pt")
  )

theme_transcend_sparse = ggthemes::theme_few(base_size = 12, base_family = "Open Sans") +
  ggplot2::theme(
    plot.title = element_text(family = "Bebas Neue", colour = "black"),
    axis.text = element_text(colour = "black")
  )

