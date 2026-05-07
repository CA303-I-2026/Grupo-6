jama <- pal_jama()(5)

estilo_grupo <- function() {
  theme_bw(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 14, hjust = 0),
      plot.subtitle    = element_text(size = 12, color = "grey40"),
      axis.title       = element_text(size = 12),
      axis.text        = element_text(size = 11),
      legend.title     = element_text(size = 12, face = "bold"),
      legend.text      = element_text(size = 11),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey92", color = NA),
      strip.text       = element_text(size = 11, face = "bold")
    )
}