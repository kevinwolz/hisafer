#' A \code{ggplot2} theme for Hi-sAFe timeseries plots
#' @description The default ggplot2 theme for Hi-sAFe timeseries plots.
#' @return A ggplot modifier object containing the theme.
#' @param base_size A number indicating the base \code{ggplot} text size.
#' @import ggplot2
theme_hisafe_ts <- function(base_size = 18) {
  thm <- theme_bw(base_size = base_size) %+%
    theme(
      plot.margin       = unit(base_size * c(1,1,1,1), "points"),
      panel.grid.minor  = element_blank(),
      panel.border      = element_rect(size = 2,
                                       color = "black"),
      axis.line         = element_blank(),
      axis.ticks        = element_line(color = "black"),
      axis.ticks.length = unit(-(base_size * 0.5), "points"),
      axis.text         = element_text(color = "black"),
      axis.text.x       = element_text(margin = margin(t = (base_size * 0.8),
                                                       unit = "points")),
      axis.text.y       = element_text(margin = margin(r = (base_size * 0.8),
                                                       unit = "points")),
      axis.title.x      = element_text(vjust = -1),
      axis.title.y      = element_text(vjust = 2),
      plot.title        = element_text(hjust = 0.5,
                                       margin = margin(b = (base_size * 1), unit = "points")),
      legend.position   = "bottom",
      legend.title      = element_blank(),
      legend.key        = element_blank(),
      #legend.text      = element_text(margin = margin(r = 24, unit = "pt")), # does not work; known ggplot bug
      aspect.ratio      = 0.75
    )
}

#' A \code{ggplot2} theme for Hi-sAFe monthCells tile plots
#' @description The default ggplot2 theme for Hi-sAFe monthCells tile plots.
#' @return A ggplot modifier object containing the theme.
#' @param base_size A number indicating the base \code{ggplot} text size.
#' @import ggplot2
theme_hisafe_tile <- function(base_size = 18) {
  thm <- theme_bw(base_size = base_size) %+%
    theme(
      plot.margin      = unit(base_size * c(1,1,1,1), "points"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(size = 2,
                                      color = "black"),
      axis.line        = element_blank(),
      axis.ticks       = element_blank(),
      axis.text        = element_blank(),
      axis.title.x     = element_text(vjust = -1),
      axis.title.y     = element_text(vjust = 2),
      plot.title       = element_text(hjust = 0.5,
                                      margin = margin(b = (base_size * 1), unit = "points")),
      legend.position  = "bottom"
    )
}
