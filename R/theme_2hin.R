#' A Custom ggplot2 Theme
#'
#' A complete ggplot2 theme using the Times New Roman font family, with a style
#' similar to `theme_grey`.
#'
#' @param base_size The base font size in points.
#' @param base_family The base font family. Defaults to "Times New Roman", which
#'   is loaded with the package.
#'
#' @returns A ggplot theme object (`theme`).
#'
#' @importFrom ggplot2 theme element_line element_rect element_text element_blank margin unit rel
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' ggplot(mpg, aes(displ, hwy, color = class)) +
#'   geom_point() +
#'   labs(title = "Fuel Economy by Engine Displacement") +
#'   theme_2hin()
#'}
theme_2hin <- function(base_size = 12, base_family = "Times New Roman") {
  half_line <- base_size / 2
  ggplot2::theme(
    line = ggplot2::element_line(color = "black", linewidth = .5, linetype = 1, lineend = "butt"),
    rect = ggplot2::element_rect(fill = "white", color = "black", linewidth = .5, linetype = 1),
    text = ggplot2::element_text(
      family = base_family, face = "plain", color = "black", size = base_size,
      lineheight = .9, hjust = .5, vjust = .5, angle = 0,
      margin = ggplot2::margin(), debug = FALSE
    ),
    axis.line = ggplot2::element_blank(),
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = ggplot2::element_text(size = base_size, color = "gray30"),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = .8 * half_line / 2), vjust = 1),
    axis.text.x.top = ggplot2::element_text(margin = ggplot2::margin(b = .8 * half_line / 2), vjust = 0),
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = .8 * half_line / 2), hjust = 1),
    axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = .8 * half_line / 2), hjust = 0),
    axis.ticks = ggplot2::element_line(color = "gray30", linewidth = .7),
    axis.ticks.length = ggplot2::unit(half_line / 1.5, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(t = half_line), vjust = 1,
      size = base_size * 1.3, face = "bold"
    ),
    axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(b = half_line), vjust = 0),
    axis.title.y = ggplot2::element_text(
      angle = 90, vjust = 1, margin = ggplot2::margin(r = half_line),
      size = base_size * 1.3, face = "bold"
    ),
    axis.title.y.right = ggplot2::element_text(angle = -90, vjust = 0, margin = ggplot2::margin(l = half_line)),
    legend.background = ggplot2::element_rect(color = NA),
    legend.spacing = ggplot2::unit(.4, "cm"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin = ggplot2::margin(.2, .2, .2, .2, "cm"),
    legend.key = ggplot2::element_rect(fill = "gray95", color = "white"),
    legend.key.size = ggplot2::unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = ggplot2::element_text(size = ggplot2::rel(.8)),
    legend.text.align = NULL,
    legend.title = ggplot2::element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    legend.box.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
    legend.box.background = ggplot2::element_blank(),
    legend.box.spacing = ggplot2::unit(.4, "cm"),
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.border = ggplot2::element_rect(color = "gray30", fill = NA, linewidth = .7),
    panel.grid.major = ggplot2::element_line(color = "gray90", linewidth = 1),
    panel.grid.minor = ggplot2::element_line(color = "gray90", linewidth = .5, linetype = "dashed"),
    panel.spacing = ggplot2::unit(base_size, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = ggplot2::element_rect(fill = "white", color = "gray30"),
    strip.text = ggplot2::element_text(color = "black", size = base_size),
    strip.text.x = ggplot2::element_text(margin = ggplot2::margin(t = half_line, b = half_line)),
    strip.text.y = ggplot2::element_text(angle = -90, margin = ggplot2::margin(l = half_line, r = half_line)),
    strip.text.y.left = ggplot2::element_text(angle = 90),
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = ggplot2::unit(0.1, "cm"),
    strip.switch.pad.wrap = ggplot2::unit(0.1, "cm"),
    plot.background = ggplot2::element_rect(color = NA),
    plot.title = ggplot2::element_text(
      size = base_size * 1.8, hjust = .5, vjust = 1, face = "bold",
      margin = ggplot2::margin(b = half_line * 1.2)
    ),
    plot.title.position = "panel",
    plot.subtitle = ggplot2::element_text(
      size = base_size * 1.3, hjust = .5, vjust = 1,
      margin = ggplot2::margin(b = half_line * .9)
    ),
    plot.caption = ggplot2::element_text(
      size = ggplot2::rel(0.9), hjust = 1, vjust = 1,
      margin = ggplot2::margin(t = half_line * .9)
    ),
    plot.caption.position = "panel",
    plot.tag = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = .5, vjust = .5),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin(rep(base_size, 4)),
    complete = TRUE
  )
}
