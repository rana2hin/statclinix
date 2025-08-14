#' Create a Boxplot with Statistical Comparisons
#'
#' This function generates a boxplot using ggpubr, comparing a numerical variable
#' across the categories of a factor variable. It automatically generates all pairwise
#' comparisons and allows for customization of p-values and labels.
#'
#' @param data A data.frame containing the data for plotting.
#' @param x_var A string with the name of the categorical (factor) column for the x-axis.
#' @param y_var A string with the name of the numerical column for the y-axis.
#' @param palette A character vector of colors for the plot palette.
#' @param xlab_text An optional string for the x-axis label.
#' @param ylab_text An optional string for the y-axis label.
#' @param title An optional string for the plot's main title.
#' @param subtitle An optional string for the plot's subtitle.
#' @param show_total_p A logical value (TRUE/FALSE) to show the overall p-value. Defaults to TRUE.
#' @param show_pairwise_p A logical value (TRUE/FALSE) to show pairwise comparison p-values. Defaults to TRUE.
#'
#' @returns A ggplot object.
#'
#' @importFrom ggplot2 element_blank element_line
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Default behavior
#' candy_box(
#'   data = iris,
#'   x_var = "Species",
#'   y_var = "Sepal.Width",
#'   xlab_text = "Iris Species",
#'   ylab_text = "Sepal Width (cm)"
#' )
#'
#' # Example 2: With most options turned off and custom titles
#' candy_box(
#'   data = iris,
#'   x_var = "Species",
#'   y_var = "Sepal.Width",
#'   show_pairwise_p = FALSE,
#'   show_total_p = FALSE,
#'   title = "Testing New Function",
#'   subtitle = "This is where the subtitle goes"
#' )
#'}
candy_box <- function(data, x_var, y_var,
                                    palette = c("#00AFBB", "#E7B800", "#FC4E07", "#A020F0"),
                                    xlab_text = NULL, ylab_text = NULL, title = NULL, subtitle = NULL,
                                    show_total_p = TRUE, show_pairwise_p = TRUE) {

  # 1. Automatically generate all unique pairwise comparisons
  categories <- unique(as.character(data[[x_var]]))
  my_comparisons <- utils::combn(categories, 2, simplify = FALSE)

  # 2. Dynamically calculate y-position for the total p-value label
  # Position it 10% above the maximum y-value
  max_y <- max(data[[y_var]], na.rm = TRUE)
  total_p_y_pos <- max_y + (max_y * 0.2)

  # 3. Create the base plot
  p <- ggpubr::ggboxplot(
    data,
    x = x_var,
    y = y_var,
    color = x_var,
    palette = palette,
    add = "jitter",
    shape = x_var
  ) + theme_2hin() + ggplot2::theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = .5,
                                      linetype = "dashed")
  )

  # 4. Conditionally add pairwise p-values
  if (show_pairwise_p) {
    p <- p + ggpubr::stat_compare_means(comparisons = my_comparisons)
  }

  # 5. Conditionally add the total p-value
  if (show_total_p) {
    p <- p + ggpubr::stat_compare_means(
      label.y = total_p_y_pos,
      aes(label = sprintf("P-value = %0.4f", after_stat(p)))
    )
  }

  # 6. Add fixed theme elements and custom labels
  p <- p +
    ggplot2::theme(legend.position = "none")

  if (!is.null(xlab_text)) {
    p <- p + ggplot2::xlab(xlab_text)
  }

  if (!is.null(ylab_text)) {
    p <- p + ggplot2::ylab(ylab_text)
  }

  if (!is.null(title)) {
    p <- p + ggplot2::labs(title = title)
  }

  if (!is.null(subtitle)) {
    p <- p + ggplot2::labs(subtitle = subtitle)
  }

  return(p)
}
