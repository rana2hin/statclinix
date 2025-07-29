#' Create a Grouped Bar Chart
#'
#' Generates a dodged bar chart. It can either calculate frequencies from two
#' categorical variables or plot pre-calculated values from a numeric variable.
#'
#' @param data A data.frame containing the data.
#' @param x_col A string with the name of the primary categorical column for the x-axis.
#' @param group_col A string with the name of the column to group by and use for the fill color.
#' @param value_var An optional string with the name of a numeric column. If provided,
#'   its values will be used for bar heights instead of calculating frequencies.
#' @param xlab_text An optional string for the x-axis label. Defaults to the name of `x_col`.
#' @param ylab_text An optional string for the y-axis label.
#' @param title An optional string for the plot's main title.
#' @param subtitle An optional string for the plot's subtitle.
#'
#' @returns A ggplot object.
#'
#' @importFrom ggplot2 element_blank
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Calculating frequencies from categorical data
#' trial_data <- gtsummary::trial[, c("trt", "grade")]
#' choco_bar_gr(
#'   data = trial_data,
#'   x_col = "grade",
#'   group_col = "trt",
#'   title = "Distribution of Tumor Grade by Treatment Type"
#' )
#'
#' # Example 2: Using pre-calculated values
#' sales_data <- data.frame(
#'   Quarter = rep(c("Q1", "Q2", "Q3", "Q4"), 2),
#'   Region = rep(c("North", "South"), each = 4),
#'   Revenue = c(120, 150, 180, 210, 80, 95, 110, 130)
#' )
#' choco_bar_gr(
#'   data = sales_data,
#'   x_col = "Quarter",
#'   group_col = "Region",
#'   value_var = "Revenue",
#'   title = "Quarterly Revenue by Region (in thousands)"
#' )
#'}
choco_bar_gr <- function(data, x_col, group_col, value_var = NULL, xlab_text = NULL, ylab_text = NULL, title = NULL, subtitle = NULL) {

  # --- Data Preparation Step ---
  if (is.null(value_var)) {
    # CASE A: Calculate frequencies from two categorical columns
    if (!all(c(x_col, group_col) %in% names(data))) {
      stop("One or both specified columns not found in the data frame.")
    }
    plot_data <- as.data.frame(table(data[[x_col]], data[[group_col]]))
    names(plot_data) <- c("category", "group", "count")

    # Calculate percentage within each primary group
    plot_data <- plot_data %>%
      dplyr::group_by(group) %>%
      dplyr::mutate(percentage = round((count / sum(count)) * 100, 1)) %>%
      dplyr::ungroup()

    # Create label with count and percentage
    plot_data$label <- paste0(plot_data$count, " (", plot_data$percentage, "%)")

  } else {
    # CASE B: Use provided values from a numeric column
    if (!all(c(x_col, group_col, value_var) %in% names(data))) {
      stop("One or more specified columns not found in the data frame.")
    }
    plot_data <- data.frame(
      category = data[[x_col]],
      group = data[[group_col]],
      count = data[[value_var]]
    )
    # Label is just the raw value
    plot_data$label <- as.character(plot_data$count)
  }

  # --- Plotting Step ---
  # Calculate a dynamic upper limit for the axis to make space for labels
  upper_limit <- max(plot_data$count, na.rm = TRUE) * 1.25

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = category, y = count, fill = group)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      position = ggplot2::position_dodge(width = 0.7),
      angle = 90,    # Rotate text 90 degrees
      hjust = -0.2,  # Adjust position to be just above the bar
      vjust = 0.5,   # Center text vertically after rotation
      size = 3.5
    ) +
    ggpubr::theme_pubr() +
    ggplot2::scale_y_continuous(limits = c(0, upper_limit), expand = c(0, 0)) +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::labs(
      title = title,
      fill = group_col, # Use the original column name for the legend title
      x = x_col         # Use the original column name for the x-axis title
    ) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5)
    ) + theme_2hin() + ggplot2::theme(
      panel.grid = element_blank()
    )

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
