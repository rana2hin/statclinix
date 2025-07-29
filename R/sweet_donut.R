#' Create a Donut Chart
#'
#' Generates a donut chart from either a single categorical vector (by calculating
#' frequencies) or from a data frame with pre-supplied categories and values.
#'
#' @param data A data.frame, vector, or factor.
#' @param category_var A string with the name of the categorical column. Only used if `data` is a data.frame and `value_var` is also provided.
#' @param value_var An optional string with the name of a numeric column. If provided,
#'   its values will be used for slice sizes instead of calculating frequencies.
#' @param title An optional string for the plot's main title.
#'
#' @returns A ggplot object.
#'
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Using a single categorical vector
#' diagnosis_data <- data.frame(
#'   diagnosis = as.factor(c(
#'     rep("Infectious", 45),
#'     rep("Non infectious", 30),
#'     rep("Etiology not found", 15)
#'   ))
#' )
#' sweet_donut(
#'   diagnosis_data,
#'   title = "Distribution of HIV Patient Diagnoses"
#' )
#'
#' # Example 2: Using a data frame with pre-calculated values
#' sales_data <- data.frame(
#'   Product = c("Laptops", "Monitors", "Keyboards", "Mice"),
#'   Sales = c(250000, 175000, 80000, 65000)
#' )
#' sweet_donut(
#'   sales_data,
#'   category_var = "Product",
#'   value_var = "Sales",
#'   title = "Total Sales by Product Category"
#' )
#'}
sweet_donut <- function(data, category_var = NULL, value_var = NULL, title = "Default Title") {

  # --- Data Preparation Step ---
  # Check the type of input and prepare a standardized data frame for plotting.

  if (is.data.frame(data) && !is.null(category_var) && !is.null(value_var)) {
    # CASE 1: A two-column data frame with specified category and value columns.
    plot_data <- data.frame(
      category = data[[category_var]],
      count = data[[value_var]]
    )

    # Create labels showing the actual value, not a percentage.
    plot_data$label <- paste0(plot_data$category, "\nValue: ", plot_data$count)

  } else if (is.data.frame(data) || is.vector(data) || is.factor(data)) {
    # CASE 2: A single vector. Frequencies will be calculated.
    plot_data <- as.data.frame(table(data))
    names(plot_data) <- c("category", "count")

    # Calculate percentages for the label.
    plot_data$percentage <- round((plot_data$count / sum(plot_data$count)) * 100, 1)
    plot_data$label <- paste0(plot_data$category, "\n", plot_data$count, " (", plot_data$percentage, "%)")

  } else {
    stop("Invalid input. Please provide a single vector OR a data frame with 'category_var' and 'value_var'.")
  }


  # --- Common ggplot Calculations (for positioning slices and labels) ---
  plot_data <- plot_data[order(plot_data$count), ] # Sort for better visual separation
  plot_data$fraction <- plot_data$count / sum(plot_data$count)
  plot_data$ymax <- cumsum(plot_data$fraction)
  plot_data$ymin <- c(0, head(plot_data$ymax, n = -1))
  plot_data$labelPosition <- (plot_data$ymax + plot_data$ymin) / 2


  # --- Plotting Step ---
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
    ggplot2::geom_rect(color = "white") +
    ggrepel::geom_label_repel(
      x = 3.5,
      ggplot2::aes(y = labelPosition, label = label),
      size = 4,
      color = "white",
      max.overlaps = Inf # Ensure all labels are shown
    ) +
    ggplot2::scale_fill_brewer(palette = "Paired") +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::xlim(c(1.5, 4.5)) + # Adjust xlim for better label spacing
    ggplot2::theme_void() +
    ggplot2::labs(title = title) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5)
    )

  return(p)
}
