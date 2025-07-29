#' Create a Horizontal Bar Chart
#'
#' Generates a sorted, horizontal bar chart. It can either calculate and plot
#' frequencies from a single categorical vector or plot pre-calculated values
#' from a data frame.
#'
#' @param data A data.frame, vector, or factor.
#' @param category_var A string with the name of the categorical column. Only used if `data` is a data.frame and `value_var` is also provided.
#' @param value_var An optional string with the name of a numeric column. If provided,
#'   its values will be used for bar lengths instead of calculating frequencies.
#' @param xlab_text An optional string for the x-axis label.
#' @param ylab_text An optional string for the y-axis label.
#' @param title An optional string for the plot's main title.
#' @param subtitle An optional string for the plot's subtitle.
#'
#' @returns A ggplot object.
#'
#' @importFrom ggplot2 element_blank
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Using a single categorical vector
#' diagnosis_data <- data.frame(
#'   diagnosis = c(
#'     rep("Infectious", 45),
#'     rep("Non infectious", 30),
#'     rep("Etiology not found", 15),
#'     rep("Co-infection", 22)
#'   )
#' )
#' choco_bar(
#'   diagnosis_data,
#'   title = "Frequency of Patient Diagnoses"
#' )
#'
#' # Example 2: Using a data frame with pre-calculated values
#' revenue_data <- data.frame(
#'   Department = c("Cardiology", "Neurology", "Oncology", "Orthopedics"),
#'   Revenue = c(4200000, 3100000, 5300000, 2800000)
#' )
#' choco_bar(
#'   revenue_data,
#'   category_var = "Department",
#'   value_var = "Revenue",
#'   title = "Departmental Revenue"
#' )
#'}
choco_bar <- function(data, category_var = NULL, value_var = NULL, xlab_text = NULL, ylab_text = NULL, title = NULL, subtitle = NULL) {

  # --- Data Preparation Step ---
  if (is.data.frame(data) && !is.null(category_var) && !is.null(value_var)) {
    # CASE 1: Data frame with specified columns.
    plot_data <- data.frame(
      category = data[[category_var]],
      count = data[[value_var]]
    )
    # Label will only show the provided value.
    plot_data$label <- as.character(plot_data$count)

  } else if (is.data.frame(data) || is.vector(data) || is.factor(data)) {
    # CASE 2: Single vector; frequencies will be calculated.
    plot_data <- as.data.frame(table(data))
    names(plot_data) <- c("category", "count")
    plot_data$percentage <- round((plot_data$count / sum(plot_data$count)) * 100, 1)
    plot_data$label <- paste0(plot_data$count, " (", plot_data$percentage, "%)")

  } else {
    stop("Invalid input. Please provide a single vector OR a data frame with 'category_var' and 'value_var'.")
  }

  # --- Plotting Step ---
  # Order the data for a clean visual and set factor levels to maintain order in ggplot
  plot_data <- plot_data[order(plot_data$count), ]
  plot_data$category <- factor(plot_data$category, levels = plot_data$category)

  # Calculate a dynamic upper limit for the axis to make space for labels
  upper_limit <- max(plot_data$count) * 1.3

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = category, y = count, fill = category)) +
    ggplot2::geom_bar(stat = "identity", show.legend = FALSE, width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = label, color = category),
      hjust = -0.1,  # Position text just outside the bar
      size = 4,
      show.legend = FALSE
    ) +
    ggpubr::theme_pubr() +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::scale_color_brewer(palette = "Set2") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(limits = c(0, upper_limit), expand = c(0, 0)) +
    ggplot2::labs(title = title) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
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
