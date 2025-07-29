#' Create a Scatter Plot with Correlation Analysis
#'
#' This function takes a data frame with two numeric columns, creates a scatter plot,
#' adds a linear regression line, and displays the Pearson correlation coefficient (r)
#' and p-value on the plot. It can also return a descriptive sentence of the results.
#'
#' @param data A data.frame with exactly two numeric columns.
#' @param xlab_text An optional string for the x-axis label.
#' @param ylab_text An optional string for the y-axis label.
#' @param text_output A logical value. If `TRUE`, the function returns a list
#'   containing both the plot and a descriptive sentence. If `FALSE` (the default),
#'   it returns only the plot object.
#'
#' @returns By default, a `ggplot` object. If `text_output = TRUE`, a `list`
#'   containing `scatter_plot` (the ggplot object) and `correlation_sentence`
#'   (a character string).
#'
#' @importFrom ggplot2 element_text
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Get only the plot (default behavior)
#' plot_object <- sugar_plot(iris[, c("Sepal.Length", "Petal.Length")])
#' print(plot_object)
#'
#' # Example 2: Get both the plot and the descriptive sentence
#' output_list <- sugar_plot(
#'   iris[, c("Sepal.Length", "Petal.Length")],
#'   text_output = TRUE
#' )
#' cat(output_list$correlation_sentence)
#' print(output_list$scatter_plot)
#'}
sugar_plot <- function(data, xlab_text = NULL, ylab_text = NULL, text_output = FALSE) {
  # Check if the dataframe has exactly two numeric columns
  if (ncol(data) != 2 || !all(sapply(data, is.numeric))) {
    stop("Input dataframe must have exactly two numeric columns.")
  }

  # Extract column names
  var1_name <- colnames(data)[1]
  var2_name <- colnames(data)[2]

  # --- Correlation Analysis ---
  correlation_result <- stats::cor.test(data[[var1_name]], data[[var2_name]])
  r_value <- correlation_result$estimate
  p_value <- correlation_result$p.value

  # --- Build the Descriptive Sentence ---
  r_abs <- abs(r_value)
  strength_desc <- dplyr::case_when(
    r_abs >= 0.8 ~ "very strong",
    r_abs >= 0.6 ~ "strong",
    r_abs >= 0.4 ~ "moderate",
    r_abs >= 0.2 ~ "weak",
    TRUE ~ "very weak"
  )
  direction_desc <- ifelse(r_value > 0, "positive", "negative")
  p_value_formatted <- ifelse(p_value < 0.001, "< 0.001", paste0("= ", as.character(round(p_value, 3))))
  significance_desc <- ifelse(p_value < 0.05, "statistically significant", "not statistically significant")

  correlation_sentence <- glue::glue(
    "A Pearson correlation test revealed a {strength_desc}, {direction_desc} association between {var1_name} and {var2_name}, ",
    "which was found to be {significance_desc} (r = {round(r_value, 2)}, p {p_value_formatted})."
  )

  # --- Create the Plot ---
  label_sentence <- glue::glue("**r =** {round(r_value, 2)}<br>**p** {p_value_formatted}")
  midpoint_x <- mean(range(data[[var1_name]], na.rm = TRUE))
  midpoint_y <- mean(range(data[[var2_name]], na.rm = TRUE))

  scatter_plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[var1_name]], y = .data[[var2_name]])) +
    ggplot2::geom_point(size = 1.5, color = "#A020F0") +
    ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#FC4E07") +
    ggplot2::labs(
      title = glue::glue("Correlation between {var1_name} and {var2_name}"),
      x = var1_name,
      y = var2_name
    ) +
    ggtext::geom_richtext(
      x = midpoint_x, y = midpoint_y,
      label = label_sentence,
      color = "white", fill = "#00AFBB", stat = "unique"
    ) +
    theme_2hin() +
    ggplot2::theme(
      plot.title = element_text(size = 18), # Set plot title font size
      axis.title = element_text(face = "plain") # Remove bold from axis titles
    )

  if (!is.null(xlab_text)) {
    scatter_plot <- scatter_plot + ggplot2::xlab(xlab_text)
  }

  if (!is.null(ylab_text)) {
    scatter_plot <- scatter_plot + ggplot2::ylab(ylab_text)
  }

  # --- Conditional Return ---
  if (text_output) {
    return(list(correlation_sentence = correlation_sentence, scatter_plot = scatter_plot))
  } else {
    return(scatter_plot)
  }
}
