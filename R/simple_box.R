#' Create a customized boxplot using ggplot2.
#'
#' This function generates a boxplot with options for a user-defined single color
#' or multiple colors, along with customizable labels, title, and subtitle.
#'
#' @param data A data frame containing the plotting data.
#' @param x_var A string with the name of the column for the x-axis (categorical).
#' @param y_var A string with the name of the column for the y-axis (numerical).
#' @param xlab_text A string for the x-axis label. Defaults to the x_var name.
#' @param ylab_text A string for the y-axis label. Defaults to the y_var name.
#' @param title A string for the plot's main title. Defaults to NULL.
#' @param subtitle A string for the plot's subtitle. Defaults to NULL.
#' @param one_color A string specifying a color (e.g., "orange", "#FF8C00").
#'   If a color is provided, all boxes will use it. If NULL (the default),
#'   colors are mapped to the x-axis variable.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' # Example 1: Multi-color boxplot (one_color = NULL)
#' simple_box(
#'   data = mpg,
#'   x_var = "class",
#'   y_var = "hwy",
#'   title = "Highway MPG by Vehicle Class",
#'   subtitle = "Colors are mapped to vehicle class"
#' )
#'
#' # Example 2: Single-color boxplot with a specified color
#' simple_box(
#'   data = mpg,
#'   x_var = "class",
#'   y_var = "hwy",
#'   title = "Highway MPG by Vehicle Class",
#'   subtitle = "All boxes are a single color",
#'   one_color = "darkgreen"
#' )

simple_box <- function(data, x_var, y_var,
                       xlab_text = NULL, ylab_text = NULL,
                       title = NULL, subtitle = NULL,
                       one_color = NULL) {

  # Create the base ggplot object.
  # We use .data[[variable_name]] to correctly handle string inputs in aes().
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]]))

  # Conditionally apply coloring based on the 'one_color' parameter
  if (!is.null(one_color)) {
    # --- SINGLE COLOR PLOT ---
    p <- p + ggplot2::geom_boxplot(color = one_color, fill = one_color, alpha = 0.2, outlier.alpha = 0.1)
  } else {
    # --- MULTI-COLOR PLOT ---
    # Map the fill color to the x-axis variable
    p <- p +
      ggplot2::geom_boxplot(ggplot2::aes(fill = .data[[x_var]]), alpha = 0.3, outlier.alpha = 0.1) +
      ggplot2::scale_fill_brewer(palette = "Paired") # Apply a color palette
  }

  # Apply theme and customizations
  # Note: theme_2hin() appears to be a custom theme function.
  # Ensure it is loaded in your environment.
  p <- p +
    theme_2hin() +
    ggplot2::theme(
      legend.position = "none", # Hide the legend
      panel.grid.major.y = ggplot2::element_blank(), # Remove horizontal major grid lines
      panel.grid.minor.y = ggplot2::element_blank(), # Remove horizontal minor grid lines
      panel.grid.major.x = ggplot2::element_line(
        color = "gray90",
        linewidth = 0.5,
        linetype = "dashed"
      ), # Style vertical grid lines
      axis.title = ggplot2::element_text(face = "bold"), # Make axis titles bold
      plot.title = ggplot2::element_text(face = "bold", size = 16) # Style plot title
    )

  # Apply labels, title, and subtitle using your original logic
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

  # Return the final plot
  return(p)
}
