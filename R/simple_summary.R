#' Create a Formatted Summary Table
#'
#' This function uses `gtsummary` to create a publication-ready descriptive
#' summary table, with options for grouping by a variable, adding p-values,
#' and saving the output as a Word document.
#'
#' @param data A data.frame containing the data to summarize.
#' @param by_variable An optional string with the name of a column to group by.
#'   If provided, p-values will be calculated across groups.
#' @param add_overall A logical value. If `TRUE` and `by_variable` is used, an
#'   "Overall" column will be added to the table. Defaults to `FALSE`.
#' @param title An optional string for the table's main title.
#' @param subtitle An optional string for the table's subtitle.
#' @param save_as_word A logical value. If `TRUE`, the table will be saved as a
#'   .docx file. Defaults to `FALSE`.
#' @param filename A string specifying the filename for the saved Word document.
#'   Defaults to "summary_table.docx".
#'
#' @returns A `gt` table object.
#'
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' # Load the trial dataset for examples
#' trial_data <- gtsummary::trial
#'
#' # Example 1: Basic summary of selected columns
#' simple_summary(
#'   data = trial_data[, c("age", "grade", "response")],
#'   title = "Basic Patient Summary"
#' )
#'
#' # Example 2: Summary grouped by treatment, with an "Overall" column
#' simple_summary(
#'   data = trial_data[, c("age", "grade", "trt")],
#'   by_variable = "trt",
#'   add_overall = TRUE,
#'   title = "Patient Characteristics by Treatment"
#' )
#'
#' # Example 3: Grouped summary saved to a Word document
#' simple_summary(
#'   data = trial_data[, c("marker", "stage", "trt")],
#'   by_variable = "trt",
#'   save_as_word = TRUE,
#'   filename = "treatment_marker_summary.docx"
#' )
#'}
simple_summary <- function(data, by_variable = NULL, add_overall = FALSE, title = NULL, subtitle = NULL, save_as_word = FALSE, filename = "summary_table.docx") {
  # Check if the 'by_variable' exists in the data frame if it's provided
  if (!is.null(by_variable) && !by_variable %in% names(data)) {
    stop(paste0("Error: The column '", by_variable, "' was not found in the data frame."))
  }

  # Base table summary
  # The by argument is only added if by_variable is not NULL
  tbl <- gtsummary::tbl_summary(
    data,
    by = by_variable,
    statistic = list(
      gtsummary::all_continuous() ~ "{mean} ± {sd}",
      gtsummary::all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      gtsummary::all_continuous() ~ 2,
      gtsummary::all_categorical() ~ 2
    )
  )

  # If a 'by' variable exists, conditionally add overall and p-values
  if (!is.null(by_variable)) {
    if (add_overall) {
      tbl <- gtsummary::add_overall(tbl)
    }
    # As requested, add_p is TRUE if there is a by_variable
    tbl <- gtsummary::add_p(tbl)
  }

  # Convert to a gt object for styling and saving
  gt_object <- tbl %>%
    gtsummary::as_gt() %>%
    gt::tab_header(
      title = title,
      subtitle = subtitle
    ) %>%
    gt::opt_stylize(style = 2, color = "gray")


  # Conditionally save the table as a .docx file
  if (save_as_word) {
    gt::gtsave(gt_object, filename = filename)
    # Provide a message to the user confirming the save location
    message(paste("Table successfully saved to:", filename))
  }

  # Return the final gt table object
  return(gt_object)
}
