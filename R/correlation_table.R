#' Create and Optionally Save a Styled Correlation Table
#'
#' This function calculates Pearson correlation coefficients and p-values for pairs of
#' numeric variables in a data frame. It can operate on all possible pairs or on
#' pairs relative to a specified set of base variables. The output is a `gt` table,
#' which can also be saved as a .docx file.
#'
#' @param data A data.frame containing the data. Must have at least two numeric columns.
#' @param base An optional character vector of column names or a numeric vector of
#'   column indices to be used as the base for correlation. If NULL (the default),
#'   all-pairs correlations are computed.
#' @param base_to_base A logical value. If `TRUE` and `base` contains more than one
#'   variable, correlations between the base variables themselves are also calculated.
#'   Defaults to `FALSE`.
#' @param save_as_docx An optional character string. If provided, specifies the file
#'   path and name (e.g., "reports/correlation_table.docx") where the output table
#'   should be saved as a Word document. Defaults to `NULL`, in which case the table
#'   is not saved to a file.
#'
#' @return A `gt_tbl` object.
#' @importFrom dplyr %>%
#' @export
#'
#' @importFrom dplyr select where rename rowwise mutate bind_rows setdiff
#' @importFrom tidyr expand_grid
#' @importFrom gt gt fmt_number fmt tab_header tab_style cells_title cells_column_labels everything px tab_options gtsave
#' @importFrom stats cor.test
#' @importFrom utils combn
#'
#' @examples
#' # Create a sample data frame
#' set.seed(123)
#' sample_data <- data.frame(
#'   hs_CRP = rnorm(100),
#'   ALT = rnorm(100),
#'   AST = rnorm(100),
#'   FBG = rnorm(100),
#'   TG = rnorm(100),
#'   BMI = rnorm(100),
#'   NonNumeric = sample(letters, 100, replace = TRUE)
#' )
#'
#' # Example 1: Correlation of all numeric variables
#' # correlation_table(sample_data)
#'
#' # Example 2: Correlation with a single base variable
#' # correlation_table(sample_data, base = "hs_CRP")
#'
#' # Example 3: Correlation with multiple base variables
#' # correlation_table(sample_data, base = c("hs_CRP", "BMI"))
#'
#' # Example 4: Correlation with multiple base variables, including base-to-base
#' # correlation_table(sample_data, base = c("hs_CRP", "BMI"), base_to_base = TRUE)
#'
#' # Example 5: Save the output to a Word document
#' # temp_file <- tempfile(fileext = ".docx")
#' # correlation_table(sample_data, base = "hs_CRP", save_as_docx = temp_file)
#' # print(paste("Table saved to:", temp_file))

correlation_table <- function(data, base = NULL, base_to_base = FALSE, save_as_docx = NULL) {

  # --- 1. Input Validation and Preparation ---

  # Ensure the input is a data frame
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }

  # Identify numeric columns
  numeric_cols <- data %>%
    dplyr::select(where(is.numeric)) %>%
    colnames()

  # Stop if there are fewer than two numeric columns
  if (length(numeric_cols) < 2) {
    stop("The data frame must contain at least two numeric columns.")
  }

  # Convert base indices to names if necessary
  if (!is.null(base)) {
    if (is.numeric(base)) {
      base <- numeric_cols[base]
    }
    # Check if base columns exist in the data
    if (!all(base %in% numeric_cols)) {
      stop("One or more 'base' columns are not found in the numeric columns of the data frame.")
    }
  }

  # --- 2. Generate Variable Pairs for Correlation ---

  variable_pairs <- if (is.null(base)) {
    # If no base is specified, get all unique pairs of numeric columns
    utils::combn(numeric_cols, 2) %>%
      t() %>%
      as.data.frame() %>%
      dplyr::rename(Var1 = V1, Var2 = V2)
  } else {
    # If base is specified
    other_vars <- dplyr::setdiff(numeric_cols, base)

    # Create pairs between base variables and other variables
    base_other_pairs <- tidyr::expand_grid(Var1 = base, Var2 = other_vars)

    if (base_to_base && length(base) > 1) {
      # If base_to_base is TRUE, create pairs among base variables
      base_base_pairs <- utils::combn(base, 2) %>%
        t() %>%
        as.data.frame() %>%
        dplyr::rename(Var1 = V1, Var2 = V2)
      # Combine the two sets of pairs
      dplyr::bind_rows(base_other_pairs, base_base_pairs)
    } else {
      base_other_pairs
    }
  }

  # --- 3. Calculate Correlations and P-values ---

  correlation_results <- variable_pairs %>%
    # Use rowwise to iterate over each pair
    dplyr::rowwise() %>%
    # Calculate correlation test for each pair
    dplyr::mutate(corr_test = list(stats::cor.test(data[[Var1]], data[[Var2]], method = "pearson"))) %>%
    # Extract the correlation coefficient and p-value
    dplyr::mutate(
      `Correlation Coefficient (r)` = corr_test$estimate,
      `p-value` = corr_test$p.value
    ) %>%
    # Select and rename the final columns
    dplyr::select(
      `Variable 1` = Var1,
      `Variable 2` = Var2,
      `Correlation Coefficient (r)`,
      `p-value`
    )

  # --- 4. Create and Style the GT Table ---

  gt_table <- correlation_results %>%
    gt::gt() %>%
    # Format the correlation coefficient to 2 decimal places
    gt::fmt_number(
      columns = `Correlation Coefficient (r)`,
      decimals = 2
    ) %>%
    # Format the p-value
    gt::fmt(
      columns = `p-value`,
      fns = function(x) {
        ifelse(x < 0.001, "< 0.001", sprintf("%.3f", x))
      }
    ) %>%
    # Add a title to the table
    gt::tab_header(
      title = "Pearson Correlation Analysis"
    ) %>%
    # Style the table header
    gt::tab_style(
      style = list(
        gt::cell_text(weight = "bold")
      ),
      locations = gt::cells_title(groups = "title")
    ) %>%
    # Style the column labels
    gt::tab_style(
      style = list(
        gt::cell_text(weight = "bold")
      ),
      locations = gt::cells_column_labels(columns = gt::everything())
    ) %>%
    # Add options for a clean look
    gt::tab_options(
      table.border.top.color = "black",
      table.border.bottom.color = "black",
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width = gt::px(2)
    )

  # --- 5. Save Table to File (Optional) ---
  if (!is.null(save_as_docx)) {
    if (!is.character(save_as_docx)) {
      stop("'save_as_docx' must be a character string specifying the file path.")
    }
    gt::gtsave(data = gt_table, filename = save_as_docx)
  }

  return(gt_table)
}
