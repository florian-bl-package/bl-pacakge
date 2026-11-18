
#' bl_add_symmetric_jitter
#'
#' @description function to add symmetric jitter to presence / absence data. can be useful for plotting. Works for 1 -- 0  presence -- absence data so far with inside symmetry only.
#'
#' @param data_table a data.frame
#' @param variable the variable (presence -- absence) that shall be jittered
#' @param co_var a co-variable the jitter should be based on - jitter is only applied to duplicates in variable - co_var pairs.
#' @export
#' @examples
#' # Set seed for reproducibility
#' set.seed(123)
#'
#' # Create example dataset
#' example_data <- data.frame(
#'   presence = sample(c(0, 1), 40, replace = TRUE),
#'   var1 = sample(0:10, 40, replace = TRUE),
#'   var2 = sample(0:10, 40, replace = TRUE)
#' )
#'
#' bl_add_symmetric_jitter (example_data, variable = "presence", co_var = "var1")

bl_add_symmetric_jitter <- function (data_table, variable, co_var, lower = 0, upper = 1, symmetry = "inside", jitter = 0.01) {

    data_table$knurpsl <- data_table[[variable]]
    data_table$covarls <- data_table[[co_var]]

    data_table <- data_table %>%
        dplyr::group_by(knurpsl, covarls) %>%
        dplyr::mutate(symmetric_jitter =   ifelse(knurpsl == lower, (seq_along(covarls) - 1) * jitter, upper - (seq_along(covarls) - 1) * jitter)) %>%
        dplyr::ungroup() %>% dplyr::select(-c ("knurpsl", "covarls"))

    return (data_table$symmetric_jitter)
}

