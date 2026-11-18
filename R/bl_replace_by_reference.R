
#' bl_replace_by_reference
#'
#' @description function to exchange values in a input vector with according values from a reference data.frame. This can be useful when placeholder varnames are used in an analyses that should be replaced consistently in all output elements (tables, graphs)
#'
#' @param input_vector a vector (of e.g. improvised variable names)
#' @param reference_dataframe a dataframe with two columns: 'reference' and "replacement'. Note that the column names are mandatory! Note also that the order of the rows does not need to match the order of the input vector (which is kept in the output). Note also that length and values in the reference column may vary from the input vector. According behavior is manages via 'complete'.
#' @param complete logical. Should non-referenced values of the input vector be kept in the output?
#' @export
#' @examples
#' # make a vector of squirky varnems
#' squirky_names <- c ("var_2", "var_1", "oh_var_3", "var_var_4")
#'
#' # make a reference data.frame
#' a_replacement_dataframe <- data.frame (reference = c ("var_1", "var_2", "oh_var_3", "var_var_5", "var_6"), replacement = c ("Variable 1", "Variable 2", "Variable 3", "Variable 4", "Variable 5"))
#'
#' # replace squirky names with corresponding values from reference data.frame
#' bl_replace_by_reference (squirky_names, a_replacement_dataframe, complete = F)
#' bl_replace_by_reference (squirky_names, a_replacement_dataframe, complete = T)

bl_replace_by_reference <- function (input_vector, reference_dataframe, complete = F) {
    # check if the reference_dataframe contains the columns "reference" and "replacement"
    if (!all(c("reference", "replacement") %in% colnames(reference_dataframe))) {
       #"reference" %in% colnames(reference_dataframe) & "replacement" %in% colnames(reference_dataframe)) {
        stop ("reference_dataframe needs to be a data.frame object and contain the columns 'reference' and 'replacement'")
    }
    target_df <- data.frame (reference = input_vector)
    filter_reference <- dplyr::filter(reference_dataframe, reference %in% target_df$reference)
    output_df <- target_df %>%
        dplyr::left_join(filter_reference, by = "reference")
    if (complete == F) {
        if (length (output_df$reference [is.na(output_df$replacement)]) > 0.5) {
            warning (paste0  ("The values", " '",
                            paste (output_df$reference [is.na(output_df$replacement)], collapse = "' and '"),
                            "' ",
                            "have no reference and were not replaced. Set 'complete = TRUE' to obtain NAs instead."
                            )
            )
            }
        output_df$replacement [is.na(output_df$replacement)] <- output_df$reference [is.na(output_df$replacement)]
            }

    return (output_df$replacement)
}


