
#' bl_order_by_reference
#'
#' @description orders a vector or dataframe according to a reference vector or factor. This is useful to guarantee consistent order of output tables, inpedenendtly of e.g. variable sorting in MuMIn's 'dredge'.
#'
#' @param input a vector or data.frame
#' @param order_column if 'input' is a data.frame, the column to be ordered can be specified via 'order_column'. Else, the first column determines the reference of a data.frame.
#' @param reference a vector or factor containing the correct order. May not contain duplicates. It is recommended but not necessary that 'reference' contains all values of the input.
#' @param factor_levels if 'reference' is a factor, should the input be ordered by the factor levels (TRUE) or the order of the elements (FALSE) in the factor? Defaults to 'TRUE'
#' @param complete determines how values are treated that are present in the 'input', but ,issing in 'reference'. 'TRUE' (the default) appends these values at the end of the output. 'FALSE' omits these values.
#' @export
#' @examples
#' # create data.frame
#' df <- data.frame ('variable' = c ("first_var", "fourth_var", "third_var", "sixth_var", "fifth_var"), 'value' = c (rnorm (5)))
#' # create vector of order
#' vector_of_order <- c ("first_var", "second_var", "fourth_var", "fifth_var", "sixth_var")
#'
#' # look at the data.frame
#' df
#'
#' # order: note that "third_var" is not included in "vector_of_order" and thus appended
#' bl_order_by_reference (input = df, order_column = 'variable', reference = vector_of_order)
#'
#' # order: note that "third_var" is not included in "vector_of_order" and thus supressed
#' bl_order_by_reference (input = df, order_column = 'variable', reference = vector_of_order, complete = F)
#'


bl_order_by_reference <- function (input = NA, order_column = NA, reference=NA, factor_levels = T, complete = T, keep_col_order = T) {
    # check if reference is a vector or factor
    if (is.factor(reference)) {
        if (factor_levels == T) { # check if factor_levels == TRUE --> order according to levels. Else, order accoring to order of the factor
        reference <- levels (reference)
        } else if (factor_levels == F) {
            reference <- reference %>% as.vector ()
        }
    }
    if (!is.vector (reference)) {
        stop ("'reference' needs to be a vector or factor")
    }
    # check if there are duplicates in "reference"
    if (length (reference [duplicated (reference)]) > 0.5) {
        stop ("There are duplicates in 'reference'.")
    }
    # if input is a data.frame, convert "order_column" to vector
    input_is_frame = F
    if (is.data.frame(input)) {
        input_is_frame = T
        # if "order_column" is not specified, choose the first column of the data.frame
        if (identical(order_column, NA)) {
            order_column <- 1
        }
        input_vector <- input [[order_column]]
    } else {
        input_vector <- input
    }

    # order the input values according to reference
    correct_order <- data.frame(
                            name = reference[reference %in% input_vector]
                            )

    # make data.frame with column "name" from input values
    input_values <- data.frame (input_vector)
    names (input_values) <- "name"

    # create output vector (ordered)
    ordered_vector <- dplyr::left_join (correct_order, input_values, by = "name") [[1]]

    # check for duplicates in ordered vector
    if (length (ordered_vector [duplicated (ordered_vector)]) > 0.5) {
        warning ("input to be ordered has duplicates. Consider re-naming.")
        ordered_vector [duplicated (ordered_vector)] <- paste0 (ordered_vector [duplicated (ordered_vector)],"_", c(1:length (ordered_vector [duplicated (ordered_vector)])))
    }

    # get values from input that are not included in "reference"
    not_sorted_vales <- input_values [!input_values[[1]] %in% correct_order [[1]],]

    # append values from input that are not included in "reference"
    if (complete == T) {
      ordered_vector <- c (ordered_vector, not_sorted_vales)
      if (length (not_sorted_vales) > 0.5) {
          message (paste0 (not_sorted_values_message(not_sorted_vales),
                           ". They are appended at the end of the ordered output according to their input order.
                           In order to supress these values, set 'complete = FALSE'"))
      }
    } else if (length (not_sorted_vales) > 0.5) {
          message (paste0 (not_sorted_values_message(not_sorted_vales),
                           "and are suppressed in the ordered output. To append them at the end of the ordered output, set 'complete = TRUE'"))
      }

    # create output dataframe (if input is dataframe)
    if (isTRUE(input_is_frame)) {
        final_order <- data.frame (name=ordered_vector)

        # make dataframe from input dataframe to be joined with the final order dataframe
        input_helper <- data.frame (input, check.names = FALSE)
        if (is.character(order_column)) {
            names(input_helper)[names(input_helper) == order_column] <- 'name'
        }
        if (is.numeric (order_column)) {
            names(input_helper)[round (order_column,0)] <- 'name'
        }


        # check for duplicates in input_helper$name
        if (length (input_helper$name [duplicated (input_helper$name)]) > 0.5) {
        input_helper$name [duplicated (input_helper$name)] <- paste0 (input_helper$name [duplicated (input_helper$name)],"_", c(1:length (input_helper$name [duplicated (input_helper$name)])))
        }

        # create output dataframe by joining the final order and the input_helper
        the_output <- dplyr::left_join (final_order, input_helper, by = "name")

        # replace "name" as name for the first column (which contains the values of the column to be ordered in the input dataframe) with the original column name
        names (the_output)[1] <- order_column

        # order the output columns according to the input columns
        if (keep_col_order == T) {
            the_output <- the_output[, colnames(input)]
        }
    } else {
       the_output <- ordered_vector
    }

    return (the_output)
}





# helper functions
not_sorted_values_message <- function(not_sorted_values) {
  if (length(not_sorted_values) <= 3) {
    message(paste0("Values '",
                   paste(not_sorted_values, collapse = "' and '"),
                   "' are not included in 'reference'"))
  } else {
    message(paste0("Values '",
                   paste(not_sorted_values[1:3], collapse = "', '"),
                   "' and ",
                   length(not_sorted_values) - 3,
                   " more values are not included in 'reference'"))
  }
}

