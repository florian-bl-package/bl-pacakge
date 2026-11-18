#' bl summary functions
#' @description
#' * `bl_get_mean_SE_via_GLM` calculates a glm that puts out mean and SE from a vector
#'
#' * `bl_get_mean_SE_by_column` calculates mean and SE for each column in a data.frame using glms
#'
#' * `bl_make_mean_SE_by_column_table` makes a neat mean ± SE Table considering each column of a data.frame
#'
#' * `bl_make_mean_SD_by_column_table` makes a neat mean ± SD Table considering each column of a data.frame
#'
#' * `bl_make_minMax_by_column_table` makes a neat Min. Max table considering each column of a data.frame
#'
#' @param x A numeric vector
#' @param print_model Logical. Should a summary of the GLM used for mean and SE calculation be displayed?
#' @examples
#' bl_get_mean_SE_via_GLM(bl_ecology_data$elevation)
#'
#' bl_get_mean_SE_by_column(bl_ecology_data[4:10])
#'
#' bl_make_mean_SE_by_column_table(bl_ecology_data[4:10])
#'
#' bl_make_mean_SD_by_column_table(bl_ecology_data)
#'
#' bl_make_minMax_by_column_table(bl_ecology_data[4:10])
#'
#' Mean_MinMax_Table <- data.frame (bl_make_mean_SE_by_column_table(bl_ecology_data[4:10]), bl_make_minMax_by_column_table(bl_ecology_data[4:10]))
#' names (Mean_MinMax_Table) <- c("Mean ± SE", "Min.–Max.")
#'
#' @export
bl_get_mean_SE_via_GLM <- function (x, print_model = F) {
    if (!is.numeric(x)){
        stop ("Mean models can only be calculated for numeric variables")
    }

    # calculate GLM with no predictor variables
    my_mean_SE_model <-  glm (x ~ 1) %>% summary ()

    if (print_model == T) {
        mean_model <- list ("Model.Summary" = my_mean_SE_model,
                        "Brief.Summary" = data.frame("Mean" = my_mean_SE_model$coefficients[[1]],
                                                     "SE" = my_mean_SE_model$coefficients[[2]]))
    } else {
        mean_model <- list ("Brief.Summary" = data.frame("Mean" = my_mean_SE_model$coefficients[[1]],
                                                     "SE" = my_mean_SE_model$coefficients[[2]]))
    }

    return (mean_model)
}


#' bl summary functions
#' @description
#' * `bl_get_mean_SE_via_GLM` calculates a glm that puts out mean and SE from a vector
#'
#' * `bl_get_mean_SE_by_column` calculates mean and SE for each column in a data.frame using glms
#'
#' * `bl_make_mean_SE_by_column_table` makes a neat mean ± SE Table considering each column of a data.frame
#'
#' * `bl_make_mean_SD_by_column_table` makes a neat mean ± SD Table considering each column of a data.frame
#'
#' * `bl_make_minMax_by_column_table` makes a neat Min. Max table considering each column of a data.frame

#' @param data_table A data table containing numeric variables
#' @param print_model Should the model summaries be printed?
#' @examples
#' bl_get_mean_SE_via_GLM(bl_ecology_data$elevation)
#'
#' bl_get_mean_SE_by_column(bl_ecology_data[4:10])
#'
#' bl_make_mean_SE_by_column_table(bl_ecology_data[4:10])
#'
#' bl_make_mean_SD_by_column_table(bl_ecology_data)
#'
#' bl_make_minMax_by_column_table(bl_ecology_data[4:10])
#'
#' Mean_MinMax_Table <- data.frame (bl_make_mean_SE_by_column_table(bl_ecology_data[4:10]), bl_make_minMax_by_column_table(bl_ecology_data[4:10]))
#' names (Mean_MinMax_Table) <- c("Mean ± SE", "Min.–Max.")
#'
#' @export

bl_get_mean_SE_by_column <- function (data_table, print_model = F) {

    # select non-numeric variables to report them
    non_numeric_vars <- data_table %>%
                        select_if(purrr::negate(is.numeric)) %>% colnames()

    #select only numeric variables
    data_table <- data_table %>%
                        select_if(is.numeric)

    # calculate GLMs with no predictor variables
    my_mean_SE_models <- apply (data_table,2,bl_get_mean_SE_via_GLM, print_model = T)
    if (print_model == T) {
    mean_models <- list ("Model.Summary" = lapply (my_mean_SE_models, "[[", "Model.Summary"),
                         "Brief.Summary" = sapply (my_mean_SE_models, "[[", "Brief.Summary") %>% t ())
    } else {
        mean_models <- list ("Brief.Summary" = my_mean_SE_models%>% sapply (., "[[", "Brief.Summary") %>% t ())
    }

    return (mean_models)
}


#' bl summary functions
#' @description
#' * `bl_get_mean_SE_via_GLM` calculates a glm that puts out mean and SE from a vector
#'
#' * `bl_get_mean_SE_by_column` calculates mean and SE for each column in a data.frame using glms
#'
#' * `bl_make_mean_SE_by_column_table` makes a neat mean ± SE Table considering each column of a data.frame
#'
#' * `bl_make_mean_SD_by_column_table` makes a neat mean ± SD Table considering each column of a data.frame
#'
#' * `bl_make_minMax_by_column_table` makes a neat Min. Max table considering each column of a data.frame
#'
#' @param data_table A data table containing numeric variables
#' @param beautiful_round Logical. Should numbers be rounded based on their significant digits? Defaults to TRUE
#' @param round_digits Numeric. Number of digits. Only applies when beautiful_round == F.
#' @examples
#' bl_get_mean_SE_via_GLM(bl_ecology_data$elevation)
#'
#' bl_get_mean_SE_by_column(bl_ecology_data[4:10])
#'
#' bl_make_mean_SE_by_column_table(bl_ecology_data[4:10])
#'
#' bl_make_mean_SD_by_column_table(bl_ecology_data)
#'
#' bl_make_minMax_by_column_table(bl_ecology_data[4:10])
#'
#' Mean_MinMax_Table <- data.frame (bl_make_mean_SE_by_column_table(bl_ecology_data[4:10]), bl_make_minMax_by_column_table(bl_ecology_data[4:10]))
#' names (Mean_MinMax_Table) <- c("Mean ± SE", "Min.–Max.")
#'@export

bl_make_mean_SE_by_column_table <- function(data_table,
                                            beautiful_round = TRUE,
                                            round_digits = 2) {
    # Nur numerische Spalten behalten (konsistent mit bl_get_mean_SE_by_column)
    numeric_cols <- data_table %>% select_if(is.numeric) %>% colnames()
    non_numeric_vars <- data_table %>% select_if(purrr::negate(is.numeric)) %>% colnames()

    mean_models <- bl_get_mean_SE_by_column(data_table, TRUE)

    my_coefs <- sapply(mean_models$Model.Summary, "[[", "coefficients")
    my_means <- my_coefs[1,]
    my_SE    <- my_coefs[2,]

    mean_table <- data.frame()
    for (i in 1:length(my_means)) {
        if (beautiful_round == TRUE) {
            mean_table[i, "Mean ± SE"] <- paste(
                my_means[[i]] %>% bl_round_beautifully(last = FALSE),
                my_SE[[i]]    %>% bl_round_beautifully(last = TRUE),
                sep = " ± "
            )
        } else {
            mean_table[i, "Mean ± SE"] <- paste(
                round(my_means[[i]], digits = round_digits),
                round(my_SE[[i]],    digits = round_digits),
                sep = " ± "
            )
        }
    }

    row.names(mean_table) <- numeric_cols  # ← nur numerische Spaltennamen
    if (length (non_numeric_vars) > 0){
        warning (paste0 ("The following non-numeric variables were excluded: ",
                         paste (non_numeric_vars, collapse = ", ")))
    }
    return(mean_table)
}



#' bl summary functions
#' @description
#' * `bl_get_mean_SE_via_GLM` calculates a glm that puts out mean and SE from a vector
#'
#' * `bl_get_mean_SE_by_column` calculates mean and SE for each column in a data.frame using glms
#'
#' * `bl_make_mean_SE_by_column_table` makes a neat mean ± SE Table considering each column of a data.frame
#'
#' * `bl_make_mean_SD_by_column_table` makes a neat mean ± SD Table considering each column of a data.frame
#'
#' * `bl_make_minMax_by_column_table` makes a neat Min.-Max. table considering each column of a data.frame
#'
#' @param data_table A data table containing numeric variables
#' @param beautiful_round Logical. Should numbers be rounded based on their significant digits? Defaults to TRUE
#' @param round_digits Numeric. Number of digits. Only applies when beautiful_round == F.
#' @examples
#' bl_get_mean_SE_via_GLM(bl_ecology_data$elevation)
#'
#' bl_get_mean_SE_by_column(bl_ecology_data[4:10])
#'
#' bl_make_mean_SE_by_column_table(bl_ecology_data[4:10])
#'
#' bl_make_mean_SD_by_column_table(bl_ecology_data)
#'
#' bl_make_minMax_by_column_table(bl_ecology_data[4:10])
#'
#' Mean_MinMax_Table <- data.frame (bl_make_mean_SE_by_column_table(bl_ecology_data[4:10]), bl_make_minMax_by_column_table(bl_ecology_data[4:10]))
#' names (Mean_MinMax_Table) <- c("Mean ± SE", "Min.–Max.")
#'@export

bl_make_mean_SD_by_column_table <- function(data_table,
                                            beautiful_round = TRUE,
                                            round_digits = 2) {
    # Nur numerische Spalten behalten
    data_numeric <- data_table %>% select_if(is.numeric)
    numeric_cols <- colnames(data_numeric)

    # für warnung nicht-numerische variablen sammeln
    non_numeric_vars <- data_table %>% select_if(purrr::negate(is.numeric)) %>% colnames()


    # Mittelwert und SD direkt berechnen
    my_means <- sapply(data_numeric, mean, na.rm = TRUE)
    my_SDs   <- sapply(data_numeric, sd,   na.rm = TRUE)

    # Tabelle befüllen
    mean_table <- data.frame()
    for (i in 1:length(my_means)) {
        if (beautiful_round == TRUE) {
            mean_table[i, "Mean ± SD"] <- paste(
                my_means[[i]] %>% bl_round_beautifully(last = FALSE),
                my_SDs[[i]]   %>% bl_round_beautifully(last = TRUE),
                sep = " ± "
            )
        } else {
            mean_table[i, "Mean ± SD"] <- paste(
                round(my_means[[i]], digits = round_digits),
                round(my_SDs[[i]],   digits = round_digits),
                sep = " ± "
            )
        }
    }

    row.names(mean_table) <- numeric_cols
    if (length (non_numeric_vars) > 0){
        warning (paste0 ("The following non-numeric variables were excluded: ",
                         paste (non_numeric_vars, collapse = ", ")))
    }
    return(mean_table)
}




#' bl summary functions
#' @description
#' * `bl_get_mean_SE_via_GLM` calculates a glm that puts out mean and SE from a vector
#'
#' * `bl_get_mean_SE_by_column` calculates mean and SE for each column in a data.frame using glms
#'
#' * `bl_make_mean_SE_by_column_table` makes a neat mean ± SE Table considering each column of a data.frame
#'
#' * `bl_make_mean_SD_by_column_table` makes a neat mean ± SD Table considering each column of a data.frame
#'
#' * `bl_make_minMax_by_column_table` makes a neat Min.-Max. table considering each column of a data.frame
#'
#' @param data_table A data table containing numeric variables
#' @param beautiful_round Logical. Should numbers be rounded based on their significant digits? Defaults to TRUE
#' @param round_digits Numeric. Number of digits. Only applies when beautiful_round == F.
#' @examples
#' bl_get_mean_SE_via_GLM(bl_ecology_data$elevation)
#'
#' bl_get_mean_SE_by_column(bl_ecology_data[4:10])
#'
#' bl_make_mean_SE_by_column_table(bl_ecology_data[4:10])
#'
#' bl_make_mean_SD_by_column_table(bl_ecology_data)
#'
#' bl_make_minMax_by_column_table(bl_ecology_data[4:10])
#'
#' Mean_MinMax_Table <- data.frame (bl_make_mean_SE_by_column_table(bl_ecology_data[4:10]), bl_make_minMax_by_column_table(bl_ecology_data[4:10]))
#' names (Mean_MinMax_Table) <- c("Mean ± SE", "Min.–Max.")
#'@export


bl_make_minMax_by_column_table <- function (data_table,
                               beautiful_round = T,
                               round_digits = 2) {
    # get min and max values of each column
    mins <- sapply (data_table, min)
    maxs <- sapply (data_table, max)

    # make min max values table
    min_max_table <- data.frame ()
    for (i in 1:length (mins)) {
        if (beautiful_round == T) {
             min_max_table [i, "Min.–Max."] <- paste(mins[[i]]  %>% bl_round_beautifully (last = F),
                                      maxs[[i]]  %>% bl_round_beautifully (last = T),
                                      sep = "-")
        } else {
             min_max_table [i, "Min.–Max."] <- paste(mins[[i]]  %>% round (digits = round_digits),
                          maxs[[i]]  %>% round (digits = round_digits),
                          sep = "–")
        }

    }
    row.names(min_max_table) <- colnames(data_table)
    return (min_max_table)
}


