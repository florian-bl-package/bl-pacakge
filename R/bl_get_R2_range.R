#' bl_get_R2_range
#'
#' @description A function to get the highest and lowest marginal and conditional R2 values of all modles in a dredge selection table within a specified AICc range (by MuMIn package)
#'
#' @param dredge_object A dredge object from MuMIn package
#' @param AICc_delta numeric. Determines the least model to be considered for R2 calculation (in case of using this with averaged models, should be the same value as "subset = delta < x" in "MuMIn::model.avg").
#'
#' @examples
#' # load packages
#' library (MuMIn)
#' library (lme4)
#' # calculate global model with all independent variables
#' global_model <- lmer (mpg ~ hp + drat + (1|cyl ),
#'                       data = mtcars)
#' # set na.action for dredge
#' options (na.action = na.fail)
#'
#' # perform dredge on global model
#' cars_dredge <- dredge(global_model)
#'
#' # get conditional and marginal R2 value range
#' bl_get_R2_range (cars_dredge, AICc_delta = 2)
#' @export


bl_get_R2_range <- function (dredge_object, AICc_delta = NA, show_all = F) {
    # check if AICc_delta is provided
    if (is.na (AICc_delta)) {
        stop ("AICc_delta must be set")
    }

    # select all models of dredging table within aicc delta == AICc_delta
    selected_models <- get.models(dredge_object, subset = delta <= AICc_delta)

    # get all R2 values for all selected models
    R2_table <- tibble (Conditional = NA, Marginal = NA)
    suppressWarnings(
        for (i in seq_along(selected_models)) {
            R2_table[i, "Conditional"] <- (selected_models[[i]] %>% performance::r2())$R2_conditional %>% round (digits = 2)
            R2_table[i, "Marginal"] <- (selected_models[[i]] %>% performance::r2())$R2_marginal %>% round (digits = 2)
        }
    )
    if (show_all == T){
        cat(crayon::blue("Overview of R²-values of all models within specified AICc_delta: \n \n "))
        print(R2_table)
    }

    # teste auf singularity (vereinfacht: kein effekt der random effects - kein conditional R² möglich/sinnvoll)
    singular_models <- lapply (selected_models, lme4::isSingular)

    # if at least one model is not singular, get marginal AND conditional R2
    if (!any(unlist(singular_models))) {
            R2_min_max <- tibble (Conditional = paste0 (min(R2_table$Conditional, na.rm = T), "–", max(R2_table$Conditional, na.rm = T)),
                          Marginal = paste0 (min(R2_table$Marginal, na.rm = T), "–", max(R2_table$Marginal, na.rm = T)))
            R2_min_max["Conditional/Marginal"] <- paste0(R2_min_max$Conditional, "/", R2_min_max$Marginal)
        warning_message <- "\n"
    } else {
        # else if all models are singular, only get marginal R2
        R2_min_max <- tibble (Marginal = paste0 (min(R2_table$Marginal, na.rm = T), "–", max(R2_table$Marginal, na.rm = T)))
        warning_message <- "\n \n All models are (almost) singular. Fitting conditional R2 hence makes no sense.\n \n"
    }

    return (c (cat(crayon::bgYellow(warning_message)), cat(crayon::blue("Least and best R² values: \n")), R2_min_max))


}




