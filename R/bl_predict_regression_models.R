#' bl_predict_regression_models
#'
#' @description Makes tables of predictions for each fixed effect in a regression model including 95 % Confidence intervals.
#'
#' @param model A model (lm, glm, lmm, glmm; others might also work)
#' @param order optionally. Order in which the fixed effects should be predicted.
#' @export
#' @examples
#'
#' # build a model
#' a_model <- lmer (mpg ~ hp + drat + (1|cyl ),
#'                       data = mtcars)
#'
#' # get coefficients
#' bl_predict_regression_models (a_model)
#'
#'
#'

# bl_predict_regression_models: puts out a list of predictions for all fixed effects in a multivariable model####
# add ggeffects to bl

bl_predict_regression_models <- function (model = NA, order = NA) {

    # make empty list of predictions
    the_predictions <- list ()

    # get model coefficients
    coeff <- bl_get_model_coefficients(model)$fixed_effects

    # check if order is set. If yes, order coefficients accordingly
    if (any(!is.na(order))) {
    coeff <- bl_order (coeff, ordering = order)
    }

    # get the data the model is based on (must be in the environment)
    model_data <- model.frame(model)

    # predict based on the model.
    for (i in coeff) {
        # ACHTUNG: xxx_seq__xxx_xxx_xxx wird hier mit <<- ins global environment geschrieben (und später wieder gelöscht)
        xxx_seq__xxx_xxx_xxx <<- seq (min (model_data[[i]]), max (model_data[[i]]), length.out = 120) # make continuous vector for x-axis
        the_predictions [[i]] <- ggeffects::ggpredict (model, paste0 (i, " [xxx_seq__xxx_xxx_xxx]"))
    }

    # delete xxx_seq__xxx_xxx_xxx from global environment
    rm (xxx_seq__xxx_xxx_xxx, envir = .GlobalEnv)

    # return resulting list of predictions
    return(the_predictions)
}

