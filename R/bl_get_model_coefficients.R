
#' bl_get_model_coefficients
#'
#' @description get the coefficients (response, fixed effects and also random effects) from a model. This can be useful in functions where the model coefficients should be recycled
#'
#' @param model a model (lm, glm, lmm, glmm, others might also work)
#' @export
#' @examples
#'
#' # build a model
#' a_model <- lme4::lmer (mpg ~ hp + drat + (1|cyl ),
#'                       data = mtcars)
#'
#' # get coefficients
#' bl_get_model_coefficients (a_model)
#'
bl_get_model_coefficients <- function (model) {

    # independent vars:
    # get string of model formula and split by "+"
    independ_string <-  formula(model)[[3]][2] %>% as.character() %>% stringr::str_split (pattern = " +") %>% unlist
    # remove the "+"
    independ_string <- independ_string [! independ_string %in% "+"]

    # dependent var:
    dependent_var <- formula(model)[[2]] %>% as.character()

    # random effects:
    if ("|" %in% all.names(formula (model))) {
            random_effects <- formula(model)[[3]][3] %>% as.character()
    } else {
        random_effects <- NA
    }

    return (list (response_variable = dependent_var, fixed_effects = independ_string,  random_effects = random_effects))
}
