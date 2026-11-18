#' bl_get_best_and_least_R2
#'
#' @description A function to get marginal and conditional R2 values of the best and least model within a specified AICc values from a dredge selection table (by MuMIn package)
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
#' bl_get_best_and_least_R2 (cars_dredge, AICc_delta = 2)
#'
#' @export


bl_get_best_and_least_R2 <- function (dredge_object, AICc_delta = NA) {
    # check if AICc_delta is provided
    if (is.na (AICc_delta)) {
        stop ("AICc_delta must be set")
    }

    # select first and last model of dredging table within aicc delta == AICc_delta
    best_model <- get.models(dredge_object, subset = 1)[[1]]
    models_in_delta <- get.models(dredge_object, subset = delta <= AICc_delta) #only call get.models(...) once to reduce computational power and avoid duplicate warnigns
    least_model <- models_in_delta[[length(models_in_delta)]]

    # teste auf singularity (vereinfacht: kein effekt der random effects - kein conditional R² möglich/sinnvoll)
    sing_best  <- lme4::isSingular(best_model)
    sing_least <- lme4::isSingular(least_model)

    if (sing_best & sing_least) {
        # Both singular → only marginal R2 makes sense for both
        warning("best and least model both are (almost) singular. Fitting conditional R2 hence makes no sense. Only marginal R2 is reported.")
        R2best  <- performance::r2_nakagawa(best_model,  tolerance = NULL)
        R2least <- performance::r2_nakagawa(least_model, tolerance = NULL)

        R2_output <- data.frame(
            R2_marginal = paste0(
                min(R2best$R2_marginal, R2least$R2_marginal) %>% round(2),
                "-",
                max(R2best$R2_marginal, R2least$R2_marginal) %>% round(2)
            )
        )

    } else if (sing_best | sing_least) {
        # One singular → report marginal for both (conditional unreliable for the singular one)
        warning("One of best/least model is (almost) singular. Reporting marginal R2 only for comparability.")
        R2best  <- performance::r2_nakagawa(best_model,  tolerance = NULL)
        R2least <- performance::r2_nakagawa(least_model, tolerance = NULL)

        R2_output <- data.frame(
            R2_marginal = paste0(
                min(R2best$R2_marginal, R2least$R2_marginal) %>% round(2),
                "-",
                max(R2best$R2_marginal, R2least$R2_marginal) %>% round(2)
            )
        )

    } else {
        # Neither singular → full conditional / marginal output
        # Neither singular → full conditional / marginal output
        R2best  <- performance::r2(best_model)   # ← missing in your current code
        R2least <- performance::r2(least_model)

        r2_cond_best  <- extract_r2(R2best,  "R2_conditional")
        r2_cond_least <- extract_r2(R2least, "R2_conditional")
        r2_marg_best  <- extract_r2(R2best,  "R2_marginal")
        r2_marg_least <- extract_r2(R2least, "R2_marginal")

        R2_output <- data.frame(
            "R2_Conditional / R2_marginal" = paste0(
                min(r2_cond_best, r2_cond_least) %>% round(2), "-",
                max(r2_cond_best, r2_cond_least) %>% round(2),
                " / ",
                min(r2_marg_best, r2_marg_least) %>% round(2), "-",
                max(r2_marg_best, r2_marg_least) %>% round(2)
            ),
            check.names = FALSE
        )
    }

    # # calculate R2 values for first and last model
    # R2best <- best_model %>%
    #     performance::r2 ()
    # R2least <-  least_model %>%
    #     performance::r2 ()
    #
    # # teste auf singularity (vereinfacht: kein effekt der random effects - kein conditional R² möglich/sinnvoll)
    # if (isTRUE (lme4::isSingular (best_model) & lme4::isSingular (least_model))) {
    #     warning ("best and least model both are (almost) singular. Fitting conditional R2 hence makes no sense.")
    #
    #     # in this case, only marginal R2 makes sense:
    #     r2_marg_best  <- extract_r2(R2best,  "R2_marginal")
    #     r2_marg_least <- extract_r2(R2least, "R2_marginal")
    #
    #     R2_output <- data.frame(
    #         R2_marginal = paste0(
    #             min(r2_marg_best, r2_marg_least)  %>% round(2),
    #             "-",
    #             max(r2_marg_best, r2_marg_least)  %>% round(2)
    #         )
    #     )
    #
    # } else {
    #
    #     # in this case, conditional AND marginal make sense:
    #     r2_cond_best  <- extract_r2(R2best,  "R2_conditional")
    #     r2_cond_least <- extract_r2(R2least, "R2_conditional")
    #     r2_marg_best  <- extract_r2(R2best,  "R2_marginal")
    #     r2_marg_least <- extract_r2(R2least, "R2_marginal")
    #
    #     R2_output <- data.frame(
    #         "R2_Conditional / R2_marginal" = paste0(
    #             min(r2_cond_best, r2_cond_least) %>% round(2), "-",
    #             max(r2_cond_best, r2_cond_least) %>% round(2),
    #             " / ",
    #             min(r2_marg_best, r2_marg_least) %>% round(2), "-",
    #             max(r2_marg_best, r2_marg_least) %>% round(2)
    #         ),
    #         check.names = FALSE
    #     )
    # }
    return (R2_output)
}



# Safely extract R2 values regardless of whether r2() returns a list or named vector
extract_r2 <- function(r2_obj, component) {
    if (is.list(r2_obj)) {
        r2_obj[[component]]
    } else {
        r2_obj[component]  # named numeric vector fallback
    }
}
