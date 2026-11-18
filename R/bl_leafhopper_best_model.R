#' A GLMM
#'
#' The single best model from dredging based on the bl_ecology_data dataset using leafhoppers as the response
#' @examples
#' # # # # not run - this is how the average model was obtained # # # #
#' # set delta
#' my_delta <- 3 # set aicc delta for model averaging. Hier 1 gewählt, weil das einer "Stufe" in den AICc-Werten enspricht. Oft wird eher 3 gewählt.
#'
#' # get average model from dredge object
#' bl_avg_leafhopper_model <- model.avg(bl_leafhopper_multivar_dredge, subset = delta < my_delta, fit=TRUE) # only one model
#' # so get best model
#' bl_leafhopper_best_model <- glmer (leafhoppers  ~ area + herbs + land_use_type + summer_temperature + (1 | sub_area),
#'                                 data = bl_ecology_scaled,
#'                                 family = "poisson")
#'
#' # get summary of best model
#' summary (bl_leafhopper_best_model)


"bl_leafhopper_best_model"


