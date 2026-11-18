#' An average model
#'
#' An average model from dredging based on the bl_ecology_data dataset using butterflies as the response
#' @examples
#' ## # # # not run - this is how the average model was obtained # # # #
#' ## set delta
#' #my_delta <- 2 # set aicc delta for model averaging. Hier 1 gewählt, weil das einer "Stufe" in den AICc-Werten enspricht. Oft wird eher 3 gewählt.
#'
#' ## get average model from dredge object
#' #bl_butterfly_avg_model <- MuMIn::model.avg(bl_butterfly_multivar_dredge, subset = delta < my_delta, fit=TRUE)


"bl_butterfly_avg_model"


