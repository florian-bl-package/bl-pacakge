#' A dredge object
#'
#' A dredge object based on the bl_ecology_data dataset using grasshoppers as the response
#'
#' @examples
#' # # # # not run - this is how the dredge object was created: # # # #
#' # library ####
#' library(bl)
#' library(lme4)
#' library(MuMIn)
#' library(tidyverse)
#'
#' # A: Bereite Berechnung des Modells vor ####
#' # define independent variables
#' # view variable names
#' names (bl_ecology_data)
#'
#' # make vektor of numeric independent variables for the respective model
#' independent_numeric_vars <- c("elevation",
#'                       "summer_temperature", "summer_precipitation", "snow_cover_duration",
#'                       "heat_load", "ndvi",
#'                       "isolation", "area",
#'                       "bare_ground", "moss", "herbs", "grasses", "shrubs",
#'                       "plant_species"
#'                       )
#' # add non-numeric variables to vector of independent variables
#' independent_vars <- c (independent_numeric_vars, "land_use_type")
#'
#' # scale independent variables
#' bl_ecology_scaled <- bl_ecology_data
#' bl_ecology_scaled [independent_numeric_vars] <- scale (bl_ecology_scaled [independent_numeric_vars])
#'
#' # define response variable
#' response_var <- "grasshopper"
#'
#' # define random factor
#' random_intercept <- "sub_area"
#'
#' # make formula for global model
#' global_formula <- as.formula (
#'                             paste (response_var,
#'                                    "~",
#'                                    paste (independent_vars, collapse= " + "),
#'                                    " + (1|",
#'                                    random_intercept,
#'                                    ")"
#'                                 )
#'                             )
#' # B: calculate global model with all independent variables ####
#' global_model <- glmer (global_formula,
#'                        data = bl_ecology_scaled,
#'                        family = "poisson")
#'
#' summary (global_model)
#'
#' # c: dredge ####
#' # set na.action = fail for dredge
#' options (na.action = na.fail)
#'
#' # dredge: dauert lang. Zweite Zeile mitausführen - dann gibt es einen Signalton, wenn dredge fertig ist.
#' bl_grasshopper_multivar_dredge <- dredge(global_model, m.lim=c(0,4))
#' beepr::beep()
#'
#'
#' bl_grasshopper_multivar_dredge

#'
"bl_grasshopper_multivar_dredge"
