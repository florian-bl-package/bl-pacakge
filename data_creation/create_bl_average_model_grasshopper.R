# library ####
library(bl)
library(lme4)
library(MuMIn)
library(tidyverse)

# A: Bereite Berechnung des Modells vor ####
# define independent variables
# view variable names
names (bl_ecology_data)

# make vektor of numeric independent variables for the respective model
independent_numeric_vars <- c("elevation",
                      "summer_temperature", "summer_precipitation", "snow_cover_duration",
                      "heat_load", "ndvi",
                      "isolation", "area",
                      "bare_ground", "moss", "herbs", "grasses", "shrubs",
                      "plant_species"
                      )
# add non-numeric variables to vector of independent variables
independent_vars <- c (independent_numeric_vars, "land_use_type")

# scale independent variables
bl_ecology_scaled <- bl_ecology_data
bl_ecology_scaled [independent_numeric_vars] <- scale (bl_ecology_scaled [independent_numeric_vars])

# define response variable
response_var <- "grasshopper"

# define random factor
random_intercept <- "sub_area"

# make formula for global model
global_formula <- as.formula (
                            paste (response_var,
                                   "~",
                                   paste (independent_vars, collapse= " + "),
                                   " + (1|",
                                   random_intercept,
                                   ")"
                                )
                            )
# B: calculate global model with all independent variables ####
global_model <- glmer (global_formula,
                       data = bl_ecology_scaled,
                       family = "poisson")

summary (global_model)

# c: dredge ####
# set na.action = fail for dredge
options (na.action = na.fail)

# dredge: dauert lang. Zweite Zeile mitausführen - dann gibt es einen Signalton, wenn dredge fertig ist.
bl_grasshopper_multivar_dredge <- dredge(global_model, m.lim=c(0,4))
beepr::beep()

# die Zeilen Code hier einfach wegkommentieren, wenn das Objekt einmal gespeicher ist
# speichere dredge objekt, um es nicht immer wieder neu berechnen zu müssen
rlist::list.save (bl_grasshopper_multivar_dredge,
           file = "my_path.rds")

# Die Zeilen Code hier einfach "uncommenten", falls das dredge-Objekt schon existiert.
## öffne vorher gespeichertes Objekt
# bl_grasshopper_multivar_dredge <- rlist::list.load("my_path.rds")

bl_grasshopper_multivar_dredge

# D: model averaging ####
# set delta
my_delta <- 1 # set aicc delta for model averaging. Hier 1 gewählt, weil das einer "Stufe" in den AICc-Werten enspricht. Oft wird eher 3 gewählt.

# get average model from dredge object
bl_grasshopper_avg_model <- model.avg(bl_grasshopper_multivar_dredge, subset = delta < my_delta, fit=TRUE)

# get summary of average model
summary (bl_grasshopper_avg_model)
summary (bl_grasshopper_avg_model)["coefmat.full"]

# E: R2 values ####
bl_get_R2_range (bl_grasshopper_multivar_dredge, AICc_delta = 1)


# F: make beautiful results table of average model (full average) ####
# results table of average model for publication
grasshopper_avg_export_table <- bl_make_averageModel_table(bl_grasshopper_avg_model)
grasshopper_avg_export_table

# G: save average model for figures ####

# make usable for bl ####
usethis::use_data(bl_grasshopper_multivar_dredge)
usethis::use_data(bl_grasshopper_avg_model)
