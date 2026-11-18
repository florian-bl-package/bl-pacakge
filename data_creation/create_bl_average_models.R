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
grasshopper_multivar_dredge <- dredge(global_model, m.lim=c(0,4))
beepr::beep()

# die Zeilen Code hier einfach wegkommentieren, wenn das Objekt einmal gespeicher ist
# speichere dredge objekt, um es nicht immer wieder neu berechnen zu müssen
rlist::list.save (grasshopper_multivar_dredge,
           file = "my_path.rds")

# Die Zeilen Code hier einfach "uncommenten", falls das dredge-Objekt schon existiert.
## öffne vorher gespeichertes Objekt
# grasshopper_multivar_dredge <- rlist::list.load("my_path.rds")

grasshopper_multivar_dredge

# D: model averaging ####
# set delta
my_delta <- 1 # set aicc delta for model averaging. Hier 1 gewählt, weil das einer "Stufe" in den AICc-Werten enspricht. Oft wird eher 3 gewählt.

# get average model from dredge object
avg_grasshopper_model <- model.avg(grasshopper_multivar_dredge, subset = delta < my_delta, fit=TRUE)

# get summary of average model
summary (avg_grasshopper_model)
summary (avg_grasshopper_model)["coefmat.full"]

# E: R2 values ####
# select first and last model of dredging table within aicc delta == my_delta
best_model <- get.models(grasshopper_multivar_dredge, subset = 1)[[1]]
least_model <- get.models(grasshopper_multivar_dredge, subset = delta <= my_delta) [[length(get.models(grasshopper_multivar_dredge, subset = delta <= my_delta))]]

# teste auf singularity
# (vereinfacht: wenn das model "singular" ist, gibt es keinen effekt der random effects -
# dann ist kein conditional R² möglich/sinnvoll)
lme4::isSingular (best_model)
lme4::isSingular (least_model)

# checke variance inflation factor
# (ein maß für Interkollinearität, das auch komplexere Zusammenhänge erkennt als Interkorrelationen zwischen nur zwei Variablen)
# Werte nahe 1 sind top, bis ca. 3 kein Problem, ab 5 schwierig und ab 10 ein echtes Problem - dann sollte man es mit weniger Variablen probieren
car::vif(best_model)
car::vif(least_model)

# calculate R2 values for first and last model
R2best <- best_model %>%
    performance::r2 ()
R2least <-  least_model %>%
    performance::r2 ()

R2best
R2least


# E: make beautiful results table of average model (full average) ####
# results table of average model for publication
grasshopper_avg_export_table <- bl_make_averageModel_table(avg_grasshopper_model)
grasshopper_avg_export_table

# show R2 values
data.frame ("R2_Conditional / R2_marginal"=paste0 (min (R2best$R2_conditional, R2least$R2_conditional)%>% round (2),
                 "-",
                 max (R2best$R2_conditional, R2least$R2_conditional)%>% round (2),

                 "/",

                 min (R2best$R2_marginal, R2least$R2_marginal)%>% round (2),
                 "-",
                 max (R2best$R2_marginal, R2least$R2_marginal)%>% round (2)
                 )
)

# in this case, only marginal R2 makes sense:
data.frame(R2_marginal = paste0 (min (R2best$R2_marginal, R2least$R2_marginal)%>% round (2),
                 "-",
                 max (R2best$R2_marginal, R2least$R2_marginal)%>% round (2))
)


# F: export table for publication ####
bl_save_xlsx (grasshopper_avg_export_table, "my_path.xlsx", header_width = 2)

# G: save average model for figures ####
list.save (avg_grasshopper_model,
           file = "my_path.rds")
