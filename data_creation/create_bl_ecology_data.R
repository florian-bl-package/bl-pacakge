# create variables ####
set.seed(123)  # Set seed for reproducibility

# Number of observations
n <- 140
n <- ((n/3) %>% round (digits = 0))*3 # sorge dafür, dass durch 3 teilbar (weil 3 land_use_types)



# sub area
set.seed(123)  # Set seed for reproducibility
sub_area <- as.factor(sample(LETTERS [1:6], n, T))

# land use type
land_use_type <- as.factor(c(rep("meadow", n/3),
                        rep("pasture", n/3),
                        rep ("fallow", n/3)
                        )
)

# Generate random data for each variable with more complex relationships
elevation <- sample(350:1500, n, replace = TRUE)
elevation <- elevation + ifelse(sub_area == "E", 230, 0)
el_scaled <- elevation/mean(elevation) - min (elevation/mean(elevation)) + 1

# calculate envir vars
summer_temp <- rnorm(n, mean = 21.2 - 3.5 * el_scaled, sd = 1.5)
winter_temp <- rnorm(n, mean = 2 - 3.5 * el_scaled^0.9, sd = 1.7)
summer_precip <- rnorm(n, mean = 950 + 0.7 * elevation^0.8, sd = 80)
winter_precip <- rnorm(n, mean = 600 + 0.8 * elevation^0.81, sd = 65) + ifelse (sub_area %in% c("D", "E"), 100, -250)


# Generate Snow_Cover_Duration with higher correlation to Winter_Temperature and lower correlation to Winter_Precipitation
snow_cover_duration <- ((rnorm(n, mean = 50 - 8 * winter_temp + 0.063 * winter_precip, sd = 11)- 48)*1.5) %>% round (digits = 0)


# Create a data frame
climate_data <- data.frame(
  elevation = elevation,
  summer_temperature = summer_temp,
  winter_temperature = winter_temp,
  summer_precipitation = summer_precip,
  winter_precipitation = winter_precip,
  snow_cover_duration = snow_cover_duration
)

#library(bl)
climate_data%>% summary()

IBL::IBL_correlations(climate_data)

# isolation + area
set.seed(123)  # Set seed for reproducibility
isolation <-  abs (((abs(rnorm (n) * rpois(n, 5) * 1000)  + 120)/5) %>% round (digits = 0) - ifelse (sub_area=="E", 200, 0))
rarea <- rgamma (n, 1,10)
area <- (sqrt ((rarea * 10000000) + 1800) * ifelse(sub_area == "B", 1.7, 1) - ifelse(sub_area == "A", -200, 0)) %>% round (digits = 0)

# vegetation layers
set.seed(123)  # Set seed for reproducibility
bare_ground <- ((c ((runif (20, 0.1, 9.5) %>% round (digits = 0) )*10, 0, 2.5, 2.5, 2.5, 5, 5, 5, 5, 0, 0, (runif (70, 0.3, 8) %>% round (digits = 0) )*10) %>% sample()) / 2 )*el_scaled %>% round (digits = 0)
bare_ground <-  sample (bare_ground)  # zufällige reihenfolge
bare_ground  <- ifelse(land_use_type == "meadow", bare_ground  * 0.3,
                       ifelse(land_use_type == "fallow", bare_ground*0.05, bare_ground * 1)
                       ) # mehr offenboden in den pastures als in den meadows und kaum offenboden in den fallows

moss <- (100 - bare_ground - ((c (
    runif (50, 0.07,0.09),
    runif (50, 0.06,0.09)
    ) * (100 - (bare_ground))
    ) %>% round (digits = 0)*10)) %>% round (digits = 0)

herbs <- sqrt((100 - ((bare_ground+moss)%>% round (digits = 0) +
                    rpois(n, 2)*10
                ))^2) +20
grasses <- sqrt((100 - ((bare_ground+moss+herbs)%>% round (digits = 0) +
                    rpois(n, .5)*10
                ))^2) +10
shrubs <- (sqrt((150 - ((bare_ground+moss+herbs+grasses)%>% round (digits = 0) +
                    rpois(n, .5)*10
                ))^2)/4) %>% round (digits = 0)

vegetation_layers <- data.frame (bare_ground, moss,herbs,grasses, shrubs)


# plant species
set.seed(123)  # Set seed for reproducibility
plant_species <- (((rpois (n, 10) + 15) * (bare_ground/140+1.7)) / 2 )%>% round (digits = 0)
plant_species <- plant_species + ifelse (land_use_type == "pasture", 5, -1)




# heat load
set.seed(123)  # Set seed for reproducibility
heat_load <- sqrt (((bare_ground/100+1)*runif(n,0.3,0.7))^2*10 + 0.7)-1


# ndvi
set.seed(123)  # Set seed for reproducibility
ndvi_random <- (rnorm (n, 3,0.9))
ndvi_elevation_factor <- sqrt(
                    scale(elevation) + abs (min (scale(elevation)))
                    )/3

summary (ndvi_elevation_factor)
ndvi_layer_cover_factor <- -((bare_ground+5)* rowSums(vegetation_layers[c(2:4)])/15 + 10) %>%
                                scale (center = F)
ndvi_layer_cover_factor <- (ndvi_layer_cover_factor - max (ndvi_layer_cover_factor) + 3)*(bare_ground+10)

summary(ndvi_layer_cover_factor)
ndvi_raw <- (ndvi_random - ndvi_elevation_factor) * -ndvi_layer_cover_factor + 4
ndvi_raw <- ndvi_raw - min (ndvi_raw)
ndvi <- abs (ndvi_raw / max(ndvi_raw) - 0.07 - ifelse (bare_ground>55, bare_ground*.003,0))

plot (ndvi~bare_ground)
plot (ndvi~elevation)



# create response (species) variables ####
set.seed(123)  # Set seed for reproducibility
# Calculate leafhoppers based on the specified relationships
significant_variables_lh <- ((cbind(climate_data, vegetation_layers, plant_species, area) %>% scale ())+2) %>% data.frame()
significant_variables_lh ["sub_area"] <- sub_area
significant_variables_lh ["land_use_type"] <- land_use_type

leafhoppers <- with(significant_variables_lh, {
  base_response <- -(bare_ground*5 + summer_temp*2)
  positive_response <- herbs*8 + plant_species*2 + area*5 + scale (winter_precip)*0.5
  sub_area_effect1 <- ifelse(sub_area == "B", 10, 0)
  sub_area_effect2 <- ifelse(sub_area == "E", -15, 3)
  sub_area_effect3 <- ifelse(sub_area == "D", -14, 4)
  random_baseline <- rnorm (n, 36, 4) %>% round (digits = 0)
  land_use_type_effect <- ifelse(land_use_type == "meadow", -2,
                                 ifelse (land_use_type == "pasture", 7, 0))


  leafhoppers <- base_response + positive_response + sub_area_effect1 + sub_area_effect2 + sub_area_effect3 + random_baseline + land_use_type_effect

}
)
leafhoppers <- (leafhoppers*0.6 + 0.4) %>% round (0) %>% abs()




# Calculate grasshoppers based on the specified relationships
set.seed(123)  # Set seed for reproducibility

significant_variables_gh <- ((cbind(climate_data, vegetation_layers, heat_load, isolation) %>% scale ())+2) %>% data.frame()
significant_variables_gh ["sub_area"] <- sub_area
grasshoppers <- with(significant_variables_gh, {
  base_response <- (scale (bare_ground, center = F )*2 + scale (heat_load)*2.3 + scale (winter_temp, center = F )*3 - 18 +
      (vegan::diversity (vegetation_layers)-0.5)*5)/2
  negative_response <- scale ((shrubs+ moss+3)^2) + abs (min (scale ((shrubs + moss+3)^2))) + (snow_cover_duration^8 %>% scale (center = F))*3
  sub_area_effect1 <- ifelse(sub_area %in% c("A", "D"), -1.3, 1.2)
  sub_area_effect2 <- ifelse(sub_area == "B", 0.5, 0)
  # sub_area_effect3 <- ifelse(sub_area == "D", -12, 0)
  random_baseline <- rnorm (n, 15, .8) %>% round (digits = 0)

  land_use_type_effect <- ifelse(land_use_type == "meadow", -9, ifelse(land_use_type == "fallow", -8, 3))



  grasshoppers <- base_response -
      negative_response -
      sub_area_effect1*scale (isolation)*4 +
      random_baseline +
      sub_area_effect2 +
      land_use_type_effect
  grasshoppers
})
grasshoppers <- ((grasshoppers+abs (min(grasshoppers)))*0.4) %>% round (0) %>% abs()
grasshoppers




# Calculate butterflies based on the specified relationships
set.seed(123)  # Set seed for reproducibility

significant_variables_bf <- ((cbind(climate_data, plant_species, vegetation_layers, heat_load, ndvi, isolation, area) %>% scale ())+2) %>% data.frame()
significant_variables_bf ["sub_area"] <- sub_area
butterflies <- with (significant_variables_bf, {
  positive_response <-  scale (plant_species, center = 1.5)*1.5 + scale (area, center = 1.5)
  negative_response <- scale (isolation^2, center = 1.5)*4 + scale (ndvi, center = F)*4
  quadratic_response <- scale (snow_cover_duration)*1.5
  sub_area_effect1 <- ifelse(sub_area == "E", 1.8, 1)
  sub_area_effect2 <- ifelse(sub_area == "E", -1, 1)
  # sub_area_effect3 <- ifelse(sub_area == "D", -12, 0)
  random_baseline <- rpois (n, 20) %>% scale (center = 1.5) + 4

  land_use_type_effect <- ifelse(land_use_type == "meadow", -2, ifelse(land_use_type == "fallow", -8, 2))


  butterflies <- random_baseline +
      positive_response -
      negative_response +
      quadratic_response^2+
      sub_area_effect1 +
      sub_area_effect2*scale (summer_temp, center = 1.5) +
      land_use_type_effect
  butterflies
})
butterflies <- ((butterflies+abs (min(butterflies)))) %>% round (0) %>% abs()
butterflies


# create data.frame ####
area <- area %>% round (digits = 0)
bl_environmental_data <- cbind (climate_data,
                           isolation,
                           area,
                           vegetation_layers,
                           plant_species,
                           heat_load,
                           ndvi,
                           land_use_type,
                           sub_area
                           ) %>% dplyr::tibble()

bl_species_data <- tibble (grasshoppers = c (grasshoppers), butterflies = c (butterflies), leafhoppers = leafhoppers)
colnames(bl_species_data) <- c ("grasshopper", "butterflies", "leafhoppers")

# make some overview plots ####
for (i in 1:(length (bl_environmental_data)-1)) {
    plot (grasshoppers~bl_environmental_data[[i]], xlab = names (bl_environmental_data[i]))
}

for (i in 1:(length (bl_environmental_data)-1)) {
    plot (leafhoppers~bl_environmental_data[[i]], xlab = names (bl_environmental_data[i]))
}

for (i in 1:(length (bl_environmental_data)-1)) {
    plot (butterflies~bl_environmental_data[[i]], xlab = names (bl_environmental_data[i]))
}

# some example models ####
plot (grasshoppers~isolation)
# glm
glm (grasshoppers ~ isolation + bare_ground + heat_load , family = "poisson") %>% summary ()
# glmm mit sub_area als random intercept - fast das gleiche ergebnis
lme4::glmer (grasshoppers ~ isolation + bare_ground + heat_load + (1|sub_area), family = "poisson") %>% summary ()
# glmm mit sub_area als random intercept + random slope für isolation - isolation nicht mehr signifikant
lme4::glmer (grasshoppers ~ isolation + bare_ground + heat_load + (1+isolation|sub_area) + (1|sub_area), family = "poisson") %>% summary ()

# Create scatterplot
ggplot(bl_ecology_data, aes(x = isolation, y = grasshoppers, color = sub_area)) +
  geom_point() +
  labs(title = "Scatterplot with Color-coded Categories", x = "isolation", y = "grasshoppers") +
  facet_wrap(~ sub_area, scales = "free")  # Use facet_wrap to create separate plots for each level





plot (leafhoppers ~ winter_precip)
# glm
glm (leafhoppers ~ scale (winter_precip) , family = "poisson") %>% summary ()
# glmm mit sub_area als random intercept - vorzeichen von winter_precitation umgedreht!
lme4::glmer (leafhoppers ~ scale (winter_precip) + (1|sub_area), family = "poisson") %>% summary ()
# Create scatterplot
ggplot(bl_ecology_data, aes(x = winter_precip, y = leafhoppers, color = sub_area)) +
  geom_point() +
  labs(title = "Scatterplot with Color-coded Categories", x = "winter_precip", y = "leafhoppers")

ggplot(bl_ecology_data, aes(x = winter_precip, y = leafhoppers, color = sub_area)) +
  geom_point() +
  labs(title = "Scatterplot with Color-coded Categories", x = "winter_precip", y = "leafhoppers")+
  facet_wrap(~ sub_area, scales = "free")  # Use facet_wrap to create separate plots for each level


# nur linearer zusammenhang - nichts
glm (butterflies ~  snow_cover_duration , family = "poisson") %>% summary ()
# quadratischer zusammenhand - hochsignifikant
glm (butterflies ~  snow_cover_duration + I(snow_cover_duration^2) , family = "poisson") %>% summary ()



# # Ensure variables are within specified ranges
# ecology_data$Summer_Temperature <- pmax(pmin(ecology_data$Summer_Temperature, 21.2), 13.6)
# ecology_data$Winter_Temperature <- pmax(pmin(ecology_data$Winter_Temperature, 2), -8)
# ecology_data$Summer_Precipitation <- pmax(pmin(ecology_data$Summer_Precipitation, 1190), 750)
# ecology_data$Winter_Precipitation <- pmax(pmin(ecology_data$Winter_Precipitation, 1430), 600)
# ecology_data$Snow_Cover_Duration <- pmax(pmin(ecology_data$Snow_Cover_Duration, 175), 10)
bl_ecology_data <- cbind (bl_species_data, bl_environmental_data) %>% dplyr::tibble()



bl_ecology_data

summary (bl_ecology_data)
dplyr::glimpse (bl_ecology_data)

usethis::use_data(bl_species_data)
usethis::use_data(bl_environmental_data)
usethis::use_data(bl_ecology_data)

