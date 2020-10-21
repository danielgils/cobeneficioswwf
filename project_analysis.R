# This script is to run the analysis for each project
# Octubre 2020

rm(list = ls())

# Loading libraries
library(cobeneficioswwf)

## Medellin processing
## Routes medellin datasets
# trip_set <- read_csv("C:/Users/danie/Documents/R/win-library/4.0/cobeneficioswwf/data/local/medellin/trips_medellin.csv", col_types = cols())
# demographic <- read_csv("C:/Users/danie/Documents/R/win-library/4.0/cobeneficioswwf/data/local/medellin/population_medellin.csv", col_types = cols())
# GBD_DATA <- read_csv("C:/Users/danie/Documents/R/win-library/4.0/cobeneficioswwf/data/local/medellin/gbd_medellin.csv", col_types = cols())
# pa_set <- read_csv("C:/Users/danie/Documents/R/win-library/4.0/cobeneficioswwf/data/local/medellin/pa_medellin.csv", col_types = cols())
# injuries <- read_csv("C:/Users/danie/Documents/R/win-library/4.0/cobeneficioswwf/data/local/medellin/injuries_medellin.csv", col_types = cols())
# route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WWF/cobeneficioswwf/data/local/medellin/"
medellin <- setup_data(city = "medellin")
# All synthetic trips
dim(medellin$trip_scen_sets)
table(medellin$trip_scen_sets$trip_mode, medellin$trip_scen_sets$scenario)
# Distances and durations
medellin$dist
medellin$dur
# Distance for the whole population
medellin$true_dist
# Distance for the while population by age_cat, sex scenario and stage_mode
medellin$inj_distances$true_distances
# Data used for modelling
head(medellin$inj_distances$injuries_list$Baseline$whw)
# Model trained with baseline datasets
medellin$inj_distances$reg_model$whw$coefficients

# Output de tigthat
medellin$outcomes <- run_model(ithim_object = medellin)

# Mmets in synthetic population (dataframe)
dim(medellin$outcomes$mmets)
head(medellin$outcomes$mmets)

# Estimation of pm in each scenario
medellin$outcomes$scenario_pm

# PM dose in synthetic population (dataframe)
dim(medellin$outcomes$pm_conc_pp)
head(medellin$outcomes$pm_conc_pp)

# Estimation of road-injury deaths for each combination of age,sex, scenario and mode
dim(medellin$outcomes$injuries)
View(medellin$outcomes$injuries)

# Estimation of road-injury deaths and Ylls for baseline
medellin$outcomes$ref_injuries

# Estimation of deaths for each cause (joined AP and PA), age, sex with respect
# to baseline
dim(medellin$outcomes$hb$deaths)
View(medellin$outcomes$hb$deaths)

# Estimation of ylls for each cause (joined AP and PA), age, sex with respect
# to baseline
dim(medellin$outcomes$hb$ylls)
View(medellin$outcomes$hb$ylls)

# Estimation of deaths for each cause (AP and PA separately), age, sex with
# respect to baseline
dim(medellin$outcomes$pathway_hb$deaths)
View(medellin$outcomes$pathway_hb$deaths)

# Estimation of ylls for each cause (AP and PA separately), age, sex with
# respect to baseline
dim(medellin$outcomes$pathway_hb$ylls)
View(medellin$outcomes$pathway_hb$ylls)

# Estimation of who-hit-whom matrices
round(medellin$outcomes$whw$Baseline$whw, 2)
round(medellin$outcomes$whw$Baseline$nov, 2)
