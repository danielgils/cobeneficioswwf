#' Script to get trips dataset cleaned for Cali
#' Daniel Gil
#' October 2020

#' Cleaning Workspace
rm(list = ls())

#' To show/export more decimal points
options(scipen = 50)

#' Loading libraries
library(tidyverse)
library(readxl)

#' Loading function that standardizes modes
#' Here the working directory should be home in the project cobeneficioswwf
source("cleaning_wrangling/ODSurveys/otherfunctions.R")

#####
#' Importing datasets
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WWF/Datos/Local/Cali/Trips/1. Base de datos EODH 2015/2. Módulos/"

#'*Trips*
#' According to the documentation of the survey, the dataset with imputed trips
#' is the one that we should use (pag 102, "160216_Producto 4_Indicadores EODH.pdf")
#' The number of rows corresponds to what is mentioned in that same document.
#' Before importing this dataset I added a new column called "EMPTY" because values
#' didn't correspond to the label of the variable. By creating this empty label
#' everything makes more sense now. This file is now called:
#' "MOD_D_VIAJES_IMP_Modified.xlsx"
trips <- read_excel(paste0(route, "MOD_D_VIAJES_IMP_Modified.xlsx"))
trips <- trips[rowSums(is.na(trips)) != ncol(trips), ] # Removing files that only have NAs

#'*Personas*
#' In the original file the first row is the label of each variable and the
#' second row is the meaning of that label. I had to remove this second row and
#' delete some images that were inside the file to read it easily here in R.
#' This files is now called: MOD_B_PERSONAS_Modified.xlsx
#' When reading person there are some warnings related to people without age
person <- read_excel(paste0(route, "MOD_B_PERSONAS_Modified.xlsx"))
#hogar <- read_excel(paste0(route, "EOD_2017_DatosViajes.xlsx"), sheet = "DATOS HOGARES")

#'*Hierarchy main mode*
#' In this case I didn't have to import any hierarchy to define the main mode of
#' each trip because only P_E1_14_D have data for all rows, and only in P_E3_14_D
#' There were 773 rows with info (less than 2%). The remaining variables about
#' stages didn't have any information
#' However, I have to make the translation of these modes taking into account
#' the meaning of each number from Pag 3 "150508_EODH_TABLAS_AUX_V06.pdf"
main_mode <- read_excel(paste0(route, "TraduccionModos.xlsx"))

# In person, create an ID for each person taking into account its household
#table(person$P_04_B, useNA = "always")
#table(person$P_05_B, useNA = "always")
person <- person %>%
  mutate(id_person = paste(ORDEN, ID_PER, sep = "-"),
         participant_id = paste0(ORDEN, ID_PER),
         sex = ifelse(P_04_B == "HOMBRE", "male", "female"),
         age = P_05_B)
#length(unique(person$id_person)) == nrow(person) #OK
#length(unique(person$participant_id)) == nrow(person) #OK

#' In trips, create new variables:
#' ID for each person taking into account its household
#' Trip id
#table(trips$P_E1_14_D, useNA = "always")
trips2 <- trips %>%
  mutate(id_person = paste(ORDEN, ID_PER, sep = "-"),
         participant_id = paste0(ORDEN, ID_PER),
         id_trip = DONANTE,
         trip_id = paste0(participant_id, NO_VIAJE),
         trip_duration = T_VIAJE,
         trip_mode = main_mode$ITHIM[
           match(P_E1_14_D, main_mode$Codigo)]
         )

#table(trips2$P_E1_14_D, trips2$trip_mode)

# Merge sex and age variables
trips3 <- merge(trips2, person[,c("participant_id", "sex", "age")],
                by = "participant_id", all.x = T)

# Remove dataframes I don't need anymore
rm(person, trips, trips2)

# Standardized travel modes for the package
trips4 <- standardize_modes(trips3, mode = c('trip'))

rd <- trips4 %>% select(participant_id, age, sex, trip_id, trip_mode, trip_duration)

# Export
write_csv(rd, 'inst/data/local/cali/trips_cali.csv')

