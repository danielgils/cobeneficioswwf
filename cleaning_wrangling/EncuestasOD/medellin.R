#' Script para organizar la información de los viajes en Medellin
#' Daniel Gil
#' Octubre 2020

#' Se limpiar el Workspace
rm(list = ls())

#' Para mostrar/exportar con todos los decimales posibles
options(scipen = 50)

#' Se cargan las librerias
library(tidyverse)
library(readxl)

#' Se carga la funcion para estandarizar los modos
source("cleaning_wrangling/EncuestasOD/otrasfunciones.R")

#' Se importan los datasets
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WWF/Datos/Local/Medellin/EOD2017/"
trips <- read_excel(paste0(route, "EOD_2017_DatosViajes.xlsx"), sheet = "DATOS VIAJES")
trips <- trips[rowSums(is.na(trips)) != ncol(trips), ] # Removing files that only have NAs
#' When reading person there are some warnings related to people without age
person <- read_excel(paste0(route, "EOD_2017_DatosViajes.xlsx"), sheet = "DATOS MORADORES")
#hogar <- read_excel(paste0(route, "EOD_2017_DatosViajes.xlsx"), sheet = "DATOS HOGARES")

#' Se importa la jerarquía usada para definir el modo principal de cada viaje.
#' Este archivo lo hice yo (Daniel), a mi criterio. En teoria Saul Andrés
#' Rivera Betancur <saul.rivera@metropol.gov.co> del ANVA, quedo de enviarme la
#' jerarquia pero no la he recibido
main_mode <- read_excel(paste0(route, "Jerarquia.xlsx"), sheet = "Medellin")

# In person, create an ID for each person taking into account its household
person <- person %>%
  mutate(id_person = paste(ID_HOGAR, ORDEN_MORADOR, sep = "-"),
         participant_id = paste0(ID_HOGAR, ORDEN_MORADOR),
         sex = ifelse(GENERO == "1", "male", "female"),
         age = EDAD)
#length(unique(person$id_person)) == nrow(person) #OK
#length(unique(person$participant_id)) == nrow(person) #OK

# In trips, create new variables:
# ID for each person taking into account its household
# Trip id
trips2 <- trips %>%
  mutate(id_person = paste(ID_HOGAR, ID_MORADOR, sep = "-"),
         participant_id = paste0(ID_HOGAR,ID_MORADOR),
         id_trip = paste(id_person, SEC_VIAJE, sep = "-"),
         trip_id = paste0(participant_id, SEC_VIAJE),
         trip_duration = as.numeric(difftime(HORA_D, HORA_O, units = "mins")),
         # Replace modes by its hierarchy
         mode_e1 = main_mode$Jerarquia_modo_principal[
           match(DESC_MODO_TTE_E1, main_mode$Modo_transporte_formulario_EODH)],
         mode_e2 = main_mode$Jerarquia_modo_principal[
           match(DESC_MODO_TTE_E2, main_mode$Modo_transporte_formulario_EODH)],
         mode_e3 = main_mode$Jerarquia_modo_principal[
           match(DESC_MODO_TTE_E3, main_mode$Modo_transporte_formulario_EODH)],
         mode_e4 = main_mode$Jerarquia_modo_principal[
           match(DESC_MODO_TTE_E4, main_mode$Modo_transporte_formulario_EODH)],
         mode_e5 = main_mode$Jerarquia_modo_principal[
           match(DEC_MODO_TTE_E5, main_mode$Modo_transporte_formulario_EODH)],
         mode_e6 = main_mode$Jerarquia_modo_principal[
           match(DESC_MODO_TTE_E6, main_mode$Modo_transporte_formulario_EODH)],
         mode_e7 = main_mode$Jerarquia_modo_principal[
           match(DESC_MODO_TTE_E7, main_mode$Modo_transporte_formulario_EODH)]
  ) %>%
  rowwise() %>% mutate(
    main_modes = min(mode_e1, mode_e2, mode_e3, mode_e4, mode_e5, mode_e6,
                     mode_e7, na.rm = T),
    trip_mode = main_mode$ITHIM[
      match(main_modes, main_mode$Jerarquia_modo_principal)]
  )

# Merge sex and age variables
trips3 <- merge(trips2, person[,c("participant_id", "sex", "age")],
                by = "participant_id", all.x = T)

# Remove dataframes I don't need anymore
rm(person, trips, trips2)

# write_csv(trips3, 'data/local/medellin_wb/medellin_wb_trip.csv')
#
# trip <- read_csv('data/local/medellin_wb/medellin_wb_trip.csv')


# Standardized travel modes
trips4 <- standardize_modes(trips3, mode = c('trip'))

rd <- trips4 %>% select(participant_id, age, sex, trip_id, trip_mode, trip_duration)

# Export
write_csv(rd, 'data/local/medellin/trips_medellin.csv')

