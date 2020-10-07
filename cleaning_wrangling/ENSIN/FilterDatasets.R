#' Script para organizar la información y sacar el dataset correspondiente a
#' cada ciudad
#' Daniel Gil
#' Agosto 2020

#' Se limpiar el Workspace
rm(list = ls())

#' Se cargan las librerias
library(tidyverse)

#' Se definen los marginal mets (MMETs) para cada actividad
MMET_walking <- 2.3 # METS = 3.3 According to International Physical Activity Questionnarie, 2005
MMET_moderate <- 3 # METS = 4 According to International Physical Activity Questionnarie, 2005
MMET_vigorous <- 7 # METS = 7 According to International Physical Activity Questionnarie, 2005

#' Se leen las tablas que nos interesan
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WWF/Datos/Global/ENSIN/base-ensin-2015-publica/Formato_Stata/"

hogar <- read_csv(paste0(route,"PTS.csv"))
persona <- read_csv(paste0(route,"PTS_2.csv"))
preescolar <- read_csv(paste0(route,"AF_PREESCOLARES.csv"))
gestante <- read_csv(paste0(route,"AF_GESTANTES.csv"))
gestante2 <- read_csv(paste0(route,"AF_A_GESTANTES.csv"))
escolar <- read_csv(paste0(route,"AF_ESCOLARES.csv"))
adoles <- read_csv(paste0(route,"AF_ADOLESCENTES.csv"))
adulto <- read_csv(paste0(route,"AF_ADULTOS.csv"))

#####
#' *Hogares*
#' Depto and Area are the variables that allow to filter for department and urban areas
# unique(hogar$nivel_urb)
# table(hogar$AREA, hogar$nivel_urb)
# unique(hogar$cabecera)
# table(hogar$AREA, hogar$cabecera)
names(hogar)

#' Filtro las columnas que me interesan
hogar2 <- subset(hogar, select = c("LLAVE_HOGAR", "Region", "Subregion", "DEPTO",
                                   "C_DEPTO", "AREA", "FactorExpansion"))
#####
#' *Personas*
names(persona)
#View(head(persona))

# counting people in urban areas (Cabecera)
#' 1: Urban area with more than 1 million inhabitants
#' 2: Urban area with 100 001 to 1 million inhabitants
#' 3: Urban area with less than 100 001 inhabitants
#' 4: rural areas
#' I compared this definitions with the document "libro_ensin_2015.pdf" pag 114
count(x = persona, nivel_urb, wt = FactorExpansionPer) # Same values
table(persona$nivel_urb, persona$cabecera)
table(persona$nivel_urb, persona$AREA) # OK

#' Age is column "PT5_5"
unique(persona$PT5_5) # Esta es la variable edad
table(persona$PT5_5, persona$edades)
table(persona$PT5_5, persona$edades2)

#' Sex is column "PT5_6" or "sexo"
#' 1: Hombre/Male
#' 2: Mujer/Female
unique(persona$PT5_6)
table(persona$PT5_6, persona$sexo)

#' Choose the columns that I need
persona2 <- subset(persona, select = c("LLAVE_HOGAR", "LLAVE_PERSONA", "cabecera",
                                       "PT5_5", "sexo", "FactorExpansionPer"))

#####
#' Merge de ambos datasets
persona_hogar <- merge(persona2, hogar2, by = "LLAVE_HOGAR", all.x = T)
View(head(persona_hogar))


#####
#' *AF_Adultos* PA of people from 18 to 64 years old
#' Questions are in page 667 file "libro-ensin-2015.pdf"
names(adulto)
View(head(adulto))

# Merge demographic and location info
adulto_persona <- merge(adulto, persona_hogar, by = "LLAVE_PERSONA", all.x = T)
names(adulto_persona)

#####
#' Creating PA variables
#' *ToDo*:
#' Check how to impute duration information for those that answer -6 o -3, which
#' means variable time or doesn't know/no opinion
unique(adulto_persona$AFAD22)
unique(adulto_persona$AFAD24A)
unique(adulto_persona$AFAD24B)
table(adulto_persona$AFAD16B, adulto_persona$AFAD16A, useNA = "always")

adulto_persona2 <- adulto_persona %>%
  mutate(id = LLAVE_PERSONA,
         sex = ifelse(sexo.x == 1, "male", "female"),
         age = PT5_5,
         ltpa_walk_duration = ifelse(AFAD14 == 1 & !is.na(AFAD14),
                                     ifelse(AFAD16A >= 0,
                                            AFAD16A * 60 + AFAD16B, 0),0),
         ltpa_walk_days = AFAD15,
         ltpa_walk_mmet_value = MMET_walking,
         ltpa_mod_duration = ifelse(AFAD18 == 1 & !is.na(AFAD18),
                                    ifelse(AFAD20A >= 0,
                                           AFAD20A * 60 + AFAD20B, 0),0),
         ltpa_mod_days = AFAD19,
         ltpa_mod_mmet_value = MMET_moderate,
         ltpa_vig_duration = ifelse(AFAD22 == 1 & !is.na(AFAD22),
                                    ifelse(AFAD24A >= 0,
                                           AFAD24A * 60 + AFAD24B, 0),0),
         ltpa_vig_days = AFAD23,
         ltpa_vig_mmet_value = MMET_vigorous
  ) %>%
  rowwise() %>%
  mutate(ltpa_min_week = sum(ltpa_walk_duration * ltpa_walk_days,
                             ltpa_mod_duration * ltpa_mod_days,
                             ltpa_vig_duration * ltpa_vig_days, na.rm = T),
         ltpa_marg_met = (sum(ltpa_walk_duration * ltpa_walk_days * ltpa_walk_mmet_value,
                              ltpa_mod_duration * ltpa_mod_days * ltpa_mod_mmet_value,
                              ltpa_vig_duration * ltpa_vig_days * ltpa_vig_mmet_value,
                              na.rm = T))/60,
         work_ltpa_marg_met = ltpa_marg_met
  )

# Filtering only the variables I need in ithim
#adulto_persona2 <- subset(adulto_persona2, select = c(""))

#' *ToDo*: physical activity in children and teenagers. Although there's no enough
#' information

# Imputing values for variable durations
# adulto_persona2$ltpa_walk_duration <-
# adulto_persona2$ltpa_mod_duration <-
# adulto_persona2$ltpa_vig_duration <-

#write.csv(adulto_persona2, "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/PhysicalActivity/clipboard.csv")


#####
#' *Filtros por departamento*
#' The sampling design does not cover specific cities, this sample is only
#' representativo at department level. So there are two choices here:
#' 1- Take individuals from the same department only in urban areas under the
#' assumption that people in the department make the same physical activity
#' that in the city.
#' 2- Take individuals in only urban areas with more than 1 million inhabitants
#' (regardless the department) under the assumption that people that live in bigger
#' cities make the same physical activity that in the city.
#' As consequence, I filter both datasets to use them in the analysis.

#' *Medellin*
medellin1 <- subset(adulto_persona2, DEPTO == "ANTIOQUIA" & cabecera.x == 1)
medellin2 <- subset(adulto_persona2, nivel_urb == 1)
write_csv(medellin1, "data/medellin/pa_medellin.csv")
write_csv(medellin2, "data/medellin/pa_medellin2.csv")


#' *Monteria*
monteria1 <- subset(adulto_persona2, DEPTO == "CORDOBA" & cabecera.x == 1)
monteria2 <- subset(adulto_persona2, nivel_urb == 1)
write_csv(monteria1, "data/monteria/pa_monteria.csv")
write_csv(monteria2, "data/monteria/pa_monteria2.csv")

#' *Cali*
cali1 <- subset(adulto_persona2, DEPTO == "VALLE DEL CAUCA" & cabecera.x == 1)
cali2 <- subset(adulto_persona2, nivel_urb == 1)
write_csv(cali1, "data/cali/pa_cali.csv")
write_csv(cali2, "data/cali/pa_cali2.csv")

#' *Pereira*
pereira1 <- subset(adulto_persona2, DEPTO == "RISARALDA" & cabecera.x == 1)
pereira2 <- subset(adulto_persona2, nivel_urb == 1)
write_csv(pereira1, "data/medellin/pa_pereira.csv")
write_csv(pereira2, "data/medellin/pa_pereira2.csv")
