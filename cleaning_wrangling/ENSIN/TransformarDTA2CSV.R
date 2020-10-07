#' Script para transformar el formato de la base de datos de ENSIN 2015
#' Daniel Gil
#' Agosto 2020

#' Se cambia la ruta del wd
setwd("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WWF/Datos/Global/ENSIN/base-ensin-2015-publica/Formato_Stata")

#' Se cargan las librerias
#library(foreign) # no funcionó con este paquete
library(haven)

#' Se listan los archivos en el folder
files <- list.files()

for(i in 1:length(files)){
  print(files[i])
  temp <- read_dta(files[i])
  write.csv(temp,paste(gsub(".dta", ".csv", files[i])))
}
gsub(".dta", ".csv", files[1])
data <- read_dta("AF_A_GESTANTES.dta")
head(data)
