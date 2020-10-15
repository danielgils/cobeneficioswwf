# This script is to run the analysis for each project
# Octubre 2020

rm(ls = list())
# Loading libraries
library(cobeneficioswwf)

## Medellin processing
# Defining route
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WWF/cobeneficioswwf/data/local/medellin/"
medellin <- setup_data(project = "medellin")
medellin <- setup_data()


trip_set <- read_csv("C:/Users/danie/Documents/R/win-library/4.0/cobeneficioswwf/data/local/medellin/trips_medellin.csv", col_types = cols())
demographic <- read_csv("C:/Users/danie/Documents/R/win-library/4.0/cobeneficioswwf/data/local/medellin/population_medellin.csv", col_types = cols())
GBD_DATA <- read_csv("C:/Users/danie/Documents/R/win-library/4.0/cobeneficioswwf/data/local/medellin/gbd_medellin.csv", col_types = cols())

pa_set <- read_csv("C:/Users/danie/Documents/R/win-library/4.0/cobeneficioswwf/data/local/medellin/pa_medellin.csv", col_types = cols())

injuries <- read_csv("C:/Users/danie/Documents/R/win-library/4.0/cobeneficioswwf/data/local/medellin/injuries_medellin.csv", col_types = cols())

