# Esta funcion la cogi desde el paquete de ithimr
standardize_modes <- function(trip, mode){
  # Read lookup table
  smodes <- read_csv('inst/data/global/standardized_modes.csv')
  # Separate rows
  smodes <- smodes %>% separate_rows(original, sep = ';')

  smodes <- smodes %>%
    mutate(across(where(is.character), str_trim))

  if (length(mode) == 1){
    if (mode == 'stage')
      trip$stage_mode <- smodes$exhaustive_list[match(trip$stage_mode, smodes$original)]
    else
      trip$trip_mode <- smodes$exhaustive_list[match(trip$trip_mode, smodes$original)]
  }else if (length(mode) == 2){
    if (all(mode %in% c('stage', 'trip'))){
      trip$trip_mode <- smodes$exhaustive_list[match(trip$trip_mode, smodes$original)]
      trip$stage_mode <- smodes$exhaustive_list[match(trip$stage_mode, smodes$original)]
    }
  }

  return(trip)

}
