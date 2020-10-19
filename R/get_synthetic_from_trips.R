#' Generate synthetic data from trip set
#'
#' Sequence of functions to set up the synthetic population, the synthetic trips,
#' and the scenarios. Also sets global variables for later use.
#'
#' @return data frame of all trips from all scenarios
#'
#' @export
get_synthetic_from_trips <- function(){

  ##!! to get the right order of trip columns; needed if trips are added
  # if (CITY == 'bogota_wb') {
  #   raw_trip_set <- data.frame(trip_id=TRIP_SET$trip_id,
  #                              trip_mode=TRIP_SET$trip_mode,
  #                              trip_distance=TRIP_SET$trip_distance,
  #                              stage_mode=TRIP_SET$stage_mode,
  #                              stage_distance=TRIP_SET$stage_distance,
  #                              stage_duration=TRIP_SET$stage_duration,
  #                              participant_id=TRIP_SET$participant_id,
  #                              age=TRIP_SET$age,
  #                              sex=TRIP_SET$sex,
  #                              strata = TRIP_SET$strata,
  #                              limitation = TRIP_SET$limitation,
  #                              trip_motive = TRIP_SET$trip_motive,
  #                              trip_start_time = TRIP_SET$trip_start_time,
  #                              trip_end_time = TRIP_SET$trip_end_time,
  #                              stringsAsFactors = F)
  # } else {
    raw_trip_set <- data.frame(trip_id = TRIP_SET$trip_id,
                               trip_mode = TRIP_SET$trip_mode,
                               trip_distance = TRIP_SET$trip_distance,
                               stage_mode = TRIP_SET$stage_mode,
                               stage_distance = TRIP_SET$stage_distance,
                               stage_duration = TRIP_SET$stage_duration,
                               participant_id = TRIP_SET$participant_id,
                               age = TRIP_SET$age,
                               sex = TRIP_SET$sex,
                               stringsAsFactors = F)
  #} # End if

  # To save memory
  TRIP_SET <- NULL

  # TODO: Check whether I need to add ghost_trips
  ## add bus and truck trips
  # if (ADD_BUS_DRIVERS)
  #   raw_trip_set <- add_ghost_trips(raw_trip_set)
  # if (ADD_TRUCK_DRIVERS)
  #   raw_trip_set <- add_ghost_trips(raw_trip_set, trip_mode = 'truck',
  #                                   distance_ratio = TRUCK_TO_CAR_RATIO*DISTANCE_SCALAR_CAR_TAXI,
  #                                   reference_mode = 'car')
  # ## because we have the fraction of total MC travel that is fleet, we need to
  # # adjust the parameter to compute fleet travel from non-fleet motorcycle travel
  # if (ADD_MOTORCYCLE_FLEET)
  #   raw_trip_set <- add_ghost_trips(raw_trip_set, trip_mode = 'motorcycle',
  #                                   distance_ratio = FLEET_TO_MOTORCYCLE_RATIO/(1 - FLEET_TO_MOTORCYCLE_RATIO),
  #                                   reference_mode = 'motorcycle')

  # create synthetic population and filter trips with mode == "other" or not in our
  # vehicle inventory
  synth_pop <- create_synth_pop(raw_trip_set)

  # To save memory
  raw_trip_set <- NULL

  # Save synthetic population in Global Env.
  SYNTHETIC_POPULATION <<- synth_pop$synthetic_population
  trip_set <- synth_pop$trip_set

  # To save memory
  synth_pop <- NULL

  # Create new columns to define the baseline scenario (current situation)
  trip_set <- setup_baseline_scenario(trip_set)

  # Create scenarios based on trip_set
  # TODO: I have to update create_all_scenarios and create_scenarios functions
  SYNTHETIC_TRIPS <- create_all_scenarios(trip_set)

  # Define some useful variables.
  NSCEN <<- length(SYNTHETIC_TRIPS) - 1
  SCEN <<- sapply(SYNTHETIC_TRIPS, function(x) x$scenario[1])
  SCEN_SHORT_NAME <<- c("base",paste0("scen", 1:NSCEN))

  # add walk-to-bus trips, as appropriate, and combines list of scenarios
  trip_scen_sets <- walk_to_pt_and_combine_scen(SYNTHETIC_TRIPS)
  trip_scen_sets

}
