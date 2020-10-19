#' Set up baseline scenario data frame
#'
#' Create scenario by adding distance categories and scenario = baseline column
#' to trip set data frame
#'
#' @param trip_set data frame of trips
#'
#' @return trip_set as baseline scenario
#'
#' @export
setup_baseline_scenario <- function(trip_set){
  # Initialize distance categories
  ## Distance categories are used in scenario generation.
  ## They correspond to e.g. ``long trips'' and ``short trips''
  trip_set$trip_distance_cat <- 0

  # Categorizing distances
  for (i in 2:length(DIST_LOWER_BOUNDS) - 1) {
    trip_set$trip_distance_cat[trip_set$trip_distance >= DIST_LOWER_BOUNDS[i] &
                                 trip_set$trip_distance < DIST_LOWER_BOUNDS[i + 1]] <-
      DIST_CAT[i]
  }

  # Trips with distances larger than the larger category are defined as this category
  trip_set$trip_distance_cat[trip_set$trip_distance >= DIST_LOWER_BOUNDS[length(DIST_LOWER_BOUNDS)]] <- DIST_CAT[length(DIST_LOWER_BOUNDS)]

  # Create column that identifies trips in the baseline scenario
  trip_set$scenario <- "Baseline"

  return(trip_set)
}
