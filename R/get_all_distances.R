#' Sequence to get distance data
#'
#' Sequence of function calls to get distance data for modules from synthetic population
#'
#' @param ithim_object list containing synthetic trip set
#'
#' @return ithim_object again, with additional distance objects
#'
#' @export
get_all_distances <- function(ithim_object){

  # Generate distance and duration matrices
  dist_and_dir <- dist_dur_tbls(ithim_object$trip_scen_sets)

  ithim_object$dist <- dist_and_dir$dist
  ithim_object$dur <- dist_and_dir$dur

  trip_scen_sets <- ithim_object$trip_scen_sets

  # Use demographic
  pop <- DEMOGRAPHIC

  # Rename col
  pop <- pop %>% dplyr::rename(age_cat = age)

  # trip_scen_sets <- io$delhi$trip_scen_sets
  #
  # pop <- io$delhi$demographic

  total_synth_pop <- nrow(SYNTHETIC_POPULATION)

  # Recalculate dist by using total distance - using overall population
  # This is like an estimate for the whole population
  dist <- trip_scen_sets %>%
    group_by(stage_mode, scenario) %>%
    summarise(ave_dist = sum(stage_distance) / total_synth_pop * sum(pop$population)) %>%
    spread(scenario, ave_dist) # Spread is like reshape in wide format

  # If walk_to_pt is in distance/duration output then sum them and delete walk_to_pt
  if ('pedestrian' %in% dist$stage_mode && 'walk_to_pt' %in% dist$stage_mode) {
    dist[dist$stage_mode == "pedestrian",][2:ncol(dist)] <- dist[dist$stage_mode == "pedestrian",][2:ncol(dist)] +
      dist[dist$stage_mode == "walk_to_pt",][2:ncol(dist)]
    dist <- dist %>% filter(stage_mode != 'walk_to_pt')
  } # End if

  # browser()

  if (any(names(INJURY_TABLE) %in% 'whw')) {
    if (any(unique(INJURY_TABLE$whw$strike_mode) %in% 'unknown')) {

      temp <- dist[1,] %>% as.data.frame()
      temp[1,] <- c('unknown', rep(1, ncol(dist) - 1))

      # dist <- plyr::rbind.fill(dist, temp)

      # dist_and_dir$dur <- dist_and_dir$dur %>%
      #   rbind(c('unknown', rep(1, ncol(dist_and_dir$dur) - 1)))

    }

  }

  ## for injury_function
  # get average total distances by sex and age cat
  journeys <- trip_scen_sets %>%
    group_by(age_cat, sex, stage_mode, scenario) %>%
    summarise(tot_dist = sum(stage_distance) / total_synth_pop)
  trip_scen_sets <- NULL

  # Add population values by sex and age category
  journeys <- dplyr::left_join(journeys, pop, by = c('sex', 'age_cat'))

  # Calculate total distance by population
  journeys$tot_dist <- journeys$tot_dist * journeys$population

  # Remove additional population column
  journeys <- journeys %>% dplyr::select(-population)

  # dist <- journeys %>% group_by(stage_mode, scenario) %>% summarise(dist = sum(tot_dist)) %>% spread(scenario, dist)
  #
  # if ('walk_to_pt' %in% dist$stage_mode && 'walk_to_pt' %in% dist$stage_mode){
  #   dist[dist$stage_mode == "pedestrian",][2:ncol(dist)] <- dist[dist$stage_mode == "pedestrian",][2:ncol(dist)] +
  #     dist[dist$stage_mode == "pedestrian",][2:ncol(dist)]
  #
  #   dist <- dist %>% filter(stage_mode != 'walk_to_pt')
  # }

  # browser()

  # Add true_dist to the ithim_object. This is the distance estimated for the whole population
  ithim_object$true_dist <- dist

  # distances for injuries calculation
  ithim_object$inj_distances <- distances_for_injury_function(journeys = journeys, dist = dist)
  return(ithim_object)
}
