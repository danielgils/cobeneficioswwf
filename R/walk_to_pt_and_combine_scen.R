#' Add walk to PT
#'
#' Adds a short walk stage to any PT trip if required.
#' Combines list of scenarios into one data frame
#'
#' @param trip_set list of data frames, trips from all scenarios
#'
#' @return data frame, all trips from all scenarios
#'
#' @export
walk_to_pt_and_combine_scen <- function(SYNTHETIC_TRIPS){
  rd_list <- list()
  for (i in 1:length(SYNTHETIC_TRIPS))
    rd_list[[i]] <- setDT(SYNTHETIC_TRIPS[[i]])

  # To save memory
  SYNTHETIC_TRIPS <- NULL

  ## pt = public transport
  pt_modes <- c('bus','minibus','subway','rail')

  ##!! this function and add_walk_trips need some attention
  if (ADD_WALK_TO_BUS_TRIPS)
    for (i in 1:length(rd_list)) { # Loop for each scenario
      # separate out bus trips
      pt_trips <- subset(rd_list[[i]], stage_mode %in% pt_modes)
      not_pt_trips <- subset(rd_list[[i]], !stage_mode %in% pt_modes)
      # divide bus trips into bus and pedestrian
      pt_walk_trips <- add_walk_trips(pt_trips)
      # recombine all trips,
      # pt_walk_trips have the distance recomputed by substracting walk_to_pt distance
      rd_list[[i]] <- rbind(not_pt_trips, pt_walk_trips[[1]], pt_walk_trips[[2]])
    }

  # Transform list to dataframe by appending each dataframe within the list
  trip_df <- do.call('rbind', rd_list)
  rd_list <- NULL

  ## update all distances and durations
  trip_df <- scale_trip_distances(trip_df)

  return(trip_df)

}


#' Scale trip distances
#'
#' Applies mode-specific distance scalars to all trips
#'
#' @param trips data frame, all trips from all scenarios
#'
#' @return data frame, all trips from all scenarios
#'
#' @export
scale_trip_distances <- function(trips){
  car_taxi_modes <- c('car','taxi','auto_rickshaw','shared_auto')
  pt_modes <- c('bus','minibus','subway','rail','walk_to_pt')
  ## omit trip distance as it has already been used to create scenarios and has no other use
  #column_names <- c('stage_distance','stage_duration')
  match_modes <- rep(1, nrow(trips))
  stage_modes <- trips$stage_mode
  # TODO: ask what is this for
  # match_modes[stage_modes %in% car_taxi_modes] <- DISTANCE_SCALAR_CAR_TAXI
  # match_modes[stage_modes == 'pedestrian'] <- DISTANCE_SCALAR_WALKING
  # match_modes[stage_modes %in% pt_modes] <- DISTANCE_SCALAR_PT
  # match_modes[stage_modes == 'cycle'] <- DISTANCE_SCALAR_CYCLING
  # match_modes[stage_modes == 'motorcycle'] <- DISTANCE_SCALAR_MOTORCYCLE
  # trips$stage_distance <- trips$stage_distance * match_modes
  # trips$stage_duration <- trips$stage_duration * match_modes

  trips
}
