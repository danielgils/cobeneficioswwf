#' Get distances and durations
#'
#' Summaries of total distances and durations spent travelling per mode per scenario
#'
#' @param trip_scen_sets list of synthetic trip sets for scenarios
#'
#' @return list of table of (total) distances and durations per mode per scenario
#'
#' @export
dist_dur_tbls <- function(trip_scen_sets){

  # Create a copy
  bs <- trip_scen_sets
  # To save memory
  trip_scen_sets <- NULL

  # Unique stage's modes
  stage_modes <- unique(bs$stage_mode)

  ## calculate all distances & durations for each scenario
  l_dist <-  l_dur <- list()
  for (i in 1:length(SCEN)) { # Loop for each scenario
    # Compute total stage distance and duration by mode and scenario
    local <- bs[bs$scenario == SCEN[i],]
    local_dist <- local[,.(sum_dist = sum(stage_distance)),by = 'stage_mode']
    local_dur <- local[,.(sum_dur = sum(stage_duration)),by = 'stage_mode']

    # If walk_to_pt has been added, add these distances/duration to pedestrian
    if ("walk_to_pt" %in% local_dist$stage_mode) {
      local_dist$sum_dist[local_dist$stage_mode == "pedestrian"] <-
        local_dist$sum_dist[local_dist$stage_mode == "pedestrian"] +
        local_dist$sum_dist[local_dist$stage_mode == "walk_to_pt"]
      local_dur$sum_dur[local_dur$stage_mode == "pedestrian"] <-
        local_dur$sum_dur[local_dur$stage_mode == "pedestrian"] +
        local_dur$sum_dur[local_dur$stage_mode == "walk_to_pt"]
    } # End if

    # store results
    colnames(local_dist)[2] <- SCEN[i]
    l_dist[[i]] <- local_dist
    colnames(local_dur)[2] <- SCEN[i]
    l_dur[[i]] <- local_dur
  } # End for
  bs <- NULL

  ## join distances & durations in a single dataframe
  for (i in 1:length(l_dist)) { # Loop for each scenario
    if (i == 1) {
      local_dist <- l_dist[[i]]
      local_dur <- l_dur[[i]]
    }else{
      # on is used as a key to paste info for each scenario. Works like a left_joing
      local_dist <- local_dist[l_dist[[i]], on = "stage_mode"]  #<- left_join(local_dist, , by = "stage_mode")
      local_dur <- local_dur[l_dur[[i]], on = "stage_mode"]  # <- left_join(local_dur, l_dur[[i]], by = "stage_mode")
    }
  }

  # Remove short walk_to_pt from distance/duration output
  dist <- as.data.frame(local_dist[local_dist$stage_mode != 'walk_to_pt',])
  dur <- as.data.frame(local_dur[local_dur$stage_mode != 'walk_to_pt',])

  # Transform to character
  dist$stage_mode <- as.character(dist$stage_mode)
  dur$stage_mode <- as.character(dur$stage_mode)

  ## bus travel is linear in bus passenger travel
  bus_passenger_row <- which(dist$stage_mode == 'bus')
  if ('bus_driver' %in% dist$stage_mode) {
    bus_driver_row <- which(dist$stage_mode == 'bus_driver')
    base_col <- which(colnames(dist) == 'Baseline')
    dist[bus_driver_row, colnames(dist) %in% SCEN] <- as.numeric(dist[bus_driver_row, base_col] / dist[bus_passenger_row,base_col]) * dist[bus_passenger_row, colnames(dist) %in% SCEN]
    dur[bus_driver_row, colnames(dur) %in% SCEN] <- as.numeric(dur[bus_driver_row, base_col] / dur[bus_passenger_row, base_col]) * dur[bus_passenger_row, colnames(dur) %in% SCEN]
  }else{
    dist <- rbind(dist,dist[bus_passenger_row,])
    dist[nrow(dist),1] <- 'bus_driver'
    dur <- rbind(dur,dur[bus_passenger_row,])
    dur[nrow(dur),1] <- 'bus_driver'
  }

  return(list(dist = dist, dur = dur))
}
