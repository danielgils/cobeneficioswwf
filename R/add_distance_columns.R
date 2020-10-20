#' Add distance columns to injury tables
#'
#' Add distance columns to injury tables, matching on information in the injury contingency table
#'
#' @param injury_table (list of) data frame(s) to be edited
#' @param mode_names which modes to take distances for
#' @param true_distances_0 distances to add to injury table
#' @param dist table used to access bus distance
#' @param scenarios which scenarios to process
#'
#' @return edited (list of) data frame(s)
#'
#' @export
add_distance_columns <- function(injury_table, mode_names, true_distances_0,
                                 dist, scenarios = SCEN){

  injury_temp <- injury_table
  # Dummy to know if age_cat and cas_gender is in injury dataset
  by_age <- 'age_cat' %in% names(injury_table[[1]])
  by_gender <- 'cas_gender' %in% names(injury_table[[1]])
  # If they're not in the dataset just add them as a constant
  for (type in INJURY_TABLE_TYPES) { # Loop for whw and nov
    if (!by_age)
      injury_temp[[type]]$age_cat <- 1
    if (!by_gender)
      injury_temp[[type]]$cas_gender <- 1
  } # End for
  u_gen <- unique(injury_temp[[1]]$cas_gender)
  u_age <- unique(injury_temp[[1]]$age_cat)
  # Create a table of unique ids for each combination of age range and sex
  dem_index_table <- expand.grid(cas_gender = u_gen, age_cat = u_age)

  # If age and gender are not in injuries then change age_cat and sex for 1
  if (!by_age)
    true_distances_0$age_cat <- 1
  if (!by_gender)
    true_distances_0$sex <- 1

  # Create index for each combination of age range and sex in distance dataset
  true_distances_0$dem_index <- length(u_gen) *
    (match(true_distances_0$age_cat, u_age) - 1) +
    match(true_distances_0$sex, u_gen)

  cas_mode_indices <- list()
  dem_index <- list()
  # initialise tables and store indices
  for (type in INJURY_TABLE_TYPES) { # Loop for whw and nov
    ##TODO make contingency table without prior knowledge of column names
    # create index for each combintation of age range and sex in injuries dataset
    gen_index <- match(injury_temp[[type]]$cas_gen, u_gen)
    age_index <- match(injury_temp[[type]]$age_cat, u_age)
    dem_index[[type]] <- length(u_gen) * (age_index - 1) + gen_index
    cas_mode_indices[[type]] <- match(injury_table[[type]]$cas_mode, mode_names)
  } # End loop

  if ('whw' %in% INJURY_TABLE_TYPES) {
    ## group 2W and 3W striking vehicles
    strike_distances <- true_distances_0
    strike_distances$motorcycle <- rowSums(strike_distances[,colnames(strike_distances) %in% c('motorcycle','auto_rickshaw')])
    strike_modes <- unique(as.character(injury_table$whw$strike_mode))
    ##!! this order matters to strike_distance_sums later
    strike_mode_indices <- match(injury_table$whw$strike_mode,
                                 unique(c(mode_names, strike_modes)))
  } # End if

  ## Calculated distances
  ## true distances should be the total for the whole population for a whole year.
  injuries_list <- list()
  if (!'bus_driver' %in% mode_names)
    bus_base <- dist[which(dist$stage_mode == 'bus_driver'), 2]
  for (i in 1:length(scenarios)) { # Loop for each scenario
    scen <- scenarios[i]
    injuries_list[[scen]] <- list()
    # Filter distances for i-th scenario
    true_scen_dist <- subset(true_distances_0, scenario == scen)
    # Since there's only a single row for each combination of ('age_cat','sex','scenario','sex_age','dem_index')
    # This line is only sorting the dataframe
    dist_summary <- as.data.frame(t(sapply(sort(unique(true_scen_dist$dem_index)),
                                           function(x)
                                             colSums(subset(true_scen_dist,
                                                            dem_index == x)[,!colnames(true_scen_dist) %in% c('age_cat','sex','scenario','sex_age','dem_index')]))))
    # apply casualty distance sums (sum for each column)
    distance_sums <- sapply(mode_names, function(x) sum(dist_summary[[x]]))
    if ('whw' %in% INJURY_TABLE_TYPES) {
      strike_true_scen_dist <- subset(strike_distances, scenario == scen) # Strike distance is the same as regular distance
      strike_dist_summary <- as.data.frame(t(sapply(unique(strike_true_scen_dist$dem_index),
                                                    function(x)
                                                      colSums(subset(strike_true_scen_dist,
                                                                     dem_index == x)[,!colnames(strike_true_scen_dist) %in% c('age_cat','sex','scenario','sex_age','dem_index')]))))
      # apply strike distance sums
      strike_distance_sums <- sapply(mode_names, function(x) sum(strike_dist_summary[[x]]))
      old_length <- length(strike_distance_sums)
      # Since there are strike modes that are not in casualty modes, such as truck,
      # then this loop is to assign to it the average distance of other modes
      for (str_mode in strike_modes[!strike_modes %in% names(strike_distance_sums)]) {
        strike_distance_sums <- c(strike_distance_sums,mean(strike_distance_sums))
      } # End for
      names(strike_distance_sums)[(old_length + 1):length(strike_distance_sums)] <- strike_modes[!strike_modes %in% names(strike_distance_sums)]
    } # End if
    for (type in INJURY_TABLE_TYPES) { # Loop for whw and nov
      injuries_list[[scen]][[type]] <- injury_table[[type]]
      ##TODO get distances without prior knowledge of column names
      ##TODO differentiate between driver and passenger for casualty and striker distances

      # initialise all strike distances as 1
      injuries_list[[scen]][[type]]$strike_distance <- 1
      injuries_list[[scen]][[type]]$strike_distance_sum <- 1

      injuries_list[[scen]][[type]]$cas_distance_sum <- distance_sums[cas_mode_indices[[type]]]

      # apply group-level casualty distances
      injuries_list[[scen]][[type]]$cas_distance <- as.numeric(as.data.frame(dist_summary)[cbind(dem_index[[type]],cas_mode_indices[[type]])])

      # apply striker distances for whw
      if (type == 'whw') {
        injuries_list[[scen]][[type]]$strike_distance <- strike_distance_sums[strike_mode_indices]
        injuries_list[[scen]][[type]]$strike_distance_sum <- injuries_list[[scen]][[type]]$strike_distance
      } else {
        if (!'bus_driver' %in% mode_names) {

          # browser()

          if (length(bus_base) > 0) {
            injuries_list[[scen]][[type]]$strike_distance[injuries_list[[scen]][[type]]$strike_mode == 'bus_driver'] <- dist[which(dist$stage_mode == 'bus_driver'),i + 1] / bus_base
            injuries_list[[scen]][[type]]$strike_distance_sum[injuries_list[[scen]][[type]]$strike_mode == 'bus_driver'] <- dist[which(dist$stage_mode == 'bus_driver'),i + 1] / bus_base
          }
        }
      }

      # omit any rows with zero travel
      injuries_list[[scen]][[type]] <- subset(injuries_list[[scen]][[type]],
                                              strike_distance > 0 &
                                                cas_distance > 0)
    } # End loop for whw and nov
  } # End loop for each scenario

  return(injuries_list)
}
