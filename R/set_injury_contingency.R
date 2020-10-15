#' Create contingency table from itemised list of injuries
#'
#' One of the inputs is a list of injury events. This function aggregates injuries
#' by type into a long contingency table with prespecified column names.
#' Write tables to global environment.
#'
#' @param injuries data frame of injury events
#'
#'
#' @export
set_injury_contingency <- function(injuries){
  # Define modes that appears in the tripset, leaving out those modes that are not
  # denfined in mode_speeds, e.g., other
  mode_names <- c(intersect(unique(TRIP_SET$stage_mode), MODE_SPEEDS$stage_mode),"pedestrian")
  mode_names <- mode_names[mode_names != 'other']
  # TODO: I have to ask what is bus_drivers and truck_drivers mean.
  # if(ADD_BUS_DRIVERS) mode_names <- c(mode_names,'bus_driver')
  # if(ADD_TRUCK_DRIVERS) mode_names <- c(mode_names,'truck')

  injury_list <- list()
  injury_table_types <- c()
  # Injuries for incidents with other cars (whw: who hit whom)
  if (length(unique(injuries$strike_mode)) == 1 &&
      !'nov' %in% injuries$strike_mode ||
      length(unique(injuries$strike_mode)) > 1) {
    injury_list$whw <- subset(injuries, cas_mode %in% mode_names & strike_mode != 'nov')
    injury_table_types <- c(injury_table_types,'whw')
  }
  # Injuries for incidents with no other vehicle (NOV)
  if ('nov' %in% injuries$strike_mode) {
    injury_list$nov <- subset(injuries,cas_mode %in% mode_names & strike_mode == 'nov')
    injury_table_types <- c(injury_table_types,'nov')
  }

  injury_table <- list()
  for (type in c(injury_table_types)) { # Loop for each type of incident
    keep_names <- names(injury_list[[type]]) %in% c('year', 'cas_mode',
                                                    'strike_mode', 'age_cat',
                                                    'cas_gender')
    # summarise list of injuries by group
    setDT(injury_list[[type]]) # Save this object as data.table

    # Compute the number of accidents and the average weight for each combination
    # of year, cas_mode, strike_mode, age_cat and cas_gender
    injury_summary <- as.data.frame(injury_list[[type]][,.(count = .N, weight = mean(weight)),
                                                        by = c(names(injury_list[[type]])[keep_names])])

    # Create all possible combinations of year, cas_mode, strike_mode, age_cat and cas_gender
    injury_table[[type]] <- expand.grid(lapply(as.data.frame(injury_list[[type]])[,keep_names],unique))
    # match summary numbers to table indices. Used to merge the data later
    injury_summary_index <- apply(injury_summary[,-c(ncol(injury_summary) - 0:1)], 1,
                                  function(x)
                                    which(apply(injury_table[[type]], 1, function(y) all(x == y))))
    # initialise all at 0
    injury_table[[type]]$count <- 0
    injury_table[[type]]$weight <- mean(injury_list[[type]]$weight)

    # Merge count and average weight using index
    injury_table[[type]]$count[injury_summary_index] <- injury_summary[,ncol(injury_summary)-1]
    injury_table[[type]]$weight[injury_summary_index] <- injury_summary[,ncol(injury_summary)]
  } # End loop
  # Save in Global env.
  INJURY_TABLE <<- injury_table
  INJURY_TABLE_TYPES <<- injury_table_types
}
