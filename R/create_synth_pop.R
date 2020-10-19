#' Creates synthetic population
#'
#' Creates a synthetic population by matching individuals in the trip set to
#' individuals in the PA set
#'
#' @param raw_trip_set data frame of raw trips taken
#'
#' @return the synthetic population and the trip set which has been pruned
#'
#' @export
create_synth_pop <- function(raw_trip_set){
  ##duration: units are minutes per day.
  ##work_ltpa_marg_met: units are marginal MET-h/week.

  # Create age categories for trip_set dataset.
  trip_set <- assign_age_groups(raw_trip_set, age_category = AGE_CATEGORY,
                                age_lower_bounds = AGE_LOWER_BOUNDS,
                                max_age = MAX_AGE)

  # Adding PA data
  pa <- PA_SET
  age_category <- AGE_CATEGORY
  age_lower_bounds <- AGE_LOWER_BOUNDS

  # Create age categories for physical activity dataset.
  pa <- assign_age_groups(pa, age_category = age_category, age_lower_bounds)

  #Match persons in the trip (trip_set) with physical activity datasets.
  column_to_keep <- which(colnames(pa) %in% c('work_ltpa_marg_met'))
  unique_ages <- unique(trip_set$age_cat)
  unique_genders <- unique(trip_set$sex)

  # get population from trip_set: all the unique ids, and their demographic information
  # match only for "real" people (i.e. not `ghost drivers', whose id is 0)
  # The synthetic population corresponds to all people surveyed in the OD survey
  synthetic_population <- subset(trip_set, !duplicated(participant_id) &
                                   participant_id > 0,
                                 select = c("participant_id","age","sex","age_cat"))

  ## get zeros and densities
  # Zeros is the proportion of people in each combination of age range and sex
  # that didn't do any PA, i.e., work_ltpa_marg_met == 0
  zeros <- densities <- list() # Zeros is list of proportions and densities is
  # a list with MMETs that can be chosen in each age range and sex

  for (age_group in unique_ages) { # Loop for each age range
    zeros[[age_group]] <- densities[[age_group]] <- list()
    pa_age_category <- age_category[which(AGE_CATEGORY == age_group)]
    for (gender in unique_genders) { # Loop for each sex
      # Filter people in PA dataset that have the same sex and age range
      matching_people <- as.data.frame(filter(pa, age_cat == pa_age_category &
                                                sex == gender)[,column_to_keep])
      raw_zero <- 1

      # If there are people in the age range and sex, compute the proportion of
      # people that didn't do any PA, i.e., work_ltpa_marg_met == 0
      if (nrow(matching_people) > 0)
        raw_zero <- sum(matching_people$work_ltpa_marg_met == 0) / length(matching_people$work_ltpa_marg_met)

      # Save proportion and candidate people from PA datasets
      zeros[[age_group]][[gender]] <- raw_zero
      densities[[age_group]][[gender]] <- matching_people$work_ltpa_marg_met[matching_people$work_ltpa_marg_met > 0]
    } # End loop sex
  } # End loop age range

  # assign all participants 0 leisure/work mmets
  synthetic_population$work_ltpa_marg_met <- 0

  # match population to PA dataset via demographic information
  for (age_group in unique_ages) { # Loop for each age range
    pa_age_category <- age_category[which(AGE_CATEGORY == age_group)]
    for (gender in unique_genders) { # Loop for each sex
      # Indices of people in same age range and sex
      i <- which(synthetic_population$age_cat == age_group &
                   synthetic_population$sex == gender)
      raw_density <- densities[[age_group]][[gender]]
      prob_zero <- zeros[[age_group]][[gender]]
      # Sampling with replacement with n probabilities of selection
      v <- sample(c(0, raw_density), length(i), replace = T,
                  prob = c(prob_zero,
                           (rep(1, length(raw_density)) - prob_zero) / length(raw_density)))
      # If there are people to choose from, then paste PA info (raw_density)
      if (length(v) > 0)
        synthetic_population$work_ltpa_marg_met[i] <- c(v)
    }
  }

  # Convert all int columns to numeric
  synthetic_population[, sapply(synthetic_population, class) == 'integer'] <- lapply(synthetic_population[, sapply(synthetic_population,class) == 'integer'], as.numeric)


  # Now remove non-travelling participants which are those with modes out of our
  # inventory or "other"
  trip_set <- subset(trip_set, trip_mode %in% VEHICLE_INVENTORY$stage_mode &
                       stage_mode %in% VEHICLE_INVENTORY$stage_mode)
  trip_set <- subset(trip_set, trip_mode != 'other')
  trip_set <- drop_na(trip_set)

  return(list(trip_set = trip_set, synthetic_population = synthetic_population))

}
