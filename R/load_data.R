#' Load data for model and setting
#'
#' Loads and processes data from file. Local data for the setting and global data for the model.
#' Writes objects to the global environment.
#'
#' @param setup_call_summary_filename name of file to write to
#' @param speeds named list of mode speeds
#'
#'
#' @export
load_data <- function(setup_call_summary_filename,speeds = list(
  bus = 15,
  bus_driver = 15,
  minibus = 15,
  minibus_driver = 15,
  car = 21,
  taxi = 21,
  pedestrian = 4.8,
  walk_to_pt = 4.8,
  cycle = 14.5,
  motorcycle = 25,
  truck = 21,
  van = 15,
  subway = 28,
  rail = 35,
  auto_rickshaw = 22,
  shared_auto = 22,
  cycle_rickshaw = 10
)){
  # Loading global data of Dose-Responses curves
  global_path <- paste0(file.path(find.package('cobeneficioswwf',
                                               lib.loc = .libPaths()),
                                  'data/global/'), '/')

  # Importing mortality causes and it's relation to AP and PA
  DISEASE_INVENTORY <<- read.csv(paste0(global_path, "dose_response/disease_outcomes_lookup.csv"))
  # Importing dose-response for causes related to air pollution
  DR_AP <<- read.csv(paste0(global_path, "dose_response/drap/dose_response.csv"))
  # Importing dose-response for causes related to physical activity
  list_of_files <- list.files(path = paste0(global_path,"dose_response/drpa/extdata/"), recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)
  for (i in 1:length(list_of_files)) { # Loop through all files
    # Save each file in the global environment
    # Really nice explanation of how R saves and searches stuff: http://blog.obeautifulcode.com/R/How-R-Searches-And-Finds-Stuff/
    assign(stringr::str_sub(basename(list_of_files[[i]]), end = -5),
           readr::read_csv(list_of_files[[i]], col_types = readr::cols()),
           pos = 1)
  } #End loop
  cat(paste0('\n  Dose--response read from ',
             global_path,
             'dose_response/drpa/extdata/ \n\n'),
      file = setup_call_summary_filename, append = T)

  # Importing local datasets
  # These datasets are all local, saved in local folder.
  local_path <- PATH_TO_LOCAL_DATA

  #####
  # Trip dataset
  ## we need columns: trip_id, trip_mode, stage_mode, stage_duration,
  ##    trip_distance, stage_distance
  ## trips can be composed of multiple stages
  ## all trip columns are used for scenario generation alone
  ## stage columns are used for downstream calculation
  ## if either trip or stage labels are missing, we copy over from the other.
  filename <- paste0(local_path, "trips_", PROJECT, ".csv")
  trip_set <- read_csv(filename, col_types = cols())
  cat(paste0('\n  Trips read from ', filename,' \n\n'), file = setup_call_summary_filename, append = T)
  # Create new participant_id
  trip_set$participant_id <- as.numeric(as.factor(trip_set$participant_id))

  ## Copy over trip or stage information as required
  mode_cols <- c('trip_mode','stage_mode')
  if (sum(mode_cols %in% colnames(trip_set)) == 0)
    stop(paste0('Please include a column labelled "trip_mode" or "stage_mode" in ', filename))
  if ('trip_mode' %in% colnames(trip_set) && !'stage_mode' %in% colnames(trip_set))
    trip_set$stage_mode <- trip_set$trip_mode
  if ('stage_mode' %in% colnames(trip_set) && !'trip_mode' %in% colnames(trip_set))
    trip_set$trip_mode <- trip_set$stage_mode
  if ('trip_duration' %in% colnames(trip_set) && !'stage_duration' %in% colnames(trip_set))
    trip_set$stage_duration <- trip_set$trip_duration
  if ('trip_distance' %in% colnames(trip_set) && !'stage_distance' %in% colnames(trip_set))
    trip_set$stage_distance <- trip_set$trip_distance
  if ('stage_distance' %in% colnames(trip_set) && !'trip_distance' %in% colnames(trip_set))
    trip_set$trip_distance <- trip_set$stage_distance

  # Transform any mode for standardized ones
  # This shouldn't be needed because of the function "standardize_modes" in file
  # other_functions.R when cleaning trip datasets
  ## use specified words for key modes
  walk_words <- c('walk','walked','pedestrian')
  cycle_words <- c('bike','cycle','cycling')
  mc_words <- c('motorcycle','mcycle','mc','mtw')
  subway_words <- c('metro','underground')
  rail_words <- c('train')
  for (i in 1:length(mode_cols)) { # Loop for each column
    ## lower case mode names
    trip_set[[mode_cols[i]]] <- tolower(trip_set[[mode_cols[i]]])
    ## replaces spaces with _
    trip_set[[mode_cols[i]]] <- sapply(trip_set[[mode_cols[i]]],
                                       function(x) gsub(' ','_',as.character(x)))
    trip_set[[mode_cols[i]]][trip_set[[mode_cols[i]]] == 'private_car'] <- 'car'
    trip_set[[mode_cols[i]]][trip_set[[mode_cols[i]]] %in% walk_words] <- 'pedestrian'
    trip_set[[mode_cols[i]]][trip_set[[mode_cols[i]]] %in% cycle_words] <- 'cycle'
    trip_set[[mode_cols[i]]][trip_set[[mode_cols[i]]] %in% mc_words] <- 'motorcycle'
    trip_set[[mode_cols[i]]][trip_set[[mode_cols[i]]] %in% subway_words] <- 'subway'
    trip_set[[mode_cols[i]]][trip_set[[mode_cols[i]]] %in% rail_words] <- 'rail'
  }

  # Remove rows that doesn't have age or sex
  trip_set <- subset(trip_set,!is.na(age))
  trip_set <- subset(trip_set,!is.na(sex))
  trip_set$sex <- tolower(trip_set$sex)
  trip_set$trip_id[is.na(trip_set$stage_mode)] <- 0
  TRIP_SET <<- trip_set

  #####
  # GBD and population datasets
  # In GBD "cause" column should match what is in DISEASE_INVENTORY (above)
  # Import GBD dataset
  filename <- paste0(local_path, "/gbd_", PROJECT, ".csv")
  GBD_DATA <- read_csv(filename, col_types = cols())
  cat(paste0('\n  GBD read from ', filename, ' \n\n'), file = setup_call_summary_filename, append = T)

  # Import population dataset
  filename <- paste0(local_path,"/population_",PROJECT,".csv")
  demographic <- read_csv(filename,col_types = cols())
  # Remove rows with NA
  demographic <- demographic[!apply(demographic, 1, anyNA),]
  demographic$sex <- tolower(demographic$sex)
  cat(paste0('\n  Population read from ', filename, ' \n\n'), file = setup_call_summary_filename, append = T)

  # Obtain max and min age from trip and population datasets as well as AGE_RANGE parameter
  age_category <- demographic$age
  max_age <- max(as.numeric(sapply(age_category, function(x) strsplit(x, '-')[[1]][2])))
  max_age <- min(max_age, max(trip_set$age), AGE_RANGE[2])
  min_age <- min(as.numeric(sapply(age_category, function(x) strsplit(x, '-')[[1]][1])))
  min_age <- max(min_age, min(trip_set$age), AGE_RANGE[1])

  # Filter demographic dataset to only age ranges considered and save it in global environment
  DEMOGRAPHIC <<- demographic[as.numeric(sapply(age_category, function(x) strsplit(x,'-')[[1]][1])) <= max_age &
                                as.numeric(sapply(age_category, function(x) strsplit(x, '-')[[1]][2])) >= min_age,]

  # Get age-category details from population data
  AGE_CATEGORY <<- unique(DEMOGRAPHIC$age)
  AGE_LOWER_BOUNDS <<- as.numeric(sapply(AGE_CATEGORY, function(x) strsplit(x, '-')[[1]][1]))
  MAX_AGE <<- max(as.numeric(sapply(AGE_CATEGORY, function(x) strsplit(x, '-')[[1]][2])))

  # Filter GBD datasets to only causes we're interested in and only age ranges considered
  disease_names <- c(as.character(DISEASE_INVENTORY$GBD_name), 'Road injuries')
  GBD_DATA <- subset(GBD_DATA,cause_name %in% disease_names)
  GBD_DATA$min_age <- as.numeric(sapply(GBD_DATA$age_name, function(x) str_split(x, ' to ')[[1]][1]))
  GBD_DATA$max_age <- as.numeric(sapply(GBD_DATA$age_name, function(x) str_split(x, ' to ')[[1]][2]))
  GBD_DATA <- subset(GBD_DATA,max_age >= AGE_LOWER_BOUNDS[1])
  GBD_DATA <- subset(GBD_DATA,min_age <= MAX_AGE)
  names(GBD_DATA)[c(1,3,4,5)] <- c('measure','sex','age','cause')
  GBD_DATA$sex <- tolower(GBD_DATA$sex)

  # Create a dataframe of burden of disease with info from GBD and population datasets
  # Expand.grid creates all possible combinations of these variables
  burden_of_disease <- expand.grid(measure = unique(GBD_DATA$measure),
                                   sex = unique(DEMOGRAPHIC$sex),
                                   age = unique(DEMOGRAPHIC$age),
                                   cause = disease_names,
                                   stringsAsFactors = F)
  # Add population considering age range and sex from demographic
  burden_of_disease <- dplyr::left_join(burden_of_disease, DEMOGRAPHIC, by = c('age', 'sex'))
  burden_of_disease$min_age <- as.numeric(sapply(burden_of_disease$age, function(x) str_split(x, '-')[[1]][1]))
  burden_of_disease$max_age <- as.numeric(sapply(burden_of_disease$age, function(x) str_split(x, '-')[[1]][2]))
  ## when we sum ages, we assume that all age boundaries used coincide with the GBD age boundaries.
  # Compute rate of Ylls and deaths for country's population
  burden_of_disease$rate <- apply(burden_of_disease, 1,
                                  function(x){
                                    # Filter only rows needed in GBD for each row in burden_of_disease
                                    # subtab is usually a single row, but if there's
                                    # overlap in age ranges, then there will be more
                                    # than one row
                                    subtab <- subset(GBD_DATA,
                                                     measure == as.character(x[1]) &
                                                       sex == as.character(x[2]) &
                                                       cause == as.character(x[4]) &
                                                       min_age >= as.numeric(x[6]) &
                                                       max_age <= as.numeric(x[7]));
                                    # Compute the rate between Ylls or deaths and country's population
                                    # sum is used in case there are more than 1 row
                                    sum(subtab$val)/sum(subtab$population)
                                  }
  )

  # To compute the burden at city level, we applied the rate computed at country level
  # to city's population
  burden_of_disease$burden <- burden_of_disease$population * burden_of_disease$rate
  burden_of_disease$burden[is.na(burden_of_disease$burden)] <- 0
  DISEASE_BURDEN <<- burden_of_disease # Saved in global env

  # Compute the average number of YLLs due to road injuries
  gbd_injuries <- DISEASE_BURDEN[which(DISEASE_BURDEN$cause == "Road injuries"),]
  gbd_injuries$sex_age <- paste0(gbd_injuries$sex, "_", gbd_injuries$age)

  ## calculating the ratio of YLL to deaths for each age and sex group (average YLLs)
  gbd_injuries <- arrange(gbd_injuries, measure)
  gbd_inj_yll <- gbd_injuries[which(gbd_injuries$measure == "YLLs (Years of Life Lost)"),]
  gbd_inj_dth <- gbd_injuries[which(gbd_injuries$measure == "Deaths"),]
  gbd_inj_yll$yll_dth_ratio <- gbd_inj_yll$burden/gbd_inj_dth$burden
  GBD_INJ_YLL <<- gbd_inj_yll


  #####
  ## Physical activity dataset
  filename <- paste0(local_path, "/pa_", PROJECT, ".csv")
  pa_set <- read_csv(filename,col_types = cols())
  pa_set$sex <- tolower(pa_set$sex)
  PA_SET <<- pa_set
  cat(paste0('\n  Physical activity survey read from ', filename, ' \n\n'), file = setup_call_summary_filename, append = T)

  #####
  ## Injuries dataset
  ## injury data
  filename <- paste0(local_path,"/injuries_", PROJECT,".csv")
  injuries <- read_csv(filename,col_types = cols())
  cat(paste0('\n  Injuries read from ', filename, ' \n\n'), file = setup_call_summary_filename, append = T)

  # Categorize victim's age
  if ('cas_age' %in% colnames(injuries))
    injuries <- assign_age_groups(injuries,age_label = 'cas_age')

  # Lowercase everything
  injuries$cas_mode <- tolower(injuries$cas_mode)
  injuries$strike_mode <- tolower(injuries$strike_mode)
  if ('cas_gender' %in% colnames(injuries))
    injuries$cas_gender <- tolower(injuries$cas_gender)
  #injuries$strike_mode[is.na(injuries$strike_mode)] <- 'listed_na'

  # Replace nov words to nov, although this shouldn't be used because this should've
  # been done in the cleaning
  nov_words <- c('no.other.fixed.or.stationary.object', 'no other vehicle', 'none')
  injuries$strike_mode[injuries$strike_mode %in% nov_words] <- 'nov'

  ## add weight column if missing
  if (!'weight' %in% colnames(injuries))
    injuries$weight <- 1
}
