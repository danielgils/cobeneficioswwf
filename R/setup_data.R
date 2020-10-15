#' Run the setup script for the model
#'
#' Sets up the basic ITHIM object for onward calculation. Data loading, processing and harmonisation. Setting global values.
#'
#' Parameters have two options: to be set to a constant, and to be sampled from a prespecified distribution.
#' Each parameter is given as an argument of length 1 or 2.
#' If length 1, it's constant, and set to the global environment.
#' If length 2, a distribution is defined and sampled from NSAMPLE times.
#' There are some exceptions, listed above

#' @return ithim_object list of objects for onward use.
#' @export
# This function is similar to run_ithim_setup from ithimr package
setup_data <- function(seed = 1,
                  project = 'prueba',
                  speeds = NULL,
                  PM_emission_inventory = NULL,
                  setup_call_summary_filename = 'setup_call_summary.txt',
                  DIST_CAT = c("0-6 km", "7-9 km", "10+ km"),
                  AGE_RANGE = c(0,150),
                  ADD_WALK_TO_BUS_TRIPS = T,
                  PATH_TO_LOCAL_DATA = NULL,
                  BUS_WALK_TIME= 5,
                  MMET_CYCLING = 4.63,
                  MMET_WALKING = 2.53,
                  PM_CONC_BASE = 50,
                  PM_TRANS_SHARE = 0.225,
                  DAY_TO_WEEK_TRAVEL_SCALAR = 7){
  ## Summary of outputs
  # seed = double. sets seed to allow some reproducibility.
  # CITY = string. used to identify input files.
  # speeds = named list of doubles. average mode speeds.
  # PM_emission_inventory = named list of doubles. vehicle emission factors.
  # setup_call_summary_filename = string. Where to write input call summary.
  # DIST_CAT = vector of strings. defines distance categories for scenario generation (5 accra scenarios)

  # AGE_RANGE = vector of length 2, specifying the minimum and maximum ages to be used in the model. Note that the actual
  # maximum and minimum will coincide with boundaries in the population and GBD files.
  # ADD_WALK_TO_BUS_TRIPS = logic. T: adds walk trips to all bus trips whose duration exceeds BUS_WALK_TIME. F: no trips added
  # PATH_TO_LOCAL_DATA = string: path to input files, if not one of the default case studies
  # BUS_WALK_TIME = parameter. double: time taken to walk to bus. vector: samples from distribution.
  # MMET_CYCLING = parameter. double: sets cycling (M)METs. vector: samples from distribution.
  # MMET_WALKING = parameter. double: sets walking (M)METs. vector: samples from distribution.
  # PM_CONC_BASE = parameter. double: sets background PM. vector: samples from distribution.
  # PM_TRANS_SHARE = parameter. double: sets PM proportion that comes from transport. vector: samples from distribution.
  # DAY_TO_WEEK_TRAVEL_SCALAR = parameter. double: sets scalar for extrapolation from day to week. vector: samples from distribution.

  set.seed(seed)

  # Initializing object where everything is going to be saved
  ithim_object <- list()

  # Setting global variables
  ADD_WALK_TO_BUS_TRIPS <<- ADD_WALK_TO_BUS_TRIPS
  if(is.null(PATH_TO_LOCAL_DATA)){
    #PATH_TO_LOCAL_DATA <<- file.path(find.package('ithimr',lib.loc=.libPaths()), 'extdata/local/',CITY,'/')
    stop("You have to provide a path to project's datasets")
  }else{
    PATH_TO_LOCAL_DATA <<- PATH_TO_LOCAL_DATA
  }

  AGE_RANGE <<- AGE_RANGE
  DIST_CAT <<- DIST_CAT

  ## fixed parameters for AP inhalation
  # TODO: we have to define whether we are going to used these values or not
  BASE_LEVEL_INHALATION_RATE <<- 1
  CLOSED_WINDOW_PM_RATIO <<- 0.5
  CLOSED_WINDOW_RATIO <<- 0.5
  ROAD_RATIO_MAX <<- 3.216
  ROAD_RATIO_SLOPE <<- 0.379
  SUBWAY_PM_RATIO <<- 0.8

  ## default speeds that can be edited by input.
  # TODO: we have to define whether we are going to used these values or not
  default_speeds <- list(
    bus=15,
    bus_driver=15,
    minibus=15,
    minibus_driver=15,
    car=21,
    taxi=21,
    pedestrian=4.8,
    walk_to_pt=4.8,
    cycle=14.5,
    motorcycle=25,
    truck=21,
    van=15,
    subway=28,
    rail=35,
    auto_rickshaw=22,
    shared_auto=22,
    shared_taxi=21,
    cycle_rickshaw=10
  )
  # If the user gives different speeds, the default ones are overwritten
  if(!is.null(speeds)){
    for(m in names(speeds))
      default_speeds[[m]] <- speeds[[m]]
  }

  TRAVEL_MODES <<- tolower(names(default_speeds))
  # Creating a dataframe to ease the use of it later
  MODE_SPEEDS <<- data.frame(stage_mode = TRAVEL_MODES, speed = unlist(default_speeds), stringsAsFactors = F)

  # Printing the speeds used in the analysis
  cat('\n -- PROJECT -- \n',file=setup_call_summary_filename,append=F)
  cat(project,file=setup_call_summary_filename,append=T)
  cat('\n\n -- SPEEDS -- \n\n',file=setup_call_summary_filename,append=T)
  #print(MODE_SPEEDS)
  for(i in 1:nrow(MODE_SPEEDS)) {
    cat(paste0(MODE_SPEEDS[i,]),file=setup_call_summary_filename,append=T);
    cat('\n',file=setup_call_summary_filename,append=T)
  }

  ## default PM2.5 emission contributions that can be edited by input.
  # TODO: we have to define whether we are going to used these values or not
  # TODO: Ask Ali why the emission of bus is zero and bus_driver is a value
  default_PM_emission_inventory <- list(
    bus=0,
    bus_driver=0.82,
    car=0.228,
    taxi=0.011,
    pedestrian=0,
    cycle=0,
    motorcycle=0.011,
    truck=0.859,
    big_truck=0.711,
    other=0.082
  )

  if(!is.null(PM_emission_inventory)){
    for(m in names(PM_emission_inventory))
      # if(grepl('bus$',m,ignore.case=T)&&!paste0(m,'_driver')%in%names(PM_emission_inventory)){
      #   default_PM_emission_inventory[[paste0(m,'_driver')]] <- PM_emission_inventory[[m]]
      # }else{
        default_PM_emission_inventory[[m]] <- PM_emission_inventory[[m]]
      # }
  }
  names(default_PM_emission_inventory) <- tolower(names(default_PM_emission_inventory))

  PM_EMISSION_INVENTORY <<- default_PM_emission_inventory
  cat('\n -- PM 2.5 EMISSION INVENTORY -- \n\n',file=setup_call_summary_filename,append=T)
  for(i in 1:length(default_PM_emission_inventory)) {
    cat(paste(names(PM_EMISSION_INVENTORY)[i],PM_EMISSION_INVENTORY[[i]]),file=setup_call_summary_filename,append=T);
    cat('\n',file=setup_call_summary_filename,append=T)
  }

  # TODO: Cambridge added CO2 to the analysis, see whether we should use it as well.

  ## LOAD DATA
  #load_data(setup_call_summary_filename,speeds=default_speeds)
}