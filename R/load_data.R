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
load_data <- function(setup_call_summary_filename,speeds=list(
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
  cycle_rickshaw=10
)){
  # Loading global data
  global_path <- paste0(file.path(find.package('cobeneficioswwf',
                                               lib.loc = .libPaths()),
                                  'data_wff/global/'), '/')
}
