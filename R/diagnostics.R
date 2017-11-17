#' Plot timeseries diagnostics of Hi-sAFe output
#' @description Plots a daily or annual timeseries of every Hi-sAFe output variable in a specified output profile.
#' All plots are saved as png files to a specifified output path.
#' @return Invisibly returns a list of \code{ggplot} objects. If the data is of class "hop-group" and contains
#' data from more than one Hi-sAFe simulation, the plots will contain multiple lines, colored and labeled by SimulationName.
#' If the data contains two more tree ids, the plots will be faceted by tree id.
#' @param hop An object of class "hop" or "hop-group" containing output data from one or more Hi-sAFe simulations.
#' @param profile The profile for which to plot a timeseries. If 'annualtree' or 'annualplot', annual timeseries are created.
#' If 'trees', 'plot', or 'climate', daily timeseries are created.
#' @param output.path A character stting indicating the path to the directory where plots should be saved.
#' Plots aresaved in a subdirectory within this directory named by \code{profile}.
#' If no value is provided, the experiment/simulation path is read from the hop object, and a folder is created there called "diagnostics".
#' @param time.lim If profile is an annual profile, a numeric vector of length two providing the \code{c(minimum, maximum)} of years (since planting) to plot.
#' If profile is daily profile, a character vector of length two providing the \code{c(minimum, maximum)} dates ('yyyy-mm-dd') to plot.
#' If no input, the full available time range is plotted. Use \code{NA} to refer to the start or end of the simulation.
#' @param tree.id A numeric vector indicating the ids of a subset of tree ids to plot. If no input, all trees will be plotted.
#' @param color.palette A character stirng of hex values  or R standard color names defining the color palette to use in plots with multiple simulations.
#' If \code{NULL}, the default, then the default color palette is a color-blind-friendly color palette.
#' @export
#' @importFrom dplyr %>%
#' @family hisafe diagnostic fucntions
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' mydata <- read_hisafe_output("MySimulation", "./")
#'
#' # You can create an annual timeseries of every variable:
#' diag_hisafe_ts(mydata)
#'
#' # For daily timeseries instead:
#' diag_hisafe_ts(mydata, "daily")
#' }
diag_hisafe_ts <- function(hop,
                           profile,
                           output.path   = NULL,
                           time.lim      = NULL,
                           tree.id       = NULL,
                           color.palette = NULL,
                           lty.palette   = NULL) {

  annual.profiles <- c("annualtree", "annualplot")
  daily.profiles  <- c("trees", "plot", "climate")

  ## Check for data class and if profile exists
  if(!any(c("hop", "hop-group") %in% class(hop)))        stop("hop argument not of class hop", call. = FALSE)
  if(!(profile %in% c(annual.profiles, daily.profiles))) stop("supplied profile is not supported", call. = FALSE)
  if(nrow(hop[[profile]]) == 0)                          stop(paste("no data from", profile, "profile found"), call. = FALSE)

  ## Create output directory
  if(is.null(output.path) & "hop-group" %in% class(hop)) {
    output.path <- clean_path(paste0(hop$exp.path, "/diagnostics"))
  } else if(is.null(output.path) & !("hop-group" %in% class(hop))){
    output.path <- clean_path(paste0(hop$path, "/diagnostics"))
  }
  ts.path <- clean_path(paste0(output.path, "/", profile, "/"))
  dir.create(ts.path, recursive = TRUE, showWarnings = FALSE)


  ## Clean columns & extract names of variables to plot
  # cols with only "error!" output are all NA's and cause plot errors
  if(which(names(hop[[profile]]) == "Date") > 1){
    exp.plan.vars <- names(hop[[profile]])[1:(which(names(hop[[profile]]) == "Date")-1)]
  } else {
    exp.plan.vars <- NULL
  }
  dont.plot.vars <- c(exp.plan.vars, "Date", "Day", "Month", "Year", "JulianDay", "stepNum", "id", "x", "y", "mainCropName")
  var.names <- names(hop[[profile]])[!(names(hop[[profile]]) %in% dont.plot.vars)]

  ## Create plots
  plot.list <- purrr::map(var.names, plot_hisafe_ts,
                          hop           = hop,
                          profile       = profile,
                          time.lim      = time.lim,
                          tree.id       = tree.id,
                          color.palette = color.palette,
                          lty.palette   = lty.palette)

  ## Write plots to disk
  file.names <- paste0(profile, "_", var.names, ".png")
  purrr::pwalk(list(file.names, plot.list), ggplot2::ggsave, path = ts.path, width = 7, height = 7)

  ## Invisibly return list of plot objects
  invisible(plot.list)
}

#' Plot monthCells diagnostics of Hi-sAFe output
#' @description Creates three tile plots of every Hi-sAFe monthCells output variable, one plot for each possible
#' \code{plot_hisafe_monthcells} facet scheme. All plots are saved as png files to a specifified output path.
#' @details The default data presentation for each of the facet schemes is:
#' Year~SimulationName plots are for Month==6, every 5 years from 0:max, and all simulations.
#' Month~SimulationName plots are for Year==median, all 12 months, and all simulations.
#' Month~Year plots are for all 12 months and every 5 years from 0:max, with each simulation in its own plot.
#' @return Invisibly returns a list of \code{ggplot} objects, grouped into two sublists based on the faceting scheme.
#' The first sublist contains  Year~SimulationName, and the second contains Month~SimulationName.
#' @param hop An object of class "hop" or "hop-group" containing output data from one or more Hi-sAFe simulations.
#' @param output.path A character stting indicating the path to the directory where plots should be saved. Plots are
#' saved in a subdirectory within this directory named /monthCells/facetScheme.
#' If no value is provided, the experiment/simulation path is read from the hop object, and a folder is created there called "diagnostics".
#' @export
#' @importFrom dplyr %>%
#' @family hisafe diagnostic fucntions
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' mydata <- read_hisafe_output("MySimulation", "./")
#'
#' # You can create tile plots of every monthCells variable:
#' diag_hisafe_monthcells(mydata)
#' }
diag_hisafe_monthcells <- function(hop, output.path = NULL) {

  ## Check for data class and if profile exists
  if(!any(c("hop", "hop-group") %in% class(hop))) stop("hop argument not of class hop", call. = FALSE)
  if(nrow(hop$monthCells) == 0)                   stop("no data from monthCells profile found", call. = FALSE)

  ## Create output directories
  if(is.null(output.path) & "hop-group" %in% class(hop)) {
    output.path <- clean_path(paste0(hop$exp.path, "/diagnostics"))
  } else if(is.null(output.path) & !("hop-group" %in% class(hop))){
    output.path <- clean_path(paste0(hop$path, "/diagnostics"))
  }
  monthcells.path <- clean_path(paste0(output.path, "/monthCells/"))
  plot.dirs <- paste0(monthcells.path, c("year_simname/", "month_simname/", "month_year/"))
  purrr::walk(plot.dirs, dir.create, recursive = TRUE, showWarnings = FALSE)

  ## Clean columns & extract names of variables to plot
  # cols with only "error!" output are all NA's and cause plot errors
  hop$monthCells <- hop$monthCells %>% dplyr::select_if(~sum(!is.na(.)) > 0)
  var.names      <- names(hop$monthCells)[(which(names(hop$monthCells) == "cropSpeciesName") + 1):length(names(hop$monthCells))]

  ## Create plots
  plot.list1 <- purrr::map(var.names, plot_hisafe_monthcells,
                           hop       = hop,
                           rowfacet  = "SimulationName",
                           colfacet  = "Year",
                           sim.names = "all",
                           years     = seq(0, (max(hop$monthCells$Year) - min(hop$monthCells$Year)), 5),
                           months    = 6)
  file.names <- paste0("monthCells_year_simname_", var.names, ".png")
  purrr::pwalk(list(file.names, plot.list1), ggplot2::ggsave, path = plot.dirs[1], scale = 2, width = 10, height = 10)

  plot.list2 <- purrr::map(var.names, plot_hisafe_monthcells,
                           hop       = hop,
                           rowfacet  = "SimulationName",
                           colfacet  = "Month",
                           sim.names = "all",
                           years     = (round(median(hop$monthCells$Year),0) - min(hop$monthCells$Year)),
                           months    = 1:12)
  file.names <- paste0("monthCells_month_simname_", var.names, ".png")
  purrr::pwalk(list(file.names, plot.list2), ggplot2::ggsave, path = plot.dirs[2], scale = 2, height = 10, width = 10)

  plot.list3.tog <- list()
  for(sim.name in unique(hop$monthCells$SimulationName)){
    plot.list3 <- purrr::map(var.names, plot_hisafe_monthcells,
                             hop       = hop,
                             rowfacet  = "Year",
                             colfacet  = "Month",
                             sim.names = sim.name,
                             years     = seq(0, (max(hop$monthCells$Year) - min(hop$monthCells$Year)), 5),
                             months    = 1:12)
    file.names <- paste0("monthCells_month_year_", sim.name, "_", var.names, ".png")
    purrr::pwalk(list(file.names, plot.list3), ggplot2::ggsave, path = plot.dirs[3], scale = 2, height = 10, width = 10)
    plot.list3.tog <- c(plot.list3.tog, plot.list3)
  }

  ## Invisibly return list of plot objects
  invisible(list(year.simname = plot.list1, month.simname = plot.list2, month.year = plot.list3.tog))
}
