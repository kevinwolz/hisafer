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
#' If no value is provided, the experiment/simulation path is read from the hop object, and a directory is created there called "analysis/diagnostics".
#' @param simu.names A character vector of the SimulationNames within \code{hop} to include. Use "all" to include all available values.
#' @param years A numeric vector of the years within \code{hop} to include. Use "all" to include all available values.
#' @param tree.ids A numeric vector indicating a subset of tree ids to plot. Use "all" to include all available values.
#' @param doy.lim A numeric vector of length two providing the \code{c(minimum, maximum)} of julian days to plot. Only applies if \code{profile} is a daily profile.
#' @param color.palette A character stirng of hex values or R standard color names defining the color palette to use in plots with multiple simulations.
#' If \code{NULL}, the default, then the default color palette is a color-blind-friendly color palette. The default supports up to 24 simulations.
#' @param linetype.palette A character stirng of values defining the linetype palette to use in plots with multiple simulations.
#' If \code{NULL}, the default, then solid lines are used for all simulations. The default supports up to 24 simulations.
#' @param aes.cols A list with arguments "color" and "linetype" containing character stirngs of the column names to use for plot aesthetics.
#' @param facet.year A logical indicating whether, for daily profiles, the plot should be faceted by year. This helps with seeing finer level detail.
#' @param crop.points Logical indicating if points should be plotted as well, with point shape desgnating the main crop name.
#' Only applies when \code{profile} is 'plot' or 'annualplot'.
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
                           output.path      = NULL,
                           simu.names       = "all",
                           years            = "all",
                           tree.ids         = "all",
                           doy.lim          = c(1, 366),
                           color.palette    = NULL,
                           linetype.palette = NULL,
                           aes.cols         = list(color = "SimulationName", linetype = "SimulationName"),
                           facet.year       = TRUE,
                           crop.points      = FALSE) {

  annual.profiles <- c("annualtree", "annualplot")
  daily.profiles  <- c("trees", "plot", "climate")

  if(!("hop" %in% class(hop)))                                  stop("hop argument not of class hop",                             call. = FALSE)
  if(!(profile %in% c(annual.profiles, daily.profiles)))        stop("supplied profile is not supported",                         call. = FALSE)
  if(nrow(hop[[profile]]) == 0)                                 stop(paste("no data from", profile, "profile found"),             call. = FALSE)
  if(!is.character(output.path))                                stop("output.path argument must be a character vector",           call. = FALSE)

  ## Create output directory
  if(is.null(output.path) & "hop-group" %in% class(hop)) {
    output.path <- clean_path(paste0(hop$exp.path, "/analysis/diagnostics"))
  } else if(is.null(output.path) & !("hop-group" %in% class(hop))){
    output.path <- clean_path(paste0(hop$path$path, "/analysis/diagnostics"))
  }
  ts.path <- clean_path(paste0(output.path, "/", profile, "/"))
  dir.create(ts.path, recursive = TRUE, showWarnings = FALSE)


  ## Clean columns & extract names of variables to plot
  # cols with only "error!" output are all NA's and cause plot errors
  hop[[profile]]  <- hop[[profile]] %>% dplyr::select_if(~sum(!is.na(.)) > 0)
  if(which(names(hop[[profile]]) == "Date") > 1){
    exp.plan.vars <- names(hop[[profile]])[1:(which(names(hop[[profile]]) == "Date")-1)]
  } else {
    exp.plan.vars <- NULL
  }
  dont.plot.vars <- c(exp.plan.vars, "Date", "Day", "Month", "Year", "JulianDay", "stepNum", "id",
                      "x", "y", "mainCropName", "interCropName", "mainCropArea", "interCropArea")
  var.names <- names(hop[[profile]])[!(names(hop[[profile]]) %in% dont.plot.vars)]


  ## Create plots
  plot.list <- purrr::map(var.names, plot_hisafe_ts,
                          hop              = hop,
                          profile          = profile,
                          simu.names       = simu.names,
                          years            = years,
                          tree.ids         = tree.ids,
                          doy.lim          = doy.lim,
                          color.palette    = color.palette,
                          linetype.palette = linetype.palette,
                          aes.cols         = aes.cols,
                          facet.year       = facet.year,
                          crop.points      = crop.points)

  ## Write plots to disk
  file.names <- paste0(profile, "_", var.names, ".png")
  purrr::pwalk(list(file.names, plot.list), ggplot2::ggsave, path = ts.path, width = 11, height = 8.5)

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
#' If no value is provided, the experiment/simulation path is read from the hop object, and a directory is created there called "analysis/diagnostics".
#' @param schemes A character vector of the facet schemes to run. Possible schemes are described in Details and have the following names: \code{year.simname}, \code{month.simname}, \code{month.year}.
#' @param trees Logical indicating if a point should be plotted at the location of each tree.
#' @param canopies Logical indicating if an elipsoid should be plotted representing the size of each tree canopy.
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
diag_hisafe_monthcells <- function(hop,
                                   output.path = NULL,
                                   schemes     = c("year.simname", "month.simname", "month.year"),
                                   trees       = TRUE,
                                   canopies    = TRUE) {

  ## Check for data class and if profile exists
  if(!("hop" %in% class(hop)))   stop("hop argument not of class hop",                   call. = FALSE)
  if(nrow(hop$monthCells) == 0)  stop("no data from monthCells profile found",           call. = FALSE)
  if(!is.character(output.path)) stop("output.path argument must be a character vector", call. = FALSE)
  if(!(schemes %in% c("year.simname", "month.simname", "month.year"))) stop("schemes argument must be one of: year.simname, month.simname, month.year", call. = FALSE)

  ## Create output directories
  if(is.null(output.path) & "hop-group" %in% class(hop)) {
    output.path <- clean_path(paste0(hop$exp.path, "/analysis/diagnostics"))
  } else if(is.null(output.path) & !("hop-group" %in% class(hop))){
    output.path <- clean_path(paste0(hop$path, "/analysis/diagnostics"))
  }
  monthcells.path <- clean_path(paste0(output.path, "/monthCells/"))

  ## Clean columns & extract names of variables to plot
  # cols with only "error!" output are all NA's and cause plot errors
  hop$monthCells <- hop$monthCells %>% dplyr::select_if(~sum(!is.na(.)) > 0)
  var.names      <- names(hop$monthCells)[(which(names(hop$monthCells) == "cropSpeciesName") + 1):length(names(hop$monthCells))]

  ## Create plots
  if("year.simname" %in% schemes){
    plot.dir1 <- paste0(monthcells.path, "year_simname/")
    dir.create(plot.dir1, recursive = TRUE, showWarnings = FALSE)
    plot.list1 <- purrr::map(var.names, plot_hisafe_monthcells,
                             hop       = hop,
                             colfacet  = "SimulationName",
                             rowfacet  = "Year",
                             sim.names = "all",
                             years     = seq(0, (max(hop$monthCells$Year) - min(hop$monthCells$Year)), 5),
                             months    = 6,
                             trees     = trees,
                             canopies  = canopies)
    file.names <- paste0("monthCells_year_simname_", var.names, ".png")
    purrr::pwalk(list(file.names, plot.list1), ggplot2::ggsave, path = plot.dir1, scale = 2, width = 10, height = 10)
  } else { plot.list1 <- list() }

  if("month.simname" %in% schemes){
    plot.dir2 <- paste0(monthcells.path, "month_simname/")
    dir.create(plot.dir2, recursive = TRUE, showWarnings = FALSE)
    plot.list2 <- purrr::map(var.names, plot_hisafe_monthcells,
                             hop       = hop,
                             colfacet  = "SimulationName",
                             rowfacet  = "Month",
                             sim.names = "all",
                             years     = (round(median(hop$monthCells$Year),0) - min(hop$monthCells$Year)),
                             months    = 1:12,
                             trees     = trees,
                             canopies  = canopies)
    file.names <- paste0("monthCells_month_simname_", var.names, ".png")
    purrr::pwalk(list(file.names, plot.list2), ggplot2::ggsave, path = plot.dir2, scale = 2, height = 10, width = 10)
  } else { plot.list2 <- list() }

  plot.list3.tog <- list()
  if("month.year" %in% schemes){
    plot.dir3 <- paste0(monthcells.path, "month_year/")
    dir.create(plot.dir3, recursive = TRUE, showWarnings = FALSE)
    for(sim.name in unique(hop$monthCells$SimulationName)){
      plot.list3 <- purrr::map(var.names, plot_hisafe_monthcells,
                               hop       = hop,
                               colfacet  = "Year",
                               rowfacet  = "Month",
                               sim.names = sim.name,
                               years     = seq(0, (max(hop$monthCells$Year) - min(hop$monthCells$Year)), 5),
                               months    = 1:12,
                               trees     = trees,
                               canopies  = canopies)

      bad.plot.check <- unlist(purrr::map(plot.list3, is.logical))
      plot.list3 <- plot.list3[!bad.plot.check]

      file.names <- paste0("monthCells_month_year_", sim.name, "_", var.names, ".png")
      file.names <- file.names[!bad.plot.check]

      purrr::pwalk(list(file.names, plot.list3), ggplot2::ggsave, path = plot.dir3, scale = 2, height = 10, width = 10)
      plot.list3.tog <- c(plot.list3.tog, plot.list3)
    }
  }

  ## Invisibly return list of plot objects
  invisible(list(year.simname = plot.list1, month.simname = plot.list2, month.year = plot.list3.tog))
}
