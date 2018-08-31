#' Plot timeseries diagnostics of Hi-sAFe output
#' @description Plots a daily timeseries of every Hi-sAFe output variable in a specified output profile.
#' All plots are saved as png files to a specifified output path.
#' @return Invisibly returns a list of \code{ggplot} objects. If \code{hop} contains
#' data from more than one Hi-sAFe simulation, the plots will contain multiple lines, colored and labeled by SimulationName.
#' If the data contains two more tree ids, the plots will be faceted by tree id.
#' @param hop An object of class "hop" or "face" containing output data from one or more Hi-sAFe simulations.
#' @param profile The profile for which to plot a timeseries. One of 'trees', 'plot', 'climate', or 'cells'.
#' @param output.path A character string indicating the path to the directory where plots should be saved.
#' Plots are saved in a subdirectory within this directory named by \code{profile}.
#' If no value is provided, the experiment/simulation path is read from the hop object, and a directory is created there called "analysis/diagnostics".
#' @param ... Other arguments passed to \code{\link{plot_hisafe_ts}}.
#' @export
#' @importFrom dplyr %>%
#' @family hisafe diagnostic fucntions
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' mydata <- read_hisafe(path = "mydir", simu.names = "MySimulation")
#'
#' # You can create a daily timeseries of every trees variable:
#' diag_hisafe_ts(mydata, "trees")
#' }
diag_hisafe_ts <- function(hop, profile, output.path = NULL, ...) {

  supported.profiles <- c("trees", "plot", "climate", "cells")
  is_hop(hop, error = TRUE)
  profile_check(hop, profile, error = TRUE)
  if(!(profile %in% supported.profiles))                  stop("supplied profile is not supported by plot_hisafe_ts()", call. = FALSE)
  if(!(is.character(output.path) | is.null(output.path))) stop("output.path argument must be a character string",       call. = FALSE)

  ts.path <- clean_path(paste0(diag_output_path(hop = hop, output.path = output.path), "/diagnostics/", profile, "/"))
  dir.create(ts.path, recursive = TRUE, showWarnings = FALSE)

  if(profile == "trees") last.col <- "idTree" else if(profile == "cells") last.col <- "y" else last.col <- "JulianDay"

  var.names <- vars_to_diag(hop = hop, profile = profile, last.col = last.col)
  plot.list <- purrr::map(var.names, plot_hisafe_ts, hop = hop, profile = profile, ...)

  file.names <- paste0(profile, "_", var.names, ".png")
  purrr::pwalk(list(file.names, plot.list), ggsave_fitmax, path = ts.path, scale = 2)

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
#' @param hop An object of class "hop" or "face" containing output data from one or more Hi-sAFe simulations.
#' @param output.path A character stting indicating the path to the directory where plots should be saved. Plots are
#' saved in a subdirectory within this directory named /monthCells/facetScheme.
#' If no value is provided, the experiment/simulation path is read from the hop object, and a directory is created there called "analysis/diagnostics".
#' @param schemes A character vector of the facet schemes to run. Possible schemes are described in Details and have the following names: \code{year.simname}, \code{month.simname}, \code{month.year}.
#' @param trees Logical indicating if a point should be plotted at the location of each tree.
#' @param canopies Logical indicating if an elipsoid should be plotted representing the size of each tree canopy.
#' @param tree.simus Logical indicating whether only simulations containing trees should be plotted.
#' @export
#' @importFrom dplyr %>%
#' @family hisafe diagnostic fucntions
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' mydata <- read_hisafe(path = "mydir", simu.names = "MySimulation")
#'
#' # You can create tile plots of every monthCells variable:
#' diag_hisafe_monthcells(mydata)
#' }
diag_hisafe_monthcells <- function(hop,
                                   output.path = NULL,
                                   schemes     = c("year.simname", "month.simname", "month.year"),
                                   trees       = TRUE,
                                   canopies    = TRUE,
                                   tree.simus  = FALSE) {

  allowed.schemes <- c("year.simname", "month.simname", "month.year")
  is_hop(hop, error = TRUE)
  profile_check(hop, "monthCells", error = TRUE)
  if(!(is.character(output.path) | is.null(output.path))) stop("output.path argument must be a character vector",                          call. = FALSE)
  if(!all(schemes %in% allowed.schemes))                  stop("schemes argument must be one of: year.simname, month.simname, month.year", call. = FALSE)

  monthcells.path <- clean_path(paste0(diag_output_path(hop = hop, output.path = output.path), "/diagnostics/monthCells/"))
  dir.create(monthcells.path, recursive = TRUE, showWarnings = FALSE)

  var.names <- vars_to_diag(hop = hop, profile = "monthCells", last.col = "y")

  ## Create plots
  if("year.simname" %in% schemes) {
    plot.dir1 <- paste0(monthcells.path, "year_simname/")
    dir.create(plot.dir1, recursive = TRUE, showWarnings = FALSE)
    plot.list1 <- purrr::map(var.names, plot_hisafe_monthcells,
                             hop        = hop,
                             colfacet   = "SimulationName",
                             rowfacet   = "Year",
                             simu.names = "all",
                             years      = seq(min(hop$monthCells$Year), max(hop$monthCells$Year), 5),
                             months     = 6,
                             trees      = trees,
                             canopies   = canopies,
                             tree.simus = tree.simus)
    file.names <- paste0("monthCells_year_simname_", var.names, ".png")
    purrr::pwalk(list(file.names, plot.list1), ggsave_fitmax, path = plot.dir1, scale = 2)
  } else { plot.list1 <- list() }

  if("month.simname" %in% schemes) {
    plot.dir2 <- paste0(monthcells.path, "month_simname/")
    dir.create(plot.dir2, recursive = TRUE, showWarnings = FALSE)
    plot.list2 <- purrr::map(var.names, plot_hisafe_monthcells,
                             hop        = hop,
                             colfacet   = "SimulationName",
                             rowfacet   = "Month",
                             simu.names = "all",
                             years      = round(median(hop$monthCells$Year), 0),
                             months     = 1:12,
                             trees      = trees,
                             canopies   = canopies,
                             tree.simus = tree.simus)
    file.names <- paste0("monthCells_month_simname_", var.names, ".png")
    purrr::pwalk(list(file.names, plot.list2), ggsave_fitmax, path = plot.dir2, scale = 2)
  } else { plot.list2 <- list() }

  plot.list3.tog <- list()
  if("month.year" %in% schemes) {
    plot.dir3 <- paste0(monthcells.path, "month_year/")
    dir.create(plot.dir3, recursive = TRUE, showWarnings = FALSE)
    simus.for.scheme <- unique(hop$monthCells$SimulationName)
    if(tree.simus) {
      profile_check(hop, "tree.info", error = TRUE)
      simus.for.scheme <- simus.for.scheme[simus.for.scheme %in% unique(hop$tree.info$SimulationName)]
    }
    for(sim.name in simus.for.scheme) {
      plot.list3 <- purrr::map(var.names, plot_hisafe_monthcells,
                               hop        = hop,
                               colfacet   = "Year",
                               rowfacet   = "Month",
                               simu.names = sim.name,
                               years      = seq(min(hop$monthCells$Year), max(hop$monthCells$Year), 5),
                               months     = 1:12,
                               trees      = trees,
                               canopies   = canopies,
                               tree.simus = tree.simus)

      bad.plot.check <- unlist(purrr::map(plot.list3, is.logical))
      plot.list3 <- plot.list3[!bad.plot.check]

      file.names <- paste0("monthCells_month_year_", sim.name, "_", var.names, ".png")
      file.names <- file.names[!bad.plot.check]

      purrr::pwalk(list(file.names, plot.list3), ggsave_fitmax, path = plot.dir3, scale = 2)
      plot.list3.tog <- c(plot.list3.tog, plot.list3)
    }
  }

  ## Invisibly return list of plot objects
  invisible(list(year.simname = plot.list1, month.simname = plot.list2, month.year = plot.list3.tog))
}

#' Plot annualCells diagnostics of Hi-sAFe output
#' @description Creates a tile plot of every Hi-sAFe annualCells output variable, with one plot for every 5 simulation years.
#' All plots are saved as png files to a specifified output path.
#' @return Invisibly returns a list of \code{ggplot} objects.
#' @param hop An object of class "hop" or "face" containing output data from one or more Hi-sAFe simulations.
#' @param years A numeric vector containing the years (after planting) to include. Use "all" to include all available values.
#' @param output.path A character stting indicating the path to the directory where plots should be saved. Plots are
#' saved in a subdirectory within this directory named /annualCells
#' If no value is provided, the experiment/simulation path is read from the hop object, and a directory is created there called "analysis/diagnostics".
#' @param ... Other arguments passed to \code{\link{plot_hisafe_annualcells}}.
#' @export
#' @importFrom dplyr %>%
#' @family hisafe diagnostic fucntions
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' mydata <- read_hisafe(path = "mydir", simu.names = "MySimulation")
#'
#' # You can create tile plots of every annualCells variable:
#' diag_hisafe_annualcells(mydata)
#' }
diag_hisafe_annualcells <- function(hop,
                                   years       = seq(min(hop$annualCells$Year), max(hop$annualCells$Year), 5),
                                   output.path = NULL, ...) {

  is_hop(hop, error = TRUE)
  profile_check(hop, "annualCells", error = TRUE)
  if(!(is.character(output.path) | is.null(output.path))) stop("output.path argument must be a character vector", call. = FALSE)

  annualcells.path <- clean_path(paste0(diag_output_path(hop = hop, output.path = output.path), "/diagnostics/annualCells/"))
  dir.create(annualcells.path, recursive = TRUE, showWarnings = FALSE)

  var.names <- vars_to_diag(hop = hop, profile = "annualCells", last.col = "y")
  plot.list <- purrr::map(var.names, plot_hisafe_annualcells, hop = hop, years = years, ...)

  file.names <- paste0("annualCells_", var.names, ".png")
  purrr::pwalk(list(file.names, plot.list), ggsave_fitmax, path = annualcells.path, scale = 2)

  invisible(plot.list)
}

#' Plot cells diagnostics of Hi-sAFe output
#' @description Creates a tile plot of every Hi-sAFe cells output variable. All plots are saved as png files to a specifified output path.
#' @return Invisibly returns a list of \code{ggplot} objects.
#' @param hop An object of class "hop" or "face" containing output data from one or more Hi-sAFe simulations.
#' @param dates A character vector (in the format "YYYY-MM-DD") or a vector of class Date of the dates to include.
#' @param output.path A character stting indicating the path to the directory where plots should be saved. Plots are
#' saved in a subdirectory within this directory named /annualCells
#' If no value is provided, the experiment/simulation path is read from the hop object, and a directory is created there called "analysis/diagnostics".
#' @param ... Other arguments passed to \code{\link{plot_hisafe_cells}}.
#' @export
#' @importFrom dplyr %>%
#' @family hisafe diagnostic fucntions
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' mydata <- read_hisafe(path = "mydir", simu.names = "MySimulation")
#'
#' # You can create tile plots of every annualCells variable:
#' diag_hisafe_cells(mydata, "2000-07-01")
#' }
diag_hisafe_cells <- function(hop,
                              dates       = unique(grep("-07-01", as.character(hop$cells$Date), value = TRUE))[seq(1, 40, 5)],
                              output.path = NULL, ...) {

  is_hop(hop, error = TRUE)
  profile_check(hop, "cells", error = TRUE)
  if(!(is.character(output.path) | is.null(output.path))) stop("output.path argument must be a character vector", call. = FALSE)

  cells.path <- clean_path(paste0(diag_output_path(hop = hop, output.path = output.path), "/diagnostics/cells/"))
  dir.create(cells.path, recursive = TRUE, showWarnings = FALSE)

  var.names <- vars_to_diag(hop = hop, profile = "cells", last.col = "y")
  plot.list <- purrr::map(var.names, plot_hisafe_cells, hop = hop, dates = dates, ...)

  file.names <- paste0("cells_", var.names, ".png")
  purrr::pwalk(list(file.names, plot.list), ggsave_fitmax, path = cells.path, scale = 2)

  invisible(plot.list)
}

#' Plot timeseries diagnostics of Hi-sAFe voxels output
#' @description Plots a dail timeseries of every Hi-sAFe output variable in the voxels output profile.
#' All plots are saved as png files to a specifified output path.
#' @return Invisibly returns a list of \code{ggplot} objects. If \code{hop} contains
#' data from more than one Hi-sAFe simulation, the plots will by faceted by SimulationName.
#' @param hop An object of class "hop" or "face" containing output data from one or more Hi-sAFe simulations.
#' @param output.path A character stting indicating the path to the directory where plots should be saved.
#' Plots are saved in a subdirectory within this directory named by \code{profile}.
#' If no value is provided, the experiment/simulation path is read from the hop object, and a directory is created there called "analysis/diagnostics".
#' @param ... Other arguments passed to \code{\link{plot_hisafe_voxels}}.
#' @export
#' @importFrom dplyr %>%
#' @family hisafe diagnostic fucntions
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' mydata <- read_hisafe(path = "mydir", simu.names = "MySimulation")
#'
#' # You can create an annual timeseries of every variable:
#' diag_hisafe_ts(mydata)
#'
#' # For daily timeseries instead:
#' diag_hisafe_ts(mydata, "daily")
#' }
diag_hisafe_voxels <- function(hop, output.path = NULL, ...) {

  is_hop(hop, error = TRUE)
  profile_check(hop, "voxels", error = TRUE)
  if(!(is.character(output.path) | is.null(output.path))) stop("output.path argument must be a character vector", call. = FALSE)

  voxels.path <- clean_path(paste0(diag_output_path(hop = hop, output.path = output.path), "/diagnostics/voxels/"))
  dir.create(voxels.path, recursive = TRUE, showWarnings = FALSE)

  var.names <- vars_to_diag(hop = hop, profile = "voxels", last.col = "z")
  plot.list <- purrr::map(var.names, plot_hisafe_voxels, hop = hop, ...)

  file.names <- paste0("voxels_", var.names, ".png")
  purrr::pwalk(list(file.names, plot.list), ggsave_fitmax, path = voxels.path, scale = 2)

  invisible(plot.list)
}

#' Shortcut to Hi-sAFe diagnostics
#' @description Runs the various Hi-sAFe diagnostic functions from a single call.
#' @return Invisibly returns \code{TRUE}.
#' @param hop An object of class hop or face.
#' @param trees Logical indicating if trees profile diagnostic plots should be made.
#' @param plot Logical indicating if plot profile diagnostic plots should be made.
#' @param climate Logical indicating if climate profile diagnostic plots should be made.
#' @param cells Logical indicating if cells profile diagnostic plots should be made.
#' @param monthCells Logical indicating if monthCells profile diagnostic plots should be made (with tree.simus = TRUE).
#' @param annualCells Logical indicating if annualCells profile diagnostic plots should be made.
#' @param voxels Logical indicating if voxels profile diagnostic plots should be made.
#' @param ... Other arguments passed to \code{\link{plot_hisafe_ts}}.
#' @export
#' @family hisafe diagnostic functions
#' @examples
#' \dontrun{
#' diag_hisafe(myhop)
#' }
diag_hisafe <- function(hop,
                        trees       = TRUE,
                        plot        = TRUE,
                        climate     = TRUE,
                        cells       = TRUE,
                        monthCells  = TRUE,
                        annualCells = FALSE,
                        voxels      = TRUE, ...) {

  is_hop(hop, error = TRUE)
  if(!all(is.logical(c(trees, plot, climate, cells, monthCells, annualCells, voxels)))) {
    stop("all arguments except for hop must be logicals", call. = FALSE)
  }

  profiles.todo <- which_profiles(hop = hop, profiles = c("trees", "plot", "climate", "cells")[c(trees, plot, climate, cells)])

  if(length(profiles.todo) >= 1) {
    for(p in profiles.todo) {
      cat("\n-- Plotting", p, "diagnostics")
      diag_hisafe_ts(hop = hop, profile = p, ...)
    }
  }

  if(annualCells & profile_check(hop, "annualCells")) {
    cat("\n-- Plotting annualCells diagnostics")
    diag_hisafe_annualcells(hop = hop)
  }

  if(monthCells & profile_check(hop, "monthCells")) {
    cat("\n-- Plotting monthCells diagnostics")
    diag_hisafe_monthcells(hop = hop, tree.simus = TRUE)
  }

  if(voxels & profile_check(hop, "voxels")) {
    cat("\n-- Plotting voxels diagnostics")
    diag_hisafe_voxels(hop = hop)
  }

  invisible(TRUE)
}

#' Determine diagnostic output path
#' @description Determines diagnostic output path
#' @return The output path
#' @param hop An object of class hop or face
#' @param output.path The output.path argument from a diagnostic function
#' @keywords internal
diag_output_path <- function(hop, output.path) {
  if(is.null(output.path)) output.path <- clean_path(paste0(hop$exp.path, "/analysis"))
  return(output.path)
}

#' Determine diagnostic variables to plot
#' @description Determines diagnostic variables to plot
#' @return A character vector of the names of variables to plot
#' @param hop An object of class hop or face
#' @param profile A character string of the profile
#' @param last.call A character string of the name of the variable which is the last "header" variable added by Hi-sAFe
#' @keywords internal
vars_to_diag <- function(hop, profile, last.col) {
  hop[[profile]] <- hop[[profile]] %>%
    dplyr::select_if(is.numeric) %>%
    dplyr::select_if(~sum(!is.na(.)) > 0) # cols with only "error!" output are all NA's and cause plot errors
  var.names <- names(hop[[profile]])[(which(names(hop[[profile]]) == last.col) + 1):length(names(hop[[profile]]))]
return(var.names)
}
