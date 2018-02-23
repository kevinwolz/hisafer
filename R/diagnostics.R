#' Plot timeseries diagnostics of Hi-sAFe output
#' @description Plots a daily or annual timeseries of every Hi-sAFe output variable in a specified output profile.
#' All plots are saved as png files to a specifified output path.
#' @return Invisibly returns a list of \code{ggplot} objects. If \code{hop} contains
#' data from more than one Hi-sAFe simulation, the plots will contain multiple lines, colored and labeled by SimulationName.
#' If the data contains two more tree ids, the plots will be faceted by tree id.
#' @param hop An object of class "hop" or "face" containing output data from one or more Hi-sAFe simulations.
#' @param profile The profile for which to plot a timeseries. If 'annualtree' or 'annualplot', annual timeseries are created.
#' If 'trees', 'plot', or 'climate', daily timeseries are created.
#' @param output.path A character stting indicating the path to the directory where plots should be saved.
#' Plots aresaved in a subdirectory within this directory named by \code{profile}.
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
#' # You can create an annual timeseries of every annualtree variable:
#' diag_hisafe_ts(mydata, "annualtree")
#'
#' # For daily timeseries of the trees profile instead:
#' diag_hisafe_ts(mydata, "trees")
#' }
diag_hisafe_ts <- function(hop, profile, output.path = NULL, ...) {

  supported.profiles <- c("annualtree", "annualplot", "trees", "plot", "climate")
  is_hop(hop, error = TRUE)
  profile_check(hop, profile, error = TRUE)
  if(!(profile %in% supported.profiles))                  stop("supplied profile is not supported by plot_hisafe_ts()", call. = FALSE)
  if(!(is.character(output.path) | is.null(output.path))) stop("output.path argument must be a character vector",       call. = FALSE)

  ts.path <- clean_path(paste0(diag_output_path(hop, output.path), "/diagnostics/", profile, "/"))
  dir.create(ts.path, recursive = TRUE, showWarnings = FALSE)

  last.col  <- ifelse(grepl("tree", profile), "id", "stepNum")
  var.names <- vars_to_diag(hop, profile, last.col)
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
                                   canopies    = TRUE) {

  allowed.schemes <- c("year.simname", "month.simname", "month.year")
  is_hop(hop, error = TRUE)
  profile_check(hop, "monthCells", error = TRUE)
  if(!(is.character(output.path) | is.null(output.path))) stop("output.path argument must be a character vector",                          call. = FALSE)
  if(!all(schemes %in% allowed.schemes))                  stop("schemes argument must be one of: year.simname, month.simname, month.year", call. = FALSE)

  monthcells.path <- clean_path(paste0(diag_output_path(hop, output.path), "/diagnostics/monthCells/"))
  dir.create(monthcells.path, recursive = TRUE, showWarnings = FALSE)

  var.names <- vars_to_diag(hop, "monthCells", "y")

  ## Create plots
  if("year.simname" %in% schemes){
    plot.dir1 <- paste0(monthcells.path, "year_simname/")
    dir.create(plot.dir1, recursive = TRUE, showWarnings = FALSE)
    plot.list1 <- purrr::map(var.names, plot_hisafe_monthcells,
                             hop        = hop,
                             colfacet   = "SimulationName",
                             rowfacet   = "Year",
                             simu.names = "all",
                             years      = seq(0, (max(hop$monthCells$Year) - min(hop$monthCells$Year)), 5),
                             months     = 6,
                             trees      = trees,
                             canopies   = canopies)
    file.names <- paste0("monthCells_year_simname_", var.names, ".png")
    purrr::pwalk(list(file.names, plot.list1), ggsave_fitmax, path = plot.dir1, scale = 2)
  } else { plot.list1 <- list() }

  if("month.simname" %in% schemes){
    plot.dir2 <- paste0(monthcells.path, "month_simname/")
    dir.create(plot.dir2, recursive = TRUE, showWarnings = FALSE)
    plot.list2 <- purrr::map(var.names, plot_hisafe_monthcells,
                             hop        = hop,
                             colfacet   = "SimulationName",
                             rowfacet   = "Month",
                             simu.names = "all",
                             years      = (round(median(hop$monthCells$Year),0) - min(hop$monthCells$Year)),
                             months     = 1:12,
                             trees      = trees,
                             canopies   = canopies)
    file.names <- paste0("monthCells_month_simname_", var.names, ".png")
    purrr::pwalk(list(file.names, plot.list2), ggsave_fitmax, path = plot.dir2, scale = 2)
  } else { plot.list2 <- list() }

  plot.list3.tog <- list()
  if("month.year" %in% schemes){
    plot.dir3 <- paste0(monthcells.path, "month_year/")
    dir.create(plot.dir3, recursive = TRUE, showWarnings = FALSE)
    for(sim.name in unique(hop$monthCells$SimulationName)){
      plot.list3 <- purrr::map(var.names, plot_hisafe_monthcells,
                               hop        = hop,
                               colfacet   = "Year",
                               rowfacet   = "Month",
                               simu.names = sim.name,
                               years      = seq(0, (max(hop$monthCells$Year) - min(hop$monthCells$Year)), 5),
                               months     = 1:12,
                               trees      = trees,
                               canopies   = canopies)

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

#' Plot annualcrop diagnostics of Hi-sAFe output
#' @description Creates a tile plot of every Hi-sAFe annualcrop output variable, with one plot for every 5 simulation years.
#' All plots are saved as png files to a specifified output path.
#' @return Invisibly returns a list of \code{ggplot} objects.
#' @param hop An object of class "hop" or "face" containing output data from one or more Hi-sAFe simulations.
#' @param years A numeric vector containing the years (after planting) to include. Use "all" to include all available values.
#' @param output.path A character stting indicating the path to the directory where plots should be saved. Plots are
#' saved in a subdirectory within this directory named /annualcrop
#' If no value is provided, the experiment/simulation path is read from the hop object, and a directory is created there called "analysis/diagnostics".
#' @param ... Other arguments passed to \code{\link{plot_hisafe_annualplot}}.
#' @export
#' @importFrom dplyr %>%
#' @family hisafe diagnostic fucntions
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' mydata <- read_hisafe(path = "mydir", simu.names = "MySimulation")
#'
#' # You can create tile plots of every annualcrop variable:
#' diag_hisafe_annualcrop(mydata)
#' }
diag_hisafe_annualcrop <- function(hop,
                                   years       = seq(0, (max(hop$annualcrop$Year) - min(hop$annualcrop$Year)), 5),
                                   output.path = NULL, ...) {

  is_hop(hop, error = TRUE)
  profile_check(hop, "annualcrop", error = TRUE)
  if(!(is.character(output.path) | is.null(output.path))) stop("output.path argument must be a character vector", call. = FALSE)

  annualcrop.path <- clean_path(paste0(diag_output_path(hop, output.path), "/diagnostics/annualcrop/"))
  dir.create(annualcrop.path, recursive = TRUE, showWarnings = FALSE)

  var.names <- vars_to_diag(hop, "annualcrop", "y")
  plot.list <- purrr::map(var.names, plot_hisafe_annualcrop, hop = hop, years = years, ...)

  file.names <- paste0("annualcrop_", var.names, ".png")
  purrr::pwalk(list(file.names, plot.list), ggsave_fitmax, path = annualcrop.path, scale = 2)

  invisible(plot.list)
}

#' Plot cells diagnostics of Hi-sAFe output
#' @description Creates a tile plot of every Hi-sAFe cells output variable. All plots are saved as png files to a specifified output path.
#' @return Invisibly returns a list of \code{ggplot} objects.
#' @param hop An object of class "hop" or "face" containing output data from one or more Hi-sAFe simulations.
#' @param dates A character vector (in the format "YYYY-MM-DD") or a vector of class Date of the dates to include.
#' @param output.path A character stting indicating the path to the directory where plots should be saved. Plots are
#' saved in a subdirectory within this directory named /annualcrop
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
#' # You can create tile plots of every annualcrop variable:
#' diag_hisafe_cells(mydata, "2000-07-01")
#' }
diag_hisafe_cells <- function(hop,
                              dates       = unique(grep("-07-01", as.character(hop$cells$Date), value = TRUE)),
                              output.path = NULL, ...) {

  is_hop(hop, error = TRUE)
  profile_check(hop, "cells", error = TRUE)
  if(!(is.character(output.path) | is.null(output.path))) stop("output.path argument must be a character vector", call. = FALSE)

  cells.path <- clean_path(paste0(diag_output_path(hop, output.path), "/diagnostics/cells/"))
  dir.create(cells.path, recursive = TRUE, showWarnings = FALSE)

  var.names <- vars_to_diag(hop, "cells", "y")
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

  voxels.path <- clean_path(paste0(diag_output_path(hop, output.path), "/diagnostics/voxels/"))
  dir.create(voxels.path, recursive = TRUE, showWarnings = FALSE)

  var.names <- vars_to_diag(hop, "voxels", "z")
  plot.list <- purrr::map(var.names, plot_hisafe_voxels, hop = hop, ...)

  file.names <- paste0("voxels_", var.names, ".png")
  purrr::pwalk(list(file.names, plot.list), ggsave_fitmax, path = voxels.path, scale = 2)

  invisible(plot.list)
}

#' Shortcut to Hi-sAFe diagnostics
#' @description Runs the various Hi-sAFe diagnostic functions from a single call.
#' @return Invisibly returns \code{TRUE}.
#' @param hop An object of class hop or face.
#' @param annualtree Logical indicating if annualtree profile diagnostic plots should be made.
#' @param annualplot Logical indicating if annualplot profile diagnostic plots should be made.
#' @param trees Logical indicating if trees profile diagnostic plots should be made.
#' @param plot Logical indicating if plot profile diagnostic plots should be made.
#' @param climate Logical indicating if climate profile diagnostic plots should be made.
#' @param annualcrop Logical indicating if annualcrop profile diagnostic plots should be made.
#' @param monthCells Logical indicating if monthCells profile diagnostic plots should be made.
#' @param voxels Logical indicating if voxels profile diagnostic plots should be made.
#' @param ... Other arguments passed to \code{\link{plot_hisafe_ts}}.
#' @export
#' @family hisafe diagnostic functions
#' @examples
#' \dontrun{
#' diag_hisafe(myhop)
#' }
diag_hisafe <- function(hop,
                        annualtree = TRUE,
                        annualplot = TRUE,
                        trees      = TRUE,
                        plot       = TRUE,
                        climate    = TRUE,
                        annualcrop = TRUE,
                        monthCells = TRUE,
                        cells      = TRUE,
                        voxels     = TRUE, ...) {

  is_hop(hop, error = TRUE)
  if(!all(is.logical(c(annualtree, annualplot, trees, plot, climate, annualcrop, monthCells, cells, voxels)))) {
    stop("all arguments except for hop must be logicals", call. = FALSE)
  }

  profiles.to.check <- names(hop)[!(names(hop) %in% c("variables", "plot.info", "tree.info", "exp.plan", "path", "exp.path"))]
  profiles.avail <- profiles.to.check[purrr::map_lgl(profiles.to.check, function(x) nrow(hop[[x]]) > 0)]
  profiles.todo  <- c("annualtree", "annualplot", "trees", "plot", "climate")[c(annualtree, annualplot, trees, plot, climate)]
  profiles.todo  <- profiles.avail[profiles.avail %in% profiles.todo]

  if(length(profiles.todo) >= 1) {
    for(p in profiles.todo) {
      cat("\n-- Plotting", p, "diagnostics")
      diag_hisafe_ts(hop = hop, profile = p, ...)
    }
  }

  if(annualcrop & profile_check(hop, "annualcrop")) {
    cat("\n-- Plotting annualcrop diagnostics")
    diag_hisafe_annualcrop(hop)
  }

  if(monthCells & profile_check(hop, "monthCells")) {
    cat("\n-- Plotting monthCells diagnostics")
    diag_hisafe_monthcells(hop)
  }

  if(cells & profile_check(hop, "cells")) {
    cat("\n-- Plotting voxels diagnostics")
    diag_hisafe_cells(hop)
  }

  if(voxels & profile_check(hop, "voxels")) {
    cat("\n-- Plotting voxels diagnostics")
    diag_hisafe_voxels(hop)
  }

  invisible(TRUE)
}

#' Determine diagnostic output path
#' @description Determines diagnostic output path
#' @return The output path
#' @param hop An object of class hop or face
#' @param output.path The output.path argument from a diagnostic function
diag_output_path <- function(hop, output.path) {
  if(is.null(output.path) & "hop-group" %in% class(hop)) {
    output.path <- clean_path(paste0(hop$exp.path, "/analysis"))
  } else if(is.null(output.path) & !("hop-group" %in% class(hop))){
    output.path <- clean_path(paste0(hop$path$path, "/analysis"))
  }
  return(output.path)
}

#' Determine diagnostic variables to plot
#' @description Determines diagnostic variables to plot
#' @return A character vector of the names of variables to plot
#' @param hop An object of class hop or face
#' @param profile A character string of the profile
#' @param last.call A character string of the name of the variable which is the last "header" variable added by Hi-sAFe
vars_to_diag <- function(hop, profile, last.col) {
  hop[[profile]] <- hop[[profile]] %>%
    dplyr::select_if(is.numeric) %>%
    dplyr::select_if(~sum(!is.na(.)) > 0) # cols with only "error!" output are all NA's and cause plot errors
  var.names <- names(hop[[profile]])[(which(names(hop[[profile]]) == last.col) + 1):length(names(hop[[profile]]))]
return(var.names)
}
