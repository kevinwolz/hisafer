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

  annual.profiles <- c("annualtree", "annualplot")
  daily.profiles  <- c("trees", "plot", "climate")
  if(!("hop" %in% class(hop)))                            stop("hop argument not of class hop",                   call. = FALSE)
  if(!(profile %in% c(annual.profiles, daily.profiles)))  stop("supplied profile is not supported",               call. = FALSE)
  if(nrow(hop[[profile]]) == 0)                           stop(paste("no data from", profile, "profile found"),   call. = FALSE)
  if(!(is.character(output.path) | is.null(output.path))) stop("output.path argument must be a character vector", call. = FALSE)

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
  plot.list <- purrr::map(var.names,
                          plot_hisafe_ts,
                          hop     = hop,
                          profile = profile,
                          plot    = TRUE, ...)

  ## Write plots to disk
  file.names <- paste0(profile, "_", var.names, ".png")
  purrr::pwalk(list(file.names, plot.list), ggsave_fitmax, path = ts.path, scale = 2)

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
  if(!("hop" %in% class(hop)))                            stop("hop argument not of class hop",                                            call. = FALSE)
  if(nrow(hop$monthCells) == 0)                           stop("no data from monthCells profile found",                                    call. = FALSE)
  if(!(is.character(output.path) | is.null(output.path))) stop("output.path argument must be a character vector",                          call. = FALSE)
  if(!all(schemes %in% allowed.schemes))                  stop("schemes argument must be one of: year.simname, month.simname, month.year", call. = FALSE)

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
#' @description Creates a tile plot of every Hi-sAFe annualcrop output variable. All plots are saved as png files to a specifified output path.
#' @return Invisibly returns a list of \code{ggplot} objects.
#' @param hop An object of class "hop" or "face" containing output data from one or more Hi-sAFe simulations.
#' @param output.path A character stting indicating the path to the directory where plots should be saved. Plots are
#' saved in a subdirectory within this directory named /annualcrop
#' If no value is provided, the experiment/simulation path is read from the hop object, and a directory is created there called "analysis/diagnostics".
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
#' # You can create tile plots of every annualcrop variable:
#' diag_hisafe_annualcrop(mydata)
#' }
diag_hisafe_annualcrop <- function(hop,
                                   output.path = NULL,
                                   trees       = TRUE,
                                   canopies    = TRUE) {

  if(!("hop" %in% class(hop)))                                         stop("hop argument not of class hop",                                            call. = FALSE)
  if(nrow(hop$annualcrop) == 0)                                        stop("no data from annualcrop profile found",                                    call. = FALSE)
  if(!(is.character(output.path) | is.null(output.path)))              stop("output.path argument must be a character vector",                          call. = FALSE)

  ## Create output directories
  if(is.null(output.path) & "hop-group" %in% class(hop)) {
    output.path <- clean_path(paste0(hop$exp.path, "/analysis/diagnostics"))
  } else if(is.null(output.path) & !("hop-group" %in% class(hop))){
    output.path <- clean_path(paste0(hop$path, "/analysis/diagnostics"))
  }
  annualcrop.path <- clean_path(paste0(output.path, "/annualcrop/"))
  dir.create(annualcrop.path, recursive = TRUE, showWarnings = FALSE)

  ## Clean columns & extract names of variables to plot
  # cols with only "error!" output are all NA's and cause plot errors
  hop$annualcrop <- hop$annualcrop %>% dplyr::select_if(~sum(!is.na(.)) > 0)
  var.names      <- names(hop$annualcrop)[(which(names(hop$annualcrop) == "y") + 1):length(names(hop$annualcrop))]

  ## Create plots
  plot.list <- purrr::map(var.names, plot_hisafe_annualcrop,
                          hop        = hop,
                          simu.names = "all",
                          years      = seq(0, (max(hop$annualcrop$Year) - min(hop$annualcrop$Year)), 5),
                          trees      = trees,
                          canopies   = canopies)
  file.names <- paste0("annualcrop_", var.names, ".png")
  purrr::pwalk(list(file.names, plot.list), ggsave_fitmax, path = annualcrop.path, scale = 2)

  ## Invisibly return list of plot objects
  invisible(plot.list)
}

#' Plot timeseries diagnostics of Hi-sAFe voxels output
#' @description Plots a dail timeseries of every Hi-sAFe output variable in the voxels output profile.
#' All plots are saved as png files to a specifified output path.
#' @return Invisibly returns a list of \code{ggplot} objects. If \code{hop} contains
#' data from more than one Hi-sAFe simulation, the plots will by faceted by SimulationName.
#' @param hop An object of class "hop" or "face" containing output data from one or more Hi-sAFe simulations.
#' @param output.path A character stting indicating the path to the directory where plots should be saved.
#' Plots aresaved in a subdirectory within this directory named by \code{profile}.
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

  if(!("hop" %in% class(hop)))                            stop("hop argument not of class hop",                   call. = FALSE)
  if(nrow(hop$voxels) == 0)                               stop(paste("no data from voxels profile found"),        call. = FALSE)
  if(!(is.character(output.path) | is.null(output.path))) stop("output.path argument must be a character vector", call. = FALSE)

  ## Create output directory
  if(is.null(output.path) & "hop-group" %in% class(hop)) {
    output.path <- clean_path(paste0(hop$exp.path, "/analysis/diagnostics"))
  } else if(is.null(output.path) & !("hop-group" %in% class(hop))){
    output.path <- clean_path(paste0(hop$path$path, "/analysis/diagnostics"))
  }
  ts.path <- clean_path(paste0(output.path, "/voxels/"))
  dir.create(ts.path, recursive = TRUE, showWarnings = FALSE)

  ## Clean columns & extract names of variables to plot
  # cols with only "error!" output are all NA's and cause plot errors
  hop$voxels  <- hop$voxels %>% dplyr::select_if(~sum(!is.na(.)) > 0)
  if(which(names(hop$voxels) == "Date") > 1){
    exp.plan.vars <- names(hop$voxels)[1:(which(names(hop$voxels) == "Date")-1)]
  } else {
    exp.plan.vars <- NULL
  }
  dont.plot.vars <- c(exp.plan.vars, "Date", "Day", "Month", "Year", "JulianDay", "stepNum", "cellId", "id", "x", "y", "z")
  var.names <- names(hop$voxels)[!(names(hop$voxels) %in% dont.plot.vars)]

  ## Create plots
  plot.list <- purrr::map(var.names,
                          plot_hisafe_voxels,
                          hop     = hop,
                          plot    = TRUE, ...)

  ## Write plots to disk
  file.names <- paste0("voxels_", var.names, ".png")
  purrr::pwalk(list(file.names, plot.list), ggsave_fitmax, path = ts.path, scale = 2)

  ## Invisibly return list of plot objects
  invisible(plot.list)
}
