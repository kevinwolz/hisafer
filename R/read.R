#' Read output from one or more Hi-sAFe simulations
#' @description Reads the designated output profiles from one or more Hi-sAFe simulations
#' @return An object of class "hop". This is a list of 13 data frames (tibbles):
#' \itemize{
#'  \item{trees}
#'  \item{plot}
#'  \item{climate}
#'  \item{cells}
#'  \item{monthCells}
#'  \item{annualCells}
#'  \item{voxels}
#'  \item{plot.info}{ - plot geometry data for each simulation}
#'  \item{tree.info}{ - tree species and location data for each simulation}
#'  \item{exp.plan}{ - the exp.plan of the hip object that generated the simulations}
#'  \item{metadata}{ - the metadata of each simulations (path to simulation folders, model versions, run date , run duration)}
#'  \item{path}{ - the path to the experiment/simulation folder}
#' }
#' @param hip An object of class "hip". To create a hip object see \code{\link{define_hisafe}}.
#' If \code{hip} is not provided, then \code{path} is required and the input data for the experiment is read from the experiment
#' summary .csv file created when building the experiment.
#' @param path A character string of the path to the directory containing the Hi-sAFe simulation folders.
#' If \code{hip} is not provided, then \code{path} is required. If \code{hip} is provided, \code{path} is ignored.
#' @param simu.names Names of the simulations to read. If "all", the default, then all simulations in the folder are read.
#' @param profiles A character vector of the names of Hi-sAFe output profiles to read.
#' #' If "all" the default, reads all supported Hi-sAFe output profiles. For currently supported profiles see: \code{\link{hisafe_profiles}}
#' @param show.progress Logical indicating whether progress messsages should be printed to the console.
#' @param read.inputs Logical indicating whether data should be read from the .PLD and .SIM files for the tree.info and plot.info data slots.
#' Setting this to \code{FALSE} severly limits hisafer's plotting/analysis capacity.
#' Should only be set to \code{FALSE} if input files are corrupted.
#' @param max.size The maximum file size (MB) that should be read. Files larger than this value will be ignored, with a warning.
#' @param date.min A character string of the minimum date to keep, in the format "YYYY-MM-DD".
#' If NA, the minimum date in the output data is used. Only used if \code{dates} is \code{NULL}.
#' @param date.max A character string of the maximum date to keep, in the format "YYYY-MM-DD".
#' If NA, the maximum date in the output data is used. Only used if \code{dates} is \code{NULL}.
#' @param dates A character vector (in the format "YYYY-MM-DD") or a vector of class Date of the dates to keep.
#' If \code{NULL}, then \code{date.max} and \code{date.min} are used instad.
#' @export
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#' # Reading in Hi-sAFe simulation:
#' myexp <- read_hisafe(myhip)
#'
#' # If only the tree data is required:
#' mytreeexp <- read_hisafe(myhip, profiles = "trees")
#' }
read_hisafe <- function(hip           = NULL,
                        path          = NULL,
                        simu.names    = "all",
                        profiles      = "all",
                        show.progress = TRUE,
                        read.inputs   = TRUE,
                        max.size      = 300,
                        date.min      = NA,
                        date.max      = NA,
                        dates         = NULL) {

  is_hip(hip, error = TRUE)
  if(is.null(hip) == is.null(path))                             stop("must provide hip or path, not both",                      call. = FALSE)
  if(!(all(is.character(simu.names)) | simu.names[1] == "all")) stop("simu.names argument must be 'all' or a character vector", call. = FALSE)
  if(!(all(is.character(profiles))   | profiles[1]   == "all")) stop("profiles argument must be 'all' or a character vector",   call. = FALSE)
  is_TF(show.progress)
  if(!(is.numeric(max.size) & length(max.size) == 1 & max.size > 0)) stop("max.size argument must be a positive number", call. = FALSE)

  if(profiles[1] == "all" & !is.null(hip)) profiles <- hip$profiles

  ## Read simulation inputs & extract cols that vary for binding to output data
  if(!is.null(hip)) {
    EXP.PLAN   <- hip$exp.plan
    path       <- hip$path
    if(simu.names[1] == "all") simu.names <- EXP.PLAN$SimulationName
  } else {
    path <- get_absolute_path(path)
    if(!dir.exists(path)) stop("directory specified by path does not exist", call. = FALSE)
    exp.summary.file <- clean_path(paste0(path, "/", basename(path), "_exp_summary.csv"))
    if(simu.names[1] == "all" & file.exists(exp.summary.file)) {
      EXP.PLAN <- readr::read_csv(exp.summary.file, col_types = readr::cols())
      simu.names <- EXP.PLAN$SimulationName
    } else if(simu.names[1] == "all" & !file.exists(exp.summary.file)){
      stop("simu.names argument can only be 'all' if hip is provided or if an experiment summary file is available in the experiment folder")
    } else if(simu.names[1] != "all" & file.exists(exp.summary.file)){
      EXP.PLAN <- readr::read_csv(exp.summary.file, col_types = readr::cols())
      if(!all(simu.names %in% EXP.PLAN$SimulationName)) {
        missing_simus <- simu.names[!(simu.names %in% EXP.PLAN$SimulationName)]
        stop(paste("the following simulations do not exist in the experiment summary file:", paste(missing_simus, collapse = ", ")), call. = FALSE)
      }
      EXP.PLAN <- dplyr::filter(EXP.PLAN, SimulationName %in% simu.names)
      simu.names <- EXP.PLAN$SimulationName
    } else if(length(simu.names) == 1) {
      EXP.PLAN <- dplyr::tibble(SimulationName = simu.names)
    } else {
      warning("hip not provided, and no experiment summary to read. This experiment may not have been created with hisafer.", call. = FALSE)
      EXP.PLAN <- dplyr::tibble(SimulationName = simu.names)
    }
  }

  ## Check for existance of all simualation folders
  simu.paths <- clean_path(paste0(path, "/" , simu.names))
  if(!all(purrr::map_lgl(simu.paths, dir.exists))) {
    missing_simus <- simu.names[!purrr::map_lgl(simu.paths, dir.exists)]
    stop(paste("the following simulations do not exist in the specified path:", paste(missing_simus, collapse = ", ")), call. = FALSE)
  }

  ## Read all data from all simulations & combine
  data <- purrr::map(simu.names,
                     read_simulation,
                     hip           = NULL,
                     path          = path,
                     profiles      = profiles,
                     show.progress = show.progress,
                     read.inputs   = read.inputs,
                     max.size      = max.size) %>%
    purrr::pmap(dplyr::bind_rows)
  ## Bind multiple simulations together
  # a more generic version of this line that handles cases where the number
  # of elements and order of names in each sublist can vary is:
  # purrr::map(map_df(data, ~ as.data.frame(purrr::map(.x, ~ unname(nest(.))))), bind_rows)
  # However, this isn't necessary becasue read_simulation always produces identically sized and named lists.

  ## Tidy up data
  data_tidy <- function(x){
    if(nrow(x) > 0) {
      x <- dplyr::left_join(EXP.PLAN, x, by = "SimulationName") %>%   # add EXP.PLAN cols
        dplyr::filter(!is.na(Date))                                   # output profiles with no data will have NA's in all columns
    } else {
      x <- dplyr::tibble()
    }
    return(x)
  }

  data$trees       <- data_tidy(data$trees)
  data$plot        <- data_tidy(data$plot)
  data$climate     <- data_tidy(data$climate)
  data$cells       <- data_tidy(data$cells)
  data$monthCells  <- data_tidy(data$monthCells)
  data$annualCells <- data_tidy(data$annualCells)
  data$voxels      <- data_tidy(data$voxels)
  data$path        <- ifelse(nrow(EXP.PLAN) > 1, path, simu.paths)

  ## Assign class designators
  if(length(simu.names) > 1) {
    class(data) <- c("hop-group", "hop", class(data))
  } else {
    class(data) <- c("hop", class(data))
  }

  ## Check if there are NO results at all
  if(!any(purrr::map_lgl(DATA.PROFILES, profile_check, hop = data))) {
    stop("No requested profiles were found for any of the requested simulations", call. = FALSE)
  }

  ## Warn if lengths of all simulations are not equal
  dum <- warn_unequal_lengths(data)

  data <- hop_filter(hop = data, date.min = date.min, date.max = date.max, dates = dates)

  return(data)
}

#' Read output from a single Hi-sAFe simulation
#' @description Reads the designated output profiles from a single Hi-sAFe simulation. Called from within \code{\link{read_hisafe}}.
#' @return An object of class "hop". This is a list of 12 data frames (tibbles):
#' \itemize{
#'  \item{trees}
#'  \item{plot}
#'  \item{climate}
#'  \item{cells}
#'  \item{monthCells}
#'  \item{annualCells}
#'  \item{voxels}
#'  \item{plot.info}{ - plot geometry data}
#'  \item{tree.info}{ - tree species and location data}
#'  \item{exp.plan}{ - the exp.plan of the hip object that generated the simulation}
#'  \item{path}{ - the path to the simulation folder}
#' }
#' @param hip An object of class "hip". To create a hip object see \code{\link{define_hisafe}}.
#' Cannot provided both \code{hip} and \code{simu.name}.
#' If \code{hip} is not provided, then \code{path} is required and the input data for the experiment is read from the experiment
#' summary .csv file created when building the experiment.
#' @param path A character string of the path to the directory containing the Hi-sAFe simulation folder.
#' If \code{hip} is not provided, then \code{path} is required. If both \code{hip} and \code{path} are provided, \code{path} is used.
#' @param simu.name The \code{SimulationName} of the Hi-sAFe simulation to read. This must be the same as the name of the Hi-sAFe simulation folder.
#' Cannot provided both \code{hip} and \code{simu.name}.
#' @param read.inputs Logical indicating whether data should be read from the .PLD and .SIM files for the tree.info and plot.info data slots.
#' Setting this to \code{FALSE} severly limits hisafer's plotting/analysis capacity.
#' Should only be set to \code{FALSE} if input files are corrupted.
#' @param profiles A character vector of the names of Hi-sAFe output profiles to read.
#' If "all" the default, reads all supported Hi-sAFe output profiles. For currently supported profiles see: \code{\link{hisafe_profiles}}
#' @param show.progress Logical indicating whether progress messsages should be printed to the console.
#' @param max.size The maximum file size (MB) that should be read. Files larger than this value will be ignored, with a warning.
#' @keywords internal
read_simulation <- function(simu.name, hip, path, profiles, show.progress, read.inputs, max.size) {

  is_hip(hip, error = TRUE)
  if(is.null(hip) == (is.null(simu.name) | is.null(path)))  stop("must provide hip OR (simu.name & path)", call. = FALSE)

  simu.path <- clean_path(paste0(path, "/" , simu.name))

  ## Read simulation inputs
  if(!is.null(hip)) {
    EXP.PLAN  <- hip$exp.plan
  } else {
    simu.summary.file <- paste0(simu.path, "/support/", simu.name, "_simulation_summary.csv")
    if(file.exists(simu.summary.file)){
      EXP.PLAN <- readr::read_csv(paste0(simu.path, "/support/", simu.name, "_simulation_summary.csv"), col_types = readr::cols()) %>%
        dplyr::mutate_if(function(x) all(is.na(x)), as.numeric)
    } else {
      warning("No simulation inputs summary (experimental plan) to read from simulation directory. This simulation may not have been created with hisafer.", call. = FALSE)
      EXP.PLAN <- dplyr::tibble(SimulationName = simu.name)
    }
  }

  ## Create profile paths
  if(profiles[1] == "all") profiles <- PUBLIC.PROFILES
  file.prefix <- paste0(simu.path, "/output-", simu.name, "/", simu.name, "_")
  files       <- paste0(file.prefix, profiles, ".txt" )

  ## Check for existence of all requested profiles and warn if profile does not exist
  if(!any(file.exists(files))) {
    warning(paste("No requested profiles found for the following simulation:", simu.name), call. = FALSE)
    output <- list(trees       = dplyr::tibble(),
                   plot        = dplyr::tibble(),
                   climate     = dplyr::tibble(),
                   cells       = dplyr::tibble(),
                   monthCells  = dplyr::tibble(),
                   annualCells = dplyr::tibble(),
                   voxels      = dplyr::tibble(),
                   plot.info   = dplyr::tibble(),
                   tree.info   = dplyr::tibble(),
                   exp.plan    = dplyr::tibble(),
                   metadata    = dplyr::tibble())
    return(output)
  } else if(!all(file.exists(files))) {
    missing.profiles <- basename(files[!file.exists(files)])
    missing.profile.error <- paste(c("The following requested profiles do not exist:",
                                     paste0("      --", missing.profiles)),
                                   collapse = "\n")
    warning(missing.profile.error, call. = FALSE)
    profiles <- profiles[file.exists(files)]
  }

  ## Read profiles
  if(show.progress) cat("\n\nReading:", simu.name, "\nProfiles:", paste0(profiles, collapse = ", "))

  if(length(profiles) >= 1) {
    out <- purrr::map(profiles, read_profile, path = file.prefix, simu.name = simu.name, show.progress = show.progress, max.size = max.size)
    names(out) <- profiles
  } else {
    out <- list()
  }

  join_plot   <- function(...) dplyr::left_join(..., by = BASE.COLS, suffix = c("", ".REMOVE"))
  join_trees  <- function(...) dplyr::left_join(..., by = c(BASE.COLS, "idTree"), suffix = c("", ".REMOVE"))
  join_cells  <- function(...) dplyr::left_join(..., by = c(BASE.COLS, "idCell", "x", "y"), suffix = c("", ".REMOVE"))
  join_voxels <- function(...) dplyr::left_join(..., by = c(BASE.COLS, "idCell", "idVoxel", "x", "y", "z"), suffix = c("", ".REMOVE"))

  plot.data   <- out[grep("^plot",        names(out))]
  trees.data  <- out[grep("^trees",       names(out))]
  cells.data  <- out[grep("^cells",       names(out))]
  voxels.data <- out[grep("^voxels",      names(out))]
  mcells.data <- out[grep("^monthCells",  names(out))]
  acells.data <- out[grep("^annualCells", names(out))]

  check_function <- function(x) is.null(x) | nrow(x) == 0
  out[["plot"]]        <- Reduce(join_plot,   plot.data[  !purrr::map_lgl(plot.data,   check_function)])
  out[["trees"]]       <- Reduce(join_trees,  trees.data[ !purrr::map_lgl(trees.data,  check_function)])
  out[["cells"]]       <- Reduce(join_cells,  cells.data[ !purrr::map_lgl(cells.data,  check_function)])
  out[["voxels"]]      <- Reduce(join_voxels, voxels.data[!purrr::map_lgl(voxels.data, check_function)])
  out[["monthCells"]]  <- Reduce(join_cells,  mcells.data[!purrr::map_lgl(mcells.data, check_function)])
  out[["annualCells"]] <- Reduce(join_cells,  acells.data[!purrr::map_lgl(acells.data, check_function)])

  get_prof <- function(out, prof) {
    if(is.null(out[[prof]])) {
      dv <- dplyr::tibble()
    } else {
      dv <- out[[prof]] %>%
        dplyr::distinct() %>%
        dplyr::select(-dplyr::ends_with(".REMOVE", ignore.case = FALSE))
    }
    return(dv)
  }

  ## Read plot characteristics from .PLD file
  if(read.inputs) {
    sim.path <- list.files(simu.path, ".sim$", full.names = TRUE)
    pld.path <- list.files(simu.path, ".pld$", full.names = TRUE)

    if(length(sim.path) == 0) stop(paste("there is no SIM file present in the simulation directory of:", simu.name, ". Set read.inputs to FALSE."), call. = FALSE)
    if(length(pld.path) == 0) stop(paste("there is no PLD file present in the simulation directory of:", simu.name, ". Set read.inputs to FALSE."), call. = FALSE)
    if(length(sim.path) > 1)  stop(paste("there is >1 SIM file present in the simulation directory of:", simu.name, ". Set read.inputs to FALSE."), call. = FALSE)
    if(length(pld.path) > 1)  stop(paste("there is >1 PLD file present in the simulation directory of:", simu.name, ". Set read.inputs to FALSE."), call. = FALSE)

    tree.info <- read_tree_info(path, simu.name)

    sim <- read_param_file(sim.path)
    pld <- read_param_file(pld.path)
    plotWidth          <- as.numeric(pld$PLOT$plotWidth$value)
    plotHeight         <- as.numeric(pld$PLOT$plotHeight$value)
    northOrientation   <- as.numeric(pld$PLOT$northOrientation$value)
    cellWidth          <- as.numeric(pld$PLOT$cellWidth$value)
    soilDepth          <- sum(pld$LAYERS$layers$value[[1]]$thick)
    waterTable         <- pld$WATER$waterTable$value
    simulationDayStart <- sim$SIMULATION$simulationDayStart$value[1]

    plot.info <- dplyr::tibble(SimulationName      = simu.name,
                               plotWidth           = plotWidth,
                               plotHeight          = plotHeight,
                               plotAreaM2          = plotWidth * plotHeight,
                               plotAreaHa          = plotWidth * plotHeight / 10000,
                               northOrientation    = northOrientation,
                               cellWidth           = cellWidth,
                               soilDepth           = soilDepth,
                               waterTable          = waterTable,
                               simulationDayStart  = simulationDayStart)
  } else {
    plot.info <- tree.info <- dplyr::tibble()
  }

  ## Read simulation metadata
  simu.metadata <- dplyr::tibble(SimulationName = simu.name, path = simu.path)

  session.path <- clean_path(paste0(path, "/", simu.name, "/output-", simu.name, "/session.txt"))
  if(file.exists(session.path)) {
    session.names <- c("hisafe.version", "stics.version", "capsis.version", "simulation.start", "simulation.seconds")
    session.info <- scan(session.path, what = "character", encoding = "latin1", sep = "\n", quiet = TRUE) %>%
      .[-1] %>%
      purrr::map(strsplit, split = " = ") %>%
      purrr::map(1) %>%
      purrr::map(2) %>%
      c(rep(list(NA), length(session.names) - length(.))) %>% # if a simulation was not completed, simulation.start & simulation.seconds will not be in the file
      as.data.frame(col.names = session.names, stringsAsFactors = FALSE) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(simulation.start   = lubridate::ymd_hms(simulation.start)) %>%
      dplyr::mutate(simulation.seconds = as.numeric(simulation.seconds))
    simu.metadata <- dplyr::bind_cols(simu.metadata, session.info)
  }


  ## Ensure crop names are characters in plot
  clean_crop_name <- function(x) {
    for(i in c("mainCropName", "interCropName")) {
      if(i %in% names(x)) {
        x[[i]] <- as.character(x[[i]])
        x[[i]][x[[i]] == "0"]   <- NA
      }
    }
    return(x)
  }

  ## Creatd output list & assign class
  output <- list(trees       = get_prof(out, "trees"),
                 plot        = clean_crop_name(get_prof(out, "plot")),
                 climate     = get_prof(out, "climate"),
                 cells       = get_prof(out, "cells"),
                 monthCells  = get_prof(out, "monthCells"),
                 annualCells = get_prof(out, "annualCells"),
                 voxels      = get_prof(out, "voxels"),
                 plot.info   = plot.info,
                 tree.info   = tree.info,
                 exp.plan    = EXP.PLAN,
                 metadata    = simu.metadata)

  return(output)
}

#' Read example Hi-sAFe experiment output
#' @description Reads in an example Hi-sAFe experiment. For more details see \code{\link{read_hisafe}}.
#' @return An object of class "hop".
#' @param profiles A character vector of the names of Hi-sAFe output profiles to read.
#' @param ... Other arguments passed to \code{\link{read_hisafe}}.
#' @export
read_hisafe_example <- function(profiles = c("plot", "trees", "climate", "monthCells", "cells"), ...) {

  if(!all(is.character(profiles))) stop("profiles argument must be a character vector", call. = FALSE)

  hop <- read_hisafe(path     = clean_path(paste0(system.file("extdata", "example_output", package = "hisafer"), "/")),
                     profiles = profiles, ...)
  return(hop)
}

#' Read a Hi-sAFe output profile
#' @description Wrapper for read_hisafe_output_file.
#' @return A list containing the profile data and the profile variable definitions.
#' @param profile A character string of the profile name.
#' @param path A character string of the path to the folder containing the profiles.
#' @param simu.name A character string of the name of the simulation being read.
#' @param show.progress Logical indicating whether progress messsages should be printed to the console.
#' @param max.size The maximum file size (MB) that should be read. Files larger than this value will be ignored, with a warning.
#' @keywords internal
read_profile <- function(profile, path, simu.name, show.progress, max.size) {
  file <- paste0(path, profile, ".txt")
  if(show.progress) cat(paste0("\n   -- reading:  ", profile, collapse = ", "))
  if((file.info(file)$size / 1e6) < max.size) {
    profile.data <- read_hisafe_output_file(profile = file, simu.name = simu.name) %>%
      dplyr::mutate_if(is.logical, as.numeric)   # columns read as logical must be coered to numeric to prevent plotting errors
  } else {
    warning(paste(profile, "profile too large (> max.size) to read"), call. = FALSE)
    profile.data <- dplyr::tibble()
  }
  return(profile.data)
}

#' Read a single Hi-sAFe output profile
#' @description Reads the designated output profiles from a single Hi-sAFe simulation.
#' @return A data frame (tibbles) containing the data from the profile.
#' @param profile A character string of the path to the profile to be read.
#' @param simu.name A character string of the name of the simulation being read.
#' @importFrom dplyr %>%
#' @keywords internal
read_hisafe_output_file <- function(profile, simu.name){
  dat <- dplyr::as_tibble(read.table(file             = profile,
                                     header           = TRUE,
                                     sep              = "\t",
                                     stringsAsFactors = FALSE,
                                     # "error!" is output by HISAFE & causes table merge errors if left; "NaN" output causes plot problems
                                     na.strings       = c("NA", "error!", "NaN", "-9999", "Infinity"),
                                     encoding         = "latin1"))

  if(nrow(dat) == 0) {
    warning(paste0(basename(profile), " exists but contains no data"), call. = FALSE)
  } else {
    dat <- dat %>%
      dplyr::filter(Year != 0) %>%
      dplyr::mutate(Date = lubridate::dmy(Date)) # convert Date column into date class
    if(nrow(dat) == 0) {
      warning(paste0(basename(profile), " exists but contains no data"), call. = FALSE)
    } else if(unique(dat$SimulationName) != simu.name) {
      warning(paste0("SimulationName in ", basename(profile), " (", unique(dat$SimulationName), ") does not match the name of the simulation folder (",
                     simu.name, "). Simulation folder name will override SimulationName in export profile."), call. = FALSE)
      dat$SimulationName <- simu.name
    }
  }
  return(dat)
}

#' Read tree information from a Hi-sAFe pld file
#' @description Reads tree information from a Hi-sAFe pld file. Used by \code{\link{read_simulation}}.
#' @return A data frame (tibble) containing tree id, species, x, y, and pruning/root pruning parameters.
#' @param path A character string of the path to the directory containing the simulation folder.
#' @param simu.name A character string of the simualation name.
#' @importFrom dplyr %>%
#' @keywords internal
read_tree_info <- function(path, simu.name) {
  sim.path <- list.files(clean_path(paste0(path, "/", simu.name, "/")), ".sim$", full.names = TRUE)
  if(length(sim.path) > 1) stop(paste("there is more than 1 SIM file present in the simulation directory of:", simu.name), call. = FALSE)
  pld.path <- list.files(clean_path(paste0(path, "/", simu.name, "/")), ".pld$", full.names = TRUE)
  if(length(pld.path) > 1) stop(paste("there is more than 1 PLD file present in the simulation directory of:", simu.name), call. = FALSE)
  sim <- read_param_file(sim.path)
  pld <- read_param_file(pld.path)
  if(!pld$TREE_INITIALIZATION$tree.initialization$commented) {
    tree.info <- pld$TREE_INITIALIZATION$tree.initialization$value[[1]] %>%
      dplyr::mutate(special.case = treeX == 0 & treeY == 0) %>% # special case when x == 0 & y == 0 : tree is at scene center
      dplyr::mutate(treeX = treeX + special.case * pld$PLOT$plotWidth$value  / 2) %>%
      dplyr::mutate(treeY = treeY + special.case * pld$PLOT$plotHeight$value / 2) %>%
      dplyr::rename(x = treeX, y = treeY) %>%
      dplyr::select(species, x, y)
    tree.info <- tree.info %>%
      dplyr::mutate(idTree = 1:nrow(tree.info)) %>%
      dplyr::mutate(SimulationName = rep(simu.name, nrow(tree.info))) %>%
      dplyr::mutate(simulationYearStart = sim$SIMULATION$simulationYearStart$value) %>%
      dplyr::mutate(simulationDayStart  = sim$SIMULATION$simulationDayStart$value) %>%
      dplyr::select(SimulationName, idTree, dplyr::everything())
    if(!sim$TREE_PRUNING$treePruningYears$commented) {
      tree.info <- tree.info %>%
        dplyr::mutate(treePruningYears     = list(sim$TREE_PRUNING$treePruningYears$value)) %>%
        dplyr::mutate(treePruningProp      = list(sim$TREE_PRUNING$treePruningProp$value)) %>%
        dplyr::mutate(treePruningMaxHeight = list(sim$TREE_PRUNING$treePruningMaxHeight$value)) %>%
        dplyr::mutate(treePruningDays      = list(sim$TREE_PRUNING$treePruningDays$value))
    } else {
      tree.info <- tree.info %>%
        dplyr::mutate(treePruningYears     = list(NA_real_)) %>%
        dplyr::mutate(treePruningProp      = list(NA_real_)) %>%
        dplyr::mutate(treePruningMaxHeight = list(NA_real_)) %>%
        dplyr::mutate(treePruningDays      = list(NA_real_))
    }
    if(!sim$TREE_ROOT_PRUNING$treeRootPruningYears$commented) {
      tree.info <- tree.info %>%
        dplyr::mutate(treeRootPruningYears    = list(sim$TREE_ROOT_PRUNING$treeRootPruningYears$value)) %>%
        dplyr::mutate(treeRootPruningDays     = list(sim$TREE_ROOT_PRUNING$treeRootPruningDays$value)) %>%
        dplyr::mutate(treeRootPruningDistance = list(sim$TREE_ROOT_PRUNING$treeRootPruningDistance$value)) %>%
        dplyr::mutate(treeRootPruningDepth    = list(sim$TREE_ROOT_PRUNING$treeRootPruningDepth$value))
    } else {
      tree.info <- tree.info %>%
        dplyr::mutate(treeRootPruningYears    = list(NA_real_)) %>%
        dplyr::mutate(treeRootPruningDays     = list(NA_real_)) %>%
        dplyr::mutate(treeRootPruningDistance = list(NA_real_)) %>%
        dplyr::mutate(treeRootPruningDepth    = list(NA_real_))
    }
  } else {
    tree.info <- dplyr::tibble()
  }
  return(tree.info)
}
