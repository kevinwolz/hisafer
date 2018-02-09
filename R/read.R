#' Read output from one or more Hi-sAFe simulations
#' @description Reads the designated output profiles from one or more Hi-sAFe simulations
#' @return An object of class "hop". This is a list of 16 data frames (tibbles):
#' \itemize{
#'  \item{annualtree}
#'  \item{annualcrop}
#'  \item{annualplot}
#'  \item{trees}
#'  \item{plot}
#'  \item{climate}
#'  \item{monthCells}
#'  \item{cells}
#'  \item{roots}
#'  \item{voxels}
#'  \item{variables}{ - variable descriptions and units from all read profiles}
#'  \item{plot.info}{ - plot geometry data for each simulation}
#'  \item{tree.info}{ - tree species and location data for each simulation}
#'  \item{exp.plan}{ - the exp.plan of the hip object that generated the simulation}
#'  \item{path}{ - the paths to the simulation folders}
#'  \item{exp.path}{ - the path to the experiment folder}
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
#' @param max.size The maximum file size (in bytes) that should be read. Files larger than this value will be ignored, with a warning.
#' @param date.min A character string of the minimum date to keep, in the format "YYYY-MM-DD".
#' If NA, the minimum date in the output data is used.
#' @param date.max A character string of the maximum date to keep, in the format "YYYY-MM-DD".
#' If NA, the maximum date in the output data is used.
#' @export
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#' # Reading in Hi-sAFe simulation:
#' myexp <- read_hisafe(myhip)
#'
#' # If only the annual tree data is required:
#' mytreeexp <- read_hisafe(myhip, profiles = "annualtree")
#' }
read_hisafe <- function(hip           = NULL,
                        path          = NULL,
                        simu.names    = "all",
                        profiles      = "all",
                        show.progress = TRUE,
                        max.size      = 3e8,
                        date.min      = NA,
                        date.max      = NA) {

  if(!is.null(hip) & !("hip" %in% class(hip)))                  stop("data not of class hip",                                   call. = FALSE)
  if(is.null(hip) == is.null(path))                             stop("must provide hip or path, not both",                      call. = FALSE)
  if(!(all(is.character(simu.names)) | simu.names[1] == "all")) stop("simu.names argument must be 'all' or a character vector", call. = FALSE)
  if(!(all(is.character(profiles))   | profiles[1]   == "all")) stop("profiles argument must be 'all' or a character vector",   call. = FALSE)
  if(!is.logical(show.progress))                                stop("show.progress argument must be a logical",                call. = FALSE)
  if(!(is.numeric(max.size) & length(max.size) == 1))           stop("max.size argument must be a positive integer",            call. = FALSE)
  if(max.size %% 1 != 0 & max.size > 0)                         stop("max.size argument must be a positive integer",            call. = FALSE)

  if(profiles[1] == "all" & !is.null(hip)) profiles <- hip$profiles

  ## Read simulation inputs & extract cols that vary for binding to output data
  if(!is.null(hip)) {
    EXP.PLAN   <- hip$exp.plan
    path       <- hip$path
    if(simu.names[1] == "all") simu.names <- EXP.PLAN$SimulationName
  } else {
    path <- R.utils::getAbsolutePath(path)
    if(!dir.exists(path)) stop("directory specified by path does not exist", call. = FALSE)
    exp.summary.file <- clean_path(paste0(path, "/", tail(strsplit(path, "/")[[1]], n = 1), "_exp_summary.csv"))
    if(simu.names[1] == "all" & file.exists(exp.summary.file)) {
      EXP.PLAN <- readr::read_csv(exp.summary.file, col_types = readr::cols())
      simu.names <- EXP.PLAN$SimulationName
    } else if(simu.names[1] == "all" & !file.exists(exp.summary.file)){
      stop("simu.names argument can only be 'all' if hip is provided or if an experiment summary file is available in the experiment folder")
    } else if(simu.names[1] != "all" & file.exists(exp.summary.file)){
      EXP.PLAN <- readr::read_csv(exp.summary.file, col_types = readr::cols()) %>%
        dplyr::filter(SimulationName %in% simu.names)
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
      x <- dplyr::left_join(EXP.PLAN, x, by = "SimulationName") %>%   # add EXP.PLAN cols to annual data
        dplyr::filter(!is.na(Date))                                   # output profiles with no data will have NA's in all columns
    } else {
      x <- dplyr::tibble()
    }
    return(x)
  }

  data$annualtree  <- data_tidy(data$annualtree)
  data$annualcrop  <- data_tidy(data$annualcrop)
  data$annualplot  <- data_tidy(data$annualplot)
  data$trees       <- data_tidy(data$trees)
  data$plot        <- data_tidy(data$plot)
  data$climate     <- data_tidy(data$climate)
  data$roots       <- data_tidy(data$roots)
  data$monthCells  <- data_tidy(data$monthCells)
  data$cells       <- data_tidy(data$cells)
  data$voxels      <- data_tidy(data$voxels)
  data$variables   <- dplyr::distinct(data$variables)            # remove duplicate variable descriptions
  data$exp.path    <- ifelse(nrow(EXP.PLAN) > 1, path, NA)

  ## Warn if lengths of all simulations are not equal
  if(length(simu.names) > 1) {
    year.summary <- data[[as.numeric(which.max(purrr::map_int(data[names(data) != "exp.path"], nrow)))]] %>%
      dplyr::group_by(SimulationName) %>%
      dplyr::summarize(n = dplyr::n_distinct(Year) - 1) %>%
      tidyr::unite(label, SimulationName, n, sep = ": ", remove = FALSE)
    if(length(unique(year.summary$n)) != 1) {
      year.length.warning <- paste(c("Simulation durations not equal!",
                                     "  Be careful when comparing simulations.",
                                     "  Simulation durations:",
                                     paste("   --", year.summary$label, "years")),
                                   collapse = "\n")
      warning(year.length.warning, call. = FALSE)
    }
  }

  ## Assign class designators
  if(length(simu.names) > 1) {
    class(data) <- c("hop-group", "hop", class(data))
  } else {
    class(data) <- c("hop", class(data))
  }

  data <- hop_date_filter(data, date.min = date.min, date.max = date.max)

  return(data)
}

#' Read output from a single Hi-sAFe simulation
#' @description Reads the designated output profiles from a single Hi-sAFe simulation. Called from within \code{\link{read_hisafe}}.
#' @return An object of class "hop". This is a list of 15 data frames (tibbles):
#' \itemize{
#'  \item{annualtree}
#'  \item{annualcrop}
#'  \item{annualplot}
#'  \item{trees}
#'  \item{plot}
#'  \item{climate}
#'  \item{monthCells}
#'  \item{cells}
#'  \item{roots}
#'  \item{voxels}
#'  \item{variables}{ - variable descriptions and units from all profiles}
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
#' @param profiles A character vector of the names of Hi-sAFe output profiles to read.
#' If "all" the default, reads all supported Hi-sAFe output profiles. For currently supported profiles see: \code{\link{hisafe_profiles}}
#' @param show.progress Logical indicating whether progress messsages should be printed to the console.
#' @param max.size The maximum file size (in bytes) that should be read. Files larger than this value will be ignored, with a warning.
read_simulation <- function(simu.name, hip, path, profiles, show.progress, max.size) {

  if(!is.null(hip) & !("hip" %in% class(hip)))              stop("data not of class hip", call. = FALSE)
  if(is.null(hip) == (is.null(simu.name) | is.null(path)))  stop("must provide hip OR (simu.name & path)", call. = FALSE)

  simu.path <- clean_path(paste0(path, "/" , simu.name))

  ## Read simulation inputs
  if(!is.null(hip)) {
    EXP.PLAN  <- hip$exp.plan
  } else {
    simu.summary.file <- paste0(simu.path, "/support/", simu.name, "_simulation_summary.csv")
    if(file.exists(simu.summary.file)){
      EXP.PLAN <- readr::read_csv(paste0(simu.path, "/support/", simu.name, "_simulation_summary.csv"), col_types = readr::cols())
    } else {
      warning("No simulation inputs summary (experimental plan) to read from simulation directory. This simulation may not have been created with hisafer.", call. = FALSE)
      EXP.PLAN <- dplyr::tibble(SimulationName = simu.name)
    }
  }

  ## Create profile paths
  if(profiles[1] == "all") profiles <- SUPPORTED.PROFILES$profiles
  file.prefix <- paste0(simu.path, "/output-", simu.name, ".sim", "/", simu.name, "_")
  files       <- paste0(file.prefix, profiles, ".txt" )

  ## Check for existence of all requested profiles and warn if profile does not exist
  if(!all(file.exists(files))) {
    missing.profiles <- files[!file.exists(files)] %>%
      strsplit("/") %>%
      purrr::map_chr(tail, n = 1)
    missing.profile.error <- paste(c("The following requested profiles do not exist:",
                                     paste0("      --", missing.profiles)),
                                   collapse = "\n")
    warning(missing.profile.error, call. = FALSE)
    profiles <- profiles[file.exists(files)]
  }

  ## Read profiles
  if(show.progress) cat("\n\nReading:", simu.name, "\nProfiles:", paste0(profiles, collapse = ", "))

  if(length(profiles) >= 1) {
    out <- purrr::map(profiles, read_profile, path = file.prefix, show.progress = show.progress, max.size = max.size)
    names(out) <- profiles
  } else {
    out <- list()
  }

  get_prof <- function(out, prof) {
    if(is.null(out[[prof]])) {
      dv <- list(data = dplyr::tibble(), variables = dplyr::tibble())
    } else {
      dv <- out[[prof]]
    }
    return(dv)
  }

  annualtree.dv <- get_prof(out, "annualtree")
  annualcrop.dv <- get_prof(out, "annualcrop")
  annualplot.dv <- get_prof(out, "annualplot")
  trees.dv      <- get_prof(out, "trees")
  plot.dv       <- get_prof(out, "plot")
  climate.dv    <- get_prof(out, "climate")
  roots.dv      <- get_prof(out, "roots")
  monthCells.dv <- get_prof(out, "monthCells")
  cells.dv      <- get_prof(out, "cells")
  voxels.dv     <- get_prof(out, "voxels")

  ## Combine variables into one tibble, remove duplicates, rename col headers in English
  vars.to.pull <- list(annualtree.dv, annualcrop.dv, annualplot.dv, trees.dv, plot.dv, climate.dv, roots.dv, monthCells.dv, cells.dv, voxels.dv)
  variables <- purrr::map(vars.to.pull, "variables") %>%
    dplyr::bind_rows() %>%
    dplyr::distinct()
  if(ncol(variables) > 0) names(variables) <- c("Subject", "SubjectId", "VariableName", "Units", "Description", "VariableClass")

  ## Read plot characteristics from .PLD file
  pld.path <- paste0(simu.path, "/", simu.name, ".pld")
  pld <- read_param_file(pld.path)
  geometryOption      <- as.numeric(pld$PLOT$geometryOption$value)
  spacingBetweenRows  <- as.numeric(pld$PLOT$spacingBetweenRows$value)
  spacingWithinRows   <- as.numeric(pld$PLOT$spacingWithinRows$value)
  plotWidth           <- as.numeric(pld$PLOT$plotWidth$value)
  plotHeight          <- as.numeric(pld$PLOT$plotHeight$value)
  treeLineOrientation <- as.numeric(pld$PLOT$treeLineOrientation$value)
  cellWidth           <- as.numeric(pld$PLOT$cellWidth$value)
  soil.depth          <- sum(pld$LAYERS$layers$value[[1]]$thick)

  plot.area <- ifelse(geometryOption == 1, spacingBetweenRows * spacingWithinRows, plotWidth * plotHeight)
  plot.info <- dplyr::tibble(SimulationName      = simu.name,
                             plot.area           = plot.area,
                             treeLineOrientation = treeLineOrientation,
                             cellWidth           = cellWidth,
                             soilDepth           = soil.depth)

  ## Ensure crop names are characters in annual plot and plot
  clean_crop_name <- function(x) {
    if("mainCropName" %in% names(x)) {
      x$mainCropName <- as.character(x$mainCropName)
      x$mainCropName[x$mainCropName == "0"]   <- NA
    }
    if("interCropName" %in% names(x)) {
      x$interCropName <- as.character(x$interCropName)
      x$interCropName[x$interCropName == "0"]   <- NA
    }
    return(x)
  }

  ## Creatd output list & assign class
  output <- list(annualtree = dplyr::distinct(annualtree.dv$data),
                 annualcrop = dplyr::distinct(annualcrop.dv$data),
                 annualplot = dplyr::distinct(clean_crop_name(annualplot.dv$data)),
                 trees      = dplyr::distinct(trees.dv$data),
                 plot       = dplyr::distinct(clean_crop_name(plot.dv$data)),
                 climate    = dplyr::distinct(climate.dv$data),
                 monthCells = dplyr::distinct(monthCells.dv$data),
                 cells      = dplyr::distinct(cells.dv$data),
                 roots      = dplyr::distinct(roots.dv$data),
                 voxels     = dplyr::distinct(voxels.dv$data),
                 variables  = variables,
                 plot.info  = plot.info,
                 tree.info  = read_tree_info(path, simu.name),
                 exp.plan   = EXP.PLAN,
                 path       = dplyr::tibble(SimulationName = simu.name, path = simu.path))

  return(output)
}

#' Read example Hi-sAFe experiment output
#' @description Reads in an example Hi-sAFe experiment. For more details see \code{\link{read_hisafe}}.
#' @return An object of class "hop".
#' @param profiles A character vector of the names of Hi-sAFe output profiles to read.
#' "cells", "roots" and "voxels" profiles are not available in the example.
#' @param show.progress Logical indicating whether progress messsages should be printed to the console.
#' @export
read_hisafe_example <- function(profiles      = c("annualplot", "annualtree", "annualcrop", "plot", "trees", "climate", "monthCells"),
                                show.progress = TRUE) {

  if(!all(is.character(profiles))) stop("profiles argument must be a character vector", call. = FALSE)
  if(!is.logical(show.progress))   stop("show.progress argument must be a logical",     call. = FALSE)

  hop <- read_hisafe(path          = clean_path(paste0(system.file("extdata", "example_output", package = "hisafer"), "/")),
                     profiles      = profiles,
                     show.progress = show.progress)
  return(hop)
}

#' Read a single Hi-sAFe output profile
#' @description Reads the designated output profiles from a single Hi-sAFe simulation.
#' @return An list of two data frames (tibbles): \code{data} contains the data from the profile; \code{variables} contains the variable descriptions.
#' @param profile A character string of the path to the profile to be read.
#' @importFrom dplyr %>%
read_hisafe_output_file <- function(profile){

  ## Read raw text & find break between description & data
  raw.text <- readLines(profile)
  end.of.var.list <- which(raw.text[-1] == "")[1]

  ## Read variable descriptions
  variables <- read_table_hisafe(profile, skip = 7, nrows = end.of.var.list - 8) # always 7 header rows at start of each profile

  ## Read data
  dat <- read_table_hisafe(profile, skip = end.of.var.list) %>%
    dplyr::filter(Year != 0)
  if(nrow(dat) == 0) {
    profile.name <- tail(strsplit(profile, "/")[[1]], n = 1)
    warning(paste0(profile.name, " exists but contains no data"), call. = FALSE)
  } else {
    dat <- dat %>%                                               # remove row with Year==0 at the start of every output profile
      dplyr::mutate(Date = gsub(pattern = "a.", replacement = "", x = Date, fixed = TRUE)) %>% # is this here to fix an old bug?
      dplyr::mutate(Date = lubridate::dmy(Date))                 # convert Date column into date class
  }

  return(list(data = dat, variables = variables))
}

#' Read from a Hi-sAFe output profile
#' @description Customized read.table call to read from a Hi-sAFe output profile.
#' @return A data frame.
#' @param file A character string of the path to the file to be read.
#' @param ... Any other arguements passed to \code{read.table}
read_table_hisafe <- function(file, ...) {
  # using read.table rather readr::read_table because read_table is not working
  dplyr::as_tibble(read.table(file,
                              header           = TRUE,
                              sep              = "\t",
                              stringsAsFactors = FALSE,
                              na.strings       = c("NA", "error!", "NaN"), # "error!" is output by HISAFE & causes table merge errors if left; "NaN" output causes plot problems
                              encoding         = "latin1", ...))
}

#' Read a Hi-sAFe output profile
#' @description Wrapper for read_hisafe_output_file.
#' @return A list containing the profile data and the profile variable definitions.
#' @param profile A character string of the profile name.
#' @param path A character string of the path to the folder containing the profiles.
#' @param show.progress Logical indicating whether progress messsages should be printed to the console.
read_profile <- function(profile, path, show.progress = TRUE, max.size = 3e8) {
  file <- paste0(path, profile, ".txt")
  if(show.progress) cat(paste0("\n   -- reading:  ", profile, collapse = ", "))
  if(file.info(file)$size < max.size) {
    profile.list <- read_hisafe_output_file(file)
    profile.data <- profile.list$data %>%
      dplyr::mutate_if(is.logical, as.numeric)   # columns read as logical must be coered to numeric to prevent plotting errors
    profile.variables <- profile.list$variables %>% dplyr::mutate(VariableClass = profile)
  } else {
    warning(paste(profile, "profile too large (> max.size) to read"), call. = FALSE)
    profile.data <- profile.variables <- dplyr::tibble()
  }
  return(list(data = profile.data, variables = profile.variables))
}

#' Read tree information from a Hi-sAFe pld file
#' @description Reads tree information from a Hi-sAFe pld file. Used by \code{\link{read_simulation}}.
#' @return A data frame (tibble) containing tree id, species, x and y.
#' @param path A character string of the path to the directory containing the simulation folder.
#' @param simu.name A character string of the simualation name.
#' @importFrom dplyr %>%
read_tree_info <- function(path, simu.name) {
  pld <- read_param_file(clean_path(paste0(path, "/", simu.name, "/", simu.name, ".pld")))
  if(pld$TREE_INITIALIZATION$tree.initialization$commented == FALSE) {
    tree.info <- pld$TREE_INITIALIZATION$tree.initialization$value[[1]] %>%
      dplyr::mutate(x = treeX, y = treeY) %>%
      dplyr::select(species, x, y)
    tree.info <- tree.info %>%
      dplyr::mutate(id = 1:nrow(tree.info)) %>%
      dplyr::mutate(SimulationName = rep(simu.name, nrow(tree.info))) %>%
      dplyr::select(SimulationName, id, dplyr::everything())
  } else {
    tree.info <- dplyr::tibble()
  }
}
