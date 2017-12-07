#' Define a Hi-sAFe experiment
#' @description Defines a Hi-sAFe experiment - the input parameters to one or more Hi-sAFe simulations.
#' @details It is strongly recommended to name each simulation in your experiment. This can be done via the \code{SimulationName} parameter.
#' If no names are provided, then generic names of "Sim_1", "Sim_2", etc. will be generated.
#' @return An object of class "hip". This is a list of 4 elements:
#' \itemize{
#'  \item{"exp.plan"}{ - A data frame (tibble) of manipulated Hi-sAFe input parameters, with each row a Hi-sAFe simulation and each column a Hi-sAFe input parameter.}
#'  \item{"template"}{ - A character string of the path to the directory containing the template set of Hi-sAFe simulation folders/files used.}
#'  \item{"profiles"}{ - A character vector of the names of the Hi-sAFe export profiles that will be exported by Hi-sAFe.}
#'  \item{"path"}{ - A character string of the absolute path to the directory where the simulation/experiment is to be built.
#' }
#' If a relative path is via \code{path}, it is converted to an absolute path to maximize "hip" object versaitility.}
#' @param path A character string of the path (relative or absolute) to the directory where the simulation/experiment is to be built.
#' @param exp.name A character string of the name of the experiment folder. Only used if defining more than one simulation.
#' @param profiles A character vector of Hi-sAFe export profiles to be exported by Hi-sAFe.
#' If "all" (the default), then all supported profiles will be exported.
#' @param template A character string of the path to the directory containing the template set of Hi-sAFe simulation folders/files to use.
#' hisafer comes with three "default" templates than can be used by specificying specific character strings:
#' \itemize{
#'  \item{"agroforestry_default"}{ - An agroforestry template based on the calibration simulation of Hi-sAFe using the Restinclieres A2
#'  walnut-wheat agroforestry plot in Montpellier, France. Spacing is 13m between rows and 8m within rows.}
#'  \item{"forestry_default"}{ - Walnut plantation forestry at tigher tree spacing of 5m between row and 3m within rows.
#'  Crop is grass in all cells except for where the tree trunk is. Serves as a forestry control.}
#'  \item{"monocrop_default"}{ - No trees. A scene with just a single cell of wheat. Serves as a monocrop control.}
#' }
#' If "default", then the default template inluded with hisafer (i.e. the files used for Hi-sAFe calibtation) will be used.
#' @param factorial If \code{FALSE}, the default, then supplied input values are recycled (i.e. such as for default behavior of \code{\link{data.frame}}).
#' If \code{TRUE}, then a factorial experiment is created, in which an experiment is defined for each possible combination of supplied values.
#' @param force Logical indicating wether the supplied values should be forced past the constraint checks. Use \code{TRUE} for development only.
#' @param ... Any Hi-sAFe input parameter in the .sim, .pld, and .tree files can be passed.
#' To display supported parameters, use \code{\link{hisafe_params}}. See below for further details.
#' There are three methods for passing parameters to \code{define_hisafe}, one for each of the three types of parameters within the parameter files:
#' \itemize{
#'  \item{individual numeric or character values}{ - For parameters that require a single value (most parameters),
#'  a simulation can be defined via \code{parameterName = value}. To define an experiment, use \code{parameterName = c(value1, value2)}}
#'  \item{multiple numeric values}{ - For parameters that require multiple numeric values (e.g. tree pruning, tree thinning, and root pruning parameters),
#'  a simulation can be defined by wrapping one or more numeric vectors within a list.
#'  For a single simulation, use \code{parameterName = list(c(value1, value2, ...))}.
#'  For an experiment, use \code{parameterName = list(c(value1, value2, ...), c(value3, value4, ...))}.}
#'  \item{"tables"}{ - There are four parameter tables within the .pld file (layers, layer.initialization, tree.initialization, root.initialization).
#'  To define prameters within these tables, use the helper functions
#'  \code{\link{layer_params}}, \code{\link{layer_init_params}}, \code{\link{tree_init_params}}, and \code{\link{root_init_params}}.
#'  These functions create a list of data frames (tibbles).}
#' }
#' @export
#' @importFrom dplyr %>%
#' @family hisafe definition functions
#' @examples
#' \dontrun{
#' # To define a Hi-sAFe simulation using all default parameter values:
#' default.exp <- define_hisafe(path = "./")
#'
#' # To define a Hi-sAFe experiment analyzing only variation in latitude:
#' latitude.exp <- define_hisafe(path = "./", SimulationName = c("Lat15", "Lat30", "Lat45", "Lat60"),
#'                                latitude = seq(15, 60, 15))
#'
#' # To define a factorial Hi-sAFe experiment analyzing
#' # simultaneous variation in latitude and tree line orientation:
#' lat.orient.exp <- define_hisafe(path = "./",
#'                                  latitude = seq(15, 60, 15),
#'                                  treeLineOrientation = c(0,90))
#' }
define_hisafe <- function(path,
                          exp.name = "experiment",
                          profiles = "all",
                          template = "agroforestry_default",
                          factorial = FALSE,
                          force = FALSE, ...) {

  param.list <- list(...)

  ## Get profile names and check that they are present in template directory
  available.profiles <- get_available_profiles(get_template_path(template))
  if(profiles[1] == "all") {
    profiles <- available.profiles
  } else if(!all(profiles %in% available.profiles)) {
    missing.profiles      <- profiles[!(profiles %in% available.profiles)]
    stop(paste(c("The following profiles are not available:", missing.profiles), collapse = "\n"), call. = FALSE)
  }

  if(factorial) {
    exp.plan <- dplyr::as_tibble(expand.grid(param.list, stringsAsFactors = FALSE))
  } else if(length(param.list) > 0){
    lengths <- purrr::map(param.list, length)
    max.length <- max(unlist(lengths))
    reps_maker <- function(x) max.length / x
    check_integers <- function(x) x %% 1 == 0
    reps.to.make <- purrr::map(lengths, reps_maker)
    integer.check <- unlist(purrr::map(reps.to.make, check_integers))
    param.list.filled <- purrr::map2(param.list, reps.to.make, rep.int)
    exp.plan <- dplyr::as_tibble(param.list.filled)
  } else {
    exp.plan <- dplyr::tibble(SimulationName = "Sim_1")
  }

  if(!("SimulationName" %in% names(exp.plan))) exp.plan$SimulationName <- paste0("Sim_", 1:nrow(exp.plan))
  exp.plan <- dplyr::select(exp.plan, SimulationName, dplyr::everything())

  if("weatherFile" %in% names(exp.plan)) exp.plan$weatherFile <- as.character(R.utils::getAbsolutePath(exp.plan$weatherFile))

  if(nrow(exp.plan) > 1) path <- clean_path(paste0(path, "/", exp.name))
  hip <- list(exp.plan = exp.plan,
              template = template,
              profiles = profiles,
              path     = R.utils::getAbsolutePath(path))

  if(!force) {
    check_input_values(hip)
  } else {
    warning("Bypassing validation of input parameter definitions can lead to unpredictable functionality of Hi-sAFe." ,
            call. = FALSE, immediate. = TRUE)
  }

  class(hip) <- c("hip", class(hip))
  return(hip)
}

#' Define a Hi-sAFe experiment from a file
#' @description Defines a Hi-sAFe experiment - the input parameters to one or more Hi-sAFe simulations.
#' \code{define_hisafe_file} cannot handle parameters that require numeric vectors or are within parameters tables. Use \code{\link{define_hisafe}} instead.
#' @details It is strongly recommended to name each simulation in your experiment. This can be done via the \code{SimulationName} parameter.
#' If no names are provided, then generic names of "Sim_1", "Sim_2", etc. will be generated.
#' @return An object of class "hip". See \code{\link{define_hisafe}} for more details.
#' @param file A character string of the path to a csv file.
#' Each row in the file should represent a Hi-sAFe simulation and each column a Hi-sAFe input parameter.
#' For more information on supported parameters, use \code{\link{hisafe_params}}.
#' @param path A character string of the path (relative or absolute) to the directory where the simulation/experiment is to be built.
#' @param profiles A character vector of Hi-sAFe export profiles to be exported by Hi-sAFe. If "all" (the default), then all supported profiles will be exported.
#' @param template A character string of the path to the directory containing the template set of Hi-sAFe simulation folders/files to use.
#' @param force Logical indicating wether the supplied values should be forced past the constraint checks. Use \code{TRUE} for development only.
#' See \code{\link{define_hisafe}} for more details.
#' @export
#' @importFrom dplyr %>%
#' @family hisafe definition functions
#' @examples
#' \dontrun{
#' # To define a Hi-sAFe experiment from a file:
#' myexp <- define_hisafe_file("./example_exp.csv")
#' }
define_hisafe_file <- function(file, path, profiles = "all", template = "agroforestry_default", force = FALSE) {
  exp.plan <- dplyr::as_tibble(read.csv(file, header = TRUE, stringsAsFactors = FALSE))

  ## Get profile names and check that they are present in template directory
  available.profiles <- get_available_profiles(get_template_path(template))
  if(profiles[1] == "all") {
    profiles <- available.profiles
  } else if(!all(profiles %in% available.profiles)) {
    missing.profiles      <- profiles[!(profiles %in% available.profiles)]
    stop(paste(c("The following profiles are not available:", missing.profiles), collapse = "\n"), call. = FALSE)
  }

  if(!("SimulationName" %in% names(exp.plan))) exp.plan$SimulationName <- paste0("Sim_", 1:nrow(exp.plan))
  exp.plan <- dplyr::select(exp.plan, SimulationName, dplyr::everything())

  hip <- list(exp.plan = exp.plan,
              template = template,
              profiles = profiles,
              path     = R.utils::getAbsolutePath(path))

  if(!force) {
    check_input_values(hip)
  } else {
    warning("Bypassing validation of input parameter definitions can lead to unpredictable functionality of Hi-sAFe." ,
            call. = FALSE, immediate. = TRUE)
  }

  class(hip) <- c("hip", class(hip))
  return(hip)
}

#' Check validity of all Hi-sAFe inputs
#' @description Checks the validity of all defined inputs against Hi-sAFe model constraints
#' and constraints found in the template file comments. Errors are generated if issues are found.
#' Used within \code{\link{define_hisafe}} and \code{\link{define_hisafe_file}}.
#' @return Produces errors if issues are found. Otherwise, invisibly returns \code{TRUE}.
#' @param hip An object of class "hip".
check_input_values <- function(hip) {
  EXP.PLAN <- hip$exp.plan

  ## Get template params & names
  template.path   <- get_template_path(hip$template)
  TEMPLATE_PARAMS <- get_template_params(template.path)
  PARAM_NAMES     <- get_param_names(TEMPLATE_PARAMS)
  PARAM_DEFAULTS  <- get_param_vals(TEMPLATE_PARAMS, "value")
  PARAM_COMMENTED <- get_param_vals(TEMPLATE_PARAMS, "commented")
  AVAIL_CROPS     <- list.files(clean_path(paste0(template.path, "/cropSpecies")))
  AVAIL_TECS      <- list.files(clean_path(paste0(template.path, "/cropInterventions")))
  AVAIL_TREES     <- gsub("\\.tree", "", list.files(clean_path(paste0(template.path, "/treeSpecies"))))

  ## Get used parameters
  USED_PARAMS <- purrr::map(as.list(unlist(PARAM_NAMES, use.names = FALSE)),
                            get_used_param,
                            exp.plan = EXP.PLAN,
                            template.defaults = PARAM_DEFAULTS,
                            template.commented = PARAM_COMMENTED)
  names(USED_PARAMS) <- unlist(PARAM_NAMES, use.names = FALSE)

  ## Initialize Error Message
  errors <-   "Hi-sAFe definition errors:"

  ## Unsupported inputs
  names.to.check <- names(EXP.PLAN)[!(names(EXP.PLAN) %in% c("SimulationName", "weatherFile"))]
  if(any(!(names.to.check %in% unlist(PARAM_NAMES, use.names = FALSE)))) {
    unsupported.names   <- names.to.check[!(names.to.check %in% PARAM_NAMES)]
    unsupported.var.error <- c("The following variables are not supported:", paste0(unsupported.names, collapse = ", "))
  } else {
    unsupported.var.error <- ""
  }

  ## SimulationName errors
  unique.sim.error     <- ifelse(identical(EXP.PLAN, dplyr::distinct(EXP.PLAN)),
                                 "", "-- Each simulaton must be distinct.")
  unique.simname.error <- ifelse(unique(table(EXP.PLAN$SimulationName)) == 1,
                                 "", "-- SimulationName - each siulation must have a unique name")
  simname.space.error  <- ifelse(!any(grepl(" ", EXP.PLAN$SimulationName)),
                                 "", "-- SimulationName - names cannot contains spaces")

  ## Tree Errors
  if(USED_PARAMS$tree.initialization$exp.plan) {
    unique_tree_species <- function(x) unique(x$species)
    tree.species.used <- unique(unlist(purrr::map(EXP.PLAN$tree.initialization, unique_tree_species)))
    if(any(!(tree.species.used %in% AVAIL_TREES))) {
      tree.species.missing <- tree.species.used[!(tree.species.used %in% AVAIL_TREES)]
      unsupported.trees.error <- paste("--", tree.species.missing, "is not a tree available in the template directory.")
    } else {
      unsupported.trees.error <- ""
    }
    several.trees <- length(tree.species.used) > 1
    tree.params.edited <- any(names(EXP.PLAN) %in% PARAM_NAMES$tree)
    too.many.trees.error <- ifelse(several.trees & tree.params.edited,
                                   "-- Cannot edit tree paramaters when simulations contain more than one tree species.", "")
  } else {
    unsupported.trees.error <- ""
    too.many.trees.error    <- ""
  }

  ## Crop Errors
  crop.species.used <- unique(c(unlist(USED_PARAMS$mainCropSpecies$value), unlist(USED_PARAMS$interCropSpecies$value)))
  if(any(!(crop.species.used %in% AVAIL_CROPS))) {
    crop.species.missing <- crop.species.used[!(crop.species.used %in% AVAIL_CROPS)]
    unsupported.crops.error <- paste("-- The following crop files are not available in the template idrectory: ", crop.species.missing)
  } else {
    unsupported.crops.error <- ""
  }

  ## itk Errors
  crop.itks.used <- unique(c(unlist(USED_PARAMS$mainCropItk$value), unlist(USED_PARAMS$interCropItk$value)))
  if(any(!(crop.itks.used %in% AVAIL_TECS))) {
    crop.itks.missing <- crop.itks.used[!(crop.itks.used %in% AVAIL_TECS)]
    unsupported.itks.error <- paste("-- The following crop files are not available in the template idrectory: ", crop.itks.missing)
  } else {
    unsupported.itks.error <- ""
  }

  ## Spacing Errors
  btwn.tree.error   <- ifelse(all(((USED_PARAMS$spacingBetweenRows$value / USED_PARAMS$cellWidth$value) %% 1) == 0),
                              "", "-- (spacingBetweenRows / cellWidth) should be a whole number")
  within.tree.error <- ifelse(all(((USED_PARAMS$spacingWithinRows$value  / USED_PARAMS$cellWidth$value) %% 1) == 0),
                              "", "-- (spacingWithinRows / cellWidth) should be a whole number")
  # btwn.tree.odd.error      <- ifelse(all(((USED_PARAMS$spacingBetweenRows$value / USED_PARAMS$cellWidth$value) %% 2) == 1), "",
  #                                "-- Tree should be centered in a cell. (spacingBetweenRows / cellWidth) should be an odd integer")
  # within.tree.odd.error    <- ifelse(all(((USED_PARAMS$spacingWithinRows$value  / USED_PARAMS$cellWidth$value) %% 2) == 1), "",
  #                                "-- Tree should be centered in a cell. (spacingWithinRows / cellWidth) should be an odd integer")

  ## Distance Errors
  treeCropDistance.error        <- ifelse(all(USED_PARAMS$treeCropDistance$value <= USED_PARAMS$spacingBetweenRows$value),
                                          "", "-- treeCropDistance must be less than spacingBetweenRows")
  treeRootPruningDistance.error <- ifelse(all(USED_PARAMS$treeRootPruningDistance$value <= USED_PARAMS$spacingBetweenRows$value),
                                          "", "-- treeRootPruningDistance must be less than spacingBetweenRows")

  ## Time Errors
  less_than_nbSimulations <- function(x,y) all(x <= y)
  treePruningYears.error     <- ifelse(all(purrr::map2_lgl(USED_PARAMS$treePruningYears$value,
                                                           as.list(USED_PARAMS$nbSimulations$value),
                                                           less_than_nbSimulations)),
                                       "", "-- treePruningYears must be less than nbSimulations")
  treeThinningYears.error    <- ifelse(all(purrr::map2_lgl(USED_PARAMS$treeThinningYears$value,
                                                           as.list(USED_PARAMS$nbSimulations$value),
                                                           less_than_nbSimulations)),
                                       "", "-- treeThinningYears must be less than nbSimulations")
  treeRootPruningYears.error <- ifelse(all(purrr::map2_lgl(USED_PARAMS$treeRootPruningYears$value,
                                                           as.list(USED_PARAMS$nbSimulations$value),
                                                           less_than_nbSimulations)),
                                       "", "-- treeRootPruningYears must be less than nbSimulations")

  ## nrow(tree_init) == nrow(root_init) Error
  tree.init <- USED_PARAMS$tree.initialization$value
  if(all(is.na(tree.init))) {
    tree.rows <- rep(0, length(tree.init))
  } else {
    if(!("list" %in% class(tree.init))) tree.init <- list(tree.init)
    tree.rows <- purrr::map_dbl(tree.init, nrow)
  }

  root.init <- USED_PARAMS$root.initialization$value
  if(all(is.na(root.init))) {
    root.rows <- 0
  } else {
    if(!("list" %in% class(root.init))) root.init <- list(root.init)
    root.rows <- purrr::map_dbl(root.init, nrow)
  }

  tree.root.error <- ifelse(all(tree.rows == root.rows),
                            "", "-- number of rows in the tree initialization and root initialization tables must be equal")

  ## Don't Edit Export Profile Errors
  EP.error <- ifelse((USED_PARAMS$profileNames$exp.plan) | (USED_PARAMS$exportFrequencies$exp.plan),
                     "-- profileNames and exportFrequencies cannot be defined using define_hisafe(). Use the 'profiles' argument of build_hisafe().", "")

  ## Don't Edit nbTrees Error
  nbTrees.error <- ifelse(USED_PARAMS$nbTrees$exp.plan,
                          "-- nbTrees cannot be defined directly using define_hisafe(). Instead the size of the tree initialziation table will be used.", "")

  ## Timeseries Length Errors
  treePruningYears.length     <- unique(purrr::map_dbl(USED_PARAMS$treePruningYears$value,     length))
  treePruningProp.length      <- unique(purrr::map_dbl(USED_PARAMS$treePruningProp$value,      length))
  treePruningMaxHeight.length <- unique(purrr::map_dbl(USED_PARAMS$treePruningMaxHeight$value, length))
  treePruningDays.length      <- unique(purrr::map_dbl(USED_PARAMS$treePruningDays$value,      length))

  treePruning.length.error <- ifelse(all(purrr::map_lgl(list(treePruningProp.length,
                                                             treePruningMaxHeight.length,
                                                             treePruningDays.length), identical, y = treePruningYears.length)),
                                     "", "-- treePruningYears, treePruningProp, treePruningMaxHeight, and treePruningDays must have the same length")

  treeThinningIds.length   <- unique(purrr::map_dbl(USED_PARAMS$treeThinningIds$value,   length))
  treeThinningYears.length <- unique(purrr::map_dbl(USED_PARAMS$treeThinningYears$value, length))
  treeThinningDays.length  <- unique(purrr::map_dbl(USED_PARAMS$treeThinningDays$value,  length))
  treeThinning.length.error <- ifelse(all(purrr::map_lgl(list(treeThinningYears.length, treeThinningDays.length), identical, y = treeThinningIds.length)),
                                      "", "-- treeThinningIds, treeThinningYears, and treeThinningDays must have the same length")

  treeRootPruningYears.length    <- unique(purrr::map_dbl(USED_PARAMS$treeRootPruningYears$value,    length))
  treeRootPruningDays.length     <- unique(purrr::map_dbl(USED_PARAMS$treeRootPruningDays$value,     length))
  treeRootPruningDistance.length <- unique(purrr::map_dbl(USED_PARAMS$treeRootPruningDistance$value, length))
  treeRootPruningDepth.length    <- unique(purrr::map_dbl(USED_PARAMS$treeRootPruningDepth$value,    length))
  rootPruning.length.error <- ifelse(all(purrr::map_lgl(list(treeRootPruningDays.length,
                                                             treeRootPruningDistance.length,
                                                             treeRootPruningDepth.length), identical, y = treeRootPruningYears.length)), "",
                                     "-- treeRootPruningYears, treeRootPruningDays, treeRootPruningDistance, and treeRootPruningDepth must have the same length")

  ## Geometry Option Errors
  if(1 %in% USED_PARAMS$geometryOption$value & (USED_PARAMS$plotHeight$exp.plan | USED_PARAMS$plotWidth$exp.plan)) {
    warning("-- when geometryOption = 1, plotHeight and plotWidth are not used." , call. = FALSE, immediate. = TRUE)
  }
  if(3 %in% USED_PARAMS$geometryOption$value & (USED_PARAMS$spacingBetweenRows$exp.plan | USED_PARAMS$spacingWithinRows$exp.plan)) {
    warning("-- when geometryOption = 3, spacingBetweenRows and spacingWithinRows are not used." , call. = FALSE, immediate. = TRUE)
  }
  nbtree.error <- ifelse(any(USED_PARAMS$geometryOption$value == 1 & !(tree.rows %in% c(1,4,9))),
                         "-- when geompetryOption = 1, the number of trees can only be 1, 4, or 9", "")

  get_n_species <- function(x) length(unique(x$species))
  if(all(is.na(tree.init))) {
    n.tree.species <- rep(0, length(tree.init))
  } else {
    n.tree.species <- purrr::map_dbl(tree.init, get_n_species)
  }
  species.error <- ifelse(any(USED_PARAMS$geometryOption$value == 1 & n.tree.species > 1),
                          "-- when geompetryOption = 1, there can only be one tree species", "")

  weed.error <- ifelse(any(USED_PARAMS$treeCropDistance$value > 0 & USED_PARAMS$weededAreaRadius$value > 0),
                       "-- treeCropDistance and weededAreaRadius cannont both be greater than 0", "")

  ## Root Init Errors
  get_init_vals <- function(x, param) {
    val <- NULL
    if("list" %in% class(x)) {
      for(i in 1:length(x)){
        val <- c(val, x[[i]][[param]])
      }
    } else if("tbl" %in% class(x)) {
      val <- x[[param]]
    } else {
      val <- NA
    }
    return(val)
  }
  root.init.diam.error <- ifelse(any(get_init_vals(USED_PARAMS$root.initialization$value,
                                                   "paramShape1") < 0.75 * unlist(purrr::map2(USED_PARAMS$cellWidth$value, root.rows, rep))),
                                 "-- paramShape1 of root initialization table cannot be smaller than 0.75 * cellWidth", "")
  ## All weatherFile files exist
  if("weatherFile" %in% names(EXP.PLAN)) {
    if(all(file.exists(EXP.PLAN$weatherFile))) {
      wth.error <- ""
    } else {
      missing.wth.files <- EXP.PLAN$weatherFile[!file.exists(EXP.PLAN$weatherFile)]
      wth.error <- paste("-- the following .WTH files do not exist:", paste(missing.wth.files, collapse = ", "))
    }
  } else {
    wth.error <- ""
  }

  ## Accepted & Range Errors
  accepted.errors <- purrr::map_chr(names.to.check, check_accepted, exp.plan = EXP.PLAN, template = TEMPLATE_PARAMS)
  range.errors    <- purrr::map_chr(names.to.check, check_range,    exp.plan = EXP.PLAN, template = TEMPLATE_PARAMS)
  type.errors     <- purrr::map_chr(names.to.check, check_type,     exp.plan = EXP.PLAN, template = TEMPLATE_PARAMS)

  all.errors <- c(errors, unsupported.var.error,
                  unique.sim.error, unique.simname.error, simname.space.error,
                  unsupported.trees.error, too.many.trees.error,
                  unsupported.crops.error, unsupported.itks.error,
                  btwn.tree.error, within.tree.error,
                  treeCropDistance.error, treeRootPruningDistance.error,
                  treePruningYears.error, treeThinningYears.error, treeRootPruningYears.error,
                  tree.root.error, EP.error, nbTrees.error,
                  treePruning.length.error, treeThinning.length.error, rootPruning.length.error,
                  nbtree.error, species.error, weed.error,
                  root.init.diam.error, wth.error,
                  accepted.errors, range.errors, type.errors)

  all.errors <- paste0(all.errors[!(all.errors == "") & !is.na(all.errors)], collapse = "\n")

  if(all.errors != errors) stop(all.errors, call. = FALSE)

  invisible(TRUE)
}

#' Check validity of Hi-sAFe accepted values
#' @description Checks validity of Hi-sAFe inputs accepted values found in the comments of template files.
#' Used within \code{\link{check_input_values}}.
#' @return An error message or empty character stirng.
#' @param variable A character string of the name of the variable to check.
#' @param exp.plan The exp.plan of a "hip" object.
#' @param template A list of template parameters and constraints
check_accepted <- function(variable, exp.plan, template) {
  accepted.vals <- get_param_vals(template, "accepted")[[variable]]
  accepted.pass <- (all(is.na(accepted.vals)) | all(as.character(exp.plan[[variable]]) %in% accepted.vals))
  if(accepted.pass) {
    return("")
  } else {
    return(paste0("-- ", variable, " - must be one of: ", paste0(accepted.vals, collapse = ", ")))
  }
}

#' Check validity of Hi-sAFe input ranges
#' @description Checks validity of Hi-sAFe inputs against accepted ranges found in the comments of template files.
#' Used within \code{\link{check_input_values}}.
#' @return An error message or empty character stirng.
#' @param variable A character string of the name of the variable to check.
#' @param exp.plan The exp.plan of a "hip" object.
#' @param template A list of template parameters and constraints
check_range <- function(variable, exp.plan, template) {
  if(!is.numeric(exp.plan[[variable]])) return("")
  min.val  <- get_param_vals(template, "range")[[variable]][1]
  max.val  <- get_param_vals(template, "range")[[variable]][2]
  max.pass <- (is.na(max.val) | all(exp.plan[[variable]] <= max.val))
  min.pass <- (is.na(min.val) | all(exp.plan[[variable]] >= min.val))
  if(max.pass & min.pass) {
    return("")
  } else if(!is.na(max.val) & !is.na(min.val)) {
    return(paste0("-- ", variable, " - must be betwen ", min.val, " and ", max.val))
  } else if(is.na(max.val) & !is.na(min.val)) {
    return(paste0("-- ", variable, " - must be greater than ", min.val))
  } else if(!is.na(max.val) & is.na(min.val)) {
    return(paste0("-- ", variable, " - must be less than ", max.val))
  }
}

#' Check validity of Hi-sAFe input numeric types
#' @description Checks validity of Hi-sAFe inputs against accepted numeric types (continuous, integer) found in the comments of template files.
#' Used within \code{\link{check_input_values}}.
#' @return An error message or empty character stirng.
#' @param variable A character string of the name of the variable to check.
#' @param exp.plan The exp.plan of a "hip" object.
#' @param template A list of template parameters and constraints
check_type <- function(variable, exp.plan, template) {
  if(!is.numeric(exp.plan[[variable]])) return("")
  type  <- get_param_vals(template, "type")[[variable]]
  if(is.na(type)) return("")
  if(type == "integer"){
    if(all(exp.plan[[variable]] %% 1 == 0)) {
      return("")
    } else {
      return(paste0("-- ", variable, " - must be an integer"))
    }
  } else {
    return("")
  }
}

#' Generate root initialization table for define_hisafe
#' @description Generates a root initialization table suitable for passing to \code{\link{define_hisafe}}.
#' @return A list containing one or more data frames (tibbles).
#' @param reps Number of times to repeats the rows of the defined table.
#' @param shape Parameter of Hi-sAFe root initialization table.
#' @param repartition Parameter of Hi-sAFe root initialization table.
#' @param paramShape1 Parameter of Hi-sAFe root initialization table.
#' @param paramShape2 Parameter of Hi-sAFe root initialization table.
#' @param paramShape3 Parameter of Hi-sAFe root initialization table.
#' @param amount Parameter of Hi-sAFe root initialization table.
#' @export
#' @family hisafe definition functions
#' @examples
#' \dontrun{
#' root.init <- root_init_params()
#' }
root_init_params <- function(reps        = 1,
                             shape       = 1,
                             repartition = 3,
                             paramShape1 = 0.75,
                             paramShape2 = 0,
                             paramShape3 = 0,
                             amount      = 0.5) {
  temp <- dplyr::as_tibble(data.frame(name        = "RootInit",
                                      shape       = shape,
                                      repartition = repartition,
                                      paramShape1 = paramShape1,
                                      paramShape2 = paramShape2,
                                      paramShape3 = paramShape3,
                                      amount      = amount))
  out <- list(temp)
  if(reps > 1) {
    for(i in 2:reps) {
      out[[i]] <- temp
      out <- list(purrr::map_df(out, dplyr::bind_rows))
    }
  }
  return(out)
}

#' Generate tree initialization table for define_hisafe
#' @description Generates a tree initialization table suitable for passing to \code{\link{define_hisafe}}.
#' @return A list containing one or more data frames (tibbles).
#' @param species Parameter of Hi-sAFe tree initialization table.
#' @param age Parameter of Hi-sAFe tree initialization table.
#' @param height Parameter of Hi-sAFe tree initialization table.
#' @param crownBaseHeight Parameter of Hi-sAFe tree initialization table.
#' @param truncatureRatio Parameter of Hi-sAFe tree initialization table.
#' @param leafToFineRootsRatio Parameter of Hi-sAFe tree initialization table.
#' @param crownRadius Parameter of Hi-sAFe tree initialization table.
#' @param treeX Parameter of Hi-sAFe tree initialization table.
#' @param treeY Parameter of Hi-sAFe tree initialization table.
#' @export
#' @family hisafe definition functions
#' @examples
#' \dontrun{
#' tree.init <- tree_init_params()
#' }
tree_init_params <- function(species               = "walnut-hybrid",
                             age                   = 1,
                             height                = 1,
                             crownBaseHeight       = 0.5,
                             truncatureRatio       = 0,
                             leafToFineRootsRatio  = 0.5,
                             crownRadius           = 0.25,
                             treeX                 = 0,
                             treeY                 = 0) {
  out <- dplyr::as_tibble(data.frame(name                  = "TreeInit",
                                     species               = species,
                                     age                   = age,
                                     height                = height,
                                     crownBaseHeight       = crownBaseHeight,
                                     truncatureRatio       = truncatureRatio,
                                     leafToFineRootsRatio  = leafToFineRootsRatio,
                                     crownRadius           = crownRadius,
                                     treeX                 = treeX,
                                     treeY                 = treeY))
  return(list(out))
}

#' Generate soil layer initialization table for define_hisafe
#' @description Generates a soil layer initialization table suitable for passing to \code{\link{define_hisafe}}.
#' @return A list containing one or more data frames (tibbles).
#' @param waterContent Parameter of Hi-sAFe soil layer initialization table.
#' @param no3Concentration Parameter of Hi-sAFe soil layer initialization table.
#' @param nh4concentration Parameter of Hi-sAFe soil layer initialization table.
#' @export
#' @family hisafe definition functions
#' @examples
#' \dontrun{
#' layer.init <- layer_init_params()
#' }
layer_init_params <- function(waterContent     = c(0.2, 0.3, 0.3, 0.3, 0.3),
                              no3Concentration = c(30, 14, 5, 2, 0),
                              nh4concentration = 0) {
  out <- dplyr::as_tibble(data.frame(name             = "LayerInit",
                                     waterContent     = waterContent,
                                     no3Concentration = no3Concentration,
                                     nh4concentration = nh4concentration))
  return(list(out))
}

#' Generate soil layer table for define_hisafe
#' @description Generates a soil layer table suitable for passing to \code{\link{define_hisafe}}.
#' @return A list containing one or more data frames (tibbles).
#' @param thick Parameter of Hi-sAFe soil layer table.
#' @param sand Parameter of Hi-sAFe soil layer table.
#' @param clay Parameter of Hi-sAFe soil layer table.
#' @param limeStone Parameter of Hi-sAFe soil layer table.
#' @param organicMatter Parameter of Hi-sAFe soil layer table.
#' @param partSizeSand Parameter of Hi-sAFe soil layer table.
#' @param stone Parameter of Hi-sAFe soil layer table.
#' @param stoneType Parameter of Hi-sAFe soil layer table.
#' @param infiltrability Parameter of Hi-sAFe soil layer table.
#' @export
#' @family hisafe definition functions
#' @examples
#' \dontrun{
#' layers <- layer_params()
#' }
layer_params <- function(thick          = c(0.4, 0.4, 0.6, 1, 7),
                         sand           = c(20.35, 16.80, 11.83, 18.76, 6.5),
                         clay           = c(20.15, 24.95, 25.97, 24.70, 32.35),
                         limeStone      = c(45.7, 49.1, 47.1, 50.4, 52.1),
                         organicMatter  = c(2.42, 1.52, 1.86, 1.67, 3.25),
                         partSizeSand   = 290,
                         stone          = 0,
                         stoneType      = 6,
                         infiltrability = 50) {
  out <- dplyr::as_tibble(data.frame(name           = "Layer",
                                     thick          = thick,
                                     sand           = sand,
                                     clay           = clay,
                                     limeStone      = limeStone,
                                     organicMatter  = organicMatter,
                                     partSizeSand   = partSizeSand,
                                     stone          = stone,
                                     stoneType      = stoneType,
                                     infiltrability = infiltrability))
  return(list(out))
}
