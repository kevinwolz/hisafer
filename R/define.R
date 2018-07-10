#' Define a Hi-sAFe experiment
#' @description Defines a Hi-sAFe experiment - the input parameters to one or more Hi-sAFe simulations.
#' @details It is strongly recommended to name each simulation in your experiment. This can be done via the \code{SimulationName} parameter.
#' If no names are provided, then generic names of "Sim_1", "Sim_2", etc. will be generated.
#' The only additional input parameter that is available but not part of the input files is **weatherFile**, which specifies a path to a .WTH file to use.
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
#'  \item{"agroforestry"}{ - a simple alley cropping template, with trees spaced at 9m x 13m and a durum wheat alley crop}
#'  \item{"forestry"}{ - a simple forestry template, with trees spaced at 5m x 7m and an understory of bare soil}
#'  \item{"monocrop"}{ - a simple, single-celled monocrop template with durum-wheat}
#'  \item{"restinclieres_agroforestry_A2"}{ - a template based on the agroforestry calibration simulation of Hi-sAFe in Plot A2 at Restinclieres in Southern France}
#'  \item{"restinclieres_agroforestry_A3"}{ - a template based on the agroforestry calibration simulation of Hi-sAFe in Plot A3 at Restinclieres in Southern France}
#'  \item{"restinclieres_forestry_A4"}{ - a template based on the forestry calibration simulation of Hi-sAFe in Plot A4 at Restinclieres in Southern France}
#'  \item{"restinclieres_monocrop_A2"}{ - a template basd on the monocrop calibration simulation of Hi-sAFe in Plot A2 at Restinclieres in Southern France}
#'  \item{"restinclieres_monocrop_A3"}{ - a template basd on the monocrop calibration simulation of Hi-sAFe in Plot A3 at Restinclieres in Southern France}
#'  \item{"castries_agroforestry"}{ - a template basd on the agroforestry validation simulation of Hi-sAFe at Castries in Southern France}
#' }
#' @param factorial If \code{FALSE}, the default, then supplied input values are recycled (i.e. such as for default behavior of \code{\link{data.frame}}).
#' If \code{TRUE}, then a factorial experiment is created, in which an experiment is defined for each possible combination of supplied values.
#' @param force Logical indicating wether the supplied values should be forced past the constraint checks. Use \code{TRUE} for development only.
#' @param bulk.pass Any Hi-sAFe input parameter in the .SIM, .PLD, .TREE, and .PAR files can be passed here grouped as a list,
#' just as can be passed to \code{...}. This facilitates sending the same list of arguments to multiple calls of \code{define_hisafe}.
#' @param ... Any Hi-sAFe input parameter in the .SIM, .PLD, .TREE, .PLT, .TEC, and .PAR files can be passed.
#' Parameters in .TREE, .PLT, and .TEC files will be applied to **all** .TREE, .PLT, and .TEC files in each simulation.
#' To display supported parameters, use \code{\link{hip_params}}. See below for further details.
#' There are three methods for passing parameters to \code{define_hisafe}, one for each of the three types of parameters within the parameter files:
#' \itemize{
#'  \item{individual numeric or character values}{ - For parameters that require a single value (most parameters),
#'  a simulation can be defined via \code{parameterName = value}. To define an experiment, use \code{parameterName = c(value1, value2)}}
#'  \item{multiple numeric values}{ - For parameters that require multiple numeric values
#'  (e.g. tree planting, tree pruning, tree thinning, and root pruning parameters),
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
                          exp.name  = "experiment",
                          profiles  = "all",
                          template  = "agroforestry",
                          factorial = FALSE,
                          force     = FALSE,
                          bulk.pass = NULL, ...) {

  if(!(is.character(exp.name) & length(exp.name) == 1))     stop("exp.name argument must be a character vector of length 1", call. = FALSE)
  if(!(all(is.character(profiles)) | profiles[1] == "all")) stop("profiles argument must be 'all' or a character vector",    call. = FALSE)
  if(!(is.character(template) & length(template) == 1))     stop("template argument must be a character vector of length 1", call. = FALSE)
  if(!(is.null(bulk.pass) | is.list(bulk.pass)))            stop("bulk.pass argument must be a list",                        call. = FALSE)
  is_TF(factorial)
  is_TF(force)

  path          <- R.utils::getAbsolutePath(path)
  param.list    <- list(...)
  if(!is.null(bulk.pass)) param.list <- c(param.list, bulk.pass)

  ## Get profile names and check that they are present in template directory
  available.profiles <- get_available_profiles(template)
  if(profiles[1] == "all") {
    profiles <- available.profiles[!(available.profiles %in% PRIVATE.PROFILES)]
  } else if(!all(profiles %in% available.profiles)) {
    missing.profiles <- profiles[!(profiles %in% available.profiles)]
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
              path     = path)

  check_input_values(hip = hip, force = force)

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
#' For more information on supported parameters, use \code{\link{hip_params}}.
#' @param path A character string of the path (relative or absolute) to the directory where the simulation/experiment is to be built.
#' @param exp.name A character string of the name of the experiment folder. Only used if defining more than one simulation.
#' @param profiles A character vector of Hi-sAFe export profiles to be exported by Hi-sAFe. If "all" (the default), then all supported profiles will be exported.
#' @param template A character string of the path to the directory containing the template set of Hi-sAFe simulation folders/files to use.
#' See \code{\link{define_hisafe}} for more details.
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
define_hisafe_file <- function(file,
                               path,
                               exp.name = "experiment",
                               profiles = "all",
                               template = "agroforestry",
                               force    = FALSE) {

  path          <- R.utils::getAbsolutePath(path)
  template.path <- get_template_path(template)

  if(!(is.character(file) & length(file) == 1))             stop("file argument must be a character vector of length 1",     call. = FALSE)
  if(!dir.exists(path))                                     stop("directory specified by path does not exist",               call. = FALSE)
  if(!(is.character(exp.name) & length(exp.name) == 1))     stop("exp.name argument must be a character vector of length 1", call. = FALSE)
  if(!(all(is.character(profiles)) | profiles[1] == "all")) stop("profiles argument must be 'all' or a character vector",    call. = FALSE)
  if(!(is.character(template) & length(template) == 1))     stop("template argument must be a character vector of length 1", call. = FALSE)
  if(!dir.exists(template.path))                            stop("template directory does not exist",                        call. = FALSE)
  is_TF(force)

  exp.plan <- dplyr::as_tibble(read.csv(file, header = TRUE, stringsAsFactors = FALSE))

  ## Get profile names and check that they are present in template directory
  available.profiles <- get_available_profiles(template)
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
              path     = clean_path(paste0(path, "/", exp.name)))

  check_input_values(hip = hip, force = force)

  class(hip) <- c("hip", class(hip))
  return(hip)
}

#' Check validity of all Hi-sAFe inputs
#' @description Checks the validity of all defined inputs against Hi-sAFe model constraints
#' and constraints found in the template file comments. Errors are generated if issues are found.
#' Used within \code{\link{define_hisafe}} and \code{\link{define_hisafe_file}}.
#' @return Produces errors if issues are found. Otherwise, invisibly returns \code{TRUE}.
#' @param hip An object of class "hip".
#' @param force Logical indicating wether the supplied values should be forced past the constraint checks. Use \code{TRUE} for development only.
#' @keywords internal
check_input_values <- function(hip, force) {

  if(force) {
    warning("Bypassing validation of input parameter definitions can lead to unpredictable functionality of Hi-sAFe.",
            call. = FALSE, immediate. = TRUE)
    return(FALSE)
  }

  USED.PARAMS <- get_used_params(hip)

  get_used       <- function(param) USED.PARAMS[[param]]$value
  get_used_un    <- function(param) unlist(get_used(param))
  is_mod         <- function(param) USED.PARAMS[[param]]$exp.plan
  rm.na          <- function(x) x[!is.na(x)]
  get_length     <- function(param) purrr::map(get_used(param), function(x) if(is.na(x[1])) as.integer(0) else length(x))
  get_init_vals  <- function(tab, param) {
    tab <- get_used(tab)
    out.list <- list()
    for(i in 1:length(tab)) {
      if(all(is.na(tab[[i]]))) {
        out.list <- c(out.list, list(NA))
      } else {
        out.list <- c(out.list, list(tab[[i]][[param]]))
      }
    }
    return(out.list)
  }
  less_than      <- function(x, y) all(is.na(x) | x <= y)

  ## Get available template file names
  avail.path  <- get_template_subpath(hip$template)
  AVAIL.CROPS <- list.files(clean_path(paste0(avail.path, "/cropSpecies")))
  AVAIL.TECS  <- list.files(clean_path(paste0(avail.path, "/cropInterventions")))
  AVAIL.TREES <- gsub("\\.tree", "", list.files(clean_path(paste0(avail.path, "/treeSpecies"))))

  ## Initialize Error Message
  errors <-   "Hi-sAFe definition errors:"

  ## UNSUPPORTED INPUT, BAD CLASS, BAD RANGE ERRORS FIRST
  names.to.check <- names(hip$exp.plan)[!(names(hip$exp.plan) %in% c("SimulationName", "weatherFile"))]
  if(any(!(names.to.check %in% names(USED.PARAMS)))) {
    unsupported.names   <- names.to.check[!(names.to.check %in% names(USED.PARAMS))]
    unsupported.var.error <- c("The following variables are not supported:", paste0(unsupported.names, collapse = ", "))
  } else {
    unsupported.var.error <- ""
  }

  accepted.errors <- purrr::map_chr(names.to.check, check_accepted, exp.plan = hip$exp.plan)
  range.errors    <- purrr::map_chr(names.to.check, check_range,    exp.plan = hip$exp.plan)
  type.errors     <- purrr::map_chr(names.to.check, check_type,     exp.plan = hip$exp.plan)

  prelim.errors <- c(errors, unsupported.var.error, accepted.errors, range.errors, type.errors)
  prelim.errors <- paste0(prelim.errors[!(prelim.errors == "") & !is.na(prelim.errors)], collapse = "\n")
  if(prelim.errors != errors) stop(prelim.errors, call. = FALSE)

  ## SimulationName errors
  paste_together  <- function(x) unlist(purrr::map(x, paste, collapse = ";"))
  orig.exp.plan <- hip$exp.plan %>%
    dplyr::mutate_if(is.list, paste_together)
  test.exp.plan <- orig.exp.plan %>%
    dplyr::distinct()

  unique.sim.error     <- ifelse(identical(orig.exp.plan, test.exp.plan),
                                 "", "-- Each simulaton must be distinct.")
  unique.simname.error <- ifelse(unique(table(unlist(hip$exp.plan$SimulationName))) == 1,
                                 "", "-- SimulationName - each siulation must have a unique name")
  simname.space.error  <- ifelse(!any(grepl(" ", unlist(hip$exp.plan$SimulationName))),
                                 "", "-- SimulationName - names cannot contains spaces")

  ## Tree Errors
  tree.species.used <- unique(unlist(get_init_vals("tree.initialization", "species")))
  if(!is.na(tree.species.used[1]) & any(!(tree.species.used %in% AVAIL.TREES))) {
    tree.species.missing <- tree.species.used[!(tree.species.used %in% AVAIL.TREES)]
    unsupported.trees.error <- paste("--", tree.species.missing, "is not a tree available in the template directory.")
  } else {
    unsupported.trees.error <- ""
  }
  several.trees <- length(tree.species.used) > 1
  tree.params.edited <- any(names(hip$exp.plan) %in% dplyr::filter(INPUT.DEFS, file == "TREE")$name)
  too.many.trees.error <- ifelse(several.trees & tree.params.edited,
                                 "-- Cannot edit tree paramaters when simulations contain more than one tree species.", "")

  ## Crop Errors
  crop.species.used <- unique(c(get_used_un("mainCropSpecies"), get_used_un("interCropSpecies")))
  if(any(!(crop.species.used %in% AVAIL.CROPS))) {
    crop.species.missing <- crop.species.used[!(crop.species.used %in% AVAIL.CROPS)]
    unsupported.crops.error <- paste("-- The following crop .PLT files are not available in the template directory: ", paste(crop.species.missing, collapse = ", "))
  } else {
    unsupported.crops.error <- ""
  }

  ## itk Errors
  crop.itks.used <- unique(c(get_used_un("mainCropItk"), get_used_un("interCropItk")))
  if(any(!(crop.itks.used %in% AVAIL.TECS))) {
    crop.itks.missing <- crop.itks.used[!(crop.itks.used %in% AVAIL.TECS)]
    unsupported.itks.error <- paste("-- The following crop .TEC files are not available in the template directory: ", paste(crop.itks.missing, collapse = ", "))
  } else {
    unsupported.itks.error <- ""
  }

  ## Spacing Errors
  plot.width.error   <- ifelse(!all(((get_used_un("plotWidth") / get_used_un("cellWidth")) %% 1) != 0),
                               "", "-- (plotWidth / cellWidth) should be a whole number")
  plot.height.error <- ifelse(!all(((get_used_un("plotHeight")  / get_used_un("cellWidth")) %% 1) != 0),
                              "", "-- (plotHeight / cellWidth) should be a whole number")

  ## Tree Centered in Cell Error
  on_scene_check <- function(x, edge) is.na(x) | (x <= edge & x >=0)
  bad.trees <- ""
  off.scene.trees <- ""
  coloc.sims <- ""
  for(i in 1:nrow(hip$exp.plan)) {
    X <- get_init_vals("tree.initialization", "treeX")[[i]]
    Y <- get_init_vals("tree.initialization", "treeY")[[i]]
    if(all(is.na(X) & is.na(Y))) next

    okay.loc <- (X == 0 & Y == 0) | (abs(X %% get_used("cellWidth")[[i]] - rep(get_used("cellWidth")[[i]] / 2, length(X))) < 1e-5 &
                                       abs(Y %% get_used("cellWidth")[[i]] - rep(get_used("cellWidth")[[i]] / 2, length(Y))) < 1e-5)

    x.on.scene <- on_scene_check(X, get_used("plotWidth")[[i]])
    y.on.scene <- on_scene_check(Y, get_used("plotHeight")[[i]])
    on.scene <- (x.on.scene & y.on.scene)

    coloc <- any(duplicated(dplyr::tibble(x = X, y = Y)))

    if(any(!okay.loc)) bad.trees       <- c(bad.trees,       paste0(hip$exp.plan$SimulationName[i], "-Tree", c(1:length(X))[!okay.loc]))
    if(any(!on.scene)) off.scene.trees <- c(off.scene.trees, paste0(hip$exp.plan$SimulationName[i], "-Tree", c(1:length(X))[!on.scene]))
    if(any(coloc))     coloc.sims      <- c(coloc.sims,      hip$exp.plan$SimulationName[i])
  }
  bad.trees       <- bad.trees[bad.trees != ""]
  off.scene.trees <- off.scene.trees[off.scene.trees != ""]
  coloc.sims      <- coloc.sims[coloc.sims != ""]

  tree.centered.error <- ifelse(length(bad.trees)       == 0, "", paste("-- The following trees are not centered on a cell:",
                                                                        paste(bad.trees, collapse = ", ")))
  tree.offscene.error <- ifelse(length(off.scene.trees) == 0, "", paste("-- The following trees' coordinates are beyond the scene boundaries:",
                                                                        paste(off.scene.trees, collapse = ", ")))
  tree.coloc.error    <- ifelse(length(coloc.sims)      == 0, "", paste("-- The following simulations have two or more trees located on the same cell:",
                                                                        paste(hip$exp.plan$SimulationName[coloc], collapse = ', ')))

  ## Distance & Time Errors
  less_than_comp <- function(param, ref) {
    ifelse(all(purrr::map2_lgl(get_used(param), get_used(ref), less_than)), "", paste0("-- ", param, " must be less than ", ref))
  }
  treeCropDistance.error        <- less_than_comp("treeCropDistance",        "plotWidth")
  treeRootPruningDistance.error <- less_than_comp("treeRootPruningDistance", "plotWidth")

  ## nrow(tree_init) == nrow(root_init) Error
  tree.init <- get_used("tree.initialization")
  if(all(is.na(tree.init))) {
    tree.rows <- rep(0, length(tree.init))
  } else {
    tree.rows <- purrr::map_dbl(tree.init, nrow)
  }

  root.init <- get_used("root.initialization")
  if(all(is.na(root.init))) {
    root.rows <- 0
  } else {
    root.rows <- purrr::map_dbl(root.init, nrow)
  }

  tree.root.error <- ifelse(all(tree.rows == root.rows),
                            "", "-- The number of rows in the tree initialization and root initialization tables must be equal.")

  ## Don't Edit Export Profile Errors
  EP.error <- ifelse(is_mod("profileNames") | is_mod("exportFrequencies"),
                     "-- profileNames and exportFrequencies cannot be defined using define_hisafe(). Use the 'profiles' argument of build_hisafe().", "")

  ## STICS parameter dependencies check
  capillary.error <- ifelse((is_mod("capillaryUptake") | is_mod("capillaryUptakeMinWater")) & (all(get_used("capillary") == 0) & all(get_used("macropososity") == 0)),
                            "-- capillaryUptake and capillaryUptakeMinWater are not active parameters when capillary = 0 or macropososity = 0.", "")
  drainage.error <- ifelse((is_mod("drainagePipesSpacing") | is_mod("drainagePipesDepth") | is_mod("waterConductivity") | is_mod("impermeableLayerDepth"))
                           & (all(get_used("artificialDrainage") == 0)& all(get_used("macropososity") == 0)),
                           "-- drainagePipesSpacing, drainagePipesDepth, waterConductivity, and impermeableLayerDepth are not active parameters when artificialDrainage = 0 or macroporosity = 0.",
                           "")
  denitrif.error <- ifelse((is_mod("denitrificationDepth") | is_mod("denitrificationRate")) & all(get_used("denitrification") == 0),
                           "-- denitrificationDepth and denitrificationRate are not active parameters when denitrification = 0.", "")
  watertable.error <- ifelse((is_mod("no3ConcentrationInWaterTable") | is_mod("nh4ConcentrationInWaterTable")) & all(get_used("waterTable") == 0),
                             "-- no3ConcentrationInWaterTable and nh4ConcentrationInWaterTable are not active parameters when waterTable = 0.", "")
  dm.error <- ifelse(any(get_used("artificialDrainage") == 1 & get_used("macroporosity") == 0),
                     "-- macroporosity mucst be activated (set to 1) if artificialDrainage is activated (set to 1).", "")
  cap.error <- ifelse(any(get_used("capillary") == 1 & get_used("macroporosity") == 0),
                     "-- macroporosity mucst be activated (set to 1) if capillary is activated (set to 1).", "")

  ## Timeseries Length Errors
  treePlanting.length.error <- ifelse(all(purrr::map_lgl(list(get_length("treePlantingYears"),
                                                              get_length("treePlantingDays")),
                                                         identical,
                                                         y = as.list(as.integer(tree.rows)))), "",
                                      "-- treePlantingYears and treePlantingDays must have the same length as the number of rows in the tree initialziation table")

  treePruning.length.error <- ifelse(all(purrr::map_lgl(list(get_length("treePruningProp"),
                                                             get_length("treePruningMaxHeight"),
                                                             get_length("treePruningDays")),
                                                        identical,
                                                        y = get_length("treePruningYears"))),
                                     "", "-- treePruningYears, treePruningProp, treePruningMaxHeight, and treePruningDays must have the same length")

  treeThinning.length.error <- ifelse(all(purrr::map_lgl(list(get_length("treeThinningYears"),
                                                              get_length("treeThinningDays")),
                                                         identical,
                                                         y = get_length("treeThinningIds"))),
                                      "", "-- treeThinningIds, treeThinningYears, and treeThinningDays must have the same length")

  rootPruning.length.error <- ifelse(all(purrr::map_lgl(list(get_length("treeRootPruningDays"),
                                                             get_length("treeRootPruningDistance"),
                                                             get_length("treeRootPruningDepth")),
                                                        identical,
                                                        y = get_length("treeRootPruningYears"))), "",
                                     "-- treeRootPruningYears, treeRootPruningDays, treeRootPruningDistance, and treeRootPruningDepth must have the same length")


  ## Crop Length & Simulation Length Errors
  goes_evenly  <- function(x, y) x > y | y %% x == 0
  if(!all(purrr::map2_lgl(get_length("mainCropSpecies"), get_used("nbSimulations"), goes_evenly))) {
    warning("-- mainCropSpecies length does not go evenly into nbSimulations." , call. = FALSE, immediate. = TRUE)
  }
  if(!all(purrr::map2_lgl(get_length("interCropSpecies"), get_used("nbSimulations"), goes_evenly))) {
    warning("-- interCropSpecies length does not go evenly into nbSimulations." , call. = FALSE, immediate. = TRUE)
  }

  mainCrop.long.error    <- ifelse(all(purrr::map2_lgl(get_length("mainCropSpecies"), get_used("nbSimulations"), less_than)),
                                   "", "-- length of mainCropSpecies cannot be larger than value of nbSimulations")
  interCrop.long.error   <- ifelse(all(purrr::map2_lgl(get_length("interCropSpecies"), get_used("nbSimulations"), less_than)),
                                   "", "-- length of interCropSpecies cannot be larger than value of nbSimulations")
  simuNbrDays.length.error <- ifelse(all(unlist(get_length("simulationNbrDays")) == 1) | identical(get_length("mainCropSpecies"), get_length("simulationNbrDays")),
                                     "", "-- simulationNbrDays and mainCropSpecies must have the same length or simulationNbrDays must have length 1")

  ## Geometry Errors
  weed.error <- ifelse(any(get_used_un("treeCropDistance") > 0 & get_used_un("treeCropRadius") > 0),
                       "-- treeCropDistance and treeCropRadius can not both be greater than 0", "")

  ## All weatherFile files exist
  if("weatherFile" %in% names(hip$exp.plan)) {
    if(all(file.exists(hip$exp.plan$weatherFile))) {
      wth.error <- ""
    } else {
      missing.wth.files <- hip$exp.plan$weatherFile[!file.exists(hip$exp.plan$weatherFile)]
      wth.error <- paste("-- the following .WTH files do not exist:", paste(missing.wth.files, collapse = ", "))
    }
  } else {
    wth.error <- ""
  }

  all.errors <- c(errors,
                  unique.sim.error, unique.simname.error, simname.space.error,
                  unsupported.trees.error, too.many.trees.error,
                  unsupported.crops.error, unsupported.itks.error,
                  plot.width.error, plot.height.error,
                  treeCropDistance.error, treeRootPruningDistance.error,
                  tree.centered.error, tree.offscene.error, tree.coloc.error,
                  tree.root.error, EP.error,
                  capillary.error, drainage.error, denitrif.error, watertable.error, dm.error, cap.error,
                  treePlanting.length.error, treePruning.length.error, treeThinning.length.error, rootPruning.length.error,
                  simuNbrDays.length.error,
                  weed.error, wth.error)
  all.errors <- paste0(all.errors[!(all.errors == "") & !is.na(all.errors)], collapse = "\n")
  if(all.errors != errors) stop(all.errors, call. = FALSE)

  invisible(TRUE)
}

#' Check validity of Hi-sAFe accepted values
#' @description Checks validity of Hi-sAFe inputs accepted values found in the package param_defs.txt file
#' Used within \code{\link{check_input_values}}.
#' @return An error message or empty character stirng.
#' @param variable A character string of the name of the variable to check.
#' @param exp.plan The exp.plan of a "hip" object.
#' @keywords internal
check_accepted <- function(variable, exp.plan) {
  exp.plan <- dplyr::mutate_all(exp.plan, as.list)
  if(variable %in% INPUT.DEFS$name) {
    to.check <- unlist(exp.plan[[variable]])
    element.def <- dplyr::filter(INPUT.DEFS, name == variable)
    accepted.vals <- stringr::str_split(element.def$accepted, ";")[[1]]
    accepted.pass <- (all(is.na(accepted.vals)) | all(as.character(to.check) %in% accepted.vals))
    if(accepted.pass) {
      return("")
    } else {
      return(paste0("-- ", variable, " - must be one of: ", paste0(accepted.vals, collapse = ", ")))
    }
  } else {
    return("")
  }
}

#' Check validity of Hi-sAFe input ranges
#' @description Checks validity of Hi-sAFe inputs against accepted ranges found in the package param_defs.txt file
#' Used within \code{\link{check_input_values}}.
#' @return An error message or empty character stirng.
#' @param variable A character string of the name of the variable to check.
#' @param exp.plan The exp.plan of a "hip" object.
#' @keywords internal
check_range <- function(variable, exp.plan) {
  exp.plan <- dplyr::mutate_all(exp.plan, as.list)
  if(variable %in% INPUT.DEFS$name) {
    to.check <- unlist(exp.plan[[variable]])
    element.def <- dplyr::filter(INPUT.DEFS, name == variable)
    min.val  <- element.def$min
    max.val  <- element.def$max
    max.pass <- (is.na(max.val) | all(to.check <= max.val))
    min.pass <- (is.na(min.val) | all(to.check >= min.val))
    if(max.pass & min.pass) {
      return("")
    } else if(!is.na(max.val) & !is.na(min.val)) {
      return(paste0("-- ", variable, " - must be betwen ", min.val, " and ", max.val))
    } else if(is.na(max.val) & !is.na(min.val)) {
      return(paste0("-- ", variable, " - must be greater than ", min.val))
    } else if(!is.na(max.val) & is.na(min.val)) {
      return(paste0("-- ", variable, " - must be less than ", max.val))
    }
  } else {
    return("")
  }
}

#' Check validity of Hi-sAFe input numeric types
#' @description Checks validity of Hi-sAFe inputs against accepted numeric types (continuous, integer) found in the package param_defs.txt file
#' Used within \code{\link{check_input_values}}.
#' @return An error message or empty character stirng.
#' @param variable A character string of the name of the variable to check.
#' @param exp.plan The exp.plan of a "hip" object.
#' @keywords internal
check_type <- function(variable, exp.plan) {
  exp.plan <- dplyr::mutate_all(exp.plan, as.list)
  if(variable %in% INPUT.DEFS$name) {
    to.check <- unlist(exp.plan[[variable]])
    element.def <- dplyr::filter(INPUT.DEFS, name == variable)
    type  <- element.def$type
    if(is.na(type)) return("")
    if(type == "integer"){
      if(all(is.numeric(to.check))) {
        if(!all(to.check %% 1 == 0)) {
          return(paste0("-- ", variable, " - must be an integer"))
        } else {
          return("")
        }
      } else {
        return(paste0("-- ", variable, " - must be an integer"))
      }
    } else if(type == "real" & !all(is.numeric(to.check))){
      return(paste0("-- ", variable, " - must be numeric"))
    } else if(type == "character" & !all(is.character(to.check))){
      return(paste0("-- ", variable, " - must be a character string/vector"))
    } else {
      return("")
    }
  } else {
    return("")
  }
}

#' Generate root initialization table for define_hisafe
#' @description Generates a root initialization table suitable for passing to \code{\link{define_hisafe}}.
#' Any passed parameters modify the table in the provided template.
#' @return A list containing a Hi-sAFE root initialization table.
#' @param template A character string of the path to the directory containing the template set of Hi-sAFe simulation folders/files to use.
#' See \code{\link{define_hisafe}} for more details.
#' @param rep Number of times to repeats the rows of the defined table.
#' @param ... Any parameters of Hi-sAFe root initialization table:
#'  \itemize{
#'  \item{"shape"}{}
#'  \item{"repartition"}{}
#'  \item{"paramShape1"}{}
#'  \item{"paramShape2"}{}
#'  \item{"paramShape3"}{}
#' }
#' @export
#' @family hisafe definition functions
#' @examples
#' \dontrun{
#' root.init <- root_init_params(template = "agroforestry", reps = 2, amount = 1)
#' }
root_init_params <- function(template, reps = 1, ...) {
  supported <- c("shape", "repartition", "paramShape1", "paramShape2", "paramShape3")
  temp <- modify_table(args           = list(...),
                       supported.args = supported,
                       character.args = NULL,
                       numeric.args   = supported,
                       positive.args  = supported,
                       perc.args      = NULL,
                       table.name     = "root.initialization",
                       template       = template)

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
#' Any passed parameters modify the table in the provided template.
#' @return A list containing a Hi-sAFE tree initialization table.
#' @param template A character string of the path to the directory containing the template set of Hi-sAFe simulation folders/files to use.
#' See \code{\link{define_hisafe}} for more details.
#' @param ... Any parameters of Hi-sAFe tree initialization table:
#'  \itemize{
#'  \item{"species"}{}
#'  \item{"height"}{}
#'  \item{"crownBaseHeight"}{}
#'  \item{"leafToFineRootsRatio"}{}
#'  \item{"crownRadius"}{}
#'  \item{"treeX"}{}
#'  \item{"treeY"}{}
#' }
#' @export
#' @family hisafe definition functions
#' @examples
#' \dontrun{
#' tree.init <- tree_init_params(template = "agroforestry", height = 2)
#' }
tree_init_params <- function(template, ...) {
  supported <- c("species", "height", "crownBaseHeight", "leafToFineRootsRatio", "crownRadius", "treeX", "treeY")
  out <- modify_table(args           = list(...),
                      supported.args = supported,
                      character.args = "species",
                      numeric.args   = supported[supported != "species"],
                      positive.args  = supported[supported != "species"],
                      perc.args      = NULL,
                      table.name     = "tree.initialization",
                      template       = template)
  return(list(out))
}

#' Generate soil layer initialization table for define_hisafe
#' @description Generates a soil layer initialization table suitable for passing to \code{\link{define_hisafe}}.
#' Any passed parameters modify the table in the provided template.
#' @return A list containing a Hi-sAFE soil layer initialization table.
#' @param template A character string of the path to the directory containing the template set of Hi-sAFe simulation folders/files to use.
#' See \code{\link{define_hisafe}} for more details.
#' @param ... Any parameters of Hi-sAFe soil layer initialization table:
#'  \itemize{
#'  \item{"waterContent"}{}
#'  \item{"no3Concentration"}{}
#'  \item{"nh4concentration"}{}
#' }
#' @export
#' @importFrom dplyr %>%
#' @family hisafe definition functions
#' @examples
#' \dontrun{
#' layer.init <- layer_init_params(template = "agroforestry", waterContent = 0.3)
#' }
layer_init_params <- function(template, ...) {
  supported <- c("waterContent", "no3Concentration", "nh4concentration")
  out <- modify_table(args           = list(...),
                      supported.args = supported,
                      numeric.args   = supported,
                      character.args = NULL,
                      positive.args  = supported,
                      perc.args      = NULL,
                      table.name     = "layer.initialization",
                      template       = template) %>%
    dplyr::mutate_all(function(x) format(x, nsmall = 1, trim = TRUE))

  if(nrow(out) > 5) stop(paste("Hi-sAFe supports a maximum of 5 soil layers"), call. = FALSE)

  return(list(out))
}

#' Generate soil layer table for define_hisafe
#' @description Generates a soil layer table suitable for passing to \code{\link{define_hisafe}}.
#' Any passed parameters modify the table in the provided template.
#' @return A list containing a Hi-sAFE soil layer table.
#' @param template A character string of the path to the directory containing the template set of Hi-sAFe simulation folders/files to use.
#' See \code{\link{define_hisafe}} for more details.
#' @param ... Any parameters of Hi-sAFe soil layer table:
#'  \itemize{
#'  \item{"thick"}{}
#'  \item{"sand"}{}
#'  \item{"clay"}{}
#'  \item{"limeStone"}{}
#'  \item{"organicMatter"}{}
#'  \item{"partSizeSand"}{}
#'  \item{"stone"}{}
#'  \item{"stoneType"}{}
#'  \item{"infiltrability"}{}
#' }
#' @export
#' @importFrom dplyr %>%
#' @family hisafe definition functions
#' @examples
#' \dontrun{
#' layers <- layer_params(template = "agroforestry", sand = 20)
#' }
layer_params <- function(template, ...) {
  supported <- c("thick", "sand", "clay", "limeStone", "organicMatter",
                 "partSizeSand", "stone", "stoneType", "infiltrability")
  out <- modify_table(args           = list(...),
                      supported.args = supported,
                      numeric.args   = supported,
                      character.args = NULL,
                      positive.args  = c("thick", "partSizeSand", "stoneType", "infiltrability"),
                      perc.args      = c("sand", "clay", "limeStone", "organicMatter", "stone"),
                      table.name     = "layers",
                      template       = template) %>%
    dplyr::mutate_at(.vars = dplyr::vars(-stoneType), .funs = function(x) format(x, nsmall = 1, trim = TRUE)) %>%
    dplyr::mutate(stoneType = format(stoneType, nsmall = 0, trim = TRUE))

  if(nrow(out) > 5) stop(paste("Hi-sAFe supports a maximum of 5 soil layers"), call. = FALSE)

  return(list(out))
}

#' Build tables for table param functions
#' @description Builds tables for table param functions
#' @return A data.frame (tibble) containing a Hi-sAFE soil layer table.
#' @param args From table param function
#' @param supported.args From table param function
#' @param numeric.args From table param function
#' @param character.args From table param function
#' @param positive.args From table param function
#' @param perc.args From table param function
#' @param table.name From table param function
#' @param template From table param function
#' @keywords internal
modify_table <- function(args, supported.args, numeric.args, character.args, positive.args, perc.args, table.name, template) {
  unsupported.args <- names(args)[!(names(args) %in% supported.args)]
  if(length(unsupported.args) > 0) {
    stop(paste0("The following arguments are not supported: ", paste(unsupported.args, collapse = ", "),
                "\nSupported arguments include: ", paste(supported.args, collapse = ", ")), call. = FALSE)
  }

  errors <- ""
  for(i in numeric.args) {
    if(i %in% names(args)) if(!is.numeric(args[[i]]))               errors <- c(errors, paste("--", i, "must be numeric"))
  }
  for(i in character.args) {
    if(i %in% names(args)) if(!is.numeric(args[[i]]))               errors <- c(errors, paste("--", i, "must be character"))
  }
  for(i in positive.args) {
    if(i %in% names(args)) if(any(args[[i]] < 0))                   errors <- c(errors, paste("--", i, "must be greater than or equal to 0"))
  }
  for(i in perc.args) {
    if(i %in% names(args)) if(any(args[[i]] < 0 | args[[i]] > 100)) errors <- c(errors, paste("--", i, "must be between 0 and 100"))
  }

  error.starter <- "One or more arguments with incorrect values:"
  errors <- paste(c(error.starter, errors[!(errors == "")]), collapse = "\n")
  if(errors != error.starter) stop(errors, call. = FALSE)

  TEMPLATE_PARAMS <- get_template_params(template)
  PARAM_NAMES     <- get_param_names(TEMPLATE_PARAMS)
  PARAM_DEFAULTS  <- get_param_vals(TEMPLATE_PARAMS, "value")

  out <- PARAM_DEFAULTS[[table.name]][[1]]

  for(arg in names(args)) {
    if(length(out[[arg]]) %% length(args[[arg]]) != 0) stop(paste("Length of provided", arg, "values does not go evenly into template table"), call. = FALSE)
    out[[arg]] <- args[[arg]]
  }

  return(out)
}
