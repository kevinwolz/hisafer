#' Define a Hi-sAFe experiment
#' @description Defines a Hi-sAFe experiment - the input parameters to one or more Hi-sAFe simulations.
#' @details It is strongly recommended to name each simulation in your experiment. This can be done via the \code{SimulationName} parameter.
#' If no names are provided, then \code{define_hisafe} will provide generic names of "Sim_1", "Sim_2", etc.
#' If \code{factorial = FALSE}, the default, then supply a value of \code{SimulationName} for each experiment.
#' If \code{factorial = TRUE}, then supply a single value of \code{SimulationName} as a prefix.
#' \code{define_hisafe} will append "_1", "_2", etc. for each of the (unknown number of) factorial experiments.
#' @return An object of class \code{hip}. This is a data frame (tibble) with each row a Hi-sAFe simulation and each column a Hi-sAFe input parameter.
#' @param factorial If \code{FALSE}, the default, then values are recycled (i.e. such as for default behavior of \code{data.frame()}).
#' If \code{TRUE}, then a factorial experiment is created, in which an ecperiment is defined for each possible combination of supplied values.
#' @param ... Any suported Hi-sAFe input parameter in the .sim and .pld files can be passed.
#' Parameters should be passed as \code{parameterName = values}. For more information on supported parameters, see _____.
#' @export
#' @importFrom dplyr %>%
#' @family hisafe definition functions
#' @examples
#' \dontrun{
#' # To define a Hi-sAFe simulation using all default parameter values:
#' default.exp <- define_hisfafe()
#'
#' # To define a Hi-sAFe experiment analyzing only variation in latitude:
#' latitude.exp <- define_hisfafe(SimulationName = c("Lat15", "Lat30", "Lat45", "Lat60"),
#'                                latitude = seq(15, 60, 15))
#'
#' # To define a factorial Hi-sAFe experiment analyzing
#' # simultaneous variation in latitude and tree line orientation:
#' lat.orient.exp <- define_hisfafe(SimulationName = c("Lat15", "Lat30", "Lat45", "Lat60"),
#'                                  latitude = seq(15, 60, 15),
#'                                  treeLineOrientation = c(0,90))
#' }
define_hisafe <- function(factorial = FALSE, ...) {

  # FOR TESTING
  # tmpfun <- function(...){ list(...) }
  # arg.list <- tmpfun(SimulationName = c("my1", "my2"), spacingBetweenRows = c(7,7), spacingWithinRows = c(5,5), cellWidth = 1)

  arg.list <- list(...)

  default.params <- purrr::map(HISAFE.PARAMS, ~ .x[["default"]])
  defaults.to.add <- default.params[which(!(names(default.params) %in% names(arg.list)))]

  param.list <- c(arg.list, defaults.to.add)

  if(factorial) {
    hip <- dplyr::as_tibble(expand.grid(param.list, stringsAsFactors = FALSE))
    hip$SimulationName <- paste0(hip$SimulationName, "_", 1:nrow(hip))
  } else {
    hip <- dplyr::as_tibble(as.data.frame(param.list, stringsAsFactors = FALSE))
    if(all(unique(hip$SimulationName) == "Sim")) hip$SimulationName <- paste0(hip$SimulationName, "_", 1:nrow(hip))
  }

  is.unique <- function(x) { length(unique(x)) != 1 }
  hip <- dplyr::bind_cols(hip[,  purrr::map_lgl(hip, is.unique)],
                          hip[, !purrr::map_lgl(hip, is.unique)]) %>%
    dplyr::select(SimulationName, everything())

  check_input_values(hip)
  class(hip) <- c("hip", class(hip))
  return(hip)
}

#' Define a Hi-sAFe experiment from a file
#' @description Defines a Hi-sAFe experiment - the input parameters to one or more Hi-sAFe simulations.
#' @details It is strongly recommended to name each simulation in your experiment. This can be done via the \code{SimulationName} parameter.
#' If no names are provided, then \code{define_hisafe_file} will provide generic names of "Sim_1", "Sim_2", etc.
#' @return An object of class \code{hip}. This is a data frame (tibble) with each row a Hi-sAFe simulation and each column a Hi-sAFe input parameter.
#' @param file A character string of the path to a csv file.
#' Each row in the file should represent a Hi-sAFe simulation and each column a Hi-sAFe input parameter.
#' For more information on supported parameters, see _____.
#' @export
#' @importFrom dplyr %>%
#' @family hisafe definition functions
#' @examples
#' \dontrun{
#' # To define a Hi-sAFe experiment from a file:
#' myexp <- define_hisafe_file("./data/example_exp.csv")
#' }
define_hisafe_file <- function(file) {

  provided <- dplyr::as_tibble(read.csv(file, header = TRUE, stringsAsFactors = FALSE))

  default.params  <- purrr::map(HISAFE.PARAMS, ~ .x[["default"]])
  defaults.to.add <- dplyr::as_tibble(default.params[which(!(names(default.params) %in% names(provided)))])
  defaults.to.add <- defaults.to.add[rep(1,nrow(provided)),]

  hip <- dplyr::bind_cols(provided, defaults.to.add)

  if(length(unique(hip$SimulationName)) == 1) hip$SimulationName <- paste0(hip$SimulationName, "_", 1:nrow(hip))

  is.unique <- function(x) { length(unique(x)) != 1 }
  hip <- dplyr::bind_cols(hip[,  purrr::map_lgl(hip, is.unique)],
                          hip[, !purrr::map_lgl(hip, is.unique)]) %>%
    dplyr::select(SimulationName, everything())

  check_input_values(hip)
  class(hip) <- c("hip", class(hip))
  return(hip)
}

#' Check validity of all Hi-sAFe inputs
#' @description Checks the validity of all Hi-sAFe inputs against \code{HISAFE.PARAMS} and provides errors and/or warnings if issues are found.
#' Used within \code{define_hisafe} and \code{define_hisafe_file}.
#' @return Produces errors and/or warnings if issues are found. Otherwise, invisibly returns \code{TRUE}.
#' @param hip An object of class \code{hip}.
check_input_values <- function(hip) {

  ## Check for unsupported inputs
  if(any(!(names(hip) %in% names(HISAFE.PARAMS)))) {
    extra.cols <- names(hip)[!(names(hip) %in% names(HISAFE.PARAMS))]
    extra.message <- c("The following variables are not supported:", paste0(extra.cols, collapse = ", "))
    stop(extra.message, call. = FALSE)
  }

  ## Check for validity of input values
  messages <- "Hi-sAFe definition warnings:"
  errors <-   "Hi-sAFe definition errors:"

  hip.no.name <- dplyr::select(hip, -SimulationName)
  unique.sim.error     <- ifelse(identical(hip.no.name, dplyr::distinct(hip.no.name)),   "", "-- Each simulaton must be distinct.")
  unique.simname.error <- ifelse(unique(table(hip$SimulationName)) == 1, "", "-- SimulationName - each siulation must have a unique name")
  btwn.tree.error      <- ifelse(((hip$spacingBetweenRows / hip$cellWidth) %% 2) == 1, "",
                                 "-- Tree should be centered in a cell. (spacingBetweenRows / cellWidth) should be an odd integer")
  within.tree.error    <- ifelse(((hip$spacingWithinRows  / hip$cellWidth) %% 2) == 1, "",
                                 "-- Tree should be centered in a cell. (spacingWithinRows / cellWidth) should be an odd integer")
  allowed.errors       <- purrr::map_chr(names(hip), check_allowed, hip = hip)
  minmax.errors        <- purrr::map_chr(names(hip), check_minmax,  hip = hip)

  diff.crop.warning    <- ifelse(any(hip$mainCropSpecies == hip$interCropSpecies), "-- mainCropSpecies & interCropSpecies are typically different", "")
  minmax.warnings      <- purrr::map_chr(names(hip), check_minmax_sug, hip = hip)

  all.messages <- c(messages, minmax.warnings, diff.crop.warning)
  all.errors   <- c(errors, unique.sim.error, unique.simname.error, btwn.tree.error, within.tree.error, allowed.errors, minmax.errors)
  all.messages <- paste0(all.messages[!all.messages == ""], collapse = "\n")
  all.errors   <- paste0(all.errors[!all.errors == ""], collapse = "\n")

  if(all.messages != messages) warning(all.messages, call. = FALSE)
  if(all.errors   != errors)   stop(all.errors, call. = FALSE)

  invisible(TRUE)
}

#' Check validity of Hi-sAFe input ranges
#' @description Checks validity of Hi-sAFe inputs ranges against ranges in \code{HISAFE.PARAMS}.
#' Used within \code{check_input_values}.
#' @return An error message or empty character stirng.
#' @param variable A character string of the name of the variable to check.
#' @param hip An object of class \code{hip}.
check_allowed <- function(variable, hip) {
  allowed.vals <- purrr::map(HISAFE.PARAMS, ~ .x[["allowed"]])[[variable]]
  allowed.pass <- (is.na(allowed.vals[1]) | all(hip[[variable]] %in% allowed.vals))
  if(allowed.pass) {
    return("")
  } else {
    return(paste0("-- ", variable, " - must be one of: ", paste0(allowed.vals, collapse = ", ")))
  }
}

#' Check validity of Hi-sAFe input ranges
#' @description Checks validity of Hi-sAFe input min and max values against values in \code{HISAFE.PARAMS}.
#' Used within \code{check_input_values}.
#' @return An error message or empty character stirng.
#' @param variable A character string of the name of the variable to check.
#' @param hip An object of class \code{hip}.
check_minmax <- function(variable, hip) {
  min.val  <- purrr::map(HISAFE.PARAMS, ~ .x[["min"]])[[variable]]
  max.val  <- purrr::map(HISAFE.PARAMS, ~ .x[["max"]])[[variable]]
  max.pass <- (is.na(max.val) | all(hip[[variable]] <= max.val))
  min.pass <- (is.na(min.val) | all(hip[[variable]] >= min.val))
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

#' Check plausbility of Hi-sAFe input ranges
#' @description Checks plausibility of Hi-sAFe input min and max values against suggested values in \code{HISAFE.PARAMS}.
#' Used within \code{check_input_values}.
#' @return A warning message or empty character stirng.
#' @param variable A character string of the name of the variable to check.
#' @param hip An object of class \code{hip}.
check_minmax_sug <- function(variable, hip) {
  min.sug.val <- purrr::map(HISAFE.PARAMS, ~ .x[["min.sug"]])[[variable]]
  max.sug.val <- purrr::map(HISAFE.PARAMS, ~ .x[["max.sug"]])[[variable]]
  max.pass    <- (is.na(max.sug.val) | all(hip[[variable]] <= max.sug.val))
  min.pass    <- (is.na(min.sug.val) | all(hip[[variable]] >= min.sug.val))
  if(max.pass & min.pass) {
    return("")
  } else if(!is.na(max.sug.val) & !is.na(min.sug.val)) {
    return(paste0("-- ", variable, " - is typically betwen ", min.sug.val, " and ", max.sug.val))
  } else if(is.na(max.sug.val) & !is.na(min.sug.val)) {
    return(paste0("-- ", variable, " - is typically greater than ", min.sug.val))
  } else if(!is.na(max.sug.val) & is.na(min.sug.val)) {
    return(paste0("-- ", variable, " - is typically less than ", max.sug.val))
  }
}
