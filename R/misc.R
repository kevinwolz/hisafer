#' Display supported Hi-sAFe input parameters
#' @description Displays supported Hi-sAFe input parameters, their default values, and their accepted/suggested ranges.
#' @return Invisibly returns an alphebetized character vector of the names of supported Hi-sAFe prameters.
#' @param variable If "names", the default, then just the names of supported Hi-sAFe parameters is printed to the console.
#' If "all", then the names, default values, and accepted/suggested ranges of supported Hi-sAFe parameters is printed.
#' Can also be a character vector of specific Hi-sAFe parameters of which to display details.
#' @export
#' @family hisafe helper functions
#' @examples
#' \dontrun{
#' hisafe_params()            # just for parameter names
#' hisafe_params("cellWidth") # details of cellWidth parameter
#' hisafe_params("all")       # details of all parameters
#' }
hisafe_params <- function(variable = "names", template = "default", template.tree = "walnut-hybrid") {

  TEMPLATE_PARAMS <- get_template_params(template = template, template.tree = template.tree)
  PARAM_NAMES     <- unlist(get_param_names(TEMPLATE_PARAMS), use.names = FALSE)
  PARAM_DEFAULTS  <- get_param_vals(TEMPLATE_PARAMS, "value")
  PARAM_RANGES    <- get_param_vals(TEMPLATE_PARAMS, "range")
  PARAM_ACCEPTED  <- get_param_vals(TEMPLATE_PARAMS, "accepted")

  acceptable <- c(PARAM_NAMES, "names", "all")
  if(any(!(variable %in% acceptable))) {
    bad.vars <- variable[!(variable %in% acceptable)]
    stop(paste0("The following are not supported Hi-sAFe input parameters: ", bad.vars))
  }

  if(variable[1] == "all") {
    for(i in 1:length(PARAM_NAMES)){
      cat(paste0("\n\n", PARAM_NAMES[i]))
      cat(paste0("\nDefault: ", PARAM_DEFAULTS[[i]]))
      if(!is.na(PARAM_RANGES[[i]]))   cat(paste0("\nAccepted Range: ",   PARAM_RANGES[[i]]))
      if(!is.na(PARAM_ACCEPTED[[i]])) cat(paste0("\nAccepted Values: ",  PARAM_ACCEPTED[[i]]))
    }
  } else if (variable[1] == "names") {
    cat(paste0(PARAM_NAMES, collapse = "\n"))
  } else {
    for(i in 1:length(variable)){
      cat(paste0("\n", variable[i], "\n"))
      cat(paste0("Default: ", PARAM_DEFAULTS[[variable]]))
      if(!is.na(PARAM_RANGES[[variable]]))   cat(paste0("Accepted Range: ",   PARAM_RANGES[[variable]]))
      if(!is.na(PARAM_ACCEPTED[[variable]])) cat(paste0("Accepted Values: ",  PARAM_ACCEPTED[[variable]]))
    }
  }
  invisible(PARAM_NAMES)
}

#' Display supported Hi-sAFe output profiles
#' @description Displays supported Hi-sAFe output profiles and standard output frequency.
#' @return Invisibly returns a data frame containing the profiles names and output frequency.
#' @export
#' @family hisafe helper functions
#' @examples
#' \dontrun{
#' hisafe_profiles()
#' }
hisafe_profiles <- function(variable = "names") {
  print(as.data.frame(SUPPORTED.PROFILES), row.names = FALSE)
}

#' Change SimulationNames in a hop object
#' @description Changes SimulationNames in a hop object.
#' @return Returns the provided hop object with names changed.
#' @param hop A object of class "hop".
#' @param old.names A character vector of the old SimulationNames to change.
#' @param new.names A character vector of the new SimulationNames, in the same order as they apply to \code{old.names}.
#' @export
#' @family hisafe helper functions
#' @examples
#' \dontrun{
#' simu_rename(myhop, old.names = c("Sim_1", "Sim_2"), new.names = c("Lat30", "Lat60"))
#' }
simu_rename <- function(hop, old.names, new.names) {
  if(!any(c("hop", "hop-group") %in% class(hop))) stop("data not of class hop or hop-group", call. = FALSE)

  profiles.to.check <- c("annualtree", "annualcrop", "annualplot",
                         "trees", "plot", "climate", "roots",
                         "monthCells", "cells", "voxels",
                         "inputs", "exp.plan")
  profiles <- profiles.to.check[purrr::map_lgl(profiles.to.check, function(x) nrow(hop[[x]]) > 0)]

  for(i in profiles) {
    #if(!is.factor(hop[[i]]$SimulationName)) hop[[i]]$SimulationName <- factor(hop[[i]]$SimulationName)
    hop[[i]]$SimulationName <- new.names[match(hop[[i]]$SimulationName, old.names)]
  }

  return(hop)
}

#' Merge multiple hop objects
#' @description Merges multiple hop objects, renaming simulation names if there are duplicates
#' @return Returns a hop object.
#' @param ... Any number of individual hop objects to merge.
#' @export
#' @importFrom dplyr %>%
#' @family hisafe helper functions
#' @examples
#' \dontrun{
#' new_hop <- hop_merge(list(hop1, hop2, hop3))
#' }
hop_merge <- function(...) {

  hops <- list(...)

  check_class <- function(x) { any(c("hop", "hop-group") %in% class(x)) }
  if(!all(purrr::map_lgl(hops, check_class))) stop("one or more list elements not of class hop or hop-group", call. = FALSE)

  make_names_unique <- function(x, num){ paste0(num, "-", x) }
  old.names <- purrr::map(purrr::map(hops, "exp.plan"), "SimulationName")

  if(any(duplicated(as.character(unlist(old.names))))) {
    hops <- purrr::pmap(list(hop = hops,
                             old.names = old.names,
                             new.names = purrr::map2(old.names, 1:length(old.names), make_names_unique)),
                        simu_rename)
  }

  clear_elements <- function(x) {
    x$exp.path <- NULL
    return(x)
  }

  merged_hop <- hops %>%
    purrr::map(clear_elements) %>%
    purrr::pmap(dplyr::bind_rows)

  hip <- merged_hop$exp.plan

  unique.cols <- names(hip)[purrr::map_lgl(hip, function(x) (length(unique(x)) != 1))]
  unique.cols <- unique.cols[unique.cols != "SimulationName"]
  other.cols  <- names(hip)[!(names(hip) %in% c("SimulationName", unique.cols))]

  merged_hop$exp.plan <- dplyr::bind_cols(hip[, "SimulationName"], hip[,  unique.cols], hip[, other.cols])

  merged_hop$exp.plan <- select(merged_hop$exp.plan, "SimulationName", unique.cols)

  merged_hop$variables <- dplyr::distinct(merged_hop$variables)

  class(merged_hop) <- c("hop-group", "hop", class(merged_hop))
  return(merged_hop)
}

#' Display version numbers of Hi-sAFe and Java
#' @description Displays the version numbers of Hi-sAFe and Java.
#' @return Invisibly returns the Hi-sAFe version number
#' @param capsis.path A character string of the path to the Capsis folder
#' @export
#' @examples
#' \dontrun{
#' hisafe_info()
#' }
hisafe_info <- function(capsis.path = "/Applications/Capsis") {
  #cat("Capsis Version:",  capsis.version)

  hisafe.id.card <- clean_path(paste0(capsis.path, "/src/safe/idcard.properties"))
  hisafe.info <- scan(hisafe.id.card, what = "character", encoding = "latin1", sep = "\n", quiet = TRUE)
  hisafe.version <- strsplit(grep("Version = ", hisafe.info, value = TRUE), split = " = ", fixed = TRUE)[[1]][2]
  cat("Hi-sAFe Version:", hisafe.version)

  #cat("\n\nSTICS Version:",   stics.version)

  cat("\n\nJava Version:")
  system("java -version", wait = TRUE)

  invisible(hisafe.version)
}
