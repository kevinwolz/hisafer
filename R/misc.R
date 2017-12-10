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
hisafe_params <- function(variable = "names", template = "agroforestry_default") {

  TEMPLATE_PARAMS <- get_template_params(get_template_path(template))
  PARAM_NAMES     <- unlist(get_param_names(TEMPLATE_PARAMS), use.names = FALSE)
  PARAM_DEFAULTS  <- get_param_vals(TEMPLATE_PARAMS, "value")
  PARAM_RANGES    <- get_param_vals(TEMPLATE_PARAMS, "range")
  PARAM_ACCEPTED  <- get_param_vals(TEMPLATE_PARAMS, "accepted")
  PARAM_TYPE      <- get_param_vals(TEMPLATE_PARAMS, "type")

  acceptable <- c(PARAM_NAMES, "names", "all")
  if(any(!(variable %in% acceptable))) {
    bad.vars <- variable[!(variable %in% acceptable)]
    stop(paste0("The following are not supported Hi-sAFe input parameters: ", bad.vars))
  }

  if(variable[1] == "all") {
    for(i in 1:length(PARAM_NAMES)){
      if(i == 1) { cat(PARAM_NAMES[i]) } else { cat("\n\n", PARAM_NAMES[i]) }
      if("tbl" %in% class(PARAM_DEFAULTS[[i]])){
        cat("\n  -- Default:\n")
        print(PARAM_DEFAULTS[[i]])
      } else {
        cat("\n  -- Default:", paste0(PARAM_DEFAULTS[[i]], collapse = ", "))
      }

      if(!all(is.na(PARAM_RANGES[[i]])))   cat("\n  -- Accepted Range: [",  paste0(PARAM_RANGES[[i]], collapse = ", "), "] ", sep = "")
      if(!all(is.na(PARAM_TYPE[[i]])))     cat("(", PARAM_TYPE[[i]], ")", sep = "")
      if(!all(is.na(PARAM_ACCEPTED[[i]]))) cat("\n  -- Accepted Values: ", paste0(PARAM_ACCEPTED[[i]], collapse = ", "))

    }
  } else if (variable[1] == "names") {
    cat(paste0(PARAM_NAMES, collapse = "\n"))
  } else {
    for(i in 1:length(variable)){
      if(i == 1) { cat(variable[i]) } else { cat("\n\n", variable[i]) }
      if("tbl" %in% class(PARAM_DEFAULTS[[variable[i]]])){
        cat("\n  -- Default:\n")
        print(PARAM_DEFAULTS[[variable[i]]])
      } else {
        cat("\n  -- Default:", paste0(PARAM_DEFAULTS[[variable[i]]], collapse = ", "))
      }

      if(!all(is.na(PARAM_RANGES[[variable[i]]])))   cat("\n  -- Accepted Range: [",  paste0(PARAM_RANGES[[variable[i]]], collapse = ", "), "] ", sep = "")
      if(!all(is.na(PARAM_TYPE[[variable[i]]])))     cat("(", PARAM_TYPE[[variable[i]]], ")", sep = "")
      if(!all(is.na(PARAM_ACCEPTED[[variable[i]]]))) cat("\n  -- Accepted Values:", paste0(PARAM_ACCEPTED[[variable[i]]], collapse = ", "))
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
  if(!("hop" %in% class(hop))) stop("data not of class hop", call. = FALSE)

  profiles.to.check <- names(hop)[!(names(hop) %in% c("variables", "exp.path"))]
  profiles <- profiles.to.check[purrr::map_lgl(profiles.to.check, function(x) nrow(hop[[x]]) > 0)]

  existing.names <- unique(hop[[profiles[1]]]$SimulationName)
  missing.names  <- existing.names[!(unique(existing.names) %in% old.names)]
  old.names <- c(old.names, missing.names)
  new.names <- c(new.names, missing.names)

  for(i in profiles) {
    hop[[i]]$SimulationName <- new.names[match(hop[[i]]$SimulationName, old.names)]
  }

  return(hop)
}

#' Filter a hop object by SimulationName
#' @description Filters a hop object by SimulationName
#' @return A hop object.
#' @param hop A object of class hop or face.
#' @param simu.names A character vector of the SimulationNames to keep. If "all", no filtering occurs.
#' @export
#' @family hisafe helper functions
#' @examples
#' \dontrun{
#' newhop <- hop_filter(myhop, c("Sim_1", "Sim_2"))
#' }
hop_filter <- function(hop, simu.names) {
  if(simu.names[1] == "all") return(hop)
  if(!("hop" %in% class(hop))) stop("data not of class hop", call. = FALSE)
  profiles.to.check <- names(hop)[!(names(hop) %in% c("variables", "exp.path"))]
  profiles <- profiles.to.check[purrr::map_lgl(profiles.to.check, function(x) nrow(hop[[x]]) > 0)]
  for(i in profiles) { hop[[i]] <- dplyr::filter(hop[[i]], SimulationName %in% simu.names) }
  if(length(simu.names) == 1) { class(hop) <- class(hop)[class(hop) != "hop-group"] }
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
#' new_hop <- hop_merge(hop1, hop2, hop3)
#' }
hop_merge <- function(...) {

  hops <- list(...)

  check_class <- function(x) { ("hop" %in% class(x)) }
  if(!all(purrr::map_lgl(hops, check_class))) stop("one or more supplied objects not of class hop", call. = FALSE)

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

  merged_hop$exp.plan <- dplyr::select(merged_hop$exp.plan, "SimulationName", unique.cols)

  merged_hop$variables <- dplyr::distinct(merged_hop$variables)

  merged_hop$exp.path <- NA

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
