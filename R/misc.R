#' Display version numbers of Hi-sAFe and Java
#' @description Displays the version numbers of Hi-sAFe and Java.
#' @return Invisibly returns the Hi-sAFe version number
#' @param capsis.path A character string of the path to the Capsis folder
#' @export
#' @examples
#' \dontrun{
#' hisafe_info()
#' }
hisafe_info <- function(capsis.path) {

  capsis.path <- R.utils::getAbsolutePath(capsis.path)

  if(!dir.exists(capsis.path))                    stop("directory specified by capsis.path does not exist",          call. = FALSE)
  if(!("capsis.sh" %in% list.files(capsis.path))) stop("directory specified by capsis.path does not contain Capsis", call. = FALSE)

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

#' Copy a Hi-sAFe template to specified location
#' @description Copies a Hi-sAFe template to specified location.
#' @return Invisibly returns a logical vector indicating if the attempted fily copy succeeded.
#' @param template A character string of the path to the Hi-sAFe directory structure/files to use as a template
#' (or one of the strings signaling a default template)
#' See \code{\link{define_hisafe}} for more details on available default templates.
#' @param path A character string of the path to where the template folder should be copied.
#' @param new.name A character string of the a name for the newly copied folder.
#' If \code{NULL}, the default, then the name will remain the same as the original template folder.
#' @export
#' @examples
#' \dontrun{
#' copy_hisafe_template("agroforestry_default", "/Users/myname/Desktop/")
#' }
copy_hisafe_template <- function(template, destination, overwrite = TRUE, new.name = NULL) {
  template.path        <- get_template_path(template)
  template.subpath     <- get_template_subpath(template)
  template.folder.name <- basename(template.path)
  dum <- file.copy(template.path, destination, recursive = TRUE, overwrite = overwrite)
  dum <- file.copy(list.files(template.subpath, full.names = TRUE),
                   clean_path(paste0(destination, "/", template.folder.name)),
                   recursive = TRUE,
                   overwrite = overwrite)
  if(!is.null(new.name)) {
    dum <- file.rename(clean_path(paste0(destination, "/", template.folder.name)),
                       clean_path(paste0(destination, "/", new.name)))
  }
  invisible(dum)
}

#' Display supported Hi-sAFe input parameters
#' @description Displays supported Hi-sAFe input parameters, their default values, and their accepted/suggested ranges.
#' @return Invisibly returns an alphebetized character vector of the names of supported Hi-sAFe prameters.
#' @param variable A character vector of specific Hi-sAFe parameters of which to display details.
#' Can also be a regular expression for which to search in Hi-sAFe parameter names.
#' If "names", the default, then just the names of Hi-sAFe parameters is printed to the console.
#' If "all", then the names, definitions, and units of all Hi-sAFe parameters is printed.
#' @param search Logical indicating whether \code{variable} should be treated as a regular expression and
#' searched for in the parameter names rather than matched literally.
#' @param template A character string of the path to the directory containing the template set of Hi-sAFe simulation folders/files to use.
#' hisafer comes with three "default" templates than can be used by specificying specific character strings:
#' \itemize{
#'  \item{"agroforestry_default"}{ - An agroforestry template based on the calibration simulation of Hi-sAFe using the Restinclieres A2
#'  walnut-wheat agroforestry plot in Montpellier, France. Spacing is 13m between rows and 8m within rows.}
#'  \item{"forestry_default"}{ - Walnut plantation forestry at tigher tree spacing of 5m between row and 3m within rows.
#'  Crop is grass in all cells except for where the tree trunk is. Serves as a forestry control.}
#'  \item{"monocrop_default"}{ - No trees. A scene with just a single cell of wheat. Serves as a monocrop control.}
#' }
#' @export
#' @family hisafe helper functions
#' @examples
#' \dontrun{
#' hip_params()            # just for parameter names
#' hip_params("cellWidth") # details of cellWidth parameter
#' hip_params("all")       # details of all parameters
#' }
hip_params <- function(variable = "names", search = FALSE, template = "agroforestry") {

  if(!is.character(variable))                           stop("variable argument must be a character vector",                call. = FALSE)
  if(!(is.character(template) & length(template) == 1)) stop("template argument must be a character vector of length 1",    call. = FALSE)
  if(search & length(variable) > 1)                     stop("search = TRUE is only possible with a single variable input", call. = FALSE)

  TEMPLATE_PARAMS <- get_template_params(template)
  PARAM_NAMES     <- sort(unlist(get_param_names(TEMPLATE_PARAMS), use.names = FALSE))
  PARAM_DEFAULTS  <- get_param_vals(TEMPLATE_PARAMS, "value")

  acceptable <- c(PARAM_NAMES, "names", "all")
  if(any(!(variable %in% acceptable)) & !search) {
    bad.vars <- sort(variable[!(variable %in% acceptable)])
    close.matches  <- purrr::map(bad.vars, stringdist::stringdist, b = PARAM_NAMES)
    suggested.vars <- PARAM_NAMES[unlist(purrr::map(close.matches, which.min))]
    stop(paste0("The following are not supported Hi-sAFe input parameters: ", paste(bad.vars, collapse = ", "),
                "\n       Did you mean: ", paste(suggested.vars, collapse = " or "), "?"), call. = FALSE)
  }

  if(variable[1] == "all") {
    for(i in 1:length(PARAM_NAMES)){
      var.def <- dplyr::filter(INPUT.DEFS, name == PARAM_NAMES[i])
      if(i == 1) { cat(PARAM_NAMES[i]) } else { cat("\n\n", PARAM_NAMES[i]) }
      if("tbl" %in% class(PARAM_DEFAULTS[[i]])){
        cat("\n  -- Default:\n")
        print(PARAM_DEFAULTS[[i]])
      } else {
        cat("\n  -- Default:", paste0(PARAM_DEFAULTS[[i]], collapse = ", "))
      }
      cat("\n  -- Definition:", var.def$definition)
      cat("\n  -- Units: ", var.def$unit, " (", var.def$type, ")", sep = "")
      if(!all(is.na(c(var.def$min, var.def$max)))) cat("\n  -- Accepted Range: [", paste0(c(var.def$min, var.def$max), collapse = ", "), "] ", sep = "")
      if(!all(is.na(var.def$accepted)))            cat("\n  -- Accepted Values: ", paste0(var.def$accepted, collapse = ", "))
    }
  } else if (variable[1] == "names") {
    cat(paste0(PARAM_NAMES, collapse = "\n"))
  } else {
    if(search) cat("'variable' values will be searched as regular expressions.")
    for(i in 1:length(variable)){
      if(search) {
        var.def <- dplyr::filter(INPUT.DEFS, stringr::str_detect(tolower(name), variable[i]))
        if(nrow(var.def) == 0) {
          cat("\n\n  --", paste(variable[i], "was not detected in any Hi-sAFe input parameter names"))
          next
        }
      } else {
        var.def <- dplyr::filter(INPUT.DEFS, name == variable[i])
      }
      for(j in 1:nrow(var.def)){
        cat("\n\n", var.def$name[j])
        if("tbl" %in% class(PARAM_DEFAULTS[[var.def$name[j]]])){
          cat("\n  -- Default:\n")
          print(PARAM_DEFAULTS[[var.def$name[j]]])
        } else {
          cat("\n  -- Default:", paste0(PARAM_DEFAULTS[[var.def$name[j]]], collapse = ", "))
        }
        cat("\n  -- Definition:", var.def$definition[j])
        cat("\n  -- Units: ", var.def$unit[j], " (", var.def$type[j], ")", sep = "")
        if(!all(is.na(c(var.def$min[j], var.def$max[j])))) cat("\n  -- Accepted Range: [", paste0(c(var.def$min[j], var.def$max[j]), collapse = ", "), "] ", sep = "")
        if(!all(is.na(var.def$accepted[j])))               cat("\n  -- Accepted Values: ", paste0(var.def$accepted[j], collapse = ", "))
      }
    }
  }
  invisible(PARAM_NAMES)
}

#' Display Hi-sAFe output variables
#' @description Displays Hi-sAFe output variables, their definitions, and thier units.
#' @return Invisibly returns an alphebetized character vector of the names of Hi-sAFe output variables
#' @param variable A character vector of specific Hi-sAFe output variables of which to display details.
#' Can also be a regular expression for which to search in Hi-sAFe output variable names.
#' If "names", the default, then just the names of Hi-sAFe output variables is printed to the console.
#' If "all", then the names, definitions, and units of all Hi-sAFe output variables is printed.
#' @param search Logical indicating whether \code{variable} should be treated as a regular expression and
#' searched for in the variable names rather than matched literally.
#' @export
#' @family hisafe helper functions
#' @examples
#' \dontrun{
#' hop_params()                 # just for parameter names
#' hop_params("carbonBranches") # details of cellWidth parameter
#' hop_params("all")            # details of all parameters
#' }
hop_params <- function(variable = "names", search = FALSE) {

  if(!is.character(variable))                           stop("variable argument must be a character vector",                call. = FALSE)
  if(search & length(variable) > 1)                     stop("search = TRUE is only possible with a single variable input", call. = FALSE)

  acceptable <- c(OUTPUT.DEFS$name, "names", "all")
  if(any(!(variable %in% acceptable)) & !search) {
    bad.vars <- sort(variable[!(variable %in% acceptable)])
    close.matches  <- purrr::map(bad.vars, stringdist::stringdist, b = OUTPUT.DEFS$name)
    suggested.vars <- OUTPUT.DEFS$name[unlist(purrr::map(close.matches, which.min))]
    stop(paste0("The following are not supported Hi-sAFe input parameters: ", paste(bad.vars, collapse = ", "),
                "\n       Did you mean: ", paste(suggested.vars, collapse = " or "), "?"), call. = FALSE)
  }

  if(variable[1] == "all") {
    for(i in 1:nrow(OUTPUT.DEFS)){
      var.def <- OUTPUT.DEFS[i, ]
      if(i == 1) { cat(var.def$name) } else { cat("\n\n", var.def$name) }
      cat("\n  -- Output profile:", var.def$profile)
      cat("\n  -- Definition:",     var.def$definition)
      cat("\n  -- Units:",          var.def$unit)
    }
  } else if (variable[1] == "names") {
    cat(paste0(OUTPUT.DEFS$profile, " - ", OUTPUT.DEFS$name, collapse = "\n"))
  } else {
    if(search) cat("'variable' values will be searched as regular expressions.")
    for(i in 1:length(variable)){
      if(search) {
        var.def <- dplyr::filter(OUTPUT.DEFS, stringr::str_detect(tolower(name), variable[i]))
        if(nrow(var.def) == 0) {
          cat("\n\n  --", paste(variable[i], "was not detected in any Hi-sAFe input parameter names"))
          next
        }
      } else {
        var.def <- dplyr::filter(OUTPUT.DEFS, name == variable[i])
      }
      for(j in 1:nrow(var.def)){
        cat("\n\n", var.def$name[j])
        cat("\n  -- Output profile:", var.def$profile[j])
        cat("\n  -- Definition:",     var.def$definition[j])
        cat("\n  -- Units:",          var.def$unit[j])
      }
    }
    invisible(OUTPUT.DEFS$name)
  }
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
#' @param hop An object of class "hop".
#' @param old.names A character vector of the old SimulationNames to change.
#' @param new.names A character vector of the new SimulationNames, in the same order as they apply to \code{old.names}.
#' @export
#' @family hisafe helper functions
#' @examples
#' \dontrun{
#' hop_rename(myhop, old.names = c("Sim_1", "Sim_2"), new.names = c("Lat30", "Lat60"))
#' }
hop_rename <- function(hop, old.names, new.names) {
  is_hop(hop, error = TRUE)
  if(!all(old.names %in% hop$exp.plan$SimulationName)) stop("one or more values in old.names is not present in hop", call. = FALSE)

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

#' Filter a hop object by SimulationName, Date, and id
#' @description Filters a hop object by SimulationName, Date, and id
#' @return A hop object.
#' @param hop An object of class hop or face.
#' @param simu.names A character vector of the SimulationNames to keep. If "all", no filtering occurs.
#' @param tree.ids A numeric vector of the tree ids to keep. If "all", no filtering occurs.
#' @param date.min A character string of the minimum date to keep, in the format "YYYY-MM-DD" or of class Date.
#' If NA, the minimum date in \code{hop} is used. Only used if \code{dates} is \code{NULL}.
#' @param date.max A character string of the maximum date to keep, in the format "YYYY-MM-DD" or of class Date.
#' If NA, the maximum date in \code{hop} is used. Only used if \code{dates} is \code{NULL}.
#' @param dates A character vector (in the format "YYYY-MM-DD") or a vector of class Date of the dates to keep.
#' If \code{NULL}, then \code{date.max} and \code{date.min} are used instad.
#' @param strip.exp.plan Logical indicating whether or not to remove the exp.plan variables
#' (which are appended by \code{\link{read_hisafe}}) from each profile in \code{hop}.
#' @export
#' @family hisafe helper functions
#' @examples
#' \dontrun{
#' newhop <- hop_filter(myhop, c("Sim_1", "Sim_2"))
#' }
hop_filter <- function(hop,
                       simu.names     = "all",
                       tree.ids       = "all",
                       date.min       = NA,
                       date.max       = NA,
                       dates          = NULL,
                       strip.exp.plan = FALSE) {
  is_hop(hop, error = TRUE)
  if(!all(is.character(simu.names)))                                        stop("simu.names argument must be 'all' or a character vector",  call. = FALSE)
  if(!(tree.ids[1] == "all" | all(is.numeric(tree.ids))))                   stop("tree.ids argument must be 'all' or a numeric vector",      call. = FALSE)
  if(!(length(date.min) == 1 & (is.character(date.min) | is.na(date.min)))) stop("date.min argument must be a character vector of length 1", call. = FALSE)
  if(!(length(date.max) == 1 & (is.character(date.max) | is.na(date.max)))) stop("date.max argument must be a character vector of length 1", call. = FALSE)
  if(!(is.character(dates) | is.null(dates) | "Date" %in% class(dates)))    stop("dates argument must be a character or vector in the
                                                                                 format YYYY-MM-DD or a vector of class Date",               call. = FALSE)
  if(!is.null(dates) & (!is.na(date.min) | !is.na(date.min)))            warning("date.min and date.max are ignored if dates is not NULL", .immediate = TRUE)

  ## SimulationName
  if(simu.names[1] != "all") {
    if(!all(simu.names %in% hop$exp.plan$SimulationName)) stop("one or more values in simu.names is not present in hop", call. = FALSE)
    profiles.to.check <- names(hop)[!(names(hop) %in% c("variables", "exp.path"))]
    profiles <- profiles.to.check[purrr::map_lgl(profiles.to.check, function(x) nrow(hop[[x]]) > 0)]
    for(i in profiles) { hop[[i]] <- dplyr::filter(hop[[i]], SimulationName %in% simu.names) }
    if(length(simu.names) == 1) { class(hop) <- class(hop)[class(hop) != "hop-group"] }
  }

  ## id
  if(tree.ids[1] != "all") {
    profiles.to.check <- c("annualtree", "trees", "tree.info")
    profiles <- profiles.to.check[purrr::map_lgl(profiles.to.check, function(x) nrow(hop[[x]]) > 0)]
    for(i in profiles) {
      if(!all(tree.ids %in% unique(hop[[i]]$id))) stop(paste0("one or more values of tree.id are not present in the ", i, " profile"), call. = FALSE)
      hop[[i]] <- dplyr::filter(hop[[i]], id %in% tree.ids)
    }
  }

  ## Date
  profiles.to.check <- names(hop)[!(names(hop) %in% c("exp.plan", "variables", "exp.path", "tree.info", "plot.info", "path"))]
  profiles <- profiles.to.check[purrr::map_lgl(profiles.to.check, function(x) nrow(hop[[x]]) > 0)]

  if(is.null(dates) & !is.na(date.min) & !is.na(date.max)) {
    date.min <- lubridate::ymd(date.min)
    date.max <- lubridate::ymd(date.max)

    get_date_range <- function(profile, h) range(h[[profile]]$Date)
    existing.ranges <- purrr::map(profiles, get_date_range, hop) %>%
      do.call(what = "c")
    if(is.na(date.min)) date.min <- min(existing.ranges)
    if(is.na(date.max)) date.max <- max(existing.ranges)

    for(i in profiles) { hop[[i]] <- dplyr::filter(hop[[i]], Date %in% seq(date.min, date.max, 1)) }
  } else if(!is.null(dates)) {
    dates <- lubridate::ymd(dates)
    for(i in profiles) {
      hop[[i]] <- dplyr::filter(hop[[i]], Date %in% dates)
    }
  }

  ## STRIP EXP PLAN VARS
  if(strip.exp.plan) {
    for(p in c("annualplot", "annualtree", "annualcrop", "plot", "trees", "cells", "voxels", "climate", "monthCells")) {
      if(nrow(hop[[p]]) > 0) {
        keep.cols <- c(1, which(names(hop[[p]]) == "Date"):ncol(hop[[p]]))
        hop[[p]] <- dplyr::select(hop[[p]], names(hop[[p]])[keep.cols])
      }
    }
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
#' new_hop <- hop_merge(hop1, hop2, hop3)
#' }
hop_merge <- function(...) {

  hops <- list(...)
  if(!all(purrr::map_lgl(hops, is_hop))) stop("one or more supplied objects not of class hop", call. = FALSE)

  make_names_unique <- function(x, num){ paste0(num, "-", x) }
  old.names <- purrr::map(purrr::map(hops, "exp.plan"), "SimulationName")

  if(any(duplicated(as.character(unlist(old.names))))) {
    hops <- purrr::pmap(list(hop       = hops,
                             old.names = old.names,
                             new.names = purrr::map2(old.names, 1:length(old.names), make_names_unique)),
                        hop_rename)
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

  merged_hop$exp.plan  <- dplyr::bind_cols(hip[, "SimulationName"], hip[,  unique.cols], hip[, other.cols])
  merged_hop$exp.plan  <- dplyr::select(merged_hop$exp.plan, "SimulationName", unique.cols)
  merged_hop$variables <- dplyr::distinct(merged_hop$variables)
  merged_hop$exp.path  <- NA

  class(merged_hop) <- c("hop-group", "hop", class(merged_hop))
  return(merged_hop)
}

#' Check if an object is of class hip
#' @description Checks if an object is of class hip
#' @return A logical.
#' @param x An object to check.
#' @export
#' @family hisafe helper functions
#' @examples
#' \dontrun{
#' is_hip(myhip)
#' }
is_hip <- function(hip, error = FALSE) {
  check <- (is.null(hip) | "hip" %in% class(hip))
  if(error) {
    if(!check) stop("hip argument not of class hip", call. = FALSE)
    invisible(check)
  } else {
    return(check)
  }
}

#' Check if an object is of class hop
#' @description Checks if an object is of class hop.
#' @return A logical.
#' @param x An object to check.
#' @export
#' @family hisafe helper functions
#' @examples
#' \dontrun{
#' is_hop(myhop)
#' }
is_hop <- function(hop, error = FALSE) {
  check <- (is.null(hop) | "hop" %in% class(hop))
  if(error) {
    if(!check) stop("hop argument not of class hop", call. = FALSE)
    invisible(check)
  } else {
    return(check)
  }
}

#' Check for existiance of profiles in a hop object
#' @description Checks for existiance of profiles in a hop object
#' @return A logical vector the same length as \code{profiles} indicating whether each profile is found in \code{hop}.
#' If \code{error} is \code{TRUE}, this vector is returned invisibly.
#' @param hop An object of class hop or face.
#' @param profiles A character vector of the names of the profiles to check for.
#' @param error Logical indicating whehter or not an error should be thrown if any profiles in \code{profiles} are not found.
#' @export
#' @family hisafe helper functions
#' @examples
#' \dontrun{
#' profile_check(myhop, "voxels")
#' }
profile_check <- function(hop, profiles, error = FALSE) {
  is_hop(hop, error = TRUE)
  not.supported <- profiles[!(profiles %in% SUPPORTED.PROFILES$profiles)]
  if(length(not.supported) > 0) stop(paste("The following profiles are not supported profiles:", paste(not.supported, collapse = ", ")), call. = FALSE)
  is_logical(x = error)

  check <- purrr::map_lgl(profiles, function(x) nrow(hop[[x]]) > 0)
  not.found <- profiles[!check]
  if(error) {
    if(length(not.found) > 0) {
      stop(paste("The following export profiles are required but not found in hop:",
                 paste(not.found, collapse = ", ")), call. = FALSE)
    }
    invisible(check)
  } else {
    return(check)
  }
}

#' Check for existiance of variables in a hop object
#' @description Checks for existiance of variables within a profile of a hop object
#' @return A logical vector the same length as \code{variables} indicating whether each variable is found in \code{hop}.
#' If \code{error} is \code{TRUE}, this vector is returned invisibly.
#' @param hop An object of class hop or face.
#' @param profile A character string of the name of the profile to check within.
#' @param variables A character vector of the names of the variables to check for.
#' @param error Logical indicating whehter or not an error should be thrown if any variables in \code{variables} are not found.
#' @export
#' @family hisafe helper functions
#' @examples
#' \dontrun{
#' variable_check(myhop, "trees", "carbonBranches")
#' }
variable_check <- function(hop, profile, variables, error = FALSE) {
  is_hop(hop, error = TRUE)
  profile_check(hop, profile, error = TRUE)
  if(!is.character(variables)) stop("variable(s) argument must be a character vector", call. = FALSE)
  is_logical(x = error)

  check <- purrr::map_lgl(variables, function(x) x %in% names(hop[[profile]]))
  not.found <- variables[!check]
  if(error) {
    if(length(not.found) > 0) {
      stop(paste0("The following variables were not found within the ", profile, " profile in hop: ",
                  paste(not.found, collapse = ", "),
                  "\nCheck spelling and capitalization of variable names.",
                  "\nEnsure that the variables were included within the output profile."),
           call. = FALSE)
    }
    invisible(check)
  } else {
    return(check)
  }
}

#' Shortcut to Hi-sAFe analysis
#' @description Runs the various Hi-sAFe analysis functions from a single call.
#' @return Invisibly returns \code{TRUE}.
#' @param hop An object of class hop or face.
#' @param carbon Logical indicating if annual carbon plot should be made.
#' @param light Logical indicating if annual light plot should be made.
#' @param nitrogen Logical indicating if annual nitrogen plot should be made.
#' @param water Logical indicating if annual water plot should be made.
#' @param carbon.daily Logical indicating if daily carbon plots should be made.
#' @param light.daily Logical indicating if daily light plots should be made.
#' @param nitrogen.daily Logical indicating if daily nitrogens plot should be made.
#' @param water.daily Logical indicating if daily water plots should be made.
#' @export
#' @family hisafe analysis functions
#' @examples
#' \dontrun{
#' analyze_hisafe(myhop)
#' }
analyze_hisafe <- function(hop,
                           carbon         = TRUE,
                           light          = TRUE,
                           nitrogen       = TRUE,
                           water          = TRUE,
                           carbon.daily   = TRUE,
                           light.daily    = TRUE,
                           nitrogen.daily = TRUE,
                           water.daily    = TRUE) {

  is_hop(hop, error = TRUE)
  if(!all(is.logical(c(carbon, light, nitrogen, water, light.daily, nitrogen.daily, water.daily)))) {
    stop("all arguments except for hop must be logicals", call. = FALSE)
  }

  annual.cycles.todo <- c("carbon", "light", "nitrogen", "water")[c(carbon, light, nitrogen, water)]
  daily.cycles.todo  <- c("carbon", "light", "nitrogen", "water")[c(carbon.daily, light.daily, nitrogen.daily, water.daily)]

  dir.create(clean_path(paste0(hop$exp.path, "/analysis/cycles/")), showWarnings = FALSE, recursive = TRUE)

  # ## SIMULATION CYCLES
  # if(length(annual.cycles.todo) >= 1) {
  #   cat("\n-- Plotting annual cycles")
  #   sim.cycle.plots <- purrr::map(annual.cycles.todo,
  #                                 plot_hisafe_cycle,
  #                                 hop = hop)
  #   purrr::walk2(paste0(hop$exp.path, "/analysis/cycles/", annual.cycles.todo, ".png"),
  #                sim.cycle.plots,
  #                ggsave_fitmax,
  #                scale = 2)
  # }

  ## ANNUAL CYCLES
  if(length(annual.cycles.todo) >= 1) {
    cat("\n-- Plotting annual cycles")
    annual.cycle.plots <- purrr::map(annual.cycles.todo,
                                     plot_hisafe_cycle_annual,
                                     hop = hop)
    purrr::walk2(paste0(hop$exp.path, "/analysis/cycles/", annual.cycles.todo, "_annual.png"),
                 annual.cycle.plots,
                 ggsave_fitmax,
                 scale = 2)
  }

  ## DAILY CYCLES
  if(length(daily.cycles.todo) >= 1) {
    cat("\n-- Plotting daily cycles")
    for(cycle in daily.cycles.todo) {
      daily.cycle.plots <- purrr::map(hop$exp.plan$SimulationName,
                                      plot_hisafe_cycle_daily,
                                      hop   = hop,
                                      cycle = cycle,
                                      years = "all")
      purrr::walk2(paste0(hop$exp.path, "/analysis/cycles/", cycle, "_", hop$exp.plan$SimulationName, ".png"),
                   daily.cycle.plots,
                   ggsave_fitmax,
                   scale = 2)
    }
  }

  invisible(TRUE)
}
