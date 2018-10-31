#' Display version numbers of Hi-sAFe and Java
#' @description Displays the version numbers of Hi-sAFe and Java.
#' @return Invisibly returns the Hi-sAFe version number
#' @param capsis.path A character string of the path to the Capsis folder
#' @export
#' @family hisafe helper functions
#' @examples
#' \dontrun{
#' hisafe_info()
#' }
hisafe_info <- function(capsis.path) {

  capsis.path <- get_absolute_path(capsis.path)

  if(!dir.exists(capsis.path))                    stop("directory specified by capsis.path does not exist",          call. = FALSE)
  if(!("capsis.sh" %in% list.files(capsis.path))) stop("directory specified by capsis.path does not contain Capsis", call. = FALSE)

  hisafe.id.card <- clean_path(paste0(capsis.path, "/src/safe/session.txt"))
  hisafe.info <- scan(hisafe.id.card, what = "character", encoding = "latin1", sep = "\n", quiet = TRUE) %>%
    .[-1] %>%
    purrr::map(strsplit, split = " = ") %>%
    purrr::map(1) %>%
    purrr::map(2) %>%
    as.data.frame(col.names = c("hisafe", "stics", "capsis"), stringsAsFactors = FALSE) %>%
    dplyr::as_tibble()

  cat("\nHi-sAFe Version:", hisafe.info$hisafe)
  cat("\nCapsis Version:",  hisafe.info$capsis)
  cat("\nSTICS Version:",   hisafe.info$stics)
  cat("\nJava Version:")
  system("java -version", wait = TRUE)

  invisible(hisafe.info)
}

#' Copy a Hi-sAFe template to specified location
#' @description Copies a Hi-sAFe template to specified location.
#' @return Invisibly returns a logical vector indicating if the attempted fily copy succeeded.
#' @param template A character string of the path to the Hi-sAFe directory structure/files to use as a template
#' (or one of the strings signaling a default template)
#' See \code{\link{define_hisafe}} for more details on available default templates.
#' @param destination A character string of the path to where the template folder should be copied.
#' @param overwrite A logical indicating whether or not to allow overwriting of an existing folder.
#' @param new.name A character string of the a name for the newly copied folder.
#' If \code{NULL}, the default, then the name will remain the same as the original template folder.
#' @export
#' @family hisafe helper functions
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
    if(requireNamespace("stringdist", quietly = TRUE)) {
      close.matches  <- purrr::map(bad.vars, stringdist::stringdist, b = PARAM_NAMES)
      suggested.vars <- PARAM_NAMES[unlist(purrr::map(close.matches, which.min))]
      stop(paste0("The following are not supported Hi-sAFe input parameters: ", paste(bad.vars, collapse = ", "),
                  "\n       Did you mean: ", paste(suggested.vars, collapse = " or "), "?"), call. = FALSE)
    } else {
      stop(paste0("The following are not supported Hi-sAFe input parameters: ", paste(bad.vars, collapse = ", "),
                  "\nPlease install the 'stringdist' package for hip_params() to provide suggestions."), call. = FALSE)
    }
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
        var.def <- dplyr::filter(INPUT.DEFS, stringr::str_detect(tolower(name), tolower(variable[i])))
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
#' @param quiet Logical indicating whether or not to supress output printed to console.
#' @export
#' @family hisafe helper functions
#' @examples
#' \dontrun{
#' hop_params()                 # just for parameter names
#' hop_params("carbonBranches") # details of cellWidth parameter
#' hop_params("all")            # details of all parameters
#' }
hop_params <- function(variable = "names", search = FALSE, quiet = FALSE) {

  if(!is.character(variable))                           stop("variable argument must be a character vector",                call. = FALSE)
  if(search & length(variable) > 1)                     stop("search = TRUE is only possible with a single variable input", call. = FALSE)

  acceptable <- c(OUTPUT.DEFS$name, "names", "all")
  if(any(!(variable %in% acceptable)) & !search & !quiet) {
    bad.vars <- sort(variable[!(variable %in% acceptable)])
    if(requireNamespace("stringdist", quietly = TRUE)) {
      close.matches  <- purrr::map(bad.vars, stringdist::stringdist, b = OUTPUT.DEFS$name)
      suggested.vars <- OUTPUT.DEFS$name[unlist(purrr::map(close.matches, which.min))]
      stop(paste0("The following are not supported Hi-sAFe output parameters: ", paste(bad.vars, collapse = ", "),
                  "\n       Did you mean: ", paste(suggested.vars, collapse = " or "), "?"), call. = FALSE)
    } else {
      stop(paste0("The following are not supported Hi-sAFe output parameters: ", paste(bad.vars, collapse = ", "),
                  "\nPlease install the 'stringdist' package for hop_params() to provide suggestions."), call. = FALSE)
    }
  }

  if(variable[1] == "all") {
    for(i in 1:nrow(OUTPUT.DEFS)){
      var.def <- OUTPUT.DEFS[i, ]
      if(i == 1) cat(var.def$name) else if(!quiet) cat("\n\n", var.def$name)
      if(!quiet) {
        cat("\n  -- Output profile:", var.def$profile)
        cat("\n  -- Definition:",     var.def$definition)
        cat("\n  -- Units:",          var.def$unit)
      }
    }
  } else if (variable[1] == "names" & !quiet) {
    cat(paste0(OUTPUT.DEFS$profile, " - ", OUTPUT.DEFS$name, collapse = "\n"))
  } else {
    if(search & !quiet) cat("'variable' values will be searched as regular expressions.")
    for(i in 1:length(variable)){
      if(search) {
        var.def <- dplyr::filter(OUTPUT.DEFS, stringr::str_detect(tolower(name), tolower(variable[i])))
        if(nrow(var.def) == 0) {
          if(!quiet) cat("\n\n  --", paste(variable[i], "was not detected in any Hi-sAFe output parameter names"))
          next
        }
      } else {
        var.def <- dplyr::filter(OUTPUT.DEFS, name == variable[i])
      }
      if(!quiet) {
        for(j in 1:nrow(var.def)){
          cat("\n\n", var.def$name[j])
          cat("\n  -- Output profile:", var.def$profile[j])
          cat("\n  -- Definition:",     var.def$definition[j])
          cat("\n  -- Units:",          var.def$unit[j])
        }
      }
    }
    invisible(var.def)
  }
}

#' Display supported Hi-sAFe output profiles
#' @description Displays supported Hi-sAFe output profiles and standard output frequency.
#' @export
#' @family hisafe helper functions
#' @examples
#' \dontrun{
#' hisafe_profiles()
#' }
hisafe_profiles <- function() {
  print(as.data.frame(PUBLIC.PROFILES), right = FALSE, row.names = FALSE)
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

  profiles <- which_profiles(hop = hop, profiles = FILTERABLE.ELEMENTS)

  existing.names <- unique(hop[[profiles[1]]]$SimulationName)
  missing.names  <- existing.names[!(unique(existing.names) %in% old.names)]
  old.names <- c(old.names, missing.names)
  new.names <- c(new.names, missing.names)

  for(i in profiles) {
    hop[[i]]$SimulationName <- new.names[match(hop[[i]]$SimulationName, old.names)]
  }

  return(hop)
}

#' Filter a hop object by SimulationName, Date, and idTree
#' @description Filters a hop object by SimulationName, Date, and idTree
#' @return A hop object.
#' @param hop An object of class hop or face.
#' @param simu.names A character vector of the SimulationNames to keep. If "all", no filtering occurs.
#' @param tree.ids A numeric vector of the tree ids to keep. If "all", no filtering occurs.
#' @param years A numeric vector of the years to keep. If "all", no filtering occurs.
#' @param months A numeric vector of the months to keep. If "all", no filtering occurs.
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
                       years          = "all",
                       months         = "all",
                       date.min       = NA,
                       date.max       = NA,
                       dates          = NULL,
                       strip.exp.plan = FALSE) {
  is_hop(hop, error = TRUE)
  if(!all(is.character(simu.names)))                                        stop("simu.names argument must be 'all' or a character vector",  call. = FALSE)
  if(!(years[1]    == "all" | all(is.numeric(years))))                      stop("years argument must be 'all' or a numeric vector",         call. = FALSE)
  if(!(months[1]   == "all" | all(is.numeric(months))))                     stop("months argument must be 'all' or a numeric vector",        call. = FALSE)
  if(!(tree.ids[1] == "all" | all(is.numeric(tree.ids))))                   stop("tree.ids argument must be 'all' or a numeric vector",      call. = FALSE)

  date_check <- function(x) is.character(x) | is.na(x) |  "Date" %in% class(x)
  if(!(length(date.min) == 1 & date_check(date.min)))                       stop("date.min argument must be a character vector of length 1", call. = FALSE)
  if(!(length(date.max) == 1 & date_check(date.max)))                       stop("date.max argument must be a character vector of length 1", call. = FALSE)
  if(!(is.character(dates) | is.null(dates) | "Date" %in% class(dates)))    stop("dates argument must be a character or vector in the
                                                                                 format YYYY-MM-DD or a vector of class Date",               call. = FALSE)
  if(!is.null(dates) & (!is.na(date.min) | !is.na(date.min)))            warning("date.min and date.max are ignored if dates is not NULL", .immediate = TRUE)

  ## SimulationName
  if(simu.names[1] != "all") {
    if(!all(simu.names %in% hop$exp.plan$SimulationName)) stop("one or more values in simu.names is not present in hop", call. = FALSE)
    profiles <- which_profiles(hop = hop, profiles = FILTERABLE.ELEMENTS)
    for(i in profiles) hop[[i]] <- dplyr::filter(hop[[i]], SimulationName %in% simu.names)
    if(length(simu.names) == 1) class(hop) <- class(hop)[class(hop) != "hop-group"]
  }

  ## idTree
  if(tree.ids[1] != "all") {
    profiles <- which_profiles(hop = hop, profiles = c("trees", "tree.info"))
    for(i in profiles) {
      if(!all(tree.ids %in% unique(hop[[i]]$idTree))) stop(paste0("one or more values of tree.ids are not present in the ", i, " profile"), call. = FALSE)
      hop[[i]] <- dplyr::filter(hop[[i]], idTree %in% tree.ids)
    }
  }

  time.profiles <- which_profiles(hop = hop, profiles = DATA.PROFILES)

  ## Year
  if(years[1]  != "all") for(i in time.profiles) hop[[i]] <- dplyr::filter(hop[[i]], Year  %in% years)

  ## Month
  if(months[1] != "all") for(i in time.profiles) hop[[i]] <- dplyr::filter(hop[[i]], Month %in% months)

  ## Date
  if(is.null(dates) & (!is.na(date.min) | !is.na(date.max))) {
    date.min <- lubridate::ymd(date.min)
    date.max <- lubridate::ymd(date.max)

    get_date_range <- function(profile, h) range(h[[profile]]$Date)
    existing.ranges <- purrr::map(time.profiles, get_date_range, hop) %>%
      do.call(what = "c")
    if(is.na(date.min)) date.min <- min(existing.ranges)
    if(is.na(date.max)) date.max <- max(existing.ranges)

    if(date.max < date.min) stop("date.min must be less than date.max", call. = FALSE)

    for(i in time.profiles) hop[[i]] <- dplyr::filter(hop[[i]], Date %in% seq(date.min, date.max, 1))
  } else if(!is.null(dates)) {
    dates <- lubridate::ymd(dates)
    for(i in time.profiles) hop[[i]] <- dplyr::filter(hop[[i]], Date %in% dates)
  }

  ## STRIP EXP PLAN VARS
  if(strip.exp.plan) {
    for(p in DATA.PROFILES) {
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
#' @param path A character string to be stored in \code{hop$path}.
#' @export
#' @importFrom dplyr %>%
#' @family hisafe helper functions
#' @examples
#' \dontrun{
#' new_hop <- hop_merge(hop1, hop2, hop3)
#' }
hop_merge <- function(..., path) {

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
    x$path <- NULL
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
  merged_hop$path      <- path

  class(merged_hop) <- c("hop-group", "hop", class(merged_hop))

  # Check numbers of years and warn if different
  dum <- warn_unequal_lengths(merged_hop)

  return(merged_hop)
}

#' Check if an object is of class hip
#' @description Checks if an object is of class hip
#' @return A logical.
#' @param hip An object to check.
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
#' @param hop An object to check.
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

#' Check if an object is of class face
#' @description Checks if an object is of class face.
#' @return A logical.
#' @param face An object to check.
#' @export
#' @family hisafe helper functions
#' @examples
#' \dontrun{
#' is_face(myface)
#' }
is_face <- function(face, error = FALSE) {
  check <- (is.null(face) | "face" %in% class(face))
  if(error) {
    if(!check) stop("face argument not of class face", call. = FALSE)
    invisible(check)
  } else {
    return(check)
  }
}


#' Check for existiance of profiles in a hop object
#' @description Checks for existiance of profiles in a hop object
#' @return A logical vector the same length as \code{profiles} indicating whether each profile is found in \code{hop}.
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
  is_TF(x = error)

  not.supported <- profiles[!(profiles %in% FILTERABLE.ELEMENTS)]
  if(length(not.supported) > 0) stop(paste("The following profiles are not supported profiles:", paste(not.supported, collapse = ", ")), call. = FALSE)

  check <- purrr::map_lgl(profiles, function(x) nrow(hop[[x]]) > 0)
  not.found <- profiles[!check]
  if(error) {
    if(length(not.found) > 0) {
      stop(paste("The following export profiles are required but not found in hop:",
                 paste(not.found, collapse = ", ")), call. = FALSE)
    }
  } else {
    return(check)
  }
}

#' Get which profiles exist in a hop
#' @description Gets names of which profiles exist in a hop.
#' @return A character vector of the available profiles in the hop.
#' @param hop An object of class hop or face.
#' @param profiles A character vector of the names of the profiles to check for.
#' If \code{NULL}, returns the names of all available profiles.
#' @export
#' @family hisafe helper functions
#' @examples
#' \dontrun{
#' what_profiles(myhop, c("cells", "voxels"))
#' }
which_profiles <- function(hop, profiles = NULL) {
  if(is.null(profiles)) profiles <- FILTERABLE.ELEMENTS
  return(profiles[profile_check(hop = hop, profiles = profiles)])
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
  is_TF(x = error)

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
#' @return Invisibly returns \code{hop}.
#' @param hop An object of class hop or face.
#' @param carbon Logical indicating if annual carbon plot should be made.
#' @param light Logical indicating if annual light plot should be made.
#' @param nitrogen Logical indicating if annual nitrogen plot should be made.
#' @param water Logical indicating if annual water plot should be made.
#' @param carbon.daily Logical indicating if daily carbon plots should be made.
#' @param light.daily Logical indicating if daily light plots should be made.
#' @param nitrogen.daily Logical indicating if daily nitrogens plot should be made.
#' @param water.daily Logical indicating if daily water plots should be made.
#' @param carbon.increment Logical indicating if daily carbon increment plots should be made.
#' @param carbon.allocation Logical indicating if daily carbon allocation plots should be made.
#' @param tree.ids A numeric vector indicating a subset of tree ids to plot. Use "all" to include all available values.
#' This only applies for carbon-related plots.
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
                           water.daily    = TRUE,
                           carbon.increment  = TRUE,
                           carbon.allocation = TRUE,
                           tree.ids          = "all") {

  is_hop(hop, error = TRUE)
  if(!all(is.logical(c(carbon, light, nitrogen, water, light.daily, nitrogen.daily, water.daily, carbon.increment, carbon.allocation)))) {
    stop("all arguments except for hop must be logicals", call. = FALSE)
  }

  annual.cycles.todo <- c("carbon", "light", "nitrogen", "water")[c(carbon, light, nitrogen, water)]
  daily.cycles.todo  <- c("carbon", "light", "nitrogen", "water",
                          "carbon-increment", "carbon-allocation")[c(carbon.daily, light.daily, nitrogen.daily, water.daily,
                                                                     carbon.increment, carbon.allocation)]

  dir.create(clean_path(paste0(hop$path, "/analysis/cycles/")), showWarnings = FALSE, recursive = TRUE)

  ## ANNUAL CYCLES
  if(length(annual.cycles.todo) >= 1) {
    cat("\n-- Plotting annual cycles")
    annual.cycle.plots <- purrr::map(annual.cycles.todo,
                                     plot_hisafe_cycle_bar,
                                     hop      = hop,
                                     tree.ids = tree.ids)
    purrr::walk2(paste0(hop$path, "/analysis/cycles/", annual.cycles.todo, "_annual.png"),
                 annual.cycle.plots,
                 ggsave_fitmax,
                 scale = 2)
  }

  ## DAILY CYCLES
  if(length(daily.cycles.todo) >= 1) {
    cat("\n-- Plotting daily cycles")
    for(cycle in daily.cycles.todo) {
      daily.cycle.plots <- purrr::map(hop$exp.plan$SimulationName,
                                      plot_hisafe_cycle_ts,
                                      hop      = hop,
                                      cycle    = cycle,
                                      years    = "all",
                                      tree.ids = tree.ids)
      purrr::walk2(paste0(hop$path, "/analysis/cycles/", cycle, "_", hop$exp.plan$SimulationName, ".png"),
                   daily.cycle.plots,
                   ggsave_fitmax,
                   scale = 2)
    }
  }

  invisible(hop)
}

#' Write hop profiles to CSV files
#' @description Writes hop profiles to CSV files.
#' @return Invisibly returns \code{hop}.
#' @param hop An object of class hop or face.
#' @param profiles The profiles which to each write as a CSV.
#' @param output.path A character string indicating the path to the directory where CSV files should be saved.
#' Plots aresaved in a subdirectory within this directory named \code{/analysis/combined_outputs/}.
#' If \code{NULL}, the experiment/simulation path is read from the hop object.
#' @export
#' @family hisafe analysis functions
#' @examples
#' \dontrun{
#' write_hop(myhop)
#' }
write_hop <- function(hop, profiles = "all", output.path = NULL) {
  is_hop(hop, error = TRUE)

  if(profiles[1] == "all") profiles <- DATA.PROFILES
  if(!all(profiles %in% DATA.PROFILES)) stop(paste0("profiles argument must be 'all' or one or more of ",
                                                    paste(DATA.PROFILES, collapse = ", ")), call. = FALSE)
  profiles <- c(which_profiles(hop = hop, profiles = profiles), "metadata")

  if(is.null(output.path)) {
    output.path <- hop$path
  } else if(!is.character(output.path)) stop("output.path argument must be NULL or a character string", call. = FALSE)

  dir.create(paste0(output.path, "/analysis/combined_outputs"), recursive = TRUE, showWarnings = FALSE)
  write_profile <- function(profile, hop, output.path) {
    readr::write_csv(hop[[profile]], paste0(output.path, "/analysis/combined_outputs/", basename(output.path), "_", profile, ".csv"))
  }
  purrr::walk(profiles, write_profile, hop = hop, output.path = output.path)

  invisible(hop)
}

#' Join multiple hop profiles together
#' @description Joins multiple hop profiles together into a single tibble (data.frame) using \code{dplyr::full_join()}.
#' When joining the 'cells' profile with the 'trees', 'plot', or 'climate' profiles, only numeric columns from the 'cells'
#' profile are kept, and values are averaged across all cells before joining.
#' When joining the 'voxels' profile with the 'cells' profile, only numeric columns from the 'voxels'
#' profile are kept, and values are summed across all voxels in each cell before joining.
#' The 'voxels' profile cannot be joinged with the 'trees', 'plot', or 'climate' profiles.
#' @return A tibble (data.frame)
#' @param hop An object of class hop or face.
#' @param profiles The profiles to join
#' @param ... Other arguments passed to \code{\link{hop_filter}}
#' @export
#' @family hisafe helper functions
#' @examples
#' \dontrun{
#' my.df <- join_profiles(hop, c("trees", "plot"))
#' }
join_profiles <- function(hop, profiles, ...) {
  is_hop(hop, error = TRUE)
  profile_check(hop = hop, profiles = profiles, error = TRUE)
  hop <- hop_filter(hop, strip.exp.plan = TRUE, ...)

  group1 <- c("trees", "plot", "climate")
  group2 <- c("cells")
  group3 <- c("voxels")
  core.join.cols <- c("SimulationName", "Date", "Day", "Month", "Year", "JulianDay")

  if(all(profiles %in% group1)){
    out <- hop[profiles] %>%
      purrr::reduce(dplyr::full_join, by = core.join.cols)
  } else if(all(profiles %in% c(group2, group3))){
    hop$voxels <- hop$voxels %>%
      dplyr::group_by_at(c(core.join.cols, "idCell")) %>%
      dplyr::summarize_if(is.numeric, sum) %>%
      dplyr::ungroup()
    out <- hop[profiles] %>%
      purrr::reduce(dplyr::full_join, by = c(core.join.cols, "idCell"))
  } else if(all(profiles %in% c(group1, group2))){
    hop$cells <- hop$cells %>%
      dplyr::group_by_at(core.join.cols) %>%
      dplyr::summarize_if(is.numeric, mean) %>%
      dplyr::ungroup()
    out <- hop[profiles] %>%
      purrr::reduce(dplyr::full_join, by = core.join.cols)
  } else if(all(profiles %in% c(group1, group3))){
    stop("Cannot join voxels profile with trees, plot, or climate profiles.", call. = FALSE)
  } else {
    stop(paste("join_profiles() only supports the following profiles:", paste(c(group1, group2, group3), collapse = ", ")), call. = FALSE)
  }
  return(out)
}

#' Get branch pruning dates from a hop
#' @description Gets branch pruning dates from a hop in a format suitable for supplementing plots.
#' @return A tibble (data.frame)
#' @param hop An object of class hop or face.
#' @param type One of "branch" or "root".
#' @param tree.ids A numeric vector of the values of idTree to include.
#' @export
#' @family hisafe helper functions
#' @examples
#' \dontrun{
#' pruning.dates <- get_pruning_dates(hop)
#' }
get_pruning_dates <- function(hop, type = "branch", tree.ids = 1) {
  is_hop(hop, error = TRUE)
  profile_check(hop, "tree.info", error = TRUE)

  hop <- hop_filter(hop = hop, tree.ids = tree.ids)

  if(type == "branch") {
    pruning.data <- hop$tree.info %>%
      dplyr::rename(pruningYears = treePruningYears) %>%
      dplyr::rename(pruningDays  = treePruningDays)
  } else if(type == "root") {
    pruning.data <- hop$tree.info %>%
      dplyr::rename(pruningYears = treeRootPruningYears) %>%
      dplyr::rename(pruningDays  = treeRootPruningDays)
  } else {
    stop("type argument must be one of 'branch' or 'root'", call. = FALSE)
  }

  get_dates <- function(x) {
    pruningYears <- unlist(x$pruningYears) + x$simulationYearStart - 1 + as.numeric(unlist(x$pruningDays) < x$simulationDayStart)
    if(!is.na(pruningYears[1])) {
      Date <- lubridate::ymd(paste0(pruningYears, "-01-01")) + unlist(x$pruningDays) - 1
      out <- dplyr::tibble(SimulationName = x$SimulationName, idTree = x$idTree, Year = as.integer(pruningYears), Date = Date) %>%
        dplyr::mutate(Month = lubridate::month(Date)) %>%
        dplyr::mutate(Day   = lubridate::day(Date)) %>%
        dplyr::select(SimulationName, idTree, Year, Month, Day, Date)
    } else {
      out <- dplyr::tibble()
    }
    return(out)
  }

  pruning.data <- pruning.data %>%
    split(seq(nrow(.)))
  out <- purrr::map_df(pruning.data, get_dates)
  return(out)
}

#' Get dates of tree phenological stage changes from a hop
#' @description Gets dates of tree phenological stage changes from a hop in a format suitable for supplementing plots.
#' @return A tibble (data.frame)
#' @param hop An object of class hop or face.
#' @param tree.ids A numeric vector of the values of idTree to include.
#' @export
#' @family hisafe helper functions
#' @examples
#' \dontrun{
#' pheno.dates <- get_pheno_dates(hop)
#' }
get_pheno_dates <- function(hop, tree.ids = 1) {
  is_hop(hop, error = TRUE)
  profile_check(hop, "trees", error = TRUE)
  variable_check(hop, "trees", "phenologicalStage", error = TRUE)
  if(!is.numeric(tree.ids)) stop("tree.ids argument must be a numeric vector", call. = FALSE)

  out <- hop$trees %>%
    dplyr::filter(idTree %in% tree.ids) %>%
    dplyr::arrange(SimulationName, idTree, Date) %>%
    dplyr::mutate(dif = phenologicalStage - c(NA, phenologicalStage[-nrow(.)])) %>%
    dplyr::filter(dif != 0) %>%
    dplyr::select(SimulationName, idTree, Year, Month, Day, Date, JulianDay, phenologicalStage)
  return(out)
}

#' Provide warning if simulation lengths are unequal
#' @description  Provides warning if simulation lengths are unequal.
#' @return A tibble (data.frame) containing the SimulationName and durations in years.
#' @param hop An object of class hop or a hop-like object.
#' @keywords internal
warn_unequal_lengths <- function(hop) {
  year.summary <- hop[[as.numeric(which.max(purrr::map_int(hop[names(hop) %in% DATA.PROFILES], nrow)))]] %>%
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
  return(dplyr::select(year.summary, -label))
}

#' Convert absolute years to relative years in a hop
#' @description Converts absolute years to relative years (minimum year is year 1) in the Year column of all hop elements.
#' Does NOT convert dates in the Date column.
#' @return A hop
#' @param hop An object of class hop or face.
#' @param year1 The year that should be treated as year 1. Must be less than or equal to the minimum year in the hop.
#' @export
#' @family hisafe helper functions
#' @examples
#' \dontrun{
#' range(hop$trees$Year)
#' hop.mod <- make_rel_years(hop)
#' range(hop.mod$trees$Year)
#' }
make_rel_years <- function(hop, year1 = NULL) {
  profiles <- which_profiles(hop = hop, profiles = DATA.PROFILES)
  if(is.null(year1)) year1 <- min(hop[[profiles[1]]]$Year)
  for(i in profiles) {
    #hop[[i]]$Date <- hop[[i]]$Date - lubridate::years(year1) + 1
    hop[[i]]$Year <- hop[[i]]$Year - year1 + 1
  }
  return(hop)
}

#' Shift Year column based on a provided JulianDay
#' @description Shifts Year column based on a provided JulianDay.
#' @return A hop object.
#' @param hop An object of class hop or a hop-like object.
#' @param doy.start The JulianDay [1-365] on which to start the annual cycle accounting. Use 'sim' to specify the starting JulianDay of the simulation.
#' @keywords internal
shift_year <- function(hop, doy.start) {
  profiles <- which_profiles(hop = hop, profiles = DATA.PROFILES)
  if(doy.start == "sim") {
    profile_check(hop, "plot.info", error = TRUE)
    plot.info <- dplyr::select(hop$plot.info, SimulationName, simulationDayStart)
    for (i in profiles) hop[[i]] <- dplyr::left_join(hop[[i]], plot.info, by = "SimulationName")
  } else {
    for(i in profiles) hop[[i]]$simulationDayStart <- doy.start
  }
  for(i in profiles) {
    hop[[i]]$Year <- hop[[i]]$Year - as.numeric(hop[[i]]$JulianDay < hop[[i]]$simulationDayStart)
    hop[[i]]$simulationDayStart <- NULL
  }

  return(hop)
}
