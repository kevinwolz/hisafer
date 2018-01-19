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

#' Display supported Hi-sAFe input parameters
#' @description Displays supported Hi-sAFe input parameters, their default values, and their accepted/suggested ranges.
#' @return Invisibly returns an alphebetized character vector of the names of supported Hi-sAFe prameters.
#' @param variable If "names", the default, then just the names of supported Hi-sAFe parameters is printed to the console.
#' If "all", then the names, default values, and accepted/suggested ranges of supported Hi-sAFe parameters is printed.
#' Can also be a character vector of specific Hi-sAFe parameters of which to display details.
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
#' hisafe_params()            # just for parameter names
#' hisafe_params("cellWidth") # details of cellWidth parameter
#' hisafe_params("all")       # details of all parameters
#' }
hisafe_params <- function(variable = "names", search = FALSE, template = "agroforestry_default") {

  if(!is.character(variable))                           stop("variable argument must be a character vector",                call. = FALSE)
  if(!(is.character(template) & length(template) == 1)) stop("template argument must be a character vector of length 1",    call. = FALSE)
  if(search & length(variable) > 1)                     stop("search = TRUE is only possible with a single variable input", call. = FALSE)

  TEMPLATE_PARAMS <- get_template_params(get_template_path(template))
  PARAM_NAMES     <- sort(unlist(get_param_names(TEMPLATE_PARAMS), use.names = FALSE))
  PARAM_DEFAULTS  <- get_param_vals(TEMPLATE_PARAMS, "value")

  acceptable <- c(PARAM_NAMES, "names", "all")
  if(any(!(variable %in% acceptable))) {
    bad.vars <- sort(variable[!(variable %in% acceptable)])
    close.matches  <- purrr::map(bad.vars, stringdist::stringdist, b = PARAM_NAMES)
    suggested.vars <- PARAM_NAMES[unlist(purrr::map(close.matches, which.min))]
    if(!search){
      stop(paste0("The following are not supported Hi-sAFe input parameters: ", paste(bad.vars, collapse = ", "),
                  "\n       Did you mean: ", paste(suggested.vars, collapse = " or "), "?"), call. = FALSE)
    } else {
      cat(paste0("The following are not supported Hi-sAFe input parameters: ", paste(bad.vars, collapse = ", "),
                 "\n Parameter names will be searched for these as regular expressions."))
    }
  }

  if(variable[1] == "all") {
    for(i in 1:length(PARAM_NAMES)){
      var.def <- dplyr::filter(PARAM.DEFS, name == PARAM_NAMES[i])
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
    for(i in 1:length(variable)){
      if(search) {
        var.def <- dplyr::filter(PARAM.DEFS, stringr::str_detect(name, variable[i]))
        if(nrow(var.def) == 0) stop(paste(variable[i], "was not detected in any Hi-sAFe input parameter names"), call. = FALSE)
      } else {
        var.def <- dplyr::filter(PARAM.DEFS, name == variable[i])
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
#' hop_rename(myhop, old.names = c("Sim_1", "Sim_2"), new.names = c("Lat30", "Lat60"))
#' }
hop_rename <- function(hop, old.names, new.names) {
  if(!("hop" %in% class(hop)))                         stop("hop argument is not of class hop",                      call. = FALSE)
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

  if(!("hop" %in% class(hop)))                          stop("hop argument is not of class hop",                       call. = FALSE)
  if(!all(simu.names %in% hop$exp.plan$SimulationName)) stop("one or more values in simu.names is not present in hop", call. = FALSE)

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

#' Filter a hop object by date
#' @description Filters a hop object by date.
#' @return A hop object.
#' @param hop A object of class hop or face.
#' @param date.min A character string of the minimum date to keep in the format "YYYY-MM-DD".
#' If NA, the minimum date in \code{hop} is used.
#' @param date.max A character string of the maximum date to keep in the format "YYYY-MM-DD".
#' If NA, the maximum date in \code{hop} is used.
#' @export
#' @family hisafe helper functions
#' @examples
#' \dontrun{
#' newhop <- hop_date_filter(myhop, NA, "2010-01-01")
#' }
hop_date_filter <- function(hop, date.min = NA, date.max = NA) {
  if(!("hop" %in% class(hop)))                                              stop("hop argument is not of class hop",                         call. = FALSE)
  if(!(length(date.min) == 1 & (is.character(date.min) | is.na(date.min)))) stop("date.min argument must be a character vector of length 1", call. = FALSE)
  if(!(length(date.max) == 1 & (is.character(date.max) | is.na(date.max)))) stop("date.max argument must be a character vector of length 1", call. = FALSE)

  date.min <- lubridate::ymd(date.min)
  date.max <- lubridate::ymd(date.max)

  profiles.to.check <- names(hop)[!(names(hop) %in% c("exp.plan", "variables", "exp.path", "tree.info", "plot.info", "path"))]
  profiles <- profiles.to.check[purrr::map_lgl(profiles.to.check, function(x) nrow(hop[[x]]) > 0)]

  get_date_range <- function(profile, h) range(h[[profile]]$Date)
  existing.ranges <- purrr::map(profiles, get_date_range, hop) %>%
    do.call(what = "c")
  if(is.na(date.min)) date.min <- min(existing.ranges)
  if(is.na(date.max)) date.max <- max(existing.ranges)

  for(i in profiles) { hop[[i]] <- dplyr::filter(hop[[i]], Date %in% seq(date.min, date.max, 1)) }
  return(hop)
}
