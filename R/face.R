#' Combine hop objects into a face object
#' @description Combines three hop objects for agroforestry, forestry, and monocrop simulations into a face object.
#' @return An object of class "face", which is a list of three objects of class "hop":
#' \itemize{
#'  \item{agroforestry}
#'  \item{forestry}
#'  \item{monocrop}
#' }
#' @param agroforestry An object of class "hop" containing one or more agroforestry simulations.
#' @param forestry An object of class "hop" containing a single simulation to serve as a forestry control.
#' @param monocrop An object of class "hop" containing a single simulation to serve as a monocrop control. This simulation must not contain any trees.
#' @param face.path A character string of the path to a directory where any face analyses should be written to.
#' @export
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulations for agroforestry, forestry, and monocrop simulations via:
#' AF.hop <- read_hisafe(myAFhip)
#' FC.hop <- read_hisafe(myFChip)
#' CC.hop <- read_hisafe(myCChip)
#'
#' # Combine these into a "face" object via:
#' my.face <- create_face(AF.hop, FC.hop, CC.hop)
#' }
create_face <- function(agroforestry, forestry, monocrop, face.path) {

  # hop checks
  AF.hop.check <- ifelse(!("hop" %in% class(agroforestry)), "-- agroforesry", "")
  FC.hop.check <- ifelse(!("hop" %in% class(forestry)),     "-- forestry",    "")
  CC.hop.check <- ifelse(!("hop" %in% class(monocrop)),     "-- monocrop",    "")
  hop.errors <- c(AF.hop.check, FC.hop.check, CC.hop.check)
  hop.errors <- paste0(hop.errors[!(hop.errors == "")], collapse = "\n")
  if(hop.errors != "") stop(paste("The following objects are not of class hop: ", hop.errors, collapse = "\n"), call. = FALSE)

  # Multiple control checks
  FC.multi.check <- ifelse("hop-group" %in% class(forestry), "-- forestry object can only contain a single simulation", "")
  CC.multi.check <- ifelse("hop-group" %in% class(monocrop), "-- monocrop object can only contain a single simulation", "")
  multi.errors <- c(FC.multi.check, CC.multi.check)
  multi.errors <- paste0(multi.errors[!(multi.errors == "")], collapse = "\n")
  if(multi.errors != "") stop(multi.errors, call. = FALSE)
  agroforestry$exp.path <- NULL

  # Profile checks
  nonempty_profiles <- function(x) {
    nonempty <- NULL
    for(i in names(x)) {
      if(nrow(x[[i]]) != 0) nonempty <- c(nonempty, i)
    }
    nonempty <- nonempty[!(nonempty %in% c("variables", "tree.info", "exp.plan", "path"))]
    return(sort(nonempty))
  }
  AF.profiles <- nonempty_profiles(agroforestry)
  FC.profiles <- nonempty_profiles(forestry)
  CC.profiles <- nonempty_profiles(monocrop)
  common.col <- c(AF.profiles, FC.profiles, CC.profiles)
  common.col <- names(table(common.col)[table(common.col) == 3])[1]
  if(is.na(common.col)) stop("There are no common data profiles with the agroforestry, forestry, and monocrop objects", call. = FALSE)
  if(!all(identical(AF.profiles[!(AF.profiles %in% c("annualtree", "trees"))], CC.profiles),
          identical(FC.profiles[!(FC.profiles %in% c("annualtree", "trees"))], CC.profiles),
          identical(AF.profiles, FC.profiles))) {
    profile.error <- c("agroforestry, forestry, and monocrop hops do not contain the same elements. They contain:",
                       paste0("   -- agroforestry: ", paste(AF.profiles, collapse = ", ")),
                       paste0("   -- forestry:     ",     paste(FC.profiles, collapse = ", ")),
                       paste0("   -- monocrop:     ",     paste(CC.profiles, collapse = ", ")))
    warning(paste(profile.error, collapse = "\n"), call. = FALSE, immediate. = TRUE)
  }

  # Tree checks
  AG.tree.check <- ifelse(any(unique(table(agroforestry$tree.info$SimulationName)) == 0),
                          "-- one or more of the agroforestry simulation does/do not contain any trees", "")
  FC.tree.check <- ifelse(nrow(forestry$tree.info) == 0,
                          "-- the forestry simulation does/do not contain any trees", "")
  CC.tree.check <- ifelse(nrow(monocrop$tree.info) > 0,
                          "-- the monocrop simulation contains trees", "")
  tree.errors <- c(AG.tree.check, FC.tree.check, CC.tree.check)
  tree.errors <- paste0(tree.errors[!(tree.errors == "")], collapse = "\n")
  if(tree.errors != "") stop(tree.errors, call. = FALSE)

  # Edit SimluationNames & add system column
  agroforestry <- simu_rename(agroforestry,
                              old.names = unique(agroforestry[[AF.profiles[1]]]$SimulationName),
                              new.names = paste0("AF-", unique(agroforestry[[AF.profiles[1]]]$SimulationName)))
  forestry <- simu_rename(forestry,
                              old.names = unique(forestry[[FC.profiles[1]]]$SimulationName),
                              new.names = "Forestry")
  monocrop <- simu_rename(monocrop,
                              old.names = unique(monocrop[[CC.profiles[1]]]$SimulationName),
                              new.names = "Monocrop")
  agroforestry <- augment_with_system(agroforestry, "Agroforestry")
  forestry     <- augment_with_system(forestry,     "Forestry")
  monocrop     <- augment_with_system(monocrop,     "Monocrop")

  # Remove irrelevant moncrop profiles
  monocrop[["annualtree"]] <- dplyr::tibble()
  monocrop[["trees"]]      <- dplyr::tibble()

  # Merge hops
  hops <- list(agroforestry, forestry, monocrop)
  merged_hop <- purrr::pmap(hops, dplyr::bind_rows)
  merged_hop$variables <- dplyr::distinct(merged_hop$variables)

  # Check numbers of years and warn if different
  year.summary <- merged_hop[[common.col]] %>%
    dplyr::group_by(System, SimulationName) %>%
    dplyr::summarize(n = dplyr::n_distinct(Year) - 1) %>%
    tidyr::unite(label, System, SimulationName, n, sep = ": ", remove = FALSE)
  year.summary$label <- gsub("NA: ", "", year.summary$label)
  if(length(unique(year.summary$n)) != 1) {
    year.length.warning <- paste(c("Simulation durations not equal!",
                                   "  Be careful when comparing simulations.",
                                   "  Simulation durations:",
                                   paste("   --", year.summary$label, "years")),
                                 collapse = "\n")
    warning(year.length.warning, call. = FALSE)
  }

  merged_hop$exp.path <- face.path

  class(merged_hop) <- c("face", "hop-group", "hop", class(merged_hop))
  return(merged_hop)
}


augment_with_system <- function(hop, system) {
  profiles.to.check <- c("annualtree", "annualcrop", "annualplot",
                         "trees", "plot", "climate", "roots",
                         "monthCells", "cells", "voxels",
                         "tree.info", "exp.plan", "path")
  profiles <- profiles.to.check[purrr::map_lgl(profiles.to.check, function(x) nrow(hop[[x]]) > 0)]
  for(i in profiles) {
    hop[[i]]$System <- system
    hop[[i]] <- dplyr::select(hop[[i]], System, everything())
  }
  return(hop)
}
