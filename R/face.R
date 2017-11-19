
create_face <- function(agroforestry, forestry, monocrop) {

  # hop checks
  AF.hop.check <- ifelse(!("hop" %in% class(agroforestry)), "-- agroforesry object not of class hop", "")
  FC.hop.check <- ifelse(!("hop" %in% class(forestry)),     "-- forestry object not of class hop",    "")
  CC.hop.check <- ifelse(!("hop" %in% class(monocrop)),     "-- monocrop object not of class hop",    "")
  hop.errors <- c(AF.hop.check, FC.hop.check, CC.hop.check)
  hop.errors <- paste0(hop.errors[!(hop.errors == "")], collapse = "\n")
  if(hop.errors != "") stop(hop.errors, call. = FALSE)

  # Multiple control checks
  FC.multi.check <- ifelse("hop-group" %in% class(forestry), "-- forestry object can only contain a single simulation", "")
  CC.multi.check <- ifelse("hop-group" %in% class(monocrop), "-- monocrop object can only contain a single simulation", "")
  multi.errors <- c(FC.multi.check, CC.multi.check)
  multi.errors <- paste0(multi.errors[!(multi.errors == "")], collapse = "\n")
  if(multi.errors != "") stop(multi.errors, call. = FALSE)

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

  # Add system column
  agroforestry <- augment_with_system(agroforestry, "Agroforestry")
  forestry     <- augment_with_system(forestry,     "Forestry")
  monocrop     <- augment_with_system(monocrop,     "Monocrop")

  face <- list(agroforestry = agroforestry, forestry = forestry, monocrop = monocrop)
  class(face) <- c("face", class(face))
  return(face)
}


augment_with_system <- function(hop, system) {
  profiles.to.check <- c("annualtree", "annualcrop", "annualplot",
                         "trees", "plot", "climate", "roots",
                         "monthCells", "cells", "voxels",
                         "tree.info", "exp.plan", "path")
  profiles <- profiles.to.check[purrr::map_lgl(profiles.to.check, function(x) nrow(hop[[x]]) > 0)]
  for(i in profiles) { hop[[i]]$System <- system }

  if(system == "Forestry") { for(i in profiles) { hop[[i]]$SimulationName <- NA } }
  if(system == "Monocrop") { for(i in profiles) { hop[[i]]$SimulationName <- NA } }

  return(hop)
}
