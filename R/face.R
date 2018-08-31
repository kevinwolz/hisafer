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

  if(!dir.exists(face.path)) stop("directory specfified by face.path does not exist", call. = FALSE)

  # Multiple control checks
  FC.multi.check <- ifelse("hop-group" %in% class(forestry), "-- forestry object can only contain a single simulation", "")
  CC.multi.check <- ifelse("hop-group" %in% class(monocrop), "-- monocrop object can only contain a single simulation", "")
  multi.errors <- c(FC.multi.check, CC.multi.check)
  multi.errors <- paste0(multi.errors[!(multi.errors == "")], collapse = "\n")
  if(multi.errors != "") stop(multi.errors, call. = FALSE)

  agroforestry$path <- forestry$path <- monocrop$path <- NULL

  # Profile checks
  AF.profiles <- which_profiles(agroforestry)
  FC.profiles <- which_profiles(forestry)
  CC.profiles <- which_profiles(monocrop)
  common.col <- c(AF.profiles, FC.profiles, CC.profiles)
  common.col <- names(table(common.col)[table(common.col) == 3])[1]
  if(is.na(common.col)) stop("There are no common data profiles with the agroforestry, forestry, and monocrop objects", call. = FALSE)
  if(!all(identical(AF.profiles[!(AF.profiles %in% c("trees", "tree.info"))], CC.profiles),
          identical(FC.profiles[!(FC.profiles %in% c("trees", "tree.info"))], CC.profiles),
          identical(AF.profiles, FC.profiles))) {
    profile.error <- c("agroforestry, forestry, and monocrop hops do not contain the same elements. They contain:",
                       paste0("   -- agroforestry: ", paste(AF.profiles, collapse = ", ")),
                       paste0("   -- forestry:     ", paste(FC.profiles, collapse = ", ")),
                       paste0("   -- monocrop:     ", paste(CC.profiles, collapse = ", ")))
    warning(paste(profile.error, collapse = "\n"), call. = FALSE, immediate. = TRUE)
  }

  # Tree checks
  AG.tree.check <- ifelse(!profile_check(agroforestry, "trees"), "-- one or more of the agroforestry simulation does/do not contain any trees", "")
  FC.tree.check <- ifelse(!profile_check(forestry,     "trees"), "-- the forestry simulation does not contain any trees", "")
  CC.tree.check <- ifelse(profile_check(monocrop,      "trees"), "-- the monocrop simulation contains trees", "")
  tree.errors <- c(AG.tree.check, FC.tree.check, CC.tree.check)
  tree.errors <- paste0(tree.errors[!(tree.errors == "")], collapse = "\n")
  if(tree.errors != "") stop(tree.errors, call. = FALSE)

  # Edit SimluationNames & add system column
  AF.simu.names <- unique(agroforestry[[AF.profiles[1]]]$SimulationName)
  if(length(AF.simu.names) == 1) {
    agroforestry <- hop_rename(agroforestry,
                               old.names = AF.simu.names,
                               new.names = "Agroforestry")
  }
  forestry <- hop_rename(forestry,
                         old.names = unique(forestry[[FC.profiles[1]]]$SimulationName),
                         new.names = "Forestry")
  monocrop <- hop_rename(monocrop,
                         old.names = unique(monocrop[[CC.profiles[1]]]$SimulationName),
                         new.names = "Monocrop")
  agroforestry <- augment_with_system(agroforestry, "Agroforestry")
  forestry     <- augment_with_system(forestry,     "Forestry")
  monocrop     <- augment_with_system(monocrop,     "Monocrop")

  # Merge hops
  merged_hop <- hop_merge(agroforestry, forestry, monocrop, path = face.path)
  class(merged_hop) <- c("face", "hop-group", "hop", class(merged_hop))

  return(merged_hop)
}


#' Augment hop elements with system name
#' @description Adds a "System" column to all hop elements
#' @return Returns the modified hop object
#' @param hop An object of class "hop" to augment
#' @param system A character string of the system name to use
#' @keywords internal
augment_with_system <- function(hop, system) {
  profiles <- which_profiles(hop = hop, profiles = FILTERABLE.ELEMENTS)
  for(i in profiles) {
    hop[[i]]$System <- system
    hop[[i]] <- dplyr::select(hop[[i]], System, dplyr::everything())
  }
  return(hop)
}
