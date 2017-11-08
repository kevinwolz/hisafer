#' Builds a Hi-sAFe experiment
#' @description Builds a Hi-sAFe experiment (a group of simulations) - creates the folder structure and input files.
#' @return Invisibly returns a list containing the original hip object and path to the experiment folder.
#' @param hip An object of class "hip" - typically with multiple simulations (rows).
#' @param path A character string of the path to the folder in which the experiment folder should be created.
#' @param profiles A character vector of output profiles the simulation to export.
#' @param saveProjectOption Logical, sets the saveProjectOption parameter in the .sim file.
#' @export
#' @importFrom dplyr %>%
#' @family hisafe build functions
#' @examples
#' \dontrun{
#' # Once a group Hi-sAFe simulations (experiment) is defined:
#' myexp <- define_hisafe(latitude = c(30,60))
#'
#' # Building the experiment folder structure & files:
#' build_hisafe_exp(myexp, "./simulations", exp.name = "lat_exp")
#' }
build_hisafe_exp <- function(hip,
                             path,
                             exp.name          = "experiment",
                             profiles          = "all",
                             saveProjectOption = FALSE,
                             controls          = FALSE) {

  ## Check if data has class hip
  if(!("hip" %in% class(hip))) stop("data not of class hip", call. = FALSE)

  ## Create experiment directory
  exp.path <- gsub("//", "/", paste0(path, "/", exp.name))
  dir.create(exp.path, showWarnings = FALSE)

  ## Write out experiment summary
  readr::write_csv(hip, gsub("//", "/", paste0(exp.path, "/", exp.name, "_exp_summary.csv")))

  ## build folder tree & input files for each simulation in experiment
  make_hip <- function(x) {
    class(x) <- c("hip", class(x))
    return(x)
  }
  hip.list <- as.list(hip) %>%
    purrr::pmap(list) %>%
    purrr::map(dplyr::as_tibble) %>%
    purrr::map(make_hip) %>%
    purrr::walk(build_hisafe, path = exp.path, profiles = profiles, saveProjectOption = saveProjectOption)

  ## Run control simulations
  if(controls) stop("support for running control simulations not yet supported", call. = FALSE)

  hip.aug <- list(hip = hip, path = exp.path)
  class(hip.aug) <- c("hip", class(hip.aug))
  invisible(hip.aug)
}

#' Builds a Hi-sAFe simulation
#' @description Builds a Hi-sAFe simulation - creates the folder structure and input files.
#' @return Invisibly returns a list containing the original hip object and supplied path.
#' @param hip An object of class "hip" containing a single simulation (row).
#' @param path A character string of the path to the simulation folder.
#' @param profiles A character vector of output profiles the simulation to export.
#' @param saveProjectOption Logical, sets the saveProjectOption parameter in the .sim file.
#' @export
#' @family hisafe build functions
#' @examples
#' \dontrun{
#' # Once a single Hi-sAFe simulation is defined:
#' myexp <- define_hisafe()
#'
#' # Building the experiment folder structure & files:
#' build_hisafe(myexp, "./simulations")
#' }
build_hisafe <- function(hip, path, profiles = "all", saveProjectOption = FALSE) {

  if(profiles[1] == "all") profiles <- SUPPORTED.PROFILES$profiles

  ## Check if data has class hip and nrow == 1
  if(!("hip" %in% class(hip))) stop("data not of class hip", call. = FALSE)
  if(nrow(hip) > 1)            stop("build_hisafe only handles hip objects containing a single simulation (1 row).
                                     Use build_hisafe_exp to build multiple simulations.", call. = FALSE)

  ## Check if path already exists
  if(!dir.exists(path)) stop("path does not exist.", call. = FALSE)

  ## Copy over folder structure & template files from Hi-sAFe template path
  ## Any newly built files below will overwrite these files
  sim.path <- gsub("//", "/", paste0(path, "/", hip$SimulationName))
  if(dir.exists(sim.path)) stop(paste0("A simulation with the name <", hip$SimulationName, "> already exisits in this location."), call. = FALSE)
  system(paste("cp -r", HISAFE.TEMPLATE, sim.path))

  ## Write out experiment summary
  readr::write_csv(hip, gsub("//", "/", paste0(sim.path, "/", hip$SimulationName, "_simu_summary.csv")))

  ## Build pld & sim files
  build_pld(hip, sim.path)
  build_sim(hip, sim.path, profiles, saveProjectOption)

  ## Move weather file if one was provided
  if(hip$weatherFile != "default") file.copy(hip$weatherFile, paste0(sim.path, "/weather/weather.wth"), overwrite = TRUE)

  ## Copy required .plt files to cropSpecies
  required.plts <- paste0(HISAFE.LIBRARY, "cropSpecies/", c(hip$mainCropSpecies, hip$interCropSpecies), ".plt")
  dum <- purrr::map(required.plts, file.copy, to = paste0(sim.path, "/cropSpecies"))

  ## Copy required .tec files to itk
  required.tecs <- paste0(HISAFE.LIBRARY, "itk/", c(hip$mainCropItk, hip$interCropItk), ".tec")
  dum <- purrr::map(required.tecs, file.copy, to = paste0(sim.path, "/itk"))

  ## Copy required .tree files to treeSpecies
  required.trees <- paste0(HISAFE.LIBRARY, "treeSpecies/", hip$treeSpecies, ".tree")
  dum <- purrr::map(required.trees, file.copy, to = paste0(sim.path, "/treeSpecies"))

  ## Copy required exportProfiles
  if(!all(profiles %in% SUPPORTED.PROFILES$profiles)) {
    missing.profiles      <- profiles[!(profiles %in% SUPPORTED.PROFILES$profiles)]
    missing.profile.error <- paste(c("The following profiles are not available:", missing.profiles), collapse = "\n")
    stop(missing.profile.error)
  }

  export.profile.paths <- paste0(HISAFE.LIBRARY, "exportParameters/", profiles, ".pro")
  dum <- purrr::map(export.profile.paths, file.copy, to = paste0(sim.path, "/exportParameters"))

  ## Edit tree file
  build_tree(hip, sim.path, hip$treeSpecies)

  hip.aug <- list(hip = hip, path = path)
  class(hip.aug) <- c("hip", class(hip.aug))
  invisible(hip.aug)
}

#' Build .pld file for Hi-sAFe simulation
#' @description Builds .pld file for a Hi-sAFe simulation. Used within \code{build_hisafe}.
#' @return Invisibly returns \code{TRUE}.
#' @param plan A single simulation (row) from an object of class "hip"
#' @param path A character string of the path to the simulation folder.
build_pld <- function(plan, path) {
  pld.file <- gsub("//", "/", paste0(path, "/plotDescription/template.pld"))
  pld <- readLines(pld.file)

  pld[6]   <- paste0("latitude = ",            plan$latitude)
  pld[11]  <- paste0("cellWidth = ",           plan$cellWidth)
  pld[13]  <- paste0("treeLineOrientation = ", plan$treeLineOrientation)
  pld[14]  <- paste0("spacingBetweenRows = ",  plan$spacingBetweenRows)
  pld[15]  <- paste0("spacingWithinRows = ",   plan$spacingWithinRows)
  pld[19]  <- paste0("slopeIntensity = ",      plan$slopeIntensity)
  pld[20]  <- paste0("slopeAspect = ",         plan$slopeAspect)
  pld[21]  <- paste0("windMeanForce = ",       plan$windMeanForce)
  pld[24]  <- paste0("waterTable = ",          plan$waterTable)
  pld[96]  <- paste0("TreeInit\t",             plan$treeSpecies, "\t1\t", plan$treeHeight, "\t0.5\t0\t0.5\t0.25\t0\t0")
  pld[101] <- paste0("RootInit\t",             plan$rootShape, "\t3\t0.6\t0\t0\t0.5")

  writeLines(pld, pld.file)
  dum <- file.rename(paste0(path, "/plotDescription/template.pld"), paste0(path, "/plotDescription/", plan$SimulationName, ".pld"))

  invisible(TRUE)
}

#' Build .sim file for Hi-sAFe simulation
#' @description Builds .sim file for a Hi-sAFe simulation. Used within \code{build_hisafe}.
#' @return Invisibly returns \code{TRUE}.
#' @param plan A single simulation (row) from an object of class "hip"
#' @param path A character string of the path to the simulation folder.
#' @param profiles A character vector of output profiles the simulation to export.
#' @param saveProjectOption Logical, sets the saveProjectOption parameter in the .sim file.
build_sim <- function(plan, path, profiles, saveProjectOption) {
  sim.file <- gsub("//", "/", paste0(path, "/template.sim"))
  sim <- readLines(sim.file)

  nyears <- plan$nbSimulations

  sim[2]  <- paste0("pldFileName = ",         plan$SimulationName, ".pld")
  sim[3]  <- paste0("nbSimulations = ",       plan$nbSimulations)
  sim[4]  <- paste0("simulationYearStart = ", plan$simulationYearStart)
  sim[5]  <- paste0("simulationDayStart = ",  plan$simulationDayStart)
  sim[6]  <- paste0("simulationNbrDays = ",   plan$simulationNbrDays)
  sim[7]  <- paste0("saveProjectOption = ",   as.numeric(saveProjectOption))
  sim[10] <- paste0("mainCropSpecies = ",     plan$mainCropSpecies,  ".plt")
  sim[11] <- paste0("interCropSpecies = ",    plan$interCropSpecies, ".plt")
  sim[12] <- paste0("mainCropItk = ",         plan$mainCropItk,  ".tec")
  sim[13] <- paste0("interCropItk = ",        plan$interCropItk, ".tec")
  sim[14] <- paste0("treeCropDistance = ",    plan$treeCropDistance)
  sim[15] <- paste0("weededAreaRadius = ",    plan$weededAreaRadius)

  sim[18] <- "weatherFile = weather.wth"

  sim[21] <- paste0("profileNames = ",        paste0(SUPPORTED.PROFILES$profiles[SUPPORTED.PROFILES$profiles %in% profiles], collapse = ","))
  sim[22] <- paste0("exportFrequencies = ",   paste0(SUPPORTED.PROFILES$freqs[SUPPORTED.PROFILES$profiles %in% profiles], collapse = ","))

  X <- as.numeric(grepl("X", plan$toricSymmetry))
  Y <- as.numeric(grepl("Y", plan$toricSymmetry))
  sim[25] <- paste0("toreXp = ", X)
  sim[26] <- paste0("toreXn = ", X)
  sim[27] <- paste0("toreYp = ", Y)
  sim[28] <- paste0("toreYn = ", Y)

  if(plan$treePruningFreq > 0) {
    tree.prune.years   <- seq(1, nyears, plan$treePruningFreq)
    n.tree.prune.years <- length(tree.prune.years)
    sim[31] <- paste0("treePruningYears = ",     paste0(tree.prune.years, collapse=","))
    sim[32] <- paste0("treePruningProp = ",      paste0(rep(plan$treePruningProp, n.tree.prune.years), collapse=","))
    sim[33] <- paste0("treePruningMaxHeight = ", paste0(rep(plan$treePruningMaxHeight, n.tree.prune.years), collapse=","))
    sim[34] <- paste0("treePruningDays = ",      paste0(rep(365, n.tree.prune.years), collapse=","))
  }

  if(plan$treeRootPruningFreq > 0) {
    root.prune.years   <- seq(1, nyears, plan$treeRootPruningFreq)
    n.root.prune.years <- length(root.prune.years)
    sim[42] <- paste0("treeRootPruningYears = ",    paste0(root.prune.years, collapse=","))
    sim[43] <- paste0("treeRootPruningDays = ",     paste0(rep(365, n.root.prune.years), collapse=","))
    sim[44] <- paste0("treeRootPruningDistance = ", paste0(rep(plan$treeRootPruningDistance, n.root.prune.years), collapse=","))
    sim[45] <- paste0("treeRootPruningDepth = ",    paste0(rep(plan$treeRootPruningDepth, n.root.prune.years), collapse=","))
  }

  writeLines(sim, sim.file)
  dum <- file.rename(paste0(path, "/template.sim"), paste0(path, "/", plan$SimulationName, ".sim"))

  invisible(TRUE)
}

#' Edit .tree file for Hi-sAFe simulation
#' @description Edits .tree file for a Hi-sAFe simulation. Used within \code{build_hisafe}.
#' @return Invisibly returns \code{TRUE}.
#' @param plan A single simulation (row) from an object of class "hip"
#' @param path A character string of the path to the simulation folder.
#' @param species A character string of the tree species to edit.
build_tree <- function(plan, path, species) {
  tree.file <- gsub("//", "/", paste0(path, "/treeSpecies/", species, ".tree"))
  tree <- readLines(tree.file)

  tree[23]  <- paste0("budBurstTempAccumulationDateStart = ", plan$budBurstTempAccumulationDateStart)
  tree[25]  <- paste0("budBurstAccumulatedTemp = ",           plan$budBurstAccumulatedTemp)
  tree[46]  <- paste0("lueMax = ",                            plan$lueMax)
  tree[80]  <- paste0("coarseRootAnoxiaResistance = ",        plan$coarseRootAnoxiaResistance)
  tree[82]  <- paste0("rootHalfLife = ",                      plan$rootHalfLife)
  tree[83]  <- paste0("rootAnoxiaHalfLife = ",                plan$rootAnoxiaHalfLife)
  tree[84]  <- paste0("colonisationThreshold = ",             plan$colonisationThreshold)

  writeLines(tree, tree.file)

  invisible(TRUE)
}
