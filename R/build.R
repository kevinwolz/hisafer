build_hisafe_exp <- function(exp, path, exp.name, export.profiles = "all", saveProjectOption = FALSE, controls = FALSE) {

  ## Check if exp has class hip
  if("hip" %in% class(exp)) stop("exp plan not of class hip")

  ## Create experiment directory
  exp.path <- gsub("//", "/", paste0(path, "/", exp.name))
  success <- dir.create(exp.path, showWarnings = FALSE)
  if(!success) stop("creation of experiment directory failed")

  ## Write out experiment summary
  readr::write_csv(exp, gsub("//", "/", paste0(exp.path, "/", exp.name, ".csv")))

  ## build folder tree & input files for each simulation in experiment
  purrr:map(exp, build_hisafe, path = exp.path, export.profiles = export.profiles)

  ## Run control simulations
  if(controls) stop("support for running control simulations not yet supported")

  invisible(NULL)
}

build_hisafe <- function(exp, path, export.profiles = "all", saveProjectOption = FALSE) {

  ## Check if exp has class hip
  if("hip" %in% class(exp)) stop("exp plan not of class hip")

  ## Create simulaton directory & folder tree
  sim.path <- gsub("//", "/", paste0(path, "/", plan$SimulationName[1]))
  #success <- dir.create(sim.path, showWarnings = FALSE)
  #if(!success) stop(paste0("creation of simulation directory for ", plan$SimulationName[1], " failed"))
  # build_dir_tree(sim.path)

  ## Copy over folder structure & template files from Hi-sAFe template path
  ## Any newly built files below will overwrite these files
  system(paste("cp -r", HISAFE.TEMPLATE, path))
  system(gsub("//", "/", paste0("mv ", path, HISAFE.TEMPLATE, " ", path, "/", plan$SimulationName[1])))

  ## Build pld & sim files
  build_pld(exp, sim.path)
  build_sim(exp, sim.path, export.profiles, saveProjectOption)

  ## Move weather file if one was provided
  if(!is.na(exp$weatherFile[1])) file.copy(exp$weatherFile[1], paste0(sim.path, "/weather/weather.wth"), overwrite = TRUE)

  ## Copy required .tec files to itk
  required.tecs <- paste0(HISAFE.LIBRARY, c(plan$Crop[1], plan$LeaveAreaCrop[1]), ".tec")
  purrr:map(required.tecs, file.copy, to = paste0(sim.path, "/itk"))

  ## Copy required species files to speciesLibraries
  required.tecs <- paste0(HISAFE.LIBRARY, c(plan$Crop[1], plan$LeaveAreaCrop[1]), ".tec")
  purrr:map(required.tecs, file.copy, to = paste0(sim.path, "/itk"))

  ## exportProfiles
  if(export.profiles == "all") export.profiles <- SUPPORTED.PROFILES$profiles

  if(!all(export.profiles %in% SUPPORTED.PROFILES)) {
    missing.profiles <- export.profiles[!(export.profiles %in% SUPPORTED.PROFILES)]
    missing.profile.error <- paste(c("The following profiles are not available:", missing.profiles), collapse = "\n")
    stop(missing.profile.error)
  }

  export.profiles <- paste0(HISAFE.LIBRARY, export.profiles, ".pro")
  purrr:map(export.profiles, file.copy, to = paste0(sim.path, "/exportParameters"))

  invisible(NULL)
}

build_pld <- function(plan, path) {
  pld.file <- paste0(path, "/plotDescription/template.pld")
  pld <- readLines(pld.file)
  pld[6] <- paste0("latitude = ",             plan$latitude[i])
  pld[6] <- paste0("cellWidth = ",            plan$cellWidth[i])
  pld[13] <- paste0("treeLineOrientation = ", plan$treeLineOrientation[i])
  pld[14] <- paste0("spacingBetweenRows = ",  plan$spacingBetweenRows[i])
  pld[15] <- paste0("spacingWithinRows = ",   plan$spacingWithinRows[i])
  pld[19] <- paste0("slopeIntensity = ",      plan$slopeIntensity[i])
  pld[20] <- paste0("slopeAspect = ",         plan$slopeAspect[i])
  pld[23] <- paste0("windMeanForce = ",       plan$windMeanForce[i])
  pld[90] <- paste0("TreeInit\t",             plan$treeSpecies, "\t1\t", plan$treeHeight, "\t0.5\t0\t0.5\t0.25\t0\t0")
  pld[95] <- paste0("RootInit\t",             plan$rootShape, "\t3\t0.6\t0\t0\t0.5")
  writeLines(pld, pld.file)
  dum <- file.rename(paste0(path, "/plotDescription/template.pld"), paste0(path, "/plotDescription/", plan$SimulationName[1], ".pld"))
}

build_sim <- function(plan, path, export.profiles, saveProjectOption) {
  nyears <- plan$nbSimulations[1]
  sim.file <- paste0(path, "/template.sim")
  sim <- readLines(sim.file)

  sim[4] <- paste0("nbSimulations = ",       plan$nbSimulations[1])
  sim[5] <- paste0("simulationYearStart = ", plan$simulationYearStart[1])
  sim[6] <- paste0("simulationDayStart = ",  plan$simulationDayStart[1])
  sim[7] <- paste0("saveProjectOption = ",   as.numeric(saveProjectOption))

  sim[10] <- paste0("mainCropSpecies = ",    plan$mainCropSpecies[1], ".tec")
  sim[11] <- paste0("interCropSpecies = ",   plan$interCropSpecies[1], ".tec")
  sim[12] <- paste0("treeCropDistance = ",   plan$treeCropDistance[1])
  sim[13] <- paste0("weededAreaRadius = ",   plan$weededAreaRadius[1])

  sim[16] <- paste0("weatherFile = ",        plan$weatherFile[1])

  sim[19] <- paste0("profileNames = ",       paste0(SUPPORTED.PROFILES$profiles[SUPPORTED.PROFILES$profiles %in% export.profiles], collapse = ", "))
  sim[20] <- paste0("exportFrequencies = ",  paste0(SUPPORTED.PROFILES$freqs[SUPPORTED.PROFILES$profiles %in% export.profiles], collapse = ", "))

  if(plan$toricSymmetry[1] == "XY") {
    sim[23] <- paste0("toreXp = 1")
    sim[24] <- paste0("toreXn = 1")
    sim[25] <- paste0("toreYp = 1")
    sim[26] <- paste0("toreYn = 1")
  } else if(plan$toricSymmetry[1] == "X") {
    sim[23] <- paste0("toreXp = 1")
    sim[24] <- paste0("toreXn = 1")
    sim[25] <- paste0("toreYp = 0")
    sim[26] <- paste0("toreYn = 0")
  } else if(plan$toricSymmetry[1] == "Y") {
    sim[23] <- paste0("toreXp = 0")
    sim[24] <- paste0("toreXn = 0")
    sim[25] <- paste0("toreYp = 1")
    sim[26] <- paste0("toreYn = 1")
  } else if(plan$toricSymmetry[1] == "NO") {
    sim[23] <- paste0("toreXp = 0")
    sim[24] <- paste0("toreXn = 0")
    sim[25] <- paste0("toreYp = 0")
    sim[26] <- paste0("toreYn = 0")
  }

  tree.prune.years <- seq(1, nyears, plan$treePruningFreq[1])
  n.tree.prune.years <- length(tree.prune.years)
  sim[29] <- paste0("treePruningYears = ",     paste0(tree.prune.years, collapse=","))
  sim[30] <- paste0("treePruningProp = ",      paste0(rep(plan$treePruningProp[1], n.tree.prune.years), collapse=","))
  sim[31] <- paste0("treePruningMaxHeight = ", paste0(rep(plan$treePruningMaxHeight[1], n.tree.prune.years), collapse=","))
  sim[32] <- paste0("treePruningDays = ",      paste0(rep(365, n.tree.prune.years), collapse=","))

  root.prune.years <- seq(1, nyears, plan$treeRootPruningFreq[1])
  n.root.prune.years <- length(root.prune.years)
  sim[40] <- paste0("treeRootPruningYears = ",    paste0(root.prune.years, collapse=","))
  sim[41] <- paste0("treeRootPruningDays = ",     paste0(rep(365, n.root.prune.years), collapse=","))
  sim[42] <- paste0("treeRootPruningDistance = ", paste0(rep(plan$treeRootPruningDistance[1], n.root.prune.years), collapse=","))
  sim[43] <- paste0("treeRootPruningDepth = ",    paste0(rep(plan$treeRootPruningDepth[1], n.root.prune.years), collapse=","))

  writeLines(sim, sim.file)
  dum <- file.rename(paste0(path, "/template.sim"), paste0(path, "/", plan$SimulationName[1], ".sim"))
}

# build_dir_tree <- function(path) {
#   dir.create(paste0(path, "/exportParameters"), showWarnings = FALSE)
#   dir.create(paste0(path, "/itk"), showWarnings = FALSE)
#   dir.create(paste0(path, "/plotDescription"), showWarnings = FALSE)
#   dir.create(paste0(path, "/speciesLibrairies"), showWarnings = FALSE)
#   dir.create(paste0(path, "/weather"), showWarnings = FALSE)
# }
