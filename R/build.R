build_hisafe_exp <- function(exp, path, exp.name, baseline.dir, controls = FALSE) {

  ## Check if exp has class hip
  if("hip" %in% class(exp)) stop("exp plan not of class hip")

  ## Create experiment directory
  exp.path <- gsub("//", "/", paste0(path, "/", exp.name))
  success <- dir.create(exp.path, showWarnings = FALSE)
  if(!success) stop("creation of experiment directory failed")

  ## Write out experiment summary
  readr::write_csv(exp, gsub("//", "/", paste0(exp.path, "/", exp.name, ".csv")))

  ## Run Hi-sAFe for each simulation in experiment
  purrr:map(exp, build_hisafe, path = exp.path, baseline.dir = baseline.dir)

  ## Run control simulations
  if(controls) stop("support for running control simulations not yet supported")

  invisible(NULL)
}

build_hisafe <- function(exp, path, baseline.dir) {

  ## Check if exp has class hip
  if("hip" %in% class(exp)) stop("exp plan not of class hip")

  ## Create simulaton directory & folder tree
  sim.path <- gsub("//", "/", paste0(path, "/", plan$SimulationName[1]))
  # success <- dir.create(sim.path, showWarnings = FALSE)
  # if(!success) stop("creation of simulation directory failed")
  # build_dir_tree(sim.path)

  ## Copy over folder structure & template files from Hi-sAFe template path
  ## Any newly built files below will overwrite these files
  system(paste("cp -r", baseline.dir, path))
  system(paste0("mv ", path, baseline.dir, " ", path, "/", plan$SimulationName[1]))

  ## Build simulation files
  build_pld(exp, sim.path)
  build_sim(exp, sim.path)

  invisible(NULL)
}

# build_dir_tree <- function(path) {
#   dir.create(paste0(path, "/exportParameters"), showWarnings = FALSE)
#   dir.create(paste0(path, "/itk"), showWarnings = FALSE)
#   dir.create(paste0(path, "/plotDescription"), showWarnings = FALSE)
#   dir.create(paste0(path, "/speciesLibrairies"), showWarnings = FALSE)
#   dir.create(paste0(path, "/weather"), showWarnings = FALSE)
# }

build_pld <- function(plan, path) {
  pld.file <- paste0(path, "/plotDescription/Plot_Description.pld")
  pld <- readLines(pld.file)
  pld[6] <- paste0("latitude = ", plan$Latitude[i])
  pld[13] <- paste0("treeLineOrientation = ", plan$Orient[i])
  pld[14] <- paste0("spacingBetweenRows = ", plan$Between[i])
  pld[15] <- paste0("spacingWithinRows = ", plan$Within[i])

  if(plan$Tree=="walnut"){
    pld[91] <- "TreeInit\twalnut-hybrid\t1\t1\t0.5\t0\t0.5\t0.25\t0\t0"
  }else if(plan$Tree=="poplar"){
    pld[91] <- "TreeInit\tpoplar\t1\t1\t0.5\t0\t0.5\t0.25\t0\t0"
  } else {
    stop(paste0("tree species ", plan$Tree, " not supported"))
  }
  writeLines(pld, pld.file)
}

build_sim <- function(plan, path) {
  sim.file <- paste0(runDir, "/Simulation.sim")
  sim <- readLines(sim.file)
  sim[7] <- paste0("nbSimulations = ", plan$Nyears[1])
  sim[8] <- paste0("simulationYearStart = ", plan$YearStart[1])
  sim[9] <- paste0("simulationDayStart = ", plan$DayStart[1])
  sim[13] <- paste0("mainCropSpecies = ", plan$Crop[1], ".tec")
  sim[15] <- paste0("treeCropDistance = ", plan$CropDist[1])
  sim[43] <- paste0("treeRootPruningYears = ", paste0(1:plan$Nyears[1], collapse=","))
  sim[44] <- paste0("treeRootPruningDays = ", paste0(rep(365, plan$Nyears[1]), collapse=","))
  sim[45] <- paste0("treeRootPruningDistance = ", paste0(rep(plan$CropDist[1], plan$Nyears[1]), collapse=","))
  sim[46] <- paste0("treeRootPruningDepth = ", paste0(rep(plan$RootPruneDepth[1], plan$Nyears[1]), collapse=","))
  writeLines(sim, sim.file)
  dum <- file.rename(paste0(runDir, "/Simulation.sim"), paste0(runDir, "/", plan$SimulationName[1], ".sim"))
}
