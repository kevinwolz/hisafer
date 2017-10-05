HISAFE.TEMPLATE <- "/Users/wolzy4u/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafer/hisafe_template/"
HISAFE.LIBRARY <- "/Users/wolzy4u/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafer/hisafe_library/"
SUPPORTED.PROFILES <- c("annualplot", "annualtree", "cells", "climate", "monthCells", "plot", "trees", "voxels",
                        "annualcrop", "roots") ## not actually supported yet
SUPPORTED.TREES <- c("walnut-hybrid", "poplar", "wild-cherry")
SUPPORTED.CROPS <- c("alfalfa", "baresoil", "durum-wheat", "grass", "maize", "rape", "soybean", "weed-restinclieres", "weed", "wheat")

HISAFE.DEFAULTS <- function() {
  list(
    ## PLD
    latitude = 45,
    cellWidth = 1,
    treeLineOrientation = 0,
    spacingBetweenRows = 15,
    spacingWithinRows = 5,
    slopeIntensity = 0,
    slopeAspect = 0,
    treeSpecies = "walnut-hybrid",
    treeHeight = 1,
    rootShape = 1,

    ## SIM
    nbSimulations = 30,
    simulationYearStart = 1995,
    simulationDayStart = 1,
    mainCropSpecies = "durum-wheat",
    interCropSpecies = "weed-restinclieres",
    treeCropDistance = 0.5,
    weatherFile = NA,
    toricSymmetry = "XY",
    treeRootPruningDepth = 1
  )
}

SUPPORTED.PARAMS <- c("SimulationName", names(HISAFE.DEFAULTS()))
