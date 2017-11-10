##### DEFAULTS AND SUPPORTED PARAMETERS/CLASSES/PROFILES/VALUES

SUPPORTED.PROFILES <- dplyr::tibble(profiles = c("annualplot", "annualtree", "annualcrop", "plot", "trees", "roots", "cells", "voxels", "climate", "monthCells"),
                                    freqs    = c(365,          365,          365,          1,      1,       1,       1,        1,       1,         30))

SUPPORTED.TREES    <- c("walnut-hybrid", "poplar", "wild-cherry")
SUPPORTED.CROPS    <- c("alfalfa", "banana", "baresoil", "barley", "durum-wheat-allur", "fescue", "flax", "grass", "lettuce", "maize",
                        "mustard", "pea", "potato", "rape", "ryegrass", "sorghum", "soybean", "sugarbet", "sugarcanne", "sunflower",
                        "tomato", "vine-grenac", "wheat")
SUPPORTED.ITK    <- c("alfalfa", "banane", "baresoil", "barley", "durum-wheat", "fescue", "flax", "grass", "lettuce", "maize", "mustard",
                      "pea", "potato", "rape", "ryegrass", "sorghum", "soybean", "sugarbet", "sugarcanne", "sunflower", "tomato","vine", "wheat")
SUPPORTED.SYMMETRY <- c("XY", "X", "Y", "NO")

param <- function(default, allowed, min, max, min.sug, max.sug) {
  list(default = default, allowed = allowed, min = min, max = max, min.sug = min.sug, max.sug = max.sug)
}

HISAFE.PARAMS <- list(
  ## PLD                             DEFAULT               ALLOWED             MIN    MAX    MIN.SUG  MAX.SUG
  latitude                  = param( 43.7,                 NA,                 -90,   90,    NA,      NA),
  cellWidth                 = param( 1,                    NA,                 0.5,   NA,    1,       2),
  treeLineOrientation       = param( 90,                   NA,                 0,     359,   NA,      NA),
  spacingBetweenRows        = param( 13,                   NA,                 1,     NA,    3,       30),
  spacingWithinRows         = param( 8,                    NA,                 1,     NA,    NA,      30),
  slopeIntensity            = param( 0,                    NA,                 0,     NA,    NA,      45),
  slopeAspect               = param( 0,                    NA,                 0,     359,   NA,      NA),
  windMeanForce             = param( 5,                    NA,                 0,     NA,    NA,      NA),
  waterTable                = param( "true",               c("true", "false"), NA,    NA,    NA,      NA),
  treeSpecies               = param( "walnut-hybrid",      SUPPORTED.TREES,    NA,    NA,    NA,      NA),   # special def
  treeHeight                = param( 1,                    NA,                 0.1,   NA,    0.25,    3),
  rootShape                 = param( 1,                    1:3,                NA,    NA,    NA,      NA),

  ## SIM
  SimulationName            = param( "Sim",                NA,                 NA,    NA,    NA,      NA),   # special def
  nbSimulations             = param( 30,                   NA,                 1,     NA,    NA,      50),
  simulationYearStart       = param( 1995,                 NA,                 NA,    NA,    NA,      NA),
  simulationDayStart        = param( 290,                  NA,                 1,     365,   NA,      NA),   # 240?
  simulationNbrDays         = param( 365,                  NA,                 1,     365,   NA,      NA),
  mainCropSpecies           = param( "durum-wheat-allur",  SUPPORTED.CROPS,    NA,    NA,    NA,      NA),
  interCropSpecies          = param( "grass",              SUPPORTED.CROPS,    NA,    NA,    NA,      NA),
  mainCropItk               = param( "durum-wheat",        SUPPORTED.ITK,      NA,    NA,    NA,      NA),
  interCropItk              = param( "grass",              SUPPORTED.ITK,      NA,    NA,    NA,      NA),
  treeCropDistance          = param( 0.5,                  NA,                 0,     NA,    0.5,     3),
  weededAreaRadius          = param( 0,                    NA,                 0,     NA,    NA,      2),
  weatherFile               = param( "default",            NA,                 NA,    NA,    NA,      NA),
  toricSymmetry             = param( "XY",                 SUPPORTED.SYMMETRY, NA,    NA,    NA,      NA),   # special def
  treePruningFreq           = param( 2,                    NA,                 0,     NA,    1,       3),    # special def
  treePruningProp           = param( 0.3,                  NA,                 0,     1,     0.25,    0.5),
  treePruningMaxHeight      = param( 4,                    NA,                 0,     NA,    2,       5),
  treeRootPruningFreq       = param( 1,                    NA,                 0,     NA,    0,       3),    # special def
  treeRootPruningDistance   = param( 0.5,                  NA,                 NA,    NA,    0.5,     2),
  treeRootPruningDepth      = param( 0.5,                  NA,                 0,     NA,    NA,      2),

  ## TREE
  budBurstTempAccumulationDateStart       = param( 61,     NA,                 1,     365,   NA,      NA),
  budBurstAccumulatedTemp                 = param( 612,    NA,                 1,     10000, 200,     1000),
  lueMax                                  = param( 0.6,    NA,                 0,     NA,    NA,      NA),
  coarseRootAnoxiaResistance              = param( 60,     NA,                 NA,    NA,    NA,      NA),
  rootHalfLife                            = param( 114,    NA,                 NA,    NA,    NA,      NA),
  rootAnoxiaHalfLife                      = param( 50,     NA,                 NA,    NA,    NA,      NA),
  colonisationThreshold                   = param( 1500,   NA,                 NA,    NA,    NA,      NA)
)

