##### GLOBAL VARIABLES ABAILABLE TO ALL hisafer FUNCTIONS DEFAULTS AND SUPPORTED PARAMETERS/CLASSES/PROFILES/VALUES

HISAFE.TEMPLATE <- "/Users/wolzy4u/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafer/data/hisafe_template/"
HISAFE.LIBRARY  <- "/Users/wolzy4u/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafer/data/hisafe_library/"

SUPPORTED.PROFILES <- dplyr::tibble(profiles = c("annualplot", "annualtree", "annualcrop", "plot", "trees", "roots", "cells", "voxels", "climate", "monthCells"),
                                    freqs    = c(365,          365,          365,          1,      1,       1,       1,        1,       1,         30))

SUPPORTED.TREES    <- c("walnut-hybrid", "poplar", "wild-cherry")
SUPPORTED.CROPS    <- c("alfalfa", "baresoil", "durum-wheat", "grass", "maize", "rape", "soybean", "weed-restinclieres", "weed", "wheat")
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
  spacingWithinRows         = param( 9,                    NA,                 1,     NA,    NA,      30),
  slopeIntensity            = param( 0,                    NA,                 0,     NA,    NA,      45),
  slopeAspect               = param( 0,                    NA,                 0,     359,   NA,      NA),
  windMeanForce             = param( 5,                    NA,                 0,     NA,    NA,      NA),
  treeSpecies               = param( "walnut-hybrid",      SUPPORTED.TREES,    NA,    NA,    NA,      NA),   # special def
  treeHeight                = param( 1,                    NA,                 0.1,   NA,    0.25,    3),
  rootShape                 = param( 1,                    1:3,                NA,    NA,    NA,      NA),

  ## SIM
  SimulationName            = param( "Sim",                NA,                 NA,    NA,    NA,      NA),   # special def
  nbSimulations             = param( 30,                   NA,                 1,     NA,    NA,      50),
  simulationYearStart       = param( 1995,                 NA,                 NA,    NA,    NA,      NA),
  simulationDayStart        = param( 240,                  NA,                 1,     365,   NA,      NA),
  mainCropSpecies           = param( "durum-wheat",        SUPPORTED.CROPS,    NA,    NA,    NA,      NA),
  interCropSpecies          = param( "weed-restinclieres", SUPPORTED.CROPS,    NA,    NA,    NA,      NA),
  treeCropDistance          = param( 0.5,                  NA,                 0,     NA,    0.5,     3),
  weededAreaRadius          = param( 0,                    NA,                 0,     NA,    NA,      2),
  weatherFile               = param( "default",            NA,                 NA,    NA,    NA,      NA),
  toricSymmetry             = param( "XY",                 SUPPORTED.SYMMETRY, NA,    NA,    NA,      NA),   # special def
  treePruningFreq           = param( 2,                    NA,                 0,     NA,    1,       3),    # special def
  treePruningProp           = param( 0.3,                  NA,                 0,     1,     0.25,    0.5),
  treePruningMaxHeight      = param( 4,                    NA,                 0,     NA,    2,       5),
  treeRootPruningFreq       = param( 0,                    NA,                 0,     NA,    1,       3),    # special def
  treeRootPruningDistance   = param( 0.5,                  NA,                 NA,    NA,    0.5,     2),
  treeRootPruningDepth      = param( 0,                    NA,                 0,     NA,    NA,      2)
)

#' Display supported Hi-sAFe input parameters
#' @description Displays supported Hi-sAFe input parameters, their default values, and their accepted/suggested ranges.
#' @return Invisibly returns an alphebetized character vector of the names of supported Hi-sAFe prameters.
#' @param variable If "names", the default, then just the names of supported Hi-sAFe parameters is printed to the console.
#' If "all", then the names, default values, and accepted/suggested ranges of supported Hi-sAFe parameters is printed.
#' Can also be a character vector of specific Hi-sAFe parameters of which to display details.
#' @export
#' @family hisafe definition functions
#' @examples
#' \dontrun{
#' hisafe_params()            # just for parameter names
#' hisafe_params("cellWidth") # details of cellWidth parameter
#' hisafe_params("all")       # details of all parameters
#' }
hisafe_params <- function(variable = "names") {
  param.names <- sort(names(HISAFE.PARAMS))

  acceptable <- c(param.names, "names", "all")
  if(any(!(variable %in% acceptable))) {
    bad.vars <- variable[!(variable %in% acceptable)]
    stop(paste0("The following are not supported Hi-sAFe input parameters: ", bad.vars))
  }

  if(variable[1] == "all") {
    param.details <- str(HISAFE.PARAMS,
                         comp.str = "",
                         give.length = FALSE,
                         give.head = FALSE,
                         no.list = TRUE)
  } else if (variable[1] == "names") {
    cat(paste0(param.names, collapse = "\n"))
  } else {
    for(i in 1:length(variable)){
      cat(paste0("\n", variable[i], "\n"))
      param.details <- str(HISAFE.PARAMS[[variable[i]]],
                           comp.str = "",
                           give.length = FALSE,
                           give.head = FALSE,
                           no.list = TRUE)
    }
  }
  invisible(param.names)
}

#' Display supported Hi-sAFe output profiles
#' @description Displays supported Hi-sAFe output profiles and standard output frequency.
#' @return Invisibly returns a data frame containing the profiles names and output frequency.
#' @export
#' @family hisafe definition functions
#' @examples
#' \dontrun{
#' hisafe_profiles()
#' }
hisafe_profiles <- function(variable = "names") {
  print(as.data.frame(SUPPORTED.PROFILES), row.names = FALSE)
}
