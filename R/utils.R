## This SUPPORTED.PROFILES object is only needed to write the exportFrequencies line of the sim file within build_structure().
## There is no better way to do this until a better way to describe export profiles and frequenceies in Hi-sAFe is determined.
SUPPORTED.PROFILES <- dplyr::tibble(profiles = c("plot",   "plotDetail",
                                                 "trees",  "treesDetail",
                                                 "cells",  "cellsDetail",
                                                 "voxels", "voxelsDetail", "voxelsDebug", "voxelsOptim",
                                                 "climate",
                                                 "monthCells", "monthCellsDetail",
                                                 "annualCells", "annualDBH", "annualCellsYield"),
                                    freqs       = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 30, 30, 365, 365, 365),
                                    description = c("daily plot-level data (core variables)",
                                                    "daily plot-level data (supplemental variables)",
                                                    "daily data for each tree in the scene (core variables)",
                                                    "daily data for each tree in the scene (supplemental variables)",
                                                    "daily data for each cell in the scene (core variables)",
                                                    "daily data for each cell in the scene (supplemental variables)",
                                                    "daily data for each voxel in the scene (core variables)",
                                                    "daily data for each voxel in the scene (supplemental variables)",
                                                    "daily data for each voxel in the scene (debugging variables)",
                                                    "daily data for each voxel in the scene (water module optimization variables)",
                                                    "daily climate data",
                                                    "monthly data for each cell in the scene (core variables)",
                                                    "monthly data for each cell in the scene (supplemental variables)",
                                                    "annual data for each cell in the scene",
                                                    "only data for annual DBH of tree 1",
                                                    "only data for annual yield of specific cells"))

PRIVATE.PROFILES    <- c("voxelsDebug", "voxelsOptim", "annualDBH", "annualCellsYield")
PUBLIC.PROFILES     <- SUPPORTED.PROFILES$profiles[!(SUPPORTED.PROFILES$profiles %in% PRIVATE.PROFILES)]
DATA.PROFILES       <- c("plot", "trees", "cells", "voxels", "climate", "monthCells", "annualCells")
FILTERABLE.ELEMENTS <- c(DATA.PROFILES, "plot.info", "tree.info", "exp.plan", "metadata")

BASE.COLS <- c("SimulationName", "Date", "Day", "Month", "Year", "JulianDay")

INPUT.DEFS  <- readr::read_delim(system.file("extdata", "input_defs.txt",  package = "hisafer"), "\t", col_types = readr::cols())
OUTPUT.DEFS <- dplyr::arrange(readr::read_delim(system.file("extdata", "output_defs.txt", package = "hisafer"), "\t", col_types = readr::cols()), profile, name)

EXTDATA <- list.files(system.file("extdata", package = "hisafer"))
INCLUDED.TEMPLATES <- EXTDATA[!(grepl("\\.", EXTDATA) | EXTDATA == "template_common")]
INCLUDED.TEMPLATE.SUBPATH <- system.file("extdata", "template_common",  package = "hisafer")

remove_whitespace <- function(x) gsub("^\\s+|\\s+$", "", x)
clean_path        <- function(x) gsub("//", "/", paste0(path, "/"), fixed = TRUE)
get_absolute_path <- function(x) clean_path(base::normalizePath(x))

get_template_path <- function(template) {
  path <- ifelse(template %in% INCLUDED.TEMPLATES,
                 system.file("extdata", template, package = "hisafer"),
                 template)
  path <- get_absolute_path(path)
  if(!dir.exists(path)) stop("template directory does not exist", call. = FALSE)
  return(path)
}

get_template_subpath <- function(template) {
  if(template %in% INCLUDED.TEMPLATES) {
    path <- get_absolute_path(INCLUDED.TEMPLATE.SUBPATH)
  } else {
    path <- get_template_path(template)
  }
  return(path)
}

get_available_profiles <- function(template) {
  path <- get_template_subpath(template)
  profiles <- gsub("\\.pro", "", list.files(clean_path(paste0(path, "/exportParameters")), pattern = "\\.pro"))
  return(profiles)
}

nan_to_zero <- function(x) {
  x[is.nan(x)] <- 0
  return(x)
}

swap_cols <- function(df, col1, col2) {
  c1 <- df[[col1]]
  c2 <- df[[col2]]
  df[[col1]] <- c2
  df[[col2]] <- c1
  return(df)
}

is_TF <- function(x, error = TRUE) {
  x.name <- deparse(substitute(x))
  check  <- is.logical(x)
  if(error) {
    if(!check) stop(paste0(x.name, " argument must be a logical"), call. = FALSE)
  } else {
    return(check)
  }
}
