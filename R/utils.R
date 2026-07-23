## This SUPPORTED.PROFILES object is only needed to write the exportFrequencies line of the sim file within build_structure().
## There is no better way to do this until a better way to describe export profiles and frequencies in Hi-sAFe is determined.
SUPPORTED.PROFILES <- dplyr::tibble(profiles = c("plot",   "annualPlot",
                                                 "zones",
                                                 "trees",  "annualTrees",
                                                 "cells",  "monthCells","annualCells",
                                                 "voxels",
                                                 "climate"),
                                    freqs       = c(1,365,1,1,365,1,30,365,1,1),
                                    description = c("daily plot-level data",
                                                    "annual plot-level data",
                                                    "daily zones-level data",
                                                    "daily data for each tree in the scene",
                                                    "annual data for each tree in the scene",
                                                    "daily data for each cell in the scene",
                                                    "monthly data for each cell in the scene",
                                                    "annual data for each cell in the scene",
                                                    "daily data for each voxel in the scene",
                                                    "daily climate data"))

CORE.PROFILES       <- c("plot", "zones", "trees", "cells", "climate", "yield")
PRIVATE.PROFILES    <- c("voxels")
DATA.PROFILES       <- c("plot", "zones", "trees", "cells", "voxels", "climate", "monthCells", "annualCells", "yield")
FILTERABLE.ELEMENTS <- c(DATA.PROFILES, "plot.info", "zone.info", "tree.info", "exp.plan", "metadata")
PUBLIC.PROFILES     <- SUPPORTED.PROFILES$profiles[!(SUPPORTED.PROFILES$profiles %in% PRIVATE.PROFILES)]
BASE.COLS <- c("SimulationName", "Date", "Day", "Month", "Year", "JulianDay")

INPUT.DEFS  <- readr::read_delim(system.file("extdata", "input_defs.txt",  package = "hisafer"), "\t", col_types = readr::cols())
OUTPUT.DEFS <- dplyr::arrange(readr::read_delim(system.file("extdata", "output_defs.txt", package = "hisafer"), "\t", col_types = readr::cols()), profile, name)

EXTDATA <- list.files(system.file("extdata", package = "hisafer"))
INCLUDED.TEMPLATES <- EXTDATA[!(grepl("\\.", EXTDATA) | EXTDATA == "template_common")]
INCLUDED.TEMPLATE.SUBPATH <- paste0(system.file("extdata", "template_common",  package = "hisafer"), "/")

remove_whitespace <- function(x) gsub("^\\s+|\\s+$", "", x)
clean_path        <- function(x) gsub("//", "/", x, fixed = TRUE)
get_absolute_path <- function(x) {
  gap <- function(y) {
    if(substr(y, 1, 1) == ".") y <- gsub(pattern     = "^\\.",
                                       replacement = getwd(),
                                       x           = y)
    return(y)
  }
  x <- purrr::map_chr(x, gap)
  return(clean_path(x))
}

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
  exportFile<-paste(path, "/export.out",   sep="")
  if(!file.exists(exportFile)) stop("le fichier export.out n'existe pas dans le répertoire : ", path)

  lignes<-readLines(exportFile)
  profiles <- c()
  for (l in lignes) {
    test<-grep(pattern="ProfileDef", x=l, value=TRUE)
    if (length(test) > 0) {
      if (substr(test, 1, 1) != "#") {
        lis<-unlist(strsplit(test, split = "\t"))
        if (length(lis) > 1) {
          profiles <- c(profiles, lis[2])
        }
      }
    }
  }

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
