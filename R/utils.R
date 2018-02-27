## This SUPPORTED.PROFILES object is only needed to write the exportFrequencies line of the sim file within build_structure().
## There is no better way to do this until a better way to describe export profiles and frequenceies in Hi-sAFe is determined.
SUPPORTED.PROFILES <- dplyr::tibble(profiles = c("annualplot", "annualtree", "annualcrop",
                                                 "plot", "trees", "cells", "cellsDetail", "voxels", "voxelsDetail", "voxelsDebug", "climate", "monthCells"),
                                    freqs    = c(365,          365,          365,
                                                 1,      1,       1,        1,            1,        1,               1,             1,         30))

INPUT.DEFS  <- readr::read_delim(system.file("extdata", "input_defs.txt",  package = "hisafer"), "\t", col_types = readr::cols())
OUTPUT.DEFS <- dplyr::arrange(readr::read_delim(system.file("extdata", "output_defs.txt", package = "hisafer"), "\t", col_types = readr::cols()), profile, name)

EXTDATA <- list.files(system.file("extdata", package = "hisafer"))
INCLUDED.TEMPLATES <- EXTDATA[!(grepl("\\.", EXTDATA) | EXTDATA == "template_common")]
INCLUDED.TEMPLATE.SUBPATH <- system.file("extdata", "template_common",  package = "hisafer")

remove_whitespace <- function(x) gsub("^\\s+|\\s+$", "", x)
clean_path        <- function(x) gsub("//", "/", x, fixed = TRUE)

get_template_path <- function(template) {
  path <- ifelse(template %in% INCLUDED.TEMPLATES,
                 system.file("extdata", template, package = "hisafer"),
                 template)
  path <- R.utils::getAbsolutePath(path)
  path <- clean_path(paste0(path, "/"))
  if(!dir.exists(path)) stop("template directory does not exist", call. = FALSE)
  return(path)
}

get_template_subpath <- function(template) {
  if(template %in% INCLUDED.TEMPLATES) {
    path <- INCLUDED.TEMPLATE.SUBPATH
    path <- clean_path(paste0(R.utils::getAbsolutePath(path), "/"))
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
