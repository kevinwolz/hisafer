
## This SUPPORTED.PROFILES object is only needed to write the exportFrequencies line of the sim file within build_structure().
## There is no better way to do this until a better way to describe export profiles and frequenceies in Hi-sAFe is determined.
SUPPORTED.PROFILES <- dplyr::tibble(profiles = c("annualplot", "annualtree", "annualcrop", "plot", "trees", "roots", "cells", "voxels", "climate", "monthCells"),
                                    freqs    = c(365,          365,          365,          1,      1,       1,       1,        1,       1,         30))

INPUT.DEFS  <- readr::read_delim(system.file("extdata", "input_defs.txt",  package = "hisafer"), "\t", col_types = readr::cols())
#OUTPUT.DEFS <- readr::read_delim(system.file("extdata", "output_defs.txt", package = "hisafer"), "\t", col_types = readr::cols())

remove_whitespace <- function(x) gsub("^\\s+|\\s+$", "", x)

clean_path <- function(x) gsub("//", "/", x, fixed = TRUE)

get_template_path <- function(input) {
  included.templates <- c("agroforestry_default", "forestry_default", "monocrop_default",
                          "restinclieres_agroforestry", "restinclieres_forestry", "restinclieres_monocrop")
  path <- ifelse(input %in% included.templates,
                 system.file("extdata", input, package = "hisafer"),
                 input)
  path <- R.utils::getAbsolutePath(path)
  path <- clean_path(paste0(path, "/"))
  if(!dir.exists(path)) stop("template directory does not exist", call. = FALSE)
  return(path)
}

get_available_profiles <- function(template.path) {
  gsub("\\.pro", "", list.files(clean_path(paste0(template.path, "/exportParameters")), pattern = "\\.pro"))
}
