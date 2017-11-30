
## This SUPPORTED.PROFILES object is only needed to write the exportFrequencies line of the sim file within build_structure().
## There is no better way to do this until a better way to describe export profiles and frequenceies in Hi-sAFe is determined.
SUPPORTED.PROFILES <- dplyr::tibble(profiles = c("annualplot", "annualtree", "annualcrop", "plot", "trees", "roots", "cells", "voxels", "climate", "monthCells"),
                                    freqs    = c(365,          365,          365,          1,      1,       1,       1,        1,       1,         30))

remove_whitespace <- function(x) gsub("^\\s+|\\s+$", "", x)

clean_path <- function(x) gsub("//", "/", x, fixed = TRUE)

get_template_path <- function(input) {
  if(input == "agroforestry_default") {
    path <- clean_path(paste0(system.file("extdata", "agroforestry_default",      package = "hisafer"), "/"))
  } else if(input == "forestry_default") {
    path <- clean_path(paste0(system.file("extdata", "forestry_default",          package = "hisafer"), "/"))
  } else if(input == "monocrop_default") {
    path <- clean_path(paste0(system.file("extdata", "monocrop_default",          package = "hisafer"), "/"))
  } else if(input == "restinclieres_agroforestry") {
    path <- clean_path(paste0(system.file("extdata", "restinclieres_agroforestry", package = "hisafer"), "/"))
  } else if(input == "restinclieres_forestry") {
    path <- clean_path(paste0(system.file("extdata", "restinclieres_forestry", package = "hisafer"), "/"))
  } else if(input == "restinclieres_monocrop") {
    path <- clean_path(paste0(system.file("extdata", "restinclieres_monocrop",     package = "hisafer"), "/"))
  } else {
    path <- clean_path(paste0(input, "/"))
  }
  return(path)
}

get_available_profiles <- function(template.path) {
  gsub("\\.pro", "", list.files(clean_path(paste0(template.path, "/exportParameters")), pattern = "\\.pro"))
}
