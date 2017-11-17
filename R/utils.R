SUPPORTED.PROFILES <- dplyr::tibble(profiles = c("annualplot", "annualtree", "annualcrop", "plot", "trees", "roots", "cells", "voxels", "climate", "monthCells"),
                                    freqs    = c(365,          365,          365,          1,      1,       1,       1,        1,       1,         30))


remove_whitespace <- function(x) gsub("^\\s+|\\s+$", "", x)
clean_path <- function(x) gsub("//", "/", x, fixed = TRUE)
