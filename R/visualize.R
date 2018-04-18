#' Plot a "sliced" profile of the simulated Hi-sAFe scene
#' @description Plots a "sliced" profile of the simulated Hi-sAFe scene. Requires the ggforce package.
#' @return A ggplot object.
#' @param hop An object of class hop.
#' @param date A character string of the date to plot, in the format "YYYY-MM-DD" or of class Date.
#' @param simu.names A character vector of the SimulationNames within \code{hop} to include. Use "all" to include all available values.
#' @param tree.ids A numeric vector indicating a subset of tree ids to plot. Use "all" to include all available values.
#' @param plot.x Either "x" or "y", indicating which axis of the Hi-sAFe scene should be plotted along the x-axis of the plot.
#' @param Y A numeric vector indicating a subset of cell locations to include when summarizing cell/voxel data.
#' If \code{plot.x} is "x", then this refers to the y coordinates of cells. If \code{plot.x} is "y", then this refers to the x coordinates of cells.
#' @param rel.dates A character vector (in the format "YYYY-MM-DD") or a vector of class Date of the dates from which to scale all aesthetics set by \code{vars}.
#' In the plot, all aesthetics set by \code{vars} will be scaled to be between the minimum and maximum values across these dates.
#' @param height.dates A character vector (in the format "YYYY-MM-DD") or a vector of class Date of the dates from which the plot height should be determined
#' from the maximum height of trees and crops in this range.
#' @param max.soil.depth The maximum depth for which to plot voxel data. To reduce confusion, this value can be supplied as positive or negative.
#' @param vars A list of variable names specifying which simulated variables in \code{hop} should be represented by various plot components.
#' Plot components include:
#' \itemize{
#'  \item{"crown.alpha"}{ - transparency of the tree crown}
#'  \item{"trunk.alpha"}{ - transparency of the tree trunk}
#'  \item{"crop.alpha"}{ - transparency of the crop}
#'  \item{"yield.alpha"}{ - transparency of the yield portion of the crop}
#'  \item{"voxel.alpha"}{ - transparency of the voxel}
#'  \item{"voxel.border"}{ - thickness of the voxel border}
#'  \item{"voxel.L.size"}{ - size of the left circle within the voxel}
#'  \item{"voxel.C.size"}{ - size of the center circle within the voxel}
#'  \item{"voxel.R.size"}{ - size of the right circle within the voxel}
#'  \item{"voxel.L.alpha"}{ - transparency of the left circle within the voxel}
#'  \item{"voxel.C.alpha"}{ - transparency of the center circle within the voxel}
#'  \item{"voxel.R.alpha"}{ - transparency of the right circle within the voxel}
#' }
#' @param tree.rel.vars A character string indicating how to group data to determine the maximum value by which to scale tree aesthetics set by \code{vars}.
#' This can include any combination of "s" for simulation, "y" for year, and "t" for tree.
#' For example, "sy" would scale all aesthetics set by \code{vars} to the maximum value of each variable in each simulation-year combination.
#' Passing an empyt string ("") means that all tree aesthetics set by \code{vars} will be scaled to maximum value across all simulations, years, and trees.
#' Values can include upper case and/or lower case letters.
#' @param crop.rel.vars Same as \code{tree.rel.vars} but for crop aesthetics set by \code{vars}. Doesn't include "t".
#' @param voxel.rel.vars Same as \code{tree.rel.vars} but for voxel aesthetics set by \code{vars}. Doesn't include "t".
#' @param trees A logical indicating whether or not to plot the trees in the scene.
#' @param crops A logical indicating whether or not to plot the crops in the scene.
#' @param voxels A logical indicating whether or not to plot the voxels in the scene.
#' @param climate A logical indicating whether or not to plot climate aspects (e.g. precipitation) in the scene.
#' @param mem.max An integer specifying the maximum number of days into the past to search
#' for crop/voxel data when no data is available for \code{date} within \code{hop}.
#' @param plot.rows An integer specifying \code{nrow} passed to \code{ggplot2::facet_wrap}.
#' @export
#' @importFrom dplyr %>%
#' @import ggplot2
#' @family hisafe visualization functions
#' @examples
#' \dontrun{
#' }
hisafe_slice <- function(hop,
                         date,
                         simu.names     = "all",
                         tree.ids       = "all",
                         plot.x         = "x",
                         Y              = "all",
                         rel.dates      = NULL,
                         height.dates   = date,
                         max.soil.depth = NA,
                         vars = list(crown.alpha   = "leafArea",
                                     trunk.alpha   = "carbonLabile",
                                     crop.alpha    = "lai",
                                     yield.alpha   = "eai",
                                     voxel.alpha   = "totalTreeRootDensity",
                                     voxel.border  = "cropRootDensity",
                                     voxel.L.size  = "waterStock",
                                     voxel.C.size  = "totalTreeCoarseRootBiomass",
                                     voxel.R.size  = "mineralNitrogenStock",
                                     voxel.L.alpha = "totalTreeWaterUptake",
                                     voxel.C.alpha = "fineRootCost",
                                     voxel.R.alpha = "totalTreeNitrogenUptake"),
                         tree.rel.vars  = "SYT",
                         crop.rel.vars  = "SY",
                         voxel.rel.vars = "SY",
                         trees          = TRUE,
                         crops          = TRUE,
                         voxels         = TRUE,
                         climate        = TRUE,
                         mem.max        = 0,
                         plot.rows      = 1) {

  if(!requireNamespace("ggforce", quietly = TRUE)) stop("The package 'ggforce' is required for hisafe_slice(). Please install and load it.", call. = FALSE)
  is_hop(hop, error = TRUE)
  is_logical(trees)
  is_logical(crops)
  is_logical(voxels)
  if(!(Y[1] == "all" | all(is.numeric(Y)))) stop("Y argument must be 'all' or a numeric vector", call. = FALSE)

  date <- lubridate::ymd(date)

  demanded.vars <- as.character(unlist(vars))
  cell.vars  <- c(demanded.vars[grep("crop|yield",  names(vars))],
                  "phenologicStage", "nitrogenFertilisation", "height", "biomass", "yield")
  voxel.vars <- demanded.vars[grep("voxel", names(vars))]
  tree.vars  <- c(demanded.vars[!(demanded.vars %in% c(cell.vars, voxel.vars))],
                  "crownRadiusInterRow", "crownRadiusTreeLine", "crownBaseHeight", "dbh", "height")
  cell.vars  <- cell.vars[!is.na(cell.vars)]
  voxel.vars <- voxel.vars[!is.na(voxel.vars)]
  tree.vars  <- tree.vars[!is.na(tree.vars)]

  if(nrow(hop$plot.info) == 0) stop("plot.info is unavilable in hop and is required", call. = FALSE)
  if(nrow(hop$tree.info) == 0) stop("tree.info is unavilable in hop and is required", call. = FALSE)
  profile_check(hop,  "trees", error = TRUE)
  variable_check(hop, "trees", tree.vars, error = TRUE)

  crops   <- crops   & profile_check(hop, "cells")
  voxels  <- voxels  & profile_check(hop, "voxels")
  climate <- climate & profile_check(hop, "climate")
  if(crops)   variable_check(hop, "cells",  cell.vars,  error = TRUE)
  if(voxels)  variable_check(hop, "voxels", voxel.vars, error = TRUE)
  if(climate) variable_check(hop, "climate", c("precipitation", "waterTableDepth"), error = TRUE)
  if(any(is.na(unlist(vars)))) { # account for any NA vars specifications
    vars <- purrr::map(vars, function(x) tidyr::replace_na(x, "none"))
    for(i in c("trees", "cells", "voxels")[c(trees, crops, voxels)]) hop[[i]]$none <- NA_real_
  }

  x.lab <- "X (m)"
  if(plot.x == "y") { # Switch x-y if plot.x == "y"
    if(!crops)  stop("crops must be TRUE if plot.x = 'y'",  call. = FALSE)
    x.lab <- "Y (m)"
    for(p in c("tree.info", "cells", "voxels")[c(TRUE, crops, voxels)]) hop[[p]] <- swap_cols(hop[[p]], "x", "y")
    hop$trees     <- swap_cols(hop$trees,     "crownRadiusInterRow", "crownRadiusTreeLine")
    hop$plot.info <- swap_cols(hop$plot.info, "plotWidth",           "plotHeight")
  }

  if(crops) {
    cell.Ys <- dplyr::as_tibble(table(hop$cells$SimulationName, hop$cells$y))[, 1:2] %>%
      dplyr::rename(SimulationName = Var1, y = Var2) %>%
      dplyr::left_join(hop$plot.info, by = "SimulationName") %>%
      dplyr::mutate(cells.Y = as.numeric(y)) %>%
      dplyr::select(SimulationName, cells.Y)
    if(Y[1] == "all") Yc <- cell.Ys$cells.Y else Yc <- Y
    if(!all(Yc %in% cell.Ys$cells.Y)) stop("one or more values of Y are not present in hop", call. = FALSE)
  }

  if(voxels) {
    voxel.Ys <- dplyr::as_tibble(table(hop$voxels$SimulationName, hop$voxels$y))[, 1:2] %>%
      dplyr::rename(SimulationName = Var1, y = Var2) %>%
      dplyr::left_join(hop$plot.info, by = "SimulationName") %>%
      dplyr::mutate(voxels.Y     = as.numeric(y)) %>%
      dplyr::mutate(voxels.Y.mod = voxels.Y - cellWidth / 2) %>%
      dplyr::select(SimulationName, voxels.Y, voxels.Y.mod)
    if(Y[1] == "all") Yv <- voxel.Ys$voxels.Y else Yv <- unique(dplyr::filter(voxel.Ys, voxels.Y.mod %in% Y)$voxels.Y)
    if(!all(Yv %in% voxel.Ys$voxels.Y)) stop("one or more values of Y are not present in hop", call. = FALSE)
  }

  hop.full <- hop_filter(hop            = hop,
                         simu.names     = simu.names,
                         tree.ids       = tree.ids,
                         dates          = rel.dates,
                         strip.exp.plan = TRUE)

  hop.height <- hop_filter(hop            = hop,
                           simu.names     = simu.names,
                           tree.ids       = tree.ids,
                           dates          = height.dates,
                           strip.exp.plan = TRUE)

  if(!(date %in% hop$trees$Date)) stop("hop does not contain any data on the date specified", call. = FALSE)
  hop       <- hop_filter(hop            = hop,
                          simu.names     = simu.names,
                          tree.ids       = tree.ids,
                          dates          = date,
                          strip.exp.plan = TRUE)

  crops   <- crops   & nrow(hop.full$cells)   > 0
  voxels  <- voxels  & nrow(hop.full$voxels)  > 0
  climate <- climate & nrow(hop.full$climate) > 0

  ## VOXEL & CELL MEMORY
  if(voxels & nrow(hop$voxels) == 0) hop$voxels <- add_historic_data(df = hop.full$voxels, dates = date, mem.max = mem.max)
  if(crops  & nrow(hop$cells)  == 0) hop$cells  <- add_historic_data(df = hop.full$cells,  dates = date, mem.max = mem.max)

  crops  <- crops  & nrow(hop$voxels) > 0
  voxels <- voxels & nrow(hop$voxels) > 0

  rect.min.border <- 0.25
  rect.max.border <- 1
  arrow.length    <- min(hop.full$plot.info$cellWidth) / 4
  arrow.type      <- ggplot2::arrow(length = unit(5, "points"))
  arrow.size      <- 1

  if(trees & crops) {
    Y.MAX <- max(max(hop.height$trees$height), max(hop.height$cells$height), na.rm = TRUE) + arrow.length
  } else if(trees) {
    Y.MAX <- max(hop.height$trees$height, na.rm = TRUE) + arrow.length
  } else if(crops) {
    Y.MAX <- max(hop.height$cells$height, na.rm = TRUE) + arrow.length
  } else {
    Y.MAX <- 0
  }

  if(voxels) {
    circle.offset     <- min(hop.full$plot.info$cellWidth) / 4
    circle.max.radius <- min(circle.offset, min(diff(unique(hop.full$voxels$z)))) * 0.9 / 2
    circle.max.border <- 0.25
    if(is.na(max.soil.depth)) max.soil.depth <- max(hop.full$voxels$z) + min(diff(hop.full$voxels$z)[diff(hop.full$voxels$z) > 0]) / 2 # cannot use plot.info$soilDepth in case depths not exported
    Y.MIN <- -max.soil.depth
  } else {
    Y.MIN <- 0
  }

  X.MIN <- 0
  X.MAX <- max(hop$plot.info$plotWidth)

  ## AESTHETIC GROUPING/RELAITVE VARIABLES
  tree.grouping.strings  <- c("SimulationName", "Year", "id")[c(grepl("s|S", tree.rel.vars), grepl("y|Y", tree.rel.vars), grepl("t|T", tree.rel.vars))]
  crop.grouping.strings  <- c("SimulationName", "Year")[c(grepl("s|S", crop.rel.vars),  grepl("y|Y", crop.rel.vars))]
  voxel.grouping.strings <- c("SimulationName", "Year")[c(grepl("s|S", voxel.rel.vars), grepl("y|Y", voxel.rel.vars))]
  tree.grouping.symbols  <- rlang::parse_quosures(paste(tree.grouping.strings,  collapse = ";"))
  crop.grouping.symbols  <- rlang::parse_quosures(paste(crop.grouping.strings,  collapse = ";"))
  voxel.grouping.symbols <- rlang::parse_quosures(paste(voxel.grouping.strings, collapse = ";"))

  if(trees) {
    hop$tree.info <- hop$tree.info %>%
      dplyr::mutate(tree.pruning.dates      = list(NA)) %>%
      dplyr::mutate(root.pruning.dates      = list(NA)) %>%
      dplyr::mutate(tree.pruning            = 0) %>%
      dplyr::mutate(root.pruning            = 0) %>%
      dplyr::mutate(root.pruning.distance   = 0) %>%
      dplyr::mutate(root.pruning.depth      = 0) %>%
      dplyr::mutate(tree.pruning.prop       = 0) %>%
      dplyr::mutate(tree.pruning.max.height = 0)
    for(i in 1:nrow(hop$tree.info)) {
      if(!is.na(hop$tree.info$treePruningYears[[i]][1])) {
        hop$tree.info$tree.pruning.dates[[i]] <- lubridate::ymd(paste0(hop$tree.info$treePruningYears[[i]] - 1 +
                                                                         hop$tree.info$simulationYearStart[i], "-01-01")) + hop$tree.info$treePruningDays[[i]] - 1
      }
      if(!is.na(hop$tree.info$treeRootPruningYears[[i]][1])) {
        hop$tree.info$root.pruning.dates[[i]] <- lubridate::ymd(paste0(hop$tree.info$treeRootPruningYears[[i]] - 1 +
                                                                         hop$tree.info$simulationYearStart[i], "-01-01")) + hop$tree.info$treeRootPruningDays[[i]] - 1
      }
      hop$tree.info$tree.pruning[i]         <- as.numeric(date %in% hop$tree.info$tree.pruning.dates[[i]])
      if(hop$tree.info$tree.pruning[i] == 1) {
        prune.id <- which(hop$tree.info$tree.pruning.dates[[i]] == date)
        hop$tree.info$tree.pruning.max.height[i] <- hop$tree.info$treePruningMaxHeight[[i]][prune.id]
        hop$tree.info$tree.pruning.prop[i]       <- hop$tree.info$treePruningProp[[i]][prune.id]
      }
      hop$tree.info$root.pruning[i] <- as.numeric(date %in% hop$tree.info$root.pruning.dates[[i]])
      if(hop$tree.info$root.pruning[i] == 1) {
        prune.id <- which(hop$tree.info$root.pruning.dates[[i]] == date)
        hop$tree.info$root.pruning.depth[i]    <- hop$tree.info$treeRootPruningDepth[[i]][prune.id]
        hop$tree.info$root.pruning.distance[i] <- hop$tree.info$treeRootPruningDistance[[i]][prune.id]
      } else {

      }
    }

    tree.max <- hop.full$trees %>%
      dplyr::mutate(crown.alpha = .[[vars$crown.alpha]]) %>%
      dplyr::mutate(trunk.alpha = .[[vars$trunk.alpha]]) %>%
      dplyr::group_by(!!!tree.grouping.symbols) %>%
      dplyr::summarize(crown.alpha.max = max(crown.alpha),
                       trunk.alpha.max = max(trunk.alpha))

    tree.growth <- hop.full$trees %>%
      dplyr::group_by(SimulationName, id) %>%
      dplyr::mutate(trunk.growth  = as.numeric(c(FALSE, diff(dbh) > 0))) %>%
      dplyr::mutate(crown.growth  = as.numeric(c(FALSE, diff(crownRadiusInterRow) > 0))) %>%
      dplyr::mutate(height.growth = as.numeric(c(FALSE, diff(height) > 0))) %>%
      dplyr::select(SimulationName, Date, id, trunk.growth, crown.growth, height.growth)

    tree.data <- hop$trees %>%
      dplyr::mutate(crown.alpha = .[[vars$crown.alpha]]) %>%
      dplyr::mutate(trunk.alpha = .[[vars$trunk.alpha]]) %>%
      dplyr::left_join(tree.max,      by = tree.grouping.strings) %>%
      dplyr::left_join(hop$tree.info, by = c("SimulationName", "id")) %>%
      dplyr::left_join(hop$plot.info, by = c("SimulationName")) %>%
      dplyr::left_join(tree.growth,   by = c("SimulationName", "Date", "id")) %>%
      dplyr::mutate(crown.radius          = crownRadiusInterRow) %>%
      dplyr::mutate(crown.base.height     = crownBaseHeight) %>%
      dplyr::mutate(tree.height           = height) %>%
      dplyr::mutate(trunk.radius          = dbh / 2 / 100) %>%
      dplyr::mutate(crown.center.y        = crown.base.height + (tree.height - crown.base.height) / 2) %>%
      dplyr::mutate(crown.alpha           = nan_to_zero(crown.alpha / crown.alpha.max)) %>%
      dplyr::mutate(trunk.alpha           = nan_to_zero(trunk.alpha / trunk.alpha.max)) %>%
      dplyr::mutate(tree.x                = x) %>%
      dplyr::mutate(crown.linetype        = "solid") %>% # for non-phantom trees
      dplyr::mutate(tree.pruning.height   = pmin(tree.height * tree.pruning.prop, tree.pruning.max.height)) %>%
      dplyr::select(SimulationName, Date, id, plotWidth, plotHeight,
                    crown.radius, crown.base.height, tree.height, trunk.radius,
                    crown.center.y, tree.x, trunk.growth, crown.growth, height.growth,
                    crown.alpha, trunk.alpha, crown.linetype,
                    tree.pruning, tree.pruning.height, root.pruning, root.pruning.depth, root.pruning.distance)

    trunk.data <- tree.data %>%
      dplyr::select(SimulationName, Date, id, tree.x, tree.height, trunk.radius, trunk.alpha) %>%
      dplyr::mutate(base.radius = trunk.radius) %>%
      dplyr::mutate(L.x = tree.x - base.radius) %>%
      dplyr::mutate(R.x = tree.x + base.radius) %>%
      dplyr::mutate(T.x = tree.x) %>%
      dplyr::mutate(L.y = 0) %>%
      dplyr::mutate(R.y = 0) %>%
      dplyr::mutate(T.y = tree.height)
    trunk.data <- dplyr::tibble(SimulationName = rep(trunk.data$SimulationName, 3),
                                Date           = rep(trunk.data$Date, 3),
                                id             = rep(trunk.data$id, 3),
                                trunk.alpha    = rep(trunk.data$trunk.alpha, 3),
                                x              = c(trunk.data$L.x, trunk.data$R.x, trunk.data$T.x),
                                y              = c(trunk.data$L.y, trunk.data$R.y, trunk.data$T.y))

    ## Add phantom trees if tree crowns grow beyond edge of scene
    phantom.data <- tree.data %>%
      dplyr::group_by(SimulationName, Date, id) %>%
      dplyr::mutate(pos = (tree.x - crown.radius) < 0) %>%
      dplyr::mutate(neg = (tree.x + crown.radius) > plotWidth) %>%
      dplyr::select(SimulationName, Date, id, pos, neg) %>%
      tidyr::gather(key = "side", value = "phantom", pos, neg) %>%
      dplyr::mutate(side = as.numeric(as.character(factor(side, levels = c("neg", "pos"), labels = c("-1", "1"))))) %>%
      dplyr::filter(phantom) %>%
      dplyr::left_join(tree.data, by = c("SimulationName", "Date", "id")) %>%
      dplyr::mutate(tree.x = tree.x + plotWidth * side) %>%
      dplyr::select(-side, -phantom) %>%
      dplyr::mutate(crown.linetype = "dotted") # for phantom trees

    tree.data <- dplyr::bind_rows(tree.data, phantom.data)
  }

  if(crops) {
    cell.border.palette <- rep(c("#999999", "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), 2)

    cell <- hop$cells %>%
      dplyr::mutate(crop.alpha  = .[[vars$crop.alpha]]) %>%
      dplyr::mutate(yield.alpha = .[[vars$yield.alpha]]) %>%
      dplyr::filter(y %in% Yc) %>%
      dplyr::left_join(hop$plot.info, by = "SimulationName") %>%
      dplyr::mutate(cell.height    = nan_to_zero((biomass - yield) / biomass * height)) %>%
      dplyr::mutate(yield.height   = nan_to_zero(yield / biomass * height)) %>%
      dplyr::mutate(cell.color     = as.numeric(factor(phenologicStage))) %>%
      dplyr::mutate(fert.level     = nitrogenFertilisation) %>%
      dplyr::select(SimulationName, Year, Date, x, cellWidth, cell.height, cell.color, crop.alpha, yield.alpha, yield.height, fert.level) %>%
      dplyr::group_by(SimulationName, Year, Date, x, cellWidth) %>%
      dplyr::summarize(cell.height  = mean(cell.height),
                       yield.height = mean(yield.height),
                       cell.color   = round(median(cell.color)),
                       crop.alpha   = sum(crop.alpha),
                       yield.alpha  = sum(yield.alpha),
                       fert.level   = sum(fert.level)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(cell.color = factor(cell.color, labels = cell.border.palette[1:length(unique(cell.color))]))

    cell.max <- hop.full$cells %>%
      dplyr::mutate(crop.alpha  = .[[vars$crop.alpha]]) %>%
      dplyr::mutate(yield.alpha = .[[vars$yield.alpha]]) %>%
      dplyr::mutate(fert.level  = nitrogenFertilisation) %>%
      dplyr::filter(y %in% Yc) %>%
      dplyr::group_by(SimulationName, Year, Date, x) %>%
      dplyr::summarize(crop.alpha  = sum(crop.alpha),
                       yield.alpha = sum(yield.alpha),
                       fert.level  = sum(fert.level)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(!!!crop.grouping.symbols) %>%
      dplyr::summarize(crop.alpha.max  = max(crop.alpha),
                       yield.alpha.max = max(yield.alpha),
                       fert.level.max  = max(fert.level))

    cell.data <- cell %>%
      dplyr::left_join(cell.max, by = crop.grouping.strings) %>%
      dplyr::mutate(crop.alpha  = nan_to_zero(crop.alpha  / crop.alpha.max)) %>%
      dplyr::mutate(yield.alpha = nan_to_zero(yield.alpha / yield.alpha.max)) %>%
      dplyr::mutate(fert.level  = nan_to_zero(fert.level  / fert.level.max))
  }

  if(voxels) {
    voxel <- hop$voxels %>%
      dplyr::mutate(voxel.alpha   = .[[vars$voxel.alpha]])   %>%
      dplyr::mutate(voxel.border  = .[[vars$voxel.border]])  %>%
      dplyr::mutate(voxel.L.size  = .[[vars$voxel.L.size]])  %>%
      dplyr::mutate(voxel.C.size  = .[[vars$voxel.C.size]])  %>%
      dplyr::mutate(voxel.R.size  = .[[vars$voxel.R.size]])  %>%
      dplyr::mutate(voxel.L.alpha = .[[vars$voxel.L.alpha]]) %>%
      dplyr::mutate(voxel.C.alpha = .[[vars$voxel.C.alpha]]) %>%
      dplyr::mutate(voxel.R.alpha = .[[vars$voxel.R.alpha]]) %>%
      dplyr::filter(y %in% Yv) %>%
      dplyr::filter(z <= abs(max.soil.depth)) %>%
      dplyr::select(SimulationName, Year, Date, x, z, voxel.alpha, voxel.border,
                    voxel.L.size, voxel.C.size, voxel.R.size,
                    voxel.L.alpha, voxel.C.alpha, voxel.R.alpha) %>%
      dplyr::group_by(SimulationName, Year, Date, x, z) %>%
      dplyr::summarize_all(sum) %>%
      dplyr::ungroup()

    voxel.max <- hop.full$voxels %>%
      dplyr::mutate(voxel.alpha   = .[[vars$voxel.alpha]])   %>%
      dplyr::mutate(voxel.border  = .[[vars$voxel.border]])  %>%
      dplyr::mutate(voxel.L.size  = .[[vars$voxel.L.size]])  %>%
      dplyr::mutate(voxel.C.size  = .[[vars$voxel.C.size]])  %>%
      dplyr::mutate(voxel.R.size  = .[[vars$voxel.R.size]])  %>%
      dplyr::mutate(voxel.L.alpha = .[[vars$voxel.L.alpha]]) %>%
      dplyr::mutate(voxel.C.alpha = .[[vars$voxel.C.alpha]]) %>%
      dplyr::mutate(voxel.R.alpha = .[[vars$voxel.R.alpha]]) %>%
      dplyr::filter(y %in% Yv) %>%
      dplyr::filter(z <= abs(max.soil.depth)) %>%
      dplyr::group_by(SimulationName, Year, Date, x, z) %>%
      dplyr::summarize(voxel.alpha   = sum(voxel.alpha),
                       voxel.border  = sum(voxel.border),
                       voxel.L.size  = sum(voxel.L.size),
                       voxel.C.size  = sum(voxel.C.size),
                       voxel.R.size  = sum(voxel.R.size),
                       voxel.L.alpha = sum(voxel.L.alpha),
                       voxel.C.alpha = sum(voxel.C.alpha),
                       voxel.R.alpha = sum(voxel.R.alpha)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(!!!voxel.grouping.symbols) %>%
      dplyr::summarize(voxel.alpha.max   = max(voxel.alpha),
                       voxel.border.max  = max(voxel.border),
                       voxel.L.size.max  = max(voxel.L.size),
                       voxel.C.size.max  = max(voxel.C.size),
                       voxel.R.size.max  = max(voxel.R.size),
                       voxel.L.alpha.max = max(voxel.L.alpha),
                       voxel.C.alpha.max = max(voxel.C.alpha),
                       voxel.R.alpha.max = max(voxel.R.alpha))

    voxel.data <- voxel %>%
      dplyr::left_join(voxel.max, by = voxel.grouping.strings) %>%
      dplyr::mutate(voxel.alpha    = voxel.alpha   / voxel.alpha.max)  %>%
      dplyr::mutate(voxel.border   = voxel.border  / voxel.border.max * rect.max.border + rect.min.border) %>%
      dplyr::mutate(voxel.L.size   = voxel.L.size  / voxel.L.size.max * circle.max.radius) %>%
      dplyr::mutate(voxel.C.size   = voxel.C.size  / voxel.C.size.max * circle.max.radius) %>%
      dplyr::mutate(voxel.R.size   = voxel.R.size  / voxel.R.size.max * circle.max.radius) %>%
      dplyr::mutate(voxel.L.alpha  = voxel.L.alpha / voxel.L.alpha.max)  %>%
      dplyr::mutate(voxel.C.alpha  = voxel.C.alpha / voxel.C.alpha.max)  %>%
      dplyr::mutate(voxel.R.alpha  = voxel.R.alpha / voxel.R.alpha.max)  %>%
      dplyr::mutate(voxel.L.border = circle.max.border * as.numeric(voxel.L.size > 0)) %>%
      dplyr::mutate(voxel.C.border = circle.max.border * as.numeric(voxel.C.size > 0)) %>%
      dplyr::mutate(voxel.R.border = circle.max.border * as.numeric(voxel.R.size > 0))
  }

  if(climate) {
    climate.data <- hop$climate %>%
      dplyr::left_join(hop$plot.info, by = "SimulationName") %>%
      dplyr::mutate(precip.magnitude = nan_to_zero(precipitation / max(hop.full$climate$precipitation))) %>%
      dplyr::mutate(soilDepth        = -soilDepth) %>%
      dplyr::mutate(water.table      = as.numeric(waterTableDepth > soilDepth)) %>%
      dplyr::select(SimulationName, Year, Date, precip.magnitude, waterTableDepth, soilDepth, plotWidth, water.table)
    climate.data$waterTableDepth[climate.data$waterTableDepth < climate.data$soilDepth] <- 0
  }

  ## CREATE PLOT
  plot.obj <- ggplot() +
    labs(x = x.lab, y = "Z (m)", title = date) +
    scale_alpha_identity() +
    scale_fill_identity() +
    scale_color_identity() +
    scale_size_identity() +
    scale_linetype_identity() +
    coord_equal(xlim   = c(X.MIN, X.MAX),
                ylim   = c(Y.MIN, Y.MAX),
                expand = FALSE) +
    scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
    scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
    theme_bw(base_size = 18) +
    theme(plot.margin      = unit(18 * c(1,1,1,1), "points"),
          panel.spacing    = unit(2, "lines"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title       = element_text(size = 30, hjust = 0.5),
          axis.text        = element_text(color = "black"),
          axis.ticks       = element_line(color = "black"),
          axis.title.x     = element_text(vjust = -1, size = 30, color = "black"),
          axis.title.y     = element_text(vjust = 2,  size = 30, color = "black"),
          strip.text       = element_text(size  = 30))

  ## PRECIPITATION
  if(climate) {
    plot.obj <- plot.obj +
      geom_rect(data = climate.data,
                fill = "blue",
                xmin = 0,
                ymin = Y.MAX - arrow.length,
                ymax = Y.MAX,
                na.rm = TRUE,
                aes(alpha = precip.magnitude,
                    xmax  = plotWidth))
  }

  if(crops) {
    plot.obj <- plot.obj +
      geom_rect(data = cell.data,
                fill = "green",
                size = 1,
                na.rm = TRUE,
                aes(alpha = crop.alpha,
                    color = cell.color,
                    xmin  = x,
                    xmax  = x + cellWidth,
                    ymin  = 0,
                    ymax  = cell.height)) +
      geom_rect(data  = cell.data,
                fill  = "black",
                color = "black",
                size  = 1,
                na.rm = TRUE,
                aes(alpha = yield.alpha,
                    xmin  = x,
                    xmax  = x + cellWidth,
                    ymin  = cell.height,
                    ymax  = yield.height + cell.height)) +
      ## FERTILIZATION
      geom_segment(data  = cell.data,
                   color = "green",
                   arrow = arrow.type,
                   na.rm = TRUE,
                   aes(size = 2 * fert.level,
                       x    = x + cellWidth / 2,
                       y    = yield.height + cell.height + arrow.length * 2,
                       xend = x + cellWidth / 2,
                       yend = yield.height + cell.height))
  }

  if(trees) {
    plot.obj <- plot.obj +
      ## TRUNK
      geom_polygon(data  = trunk.data,
                   fill  = "brown",
                   color = "brown",
                   size  = 0.5,
                   na.rm = TRUE,
                   aes(alpha = trunk.alpha,
                       group = id,
                       x     = x,
                       y     = y)) +
      ## CROWN
      ggforce::geom_ellipsis(data  = tree.data,
                             fill  = "green",
                             color = "dark green",
                             size  = 0.5,
                             na.rm = TRUE,
                             aes(alpha    = crown.alpha,
                                 linetype = crown.linetype,
                                 x0       = tree.x,
                                 y0       = crown.center.y,
                                 b        = (tree.height - crown.base.height) / 2,
                                 a        = crown.radius,
                                 angle    = 0)) +
      ## HEIGHT GROWTH
      geom_segment(data  = tree.data,
                   color = "black",
                   arrow = arrow.type,
                   na.rm = TRUE,
                   aes(size = arrow.size * height.growth,
                       x    = tree.x,
                       y    = tree.height - arrow.length,
                       xend = tree.x,
                       yend = tree.height)) +
      ## TRUNK GROWTH
      geom_segment(data  = tree.data,
                   color = "black",
                   arrow = arrow.type,
                   na.rm = TRUE,
                   aes(size = arrow.size * trunk.growth,
                       x    = tree.x - trunk.radius,
                       y    = crown.base.height / 2,
                       xend = tree.x - trunk.radius - arrow.length,
                       yend = crown.base.height / 2)) +
      geom_segment(data  = tree.data,
                   color = "black",
                   arrow = arrow.type,
                   na.rm = TRUE,
                   aes(size = arrow.size * trunk.growth,
                       x    = tree.x + trunk.radius,
                       y    = crown.base.height / 2,
                       xend = tree.x + trunk.radius + arrow.length,
                       yend = crown.base.height / 2)) +
      ## CROWN GROWTH
      geom_segment(data  = tree.data,
                   color = "black",
                   arrow = arrow.type,
                   na.rm = TRUE,
                   aes(size = arrow.size * crown.growth,
                       x    = tree.x - crown.radius + arrow.length,
                       y    = crown.center.y,
                       xend = (tree.x - crown.radius),
                       yend = crown.center.y)) +
      geom_segment(data  = tree.data,
                   color = "black",
                   arrow = arrow.type,
                   na.rm = TRUE,
                   aes(size = arrow.size * crown.growth,
                       x    = tree.x + crown.radius - arrow.length,
                       y    = crown.center.y,
                       xend = (tree.x + crown.radius),
                       yend = crown.center.y)) +
      ## TREE PRUNING
      geom_segment(data  = tree.data,
                   color = "red",
                   na.rm = TRUE,
                   aes(size = 2 * tree.pruning,
                       x    = tree.x - crown.radius,
                       y    = tree.pruning.height,
                       xend = tree.x + crown.radius,
                       yend = tree.pruning.height)) +
      ## ROOT PRUNING
      geom_segment(data  = tree.data,
                   color = "red",
                   na.rm = TRUE,
                   aes(size = 2 * root.pruning,
                       x    = tree.x + root.pruning.distance,
                       y    = max(hop$plot.info$cellWidth) / 2,
                       xend = tree.x + root.pruning.distance,
                       yend = -root.pruning.depth)) +
      geom_segment(data  = tree.data,
                   color = "red",
                   na.rm = TRUE,
                   aes(size = 2 * root.pruning,
                       x    = tree.x - root.pruning.distance,
                       y    = max(hop$plot.info$cellWidth) / 2,
                       xend = tree.x - root.pruning.distance,
                       yend = -root.pruning.depth))
  }

  if(voxels) {
    plot.obj <- plot.obj +
      ## VOXELS
      geom_tile(data  = voxel.data,
                color = "black",
                fill  = "brown",
                na.rm = TRUE,
                aes(x     = x,
                    y     = -z,
                    size  = voxel.border,
                    alpha = voxel.alpha)) +
      ## CENTER CIRCLE
      ggforce::geom_circle(data  = voxel.data,
                           color = "black",
                           fill  = "white",
                           na.rm = TRUE,
                           aes(x0    = x,
                               y0    = -z,
                               size  = voxel.C.border,
                               r     = voxel.C.size)) +
      ggforce::geom_circle(data  = voxel.data,
                           color = "black",
                           fill  = "black",
                           na.rm = TRUE,
                           aes(x0    = x,
                               y0    = -z,
                               size  = voxel.C.border,
                               r     = voxel.C.size,
                               alpha = voxel.C.alpha)) +
      ## LEFT CIRCLE
      ggforce::geom_circle(data  = voxel.data,
                           color = "blue",
                           fill  = "white",
                           na.rm = TRUE,
                           aes(x0    = x - circle.offset,
                               y0    = -z,
                               size  = voxel.L.border,
                               r     = voxel.L.size)) +
      ggforce::geom_circle(data  = voxel.data,
                           color = "blue",
                           fill  = "blue",
                           na.rm = TRUE,
                           aes(x0    = x - circle.offset,
                               y0    = -z,
                               size  = voxel.L.border,
                               r     = voxel.L.size,
                               alpha = voxel.L.alpha)) +
      ## RIGHT CIRCLE
      ggforce::geom_circle(data  = voxel.data,
                           color = "green",
                           fill  = "white",
                           na.rm = TRUE,
                           aes(x0    = x + circle.offset,
                               y0    = -z,
                               size  = voxel.R.border,
                               r     = voxel.R.size)) +
      ggforce::geom_circle(data  = voxel.data,
                           color = "green",
                           fill  = "green",
                           na.rm = TRUE,
                           aes(x0    = x + circle.offset,
                               y0    = -z,
                               size  = voxel.R.border,
                               r     = voxel.R.size,
                               alpha = voxel.R.alpha))
  }

  if(climate & voxels) {
    ## WATER TABLE
    plot.obj <- plot.obj +
      geom_segment(data     = climate.data,
                   color    = "blue",
                   linetype = "longdash",
                   x        = 0,
                   na.rm = TRUE,
                   aes(size = 1.5 * water.table,
                       y    = waterTableDepth,
                       xend = plotWidth,
                       yend = waterTableDepth))
  }

  if("hop-group" %in% class(hop)) plot.obj <- plot.obj + facet_wrap(~SimulationName, nrow = plot.rows)

  return(plot.obj)
}

#' Create daily plots combining hisafe_slice() & plot_hisafe_cells()
#' @description Creates daily plots combining \code{\link{hisafe_slice}} & \code{\link{plot_hisafe_cells}} and writes them to an output directory.
#' Requires the gtable package.
#' This function creates the raw materials (daily images) for animations/videos of Hi-sAFe simulations.
#' @return Invisibly returns the final plot object that was created.
#' @param hop An object of class hop.
#' @param output.path A character stting indicating the path to the directory where plots should be saved.
#' If no value is provided, the experiment/simulation path is read from the hop object, and a directory is created there called "analysis/snapshots".
#' @param file.prefix A character string of the prefix with which to name each plot file. File names will be this prefix appended with the date.
#' @param cells.var A character string of the name of the variable to pass to \code{\link{plot_hisafe_cells}}.
#' @param date.min A character string of the minimum date to plot, in the format "YYYY-MM-DD" or of class Date.
#' If NA, the minimum date in \code{hop} is used. Only used if \code{dates} is \code{NULL}.
#' @param date.max A character string of the maximum date to plot, in the format "YYYY-MM-DD" or of class Date.
#' If NA, the maximum date in \code{hop} is used. Only used if \code{dates} is \code{NULL}.
#' @param dates A character vector (in the format "YYYY-MM-DD") or a vector of class Date of the dates to plot.
#' If \code{NULL}, then \code{date.max} and \code{date.min} are used instad.
#' @param rel.dates A character vector (in the format "YYYY-MM-DD") or a vector of class Date of the dates from which to scale all variables.
#' In the plot, all variables will be scaled to be between their minimum and maximum values across these dates.
#' @param simu.names A character vector of the SimulationNames within \code{hop} to include. Use "all" to include all available values.
#' @param plot.x Either "x" or "y", indicating which axis of the Hi-sAFe scene should be plotted along the x-axis of the plot.
#' This will be applied to the plots from both \code{\link{hisafe_slice}} and \code{\link{plot_hisafe_cells}}.
#' Setting to "xy" is only possible when \code{hop} contains or is filtered to a single simulation.
#' @param slice A logical indicating whether the plot from \code{\link{hisafe_slice}} should be included.
#' @param trees A logical indicating whether to plot trees via \code{\link{plot_hisafe_cells}}.
#' @param crops A logical indicating whether to plot crops via \code{\link{plot_hisafe_cells}}.
#' @param voxels A logical indicating whether to plot voxels via \code{\link{plot_hisafe_cells}}.
#' @param cells A logical indicating whether the plot from \code{\link{plot_hisafe_cells}} should be included.
#' @param complete.only A logical indicating whether plots should only be created if ALL simulations have data for the desired crops/cells/voxels on \code{date}.
#' @param mem.max An integer specifying the maximum number of days into the past to search
#' for crop/cell/voxel data when no data is available for a given date within \code{hop}.
#' @param device Graphical device to use for output files. See ggplot2::ggsave().
#' @param dpi Resolution of output files. See ggplot2::ggsave().
#' @param vars A list of variable names passed to \code{\link{hisafe_slice}}. See \code{\link{hisafe_slice}} for details.
#' @param ... Other arguments passed to \code{\link{hisafe_slice}}.
#' @export
#' @importFrom dplyr %>%
#' @import ggplot2
#' @import gtable
#' @import egg
#' @family hisafe visualization functions
#' @examples
#' \dontrun{
#' }
hisafe_snapshot <- function(hop,
                            output.path   = NULL,
                            file.prefix   = "HISAFE_Snapshot",
                            cells.var     = "relativeTotalParIncident",
                            date.min      = NA,
                            date.max      = NA,
                            dates         = NULL,
                            rel.dates     = NULL,
                            simu.names    = "all",
                            plot.x        = "x",
                            slice         = TRUE,
                            trees         = TRUE,
                            crops         = TRUE,
                            voxels        = TRUE,
                            cells         = TRUE,
                            complete.only = FALSE,
                            mem.max       = 10,
                            device        = "png",
                            dpi           = 250,
                            vars = list(crown.alpha   = "leafArea",
                                        trunk.alpha   = "carbonLabile",
                                        crop.alpha    = "lai",
                                        yield.alpha   = "eai",
                                        voxel.alpha   = "totalTreeRootDensity",
                                        voxel.border  = "cropRootDensity",
                                        voxel.L.size  = "waterStock",
                                        voxel.C.size  = "totalTreeCoarseRootBiomass",
                                        voxel.R.size  = "mineralNitrogenStock",
                                        voxel.L.alpha = "totalTreeWaterUptake",
                                        voxel.C.alpha = "fineRootCost",
                                        voxel.R.alpha = "totalTreeNitrogenUptake"), ...) {

  #####################################################################
  ##### MODIFY egg::gtable_frame #####
  ## Once/if egg::gtable_frame is updated in the CRAN version of the package,
  ## this can be removed as well as the @import egg in this function's documentation.
  b <- body(gtable_frame)
  b[6] <- parse(text = "if (fixed_ar) {
                ar <- as.numeric(g$heights[tt[1]]) / as.numeric(g$widths[ll[1]])
                height <- width * (ar / length(ll))
                g$respect <- FALSE
                }")
  body(gtable_frame) <- b
  assignInNamespace("gtable_frame", gtable_frame, ns = 'egg')
  #####################################################################

  if(!requireNamespace(c("gtable"), quietly = TRUE)) stop("The package 'gtable' is required for hisafe_snapshot(). Please install and load it.", call. = FALSE)
  is_hop(hop, error = TRUE)
  profile_check(hop, "trees", error = TRUE)
  if(!(is.character(output.path) | is.null(output.path))) stop("output.path argument must be a character vector", call. = FALSE)
  if(!is.character(file.prefix))                          stop("file.prefix argument must be a character vector", call. = FALSE)
  is_logical(slice)
  is_logical(cells)

  cells <- cells & profile_check(hop, "cells")

  legend.path <- clean_path(paste0(diag_output_path(hop, output.path), "/snapshots/"))
  image.path  <- paste0(legend.path, "snapshot_images/")
  dir.create(image.path, recursive = TRUE, showWarnings = FALSE)

  hop <- hop_filter(hop = hop, simu.names = simu.names)
  if(plot.x == "xy" & length(unique(hop$trees$SimulationName)) > 1) stop("stereo = TRUE is only allowed when plotting a single simulation", call. = FALSE)

  if(is.na(date.min)) date.min <- min(hop$trees$Date)
  if(is.na(date.max)) date.max <- max(hop$trees$Date)
  if(is.null(dates)) {
    dates <- seq(lubridate::ymd(date.min), lubridate::ymd(date.max), "day")
  } else {
    dates <- lubridate::ymd(dates)
    dates <- dates[dates %in% unique(hop$trees$Date)]
  }


  if(complete.only) {
    if((cells | crops) & profile_check(hop, "cells"))  dates <- extract_complete_dates(hop = hop, profile = "cells",  dates = dates)
    if(voxels          & profile_check(hop, "voxels")) dates <- extract_complete_dates(hop = hop, profile = "voxels", dates = dates)
  }

  legend.plot <- visual_legend(hop       = hop,
                               vars      = vars,
                               cells.var = cells.var)
  ggsave_fitmax(paste0(legend.path, file.prefix, "_LEGEND.png"), legend.plot, dpi = 500)

  if(length(dates) == 0) stop("date filtering resulted in no dates to plot", call. = FALSE)
  print(paste0("Creating visualizations for ", length(dates), " dates..."), quote = FALSE)
  pb <- txtProgressBar(min = 0, max = length(dates), initial = 0, style = 3)
  for(i in 1:length(dates)) {
    if(plot.x == "xy") {
      slice.plot.1 <- hisafe_slice(hop          = hop,
                                   date         = dates[i],
                                   rel.dates    = rel.dates,
                                   height.dates = rel.dates,
                                   simu.names   = simu.names,
                                   vars         = vars,
                                   trees        = trees,
                                   crops        = crops,
                                   voxels       = voxels,
                                   plot.x       = "x",
                                   mem.max      = mem.max, ...) +
        theme(axis.title.x = element_blank(),
              axis.text.x  = element_blank(),
              plot.margin  = margin(5,5,0,10),
              plot.title   = element_text(size = 30, hjust = 0))
      slice.plot.2 <- hisafe_slice(hop          = hop,
                                   date         = dates[i],
                                   rel.dates    = rel.dates,
                                   height.dates = rel.dates,
                                   simu.names   = simu.names,
                                   vars         = vars,
                                   trees        = trees,
                                   crops        = crops,
                                   voxels       = voxels,
                                   plot.x       = "y",
                                   mem.max      = mem.max, ...) +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x  = element_blank(),
              axis.text.y  = element_blank(),
              plot.margin  = margin(5,5,0,10),
              plot.title   = element_blank())

      if(cells) {
        cells.plot <- plot_hisafe_cells(hop        = hop,
                                        variable   = cells.var,
                                        dates      = dates[i],
                                        rel.dates  = rel.dates,
                                        simu.names = simu.names,
                                        plot.x     = "x",
                                        mem.max    = mem.max,
                                        for.anim   = TRUE) +
          theme(plot.margin  = margin(10,10,15,10),
                axis.title.x = element_blank(),
                axis.text.x  = element_blank())
      }
      if(cells) {
        plot.obj <- egg::ggarrange(slice1, slice2, cells1, nrow = 2, draw = FALSE)
      } else {
        plot.obj <- egg::ggarrange(slice1, slice2, nrow = 1, draw = FALSE)
      }
    } else {
      if(slice) {
        slice.plot <- hisafe_slice(hop          = hop,
                                   date         = dates[i],
                                   rel.dates    = rel.dates,
                                   height.dates = rel.dates,
                                   simu.names   = simu.names,
                                   vars         = vars,
                                   trees        = trees,
                                   crops        = crops,
                                   voxels       = voxels,
                                   plot.x       = plot.x,
                                   mem.max      = mem.max, ...) +
          theme(axis.title.x = element_blank(),
                axis.text.x  = element_blank(),
                axis.ticks.x = element_blank(),
                plot.margin  = margin(5,5,0,10))
      }

      if(cells) {
        cells.plot <- plot_hisafe_cells(hop          = hop,
                                        variable     = cells.var,
                                        dates        = dates[i],
                                        rel.dates    = rel.dates,
                                        simu.names   = simu.names,
                                        plot.x       = plot.x,
                                        mem.max      = mem.max,
                                        for.anim     = TRUE) +
          theme(plot.margin  = margin(10,10,15,10))
      }

      if(slice & cells) {
        plot.obj <- egg::ggarrange(slice.plot, cells.plot, ncol = 1, draw = FALSE)
      } else if(slice & !cells) {
        plot.obj <- slice.plot
      } else if(!slice & cells) {
        plot.obj <- cells.plot
      }
    }
    ggsave_fitmax(paste0(image.path, file.prefix, "_", dates[i], ".", device), plot.obj, scale = 2, dpi = dpi)
    setTxtProgressBar(pb, i)
  }

  invisible(plot.obj)
}

#' Create a legend for hisafe_visual()
#' @description Creates a legend for hisafe_visual().
#' @return A ggplot object containing the legend.
#' @param hop An object of class hop.
#' @param vars A list of variable names. See \code{\link{hisafe_slice}} for details.
#' @param cells.var A character string of the name of the variable ploted by \code{\link{plot_hisafe_cells}}.
#' @import ggplot2
#' @export
visual_legend <- function(hop,
                          vars = list(crown.alpha   = "leafArea",
                                      trunk.alpha   = "carbonLabile",
                                      crop.alpha    = "lai",
                                      yield.alpha   = "eai",
                                      voxel.alpha   = "totalTreeRootDensity",
                                      voxel.border  = "cropRootDensity",
                                      voxel.L.size  = "waterStock",
                                      voxel.C.size  = "totalTreeCoarseRootBiomass",
                                      voxel.R.size  = "mineralNitrogenStock",
                                      voxel.L.alpha = "totalTreeWaterUptake",
                                      voxel.C.alpha = "fineRootCost",
                                      voxel.R.alpha = "totalTreeNitrogenUptake"),
                          cells.var = "relativeTotalParIncident") {
  text.size <- 2
  border.thickness  <- 0.5
  pointer.thickness <- border.thickness / 2
  tree.data  <- dplyr::tibble(crown.alpha       = 1,
                              tree.x            = 0.5,
                              tree.height       = 1.3,
                              crown.radius      = 0.5,
                              crown.center.y    = 1.3*3/4,
                              crown.base.height = 1.3*0.5)
  trunk.data <- dplyr::tibble(trunk.alpha = 1,
                              polygon.x   = c(0.4, 0.6, 0.5),
                              polygon.y   = c(0, 0, 1))
  cell.data  <- dplyr::tibble(crop.alpha   = 0.8,
                              cell.color   = "#E69F00",
                              x            = 1.25,
                              cellWidth    = 1,
                              cell.height  = 0.3,
                              yield.alpha  = 0.2,
                              yield.height = 0.15)
  voxel.data <- dplyr::tibble(voxel.border   = 0.5,
                              voxel.alpha    = 0.2,
                              x              = 5.25,
                              z              = -0.75,
                              cellWidth      = 1,
                              voxel.height   = 0.25,
                              circle.offset  = 0.5,
                              voxel.L.border = 1,
                              voxel.C.border = 1,
                              voxel.R.border = 1,
                              voxel.L.size   = 0.25*0.9/2,
                              voxel.C.size   = 0.25*0.9/2,
                              voxel.R.size   = 0.25*0.9/2,
                              voxel.L.alpha  = 0.5,
                              voxel.C.alpha  = 0.5,
                              voxel.R.alpha  = 0.5)
  line.data <- dplyr::tibble(x       = 8,
                             x.width = 1)


  plot.obj <- ggplot() +
    scale_x_continuous(limits = c(0, NA)) +
    scale_y_continuous(limits = c(0, NA)) +
    theme_void() +
    scale_alpha_identity() +
    scale_color_identity() +
    scale_size_identity() +
    coord_equal()

  ### TREES
  plot.obj <- plot.obj +
    ## TRUNK
    geom_polygon(data  = trunk.data,
                 fill  = "brown",
                 color = "brown",
                 size  = 1,
                 aes(alpha = trunk.alpha,
                     x     = polygon.x,
                     y     = polygon.y)) +
    ## CROWN
    ggforce::geom_ellipsis(data  = tree.data,
                           fill  = "green",
                           color = "green",
                           size  = 1,
                           aes(alpha = crown.alpha,
                               x0    = tree.x,
                               y0    = crown.center.y,
                               b     = (tree.height - crown.base.height) / 2,
                               a     = crown.radius,
                               angle = 0)) +
    geom_text(data  = tree.data,
              hjust = 0.5,
              size  = text.size,
              label = vars$crown.alpha,
              aes(x = tree.x,
                  y = crown.center.y)) +
    geom_text(data  = tree.data,
              hjust = 0.5,
              size  = text.size,
              label = vars$trunk.alpha,
              aes(x = tree.x,
                  y = tree.height / 4))

  ### CROPS
  plot.obj <- plot.obj +
    geom_rect(data = cell.data,
              fill = "green",
              size = border.thickness,
              aes(alpha    = crop.alpha,
                  color    = cell.color,
                  xmin     = x,
                  xmax     = x + cellWidth,
                  ymin     = 0,
                  ymax     = cell.height)) +
    geom_rect(data  = cell.data,
              fill  = "black",
              color = "black",
              size  = border.thickness,
              aes(alpha    = yield.alpha,
                  xmin     = x,
                  xmax     = x + cellWidth,
                  ymin     = cell.height,
                  ymax     = yield.height + cell.height)) +
    geom_text(data  = cell.data,
              hjust = 0.5,
              size  = text.size,
              label = vars$crop.alpha,
              aes(x = x + cellWidth / 2,
                  y = cell.height / 2)) +
    geom_text(data  = cell.data,
              hjust = 0.5,
              size  = text.size,
              label = vars$yield.alpha,
              aes(x = x + cellWidth / 2,
                  y = cell.height + (yield.height / 2))) +
    geom_text(data  = cell.data,
              hjust = 0,
              size  = text.size,
              label = "yield",
              aes(x = x + cellWidth + 0.25,
                  y = cell.height + (yield.height / 2))) +
    geom_segment(data  = cell.data,
                 color = "black",
                 size  = pointer.thickness,
                 aes(x    = x + cellWidth + 0.05,
                     y    = cell.height + (yield.height / 2),
                     xend = x + cellWidth + 0.2,
                     yend = cell.height + (yield.height / 2))) +
    geom_segment(data  = cell.data,
                 color = "black",
                 size  = pointer.thickness,
                 aes(x    = x + cellWidth + 0.05,
                     y    = cell.height,
                     xend = x + cellWidth + 0.05,
                     yend = cell.height + yield.height)) +
    geom_segment(data  = cell.data,
                 color = "black",
                 size  = pointer.thickness,
                 aes(x    = x + cellWidth + 0.05 + 0.025,
                     y    = cell.height,
                     xend = x + cellWidth + 0.05 - 0.025,
                     yend = cell.height)) +
    geom_segment(data  = cell.data,
                 color = "black",
                 size  = pointer.thickness,
                 aes(x    = x + cellWidth + 0.05 + 0.025,
                     y    = cell.height + yield.height,
                     xend = x + cellWidth + 0.05 - 0.025,
                     yend = cell.height + yield.height)) +
    geom_text(data  = cell.data,
              hjust = 0,
              size  = text.size,
              label = "phenological stage",
              aes(x = x + cellWidth + 0.25,
                  y = cell.height / 2)) +
    geom_segment(data  = cell.data,
                 color = "black",
                 size  = pointer.thickness,
                 aes(x    = x + cellWidth,
                     y    = cell.height / 2,
                     xend = x + cellWidth + 0.2,
                     yend = cell.height / 2))

  ### VOXELS
  plot.obj <- plot.obj +
    geom_rect(data  = voxel.data,
              color = "black",
              fill  = "brown",
              size  = border.thickness,
              aes(xmin = x - cellWidth,
                  ymin = -z - voxel.height,
                  xmax = x + cellWidth,
                  ymax = -z,
                  alpha = voxel.alpha)) +
    geom_text(data  = voxel.data,
              hjust = 0,
              size  = text.size,
              label = vars$voxel.alpha,
              aes(x = x + cellWidth + 0.25,
                  y = -z - voxel.height / 2)) +
    geom_segment(data  = voxel.data,
                 color = "black",
                 size  = pointer.thickness,
                 aes(x    = x + cellWidth * 0.8,
                     y    = -z - 0.25 / 2,
                     xend = x + cellWidth + 0.2,
                     yend = -z - voxel.height / 2)) +
    geom_text(data  = voxel.data,
              hjust = 1,
              size  = text.size,
              label = vars$voxel.border,
              aes(x = x - cellWidth - 0.25,
                  y = -z - voxel.height / 2)) +
    geom_segment(data  = voxel.data,
                 color = "black",
                 size  = pointer.thickness,
                 aes(x    = x - cellWidth,
                     y    = -z - voxel.height / 2,
                     xend = x - cellWidth - 0.2,
                     yend = -z - voxel.height / 2)) +
    ## CENTER CIRCLE
    ggforce::geom_circle(data  = voxel.data,
                         color = "black",
                         fill  = "black",
                         size  = border.thickness,
                         aes(x0    = x,
                             y0    = -z - voxel.height / 2,
                             r     = voxel.C.size,
                             alpha = voxel.C.alpha)) +
    geom_text(data  = voxel.data,
              hjust = 0.5,
              size  = text.size,
              label = vars$voxel.C.alpha,
              aes(x = x,
                  y = -z - voxel.height / 2 + 0.5)) +
    geom_segment(data  = voxel.data,
                 color = "black",
                 size  = pointer.thickness,
                 aes(x    = x,
                     y    = -z - voxel.height / 4,
                     xend = x,
                     yend =  -z - voxel.height / 2 + 0.4)) +
    geom_text(data  = voxel.data,
              hjust = 0.5,
              size  = text.size,
              label = vars$voxel.C.size,
              aes(x = x,
                  y = -z - voxel.height / 2 - 0.5)) +
    geom_segment(data  = voxel.data,
                 color = "black",
                 size  = pointer.thickness,
                 aes(x    = x,
                     y    = -z - voxel.height / 2,
                     xend = x,
                     yend =  -z - voxel.height / 2 - 0.4)) +
    geom_segment(data  = voxel.data,
                 color = "black",
                 size  = pointer.thickness,
                 aes(x    = x - voxel.C.size * 1.1,
                     y    = -z - voxel.height / 2,
                     xend = x + voxel.C.size * 1.1,
                     yend = -z - voxel.height / 2)) +
    geom_segment(data  = voxel.data,
                 color = "black",
                 size  = pointer.thickness,
                 aes(x    = x - voxel.C.size * 1.1,
                     y    = -z - voxel.height / 2 - 0.025,
                     xend = x - voxel.C.size * 1.1,
                     yend = -z - voxel.height / 2 + 0.025)) +
    geom_segment(data  = voxel.data,
                 color = "black",
                 size  = pointer.thickness,
                 aes(x    = x + voxel.C.size * 1.1,
                     y    = -z - voxel.height / 2 - 0.025,
                     xend = x + voxel.C.size * 1.1,
                     yend = -z - voxel.height / 2 + 0.025)) +
    ## LEFT CIRCLE
    ggforce::geom_circle(data  = voxel.data,
                         color = "blue",
                         fill  = "blue",
                         size  = border.thickness,
                         aes(x0    = x - circle.offset,
                             y0    = -z - voxel.height / 2,
                             r     = voxel.L.size,
                             alpha = voxel.L.alpha)) +
    geom_text(data  = voxel.data,
              hjust = 0.5,
              size  = text.size,
              label = vars$voxel.L.alpha,
              aes(x = x - circle.offset - 0.5,
                  y = -z - voxel.height / 2 + 0.3))  +
    geom_segment(data  = voxel.data,
                 color = "black",
                 size  = pointer.thickness,
                 aes(x    = x - circle.offset,
                     y    = -z - voxel.height / 4,
                     xend = x - circle.offset - 0.5,
                     yend =  -z - voxel.height / 2 + 0.2)) +
    geom_text(data  = voxel.data,
              hjust = 0.5,
              size  = text.size,
              label = vars$voxel.L.size,
              aes(x = x - circle.offset - 0.5,
                  y = -z - voxel.height / 2 - 0.3)) +
    geom_segment(data  = voxel.data,
                 color = "black",
                 size  = pointer.thickness,
                 aes(x    = x - circle.offset,
                     y    = -z - voxel.height / 2,
                     xend = x - circle.offset - 0.5,
                     yend =  -z - voxel.height / 2 - 0.2)) +
    geom_segment(data  = voxel.data,
                 color = "black",
                 size  = pointer.thickness,
                 aes(x    = x - circle.offset - voxel.L.size * 1.1,
                     y    = -z - voxel.height / 2,
                     xend = x - circle.offset + voxel.L.size * 1.1,
                     yend = -z - voxel.height / 2)) +
    geom_segment(data  = voxel.data,
                 color = "black",
                 size  = pointer.thickness,
                 aes(x    = x - circle.offset - voxel.L.size * 1.1,
                     y    = -z - voxel.height / 2 - 0.025,
                     xend = x - circle.offset - voxel.L.size * 1.1,
                     yend = -z - voxel.height / 2 + 0.025)) +
    geom_segment(data  = voxel.data,
                 color = "black",
                 size  = pointer.thickness,
                 aes(x    = x - circle.offset + voxel.L.size * 1.1,
                     y    = -z - voxel.height / 2 - 0.025,
                     xend = x - circle.offset + voxel.L.size * 1.1,
                     yend = -z - voxel.height / 2 + 0.025)) +
    ## RIGHT CIRCLE
    ggforce::geom_circle(data  = voxel.data,
                         color = "green",
                         fill  = "green",
                         size  = border.thickness,
                         aes(x0    = x + circle.offset,
                             y0    = -z - voxel.height / 2,
                             r     = voxel.R.size,
                             alpha = voxel.R.alpha)) +
    geom_text(data  = voxel.data,
              hjust = 0.5,
              size  = text.size,
              label = vars$voxel.R.alpha,
              aes(x = x + circle.offset + 0.5,
                  y = -z - voxel.height / 2 + 0.3)) +
    geom_segment(data  = voxel.data,
                 color = "black",
                 size  = pointer.thickness,
                 aes(x    = x + circle.offset,
                     y    = -z - voxel.height / 4,
                     xend = x + circle.offset + 0.5,
                     yend =  -z - voxel.height / 2 + 0.2)) +
    geom_text(data  = voxel.data,
              hjust = 0.5,
              size  = text.size,
              label = vars$voxel.R.size,
              aes(x = x + circle.offset + 0.5,
                  y = -z - voxel.height / 2 - 0.3)) +
    geom_segment(data  = voxel.data,
                 color = "black",
                 size  = pointer.thickness,
                 aes(x    = x + circle.offset,
                     y    = -z - voxel.height / 2,
                     xend = x + circle.offset + 0.5,
                     yend =  -z - voxel.height / 2 - 0.2)) +
    geom_segment(data  = voxel.data,
                 color = "black",
                 size  = pointer.thickness,
                 aes(x    = x + circle.offset - voxel.R.size * 1.1,
                     y    = -z - voxel.height / 2,
                     xend = x + circle.offset + voxel.R.size * 1.1,
                     yend = -z - voxel.height / 2)) +
    geom_segment(data  = voxel.data,
                 color = "black",
                 size  = pointer.thickness,
                 aes(x    = x + circle.offset - voxel.R.size * 1.1,
                     y    = -z - voxel.height / 2 - 0.025,
                     xend = x + circle.offset - voxel.R.size * 1.1,
                     yend = -z - voxel.height / 2 + 0.025)) +
    geom_segment(data  = voxel.data,
                 color = "black",
                 size  = pointer.thickness,
                 aes(x    = x + circle.offset + voxel.R.size * 1.1,
                     y    = -z - voxel.height / 2 - 0.025,
                     xend = x + circle.offset + voxel.R.size * 1.1,
                     yend = -z - voxel.height / 2 + 0.025)) +
    ## LINES
    geom_segment(data     = line.data,
                 size     = border.thickness * 2,
                 color    = "blue",
                 linetype = "longdash",
                 y        = 0.2,
                 yend     = 0.2,
                 aes(x    = x,
                     xend = x + x.width)) +
    geom_text(data  = line.data,
              hjust = 0.5,
              size  = text.size,
              label = "water table",
              y     = 0.2 + 0.12,
              aes(x = x + x.width/2)) +
    geom_segment(data     = line.data,
                 size     = border.thickness * 2,
                 color    = "green",
                 arrow    = ggplot2::arrow(length = unit(5, "points")),
                 linetype = "solid",
                 y        = 0.6,
                 yend     = 0.6,
                 aes(x    = x,
                     xend = x + x.width)) +
    geom_text(data  = line.data,
              hjust = 0.5,
              size  = text.size,
              label = "fertilization",
              y     = 0.6 + 0.12,
              aes(x = x + x.width/2)) +
    geom_segment(data     = line.data,
                 size     = border.thickness * 2,
                 color    = "red",
                 linetype = "solid",
                 y        = 1,
                 yend     = 1,
                 aes(x    = x,
                     xend = x + x.width)) +
    geom_text(data  = line.data,
              hjust = 0.5,
              size  = text.size,
              label = "pruning",
              y     = 1 + 0.12,
              aes(x = x + x.width / 2))

  ### CELLS
  if(profile_check(hop, "cells")) {
    min.x <- 1.25
    max.x <- 1.5
    min.y <- 1
    max.y <- 1.25
    min.f <- min(hop$cells[[cells.var]])
    max.f <- max(hop$cells[[cells.var]])

    scale_ab <- function(x, a, b) (b - a) * (x - min(x)) / (max(x) - min(x)) + a

    mini.cells <- dplyr::as_tibble(expand.grid(x = c(1, 2, 3, 4, 5),
                                               y = c(1, 2, 3, 4, 5))) %>%
      dplyr::mutate(f = c(3, 2, 3, 2, 3,
                          2, 1, 1.25, 1, 2,
                          3, 0.5, 0, 0.5, 3,
                          2.5, 1.5, 1.75, 1.5, 2.5,
                          3.75, 2.75, 3.75, 2.75, 3.75)) %>%
      dplyr::mutate(x = scale_ab(x, min.x, max.x)) %>%
      dplyr::mutate(y = scale_ab(y, min.y, max.y)) %>%
      dplyr::mutate(f = scale_ab(f, min.f, max.f))

    plot.obj <- plot.obj +
      geom_raster(data = mini.cells, aes(x = x, y = y, fill = f)) +
      viridis::scale_fill_viridis(option = "magma") +
      guides(fill = guide_colourbar(title     = NULL,
                                    barwidth  = 4,
                                    direction = "horizontal",
                                    barheight = 0.5,
                                    nbin      = 100,
                                    label     = FALSE,
                                    ticks     = FALSE)) +
      theme(legend.position = c(0.27, 0.78)) +
      geom_text(aes(x = 1.65,
                    y = 1.25),
                hjust = 0,
                size  = text.size,
                label = cells.var) +
      geom_text(aes(x = 1.65,
                    y = 0.9),
                hjust = 0,
                size  = text.size,
                label = "min") +
      geom_text(aes(x = 2.8,
                    y = 0.9),
                hjust = 1,
                size  = text.size,
                label = "max")
  }

  return(plot.obj)
}

#' Add historic data
#' @description Adds historic data when cells/voxels data is missing for \code{\link{hisafe_slice}}
#' @param df A hop profile tibble
#' @param date date argument from \code{\link{hisafe_slice}}.
#' @param mem.max mem.max argument from \code{\link{hisafe_slice}}.
#' @keywords internal
add_historic_data <- function(df, dates, mem.max) {
  dates <- lubridate::ymd(dates)
  add_hist <- function(date, df, mem.max) {
    df %>%
      dplyr::filter(Date < date, Date >= date - mem.max) %>%
      dplyr::group_by(SimulationName, Date) %>%
      dplyr::summarize(n = n()) %>%
      dplyr::filter(n > 0) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(SimulationName) %>%
      dplyr::summarize(Date = max(Date)) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(df, by = c("SimulationName", "Date")) %>%
      dplyr::mutate(Date = date)
  }
  out <- purrr::map_df(dates, add_hist, df = df, mem.max = mem.max)
  return(out)
}

#' Build white boxes to cover phantom trees
#' @description Builds white boxes to cover phantom trees for \code{\link{hisafe_slice}}
#' @param hop An object of class hop.
#' @param profile Character string of the name of a hop profile.
#' @param dates Character vector or Date vector of dates to check.
#' @keywords internal
extract_complete_dates <- function(hop, profile, dates) {
  dates <- lubridate::ymd(dates)
  complete.dates <- hop[[profile]] %>%
    dplyr::group_by(SimulationName, Date) %>%
    dplyr::summarize(n = n()) %>%
    dplyr::mutate(n = as.numeric(n > 0)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Date) %>%
    dplyr::summarize(n = sum(n)) %>%
    dplyr::filter(n == length(unique(hop[[profile]]$SimulationName))) %>%
    .$Date
  dates <- dates[dates %in% complete.dates]
  return(dates)
}
# hisafe_animate <- function(path,
#                            interval    = 10,
#                            anim.format = "gif") {
#
#   path <- clean_path(paste0(R.utils::getAbsolutePath(path), "/"))
#   folder.name <- basename(path)
#   call <- paste0("convert -delay ", interval, " *.", " ", folder.name, ".", anim.format)
#
#   pre.wd <- getwd()
#   setwd(path)
#   log <- system(call, wait = TRUE, intern = TRUE)
#   setwd(pre.wd)
#
#   anim.path <- paste0(clean_path(paste0(path, "/")), folder.name, ".", anim.format)
#   invisible(anim.path)
# }

# #' Create a simulation progress bar
# #' @description Creates a simulation progress bar. Used within \code{\link{hisafe_visual}}.
# #' @return A ggplot object containing the progress bar
# #' @param hop An object of class hop or face.
# #' @param date A character string of the current simulation date, in the format "YYYY-MM-DD" or of class Date.
# #' @import ggplot2
# date_progress <- function(hop, date){
#   date.lab <- as.character(date)
#   date     <- lubridate::ymd(date)
#   date.min <- min(hop$trees$Date)
#   date.max <- max(hop$trees$Date)
#
#   dat <- dplyr::tibble(date.min = date.min, date = date, date.max = date.max)
#
#   plot.obj <- ggplot(dat, aes(xmin = date.min)) +
#     labs(y = date.lab) +
#     geom_rect(aes(xmax = date),
#               ymin = 0,
#               ymax = 1,
#               color = NA,
#               fill  = "grey50",
#               alpha = 1) +
#     geom_rect(aes(xmax = date.max),
#               ymin = 0,
#               ymax = 1,
#               color = "black",
#               size  = 3,
#               fill  = NA) +
#     theme_void() +
#     scale_y_continuous(expand = c(0, 0)) +
#     scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) +
#     theme(#axis.title.y = element_blank(),
#       axis.title.y = element_text(color = "black",
#                                       size  = 30,
#                                       angle = 0,
#                                       hjust = 1,
#                                       vjust = 0.5),
#           axis.text.x  = element_text(color  = "black",
#                                       size   = 30,
#                                       angle  = 45,
#                                       hjust  = 1,
#                                       vjust  = 0.7),
#           plot.margin       = margin(15, 0, 15, 0),
#           aspect.ratio      = 0.1,
#           axis.ticks        = element_line(color = "black", size = 2),
#           axis.ticks.length = unit(-15, "points"))
#
#   return(plot.obj)
# }
