#' Plot annual barchart of major cycles
#' @description Plots an annual barchart of tree carbon pools, water fluxes, nitrogen fluxes, or light capture.
#' @return If \code{plot = TRUE}, returns a ggplot object. If \code{plot = FALSE}, returns the data that would create the plot.
#' If \code{hop} contains more than one simulation, the plot will be faceted by SimulationName.
#' @param hop An object of class hop or face.
#' @param cycle One of "carbon", "nitrogen", "water", or "light".
#' @param simu.names A character vector of the SimulationNames within \code{hop} to include. Use "all" to include all available values.
#' @param tree.ids A numeric vector indicating a subset of tree ids to plot. Use "all" to include all available values.
#' This only applies when \code{cycle} is "carbon".
#' @param year.lim A numeric vector of length two providing the \code{c(minimum, maximum)} of calendar years to plot.
#' If no input, the full available time range is plotted. Use \code{NA} to refer to the start or end of the simulation.
#' @param color.palette A character stirng of hex values or R standard color names defining the color palette to use in plots with multiple simulations.
#' If \code{NULL}, the default, then the default color palette is a color-blind-friendly color palette.
#' @param plot If \code{TRUE}, the default, a ggplot object is returned. If \code{FALSE}, the data that would create the plot is returned.
#' @details Detailed description of the flux components of the nitrogen and water cycles:
#'
#' NITROGEN
#' \itemize{
#'  \item{"Uptake - Trees"}{ - nitrogen uptake by trees}
#'  \item{"Uptake - Inter crop"}{ - nitrogen uptake by main crop}
#'  \item{"Uptake - Main crop"}{ - nitrogen uptake by inter crop}
#'  \item{"Gaseous losses"}{ - gaseous nitrogen losses via denitrification & volatilization of mineral/organic fertilizer inputs}
#'  \item{"Run-off"}{ - nitrogen contained in rain water (wet deposition) that runs off the scene}
#'  \item{"Leaching"}{ - nitrate leaching via the (1) bottom of the scene, (2) artificial drainage pipes, and (3) losses to the water table when the nirate concentration in the water table is lower than the nitrate concentration of voxels that it saturates}
#'  \item{"Fertilization"}{ - mineral nitrogen added by both mineral & organic fertilizers, plus organic nitrogen added by organic fetilizers}
#'  \item{"Irrigation"}{ - nitrogen added via irrigation water}
#'  \item{"Deposition"}{ - nitrogen added via wet deposition}
#'  \item{"Fixation"}{ - nitrogen added via fixation by crops}
#'  \item{"Litter"}{ - nitrogen added to soil via aerial and root litter from both trees and crops}
#'  \item{"Water table"}{ - nitrogen added by the water table when the nirate concentration in the water table is higher than the nitrate concentration of voxels that it saturates}
#' }
#'
#' WATER
#' \itemize{
#'  \item{"Uptake - Trees"}{ - water uptake by trees}
#'  \item{"Uptake - Inter crop"}{ - water uptake by main crop}
#'  \item{"Uptake - Main crop"}{ - water uptake by inter crop}
#'  \item{"Interception"}{ - rain water intercepted by both tree and crop canopies (and then subsequently evaporated off)}
#'  \item{"Run-off"}{ - rain & irrigation water that runs off the scene (including both the "surface" run-off associated with soil surface conditions plus the "overflow" runoff associated with saturation of the top soil layer and lack of infiltribility)}
#'  \item{"Soil evaporation"}{ - water evaporated from surface soil layers}
#'  \item{"Drainage"}{ - water drainage (1) out of the bottom of the scene, (2) into the water table, and (3) into artificial drainage pipes}
#'  \item{"Irrigation"}{ - water added via irrigation}
#'  \item{"Water table"}{ - water added to soil when the water table saturates voxels (the water added is the difference between field capacity and the current water content of the voxel)}
#'  \item{"Precipitation"}{ - precipitation (rain & snow)}
#' }
#' @export
#' @importFrom dplyr %>%
#' @import ggplot2
#' @family hisafe analysis functions
#' @examples
#' \dontrun{
#' # To plot the simulated water cycle:
#' water.plot <- plot_hisafe_cycle_annual(myhop, "water")
#'
#' # Once you have the plot object, you can display it and save it:
#' water.plot
#' ggsave_fitmax("water_cycle.png", water.plot)
#' }
plot_hisafe_cycle_annual <- function(hop,
                                     cycle,
                                     simu.names    = "all",
                                     tree.ids      = "all",
                                     year.lim      = c(NA, NA),
                                     color.palette = NULL,
                                     plot          = TRUE) {

  is_hop(hop, error = TRUE)
  if(simu.names[1] == "all") simu.names <- unique(hop$exp.plan$SimulationName)

  if(!(cycle %in% c("carbon", "nitrogen", "water", "light")))                  stop("cycle argument must be one of: carbon, nitrogen, water, light", call. = FALSE)
  if(!(length(year.lim) == 2 & (is.numeric(year.lim) | all(is.na(year.lim))))) stop("year.lim argument must be a numeric vector of length 2",        call. = FALSE)
  is_TF(plot)

  hop <- hop_filter(hop = hop, simu.names = simu.names, tree.ids = tree.ids)

  METHOD <- ifelse(profile_check(hop, "cells"), "cells", "plot")

  if(cycle == "water") {
    plot.data <- get_water_fluxes(hop = hop, profile = METHOD)
    plot.title <- "Water Cycle"
    y.lab      <- "Water flux (mm)"
    if(is.null(color.palette)) color.palette <- c("#D55E00", "#E69F00", "#F0E442", "grey20", "grey40", "grey60", "grey80",
                                                  "#009E73", "#0072B2", "#56B4E9")

  } else if(cycle == "nitrogen") {
    plot.data <- get_nitrogen_fluxes(hop = hop, profile = METHOD)
    plot.title <- "Nitrogen Cycle"
    y.lab      <- "N flux (kg N ha-1)"
    if(is.null(color.palette)) color.palette <- c("#D55E00", "#E69F00", "#F0E442", "grey20", "grey40", "grey80",
                                                  "#009E73", "#0072B2", "#56B4E9", "#CC79A7", "black", "white")

  } else if(cycle == "light") {
    plot.data  <- get_light_fluxes(hop = hop)
    plot.title <- "Light Capture"
    y.lab      <- "Intercepted PAR (% of incident PAR)"
    if(is.null(color.palette)) color.palette <- c("#E69F00", "grey20", "#56B4E9", "#009E73")

  } else if(cycle == "carbon") {
    if(!profile_check(hop, "trees")) return(NULL)
    plot.data  <- get_carbon_pools(hop = hop)
    plot.title <- "Tree Carbon Pools"
    y.lab      <- "Tree C storage (Mg C ha-1)"
    if(is.null(color.palette)) color.palette <- c("#009E73", "#999999", "#D55E00", "#E69F00", "#56B4E9", "#0072B2", "#F0E442")

  } else {
    stop("cycle argument not supported. Use one of: carbon, nitrogen, water, light.", call. = FALSE)
  }

  ## Set time limits
  if(is.na(year.lim[1])) year.lim[1] <- min(plot.data$Year)
  if(is.na(year.lim[2])) year.lim[2] <- max(plot.data$Year)

  ## Filter & Summarize plot data
  complete.yrs <- plot.data %>%
    dplyr::group_by(SimulationName, Year) %>%
    dplyr::filter(flux == flux[1]) %>%
    dplyr::summarize(n = n() >= 365)

  plot.data <- plot.data %>%
    dplyr::filter(Year >= year.lim[1], Year <= year.lim[2]) %>%
    dplyr::select(-Month, -Day, -Date, -JulianDay) %>%
    dplyr::left_join(complete.yrs, by = c("SimulationName", "Year")) %>%
    dplyr::filter(n) %>%
    dplyr::group_by(SimulationName, Year, flux)

  if(cycle == "carbon") {
    plot.data <- dplyr::summarize(plot.data, value = max(value))
  } else if(cycle == "light") {
    plot.data <- dplyr::summarize(plot.data, value = mean(value))
  } else {
    plot.data <- dplyr::summarize(plot.data, value = sum(value))
  }

  ## Set faceting
  if("face" %in% class(hop)) {
    EXP.PLAN <- hop$exp.plan
    AF.sims <- sort(EXP.PLAN$SimulationName[!(EXP.PLAN$SimulationName %in% c("Monocrop", "Forestry"))])
    if(length(AF.sims) == 1) facet_simu <- facet_wrap(~SimulationName, nrow = 1) else facet_simu <- facet_wrap(~SimulationName)
    all.sims <- c("Monocrop", AF.sims, "Forestry")
    all.sims <- all.sims[all.sims %in% simu.names]
    plot.data$SimulationName <- factor(plot.data$SimulationName, levels = all.sims)
    scale.x    <- scale_x_continuous(expand = c(0,0))
  } else if ("hop-group" %in% class(hop)) {
    facet_simu <- facet_wrap(~SimulationName)
    scale.x    <- scale_x_continuous(expand = c(0,0))
  } else {
    facet_simu <- geom_blank()
    scale.x    <- scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL), expand = c(0,0))
  }

  ## Create plot
  plot.obj <- ggplot(plot.data, aes(x = Year, y = value, fill = flux)) +
    labs(x     = "Year",
         y     = y.lab,
         title = plot.title) +
    facet_simu +
    scale.x +
    scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL), expand = c(0,0)) +
    geom_bar(stat = "identity", color = "black") +
    scale_fill_manual(values = color.palette) +
    theme_hisafe_ts() +
    theme(legend.title      = element_blank(),
          axis.ticks.length = unit(5, "points"),
          axis.text.x       = element_text(margin = margin(t = 5, unit = "points"), angle = 90, hjust = 1, vjust = 0.5),
          axis.text.y       = element_text(margin = margin(r = 5, unit = "points")))

  #ggsave_fitmax("/Users/kevinwolz/Desktop/water.png", plot.obj, scale = 2)
  #ggsave_fitmax("/Users/kevinwolz/Desktop/nitrogen.png", plot.obj, scale = 2)
  out.data <- plot.data %>%
    dplyr::mutate(cycle = cycle) %>%
    dplyr::mutate_if(is.numeric, abs)

  if(plot) return(plot.obj) else return(out.data)
}


#' Plot daily timeseries of major cycles
#' @description Plots a daily timeseries of carbon pools, water uptake, nitrogen uptake, or light capture
#' @return If \code{plot = TRUE}, returns a ggplot object. If \code{plot = FALSE}, returns the data that would create the plot.
#' If \code{hop} contains more than one simulation, the plot will be faceted by SimulationName.
#' @param hop An object of class hop or face.
#' @param cycle One of "carbon", "nitrogen", "water", or "light".
#' @param years A numeric vector of the calendar years to include.
#' If more than one year is provided, years are used as facets, and only a single value can be supplied to \code{simu.names}.
#' Use "all" to include all available values.
#' @param simu.names A character vector of the SimulationNames within \code{hop} to include. Use "all" to include all available values.
#' If more than one value is supplied to \code{years}, then a single value must be suppied to \code{simu.names}.
#' @param tree.ids A numeric vector indicating a subset of tree ids to plot. Use "all" to include all available values.
#' This only applies when \code{cycle} is "carbon".
#' @param doy.lim A numeric vector of length two providing the \code{c(minimum, maximum)} of julian days to plot.
#' @param color.palette A character stirng of hex values or R standard color names defining the color palette to use in plots with multiple simulations.
#' If \code{NULL}, the default, then the default color palette is a color-blind-friendly color palette.
#' @param plot If \code{TRUE}, the default, a ggplot object is returned. If \code{FALSE}, the data that would create the plot is returned.
#' @export
#' @importFrom dplyr %>%
#' @import ggplot2
#' @family hisafe analysis functions
#' @examples
#' \dontrun{
#' # To plot the simulated light capture in the year 2000:
#' light.capture.plot <- plot_hisafe_use(myhop, "light", 2000)
#'
#' # Once you have the plot object, you can display it and save it:
#' water.plot
#' ggsave_fitmax("light_capture.png", light.capture.plot)
#' }
plot_hisafe_cycle_daily <- function(hop,
                                    cycle,
                                    years,
                                    simu.names    = "all",
                                    tree.ids      = "all",
                                    doy.lim       = c(1, 366),
                                    color.palette = NULL,
                                    plot          = TRUE) {

  is_hop(hop, error = TRUE)
  profile_check(hop, "plot", error = TRUE)
  if(!(cycle %in% c("carbon", "nitrogen", "water", "light")))   stop("cycle argument must be one of: carbon, nitrogen, water, light", call. = FALSE)
  if(!(all(is.numeric(years))        | years[1]      == "all")) stop("years argument must be 'all' or a numeric vector",              call. = FALSE)
  if(!(length(doy.lim) == 2 & all(doy.lim %in% 1:366)))         stop("doy.lim argument must be of length 2 with values in 1:366",     call. = FALSE)
  is_TF(plot)

  if(simu.names[1] == "all") simu.names <- unique(hop$exp.plan$SimulationName)
  if(years[1]      == "all") years      <- unique(hop$plot$Year[which(hop$plot$SimulationName %in% simu.names)])
  if(length(years) > 1 & length(simu.names) > 1) stop("cannot supply multiple simu.names and multiple years", call. = FALSE)

  if(!all(years %in% hop$plot$Year))                stop("one or more values in years is not present in the plot profile of hop",      call. = FALSE)

  hop <- hop_filter(hop = hop, simu.names = simu.names, tree.ids = tree.ids)

  METHOD <- ifelse(profile_check(hop, "cells"), "cells", "plot")

  if(cycle == "water") {
    plot.data <- get_water_fluxes(hop = hop, profile = METHOD) %>%
      dplyr::filter(flux %in% c("Uptake - Trees", "Uptake - Inter crop", "Uptake - Main crop")) %>%
      dplyr::mutate(flux = droplevels(flux))
    levels(plot.data$flux) <- c("Trees", "Inter crop", "Main crop")
    if(is.null(color.palette)) color.palette <- c("#E69F00", "#56B4E9", "#009E73")
    cycle.geom  <- geom_area(aes(fill = flux), na.rm = TRUE)
    cycle.scale <- scale_fill_manual(values = color.palette)
    pre.title   <- "Water Uptake"
    y.lab       <- "Water uptake (mm)"

  } else if(cycle == "nitrogen") {
    plot.data <- get_nitrogen_fluxes(hop = hop, profile = METHOD) %>%
      dplyr::filter(flux %in% c("Uptake - Trees", "Uptake - Inter crop", "Uptake - Main crop")) %>%
      dplyr::mutate(flux = droplevels(flux))
    levels(plot.data$flux) <- c("Trees", "Inter crop", "Main crop")
    if(is.null(color.palette)) color.palette <- c("#E69F00", "#56B4E9", "#009E73")
    cycle.geom  <- geom_area(aes(fill = flux), na.rm = TRUE)
    cycle.scale <- scale_fill_manual(values = color.palette)
    pre.title   <- "Nitrogen Uptake"
    y.lab       <- "Nitrogen uptake (kg N ha-1)"

  } else if(cycle == "light") {
    plot.data   <- get_light_fluxes(hop = hop)
    if(is.null(color.palette)) color.palette <- c("#E69F00", "grey20", "#56B4E9", "#009E73")
    cycle.geom  <- geom_area(aes(fill = flux), na.rm = TRUE)
    cycle.scale <- scale_fill_manual(values = color.palette)
    pre.title   <- "Light Capture"
    y.lab       <- "Intercepted PAR (% of incident PAR)"

  } else if(cycle == "carbon") {
    if(!profile_check(hop, "trees")) return(NULL)
    plot.data   <- get_carbon_pools(hop = hop)
    if(is.null(color.palette)) color.palette <- c("#009E73", "#999999", "#D55E00", "#E69F00", "#56B4E9", "#0072B2", "#F0E442")
    cycle.geom  <- geom_area(aes(fill = flux), na.rm = TRUE)
    cycle.scale <- scale_fill_manual(values = color.palette)
    pre.title   <- "Tree Carbon Pools"
    y.lab       <- "Tree C storage (Mg C ha-1)"
  }else {
    stop("cycle argument not supported. Use one of: carbon, nitrogen, water, light.", call. = FALSE)
  }

  ## Filter plot data & add (fake) date
  plot.data <- plot.data %>%
    dplyr::filter(Year %in% years) %>%
    dplyr::filter(JulianDay >= doy.lim[1], JulianDay <= doy.lim[2]) %>%
    dplyr::mutate(date = lubridate::as_date(lubridate::parse_date_time(paste0("8000-", JulianDay), "%Y-%j")))

  ## Remove years with just one day
  yrs.to.remove <- unique(plot.data$Year)[table(plot.data$Year) == length(unique(plot.data$flux))]
  plot.data <- dplyr::filter(plot.data, !(Year %in% yrs.to.remove))

  ## Set faceting
  if(length(years) == 1) {
    plot.title <- paste(pre.title, "-", years)
    scale.x <- scale_x_date(expand = c(0,0), date_labels = "%b")
    if("face" %in% class(hop)) {
      AF.sims <- sort(hop$exp.plan$SimulationName[!(hop$exp.plan$SimulationName %in% c("Monocrop", "Forestry"))])
      if(length(AF.sims) == 1) facet <- facet_wrap(~SimulationName, nrow = 1) else facet <- facet_wrap(~SimulationName)
      all.sims <- c("Monocrop", AF.sims, "Forestry")
      all.sims <- all.sims[all.sims %in% simu.names]
      plot.data$SimulationName <- factor(plot.data$SimulationName, levels = all.sims)
    } else if ("hop-group" %in% class(hop)) {
      facet <- facet_wrap(~SimulationName)
    } else {
      facet <- geom_blank()
    }
  } else {
    plot.title <- paste(pre.title, "-", simu.names)
    scale.x    <- scale_x_date(expand = c(0,0), date_labels = "%b", limits = lubridate::as_date(lubridate::parse_date_time(paste0("8000-", doy.lim), "%Y-%j")))
    facet      <- facet_wrap(~Year)

  }

  ## Create plot
  plot.obj <- ggplot(plot.data, aes(x = date, y = value)) +
    labs(x     = "Date",
         y     = y.lab,
         title = plot.title) +
    scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL), expand = c(0,0)) +
    scale.x +
    facet +
    cycle.geom +
    cycle.scale +
    theme_hisafe_ts() +
    theme(legend.title      = element_blank(),
          axis.ticks.length = unit(5, "points"),
          axis.text.x       = element_text(margin = margin(t = 5, unit = "points"), angle = 90, hjust = 1, vjust = 0.5),
          axis.text.y       = element_text(margin = margin(r = 5, unit = "points")))

  out.data <- plot.data %>%
    dplyr::mutate(cycle = cycle) %>%
    dplyr::mutate_if(is.numeric, abs)

  #ggsave_fitmax("/Users/kevinwolz/Desktop/nitrogen_daily.png", plot.obj, scale = 2)
  if(plot) return(plot.obj) else return(dplyr::select(out.data, -date))
}

#' #' Get water fluxes from a hop object
#' @description Get water fluxes from a hop object.
#' Used within hisafe cycle functions.
#' @return A tibble with extracted and calculated water fluxes.
#' @param hop An object of class hop or face.
#' @param profile An character string indicating from which profile to pull flux data. Either "cells" or "plot".
#' @importFrom dplyr %>%
#' @keywords internal
get_water_fluxes <- function(hop, profile) {
  profile_check(hop, c(profile, "plot"), error = TRUE)
  variable_check(hop, "plot", "precipitation", error = TRUE)

  if(profile == "cells") {
    variable_check(hop, "cells",
                   c("cropType", "rainInterceptedByTrees", "rainInterceptedByCrop",
                     "runOff", "soilEvaporation", "drainageBottom", "drainageArtificial", "drainageWaterTable", "waterUptakeByTrees",
                     "waterUptake", "irrigation", "waterAddedByWaterTable",
                     "waterUptakeInSaturationByTrees", "waterUptakeInSaturationByCrop", "capillaryRise"),
                   error = TRUE)

    plot.data <- hop$plot %>%
      dplyr::select(SimulationName, Date, precipitation)

    out <- hop$cells %>%
      dplyr::left_join(plot.data, by = c("SimulationName", "Date")) %>%
      replace(is.na(.), 0) %>%
      dplyr::mutate(interception  = rainInterceptedByTrees + rainInterceptedByCrop,
                    runOff        = runOff,
                    evaporation   = soilEvaporation,
                    drainage      = drainageBottom + drainageArtificial + drainageWaterTable,
                    uptakeTree    = waterUptakeByTrees,
                    uptakeMain    = waterUptake * as.numeric(cropType == "mainCrop"),
                    uptakeInter   = waterUptake * as.numeric(cropType == "interCrop"),
                    irrigation    = -irrigation,
                    waterTable    = -waterAddedByWaterTable + -waterUptakeInSaturationByTrees + -waterUptakeInSaturationByCrop + -capillaryRise,
                    precipitation = -precipitation) %>%
      dplyr::select(SimulationName, Year, Month, Day, Date, JulianDay,
                    interception, runOff, evaporation, drainage, uptakeTree, uptakeMain, uptakeInter, irrigation, waterTable, precipitation) %>%
      dplyr::group_by(SimulationName, Year, Month, Day, Date, JulianDay) %>%
      dplyr::summarize_all(mean) %>% # mean of all cells in scene
      dplyr::ungroup()

  } else {
    stop("Plotting water cycle using plot profile is currently not supported. Export cells profile.", call. = FALSE)
  }

  out <- out %>%
    tidyr::gather(key = "flux", value = "value", interception:precipitation) %>%
    dplyr::mutate(flux = factor(flux,
                                levels = c("uptakeTree",
                                           "uptakeInter",
                                           "uptakeMain",
                                           "interception",
                                           "runOff",
                                           "evaporation",
                                           "drainage",
                                           "irrigation",
                                           "waterTable",
                                           "precipitation"),
                                labels = c("Uptake - Trees",
                                           "Uptake - Inter crop",
                                           "Uptake - Main crop",
                                           "Interception",
                                           "Run-off",
                                           "Soil evaporation",
                                           "Drainage",
                                           "Irrigation",
                                           "Water table",
                                           "Precipitation")))
  return(out)
}

#' Get nitrogen fluxes in and out of soil from a hop object
#' @description Get nitrogen fluxes in and our of soil from a hop object.
#' Used within hisafe cycle functions.
#' @return A tibble with extracted and calculated nitrogen fluxes.
#' @param hop An object of class hop or face.
#' @param profile An character string indicating from which profile to pull flux data. Either "cells" or "plot".
#' @importFrom dplyr %>%
#' @keywords internal
get_nitrogen_fluxes <- function(hop, profile) {
  profile_check(hop, profile, error = TRUE)

  if(profile == "cells") {
    variable_check(hop, "cells",
                   c("cropType", "nitrogenFertilisationMineral", "nitrogenFertilisationMineral", "nitrogenIrrigation", "nitrogenRain", "nitrogenFixation",
                     "nitrogenUptakeByTrees", "nitrogenUptake", "nitrogenVolatilisation", "nitrogenVolatilisationOrganic",
                     "nitrogenDenitrification", "nitrogenLeachingBottom", "nitrogenLeachingArtificial", "nitrogenLeachingWaterTable",
                     "treeNitrogenLeafLitter", "treeNitrogenFineRootLitter", "treeNitrogenCoarseRootLitter",
                     "treeNitrogenFineRootDeepLitter", "treeNitrogenCoarseRootDeepLitter",
                     "cropNitrogenLeafLitter", "cropNitrogenRootLitter"),
                   error = TRUE)

    out <- hop$cells %>%
      replace(is.na(.), 0) %>%
      dplyr::mutate(fertilization  = -nitrogenFertilisationMineral + -nitrogenFertilisationMineral,
                    irrigation     = -nitrogenIrrigation,
                    deposition     = -nitrogenRain,
                    fixation       = -nitrogenFixation,
                    watertable     = -nitrogenAddedByWaterTable,
                    upatakeTree    = nitrogenUptakeByTrees,
                    uptakeMain     = nitrogenUptake * as.numeric(cropType == "mainCrop"),
                    uptakeInter    = nitrogenUptake * as.numeric(cropType == "interCrop"),
                    gaseous        = nitrogenVolatilisation + nitrogenVolatilisationOrganic + nitrogenDenitrification,
                    leaching       = nitrogenLeachingBottom + nitrogenLeachingArtificial + nitrogenLeachingWaterTable,
                    runoff         = nitrogenRunOff,
                    litter         = -treeNitrogenLeafLitter + -treeNitrogenFineRootLitter + -treeNitrogenCoarseRootLitter +
                                     -treeNitrogenFineRootDeepLitter + -treeNitrogenCoarseRootDeepLitter +
                                     -cropNitrogenLeafLitter + -cropNitrogenRootLitter) %>%
      dplyr::select(SimulationName, Year, Month, Day, Date, JulianDay,
                    fertilization, irrigation, deposition, fixation, watertable, upatakeTree, uptakeMain, uptakeInter, gaseous, leaching, runoff, litter) %>%
      dplyr::group_by(SimulationName, Year, Month, Day, Date, JulianDay) %>%
      dplyr::summarize_all(mean) %>% # mean of all cells in scene
      dplyr::ungroup()

  } else {
    stop("Plotting nitrogen cycle using plot profile is currently not supported. Export cells profile.", call. = FALSE)
  }

  out <- out %>%
    tidyr::gather(key = "flux", value = "value", fertilization:litter) %>%
    dplyr::mutate(flux = factor(flux,
                                levels = c("upatakeTree",
                                           "uptakeInter",
                                           "uptakeMain",
                                           "gaseous",
                                           "runoff",
                                           "leaching",
                                           "fertilization",
                                           "irrigation",
                                           "deposition",
                                           "fixation",
                                           "litter",
                                           "watertable"),
                                labels = c("Uptake - Trees",
                                           "Uptake - Inter crop",
                                           "Uptake - Main crop",
                                           "Gaseous losses",
                                           "Run-off",
                                           "Leaching",
                                           "Fertilization",
                                           "Irrigation",
                                           "Deposition",
                                           "Fixation",
                                           "Litter",
                                           "Water table")))
  return(out)
}

#' Get light fluxes from a hop object
#' @description Get light fluxes from a hop object.
#' Used within hisafe cycle functions.
#' @return A tibble with extracted and calculated light fluxes.
#' @param hop An object of class hop or face.
#' @importFrom dplyr %>%
#' @keywords internal
get_light_fluxes <- function(hop) {
  profile_check(hop, "plot", error = TRUE)
  variable_check(hop, "plot",
                 c("mainCropArea", "interCropArea", "parIncident", "parInterceptedByMainCrop", "parInterceptedByInterCrop", "parInterceptedByTrees"),
                 error = TRUE)
  out <- hop$plot %>%
    replace(is.na(.), 0) %>%
    # Scale by area to conver to moles and then make percentage of inceident par
    dplyr::mutate(parIncident            = parIncident               * (mainCropArea + interCropArea),
                  InterceptedByMainCrop  = parInterceptedByMainCrop  * mainCropArea                   / parIncident * 100,
                  InterceptedByInterCrop = parInterceptedByInterCrop * interCropArea                  / parIncident * 100,
                  InterceptedByTrees     = parInterceptedByTrees     * (mainCropArea + interCropArea) / parIncident * 100,
                  notCaptured            = 100 - InterceptedByMainCrop - InterceptedByInterCrop - InterceptedByTrees) %>%
    dplyr::select(SimulationName, Year, Month, Day, Date, JulianDay,
                  InterceptedByTrees, notCaptured, InterceptedByInterCrop, InterceptedByMainCrop) %>%
    tidyr::gather(key = "flux", value = "value", InterceptedByTrees:InterceptedByMainCrop) %>%
    dplyr::mutate(flux = factor(flux,
                                levels = c("InterceptedByTrees",
                                           "notCaptured",
                                           "InterceptedByInterCrop",
                                           "InterceptedByMainCrop"),
                                labels = c("Trees",
                                           "Not captured",
                                           "Secondary crop",
                                           "Main crop")))
  return(out)
}

#' Get carbon pools from a hop object
#' @description Get carbon pools from a hop object.
#' Used within hisafe cycle functions.
#' @return A tibble with extracted and calculated carbon pools.
#' @param hop An object of class hop or face.
#' @importFrom dplyr %>%
#' @keywords internal
get_carbon_pools <- function(hop) {
  profile_check(hop, "trees", error = TRUE)
  variable_check(hop, "trees",
                 c("carbonFoliage", "carbonBranches", "carbonCoarseRoots", "carbonFineRoots", "carbonLabile", "carbonStem", "carbonStump"),
                 error = TRUE)
  out <- hop$trees %>%
    replace(is.na(.), 0) %>%
    dplyr::select(SimulationName, Year, Month, Day, Date, JulianDay, id,
                  carbonFoliage, carbonBranches, carbonCoarseRoots, carbonFineRoots, carbonLabile, carbonStem, carbonStump) %>%
    dplyr::select(-id) %>%
    dplyr::group_by(SimulationName, Year, Month, Day, Date, JulianDay) %>%
    dplyr::summarize_all(sum) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(carbonStump       = -carbonStump) %>%
    dplyr::mutate(carbonCoarseRoots = -carbonCoarseRoots) %>%
    dplyr::mutate(carbonFineRoots   = -carbonFineRoots) %>%
    tidyr::gather(key = "flux", value = "value", carbonFoliage:carbonStump) %>%
    dplyr::mutate(flux = factor(flux,
                                levels = c("carbonFoliage",
                                           "carbonLabile",
                                           "carbonBranches",
                                           "carbonStem",
                                           "carbonFineRoots",
                                           "carbonCoarseRoots",
                                           "carbonStump"),
                                labels = c("Leaves",
                                           "Labile",
                                           "Branches",
                                           "Stem",
                                           "Fine Roots",
                                           "Coarse Roots",
                                           "Stump"))) %>%
    dplyr::mutate(value = value / 1e3) %>% # convert from kg C tree-1 to Mg C tree-1
    dplyr::left_join(dplyr::select(hop$plot.info, SimulationName, plotAreaHa), by = "SimulationName") %>%
    dplyr::mutate(value = value / plotAreaHa) %>% # convert from Mg C tree-1 to Mg C ha-1
    dplyr::select(-plotAreaHa)
  return(out)
}
