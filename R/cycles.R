#' Plot annual barchart of major cycles
#' @description Plots an annual barchart of tree carbon pools, water fluxes, nitrogen fluxes, or light capture.
#' @return If \code{plot = TRUE}, returns a ggplot object. If \code{plot = FALSE}, returns the data that would create the plot.
#' If \code{hop} contains more than one simulation, the plot will be faceted by SimulationName.
#' @param hop An object of class hop or face.
#' @param cycle One of "carbon", "nitrogen", "water", "light", or "yield".
#' @param simu.names A character vector of the SimulationNames within \code{hop} to include. Use "all" to include all available values.
#' @param tree.ids A numeric vector indicating a subset of tree ids to plot. Use "all" to include all available values.
#' This only applies when \code{cycle} is "carbon".
#' @param year.lim A numeric vector of length two providing the \code{c(minimum, maximum)} of calendar years to plot.
#' If no input, the full available time range is plotted. Use \code{NA} to refer to the start or end of the simulation.
#' @param color.palette A character string of hex values or R standard color names defining the color palette to use in plots with multiple simulations.
#' If \code{NULL}, the default, then the default color palette is a color-blind-friendly color palette.
#' @param bar.color A hex value or R standard color name defining the color to use for bar plot borders
#' @param crop.names A character vector of length 2 containing the names to use in the legend for the mainCrop and interCrop of Hi-sAFe, in that order.
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
#'  \item{"Soil evaporation"}{ - water evaporated from (1) soil and (2) intercepted rain held in litter/mulch on soil surface}
#'  \item{"Drainage"}{ - water drainage (1) out of the bottom of the scene and (2) into artificial drainage pipes}
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
                                     bar.color     = "black",
                                     crop.names    = c("Main crop", "Inter crop"),
                                     plot          = TRUE) {

  is_hop(hop, error = TRUE)
  if(simu.names[1] == "all") simu.names <- unique(hop$exp.plan$SimulationName)

  if(!(cycle %in% c("carbon", "nitrogen", "water", "light", "yield")))           stop("cycle argument must be one of: carbon, nitrogen, water, light", call. = FALSE)
  if(!(length(year.lim)   == 2 & (is.numeric(year.lim) | all(is.na(year.lim))))) stop("year.lim argument must be a numeric vector of length 2",        call. = FALSE)
  if(!(length(bar.color)  == 1 & is.character(bar.color)))                       stop("bar.plot argument must be a character vector of length 1",      call. = FALSE)
  if(!(length(crop.names) == 2 & is.character(crop.names)))                      stop("crop.names argument must be a character vector of length 2",    call. = FALSE)
  is_TF(plot)

  hop <- hop_filter(hop = hop, simu.names = simu.names, tree.ids = tree.ids)

  METHOD <- ifelse(profile_check(hop, "cells"), "cells", "plot")

  if(cycle == "yield") {
    plot.data <- get_yields(hop = hop, profile = METHOD, crop.names = crop.names)
    geom       <- geom_bar(stat = "identity", color = bar.color)
    plot.title <- "Yield"
    y.lab      <- "Yield (kg ha-1)"
    if(is.null(color.palette)) color.palette <- c("#E69F00", "#009E73")

  } else if(cycle == "water") {
    plot.data <- get_water_fluxes(hop = hop, profile = METHOD, crop.names = crop.names)
    geom       <- geom_bar(stat = "identity", color = bar.color)
    plot.title <- "Water Cycle"
    y.lab      <- "Water flux (mm)"
    if(is.null(color.palette)) color.palette <- c("#D55E00", "#E69F00", "#F0E442", "grey20", "grey40", "grey60", "grey80",
                                                  "#009E73", "#0072B2", "#56B4E9")

  } else if(cycle == "nitrogen") {
    plot.data <- get_nitrogen_fluxes(hop = hop, profile = METHOD, crop.names = crop.names)
    geom       <- geom_bar(stat = "identity", color = bar.color)
    plot.title <- "Nitrogen Cycle"
    y.lab      <- "N flux (kg N ha-1)"
    if(is.null(color.palette)) color.palette <- c("#D55E00", "#E69F00", "#F0E442", "grey20", "grey40", "grey60",
                                                  "#009E73", "#0072B2", "#56B4E9", "#CC79A7", "black", "white")

  } else if(cycle == "light") {
    plot.data  <- get_light_fluxes(hop = hop, crop.names = crop.names)
    geom       <- list(geom_bar(stat = "identity", aes(color = flux)),
                       scale_color_manual(values = c("black", "white", "black", "black")),
                       guides(fill = guide_legend(override.aes = list(color = "black"))))
    plot.title <- "Light Capture"
    y.lab      <- "Intercepted PAR (%)"
    if(is.null(color.palette)) color.palette <- c("#E69F00", "white", "#56B4E9", "#009E73")

  } else if(cycle == "carbon") {
    if(!profile_check(hop, "trees")) return(NULL)
    plot.data  <- get_carbon_pools(hop = hop)
    geom       <- geom_bar(stat = "identity", color = bar.color)
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
    scale.x    <- scale_x_continuous(expand = c(0,0))
  }

  ## Create plot
  plot.obj <- ggplot(plot.data, aes(x = Year, y = value, fill = flux)) +
    labs(x     = "Year",
         y     = y.lab,
         title = plot.title) +
    facet_simu +
    scale.x +
    scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL), expand = c(0,0)) +
    geom +
    scale_fill_manual(values = color.palette) +
    theme_hisafe_ts() +
    theme(legend.title = element_blank(),
          axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.5))

  out.data <- plot.data %>%
    dplyr::mutate(cycle = cycle)

  if(plot) return(plot.obj) else return(out.data)
}

#' Plot daily timeseries of major cycles
#' @description Plots a daily timeseries of tree carbon pools, water uptake, nitrogen uptake, light capture, or tree carbon incrememnt.
#' @return If \code{plot = TRUE}, returns a ggplot object. If \code{plot = FALSE}, returns the data that would create the plot.
#' If \code{hop} contains more than one simulation, the plot will be faceted by SimulationName.
#' @param hop An object of class hop or face.
#' @param cycle One of "carbon", "nitrogen", "water", "light", "yield", or "carbon-increment.
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
#' @param crop.names A character vector of length 2 containing the names to use in the legend for the mainCrop and interCrop of Hi-sAFe, in that order.
#' @param pheno.lines Logical indicating whether or not vertical dashed lines should be plotted on dates of phenoloigical stage changes
#' @param trim Logical indicating whether or not to trim white space before and after the tree growth season when \code{cycle} is "carbon-increment"
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
                                    crop.names    = c("Main crop", "Inter crop"),
                                    pheno.lines   = TRUE,
                                    trim          = TRUE,
                                    plot          = TRUE) {

  allowed.cycles <- c("carbon", "nitrogen", "water", "light", "yield", "carbon-increment")

  is_hop(hop, error = TRUE)
  if(!(cycle %in% allowed.cycles))                          stop(paste0("cycle argument must be one of: ", paste(allowed.cycles, collapse = ", ")), call. = FALSE)
  if(!(all(is.numeric(years)) | years[1] == "all"))         stop("years argument must be 'all' or a numeric vector",           call. = FALSE)
  if(!(length(doy.lim)    == 2 & all(doy.lim %in% 1:366)))  stop("doy.lim argument must be of length 2 with values in 1:366",  call. = FALSE)
  if(!(length(crop.names) == 2 & is.character(crop.names))) stop("crop.names argument must be a character vector of length 2", call. = FALSE)
  is_TF(pheno.lines)
  is_TF(trim)
  is_TF(plot)

  METHOD <- ifelse(profile_check(hop, "cells"), "cells", "plot")
  years.profile <- ifelse(cycle %in% c("carbon", "carbon-increment"), "trees", METHOD)

  profile_check(hop, years.profile, error = TRUE)
  if(simu.names[1] == "all") simu.names <- unique(hop$exp.plan$SimulationName)
  if(years[1]      == "all") years      <- unique(hop[[years.profile]]$Year[which(hop[[years.profile]]$SimulationName %in% simu.names)])
  if(!all(years %in% unique(hop[[years.profile]]$Year))) stop("one or more values in years is not present in hop",    call. = FALSE)
  if(length(years) > 1 & length(simu.names) > 1)         stop("cannot supply multiple simu.names and multiple years", call. = FALSE)

  hop <- hop_filter(hop = hop, simu.names = simu.names, tree.ids = tree.ids)

  if(cycle == "yield") {
    plot.data <- get_yields(hop = hop, profile = METHOD, crop.names = crop.names)
    if(is.null(color.palette)) color.palette <- c("#E69F00", "#009E73")
    cycle.geom  <- geom_area(aes(fill = flux), na.rm = TRUE)
    cycle.scale <- scale_color_manual(values = color.palette)
    pre.title   <- "Yield"
    y.lab       <- "Yield (kg ha-1)"

  } else if(cycle == "water") {
    plot.data <- get_water_fluxes(hop = hop, profile = METHOD, crop.names = crop.names) %>%
      dplyr::filter(flux %in% c("Uptake - Trees",
                                paste("Uptake -", crop.names[2]),
                                paste("Uptake -", crop.names[1]))) %>%
      dplyr::mutate(flux = droplevels(flux))
    levels(plot.data$flux) <- c("Trees", "Inter crop", "Main crop")
    if(is.null(color.palette)) color.palette <- c("#E69F00", "#56B4E9", "#009E73")
    cycle.geom  <- geom_area(aes(fill = flux), na.rm = TRUE)
    cycle.scale <- scale_fill_manual(values = color.palette)
    pre.title   <- "Water Uptake"
    y.lab       <- "Water uptake (mm)"

  } else if(cycle == "nitrogen") {
    plot.data <- get_nitrogen_fluxes(hop = hop, profile = METHOD, crop.names = crop.names) %>%
      dplyr::filter(flux %in% c("Uptake - Trees",
                                paste("Uptake -", crop.names[2]),
                                paste("Uptake -", crop.names[1]))) %>%
      dplyr::mutate(flux = droplevels(flux))
    levels(plot.data$flux) <- c("Trees", "Inter crop", "Main crop")
    if(is.null(color.palette)) color.palette <- c("#E69F00", "#56B4E9", "#009E73")
    cycle.geom  <- geom_area(aes(fill = flux), na.rm = TRUE)
    cycle.scale <- scale_fill_manual(values = color.palette)
    pre.title   <- "Nitrogen Uptake"
    y.lab       <- "Nitrogen uptake (kg N ha-1)"

  } else if(cycle == "light") {
    plot.data   <- get_light_fluxes(hop = hop, crop.names = crop.names)
    if(is.null(color.palette)) color.palette <- c("#E69F00", "white", "#56B4E9", "#009E73")
    cycle.geom  <- geom_area(aes(fill = flux), na.rm = TRUE)
    cycle.scale <- scale_fill_manual(values = color.palette)
    pre.title   <- "Light Capture"
    y.lab       <- "Intercepted PAR (%)"

  } else if(cycle == "carbon") {
    if(!profile_check(hop, "trees")) return(NULL)
    plot.data   <- get_carbon_pools(hop = hop)
    if(is.null(color.palette)) color.palette <- c("#009E73", "#999999", "#D55E00", "#E69F00", "#56B4E9", "#0072B2", "#F0E442")
    cycle.geom  <- geom_area(aes(fill = flux), na.rm = TRUE)
    cycle.scale <- scale_fill_manual(values = color.palette)
    pre.title   <- "Tree Carbon Pools"
    y.lab       <- "Tree C storage (Mg C ha-1)"

  } else if(cycle == "carbon-increment") {
    if(!profile_check(hop, "trees")) return(NULL)
    plot.data   <- get_carbon_increment(hop = hop)
    if(is.null(color.palette)) color.palette <- c("#009E73", "#D55E00", "#E69F00", "#56B4E9", "#0072B2", "#F0E442")
    cycle.geom  <- geom_area(aes(fill = flux), na.rm = TRUE)
    cycle.scale <- scale_fill_manual(values = color.palette)
    pre.title   <- "Tree Carbon Increment"
    y.lab       <- "Tree C increment (kg C tree-1)"

  } else {
    stop("cycle argument not supported. Use one of: carbon, nitrogen, water, light, carbon-increment.", call. = FALSE)
  }

  ## Filter plot data & add (fake) date
  plot.data <- plot.data %>%
    dplyr::filter(Year %in% years) %>%
    dplyr::filter(JulianDay >= doy.lim[1], JulianDay <= doy.lim[2]) %>%
    dplyr::mutate(date = lubridate::as_date(lubridate::parse_date_time(paste0("8000-", JulianDay), "%Y-%j")))

  ## Remove white space when cycle = "carbon-increment"
  if(trim & cycle == "carbon-increment") {
    date.range <- plot.data %>%
      dplyr::filter(value != 0) %>%
      .$Date %>%
      range()

    plot.data <- plot.data %>%
      dplyr::filter(Date >= date.range[1]) %>%
      dplyr::filter(Date <= date.range[2])
  }

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

  if(pheno.lines & profile_check(hop, "trees")) {
    pheno.data <- get_pheno_dates(hop) %>%
      dplyr::filter(Year %in% years) %>%
      dplyr::mutate(date = lubridate::as_date(lubridate::parse_date_time(paste0("8000-", JulianDay), "%Y-%j")))
    vert.lines <- geom_vline(data = pheno.data, aes(xintercept = date), linetype = "dashed")
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
    guides(fill = guide_legend(override.aes = list(color = "black"))) +
    theme_hisafe_ts() +
    theme(legend.title = element_blank(),
          axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.5))

  if(pheno.lines & profile_check(hop, "trees")) plot.obj <- plot.obj + vert.lines

  out.data <- plot.data %>%
    dplyr::mutate(cycle = cycle)

  #ggsave_fitmax("/Users/kevinwolz/Desktop/nitrogen_daily.png", plot.obj, scale = 2)
  if(plot) return(plot.obj) else return(dplyr::select(out.data, -date))
}

#' #' Get water fluxes from a hop object
#' @description Get water fluxes from a hop object.
#' Used within hisafe cycle functions.
#' @return A tibble with extracted and calculated water fluxes.
#' @param hop An object of class hop or face.
#' @param profile An character string indicating from which profile to pull flux data. Either "cells" or "plot".
#' @param crop.names A character vector of length 2 containing the names to use in the legend for the mainCrop and interCrop of Hi-sAFe, in that order.
#' @importFrom dplyr %>%
#' @keywords internal
get_water_fluxes <- function(hop, profile, crop.names) {
  profile_check(hop, c(profile, "plot"), error = TRUE)
  variable_check(hop, "plot", "precipitation", error = TRUE)

  if(profile == "cells") {
    variable_check(hop, "cells",
                   c("cropType", "rainInterceptedByTrees", "rainInterceptedByCrop",
                     "runOff", "soilEvaporation", "mulchEvaporation", "drainageBottom", "drainageArtificial",
                     "waterUptakeByTrees", "waterUptake", "irrigation", "waterAddedByWaterTable",
                     "waterUptakeInSaturationByTrees", "waterUptakeInSaturationByCrop", "capillaryRise"),
                   error = TRUE)

    plot.data <- hop$plot %>%
      dplyr::select(SimulationName, Date, precipitation)

    out <- hop$cells %>%
      dplyr::left_join(plot.data, by = c("SimulationName", "Date")) %>%
      replace(is.na(.), 0) %>%
      dplyr::mutate(interception  = rainInterceptedByTrees + rainInterceptedByCrop,
                    runOff        = runOff,
                    evaporation   = soilEvaporation + mulchEvaporation,
                    drainage      = drainageBottom + drainageArtificial,
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
                                           paste("Uptake -", crop.names[2]),
                                           paste("Uptake -", crop.names[1]),
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
#' @param crop.names A character vector of length 2 containing the names to use in the legend for the mainCrop and interCrop of Hi-sAFe, in that order.
#' @importFrom dplyr %>%
#' @keywords internal
get_nitrogen_fluxes <- function(hop, profile, crop.names) {
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
                                           paste("Uptake -", crop.names[2]),
                                           paste("Uptake -", crop.names[1]),
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
#' @param crop.names A character vector of length 2 containing the names to use in the legend for the mainCrop and interCrop of Hi-sAFe, in that order.
#' @importFrom dplyr %>%
#' @keywords internal
get_light_fluxes <- function(hop, crop.names) {
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
                                           crop.names[2],
                                           crop.names[1])))
  return(out)
}

#' Get tree carbon pools from a hop object
#' @description Get tree carbon pools from a hop object.
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
    dplyr::select(SimulationName, Year, Month, Day, Date, JulianDay, idTree,
                  carbonFoliage, carbonBranches, carbonCoarseRoots, carbonFineRoots, carbonLabile, carbonStem, carbonStump) %>%
    dplyr::select(-idTree) %>%
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

#' Get tree carbon increment from a hop object
#' @description Get tree carbon increment from a hop object.
#' Used within hisafe cycle functions.
#' @return A tibble with extracted and calculated carbon increments.
#' @param hop An object of class hop or face.
#' @importFrom dplyr %>%
#' @keywords internal
get_carbon_increment <- function(hop) {
  profile_check(hop, "trees", error = TRUE)
  variable_check(hop, "trees",
                 c("carbonFoliageIncrement", "carbonBranchesIncrement", "carbonCoarseRootsIncrement", "carbonFineRootsIncrement", "carbonStemIncrement", "carbonStumpIncrement"),
                 error = TRUE)
  out <- hop$trees %>%
    replace(is.na(.), 0) %>%
    dplyr::select(SimulationName, Year, Month, Day, Date, JulianDay, idTree,
                  carbonFoliageIncrement, carbonBranchesIncrement, carbonCoarseRootsIncrement, carbonFineRootsIncrement, carbonStemIncrement, carbonStumpIncrement) %>%
    dplyr::select(-idTree) %>%
    dplyr::group_by(SimulationName, Year, Month, Day, Date, JulianDay) %>%
    dplyr::summarize_all(sum) %>%
    dplyr::ungroup() %>%
    tidyr::gather(key = "flux", value = "value", carbonFoliageIncrement:carbonStumpIncrement) %>%
    dplyr::mutate(flux = factor(flux,
                                levels = c("carbonFoliageIncrement",
                                           "carbonBranchesIncrement",
                                           "carbonStemIncrement",
                                           "carbonFineRootsIncrement",
                                           "carbonCoarseRootsIncrement",
                                           "carbonStumpIncrement"),
                                labels = c("Leaves",
                                           "Branches",
                                           "Stem",
                                           "Fine Roots",
                                           "Coarse Roots",
                                           "Stump")))
  return(out)
}

#' Get yields from a hop object
#' @description Gets yields from a hop object.
#' Used within hisafe cycle functions.
#' @return A tibble with extracted yields.
#' @param hop An object of class hop or face.
#' @param profile An character string indicating from which profile to pull crop yield data. Either "cells" or "plot".
#' @param crop.names A character vector of length 2 containing the names to use in the legend for the mainCrop and interCrop of Hi-sAFe, in that order.
#' @importFrom dplyr %>%
#' @keywords internal
get_yields <- function(hop, profile, crop.names) {
  profile_check(hop, c(profile, "trees", "plot.info"), error = TRUE)
  variable_check(hop, "trees", "stemYield", error = TRUE)

  if(profile == "cells") {
    variable_check(hop, "cells", c("cropType", "grainBiomass"), error = TRUE)

    mainCrop.rel.area <- hop$cells %>%
      dplyr::filter(Date == min(Date)) %>%
      dplyr::group_by(SimulationName, cropType) %>%
      dplyr::summarize(n = n()) %>%
      dplyr::mutate(perc = n / sum(n)) %>%
      dplyr::filter(cropType == "mainCrop") %>%
      dplyr::select(-cropType, -n)

    cells <- hop$cells %>%
      replace(is.na(.), 0) %>%
      dplyr::filter(cropType == "mainCrop") %>%
      dplyr::select(SimulationName, Year, Month, Day, Date, JulianDay, grainBiomass) %>%
      dplyr::group_by(SimulationName, Year, Month, Day, Date, JulianDay) %>%
      dplyr::summarize_all(mean) %>% # mean of all cells in scene
      dplyr::ungroup() %>%
      dplyr::group_by(SimulationName) %>%
      dplyr::arrange(SimulationName, Year, JulianDay) %>%
      dplyr::rename(yield = grainBiomass) %>%
      dplyr::left_join(mainCrop.rel.area, by = "SimulationName") %>%
      dplyr::mutate(yield = yield * perc) %>% # convert from cell basis to scene basis
      dplyr::mutate(yield = yield * 1000) %>% # convert t/ha to kg/ha
      dplyr::mutate(yield = c(NA, pmax(diff(yield), 0))) %>% # convert to yield increment
      dplyr::ungroup()

    out <- hop$trees %>%
      dplyr::left_join(hop$plot.info, by = "SimulationName") %>%
      replace(is.na(.), 0) %>%
      dplyr::select(SimulationName, Year, Month, Day, Date, JulianDay, stemYield, plotAreaHa) %>%
      dplyr::group_by(SimulationName, Year, Month, Day, Date, JulianDay, plotAreaHa) %>%
      dplyr::summarize_all(sum) %>% # sum of all trees in scene
      dplyr::ungroup() %>%
      dplyr::group_by(SimulationName) %>%
      dplyr::arrange(SimulationName, Year, JulianDay) %>%
      dplyr::mutate(stemYield = stemYield / plotAreaHa) %>% # convert from kg to kg/ha
      dplyr::mutate(stemYield = c(NA, pmax(diff(stemYield), 0))) %>% # convert to yield increment
      dplyr::select(-plotAreaHa) %>%
      dplyr::full_join(cells, by = c("SimulationName", "Year", "Month", "Day", "Date", "JulianDay")) %>%
      replace(is.na(.), 0)

  } else {
    stop("Plotting nitrogen cycle using plot profile is currently not supported. Export cells profile.", call. = FALSE)
  }

  out <- out %>%
    tidyr::gather(key = "flux", value = "value", stemYield, yield) %>%
    dplyr::mutate(flux = factor(flux,
                                levels = c("stemYield", "yield"),
                                labels = c("Trees", crop.names[1])))
  return(out)
}

#' Plot summary dashboard of daily and annual cycles
#' @description Plots summary dashboard of daily and annual cycles.
#' @return Invisibly retruns an egg object.
#' @param hop An object of class hop or face.
#' @param daily.years A numeric vector of legnth 1 indicating the year to use for the daily plots.
#' @param simu.name A character vector of legnth 1 indicating the SimulationName within \code{hop} to plot.
#' @param cycles A character vector of the cycles to include. Supported cycles include: 'carbon', 'light', 'water', and 'nitrogen.
#' See \code{\link{plot_hisafe_cycle_annual}} for details of each cycle.
#' @param crop.names A character vector of length 2 containing the names to use in the legend for the mainCrop and interCrop of Hi-sAFe, in that order.
#' @param output.path A character string indicating the path to the directory where plots should be saved.
#' If \code{NULL}, the experiment/simulation path is read from the hop object, and a directory is created there called "analysis".
#' The plot wil be saved in this directory as "cycles_summary_SimulationName.jpg".
#' @param plot.labels A character vector of labels to label upper-right corner of plot panels. If \code{NULL}, no labels will be added.
#' @export
#' @import ggplot2
#' @family hisafe analysis functions
#' @examples
#' \dontrun{
#' cycle.plot <- cycle_summary(myhop, 2000)
#' }
cycle_summary <- function(hop,
                          daily.year,
                          simu.name   = NULL,
                          cycles      = c("carbon", "light", "water", "nitrogen"),
                          crop.names  = c("Main crop", "Inter crop"),
                          output.path = NULL,
                          plot.labels = NULL) {

  if(!(length(daily.year) == 1 & is.numeric(daily.year))) stop("daily.year argument must be a numeric vector of length 1", call. = FALSE)
  if(!(is.character(output.path) | is.null(output.path))) stop("output.path argument must be a character string",          call. = FALSE)
  if(!(is.character(plot.labels) | is.null(plot.labels))) stop("plot.labels argument must be a character string",          call. = FALSE)

  if(is.null(simu.name)) {
    simu.name <- hop$metadata$SimulationName
    if(length(simu.name) > 1) stop("simu.name must be specfied if hop contains more than one simulation.", call. = FALSE)
  }

  supported.cycles <- c("carbon", "light", "water", "nitrogen")

  ## CARBON
  carbon.daily <- plot_hisafe_cycle_daily(hop           = hop,
                                          simu.names    = simu.name,
                                          cycle         = "carbon",
                                          years         = daily.year,
                                          pheno.lines   = FALSE) +
    guides(fill = FALSE)

  carbon.annual <- plot_hisafe_cycle_annual(hop        = hop,
                                            simu.names = simu.name,
                                            cycle      = "carbon")

  ## LIGHT
  light.daily <- plot_hisafe_cycle_daily(hop           = hop,
                                         simu.names    = simu.name,
                                         cycle         = "light",
                                         years         = daily.year,
                                         crop.names    = crop.names,
                                         pheno.lines   = FALSE) +
    guides(fill = FALSE)

  light.annual <- plot_hisafe_cycle_annual(hop           = hop,
                                           simu.names    = simu.name,
                                           cycle         = "light",
                                           crop.names    = crop.names,
                                           bar.color     = "transparent")

  ## WATER
  water.daily <- plot_hisafe_cycle_daily(hop           = hop,
                                         simu.names    = simu.name,
                                         cycle         = "water",
                                         years         = daily.year,
                                         crop.names    = crop.names,
                                         pheno.lines   = FALSE) +
    guides(fill = FALSE)

  water.annual <- plot_hisafe_cycle_annual(hop        = hop,
                                           simu.names = simu.name,
                                           cycle      = "water",
                                           crop.names = crop.names)

  ## NITROGEN
  nitrogen.daily <- plot_hisafe_cycle_daily(hop         = hop,
                                            simu.names  = simu.name,
                                            cycle       = "nitrogen",
                                            years       = daily.year,
                                            crop.names  = crop.names,
                                            pheno.lines = FALSE) +
    guides(fill = FALSE)

  nitrogen.annual <- plot_hisafe_cycle_annual(hop        = hop,
                                              simu.names = simu.name,
                                              cycle      = "nitrogen",
                                              crop.names = crop.names)

  ## MERGE & ADD LEGEND
  plot.list <- list(carbon.daily,   carbon.annual,
                    light.daily,    light.annual,
                    water.daily,    water.annual,
                    nitrogen.daily, nitrogen.annual)[rep(supported.cycles %in% cycles, each = 2)]

  n.plots <- length(plot.list)
  for(i in 1:n.plots) {
    if(i %in% c(n.plots-1, n.plots)) {
      plot.list[[i]] <- plot.list[[i]] +
        labs(title = NULL) +
        theme(axis.text.x      = element_text(angle = 0, hjust = 0.5),
              panel.grid.major = element_blank(),
              legend.box.just  = "left")
    } else {
      plot.list[[i]] <- plot.list[[i]] +
        labs(title = NULL) +
        theme(axis.text.x      = element_blank(),
              axis.title.x     = element_blank(),
              panel.grid.major = element_blank())
    }
  }

  ## ADD LABELS
  if(!is.null(plot.labels)) plot.list <- annotator(plot.list, labels = plot.labels)

  ## PLOT
  cycle.plot <- egg::ggarrange(plots = plot.list, ncol = 2, draw = FALSE)

  output.path <- clean_path(paste0(diag_output_path(hop = hop, output.path = output.path), "/"))
  dir.create(output.path, recursive = TRUE, showWarnings = FALSE)

  ggsave(paste0(output.path, "cycle_summary_", simu.name, ".jpg"), cycle.plot, scale = 2.2, width = 6, height = 1.5 * n.plots / 2)

  invisible(cycle.plot)
}
