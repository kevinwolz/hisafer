#' Plot annual barchart of  major cycles
#' @description Plots an annual barchart of tree carbon pools, water fluxes, nitrogen fluxes, or light capture.
#' @return If \code{plot = TRUE}, returns a ggplot object. If \code{plot = FALSE}, returns the data that would create the plot.
#' If \code{hop} contains more than one simulation, the plot will be faceted by SimulationName.
#' @param hop An object of class hop or face.
#' @param cycle One of "carbon", "nitrogen", "water", or "light".
#' @param simu.names A character vector of the SimulationNames within \code{hop} to include. Use "all" to include all available values.
#' @param year.lim A numeric vector of length two providing the \code{c(minimum, maximum)} of calendar years to plot.
#' If no input, the full available time range is plotted. Use \code{NA} to refer to the start or end of the simulation.
#' @param color.palette A character stirng of hex values or R standard color names defining the color palette to use in plots with multiple simulations.
#' If \code{NULL}, the default, then the default color palette is a color-blind-friendly color palette.
#' @param plot If \code{TRUE}, the default, a ggplot object is returned. If \code{FALSE}, the data that would create the plot is returned.
#' @export
#' @importFrom dplyr %>%
#' @import ggplot2
#' @family hisafe analysis functions
#' @examples
#' \dontrun{
#' # To plot the simulated water cycle:
#' water.plot <- plot_hisafe_cycle(myhop, "water")
#'
#' # Once you have the plot object, you can display it and save it:
#' water.plot
#' ggsave_fitmax("water_cycle.png", water.plot)
#' }
plot_hisafe_cycle_annual <- function(hop,
                                     cycle,
                                     simu.names    = "all",
                                     year.lim      = c(NA, NA),
                                     color.palette = NULL,
                                     plot          = TRUE) {

  is_hop(hop, error = TRUE)
  if(simu.names[1] == "all") simu.names <- unique(hop$exp.plan$SimulationName)

  if(!(cycle %in% c("carbon", "nitrogen", "water", "light")))                  stop("cycle argument must be one of: carbon, nitrogen, water, light", call. = FALSE)
  if(!(length(year.lim) == 2 & (is.numeric(year.lim) | all(is.na(year.lim))))) stop("year.lim argument must be a numeric vector of length 2",        call. = FALSE)
  if(!is.logical(plot))                                                        stop("plot argument must be a logical",                               call. = FALSE)

  hop <- hop_filter(hop = hop, simu.names = simu.names)

  if(cycle == "water") {
    plot.data  <- get_water_fluxes(hop)
    plot.title <- "Water Cycle"
    y.lab      <- "Water flux (mm)"
    if(is.null(color.palette)) color.palette <- c("grey20", "grey40", "grey60", "grey80", "#D55E00", "#E69F00", "#F0E442", "#009E73", "#0072B2", "#56B4E9")

  } else if(cycle == "nitrogen") {
    plot.data  <- get_nitrogen_fluxes(hop)
    plot.title <- "Nitrogen Cycle"
    y.lab      <- "N flux (kg M ha-1)"
    if(is.null(color.palette)) color.palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "black", "grey50")

  } else if(cycle == "light") {
    plot.data  <- get_light_fluxes(hop)
    plot.title <- "Light Capture"
    y.lab      <- "Intercepted PAR (% of incident PAR)"
    if(is.null(color.palette)) color.palette <- c("#E69F00", "grey20", "#56B4E9", "#009E73")

  } else if(cycle == "carbon") {
    plot.data  <- get_carbon_pools(hop) %>%
      dplyr::group_by(SimulationName, Year, Month, Day, Date, JulianDay, flux) %>%
      dplyr::summarize(value = sum(value)) %>% # sum all trees on scene
      dplyr::ungroup()
    plot.title <- "Tree Carbon Pools"
    y.lab      <- "Tree C storage (Mg C ha-1)"
    if(is.null(color.palette)) color.palette <- c("#009E73","#999999", "#D55E00", "#E69F00", "#56B4E9", "#0072B2", "#F0E442", "#CC79A7")

  } else {
    stop("cycle argument not supported. Use one of: carbon, nitrogen, water, light.", call. = FALSE)
  }

  ## Set time limits
  if(is.na(year.lim[1])) year.lim[1] <- min(plot.data$Year)
  if(is.na(year.lim[2])) year.lim[2] <- max(plot.data$Year)

  ## Filter & Summarize plot data
  complete.yrs <- plot.data %>%
    dplyr::group_by(SimulationName, Year) %>%
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
#' @description Plots a daily timeseries of carbon pools, water extraction, nitrogen extraction, or light capture
#' @return If \code{plot = TRUE}, returns a ggplot object. If \code{plot = FALSE}, returns the data that would create the plot.
#' If \code{hop} contains more than one simulation, the plot will be faceted by SimulationName.
#' @param hop An object of class hop or face.
#' @param cycle One of "carbon", "nitrogen", "water", or "light".
#' @param years A numeric vector of the calendar years to include.
#' If more than one year is provided, years are used as facets, and only a single value can be supplied to \code{simu.names}.
#' Use "all" to include all available values.
#' @param simu.names A character vector of the SimulationNames within \code{hop} to include. Use "all" to include all available values.
#' If more than one value is supplied to \code{years}, then a single value must be suppied to \code{simu.names}.
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
                                    doy.lim       = c(1, 366),
                                    color.palette = NULL,
                                    plot          = TRUE) {

  is_hop(hop, error = TRUE)
  profile_check(hop, "plot", error = TRUE)
  if(!(cycle %in% c("carbon", "nitrogen", "water", "light")))   stop("cycle argument must be one of: carbon, nitrogen, water, light", call. = FALSE)
  if(!(all(is.numeric(years))        | years[1]      == "all")) stop("years argument must be 'all' or a numeric vector",              call. = FALSE)
  if(!(length(doy.lim) == 2 & all(doy.lim %in% 1:366)))         stop("doy.lim argument must be of length 2 with values in 1:366",     call. = FALSE)
  if(!is.logical(plot))                                         stop("plot argument must be a logical",                               call. = FALSE)

  if(simu.names[1] == "all") simu.names <- unique(hop$exp.plan$SimulationName)
  if(years[1]      == "all") years      <- unique(hop$plot$Year[which(hop$plot$SimulationName %in% simu.names)])
  if(length(years) > 1 & length(simu.names) > 1) stop("cannot supply multiple simu.names and multiple years", call. = FALSE)

  if(!all(years %in% hop$plot$Year))                stop("one or more values in years is not present in the plot profile of hop",      call. = FALSE)

  hop <- hop_filter(hop = hop, simu.names = simu.names)

  if(cycle == "water") {
    plot.data   <- get_water_fluxes(hop) %>%
      dplyr::filter(flux %in% c("Transpiration - Trees", "Transpiration - Secondary crop", "Transpiration - Main crop")) %>%
      dplyr::mutate(flux = droplevels(flux))
    levels(plot.data$flux) <- c("Trees", "Secondary crop", "Main crop")
    if(is.null(color.palette)) color.palette <- c("#E69F00", "#56B4E9", "#009E73")
    cycle.geom  <- geom_area(aes(fill = flux), na.rm = TRUE)
    cycle.scale <- scale_fill_manual(values = color.palette)
    pre.title   <- "Water Extraction"
    y.lab       <- "Extracted water (mm)"

  } else if(cycle == "nitrogen") {
    plot.data   <- get_nitrogen_fluxes(hop) %>%
      dplyr::filter(flux %in% c("Trees", "Secondary crop", "Main crop")) %>%
      dplyr::mutate(flux = droplevels(flux))
    if(is.null(color.palette)) color.palette <- c("#E69F00", "#56B4E9", "#009E73")
    cycle.geom  <- geom_area(aes(fill = flux), na.rm = TRUE)
    cycle.scale <- scale_fill_manual(values = color.palette)
    pre.title   <- "Nitrogen Extraction"
    y.lab       <- "Extracted nitrogen (kg ha-1)"

  } else if(cycle == "light") {
    plot.data   <- get_light_fluxes(hop)
    if(is.null(color.palette)) color.palette <- c("#E69F00", "grey20", "#56B4E9", "#009E73")
    cycle.geom  <- geom_area(aes(fill = flux), na.rm = TRUE)
    cycle.scale <- scale_fill_manual(values = color.palette)
    pre.title   <- "Light Capture"
    y.lab       <- "Intercepted PAR (% of incident PAR)"

  } else if(cycle == "carbon") {
    plot.data   <- get_carbon_pools(hop)
    if(is.null(color.palette)) color.palette <- c("#009E73","#999999", "#D55E00", "#E69F00", "#56B4E9", "#0072B2", "#F0E442", "#CC79A7")
    cycle.geom  <- geom_area(aes(fill = flux), na.rm = TRUE)
    cycle.scale <- scale_fill_manual(values = color.palette)
    pre.title <- "Tree Carbon Pools"
    y.lab      <- "Tree C storage (Mg C ha-1)"
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
  plot.data <- filter(plot.data, !(Year %in% yrs.to.remove))

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
  if(plot) return(plot.obj) else return(out.data)
}


#' Get water fluxes from a hop object
#' @description Get water fluxes from a hop object.
#' Used within \code{\link{plot_hisafe_cycle}}, \code{\link{plot_hisafe_cycle_annual}}, \code{\link{plot_hisafe_cycle_daily}}
#' @return A tibble with extracted and calculated water fluxes.
#' @param hop An object of class hop or face.
#' @importFrom dplyr %>%
get_water_fluxes <- function(hop) {
  profile_check(hop, c("plot", "climate"), error = TRUE)
  # variable_check(hop, "plot",
  #                c("mainCropArea", "interCropArea", "parIncident", "parInterceptedByMainCrop", "parInterceptedByInterCrop", "parInterceptedByTrees"),
  #                error = TRUE)
  # variable_check(hop, "climate",
  #                c("mainCropArea", "interCropArea", "parIncident", "parInterceptedByMainCrop", "parInterceptedByInterCrop", "parInterceptedByTrees"),
  #                error = TRUE)

  climate <- hop$climate %>%
    dplyr::select(SimulationName, Year, Date, precipitation)

  out <- hop$plot %>%
    replace(is.na(.), 0) %>%
    dplyr::mutate(#waterEvaporatedInMainCrop  = waterEvaporatedInMainCrop  * mainCropArea  / (mainCropArea + interCropArea),
      #waterEvaporatedInInterCrop = waterEvaporatedInInterCrop * interCropArea / (mainCropArea + interCropArea),
      #waterExtractedByMainCrop   = waterExtractedByMainCrop   * mainCropArea  / (mainCropArea + interCropArea),
      #waterExtractedByInterCrop  = waterExtractedByInterCrop  * interCropArea / (mainCropArea + interCropArea),
      #irrigationInMainCrop       = irrigationInMainCrop       * mainCropArea  / (mainCropArea + interCropArea),
      #irrigationInInterCrop      = irrigationInInterCrop      * interCropArea / (mainCropArea + interCropArea),
      #interceptedRainByMainCrop  = interceptedRainByMainCrop  * mainCropArea  / (mainCropArea + interCropArea),
      #interceptedRainByInterCrop = interceptedRainByInterCrop * interCropArea / (mainCropArea + interCropArea),
      irrigation   = -irrigationInMainCrop     + -irrigationInInterCrop,
      aquifer      = -waterFromSaturation +
        -waterExtractedInSaturationByMainCrop + -waterExtractedInSaturationByInterCrop + -waterExtractedInSaturationByTrees,
      interception = interceptedRainByMainCrop + interceptedRainByInterCrop + interceptedRainByTrees,
      evaporation  = waterEvaporatedInMainCrop + waterEvaporatedInInterCrop) %>%
    dplyr::left_join(climate, by = c("SimulationName", "Year", "Date")) %>%
    dplyr::mutate(precipitation = -precipitation) %>%
    dplyr::select(SimulationName, Year, Month, Day, Date, JulianDay,
                  runOff, drainage, evaporation, interception,
                  waterExtractedByMainCrop, waterExtractedByInterCrop, waterExtractedByTrees,
                  irrigation, aquifer, precipitation) %>%
    tidyr::gather(key = "flux", value = "value", runOff:precipitation) %>%
    dplyr::mutate(flux = factor(flux,
                                levels = c("interception",
                                           "runOff",
                                           "drainage",
                                           "evaporation",
                                           "waterExtractedByTrees",
                                           "waterExtractedByInterCrop",
                                           "waterExtractedByMainCrop",
                                           "irrigation",
                                           "aquifer",
                                           "precipitation"),
                                labels = c("Interception",
                                           "Run-off",
                                           "Drainage",
                                           "Soil evaporation",
                                           "Transpiration - Trees",
                                           "Transpiration - Secondary crop",
                                           "Transpiration - Main crop",
                                           "Irrigation",
                                           "Aquifer",
                                           "Precipitation")))
  return(out)
  # Extract variables required for the water budget
  # dplyr::select(SimulationName, Year, mainCropArea, interCropArea,
  #               waterEvaporatedInMainCrop,
  #               waterEvaporatedInInterCrop,
  #               waterExtractedByMainCrop,
  #               waterExtractedByInterCrop,
  #               waterExtractedByTrees,
  #               irrigationInMainCrop,
  #               irrigationInInterCrop,
  #               drainage,
  #               interceptedRainByMainCrop,
  #               interceptedRainByInterCrop,
  #               interceptedRainByTrees,
  #               #rainTransmitted,
  #               runOff,
  #               #surfaceRunOff,
  #               waterFromSaturation) %>%
}

#' Get nitrogen fluxes from a hop object
#' @description Get nitrogen fluxes from a hop object.
#' Used within \code{\link{plot_hisafe_cycle}}, \code{\link{plot_hisafe_cycle_annual}}, \code{\link{plot_hisafe_cycle_daily}}
#' @return A tibble with extracted and calculated nitrogen fluxes.
#' @param hop An object of class hop or face.
#' @importFrom dplyr %>%
get_nitrogen_fluxes <- function(hop) {
  profile_check(hop, "plot", error = TRUE)
  # variable_check(hop, "plot",
  #                c("mainCropArea", "interCropArea", "parIncident", "parInterceptedByMainCrop", "parInterceptedByInterCrop", "parInterceptedByTrees"),
  #                error = TRUE)
  out <- hop$plot %>%
    replace(is.na(.), 0) %>%
    # Extract variables required for the nitrogen budget
    # dplyr::select(SimulationName, Year, mainCropArea, interCropArea,
    #               # mineralization
    #               # leaching (lixivation)
    #               # denitrification
    #               # exportation
    #               # fixation
    #               # humification
    #               # humus mineralization
    #               # immobilization
    #               # organisation
  #               # residuMineralization (INTERNAL?)
  #               # residus (INTERNAL?)
  #               # restitution
  #               # volatilization
  #               nitrogenExtractedByMainCrop, # scale by main crop area?
  #               nitrogenExtractedByInterCrop, # scale by inter crop area?
  #               nitrogenExtractedByTrees,
  #               nitrogenFertilisationInMainCrop,
  #               nitrogenFertilisationInInterCrop,
  #               nitrogenIrrigationInMainCrop,
  #               nitrogenIrrigationInInterCrop,
  #               nitrogenRainInMainCrop,
  #               nitrogenRainInInterCrop) %>%
  # Combine variables
  dplyr::mutate(mineralization = -nitrogenHumusMineralisation     + -nitrogenResiduMineralisation,
                fertilization  = -nitrogenFertilisationInMainCrop + -nitrogenFertilisationInInterCrop,
                irrigation     = -nitrogenIrrigationInMainCrop    + -nitrogenIrrigationInInterCrop,
                deposition     = -nitrogenRainInMainCrop          + -nitrogenRainInInterCrop) %>%
    dplyr::select(SimulationName, Year, Month, Day, Date, JulianDay,
                  nitrogenExtractedByMainCrop, nitrogenExtractedByInterCrop, nitrogenExtractedByTrees,
                  nitrogenFixation, nitrogenVolatilisation, nitrogenDenitrification,
                  mineralization, fertilization, irrigation, deposition) %>%
    tidyr::gather(key = "flux", value = "value", nitrogenExtractedByMainCrop:deposition) %>%
    dplyr::mutate(flux = factor(flux,
                                levels = c("nitrogenExtractedByTrees",
                                           "nitrogenExtractedByInterCrop",
                                           "nitrogenExtractedByMainCrop",
                                           "nitrogenVolatilisation",
                                           "nitrogenDenitrification",
                                           "mineralization",
                                           "fertilization",
                                           "irrigation",
                                           "deposition",
                                           "nitrogenFixation"),
                                labels = c("Trees",
                                           "Secondary crop",
                                           "Main crop",
                                           "Volatilization",
                                           "Denitrification",
                                           "Mineralization",
                                           "Fertilization",
                                           "Irrigation",
                                           "Deposition",
                                           "Fixation")))
  return(out)
}

#' Get light fluxes from a hop object
#' @description Get light fluxes from a hop object.
#' Used within \code{\link{plot_hisafe_cycle}}, \code{\link{plot_hisafe_cycle_annual}}, \code{\link{plot_hisafe_cycle_daily}}
#' @return A tibble with extracted and calculated light fluxes.
#' @param hop An object of class hop or face.
#' @importFrom dplyr %>%
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
#' Used within \code{\link{plot_hisafe_cycle}}, \code{\link{plot_hisafe_cycle_annual}}, \code{\link{plot_hisafe_cycle_daily}}
#' @return A tibble with extracted and calculated carbon pools.
#' @param hop An object of class hop or face.
#' @importFrom dplyr %>%
get_carbon_pools <- function(hop) {
  profile_check(hop, "trees", error = TRUE)
  variable_check(hop, "trees",
                 c("carbonFoliage", "carbonBranches", "carbonCoarseRoots", "carbonFineRoots", "carbonLabile", "carbonStem", "carbonStump"),
                 error = TRUE)
  out <- hop$trees %>%
    replace(is.na(.), 0) %>%
    dplyr::select(SimulationName, Year, Month, Day, Date, JulianDay, id,
                  carbonFoliage, carbonBranches, carbonCoarseRoots, carbonFineRoots, carbonLabile, carbonStem, carbonStump) %>%
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
    dplyr::left_join(dplyr::select(hop$plot.info, SimulationName, plot.area), by = "SimulationName") %>%
    dplyr::mutate(value = value / (plot.area / 10000)) # convert from Mg C tree-1 to Mg C ha-1
  return(out)
}
