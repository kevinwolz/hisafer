#' Plot annual barchart of carbon, nitrogen, water, or light cycle
#' @description Plots an annual barchart of the water, nitrogen, light, or carbon cycle
#' @return If \code{plot = TRUE}, returns a ggplot object. If \code{plot = FALSE}, returns the data that would create the plot.
#' If \code{hop} contains more than one simulation, the plot will be faceted by SimulationName.
#' @param hop An object of class hop or face.
#' @param cycle One of "carbon", "nitrogen", "water", or "light".
#' @param simu.names A character vector of the SimulationNames within \code{hop} to include.
#' @param time.lim A numeric vector of length two providing the \code{c(minimum, maximum)} of years (absolute or since planting) to plot.
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
#' # To plot the water cycle of a plot:
#' water.plot <- plot_annual_cycle(myhop, "water")
#'
#' # Once you have the plot object, you can display it and save it:
#' water.plot
#' ggplot2::ggsave("water_cycle.png", water.plot)
#' }
plot_annual_cycle <- function(hop,
                              cycle,
                              simu.names    = "all",
                              time.lim      = NULL,
                              color.palette = NULL,
                              plot          = TRUE) {

  if(!("hop" %in% class(hop))) stop("hop argument not of class hop", call. = FALSE)
  if(simu.names[1] == "all") simu.names <- unique(hop$exp.plan$SimulationName)

  ## Extract required data from hop
  fhop <- hop_filter(hop, simu.names)

  if(cycle == "water") {
    required.profiles <- c("climate", "annualplot")
    if(!all(required.profiles %in% names(fhop))) stop(paste(cycle, "cycle calculations require data from",
                                                            paste(required.profiles, collapse = " and "),
                                                            "export profiles"), call. = FALSE)

    climate <- fhop$climate %>%
      dplyr::select(SimulationName, Year, precipitation) %>%
      dplyr::group_by(SimulationName, Year) %>%
      dplyr::summarize(precipitation = -sum(precipitation)) %>%
      tidyr::gather(key = "flux", value = "value", precipitation)

    plot.data <- fhop$annualplot %>%
      # Extract variables required for the water budget
      dplyr::select(SimulationName, Year, mainCropArea, interCropArea,
                    annualWaterEvaporatedInMainCrop,
                    annualWaterEvaporatedInInterCrop,
                    annualWaterExtractedByMainCrop,
                    annualWaterExtractedByInterCrop,
                    annualWaterExtractedByTrees,
                    annualIrrigationInMainCrop,
                    annualIrrigationInInterCrop,
                    annualDrainage,
                    annualInterceptedRainByMainCrop,
                    annualInterceptedRainByInterCrop,
                    annualInterceptedRainByTrees,
                    annualRainTransmitted,
                    annualRunOff,
                    annualSurfaceRunOff,
                    annualWaterExtractedInSaturationByMainCrop,
                    annualWaterExtractedInSaturationByInterCrop,
                    annualWaterExtractedInSaturationByTrees,
                    annualWaterFromSaturation,
                    annualWaterToDesaturation) %>%
      # Scale by mainCrop or interCrop area
      dplyr::mutate(annualWaterEvaporatedInMainCrop             = annualWaterEvaporatedInMainCrop             * mainCropArea  / (mainCropArea + interCropArea),
                    annualWaterEvaporatedInInterCrop            = annualWaterEvaporatedInInterCrop            * interCropArea / (mainCropArea + interCropArea),
                    annualWaterExtractedByMainCrop              = annualWaterExtractedByMainCrop              * mainCropArea  / (mainCropArea + interCropArea),
                    annualWaterExtractedByInterCrop             = annualWaterExtractedByInterCrop             * interCropArea / (mainCropArea + interCropArea),
                    annualIrrigationInMainCrop                  = annualIrrigationInMainCrop                  * mainCropArea  / (mainCropArea + interCropArea),
                    annualIrrigationInInterCrop                 = annualIrrigationInInterCrop                 * interCropArea / (mainCropArea + interCropArea),
                    annualInterceptedRainByMainCrop             = annualInterceptedRainByMainCrop             * mainCropArea  / (mainCropArea + interCropArea),
                    annualInterceptedRainByInterCrop            = annualInterceptedRainByInterCrop            * interCropArea / (mainCropArea + interCropArea),
                    annualWaterExtractedInSaturationByMainCrop  = annualWaterExtractedInSaturationByMainCrop  * mainCropArea  / (mainCropArea + interCropArea),
                    annualWaterExtractedInSaturationByInterCrop = annualWaterExtractedInSaturationByInterCrop * interCropArea / (mainCropArea + interCropArea)) %>%
      # Combine variables
      dplyr::mutate(irrigation = -annualIrrigationInMainCrop + -annualIrrigationInInterCrop,
                    aquifer = -annualWaterExtractedInSaturationByMainCrop + -annualWaterExtractedInSaturationByInterCrop + -annualWaterExtractedInSaturationByTrees,
                    interception = annualInterceptedRainByMainCrop + annualInterceptedRainByInterCrop + annualInterceptedRainByTrees - annualRainTransmitted,
                    evaporation = annualWaterEvaporatedInMainCrop + annualWaterEvaporatedInInterCrop) %>%
      tidyr::gather(key = "flux", value = "value", annualWaterEvaporatedInMainCrop:annualWaterExtractedByTrees) %>%
      dplyr::bind_rows(climate) %>%
      dplyr::mutate(flux = factor(flux,
                                  levels = c("interception",
                                             "annualSurfaceRunOff",
                                             "annualDrainage",
                                             "evaporation",
                                             "annualWaterExtractedByMainCrop",
                                             "annualWaterExtractedByInterCrop",
                                             "annualWaterExtractedByTrees",
                                             "irrigation",
                                             "aquifer",
                                             "precipitation"),
                                  labels = c("Interception - all",
                                             "Surface run-off",
                                             "Drainage",
                                             "Soil evaporation",
                                             "Transpiration - main crop",
                                             "Transpiration - secondary crop",
                                             "Transpiration - trees",
                                             "Irrigation",
                                             "Aquifer",
                                             "Precipitation")))

    plot.title <- "Water Cycle"
    y.lab      <- "Water flux (mm)"
    if(is.null(color.palette)) color.palette <- c("grey20", "grey40", "grey60", "grey80",
                                                  "#D55E00", "#E69F00", "#F0E442",
                                                  "#009E73", "#0072B2", "#56B4E9") # "#CC79A7"

  } else if(cycle == "nitrogen") {
    required.profiles <- "annualplot"
    if(!all(required.profiles %in% names(fhop))) stop(paste(cycle, "cycle calculations require data from",
                                                            paste(required.profiles, collapse = " and "),
                                                            "export profiles"), call. = FALSE)
    plot.data <- fhop$annualplot %>%
      # Extract variables required for the nitrogen budget
      dplyr::select(SimulationName, Year, mainCropArea, interCropArea,
                    # mineralization
                    # leaching (lixivation)
                    # denitrification
                    # exportation
                    # fixation
                    # humification
                    # humus mineralization
                    # immobilization
                    # organisation
                    # residuMineralization (INTERNAL?)
                    # residus (INTERNAL?)
                    # restitution
                    # volatilization
                    annualNitrogenExtractedByMainCrop, # scale by main crop area?
                    annualNitrogenExtractedByInterCrop, # scale by inter crop area?
                    annualNitrogenExtractedByTrees,
                    annualNitrogenFertilisationInMainCrop,
                    annualNitrogenFertilisationInInterCrop,
                    annualNitrogenIrrigationInMainCrop,
                    annualNitrogenIrrigationInInterCrop,
                    annualNitrogenRainInMainCrop,
                    annualNitrogenRainInInterCrop) %>%
      # Combine variables
      dplyr::mutate(fertilization = -annualNitrogenFertilisationInMainCrop + -annualNitrogenFertilisationInInterCrop,
                    irrigation    = -annualNitrogenIrrigationInMainCrop    + -annualNitrogenIrrigationInInterCrop,
                    deposition    = -annualNitrogenRainInMainCrop          + -annualNitrogenRainInInterCrop) %>%
      dplyr::select(-(annualNitrogenFertilisationInMainCrop:annualNitrogenRainInInterCrop)) %>%
      tidyr::gather(key = "flux", value = "value", annualNitrogenExtractedByMainCrop:deposition) %>%
      dplyr::mutate(flux = factor(flux,
                                  levels = c("annualNitrogenExtractedByMainCrop",
                                             "annualNitrogenExtractedByInterCrop",
                                             "annualNitrogenExtractedByTrees",
                                             "fertilization",
                                             "irrigation",
                                             "deposition"),
                                  labels = c("Main crop",
                                             "Intercrop",
                                             "Trees",
                                             "Fertilization",
                                             "Irrigation",
                                             "Deposition")))

    plot.title <- "Nitrogen Cycle"
    y.lab      <- "N flux (kg M ha-1)"
    if(is.null(color.palette)) color.palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                                                  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  } else if(cycle == "light") {
    required.profiles <- "annualplot"
    if(!all(required.profiles %in% names(fhop))) stop(paste(cycle, "cycle calculations require data from",
                                                            paste(required.profiles, collapse = " and "),
                                                            "export profiles"), call. = FALSE)
    na.to.zero <- function(x) {
      x[is.na(x)] <- 0
      return(x)
    }

    plot.data <- fhop$annualplot %>%
      # Extract variables required for the light budget
      dplyr::select(SimulationName, Year, mainCropArea, interCropArea,
                    annualParIncident,
                    annualParInterceptedByMainCrop,
                    annualParInterceptedByInterCrop,
                    annualParInterceptedByTrees) %>%
      dplyr::mutate(annualParInterceptedByMainCrop  = na.to.zero(annualParInterceptedByMainCrop),
                    annualParInterceptedByInterCrop = na.to.zero(annualParInterceptedByInterCrop),
                    annualParInterceptedByTrees     = na.to.zero(annualParInterceptedByTrees)) %>%
      # Scale by area to conver to moles and then make percentage of inceident par
      dplyr::mutate(annualParIncident      = annualParIncident               * (mainCropArea + interCropArea),
                    InterceptedByMainCrop  = annualParInterceptedByMainCrop  * mainCropArea                   / annualParIncident * 100,
                    InterceptedByInterCrop = annualParInterceptedByInterCrop * interCropArea                  / annualParIncident * 100,
                    InterceptedByTrees     = annualParInterceptedByTrees     * (mainCropArea + interCropArea) / annualParIncident * 100,
                    notCaptured            = 100 - InterceptedByMainCrop - InterceptedByInterCrop - InterceptedByTrees) %>%
      dplyr::select(-(annualParIncident:annualParInterceptedByTrees)) %>%
      tidyr::gather(key = "flux", value = "value", InterceptedByMainCrop:notCaptured) %>%
      dplyr::mutate(flux = factor(flux,
                                  levels = c("InterceptedByTrees",
                                             "notCaptured",
                                             "InterceptedByInterCrop",
                                             "InterceptedByMainCrop"),
                                  labels = c("Trees",
                                             "Not captured",
                                             "Secondary crop",
                                             "Main crop")))
    plot.title <- "Light Capture"
    y.lab      <- "Intercepted PAR (% of incident PAR)"
    if(is.null(color.palette)) color.palette <- c("#E69F00", "grey20", "#56B4E9", "#009E73")
  } else if(cycle == "carbon") {
    required.profiles <- "annualplot"
    if(!all(required.profiles %in% names(fhop))) stop(paste(cycle, "cycle calculations require data from",
                                                            paste(required.profiles, collapse = " and "),
                                                            "export profiles"), call. = FALSE)
    plot.data <- fhop$annualplot %>%
      # Extract variables required for the carbon budget
      dplyr::select(SimulationName, Year,
                    maxTreesCarbonFoliage,
                    annualTreesCarbonBranches,
                    annualTreesCarbonCoarseRoots,
                    annualTreesCarbonFineRoots,
                    annualTreesCarbonLabile,
                    annualTreesCarbonStem,
                    annualTreesCarbonStump) %>%
      dplyr::mutate(annualTreesCarbonStump       = -annualTreesCarbonStump) %>%
      dplyr::mutate(annualTreesCarbonCoarseRoots = -annualTreesCarbonCoarseRoots) %>%
      dplyr::mutate(annualTreesCarbonFineRoots   = -annualTreesCarbonFineRoots) %>%
      tidyr::gather(key = "flux", value = "value", maxTreesCarbonFoliage:annualTreesCarbonStump) %>%
      dplyr::mutate(flux = factor(flux,
                                  levels = c("maxTreesCarbonFoliage",
                                             "annualTreesCarbonLabile",
                                             "annualTreesCarbonBranches",
                                             "annualTreesCarbonStem",
                                             "annualTreesCarbonFineRoots",
                                             "annualTreesCarbonCoarseRoots",
                                             "annualTreesCarbonStump"),
                                  labels = c("Leaves",
                                             "Labile",
                                             "Branches",
                                             "Stem",
                                             "Fine Roots",
                                             "Coarse Roots",
                                             "Stump"))) %>%
      dplyr::mutate(value = value / 1e3) # convert from kg C ha-1 to Mg C ha-1

    plot.title <- "Tree Carbon Pools"
    y.lab      <- "C storage (Mg C ha-1)"
    if(is.null(color.palette)) color.palette <- c("#009E73","#999999", "#D55E00", "#E69F00",
                                                  "#56B4E9", "#0072B2", "#F0E442", "#CC79A7")
  } else {
    stop("Cycle argument not supported. Use one of: carbon, nitrogen, water, light.", call. = FALSE)
  }

  ## Set faceting
  if("face" %in% class(fhop)) {
    AF.sims <- sort(fhop$exp.plan$SimulationName[!(fhop$exp.plan$SimulationName %in% c("Monocrop", "Forestry"))])
    if(length(AF.sims) == 1) facet_simu <- facet_wrap(~SimulationName, nrow = 1) else facet_simu <- facet_wrap(~SimulationName)
    all.sims <- c("Monocrop", AF.sims, "Forestry")
    all.sims <- all.sims[all.sims %in% simu.names]
    plot.data$SimulationName <- factor(plot.data$SimulationName, levels = all.sims)
    scale.x    <- scale_x_continuous(expand = c(0,0))
  } else if ("hop-group" %in% class(fhop)) {
    facet_simu <- facet_wrap(~SimulationName)
    scale.x    <- scale_x_continuous(expand = c(0,0))
  } else {
    facet_simu <- geom_blank()
    scale.x    <- scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL), expand = c(0,0))
  }

  ## Set time limits
  plot.data$Year0 <- plot.data$Year - min(plot.data$Year) + 1
  if(!is.null(time.lim)) {
    if(all(time.lim > 1000)) time.lim    <- time.lim - min(plot.data$Year) + 1
    if(is.na(time.lim[1]))   time.lim[1] <- min(plot.data$Year0)
    if(is.na(time.lim[2]))   time.lim[2] <- max(plot.data$Year0)
    plot.data <- dplyr::filter(plot.data, Year >= time.lim[1], Year <= time.lim[2])
  }

  ## Create plot
  plot.obj <- ggplot(plot.data, aes(x = Year0, y = value, fill = flux)) +
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
          axis.text.x       = element_text(margin = margin(t = 5, unit = "points")),
          axis.text.y       = element_text(margin = margin(r = 5, unit = "points")))

  #ggsave("/Users/kevinwolz/Desktop/carbon.png", plot.obj, scale = 1, height = 8.5, width = 11)
  #ggsave("/Users/kevinwolz/Desktop/light.png", plot.obj, scale = 1, height = 8.5, width = 11)

  if(plot) return(plot.obj) else return(dplyr::mutate(plot.data, cycle = cycle))
}
