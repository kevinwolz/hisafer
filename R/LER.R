#' Plot LER for yield, light, water, or nitrogen
#' @description Plot LER for yield, light, water, or nitrogen.
#' @return If \code{plot = TRUE}, returns a ggplot object, otherwise the data that would create the plot is returned.
#' @param face An object of class "face".
#' @param cycle One of "yield", "nitrogen", "water", or "light".
#' @param timescales One or both of "Annual" and "Cumulative", indicating which timescales to plot.
#' @param components One or more of "LER", "Trees", and "Crop, indicating which components to plot.
#' @param color.palette A character vector of hex values or R standard color names defining the color palette for LER, Trees, and Crop lines, respectively.
#' @param size.palette A numeric vector of values defining the size for LER, Trees, and Crop lines, respectively.
#' @param hline1 A logical indicating whether or not to plot a dotted horizontal line at y = 1.
#' @param ymax A numeric value for the upper limit of the y-axis. If \code{NA}, the maximum value in the data is use.
#' @param plot If \code{TRUE}, the default, a ggplot object is returned. If \code{FALSE}, the data that would create the plot is returned.
#' @param ... Other arguments passed to \code{\link{plot_hisafe_cycle_bar}}.
#' @export
#' @importFrom dplyr %>%
#' @import ggplot2
#' @family hisafe analysis functions
#' @examples
#' \dontrun{
#' ler.plot <- LER(hop, "yield")
#' }
LER <- function(face,
                path          = "",
                simul         = "",
                cycle         = "yield",
                timescales    = c("Annual", "Cumulative"),
                components    = c("LER", "Trees", "Crops"),
                color.palette = c("black", "#009E73", "#E69F00"),
                size.palette  = c(2, 1, 1),
                hline1        = TRUE,
                ymax          = NA,
                plot          = TRUE, ...) {
  library(dplyr)
  supported.cycles     <- c("yield", "nitrogen", "water", "light")
  supported.timescales <- c("Annual", "Cumulative")
  supported.components <- c("LER", "Trees", "Crops")

  is_face(face, error = TRUE)
  if(nrow(face$metadata) > 3)                        stop("LER can only handle face objects containing a single agroforestry simulation",               call. = FALSE)
  if(!(length(color.palette) >= length(components))) stop("color.palette argument must have length greather or equal to length of components argument", call. = FALSE)
  if(!(length(size.palette)  >= length(components))) stop("size.palette argument must have length greather or equal to length of components argument",  call. = FALSE)
  if(!is.numeric(size.palette))                      stop("size.palette argument must be numeric",                                                      call. = FALSE)
  if(!(cycle %in% supported.cycles))             stop(paste("cycle argument must be one of:",
                                                            paste(supported.cycles, collapse = ", ")),     call. = FALSE)
  if(!all(timescales %in% supported.timescales)) stop(paste("timescales argument must be one or more of:",
                                                            paste(supported.timescales, collapse = ", ")), call. = FALSE)
  if(!all(components %in% supported.components)) stop(paste("components argument must be one or more of:",
                                                            paste(supported.components, collapse = ", ")), call. = FALSE)
  is_TF(plot)

  ## Get flux data
  cycle.data <- plot_hisafe_ler(hop        = face,
                                cycle      = cycle,
                                plot       = FALSE,
                                tidy       = TRUE, ...) %>%
  dplyr::ungroup()

  crop.descrip <- c("Crops",          # yield
                    "Uptake - Crops", # nitrogen
                    "Uptake - Crops", # water
                    "Crops")          # light

  tree.descrip <- c("Trees",          # yield
                    "Uptake - Trees", # nitrogen
                    "Uptake - Trees", # water
                    "Trees")          # light

  tree.id <- tree.descrip[cycle == supported.cycles]
  crop.id <- crop.descrip[cycle == supported.cycles]

  pf <- cycle.data %>%
    dplyr::filter(SimulationName == "Forestry") %>%
    dplyr::filter(flux %in% tree.id) %>%
    dplyr::mutate(flux = "trees.pf") %>%
    tidyr::spread(key = "flux", value = "value") %>%
    dplyr::select(-SimulationName)

  mc <- cycle.data %>%
    dplyr::filter(SimulationName == "Monocrop") %>%
    dplyr::filter(flux %in% crop.id) %>%
    dplyr::mutate(flux = "crops.mc") %>%
    tidyr::spread(key = "flux", value = "value") %>%
    dplyr::select(-SimulationName)

  af <- cycle.data %>%
    dplyr::filter(SimulationName == "Agroforestry") %>%
    dplyr::filter(flux %in% c(tree.id, crop.id)) %>%
    dplyr::mutate(flux = factor(flux, levels = c(tree.id, crop.id), labels = c("trees.af", "crops.af"))) %>%
    tidyr::spread(key = "flux", value = "value") %>%
    dplyr::left_join(pf, by = c("Year", "cycle")) %>%
    dplyr::left_join(mc, by = c("Year", "cycle")) %>%
    dplyr::arrange(SimulationName, Year) %>%
    dplyr::group_by(SimulationName)

  ## CALCULATE LER

  calculate_ler <- function(x) {
    out <- x %>%
      dplyr::mutate(ry.trees = trees.af / trees.pf) %>%
      dplyr::mutate(ry.crops = crops.af / crops.mc) %>%
      dplyr::mutate(ler = ry.trees + ry.crops)
  }

  cum.ler <- af %>%
    dplyr::mutate_at(c("trees.af", "crops.af", "trees.pf", "crops.mc"), cumsum) %>%
    calculate_ler() %>%
    dplyr::mutate(timescale = "Cumulative") %>%
    dplyr::select(SimulationName, Year, cycle, timescale, dplyr::everything())  %>%
    dplyr::mutate(SimulationName = simul)


  ler <- af %>%
    calculate_ler() %>%
    dplyr::mutate(timescale = "Annual") %>%
    dplyr::select(SimulationName, Year, cycle, timescale, dplyr::everything()) %>%
    dplyr::mutate(SimulationName = simul) %>%
    dplyr::bind_rows(cum.ler)

  if (path != "") write.csv(ler, paste(path, "/", simul, "_ler.csv" ,  sep=""), row.names = FALSE)

  plot.data <- ler %>%
    tidyr::gather(ry.trees, ry.crops, ler, key = "metric", value = "value") %>%
    dplyr::mutate(metric = factor(metric, levels = c("ler", "ry.trees", "ry.crops"), labels = c("LER", "Trees", "Crops"))) %>%
    dplyr::filter(timescale %in% timescales) %>%
    dplyr::filter(metric    %in% components)

  ## PLOT
  plot.obj <- ggplot(plot.data, aes(x        = Year,
                                    y        = value,
                                    linetype = timescale,
                                    color    = metric,
                                    size     = metric)) +
    labs(title    = paste0(toupper(substring(cycle, 1, 1)), substring(cycle, 2), " LER"),
         x        = "Year",
         y        = ifelse(cycle == "yield", "Relative yield", paste("Relative", cycle, "uptake")),
         linetype = NULL,
         color    = NULL,
         size     = NULL) +
    geom_line() +
    scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = c(0, ymax)) +
    scale_color_manual(values = color.palette) +
    scale_size_manual(values  = size.palette) +
    theme_hisafe_ts() +
    theme(panel.grid.major = element_blank())

  if(hline1)                  plot.obj <- plot.obj + geom_hline(yintercept = 1, linetype = "dotted")
  if(length(timescales) == 1) plot.obj <- plot.obj + guides(linetype = FALSE)
  if(length(components) == 1) plot.obj <- plot.obj + guides(color    = FALSE, size = FALSE)

  if(plot) return(plot.obj) else return(plot.data)
}


#' Plot annual LER
#' @description Plots an annual barchart of tree carbon pools, water fluxes, nitrogen fluxes, or light capture.
#' @return If \code{plot = TRUE}, returns a ggplot object. If \code{plot = FALSE}, returns the data that would create the plot.
#' If \code{hop} contains more than one simulation, the plot will be faceted by SimulationName.
#' @param hop An object of class hop or face. treeNitrogenFineRootsLitter
#' @param cycle One of "carbon", "nitrogen", "water", "light", or "yield".
#' @param freq One of "year", "month", "day".
#' @param simu.names A character vector of the SimulationNames within \code{hop} to include. Use "all" to include all available values.
#' @param tree.ids A numeric vector indicating a subset of tree ids to plot. Use "all" to include all available values.
#' This only applies when \code{cycle} is "carbon".
#' @param year.lim A numeric vector of length two providing the \code{c(minimum, maximum)} of calendar years to plot.
#' If no input, the full available time range is plotted. Use \code{NA} to refer to the start or end of the simulation.
#' @param doy.start The JulianDay [1-365] on which to start the annual cycle accounting. Use 'sim' to specify the starting JulianDay of the simulation.
#' @param color.palette A character string of hex values or R standard color names defining the color palette to use in plots with multiple simulations.
#' If \code{NULL}, the default, then the default color palette is a color-blind-friendly color palette.
#' @param bar.color A hex value or R standard color name defining the color to use for bar plot borders
#' @param plot If \code{TRUE}, the default, a ggplot object is returned. If \code{FALSE}, the data that would create the plot is returned.
#' @param tidy If \code{TRUE}, the summqrized version of the budget is created. Otherwise, a fully expanded budget is created.
#' @details Detailed description of the flux components of the nitrogen and water cycles:
#'
#' }
plot_hisafe_ler <- function(hop,
                            cycle,
                            freq          = "year",
                            simu.names    = "all",
                            tree.ids      = "all",
                            years         = "all",
                            months        = "all",
                            date.min      = NA,
                            date.max      = NA,
                            doy.start     = 1,
                            color.palette = NULL,
                            bar.color     = "black",
                            plot          = TRUE,
                            tidy          = plot) {

  is_hop(hop, error = TRUE)

  METHOD <- ifelse(profile_check(hop, "yield"), "yield", "plot")

  plot.data <- get_yields_ler(hop = hop, profile = METHOD)

  plot.data <- plot.data %>%
    dplyr::mutate(Month = 12, cycle = cycle) %>%
    dplyr::ungroup()

  complete <- hop$plot %>%
    dplyr::filter(SimulationName == "Monocrop") %>%
    dplyr::group_by(SimulationName, Year) %>%
    dplyr::summarize(n = dplyr::n() >= 365)


  out.data  <- plot.data %>%
    dplyr::left_join(complete, by = "Year") %>%
    dplyr::filter(n) %>%
    dplyr::select(SimulationName.x, Year, flux, value, Month, cycle)

  out.data <- out.data %>%
    dplyr::rename(SimulationName = SimulationName.x)

  return(out.data)
}

#' Get yields from a hop object
#' @description Gets yields from a hop object.
#' Used within hisafe cycle functions.
#' @return A tibble with extracted yields.
#' @param hop An object of class hop or face.
#' @param profile An character string indicating from which profile to pull crop yield data. Either "yield" or "plot".

#' @importFrom dplyr %>%
#' @keywords internal
get_yields_ler <- function(hop, profile) {

  library(dplyr)
  profile_check(hop, c(profile, "trees", "plot.info"), error = TRUE)
  variable_check(hop, "trees", "stemYield", error = TRUE)
  variable_check(hop, "yield", "grainBiomass", error = TRUE)

  if(profile == "yield") {
    variable_check(hop, "yield", c("idZone", "grainBiomass"), error = TRUE)

    zone.rel.area <- hop$annualCells %>%
      dplyr::filter(Date == min(Date)) %>%
      dplyr::group_by(SimulationName, idZone) %>%
      dplyr::summarize(n = dplyr::n()) %>%
      dplyr::mutate(perc = n / sum(n)) %>%
      dplyr::select(-n)

    cellnew <- hop$yield %>%
      replace(is.na(.), 0) %>%
      dplyr::filter(cropSpeciesName != "weed") %>%
      dplyr::select(SimulationName, Year, idZone, grainBiomass) %>%
      dplyr::group_by(SimulationName, Year, idZone) %>%
      dplyr::summarize_all(mean) %>% # mean of all yield in scene
      dplyr::ungroup() %>%
      dplyr::group_by(SimulationName) %>%
      dplyr::arrange(SimulationName, Year, idZone) %>%
      dplyr::rename(yield = grainBiomass) %>%
      dplyr::ungroup()

    cellnew2 <- cellnew %>%
      dplyr::left_join(zone.rel.area, by = c("SimulationName", "idZone")) %>%
      dplyr::mutate(yield = yield * perc) %>% # convert from cell basis to scene basis
      dplyr::mutate(yield = yield * 1000) %>% # convert t/ha to kg/ha
      dplyr::ungroup()

    cellnew3 <- cellnew2 %>%
      dplyr::select(SimulationName, Year,  yield) %>%
      dplyr::ungroup()

    cells <- cellnew3 %>%
      group_by(SimulationName, Year) %>%
      summarise(yield = sum(yield, na.rm = TRUE), .groups = "drop")

    treenew <- hop$trees %>%
      dplyr::left_join(hop$plot.info, by = "SimulationName") %>%
      replace(is.na(.), 0) %>%
      dplyr::select(SimulationName, Year,  Month, Day, Date, JulianDay, stemYield) %>%
      dplyr::group_by(SimulationName, Year, Month, Day, Date, JulianDay) %>%
      dplyr::summarize_all(sum) %>% # sum of all trees in scene
      dplyr::ungroup() %>%
      dplyr::group_by(SimulationName) %>%
      dplyr::arrange(SimulationName, Year, JulianDay) %>%
      dplyr::mutate(stemYield = c(NA, pmax(diff(stemYield), 0)))  # convert to yield increment

    trees <- treenew %>%
      group_by(SimulationName, Year) %>%
      summarise(stemYield = sum(stemYield, na.rm = TRUE), .groups = "drop")


    out <- trees %>%
      dplyr::full_join(cells, by = c("SimulationName", "Year")) %>%
      replace(is.na(.), 0)

    out <- out %>%
      tidyr::gather(key = "flux", value = "value", stemYield, yield) %>%
      dplyr::mutate(flux = factor(flux,
                                  levels = c("stemYield", "yield"),
                                  labels = c("Trees", "Crops")))

  } else {
    stop("Plotting nitrogen cycle using plot profile is currently not supported. Export yield profile.", call. = FALSE)
  }


  return(out)
}
#' Plot LER for yield, light, water, or nitrogen
#' @description Plot LER for yield, light, water, or nitrogen.
#' @return Invisibly retruns an egg object.
#' @param face An object of class "face".
#' @param metrics A character vector indicating which LER metrics to include.
#' Supported metrics include: 'yield', 'light', 'water', and 'nitrogen.
#' @param fixed.scale A logical indicating wether the y-axes of all panels should have have the same scale.
#' @param nrow A numeric value of the number of rows in which to arrange plots.
#' @param output.path A character string indicating the path to the directory where plots should be saved.
#' If \code{NULL}, the experiment/simulation path is read from the hop object, and a directory is created there called "analysis".
#' The plot wil be saved in this directory as "LER_summary.jpg".
#' @param plot.labels A character vector of labels to label upper-right corner of plot panels.
#' If \code{NULL}, no labels will be added.
#' @param legend.inside A logical indicating whether the legend should be placed inside the plots.
#' @param ... Other arguments passed to \code{\link{LER}}.
#' @export
#' @import ggplot2
#' @family hisafe analysis functions
#' @examples
#' \dontrun{
#' ler.plot <- summary_LER(face, "./")
#' }
LER_summary <- function(face,
                        metrics       = c("yield", "light", "water", "nitrogen"),
                        fixed.scale   = TRUE,
                        nrow          = 4,
                        output.path   = NULL,
                        plot.labels   = NULL,
                        legend.inside = TRUE, ...)  {

  is_TF(fixed.scale)
  if(!is.numeric(nrow))                                   stop("nrow argument must be numieric",                  call. = FALSE)
  if(!(is.character(output.path) | is.null(output.path))) stop("output.path argument must be a character string", call. = FALSE)
  if(!(is.character(plot.labels) | is.null(plot.labels))) stop("output.path argument must be a character string", call. = FALSE)

  if(!requireNamespace(c("gtable", "egg"), quietly = TRUE)) stop("The packages 'gtable' and 'egg' are required for summary_LER().
                                                                 Please install and load them", call. = FALSE)

  supported.metrics <- c("yield", "light", "water", "nitrogen")
  desired.metrics   <- (supported.metrics %in% metrics)

  if(fixed.scale) {
    yield.data    <- LER(face = face, cycle = "yield",    plot = FALSE, ...)
    light.data    <- LER(face = face, cycle = "light",    plot = FALSE, ...)
    water.data    <- LER(face = face, cycle = "water",    plot = FALSE, ...)
    nitrogen.data <- LER(face = face, cycle = "nitrogen", plot = FALSE, ...)
    data <- dplyr::bind_rows(list(yield.data, light.data, water.data, nitrogen.data)[desired.metrics])
    YMAX <- max(1, max(data$value) + 0.01)
  } else YMAX <- NA

  yield.plot <- LER(face = face, cycle = "yield", ymax = YMAX, ...) +
    labs(title = NULL, y = "Relative yield")

  light.plot <- LER(face = face, cycle = "light", ymax = YMAX, ...) +
    labs(title = NULL, y = "Relative light capture")

  water.plot <- LER(face = face, cycle = "water", ymax = YMAX, ...) +
    labs(title = NULL, y = "Relative water uptake")

  nitrogen.plot <- LER(face = face, cycle = "nitrogen", ymax = YMAX, ...) +
    labs(title = NULL, y = "Relative nitrogen uptake")

  ## MERGE & ADD LEGEND
  plot.list <- list(yield.plot, light.plot, water.plot, nitrogen.plot)[desired.metrics]

  n.plots <- length(plot.list)
  for(i in 1:n.plots) {
    if(n.plots > 1 & i < n.plots) {
      plot.list[[i]] <- plot.list[[i]] +
        theme(axis.text.x  = element_blank(),
              axis.title.x = element_blank())
    }

    if(legend.inside) {
      if(i == 1) {
        plot.list[[i]] <- plot.list[[i]] +
          guides(color = FALSE, size = FALSE) +
          theme(legend.position   = c(0.4, 0.9),
                legend.background = element_rect(fill = "transparent"))
      } else if(i == min(n.plots, 2)) {
        plot.list[[i]] <- plot.list[[i]] +
          guides(linetype = FALSE) +
          theme(legend.position   = c(0.35, 0.85),
                legend.background = element_rect(fill = "transparent"))
      } else {
        plot.list[[i]] <- plot.list[[i]] +
          theme(legend.position = "none")
      }
    } else {
      if(i == n.plots) {
        plot.list[[i]] <- plot.list[[i]] +
          theme(legend.position  = "bottom",
                legend.direction = "vertical")
      } else {
        plot.list[[i]] <- plot.list[[i]] +
          theme(legend.position   = "none")
      }
    }
  }

  ## PLOT
  if(!is.null(plot.labels)) plot.list <- annotator(plot.list, labels = plot.labels)

  ler.plot <- egg::ggarrange(plots = plot.list, nrow = min(nrow, n.plots), draw = FALSE)

  output.path <- clean_path(paste0(diag_output_path(hop = hop, output.path = output.path), "/"))
  dir.create(output.path, recursive = TRUE, showWarnings = FALSE)

  ggsave(filename = paste0(clean_path(output.path), "LER_summary.jpg"),
         plot     = ler.plot,
         scale    = 1.5,
         width    = 3.5 * ceiling(n.plots / min(nrow, n.plots)),
         height   = 2.25 * min(nrow, n.plots) + 1 * as.numeric(!legend.inside))

  invisible(ler.plot)
}
