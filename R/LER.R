#' Plot LER for yield, light, water, or nitrogen
#' @description Plot LER for yield, light, water, or nitrogen.
#' @return If \code{plot = TRUE}, returns a ggplot object, otherwise the data that would create the plot is returned.
#' @param face An object of class "face".
#' @param cycle One of "yield", "nitrogen", "water", or "light".
#' @param timescales One or both of "Annual" and "Cumulative", indicating which timescales to plot.
#' @param components One or more of "LER", "Trees", and "Crop, indicating which components to plot.
#' @param color.palette A character string of hex values or R standard color names defining the color palette to use in plots with multiple simulations.
#' @param plot If \code{TRUE}, the default, a ggplot object is returned. If \code{FALSE}, the data that would create the plot is returned.
#' @param ... Other arguments passed to \code{\link{plot_hisafe_cycle_annual}}.
#' @export
#' @examples
#' \dontrun{
#' ler.plot <- LER(hop, "yield")
#' }
LER <- function(face,
                cycle         = "yield",
                timescales    = c("Annual", "Cumulative"),
                components    = c("LER", "Trees", "Crop"),
                color.palette = c("black", "#E69F00", "#009E73"),
                plot          = TRUE, ...) {

  supported.cycles     <- c("yield", "nitrogen", "water", "light")
  supported.timescales <- c("Annual", "Cumulative")
  supported.components <- c("LER", "Trees", "Crop")

  if(!("face" %in% class(face))) stop("face argument not of class face",           call. = FALSE)
  if(length(color.palette) != 3) stop("color.palette argument must have length 3", call. = FALSE)
  if(!(cycle %in% supported.cycles))             stop(paste("cycle argument must be one of:",
                                                            paste(supported.cycles, collapse = ", ")),     call. = FALSE)
  if(!all(timescales %in% supported.timescales)) stop(paste("timescales argument must be one or more of:",
                                                            paste(supported.timescales, collapse = ", ")), call. = FALSE)
  if(!all(components %in% supported.components)) stop(paste("components argument must be one or more of:",
                                                            paste(supported.components, collapse = ", ")), call. = FALSE)
  is_TF(plot)

  ## Get flux data
  cycle.data <- plot_hisafe_cycle_annual(hop        = face,
                                         cycle      = cycle,
                                         plot       = FALSE,
                                         crop.names = c("Crop", "NA"), ...) %>%
    dplyr::ungroup()

  crop.descrip <- c("Crop",          # yield
                    "Uptake - Crop", # nitrogen
                    "Uptake - Crop", # water
                    "Crop")          # light

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
    dplyr::mutate(flux = "crop.mc") %>%
    tidyr::spread(key = "flux", value = "value") %>%
    dplyr::select(-SimulationName)

  af <- cycle.data %>%
    dplyr::filter(SimulationName == "Agroforestry") %>%
    dplyr::filter(flux %in% c(tree.id, crop.id)) %>%
    dplyr::mutate(flux = factor(flux, levels = c(tree.id, crop.id), labels = c("trees.af", "crop.af"))) %>%
    tidyr::spread(key = "flux", value = "value") %>%
    dplyr::left_join(pf, by = c("Year", "cycle")) %>%
    dplyr::left_join(mc, by = c("Year", "cycle")) %>%
    dplyr::arrange(SimulationName, Year) %>%
    dplyr::group_by(SimulationName)

  ## CALCULATE LER
  calculate_ler <- function(x) {
    out <- x %>%
      dplyr::mutate(ry.trees = trees.af / trees.pf) %>%
      dplyr::mutate(ry.crop  = crop.af  / crop.mc) %>%
      dplyr::mutate(ler = ry.trees + ry.crop)
  }

  cum.ler <- af %>%
    dplyr::mutate_at(c("trees.af", "crop.af", "trees.pf", "crop.mc"), cumsum) %>%
    calculate_ler() %>%
    dplyr::mutate(timescale = "Cumulative") %>%
    dplyr::select(SimulationName, Year, cycle, timescale, dplyr::everything())

  ler <- af %>%
    calculate_ler() %>%
    dplyr::mutate(timescale = "Annual") %>%
    dplyr::select(SimulationName, Year, cycle, timescale, dplyr::everything()) %>%
    dplyr::bind_rows(cum.ler)


  plot.data <- ler %>%
    tidyr::gather(ry.trees, ry.crop, ler, key = "metric", value = "value") %>%
    dplyr::mutate(metric = factor(metric, levels = c("ler", "ry.trees", "ry.crop"), labels = c("LER", "Trees", "Crop"))) %>%
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
    geom_hline(yintercept = 1, linetype = "dotted") +
    geom_line() +
    scale_color_manual(values = color.palette) +
    scale_size_manual(values  = c(2, 1, 1)) +
    theme_hisafe_ts() +
    theme(panel.grid.major = element_blank())

  if(length(timescales) == 1) plot.obj <- plot.obj + guides(linetype = FALSE)
  if(length(components) == 1) plot.obj <- plot.obj + guides(color    = FALSE, size = FALSE)

  if(plot) return(plot.obj) else return(ler)
}
