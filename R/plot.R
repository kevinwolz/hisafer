#' Plot timeseries of Hi-sAFe output variable
#' @description Plots a daily or annual timeseries of a single Hi-sAFe output variable.
#' @return Returns a ggplot object. If the data is of class \code{hop-group} and contains data from more than one
#' Hi-sAFe simulation, the plot will contain multiple lines, colored and labeled by SimulationName. If the data
#' contains two more tree ids, the plot will be faceted by tree id.
#' @param hop An object of class \code{hop} or \code{hop-group} containing output data from one or more Hi-sAFe simulations.
#' @param variable A character string of the name of the variable to plot.
#' @param profile The profile for which to plot a timeseries. If 'annualtree' or 'annualplot', annual timeseries are created.
#' If 'trees', 'plot', or 'climate', daily timeseries are created.
#' @param time.lim If profile is an annual profile, a numeric vector of length two providing the \code{c(minimum, maximum)} of years (since planting) to plot.
#' If profile is daily profile, a character vector of length two providing the \code{c(minimum, maximum)} dates ('yyyy-mm-dd') to plot.
#' If no input, the full available time range is plotted. Use \code{NA} to refer to the start or end of the simulation.
#' @param tree.id A numeric vector indicating the ids of a subset of tree ids to plot. If no input, all trees will be plotted.
#' @param color.palette A character stirng of hex values or R standard color names defining the color palette to use in plots with multiple simulations.
#' If \code{NULL}, the default, then the default color palette is a color-blind-friendly color palette. The default supports up to 24 simulations.
#' @param linetype.palette A character stirng of values defining the linetype palette to use in plots with multiple simulations.
#' If \code{NULL}, the default, then solid lines are used for all simulations. The default supports up to 24 simulations.
#' @param aes.cols A list with arguments "color" and "linetype" containing character stirngs of the column names to use for plot aesthetics.
#' If \code{NULL}, the default, then SimulationName is used for both aesthetics.
#' @export
#' @importFrom dplyr %>%
#' @import ggplot2
#' @family hisafe plot functions
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' mydata <- read_hisafe_output("MySimulation", "./")
#'
#' # You can create an annual timeseries of carbonCoarseRoots:
#' annual.plot <- plot_hisafe_ts(mydata, "carbonCoarseRoots")
#'
#' # For a daily timeseries instead:
#' daily.plot <- plot_hisafe_ts(mydata, "carbonCoarseRoots", "daily")
#'
#' # Once you have the plot object, you can display it and save it:
#' annual.plot
#' ggplot2::ggsave("annual_carbonCoarseRoots.png", annual.plot)
#' }
plot_hisafe_ts <- function(hop,
                           variable,
                           profile,
                           time.lim         = NULL,
                           tree.id          = NULL,
                           color.palette    = NULL,
                           linetype.palette = NULL,
                           aes.cols         = list(color = NULL, linetype = NULL)) {

  annual.profiles <- c("annualtree", "annualplot")
  daily.profiles  <- c("trees", "plot", "climate")

  ## Check for data class and if profile exists
  if(!any(c("hop", "hop-group") %in% class(hop)))        stop("hop argument not of class hop", call. = FALSE)
  if(!(profile %in% c(annual.profiles, daily.profiles))) stop("supplied profile is not supported", call. = FALSE)
  if(nrow(hop[[profile]]) == 0)                          stop(paste("no data from", profile, "profile found"), call. = FALSE)

  ## Exract units of supplied variable from the "variables" slot
  var.unit <- hop$variables %>%
    dplyr::filter(VariableClass == profile, VariableName == variable) %>%
    .$Units %>%
    gsub(pattern = "\\.", replacement = " ")

  ## Create profile-specific x aesthetic, axis label, plot theme, and time.lim filter
  if(profile %in% annual.profiles){
    x.var      <- "Year"
    x.label    <- "Years after establishment"
    plot.data  <- hop[[profile]] %>%
      dplyr::mutate(Year = Year - min(Year) + 1) # Create 0+ year values
    scale_x_ts <- scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL))
    if(!is.null(time.lim)) {
      if(is.na(time.lim[1])) { time.lim[1] <- min(plot.data$Year) }
      if(is.na(time.lim[2])) { time.lim[2] <- max(plot.data$Year) }
      plot.data <- dplyr::filter(plot.data, Year %in% (time.lim[1]:time.lim[2]))
    }
  } else {
    x.var      <- "Date"
    x.label    <- "Date"
    plot.data  <- hop[[profile]]
    scale_x_ts <- scale_x_date()
    if(!is.null(time.lim)) {
      time.lim <- lubridate::ymd(time.lim)
      if(is.na(time.lim[1])) { time.lim[1] <- min(plot.data$Date) }
      if(is.na(time.lim[2])) { time.lim[2] <- max(plot.data$Date) }
      plot.data <- dplyr::filter(plot.data, Date >= time.lim[1], Date <= time.lim[2])
    }
  }

  ## Check for existence of variable within hop profile
  if(!(variable %in% names(plot.data))) stop(paste0(variable, " does not exist within ", profile, " profile.",
                                                    "\nCheck spelling and capitalization of variable name.",
                                                    "\nAlso check to ensure that this variable was included within the output profile definition."),
                                             call. = FALSE)

  ## Filter by supplied tree.id
  if(!is.null(tree.id)) {
    plot.data <- plot.data %>% dplyr::filter(id %in% tree.id)
  }

  ## If number of trees in scene is > 1, then facet by tree id
  facet_tree <- geom_blank()
  if(profile %in% c("annualtree", "trees")) {
    if(length(unique(plot.data$id)) > 1) {
      plot.data <- plot.data %>% dplyr::mutate(id = paste("Tree", id))
      facet_tree <- facet_wrap(~id, nrow = 1)
    }
  }

  ## Create plot
  if(is.null(color.palette)) {
    cbPalette <- rep(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), 3) # Color blind-friendly palette
    color.palette <- cbPalette
  }
  if(is.null(linetype.palette)) {
    linetype.palette <- rep(c("solid", "dashed", "dotted"), each = 8)
  }

  if(is.null(aes.cols$color) & is.null(aes.cols$linetype)) {
    plot.starter <- ggplot(plot.data, aes_string(x        = x.var,
                                                 y        = variable,
                                                 color    = "SimulationName",
                                                 linetype = "SimulationName")) +
      labs(x        = x.label,
           y        = paste0(variable, " (", var.unit, ")"),
           title    = variable,
           color    = "",
           linetype = "") +
      scale_linetype_manual(values = linetype.palette)
  } else {
    if("color"    %in% names(aes.cols)) plot.data[[aes.cols$color]]    <- factor(plot.data[[aes.cols$color]])
    if("linetype" %in% names(aes.cols)) plot.data[[aes.cols$linetype]] <- factor(plot.data[[aes.cols$linetype]])
    plot.starter <- ggplot(plot.data, aes_string(x        = x.var,
                                                 y        = variable,
                                                 color    = aes.cols$color,
                                                 linetype = aes.cols$linetype,
                                                 group    = "SimulationName")) +
      labs(x        = x.label,
           y        = paste0(variable, " (", var.unit, ")"),
           title    = variable,
           color    = aes.cols$color,
           linetype = aes.cols$linetype)
  }

  plot.obj <- plot.starter +
    facet_tree +
    scale_x_ts +
    scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
    geom_line(size = 1, na.rm = TRUE) +
    scale_color_manual(values = color.palette) +
    theme_hisafe_ts()

  return(plot.obj)
}

#' Tile plot of Hi-sAFe monthCells output variable
#' @description Plots a tile plot of a single Hi-sAFe monthCells output variable.
#' If the hop objectcontains the "trees" profile with crown diameter variables, the tree locations and crown outlines are also drawn.
#' @details This function is very picky! You can only facet by two of the three manipulable variables: SimulationName, Year, Month.
#' You must ensure that the one varibale not used for faceting is fixed at a single value.
#' @return Returns a ggplot object.
#' @param hop An object of class \code{hop} or \code{hop-group} containing output data from one or more Hi-sAFe simulations.
#' @param variable A character string of the name of the variable to color the tiles.
#' @param rowfacet One of "Year", "Month", or "SimulationName", indicating which variable to use for row faceting.
#' @param colfacet One of "Year", "Month", or "SimulationName", indicating which variable to use for column faceting.
#' @param sim.names A character string containing the SimulationNames to include. Use "all" to include all available values.
#' @param years A numeric vector containing the years to include. Use "all" to include all available values.
#' @param months A numeric vector containing the months to include. Use "all" to include all available values.
#' @export
#' @importFrom dplyr %>%
#' @import ggplot2
#' @family hisafe plot functions
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' mydata <- read_hisafe_output("MySimulation", "./")
#'
#' # You can create a tile plot of monthDirectParIncident:
#' tile.plot <- plot_hisafe_monthCells(mydata, "monthDirectParIncident")
#'
#' # The default settings use data from June and facet by Year and Simulation Name.
#' # If you instead want to just look at Year 20 and facet by Month and SimulationName:
#' tile.plot2 <- plot_hisafe_monthCells(mydata, "monthDirectParIncident",
#'                                      rowfacet = "SimulationName",
#'                                      colfacet = "Month",
#'                                      sim.names = "all",
#'                                      years = 20,
#'                                      months = 1:12)
#'
#' # Once you have the plot object, you can display it and save it:
#' tile.plot2
#' ggplot2::ggsave("tiled_monthDirectParIncident.png", tile.plot2)
#' }
plot_hisafe_monthcells <- function(hop,
                                   variable,
                                   colfacet  = "SimulationName",
                                   rowfacet  = "Year",
                                   sim.names = "all",
                                   years     = seq(0, 40, 5),
                                   months    = 6) {

  ## Check for data class and if profile exists
  if(!any(c("hop", "hop-group") %in% class(hop))) stop("hop argument not of class hop", call. = FALSE)
  if(nrow(hop$monthCells) == 0)                   stop("no data from monthCells profile found", call. = FALSE)

  ## Convert "all" arguements to actual values
  if(sim.names[1] == "all") sim.names <- unique(hop$monthCells$SimulationName)
  if(years[1]     == "all") years     <- unique(hop$monthCells$Year) -  min(hop$monthCells$Year)
  if(months[1]    == "all") months    <- unique(hop$monthCells$Month)

  ## Determine which variable is not part of faceting & trigger associated error
  vars <- c("SimulationName", "Year", "Month")
  avail.vars <- list(sim.names, years, months)
  var.lengths <- purrr::map_int(avail.vars, length) > 1
  if(sum(var.lengths) > 2) stop("only two variables of (sim.names, years, months) can have length greater than one", call. = FALSE)
  fixed <- which(!(vars %in% c(colfacet, rowfacet)))
  fixed.var <- paste(vars[fixed], "=", avail.vars[[fixed]])

  ## Exract units of supplied variable from the "variables" slot
  var.unit <- hop$variables %>%
    dplyr::filter(VariableClass == "monthCells", VariableName == variable) %>%
    .$Units %>%
    gsub(pattern = "\\.", replacement = " ")

  ## Filter for provided sim.names, years & months
  #cellWidth <- max(diff(hop$monthCells$x))
  plot.data <- hop$monthCells %>%
    dplyr::mutate(Year = Year - min(Year) + 1) %>% # Create 0+ year values
    dplyr::filter(SimulationName %in% sim.names) %>%
    dplyr::filter(Year %in% years) %>%
    dplyr::filter(Month %in% months)

  xy.centers <- plot.data %>%
    dplyr::group_by(SimulationName) %>%
    dplyr::summarize(x.center = median(x), y.center = median(y)) #+ cellWidth/2

  ## Filter for sim.names & combine with canopy diameter
  n.trees <- nrow(dplyr::filter(hop$tree.info, SimulationName %in% sim.names))
  if(n.trees > 0 & nrow(hop$trees) > 0) {
    tree.data <- hop$tree.info %>%
      dplyr::filter(SimulationName %in% sim.names)

    diam.data <- hop$trees %>%
      dplyr::mutate(Year = Year - min(Year) + 1) %>% # Create 0+ year values
      dplyr::filter(SimulationName %in% sim.names) %>%
      dplyr::filter(Year %in% years) %>%
      dplyr::filter(Month %in% months) %>%
      dplyr::filter(Day == 1) %>%
      dplyr::select(SimulationName, Year, Month, id, crownRadiusInterRow, crownRadiusTreeLine) %>%
      dplyr::left_join(tree.data, by = c("SimulationName", "id")) %>%
      dplyr::left_join(xy.centers, by = "SimulationName") %>%
      dplyr::mutate(x = x + x.center, y = y + y.center) %>%
      dplyr::select(-x.center, -y.center)
  } else {
    years <- years[years %in% (unique(hop$monthCells$Year) -  min(hop$monthCells$Year))]
    years <- years[years != 0]
    diam.data <- dplyr::as_tibble(expand.grid(SimulationName = sim.names, Year = years, Month = months, id = NA,
                                              crownRadiusInterRow = NA, crownRadiusTreeLine = NA, species = NA, x = NA, y = NA)) %>%
      dplyr::mutate_if(is.logical, as.numeric)
  }

  ## Check for existence of variable within hop profile
  if(!(variable %in% names(plot.data))) stop(paste0(variable, " does not exist within monthCells profile.",
                                                    "\nCheck spelling and capitalization of variable name.",
                                                    "\nAlso check to ensure that this variable was included within the output profile definition."),
                                             call. = FALSE)

  ## Create plot
  plot.obj <- ggplot(plot.data, aes(x = x, y = y)) +
    labs(x = colfacet,
         y = rowfacet,
         fill = var.unit,
         title = paste0(variable, " (", fixed.var, ")")) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    facet_grid(reformulate(colfacet, rowfacet), switch = "both") +
    geom_tile(aes_string(fill = variable), na.rm = TRUE, color = "black") +
    geom_point(data = diam.data,
               color = "green",
               size = 2,
               na.rm = TRUE) +
    ggforce::geom_ellipsis(data = diam.data,
                           color = "green",
                           size = 2,
                           aes(x0 = x, y0 = y,
                               a = crownRadiusInterRow,
                               b = crownRadiusTreeLine,
                               angle = 0),
                           inherit.aes = FALSE,
                           na.rm = TRUE) +
    viridis::scale_fill_viridis(option = "magma") +
    guides(fill = guide_colourbar(barwidth = 15,
                                  barheight = 1.5,
                                  title.vjust = 0.8,
                                  nbin = 100)) +
    coord_equal() +
    theme_hisafe_tile()

  return(plot.obj)
}


#' Tile plot of Hi-sAFe cells output variable
#' @description Plots a tile plot of a single Hi-sAFe cells output variable.
#' SimulationName is used as the column facet. Date is used as the row facet.
#' @return Returns a ggplot object.
#' @param hop An object of class \code{hop} or \code{hop-group} containing output data from one or more Hi-sAFe simulations.
#' @param variable A character string of the name of the variable to color the tiles.
#' @param dates A character vector containing the dates (yyyy-mm-dd) to include.
#' @export
#' @importFrom dplyr %>%
#' @import ggplot2
#' @family hisafe plot functions
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' mydata <- read_hisafe_output("MySimulation", "./")
#'
#' # You can create a tile plot of relativeDirectParIncident:
#' tile.plot <- plot_hisafe_cells(mydata, "relativeDirectParIncident", paste(1998, 6:8, 1, sep = "-"))
#'
#' # Once you have the plot object, you can display it and save it:
#' tile.plot
#' ggplot2::ggsave("tiled_relativeDirectParIncident.png", tile.plot)
#' }
plot_hisafe_cells <- function(hop, variable, dates) {

  ## Check for data class and if profile exists
  if(!any(c("hop", "hop-group") %in% class(hop))) stop("hop argument not of class hop", call. = FALSE)
  if(nrow(hop$cells) == 0)                        stop("no data from cells profile found", call. = FALSE)

  ## Exract units of supplied variable from the "variables" slot
  var.unit <- hop$variables %>%
    dplyr::filter(VariableClass == "cells", VariableName == variable) %>%
    .$Units %>%
    gsub(pattern = "\\.", replacement = " ")

  ## Filter for provided dates
  plot.data <- hop$cells %>%
    dplyr::filter(Date %in% lubridate::ymd(dates))

  ## Check for existence of variable within hop profile
  if(!(variable %in% names(plot.data))) stop(paste0(variable, " does not exist within cells profile.",
                                                    "\nCheck spelling and capitalization of variable name.",
                                                    "\nAlso check to ensure that this variable was included within the output profile definition."),
                                             call. = FALSE)

  ## Find tree locations for each simulation
  tree.locations <- plot.data %>%
    dplyr::summarize(x = median(x), y = median(y))

  ## Determine faceting & axis labels
  if("hop-group" %in% class(hop) & length(dates) > 1){
    facet_cells <- facet_grid(Date ~ SimulationName, switch = "both")
    x.lab       <- "SimulationName"
    y.lab       <- "Date"
    title.lab   <- variable
  } else if(length(dates) > 1) {
    facet_cells <- facet_wrap(~Date)
    x.lab       <- ""
    y.lab       <- "Date"
    title.lab   <- variable
  } else {
    facet_cells <- geom_blank()
    x.lab       <- ""
    y.lab       <- ""
    title.lab   <- paste0(variable, " (", lubridate::ymd(dates), ")")
  }

  ## Create plot
  plot.obj <- ggplot(plot.data, aes(x = x, y = y)) +
    labs(x = x.lab,
         y = y.lab,
         fill = var.unit,
         title = title.lab) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    facet_cells +
    geom_tile(aes_string(fill = variable), na.rm = TRUE, color = "black") +
    geom_point(data = tree.locations, fill = "black", color = "white", shape = 21) +
    viridis::scale_fill_viridis(option = "magma") +
    guides(fill = guide_colourbar(barwidth = 15,
                                  barheight = 1.5,
                                  title.vjust = 0.8,
                                  nbin = 100)) +
    coord_equal() +
    theme_hisafe_tile()

  return(plot.obj)
}


#' Tile plot of Hi-sAFe voxels output variable
#' @description Plots a tile plot of a single Hi-sAFe voxels output variable.
#' If a single date is provided, SimulationName is used as the column facet. Otherwise, Date is used as the column facet.
#' @return Returns ggplot object.
#' @param hop An object of class \code{hop} or \code{hop-group} containing output data from one or more Hi-sAFe simulations.
#' @param variable A character string of the name of the variable to color the tiles.
#' @param dates A character vector containing the dates (yyyy-mm-dd) to include.
#' @export
#' @importFrom dplyr %>%
#' @import ggplot2
#' @family hisafe plot functions
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' mydata <- read_hisafe_output("MySimulation", "./")
#'
#' # You can create a tile plot of waterAvailable:
#' tile.plot <- plot_hisafe_voxels(mydata, "waterAvailable", paste(1998, 6:8, 1, sep = "-"))
#'
#' # Once you have the plot object, you can display it and save it:
#' tile.plot
#' ggplot2::ggsave("tiled_waterAvailable.png", tile.plot)
#' }
plot_hisafe_voxels <- function(hop, variable, dates) {

  ## Check for data class and if profile exists
  if(!any(c("hop", "hop-group") %in% class(hop))) stop("hop argument not of class hop", call. = FALSE)
  if(nrow(hop$voxels) == 0)                       stop("no data from voxels profile found", call. = FALSE)

  if("hop-group" %in% class(hop) & length(dates) > 1) stop("cannot supply more than one date for object of class hop-group", call. = FALSE)

  ## Exract units of supplied variable from the "variables" slot
  var.unit <- hop$variables %>%
    dplyr::filter(VariableClass == "voxels", VariableName == variable) %>%
    .$Units %>%
    gsub(pattern = "\\.", replacement = " ")

  ## Filter for provided dates
  plot.data <- hop$voxels %>%
    dplyr::filter(Date %in% lubridate::ymd(dates))

  ## Check for existence of variable within hop profile
  if(!(variable %in% names(plot.data))) stop(paste0(variable, " does not exist within voxels profile.",
                                                    "\nCheck spelling and capitalization of variable name.",
                                                    "\nAlso check to ensure that this variable was included within the output profile definition."),
                                             call. = FALSE)

  ## Find tree locations for each simulation
  tree.locations <- plot.data %>%
    dplyr::summarize(x = median(x), y = median(y))

  ## Determine faceting & axis labels
  if("hop-group" %in% class(hop) & length(dates) == 1){
    facet_voxels <- facet_grid(z ~ SimulationName, switch = "both")
    x.lab        <- "SimulationName"
    title.lab    <- paste0(variable, " (", lubridate::ymd(dates), ")")
  } else if(length(dates) == 1) {
    facet_voxels <- facet_wrap(~z, ncol = 1)
    x.lab        <- ""
    title.lab    <- paste0(variable, " (", lubridate::ymd(dates), ")")
  } else {
    facet_voxels <- facet_grid(z ~ Date, switch = "both")
    x.lab        <- "Date"
    title.lab    <- variable
  }


  ## Create plot
  plot.obj <- ggplot(plot.data, aes(x = x, y = y)) +
    labs(x = x.lab,
         y = "Depth (m)",
         fill = var.unit,
         title = title.lab) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    facet_voxels +
    geom_tile(aes_string(fill = variable), na.rm = TRUE, color = "black") +
    geom_point(data = tree.locations, fill = "black", color = "white", shape = 21) +
    viridis::scale_fill_viridis(option = "magma") +
    guides(fill = guide_colourbar(barwidth = 15,
                                  barheight = 1.5,
                                  title.vjust = 0.8,
                                  nbin = 100)) +
    coord_equal() +
    theme_hisafe_tile()

  return(plot.obj)
}
