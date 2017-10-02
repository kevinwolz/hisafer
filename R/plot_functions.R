#' Plot timeseries of Hi-sAFe output variable
#' @description Plots a daily or annual timeseries of a single Hi-sAFe output variable.
#' @return A ggplot object. If the data is of class \code{hop-group} and contains data from more than one
#' Hi-sAFe simulation, the plot will contain multiple lines, colored and labeled by SimulationName. If the data
#' contains two more tree ids, the plot will be faceted by tree id.
#' @param data An object of class \code{hop} or \code{hop-group} containing output data from one or more Hi-sAFe simulations.
#' @param variable A character string of the name of the variable to plot.
#' @param time.class If 'annual', the default, an annual timeseries is created. If 'daily', a daily timeseries is created.
#' @param time.lim If time.class is 'annual', the default, a numeric vector of length two providing the \code{c(minimum, maximum)} of years (since planting) to plot.
#' If time.class is 'daily', a character vector of length two providing the \code{c(minimum, maximum)} dates ('yyyy-mm-dd') to plot.
#' If no input, the full available time range is plotted. Use \code{NA} to refer to the start or end of the simulation.
#' @param tree.id A numeric vector indicating the ids of a subset of tree ids to plot. If no input, all trees will be plotted.
#' @export
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
plot_hisafe_ts <- function(data,
                           variable,
                           time.class = "annual",
                           time.lim = NULL,
                           tree.id = NULL) {
  time.class <- tolower(time.class) # prevents error if improper capitalization not input by user

  ## Check if data has class hisafe or hisafe-group
  if(!any(c("hop", "hop-group") %in% class(data))) {
    stop("data not of class hop or hop-group")
  }

  ## Color blind-friendly palette
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  ## Exract units of supplied variable from the "variables" slot
  var.unit <- data$variables %>%
    filter(VariableClass == time.class, VariableName == variable) %>%
    .$Units %>%
    stringr::str_replace("\\.", " ")

  ## Create time.class-specific x aesthetic, axis label, plot theme, and time.lim filter
  if(time.class == "annual"){
    x.var <- "Year"
    x.label <- "Years after establishment"
    plot.data <- data$annual
    plot.data <- plot.data %>% mutate(Year = Year - min(Year) + 1) # Create 0+ year values
    scale_x_ts <- scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL))
    if(!is.null(time.lim)) {
      if(is.na(time.lim[1])) { time.lim[1] <- min(plot.data$Year) }
      if(is.na(time.lim[2])) { time.lim[2] <- max(plot.data$Year) }
      plot.data <- plot.data %>% filter(Year %in% (time.lim[1]:time.lim[2]))
    }
  } else {
    x.var <- "Date"
    x.label <- "Date"
    plot.data <- data$daily
    scale_x_ts <- scale_x_date()
    if(!is.null(time.lim)) {
      time.lim <- ymd(time.lim)
      if(is.na(time.lim[1])) { time.lim[1] <- min(plot.data$Date) }
      if(is.na(time.lim[2])) { time.lim[2] <- max(plot.data$Date) }
      plot.data <- plot.data %>% filter(Date >= time.lim[1], Date <= time.lim[2])
    }
  }

  ## Filter by supplied tree.id
  if(!is.null(tree.id)) {
    plot.data <- plot.data %>% filter(id %in% tree.id)
  }

  ## If number of trees in scene is > 1, then facet by tree id
  if(length(unique(plot.data$id)) == 1) {
    facet_annual <- geom_blank()
  } else {
    plot.data <- plot.data %>% mutate(id = paste("Tree", id))
    facet_annual <- facet_wrap(~id, nrow = 1)
  }

  ## Pad SimulationName for legend clarity (until bug in legend.text response to margin is fixed)
  if(class(data) == "hop-group") {
    levels(plot.data$SimulationName) <- paste0(levels(plot.data$SimulationName), "  ")
  }

  ## Create plot
  plot.obj <- ggplot(plot.data, aes_string(x = x.var, y = variable, color = "SimulationName")) +
    labs(x = x.label,
         y = paste0(variable, " (", var.unit, ")"),
         title = variable) +
    facet_annual +
    scale_x_ts +
    scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
    geom_line(size = 1, na.rm = TRUE) +
    scale_color_manual(values = rep(cbPalette, 10),
                       guide = guide_legend(ncol = 2, byrow = TRUE)) +
    theme_hisafe_ts()

  return(plot.obj)
}

#' Plot timeseries diagnostics of Hi-sAFe output
#' @description Plots a daily or annual timeseries of every Hi-sAFe output variable. All plots are saved as
#' png files to a specifified output path.
#' @return A list of \code{ggplot} objects is invisibly returned. If the data is of class \code{hop-group} and contains
#' data from more than one Hi-sAFe simulation, the plots will contain multiple lines, colored and labeled by SimulationName.
#' If the data contains two more tree ids, the plots will be faceted by tree id.
#' @param data An object of class \code{hop} or \code{hop-group} containing output data from one or more Hi-sAFe simulations.
#' @param time.class If 'annual', the default, annual timeseries are created. If 'daily', daily timeseries are created.
#' @param output.path A character stting indicating the path to the directory where plots should be saved. Plots are
#' saved in a subdirectory within this directory named by \code{time.class}.
#' @param time.lim If time.class is 'annual', the default, a numeric vector of length two providing the \code{c(minimum, maximum)} of years (since planting) to plot.
#' If time.class is 'daily', a character vector of length two providing the \code{c(minimum, maximum)} dates ('yyyy-mm-dd') to plot.
#' If no input, the full available time range is plotted. Use \code{NA} to refer to the start or end of the simulation.
#' @param tree.id A numeric vector indicating the ids of a subset of tree ids to plot. If no input, all trees will be plotted.
#' @export
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' mydata <- read_hisafe_output("MySimulation", "./")
#'
#' # You can create an annual timeseries of every variable:
#' diag_hisafe_ts(mydata)
#'
#' # For daily timeseries instead:
#' diag_hisafe_ts(mydata, "daily")
#' }
diag_hisafe_ts <- function(data,
                           time.class = "annual",
                           output.path = "./diagnostics",
                           time.lim = NULL,
                           tree.id = NULL) {
  time.class <- tolower(time.class) # prevents error if improper capitalization not input by user
  ts.path <- gsub("//", "/", paste0(output.path, "/", time.class, "/"), fixed = TRUE)
  dir.create(ts.path, recursive = TRUE, showWarnings = FALSE)

  ## Clean columns & extract names of variables to plot
  # cols with only "error!" output are all NA's and cause plot errors
  if(time.class == "annual") {
    data$annual <- data$annual %>% select_if(~sum(!is.na(.)) > 0)
    var.names <- names(data$annual)[(which(names(data$annual) == "id") + 1):length(names(data$annual))]
  } else {
    data$daily <- data$daily %>% select_if(~sum(!is.na(.)) > 0)
    var.names <- names(data$daily)[(which(names(data$daily) == "id") + 1):length(names(data$daily))]
  }

  ## Create plots
  plot.list <- purrr::map(var.names, plot_hisafe_ts, data = data, time.class = time.class, time.lim = time.lim, tree.id = tree.id)

  ## Write plots to disk
  file.names <- paste0(var.names, ".png")
  pwalk(list(file.names, plot.list), ggsave, path = ts.path, width = 7, height = 7)

  ## Invisibly return list of plot objects
  invisible(plot.list)
}

#' Tile plot of Hi-sAFe monthCells output variable
#' @description Plots a daily or annual timeseries of a single Hi-sAFe output variable.
#' @details This function is very picky! You can only facet by two of the three manipulable variables: SimulationName, Year, Month. You must ensure that the one varibale not used for faceting is fixed at a single value.
#' @return A ggplot object.
#' @param data An object of class \code{hop} or \code{hop-group} containing output data from one or more Hi-sAFe simulations.
#' @param variable A character string of the name of the variable to color the tiles.
#' @param rowfacet One of "Year", "Month", or "SimulationName", indicating which variable to use for row faceting.
#' @param colfacet One of "Year", "Month", or "SimulationName", indicating which variable to use for column faceting.
#' @param sim.names A character string containing the SimulationNames to include. Use "all" to include all available values.
#' @param years A numeric vector containing the years to include. Use "all" to include all available values.
#' @param months A numeric vector containing the months to include. Use "all" to include all available values.
#' @export
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
plot_hisafe_monthcells <- function(data,
                                   variable,
                                   rowfacet = "SimulationName",
                                   colfacet = "Year",
                                   sim.names = "all",
                                   years = seq(0, 40, 5),
                                   months = 6) {

  ## Convert "all" arguements to actual values
  if(sim.names[1] == "all") { sim.names <- unique(data$monthCells$SimulationName) }
  if(years[1] == "all") { years <- unique(data$monthCells$Year) -  min(data$monthCells$Year) }
  if(months[1] == "all") { months <- unique(data$monthCells$Month) }

  ## Determine which variable is not part of faceting & trigger associated error
  vars <- c("SimulationName", "Year", "Month")
  avail.vars <- list(sim.names, years, months)
  var.lengths <- purrr::map_int(avail.vars, length) > 1
  if(sum(var.lengths) > 2) {
    stop("only two variables of (sim.names, years, months) can have length greater than one")
  }
  fixed.var <- paste(vars[!var.lengths], "=", avail.vars[[(1:3)[!var.lengths]]])

  ## Check if data has class hisafe or hisafe-group
  if(!any(c("hop", "hop-group") %in% class(data))) {
    stop("data not of class hop or hop-group")
  }

  ## Exract units of supplied variable from the "variables" slot
  var.unit <- data$variables %>%
    filter(VariableClass == "monthCells", VariableName == variable) %>%
    .$Units %>%
    gsub(pattern = "\\.", replacement = " ")

  ## Filter for provided sim.names, years & months
    plot.data <- data$monthCells %>%
      mutate(Year = Year - min(Year) + 1) %>% # Create 0+ year values
      filter(SimulationName %in% sim.names) %>%
      filter(Year %in% years) %>%
      filter(Month %in% months)

  ## Find tree locations for each simulation
    tree.locations <- plot.data %>%
      summarize(x = median(x), y = median(y))

    ## Create plot
  plot.obj <- ggplot(plot.data, aes(x = x, y = y)) +
    labs(x = rowfacet,
         y = colfacet,
         fill = var.unit,
         title = paste0(variable, " (", fixed.var, ")")) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    facet_grid(reformulate(rowfacet, colfacet), switch = "both") +
    geom_tile(aes_string(fill = variable), na.rm = TRUE, color = "black") +
    geom_point(data = tree.locations, fill = "black", color = "white", shape = 21) +
    viridis::scale_fill_viridis(option = "magma") +
    guides(fill = guide_colourbar(barwidth = 15,
                                  barheight = 1.5,
                                  title.vjust = 0.8,
                                  nbin = 100)) +
    coord_equal() +
    theme_hisafe_monthCells()

  return(plot.obj)
}
