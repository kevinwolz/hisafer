#' Plot timeseries of Hi-sAFe output variable
#' @description Plots a daily or annual timeseries of a single Hi-sAFe output variable.
#' @return If \code{plot = TRUE}, returns a ggplot object, otherwise the data that would create the plot is returned.
#' If the data contains two more tree ids, the plot will be faceted by tree id. Otherwise, if \code{facet.year = TRUE}, plots of daily profiles will be faceted by year.
#' @param hop An object of class hop.
#' @param variable A character string of the name of the variable to plot.
#' @param profile The profile for which to plot a timeseries. If 'annualtree' or 'annualplot', annual timeseries are created.
#' If 'trees', 'plot', or 'climate', daily timeseries are created.
#' @param simu.names A character vector of the SimulationNames within \code{hop} to include. Use "all" to include all available values.
#' @param years A numeric vector of the years within \code{hop} to include. Use "all" to include all available values.
#' @param tree.ids A numeric vector indicating a subset of tree ids to plot. Use "all" to include all available values.
#' @param doy.lim A numeric vector of length two providing the \code{c(minimum, maximum)} of julian days to plot. Only applies if \code{profile} is a daily profile.
#' @param color.palette A character stirng of hex values or R standard color names defining the color palette to use in plots with multiple simulations.
#' If \code{NULL}, the default, then the default color palette is a color-blind-friendly color palette. The default supports up to 24 simulations.
#' @param linetype.palette A character stirng of values defining the linetype palette to use in plots with multiple simulations.
#' If \code{NULL}, the default, then solid lines are used for all simulations. The default supports up to 24 simulations.
#' @param aes.cols A list with arguments "color" and "linetype" containing character stirngs of the column names to use for plot aesthetics.
#' @param facet.year A logical indicating whether, for daily profiles, the plot should be faceted by year. This helps with seeing finer level detail.
#' @param crop.points Logical indicating if points should be plotted as well, with point shape desgnating the main crop name.
#' Only applies when \code{profile} is 'plot' or 'annualplot'.
#' @param plot If \code{TRUE}, the default, a ggplot object is returned. If \code{FALSE}, the data that would create the plot is returned.
#' @export
#' @importFrom dplyr %>%
#' @import ggplot2
#' @family hisafe plot functions
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' mydata <- read_hisafe(path = "./")
#'
#' # You can create an annual timeseries of carbonCoarseRoots:
#' annual.plot <- plot_hisafe_ts(mydata, "carbonCoarseRoots", "annualtree")
#'
#' # For a daily timeseries instead:
#' daily.plot <- plot_hisafe_ts(mydata, "carbonCoarseRoots", "trees")
#'
#' # Once you have the plot object, you can display it and save it:
#' annual.plot
#' ggplot2::ggsave("annual_carbonCoarseRoots.png", annual.plot)
#' }
plot_hisafe_ts <- function(hop,
                           variable,
                           profile,
                           simu.names       = "all",
                           years            = "all",
                           tree.ids         = "all",
                           doy.lim          = c(1, 366),
                           color.palette    = NULL,
                           linetype.palette = NULL,
                           aes.cols         = list(color = "SimulationName", linetype = "SimulationName"),
                           facet.year       = TRUE,
                           crop.points      = FALSE,
                           plot             = TRUE) {

  annual.profiles <- c("annualtree", "annualplot")
  daily.profiles  <- c("trees", "plot", "climate")
  tree.profiles   <- c("annualtree", "trees")

  if(simu.names[1] == "all")                              simu.names <- unique(hop[[profile]]$SimulationName)
  if(years[1]      == "all")                              years      <- unique(hop[[profile]]$Year)
  if(tree.ids[1]   == "all" & profile %in% tree.profiles) tree.ids   <- unique(hop[[profile]]$id)

  if(!("hop" %in% class(hop)))                                  stop("hop argument not of class hop",                             call. = FALSE)
  if(!is.character(variable) | length(variable) > 1)            stop("variable argument must be a character vector of length 1",  call. = FALSE)
  if(!(profile %in% c(annual.profiles, daily.profiles)))        stop("supplied profile is not supported",                         call. = FALSE)
  if(nrow(hop[[profile]]) == 0)                                 stop(paste("no data from", profile, "profile found"),             call. = FALSE)
  if(!(all(is.character(simu.names)) | simu.names[1] == "all")) stop("simu.names argument must be 'all' or a character vector",   call. = FALSE)
  if(!(all(is.numeric(years))        | years[1]      == "all")) stop("years argument must be 'all' or a numeric vector",          call. = FALSE)
  if(!(all(is.numeric(tree.ids))     | tree.ids[1]   == "all")) stop("tree.ids argument must be 'all' or a numeric vector",       call. = FALSE)
  if(!(length(doy.lim) == 2 & all(doy.lim %in% 1:366)))         stop("doy.lim argument must be of length 2 with values in 1:366", call. = FALSE)
  if(!is.logical(facet.year))                                   stop("facet.year argument must be a logical",                     call. = FALSE)
  if(!is.logical(crop.points))                                  stop("crop.points argument must be a logical",                    call. = FALSE)
  if(!is.logical(plot))                                         stop("plot argument must be a logical",                           call. = FALSE)
  if(!(variable %in% names(hop[[profile]])))                    stop(paste0(variable, " does not exist within ", profile, " profile.",
                                                                                    "\nCheck spelling and capitalization of variable name.",
                                                                                    "\nEnsure that the variable was included within the output profile."),
                                                                             call. = FALSE)

  if(!all(simu.names %in% unique(hop[[profile]]$SimulationName)))          stop(paste("not all values in simu.names are present in the", profile, "profile"), call. = FALSE)
  if(!all(years      %in% unique(hop[[profile]]$Year)))                    stop(paste("not all values in years are present in the",      profile, "profile"), call. = FALSE)
  if(profile %in% tree.profiles) if(!all(tree.ids %in% hop[[profile]]$id)) stop(paste("not all values in tree.id are present in the",    profile, "profile"), call. = FALSE)
  if(!(aes.cols$color    %in% names(hop[[profile]])))                      stop(paste(aes.cols$color,    "is not a column name in the",  profile, "profile"), call. = FALSE)
  if(!(aes.cols$linetype %in% names(hop[[profile]])))                      stop(paste(aes.cols$linetype, "is not a column name in the",  profile, "profile"), call. = FALSE)

  ## Exract units of supplied variable from the "variables" slot
  var.unit <- hop$variables %>%
    dplyr::filter(VariableClass == profile, VariableName == variable) %>%
    .$Units %>%
    gsub(pattern = "\\.", replacement = " ")

  ## Create profile-specific x aesthetic, axis label, plot theme, and time.lim filter
  if(profile %in% annual.profiles){
    x.var       <- "Date"
    x.label     <- "Year"
    facet       <- geom_blank()
    scale_x_ts  <- scale_x_date(date_labels = "%Y") # "%b-%Y"
    theme.extra <- geom_blank()
    plot.data  <- hop[[profile]] %>%
      dplyr::filter(SimulationName %in% simu.names) %>%
      dplyr::filter(Year %in% years)
  } else {
    x.var       <- "fake.date"
    x.label     <- "Date"
    if(facet.year) {
      facet <- facet_wrap(~Year)
      scale_x_ts  <- scale_x_date(date_labels = "%b", expand = c(0,0),
                                limits = lubridate::as_date(lubridate::parse_date_time(paste0("8000-", doy.lim), "%Y-%j")))
      theme.extra <- theme(axis.ticks.length = unit(5, "points"),
                         axis.text.x       = element_text(margin = margin(t = 5, unit = "points"), angle = 90, hjust = 1, vjust = 0.5),
                         axis.text.y       = element_text(margin = margin(r = 5, unit = "points")))
    } else {
      x.var       <- "Date"
      facet       <- geom_blank()
      scale_x_ts  <- scale_x_date(date_labels = "%Y") # "%b-%Y"
      theme.extra <- geom_blank()
    }
    plot.data  <- hop[[profile]] %>%
      dplyr::filter(SimulationName %in% simu.names) %>%
      dplyr::filter(Year %in% years) %>%
      dplyr::filter(JulianDay >= doy.lim[1], JulianDay <= doy.lim[2]) %>%
      dplyr::mutate(fake.date = lubridate::ymd(paste0("8000-", Month, "-", Day)))
  }

  ## Filter/Facet by supplied tree.ids
  if(profile %in% tree.profiles) {
    plot.data <- plot.data %>%
      dplyr::filter(id %in% tree.ids) %>%
      dplyr::mutate(id = paste("Tree", id))
    if(length(tree.ids) > 1) {
      facet <- facet_wrap(~id)
      if(profile == "trees") {
        x.var <- "Date"
        scale_x_ts <- scale_x_date(date_labels = "%Y") # "%b-%Y"
        theme.extra <- geom_blank()
      }
    }
  }

  ## Palettes
  if(is.null(color.palette))    color.palette    <- rep(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), 3)
  if(is.null(linetype.palette)) linetype.palette <- rep(c("solid", "dashed", "dotted"), each = 8)

  ## Aesthetics
  plot.data[[aes.cols$color]]    <- factor(plot.data[[aes.cols$color]])
  plot.data[[aes.cols$linetype]] <- factor(plot.data[[aes.cols$linetype]])

  ## Create plot
  plot.obj <- ggplot(plot.data, aes_string(x        = x.var,
                                           y        = variable,
                                           color    = aes.cols$color,
                                           linetype = aes.cols$linetype,
                                           group    = "SimulationName")) +
    labs(x        = x.label,
         y        = paste0(variable, " (", var.unit, ")"),
         title    = variable,
         color    = ifelse(aes.cols$color    == "SimulationName", "", aes.cols$color),
         linetype = ifelse(aes.cols$linetype == "SimulationName", "", aes.cols$linetype)) +
    facet +
    scale_x_ts +
    scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
    geom_line(size = 1, na.rm = TRUE) +
    scale_color_manual(values = color.palette) +
    scale_linetype_manual(values = linetype.palette) +
    theme_hisafe_ts() +
    theme.extra

  if(crop.points & (profile %in% c("annualplot", "plot"))) {
    plot.obj <- plot.obj +
      geom_point(aes(shape = mainCropName), size = 2, na.rm = TRUE) +
      guides(shape = guide_legend(title = "Main crop", title.hjust = 0.5))
  }

  if(plot) return(plot.obj) else return(plot.data)
}

#' Tile plot of Hi-sAFe monthCells output variable
#' @description Plots a tile plot of a single Hi-sAFe monthCells output variable.
#' If the hop objectcontains the "trees" profile with crown diameter variables, the tree locations and crown outlines are also drawn.
#' @details This function is very picky! You can only facet by two of the three manipulable variables: SimulationName, Year, Month.
#' You must ensure that the one varibale not used for faceting is fixed at a single value.
#' @return Returns a ggplot object.
#' @param hop An object of class hop.
#' @param variable A character string of the name of the variable to color the tiles.
#' @param rowfacet One of "Year", "Month", or "SimulationName", indicating which variable to use for row faceting.
#' @param colfacet One of "Year", "Month", or "SimulationName", indicating which variable to use for column faceting.
#' @param simu.names A character string containing the SimulationNames to include. Use "all" to include all available values.
#' @param years A numeric vector containing the years (after planting) to include. Use "all" to include all available values.
#' @param months A numeric vector containing the months to include. Use "all" to include all available values.
#' @param trees Logical indicating if a point should be plotted at the location of each tree.
#' @param canopies Logical indicating if an elipsoid should be plotted representing the size of each tree canopy.
#' @param plot If \code{TRUE}, the default, a ggplot object is returned. If \code{FALSE}, the data that would create the plot is returned.
#' @export
#' @importFrom dplyr %>%
#' @import ggplot2
#' @family hisafe plot functions
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' mydata <- read_hisafe(path = "./")
#'
#' # You can create a tile plot of monthDirectParIncident:
#' tile.plot <- plot_hisafe_monthCells(mydata, "monthDirectParIncident")
#'
#' # The default settings use data from June and facet by Year and Simulation Name.
#' # If you instead want to just look at Year 20 and facet by Month and SimulationName:
#' tile.plot2 <- plot_hisafe_monthCells(mydata, "monthDirectParIncident",
#'                                      rowfacet = "SimulationName",
#'                                      colfacet = "Month",
#'                                      simu.names = "all",
#'                                      years = 20,
#'                                      months = 1:12)
#'
#' # Once you have the plot object, you can display it and save it:
#' tile.plot2
#' ggplot2::ggsave("monthDirectParIncident.png", tile.plot2)
#' }
plot_hisafe_monthcells <- function(hop,
                                   variable   = "monthRelativeTotalParIncident",
                                   colfacet   = "SimulationName",
                                   rowfacet   = "Year",
                                   simu.names = "all",
                                   years      = seq(0, 40, 5),
                                   months     = 6,
                                   trees      = TRUE,
                                   canopies   = TRUE,
                                   plot       = TRUE) {

  if(!("hop" %in% class(hop)))                                  stop("hop argument not of class hop",                                         call. = FALSE)
  if(nrow(hop$monthCells) == 0)                                 stop("no data from monthCells profile found",                                 call. = FALSE)
  if(!is.character(variable) | length(variable) > 1)            stop("variable argument must be a character vector of length 1",              call. = FALSE)
  if(!(colfacet %in% c("Year", "Month", "SimulationName")))     stop("colfacet must be one of: Year, Month, SimulationName",                  call. = FALSE)
  if(!(rowfacet %in% c("Year", "Month", "SimulationName")))     stop("rowfacet must be one of: Year, Month, SimulationName",                  call. = FALSE)
  if(!(all(is.character(simu.names)) | simu.names[1] == "all")) stop("simu.names argument must be 'all' or a character vector",               call. = FALSE)
  if(!(is.numeric(years)             | years[1]      == "all")) stop("years argument must be 'all' or a numeric vector",                      call. = FALSE)
  if(!(all(months %in% 1:12)         | months[1]     == "all")) stop("months argument must be 'all' or a numeric vector with values in 1:12", call. = FALSE)
  if(!is.logical(trees))                                        stop("trees argument must be a logical",                                      call. = FALSE)
  if(!is.logical(canopies))                                     stop("canopies argument must be a logical",                                   call. = FALSE)
  if(!is.logical(plot))                                         stop("plot argument must be a logical",                                       call. = FALSE)
  if(!(variable %in% names(hop$monthCells)))                    stop(paste0(variable, " does not exist within monthCells profile.",
                                                                            "\nCheck spelling and capitalization of variable name.",
                                                                            "\nEnsure that the variable was included within the output profile."),
                                                                     call. = FALSE)

  if(simu.names[1] == "all") simu.names <- unique(hop$monthCells$SimulationName)
  if(years[1]      == "all") years      <- unique(hop$monthCells$Year) -  min(hop$monthCells$Year)
  if(months[1]     == "all") months     <- unique(hop$monthCells$Month)

  ## Determine which variable is not part of faceting & trigger associated error
  vars <- c("SimulationName", "Year", "Month")
  avail.vars <- list(simu.names, years, months)
  var.lengths <- purrr::map_int(avail.vars, length) > 1
  if(sum(var.lengths) > 2) stop("only two variables of (simu.names, years, months) can have length greater than one", call. = FALSE)
  fixed <- which(!(vars %in% c(colfacet, rowfacet)))
  fixed.var <- ifelse(vars[fixed] == "SimulationName",
                      as.character(avail.vars[[fixed]]),
                      paste(vars[fixed], "=", avail.vars[[fixed]]))

  ## Exract units of supplied variable from the "variables" slot
  var.unit <- hop$variables %>%
    dplyr::filter(VariableClass == "monthCells", VariableName == variable) %>%
    .$Units %>%
    gsub(pattern = "\\.", replacement = " ")

  ## Filter for provided simu.names, years & months
  plot.data <- hop$monthCells %>%
    dplyr::mutate(Year = Year - min(Year) + 1) %>% # Create 0+ year values
    dplyr::filter(SimulationName %in% simu.names) %>%
    dplyr::filter(Year %in% years) %>%
    dplyr::filter(Month %in% months)

  if(nrow(plot.data) == 0) return(FALSE)

  xy.centers <- plot.data %>%
    dplyr::group_by(SimulationName) %>%
    dplyr::summarize(x.center = median(x), y.center = median(y))

  ## Filter for simu.names & combine with canopy diameter
  n.trees <- nrow(dplyr::filter(hop$tree.info, SimulationName %in% simu.names))
  if(n.trees > 0 & nrow(hop$trees) > 0) {
    tree.data <- hop$tree.info %>%
      dplyr::filter(SimulationName %in% simu.names)

    diam.data <- hop$trees %>%
      dplyr::mutate(Year = Year - min(Year) + 1) %>% # Create 0+ year values
      dplyr::filter(SimulationName %in% simu.names) %>%
      dplyr::filter(Year %in% years) %>%
      dplyr::filter(Month %in% months) %>%
      dplyr::filter(Day == 1) %>%
      dplyr::select(SimulationName, Year, Month, id, crownRadiusInterRow, crownRadiusTreeLine) %>%
      dplyr::left_join(tree.data, by = c("SimulationName", "id")) %>%
      dplyr::left_join(xy.centers, by = "SimulationName")

    for(i in 1:nrow(diam.data)) {
      if(diam.data$x[i] == 0 & diam.data$y[i] == 0){
        diam.data$x[i] <- diam.data$x[i] + diam.data$x.center[i]
        diam.data$y[i] <- diam.data$y[i] + diam.data$y.center[i]
      } else {
        cellWidth <- max(diff(plot.data[plot.data$SimulationName == diam.data$SimulationName[i],]$x))
        diam.data$x[i] <- diam.data$x[i] - cellWidth/2
        diam.data$y[i] <- diam.data$y[i] - cellWidth/2
      }
    }
  } else {
    years <- years[years %in% (unique(hop$monthCells$Year) -  min(hop$monthCells$Year))]
    years <- years[years != 0]
    diam.data <- dplyr::as_tibble(expand.grid(SimulationName = simu.names, Year = years, Month = months, id = NA,
                                              crownRadiusInterRow = NA, crownRadiusTreeLine = NA, species = NA, x = NA, y = NA)) %>%
      dplyr::mutate_if(is.logical, as.numeric)
  }

  ## Create plot
  plot.obj <- ggplot(plot.data, aes(x = x, y = y)) +
    labs(x = colfacet,
         y = rowfacet,
         fill = var.unit,
         title = paste0(variable, "\n(", fixed.var, ")")) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    facet_grid(reformulate(colfacet, rowfacet), switch = "both") +
    geom_tile(aes_string(fill = variable), na.rm = TRUE, color = "black") +
    viridis::scale_fill_viridis(option = "magma") +
    guides(fill = guide_colourbar(barwidth = 15,
                                  barheight = 1.5,
                                  title.vjust = 0.8,
                                  nbin = 100)) +
    coord_equal() +
    theme_hisafe_tile()

  if(trees) {
    plot.obj <- plot.obj +
      geom_point(data = diam.data,
                 color = "green",
                 size = 2,
                 na.rm = TRUE)
  }

  if(canopies) {
    plot.obj <- plot.obj +
      ggforce::geom_ellipsis(data = diam.data,
                             color = "green",
                             size = 2,
                             aes(x0 = x, y0 = y,
                                 a = crownRadiusInterRow,
                                 b = crownRadiusTreeLine,
                                 angle = 0),
                             inherit.aes = FALSE,
                             na.rm = TRUE)
  }

  if(plot) return(plot.obj) else return(plot.data)
}


#' Tile plot of Hi-sAFe cells output variable
#' @description Plots a tile plot of a single Hi-sAFe cells output variable.
#' SimulationName is used as the column facet. Date is used as the row facet.
#' @return Returns a ggplot object.
#' @param hop An object of class hop.
#' @param variable A character string of the name of the variable to color the tiles.
#' @param dates A character vector containing the dates (yyyy-mm-dd) to include.
#' @param plot If \code{TRUE}, the default, a ggplot object is returned. If \code{FALSE}, the data that would create the plot is returned.
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
plot_hisafe_cells <- function(hop, variable, dates, plot = TRUE) {
  if(!("hop" %in% class(hop))) stop("hop argument not of class hop", call. = FALSE)
  if(nrow(hop$cells) == 0)     stop("no data from cells profile found", call. = FALSE)

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

  if(plot) return(plot.obj) else return(plot.data)
}


#' Tile plot of Hi-sAFe voxels output variable
#' @description Plots a tile plot of a single Hi-sAFe voxels output variable.
#' If a single date is provided, SimulationName is used as the column facet. Otherwise, Date is used as the column facet.
#' @return Returns ggplot object.
#' @param hop An object of class hop.
#' @param variable A character string of the name of the variable to color the tiles.
#' @param dates A character vector containing the dates (yyyy-mm-dd) to include.
#' @param plot If \code{TRUE}, the default, a ggplot object is returned. If \code{FALSE}, the data that would create the plot is returned.
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
plot_hisafe_voxels <- function(hop, variable, dates, plot = TRUE) {
  if(!("hop" %in% class(hop)))                        stop("hop argument not of class hop", call. = FALSE)
  if(nrow(hop$voxels) == 0)                           stop("no data from voxels profile found", call. = FALSE)
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

  if(plot) return(plot.obj) else return(plot.data)
}
