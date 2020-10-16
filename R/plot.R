#' Plot timeseries of Hi-sAFe output variable
#' @description Plots a daily timeseries of a single Hi-sAFe output variable.
#' @return If \code{plot = TRUE}, returns a ggplot object, otherwise the data that would create the plot is returned.
#' If the data contains two more tree ids, the plot will be faceted by idTree.
#' Otherwise, if \code{facet.year = TRUE}, plots of daily profiles will be faceted by year.
#' If more than one value is passed to \code{variable}, then one plot will be created for each variable and combined using \code{cowplot::plot_grid}.
#' @param hop An object of class hop.
#' @param variables A character vector of the names of the variables to plot.
#' More than one variable name can be passed, but all variables must be from the same \code{profile}.
#' @param profile A character string of the profile for which to plot a timeseries. One of 'trees', 'plot', 'climate', or 'cells'.
#' @param cumulative Logical indicating wheter \code{variables} should be cumulated before plotting.
#' @param simu.names A character vector of the SimulationNames within \code{hop} to include. Use "all" to include all available values.
#' @param years A numeric vector of the years within \code{hop} to include. Use "all" to include all available values.
#' @param tree.ids A numeric vector indicating a subset of tree ids to plot. Use "all" to include all available values.
#' @param date.min A character string of the minimum date to keep, in the format "YYYY-MM-DD" or of class Date.
#' If NA, the minimum date in the output data is used.
#' @param date.max A character string of the maximum date to keep, in the format "YYYY-MM-DD" or of class Date.
#' If NA, the maximum date in the output data is used.
#' @param doy.lim A numeric vector of length two providing the \code{c(minimum, maximum)} of julian days to plot. Only applies if \code{profile} is a daily profile.
#' @param color.palette A character string of hex values or R standard color names defining the color palette to use in plots with multiple simulations.
#' If \code{NA}, the default, then the default color palette is a color-blind-friendly color palette. The default supports up to 48 simulations.
#' @param linetype.palette A character string of values defining the linetype palette to use in plots with multiple simulations.
#' If \code{NA}, the default, then solid lines are used for all simulations. The default supports up to 48 simulations.
#' @param aes.cols A list with arguments "color" and "linetype" containing character strings of the column names to use for plot aesthetics.
#' @param facet.simu A logical indicating whether the plot should be faceted by SimulationName This helps when values among simulations are overplotted.
#' @param facet.year A logical indicating whether the plot should be faceted by year. This helps with seeing finer level detail.
#' @param facet.crop A logical indicating whether the plot should be faceted by cropType (mainCrop vs. interCrop). Only applies when \code{profile} is 'cells'.
#' @param intercrop A logical indicating whether the plot should include the interCrop cropType. Only applies when \code{profile} is 'cells'.
#' @param crop.points Logical indicating if points should be plotted as well, with point shape desgnating the main crop name.
#' Only applies when \code{profile} is 'plot'.
#' @param plot If \code{TRUE}, the default, a ggplot object is returned. If \code{FALSE}, the data that would create the plot is returned.
#' @param save If \code{FALSE}, the default, a ggplot object is returned.
#' If \code{TRUE}, a ggplot object is returned invisibly and the plot is saved to \code{output.path}.
#' @param output.path A character string indicating the path to the directory where plots should be saved.
#' If \code{NULL}, the experiment/simulation path is read from the hop object, and a directory is created there called "analysis/misc".
#' The tables and plots will be saved in this directory.
#' @export
#' @importFrom dplyr %>%
#' @import ggplot2
#' @family hisafe plot functions
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' mydata <- read_hisafe(path = "./")
#'
#' # For a daily timeseries:
#' daily.plot <- plot_hisafe_ts(mydata, "carbonCoarseRoots", "trees")
#'
#' # Once you have the plot object, you can display it and save it:
#' daily.plot
#' ggplot2::ggsave("carbonCoarseRoots.png", daily.plot)
#' }
plot_hisafe_ts <- function(hop,
                           variables,
                           profile,
                           cumulative       = FALSE,
                           simu.names       = "all",
                           years            = "all",
                           tree.ids         = "all",
                           date.min         = NA,
                           date.max         = NA,
                           doy.lim          = c(1,365),
                           color.palette    = NA,
                           linetype.palette = NA,
                           aes.cols         = list(color = "SimulationName", linetype = "SimulationName"),
                           facet.simu       = FALSE,
                           facet.year       = FALSE,
                           facet.crop       = FALSE,
                           intercrop        = TRUE,
                           crop.points      = FALSE,
                           plot             = TRUE,
                           save             = FALSE,
                           output.path      = NULL) {

  allowed.profiles  <- c("trees", "plot", "climate", "cells")
  tree.profiles     <- "trees"

  is_hop(hop, error = TRUE)
  profile_check(hop, profile, error = TRUE)
  variable_check(hop, profile, variables, error = TRUE)
  if(!(profile %in% allowed.profiles)) stop("supplied profile is not supported", call. = FALSE)

  if(years[1]    == "all")                              years    <- unique(hop[[profile]]$Year)
  if(tree.ids[1] == "all" & profile %in% tree.profiles) tree.ids <- unique(hop[[profile]]$idTree)

  if(!all(is.numeric(years)))                                     stop("years argument must be 'all' or a numeric vector",          call. = FALSE)
  if(!(length(doy.lim) == 2 & all(doy.lim %in% 1:366)))           stop("doy.lim argument must be of length 2 with values in 1:366", call. = FALSE)
  if(!(is.na(color.palette) | is.character(color.palette)))       stop("color.palette argument must be a character vector",         call. = FALSE)
  if(!(is.na(linetype.palette) | is.character(linetype.palette))) stop("linetype.palette argument must be a character vector",      call. = FALSE)
  if(!all(purrr::map_lgl(aes.cols, function(x) length(x) == 1 & is.character(x)))) stop("aes.cols list elements must be character vectors of length 1", call. = FALSE)
  is_TF(cumulative)
  is_TF(facet.simu)
  is_TF(facet.year)
  is_TF(facet.crop)
  is_TF(crop.points)
  is_TF(plot)
  variable_check(hop, profile, c(variables, aes.cols$color, aes.cols$linetype), error = TRUE)
  #if(facet.simu & length(variables) > 1)                              stop("facet.simu can only be TRUE when plotting a single variable", call. = FALSE)
  if(facet.year & length(variables) > 1)                              stop("facet.year can only be TRUE when plotting a single variable", call. = FALSE)
  if(facet.simu & length(tree.ids)  > 1 & profile %in% tree.profiles) stop("facet.simu can only be TRUE when plotting a single tree",     call. = FALSE)
  if(facet.year & length(tree.ids)  > 1 & profile %in% tree.profiles) stop("facet.year can only be TRUE when plotting a single tree",     call. = FALSE)
  if(!all(years %in% unique(hop[[profile]]$Year)))                    stop(paste("not all values in years are present in the",  profile, "profile"),
                                                                           call. = FALSE)

  hop <- hop_filter(hop        = hop,
                    simu.names = simu.names,
                    tree.ids   = tree.ids,
                    date.min   = date.min,
                    date.max   = date.max)

  ## Create facet
  facet.crop <- facet.crop & profile == "cells"
  if(length(variables) > 1) force.rows <- 1 else force.rows <- NULL

  if(facet.crop & facet.simu) {
    if(length(variables) > 1) stop("facet.crop and facet.simu can only both be TRUE when plotting a single variable", call. = FALSE)
    facet <- facet_grid(cropType~SimulationName)
  } else if(facet.crop & facet.year) {
    stop("facet.crop and facet.year cannot both be TRUE", call. = FALSE)
  } else if(facet.crop) {
    facet <- facet_wrap(~cropType, nrow = force.rows)
  } else if(facet.simu & facet.year) {
    facet <- facet_grid(Year~SimulationName)
  } else if(facet.year) {
    facet <- facet_wrap(~Year)
  } else if(facet.simu) {
    facet <- facet_wrap(~SimulationName, nrow = force.rows)
  } else {
    facet <- geom_blank()
    theme.extra <- geom_blank()
  }

  # Create facet-specific x-axis aesthetic & label
  if(facet.year) {
    x.var       <- "fake.date"
    scale_x_ts  <- scale_x_date(date_labels = "%b", expand = c(0,0),
                                limits = lubridate::as_date(lubridate::parse_date_time(paste0("8000-", doy.lim), "%Y-%j")))
  } else if(length(years) == 1) {
    x.var       <- "fake.date"
    scale_x_ts  <- scale_x_date(date_labels = "%b", expand = c(0,0), date_breaks = "1 month",
                                limits = lubridate::as_date(lubridate::parse_date_time(paste0("8000-", doy.lim), "%Y-%j")))
  } else {
    x.var <- "Date"
    scale_x_ts  <- scale_x_date(date_labels = "%Y")
  }

  if(facet.simu | facet.year | facet.crop) theme.extra <- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

  plot.data  <- hop[[profile]]

  if(!intercrop & profile == "cells") plot.data <- plot.data %>% dplyr::filter(cropType == "mainCrop")

  if(profile == "cells") {
    if(facet.crop) {
      plot.data <- plot.data %>%
        dplyr::group_by(SimulationName, Date, Day, Month, Year, JulianDay, cropType)
    } else {
      plot.data <- plot.data %>%
        dplyr::group_by(SimulationName, Date, Day, Month, Year, JulianDay)
    }
    plot.data <- plot.data %>%
      dplyr::summarize_if(is.numeric, mean, na.rm = TRUE) %>%
      dplyr::ungroup()
  }

  plot.data <- plot.data %>%
    dplyr::filter(Year %in% years) %>%
    dplyr::mutate(fake.date = lubridate::ymd(paste0("8000-", Month, "-", Day)))
  yrs.to.remove <- unique(plot.data$Year)[table(plot.data$Year) == length(unique(plot.data$SimulationName))]
  plot.data <- dplyr::filter(plot.data, !(Year %in% yrs.to.remove))

  ## Filter/Facet by supplied tree.ids
  if(profile %in% tree.profiles) {
    plot.data <- plot.data %>%
      dplyr::mutate(idTree = paste("Tree", idTree))
    if(length(tree.ids) > 1) {
      facet <- facet_wrap(~idTree, nrow = force.rows)
      if(profile == "trees") {
        x.var <- "Date"
        scale_x_ts <- scale_x_date(date_labels = "%Y")
        theme.extra <- geom_blank()
      }
    }
  }

  ## Aesthetics
  plot.data[[aes.cols$color]]    <- factor(plot.data[[aes.cols$color]])
  plot.data[[aes.cols$linetype]] <- factor(plot.data[[aes.cols$linetype]])

  ## Palettes
  if(is.na(color.palette)) color.palette <- rep(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), 6)
  if(aes.cols$color == aes.cols$linetype)  {
    if(is.na(linetype.palette))  linetype.palette <- rep(1:6, each = 8)
    scale_linetype <- scale_linetype_manual(values = linetype.palette)
  } else {
    scale_linetype <- scale_linetype_discrete()
  }

  geom        <- geom_line(size = 1, na.rm = TRUE, aes_string(color = aes.cols$color, linetype = aes.cols$linetype))
  scale_color <- scale_color_manual(values = color.palette)
  if(facet.simu | length(unique(plot.data$SimulationName)) == 1) {
    scale_color <- scale_linetype <- geom_blank()
    geom <- geom_line(size = 1, na.rm = TRUE, color = "black")
  }

  ## Group for cumulation
  if(cumulative) {
    cum.group <- c("SimulationName", "idTree"[profile == "trees"], "Year"[facet.year], "cropType"[facet.crop])
    plot.data <- plot.data %>%
      dplyr::group_by_at(cum.group) %>%
      dplyr::mutate_at(variables, cumsum)
  }

  ## Create plot
  plot.obj <- list()
  for(i in 1:length(variables)) {
    plot.obj[[i]] <- ggplot(plot.data, aes_string(x     = x.var,
                                                  y     = variables[i],
                                                  group = "SimulationName")) +
      labs(x        = "Date",
           y        = gsub(" \\(\\)", "", paste0("Cumulative "[cumulative], variables[i], " (", get_units(variable = variables[i], prof = profile), ")")),
           title    = ifelse(length(variables) == 1, variables[i], ""),
           color    = ifelse(aes.cols$color    == "SimulationName", "", aes.cols$color),
           linetype = ifelse(aes.cols$linetype == "SimulationName", "", aes.cols$linetype)) +
      facet +
      scale_x_ts +
      scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
      geom +
      scale_color +
      scale_linetype +
      theme_hisafe_ts(legend.title = element_blank()) +
      theme.extra

    if(length(variables) > 1) plot.obj[[i]] <- plot.obj[[i]] + theme(plot.title = element_blank())

    if(crop.points & (profile %in% "plot")) {
      plot.obj[[i]] <- plot.obj[[i]] +
        geom_point(aes(shape = mainCropName), size = 2, na.rm = TRUE) +
        guides(shape = guide_legend(title = "Main crop", title.hjust = 0.5))
    }
  }

  if(length(plot.obj) > 1) {
    if(!requireNamespace("cowplot", quietly = TRUE)) stop("The package 'cowplot' is required when passing 2 or more variable names.
                                                          Please install and load it", call. = FALSE)
    plot.out <- cowplot::plot_grid(plotlist = plot.obj, ncol = 1)
  } else {
    plot.out <- plot.obj[[1]]
  }

  if(plot & save) {
    output.path <- clean_path(paste0(diag_output_path(hop = hop, output.path = output.path), "/misc/"))
    dir.create(output.path, recursive = TRUE, showWarnings = FALSE)
    ggsave_fitmax(paste0(output.path, profile, "_", paste(variables, collapse = "-"), ".png"), plot.out, scale = 1.5)
    invisible(plot.out)
  } else if(plot) {
    return(plot.out)
  } else {
    return(dplyr::select(plot.data, -fake.date))
  }
}

#' Tile plot of Hi-sAFe monthCells output variable
#' @description Plots a tile plot of a single Hi-sAFe monthCells output variable.
#' @details This function is very picky! You can only facet by two of the three manipulable variables: SimulationName, Year, Month.
#' You must ensure that the one varibale not used for faceting is fixed at a single value.
#' @return Returns a ggplot object.
#' @param hop An object of class hop.
#' @param variable A character string of the name of the variable to color the tiles.
#' @param rowfacet One of "Year", "Month", or "SimulationName", indicating which variable to use for row faceting.
#' @param colfacet One of "Year", "Month", or "SimulationName", indicating which variable to use for column faceting.
#' @param simu.names A character string containing the SimulationNames to include. Use "all" to include all available values.
#' @param years A numeric vector containing the years to include. Use "all" to include all available values.
#' @param months A numeric vector containing the months to include. Use "all" to include all available values.
#' @param plot.x Either "x" or "y", indicating which axis of the simulation scene should be plotted on the x-axis of the plot.
#' @param trees Logical indicating if a point should be plotted at the location of each tree.
#' @param canopies Logical indicating if an elipsoid should be plotted representing the size of each tree canopy.
#' @param tree.simus Logical indicating whether only simulations containing trees should be plotted.
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
#' tile.plot <- plot_hisafe_monthcells(mydata, "monthDirectParIncident")
#'
#' # The default settings use data from June and facet by Year and Simulation Name.
#' # If you instead want to just look at Year 20 and facet by Month and SimulationName:
#' tile.plot2 <- plot_hisafe_monthcells(mydata, "monthDirectParIncident",
#'                                      rowfacet = "SimulationName",
#'                                      colfacet = "Month",
#'                                      simu.names = "all",
#'                                      years = 2000,
#'                                      months = 1:12)
#'
#' # Once you have the plot object, you can display it and save it:
#' tile.plot2
#' ggsave_fitmax("monthDirectParIncident.png", tile.plot2)
#' }
plot_hisafe_monthcells <- function(hop,
                                   variable   = "monthRelativeTotalParIncident",
                                   colfacet   = "SimulationName",
                                   rowfacet   = "Year",
                                   simu.names = "all",
                                   years      = seq(min(hop$monthCells$Year), max(hop$monthCells$Year), 5),
                                   months     = 6,
                                   plot.x     = "x",
                                   trees      = TRUE,
                                   canopies   = TRUE,
                                   tree.simus = FALSE,
                                   plot       = TRUE) {

  is_hop(hop, error = TRUE)
  profile_check(hop, "monthCells", error = TRUE)
  variable_check(hop, "monthCells", variable, error = TRUE)

  if(simu.names[1] == "all") simu.names <- unique(hop$monthCells$SimulationName)
  if(years[1]      == "all") years      <- unique(hop$monthCells$Year)
  if(months[1]     == "all") months     <- unique(hop$monthCells$Month)

  if(length(variable) > 1)                                  stop("variable argument must be a character vector of length 1",              call. = FALSE)
  if(!(colfacet %in% c("Year", "Month", "SimulationName"))) stop("colfacet must be one of: Year, Month, SimulationName",                  call. = FALSE)
  if(!(rowfacet %in% c("Year", "Month", "SimulationName"))) stop("rowfacet must be one of: Year, Month, SimulationName",                  call. = FALSE)
  if(!is.numeric(years))                                    stop("years argument must be 'all' or a numeric vector",                      call. = FALSE)
  if(!all(months %in% 1:12))                                stop("months argument must be 'all' or a numeric vector with values in 1:12", call. = FALSE)
  if(!(plot.x %in% c("x", "y")))                            stop("plot.x must be one of 'x' or 'y'",                                      call. = FALSE)
  if(!all(months %in% hop$monthCells$Month))                stop("one or more values in months is not present in the monthCells profile of hop",     call. = FALSE)
  is_TF(trees)
  is_TF(canopies)
  is_TF(tree.simus)
  is_TF(plot)

  if(tree.simus) {
    profile_check(hop, "tree.info", error = TRUE)
    simu.names <- simu.names[simu.names %in% unique(hop$tree.info$SimulationName)]
    if(length(simu.names) < 1) stop("simulation filtering resulted in no simulations to plot", call. = FALSE)
  }

  dates <- expand.grid(year = years, month = months, day = 1)
  dates <- lubridate::ymd(paste(dates$year, dates$month, dates$day, sep = "-"))
  hop <- hop_filter(hop            = hop,
                    simu.names     = simu.names,
                    dates          = dates[dates %in% hop$monthCells$Date],
                    strip.exp.plan = TRUE)

  if(plot.x == "y") { # Rotate scene if plot.x = "y"
    for(p in c("tree.info", "monthCells")) hop[[p]] <- swap_cols(hop[[p]], "x", "y")
    hop$plot.info <- swap_cols(hop$plot.info, "plotWidth", "plotHeight")
  }

  cellWidth  <- min(abs(diff(unique(hop$monthCells$x))))
  plotWidth  <- max(hop$monthCells$x) + cellWidth
  plotHeight <- max(hop$monthCells$y) + cellWidth

  ## Determine which variable is not part of faceting & trigger associated error
  vars <- c("SimulationName", "Year", "Month")
  avail.vars <- list(SimulationName = simu.names, Year = years, Month = months)
  var.to.check <- vars[!(vars %in% c(rowfacet, colfacet))]
  var.too.long <- length(avail.vars[[var.to.check]]) > 1
  if(var.too.long) stop("only the variables specified by colfacet and rowfacet can have length greater than one", call. = FALSE)
  fixed <- which(!(vars %in% c(colfacet, rowfacet)))
  fixed.var <- ifelse(vars[fixed] == "SimulationName",
                      as.character(avail.vars[[fixed]]),
                      paste(vars[fixed], "=", avail.vars[[fixed]]))

  plot.data <- create_tile_data(hop       = hop,
                                profile   = "monthCells",
                                cellWidth = cellWidth)

  tree.data <- create_tree_data(hop      = hop,
                                trees    = trees,
                                canopies = canopies,
                                plot.x   = plot.x)

  white.boxes <- build_white_boxes_tile(hop   = hop,
                                        X.MIN = 0,
                                        X.MAX = plotWidth,
                                        Y.MIN = 0,
                                        Y.MAX = plotHeight)

  plot.obj <- ggplot(plot.data) +
    labs(x     = colfacet,
         y     = rowfacet,
         fill  = get_units(variable = variable, prof = "monthCells"),
         title = paste0(variable, "\n(", fixed.var, ")")) +
    coord_equal(xlim   = c(0, plotWidth),
                ylim   = c(0, plotHeight),
                expand = FALSE) +
    facet_grid(reformulate(colfacet, rowfacet), switch = "both") +
    geom_rect(aes_string(xmin = "x",
                         xmax = "xmax",
                         ymin = "y",
                         ymax = "ymax",
                         fill = variable)) +
    scale_fill_viridis_c(option = "magma") +
    scale_linetype_identity() +
    guides(fill = guide_colourbar(barwidth    = 15,
                                  barheight   = 1.5,
                                  title.vjust = 0.8,
                                  nbin        = 100)) +
    theme_hisafe_tile()

  plot.obj <- plot.obj %>%
    add_trees(tree.data   = tree.data,
              white.boxes = white.boxes,
              trees       = trees,
              canopies    = canopies)

  if(plot) return(plot.obj) else return(plot.data)
}

#' Tile plot of Hi-sAFe annualCells output variable
#' @description Plots a tile plot of a single Hi-sAFe annualCells output variable.
#' @details This function is very picky! You can only facet by two of the three manipulable variables: SimulationName, Year, Month.
#' You must ensure that the one varibale not used for faceting is fixed at a single value.
#' @return Returns a ggplot object.
#' @param hop An object of class hop.
#' @param variable A character string of the name of the variable to color the tiles.
#' @param simu.names A character string containing the SimulationNames to include. Use "all" to include all available values.
#' @param years A numeric vector of the years within \code{hop} to include. Use "all" to include all available values.
#' @param trees Logical indicating if a point should be plotted at the location of each tree.
#' @param plot.x Either "x" or "y", indicating which axis of the simulation scene should be plotted on the x-axis of the plot.
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
#' tile.plot <- plot_hisafe_annualcells(mydata, "yieldMax")
#'
#' # Once you have the plot object, you can display it and save it:
#' tile.plot
#' ggsave_fitmax("yield.png", tile.plot)
#' }
plot_hisafe_annualcells <- function(hop,
                                    variable   = "yieldMax",
                                    simu.names = "all",
                                    years,
                                    plot.x     = "x",
                                    trees      = TRUE,
                                    canopies   = TRUE,
                                    plot       = TRUE) {

  is_hop(hop, error = TRUE)
  profile_check(hop, "annualCells", error = TRUE)
  variable_check(hop, "annualCells", variable, error = TRUE)

  if(years[1] == "all") years <- unique(hop$annualCells$Year)

  if(length(variable) > 1)       stop("variable argument must be a character vector of length 1", call. = FALSE)
  if(!is.numeric(years))         stop("years argument must be 'all' or a numeric vector",         call. = FALSE)
  if(!(plot.x %in% c("x", "y"))) stop("plot.x must be one of 'x' or 'y'",                         call. = FALSE)
  is_TF(trees)
  is_TF(canopies)
  is_TF(plot)

  dates <- lubridate::ymd(paste0(years, "-01-01"))
  hop <- hop_filter(hop            = hop,
                    simu.names     = simu.names,
                    dates          = dates[dates %in% hop$annualCells$Date],
                    strip.exp.plan = TRUE)

  if(plot.x == "y") { # Rotate scene if plot.x = "y"
    for(p in c("tree.info", "annualCells")) hop[[p]] <- swap_cols(hop[[p]], "x", "y")
    hop$plot.info <- swap_cols(hop$plot.info, "plotWidth", "plotHeight")
  }

  cellWidth  <- min(abs(diff(unique(hop$annualCells$x))))
  plotWidth  <- max(hop$annualCells$x) + cellWidth
  plotHeight <- max(hop$annualCells$y) + cellWidth

  plot.data <- create_tile_data(hop       = hop,
                                profile   = "annualCells",
                                cellWidth = cellWidth)

  tree.data <- create_tree_data(hop      = hop,
                                trees    = trees,
                                canopies = canopies,
                                plot.x   = plot.x)

  white.boxes <- build_white_boxes_tile(hop   = hop,
                                        X.MIN = 0,
                                        X.MAX = plotWidth,
                                        Y.MIN = 0,
                                        Y.MAX = plotHeight)

  plot.obj <- ggplot(plot.data) +
    labs(x     = "SimulationName",
         y     = "Year",
         fill  = get_units(variable = variable, prof = "annualCells"),
         title = variable) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    facet_grid(Year ~ SimulationName, switch = "both") +
    geom_rect(aes_string(xmin = "x",
                         xmax = "xmax",
                         ymin = "y",
                         ymax = "ymax",
                         fill = variable)) +
    scale_fill_viridis_c(option = "magma") +
    scale_linetype_identity() +
    guides(fill = guide_colourbar(barwidth    = 15,
                                  barheight   = 1.5,
                                  title.vjust = 0.8,
                                  nbin        = 100)) +
    coord_equal(xlim   = c(0, plotWidth),
                ylim   = c(0, plotHeight),
                expand = FALSE) +
    theme_hisafe_tile()

  plot.obj <- plot.obj %>%
    add_trees(tree.data   = tree.data,
              white.boxes = white.boxes,
              trees       = trees,
              canopies    = canopies)

  if(plot) return(plot.obj) else return(plot.data)
}

#' Tile plot of Hi-sAFe cells output variable
#' @description Plots a tile plot of a single Hi-sAFe cells output variable.
#' SimulationName is used as the column facet. Date is used as the row facet.
#' @return Returns a ggplot object.
#' @param hop An object of class hop.
#' @param variable A character string of the name of the variable to color the tiles.
#' @param dates A character vector (in the format "YYYY-MM-DD") or a vector of class Date of the dates to include.
#' @param start.date If \code{NULL}, the default, then no cumulation of values occurs. Otherwise, a character string (in the format "YYYY-MM-DD") or an object of class Date indicating the date from which cumulation of \code{variable} should be calculated.
#' @param rel.dates A character vector (in the format "YYYY-MM-DD") or a vector of class Date of the dates from which to scale \code{variable}.
#' In the plot, \code{variable} will be scaled to be between the minimum and maximum values of \code{variable} across these dates.
#' @param simu.names A character string containing the SimulationNames to include. Use "all" to include all available values.
#' @param X A numeric vector of the x values of the cells to include. If \code{NA}, the default, then all x values are used.
#' @param plot.x Either "x" or "y", indicating which axis of the simulation scene should be plotted on the x-axis of the plot.
#' @param N.arrow A character string indicating the color of the north arrow. String must be a valid color name passed to ggplot2.
#' Use \code{NA} to not plot the north arrow.
#' @param trees Logical indicating if a point should be plotted at the location of each tree.
#' @param canopies Logical indicating if an elipsoid should be plotted representing the size of each tree canopy.
#' @param plot If \code{TRUE}, the default, a ggplot object is returned. If \code{FALSE}, the data that would create the plot is returned.
#' @param mem.max An integer specifying the maximum number of days into the past to search
#' for cell data when no data is available for a given date within \code{hop}.
#' @param for.anim If \code{TRUE}, the plot formatting is simplified for use in animations.
#' @param cumu.scale If \code{TRUE}, the color scale is determined by all dates from \code{start.date} to \code{dates}.
#' If \code{FALSE}, the color scale is determined only by \code{rel.dates}. Only applies when \code{start.dates} is not \code{NULL}.
#' @export
#' @importFrom dplyr %>%
#' @import ggplot2
#' @family hisafe plot functions
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' mydata <- read_hisafe(path = "./")
#'
#' # You can create a tile plot of relativeDirectParIncident:
#' tile.plot <- plot_hisafe_cells(mydata, "relativeDirectParIncident", paste(1998, 6:8, 1, sep = "-"))
#'
#' # Once you have the plot object, you can display it and save it:
#' tile.plot
#' ggsave_fitmax("tiled_relativeDirectParIncident.png", tile.plot)
#' }
plot_hisafe_cells <- function(hop,
                              variable,
                              dates,
                              start.date = NULL,
                              rel.dates  = dates,
                              simu.names = "all",
                              X          = NA,
                              plot.x     = "x",
                              N.arrow    = "#000000",
                              trees      = TRUE,
                              canopies   = TRUE,
                              plot       = TRUE,
                              mem.max    = 0,
                              for.anim   = FALSE,
                              cumu.scale = TRUE) {

  is_hop(hop, error = TRUE)
  profile_check(hop,  "cells", error = TRUE)
  variable_check(hop, "cells", variable, error = TRUE)

  if(length(variable) > 1)             stop("variable argument must be a character vector of length 1", call. = FALSE)
  if(!(all(is.na(X)) | is.numeric(X))) stop("X must be numeric",                                        call. = FALSE)
  if(!(plot.x %in% c("x", "y")))       stop("plot.x must be one of 'x' or 'y'",                         call. = FALSE)
  if(!((is.character(N.arrow) | is.na(N.arrow)) & length(N.arrow) == 1)) stop("N.arrow argument must be a character vector of length 1", call. = FALSE)
  is_TF(trees)
  is_TF(canopies)
  is_TF(plot)
  is_TF(for.anim)

  if(!is.na(X[1])) hop$cells <- hop$cells %>% dplyr::filter(x %in% X)

  if(!is.null(start.date)) {
    hop.full <- hop_filter(hop            = hop,
                           simu.names     = simu.names,
                           date.min       = start.date,
                           date.max       = max(lubridate::ymd(dates)),
                           strip.exp.plan = TRUE)
    hop.full$cells <- hop.full$cells %>%
      dplyr::select_at(c(BASE.COLS, "idCell", "x", "y", variable)) %>%
      dplyr::arrange(SimulationName, idCell, Date) %>%
      dplyr::group_by(SimulationName, idCell) %>%
      dplyr::mutate_at(variable, cumsum) %>%
      dplyr::ungroup()

    hop <- hop.full
    if(!cumu.scale) hop.full <- hop_filter(hop = hop.full, dates = rel.dates)

  } else {
    hop.full <- hop_filter(hop            = hop,
                           simu.names     = simu.names,
                           dates          = rel.dates,
                           strip.exp.plan = TRUE)
  }
  hop <- hop_filter(hop            = hop,
                    simu.names     = simu.names,
                    dates          = dates,
                    strip.exp.plan = TRUE)

  if(nrow(hop$cells) < length(dates)) hop$cells <- add_historic_data(df = hop.full$cells, dates = dates, mem.max = mem.max)

  x.lab <- "X (m)"
  y.lab <- "Y (m)"
  if(plot.x == "y") { # Rotate scene if plot.x = "y"
    for(p in c("tree.info", "cells")) hop[[p]] <- swap_cols(hop[[p]], "x", "y")
    hop$plot.info <- swap_cols(hop$plot.info, "plotWidth", "plotHeight")
    x.lab <- "Y (m)"
    y.lab <- "X (m)"
  }

  cellWidth  <- min(abs(diff(unique(hop$cells$x))))
  plotWidth  <- max(hop$cells$x) + cellWidth
  plotHeight <- max(hop$cells$y) + cellWidth

  ## Extract variable range over rel.dates to set scale
  value.range <- range(hop.full$cells[[variable]], na.rm = TRUE)

  plot.data <- create_tile_data(hop       = hop,
                                profile   = "cells",
                                cellWidth = cellWidth)

  tree.data <- create_tree_data(hop      = hop,
                                trees    = trees,
                                canopies = canopies,
                                plot.x   = plot.x)

  white.boxes <- build_white_boxes_tile(hop   = hop,
                                        X.MIN = 0,
                                        X.MAX = plotWidth,
                                        Y.MIN = 0,
                                        Y.MAX = plotHeight)

  ## Determine faceting & axis labels
  title.lab <- variable
  if("hop-group" %in% class(hop) & length(dates) > 1){
    facet_cells <- facet_grid(Date ~ SimulationName, switch = "y")
  } else if(length(dates) > 1) {
    facet_cells <- facet_wrap(~Date)
  } else if("hop-group" %in% class(hop)) {
    if(for.anim) {
      facet_cells <- facet_wrap(~SimulationName, nrow = 1, strip.position = "bottom")
    } else {
      facet_cells <- facet_wrap(~SimulationName)
    }
  } else {
    facet_cells <- geom_blank()
  }

  if(!is.na(N.arrow) & profile_check(hop, "plot.info")) {
    north <- geom_spoke(data = hop$plot.info, aes(x      = cellWidth / 2,
                                                  y      = cellWidth / 2,
                                                  angle  = -(northOrientation - 90) * pi / 180,
                                                  radius = -cellWidth / 2),
                        color = N.arrow,
                        arrow = arrow(ends = "first", length = unit(0.2, "cm")))
  } else {
    north <- geom_blank()
  }

  if(for.anim) {
    plot.labs <- labs(x = x.lab,
                      y = y.lab)
    plot.theme <- theme_bw(base_size = 18) +
      theme(panel.spacing    = unit(2, "lines"),
            panel.grid       = element_blank(),
            axis.line        = element_blank(),
            axis.text        = element_text(color = "black"),
            axis.title.x     = element_text(vjust = -1, size = 30),
            axis.title.y     = element_text(vjust = 2,  size = 30),
            strip.background = element_blank(),
            strip.text       = element_blank())
    plot.guide <- guides(fill = FALSE)
  } else {
    plot.labs <- labs(x     = x.lab,
                      y     = y.lab,
                      title = title.lab)
    plot.theme <- theme_hisafe_tile()
    plot.guide <- guides(fill = guide_colourbar(title       = get_units(variable = variable, prof = "cells"),
                                                barwidth    = 15,
                                                barheight   = 1.5,
                                                title.vjust = 0.8,
                                                nbin        = 100))
  }

  plot.obj <- ggplot(plot.data) +
    plot.labs +
    facet_cells +
    geom_rect(aes_string(xmin = "x",
                         xmax = "xmax",
                         ymin = "y",
                         ymax = "ymax",
                         fill = variable)) +
    north +
    scale_fill_viridis_c(option = "magma", limits = value.range) +
    scale_linetype_identity() +
    plot.guide +
    coord_equal(xlim   = c(0, plotWidth),
                ylim   = c(0, plotHeight),
                expand = FALSE) +
    scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
    plot.theme

  plot.obj <- plot.obj %>%
    add_trees(tree.data   = tree.data,
              white.boxes = white.boxes,
              trees       = trees,
              canopies    = canopies)

  if(plot.x == "y") {
    plot.obj <- plot.obj +
      scale_y_continuous(breaks   = 0:plotHeight,
                         labels   = as.character(abs(plotHeight - 0:plotHeight)),
                         sec.axis = sec_axis(~ ., labels = NULL))
  } else {
    plot.obj <- plot.obj +
      scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL))
  }

  if(plot) return(plot.obj) else return(plot.data)
}


#' Tile plot of Hi-sAFe voxels output variable
#' @description Plots a tile plot of a single Hi-sAFe voxels output variable.
#' If a single date is provided, SimulationName is used as the column facet. Otherwise, Date is used as the column facet.
#' @return Returns ggplot object.
#' @param hop An object of class hop.
#' @param variable A character string of the name of the variable to plot.
#' @param date.min A character vector containing the minimum date (yyyy-mm-dd) to include.
#' If \code{NA}, the default, than the minimum value in the voxels profile is used
#' @param date.max A character vector containing the minimum date (yyyy-mm-dd) to include.
#' If \code{NA}, the default, than the maximum value in the voxels profile is used
#' @param simu.names A character string containing the SimulationNames to include. Use "all" to include all available values.
#' @param X A numeric vector of the x values of the voxels to include. If \code{NA}, the default, then all x values are used.
#' @param Y A numeric vector of the y values of the voxels to include. If \code{NA}, the default, then all y values are used.
#' @param Z A numeric vector of the z values of the voxels to include. If \code{NA}, the default, then all z values are used.
#' @param summarize.by One of 'x', 'y', or 'z', indicating  an axis over which to average voxel values.
#' If \code{NA}, the default, then no averaging is done and each voxel is plotted as its own line.
#' @param vline.dates A character vector of dates (yyyy-mm-dd) at which to plot dashed vertical reference lines.
#' If \code{NA}, the default, then no reference lines are drawn.
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
#' # You can create a tile plot of waterAvailable:
#' tile.plot <- plot_hisafe_voxels(mydata, "waterAvailable", paste(1998, 6:8, 1, sep = "-"))
#'
#' # Once you have the plot object, you can display it and save it:
#' tile.plot
#' ggsave_fitmax("tiled_waterAvailable.png", tile.plot)
#' }
plot_hisafe_voxels <- function(hop,
                               variable,
                               date.min     = NA,
                               date.max     = NA,
                               simu.names   = "all",
                               X            = NA,
                               Y            = NA,
                               Z            = NA,
                               summarize.by = "z",
                               facet.simu   = TRUE,
                               facet.z      = FALSE,
                               vline.dates  = NA,
                               plot         = TRUE) {

  is_hop(hop, error = TRUE)
  profile_check(hop,  "voxels", error = TRUE)
  variable_check(hop, "voxels", variable, error = TRUE)

  if(nrow(hop$plot.info) == 0)                                      stop("plot.info is unavilable in hop and is required",           call. = FALSE)
  if(length(variable) > 1)                                          stop("variable argument must be a character vector of length 1", call. = FALSE)
  if(!(all(is.na(X[1])) | is.numeric(X)))                           stop("X must be numeric",                                        call. = FALSE)
  if(!(all(is.na(Y[1])) | is.numeric(Y)))                           stop("Y must be numeric",                                        call. = FALSE)
  if(!(all(is.na(Z[1])) | is.numeric(Z)))                           stop("Z must be numeric",                                        call. = FALSE)
  if(!((summarize.by %in% c("x", "y", "z")) | is.na(summarize.by))) stop("summarize.by must be one of 'x', 'y', or 'z'",             call. = FALSE)
  if(!(all(is.na(vline.dates)) | is.character(vline.dates)))        stop("vline.dates must be a character vector",                   call. = FALSE)
  is_TF(facet.simu)
  is_TF(facet.z)
  is_TF(plot)
  if(length(unique(hop$plot.info$soilDepth)) > 1) warning("maximum soil depth is not consistent across all simluations within the hop", call. = FALSE)

  if(all(is.na(X))) X <- unique(hop$voxels$x)
  if(all(is.na(Y))) Y <- unique(hop$voxels$y)
  if(all(is.na(Z))) Z <- unique(hop$voxels$z)
  if(!all(X %in% unique(hop$voxels$x))) stop("one or more values of X are not present in the voxel profile", call. = FALSE)
  if(!all(Y %in% unique(hop$voxels$y))) stop("one or more values of Y are not present in the voxel profile", call. = FALSE)
  if(!all(Z %in% unique(hop$voxels$z))) stop("one or more values of Z are not present in the voxel profile", call. = FALSE)

  hop <- hop_filter(hop        = hop,
                    simu.names = simu.names,
                    date.min   = date.min,
                    date.max   = date.max)

  plot.data <- hop$voxels %>%
    dplyr::select(SimulationName, Date, idVoxel, x, y, z, variable) %>%
    dplyr::filter(x %in% X,
                  y %in% Y,
                  z %in% Z)

  ## Summarize along axes
  if(summarize.by %in% c("x", "y", "z")) {
    plot.data <- plot.data %>%
      dplyr::group_by_(.dots = c("SimulationName", "Date", summarize.by)) %>%
      dplyr::select_(.dots   = c("SimulationName", "Date", summarize.by, variable)) %>%
      dplyr::summarize_all(mean, na.rm = TRUE) %>%
      dplyr::ungroup()
    group.by <- summarize.by
  } else{
    summarize.by <- "z"
    plot.data <- plot.data %>%
      dplyr::mutate(xyz = paste(x, y, z, sep = "-"))
    group.by <- "xyz"
  }

  ## Determine faceting
  if(facet.simu & facet.z) {
    facet <- facet_grid(z~SimulationName)
  } else if(facet.simu) {
    facet <- facet_wrap(~SimulationName)
  } else if(facet.z) {
    facet <- facet_wrap(~z, ncol = 1)
  } else {
    facet <- element_blank()
  }

  plot.caption <- c(paste("X =", paste(X, collapse = ", ")),
                    paste("Y =", paste(Y, collapse = ", ")),
                    paste("Z =", paste(Z, collapse = ", ")))
  plot.caption <- paste(plot.caption[!(c("x", "y", "z") %in% summarize.by)], collapse = "\n")

  plot.data[[summarize.by]] <- factor(plot.data[[summarize.by]])

  plot.obj <- ggplot(plot.data, aes_string(x = "Date", y = variable, color = summarize.by, linetype = summarize.by, group = group.by)) +
    labs(x        = "Date",
         y        = gsub(" \\(\\)", "", paste0(variable, " (", get_units(variable = variable, prof = "voxels"), ")")),
         color    = paste(summarize.by, "(m)"),
         linetype = paste(summarize.by, "(m)"),
         title    = variable,
         caption  = plot.caption) +
    facet +
    scale_x_date(date_labels = "%d %b %Y") +
    geom_line(na.rm = TRUE) +
    scale_color_manual(values    = rep(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), 6)) +
    scale_linetype_manual(values = rep(1:6, each = 8)) +
    theme_hisafe_ts() +
    theme(axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.5),
          plot.caption = element_text(hjust = 1, size = 5))

  if(all(!is.na(vline.dates))) plot.obj <- plot.obj + geom_vline(xintercept = lubridate::ymd(vline.dates), lty = "dashed")

  if(plot) return(plot.obj) else return(plot.data)
}

#' Plot belowground tree root depth & water table
#' @description Plots a daily timeseries of tree maximum rooting depth, water table depth, and root senescence by anoxia.
#' @return If \code{plot = TRUE}, returns a ggplot object, otherwise the data that would create the plot is returned.
#' @param hop An object of class hop.
#' @param simu.names A character vector of the SimulationNames within \code{hop} to include. Use "all" to include all available values.
#' @param years A numeric vector of the years within \code{hop} to include. Use "all" to include all available values.
#' @param tree.ids A numeric vector indicating a subset of tree ids to plot. Only one tree can be plotted at a time.
#' @param sen A character vector indicated whether or not to plot the fine and coarse root senesence. Any value other than than "fine" and/or "coarse" will prevent them from being plotted.
#' @param plot If \code{TRUE}, the default, a ggplot object is returned. If \code{FALSE}, the data that would create the plot is returned.
#' @export
#' @importFrom dplyr %>%
#' @import ggplot2
#' @family hisafe plot functions
#' @examples
#' \dontrun{
#' plot_hisafe_bg(hop)
#' }
plot_hisafe_bg <- function(hop,
                           simu.names = "all",
                           years      = "all",
                           tree.ids   = 1,
                           sen        = c("fine", "coarse"),
                           plot       = TRUE) {

  if(!requireNamespace(c("gtable", "egg"), quietly = TRUE)) stop("The packages 'gtable' and 'egg' are required for hisafe_snapshot().
                                                                 Please install and load them", call. = FALSE)

  is_hop(hop, error = TRUE)
  profile_check(hop, "trees", error = TRUE)
  variable_check(hop, "trees", c("rootingDepth", "carbonFineRootSenAnoxia", "carbonCoarseRootSenAnoxia"), error = TRUE)

  if(years[1] == "all") years <- unique(hop$trees$Year)
  if(!all(is.numeric(years)))                         stop("years argument must be 'all' or a numeric vector",       call. = FALSE)
  if(!(is.numeric(tree.ids) & length(tree.ids) == 1)) stop("tree.ids argument must be a numeric vector of length 1", call. = FALSE)
  is_TF(plot)

  wt.profiles <- c("climate", "plot")[profile_check(hop, c("climate", "plot"))]
  wt.profile  <- c("climate", "plot")[purrr::map_lgl(wt.profiles, variable_check, hop = hop, variables = "waterTableDepth", error = FALSE)][1]
  if(is.na(wt.profile)) stop("watertableDepth is required in either the climate or plot profiles", call. = FALSE)

  hop <- hop_filter(hop, simu.names = simu.names, tree.ids = tree.ids)

  plot.data <- join_profiles(hop, profiles = c("trees", wt.profile)) %>%
    dplyr::filter(Year %in% years) %>%
    dplyr::select(SimulationName, Year, Month, Day, Date, JulianDay,
                  rootingDepth, carbonFineRootSenAnoxia, carbonCoarseRootSenAnoxia, waterTableDepth)

  base.plot <- ggplot(plot.data, aes(x = Date)) +
    facet_wrap(~SimulationName, nrow = 1) +
    scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
    theme_hisafe_ts()

  root.wt.plot <- base.plot +
    labs(title = "Root & water table depth",
         y     = "Depth (m)") +
    geom_line(aes(y = -rootingDepth), size = 2, na.rm = TRUE) +
    geom_line(aes(y = waterTableDepth), color = "blue", na.rm = TRUE)

  if(any(c("fine", "coarse") %in% sen)) {
    root.wt.plot <- root.wt.plot +
      theme(axis.text.x  = element_blank(),
            axis.title.x = element_blank())
  } else {
    root.wt.plot <- root.wt.plot +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  }

  fr.sen.plot <- base.plot +
    labs(title = "Fine root senescence by anoxia",
         y     = "Senescence (kg C)") +
    geom_line(aes(y = carbonFineRootSenAnoxia), na.rm = TRUE)

  if("coarse" %in% sen) {
    fr.sen.plot <- fr.sen.plot +
      theme(axis.text.x  = element_blank(),
            axis.title.x = element_blank())
  } else {
    fr.sen.plot <- fr.sen.plot +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  }

  cr.sen.plot <- base.plot +
    labs(title = "Coarse root senescence by anoxia",
         y     = "Senescence (kg C)") +
    geom_line(aes(y = carbonCoarseRootSenAnoxia), na.rm = TRUE) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

  plot.list <- list(root.wt.plot)
  if("fine"   %in% sen) plot.list <- c(plot.list, list(fr.sen.plot))
  if("coarse" %in% sen) plot.list <- c(plot.list, list(cr.sen.plot))

  plot.out <- egg::ggarrange(plots = plot.list, ncol = 1, draw = FALSE)

  if(plot) return(plot.out) else return(plot.data)
}

#' Plot crop temperature and temperature stress regions
#' @description Plots histograms of crop temperature and relevant temperature stress regions.
#' If \code{type} is 'lue', then \code{hop$cells$cropTemperature} (mean crop temperature) is shown in relation to tempStressLue (FTEMP in STICS) regions.
#' If \code{type} is 'reprod', then \code{hop$cells$cropMinTemperature} (min crop temperature) and \code{hop$cells$cropMaxTemperature} (max crop temperature)
#' are shown in relation to tempStressGrainFilling (FTEMPREMP in STICS) regions.
#' @return A ggplot object.
#' @param hop An object of class hop.
#' @param simu.name A character string of the SimulationName to plot. Only one simulation can be plotted.
#' @param years A numeric vector of the years within \code{hop} to include. Use "all" to include all available values.
#' @param months A numeric vector of the months within \code{hop} to include. Use "all" to include all available values.
#' Only applies if \code{type} is 'lue'. If \code{type} is 'reprod', then all days during which grain filling occurs are used.
#' @param cell.ids A numeric vector of the values of idCell within \code{hop} to include. Use "all" to include all available values.
#' @param facet.year A logical indicating whether the plot should be faceted by year. This helps with seeing finer level detail.
#' @param facet.month A logical indicating whether the plot should be faceted by month. Only applies if \code{type} is 'lue'.
#' @param temin Minimum threshold temperature for growth in biomass. Only applies if \code{type} is 'lue'.
#' @param teopt Optimum temperature for growth in biomass. Only applies if \code{type} is 'lue'.
#' @param teoptbis Optimum temperature for growth in biomass (if plateau between \code{teopt} and \code{teoptbis}). Only applies if \code{type} is 'lue'.
#' @param temax Maximum threshold temperature for growth in biomass. Only applies if \code{type} is 'lue'.
#' @param tminremp Minimum temperature for grain filling. Only applies if \code{type} is 'reprod'.
#' @param tmaxremp Maximum temperature for grain filling. Only applies if \code{type} is 'reprod'.
#' @param shade.color A character vector of length 3 indicating the shading colors of the various stress regions.
#' Strings must be a valid color name passed to ggplot2.
#' @export
#' @importFrom dplyr %>%
#' @import ggplot2
#' @family hisafe plot functions
#' @examples
#' \dontrun{
#' plot_hisafe_tempstress(hop, "Sim_1")
#' }
plot_hisafe_tstress <- function(hop,
                                simu.name,
                                type        = "lue",
                                years       = "all",
                                months      = "all",
                                cell.ids    = "all",
                                facet.year  = TRUE,
                                facet.month = TRUE,
                                temin       = 0,
                                teopt       = 10,
                                teoptbis    = 20,
                                temax       = 40,
                                tminremp    = 0,
                                tmaxremp    = 30,
                                shade.color = c("grey50", "grey70", "white")) {

  is_hop(hop, error = TRUE)
  profile_check(hop, "cells", error = TRUE)

  if(!(length(simu.name)   == 1 & is.character(simu.name)))   stop("simu.name argument must be a character vector of length 1",   call. = FALSE)
  if(!(length(shade.color) == 3 & is.character(shade.color))) stop("shade.color argument must be a character vector of length 3", call. = FALSE)
  if(!(years[1]    == "all" | is.numeric(years)))             stop("years argument must be 'all' or a numeric vector",            call. = FALSE)
  if(!(months[1]   == "all" | is.numeric(months)))            stop("months argument must be 'all' or a numeric vector",           call. = FALSE)
  if(!(cell.ids[1] == "all" | is.numeric(cell.ids)))          stop("cell.ids argument must be 'all' or a numeric vector",         call. = FALSE)
  is_TF(facet.year)
  is_TF(facet.month)
  t.check <- purrr::map_lgl(c(temin, teopt, teoptbis, temax, tminremp, tmaxremp), function(x) length(x) ==  1 & is.numeric(x))
  if(!all(t.check)) stop("all temperature stress arguments must be numeric vectors of length 1", call. = FALSE)

  if(cell.ids == "all") cell.ids <- unique(hop$cells$idCell)

  if(type == "lue") {
    variable_check(hop, "cells", c("lai", "cropTemperature"), error = TRUE)
    filter_func <- function(x) dplyr::filter(x, lai > 0)
    shaded.boxes <- dplyr::tibble(lower    = c(-Inf,  temin, teopt,    teoptbis, temax),
                                  upper    = c(temin, teopt, teoptbis, temax,    Inf),
                                  fill.col = c("Full stress", "Gradual decline", "No stress", "Gradual decline", "Full stress"))
  } else if(type == "reprod") {
    variable_check(hop, "cells", c("grainBiomass", "phenologicStage", "cropMinTemperature", "cropMaxTemperature"), error = TRUE)
    months <- "all"
    filter_func <- function(x) dplyr::filter(x, grainBiomass > 0 & !(phenologicStage %in% 10:12))
    shaded.boxes <- dplyr::tibble(lower    = c(-Inf,     tminremp, tmaxremp),
                                  upper    = c(tminremp, tmaxremp, Inf),
                                  fill.col = c("Full stress", "No stress", "Full stress"))
    shade.color <- shade.color[c(1,3)]
  } else {
    stop("type argument must be 'lue' or 'reprod'", call. = FALSE)
  }

  plot.data <- hop %>%
    hop_filter(simu.names = simu.name,
               years      = years,
               months     = months) %>%
    .$cells %>%
    dplyr::filter(idCell %in% cell.ids) %>%
    filter_func() %>%
    dplyr::group_by(SimulationName, Year, Month, Day, Date)

  if(type == "lue") {
    plot.data <- plot.data %>%
      dplyr::summarize(cropTemperature = mean(cropTemperature)) %>%
      tidyr::gather(key = "metric", value = "temp", cropTemperature) %>%
      dplyr::mutate(metric = factor(metric, labels = "Crop mean temp"))

    x.lab <- "Mean crop temperature (C)"
    y.lab <- "# of days during growing season"
    LIMS <- c(min(temin, min(plot.data$temp)), max(temax, max(plot.data$temp)))
    color.guide <- guides(color = FALSE)

  } else if(type == "reprod") {
    plot.data <- plot.data %>%
      dplyr::summarize(cropMinTemperature = mean(cropMinTemperature),
                       cropMaxTemperature = mean(cropMaxTemperature)) %>%
      tidyr::gather(key = "metric", value = "temp", cropMinTemperature, cropMaxTemperature) %>%
      dplyr::mutate(metric = factor(metric,
                                    levels = c("cropMinTemperature", "cropMaxTemperature"),
                                    labels = c("Min temp", "Max temp")))

    x.lab <- "Min/Max crop temperature (C)"
    y.lab <- "# of days during grain filling"
    LIMS <- c(min(tminremp, min(plot.data$temp)), max(tmaxremp, max(plot.data$temp)))
    color.guide <- geom_blank()
  }

  facet.year  <- facet.year  & length(unique(plot.data$Year))  > 1
  facet.month <- facet.month & length(unique(plot.data$Month)) > 1 & type == "lue"

  if(facet.year & facet.month) {
    facet <- facet_grid(Month ~ Year)
  } else if(facet.year) {
    facet <- facet_wrap(~Year)
  } else if(facet.month) {
    facet <- facet_wrap(~Month)
  } else {
    facet <- geom_blank()
  }

  if(facet.year & facet.month) {
    title.text <- ""
  } else if(facet.year & months[1] != "all") {
    title.text <- paste("Month:", paste(months, collapse = ","))
  } else if(facet.month & years[1] != "all") {
    title.text <- paste("Year:",  paste(years,  collapse = ","))
  } else {
    title.text <- ""
  }

  plot.obj <- ggplot(plot.data) +
    facet +
    labs(title = title.text,
         x     = x.lab,
         y     = y.lab,
         fill  = "",
         color = "") +
    scale_x_continuous(limits = LIMS) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_rect(data = shaded.boxes, aes(xmin = lower, xmax = upper, fill = fill.col), ymin = -Inf, ymax = Inf, na.rm = TRUE) +
    geom_histogram(aes(x = temp, color = metric), binwidth = 1, fill = "black", position = "stack", na.rm = TRUE) +
    scale_fill_manual(values = shade.color) +
    guides(fill  = guide_legend(override.aes=list(color = "black"))) +
    color.guide +
    scale_color_manual(values = c("black", "red")) +
    theme_hisafe_ts()

  return(plot.obj)
}

#' Save ggplot to correctly-shaped image file
#' @description When a ggplot has a fixed panel aspect ratio, it can be a pain to find the right dimensions for the whole plot
#' (including axes, margins, legends, etc) when saving it to a fixed-size graphical device. ggsave_fitmax takes a ggplot
#' object and saves it as the largest image that fits inside the specified maximum height and width. The final image
#' dimensions will exactly match one of \code{maxheight} or \code{maxwidth} and will not exceed the other.
#' @param filename Name for the output image. By default the image format is guesssed from the file extension,
#' but this can be overridden by specifying a value for device. See \code{ggplot2::ggsave} for details.
#' @param plot A ggplot or gtable object.
#' @param maxheight Numeric, giving largest allowable height of the plot. The final image will exactly match one of these and will not exceed the other.
#' @param maxwidth Numeric, giving largest allowable width of the plot.
#' @param units One of "in", "cm", "mm", giving units for the dimensions. Note that "px" does not work.
#' @param ... Other arguments passed to \code{ggplot2::ggsave}, notably including dpi (to set pixel resolution of the output image)
#' and limitsize (set to FALSE if you really do want an image larger than 50 inches).
#' @details This is a convenience function that wraps two distinct steps: Looking up the plot dimensions using \code{get_dims},
#' and saving the image using \code{ggplot2::ggsave}. If you need more flexibility in either step, skip this wrapper and call get_dims directly,
#' then pass the computed dimensions to your favorite graphics device. See the examples in \code{get_dims} for an example.
#' The dimension lookup was motivated by the difficulty of predicting image height & width when the panels have a fixed aspect ratio,
#' but this wrapper should work as a one-call plotting command for plots with unconstrained ratios as well. Please report any that don't work.
#' @author Chris Black \email{chris@ckblack.org}
#' @export
#' @examples
#' \dontrun{
#' a=ggplot(mtcars, aes(wt,mpg))+geom_point()+theme(aspect.ratio=0.75)
#'
#' # Saves a.pdf at 10.55 x 8 inches
#' ggsave_fitmax(filename="a.pdf", plot=a, maxheight=8, maxwidth=12)
#'
#' # Saves a.png at 3163 x 2400 px
#' # == (nominally!) 10.55 x 8 inches at 300 ppi.
#' ggsave_fitmax(filename="a.png", plot=a, maxheight=8, maxwidth=12)
#'
#' # Saves aa.jpg at 1181 x 900 px
#' # == 7.8 x 6 inches at 150 ppi... or 3.9 x 3 inches at 300 ppi, or 16.4 x 12.5 at 72 ppi, or...
#' ggsave_fitmax(filename="aa.jpg", plot=a, maxheight=6, maxwidth=9, dpi=150)
#'
#' }
ggsave_fitmax <- function(filename,
                          plot,
                          maxheight = 7,
                          maxwidth  = maxheight,
                          units     = "in", ...) {
  if(is.null(plot)) return(FALSE)
  dims = get_dims(ggobj     = plot,
                  maxheight = maxheight,
                  maxwidth  = maxwidth,
                  units     = units)
  ggplot2::ggsave(filename = filename,
                  plot   = plot,
                  height = dims$height,
                  width  = dims$width,
                  units  = units, ...)
}

#' Find overall dimensions of a ggplot
#' @description Computes the largest possible dimensions for a fixed-aspect ggplot that still fits inside the given maximum height and width.
#' @return A list containing actual image dimensions height and width, both numeric and with the same units as units.
#' @param ggobj A ggplot or gtable object.
#' but this can be overridden by specifying a value for device. See \code{ggplot2::ggsave} for details.
#' @param maxheight Numeric, giving largest allowable height of the plot. The final image will exactly match one of these and will not exceed the other.
#' @param maxwidth Numeric, giving largest allowable width of the plot.
#' @param units Character, giving units for the dimensions. Must be recognized by both \code{png} and \code{grid::convertUnit},
#' so possible values are probably limited to "in", "cm", "mm". Note especially that "px" does not work.
#' @param ... Other arguments passed to png when setting up the throwaway plot.
#' @details The motivating problem: When making a ggplot with fixed panel aspect ratios, the overall dimensions of the full
#' plot still depend on the dimensions of other plot elements: axes, legends, titles, etc. In a facetted plot, this gets
#' even trickier: "OK, it has three panels each with aspect ratio 1.5, so that adds up to... wait, does every panel get
#' its own y-axis, or just the leftmost one?".
#'
#' ggplot apparently computes absolute dimensions for everything except the panels, so the approach taken here is to build
#' the plot inside a throwaway graphical device, subtract off the parts of the image area used by non-panel elements, then
#' divide the remainder up between panels. One dimension will be constrained by the size of the throwaway device, and we can
#' then calculate the other dimension from the panel aspect ratio.
#'
#' The biggest known issue with this approach is that it's inefficient, because we have to build the plot twice.
#' I don't know of any way around this. Do you?
#' @note For pixel-based formats such as PNG, the conversion between pixels and physical inch/mm dimensions is more complicated that it sounds.
#' In particular, some image viewers will treat a high-dpi image as a very large but low-dpi image. See the "Details" section of png for more,
#' but the upshot is that ggsave_fitmax always produces a raster image containing the right number of pixels to produce the requested physical
#' dimensions if displayed at the specified dpi.
#' @author Chris Black \email{chris@ckblack.org}
#' @examples
#' \dontrun{
#' # Extract dimensions of an existing ggplot object:
#' a=ggplot(mtcars, aes(wt,mpg))+geom_point()+theme(aspect.ratio=0.75)
#' d=get_dims(a, maxheight=12, maxwidth=8)
#' d
#' # $height
#' # [1] 6.138644
#'
#' # $width
#' # [1] 8
#'
#' # ==> "Oops, right. we want this plot to be wider than it is tall."
#'
#' d=get_dims(a, maxheight=8, maxwidth=12)
#' d
#' # $height
#' # [1] 8
#'
#' # $width
#' # [1] 10.48181
#'
#' # Can now use these dimensions to set up correctly-shaped graphics output
#' png("plot_of_a.png", height=d$height, width=d$width)
#' plot(a)
#' dev.off()
#' }
#' @keywords internal
get_dims <- function(ggobj,
                     maxheight,
                     maxwidth = maxheight,
                     units    = "in", ...) {

  # Internal helper function:
  # Treat all null units in a unit object as if they were inches.
  # This is a bad idea in gneral, but I use it here as a workaround.
  # Extracting unit names from non-atomic unit objects is a pain,
  # so questions like "which rows of this table layout have null heights?"
  # are hard to answer. To work around it, I exploit an (undocumented!)
  # quirk: When calculating the size of a table layout inside a Grid plot,
  # convertUnit(...) treats null units as zero.
  # Therefore
  #	(convertHeight(grob_height, "in", valueOnly=TRUE)
  #	- convertHeight(null_as_if_inch(grob_height), "in", valueOnly=TRUE))
  # does the inverse of convertUnit: It gives the sum of all *null* heights
  # in the object, treating *fixed* units as zero.
  #
  # Warning: I repeat, this approach ONLY makes any sense if
  #	convertUnit(unit(1, "null"), "in", "x", valueOnly=T) == 0
  # is true. Please check that it is before calling this code.
  .null_as_if_inch = function(u){
    stopifnot(packageVersion("grid") < "4.0")
    if(!grid::is.unit(u)) return(u)
    if(is.atomic(u)){
      if("null" %in% attr(u, "unit")){
        d = attr(u, "data")
        u = unit(
          x=as.vector(u),
          units=gsub("null", "in", attr(u, "unit")),
          data=d)
      }
      return(u)
    }
    if(inherits(u, "unit.arithmetic")){
      l = .null_as_if_inch(u$arg1)
      r = .null_as_if_inch(u$arg2)
      if(is.null(r)){
        args=list(l)
      }else{
        args=list(l,r)
      }
      return(do.call(u$fname, args))
    }
    if(inherits(u, "unit.list")){
      return(do.call(grid::unit.c, lapply(u, .null_as_if_inch)))
    }
    return(u)
  }

  if(inherits(ggobj, "ggplot") && !isTRUE(ggobj$respect) &&
     is.null(ggobj$theme$aspect.ratio) && is.null(ggobj$coordinates$ratio) &&
     is.null(ggplot2::theme_get()$aspect.ratio)) {
    return(list(height = maxheight, width = maxwidth))
  }

  tmpf = tempfile(pattern = "dispos-a-plot", fileext = ".png")
  png(filename = tmpf,
      height   = maxheight,
      width    = maxwidth,
      units    = units,
      res      = 120, ...)

  on.exit({
    dev.off()
    unlink(tmpf)
  })

  if (inherits(ggobj, "ggplot")) {
    g = ggplot2::ggplotGrob(ggobj)
  } else if (inherits(ggobj, "gtable")) {
    g = ggobj
  } else {
    stop("Don't know how to get sizes for object of class ", deparse(class(ggobj)))
  }

  stopifnot(grid::convertUnit(grid::unit(1, "null"), "in", "x", valueOnly = TRUE) == 0)
  known_ht = sum(grid::convertHeight(g$heights, units, valueOnly = TRUE))
  known_wd = sum(grid::convertWidth(g$widths,   units, valueOnly = TRUE))
  free_ht  = maxheight - known_ht
  free_wd  = maxwidth  - known_wd

  if (packageVersion("grid") >= "4.0.0") {
    null_rowhts <- as.numeric(g$heights[grid::unitType(g$heights) == "null"])
    null_colwds <- as.numeric(g$widths[grid::unitType(g$widths) == "null"])
    panel_asps <- (
      matrix(null_rowhts, ncol = 1)
      %*% matrix(1 / null_colwds, nrow = 1))
  } else {
    all_null_rowhts <- (
      grid::convertHeight(.null_as_if_inch(g$heights), "in", valueOnly = TRUE)
      - grid::convertHeight(g$heights, "in", valueOnly = TRUE))
    all_null_colwds <- (
      grid::convertWidth(.null_as_if_inch(g$widths), "in", valueOnly = TRUE)
      - grid::convertWidth(g$widths, "in", valueOnly = TRUE))
    null_rowhts <- all_null_rowhts[all_null_rowhts > 0]
    null_colwds <- all_null_colwds[all_null_colwds > 0]
    panel_asps <- (matrix(null_rowhts, ncol = 1) %*% matrix(1 / null_colwds, nrow = 1))
  }

  panel_asps = matrix(null_rowhts, ncol = 1) %*% matrix(1 / null_colwds, nrow = 1)
  max_rowhts = free_ht/sum(null_rowhts) * null_rowhts
  max_colwds = free_wd/sum(null_colwds) * null_colwds
  rowhts_if_maxwd = max_colwds[1] * panel_asps[, 1]
  colwds_if_maxht = max_rowhts[1]/panel_asps[1, ]
  height = min(maxheight, known_ht + sum(rowhts_if_maxwd))
  width  = min(maxwidth,  known_wd + sum(colwds_if_maxht))
  return(list(height = height, width = width))
}

#' Get variable units
#' @description Gets variable units. Used within all hisafe plot functions.
#' @param variable A character string of the name of the variable to get units for.
#' @param prof A characgter string of the profile that \code{variable} is from.
#' @keywords internal
get_units <- function(variable, prof) {
  var.unit <- gsub("_[0-9]+", "_X", variable) %>%
    hop_params(quiet = TRUE) %>%
    dplyr::filter(profile == prof) %>%
    .$unit %>%
    gsub(pattern = "\\.", replacement = " ")

  if(length(var.unit) == 0) var.unit <- ""
  if(is.na(var.unit))       var.unit <- ""
  return(var.unit)
}

#' Add trees & canopies to hisafe tile plots
#' @description Adds trees & canopies to hisafe tile plots
#' @param hop An object of class hop.
#' @param tree.data Tree data
#' @param white.boxes White box data
#' @param trees Logical for whether or not to plot trees
#' @param canopies Logical for whether or not to plot canopies
#' @keywords internal
add_trees <- function(plot.obj, tree.data, white.boxes, trees, canopies) {
  if(trees & nrow(tree.data) > 0) {
    plot.obj <- plot.obj +
      geom_point(data  = tree.data,
                 color = "green",
                 size  = 2,
                 na.rm = TRUE,
                 aes(x = x,
                     y = y))
  }

  if(canopies & nrow(tree.data) > 0) {
    package.check <- requireNamespace("ggforce", quietly = TRUE)
    if(package.check) {
      plot.obj <- plot.obj +
        ggforce::geom_ellipse(data  = tree.data,
                               color = "green",
                               size  = 1,
                               aes(linetype = crown.linetype,
                                   x0       = x,
                                   y0       = y,
                                   a        = crownRadiusInterRow,
                                   b        = crownRadiusTreeLine,
                                   angle    = 0),
                               inherit.aes = FALSE,
                               na.rm       = TRUE) +
        geom_rect(data = white.boxes,
                  size = 0,
                  fill = "white",
                  aes(xmin = xmin,
                      xmax = xmax,
                      ymin = ymin,
                      ymax = ymax))
    } else {
      warning("The package 'ggforce' is required for drawing tree conopies. Please install it or set canopies = FALSE.",
              immediate = TRUE)
    }
  }

  return(plot.obj)
}

#' Add phantom trees
#' @description Determines and appends phantom trees to tree data for hisafe tile plots
#' @param tree.data Tree data
#' @keywords internal
add_phantom_trees <- function(tree.data) {
  phantom.data.x <- tree.data %>%
    dplyr::group_by(SimulationName, Date, Year, Month, Day, idTree) %>%
    dplyr::mutate(pos = (x - crownRadiusInterRow) < 0) %>%
    dplyr::mutate(neg = (x + crownRadiusInterRow) > plotWidth) %>%
    dplyr::select(SimulationName, Date, Year, Month, Day, idTree, pos, neg) %>%
    tidyr::gather(key = "side", value = "phantom", pos, neg) %>%
    dplyr::mutate(side = as.numeric(as.character(factor(side, levels = c("neg", "pos"), labels = c("-1", "1"))))) %>%
    dplyr::filter(phantom) %>%
    dplyr::left_join(tree.data, by = c("SimulationName", "Date", "Year", "Month", "Day", "idTree")) %>%
    dplyr::mutate(x = x + plotWidth * side) %>%
    dplyr::select(-side, -phantom) %>%
    dplyr::mutate(crown.linetype = "dotted")

  phantom.data.y <- tree.data %>%
    dplyr::group_by(SimulationName, Date, Year, Month, Day, idTree) %>%
    dplyr::mutate(pos = (y - crownRadiusTreeLine) < 0) %>%
    dplyr::mutate(neg = (y + crownRadiusTreeLine) > plotHeight) %>%
    dplyr::select(SimulationName, Date, Year, Month, Day, idTree, pos, neg) %>%
    tidyr::gather(key = "side", value = "phantom", pos, neg) %>%
    dplyr::mutate(side = as.numeric(as.character(factor(side, levels = c("neg", "pos"), labels = c("-1", "1"))))) %>%
    dplyr::filter(phantom) %>%
    dplyr::left_join(tree.data, by = c("SimulationName", "Date", "Year", "Month", "Day", "idTree")) %>%
    dplyr::mutate(y = y + plotHeight * side) %>%
    dplyr::select(-side, -phantom) %>%
    dplyr::mutate(crown.linetype = "dotted")

  tree.data <- dplyr::bind_rows(tree.data, phantom.data.x, phantom.data.y)

  return(tree.data)
}

#' Generate tree data for hisafe tile plots
#' @description Generates tree data for hisafe tile plots
#' @param hop An object of class hop.
#' @param trees Logical for whether or not to plot trees
#' @param canopies Logical for whether or not to plot canopies
#' @param plot.x The plot.x argument from the tile plot function
#' @keywords internal
create_tree_data <- function(hop, trees, canopies, plot.x) {
  if(!(profile_check(hop, "tree.info") & profile_check(hop, "plot.info"))) warning("tree.info or plot.info is unavilable in hop and is required if trees or canopies is TRUE. Use read.inputs = TRUE in read_hisafe().", call. = FALSE)
  if((trees | canopies) & profile_check(hop, "tree.info") & profile_check(hop, "plot.info")){
    tree.data <- hop$tree.info %>%
      dplyr::left_join(hop$plot.info, by = "SimulationName") %>%
      dplyr::mutate(special.case = x == 0 & y == 0) %>% # special case when x == 0 & y == 0 : tree is at scene center
      dplyr::mutate(x = x + special.case * plotWidth  / 2) %>%
      dplyr::mutate(y = y + special.case * plotHeight / 2) %>%
      dplyr::select(SimulationName, idTree, species, x, y, plotWidth, plotHeight, cellWidth)

    if(canopies) {
      profile_check(hop,  "trees", error = TRUE)
      variable_check(hop, "trees", c("crownRadiusInterRow", "crownRadiusTreeLine"), error = TRUE)
      if(plot.x == "y") hop$trees <- swap_cols(hop$trees, "crownRadiusInterRow", "crownRadiusTreeLine")
      tree.data <- hop$trees %>%
        dplyr::select(SimulationName, Date, Year, Month, Day, idTree, crownRadiusInterRow, crownRadiusTreeLine) %>%
        dplyr::left_join(tree.data, by = c("SimulationName", "idTree")) %>%
        dplyr::mutate(crown.linetype = "solid") %>% # for non-phantom trees
        add_phantom_trees()
    }
  } else {
    tree.data <- dplyr::tibble()
  }
  return(tree.data)
}

#' Build white boxes to cover phantom trees
#' @description Builds white boxes to cover phantom trees for hisafe tile plot functions
#' @param hop An object of class hop.
#' @param X.MIN Lower x limit for plot.
#' @param X.MAX Upper x limit for plot.
#' @param Y.MIN Lower y limit for plot.
#' @param Y.MAX Upper y limit for plot.
#' @keywords internal
build_white_boxes_tile <- function(hop, X.MIN, X.MAX, Y.MIN, Y.MAX) {
  boxes <- hop$plot.info %>%
    dplyr::select(SimulationName, plotWidth, plotHeight) %>%
    dplyr::filter(plotWidth < max(plotWidth) | plotHeight < max(plotHeight))

  y.pos.box <- boxes %>%
    dplyr::mutate(xmin = X.MIN,
                  xmax = X.MAX,
                  ymin = plotHeight,
                  ymax = Y.MAX)
  y.neg.box <- boxes %>%
    dplyr::mutate(xmin = X.MIN,
                  xmax = X.MAX,
                  ymin = Y.MIN,
                  ymax = 0)
  x.pos.box <- boxes %>%
    dplyr::mutate(xmin = plotWidth,
                  xmax = X.MAX,
                  ymin = Y.MIN,
                  ymax = Y.MAX)
  x.neg.box <- boxes %>%
    dplyr::mutate(xmin = X.MIN,
                  xmax = 0,
                  ymin = Y.MIN,
                  ymax = Y.MAX)

  white.boxes <- dplyr::bind_rows(y.pos.box, y.neg.box, x.pos.box, x.neg.box) %>%
    dplyr::filter(SimulationName %in% boxes$SimulationName)

  return(white.boxes)
}

#' Generate tree data for hisafe tile plots
#' @description Generates tree data for hisafe tile plots
#' @param hop An object of class hop.
#' @param profile Character string of the profile to pull
#' @param cellWidth Width of cells
#' @keywords internal
create_tile_data <- function(hop, profile, cellWidth) {
  plot.data <- hop[[profile]] %>%
    dplyr::mutate(xmax = x + cellWidth) %>%
    dplyr::mutate(ymax = y + cellWidth)
  if(nrow(plot.data) == 0) stop("hop filtering resulted in no remaining data to plot", call. = FALSE)
  return(plot.data)
}

#' Add labels to plot panels
#' @description Adds labels to upper-right corne of plot panels
#' @param plots A list of ggplot objects
#' @param labels A character vector of labels to use
#' @keywords internal
annotator <- function(plots, labels = paste0("(", letters, ")")) {
  if(!requireNamespace("ggalt", quietly = TRUE)) stop("The package 'ggalt' is required to add plot labels. Please install and load it.", call. = FALSE)
  label <- function(i) ggalt::annotate_textp(x = 0.01, y = 0.99, label = i, size = 15, fontface = 2)
  for(i in 1:length(plots)) {
    plots[[i]] <- plots[[i]] +
      label(labels[i])
  }
  return(plots)
}
