#' Plot timeseries of Hi-sAFe output variable
#' @description Plots a daily or annual timeseries of a single Hi-sAFe output variable.
#' @return A ggplot object. If the data is of class \code{hisafe-group} and contains data from more than one
#' Hi-sAFe simulation, the plot will contain multiple lines, colored and labeled by SimName. If the data
#' contains two more tree ids, the plot will be faceted by tree id.
#' @param data An object of class \code{hisafe} or \code{hisafe-group} containing output data from one or more Hi-sAFe simulations.
#' @param variable A character string of the name of the variable to plot.
#' @param time.class If 'Annual', an annual timeseries is created. If 'Daily', a daily timeseries is created.
#' @export
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' mydata <- read_hisafe_output("MySimulation", "./")
#'
#' # You can create an annual timeseries of carbonCoarseRoots:
#' annual.plot <- plot_hisafe_ts(mydata, "carbonCoarseRoots", "Annual")
#'
#' # For a daily timeseries instead:
#' daily.plot <- plot_hisafe_ts(mydata, "carbonCoarseRoots", "Daily")
#'
#' # Once you have the plot object, you can display it and save it:
#' annual.plot
#' ggsave("annual_carbonCoarseRoots.png", annual.plot)
#' }
plot_hisafe_ts <- function(data, variable, time.class = "Annual") {
  time.class <- stringr::str_to_title(time.class)

  ## Check if data has class hisafe or hisafe-group
  if(!any(c("hisafe", "hisafe-group") %in% class(data))) {
    stop("data not of class hisafe or hisafe-group")
  }

  ## Color blind-friendly palette
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  ## Exract units of supplied variable from the "variables" slot
  var.unit <- data$variables %>% filter(VariableClass == time.class, VariableName == variable) %>% .$Units

  ## Create time.class specific x aesthetic and axis label
  if(time.class == "Annual"){
    x.var <- "Year0"
    x.label <- "Years after establishment"
    plot.data <- data$annual
    plot.data <- plot.data %>% mutate(Year0 = Year - min(Year)) # Create 0+ year values
    theme_hisafe_ts <- theme_hisafe_annual
    scale_x_ts <- scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL))
  } else {
    x.var <- "Date"
    x.label <- "Date"
    plot.data <- data$daily
    theme_hisafe_ts <- theme_hisafe_daily
    scale_x_ts <- scale_x_date()
  }

  ## If number of trees in scene is > 1, then facet by tree id
  if(length(unique(plot.data$id)) == 1) {
    facet_annual <- geom_blank()
  } else {
    plot.data <- plot.data %>% mutate(id = paste("Tree", id))
    facet_annual <- facet_wrap(~id, nrow = 1)
  }

  ## Pad SimulationName for legend clarity (until bug in legend.text response to margin is fixed)
  levels(plot.data$SimName) <- paste0(levels(plot.data$SimName), "     ")

  ## Create plot
  ts.plot <- ggplot(plot.data, aes_string(x = x.var, y = variable, color = "SimName")) +
    labs(x = x.label,
         y = paste0(variable, " (", var.unit, ")"),
         title = variable) +
    facet_annual +
    scale_x_ts +
    scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
    geom_line(size = 1, na.rm = TRUE) +
    scale_color_manual(values = rep(cbPalette, 10),
                       guide = guide_legend(ncol = 3, byrow = TRUE)) +
    theme_hisafe_ts()

  return(ts.plot)
}

#' Plot timeseries diagnostics of Hi-sAFe output
#' @description Plots a daily or annual timeseries of every Hi-sAFe output variable. All plots are saved as
#' png files to a specifified output path.
#' @return A list of \code{ggplot} objects is invisibly returned. If the data is of class \code{hisafe-group} and contains
#' data from more than one Hi-sAFe simulation, the plots will contain multiple lines, colored and labeled by SimName.
#' If the data contains two more tree ids, the plots will be faceted by tree id.
#' @param data An object of class \code{hisafe} or \code{hisafe-group} containing output data from one or more Hi-sAFe simulations.
#' @param time.class If 'Annual', annual timeseries are created. If 'Daily', daily timeseries are created.
#' @param output.path A character stting indicating the path to the directory where plots should be saved. Plots are
#' saved in a subdirectory within this directory named by \code{time.class}.
#' @export
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' mydata <- read_hisafe_output("MySimulation", "./")
#'
#' # You can create an annual timeseries of every variable:
#' diag_hisafe_ts(mydata)
#'
#' # For aily timeseries instead:
#' diag_hisafe_ts(mydata, "Daily")
#' }
diag_hisafe_ts <- function(data, time.class = "Annual", output.path = "./diagnostics") {
  time.class <- stringr::str_to_title(time.class) # prevents error if proper capitalization not input by user
  ts.path <- gsub("//", "/", paste0(output.path, "/", stringr::str_to_lower(time.class), "/"), fixed = TRUE)
  dir.create(ts.path, recursive = TRUE, showWarnings = FALSE)

  ## Clean columns & extract names of variables to plot
  # cols with only "error!" output are all NA's and cause plot errors
  if(time.class == "Annual") {
    data$annual <- data$annual %>% select_if(~sum(!is.na(.)) > 0)
    var.names <- names(data$annual)[(which(names(data$annual) == "id") + 1):length(names(data$annual))]
  } else {
    data$daily <- data$daily %>% select_if(~sum(!is.na(.)) > 0)
    var.names <- names(data$daily)[(which(names(data$daily) == "id") + 1):length(names(data$daily))]
  }

  ## Create plots
  plot.list <- map(var.names, plot_hisafe_ts, data = data, time.class = time.class)

  ## Write plots to disk
  file.names <- paste0(var.names, ".png")
  pwalk(list(file.names, plot.list), ggsave, path = ts.path, width = 7, height = 7)

  ## Invisibly return list of plot objects
  invisible(plot.list)
}
