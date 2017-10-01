### R Tools for Hi-sAFe - PLOTTING OUTPUT
### Programmer: Kevin Wolz
### Originally Created: 30 Sep 2017
### Last Updated: 01 Oct 2017

##### TIMESERIES PLOTS #####
plot_hisafe_ts <- function(df, variable, time.class = "Annual") {
  time.class <- str_to_title(time.class)

  ## Check if df has class hisafe or hisafe-group
  if(!any(c("hisafe", "hisafe-group") %in% class(df))) {
    stop("df not of class hisafe or hisafe-group")
  }

  ## Color blind-friendly color palette
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  ## Exract units of supplied variable from the "variables" slot
  var.unit <- df$variables %>% filter(VariableClass == time.class, VariableName == variable) %>% .$Units

  ## Create time.class specific x aesthetic and axis label
  if(time.class == "Annual"){
    x.var <- "Year0"
    x.label <- "Years after establishment"
    plot.df <- df$annual
    plot.df <- plot.df %>% mutate(Year0 = Year - min(Year)) ## Create 0+ year values
    theme_hisafe_ts <- theme_hisafe_annual
    scale_x_ts <- scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL))
  } else {
    x.var <- "Date"
    x.label <- "Date"
    plot.df <- df$daily
    theme_hisafe_ts <- theme_hisafe_daily
    scale_x_ts <- scale_x_date() # sec.axis = sec_axis(~ ., labels = NULL))
  }

  ## If number of trees in scene is > 1, then facet by tree id
  if(length(unique(plot.df$id)) == 1) {
    facet_annual <- geom_blank()
  } else {
    plot.df <- plot.df %>% mutate(id = paste("Tree", id))
    facet_annual <- facet_wrap(~id, nrow = 1)
  }

  ## Pad SimulationName for legend clarity (until bug in legend.text response to margin is fixed)
  levels(plot.df$SimName) <- paste0(levels(plot.df$SimName), "     ")

  ## Create plot
  ts.plot <- ggplot(plot.df, aes_string(x = x.var, y = variable, color = "SimName")) +
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

diag_hisafe_ts <- function(df, time.class = "Annual", output.path = "./diagnostics") {
  time.class <- str_to_title(time.class) # prevents error if proper capitalization not input by user
  ts.path <- gsub("//", "/", paste0(output.path, "/", str_to_lower(time.class), "/"), fixed = TRUE)
  dir.create(ts.path, recursive = TRUE, showWarnings = FALSE)

  ## Clean columns & extract names of variables to plot
  # cols with only "error!" output are all NA's and cause plot errors
  if(time.class == "Annual") {
    df$annual <- df$annual %>% select_if(~sum(!is.na(.)) > 0)
    var.names <- names(df$annual)[(which(names(df$annual) == "id") + 1):length(names(df$annual))]
  } else {
    df$daily <- df$daily %>% select_if(~sum(!is.na(.)) > 0)
    var.names <- names(df$daily)[(which(names(df$daily) == "id") + 1):length(names(df$daily))]
  }

  ## Create plots
  plot.list <- map(var.names, plot_hisafe_ts, df = df, time.class = time.class)

  ## Write plots to disk
  file.names <- paste0(var.names, ".png")
  pwalk(list(file.names, plot.list), ggsave, path = ts.path, width = 7, height = 7)

  ## Invisibly return list of plot objects
  invisible(plot.list)
}

##### SPATIAL PLOT #####

