#' Read from a Hi-sAFe output profile
#' @description Customized read.table call to read from a Hi-sAFe output profile.
#' @return A data frame.
#' @param file A character string of the path to the file to be read.
#' @param ... Any other arguements passed to \code{read.table}
read_table_hisafe <- function(file, ...) {
  # using read.table rather readr::read_table because read_table is not working
  as.tibble(read.table(file,
                       header = TRUE,
                       sep = "\t",
                       stringsAsFactors = FALSE,
                       na.strings = c("NA", "error!"), # "error!" is output by HISAFE & causes table merge errors if left
                       encoding = "latin1", ...))
}

#' Read a single Hi-sAFe output profile
#' @description Reads the designated output profiles from a single Hi-sAFe simulation.
#' @return An list of two data frames: \code{data} contains the data from the profile; \code{variables} contains the variable descriptions.
#' @param profile A character string of the path to the profile to be read.
#' @param read.data If TRUE, data and variable descriptions are read. If FALSE, only variable descriptions are read.
read_hisafe_output_file <- function(profile, read.data = TRUE){

  ## Read raw text & find break between description & data
  raw.text <- readLines(profile)
  end.of.var.list <- which(raw.text[-1] == "")[1]

  ## Read variable descriptions
  variables <- read_table_hisafe(profile, skip = 7, nrows = end.of.var.list - 8) # always 7 header rows at start of each profile

  ## Read data
  if (read.data) {
    dat <- read_table_hisafe(profile, skip = end.of.var.list) %>%
      filter(Year != 0) %>%                    # remove row with Year==0 at the start of every output profile
      mutate(Date = gsub(pattern = "a.", replacement = "", x = Date, fixed = TRUE)) %>% # is this here to fix an old bug?
      mutate(Date = lubridate::dmy(Date))                 # convert Date column into date class
  } else dat <- tibble()                       # if read.data==F, return empty tibble (i.e. only read variable descriptions)

  return(list(data = dat, variables = variables))
}

#' Read output from a single Hi-sAFe simulation
#' @description Reads the designated output profiles from a single Hi-sAFe simulation.
#' @return An object of class \code{hop}. This is a list of four data frames:
#' \code{annual}, \code{daily}, \code{spatial} (monthly cell data), and \code{variables} (variable descriptions and units).
#' @param simu.name The \code{SimulationName} of the Hi-sAFe simulation. This must be the same as the name of the Hi-sAFe simulation folder.
#' @param folder A character string of the path to the directory containing the Hi-sAFe simulation folder (which contains the standard subdirectory with the output)
#' @param profiles A character vector of the names of Hi-sAFe output profiles to read. Defaults to reading the core Hi-sAFe output profiles.
#' @export
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' mydata <- read_hisafe("MySimulation", "./")
#'
#' # If only the annual tree data is required:
#' mytreedata <- read_hisafe("MySimulation", "./", profiles = "annualtree")
#' }
read_hisafe <- function(simu.name, folder, profiles = c("annualtree", "annualplot", "trees", "plot", "monthCells")) {

  file.prefix <- paste0(folder, "/" , simu.name, "/output-", simu.name, ".sim", "/", simu.name, "_")
  join.cols <- c("SimulationName",             # common columns at the start of all timeseries (Annual, Daily) profiles
                 "Date",
                 "Day",
                 "Month",
                 "Year",
                 "JulianDay",
                 "stepNum")

  ## Function for reading timeseries (Annual, Daily) profiles
  read.ts.profiles <- function(profiles, time.class) {
    if(length(profiles) >= 1) {

      ## Read in all profiles
      dat.list <- map(profiles, function(x) read_hisafe_output_file(paste0(file.prefix, x, ".txt" )))

      ## Extract & tidy data
      dat.data <- dat.list %>%
        map(1) %>%                             # extract first element (data) of each profile
        reduce(full_join, by = join.cols) %>%  # bind all variable descriptions togeter
        mutate_if(is.logical, as.numeric)      # columns read as logical must be coered to numeric to prevent plotting errors

      ## Extract & tidy variable definitions
      dat.variables <- dat.list %>%
        map(2) %>%                             # extract second element (variable descriptions) of each profile
        bind_rows() %>%                        # bind all variable descriptions togeter
        mutate(VariableClass = time.class)     # add a column that indicates which data class the variable definition is from
    }else {
      dat.data <- dat.variables <- tibble()    # if no profiles were requested from this data class, return empty tibbles
    }

    return(list(data = dat.data, variables = dat.variables))
  }

  ## Read annual data & associated variables
  annual.profiles <- profiles[profiles %in% c("annualtree", "annualplot")]
  annual.dat <- read.ts.profiles(annual.profiles, "Annual")
  annual.data <- annual.dat$data
  annual.variables <- annual.dat$variables

  ## Read daily data & associated variables
  daily.profiles <- profiles[profiles %in% c("trees", "plot")]
  daily.dat <- read.ts.profiles(daily.profiles, "Daily")
  daily.data <- daily.dat$data
  daily.variables <- daily.dat$variables

  ## Read spatial data & associated variables
  spatial.profiles <- profiles[profiles %in% c("monthCells")]
  if(length(spatial.profiles) >= 1) {
    spatial.list <- map(spatial.profiles, function(x) read_hisafe_output_file(paste0(file.prefix, x, ".txt" )))
    spatial.data <- spatial.list[[1]]$data
    spatial.variables <- spatial.list[[1]]$variables %>% mutate(VariableClass = "Spatial")
  } else {
    spatial.data <- spatial.variables <- tibble()
  }

  ## Combine variables into one tibble
  variables <- bind_rows(annual.variables, daily.variables, spatial.variables) %>%
    distinct()                                # remove all duplicate variable definitions
  names(variables) <- c("Subject",            # rename col headers bc different HISAFE versions used English/French
                        "SubjectId",
                        "VariableName",
                        "Units",
                        "Description",
                        "VariableClass")

  ## Creat output list & assign class
  output <- list(annual = annual.data, daily = daily.data, spatial = spatial.data, variables = variables)
  class(output)<-c("hop", class(output))

  return(output)
}

#' Read output from a group of Hi-sAFe simulations
#' @description Reads the designated output profiles from a group of Hi-sAFe simulations (i.e. an experiment).
#' @return An object of class \code{hop-group}. This is a list of five data frames:
#' \code{annual}, \code{daily}, \code{spatial} (monthly cell data), \code{variables} (variable descriptions and units),
#' and \code{exp.plan} (the provided experimental plan). Data frames will be empty for any data classes not included in the specified profiles
#' @param exp.plan A data frame containing the experimental plan used to generate the Hi-sAFe simulations. To create an experimental plan, see \code{\link{define_exp}}.
#' @param folder A character string of the path to the directory containing the Hi-sAFe simulation folders (which each contain the standard subdirectories with the outputs)
#' @param profiles A character vector of the names of Hi-sAFe output profiles to read. Defaults to reading the core Hi-sAFe output profiles.
#' @export
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' myexp <- read_hisafe_group(MyExpPlan, "./")
#'
#' # If only the annual tree data is required:
#' mytreeexp <- read_hisafe(MyExpPlan, "./", profiles = "annualtree")
#' }
read_hisafe_group <- function(exp.plan, folder, profiles = c("annualtree", "annualplot", "trees", "plot", "monthCells")) {

  ## Read all data from all simulations & combine
  data <- map(exp.plan$SimulationName, function(x) read_hisafe(x, folder, profiles)) %>%
    pmap(bind_rows) # a more generic version of this line that handles cases where the number
  # of elements and order of names in each sublist can vary is:
  # map(map_df(data, ~ as.data.frame(map(.x, ~ unname(nest(.))))), bind_rows)
  # However, this isn't necessary becasue read_hisafe_output always produces
  # identically sized and named lists.

  ## Tidy up data
  timeseries_tidy <- function(x){
    left_join(exp.plan, x, by = "SimulationName") %>%   # add exp.plan cols to annual data
      mutate_at(names(exp.plan), factor) %>%            # make exp.plan cols factors
      group_by(SimulationName)                          # group annual data by simulaton
  }
  data$annual <- timeseries_tidy(data$annual)
  data$daily <- timeseries_tidy(data$daily)
  data$variables <- data$variables %>% distinct()       # remove duplicate variable descriptions
  data$exp.plan <- exp.plan %>% mutate_all(factor)      # make all columns factors

  ## Warn if lengths of all simulations are not equal
  year.summary <- data$annual %>%
    summarize(n = n()) %>%
    unite(label, SimulationName, n, sep = ": ", remove = FALSE)
  if(length(unique(year.summary$n)) != 1) {
    year.length.warning <- paste(c("Simulation lengths not equal!",
                                   "Be careful when comparing end-of-simulation results.",
                                   "Simulation lengths:",
                                   year.summary$label),
                                 collapse = "\n")
    warning(year.length.warning, call. = FALSE)
  }

  ## Assign class
  class(data)<-c("hop-group", "hop", class(data)) # "hisafe"

  return(data)
}
