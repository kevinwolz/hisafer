#' Read from a Hi-sAFe output profile
#' @description Customized read.table call to read from a Hi-sAFe output profile.
#' @return A data frame.
#' @param file A character string of the path to the file to be read.
#' @param ... Any other arguements passed to \code{read.table}
#' @import tidyverse
read_table_hisafe <- function(file, ...) {
  # using read.table rather readr::read_table because read_table is not working
  tibble::as.tibble(read.table(file,
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
#' @import tidyverse
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
#' @return An object of class \code{hop}. This is a list of 6 data frames:
#' \code{annual} (includes data from annualtree and annualplot profiles), \code{daily} (includes data from trees, plot, and climate profiles), \code{monthCells}, \code{cells}, \code{voxels}, and \code{variables} (variable descriptions and units from all profiles).
#' @param simu.name The \code{SimulationName} of the Hi-sAFe simulation. This must be the same as the name of the Hi-sAFe simulation folder.
#' @param path A character string of the path to the directory containing the Hi-sAFe simulation folder (which contains the standard subdirectory with the output)
#' @param profiles A character vector of the names of Hi-sAFe output profiles to read. Defaults to reading all supported Hi-sAFe output profiles via "all". Currently supported profiles are: annualtree, annualplot, trees, plot, monthCells, cells, voxels, climate.
#' @param allow.missing If \code{TRUE}, does not produce error when profiles specified by \code{profiles} are not found in the output path.
#' It is highly discouraged to change this from the default of \code{FALSE} unless there are specific profiles intentionally missing from some simulations.
#' @export
#' @import tidyverse
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' mydata <- read_hisafe("MySimulation", "./")
#'
#' # If only the annual tree data is required:
#' mytreedata <- read_hisafe("MySimulation", "./", profiles = "annualtree")
#' }
read_hisafe <- function(simu.name, path, profiles = "all", allow.missing = FALSE) {

  supported.profiles <- c("annualtree", "annualplot", "trees", "plot", "monthCells", "cells", "voxels", "climate")
  if(profiles == "all") profiles <- supported.profiles

  ## Create profile paths
  file.prefix <- gsub("//", "/", paste0(path, "/" , simu.name, "/output-", simu.name, ".sim", "/", simu.name, "_"))
  files <- paste0(file.prefix, profiles, ".txt" )

  ## Check for existence of all requested profiles and throw error if profile does not exist
  missing <- !all(file.exists(files))
  if(missing) {
    missing.files <- files[!file.exists(files)]
    missing.profiles <- purrr::map_chr(strsplit(missing.files, "/"), tail, n = 1)
    missing.profile.error <- paste(c("The following profiles do not exist:",
                                     missing.profiles),
                                   collapse = "\n")
  }
  if(missing & !allow.missing) {
    stop(missing.profile.error)
  } else if(missing & allow.missing) {
    warning(missing.profile.error)
    profiles <- profiles[file.exists(files)]
  }

  ## Function for reading timeseries (Annual, Daily) profiles
  read.ts.profiles <- function(profiles, time.class) {

    ## Common columns at the start of all Annual & Daily profiles
    join.cols <- c("SimulationName",
                   "Date",
                   "Day",
                   "Month",
                   "Year",
                   "JulianDay",
                   "stepNum")

    if(length(profiles) >= 1) {

      ## Read in all profiles
      dat.list <- purrr::map(profiles, function(x) read_hisafe_output_file(paste0(file.prefix, x, ".txt" )))

      ## Extract & tidy data
      dat.data <- dat.list %>%
        purrr::map(1) %>%                      # extract first element (data) of each profile
        reduce(full_join, by = join.cols) %>%  # bind all variable descriptions togeter
        mutate_if(is.logical, as.numeric)      # columns read as logical must be coered to numeric to prevent plotting errors

      ## Extract & tidy variable definitions
      dat.variables <- dat.list %>%
        purrr::map(2) %>%                      # extract second element (variable descriptions) of each profile
        bind_rows() %>%                        # bind all variable descriptions togeter
        mutate(VariableClass = time.class)     # add a column that indicates which data class the variable definition is from
    }else {
      dat.data <- dat.variables <- tibble()    # if no profiles were requested from this data class, return empty tibbles
    }

    return(list(data = dat.data, variables = dat.variables))
  }

  ## Read annual data & associated variables
  annual.profiles <- profiles[profiles %in% c("annualtree", "annualplot")]
  annual.dat <- read.ts.profiles(annual.profiles, "annual")
  annual.data <- annual.dat$data
  annual.variables <- annual.dat$variables

  ## Read daily data & associated variables
  daily.profiles <- profiles[profiles %in% c("trees", "plot", "climate")]
  daily.dat <- read.ts.profiles(daily.profiles, "daily")
  daily.data <- daily.dat$data
  daily.variables <- daily.dat$variables

  ## Read monthCells data & associated variables
  monthCells.profiles <- profiles[profiles %in% c("monthCells")]
  if(length(monthCells.profiles) >= 1) {
    monthCells.list <- purrr::map(monthCells.profiles, function(x) read_hisafe_output_file(paste0(file.prefix, x, ".txt" )))
    monthCells.data <- monthCells.list[[1]]$data
    monthCells.variables <- monthCells.list[[1]]$variables %>% mutate(VariableClass = "monthCells")
  } else {
    monthCells.data <- monthCells.variables <- tibble()
  }

  ## Read cells data & associated variables
  cells.profiles <- profiles[profiles %in% c("cells")]
  if(length(cells.profiles) >= 1) {
    cells.list <- purrr::map(cells.profiles, function(x) read_hisafe_output_file(paste0(file.prefix, x, ".txt" )))
    cells.data <- cells.list[[1]]$data
    cells.variables <- cells.list[[1]]$variables %>% mutate(VariableClass = "cells")
  } else {
    cells.data <- cells.variables <- tibble()
  }

  ## Read cells data & associated variables
  voxels.profiles <- profiles[profiles %in% c("voxels")]
  if(length(voxels.profiles) >= 1) {
    voxels.list <- purrr::map(voxels.profiles, function(x) read_hisafe_output_file(paste0(file.prefix, x, ".txt" )))
    voxels.data <- voxels.list[[1]]$data
    voxels.variables <- voxels.list[[1]]$variables %>% mutate(VariableClass = "voxels")
  } else {
    voxels.data <- voxels.variables <- tibble()
  }

  ## Combine variables into one tibble
  variables <- bind_rows(annual.variables, daily.variables, monthCells.variables, cells.variables, voxels.variables) %>%
    distinct()                                # remove all duplicate variable definitions
  names(variables) <- c("Subject",            # rename col headers bc different HISAFE versions used English/French
                        "SubjectId",
                        "VariableName",
                        "Units",
                        "Description",
                        "VariableClass")

  ## Creat output list & assign class
  output <- list(annual = annual.data,
                 daily = daily.data,
                 monthCells = monthCells.data,
                 cells = cells.data,
                 voxels = voxels.data,
                 variables = variables)
  class(output)<-c("hop", class(output))

  return(output)
}

#' Read output from a group of Hi-sAFe simulations
#' @description Reads the designated output profiles from a group of Hi-sAFe simulations (i.e. an experiment).
#' @return An object of class \code{hop-group}. This is a list of 7 data frames:
#' \code{annual} (includes data from annualtree and annualplot profiles), \code{daily} (includes data from trees, plot, and climate profiles),
#' \code{monthCells}, \code{cells}, \code{voxels}, and \code{variables} (variable descriptions and units from all profiles).
#' @param exp.plan A data frame containing the experimental plan used to generate the Hi-sAFe simulations.
#' To create an experimental plan, see \code{\link{define_exp}}.
#' @param path A character string of the path to the directory containing the Hi-sAFe simulation folders (which each contain the standard subdirectories with the outputs)
#' @param profiles A character vector of the names of Hi-sAFe output profiles to read. Defaults to reading all supported Hi-sAFe output profiles via "all". Currently supported profiles are: annualtree, annualplot, trees, plot, monthCells, cells, voxels, climate.
#' @param allow.missing If \code{TRUE}, does not produce error when profiles specified by \code{profiles} are not found in the output path.
#' It is highly discouraged to change this from the default of \code{FALSE} unless there are specific profiles intentionally missing from some simulations.
#' @export
#' @import tidyverse
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' myexp <- read_hisafe_group(MyExpPlan, "./")
#'
#' # If only the annual tree data is required:
#' mytreeexp <- read_hisafe(MyExpPlan, "./", profiles = "annualtree")
#' }
read_hisafe_group <- function(exp.plan, path, profiles = "all", allow.missing = FALSE) {

  ## Read all data from all simulations & combine
  data <- purrr::map(exp.plan$SimulationName, read_hisafe, path = path, profiles = profiles, allow.missing = allow.missing) %>%
    purrr::pmap(bind_rows) # a more generic version of this line that handles cases where the number
  # of elements and order of names in each sublist can vary is:
  # purrr::map(map_df(data, ~ as.data.frame(purrr::map(.x, ~ unname(nest(.))))), bind_rows)
  # However, this isn't necessary becasue read_hisafe_output always produces
  # identically sized and named lists.

  ## Tidy up data
  data_tidy <- function(x){
    left_join(exp.plan, x, by = "SimulationName") %>%   # add exp.plan cols to annual data
      mutate_at(names(exp.plan), factor) %>%            # make exp.plan cols factors
      group_by(SimulationName)                          # group annual data by simulaton
  }
  if(ncol(data$annual) > 0) data$annual <- data_tidy(data$annual)
  if(ncol(data$daily) > 0) data$daily <- data_tidy(data$daily)
  if(ncol(data$cells) > 0) data$cells <- data_tidy(data$cells)
  if(ncol(data$voxels) > 0) data$voxels <- data_tidy(data$voxels)
  data$variables <- data$variables %>% distinct()       # remove duplicate variable descriptions
  data$exp.plan <- exp.plan %>% mutate_all(factor)      # make all columns factors

  ## Warn if lengths of all simulations are not equal
  year.summary <- data[[as.numeric(which.max(map_int(data, ncol)))]] %>%
    summarize(n = n_distinct(Year)) %>%
    unite(label, SimulationName, n, sep = ": ", remove = FALSE)
  if(length(unique(year.summary$n)) != 1) {
    year.length.warning <- paste(c("Simulation lengths not equal!",
                                   "Be careful when comparing simulations.",
                                   "Simulation lengths:",
                                   year.summary$label),
                                 collapse = "\n")
    warning(year.length.warning, call. = FALSE)
  }

  ## Assign class
  class(data)<-c("hop-group", "hop", class(data)) # "hisafe"

  return(data)
}
