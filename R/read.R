#' Read output from a Hi-sAFe experiment
#' @description Reads the designated output profiles from a Hi-sAFe experiiment (i.e. a group of Hi-sAFe simulations).
#' @return An object of class \code{hop-group}. This is a list of 11 data frames (tibbles):
#' \code{annual} (includes data from annualtree and annualplot profiles), \code{daily} (includes data from trees, plot, and climate profiles), \code{annualcrop},
#' \code{roots}, \code{monthCells}, \code{cells}, \code{voxels}, \code{variables} (variable descriptions and units from all profiles),
#' \code{inputs} (the hip object that generated the simulation), and \code{path} (the path to the simulation folder).
#' @param hip An object of class hip. To create a hip object see \code{\link{define_exp}}.
#' If the hip object contains a \code{path} value, then this can be used without providing \code{path} directly to \code{read_hisafe_exp}.
#' If \code{hip} is not provided, then \code{path} is required and the input data for the experiment is read from the experiment
#' summary .csv file created when building the experiment.
#' @param path A character string of the path to the directory containing the Hi-sAFe simulation folders
#' (which each contain the standard subdirectories with the outputs).
#' If \code{hip} is not provided, then \code{path} is required. If both \code{hip} and \code{path} are provided, \code{path} is used.
#' @param profiles A character vector of the names of Hi-sAFe output profiles to read. Defaults to reading all supported Hi-sAFe output profiles via "all".
#' Currently supported profiles are: annualplot, annualtree, annualcrop, plot, trees, roots, cells, voxels, climate, monthCells.
#' @export
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' myexp <- read_hisafe_exp(MyExpPlan, "./")
#'
#' # If only the annual tree data is required:
#' mytreeexp <- read_hisafe(MyExpPlan, "./", profiles = "annualtree")
#' }
read_hisafe_exp <- function(hip           = NULL,
                            path          = NULL,
                            profiles      = "all",
                            allow.missing = FALSE) {

  if(is.null(hip) & is.null(path))        stop("must provide at least one of hip or path")
  if(is.null(path)) path <- hip$path

  if(!is.null(hip)) {
    exp.plan <- hip
  } else {
    exp.name <- tail(strsplit(path, "/"), n = 1)
    exp.plan <- tibble::as_tibble(read.csv(gsub("//", "/", paste0(path, "/", exp.name, "_summary.csv")), header = TRUE, stringsAsFactors = FALSE))
  }

  is.unique <- function(x) { length(unique(x)) != 1 }
  exp.plan <- exp.plan[, purrr::map_lgl(exp.plan, is.unique)] %>%
    dplyr::mutate_all(factor)

  ## Read all data from all simulations & combine
  data <- purrr::map(exp.plan$SimulationName, read_hisafe, path = path, profiles = profiles) %>%
    purrr::pmap(dplyr::bind_rows) # a more generic version of this line that handles cases where the number
  # of elements and order of names in each sublist can vary is:
  # purrr::map(map_df(data, ~ as.data.frame(purrr::map(.x, ~ unname(nest(.))))), bind_rows)
  # However, this isn't necessary becasue read_hisafe_output always produces
  # identically sized and named lists.

  ## Tidy up data
  data_tidy <- function(x){
    dplyr::left_join(exp.plan, x, by = "SimulationName") %>%   # add exp.plan cols to annual data
      dplyr::group_by(SimulationName)                          # group annual data by simulaton
  }
  if(ncol(data$annual) > 0) data$annual <- data_tidy(data$annual)
  if(ncol(data$daily)  > 0) data$daily  <- data_tidy(data$daily)
  if(ncol(data$cells)  > 0) data$cells  <- data_tidy(data$cells)
  if(ncol(data$voxels) > 0) data$voxels <- data_tidy(data$voxels)
  data$variables <- dplyr::distinct(data$variables)            # remove duplicate variable descriptions
  data$exp.plan  <- dplyr::mutate_all(exp.plan, factor)        # make all columns factors

  ## Warn if lengths of all simulations are not equal
  year.summary <- data[[as.numeric(which.max(purrr::map_int(data, ncol)))]] %>%
    dplyr::summarize(n = dplyr::n_distinct(Year)) %>%
    tidyr::unite(label, SimulationName, n, sep = ": ", remove = FALSE)
  if(length(unique(year.summary$n)) != 1) {
    year.length.warning <- paste(c("Simulation lengths not equal!",
                                   "Be careful when comparing simulations.",
                                   "Simulation lengths:",
                                   year.summary$label),
                                 collapse = "\n")
    warning(year.length.warning, call. = FALSE)
  }

  class(data)<-c("hop-group", "hop", class(data))
  return(data)
}

#' Read output from a single Hi-sAFe simulation
#' @description Reads the designated output profiles from a single Hi-sAFe simulation.
#' @return An object of class \code{hop}. This is a list of 10 data frames (tibbles):
#' \code{annual} (includes data from annualtree and annualplot profiles), \code{daily} (includes data from trees, plot, and climate profiles), \code{annualcrop},
#' \code{roots}, \code{monthCells}, \code{cells}, \code{voxels}, \code{variables} (variable descriptions and units from all profiles),
#' \code{inputs} (the hip object that generated the simulation), and \code{path} (the path to the simulation folder).
#' @param hip An object of class \code{hip}. To create a hip object see \code{\link{define_exp}}.
#' If object contains a \code{path} value, then this can be used without providing \code{path} directly to \code{read_hisafe}.
#' Cannot provided both \code{hip} and \code{simu.name}.
#' If \code{hip} is not provided, then \code{path} is required and the input data for the experiment is read from the experiment
#' summary .csv file created when building the experiment.
#' @param simu.name The \code{SimulationName} of the Hi-sAFe simulation to read. This must be the same as the name of the Hi-sAFe simulation folder.
#' Cannot provided both \code{hip} and \code{simu.name}.
#' @param path A character string of the path to the directory containing the Hi-sAFe simulation folder
#' (which contains the standard subdirectory with the output).
#' If \code{hip} is not provided, then \code{path} is required. If both \code{hip} and \code{path} are provided, \code{path} is used.
#' @param profiles A character vector of the names of Hi-sAFe output profiles to read.
#' Defaults to reading all supported Hi-sAFe output profiles via "all". For currently supported profiles see: \code{\link{hisafe_profiles}}
#' @export
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' mydata <- read_hisafe("MySimulation", "./")
#'
#' # If only the annual tree data is required:
#' mytreedata <- read_hisafe("MySimulation", "./", profiles = "annualtree")
#' }
read_hisafe <- function(hip           = NULL,
                        simu.name     = NULL,
                        path          = NULL,
                        profiles      = "all") {

  if(is.null(hip) & is.null(path))        stop("must provide at least one of hip or path", call. = FALSE)
  if(is.null(hip) & is.null(simu.name))   stop("must provide hip OR simu.name", call. = FALSE)
  if(!is.null(hip) & !is.null(simu.name)) stop("must provide hip OR simu.name", call. = FALSE)

  if(is.null(path)) path <- hip$path
  if(is.null(simu.name)) sim.name <- hip$SimulationName

  ## Create profile paths
  if(profiles[1] == "all") profiles <- SUPPORTED.PROFILES$profiles
  simu.path <- gsub("//", "/", paste0(path, "/" , simu.name))
  file.prefix <- paste0(simu.path, "/output-", simu.name, ".sim", "/", simu.name, "_")
  files <- paste0(file.prefix, profiles, ".txt" )

  ## Check for existence of all requested profiles and throw error if profile does not exist
  missing <- !all(file.exists(files))
  if(missing) {
    missing.files <- files[!file.exists(files)]
    missing.profiles <- purrr::map_chr(strsplit(missing.files, "/"), tail, n = 1)
    missing.profile.error <- paste(c("The following requested profiles do not exist:",
                                     paste0("      --", missing.profiles)),
                                   collapse = "\n")
    warning(missing.profile.error, call. = FALSE)
    profiles <- profiles[file.exists(files)]
  }

  cat("Reading: ", simu.name, "\nProfiles:", paste0(profiles, collapse = ", "))

  ## Read simulation inputs & extract cols that vary for binding to output data
  if(is.null(hip)){
    cat(paste0("\nreading:  simulation inputs"))
    hip <- readr::read_csv(paste0(simu.path, "/", simu.name, "_summary.csv"), col_types = readr::cols())
  }

  ## Function for reading timeseries (Annual, Daily) profiles
  read.ts.profiles <- function(profiles, time.class) {

    ## Common columns at the start of all profiles
    join.cols <- c("SimulationName", "Date", "Day", "Month", "Year", "JulianDay", "stepNum")

    if(length(profiles) >= 1) {

      cat(paste0("\nreading:  ", profiles))

      ## Read in all profiles
      dat.list <- purrr::map(profiles, function(x) read_hisafe_output_file(paste0(file.prefix, x, ".txt" )))

      ## Extract & tidy data
      dat.data <- dat.list %>%
        purrr::map(1) %>%                                    # extract first element (data) of each profile
        purrr::reduce(dplyr::full_join, by = join.cols) %>%  # bind all variable descriptions togeter
        dplyr::mutate_if(is.logical, as.numeric)             # columns read as logical must be coered to numeric to prevent plotting errors

      ## Extract & tidy variable definitions
      dat.variables <- dat.list %>%
        purrr::map(2) %>%                              # extract second element (variable descriptions) of each profile
        dplyr::bind_rows() %>%                         # bind all variable descriptions togeter
        dplyr::mutate(VariableClass = time.class)      # add a column that indicates which data class the variable definition is from
    } else {
      dat.data <- dat.variables <- dplyr::tibble()    # if no profiles were requested from this data class, return empty tibbles
    }
    return(list(data = dat.data, variables = dat.variables))
  }

  ## Function for reading other profiles
  read.other.profiles <- function(profile) {
    file <- paste0(file.prefix, profile, ".txt")
    cat(paste0("\nreading:  ", profile, collapse = ", "))
    if(file.info(file)$size < 3e8) {
      profile.list      <- read_hisafe_output_file(file)
      profile.data      <- profile.list$data
      profile.variables <- profile.list$variables %>% dplyr::mutate(VariableClass = profile)
    } else {
      warning(paste0(profile, " profile too large (> 300 MB) to read"), call. = FALSE)
      profile.data <- profile.variables <- dplyr::tibble()
    }
    return(list(data = profile.data, variables = profile.variables))
  }

  ## Read annual data & associated variables
  annual.profiles  <- profiles[profiles %in% c("annualtree", "annualplot")]
  annual.dat       <- read.ts.profiles(annual.profiles, "annual")
  annual.data      <- annual.dat$data
  annual.variables <- annual.dat$variables

  ## Read daily data & associated variables
  daily.profiles   <- profiles[profiles %in% c("trees", "plot", "climate")]
  daily.dat        <- read.ts.profiles(daily.profiles, "daily")
  daily.data       <- daily.dat$data
  daily.variables  <- daily.dat$variables

  ## Read non-annual or daily variables
  other.profiles    <- profiles[profiles %in% c("annualcrop", "roots", "monthCells", "cells", "voxels")]
  if(length(other.profiles) >= 1) {
    other <- purrr::map(other.profiles, read.other.profiles)
    names(other) <- other.profiles

    if(is.null(other$annualcrop)) {
      annualcrop.data      <- dplyr::tibble()
      annualcrop.variables <- dplyr::tibble()
    } else {
      annualcrop.data      <- other$annual.crop$data
      annualcrop.variables <- other$annual.crop$variables
    }

    if(is.null(other$roots)) {
      roots.data      <- dplyr::tibble()
      roots.variables <- dplyr::tibble()
    } else {
      roots.data      <- other$roots$data
      roots.variables <- other$roots$variables
    }

    if(is.null(other$monthCells)) {
      monthCells.data      <- dplyr::tibble()
      monthCells.variables <- dplyr::tibble()
    } else {
      monthCells.data      <- other$monthCells$data
      monthCells.variables <- other$monthCells$variables
    }

    if(is.null(other$cells)) {
      cells.data      <- dplyr::tibble()
      cells.variables <- dplyr::tibble()
    } else {
      cells.data      <- other$cells$data
      cells.variables <- other$cells$variables
    }

    if(is.null(other$voxels)) {
      voxels.data      <- dplyr::tibble()
      voxels.variables <- dplyr::tibble()
    } else {
      voxels.data      <- other$voxels$data
      voxels.variables <- other$voxels$variables
    }

  } else {
    annualcrop.data <- annualcrop.variables <- dplyr::tibble()
    roots.data      <- roots.variables      <- dplyr::tibble()
    monthCells.data <- monthCells.variables <- dplyr::tibble()
    cells.data      <- cells.variables      <- dplyr::tibble()
    voxels.data     <- voxels.variables     <- dplyr::tibble()
  }

  ## Combine variables into one tibble, remove duplicates, rename col headers in English
  variables <- dplyr::distinct(dplyr::bind_rows(annual.variables,
                                                daily.variables,
                                                annualcrop.variables,
                                                roots.variables,
                                                monthCells.variables,
                                                cells.variables,
                                                voxels.variables))
  names(variables) <- c("Subject", "SubjectId", "VariableName", "Units", "Description", "VariableClass")

  ## Creat output list & assign class
  output <- list(annual      = annual.data,
                 daily       = daily.data,
                 annualcrop  = annualcrop.data,
                 roots       = roots.data,
                 monthCells  = monthCells.data,
                 cells       = cells.data,
                 voxels      = voxels.data,
                 variables   = variables,
                 inputs      = hip,
                 path        = simu.path)

  class(output)<-c("hop", class(output))
  return(output)
}

#' Read a single Hi-sAFe output profile
#' @description Reads the designated output profiles from a single Hi-sAFe simulation.
#' @return An list of two data frames (tibbles): \code{data} contains the data from the profile; \code{variables} contains the variable descriptions.
#' @param profile A character string of the path to the profile to be read.
#' @importFrom dplyr %>%
read_hisafe_output_file <- function(profile){

  ## Read raw text & find break between description & data
  raw.text <- readLines(profile)
  end.of.var.list <- which(raw.text[-1] == "")[1]

  ## Read variable descriptions
  variables <- read_table_hisafe(profile, skip = 7, nrows = end.of.var.list - 8) # always 7 header rows at start of each profile

  ## Read data
  dat <- read_table_hisafe(profile, skip = end.of.var.list) %>%
    dplyr::filter(Year != 0)
  if(nrow(dat) == 0) {
    profile.name <- tail(strsplit(profile, "/")[[1]], n = 1)
    warning(paste0(profile.name, " exists but contains no data"), call. = FALSE)
  } else {
    dat <- dat %>%                                               # remove row with Year==0 at the start of every output profile
      dplyr::mutate(Date = gsub(pattern = "a.", replacement = "", x = Date, fixed = TRUE)) %>% # is this here to fix an old bug?
      dplyr::mutate(Date = lubridate::dmy(Date))                 # convert Date column into date class
  }

  return(list(data = dat, variables = variables))
}

#' Read from a Hi-sAFe output profile
#' @description Customized read.table call to read from a Hi-sAFe output profile.
#' @return A data frame.
#' @param file A character string of the path to the file to be read.
#' @param ... Any other arguements passed to \code{read.table}
read_table_hisafe <- function(file, ...) {
  # using read.table rather readr::read_table because read_table is not working
  dplyr::as_tibble(read.table(file,
                              header = TRUE,
                              sep = "\t",
                              stringsAsFactors = FALSE,
                              na.strings = c("NA", "error!"), # "error!" is output by HISAFE & causes table merge errors if left
                              encoding = "latin1", ...))
}
