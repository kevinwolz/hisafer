#' Read output from a Hi-sAFe experiment
#' @description Reads the designated output profiles from a Hi-sAFe experiiment (i.e. a group of Hi-sAFe simulations).
#' @return An object of class "hop-group". This is a list of 15 data frames (tibbles):
#' \itemize{
#'  \item{annualtree}
#'  \item{annualcrop}
#'  \item{annualplot}
#'  \item{trees}
#'  \item{plot}
#'  \item{climate}
#'  \item{roots}
#'  \item{monthCells}
#'  \item{cells}
#'  \item{voxels}
#'  \item{variables}{ - variable descriptions and units from all profiles}
#'  \item{tree.info}{ - tree species and location data}
#'  \item{exp.plan}{ - the exp.plan of the hip object that generated the simulation}
#'  \item{path}{ - the path to the simulation folder}
#'  \item{exp.path}{ - the path to the experiment folder}
#' }
#' @param hip An object of class "hip". To create a hip object see \code{\link{define_hisafe}}.
#' If \code{hip} is not provided, then \code{path} is required and the input data for the experiment is read from the experiment
#' summary .csv file created when building the experiment.
#' @param path A character string of the path to the directory containing the Hi-sAFe simulation folders.
#' If \code{hip} is not provided, then \code{path} is required. If \code{hip} is provided, \code{path} is ignored.
#' @param simu.names Names of the simulations to read. If "all", the default, then all simulations in the experiment folder are read.
#' @param profiles A character vector of the names of Hi-sAFe output profiles to read.
#' #' If "all" the default, reads all supported Hi-sAFe output profiles. For currently supported profiles see: \code{\link{hisafe_profiles}}
#' @param show.progress Logical indicating whether progress messsages should be printed to the console.
#' @export
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' myexp <- read_hisafe_exp(myexphip)
#'
#' # If only the annual tree data is required:
#' mytreeexp <- read_hisafe(myexphip, profiles = "annualtree")
#' }
read_hisafe_exp <- function(hip = NULL,
                            path = NULL,
                            simu.names = "all",
                            profiles = "all",
                            show.progress = TRUE) {

  if(!is.null(hip) & !("hip" %in% class(hip))) stop("data not of class hip", call. = FALSE)
  if(is.null(hip) == is.null(path))            stop("must provide hip or path, not both", call. = FALSE)

  ## Read simulation inputs & extract cols that vary for binding to output data
  if(!is.null(hip)) {
    EXP.PLAN <- hip$exp.plan
    path     <- hip$path
  } else {
    exp.name <- tail(strsplit(path, "/")[[1]], n = 1)
    exp.summary.file <- clean_path(paste0(path, "/", exp.name, "_exp_summary.csv"))
    if(file.exists(exp.summary.file)) {
      EXP.PLAN <- readr::read_csv(exp.summary.file, col_types = readr::cols())
    } else {
      warning("No experiment summary to read. This experiment was not created with hisafer.", call. = FALSE)
      EXP.PLAN <- dplyr::tibble()
    }
  }

  EXP.PLAN <- EXP.PLAN[, purrr::map_lgl(EXP.PLAN, function(x) (length(unique(x)) != 1))]

  if(simu.names[1] != "all") EXP.PLAN <- dplyr::filter(EXP.PLAN, SimulationName %in% simu.names)

  ## Read all data from all simulations & combine
  data <- purrr::map(EXP.PLAN$SimulationName, read_hisafe, hip = NULL, path = path, profiles = profiles, show.progress = show.progress) %>%
    purrr::pmap(dplyr::bind_rows) # a more generic version of this line that handles cases where the number
  # of elements and order of names in each sublist can vary is:
  # purrr::map(map_df(data, ~ as.data.frame(purrr::map(.x, ~ unname(nest(.))))), bind_rows)
  # However, this isn't necessary becasue read_hisafe_output always produces
  # identically sized and named lists.

  ## Tidy up data
  data_tidy <- function(x){
    if(nrow(x) > 0) {
      x <- dplyr::left_join(EXP.PLAN, x, by = "SimulationName") %>%   # add EXP.PLAN cols to annual data
        dplyr::filter(!is.na(Date)) #%>%                               # output profiles with no data will have NA's in all columns
      #dplyr::mutate_at(names(EXP.PLAN), factor) #%>%
      #dplyr::group_by(SimulationName)                               # group annual data by simulaton
    }
    return(x)
  }

  data$annualtree  <- data_tidy(data$annualtree)
  data$annualcrop  <- data_tidy(data$annualcrop)
  data$annualplot  <- data_tidy(data$annualplot)
  data$trees       <- data_tidy(data$trees)
  data$plot        <- data_tidy(data$plot)
  data$climate     <- data_tidy(data$climate)
  data$roots       <- data_tidy(data$roots)
  data$monthCells  <- data_tidy(data$monthCells)
  data$cells       <- data_tidy(data$cells)
  data$voxels      <- data_tidy(data$voxels)
  data$variables   <- dplyr::distinct(data$variables)            # remove duplicate variable descriptions
  data$exp.path    <- path

  ## Warn if lengths of all simulations are not equal
  year.summary <- data[[as.numeric(which.max(purrr::map_int(data[names(data) != "exp.path"], nrow)))]] %>%
    dplyr::group_by(SimulationName) %>%
    dplyr::summarize(n = dplyr::n_distinct(Year) - 1) %>%
    tidyr::unite(label, SimulationName, n, sep = ": ", remove = FALSE)
  if(length(unique(year.summary$n)) != 1) {
    year.length.warning <- paste(c("Simulation durations not equal!",
                                   "  Be careful when comparing simulations.",
                                   "  Simulation durations:",
                                   paste("   --", year.summary$label, "years")),
                                 collapse = "\n")
    warning(year.length.warning, call. = FALSE)
  }

  class(data) <- c("hop-group", "hop", class(data))
  return(data)
}

#' Read output from a single Hi-sAFe simulation
#' @description Reads the designated output profiles from a single Hi-sAFe simulation.
#' @return An object of class "hop". This is a list of 14 data frames (tibbles):
#' \itemize{
#'  \item{annualtree}
#'  \item{annualcrop}
#'  \item{annualplot}
#'  \item{trees}
#'  \item{plot}
#'  \item{climate}
#'  \item{roots}
#'  \item{monthCells}
#'  \item{cells}
#'  \item{voxels}
#'  \item{variables}{ - variable descriptions and units from all profiles}
#'  \item{tree.info}{ - tree species and location data}
#'  \item{exp.plan}{ - the exp.plan of the hip object that generated the simulation}
#'  \item{path}{ - the path to the simulation folder}
#' }
#' @param hip An object of class "hip". To create a hip object see \code{\link{define_hisafe}}.
#' Cannot provided both \code{hip} and \code{simu.name}.
#' If \code{hip} is not provided, then \code{path} is required and the input data for the experiment is read from the experiment
#' summary .csv file created when building the experiment.
#' @param path A character string of the path to the directory containing the Hi-sAFe simulation folder.
#' If \code{hip} is not provided, then \code{path} is required. If both \code{hip} and \code{path} are provided, \code{path} is used.
#' @param simu.name The \code{SimulationName} of the Hi-sAFe simulation to read. This must be the same as the name of the Hi-sAFe simulation folder.
#' Cannot provided both \code{hip} and \code{simu.name}.
#' @param profiles A character vector of the names of Hi-sAFe output profiles to read.
#' If "all" the default, reads all supported Hi-sAFe output profiles. For currently supported profiles see: \code{\link{hisafe_profiles}}
#' @param show.progress Logical indicating whether progress messsages should be printed to the console.
#' @export
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' mydata <- read_hisafe(mysimhip)
#'
#' # If only the annual tree data is required:
#' mytreedata <- read_hisafe(mysimhip, profiles = "annualtree")
#' }
read_hisafe <- function(hip = NULL,
                        path = NULL,
                        simu.name = NULL,
                        profiles = "all",
                        show.progress = TRUE) {

  if(!is.null(hip) & !("hip" %in% class(hip)))              stop("data not of class hip", call. = FALSE)
  if(is.null(hip) == (is.null(simu.name) | is.null(path)))  stop("must provide hip OR (simu.name & path)", call. = FALSE)

  ## Read simulation inputs
  if(!is.null(hip)) {
    EXP.PLAN  <- hip$exp.plan
    path      <- hip$path
    simu.name <- EXP.PLAN$SimulationName
    simu.path <- clean_path(paste0(path, "/" , simu.name))
  } else {
    #cat("\nreading:  simulation inputs (hip)")
    simu.path <- clean_path(paste0(path, "/" , simu.name))
    simu.summary.file <- paste0(simu.path, "/", simu.name, "_simulation_summary.csv")
    if(file.exists(simu.summary.file)){
      EXP.PLAN <- readr::read_csv(paste0(simu.path, "/", simu.name, "_simulation_summary.csv"), col_types = readr::cols()) #%>%
      #mutate(SimulationName = factor(SimulationName))
    } else {
      warning("No simulation inputs summary (experimental plan) to read from simulation directory. This simulation was not created with hisafer.", call. = FALSE)
      EXP.PLAN <- dplyr::tibble()
    }
  }

  ## Create profile paths
  if(profiles[1] == "all") profiles <- SUPPORTED.PROFILES$profiles
  file.prefix <- paste0(simu.path, "/output-", simu.name, ".sim", "/", simu.name, "_")
  files       <- paste0(file.prefix, profiles, ".txt" )

  ## Check for existence of all requested profiles and warn if profile does not exist
  if(!all(file.exists(files))) {
    missing.profiles <- files[!file.exists(files)] %>%
      strsplit("/") %>%
      purrr::map_chr(tail, n = 1)
    missing.profile.error <- paste(c("The following requested profiles do not exist:",
                                     paste0("      --", missing.profiles)),
                                   collapse = "\n")
    warning(missing.profile.error, call. = FALSE)
    profiles <- profiles[file.exists(files)]
  }

  ## Read profiles
  if(show.progress) cat("\n\nReading: ", simu.name, "\nProfiles:", paste0(profiles, collapse = ", "))

  if(length(profiles) >= 1) {
    out <- purrr::map(profiles, read_profile, path = file.prefix, show.progress = show.progress)
    names(out) <- profiles
  } else {
    out <- list()
  }

  get_prof <- function(out, prof) {
    if(is.null(out[[prof]])) {
      dv <- list(data = dplyr::tibble(), variables = dplyr::tibble())
    } else {
      dv <- out[[prof]]
    }
    return(dv)
  }

  annualtree.dv <- get_prof(out, "annualtree")
  annualcrop.dv <- get_prof(out, "annualcrop")
  annualplot.dv <- get_prof(out, "annualplot")
  trees.dv      <- get_prof(out, "trees")
  plot.dv       <- get_prof(out, "plot")
  climate.dv    <- get_prof(out, "climate")
  roots.dv      <- get_prof(out, "roots")
  monthCells.dv <- get_prof(out, "monthCells")
  cells.dv      <- get_prof(out, "cells")
  voxels.dv     <- get_prof(out, "voxels")

  ## Combine variables into one tibble, remove duplicates, rename col headers in English
  vars.to.pull <- list(annualtree.dv, annualcrop.dv, annualplot.dv, trees.dv, plot.dv, climate.dv, roots.dv, monthCells.dv, cells.dv, voxels.dv)
  variables <- purrr::map(vars.to.pull, "variables") %>%
    dplyr::bind_rows() %>%
    dplyr::distinct()
  if(ncol(variables) > 0) names(variables) <- c("Subject", "SubjectId", "VariableName", "Units", "Description", "VariableClass")

  ## Creat output list & assign class
  output <- list(annualtree = annualtree.dv$data,
                 annualcrop = annualcrop.dv$data,
                 annualplot = annualplot.dv$data,
                 trees      = trees.dv$data,
                 plot       = plot.dv$data,
                 climate    = climate.dv$data,
                 roots      = roots.dv$data,
                 monthCells = monthCells.dv$data,
                 cells      = cells.dv$data,
                 voxels     = voxels.dv$data,
                 variables  = variables,
                 tree.info  = read_tree_info(path, simu.name),
                 exp.plan   = EXP.PLAN,
                 path       = dplyr::tibble(SimulationName = simu.name, path = simu.path))

  class(output) <- c("hop", class(output))
  return(output)
}

#' Read example Hi-sAFe experiment output
#' @description Reads in an example Hi-sAFe experiment. For more details see \code{\link{read_hisafe_exp}}.
#' @return An object of class "hop". For more details see \code{?read_hisafe_exp}.
#' @param profiles A character vector of the names of Hi-sAFe output profiles to read.
#' "cells", "roots" and "voxels" profiles are not available in the example.
#' @param show.progress Logical indicating whether progress messsages should be printed to the console.
#' @export
read_hisafe_example <- function(profiles = c("annualplot", "annualtree", "annualcrop", "plot", "trees", "climate", "monthCells"),
                                show.progress = TRUE) {
  hop <- read_hisafe_exp(path = clean_path(paste0(system.file("extdata", "example_output", package = "hisafer"), "/")),
                         profiles = profiles,
                         show.progress = show.progress)
  return(hop)
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

#' Read a Hi-sAFe output profile
#' @description Wrapper for read_hisafe_output_file.
#' @return A list containing the profile data and the profile variable definitions.
#' @param profile A character string of the profile name.
#' @param path A character string of the path to the folder containing the profiles.
#' @param show.progress Logical indicating whether progress messsages should be printed to the console.
read_profile <- function(profile, path, show.progress = TRUE) {
  file <- paste0(path, profile, ".txt")
  if(show.progress) cat(paste0("\n   -- reading:  ", profile, collapse = ", "))
  if(file.info(file)$size < 3e8) {
    profile.list      <- read_hisafe_output_file(file)
    profile.data      <- profile.list$data %>%
      dplyr::mutate_if(is.logical, as.numeric)   # columns read as logical must be coered to numeric to prevent plotting errors
    profile.variables <- profile.list$variables %>% dplyr::mutate(VariableClass = profile)
  } else {
    warning(paste(profile, "profile too large (> 300 MB) to read"), call. = FALSE)
    profile.data <- profile.variables <- dplyr::tibble()
  }
  return(list(data = profile.data, variables = profile.variables))
}

#' Read tree information from a Hi-sAFe pld file
#' @description Reads tree information from a Hi-sAFe pld file. Used by \code{\link{read_hisafe}}.
#' @return A data frame (tibble) containing tree id, species, x and y.
#' @param path A character string of the path to the directory containing the simulation folder.
#' @param simu.name A character string of the simualation name.
#' @importFrom dplyr %>%
read_tree_info <- function(path, simu.name) {
  pld <- read_param_file(clean_path(paste0(path, "/", simu.name, "/plotDescription/", simu.name, ".pld")))
  if(pld$TREE_INITIALIZATION$tree.initialization$commented == FALSE) {
    tree.info <- pld$TREE_INITIALIZATION$tree.initialization$value %>%
      dplyr::mutate(x = treeX, y = treeY) %>%
      dplyr::select(species, x, y)
    tree.info <- tree.info %>%
      dplyr::mutate(id = 1:nrow(tree.info)) %>%
      dplyr::mutate(SimulationName = rep(simu.name, nrow(tree.info))) %>%
      dplyr::select(SimulationName, id, dplyr::everything())
  } else {
    tree.info <- dplyr::tibble()
  }
}
