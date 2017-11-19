#' Run a Hi-sAFe experiment
#' @description Runs a Hi-sAFe experiment (a group of simulations) - calls Hi-sAFe from command line.
#' @return Invisibly returns a list of console logs from simulation runs.
#' @param hip An object of class "hip" with multiple simulations (rows). Required if \code{path} is not provided.
#' @param path A character string of the path (relative or absolute) to the experiment folder containing the simulation folders.
#' Required if \code{hip} is not provided.
#' @param simu.names Names of the simulations to run. If "all", the default, then all simulations are run.
#' @param parallel Logical, should parallel computing be used.
#' @param num.cores Numbers of cores to use in parallel computing. If not provided, will default to one less than the total number of available cores.
#' @param capsis.path A character string of the path to the Capsis folder
#' @export
#' @importFrom foreach %dopar%
#' @family hisafe run functions
#' @examples
#' \dontrun{
#' # Once a group Hi-sAFe simulations (experiment) is defined...
#' myexp <- define_hisafe(path = "./simulations", latitude = c(30,60))
#'
#' # ...and built...
#' build_hisafe_exp(myexp, exp.name = "lat_exp")
#'
#' # the simulations can all be run:
#' run_hisafe_exp(myexp)
#' }
run_hisafe_exp <- function(hip         = NULL,
                           path        = NULL,
                           simu.names  = "all",
                           parallel    = FALSE,
                           num.cores   = NULL,
                           capsis.path = "/Applications/Capsis/") {

  if(!is.null(hip) & !("hip" %in% class(hip))) stop("data not of class hip", call. = FALSE)
  if(!is.null(hip)){
    if(nrow(hip$exp.plan) <= 1) stop("run_hisafe_exp runs multiple simulations. Please use hip object with more than one experiment (row).", call. = FALSE)
  }
  if(is.null(hip) & is.null(path))    stop("must provide hip OR path", call. = FALSE)
  if(!is.null(hip) & !is.null(path))  stop("must provide hip OR path, not both", call. = FALSE)

  if(is.null(path)) {
    path <- hip$path
  } else {
    path <- R.utils::getAbsolutePath(path)
  }

  ## Determine simulations to run & check for missing directories
  if(simu.names[1] == "all") {
    sims.to.run <- list.dirs(path, recursive = FALSE)
    if(length(sims.to.run) == 0) stop("path does not contain any simulation directories")
    simu.names  <- purrr::map_chr(sims.to.run, function(x) tail(strsplit(x, "/")[[1]], n = 1))
  } else {
    sims.to.run <- clean_path(paste0(path, "//", simu.names))
    if(!all(dir.exists(sims.to.run))) {
      missing.dirs  <- simu.names[!dir.exists(sims.to.run)]
      missing.error <- paste(c("The following simulations do not exist:",
                               missing.dirs),
                             collapse = "\n")
      stop(missing.error)
    }
  }

  if(parallel) {
    ## Check for packages required for parallel computing
    parallel.packages <- c("foreach", "parallel", "doParallel")
    package.check <- purrr::map_lgl(parallel.packages, requireNamespace, quietly = TRUE)
    if(any(!package.check)) {
      missing.packages <- parallel.packages[!package.check]
      stop(paste0("The following packages are needed for parallel computing with hisafer. Please install them.\n",
           paste(missing.packages, collapse = "\n")), call. = FALSE)
    }

    if(is.null(num.cores)) num.cores <- min((parallel::detectCores() - 1), nrow(hip$exp.plan))
    if(num.cores == 1) stop("There is only 1 detectable core on this computer. Parallel computing is not possible.")
    cat("\nInitializing simulations on", num.cores, "cores")
    cl <- parallel::makeCluster(num.cores)
    doParallel::registerDoParallel(cl)
    run.log <- foreach::foreach(i = simu.names, .inorder = FALSE) %dopar% run_hisafe(path = path, simu.name = i, capsis.path = capsis.path)
    doParallel::stopImplicitCluster()
  } else {
    run.log <- foreach::foreach(i = simu.names) %do% run_hisafe(path = path, simu.name = i)
  }
  cat("\nAll simulations complete")
  invisible(run.log)
}

#' Run a Hi-sAFe simulation
#' @description Runs a Hi-sAFe simulation - calls Hi-sAFe from command line.
#' @return Invisibly returns a console log from simulation run.
#' @param hip An object of class "hip" with a single simulation (row). Required if \code{path} and \code{simu.name} are not provided.
#' @param path A character string of the path to the folder containing the simulation folder. Required if \code{hip} is not provided.
#' @param simu.name Name of the simulation to run. Required if \code{hip} is not provided.
#' @param capsis.path A character string of the path to the Capsis folder
#' @export
#' @family hisafe run functions
#' @examples
#' \dontrun{
#' # Once a group Hi-sAFe simulations (experiment) is defined...
#' myexp <- define_hisafe(path = "./simulations")
#'
#' # ...and built...
#' build_hisafe(myexp)
#'
#' # the simulations can all be run:
#' run_hisafe(myexp)
#' }
run_hisafe <- function(hip         = NULL,
                       path        = NULL,
                       simu.name   = NULL,
                       capsis.path = "/Applications/Capsis/") {

  if(!is.null(hip) & !("hip" %in% class(hip))) stop("data not of class hip", call. = FALSE)
  if(!is.null(hip)){
    if(nrow(hip$exp.plan) > 1) stop("hip object contains more than one simulation (row). Use run_hisafe_exp to run multiple simulations.",
                                    call. = FALSE)
  }
  if(is.null(hip) & is.null(path))        stop("must provide at least one of hip or path", call. = FALSE)
  if(is.null(hip) & is.null(simu.name))   stop("must provide hip OR simu.name", call. = FALSE)
  if(!is.null(hip) & !is.null(simu.name)) stop("must provide hip OR simu.name, not both", call. = FALSE)

  if(is.null(path)) path <- hip$path
  if(is.null(simu.name)) simu.name <- hip$exp.plan$SimulationName

  sim.path <- clean_path(paste0(path, "/", simu.name, "/"))

  if(!dir.exists(sim.path)) stop(paste("The simulation", simu.name, "does not exist."), call. = FALSE)

  simulationStartTime <- proc.time()[3]
  out <- file(paste0(sim.path, simu.name, "_simulation_log.txt"), open="w")
  cat("Beginning simulation:", simu.name, file = out)
  cat("\nBeginning simulation:", simu.name)

  ## Change working directory to Capsis directory
  pre.wd <- getwd()
  setwd(capsis.path)

  ## Run Hi-sAFe
  call <- paste0("sh capsis.sh -p script safe.pgms.ScriptGen ", sim.path, simu.name, ".sim")
  hisafe.log <- system(call, wait = TRUE, intern = TRUE)

  ## Save log & print elapsed time
  cat("\n", call, file = out, append = TRUE)
  cat("\n", hisafe.log, sep = "\n", file = out, append = TRUE)
  simulationElapsedTime <- signif((proc.time()[3] - simulationStartTime)/60,3)
  done.message <- paste0("\nDone with Hi-sAFe simulation: ", simu.name, ". This took ", simulationElapsedTime, " minutes.")
  cat(done.message, file = out, append = TRUE)
  cat(done.message)

  ## Change working directory back to user's original directory
  setwd(pre.wd)

  invisible(hisafe.log)
}
