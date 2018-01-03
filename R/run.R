#' Run one or more Hi-sAFe simulations
#' @description Runs one or more Hi-sAFe simulations
#' @return Invisibly returns a list of console logs from simulation runs.
#' @param hip An object of class "hip". Required if \code{path} is not provided.
#' @param path A character string of the path (relative or absolute) to the directory containing the simulation folders.
#' Required if \code{hip} is not provided.
#' @param simu.names Names of the simulations to run. If "all", the default, then all simulations are run.
#' @param parallel Logical, should parallel computing be used.
#' @param num.cores Numbers of cores to use in parallel computing. If not provided, will default to one less than the total number of available cores.
#' @param capsis.path A character string of the path (relative or absolute) to the Capsis folder
#' @export
#' @importFrom foreach %dopar%
#' @importFrom foreach %do%
#' @family hisafe run functions
#' @examples
#' \dontrun{
#' # Once a group Hi-sAFe simulations (experiment) is defined...
#' myexp <- define_hisafe(path = "./simulations", exp.name = "lat_exp", latitude = c(30,60))
#'
#' # ...and built...
#' build_hisafe(myexp)
#'
#' # the simulations can all be run:
#' run_hisafe(myexp)
#' }
run_hisafe <- function(hip         = NULL,
                       path        = NULL,
                       simu.names  = "all",
                       parallel    = FALSE,
                       num.cores   = NULL,
                       capsis.path) {

  capsis.path <- R.utils::getAbsolutePath(capsis.path)
  if(!is.null(path)) path <- R.utils::getAbsolutePath(path)

  if(!is.null(hip) & !("hip" %in% class(hip)))                  stop("hip argument not of class hip",                                      call. = FALSE)
  if(is.null(hip) == is.null(path))                             stop("must provide hip OR path, not both",                         call. = FALSE)
  if(!dir.exists(path))                                         stop("directory specified by path does not exist",                 call. = FALSE)
  if(!(all(is.character(simu.names)) | simu.names[1] == "all")) stop("simu.names argument must be 'all' or a character vector",    call. = FALSE)
  if(!is.logical(parallel))                                     stop("parallel argument must be a logical",                        call. = FALSE)
  if(!dir.exists(capsis.path))                                  stop("directory specified by capsis.path does not exist",          call. = FALSE)
  if(!("capsis.sh" %in% list.files(capsis.path)))               stop("directory specified by capsis.path does not contain Capsis", call. = FALSE)
  if(!((is.numeric(num.cores) & length(num.cores) == 1) | is.null(num.cores))) stop("num.cores argument must be a positive integer", call. = FALSE)
  if(num.cores %% 1 != 0 & num.cores > 0)                                      stop("num.cores argument must be a positive integer", call. = FALSE)

  ## Determine path and simu.names to run
  if(is.null(path)) {
    path <- hip$path
    if(simu.names[1] == "all") simu.names <- hip$exp.plan$SimulationName
  } else {
    if(simu.names[1] == "all") simu.names <- purrr::map_chr(list.dirs(path, recursive = FALSE), function(x) tail(strsplit(x, "/")[[1]], n = 1))
  }

  ## Ensure simulation directories exist
  sims.to.run <- clean_path(paste0(path, "//", simu.names))
  if(!all(dir.exists(sims.to.run))) {
    stop(paste("The following simulations do not exist:", paste(simu.names[!dir.exists(sims.to.run)], collapse = ", ")), call. = FALSE)
  }

  ## Run
  if(parallel) {
    if(length(simu.names) == 1) stop("There is only 1 simulation to run. Parallel computing is not possible.", call. = FALSE)

    ## Check for packages required for parallel computing
    parallel.packages <- c("foreach", "parallel", "doParallel")
    package.check <- purrr::map_lgl(parallel.packages, requireNamespace, quietly = TRUE)
    if(any(!package.check)) {
      missing.packages <- parallel.packages[!package.check]
      stop(paste0("The following packages are needed for parallel computing with hisafer. Please install them.\n",
                  paste(missing.packages, collapse = "\n")), call. = FALSE)
    }

    if(is.null(num.cores)) num.cores <- min((parallel::detectCores() - 1), nrow(hip$exp.plan))
    if(num.cores > parallel::detectCores()) stop(paste("There are only", parallel::detectCores(), "detectable cores on this computer."), call. = FALSE)
    if(num.cores == 1) stop("There is only 1 detectable core on this computer. Parallel computing is not possible.", call. = FALSE)
    cat("\nInitializing", length(simu.names), "simulations on", num.cores, "cores")
    cl <- parallel::makeCluster(num.cores)
    doParallel::registerDoParallel(cl)
    run.log <- foreach::foreach(i = simu.names, .inorder = FALSE) %dopar% call_hisafe(path = path, simu.name = i, capsis.path = capsis.path)
    doParallel::stopImplicitCluster()
  } else {
    cat("\nInitializing", length(simu.names), "simulations on 1 core")
    run.log <- foreach::foreach(i = simu.names) %do% call_hisafe(path = path, simu.name = i, capsis.path = capsis.path)
  }
  cat("\nAll simulations complete")
  invisible(run.log)
}

#' Calls Hi-sAFe from command line
#' @description Calls Hi-sAFe from command line to run a simulation.
#' @return Invisibly returns a console log from simulation run.
#' @param path A character string of the path to the folder containing the simulation folder. Required if \code{hip} is not provided.
#' @param simu.name Name of the simulation to run. Required if \code{hip} is not provided.
#' @param capsis.path A character string of the path to the Capsis folder
call_hisafe <- function(path, simu.name, capsis.path) {

  sim.path <- clean_path(paste0(path, "/", simu.name, "/"))
  if(!dir.exists(sim.path)) stop(paste("The simulation", simu.name, "does not exist."), call. = FALSE)

  simulationStartTime <- proc.time()[3]
  out <- file(paste0(sim.path, "support/", simu.name, "_simulation_log.txt"), open="w")
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
  close(out)

  ## Change working directory back to user's original directory
  setwd(pre.wd)

  invisible(hisafe.log)
}
