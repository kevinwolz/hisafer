#' Run one or more Hi-sAFe simulations
#' @description Runs one or more Hi-sAFe simulations
#' @return Invisibly returns a list of console logs from simulation runs.
#' @param hip An object of class "hip". Required if \code{path} is not provided.
#' @param path A character string of the path (relative or absolute) to the directory containing the simulation folders.
#' Required if \code{hip} is not provided.
#' @param simu.names Names of the simulations to run. If "all", the default, then all simulations are run.
#' @param capsis.path A character string of the path (relative or absolute) to the Capsis folder
#' @param parallel Logical, should parallel computing be used.
#' @param num.cores Numbers of cores to use in parallel computing.
#' If not provided, will default to one less than the total number of available cores.
#' @param mem.spec An integer indicating the maximum memory use (Mb) permitted for an individual instance of Capsis/Hi-sAFe.
#' If \code{TRUE}, will default to the system's total memory divided by the number of cores.
#' If \code{FALSE}, a memory specification will not be made, and Capsis/Hi-sAFe will run with defaults.
#' @param quietly Logical indicating whether status messages should printed to the console.
#' @export
#' @importFrom foreach %dopar%
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
run_hisafe <- function(hip            = NULL,
                       path           = NULL,
                       simu.names     = "all",
                       capsis.path,
                       parallel       = FALSE,
                       num.cores      = NULL,
                       mem.spec       = TRUE,
                       quietly        = FALSE,
                       launch.call    = "ScriptGen",
                       default.folder = "") {

  capsis.path <- R.utils::getAbsolutePath(capsis.path)
  if(!is.null(path)) path <- R.utils::getAbsolutePath(path)

  is_hip(hip, error = TRUE)
  if(is.null(hip) == is.null(path))                             stop("must provide hip OR path, not both",                           call. = FALSE)
  if(!(all(is.character(simu.names)) | simu.names[1] == "all")) stop("simu.names argument must be 'all' or a character vector",      call. = FALSE)
  is_TF(parallel)
  if(!dir.exists(capsis.path))                                  stop("directory specified by capsis.path does not exist",            call. = FALSE)
  if(!("capsis.sh" %in% list.files(capsis.path)))               stop("directory specified by capsis.path does not contain Capsis",   call. = FALSE)
  if(!is.null(num.cores)) {
    if(!(is.numeric(num.cores) & length(num.cores) == 1))       stop("num.cores argument must be a positive integer",                call. = FALSE)
    if(num.cores %% 1 != 0 & num.cores > 0)                     stop("num.cores argument must be a positive integer",                call. = FALSE)
  }
  if(!((is.numeric(mem.spec) & length(mem.spec) == 1) | is.logical(mem.spec))) stop("mem.spec argument must logical or a positive integer", call. = FALSE)
  if(!(is.character(launch.call)    & length(launch.call)    == 1)) stop("launch.call argument must be a character vector of length 1",    call. = FALSE)
  if(!(is.character(default.folder) & length(default.folder) == 1)) stop("default.folder argument must be a character vector of length 1", call. = FALSE)

  ## Determine path and simu.names to run
  if(is.null(path)) {
    path <- hip$path
    if(simu.names[1] == "all") simu.names <- hip$exp.plan$SimulationName
  } else {
    if(simu.names[1] == "all") simu.names <- purrr::map_chr(list.dirs(path, recursive = FALSE), function(x) tail(strsplit(x, "/")[[1]], n = 1))
  }
  if(!dir.exists(path)) stop("directory specified by path does not exist", call. = FALSE)

  ## Ensure simulation directories exist
  sims.to.run <- clean_path(paste0(path, "//", simu.names))
  if(!all(dir.exists(sims.to.run))) {
    stop(paste("The following simulations do not exist:", paste(simu.names[!dir.exists(sims.to.run)], collapse = ", ")), call. = FALSE)
  }

  ## Set allowed memory
  if(mem.spec) { # TRUE or non-zero
    if(isTRUE(mem.spec)) {
      if(!requireNamespace(c("memuse", "parallel"), quietly = TRUE)) stop("The packages 'memuse' and 'parallel' are required if mem.spec = TRUE.
                                                                          Please install and load them.", call. = FALSE)
      mem.spec <- memuse::Sys.meminfo()$totalram@size * 1024 / parallel::detectCores()
    }
    pre.wd <- getwd()
    setwd(capsis.path)
    if(.Platform$OS.type == "windows") {
      setmem.call <- paste0("setmem ", mem.spec)
    } else {
      setmem.call <- paste0("sh setmem.sh ", mem.spec)
    }
    dum <- system(setmem.call, wait = TRUE, intern = quietly)
    setwd(pre.wd)
  }

  ## Run
  if(parallel) {
    parallel.packages <- c("foreach", "parallel", "doParallel")
    if(!requireNamespace(parallel.packages, quietly = TRUE)) stop("The packages 'foreach', 'parallel', and 'doParallel are required
                                                                  for parallel computing with run_hisafe(). Please install and load them.", call. = FALSE)
    if(length(simu.names) == 1)        stop("There is only 1 simulation to run. Parallel computing is not possible.", call. = FALSE)
    if(num.cores > length(simu.names)) stop("num.cores cannot be greater than length(simu.names)",                    call. = FALSE)

    if(is.null(num.cores)) num.cores <- min((parallel::detectCores() - 1), nrow(hip$exp.plan))
    if(num.cores > parallel::detectCores()) stop(paste("There are only", parallel::detectCores(), "detectable cores on this computer."), call. = FALSE)
    if(num.cores == 1) stop("There is only 1 detectable core on this computer. Parallel computing is not possible.", call. = FALSE)
    if(!quietly) cat("\nInitializing", length(simu.names), "simulations on", num.cores, "cores")
    cl <- parallel::makeCluster(num.cores)
    doParallel::registerDoParallel(cl)
    run.log <- foreach::foreach(i = simu.names, .inorder = FALSE) %dopar% call_hisafe(path           = path,
                                                                                      simu.name      = i,
                                                                                      capsis.path    = capsis.path,
                                                                                      launch.call    = launch.call,
                                                                                      default.folder = default.folder,
                                                                                      quietly        = quietly)
    doParallel::stopImplicitCluster()
  } else {
    if(!quietly) cat("\nInitializing", length(simu.names), "simulations on 1 core")
    for(i in simu.names) {
      run.log <- call_hisafe(path           = path,
                             simu.name      = i,
                             capsis.path    = capsis.path,
                             launch.call    = launch.call,
                             default.folder = default.folder,
                             quietly        = quietly)
      if(!quietly) cat(paste("\nSimulation", i, "complete"))
    }
  }
  if(!quietly) cat("\nAll simulations complete")
  invisible(run.log)
}

#' Calls Hi-sAFe from command line
#' @description Calls Hi-sAFe from command line to run a simulation.
#' @return Invisibly returns a console log from simulation run.
#' @param path A character string of the path to the folder containing the simulation folder. Required if \code{hip} is not provided.
#' @param simu.name Name of the simulation to run. Required if \code{hip} is not provided.
#' @param capsis.path A character string of the path to the Capsis folder
#' @param quietly Logical indicating whether status messages should printed to the console.
#' @keywords internal
call_hisafe <- function(path, simu.name, capsis.path, launch.call, default.folder, quietly) {

  sim.path <- clean_path(paste0(path, "/", simu.name, "/"))
  if(!dir.exists(sim.path)) stop(paste("The simulation", simu.name, "does not exist."), call. = FALSE)

  if(default.folder != "") default.folder <- paste0(" ", default.folder)

  simulationStartTime <- proc.time()[3]
  out <- file(paste0(sim.path, "support/", simu.name, "_simulation_log.txt"), open = "w")
  cat("Beginning simulation:", simu.name, file = out)
  if(!quietly) cat("\nBeginning simulation:", simu.name)

  ## Change working directory to Capsis directory
  pre.wd <- getwd()
  setwd(capsis.path)

  ## Run Hi-sAFe
  if(.Platform$OS.type == "windows") {
    call <- paste0("capsis -p script safe.pgms.",       launch.call, " ", sim.path, simu.name, ".sim", default.folder)
  } else {
    call <- paste0("sh capsis.sh -p script safe.pgms.", launch.call, " ", sim.path, simu.name, ".sim", default.folder)
  }
  hisafe.log <- system(call, wait = TRUE, intern = TRUE)

  ## Save log & print elapsed time
  cat("\n", call, file = out, append = TRUE)
  cat("\n", hisafe.log, sep = "\n", file = out, append = TRUE)
  simulationElapsedTime <- signif((proc.time()[3] - simulationStartTime)/60,3)
  done.message <- paste0("\nDone with Hi-sAFe simulation: ", simu.name, ". This took ", simulationElapsedTime, " minutes.")
  cat(done.message, file = out, append = TRUE)
  if(!quietly) cat(done.message)
  close(out)

  ## Change working directory back to user's original directory
  setwd(pre.wd)

  invisible(hisafe.log)
}
