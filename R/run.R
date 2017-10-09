#' Run a Hi-sAFe experiment
#' @description Runs a Hi-sAFe experiment (a group of simulations) - calls Hi-sAFe from command line.
#' @return Invisibly returns a list of console logs from simulation runs.
#' @param hip An object of class hip with multiple simulations (rows).
#' If the hip object contains a \code{path} value, then this can be used without providing \code{exp.path} directly to \code{run_hisafe_exp}.
#' If \code{hip} is not provided, then \code{exp.path} is required.
#' @param exp.path A character string of the path to the experiment folder containing the simulation folders. Required if \code{hip} is not provided.
#' @param simu.names Names of the simulations to run. Default is to run all simulations in the experiment folder via "all".
#' @param parallel Logical, should parallel computing be used.
#' @export
#' @family hisafe run functions
#' @examples
#' \dontrun{
#' # Once a group Hi-sAFe simulations (experiment) is defined...
#' myexp <- define_hisafe(latitude = c(30,60))
#'
#' # ...and built...
#' myexp <- build_hisafe_exp(myexp, "./simulations", exp.name = "lat_exp")
#'
#' # the simulations can all be run:
#' run_hisafe_exp(myexp)
#' }
run_hisafe_exp <- function(hip        = NULL,
                           exp.path   = NULL,
                           simu.names = "all",
                           parallel   = FALSE) {

  if(!is.null(hip)){
    if(nrow(hip$hip) <= 1) stop("run_hisafe_exp runs multiple simulations. Please use hip object with more than one experiment (row).")
  }
  if(is.null(hip) & is.null(exp.path))    stop("must provide hip OR exp.path", call. = FALSE)
  if(!is.null(hip) & !is.null(exp.path))  stop("must provide hip OR exp.path", call. = FALSE)

  if(is.null(exp.path)) exp.path <- hip$path

  ## Determine simulations to run & check for missing directories
  if(simu.names == "all") {
    sims.to.run <- list.dirs(exp.path, recursive = FALSE)
    if(length(sims.to.run) == 0) stop("path does not contain any simulation directories")
  } else {
    sims.to.run <- gsub("//", "/", paste0(exp.path, "//", simu.names))
    if(!all(dir.exists(sims.to.run))) {
      missing.dirs <- sims.to.run[!dir.exists(sims.to.run)]
      missing.error <- paste(c("The following simulations do not exist:",
                               missing.dirs),
                             collapse = "\n")
      stop(missing.error)
    }
  }

  run.log <- list()
  if(parallel) {
    stop("parallel computing not yet supported")
  } else {
    for(i in sims.to.run){
      simu.name <- tail(strsplit(i, "/")[[1]], n = 1)
      r.log <- run_hisafe(path = exp.path, simu.name = simu.name)
      run.log <- c(run.log, r.log)
    }
  }
  invisible(run.log)
}

#' Run a Hi-sAFe simulation
#' @description Runs a Hi-sAFe simulation - calls Hi-sAFe from command line.
#' @return Invisibly returns a console log from simulation run.
#' @param hip An object of class hip with a single simulation (row).
#' If the hip object contains a \code{path} value, then this can be used without providing \code{path} directly to \code{run_hisafe}.
#' If \code{hip} is not provided, then \code{path} and \code{simu.name} are required.
#' @param path A character string of the path to the folder containing the simulation folder. Required if \code{hip} is not provided.
#' @param simu.name Name of the simulation to run. Required if \code{hip} is not provided.
#' @export
#' @family hisafe run functions
#' @examples
#' \dontrun{
#' # Once a group Hi-sAFe simulations (experiment) is defined...
#' myexp <- define_hisafe()
#'
#' # ...and built...
#' myexp <- build_hisafe(myexp, "./simulations")
#'
#' # the simulations can all be run:
#' run_hisafe(myexp)
#' }
run_hisafe <- function(hip = NULL, path = NULL, simu.name = NULL) {

  if(!is.null(hip)){
    if(nrow(hip$hip) > 1) stop("run_hisafe_exp runs multiple simulations. Please use hip object with more than one experiment (row).")
  }
  if(is.null(hip) & is.null(path))        stop("must provide at least one of hip or path", call. = FALSE)
  if(is.null(hip) & is.null(simu.name))   stop("must provide hip OR simu.name", call. = FALSE)
  if(!is.null(hip) & !is.null(simu.name)) stop("must provide hip OR simu.name", call. = FALSE)

  if(is.null(path)) path <- hip$path
  if(is.null(simu.name)) simu.name <- hip$SimulationName

  sim.path <- gsub("//", "/", paste0(path, "/", simu.name, "/"))

  if(!dir.exists(sim.path)) stop("The simulation does not exist.")

  simulationStartTime <- proc.time()[3]
  out <- file(paste0(sim.path, simu.name, "_simulation_log.txt"), open="w")
  cat("Beginning simulation:", simu.name, file = out)
  cat("Beginning simulation:", simu.name)

  ## Change working directory to Capsis directory
  pre.wd <- getwd()
  setwd("/Applications/Capsis/")

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
