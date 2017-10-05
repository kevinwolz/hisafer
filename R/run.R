call_hisafe_exp <- function(path,
                        exp.name,
                        simu.names = "all",
                        parallel = FALSE) {

  ## Determine simulations to run & check for missing directories
  exp.path <- gsub("//", "/", paste0(path, "/", exp.name))
  if(simu.names == "all") {
    sims.to.run <- list.dirs(exp.path, recursive = FALSE)
    if(length(sims.to.run) == 0) {
      stop("path does not contain any simulation directories")
    }
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

  ## Call Hi-sAFe via command line
  if(parallel) {
    stop("parallel computing not yet supported")
  } else {
    out <- file(paste0(exp.path, "/", exp.name, "_simulation_log.txt"), open="w")
    for(i in sims.to.run){
      cat("Beginning simulation: ", i, "\n\n", sep="", file=out)
      simulationStartTime <- proc.time()[3]
      call_hisafe(exp.path, i)
      simulationElapsedTime <- signif((proc.time()[3] - simulationStartTime)/60,3)
      cat("Done with simulation: ", i, ". This took ", simulationElapsedTime, " minutes.\n", sep="", file=out)
    }
  }
}

call_hisafe <- function(path, sim.name) {
  simulationStartTime <- proc.time()[3]
  setwd("/Applications/Capsis/")
  sim.path <- gsub("//", "/", paste0(exp.path, "/", sim.name, "/", sim.name, ".sim"))
  call <- paste0("sh capsis.sh -p script safe.pgms.ScriptGen ", sim.path)
  log <- system(call, wait = TRUE, intern = TRUE)
  simulationElapsedTime <- signif((proc.time()[3] - simulationStartTime)/60,3)
  cat("Done with simulation: ", sim.name, ". This took ", simulationElapsedTime, " minutes.\n", sep="", file=out)
  invisible(log)
}
