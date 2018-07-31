#' Builds a cluster shell script
#' @description Builds a shell script to initialize Hi-sAFe simulations on the MUSE cluster.
#' @return Invisibly returns \code{TRUE}.
#' @param hip A hip object. If \code{NULL}, then \code{simu.names} and \code{script.path} are required.
#' @param simu.names A character vector of the names of the simulations to create the script for.
#' This must match the names of .SIM files as well as the names of the folders that files are in.
#' @param script.path A character string of the local path for where to create the cluster script file.
#' If \code{NULL}, \code{hip} must be provided, and the path used will be the path element of the hip object.
#' @param script.name A character string of the name of the cluster script file to call.
#' @param simu.prefix A character string of the SimulationName prefix.
#' @param model.version A character string of the name of the Capsis folder (version) to call on the cluster.
#' @param launch.call The name of the safe lanuch script to use (one of 'ScriptGen' or 'ScriptCalib')
#' @param default.folder The folder in safe/data/SimSettings to use for parameter files that are not found in the simulation folder.
#' @param cluster.path A character string of the path on the cluster where the simulation folder is located.
#' @param email.type Type of email notification. Valid type values are NONE, BEGIN, END, FAIL, REQUEUE, ALL.
#' Multiple values may be specified in a comma separated list within a single character string (e.g. "BEGIN,END").
#' @param email A character string of the email address to notify with cluster job updates. Use \code{NULL} for no email generation.
#' @param num.cores The number of cores to request from the cluster. Use \code{NULL} to make no specification.
#' @export
#' @family hisafe cluster functions
#' @examples
#' \dontrun{
#' mysim <- build_cluster_script("mysim",
#'                               script.path  = "/simulations/",
#'                               cluster.path = "/nfs/work/hisafe/simulations/user/",
#'                               email        = "me@work.com")
#' }
build_cluster_script <- function(hip            = NULL,
                                 simu.names     = NULL,
                                 script.path    = NULL,
                                 script.name    = NULL,
                                 simu.prefix    = NULL,
                                 model.version  = "Capsis4",
                                 launch.call    = "ScriptGen",
                                 default.folder = "",
                                 cluster.path,
                                 email.type     = "END",
                                 email          = NULL,
                                 num.cores      = NULL) {

  is_hip(hip, error = TRUE)
  if(is.null(hip) & is.null(script.path))                                      stop("must provide hip or script.path",                                call. = FALSE)
  if(!(all(is.character(simu.names)) | is.null(simu.names)))                   stop("simu.names argument must be 'all' or a character vector",        call. = FALSE)
  if(!((is.character(email) & length(email) == 1) | is.null(email)))           stop("email argument must be a character vector of length 1",          call. = FALSE)
  if(!((is.numeric(num.cores) & length(num.cores) == 1) | is.null(num.cores))) stop("num.cores argument must be a positive integer",                  call. = FALSE)
  if(!((is.character(script.path) & length(script.path) == 1) | is.null(script.path))) stop("script.path argument must be a character vector of length 1",
                                                                                            call. = FALSE)
  if(!((is.character(script.name) & length(script.name) == 1) | is.null(script.name))) stop("script.name argument must be a character vector of length 1",
                                                                                            call. = FALSE)
  if(!(is.character(cluster.path)   & length(cluster.path)   == 1))            stop("cluster.path argument must be a character vector of length 1",   call. = FALSE)
  if(!(is.character(launch.call)    & length(launch.call)    == 1))            stop("launch.call argument must be a character vector of length 1",    call. = FALSE)
  if(!(is.character(default.folder) & length(default.folder) == 1))            stop("default.folder argument must be a character vector of length 1", call. = FALSE)
  if(is.null(email) & email.type != "NONE")                                    stop("email is required if email.type is not 'NONE'",                  call. = FALSE)

  if(default.folder != "") default.folder <- paste0(" ", default.folder)

  if(!is.null(hip)) {
    if(is.null(script.path)) script.path <- hip$path
    if(is.null(simu.names))  simu.names  <- hip$exp.plan$SimulationName
  } else {
    script.path <- R.utils::getAbsolutePath(script.path)
    if(!dir.exists(script.path)) stop("directory specified by script.path does not exist", call. = FALSE)
    if(is.null(simu.names))      stop("simu.names cannot by NULL if hip is not provided",  call. = FALSE)
  }

  ## Determine if sequential script can be built
  SEQ <- FALSE
  simu.names.split <- strsplit(simu.names, "_\\s*(?=[^_]+$)", perl=TRUE)
  simu.prefix.guess <- simu.names.split[[1]][1]
  if(!is.null(simu.prefix)) {
    SEQ <- TRUE
    seqs <- as.numeric(gsub(simu.prefix, "", simu.names))
  } else if(all(grepl(paste0(simu.prefix.guess, "_[0-9]+$"), simu.names) & map_dbl(simu.names.split, length) > 1)) {
    SEQ <- TRUE
    seqs <- as.numeric(purrr::map_chr(simu.names.split, 2))
    simu.prefix <- paste0(simu.prefix.guess, "_")
  }

  write_script <- function(i, SEQ) {
    write_line   <- function(x) cat(x, file = cluster.script, sep = "\n", append = TRUE)
    if(SEQ) {
      if(is.null(script.name)) j <- "batch" else j <- script.name
    } else j <- i
    cluster.script <- file(description = clean_path(paste0(script.path, "/", j, ".sh")),
                           open        = "wb",
                           encoding    = "UTF-8")
    cat("", file = cluster.script, sep = "", append = FALSE)
    write_line("#!/bin/sh")
    if(!is.null(num.cores)) write_line(paste0("#SBATCH -n ", num.cores))
    if(SEQ) write_line(paste0("#SBATCH --array=", min(seqs), "-", max(seqs)))
    write_line("#SBATCH --account=hisafe")
    write_line("#SBATCH --partition=defq")
    if(!is.null(email) & email.type != "NONE") {
      write_line(paste0("#SBATCH --mail-type=", email.type))
      write_line(paste0("#SBATCH --mail-user=", email))
    }
    write_line("module purge")
    write_line("module load jre/jre.8_x64")
    write_line(paste0("cd /nfs/work/hisafe/", model.version))
    write_line(clean_path(paste0("sh capsis.sh -p script safe.pgms.", launch.call, " ", cluster.path, "/", i, "/", i, ".sim", default.folder, collapse = "\n")))
    close(cluster.script)
  }

  if(SEQ) {
    write_script(paste0(simu.prefix, "$SLURM_ARRAY_TASK_ID"), SEQ = TRUE)
  } else {
    purrr::walk(simu.names, write_script, SEQ = FALSE)

    if(is.null(script.name)) script.name <- "job"
    job.script <- file(description = clean_path(paste0(script.path, "/", script.name, ".sh")),
                       open        = "wb",
                       encoding    = "UTF-8")
    cat("#!/bin/sh", file = job.script, sep = "\n")
    purrr::map(paste0("sbatch ", simu.names, ".sh"),
               cat,
               file   = job.script,
               sep    = "\n",
               append = TRUE)
    close(job.script)
  }

  invisible(TRUE)
}
