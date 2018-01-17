#' Builds a cluster shell script
#' @description Builds a shell script to initialize Hi-sAFe simulations on the MUSE cluster.
#' @return Invisibly returns \code{TRUE}.
#' @param simu.name A character string of the name of the simulation to create the script for.
#' This must match the name of .SIM file as well as the name of the folder that file is in.
#' @param script.path A character string of the local path for where to create the cluster script file.
#' @param cluster.path A character string of the path on the cluster where the simulation folder is located.
#' @param email A character string of the email address to notify with cluster job updates.
#' @param num.cores The number of cores to request from the cluster.
#' #' @export
#' @family hisafe cluster functions
#' @examples
#' \dontrun{
#' mysim <- build_cluster_script("mysim",
#'                               script.path  = "/simulations/",
#'                               cluster.path = "/nfs/work/hisafe/simulations/user/",
#'                               email        = "me@work.com")
#' }
build_cluster_script <- function(simu.name,
                                 script.path,
                                 cluster.path,
                                 email,
                                 num.cores = length(simu.name)) {

  script.path <- R.utils::getAbsolutePath(script.path)
  if(!dir.exists(script.path))                                                 stop("directory specified by script.path does not exist",             call. = FALSE)
  if(!is.character(simu.name))                                                 stop("simu.name argument must be a character",                        call. = FALSE)
  if(!(is.character(email) & length(email) == 1))                              stop("email argument must be a character vector of length 1",         call. = FALSE)
  if(!((is.numeric(num.cores) & length(num.cores) == 1) | is.null(num.cores))) stop("num.cores argument must be a positive integer",                 call. = FALSE)
  if(!(is.character(cluster.path) & length(cluster.path) == 1))                stop("cluster.path argument must be a character vector of length 1",  call. = FALSE)

  write_script <- function(x) cat(x, file = cluster.script, sep = "\n", append = TRUE)

  cluster.script <- clean_path(paste0(script.path, "/", simu.name, ".sh"))
  dum <- file.create(cluster.script, showWarnings = FALSE)
  cat("", file = cluster.script, sep = "", append = FALSE)

  write_script("#!/bin/sh")
  write_script(paste0("#SBATCH -n ", num.cores))
  write_script("#SBATCH --account=hisafe")
  write_script("#SBATCH --partition=defq")
  write_script("#SBATCH --mail-type=ALL")
  write_script(paste0("#SBATCH --mail-user=", email))
  write_script("module purge")
  write_script("module load jre/jre.8_x64")
  write_script("cd /nfs/work/hisafe/Capsis4")
  write_script(clean_path(paste0("sh capsis.sh -p script safe.pgms.ScriptGen ", cluster.path, "/", simu.name, "/", simu.name, ".sim", collapse = "\n")))

  invisible(TRUE)
}
