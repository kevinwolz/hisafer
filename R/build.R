#' Build a Hi-sAFe simulation or experiment
#' @description Builds a Hi-sAFe simulation or experiment (a group of simulations) - creates the folder structure and input files.
#' @return Invisibly returns a list containing the original hip object.
#' @param hip An object of class "hip". To create a hip object see \code{\link{define_hisafe}}.
#' @param files A character string of file types indicating which simulation files to build. Use "all" to write all required simulation files.
#' Otherwise, select one or more of "sim", "pld", "wth", "tree", "plt", "tec", "ttec", "par", and "pro".
#' @param plot.scene Logical indicating whether \code{\link{plot_hisafe_scene}} should be used to export plots of each scene during the build.
#' @param summary.files Logical indicating whether or not to write out summary .CSV files about the experiment and each simulation during the build.
#' @param stics.diagnostics Logical indicating whether or not STICS diagnostics files should be exported in the simulation.
#' @export
#' @importFrom dplyr %>%
#' @family hisafe build functions
#' @examples
#' \dontrun{
#' # For a single Hi-sAFe simulation
#' mysim <- define_hisafe(path = "./simulations", latitude = 30)
#'
#' # Building the simulation folder structure & files:
#' build_hisafe(mysim)
#'
#' # Once a group Hi-sAFe simulations (experiment) is defined:
#' myexp <- define_hisafe(path = "./simulations", latitude = c(30,60))
#'
#' # Building the experiment folder structure & files:
#' build_hisafe(myexp)
#' }
build_hisafe <- function(hip,
                         files             = "all",
                         plot.scene        = TRUE,
                         summary.files     = TRUE,
                         stics.diagnostics = FALSE) {
  is_hip(hip, error = TRUE)
  is_TF(plot.scene)

  allowed.files <- c("sim", "pld", "wth", "tree", "plt", "tec", "ttec", "par")
  if(files[1] == "all") files <- allowed.files
  if(!all(files %in% allowed.files)) stop(paste0("files argument must be 'all' or one or more of ",
                                                 paste(allowed.files, collapse = ", ")), call. = FALSE)

  EXP.PLAN <- hip$exp.plan
  dir.create(hip$path, showWarnings = FALSE, recursive = TRUE)

  if(nrow(EXP.PLAN) > 1) exp.name <- basename(hip$path)

  for(i in 1:nrow(EXP.PLAN)) {
    simu.path <- clean_path(paste0(hip$path, "/", EXP.PLAN$SimulationName[i]))
    if(dir.exists(simu.path)) stop(paste0("A simulation with the name <", EXP.PLAN$SimulationName[i], "> already exisits in this location."), call. = FALSE)
  }

  ## Write out experiment summary
  paste_together  <- function(x) unlist(purrr::map(x, paste, collapse = ";"))
  exp.plan.to.write <- dplyr::mutate_if(EXP.PLAN, is.list, paste_together)
  if(nrow(EXP.PLAN) > 1) readr::write_csv(exp.plan.to.write, clean_path(paste0(hip$path, "/", exp.name, "_exp_summary.csv")))

  ## build folder tree & input files for each simulation in experiment
  create_tibble <- function(x) {
    y <- list()
    for(i in names(x)) {
      if(length(x[[i]]) > 1){
        y[[i]] <- list(x[[i]])
      } else {
        y[[i]] <- x[[i]]
      }

    }
    return(dplyr::as_tibble(y))
  }

  hip.list <- as.list(EXP.PLAN) %>%
    purrr::pmap(list) %>%
    purrr::map(create_tibble)

  purrr::walk(hip.list,
              build_structure,
              path              = hip$path,
              template          = hip$template,
              files             = files,
              plot.scene        = plot.scene,
              summary.files     = summary.files,
              stics.diagnostics = stics.diagnostics)

  if(plot.scene) {
    purrr::walk2(as.list(unique(EXP.PLAN$SimulationName)),
                 as.list(clean_path(paste0(hip$path, "/", EXP.PLAN$SimulationName))),
                 plot_hisafe_scene,
                 hip = hip)
  }

  invisible(hip)
}

#' Builds the structure of a Hi-sAFe simulation
#' @description Does the heavy lifting for \code{\link{build_hisafe}}.
#' @return Invisibly returns a list containing the original hip object and supplied path.
#' @param exp.plan The exp.plan element of a "hip" object, containing a single row.
#' @param path A character string of the path to the simulation folder.
#' @param template A character string of the path to the Hi-sAFe directory structure/files to use as a template
#' (or one of the strings signaling a default template)
#' @param files A character string of file types indicating which simulation files to build. Use "all" to write all required simulation files.
#' Otherwise, select one or more of "sim", "pld", "wth", "tree", "plt", "tec", "ttec", "par", and "pro".
#' @param plot.scene Logical indicating whether \code{\link{plot_hisafe_scene}} should be used to export plots of each scene during the build.
#' @param summary.files Logical indicating whether or not to write out summary .CSV files about the experiment and each simulation during the build.
#' @param stics.diagnostics Logical indicating whether or not STICS diagnostics files should be exported in the simulation.
#' @keywords internal
build_structure <- function(exp.plan, path,  template, files, plot.scene, summary.files, stics.diagnostics) {



  TEMPLATE_PARAMS <- get_template_params(template)
  PARAM_NAMES     <- get_param_names(TEMPLATE_PARAMS)
  PARAM_DEFAULTS  <- get_param_vals(TEMPLATE_PARAMS, "value")
  PARAM_COMMENTED <- get_param_vals(TEMPLATE_PARAMS, "commented")

  ## Copy over folder structure & template files from Hi-sAFe template path
  ## Any newly built files below will overwrite these files
  template.dir <- clean_path(paste0(dirname(template), "/"))
  if(path == template.dir) stop("cannot build simulations within the same folder as template directory", call. = FALSE)
  copy_hisafe_template(template, path, overwrite = FALSE, new.name = exp.plan$SimulationName)
  simu.path <- clean_path(paste0(path, "/", exp.plan$SimulationName))
  if(plot.scene | summary.files) dir.create(paste0(simu.path, "/support"), showWarnings = FALSE)

  ## Write out simulation summary
  paste_together  <- function(x) unlist(purrr::map(x, paste, collapse = ";"))
  exp.plan.to.write <- dplyr::mutate_if(exp.plan, is.list, paste_together)
  if(summary.files) readr::write_csv(exp.plan.to.write, clean_path(paste0(simu.path, "/support/", exp.plan$SimulationName, "_simulation_summary.csv")))

  ## Move weather file if one was provided
  if("weatherFile" %in% names(exp.plan)) {
    dum <- file.remove(list.files(simu.path, ".wth$", full.names = TRUE))
    dum <- file.copy(exp.plan$weatherFile, simu.path)
  }

  ## Remove unused .tree files from treeSpecies
  trees.used <- NA
  if("trees" %in% names(exp.plan)){
    trees.used <- exp.plan$tree[[1]]$species
  } else if(length(PARAM_DEFAULTS$tree)>0) {
    trees.used <- PARAM_DEFAULTS$tree[[1]]$treeSpeciesFileName
  }

  if(all(is.na(trees.used))) {
    num.trees <- 0
  } else {
    num.trees <- length(trees.used)
  }

  existing.tree <- list.files(paste0(simu.path, "/treeSpecies"), full.names = TRUE)
  required.tree <- paste0(simu.path, "/treeSpecies/", trees.used, ".tree")
  remove.tree   <- existing.tree[!(existing.tree %in% required.tree)]
  dum <- purrr::map(remove.tree, file.remove)


  ## Remove unused .ttec files from treeIntervention
  if("ttec" %in% names(exp.plan)){
    ttec.used <- exp.plan$treetec[[1]]$treeTecFileName
  } else if(length(PARAM_DEFAULTS$treetec)>0) {
    ttec.used <- PARAM_DEFAULTS$treetec[[1]]$treeTecFileName
  } else {
    ttec.used <- NA
  }

  if(all(is.na(ttec.used))) {
    num.ttec <- 0
  } else {
    num.ttec <- length(ttec.used)
  }

  existing.ttec <- list.files(paste0(simu.path, "/treeInterventions"), full.names = TRUE)
  required.ttec <- paste0(simu.path, "/treeInterventions/", ttec.used)
  remove.ttec   <- existing.ttec[!(existing.ttec %in% required.ttec)]
  dum <- purrr::map(remove.ttec, file.remove)


  ## Remove unused .tec files from cropIntervention
  newtec.used <- ""
  if("tec" %in% names(exp.plan)){
    tec.used <- exp.plan$zone[[1]]$zoneTecFileNameList
  } else if(PARAM_COMMENTED$zone== FALSE) {
    tec.used <- PARAM_DEFAULTS$zone[[1]]$zoneTecFileNameList
  } else {
    tec.used <- NA
  }

  for (i in  tec.used) {

    crop.itks.used <- strsplit(i, split = ",", fixed = TRUE)

    for (j in  crop.itks.used) {
      test <- strsplit(j, split = " ", fixed = TRUE)
      newtec.used <- c(newtec.used, test)
    }
  }

  existing.tec <- list.files(paste0(simu.path, "/cropInterventions"), full.names = TRUE)
  oldrequired.tec <- paste0(simu.path, "/cropInterventions/", tec.used)
  required.tec <- paste0(simu.path, "/cropInterventions/", newtec.used)
  remove.tec   <- existing.tec[!(existing.tec %in% required.tec)]
  dum <- purrr::map(remove.tec, file.remove)


  ## Remove unused .plt files from cropSpecies
  plt.used <- NA
  existing.plt <- list.files(paste0(simu.path, "/cropSpecies"), full.names = TRUE)
  tec.path <- list.files(paste0(simu.path, "/cropInterventions"), pattern = "\\.tec$", full.names = TRUE)

  for(i in tec.path) {
    mytec      <- read_param_file(i)
    new_row = c(paste0(simu.path, "/cropSpecies/", mytec[[1]]$species$value))
    plt.used  <- rbind(plt.used ,new_row)

  }

  remove.plt   <- existing.plt[!(existing.plt %in% plt.used)]
  dum <- purrr::map(remove.plt, file.remove)


  ## Edit files
  params.to.edit        <- names(dplyr::select(exp.plan, -SimulationName))
  sim.params.to.edit    <- params.to.edit[params.to.edit %in% PARAM_NAMES$sim]
  pld.params.to.edit    <- params.to.edit[params.to.edit %in% PARAM_NAMES$pld]
  tree.params.to.edit   <- params.to.edit[params.to.edit %in% PARAM_NAMES$tree]
  crop.params.to.edit   <- params.to.edit[params.to.edit %in% PARAM_NAMES$crop]
  ttec.params.to.edit   <- params.to.edit[params.to.edit %in% PARAM_NAMES$ttec]
  tec.params.to.edit    <- params.to.edit[params.to.edit %in% PARAM_NAMES$tec]
  hisafe.params.to.edit <- params.to.edit[params.to.edit %in% PARAM_NAMES$hisafe]
  stics.params.to.edit  <- params.to.edit[params.to.edit %in% PARAM_NAMES$stics]


  ## Edit pld file
  pld.path <- list.files(simu.path, ".pld$", full.names = TRUE)
  pld      <- read_param_file(pld.path)
  pld.new  <- edit_param_file(pld, dplyr::select(exp.plan, pld.params.to.edit))
  write_param_file(pld.new, pld.path)
  dum <- file.rename(pld.path, paste0(simu.path, "/", exp.plan$SimulationName, ".pld"))

  ## Edit sim file
  sim.path <- list.files(simu.path, ".sim$", full.names = TRUE)
  sim      <- read_param_file(sim.path)
  sim.new  <- edit_param_file(sim, dplyr::select(exp.plan, sim.params.to.edit))
  write_param_file(sim.new, sim.path)
  dum <- file.rename(sim.path, paste0(simu.path, "/", exp.plan$SimulationName, ".sim"))



## Edit tree files
  tree.path <- list.files(paste0(simu.path, "/treeSpecies"), pattern = "\\.tree$", full.names = TRUE)
  for(i in tree.path) {
    tree <- read_param_file(i)
    if(length(tree.params.to.edit > 0)) {
      tree.new <- edit_param_file(tree, dplyr::select(exp.plan, tree.params.to.edit))
    } else {
      tree.new <- tree
    }
    write_param_file(tree.new, i)
  }


  ## Edit ttec files
  ttec.path <- list.files(paste0(simu.path, "/treeInterventions"), pattern = "\\.ttec$", full.names = TRUE)
  for(i in ttec.path) {
    ttec <- read_param_file(i)

    if(length(ttec.params.to.edit > 0)) {
      ttec.new <- edit_param_file(ttec, dplyr::select(exp.plan, ttec.params.to.edit))
    } else {
      ttec.new <- ttec
    }
    write_param_file(ttec.new, i)
  }


  ## Edit crop files
  crop.path <- list.files(paste0(simu.path, "/cropSpecies"), pattern = "\\.plt$", full.names = TRUE)
  for(i in crop.path) {
    crop <- read_param_file(i)
    if(length(crop.params.to.edit > 0)) {
      crop.new <- edit_param_file(crop, dplyr::select(exp.plan, crop.params.to.edit))
    } else {
      crop.new <- crop
    }
    write_param_file(crop.new, i)
  }

  ## Edit tec files
  tec.path <- list.files(paste0(simu.path, "/cropInterventions"), pattern = "\\.tec$", full.names = TRUE)
  for(i in tec.path) {
    tec <- read_param_file(i)
    if(length(tec.params.to.edit > 0)) {
      tec.new <- edit_param_file(tec, dplyr::select(exp.plan, tec.params.to.edit))
    } else {
      tec.new <- tec
    }
    write_param_file(tec.new, i)
  }





  ## Edit Hi-sAFe general parameters file
  hisafe.path <- paste0(simu.path, "/generalParameters/hisafe.par")
  hisafe      <- read_param_file(hisafe.path)
  hisafe.new  <- edit_param_file(hisafe, dplyr::select(exp.plan, hisafe.params.to.edit))
  write_param_file(hisafe.new, hisafe.path)

  ## Edit STICS general parameters file
  stics.path <- paste0(simu.path, "/generalParameters/stics.par")
  stics      <- read_param_file(stics.path)
  stics.new  <- edit_param_file(stics, dplyr::select(exp.plan, stics.params.to.edit))
  write_param_file(stics.new, stics.path)

  ## Delete files that are not desired
  remove_files <- function(x, y) if(!(x %in% files)) unlink(paste0(simu.path, y), recursive = TRUE)
  file.names     <- c("sim", "pld", "wth", "tree", "plt", "ttec", "tec", "par")
  file.locations <- c("/*.sim", "/*.pld", "/*.wth", "/treeSpecies", "/cropSpecies",
                      "/treeInterventions", "/cropInterventions", "/generalParameters")
  purrr::map2(file.names, file.locations, remove_files)

  invisible(TRUE)
}
