#' Builds a Hi-sAFe simulation or experiment
#' @description Builds a Hi-sAFe simulation or experiment (a group of simulations) - creates the folder structure and input files.
#' @return Invisibly returns a list containing the original hip object.
#' @param hip An object of class "hip". To create a hip object see \code{\link{define_hisafe}}.
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
build_hisafe <- function(hip) {
  if(!("hip" %in% class(hip))) stop("data not of class hip", call. = FALSE)

  EXP.PLAN <- hip$exp.plan
  dir.create(hip$path, showWarnings = FALSE, recursive = TRUE)

  if(nrow(EXP.PLAN) > 1) exp.name <- tail(strsplit(hip$path, split = "/", fixed = TRUE)[[1]], 1)

  for(i in 1:nrow(EXP.PLAN)) {
    simu.path <- clean_path(paste0(hip$path, "/", EXP.PLAN$SimulationName[i]))
    if(dir.exists(simu.path)) stop(paste0("A simulation with the name <", EXP.PLAN$SimulationName[i], "> already exisits in this location."), call. = FALSE)
    dir.create(simu.path, showWarnings = FALSE)
  }

  ## Deal with tibble/list cols
  tibble.params <- c("tree.initialization", "root.initialization", "layers", "layer.initialiation")
  isnt_tibble_col <- function(x) !("tbl" %in% class(x[[1]]))
  paste_together  <- function(x) unlist(purrr::map(x, paste, collapse = ","))
  if(any(tibble.params %in% names(EXP.PLAN))) {
    tibble.cols <- tibble.params[which(tibble.params %in% names(EXP.PLAN))]
    exp.plan.to.write <- dplyr::select_if(EXP.PLAN, isnt_tibble_col) %>%
      dplyr::mutate_if(is.list, paste_together)
    find.unique.tibbles <- function(x, tibble.col) {
      comps <- as.list(unique(x[tibble.col]))[[1]]
      des <- 1:length(comps)
      designators <- list()
      for(i in 1:nrow(x)) {
        designator <- des[purrr::map_lgl(comps, identical, y = x[i, tibble.col][[1]][[1]])]
        designators <- c(designators, designator)
      }
      return(unlist(designators))
    }
    for(i in tibble.cols){
      exp.plan.to.write[[i]] <- find.unique.tibbles(EXP.PLAN, i)
      add_sim_names <- function(x, sim.name) {
        x$SimulationName <- sim.name
        x <- select(x, SimulationName, everything())
        return(x)
      }
      tibble.out <- purrr::map2_df(as.list(EXP.PLAN[i])[[1]], EXP.PLAN$SimulationName, add_sim_names)

      if(nrow(EXP.PLAN) > 1) {
        readr::write_csv(tibble.out, clean_path(paste0(hip$path, "/",
                                                       exp.name, "_",
                                                       gsub("\\.", "_", i), "_summary.csv")))
      } else {
        readr::write_csv(tibble.out, clean_path(paste0(hip$path, "/",
                                                       EXP.PLAN$SimulationName, "/",
                                                       EXP.PLAN$SimulationName, "_",
                                                       gsub("\\.", "_", i), "_summary.csv")))
      }
    }
  } else {
    exp.plan.to.write <- EXP.PLAN
  }
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

  hip.to.write.list <- as.list(exp.plan.to.write) %>%
    purrr::pmap(list) %>%
    purrr::map(dplyr::as_tibble)

  purrr::walk2(hip.list,
               hip.to.write.list,
               build_structure,
               path     = hip$path,
               profiles = hip$profiles,
               template = hip$template)

  purrr::walk2(as.list(unique(EXP.PLAN$SimulationName)),
               as.list(clean_path(paste0(hip$path, "/", EXP.PLAN$SimulationName))),
               plot_hisafe_scene,
               hip = hip)

  invisible(hip)
}

#' Builds the structure of a Hi-sAFe simulation
#' @description Does the heavy lifting for \code{\link{build_hisafe}}.
#' @return Invisibly returns a list containing the original hip object and supplied path.
#' @param exp.plan The exp.plan element of a "hip" object, containing a single row.
#' @param exp.plan.to.write Same as \code{exp.plan} except that complex list/tibble elements are simplified to a number.
#' @param path A character string of the path to the simulation folder.
#' @param profiles A character vector of export profiles the simulation to export.
#' @param template A character string of the path to the Hi-sAFe directory structure/files to use as a template (or one of the strings signaling a default template)
build_structure <- function(exp.plan, exp.plan.to.write, path, profiles, template) {

  template.path   <- get_template_path(template)
  TEMPLATE_PARAMS <- get_template_params(template.path)
  PARAM_NAMES     <- get_param_names(TEMPLATE_PARAMS)
  PARAM_DEFAULTS  <- get_param_vals(TEMPLATE_PARAMS, "value")

  ## Copy over folder structure & template files from Hi-sAFe template path
  ## Any newly built files below will overwrite these files
  simu.path <- clean_path(paste0(path, "/", exp.plan$SimulationName))
  system(paste("cp -r", template.path, simu.path))

  ## Write out experiment summary
  readr::write_csv(exp.plan.to.write, clean_path(paste0(simu.path, "/", exp.plan$SimulationName, "_simulation_summary.csv")))

  ## Move weather file if one was provided
  wth.name <- "weather.wth"
  if("weatherFile" %in% names(exp.plan)) {
    wth.name <- tail(strsplit(exp.plan$weatherFile, split = "/", fixed = TRUE)[[1]], 1)
    dum <- file.remove(paste0(simu.path, "/weather/weather.wth"))
    dum <- file.copy(exp.plan$weatherFile, paste0(simu.path, "/weather/", wth.name))
  }

  ## Remove unused .plt files from cropSpecies
  if("mainCropSpecies" %in% names(exp.plan)){
    main.crop.used <- exp.plan$mainCropSpecies
  } else {
    main.crop.used <- PARAM_DEFAULTS$mainCropSpecies
  }
  if("interCropSpecies" %in% names(exp.plan)){
    inter.crop.used <- exp.plan$interCropSpecies
  } else {
    inter.crop.used <- PARAM_DEFAULTS$interCropSpecies
  }
  existing.plt <- list.files(paste0(simu.path, "/cropSpecies"), full.names = TRUE)
  required.plt <- paste0(simu.path, "/cropSpecies/", c(main.crop.used, inter.crop.used))
  remove.plt   <- existing.plt[!(existing.plt %in% required.plt)]
  dum <- purrr::map(remove.plt, file.remove)

  ## Remove unused .tec files from itk
  if("mainCropItk" %in% names(exp.plan)){
    main.itk.used <- exp.plan$mainCropItk
  } else {
    main.itk.used <- PARAM_DEFAULTS$mainCropItk
  }
  if("interCropItk" %in% names(exp.plan)){
    inter.itk.used <- exp.plan$interCropItk
  } else {
    inter.itk.used <- PARAM_DEFAULTS$interCropItk
  }
  existing.itk <- list.files(paste0(simu.path, "/itk"), full.names = TRUE)
  required.itk <- paste0(simu.path, "/itk/", c(main.itk.used, inter.itk.used))
  remove.itk   <- existing.itk[!(existing.itk %in% required.itk)]
  dum <- purrr::map(remove.itk, file.remove)

  ## Remove unused .tree files from treeSpecies
  if("tree.initialization" %in% names(exp.plan)){
    trees.used <- exp.plan$tree.initialization[[1]]$species
  } else {
    trees.used <- PARAM_DEFAULTS$tree.initialization$species
  }
  if("nbTrees" %in% names(exp.plan)){
    num.trees <- exp.plan$nbTrees
  } else {
    num.trees <- length(trees.used)
  }

  existing.tree <- list.files(paste0(simu.path, "/treeSpecies"), full.names = TRUE)
  required.tree <- paste0(simu.path, "/treeSpecies/", trees.used, ".tree")
  remove.tree   <- existing.tree[!(existing.tree %in% required.tree)]
  dum <- purrr::map(remove.tree, file.remove)

  existing.EP <- list.files(paste0(simu.path, "/exportParameters"), pattern = "\\.pro", full.names = TRUE)
  required.EP <- paste0(simu.path, "/exportParameters/", profiles, ".pro")
  remove.EP   <- existing.EP[!(existing.EP %in% required.EP)]
  dum <- purrr::map(remove.EP, file.remove)

  ## Edit files
  params.to.edit <- names(dplyr::select(exp.plan, -SimulationName))
  sim.params.to.edit  <- params.to.edit[params.to.edit %in% PARAM_NAMES$sim]
  pld.params.to.edit  <- params.to.edit[params.to.edit %in% PARAM_NAMES$pld]
  tree.params.to.edit <- params.to.edit[params.to.edit %in% PARAM_NAMES$tree]

  ## Edit pld file
  pld.path <- paste0(simu.path, "/plotDescription/template.pld")
  pld <- read_param_file(pld.path)
  pld.new <- edit_param_file(pld, dplyr::select(exp.plan, pld.params.to.edit)) %>%
    edit_param_element("nbTrees", exp.plan$SimulationName) %>%
    edit_param_element("country", exp.plan$SimulationName) %>%
    edit_param_element("townShip", exp.plan$SimulationName) %>%
    edit_param_element("site", exp.plan$SimulationName) %>%
    edit_param_element("name", exp.plan$SimulationName) %>%
    edit_param_element("longitude", "NA") %>%
    edit_param_element("elevation", "NA")
  write_param_file(pld.new, pld.path)
  dum <- file.rename(pld.path, paste0(simu.path, "/plotDescription/", exp.plan$SimulationName, ".pld"))

  ## Edit sim file
  sim.path <- paste0(simu.path, "/template.sim")
  sim <- read_param_file(sim.path)
  sim.new <- edit_param_file(sim, dplyr::select(exp.plan, sim.params.to.edit)) %>%
    edit_param_element("pldFileName", paste0(exp.plan$SimulationName, ".pld")) %>%
    edit_param_element("weatherFile", wth.name) %>%
    edit_param_element("profileNames", paste0(profiles, collapse = ",")) %>%
    edit_param_element("exportFrequencies", paste0(SUPPORTED.PROFILES$freqs[SUPPORTED.PROFILES$profiles %in% profiles], collapse = ","))
  write_param_file(sim.new, sim.path)
  dum <- file.rename(sim.path, paste0(simu.path, "/", exp.plan$SimulationName, ".sim"))

  ## Edit tree file
  tree.path <- list.files(paste0(simu.path, "/treeSpecies"), pattern = "\\.tree", full.names = TRUE)
  for(i in tree.path) {
    tree <- read_param_file(i)
    if(length(tree.params.to.edit > 0)) {
      tree.new <- edit_param_file(tree, dplyr::select(exp.plan, tree.params.to.edit))
    } else {
      tree.new <- tree
    }
    write_param_file(tree.new, i)
  }
  invisible(hip)
}
