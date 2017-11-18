#' Plot a map of the simulated Hi-sAFe scene
#' @description Plots a map of the simulated Hi-sAFe scene.
#' @return Returns a ggplot object. Also writes a pdf of the plot to disk if \code{output.path} is provied.
#' @param hip An object of class \code{hip}.
#' @param simu.name A character string of the name of the simulation within \code{hip} to plot.
#' Required only if there is more than one simulation within \code{hip}.
#' @param output.path A character string of the path where a pdf of the plot should be saved.
#' If \code{NULL}, the default, then no plot is saved to the disk.
#' @export
#' @importFrom dplyr %>%
#' @import ggplot2
#' @family hisafe plot functions
#' @examples
#' \dontrun{
#' # After reading in Hi-sAFe simulation data via:
#' # Once a hip object is created:
#' myhip <- define_hisafe(path = "./simulations", latitude = c(30,60))
#'
#' # Plot the scene of one of the simulations:
#' plot_hisafe_scene(myhip)
#' }
plot_hisafe_scene <- function(hip, simu.name = NULL, output.path = NULL) {
  EXP.PLAN <- hip$exp.plan

  if(nrow(EXP.PLAN) > 1) {
    if(is.null(simu.name)) stop("must provide simu.name if hip contains more than one simulation", call. = FALSE)
    EXP.PLAN <- dplyr::filter(EXP.PLAN, SimulationName == simu.name)
  }

  ## Get used parameters
  TEMPLATE_PARAMS <- get_template_params(get_template_path(hip$template))
  PARAM_NAMES     <- get_param_names(TEMPLATE_PARAMS)
  PARAM_DEFAULTS  <- get_param_vals(TEMPLATE_PARAMS, "value")
  PARAM_COMMENTED <- get_param_vals(TEMPLATE_PARAMS, "commented")
  USED_PARAMS <- purrr::map(as.list(unlist(PARAM_NAMES, use.names = FALSE)),
                            get_used_param,
                            exp.plan = EXP.PLAN,
                            template.defaults = PARAM_DEFAULTS,
                            template.commented = PARAM_COMMENTED)
  names(USED_PARAMS) <- unlist(PARAM_NAMES, use.names = FALSE)

  mainCropSpecies     <- gsub("\\.plt", "", USED_PARAMS$mainCropSpecies$value)
  interCropSpecies    <- gsub("\\.plt", "", USED_PARAMS$interCropSpecies$value)

  ## Calculate total soil depth
  soil.depth <- sum(USED_PARAMS$layers$value$thickness)

  if(USED_PARAMS$nbTrees$value != 0) {
    ## Extract tree data
    tree.plot.data <- tree.data <- USED_PARAMS$tree.initialization$value %>%
      dplyr::mutate(x = treeX, y = treeY) %>%
      dplyr::select(species, x, y)
    num.trees <- nrow(tree.data) #nbTrees is not an allowed entry to a hip object, but is rather modifying by build_hisafe based on nrow(tree.init.table)
  } else {
    tree.plot.data <- tree.data <- tree_init_params("No trees", NA, NA, NA, NA, NA, NA, NA, NA)[[1]] %>%
      dplyr::mutate_if(is.logical, as.numeric) %>%
      dplyr::mutate(x = treeX, y = treeY) %>%
      dplyr::select(species, x, y)
    num.trees <- 0
  }
  ## Calculate scene dimensions
  if(USED_PARAMS$geometryOption$value == 1) {
    width  <- USED_PARAMS$spacingBetweenRows$value / USED_PARAMS$cellWidth$value * sqrt(num.trees)
    height <- USED_PARAMS$spacingWithinRows$value / USED_PARAMS$cellWidth$value * sqrt(num.trees)
  } else {
    width  <- USED_PARAMS$plotWidth$value / USED_PARAMS$cellWidth$value
    height <- USED_PARAMS$plotHeight$value / USED_PARAMS$cellWidth$value
  }

  ## Create plot data
  plot.data <- expand.grid(x = 1:width,
                           y = 1:height,
                           crop = mainCropSpecies,
                           stringsAsFactors = FALSE) %>%
    dplyr::as_tibble() %>%
    dplyr::arrange(desc(y), x) %>%
    dplyr::mutate(id = 1:nrow(.), x = x - USED_PARAMS$cellWidth$value/2, y = y - USED_PARAMS$cellWidth$value/2) # x and y are now cell centers

  if(num.trees > 0) {
    ## Modify tree locations for geometryOption = 1
    if(USED_PARAMS$geometryOption$value == 1) {
      if(num.trees == 1) {
        tree.plot.data$x <- mean(plot.data$x)
        tree.plot.data$y <- mean(plot.data$y)
      } else if(num.trees == 4) {
        tree.plot.data$x <- mean(plot.data$x) * c(1,3,1,3)/2
        tree.plot.data$y <- mean(plot.data$y) * c(1,1,3,3)/2
      } else if(num.trees == 9) {
        tree.plot.data$x <- mean(plot.data$x) * c(1,3,5,1,3,5,1,3,5)/3
        tree.plot.data$y <- mean(plot.data$y) * c(1,1,1,3,3,3,5,5,5)/3
      }
    }

    ## Determine interCrop cells
    create_range <- function(x, tcd) c(x - tcd, x + tcd)
    roundup   <- function(from,to) ceiling(from/to)*to
    rounddown <- function(from,to) floor(from/to)*to
    round_if_needed <- function(x, cw){
      if(all(x %% cw == 0)){
        out <- x
      } else {
        out <- c(rounddown(x[1], cw), roundup(x[2], cw))
      }
      return(out)
    }
    which_inside <- function(boundaries, x.vals) {
      unique(x.vals[x.vals > boundaries[1] & x.vals < boundaries[2]])
    }

    if(USED_PARAMS$treeCropDistance$value > 0) {
      xs <- as.list(tree.plot.data$x)
      boundaries <- purrr::map(xs, create_range, USED_PARAMS$treeCropDistance$value)
      boundaries <- purrr::map(boundaries, round_if_needed, USED_PARAMS$cellWidth$value)
      x.inside   <- unlist(purrr::map(boundaries, which_inside, plot.data$x))
      plot.data$crop[which(plot.data$x %in% x.inside)] <- interCropSpecies
    } else if(USED_PARAMS$weededAreaRadius$value > 0) {
      xs <- as.list(tree.plot.data$x)
      x.boundaries <- purrr::map(xs, create_range, USED_PARAMS$weededAreaRadius$value)
      x.boundaries <- purrr::map(x.boundaries, round_if_needed, USED_PARAMS$cellWidth$value)
      x.inside     <- purrr::map(x.boundaries, which_inside, plot.data$x)

      ys <- as.list(tree.plot.data$y)
      y.boundaries <- purrr::map(ys, create_range, USED_PARAMS$weededAreaRadius$value)
      y.boundaries <- purrr::map(y.boundaries, round_if_needed, USED_PARAMS$cellWidth$value)
      y.inside     <- purrr::map(y.boundaries, which_inside, plot.data$y)

      cells.inside <- purrr::map2(x.inside, y.inside, expand.grid) %>%
        dplyr::bind_rows() %>%
        dplyr::transmute(x = Var1, y = Var2) %>%
        dplyr::mutate(flagged = 1)

      plot.data <- plot.data %>%
        dplyr::left_join(cells.inside, by = c("x", "y"))

      plot.data$crop[which(plot.data$flagged == 1)] <- interCropSpecies
      plot.data$flagged <- NULL
    }
  }

  # weeded.cells <- NA
  # if(weededAreaRadius > 0) {
  #   for(i in 1:nrow(tree.data)){
  #     weeded.xs <- c(tree.data$x[i] + weededAreaRadius, tree.data$x[i] - weededAreaRadius)
  #     weeded.ys <- c(tree.data$y[i] + weededAreaRadius, tree.data$y[i] - weededAreaRadius)
  #     weeded.cells <- c(weeded.cells, expand.grid(weeded.xs, weeded.ys))
  #   }
  #   plot.data$crop[which(plot.data$x %in% intercrop.xs)] <- "baresoil"
  # }

  plot.obj <- ggplot(plot.data, aes(x = x, y = y)) +
    labs(x = paste0(width, "m"),
         y = paste0(height, "m"),
         color = "",
         fill = "",
         title = paste("Scene:", hip$exp.plan$SimulationName),
         caption = paste0("Latitude: ", USED_PARAMS$latitude$value,
                          "deg - Orientation: ", USED_PARAMS$treeLineOrientation$value,
                          "deg - Cell width: ", USED_PARAMS$cellWidth$value,
                          "m - Soil depth: ", soil.depth, "m")) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    geom_tile(color = "black", aes(fill = crop)) +
    geom_text(aes(label = id)) +
    geom_point(data = tree.plot.data, size = 10, aes(color = species), na.rm = TRUE) +
    geom_point(data = tree.plot.data, shape = 21, size = 10, na.rm = TRUE) +
    scale_color_manual(values = c("black", "grey70", "grey30", "grey50")) +
    scale_fill_manual(values = c("white", "grey80")) +
    coord_equal() +
    theme_hisafe_tile() +
    theme(legend.position = "right")

  if(!is.null(output.path)){
    ggsave(filename = clean_path(paste0(output.path, "/", EXP.PLAN$SimulationName, "_Scene.pdf")),
           plot = plot.obj,
           scale = 1,
           height = 8.5,
           width = 11)
  }
  return(plot.obj)
}
