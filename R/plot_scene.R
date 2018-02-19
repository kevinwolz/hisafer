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

  is_hip(hip, error = TRUE)
  if(!(is.character(output.path) | is.null(output.path))) stop("output.path argument must be a character vector", call. = FALSE)

  if(nrow(hip$exp.plan) > 1) {
    if(is.null(simu.name))                                  stop("must provide simu.name if hip contains more than one simulation", call. = FALSE)
    if(!(is.character(simu.name) & length(simu.name) == 1)) stop("simu.name argument must be a character vector of length 1",       call. = FALSE)
    if(!(simu.name %in% hip$exp.plan$SimulationName))       stop("simu.name not present in hip",                                    call. = FALSE)
    hip$exp.plan <- dplyr::filter(hip$exp.plan, SimulationName == simu.name)
  }

  USED_PARAMS <- get_used_params(hip)
  get_used <- function(param) USED_PARAMS[[param]]$value[[1]]

  mainCropSpecies  <- gsub("\\.plt", "", get_used("mainCropSpecies")[1])
  interCropSpecies <- gsub("\\.plt", "", get_used("interCropSpecies")[1])
  toric <- purrr::map_dbl(c("toricXp", "toricXn", "toricYp", "toricYn"), get_used)
  toric.lab <- ifelse(any(toric == 1), paste(c("Xp", "Xn", "Yp", "Yn")[as.logical(toric)], collapse = ","), "off")
  toric.x.both <- ifelse(sum(toric[1:2]) == 2, TRUE, FALSE)
  toric.y.both <- ifelse(sum(toric[3:4]) == 2, TRUE, FALSE)

  ## Calculate total soil depth
  soil.depth <- sum(as.numeric(get_used("layers")$thick))

  if(get_used("nbTrees") != 0) {
    ## Extract tree data
    tree.plot.data <- get_used("tree.initialization") %>%
      dplyr::mutate(x = treeX / get_used("cellWidth"),
                    y = treeY / get_used("cellWidth")) %>%
      dplyr::select(species, x, y)
    num.trees <- nrow(tree.plot.data) #nbTrees is not an allowed entry to a hip object, but is rather modifying by build_hisafe based on nrow(tree.init.table)
  } else {
    tree.plot.data <- tree_init_params("No trees", NA, NA, NA, NA, NA, NA, NA, NA)[[1]] %>%
      dplyr::mutate_if(is.logical, as.numeric) %>%
      dplyr::mutate(x = treeX, y = treeY) %>%
      dplyr::select(species, x, y)
    num.trees <- 0
  }
  ## Calculate scene dimensions (cell size for plotting is always "1", but more/less cells added and labels adjusted based actual dimensions)
  if(get_used("geometryOption") == 1) {
    WIDTH.lab  <- get_used("spacingBetweenRows")
    HEIGHT.lab <- get_used("spacingWithinRows")
    WIDTH  <- WIDTH.lab   / get_used("cellWidth") * sqrt(num.trees)
    HEIGHT <- HEIGHT.lab  / get_used("cellWidth") * sqrt(num.trees)
  } else {
    WIDTH.lab  <- get_used("plotWidth")
    HEIGHT.lab <- get_used("plotHeight")
    WIDTH  <- WIDTH.lab  / get_used("cellWidth")
    HEIGHT <- HEIGHT.lab / get_used("cellWidth")
  }

  ## Create plot data
  plot.data <- expand.grid(x    = 1:WIDTH,
                           y    = 1:HEIGHT,
                           crop = mainCropSpecies,
                           stringsAsFactors = FALSE) %>%
    dplyr::as_tibble() %>%
    dplyr::arrange(desc(y), x) %>%
    dplyr::mutate(id = 1:nrow(.),
                  x  = x - 0.5,
                  y  = y - 0.5) # x and y are now cell centers

  if(num.trees > 0) {
    ## Modify tree locations for geometryOption = 1
    if(get_used("geometryOption") == 1) {
      if(num.trees == 1) {
        tree.plot.data$x <- mean(plot.data$x)
        tree.plot.data$y <- mean(plot.data$y)
      } else if(num.trees == 4) {
        tree.plot.data$x <- mean(plot.data$x) * c(1, 3, 1, 3) / 2
        tree.plot.data$y <- mean(plot.data$y) * c(1, 1, 3, 3) / 2
      } else if(num.trees == 9) {
        tree.plot.data$x <- mean(plot.data$x) * c(1, 3, 5, 1, 3, 5, 1, 3, 5) / 3
        tree.plot.data$y <- mean(plot.data$y) * c(1, 1, 1, 3, 3, 3, 5, 5, 5) / 3
      }
    }

    ## Determine interCrop cells
    create_range    <- function(x, tcd)   c(x - tcd, x + tcd)
    roundup         <- function(from, to) ceiling(from / to) * to
    rounddown       <- function(from, to) floor(from   / to) * to
    round_if_needed <- function(x, cw){
      if(all(x %% cw == 0)){
        out <- x
      } else {
        out <- c(rounddown(x[1], cw), roundup(x[2], cw))
      }
      return(out)
    }
    which_inside    <- function(boundaries, x.vals) {
      unique(x.vals[x.vals > boundaries[1] & x.vals < boundaries[2]])
    }
    check_x_runover <- function(boundaries) {
      low.inside <- high.inside <- NULL
      if(any(unlist(boundaries) < 0)) {
        low.runover <- min(unlist(boundaries)[unlist(boundaries) < 0]) + WIDTH
        low.inside  <- unlist(purrr::map(list(c(low.runover, WIDTH)), which_inside, plot.data$x))
      }
      if(any(unlist(boundaries) > WIDTH)) {
        high.runover <- max(unlist(boundaries)[unlist(boundaries) > WIDTH]) - WIDTH
        high.inside  <- unlist(purrr::map(list(c(0, high.runover)), which_inside, plot.data$x))
      }
      return(c(low.inside, high.inside))
    }
    check_y_runover <- function(boundaries) {
      low.inside <- high.inside <- NULL
      if(any(unlist(boundaries) < 0)) {
        low.runover <- min(unlist(boundaries)[unlist(boundaries) < 0]) + HEIGHT
        low.inside  <- unlist(purrr::map(list(c(low.runover, HEIGHT)), which_inside, plot.data$y))
      }
      if(any(unlist(boundaries) > HEIGHT)) {
        high.runover <- max(unlist(boundaries)[unlist(boundaries) > HEIGHT]) - HEIGHT
        high.inside  <- unlist(purrr::map(list(c(0, high.runover)), which_inside, plot.data$y))
      }
      return(c(low.inside, high.inside))
    }

    if(get_used("treeCropDistance") > 0) {
      xs <- as.list(tree.plot.data$x)
      boundaries <- purrr::map(xs, create_range, get_used("treeCropDistance"))
      boundaries <- purrr::map(boundaries, round_if_needed, get_used("cellWidth"))
      x.inside   <- unlist(purrr::map(boundaries, which_inside, plot.data$x))
      if(toric.x.both) x.inside <- c(x.inside, check_x_runover(boundaries)) # if toric symetry is on, check for intercrop runover across toric symmetry
      plot.data$crop[which(plot.data$x %in% x.inside)] <- interCropSpecies
    } else if(get_used("weededAreaRadius") > 0) {
      xs <- as.list(tree.plot.data$x)
      x.boundaries <- purrr::map(xs, create_range, get_used("weededAreaRadius"))
      x.boundaries <- purrr::map(x.boundaries, round_if_needed, get_used("cellWidth"))
      x.inside     <- unlist(purrr::map(x.boundaries, which_inside, plot.data$x))
      if(toric.x.both) x.inside <- c(x.inside, check_x_runover(x.boundaries)) # if toric symetry is on, check for intercrop runover across toric symmetry

      ys <- as.list(tree.plot.data$y)
      y.boundaries <- purrr::map(ys, create_range, get_used("weededAreaRadius"))
      y.boundaries <- purrr::map(y.boundaries, round_if_needed, get_used("cellWidth"))
      y.inside     <- unlist(purrr::map(y.boundaries, which_inside, plot.data$y))
      if(toric.y.both) y.inside <- c(y.inside, check_y_runover(y.boundaries)) # if toric symetry is on, check for intercrop runover across toric symmetry

      cells.inside <- expand.grid(x.inside, y.inside) %>%
        dplyr::rename(x = Var1, y = Var2) %>%
        dplyr::mutate(flagged = 1)

      plot.data <- plot.data %>%
        dplyr::left_join(cells.inside, by = c("x", "y"))

      plot.data$crop[which(plot.data$flagged == 1)] <- interCropSpecies
      plot.data$flagged <- NULL
    }
  }

  plot.obj <- ggplot(plot.data, aes(x = x, y = y)) +
    labs(x       = paste0(WIDTH.lab,  "m"),
         y       = paste0(HEIGHT.lab, "m"),
         color   = "",
         fill    = "",
         title   = paste("Scene:", hip$exp.plan$SimulationName),
         caption = paste0("Latitude: ",             get_used("latitude"),
                          " - North orientation: ", get_used("northOrientation"),
                          "\nCell width: ",         get_used("cellWidth"),
                          "m - Soil depth: ",       soil.depth, "m",
                          "\nSlope aspect: ",       get_used("slopeAspect"),
                          " - Slope intensity: ",   get_used("slopeIntensity"),
                          "\nToric symmetry: ",     toric.lab)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_tile(color = "black", aes(fill = crop)) +
    geom_text(aes(label = id)) +
    geom_point(data = tree.plot.data, size = 10, aes(color = species), na.rm = TRUE) +
    geom_point(data = tree.plot.data, shape = 21, size = 10, na.rm = TRUE) +
    scale_color_manual(values = c("black", "grey70", "grey30", "grey50")) +
    scale_fill_manual(values  = c("white", "grey80")) +
    coord_equal() +
    theme_hisafe_tile() +
    theme(legend.position = "right")

  if(!is.null(output.path)){
    ggsave(filename = clean_path(paste0(output.path, "/support/", hip$exp.plan$SimulationName, "_Scene.pdf")),
           plot     = plot.obj,
           height   = ifelse(WIDTH > HEIGHT, 8.5, 11),
           width    = ifelse(WIDTH > HEIGHT, 11,  8.5))
  }
  return(plot.obj)
}
