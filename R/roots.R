#' Create a 3D plot of tree coarse root topology
#' @description Creates a 3D plot of tree coarse root topology. 
#' The plot is saved as soon as rgl has finished drawing it on the screen. 
#' The default view is typically not very good, so zooming/rotating the plot while it's drawing can allow you to save a better plot.
#' \code{rgl::rgl.snapshot} can also be used directly to save the plot once the desired view is found.
#' @param hop An object of class hop.
#' @param date A character string of the date to plot, in the format "YYYY-MM-DD" or of class Date.
#' @param simu.name A character vector of legnth 1 indicating the SimulationName within \code{hop} to plot.
#' @param tree.id A numeric vector of lenght 1 indicating the tree id to plot.
#' @param color.var A character string indicating the name of the voxels variable to use for coloring the roots.
#' @param color.palette A character vector of colors for coloring the roots to pass to \code{grDevices::colorRampPalette}. 
#' @param bg A character string of hex value or R standard color name defining the plot background color.
#' @param grid A logical indicating whether the plot should include an x,y,z grid.
#' @param box A logical indicating whether the plot should include a boundign box.
#' @param output.path A character string indicating the path to the directory where plots should be saved.
#' If \code{NULL}, the experiment/simulation path is read from the hop object, and a directory is created there called "analysis/roots3D".
#' The plot will be saved in this directory as "simu.name_date_root3D.png".
#' @param ... Other arguments passed to \code{rgl::par3d}.
#' @details Key for voxel output "colonisationDirection":
#' \itemize{
#'  \item{0}{: x-}
#'  \item{1}{: x-}
#'  \item{2}{: y+}
#'  \item{3}{: y-}
#'  \item{4}{: z+}
#'  \item{5}{: z-}
#' }
#' Positive z (+z) is in the downward direction. 
#' Everything above specifies the direction from the parent voxel to the child voxel and is stored in the child.
#' @export
#' @importFrom dplyr %>%
#' @family hisafe visualization functions
#' @author Gregoire Talbot \email{talbot@mailoo.org}
#' @author Kevin Wolz
#' @examples
#' \dontrun{
#' root3D(hop = myhop, date = "2000-07-01")
#' }
hisafe_root3D <- function(hop,
                          date,
                          simu.name     = NULL,
                          tree.id       = 1,
                          color.var     = paste0("treeRootDensity_", tree.id),
                          color.palette = c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404"),
                          bg            = "white",
                          grid          = FALSE,
                          box           = FALSE, 
                          output.path   = NULL, ...) {
  
  if(!requireNamespace("rgl", quietly = TRUE)) stop("The package 'rgl' is required for hisafe_root3D(). Please install and load it.", call. = FALSE)
  is_hop(hop, error = TRUE)
  profile_check(hop, "voxels",    error = TRUE)
  profile_check(hop, "plot.info", error = TRUE)
  
  date <- lubridate::ymd(date)
  if(is.null(simu.name)) {
    if("hop-group" %in% class(hop)) stop("simu.name must be specified if hop contains more than one simulation", call. = FALSE)
    simu.name <- hop$metadata$SimulationName[1]
  } 
  
  if(length(date) != 1)                                   stop("date argument must have length 1",                          call. = FALSE)
  if(!(is.character(simu.name) & length(simu.name) == 1)) stop("simu.name argument must be a character vector of length 1", call. = FALSE)
  if(!(is.numeric(tree.id)     & length(tree.id)   == 1)) stop("tree.id argument must be a numeric vector of length 1",     call. = FALSE)
  if(!(is.character(color.var) & length(color.var) == 1)) stop("color.var argument must be a character vector of length 1", call. = FALSE)
  if(!(is.character(bg)        & length(bg)        == 1)) stop("bg argument must be a character vector of length 1",        call. = FALSE)
  is_TF(grid)
  is_TF(box)
  
  hop  <- hop_filter(hop = hop, simu.names = simu.name)
  
  cellWidth <- hop$plot.info$cellWidth
  Z <- c(-0.1, sort(unique(hop$voxels$z)), max(hop$voxels$z) + 0.2)
  
  rsyst <- hop$voxels %>%
    dplyr::filter(Date == date) %>%
    dplyr::mutate(colonisationDirection = .[[paste0("colonisationDirection_", tree.id)]]) %>%
    dplyr::mutate(treeCoarseRootBiomass = .[[paste0("treeCoarseRootBiomass_", tree.id)]]) %>%
    dplyr::mutate(root.color            = .[[color.var]]) %>%
    dplyr::select(SimulationName, x, y, z, colonisationDirection, treeCoarseRootBiomass, root.color) %>%
    dplyr::filter(treeCoarseRootBiomass > 0) %>%
    dplyr::mutate(z.up   = abs(Z[match(z, Z) - 1] - z)) %>%    
    dplyr::mutate(z.down = abs(Z[match(z, Z) + 1] - z)) %>%
    dplyr::mutate(len    = 0) %>%
    dplyr::mutate(len    = len + cellWidth * as.numeric(colonisationDirection %in% 0:3)) %>%
    dplyr::mutate(len    = len + z.up      * as.numeric(colonisationDirection  ==  4)) %>%
    dplyr::mutate(len    = len + z.down    * as.numeric(colonisationDirection  ==  5)) %>%
    dplyr::mutate(px     = x + as.numeric(colonisationDirection == 1) * len - as.numeric(colonisationDirection == 0) * len) %>%
    dplyr::mutate(py     = y - as.numeric(colonisationDirection == 3) * len + as.numeric(colonisationDirection == 2) * len) %>%
    dplyr::mutate(pz     = z + as.numeric(colonisationDirection == 5) * len - as.numeric(colonisationDirection == 4) * len) %>%
    dplyr::mutate(ray    = sqrt(treeCoarseRootBiomass / 616 / len / pi))
  
  ## PLOT
  rgl::rgl.open()
  rgl::par3d(...)
  rgl::rgl.bg(color = bg)
  
  helix.turns      <- 2000
  size.multiplier  <- 10
  color.multiplier <- 100
  co <- colorRampPalette(color.palette, space = "Lab")
  n.cols <- floor(quantile(rsyst$root.color * color.multiplier, 0.95))
  cols <- co(n.cols)
  
  
  for(i in 1:nrow(rsyst)) {
    
    compte <- 0
    r <- rsyst$ray[i] * size.multiplier
    
    if(rsyst$x[i] == rsyst$px[i]){
      x <- rsyst$x[i] + r * sin(1:helix.turns * pi / size.multiplier)
      compte <- 1
    } else {
      x <- seq(from = rsyst$x[i], to = rsyst$px[i], length = helix.turns)
    }
    
    if(rsyst$y[i] == rsyst$py[i]){
      y <- rsyst$y[i] + r * sin(1:helix.turns * pi / size.multiplier) * (compte == 0) + r * cos(1:helix.turns * pi / size.multiplier) * (compte == 1)
    } else {
      y <- seq(from = rsyst$y[i], to = rsyst$py[i], length = helix.turns)
    }
    
    if(rsyst$z[i] == rsyst$pz[i]){
      z <- -(rsyst$z[i] + r * cos(1:helix.turns * pi / size.multiplier))
    } else {
      z <- -(seq(from = rsyst$z[i], to = rsyst$pz[i], length = helix.turns))
    }
    
    idCol <- max(1, min(floor(rsyst$root.color[i] * color.multiplier), n.cols))
    rgl::rgl.linestrips(x   = x,
                        z   = y,
                        y   = z,
                        col = cols[idCol],
                        lwd = 3)
  }
  
  if(box)	rgl::rgl.bbox()
  if(grid) {
    centerpos <- which(rsyst$pz < 0)
    center <- c(rsyst$x[centerpos], rsyst$y[centerpos], -rsyst$z[centerpos])
    addGrid(center,
            xlim  = c(min(rsyst$x), max(rsyst$x)),
            ylim  = c(min(rsyst$y), max(rsyst$y)),
            zlim  = c(min(rsyst$z), max(rsyst$z)),
            xstep = 1,
            ystep = 1,
            zstep = 0.2)
  }
  
  output.path <- clean_path(paste0(diag_output_path(hop = hop, output.path = output.path), "/root3D/"))
  dir.create(output.path, recursive = TRUE, showWarnings = FALSE)
  rgl::rgl.snapshot(paste0(output.path, simu.name, "_", date, "_root3D.png"))
}

#' Add grid to hisafe_root3D() plot
#' @description Adds grid to \code{\link{hisafe_root3D}} plot.
#' @param center An object of class hop.
#' @param xlim Upper & lower limits for x.
#' @param ylim Upper & lower limits for y.
#' @param zlim Upper & lower limits for z.
#' @param xstep Grid size in x direction.
#' @param ystep Grid size in y direction.
#' @param zstep Grid size in z direction.
#' @keywords internal
#' @author Gregoire Talbot \email{talbot@mailoo.org}
addGrid <- function(center, xlim, ylim, zlim, xstep, ystep, zstep){
  
  zlim <- -zlim[order(-zlim)]
  
  # quels x
  x <- c(center[1] - xstep / 2, center[1] + xstep / 2)
  while((x[1]) >= xlim[1]){
    x <- c(x[1] - xstep, x)
  }
  while((x[length(x)]) <= xlim[2]){
    x <- c(x, x[length(x)] + xstep)
  }
  
  # quels y
  y <- c(center[2] - ystep / 2, center[2] + ystep / 2)
  while((y[1]) >= ylim[1]){
    y <- c(y[1] - ystep, y)
  }
  while((y[length(y)]) <= ylim[2]){
    y <- c(y, y[length(y)] + ystep)
  }
  
  # quels z
  z <- c(center[3] - zstep / 2, center[3] + zstep / 2)
  while((z[1]) >= zlim[1]){
    z <- c(z[1]-zstep, z)
  }
  while((z[length(z)]) <= zlim[2]){
    z <- c(z, z[length(z)] + zstep)
  }
  
  # tracer
  for(i in x){
    for(j in y){
      rgl::lines3d(c(i, i), c(min(z), max(z)), c(j, j), alpha = 1, col = "grey50")
    }
    for(k in z){
      rgl::lines3d(c(i, i), c(k, k), c(min(y), max(y)), alpha = 1, col = "grey50")
    }
  }
  for(j in y){
    for(k in z){
      rgl::lines3d(c(min(x), max(x)), c(k,k), c(j,j), alpha = 1, col = "grey50")
    }
  }
}
