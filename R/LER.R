LER <- function(face,
                cycles         = "all",
                color.palette = NULL,
                plot          = TRUE) {
  if(!("face" %in% class(face))) stop("face argument not of class face", call. = FALSE)
  if(cycles == "all") cycles <- c("carbon", "nitrogen", "water", "light")

  ## Get flux data
  cycle.data <- purrr::map(cycles, plot_annual_cycle, hop = face, plot = FALSE) %>% dplyr::bind_rows()
  # if("carbon" %in% cycles)   carbon   <- plot_annual_cycle(face, "carbon",   plot = FALSE) else carbon   <- NULL
  # if("nitrogen" %in% cycles) nitrogen <- plot_annual_cycle(face, "nitrogen", plot = FALSE) else nitrogen <- NULL
  # if("water" %in% cycles)    water    <- plot_annual_cycle(face, "water",    plot = FALSE) else water    <- NULL
  # if("light" %in% cycles)    light    <- plot_annual_cycle(face, "light",    plot = FALSE) else light    <- NULL

  crop.descrip <- c("Main crop yield",           # yield
                    #"", # carbon
                    "Extraction - main crop",    # nitrogen
                    "Transpiration - main crop", # water
                    "Main crop")                 # light

  tree.descrip <- c("Tree stem yield",       # yield
                    #"", # carbon
                    "Extraction - trees",    # nitrogen
                    "Transpiration - trees", # water
                    "Trees")                 # light

  cycle.data$cat <- NA
  cycle.data$cat[cycle.data$flux %in% crop.descrip] <- "crop"
  cycle.data$cat[cycle.data$flux %in% tree.descrip] <- "tree"
}
