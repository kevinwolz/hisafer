## ---- eval = FALSE-------------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  install_github("kevinwolz/hisafer")

## ------------------------------------------------------------------------
library(hisafer)

## ------------------------------------------------------------------------
hip <- define_hisafe(path     = "./simulations",
                     profiles = "all",
                     template = "agroforestry")

## ------------------------------------------------------------------------
hip 

## ------------------------------------------------------------------------
hip_params("nbSimulations")

## ------------------------------------------------------------------------
hip <- define_hisafe(path     = "./simulations",
                     profiles = "all",
                     template = "agroforestry",
                     nbSimulations = 20)
hip$exp.plan

## ------------------------------------------------------------------------
hip_params("northOrientation")

## ------------------------------------------------------------------------
hip_sim <- define_hisafe(path     = "./simulations",
                         profiles = "all",
                         template = "agroforestry",
                         latitude         = 60, 
                         northOrientation = 90)
hip_sim$exp.plan

## ------------------------------------------------------------------------
hip <- define_hisafe(path      = "./simulations",
                     profiles  = "all",
                     template  = "agroforestry",
                     exp.name  = "lat-orient",
                     factorial = FALSE,
                     latitude         = c(30, 60),
                     northOrientation = c(0,  90))
hip$exp.plan

## ------------------------------------------------------------------------
hip <- define_hisafe(path      = "./simulations",
                     profiles  = "all",
                     template  = "agroforestry",
                     exp.name  = "lat-orient",
                     factorial = TRUE, 
                     latitude         = c(30, 60),
                     northOrientation = c(0,  90))
hip$exp.plan

## ------------------------------------------------------------------------
hip <- define_hisafe(path      = "./simulations",
                     profiles  = "all",
                     template  = "agroforestry",
                     mainCropSpecies = c("durum-wheat.plt", "rape.plt"),
                     mainCropItk     = c("durum-wheat.tec", "rape.tec"))
hip$exp.plan

## ------------------------------------------------------------------------
hip <- define_hisafe(path      = "./simulations",
                     profiles  = "all",
                     template  = "agroforestry",
                     mainCropSpecies = list(c("durum-wheat.plt", "rape.plt")),
                     mainCropItk     = list(c("durum-wheat.tec", "rape.tec")))
hip$exp.plan

## ------------------------------------------------------------------------
hip$exp.plan$mainCropSpecies

## ------------------------------------------------------------------------
hip <- define_hisafe(path      = "./simulations",
                     profiles  = "all",
                     template  = "agroforestry",
                     mainCropSpecies = list("durum-wheat.plt", c("durum-wheat.plt", "rape.plt")),
                     mainCropItk     = list("durum-wheat.tec", c("durum-wheat.tec", "rape.tec")))
hip$exp.plan$mainCropSpecies

## ------------------------------------------------------------------------
hip <- define_hisafe(path      = "./simulations",
                     profiles  = c("plot", "trees", "climate", "monthCells", "cells", "voxels"),
                     template  = "agroforestry",
                     exp.name  = "lue",
                     lueMax    = c(0.6, 0.7))

## ---- eval = FALSE-------------------------------------------------------
#  build_hisafe(hip = hip)

## ---- eval = FALSE-------------------------------------------------------
#  run_hisafe(hip         = hip,
#             simu.names  = "all",
#             parallel    = TRUE,
#             capsis.path = "/Applications/Capsis/")

## ---- eval = FALSE-------------------------------------------------------
#  run_hisafe(path        = "./simulations/lue"
#             simu.names  = c("Sim_1", "Sim_2"),
#             parallel    = TRUE,
#             capsis.path = "/Applications/Capsis/")

## ---- eval = FALSE-------------------------------------------------------
#  hop <- read_hisafe(hip      = hip,
#                     profiles = c("plot", "trees", "climate", "monthCells", "cells", "voxels"))

## ---- echo = FALSE-------------------------------------------------------
#hisafer_example_hop <- read_hisafe(path       = "/Users/kevinwolz/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/vignette_output/lue", 
#                                   simu.names = c("Sim_1", "Sim_2"), 
#                                   profiles   = c("plot", "trees", "climate", "monthCells", "cells", "voxels"))
#save(hisafer_example_hop, file = "/Users/kevinwolz/Desktop/hisafer_example_hop.Rdata")
#devtools::use_data(hisafer_example_hop)
hop  <- hisafer_example_hop

## ---- eval = FALSE-------------------------------------------------------
#  hop$plot

## ------------------------------------------------------------------------
hop <- hop_rename(hop       = hop,
                  old.names = paste0("Sim_", 1:2),
                  new.names = paste0("lue-", hop$exp.plan$lueMax))

## ------------------------------------------------------------------------
hop <- hop_filter(hop        = hop,
                  simu.names = "all",
                  tree.ids   = "all",
                  date.min   = "1995-01-01",
                  date.max   = "2004-01-01")

## ---- dpi = 250, fig.width = 10------------------------------------------
plot_hisafe_ts(hop        = hop, 
               variable   = "dbh", 
               profile    = "trees", 
               facet.year = FALSE)

## ---- dpi = 250, fig.width = 10------------------------------------------
plot_hisafe_monthcells(hop        = hop, 
                       variable   = "monthRelativeDirectParIncident",
                       colfacet   = "Year",
                       rowfacet   = "SimulationName",
                       years      = 2:9,
                       months     = 7)

## ---- eval = TRUE, dpi = 250, fig.width = 10-----------------------------
plot_hisafe_cycle_annual(hop = hop, cycle = "carbon")

## ---- eval = TRUE, dpi = 250, fig.width = 10-----------------------------
plot_hisafe_cycle_annual(hop = hop, cycle = "nitrogen")

## ---- eval = TRUE, dpi = 250, fig.width = 10-----------------------------
plot_hisafe_cycle_annual(hop = hop, cycle = "water")

## ---- eval = TRUE, dpi = 250, fig.width = 10-----------------------------
plot_hisafe_cycle_annual(hop = hop, cycle = "light")

## ---- eval = TRUE, dpi = 250, fig.width = 10-----------------------------
plot_hisafe_cycle_daily(hop = hop, cycle = "carbon", years = 2003)

## ---- eval = TRUE, dpi = 250, fig.width = 10-----------------------------
plot_hisafe_cycle_daily(hop = hop, cycle = "nitrogen", years = 2003)

## ---- eval = TRUE, dpi = 250, fig.width = 10-----------------------------
plot_hisafe_cycle_daily(hop = hop, cycle = "water", years = 2003)

## ---- eval = TRUE, dpi = 250, fig.width = 10-----------------------------
plot_hisafe_cycle_daily(hop = hop, cycle = "light", years = 2003)

## ---- dpi = 250, fig.width = 20------------------------------------------
hisafe_slice(hop  = hop,
             date = "2003-06-01")

## ---- dpi = 250, fig.width = 20------------------------------------------
hisafe_slice(hop    = hop,
             plot.x = "y",
             date   = "2003-06-01")

## ---- dpi = 250, fig.width = 8-------------------------------------------
visual_legend(hop = hop)

## ---- echo = FALSE, eval = FALSE-----------------------------------------
#  #load(paste0(system.file("extdata", "vignette_output", package = "hisafer"), "/vignette_face.Rdata"))
#  face <- hisafer_example_face

## ---- eval = FALSE-------------------------------------------------------
#  face <- create_face(agroforestry = AF.hop,
#                      forestry     = FC.hop,
#                      monocrop     = CC.hop,
#                      face.path    = "./simulations")

## ---- eval = FALSE-------------------------------------------------------
#  LER(face = face)

## ---- eval = FALSE-------------------------------------------------------
#  plot_hisafe_cycle_annual(hop = face, cycle = "carbon")

## ---- eval = FALSE-------------------------------------------------------
#  plot_hisafe_cycle_annual(hop = face, cycle = "light")

