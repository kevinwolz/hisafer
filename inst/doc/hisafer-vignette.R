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

## ---- echo = FALSE, eval = FALSE-----------------------------------------
#  example.path <- "/Users/kevinwolz/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafer/inst/extdata/example_exp"
#  example.profiles <- c("plot", "plotDetail", "trees", "treesDetail", "cells",  "cellsDetail",
#                        "voxelsMonth", "climate", "monthCells", "annualCells")
#  mc.hip <- define_hisafe(path = example.path, template = "monocrop"    , SimulationName = "monocrop",     profiles = example.profiles, sticsReport = 1)
#  af.hip <- define_hisafe(path = example.path, template = "agroforestry", SimulationName = "agroforestry", profiles = example.profiles)
#  pf.hip <- define_hisafe(path = example.path, template = "forestry",     SimulationName = "forestry",     profiles = example.profiles)
#  build_hisafe(hip = mc.hip)
#  build_hisafe(hip = af.hip)
#  build_hisafe(hip = pf.hip)
#  build_cluster_script(simu.names    = c("monocrop", "agroforestry", "forestry"),
#                       hip           = NULL,
#                       script.path   = example.path,
#                       cluster.path  = paste0("/lustre/lecomtei/tests/example_exp/"),
#                       email.type    = "NONE")

## ------------------------------------------------------------------------
hop.raw <- hop <- read_hisafe_example()

## ---- eval = TRUE--------------------------------------------------------
hop$metadata

## ------------------------------------------------------------------------
hop <- hop_rename(hop       = hop,
                  old.names = c("monocrop", "agroforestry", "forestry"),
                  new.names = c("MC", "AF", "PF"))

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
                       years      = 1995:2003,
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

## ---- eval = TRUE, dpi = 250, fig.width = 10-----------------------------
plot_hisafe_cycle_daily(hop = hop, cycle = "carbon-increment", years = 2003)

## ---- eval = TRUE, dpi = 250, fig.width = 10-----------------------------
plot_hisafe_cycle_daily(hop = hop, cycle = "carbon-allocation", years = 2003)

## ---- eval = TRUE, dpi = 250, fig.width = 10-----------------------------
summary.out <- cycle_summary(hop         = hop,
                             daily.year  = 2003,
                             cycles      = c("carbon", "light", "water", "nitrogen"),
                             simu.name   = "AF",
                             crop.names  = c("Crop", "Tree line vegetation"))
summary.out

## ---- dpi = 250, fig.width = 20------------------------------------------
hisafe_slice(hop        = hop,
             simu.names = c("AF", "PF"),
             date       = "1999-06-01")

## ---- dpi = 250, fig.width = 20------------------------------------------
hisafe_slice(hop        = hop,
             simu.names = c("AF", "PF"),
             date       = "2003-07-01")

## ---- dpi = 250, fig.width = 8-------------------------------------------
visual_legend(hop = hop)

## ---- eval = FALSE-------------------------------------------------------
#  hisafe_root3D(hop      = hop,
#               simu.name = "AF",
#               date      = "2003-06-01")

## ---- eval = TRUE--------------------------------------------------------
face <- create_face(agroforestry = hop_filter(hop, "AF"), 
                    forestry     = hop_filter(hop, "PF"), 
                    monocrop     = hop_filter(hop, "MC"), 
                    face.path    = ".")

## ---- eval = TRUE, dpi = 250, fig.width = 10-----------------------------
LER(face = face)

## ---- eval = TRUE, dpi = 250, fig.width = 10-----------------------------
LER(face = face, cycle = "light")

## ---- eval = FALSE-------------------------------------------------------
#  plot_hisafe_cycle_annual(hop = face, cycle = "carbon")

## ---- eval = FALSE-------------------------------------------------------
#  plot_hisafe_cycle_annual(hop = face, cycle = "light")

## ---- eval = TRUE, dpi = 250, fig.width = 10-----------------------------
ler.out <- LER_summary(face = face)

ler.out

