## ------------------------------------------------------------------------
library(hisafer)

## ------------------------------------------------------------------------
hisafe_params()

## ---- eval = FALSE-------------------------------------------------------
#  hisafe_params("all")

## ------------------------------------------------------------------------
hisafe_params("treeLineOrientation")

## ------------------------------------------------------------------------
hip_sim <- define_hisafe(latitude = 60, treeLineOrientation = 90)
hip_sim

## ------------------------------------------------------------------------
hip_exp_F <- define_hisafe(factorial = FALSE, 
                           latitude = c(30, 60),
                           treeLineOrientation = c(0,90))
hip_exp_F

## ------------------------------------------------------------------------
hip_exp <- define_hisafe(factorial = TRUE, 
                           latitude = c(30, 60),
                           treeLineOrientation = c(0,90))
hip_exp

## ---- eval = FALSE-------------------------------------------------------
#  hip_exp <- build_hisafe(hip = hip_exp,
#                          path = "./simulations",
#                          exp.name = "lat-orient_exp",
#                          profiles = c("annualplot", "annualtree", "annualcrop", "monthCells", "trees", "plot"),
#                          saveProjectOption = FALSE)
#  hip_exp

## ---- eval = FALSE-------------------------------------------------------
#  run_hisafe_exp(hip = hip_exp,
#                 simu.names = "all",
#                 parallel = TRUE,
#                 capsis.path = "/Applications/Capsis/")

## ---- eval = FALSE-------------------------------------------------------
#  run_hisafe(path = "./simulations"
#             simu.name  = "Sim_From_Kevin",
#             capsis.path = "/Applications/Capsis/")

## ---- eval = FALSE-------------------------------------------------------
#  hop_exp <- read_hisafe_exp(hip = hip_exp, profiles = "all")
#  hop_exp

## ---- echo = FALSE-------------------------------------------------------
hop_exp <- read_hisafe_exp(path = "./experiment", profiles = "all")
#load("vignette_hop.Rdata")

## ---- echo = FALSE-------------------------------------------------------
hop_exp

