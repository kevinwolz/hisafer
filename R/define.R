define_hisafe <- function(SimulationName,
                          Nyears,
                          YearStart,
                          DayStart,
                          Tree,
                          Crop,
                          Within,
                          Between,
                          Stagger,
                          CropDist,
                          Orient,
                          Latitude,
                          RootPruneDepth,
                          wth.file,
                          factorial = FALSE
) {
  if(!factorial) {
    plan <- tibble::tibble(SimulationName = SimulationName,
                           Nyears = Nyears,
                           YearStart = YearStart,
                           DayStart = DayStart,
                           Tree = Tree,
                           Crop = Crop,
                           Within = Within,
                           Between = Between,
                           Stagger = Stagger,
                           CropDist = CropDist,
                           Orient = Orient,
                           Latitude = Latitude,
                           RootPruneDepth = RootPruneDepth,
                           wth.file = wth.file)
  } else {
    plan <- tibble::as.tibble(expand.grid(SimulationName = SimulationName,
                                          Nyears = Nyears,
                                          YearStart = YearStart,
                                          DayStart = DayStart,
                                          Tree = Tree,
                                          Crop = Crop,
                                          Within = Within,
                                          Between = Between,
                                          Stagger = Stagger,
                                          CropDist = CropDist,
                                          Orient = Orient,
                                          Latitude = Latitude,
                                          RootPruneDepth = RootPruneDepth,
                                          wth.file = wth.file),
                              stringsAsFactors = FALSE)
  }
  check_input_values(plan)
  return(plan)
}

define_hisafe_exp <- function(exp.name, ...) {
  name <- exp.name
  plan <- define_hisafe(...)
  return(list(name = name, plan = plan))
}

check_input_values <- function(plan) {
  avail.trees <- c("walnut-hybrid", "poplar")
  #avail.crops <-
  message.orig <- messages <- "Hi-sAFe definition warnings:"
  error.orig <- errors <- "Hi-sAFe definition errors:"

  attach(plan)
  ## Warnings
  if(any(Nyears > 50)) messages <- paste0(messages, "Nyears - It is not recommended to run simulations longer than 50 years.", collapse = "\n")
  if(any(Within > 15)) messages <- paste0(messages, "Within - Within row tree spacing is quite large.", collapse = "\n")
  if(any(Between > 30)) messages <- paste0(messages, "Between - Between row tree spacing is quite large.", collapse = "\n")
  if(any(CropDist > 3)) messages <- paste0(messages, "CropDist - CropDist is quite large.", collapse = "\n")
  if(any(RootPruneDepth > 3)) messages <- paste0(messages, "RootPruneDepth - RootPruneDepth is quite large.", collapse = "\n")

  ## Errors
  if(any(!(DayStart %in% 1:31))) errors <- paste0(errors, "DayStart - DayStart must be an integer between 1 and 31.", collapse = "\n")
  if(any(!(Tree %in% avail.trees))) errors <- paste0(errors, "Tree - supported tree species include: ", paste0(avail.trees, collapse = ", "), collapse = "\n")
  if(any(!(Tree %in% avail.trees))) errors <- paste0(errors, "Crop - supported crop species include: ", paste0(avail.crops, collapse = ", "), collapse = "\n")
  if(any(Stagger < 0 | Stagger > 0.5)) errors <- paste0(errors, "Stagger - Stagger must be between 0 and 0.5.", collapse = "\n")
  if(any(Orient < 0 | Orient > 359)) errors <- paste0(errors, "Orient - Orient must be between 0 and 359.", collapse = "\n")
  if(any(Latitude < -90 | Latitude > 90)) errors <- paste0(errors, "Latitude - Latitude must be between -90 and 90.", collapse = "\n")
  if(any(!is.character(wth.file))) errors <- paste0(errors, "wth.file - wth.file must be a character vector.", collapse = "\n")
  detach(plan)

  w <- ifelse(messages == message.orig, FALSE, TRUE)
  e <- ifelse(errors == error.orig, FALSE, TRUE)

  if(e & w){
    out <- paste(c(e, w), collapse = "\n")
    stop(out)
  } else if(e & !(w)) {
    stop(e)
  } else if(!(e) & w) {
    warning(w)
  }
}
