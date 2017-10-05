define_hisafe <- function(SimulationName,
                          Nyears,
                          YearStart,
                          DayStart,
                          Tree,
                          Crop,
                          Within,
                          Between,
                          #Stagger,
                          CropDist,
                          Orient,
                          Latitude,
                          RootPruneDepth,
                          wth.file
) {
  plan <- tibble::tibble(SimulationName = SimulationName,
                         Nyears = Nyears,
                         YearStart = YearStart,
                         DayStart = DayStart,
                         Tree = Tree,
                         Crop = Crop,
                         Within = Within,
                         Between = Between,
                         #Stagger = Stagger,
                         CropDist = CropDist,
                         Orient = Orient,
                         Latitude = Latitude,
                         RootPruneDepth = RootPruneDepth,
                         wth.file = wth.file)
  check_input_values(plan)
  class(plan) <- c("hip", class(plan))
  return(plan)
}


define_hisafe_factorial <- function(Nyears,
                                    YearStart,
                                    DayStart,
                                    Tree,
                                    Crop,
                                    Within,
                                    Between,
                                    #Stagger,
                                    CropDist,
                                    Orient,
                                    Latitude,
                                    RootPruneDepth,
                                    wth.file
) {
  plan <- tibble::as.tibble(expand.grid(Nyears = Nyears,
                                        YearStart = YearStart,
                                        DayStart = DayStart,
                                        Tree = Tree,
                                        Crop = Crop,
                                        Within = Within,
                                        Between = Between,
                                        #Stagger = Stagger,
                                        CropDist = CropDist,
                                        Orient = Orient,
                                        Latitude = Latitude,
                                        RootPruneDepth = RootPruneDepth,
                                        wth.file = wth.file),
                            stringsAsFactors = FALSE)

  ## Add generic SimulationName column and move to front
  plan %>%
    mutate(SimulationName = paste0("Sim", 1:nrow(plan))) %>%
    select(SimulationName, everything())


  check_input_values(plan)
  class(plan) <- c("hip", class(plan))
  return(plan)
}

define_hisafe_file <- function(file) {
  plan <- readr::read_csv(file)
  check_input_values(plan)
  class(plan) <- c("hip", class(plan))
  return(plan)
}

check_input_values <- function(plan) {
  req.cols <- c("SimulationName",
                "Nyears",
                "YearStart",
                "DayStart",
                "Tree",
                "Crop",
                "Within",
                "Between",
                #"Stagger",
                "CropDist",
                "Orient",
                "Latitude",
                "RootPruneDepth",
                "wth.file")
  avail.trees <- c("walnut-hybrid", "poplar")
  avail.crops <- c("wheat", "maize", "soybean")

  ## Check for missing/extra columns
  if(any(!(names(plan) %in% req.cols))) {
    extra.cols <- names(plan)[!(names(plan) %in% req.cols)]
    extra.message <- paste0(c("The following columns will not be used because they are not supported input parameters:", extra.cols), collapse = "\n")
    warning(extra.message, call. = FALSE)
  }

  if(any(!(req.cols %in% names(plan)))) {
    missing.cols <- req.cols[req.cols %in% names(plan)]
    missing.message <- paste0(c("The following required parameters are missing:", missing.cols), collapse = "\n")
    warning(missing.message, call. = FALSE)
  }

  ## Check for reasonableness/validity of input parameter values
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
  #if(any(Stagger < 0 | Stagger > 0.5)) errors <- paste0(errors, "Stagger - Stagger must be between 0 and 0.5.", collapse = "\n")
  if(any(Orient < 0 | Orient > 359)) errors <- paste0(errors, "Orient - Orient must be between 0 and 359.", collapse = "\n")
  if(any(Latitude < -90 | Latitude > 90)) errors <- paste0(errors, "Latitude - Latitude must be between -90 and 90.", collapse = "\n")
  if(any(!(is.character(wth.file) | is.na(wth.file)))) errors <- paste0(errors, "wth.file - wth.file must be a character vector.", collapse = "\n")
  detach(plan)

  w <- ifelse(messages == message.orig, FALSE, TRUE)
  e <- ifelse(errors == error.orig, FALSE, TRUE)

  if(e & w){
    out <- paste(c(errors, messages), collapse = "\n")
    stop(out, call. = FALSE)
  } else if(e & !(w)) {
    stop(errors, call. = FALSE)
  } else if(!(e) & w) {
    warning(messages, call. = FALSE)
  }
}
