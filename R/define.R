define_hisafe <- function(SimulationName, ..., factorial = FALSE) {

  # FOR TESTING
  # tmpfun <- function(SimulationName, ...){ c(list(SimulationName = SimulationName), list(...)) }
  # arg.list <- tmpfun("mysim", Latitude = c(20,40), Between = c(10,15))

  arg.list <- c(list(SimulationName = SimulationName), list(...))
  args.provided <- names(arg.list)

  default.args <- HISAFE.DEFAULTS()
  defaults.to.add <- default.args[which(!(names(default.args) %in% args.provided))]

  param.list <- c(arg.list, defaults.to.add)

  if(factorial) {
    plan <- tibble::as.tibble(expand.grid(param.list, stringsAsFactors = FALSE))
    plan$SimulationName <- paste0(plan$SimulationName, 1:nrow(plan))
  } else {
    plan <- tibble::as.tibble(as.data.frame(param.list, stringsAsFactors = FALSE))
  }

  check_input_values(plan)
  class(plan) <- c("hip", class(plan))
  return(plan)
}

define_hisafe_file <- function(file) {

  provided <- readr::read_csv(file)
  args.provided <- names(provided)

  default.args <- HISAFE.DEFAULTS()
  defaults.to.add <- tibble::as.tibble(default.args[which(!(names(default.args) %in% args.provided))])
  defaults.to.add <- defaults.to.add[rep(1,nrow(provided)),]

  plan <- dplyr::bind_cols(provided, defaults.to.add)

  check_input_values(plan)
  class(plan) <- c("hip", class(plan))
  return(plan)
}

check_input_values <- function(plan) {
  toric.symmetry.opts <- c("XY", "X", "Y", "NO")

  ## Check for missing/extra columns
  if(any(!(names(plan) %in% SUPPORTED.PARAMS))) {
    extra.cols <- names(plan)[!(names(plan) %in% SUPPORTED.PARAMS)]
    extra.message <- paste0(c("The following columns will not be used because they are not supported input parameters:", extra.cols), collapse = "\n")
    warning(extra.message, call. = FALSE)
  }

  if(any(!(SUPPORTED.PARAMS %in% names(plan)))) {
    missing.cols <- SUPPORTED.PARAMS[SUPPORTED.PARAMS %in% names(plan)]
    missing.message <- paste0(c("The following required parameters are missing:", missing.cols), collapse = "\n")
    warning(missing.message, call. = FALSE)
  }

  ## Check for reasonableness/validity of input parameter values
  message.orig <- messages <- "Hi-sAFe definition warnings:"
  error.orig <- errors <- "Hi-sAFe definition errors:"

  ## Warnings
  if(any(plan$Nyears > 50)) messages <- paste0(messages, "Nyears - It is not recommended to run simulations longer than 50 years.", collapse = "\n")
  if(any(plan$Within > 15)) messages <- paste0(messages, "Within - Some values are quite large.", collapse = "\n")
  if(any(plan$Between > 30)) messages <- paste0(messages, "Between - Some values are quite large.", collapse = "\n")
  if(any(plan$CropDist > 3)) messages <- paste0(messages, "CropDist - Some values are quite large.", collapse = "\n")
  if(any(plan$RootPruneDepth > 3)) messages <- paste0(messages, "RootPruneDepth - Some values are quite large.", collapse = "\n")

  ## Errors
  if(any(!(plan$DayStart %in% 1:31))) errors <- paste0(errors, "DayStart - must be an integer between 1 and 31.", collapse = "\n")
  if(any(!(plan$Tree %in% SUPPORTED.TREES))) errors <- paste0(errors, "Tree - supported tree species include: ", paste0(SUPPORTED.TREES, collapse = ", "), collapse = "\n")
  if(any(!(plan$Crop %in% SUPPORTED.CROPS))) errors <- paste0(errors, "Crop - supported crop species include: ", paste0(SUPPORTED.CROPS, collapse = ", "), collapse = "\n")
  if(any(!(plan$LeaveAreaCrop %in% SUPPORTED.CROPS))) errors <- paste0(errors, "LeaveAreaCrop - supported crop species include: ", paste0(SUPPORTED.CROPS, collapse = ", "), collapse = "\n")
  if(any(plan$Orient < 0 | Orient > 359)) errors <- paste0(errors, "Orient - must be between 0 and 359.", collapse = "\n")
  if(any(plan$Latitude < -90 | Latitude > 90)) errors <- paste0(errors, "Latitude - must be between -90 and 90.", collapse = "\n")
  if(any(!(plan$ToricSymmetry %in% toric.symmetry.opts))) errors <- paste0(errors,
                                                                     "ToricSymmetry - must be one of: ",
                                                                     paste0(toric.symmetry.opts, collapse = ", ") , collapse = "\n")
  if(any(!(is.character(plan$wth.file) | is.na(plan$wth.file)))) errors <- paste0(errors, "wth.file - must be a character vector or NA.", collapse = "\n")

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
