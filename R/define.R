define_hisafe <- function(factorial = FALSE, ...) {

  # FOR TESTING
  # tmpfun <- function(SimulationName, ...){ c(list(SimulationName = SimulationName), list(...)) }
  # arg.list <- tmpfun("myrun", latitude = 100, spacingWithinRows = 50, cellWidth = -1, rootShape = 4)

  arg.list <- list(...)

  default.params <- purrr::map(HISAFE.PARAMS, ~ .x[["default"]])
  defaults.to.add <- default.params[which(!(names(default.params) %in% names(arg.list)))]

  param.list <- c(arg.list, defaults.to.add)

  if(factorial) {
    plan <- tibble::as.tibble(expand.grid(param.list, stringsAsFactors = FALSE))
    plan$SimulationName <- paste0(plan$SimulationName, "_", 1:nrow(plan))
  } else {
    plan <- tibble::as.tibble(as.data.frame(param.list, stringsAsFactors = FALSE))
  }

  is.unique <- function(x) { length(unique(x)) != 1 }
  plan <- bind_cols(plan[, map_lgl(plan, is.unique)], plan[, !map_lgl(plan, is.unique)]) %>%
    select(SimulationName, everything())

  check_input_values(plan)
  class(plan) <- c("hip", class(plan))
  return(plan)
}

define_hisafe_file <- function(file) {

  provided <- readr::read_csv(file)

  default.params <- purrr::map(HISAFE.PARAMS, ~ .x[["default"]])
  defaults.to.add <- tibble::as.tibble(default.params[which(!(names(default.params) %in% names(provided)))])
  defaults.to.add <- defaults.to.add[rep(1,nrow(provided)),]

  plan <- dplyr::bind_cols(provided, defaults.to.add)

  is.unique <- function(x) { length(unique(x)) != 1 }
  plan <- bind_cols(plan[, map_lgl(plan, is.unique)], plan[, !map_lgl(plan, is.unique)]) %>%
    select(SimulationName, everything())

  check_input_values(plan)
  class(plan) <- c("hip", class(plan))
  return(plan)
}

check_input_values <- function(plan) {

  ## Check for unsupported inputs
  if(any(!(names(plan) %in% names(HISAFE.PARAMS)))) {
    extra.cols <- names(plan)[!(names(plan) %in% names(HISAFE.PARAMS))]
    extra.message <- paste0(c("The following variables are not supported:", extra.cols), collapse = "\n")
    stop(extra.message, call. = FALSE)
  }

  ## Check for validity of input values
  messages <- "Hi-sAFe definition warnings:"
  errors <-   "Hi-sAFe definition errors:"

  allowed.errors <-  purrr::map_chr(names(plan), check_allowed, plan = plan)
  minmax.errors <-   purrr::map_chr(names(plan), check_minmax, plan = plan)
  minmax.warnings <- purrr::map_chr(names(plan), check_minmax_sug, plan = plan)

  all.messages <- c(messages, minmax.warnings)
  all.errors <-   c(errors, allowed.errors, minmax.errors)
  all.messages <- paste0(all.messages[!all.messages == ""], collapse = "\n")
  all.errors <-   paste0(all.errors[!all.errors == ""], collapse = "\n")

  if(all.messages != messages) warning(all.messages, call. = FALSE)
  if(all.errors   != errors)   stop(all.errors, call. = FALSE)
}

check_allowed <- function(variable, plan) {
  allowed.vals <- purrr::map(HISAFE.PARAMS, ~ .x[["allowed"]])[[variable]]
  allowed.pass <- (is.na(allowed.vals[1]) | all(plan[[variable]] %in% allowed.vals))
  if(allowed.pass) return("")
  else return(paste0(variable, " - must be one of: ", paste0(allowed.vals, collapse = ", ")))
}

check_minmax <- function(variable, plan) {
  min.val <- purrr::map(HISAFE.PARAMS, ~ .x[["min"]])[[variable]]
  max.val <- purrr::map(HISAFE.PARAMS, ~ .x[["max"]])[[variable]]
  max.pass <- (is.na(max.val) | all(plan[[variable]] <= max.val))
  min.pass <- (is.na(min.val) | all(plan[[variable]] >= min.val))
  if(max.pass & min.pass) return("")
  else if(!is.na(max.val) & !is.na(min.val)) return(paste0(variable, " - must be betwen ", min.val, " and ", max.val))
  else if(is.na(max.val) & !is.na(min.val)) return(paste0(variable, " - must be greater than ", min.val))
  else if(!is.na(max.val) & is.na(min.val)) return(paste0(variable, " - must be less than ", max.val))
}

check_minmax_sug <- function(variable, plan) {
  min.sug.val <- purrr::map(HISAFE.PARAMS, ~ .x[["min.sug"]])[[variable]]
  max.sug.val <- purrr::map(HISAFE.PARAMS, ~ .x[["max.sug"]])[[variable]]
  max.pass <- (is.na(max.sug.val) | all(plan[[variable]] <= max.sug.val))
  min.pass <- (is.na(min.sug.val) | all(plan[[variable]] >= min.sug.val))
  if(max.pass & min.pass) return("")
  else if(!is.na(max.sug.val) & !is.na(min.sug.val)) return(paste0(variable, " - is typically betwen ", min.sug.val, " and ", max.sug.val))
  else if(is.na(max.sug.val) & !is.na(min.sug.val)) return(paste0(variable, " - is typically greater than ", min.sug.val))
  else if(!is.na(max.sug.val) & is.na(min.sug.val)) return(paste0(variable, " - is typically less than ", max.sug.val))
}
