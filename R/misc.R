#' Display supported Hi-sAFe input parameters
#' @description Displays supported Hi-sAFe input parameters, their default values, and their accepted/suggested ranges.
#' @return Invisibly returns an alphebetized character vector of the names of supported Hi-sAFe prameters.
#' @param variable If "names", the default, then just the names of supported Hi-sAFe parameters is printed to the console.
#' If "all", then the names, default values, and accepted/suggested ranges of supported Hi-sAFe parameters is printed.
#' Can also be a character vector of specific Hi-sAFe parameters of which to display details.
#' @export
#' @family hisafe definition functions
#' @examples
#' \dontrun{
#' hisafe_params()            # just for parameter names
#' hisafe_params("cellWidth") # details of cellWidth parameter
#' hisafe_params("all")       # details of all parameters
#' }
hisafe_params <- function(variable = "names") {
  param.names <- sort(names(HISAFE.PARAMS))

  acceptable <- c(param.names, "names", "all")
  if(any(!(variable %in% acceptable))) {
    bad.vars <- variable[!(variable %in% acceptable)]
    stop(paste0("The following are not supported Hi-sAFe input parameters: ", bad.vars))
  }

  if(variable[1] == "all") {
    param.details <- str(HISAFE.PARAMS,
                         comp.str = "",
                         give.length = FALSE,
                         give.head = FALSE,
                         no.list = TRUE)
  } else if (variable[1] == "names") {
    cat(paste0(param.names, collapse = "\n"))
  } else {
    for(i in 1:length(variable)){
      cat(paste0("\n", variable[i], "\n"))
      param.details <- str(HISAFE.PARAMS[[variable[i]]],
                           comp.str = "",
                           give.length = FALSE,
                           give.head = FALSE,
                           no.list = TRUE)
    }
  }
  invisible(param.names)
}

#' Display supported Hi-sAFe output profiles
#' @description Displays supported Hi-sAFe output profiles and standard output frequency.
#' @return Invisibly returns a data frame containing the profiles names and output frequency.
#' @export
#' @family hisafe definition functions
#' @examples
#' \dontrun{
#' hisafe_profiles()
#' }
hisafe_profiles <- function(variable = "names") {
  print(as.data.frame(SUPPORTED.PROFILES), row.names = FALSE)
}
