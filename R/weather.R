#' Read weather data from a Hi-sAFe .WTH file.
#' @description Reads weather data from a Hi-sAFe .WTH file.
#' @return A data frame (tibble) of the weather data.
#' @param path A character string of the path (relative or absolute) to the .WTH file.
#' @export
#' @family hisafe weather functions
#' @examples
#' \dontrun{
#' wth <- read_weather("./my_site.wth")
#' }
read_weather <- function(path) {
  if(!file.exists(path)) stop("file specfied by path does not exist", call. = FALSE)
  wth <- dplyr::as_tibble(read.table(file             = path,
                                     header           = FALSE,
                                     sep              = "\t",
                                     stringsAsFactors = FALSE,
                                     comment.char     = "#",
                                     encoding         = "latin1"))
  names(wth) <- c(c("doy", "year", "month", "day", "Tmax", "Tmin", "RHmax", "RHmin",
                  "Rg", "precip", "wind", "watertable", "CO2"), "trueyear"[ncol(wth) == 14])

  if(!("trueyear" %in% names(wth))) wth$trueyear <- NA_integer_

  return(wth)
}

#' Write weather data to a Hi-sAFe .WTH file.
#' @description Writes weather data to a Hi-sAFe .WTH file.
#' @param data A data frame containing the weather data.
#' @param path A character string of the path (relative or absolute) to the .WTH file.
#' @export
#' @family hisafe weather functions
#' @examples
#' \dontrun{
#' write_weather(wth, "./my_site_modified.wth")
#' }
write_weather <- function(data, path) {
  required.names <- c("doy", "year", "month", "day", "Tmax", "Tmin", "RHmax", "RHmin",
                      "Rg", "precip", "wind", "watertable", "CO2", "trueyear")

  if(!any(c("tbl", "data.frame") %in% class(data))) stop("data must be of class data.frame or tibble", call. = FALSE)
  if(!identical(names(data), required.names))       stop(paste("names of data columns must be:",
                                                               paste(required.names, collapse = ", ")), call. = FALSE)
  if(!(substr(path, nchar(path)-2, nchar(path)) %in% c("WTH", "wth"))) stop("path must point to a file with extension .wth or .WTH", call. = FALSE)
  if(file.exists(path))                             stop("file specfied by path already exists", call. = FALSE)

  readr::write_delim(x         = data,
                     path      = path,
                     delim     = "\t",
                     col_names = FALSE)
}
