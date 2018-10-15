#' Calculate water and nitrogen budgets
#' @description Calculates water and nitrogen budgets
#' See \code{\link{plot_hisafe_cycle_bar}} for details of each cycle.
#' @return A data.frame (tibble) containing the budget data.
#' @param hop An object of class hop or face.
#' @param cycle A character vector of the cycle to analyze Supported cycles include: 'water', and 'nitrogen.
#' @param freq One of "year", "month", "day".
#' @param doy.start The JulianDay [1-365] on which to start the annual cycle accounting. Use 'sim' to specify the starting JulianDay of the simulation.
#' Only used if \code{freq} is "year".
#' @param simu.names A character vector of the SimulationNames within \code{hop} to include. Use "all" to include all available values.
#' @param years A numeric vector of the years within \code{hop} to include. Use "all" to include all available values.
#' @param save.table Logical indicating whether a table of budget data and calculations should be saved to \code{output.path}.
#' @param save.plot Logical indicating whether a plot of the budget excess should be saved to \code{output.path}.
#' @param output.path A character string indicating the path to the directory where plots should be saved.
#' If \code{NULL}, the experiment/simulation path is read from the hop object, and a directory is created there called "analysis/budgets".
#' The tables and plots will be saved in this directory.
#' @export
#' @importFrom dplyr %>%
#' @import ggplot2
#' @family hisafe analysis functions
#' @examples
#' \dontrun{
#' water.budget <- hisafe_budget(myhop, "water")
#' }
hisafe_budget <- function(hop,
                          cycle,
                          freq        = "year",
                          doy.start   = "sim",
                          simu.names  = "all",
                          years       = "all",
                          months      = "all",
                          save.table  = TRUE,
                          save.plot   = TRUE,
                          output.path = NULL) {

  supported.cycles <- c("water", "nitrogen")
  supported.saves  <- c("table", "plot")

  is_hop(hop, error = TRUE)
  profile_check(hop = hop, profile = "cells", error = TRUE)

  if(years[1] == "all") years <- unique(hop$cells$Year)
  freq <- tolower(freq)

  if(!all(cycle %in% supported.cycles))                   stop(paste0("cycle argument must be one of:", paste(supported.cycles, collapse = ", ")), call. = FALSE)
  if(!(freq  %in% c("year", "month", "day")))             stop("freq argument must be one of: year, month, day",                                   call. = FALSE)
  if(!all(is.numeric(years)))                             stop("years argument must be 'all' or a numeric vector",                                 call. = FALSE)
  if(!all(years %in% unique(hop$cells$Year)))             stop(paste("not all values in years are present in the cells profile"),                  call. = FALSE)
  if(!(is.character(output.path) | is.null(output.path))) stop("output.path argument must be a character string",                                  call. = FALSE)
  is_TF(save.table)
  is_TF(save.plot)

  hop <- hop_filter(hop        = hop,
                    simu.names = simu.names,
                    years      = years,
                    months     = months)

  group.cols <- c("SimulationName", "Date")

  ## FLUXES
  flux.raw <- plot_hisafe_cycle_bar(hop       = hop,
                                    cycle     = cycle,
                                    freq      = freq,
                                    doy.start = ifelse(freq == "year", doy.start, 1),
                                    plot      = FALSE) %>%
    dplyr::select(cycle, SimulationName, Date, flux, value)

  flux.spread <- flux.raw %>%
    tidyr::spread(key = "flux", value = "value")

  flux <- flux.raw %>%
    dplyr::group_by_at(group.cols) %>%
    dplyr::summarize(value = sum(value)) %>%
    dplyr::rename(sum.of.fluxes = value)

  ## STOCK
  if(cycle == "water") {
    profile_check(hop  = hop, profile = "climate", error = TRUE)
    variable_check(hop = hop, profile = "cells",   variables = c("waterStock", "mulchWaterStock"),  error = TRUE)
    variable_check(hop = hop, profile = "climate", variables = "stockedSnow",                       error = TRUE)
    stock <- hop$cells %>%
      dplyr::select(SimulationName, Date, waterStock, mulchWaterStock) %>%
      dplyr::group_by_at(group.cols) %>%
      dplyr::summarize(waterStock      = mean(waterStock),
                       mulchWaterStock = mean(mulchWaterStock)) %>%
      dplyr::left_join(dplyr::select(hop$climate, SimulationName, Date, stockedSnow), by = group.cols) %>%
      dplyr::mutate(totalStock = waterStock + stockedSnow + mulchWaterStock)

  } else if(cycle == "nitrogen") {
    variable_check(hop = hop, profile = "cells", variables = c("mineralNitrogenStock", "activeNitrogenHumusStock", "inactiveNitrogenHumusStock",
                                                               "nitrogenResidus", "nitrogenMicrobes", "nitrogenMicrobesMulch"), error = TRUE)
    stock <- hop$cells %>%
      dplyr::select(SimulationName, Date, mineralNitrogenStock, activeNitrogenHumusStock, inactiveNitrogenHumusStock,
                    nitrogenResidus, nitrogenMulch, nitrogenMicrobes, nitrogenMicrobesMulch) %>%
      dplyr::group_by_at(group.cols) %>%
      dplyr::summarize(mineralNitrogenStock       = mean(mineralNitrogenStock),
                       activeNitrogenHumusStock   = mean(activeNitrogenHumusStock),
                       inactiveNitrogenHumusStock = mean(inactiveNitrogenHumusStock),
                       nitrogenResidus            = mean(nitrogenResidus),
                       nitrogenMulch              = mean(nitrogenMulch),
                       nitrogen.microbes          = mean(nitrogenMicrobes) + mean(nitrogenMicrobesMulch)) %>%
      dplyr::mutate(totalStock = mineralNitrogenStock + activeNitrogenHumusStock + inactiveNitrogenHumusStock +
                      nitrogenResidus + nitrogenMulch + nitrogen.microbes)
  }

  stock.times <- flux %>%
    dplyr::ungroup() %>%
    dplyr::select(SimulationName, Date)

  stock <- stock %>%
    dplyr::mutate(totalStockStart = c(NA, totalStock[1:(length(totalStock)-1)])) %>%
    dplyr::mutate(totalStockEnd   = totalStock) %>%
    dplyr::select(-totalStock) %>%
    dplyr::inner_join(stock.times, by = group.cols)

  if(freq == "day") {
    stock <- stock %>%
      dplyr::mutate(stockChange = totalStockEnd - totalStockStart)
  } else {
    stock <- stock %>%
      dplyr::mutate(stockChange   = c(diff(totalStockStart), NA)) %>%
      dplyr::mutate(totalStockEnd = c(totalStockStart[2:length(totalStockStart)], NA))
  }

  budget.data <- flux.spread %>%
    dplyr::left_join(flux,  by = group.cols) %>%
    dplyr::left_join(stock, by = group.cols) %>%
    dplyr::mutate(excess.export = sum.of.fluxes + stockChange) %>%
    dplyr::arrange(SimulationName, Date)

  if(save.table | save.plot) {
    output.path <- clean_path(paste0(diag_output_path(hop = hop, output.path = output.path), "/budgets/"))
    dir.create(output.path, recursive = TRUE, showWarnings = FALSE)

    if(save.table) readr::write_csv(budget.data, paste0(output.path, "hisafe_", cycle, "_budget_", freq, ".csv"))

    if(save.plot) {
      plot.obj <- ggplot(budget.data, aes(x = Date, y = excess.export)) +
        facet_wrap(~SimulationName) +
        labs(y = paste0("Excess ", cycle, " export from scene")) +
        geom_line(na.rm  = TRUE) +
        geom_point(na.rm = TRUE) +
        theme_hisafe_ts()
      ggsave_fitmax(paste0(output.path, "hisafe_", cycle, "_budget_excess_", freq, ".jpg"), plot.obj, scale = 1.5)
    }
  }

  return(budget.data)
}

#' Compare Hi-sAFe and STICS budget calcualations
#' @description Compares Hi-sAFe and STICS budget calcualations for debugging purposes.
#' @return A data.frame (tibble) containing the budget comparison.
#' @param hop An object of class hop or face.
#' @param cycle A character vector of the cycle to analyze Supported cycles include: 'water', and 'nitrogen.
#' @param simu.names A character vector of the SimulationNames within \code{hop} to include. Use "all" to include all available values.
#' @param years A numeric vector of the years within \code{hop} to include. Use "all" to include all available values.
#' @param save.table Logical indicating whether a table of comparison data should be saved to \code{output.path}.
#' @param output.path A character string indicating the path to the directory where plots should be saved.
#' If \code{NULL}, the experiment/simulation path is read from the hop object, and a directory is created there called "analysis/budgets".
#' The tables and plots will be saved in this directory.
#' @export
#' @importFrom dplyr %>%
#' @family hisafe analysis functions
#' @examples
#' \dontrun{
#' water.budget.comp <- stics_budget_comp(myhop, "water")
#' }
stics_budget_comp <- function(hop,
                              cycle,
                              simu.names  = "all",
                              years       = "all",
                              save.table  = TRUE,
                              output.path = NULL) {

  is_hop(hop, error = TRUE)
  profile_check(hop, "cells", error = TRUE)
  if(simu.names == "all") simu.names <- hop$metadata$SimulationName

  stock.names  <- c("waterStock", "stockedSnow", "mulchWaterStock",
                    "mineralNitrogenStock", "activeNitrogenHumusStock", "inactiveNitrogenHumusStock", "nitrogenResidus", "nitrogenMulch", "nitrogen.microbes")
  bad.names    <- c("sum.of.fluxes", "totalStock", "stockChange", "excess.export")

  if(cycle == "water") {
    start <- "WATER BALANCE  (mm)"
    end   <- "MINERAL NITROGEN BALANCE (kg N/ha)"
    search.names <- c("initial water content",
                      "rain", "irrigation", "capillary rise",
                      "final water content",
                      "evaporation", "transpiration", "runoff", "deep infiltration", "mole drainage",
                      "leaf interception", "mulch interception", "ineffective irrigation")

  } else if(cycle == "nitrogen") {
    start <- "MINERAL NITROGEN BALANCE (kg N/ha)"
    end   <- "ORGANIC CARBON BALANCE   (kg C/ha)"

    search.names <- c("initial plant N", "initial soil NH4", "initial soil NO3", "rain", "irrigation",
                      "fertiliser", "symbiotic fixation", "humus mineralisation", "residue mineralisation",
                      "final plant N", "N uptake exported", "N uptake returned", "final soil NH4", "final soil NO3", "leaching",
                      "leaching in mole drains", "fertiliser N immobilis", "fertiliser N volatilis", "manure N volatilis", "N2 and N2O losses",
                      "Active humified pool", "Inert humified pool", "Zymogeneous biomass pool", "Soil residues pool", "Mulch residues pool",
                      "Fertiliser Immobilised", "Added organic fertil", "Added Crop residues", "Added Roots", "Added Fallen leaves", "Added Trimmed leaves",
                      "Priming", "MineralisationPriming")
  }

  if(save.table) {
    output.path.simu <- clean_path(paste0(diag_output_path(hop = hop, output.path = output.path), "/budgets/"))
    dir.create(output.path.simu, recursive = TRUE, showWarnings = FALSE)
  }

  OUT <- dplyr::tibble()
  for(simu.name in simu.names) {
    ## HISAFE BUDGET
    hisafe.budget <- hisafe_budget(hop         = hop,
                                   cycle       = cycle,
                                   freq        = "year",
                                   doy.start   = "sim",
                                   simu.names  = simu.name,
                                   years       = years,
                                   save.table  = FALSE,
                                   save.plot   = FALSE,
                                   output.path = output.path) %>%
      dplyr::mutate(model = "hisafe") %>%
      dplyr::mutate(Year  = lubridate::year(Date)) %>%
      dplyr::select(cycle, SimulationName, Year, dplyr::everything(), -Date)

    hisafe.names <- names(hisafe.budget)
    stock.names  <- stock.names[stock.names %in% names(hisafe.budget)]
    flux.names   <- names(hisafe.budget)[!(names(hisafe.budget) %in% c("SimulationName", "Year", "Date", "cycle", "model", stock.names, bad.names))]

    ## STICS BUDGET
    if(years[1] == "all") YEARS <- hisafe.budget$Year else YEARS <- years

    stics.budget.raw <- purrr::map_df(YEARS, get_stics_budget,
                                      path         = ifelse(basename(hop$path) == simu.name, dirname(hop$path), hop$path),
                                      simu.name    = simu.name,
                                      stics.names  = search.names,
                                      start.header = start,
                                      end.header   = end) %>%
      tidyr::spread(key = "category", value = "value") %>%
      dplyr::arrange(SimulationName, Year) %>%
      dplyr::rename_all(function(x) stringr::str_replace_all(x, " ", "."))

    ## EDIT STICS BUDGET
    if(cycle == "water") {
      stics.budget <- stics.budget.raw  %>%
        dplyr::mutate(model = "stics") %>%
        dplyr::mutate(cycle = cycle) %>%
        dplyr::mutate(drainage.water.table      = NA,
                      tree.interception         = NA,
                      uptakeTree                = NA,
                      uptakeInter               = NA,
                      water.added.by.watertable = NA,
                      water.uptake.sat.crop     = NA,
                      water.uptake.sat.trees    = NA,
                      stockedSnow               = NA,
                      mulchWaterStock           = NA) %>%
        dplyr::mutate(capilary.rise       = -capillary.rise,
                      crop.interception   = leaf.interception,
                      drainage.artificial = mole.drainage,
                      drainage.bottom     = deep.infiltration,
                      irrigation          = -irrigation,
                      mulch.evaporation   = mulch.interception,
                      precipitation       = -rain,
                      runOff              = runoff,
                      soil.evaporation    = evaporation,
                      uptakeMain          = transpiration,
                      waterStock          = c(initial.water.content[1], final.water.content[-nrow(.)]))

    } else if(cycle == "nitrogen") {
      stics.budget <- stics.budget.raw  %>%
        dplyr::mutate(model = "stics") %>%
        dplyr::mutate(cycle = cycle) %>%
        dplyr::mutate(initial.mineral.N = initial.soil.NH4  + initial.soil.NO3) %>%
        dplyr::mutate(final.mineral.N   = final.soil.NH4    + final.soil.NO3) %>%
        dplyr::mutate(N.uptake          = N.uptake.exported + N.uptake.returned) %>%
        dplyr::mutate(leaching.watertable       = NA,
                      #runoff                  = NA, # STICS CURRENTLY DOES NOT INCLUDE THIS
                      tree.leaf.litter          = NA,
                      tree.shallow.root.litter  = NA,
                      tree.deep.root.litter     = NA,
                      uptakeTree                = NA,
                      uptakeInter               = NA,
                      watertable                = NA,
                      nitrogen.uptake.sat.crop  = NA,
                      nitrogen.uptake.sat.trees = NA) %>%
        dplyr::mutate(crop.leaf.litter           = -Added.Crop.residues + -Added.Fallen.leaves + -Added.Trimmed.leaves,
                      crop.root.litter           = -Added.Roots,
                      deposition                 = -rain,
                      fertilization.mineral      = -fertiliser,
                      fertilization.organic      = -Added.organic.fertil,
                      fixation                   = -symbiotic.fixation,
                      irrigation                 = -irrigation,
                      leaching.artificial        = leaching.in.mole.drains,
                      leaching.bottom            = leaching,
                      denitrification            = N2.and.N2O.losses,
                      uptakeMain                 = N.uptake,
                      volatilization.mineral     = fertiliser.N.volatilis,
                      volatilization.organic     = manure.N.volatilis,
                      mineralNitrogenStock       = c(initial.mineral.N[1], final.mineral.N[-nrow(.)]),
                      activeNitrogenHumusStock   = Active.humified.pool,
                      inactiveNitrogenHumusStock = Inert.humified.pool,
                      nitrogenResidus            = Soil.residues.pool,
                      nitrogenMulch              = Mulch.residues.pool,
                      nitrogen.microbes          = Zymogeneous.biomass.pool)
    }

    stics.budget <- stics.budget %>%
      dplyr::select_at(c("model", "cycle", "SimulationName", "Year", flux.names, stock.names)) %>%
      dplyr::mutate(sum.of.fluxes = dplyr::select_at(., flux.names)  %>% apply(1, sum, na.rm = TRUE)) %>%
      dplyr::mutate(totalStock    = dplyr::select_at(., stock.names) %>% apply(1, sum, na.rm = TRUE)) %>%
      dplyr::mutate(stockChange   = c(diff(totalStock), NA)) %>%
      dplyr::mutate(excess.export = sum.of.fluxes + stockChange)

    ## MERGE
    out <- hisafe.budget %>%
      dplyr::mutate(model = "hisafe") %>%
      dplyr::bind_rows(dplyr::mutate(stics.budget, model = "stics")) %>%
      tidyr::gather(key = "variable", value = "value", -SimulationName, -Year, -cycle, -model) %>%
      tidyr::spread(key = "model", value = "value") %>%
      dplyr::mutate(hiasfe.stics.diff = hisafe - stics) %>%
      dplyr::mutate(variable = factor(variable, levels = c(flux.names, "sum.of.fluxes", stock.names, "totalStock", "stockChange", "excess.export"))) %>%
      dplyr::select(cycle, dplyr::everything()) %>%
      dplyr::arrange(SimulationName, variable, Year)


    if(save.table) readr::write_csv(out, paste0(output.path.simu, "stics_", cycle, "_budget_comp_", simu.name, ".csv"))

    OUT <- dplyr::bind_rows(OUT, out)
  }
  return(OUT)
}

#' Get stics budget values from .bil export files
#' @description Gets stics budget values from .bil export files.
#' Used within \code{\link{stics_budget_comp}} functions.
#' @return A tibble with extracted values.
#' @param year Year of the file to extract.
#' @param simu.name Year of the file to extract.
#' @param path Path to simulation folder.
#' @param stics.names Names of the stics budget variables.
#' @param start.header Character string of the starting line to search.
#' @param end.header Character string of the final line to search.
#' @importFrom dplyr %>%
#' @keywords internal
get_stics_budget <- function(year,
                             simu.name,
                             path,
                             stics.names,
                             start.header,
                             end.header) {

  if(!requireNamespace("ggforce", quietly = TRUE)) stop("The package 'stringr' is required for stics_budget_comp(). Please install and load it.", call. = FALSE)

  file.paths <- list.files(path       = paste0(path, "/", simu.name, "/output-", simu.name),
                           pattern    = paste0("rapport_[0-9][0-9][0-9]_", year, ".bil"),
                           full.names = TRUE)
  if(length(file.paths) == 0) stop(paste0("No .bil files found for ", simu.name, " ", year,
                                          ". To export STICS .bil files, use sticsReport = 1 when defining a simulation."), call. = FALSE)

  OUT <- dplyr::tibble()
  for(stics.file.path in file.paths) {
    if(!file.exists(stics.file.path)) stop(paste0("The STICS output .bil file does not exist for ", simu.name, " ", year,
                                                  ". To export STICS .bil files, use sticsReport = 1 when defining a simulation."), call. = FALSE)

    idCell <- stringr::str_extract(stics.file.path, "_[0-9][0-9][0-9]_") %>%
      stringr::str_extract("[0-9][0-9][0-9]") %>%
      as.numeric()

    dum <- scan(file     = stics.file.path,
                what     = "character",
                encoding = "latin1",
                sep      = "\n",
                quiet    = TRUE) %>%
      stringr::str_replace_all("immobilis\\.",        "immobilis") %>%
      stringr::str_replace_all("volatilis\\.",        "volatilis") %>%
      stringr::str_replace_all("fertil\\.",           "fertil") %>%
      stringr::str_replace_all("Priming \\(PE\\)",    "Priming") %>%
      stringr::str_replace_all("Mineralisation - PE", "MineralisationPriming") %>%
      write(file = stics.file.path)

    stics.file <- scan(file     = stics.file.path,
                       what     = "character",
                       encoding = "latin1",
                       sep      = ".",
                       quiet    = TRUE)

    if(!(start.header %in% stics.file)) {
      stop(paste0("The STICS output .bil file for ", simu.name, " ", year, " (idCell ", idCell,
                  ") does not contain budget data. This is likely because this cell is bare soil. ",
                  "get_stics_budget() does not work with bare soil cells.Please select different years."), call. = FALSE)
    }

    stics.file <- stics.file %>%
      .[which(. == start.header):(which(. == end.header) - 1)]

    values <- purrr::map(paste0("^ *", stics.names, ".*[0-9]*"), stringr::str_extract, string = stics.file) %>%
      purrr::map(function(x) x[!is.na(x)]) %>%
      purrr::map(stringr::str_extract, pattern = "-*[0-9]+$") %>%
      purrr::map(function(x) ifelse(length(x) == 0, NA_character_, x)) %>%
      unlist() %>%
      as.numeric()

    out <- dplyr::tibble(SimulationName = simu.name, Year = year, idCell = idCell, category = stics.names, value = values)
    OUT <- dplyr::bind_rows(OUT, out)
  }

  OUT <- OUT %>%
    dplyr::group_by(SimulationName, Year, category) %>%
    dplyr::summarize(value = mean(value)) %>%
    dplyr::ungroup()

  return(OUT)
}
