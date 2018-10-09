#' Calculate water and nitrogen budgets
#' @description Calculates water and nitrogen budgets
#' See \code{\link{plot_hisafe_cycle_annual}} for details of each cycle.
#' @return A data.frame (tibble) containing the budget data.
#' @param hop An object of class hop or face.
#' @param cycle A character vector of the cycle to analyze Supported cycles include: 'water', and 'nitrogen.
#' @param doy.start The JulianDay [1-365] on which to start the annual cycle accounting. Use 'sim' to specify the starting JulianDay of the simulation.
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
                          doy.start   = "sim",
                          simu.names  = "all",
                          years       = "all",
                          save.table  = TRUE,
                          save.plot   = TRUE,
                          output.path = NULL) {

  supported.cycles <- c("water", "nitrogen")
  supported.saves  <- c("table", "plot")

  is_hop(hop, error = TRUE)
  profile_check(hop, c("cells", "plot"), error = TRUE)

  if(years[1] == "all") years <- unique(hop$cells$Year)

  if(!all(cycle %in% supported.cycles))                   stop(paste0("cycle argument must be one of:", paste(supported.cycles, collapse = ", ")), call. = FALSE)
  if(!all(is.numeric(years)))                             stop("years argument must be 'all' or a numeric vector",                                 call. = FALSE)
  if(!all(years %in% unique(hop$cells$Year)))             stop(paste("not all values in years are present in the cells profile"),                  call. = FALSE)
  if(!(is.character(output.path) | is.null(output.path))) stop("output.path argument must be a character string",                                  call. = FALSE)
  is_TF(save.table)
  is_TF(save.plot)

  hop <- hop_filter(hop = hop, simu.names = simu.names)

  ## FLUXES
  flux.raw <- plot_hisafe_cycle_annual(hop = hop, cycle = cycle, doy.start = doy.start, plot = FALSE) %>%
    dplyr::filter(Year %in% years)

  flux.spread <- flux.raw %>%
    tidyr::spread(key = "flux", value = "value")

  flux <- flux.raw %>%
    dplyr::summarize(value = sum(value)) %>%
    dplyr::rename(sum.of.fluxes = value)

  ## STOCK
  if(doy.start != "sim") hop$plot.info$simulationDayStart <- doy.start
  stock <- hop$cells %>%
    dplyr::left_join(hop$plot.info, by = "SimulationName") %>%
    dplyr::filter(Year %in% years) %>%
    dplyr::filter(JulianDay == simulationDayStart)

  if(cycle == "water") {
    variable_check(hop = hop, profile = "cells", variables = "waterStock")
    stock <- stock %>%
      dplyr::select(SimulationName, Year, waterStock) %>%
      dplyr::group_by(SimulationName, Year) %>%
      dplyr::summarize(waterStock = mean(waterStock)) %>%
      dplyr::mutate(totalStock = waterStock) %>%
      dplyr::mutate(stockChange = c(diff(totalStock), NA))

  } else if(cycle == "nitrogen") {
    variable_check(hop = hop, profile = "cells", variables = c("mineralNitrogenStock", "activeNitrogenHumusStock", "inactiveNitrogenHumusStock",
                                                               "nitrogenResidus", "nitrogenMicrobes", "nitrogenMicrobesMulch"))
    stock <- stock %>%
      dplyr::select(SimulationName, Year, mineralNitrogenStock, activeNitrogenHumusStock, inactiveNitrogenHumusStock,
                    nitrogenResidus, nitrogenMulch, nitrogenMicrobes, nitrogenMicrobesMulch) %>%
      dplyr::group_by(SimulationName, Year) %>%
      dplyr::summarize(mineralNitrogenStock       = mean(mineralNitrogenStock),
                       activeNitrogenHumusStock   = mean(activeNitrogenHumusStock),
                       inactiveNitrogenHumusStock = mean(inactiveNitrogenHumusStock),
                       nitrogenResidus            = mean(nitrogenResidus),
                       nitrogenMulch              = mean(nitrogenMulch),
                       nitrogen.microbes          = mean(nitrogenMicrobes) + mean(nitrogenMicrobesMulch)) %>%
      dplyr::mutate(totalStock = mineralNitrogenStock + activeNitrogenHumusStock + inactiveNitrogenHumusStock +
                      nitrogenResidus + nitrogenMulch + nitrogen.microbes) %>%
      dplyr::mutate(stockChange = c(diff(totalStock), NA))
  }

  budget.data <- flux.spread %>%
    dplyr::left_join(flux,  by = c("SimulationName", "Year")) %>%
    dplyr::left_join(stock, by = c("SimulationName", "Year")) %>%
    dplyr::mutate(excess.export = sum.of.fluxes + stockChange)

  if(save.table | save.plot) {
    output.path <- clean_path(paste0(diag_output_path(hop = hop, output.path = output.path), "/budgets/"))
    dir.create(output.path, recursive = TRUE, showWarnings = FALSE)

    if(save.table) readr::write_csv(budget.data, paste0(output.path, "hisafe_", cycle, "_budget.csv"))

    if(save.plot) {
      plot.obj <- ggplot(budget.data, aes(x = Year, y = excess.export, color = SimulationName, linetype = SimulationName)) +
        labs(y = paste0("Excess ", cycle, " export from scene")) +
        geom_line(na.rm = TRUE) +
        geom_point(na.rm = TRUE) +
        scale_color_manual(values    = rep(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), 6)) +
        scale_linetype_manual(values = rep(1:6, each = 8)) +
        theme_hisafe_ts()
      ggsave_fitmax(paste0(output.path, "hisafe_", cycle, "_budget_excess.jpg"), plot.obj, scale = 1.5)
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

  stock.names  <- c("waterStock",
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
                                   doy.start   = "sim",
                                   simu.names  = simu.name,
                                   years       = years,
                                   save.table  = FALSE,
                                   save.plot   = FALSE,
                                   output.path = output.path) %>%
      dplyr::mutate(model = "hisafe")

    hisafe.names <- names(hisafe.budget)
    stock.names  <- stock.names[stock.names %in% names(hisafe.budget)]
    flux.names   <- names(hisafe.budget)[!(names(hisafe.budget) %in% c("SimulationName", "Year", "cycle", "model", stock.names, bad.names))]

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
                      water.uptake.sat.trees    = NA) %>%
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
        dplyr::mutate(leaching.watertable = NA,
                      runoff              = NA,
                      tree.leaf.litter    = NA,
                      tree.root.litter    = NA,
                      uptakeTree         = NA,
                      uptakeInter         = NA,
                      watertable          = NA) %>%
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
