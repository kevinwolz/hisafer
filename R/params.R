#' Reads parameter files
#' @description Reads parameter files for editing
#' Used within \code{\link{build_structure}}.
#' @return A list containing all parameter values and constraints.
#' @param path A character string of the path to the file to read.
#' @importFrom dplyr %>%
#' @family hisafe param functions
#' @keywords internal
read_param_file <- function(path) {
  sim <- scan(file = path, what = "character", encoding = "latin1", sep = "\n", quiet = TRUE)
  titles        <- grepl("##", substr(sim, 1, 2))         # which lines have are headers
  single.hash   <- grepl("#",  substr(sim, 1, 1))          # which lines are commented out
  has.equals    <- grepl(" = ", sim)
  pld.tabledata <- grepl("LAYER", sim) | grepl("LAYERINIT", sim) | grepl("TREE", sim)| grepl("ZONE", sim)| grepl("TREETEC", sim)| grepl("FERTILPARAM", sim)| grepl("RESIDUEPARAM", sim) | grepl("RESIDUEINC", sim) | grepl("TILLAGE", sim) | grepl("IRRIGATION", sim) | grepl("FERTILIZATION", sim) | grepl("CUTTING", sim)
  notes         <- single.hash & !titles & !has.equals & !pld.tabledata # which lines are notes
  comment       <- substr(sim, 1, 1) == "#"

  read_element_table <- function(sim, i, titles, table.names) {
    if(any(which(titles) > i)) {
      next.header <- which(titles)[which(titles) > i][[1]]
      table.elements <- strsplit(remove_whitespace(sim[i:(next.header - 1)]), split = "\t")
    } else {
      table.elements <- strsplit(remove_whitespace(sim[i:length(sim)]), split = "\t")
    }

    clean_elements <- function(x, table.names) {
      data.frame(t(matrix(x)))
      names(x) <- table.names
      return(x)
    }
    col_types <- readr::cols()
    table.tibble <- purrr::map(table.elements, clean_elements, table.names = table.names) %>%
      purrr::map_df(dplyr::bind_rows) %>%
      readr::type_convert(col_types = readr::cols())

    return(table.tibble)
  }

  tables <- list(ZONE             = c("name", "zoneName", "zoneCellList", "zoneTecFileNameList"),                                    # .SIM
                 TREETEC          = c("name", "treeTecFileName"),                                                                    # .SIM
                 LAYER            = c("name", "thick", "sand", "clay", "limeStone", "organicMatter",                                 # .PLD
                                       "partSizeSand", "stone", "stoneType", "infiltrability"),
                 LAYERINIT        = c("name", "waterContent", "no3Concentration", "nh4concentration"),                               # .PLD
                 TREE             = c("name", "treeSpeciesFileName", "treeX", "treeY"),                                   # .PLD
                 VARIETE          = c("name", "ID", "codevar", "stlevamf" ,"stamflax" ,"stlevdrp" ,"stflodrp", "stdrpdes", "pgrainmaxi",     # .PLT
                                       "adens", "croirac", "durvieF", "jvc", "sensiphot", "stlaxsen", "stsenlan", "nbgrmax",
                                       "stdrpmat", "afruitpot", "dureefruit"),
                 RESIDUEINC     = c("name", "julres", "coderes", "qres", "Crespc", "CsurNres", "Nminres", "eaures"),                       # .TEC
                 TILLAGE        = c("name", "jultrav", "profres", "proftrav"),                                                             # .TEC
                 IRRIGATION     = c("name", "julapl", "qte"),                                                                              # .TEC
                 FERTILIZATION  = c("name", "julapN", "qte"),                                                                              # .TEC
                 CUTTING        = c("name", "julfauche", "hautcoupe", "lairesiduel", "msresiduel", "anitcoupe"),                           # .TEC
                 FERTILPARAM    = c("name", "engamm", "orgeng", "deneng", "voleng"),                                                       # stics.par
                 RESIDUEPARAM   = c("name", "CroCo", "akres", "bkres", "awb", "bwb", "cwb", "ahres", "bhres", "kbio", "yres", "CNresmin",  # stics.par
                                     "CNresmax", "qmulchruis0", "mouillabilmulch", "kcouvmlch", "albedomulchresidus", "Qmulchdec"))
  new.sim <- list()
  next_threshold <- 0
  list.title <- "temp"
  for(i in 1:length(sim)) {

    if(i < next_threshold) next

    ##this is a comment
    if(notes[i]) next

    ##this is a title
    if(titles[i]){
      list.title <- gsub(pattern = " ",
                         replacement = "_",
                         remove_whitespace(gsub(pattern = "#", replacement = "", sim[i]))) #on enleve les tabs, leading et trailing blanks
      toto <- list(c())
      names(toto) <- list.title
      new.sim <- c(new.sim, toto)

    ##this is a keyword = value
    } else if (has.equals[i]) {

      line.text <- ifelse(comment[i], substr(sim[i], start = 2, stop = 10000), sim[i]) # remove first # only (definitions possible after another #)

      element.name  <- unlist(lapply(strsplit(line.text, split = "=", fixed = TRUE), "[[", 1))
      element.name  <- remove_whitespace(element.name) # remove tabs, leading, and trailing blanks

      element.vals  <- purrr::map_chr(strsplit(line.text, split = "=", fixed = TRUE), 2)
      element.vals  <- remove_whitespace(strsplit(element.vals, split = "#", fixed = TRUE)[[1]])

      element.value <- element.vals[1]

      test <- strsplit(element.value, split = "-", fixed = TRUE)

      ## This is a date format YYYY-MM-DD
      if (nchar(element.value)==10 && grepl("-", element.value) && nchar(test[[1]][1])==4 && grepl("^[0-9]+$", test[[1]][1])) {
        element.value <- as.Date(element.value);

      } else {
        if (element.name=="zoneCellList") {

        }
        else {
          if(grepl(",", element.value)) {
            element.value <- strsplit(element.value, split = ",")
            if (nchar(element.value[[1]][1])==5 && grepl("-", element.value[[1]][1])) {

            }
            else {
              if(substr(element.value[[1]][1], 1, 1) %in% as.character(0:9)) {
                element.value <- purrr::map(element.value, as.numeric)
              }
            }

          } else {

            if (grepl("-", element.value) || grepl("Version", element.name)) {

            }
            else {
              if(substr(element.value, 1, 1) %in% as.character(0:9)) {
                element.value <- as.numeric(element.value)
              }
            }

          }
        }
      }

      toto <- list(list(value     = element.value,
                        commented = comment[i]))

      names(toto) <- element.name
      new.sim[[list.title]] <- c(new.sim[[list.title]], toto)
    }
    ##this is a table
    else {


      line.text <- sim[i]
      if(grepl("ZONE", line.text)) {
        list.names <- c("name", "zoneName", "zoneCellList", "zoneTecFileNameList")
        table.name <- "zone"
      }
      else if(grepl("TREETEC", line.text)) {
        list.names <- c("name", "treeTecFileName")
        table.name <- "treetec"
      }
      else if(grepl("LAYERINIT", line.text)) {
        list.names <- c("name", "waterContent", "no3Concentration", "nh4concentration")
        table.name <- "layerinit"
      }
      else if(grepl("LAYER", line.text)) {
        list.names <- c("name", "thick", "sand", "clay", "limeStone", "organicMatter", "partSizeSand", "stone", "stoneType", "infiltrability")
        table.name <- "layer"
      }
      else if(grepl("TREE", line.text)) {
        list.names <- c("name", "treeSpeciesFileName", "treeX", "treeY")
        table.name <- "tree"
      }
      else if(grepl("VARIETE", line.text)) {
        list.names <- c("name", "ID", "codevar", "stlevamf" ,"stamflax" ,"stlevdrp" ,"stflodrp", "stdrpdes", "pgrainmaxi","adens", "croirac", "durvieF", "jvc", "sensiphot", "stlaxsen", "stsenlan", "nbgrmax", "stdrpmat", "afruitpot", "dureefruit")
        table.name <- "variete"
      }
      else if(grepl("RESIDUEINC", line.text)) {
        list.names <- c("name", "julres", "coderes", "qres", "Crespc", "CsurNres", "Nminres", "eaures")
        table.name <- "residueinc"
      }
      else if(grepl("TILLAGE", line.text)) {
        list.names <- c("name", "jultrav", "profres", "proftrav")
        table.name <- "tillage"
      }
      else if(grepl("IRRIGATION", line.text)) {
        list.names <- c("name", "julapl", "qte")
        table.name <- "irrigation"
      }
      else if(grepl("FERTILIZATION", line.text)) {
        list.names <-  c("name", "julapN", "qte")
        table.name <- "fertilization"
      }
      else if(grepl("CUTTING", line.text)) {
        list.names <- c("name", "julfauche", "hautcoupe", "lairesiduel", "msresiduel", "anitcoupe")
        table.name <- "cutting"
      }
      else if(grepl("FERTILPARAM", line.text)) {
        list.names <- c("name", "engamm", "orgeng", "deneng", "voleng")
        table.name <- "fertilparam"
      }
      else if(grepl("RESIDUEPARAM", line.text)) {
        list.names <- c("name", "CroCo", "akres", "bkres", "awb", "bwb", "cwb", "ahres", "bhres", "kbio", "yres", "CNresmin", "CNresmax", "qmulchruis0", "mouillabilmulch", "kcouvmlch", "albedomulchresidus", "Qmulchdec")
        table.name <- "residueparam"
      }

      element.table <- read_element_table(sim, i, titles, list.names)
      element.table.reduced <- element.table[!grepl("#", element.table[[1]]),]
      if(nrow(element.table.reduced) > 0) {
        toto <- list(list(value = list(element.table), commented = FALSE, range = NA, type = NA, accepted = NA))
      } else {
        toto <- list(list(value = list(element.table), commented = TRUE,  range = NA, type = NA, accepted = NA))
      }
      names(toto) <- table.name
      new.sim[[list.title]] <- c(new.sim[[list.title]], toto)
      next_threshold <- i + nrow(element.table)
    }

  }
  return(new.sim)
}

#' Writes parameter files
#' @description Writes parameter files after editing
#' Used within \code{\link{build_structure}}.
#' @return Invisibly returns \code{TRUE}.
#' @param.list A list containing all parameter values and constraints.
#' @param path A character string of the path to the file to read.
#' @family hisafe param functions
#' @keywords internal
write_param_file <- function(param.list, path) {
  sim.out <- character(0)
  for(i in 1:length(param.list)){ # headings
    if(i == 1) {
      sim.out <- paste0(sim.out, paste("##", gsub("_", " ", names(param.list)[i])))
    } else {
      sim.out <- paste(sim.out, paste("\n##", gsub("_", " ", names(param.list)[i])), sep="\n")
    }
    if(length(param.list[[i]]) > 0) {
      for(j in 1:length(param.list[[i]])) { # elements
        if(!is.data.frame(param.list[[i]][[j]]$value[[1]])){ # is this a normal variable or a table of variables
          to.comment <- param.list[[i]][[j]]$commented | all(is.na(param.list[[i]][[j]]$value))
          comment.sign <- ifelse(to.comment, "#", "")
          sim.out <- paste(sim.out, paste0(comment.sign,
                                           names(param.list[[i]])[j],
                                           " = ",
                                           paste0(param.list[[i]][[j]]$value[[1]], collapse = ",")),
                           sep = "\n")
        } else {
          table_out <- function(x, sim.out) {
            sim.out <- paste(sim.out, paste0("#", paste(names(x), collapse = "\t")), sep = "\n")
            for(i in 1:nrow(x)) {
              sim.out <- paste(sim.out, paste(as.matrix(x[i,]), collapse = "\t"), sep = "\n")
            }
            return(sim.out)
          }

          char.table <- dplyr::as_tibble(param.list[[i]][[j]]$value[[1]]) %>%
            dplyr::mutate_all(as.character)
          sim.out <- table_out(char.table, sim.out)
        }
      }
    }
  }
  write(sim.out, file = path)
  invisible(TRUE)
}

#' Edits parameter files
#' @description Edits parameter files using the exp.plan of a hip object
#' Used within \code{\link{build_structure}}.
#' @return A edited list containing all parameter values and constraints.
#' @param param.list A list containing all parameter values and constraints.
#' @param exp.plan A exp.plan element of a hip object.
#' @family hisafe param functions
#' @keywords internal
edit_param_file <- function(param.list, exp.plan) {
  for(i in names(exp.plan)){
    param.list <- edit_param_element(param.list, i, exp.plan[[i]])
  }
  return(param.list)
}

#' Edit an individual parameter element
#' @description Edits an individual element of a parameter list
#' Used within \code{\link{build_structure}}.
#' @return A edited list containing all parameter values and constraints.
#' @param param.list A list containing all parameter values and constraints.
#' @param variable A character string of the name of the variable to edit.
#' @param value A numeric vector of the value to apply to the variable.
#' @family hisafe param functions
#' @keywords internal
edit_param_element <- function(param.list, variable, value) {
  success <- FALSE
  headers <- names(param.list)
  for(i in headers) {
    if(variable %in% names(param.list[[i]])) {
      param.list[[i]][[variable]]$value     <- value
      param.list[[i]][[variable]]$commented <- FALSE
      success <- TRUE
    }
  }
  if(!success) stop(paste0("Variable ", variable, " was not found in the template file and could not be edited."), .call = FALSE)
  return(param.list)
}

#' Read all template parameters
#' @description Reads all template parameter values and constraints
#' @return A list containing all parameter values and constraints.
#' @param template A character string of the path to the Hi-sAFe directory structure/files to use as a template
#' (or one of the strings signaling a default template)
#' @keywords internal
get_template_params <- function(template) {
  template.path    <- get_template_path(template)
  template.subpath <- get_template_subpath(template)

  ## check sim, pld and general parameters files
  sim.file    <- clean_path(list.files(template.path, ".sim$", full.names = TRUE))
  pld.file    <- clean_path(list.files(template.path, ".pld$", full.names = TRUE))
  hisafe.file <- clean_path(paste0(template.subpath, "/generalParameters/hisafe.par"))
  stics.file  <- clean_path(paste0(template.subpath, "/generalParameters/stics.par"))

  if(length(sim.file) > 1)  stop("there is more than 1 SIM file present in the template directory", call. = FALSE)
  if(length(pld.file) > 1)  stop("there is more than 1 PLD file present in the template directory", call. = FALSE)
  if(length(sim.file) == 0) stop("there is no SIM file present in the template directory",          call. = FALSE)
  if(length(pld.file) == 0) stop("there is no PLD file present in the template directory",          call. = FALSE)

  sim.params    <- read_param_file(sim.file)
  pld.params    <- read_param_file(pld.file)
  hisafe.params <- read_param_file(hisafe.file)
  stics.params  <- read_param_file(stics.file)

  ## check tree species and tec files
  L.TREES <- list.files(clean_path(paste0(template.subpath, "/treeSpecies")))
  L.TTEC <- list.files(clean_path(paste0(template.subpath, "/treeInterventions")))
  tree.file   <- paste0(template.subpath, "/treeSpecies/", L.TREES[1])
  ttec.file   <- paste0(template.subpath, "/treeInterventions/", L.TTEC[1])
  tree.params   <- read_param_file(tree.file)
  ttec.params   <- read_param_file(ttec.file)

  ## check crop species and tec files
  L.CROPS <- list.files(clean_path(paste0(template.subpath, "/cropSpecies")))
  L.TECS <- list.files(clean_path(paste0(template.subpath, "/cropInterventions")))
  crop.file   <- paste0(template.subpath, "/cropSpecies/", L.CROPS[1])
  tec.file    <- paste0(template.subpath, "/cropInterventions/", L.TECS[1])
  crop.params   <- read_param_file(crop.file)
  tec.params    <- read_param_file(tec.file)

  return(list(sim = sim.params, pld = pld.params, tree = tree.params, crop = crop.params, ttec = ttec.params, tec = tec.params, hisafe = hisafe.params, stics = stics.params))
}

#' Get names of template parameters
#' @description Get names of all template parameters
#' @return A list containing parameter names by file type.
#' @param x A list containing all parameter values and constraints.
#' @keywords internal
get_param_names <- function(x) {
  sim.names    <- unlist(purrr::map(x$sim,    names), use.names = FALSE)
  pld.names    <- unlist(purrr::map(x$pld,    names), use.names = FALSE)
  tree.names   <- unlist(purrr::map(x$tree,   names), use.names = FALSE)
  crop.names   <- unlist(purrr::map(x$crop,   names), use.names = FALSE)
  ttec.names   <- unlist(purrr::map(x$ttec,   names), use.names = FALSE)
  tec.names    <- unlist(purrr::map(x$tec,    names), use.names = FALSE)
  hisafe.names <- unlist(purrr::map(x$hisafe, names), use.names = FALSE)
  stics.names  <- unlist(purrr::map(x$stics,  names), use.names = FALSE)
  return(list(sim = sim.names, pld = pld.names, tree = tree.names, crop = crop.names, ttec = ttec.names, tec = tec.names, hisafe = hisafe.names, stics = stics.names))
}

#' Get values/constraints of template parameters
#' @description Gets values/constraints of all template parameters
#' @return A list containing parameter values/constraints.
#' @param x A list containing all parameter values and constraints.
#' @keywords internal
get_param_vals <- function(x, type) {
  sim.vals <- pld.vals <- tree.vals <- crop.vals <- ttec.vals  <- tec.vals <- hisafe.vals <- stics.vals <- list()
  for(i in names(x$sim))    sim.vals    <- c(sim.vals,    purrr::map(x$sim[[i]],    type))
  for(i in names(x$pld))    pld.vals    <- c(pld.vals,    purrr::map(x$pld[[i]],    type))
  for(i in names(x$tree))   tree.vals   <- c(tree.vals,   purrr::map(x$tree[[i]],   type))
  for(i in names(x$crop))   crop.vals   <- c(crop.vals,   purrr::map(x$crop[[i]],   type))
  for(i in names(x$ttec))   ttec.vals   <- c(ttec.vals,   purrr::map(x$ttec[[i]],   type))
  for(i in names(x$tec))    tec.vals    <- c(tec.vals,    purrr::map(x$tec[[i]],    type))
  for(i in names(x$hisafe)) hisafe.vals <- c(hisafe.vals, purrr::map(x$hisafe[[i]], type))
  for(i in names(x$stics))  stics.vals  <- c(stics.vals,  purrr::map(x$stics[[i]],  type))
  return(c(sim.vals, pld.vals, tree.vals, crop.vals, ttec.vals, tec.vals, hisafe.vals, stics.vals))
}

#' Complies list of parameters actually used
#' @description Compiles list of the parameters actually used (i.e. the default or the defined)
#' @return List of used parameter values
#' @param hip A "hip" object containing only a single simulation
#' @keywords internal
get_used_params <- function(hip) {
  get_used_param <- function(variable, exp.plan, template.defaults, template.commented){
    n.sims <- nrow(exp.plan)
    if(variable %in% names(exp.plan)){
      val <- exp.plan[[variable]]
      exp <- TRUE
    } else {
      commented <- template.commented[[variable]]
      if (is.null(commented)) {
        print(variable)
        val <- NA
        exp <- FALSE
      }
      else {
        if(commented) {
          val <- NA
          exp <- FALSE
        } else {
          val <- template.defaults[[variable]]
          exp <- FALSE
          ## This is a date format YYYY-MM-DD
          if (nchar(val)==10 && grepl("-", val)) {
            test <- strsplit(val, split = "-", fixed = TRUE)
            if (nchar(test[[1]][1])==4 && grepl("^[0-9]+$", test[[1]][1])) {
              val <- as.Date(val);
            }
          }
          else {
            if (nchar(val)==5 && grepl("-", val)) {

            }
            else {
              if(substr(as.character(val)[1], 1, 1) %in% as.character(0:9)){
                val <- as.numeric(val)
              }
            }
          }
        }
      }

      if(!("list" %in% class(val))) {
        val <- rep(list(val), n.sims)
      } else {
        val <- rep(val, n.sims)
      }
    }
    out <- list(value = val, exp.plan = exp)
    return(out)
  }


  TEMPLATE_PARAMS <- get_template_params(hip$template)
  PARAM_NAMES     <- get_param_names(TEMPLATE_PARAMS)
  PARAM_DEFAULTS  <- get_param_vals(TEMPLATE_PARAMS, "value")
  PARAM_COMMENTED <- get_param_vals(TEMPLATE_PARAMS, "commented")
  USED_PARAMS <- purrr::map(as.list(unlist(PARAM_NAMES, use.names = FALSE)),
                            get_used_param,
                            exp.plan           = dplyr::mutate_all(hip$exp.plan, as.list),
                            template.defaults  = PARAM_DEFAULTS,
                            template.commented = PARAM_COMMENTED)
  names(USED_PARAMS) <- unlist(PARAM_NAMES, use.names = FALSE)
  return(USED_PARAMS)
}
