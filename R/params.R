#' Reads parameter files
#' @description Reads parameter files for editing
#' Used within \code{\link{build_structure}}.
#' @return A list containing all parameter values and constraints.
#' @param path A character string of the path to the file to read.
#' @importFrom dplyr %>%
#' @family hisafe param functions
read_param_file <- function(path) {
  sim <- scan(file = path, what = "character", encoding = "latin1", sep = "\n", quiet = TRUE)

  titles        <- grepl("##", substr(sim, 1, 2))         # which lines have are headers
  single.hash   <- grepl("#",  substr(sim, 1, 1))          # which lines are commented out
  has.equals    <- grepl(" = ", sim)
  pld.tabledata <- grepl("Layer", sim) | grepl("LayerInit", sim) | grepl("TreeInit", sim) | grepl("RootInit", sim)
  notes         <- single.hash & !titles & !has.equals & !pld.tabledata # which lines are notes
  comment       <- substr(sim, 1, 1) == "#"

  read_element_table <- function(sim, i, titles, table.names) {
    if(any(which(titles) > i)) {
      next.header <- which(titles)[which(titles) > i][[1]]
      table.elements <- strsplit(remove_whitespace(sim[i:(next.header - 1)]), split = "\t")
    } else {
      table.elements <- strsplit(sim[i], split = "\t")
    }

    clean_elements <- function(x, table.names) {
      data.frame(t(matrix(x)))
      names(x) <- table.names
      return(x)
    }

    table.tibble <- purrr::map(table.elements, clean_elements, table.names = table.names) %>%
      purrr::map_df(dplyr::bind_rows) %>%
      readr::type_convert(col_types = readr::cols())

    return(table.tibble)
  }

  tables <- list(layers                      = c("name", "thickness", "sand", "clay", "limeStone", "organicMatter",                             # .SIM
                                                 "partSizeSand", "stone", "stoneType", "infiltrability"),
                 layer_initialization        = c("name", "waterContent", "no3Concentration", "nh4concentration"),                               # .SIM
                 tree_initialization         = c("name", "species", "age", "height", "crownBaseHeight", "truncatureRatio",                      # .SIM
                                                 "leafToFineRootsRatio", "crownRadius", "treeX", "treeY"),
                 root_initialization         = c("name", "shape", "repartition", "paramShape1", "paramShape2", "paramShape3", "amount"),        # .SIM
                 residue_incorporation_table = c("julres", "coderes", "P_qres", "P_Crespc", "P_CsurNres", "P_Nminres", "P_eaures"),             # .TEC
                 tillage_table               = c("jultrav", "profres", "proftrav"),                                                             # .TEC
                 irrigation_table            = c("julapl", "qte"),                                                                              # .TEC
                 fertilization_table         = c("julapN", "qte"),                                                                              # .TEC
                 cutting_table               = c("julfauche", "hautcoupe", "lairesiduel", "msresiduel", "anitcoupe"),                           # .TEC
                 fertilization_parameters    = c("engamm", "orgeng", "deneng", "voleng"),                                                       # hisafe.par
                 residue_parameters          = c("CroCo", "akres", "bkres", "awb", "bwb", "cwb", "ahres", "bhres", "kbio", "yres", "CNresmin",  # hisafe.par
                                                 "CNresmax", "qmulchruis0", "mouillabilmulch", "kcouvmlch", "albedomulchresidus", "Qmulchdec"))
  new.sim <- list()
  next_threshold <- 0
  list.title <- "temp"
  for(i in 1:length(sim)) {
    if(i < next_threshold) next
    if(notes[i]) next
    if(titles[i]){
      list.title <- gsub(pattern = " ",
                         replacement = "_",
                         remove_whitespace(gsub(pattern = "#", replacement = "", sim[i]))) #on enleve les tabs, leading et trailing blanks
      toto <- list(c())
      names(toto) <- list.title
      new.sim <- c(new.sim, toto)
    } else if(tolower(list.title) %in% names(tables)){
      list.names <- tables[[tolower(list.title)]]
      element.table <- read_element_table(sim, i, titles, list.names)
      element.table.reduced <- element.table[!grepl("#", element.table[[1]]),]
      if(nrow(element.table.reduced) > 0) {
        toto <- list(list(value = list(element.table), commented = FALSE, range = NA, type = NA, accepted = NA))
      } else {
        toto <- list(list(value = list(element.table), commented = TRUE, range = NA, type = NA, accepted = NA))
      }
      names(toto) <- gsub("_", ".", names(tables)[names(tables) == tolower(list.title)])
      new.sim[[list.title]] <- c(new.sim[[list.title]], toto)
      next_threshold <- i + nrow(element.table)
    } else {
      line.text <- ifelse(comment[i], substr(sim[i], start = 2, stop = 10000), sim[i]) # remove first # only (definitions possible after another #)

      element.name  <- unlist(lapply(strsplit(line.text, split = "=", fixed = TRUE), "[[", 1))
      element.name  <- remove_whitespace(element.name) # remove tabs, leading, and trailing blanks

      element.vals  <- purrr::map_chr(strsplit(line.text, split = "=", fixed = TRUE), 2)
      element.vals  <- remove_whitespace(strsplit(element.vals, split = "#", fixed = TRUE)[[1]])

      element.value <- element.vals[1]
      if(grepl(",", element.value)) {
        element.value <- strsplit(element.value, split = ",")
        if(substr(element.value[[1]][1], 1, 1) %in% as.character(0:9)) {
          element.value <- purrr::map(element.value, as.numeric)
        }
      } else {
        if(substr(element.value, 1, 1) %in% as.character(0:9)) {
          element.value <- as.numeric(element.value)
        }
      }

      toto <- list(list(value     = element.value,
                        commented = comment[i]))

      names(toto) <- element.name
      new.sim[[list.title]] <- c(new.sim[[list.title]], toto)
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
          comment.sign <- ifelse(param.list[[i]][[j]]$commented, "#", "")
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

#' Edits sim, pld, and tree files
#' @description Edits parameter files using the exp.plan of a hip object
#' Used within \code{\link{build_structure}}.
#' @return A edited list containing all parameter values and constraints.
#' @param param.list A list containing all parameter values and constraints.
#' @param exp.plan A exp.plan element of a hip object.
#' @family hisafe param functions
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
#' @param template.path A character string of the path to the directory containing the template set of Hi-sAFe simulation folders/files to use.
get_template_params <- function(template.path) {

  ## Determine which tree species to use from within the template for the .tree params
  avail.template.trees <- unlist(purrr::map(strsplit(list.files(clean_path(paste0(template.path, "/treeSpecies"))), split = ".", fixed = TRUE), 1))
  if(length(avail.template.trees) == 1) {
    template.tree <- avail.template.trees
  } else if("walnut-hybrid" %in% avail.template.trees) {
    template.tree <- "walnut-hybrid"
  } else {
    template.tree <- avail.template.trees[1]
  }

  sim.file    <- clean_path(list.files(template.path, ".sim", full.names = TRUE))
  pld.file    <- clean_path(list.files(template.path, ".pld", full.names = TRUE))
  tree.file   <- list.files(paste0(template.path, "treeSpecies"), paste0(template.tree, ".tree"), full.names = TRUE)
  hisafe.file <- clean_path(paste0(template.path, "generalParameters/hisafe.par"))
  stics.file  <- clean_path(paste0(template.path, "generalParameters/stics.par"))

  sim.params    <- read_param_file(sim.file)
  pld.params    <- read_param_file(pld.file)
  tree.params   <- read_param_file(tree.file)
  hisafe.params <- read_param_file(hisafe.file)
  stics.params  <- read_param_file(stics.file)
  return(list(sim = sim.params, pld = pld.params, tree = tree.params, hisafe = hisafe.params, stics = stics.params))
}

#' Get names of template parameters
#' @description Get names of all template parameters
#' @return A list containing parameter names by file type.
#' @param x A list containing all parameter values and constraints.
get_param_names <- function(x) {
  sim.names    <- unlist(purrr::map(x$sim,  names), use.names = FALSE)
  pld.names    <- unlist(purrr::map(x$pld,  names), use.names = FALSE)
  tree.names   <- unlist(purrr::map(x$tree, names), use.names = FALSE)
  hisafe.names <- unlist(purrr::map(x$hisafe, names), use.names = FALSE)
  stics.names  <- unlist(purrr::map(x$stics, names), use.names = FALSE)
  return(list(sim = sim.names, pld = pld.names, tree = tree.names, hisafe = hisafe.names, stics = stics.names))
}

#' Get values/constraints of template parameters
#' @description Gets values/constraints of all template parameters
#' @return A list containing parameter values/constraints.
#' @param x A list containing all parameter values and constraints.
get_param_vals <- function(x, type) {
  sim.vals <- pld.vals <- tree.vals <- hisafe.vals <- stics.vals <- list()
  for(i in names(x$sim))    { sim.vals    <- c(sim.vals,    purrr::map(x$sim[[i]],    type)) }
  for(i in names(x$pld))    { pld.vals    <- c(pld.vals,    purrr::map(x$pld[[i]],    type)) }
  for(i in names(x$tree))   { tree.vals   <- c(tree.vals,   purrr::map(x$tree[[i]],   type)) }
  for(i in names(x$hisafe)) { hisafe.vals <- c(hisafe.vals, purrr::map(x$hisafe[[i]], type)) }
  for(i in names(x$stics))  { stics.vals  <- c(stics.vals,  purrr::map(x$stics[[i]],  type)) }
  return(c(sim.vals, pld.vals, tree.vals, hisafe.vals, stics.vals))
}

#' Complies list of parameters actually used
#' @description Compiles list of the parameters actually used (i.e. the default or the defined)
#' @return List of used parameter values
#' @param hip A "hip" object containing only a single simulation
get_used_params <- function(hip) {
  get_used_param <- function(variable, exp.plan, template.defaults, template.commented){
    n.sims <- nrow(exp.plan)
    if(variable %in% names(exp.plan)){
      val <- exp.plan[[variable]]
      exp <- TRUE
    } else {
      commented <- template.commented[[variable]]
      if(commented) {
        val <- NA
        exp <- FALSE
      } else {
        val <- template.defaults[[variable]]
        exp <- FALSE
        if(substr(as.character(val)[1], 1, 1) %in% as.character(0:9)){
          val <- as.numeric(val)
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

  TEMPLATE_PARAMS <- get_template_params(get_template_path(hip$template))
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
