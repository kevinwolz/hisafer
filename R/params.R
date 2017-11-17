#' Reads sim, pld, and tree files
#' @description Reads sim, pld, and tree files for editing
#' Used within \code{\link{build_structure}}.
#' @return A list containing all parameter values and constraints.
#' @param path A character string of the path to the file to read.
#' @export
#' @importFrom dplyr %>%
#' @family hisafe param functions
read_param_file <- function(path) {
  sim <- scan(file = path, what = "character", encoding = "latin1", sep = "\n", quiet = TRUE)

  titles    <- grepl("##", substr(sim, 1, 2))    # which lines have are headers
  withdata  <- grepl(" = ", sim)                 # which lines have data
  tabledata <- grepl("Layer", sim) | grepl("LayerInit", sim) | grepl("TreeInit", sim) | grepl("RootInit", sim)
  withdata  <- withdata | tabledata
  notes     <- !(titles | withdata)              # lines which are neither headers nor have data are notes
  comment  <- substr(sim, 1, 1) == "#"           # which lines are commented out

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

  new.sim <- list()
  next_threshold <- 0
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
    } else if(tolower(list.title) == "layers"){
      NAMES <- c("name", "thickness", "sand", "clay", "limeStone", "organicMatter", "partSizeSand", "stone", "stoneType", "infiltrability")
      element.table <- read_element_table(sim, i, titles, NAMES)
      toto <- list(list(value = element.table, commented = FALSE, range = NA, accepted = NA))
      names(toto) <- "layers"
      new.sim[[list.title]] <- c(new.sim[[list.title]], toto)
      next_threshold <- i + nrow(element.table)
    } else if(tolower(list.title) == "layer_initialization"){
      NAMES <- c("name", "waterContent", "no3Concentration", "nh4concentration")
      element.table <- read_element_table(sim, i, titles, NAMES)
      toto <- list(list(value = element.table, commented = FALSE, range = NA, accepted = NA))
      names(toto) <- "layer.initialization"
      new.sim[[list.title]] <- c(new.sim[[list.title]], toto)
      next_threshold <- i + nrow(element.table)
    } else if(tolower(list.title) == "tree_initialization"){
      NAMES <- c("name", "species", "age", "height", "crownBaseHeight", "truncatureRatio", "leafToFineRootsRatio", "crownRadius", "treeX", "treeY")
      element.table <- read_element_table(sim, i, titles, NAMES)
      toto <- list(list(value = element.table, commented = FALSE, range = NA, accepted = NA))
      names(toto) <- "tree.initialization"
      new.sim[[list.title]] <- c(new.sim[[list.title]], toto)
      next_threshold <- i + nrow(element.table)
    } else if(tolower(list.title) == "root_initialization") {
      NAMES <- c("name", "shape", "repartition", "paramShape1", "paramShape2", "paramShape3", "amount")
      element.table <- read_element_table(sim, i, titles, NAMES)
      toto <- list(list(value = element.table, commented = FALSE, range = NA, accepted = NA))
      names(toto) <- "root.initialization"
      new.sim[[list.title]] <- c(new.sim[[list.title]], toto)
      next_threshold <- i + nrow(element.table)
    } else {
      line.text <- ifelse(comment[i], substr(sim[i], start = 2, stop = 10000), sim[i]) # remove first # only (definitions possible after another #)

      element.name  <- unlist(lapply(strsplit(line.text, split = "=", fixed = TRUE), "[[", 1))
      element.name  <- remove_whitespace(element.name) # remove tabs, leading, and trailing blanks

      element.vals    <- purrr::map_chr(strsplit(line.text, split = "=", fixed = TRUE), 2)
      element.vals    <- remove_whitespace(strsplit(element.vals, split = "#", fixed = TRUE)[[1]])

      element.value   <- element.vals[1]
      element.value   <- strsplit(element.value, split = ",")[[1]]

      if(length(element.vals) > 1) {
        value.range <- element.vals[2]

        if(grepl("(", value.range, fixed = TRUE)) {
          value.type <- "continuous"
          } else if(grepl("[", value.range, fixed = TRUE)) {
            value.type <- "integer"
          } else {
            value.type <- NA
          }

        if(value.range == "NA") {
          value.range <- NA
        } else {
          value.range <- strsplit(gsub("\\[|\\]|\\(|\\)", "", value.range), split = ",")[[1]]
          value.range[value.range == "NA"] <- NA
          value.range <- as.numeric(value.range)
        }

        value.accepted <- element.vals[3]
        if(value.accepted == "NA") {
            value.accepted <- NA
          } else {
            value.accepted <- strsplit(gsub("\\[|\\]|\\(|\\)", "", value.accepted), split = ",")[[1]]
            value.accepted[value.accepted == "NA"] <- NA
          }
      } else {
        value.range    <- c(NA, NA)
        value.type     <- NA
        value.accepted <- NA
      }

      toto <- list(list(value     = element.value,
                        commented = comment[i],
                        range     = value.range,
                        type      = value.type,
                        accepted  = value.accepted))

      names(toto) <- element.name
      new.sim[[list.title]] <- c(new.sim[[list.title]], toto)
    }
  }
  return(new.sim)
}

#' Writes sim, pld, and tree files
#' @description Writes sim, pld, and tree files after editing
#' Used within \code{\link{build_structure}}.
#' @return Invisibly returns \code{TRUE}.
#' @param.list A list containing all parameter values and constraints.
#' @param path A character string of the path to the file to read.
#' @export
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
        if(!is.data.frame(param.list[[i]][[j]]$value)){ # is this a normal variable or a table of variables
          comment.sign <- ifelse(param.list[[i]][[j]]$commented, "#", "")
          sim.out <- paste(sim.out, paste0(comment.sign,
                                           names(param.list[[i]])[j],
                                           " = ",
                                           paste0(param.list[[i]][[j]]$value, collapse = ",")),
                           sep = "\n")
        } else {
          table_out <- function(x, sim.out) {
            sim.out <- paste(sim.out, paste0("#", paste(names(x), collapse = "\t")), sep = "\n")
            for(i in 1:nrow(x)) {
              sim.out <- paste(sim.out, paste(as.matrix(x[i,]), collapse = "\t"), sep = "\n")
            }
            return(sim.out)
          }

          char.table <- dplyr::as_tibble(param.list[[i]][[j]]$value) %>%
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
#' @description Edits sim, pld, and tree files using the exp.plan of a hip object
#' Used within \code{\link{build_structure}}.
#' @return A edited list containing all parameter values and constraints.
#' @param param.list A list containing all parameter values and constraints.
#' @param exp.plan A exp.plan element of a hip object.
#' @export
#' @family hisafe param functions
edit_param_file <- function(param.list, exp.plan) {
  for(i in names(exp.plan)){
    if(is.list(exp.plan[[i]])) {
      value <- exp.plan[[i]][[1]]
    } else {
      value <- exp.plan[[i]]
    }
    param.list <- edit_param_element(param.list, i, value)
  }
  return(param.list)
}

#' Edit an individual parameter element
#' @description Edits an individual element of a sim, pld, or tree parameter list
#' Used within \code{\link{build_structure}}.
#' @return A edited list containing all parameter values and constraints.
#' @param param.list A list containing all parameter values and constraints.
#' @param variable A character string of the name of the variable to edit.
#' @param value A numeric vector of the value to apply to the variable.
#' @export
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

#' Read all template parameters from sim, pld, and tree files
#' @description Reads all template parameter values and constraints from sim, pld, and tree files
#' @return A list containing all parameter values and constraints.
#' @param template A character string of the path to the directory containing the template set of Hi-sAFe simulation folders/files to use.
get_template_params <- function(template) {
  if(template == "default") template <- clean_path(paste0(system.file("extdata", "hisafe_template", package = "hisafer"), "/"))

  avail.template.trees <- unlist(purrr::map(strsplit(list.files(paste0(template, "treeSpecies")), split = ".", fixed = TRUE), 1))
  if(length(avail.template.trees) == 1) {
    template.tree <- avail.template.trees
  } else if("walnut-hybrid" %in% avail.template.trees) {
    template.tree <- "walnut-hybrid"
  } else {
    template.tree <- avail.template.trees[1]
  }

  sim.file  <- clean_path(list.files(template, ".sim", full.names = TRUE))
  pld.file  <- list.files(clean_path(paste0(template, "/plotDescription")), ".pld", full.names = TRUE)
  tree.file <- list.files(clean_path(paste0(template, "/treeSpecies")), paste0(template.tree, ".tree"), full.names = TRUE)

  sim.params  <- read_param_file(sim.file)
  pld.params  <- read_param_file(pld.file)
  tree.params <- read_param_file(tree.file)

  return(list(sim = sim.params, pld = pld.params, tree = tree.params))
}

#' Get names of template parameters from sim, pld, and tree files
#' @description Get names of all template parameters from sim, pld, and tree files
#' @return A list containing parameter names by file type.
#' @param x A list containing all parameter values and constraints.
get_param_names <- function(x) {
  sim.names  <- unlist(purrr::map(x$sim,  names), use.names = FALSE)
  pld.names  <- unlist(purrr::map(x$pld,  names), use.names = FALSE)
  tree.names <- unlist(purrr::map(x$tree, names), use.names = FALSE)
  return(list(sim = sim.names, pld = pld.names, tree = tree.names))
}

#' Get values/constraints of template parameters from sim, pld, and tree files
#' @description Gets values/constraints of all template parameters from sim, pld, and tree files
#' @return A list containing parameter values/constraints.
#' @param x A list containing all parameter values and constraints.
get_param_vals <- function(x, type) {
  sim.vals <- pld.vals <- tree.vals <- list()
  for(i in names(x$sim))  { sim.vals  <- c(sim.vals,  purrr::map(x$sim[[i]],  type)) }
  for(i in names(x$pld))  { pld.vals  <- c(pld.vals,  purrr::map(x$pld[[i]],  type)) }
  for(i in names(x$tree)) { tree.vals <- c(tree.vals, purrr::map(x$tree[[i]], type)) }
  return(c(sim.vals, pld.vals, tree.vals))
}

#' Gets the parameter actually used
#' @description Gets the parameter actually used (i.e. the default or the defined)
#' @return The value of the parameter
#' @param variable A character string of the name of the variable to check.
#' @param hip A "hip" object
#' @param template.defaults A list containing the default values for all parameters.
get_used_param <- function(variable, exp.plan, template.defaults, template.commented) {
  if(variable %in% names(exp.plan)){
    val <- exp.plan[[variable]]
    exp <- TRUE
    if("list" %in% class(val)) {
      val <- val[[1]]
    }
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
  }
  out <- list(value = val, exp.plan = exp)
  return(out)
}
