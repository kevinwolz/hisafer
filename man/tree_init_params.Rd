% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/define.R
\name{tree_init_params}
\alias{tree_init_params}
\title{Generate tree initialization table for define_hisafe}
\usage{
tree_init_params(template, ...)
}
\arguments{
\item{template}{A character string of the path to the directory containing the template set of Hi-sAFe simulation
folders/files to use.
See \code{\link{define_hisafe}} for more details.}

\item{...}{Any parameters of Hi-sAFe tree initialization table:
 \itemize{
 \item{"species"}{}
 \item{"height"}{}
 \item{"crownBaseHeight"}{}
 \item{"crownRadius"}{}
 \item{"treeX"}{}
 \item{"treeY"}{}
}}
}
\value{
A list containing a Hi-sAFE tree initialization table.
}
\description{
Generates a tree initialization table suitable for passing to \code{\link{define_hisafe}}.
The output of this function is always passed to \code{\link{define_hisafe}} via the \code{tree.initialization} argument
(see example below). Any passed parameters modify the table in the provided template.
}
\examples{
\dontrun{
hip <- define_hisafe(path = getwd(), template = "agroforestry",
                    tree.initialization = tree_init_params(template = "agroforestry",
                                                           height   = 2))
}
}
\seealso{
Other hisafe definition functions: 
\code{\link{define_hisafe_file}()},
\code{\link{define_hisafe}()},
\code{\link{layer_init_params}()},
\code{\link{layer_params}()},
\code{\link{root_init_params}()},
\code{\link{variety_params}()}
}
\concept{hisafe definition functions}
