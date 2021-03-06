% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{hop_filter}
\alias{hop_filter}
\title{Filter a hop object by SimulationName, Date, and idTree}
\usage{
hop_filter(
  hop,
  simu.names = "all",
  tree.ids = "all",
  years = "all",
  months = "all",
  date.min = NA,
  date.max = NA,
  dates = NULL,
  strip.exp.plan = FALSE
)
}
\arguments{
\item{hop}{An object of class hop or face.}

\item{simu.names}{A character vector of the SimulationNames to keep. If "all", no filtering occurs.}

\item{tree.ids}{A numeric vector of the tree ids to keep. If "all", no filtering occurs.}

\item{years}{A numeric vector of the years to keep. If "all", no filtering occurs.}

\item{months}{A numeric vector of the months to keep. If "all", no filtering occurs.}

\item{date.min}{A character string of the minimum date to keep, in the format "YYYY-MM-DD" or of class Date.
If NA, the minimum date in \code{hop} is used. Only used if \code{dates} is \code{NULL}.}

\item{date.max}{A character string of the maximum date to keep, in the format "YYYY-MM-DD" or of class Date.
If NA, the maximum date in \code{hop} is used. Only used if \code{dates} is \code{NULL}.}

\item{dates}{A character vector (in the format "YYYY-MM-DD") or a vector of class Date of the dates to keep.
If \code{NULL}, then \code{date.max} and \code{date.min} are used instad.}

\item{strip.exp.plan}{Logical indicating whether or not to remove the exp.plan variables
(which are appended by \code{\link{read_hisafe}}) from each profile in \code{hop}.}
}
\value{
A hop object.
}
\description{
Filters a hop object by SimulationName, Date, and idTree
}
\examples{
\dontrun{
newhop <- hop_filter(myhop, c("Sim_1", "Sim_2"))
}
}
\seealso{
Other hisafe helper functions: 
\code{\link{copy_hisafe_template}()},
\code{\link{get_pheno_dates}()},
\code{\link{get_pruning_dates}()},
\code{\link{hip_params}()},
\code{\link{hisafe_info}()},
\code{\link{hisafe_profiles}()},
\code{\link{hop_merge}()},
\code{\link{hop_params}()},
\code{\link{hop_rename}()},
\code{\link{is_face}()},
\code{\link{is_hip}()},
\code{\link{is_hop}()},
\code{\link{join_profiles}()},
\code{\link{make_rel_years}()},
\code{\link{profile_check}()},
\code{\link{variable_check}()},
\code{\link{which_profiles}()}
}
\concept{hisafe helper functions}
