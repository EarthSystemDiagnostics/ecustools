% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReadFraile.R
\name{ReadFraile}
\alias{ReadFraile}
\alias{read_fraile}
\title{ReadFraile}
\source{
copied from paleolibrary/src/single_functions/speclib/read.fraile.R
and updated to use \code{ncdf4}
}
\usage{
ReadFraile(FILENAME, varname)
}
\arguments{
\item{FILENAME}{character, path to netcdf file}

\item{varname}{character, the variable to be read}
}
\value{
a \code{\link[pfields]{pField}} object
}
\description{
Reads the biogeochemical dataset from Igaraza Faile, Monthly data varname
e.g. bulloides or ruber... to get the list, leave it empty
}
\examples{
\dontrun{
ReadFrail("path/to/file.nc",
                  varname = "var.name")
}
}
\seealso{
\url{https://github.com/EarthSystemDiagnostics/pfields}
}
\author{
Thomas Laepple
}
