% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_flood_extent.R
\name{estimate_flood_extent}
\alias{estimate_flood_extent}
\title{Estimate flooding extent}
\usage{
estimate_flood_extent(
  df,
  prob = 0.95,
  full_prop_threshold = 0.55,
  wet_prop_threshold = 0.2
)
}
\arguments{
\item{df}{Input tibble from \code{\link[=format_watertracker]{format_watertracker()}}.}

\item{prob}{Single numeric value passed to \code{\link[=quantile]{quantile()}}; see Details.}

\item{full_prop_threshold}{Numeric value (0-1); see Details.}

\item{wet_prop_threshold}{Numeric value (0-1); see Details.}
}
\value{
tibble with added fields: \code{ObservedAreaWater_pq},
\code{ObservedAreaWater_adjust}, \code{flood_prop}, and \code{flood_status}
}
\description{
Estimates the typical maximum extent of the open water footprint in each unit
as a percentile of all observations in the dataset, helping to correct for
upland areas that may be included within each unit that are rarely, if ever,
flooded.
}
\details{
For each unit, calculates \code{ObservedAreaWater_pq} as a percentile
value, as determined by \code{prob}, of all \code{ObservedAreaWaterHa} in the dataset
(as provided in the raw Water Tracker data). This value is intended to
represent the typical upper limit of wetted area, to account for upland
areas in the unit that are not typically flooded as part of regular
management.

The value for \code{prob} should typically be set less than 1 to allow for
outliers resulting from occasional extreme flooding events, such as driven
by precipitation. (Note that providing multiple values of \code{prob}, while
supported by the \code{\link[=quantile]{quantile()}} function, is not supported by this function.)

The \code{ObservedAreaWater_adjust} is then calculated to reflect this upper
limit of wetted area, such that values of \code{ObservedAreaWaterHa} that exceed
\code{ObservedAreaWater_pq} are reduced to this value. Likewise, \code{flood_prop} is
calculated as the proportion of observed \code{ObservedAreaWater_adjust} on each
date relative to this upper limit.

Finally a categorical \code{flood_status} is assigned to each observation date as:
\itemize{
\item \code{full}: \code{flood_prop} >= \code{full_prop_threshold}
\item \code{wet}: \code{wet_prop_threshold} <= \code{flood_prop} < \code{full_prop_threshold},
\item \code{trace}: \code{flood_prop} > 0 but < \verb{wet prop threshold}
\item \code{dry}: \code{flood_prop} = 0
}
}
\examples{
df = format_watertracker(sampledat)
estimate_flood_extent(df)
}
