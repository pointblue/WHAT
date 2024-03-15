#' Sample Water Tracker data for demonstrating package functions
#'
#' @description Sample hydrological data for wetland complexes in the Central
#'   Valley. This data set includes data extracted from Point Blue'
#'   sWaterTracker from two wetland complexes:
#'
#'   "SampleWetland1" includes 9 individual units, of which all are expected to
#'   be "managed" wetlands
#'
#'   "SampleWetland2" contains 8 units, and all but unit 8 are expected to be
#'   "managed".
#'
#'
#' @format A tibble with 7803 rows and 13 columns
#' \describe{
#'    \item{WETLAND}{Name of sample wetland complex}
#'    \item{MU}{Unique ID of sample wetland management unit}
#'    \item{CLASS}{Predicted status as "managed" wetland or "other"}
#'    \item{AREA_HA}{Polygon area in hectares}
#'    \item{AREA_AC}{Polygon area in acres}
#'    \item{Mosaic}{Unique ID for the mosaic of satellite images included}
#'    \item{MosaicDateStart}{Earliest date of satellite imagery mosaic}
#'    \item{MosaicDateEnd}{Latest date of satelliate imagery mosaic}
#'    \item{ObservedArea}{Total area (m^2) for which flooding status could be interpreted}
#'    \item{ObservedAreaWater}{Total area (m^2) estimated to be flooded}
#'    \item{PercentObserved}{Percent of total area for which flooding status could be interpreted}
#'    \item{PercentWater}{Percent of observed area estimated to be flooded}
#'    \item{EstimatedAreaWAter}{?}
#'    \item{Threshold}{?}
#' }
#'
"sampledat"
