#' Format Water Tracker data
#'
#' Estimates the observation date associated with each image mosaic and assigns
#' corresponding values for year, water year, and month.
#'
#' @details For each unique wetland management unit, identifies and removes
#'   likely pseudoreplicate data from sequential observations with identical
#'   values that may represent the same image in multiple mosaics. Estimates the
#'   actual observation date as the midpoint between mosaic start and end dates,
#'   unless the next observation is a likely pseudoreplicate, in which case the
#'   date is estimated as the midpoint of the overlapping date range between the
#'   two observations. Also recalculate PercentWater as ObservedAreaWaterHa /
#'   ObservedAreaHa * 100 (to ensure none are >100%).
#'
#'   Wrapper function for [find_duplicates()], [estimate_obsdate()], and
#'   [add_date_details()]. Expects standard output from Water Tracker data with
#'   fields MosaicDateStart, MosaicDateEnd, ObservedAreaHa, and PercentWater.
#'
#' @param df Input data frame from water tracker
#' @param unitID Character string for the field containing unique values for
#'   each of the management units for which hydrological data will be evaluated;
#'   default is "MU"
#'
#' @return tibble with added fields unit, wateryear, year, month, month_name
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' data(sampledat)
#' format_watertracker(sampledat)
#'
format_watertracker = function(df, unitID = 'MU') {
  df |> dplyr::select(unit = unitID, dplyr::everything()) |>
    dplyr::group_by(.data$unit) |> find_duplicates() |> estimate_obsdate() |>
    dplyr::ungroup() |>
    dplyr::filter(!.data$dup) |> # exclude duplicate rows
    dplyr::select(.data$unit, .data$obsdate, dplyr::everything()) |>
    dplyr::select(-.data$dup, -.data$overlap_days) |>
    add_date_details()
}

