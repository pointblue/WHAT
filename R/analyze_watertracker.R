#' Analyze Water Tracker data
#'
#' Interprets sequence of Water Tracker data in each unit to interpret flooding
#' status and assign monthly water management modes for use with the `Wetland
#' Water Budget Tool` a simple spread-sheet based tool allowing non-specialists
#' to estimate wetland water budgets from a combination of management unit size
#' and schedule of monthly operational modes.
#'
#' @details Wrapper function that calls [estimate_floodstatus()] and
#'   [estimate_wetlandmode()]. See details of each of those functions for more
#'   information.
#'
#' @param df tibble resulting from [format_watertracker()]
#' @param prob Single numberic value passed to [quantile()]; see Details in
#'   [estimate_floodstatus()].
#' @param delta Numeric proportion change representing significant increase or decrease in flooding.
#' @param full_prop_threshold Numeric value (0-1), see Details in
#'   [estimate_floodstatus()].
#' @param wet_prop_threshold Numeric value (0-1), see Details in
#'   [estimate_floodstatus()].
#' @param fullmode One of `M` or `H`; default operational mode for units
#'   considered fully flooded; see [estimate_wetlandmode()] for details.
#'
#' @return tibble with fields: `unit`, `wateryear`, `month`, `month_name`,
#'   `year`, `PercentWater` (the maximum observed in the given month and year),
#'   `PercentWater_max` (the maximum observed over all dates in the dataset),
#'   `flood_prop` (`PercentWater`/`PercentWater_max`), and `flood_status`.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' #format_watertracker(sampledat) |> analyze_watertracker()
#'
analyze_watertracker = function(df, prob = 0.95,
                                delta = 0.2,
                                full_prop_threshold = 0.55,
                                wet_prop_threshold = 0.2,
                                fullmode = 'M') {

  df |>
    estimate_floodstatus(prob, delta = delta,
                         full_prop_threshold = full_prop_threshold,
                         wet_prop_threshold = wet_prop_threshold) |>
    estimate_wetlandmode(fullmode = fullmode)
}
