#' Estimate change in flooding
#'
#' Interprets sequence of Water Tracker data in each unit to estimate whether
#' the change in the extent of flooding from the previous observation represents
#' a substantial increase or decrease.
#'
#' @details For each unit, the proportion change in flooded area from the
#'   previous observation date is calculated (`flood_delta`) and the direction
#'   and magnitude of change is categorized (`flood_trend`) based on the input
#'   value for `delta` provided, as:
#'   * `INCREASE`:  `flood_delta` >= `delta`
#'   * `up`: `delta` > `flood_delta` > 0
#'   * `0`: `flood_delta` = 0
#'   * `down`: 0 > `flood_delta` > -`delta`
#'   * `DECREASE`: -`delta` >= `flood_delta`
#'
#' @param df Input tibble from [estimate_flood_extent()].
#' @param delta Numeric proportion change representing significant increase or
#'   decrease in flooding.
#'
#' @return tibble with added fields: `flood_delta` and `flood_trend`
#' @export
#' @importFrom rlang .data
#' @importFrom purrr map_df
#' @examples
#' df = format_watertracker(sampledat) |> estimate_flood_extent()
#' estimate_flood_delta(df)


estimate_flood_delta = function(df, delta = 0.2) {
  df |>
    split(df$unit) |>
    purrr::map_df(
      function(x) {
        x |>
          dplyr::mutate(
            flood_delta = (.data$ObservedAreaWater_adjust - dplyr::lag(.data$ObservedAreaWater_adjust, 1))/.data$ObservedAreaWaterHa_pq,
            flood_trend = dplyr::case_when(flood_delta <= -delta ~ 'DECREASE',
                                           flood_delta < 0 ~ 'down',
                                           flood_delta >= delta ~ 'INCREASE',
                                           flood_delta > 0 ~ 'up',
                                           TRUE ~ '0'))
      })
}
