#' Estimate flooding status
#'
#' Internal to [analyze_watertracker()]. Not meant to be called separately.
#'
#' @details For each unit, this function first calculates `ObservedAreaWater_pq`
#'   as a percentile value of 'ObservedAreaWaterHa' (provided in the raw Water
#'   Tracker data) over all observation dates in the dataset, as determined by
#'   `prob`. This value is intended to represents the typical upper limit of
#'   wetted area, to account for upland areas in the unit that are not typically
#'   intentionally flooded as part of regular manaagement. The value for `prob`
#'   should typically be set less than 1 to allow for outliers with extreme
#'   flooding events. Note that providing multiple values of `prob`, while
#'   supported by the [quantile()] function, is not supported by this function.
#'
#'   The `ObservedAreaWater_adjust` is then calculated to reflect this upper
#'   limit of wetted area, such that values of `ObservedAreaWaterHa` that exceed
#'   the are reduced to this value. Likewise, `flood_prop` is calculated as the
#'   proportion of `ObservedAreaWater_adjust` relative to this upper limit.
#'
#'   The proportion change in flooded area from the previous observation date is
#'   then calculated (`flood_delta`) and categorized (`flood_trend`) by
#'   direction and magnitude of change, based on the input value for `delta`
#'   provided, as:
#'   * `+`:  `flood_delta` >= `delta`
#'   * `(+)`: `delta` > `flood_delta` > 0
#'   * `0`: `flood_delta` = 0
#'   * `(-)`: 0 > `flood_delta` > -`delta`
#'   * `-`: -`delta` >= `flood_delta`
#'
#'   Finally the `flood_status` on each observation date is then designated as:
#'   * `full`: `flood_prop` >= `full_prop_threshold`
#'   * `wet`: `wet_prop_threshold` <= `flood_prop` < `full_prop_threshold`,
#'   * `trace`: `flood_prop` > 0 but < `wet prop threshold`
#'   * `dry`: `flood_prop` = 0
#'
#' @param df Input tibble from [format_watertracker()].
#' @param prob Numeric value passed to [quantile()]; see Details.
#' @param delta Numeric proportion change representing significant increase or
#'   decrease in flooding.
#' @param full_prop_threshold Numeric value (0-1); see Details.
#' @param wet_prop_threshold Numeric value (0-1); see Details.
#'
#' @return tibble with added fields: `ObservedAreaWater_pq`,
#'   `ObservedAreaWater_adjust`, `flood_delta`, `flood_trend`, `flood_prop`, and
#'   `flood_status`
#' @importFrom rlang .data
#' @importFrom purrr map_df


estimate_floodstatus = function(df, prob, delta,
                                full_prop_threshold,
                                wet_prop_threshold) {
  df |>
    split(df$unit) |>
    purrr::map_df(
      function(x) {
        x |>
          dplyr::filter(.data$ObservedPixels > 0) |> #drop dates with no data
          dplyr::mutate(
            ObservedAreaWaterHa_pq = stats::quantile(.data$ObservedAreaWaterHa, prob, na.rm = TRUE),
            # change from previous observation in that unit
            ObservedAreaWater_adjust = dplyr::if_else(
              .data$ObservedAreaWaterHa > .data$ObservedAreaWaterHa_pq,
              .data$ObservedAreaWaterHa_pq,
              .data$ObservedAreaWaterHa),
            flood_delta = (.data$ObservedAreaWater_adjust - dplyr::lag(.data$ObservedAreaWater_adjust, 1))/.data$ObservedAreaWaterHa_pq,
            flood_trend = dplyr::case_when(flood_delta <= -delta ~ '-',
                                           flood_delta < 0 ~ '(-)',
                                           flood_delta >= delta ~ '+',
                                           flood_delta > 0 ~ '(+)',
                                           TRUE ~ '0'))
      }) |>
    dplyr::mutate(
      flood_prop = .data$ObservedAreaWater_adjust/.data$ObservedAreaWaterHa_pq,
      flood_status = dplyr::case_when(
        .data$flood_prop == 0 ~ 'dry',
        .data$flood_prop >= full_prop_threshold ~ 'full',
        .data$flood_prop >= wet_prop_threshold ~ 'wet',
        .data$flood_prop > 0 ~ 'trace')
    )
}
