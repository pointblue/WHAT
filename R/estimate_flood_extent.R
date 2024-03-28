#' Estimate flooding extent
#'
#' Estimates the typical maximum extent of the open water footprint in each unit
#' as a percentile of all observations in the dataset, helping to correct for
#' upland areas that may be included within each unit that are rarely, if ever,
#' flooded.
#'
#' @details For each unit, calculates `ObservedAreaWater_pq` as a percentile
#'   value, as determined by `prob`, of all `ObservedAreaWaterHa` in the dataset
#'   (as provided in the raw Water Tracker data). This value is intended to
#'   represent the typical upper limit of wetted area, to account for upland
#'   areas in the unit that are not typically flooded as part of regular
#'   management.
#'
#'   The value for `prob` should typically be set less than 1 to allow for
#'   outliers resulting from occasional extreme flooding events, such as driven
#'   by precipitation. (Note that providing multiple values of `prob`, while
#'   supported by the [quantile()] function, is not supported by this function.)
#'
#'   The `ObservedAreaWater_adjust` is then calculated to reflect this upper
#'   limit of wetted area, such that values of `ObservedAreaWaterHa` that exceed
#'   `ObservedAreaWater_pq` are reduced to this value. Likewise, `flood_prop` is
#'   calculated as the proportion of observed `ObservedAreaWater_adjust` on each
#'   date relative to this upper limit.
#'
#'   Finally a categorical `flood_status` is assigned to each observation date as:
#'   * `full`: `flood_prop` >= `full_prop_threshold`
#'   * `wet`: `wet_prop_threshold` <= `flood_prop` < `full_prop_threshold`,
#'   * `trace`: `flood_prop` > 0 but < `wet prop threshold`
#'   * `dry`: `flood_prop` = 0
#'
#' @param df Input tibble from [format_watertracker()].
#' @param prob Single numeric value passed to [quantile()]; see Details.
#' @param full_prop_threshold Numeric value (0-1); see Details.
#' @param wet_prop_threshold Numeric value (0-1); see Details.
#'
#' @return tibble with added fields: `ObservedAreaWater_pq`,
#'   `ObservedAreaWater_adjust`, `flood_prop`, and `flood_status`
#' @export
#' @importFrom rlang .data
#' @importFrom purrr map_df
#' @examples
#' df = format_watertracker(sampledat)
#' estimate_flood_extent(df)


estimate_flood_extent = function(df, prob = 0.95,
                                 full_prop_threshold = 0.55,
                                 wet_prop_threshold = 0.2) {
  df |>
    split(df$unit) |>
    purrr::map_df(
      function(x) {
        x |>
          dplyr::filter(.data$ObservedPixels > 0) |> #drop dates with no data
          dplyr::mutate(
            ObservedAreaWaterHa_pq = stats::quantile(.data$ObservedAreaWaterHa, prob, na.rm = TRUE),
            ObservedAreaWater_adjust = dplyr::if_else(
              .data$ObservedAreaWaterHa > .data$ObservedAreaWaterHa_pq,
              .data$ObservedAreaWaterHa_pq,
              .data$ObservedAreaWaterHa),
            flood_prop = .data$ObservedAreaWater_adjust/.data$ObservedAreaWaterHa_pq,
            flood_status = dplyr::case_when(
              .data$flood_prop == 0 ~ 'dry',
              .data$flood_prop >= full_prop_threshold ~ 'full',
              .data$flood_prop >= wet_prop_threshold ~ 'wet',
              .data$flood_prop > 0 ~ 'trace'))
        })
}
