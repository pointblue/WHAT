#' Estimate area
#'
#' Estimate the area of each wetland management unit from the area observed in
#' the Water Tracker data.
#'
#' @details This function converts the "ObservedArea" field (m^2) in the
#'   original Water Tracker output data to an estimate of the total area of each
#'   management unit, in acres. The area is estimated as the maximum of all
#'   ObservedArea values with the unit name.
#'
#' @param df tibble resulting from [format_watertracker()]
#'
#' @return tibble with additional field "area_ac"
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' format_watertracker(sampledat) |> estimate_area()
estimate_area = function(df) {
  df |>
    dplyr::group_by(.data$unit) |>
    dplyr::mutate(area_ac = max(.data$ObservedArea) / 900 / 10000 * 2.47105) |>
    dplyr::ungroup()
}
