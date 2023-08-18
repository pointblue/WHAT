#' Estimate area
#'
#' Estimate the area of each wetland management unit from the area observed in
#' the Water Tracker data.
#'
#' @details This function converts the "ObservedArea" field (m^2) in the
#'   original Water Tracker output data to an estimate of the total area of each
#'   management unit, in acres. The area is estimated as the maximum of all
#'   ObservedArea values with the same grouping ID, which by default is "unit".
#'
#' @param df tibble resulting from [format_watertracker()]
#' @param groupby Character string for the field by which area should be
#'   estimated
#'
#' @return tibble with additional field "area_ac"
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' #estimate_area(df)
estimate_area = function(df, groupby = 'unit') {
  df |>
    dplyr::group_by(!!groupby) |>
    dplyr::mutate(area_ac = max(.data$ObservedArea) / 900 / 10000 * 2.47105)
}
