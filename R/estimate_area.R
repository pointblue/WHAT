#' Estimate area
#'
#' @param df tibble resulting from [format_watertracker()]
#'
#' @return tibble with additional field "area_ac"
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' #estimate_area(df)
estimate_area = function(df) {
  df |>
    dplyr::mutate(area_ac = max(.data$ObservedArea) / 900 / 10000 * 2.47105) |>
    dplyr::select(.data$ID, .data$unit, .data$area_ac, dplyr::everything())

}
