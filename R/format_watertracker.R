#' Format Water Tracker data
#'
#' @param df Input data frame from water tracker
#' @param wetthreshold Threshold applied to proportion open water for
#'   considering a wetland unit to be flooded
#' @param obsthreshold Threshold applied to the proportion observed for
#'   including water tracker data
#' @param unitID Character string for the field containing the management units
#'   for which hydrological data will be evaluated
#'
#' @return tibble with added fields year, month, wateryear, month_name, status,
#'   midpoint
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' # format_watertracker(df)
#'
format_watertracker = function(df, unitID = 'Name', wetthreshold = 50,
                               obsthreshold = 80) {
  df |>
    # find mosaic midpoint
    dplyr::rowwise() |>
    dplyr::mutate(midpoint = mean(c(.data$MosaicDateStart,
                                    .data$MosaicDateEnd))) |>
    dplyr::tibble() |>
    # get year and month; translate to water year
    dplyr::mutate(year = format(.data$midpoint, '%Y') |>  as.numeric(),
                  month = format(.data$midpoint, '%m') |>  as.numeric(),
                  wateryear = dplyr::if_else(.data$month >= 10,
                                             .data$year + 1, .data$year),
                  month_name = format(.data$midpoint, '%b') |>
                    factor(levels = c('Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar',
                                      'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep'))
    ) |>
    # interpret flooding status as management mode on each scene date
    dplyr::mutate(
      status = dplyr::if_else(.data$PercentWater > wetthreshold, 'wet', 'dry'),
      status = dplyr::if_else(.data$PercentObserved < obsthreshold,
                              NA, .data$status)) |>
    dplyr::select(unit = unitID, .data$year:.data$status, dplyr::everything())
}
