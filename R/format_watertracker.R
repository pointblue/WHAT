#' Format Water Tracker data
#'
#' @param df Input data frame from water tracker
#' @param wetthreshold Threshold applied to proportion open water for
#'   considering a wetland unit to be flooded
#' @param obsthreshold Threshold applied to the proportion observed for
#'   including water tracker data
#'
#' @return tibble with added fields year, month, wateryear, month_name, status, midpoint
#' @export
#'
#' @examples
#' # format_watertracker(df)
#'
format_watertracker = function(df, wetthreshold = 50, obsthreshold = 80) {
  df |>
    # find mosaic midpoint
    dplyr::rowwise() |>
    dplyr::mutate(midpoint = mean(c(MosaicDateStart, MosaicDateEnd))) |>
    dplyr::tibble() |>
    # get year and month; translate to water year
   dplyr:: mutate(year = format(midpoint, '%Y') |>  as.numeric(),
                  month = format(midpoint, '%m') |>  as.numeric(),
                  wateryear = dplyr::if_else(month >= 10, year + 1, year),
                  month_name = format(midpoint, '%b') |>
                    factor(levels = c('Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar',
                                      'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep'))
                  ) |>
    # interpret flooding status as management mode on each scene date
    dplyr::mutate(
      status = dplyr::if_else(PercentWater > wetthreshold, 'wet', 'dry'),
      status = dplyr::if_else(PercentObserved < obsthreshold, NA, status)) |>
    dplyr::select(year:status, dplyr::everything())
}
