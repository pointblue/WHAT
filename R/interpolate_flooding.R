#' Interpolate flooding
#'
#' Interpolate adjusted estimates of flooded area from [estimate_flood_extent()]
#' to cover specific dates, facilitating a sum across multiple units.
#'
#' @details For each unit, this function interpolates values of
#'   `ObservedAreaWater_adjust` generated form [estimate_flood_extent()] across a
#'   desired water years. For each water year, the sequence of dates to
#'   interpolate is first generated from Oct 1 through Sep 30 at the desired
#'   interval. For intervals of weeks or days in leap years, leap day is
#'   intentionally skipped in the sequence of dates, facilitating comparisons
#'   across multiple years on common dates.
#'
#'   Values are then interpreted using [zoo::na.spline()], and any values less
#'   than zero are changed to zero. Optionally, if `sum = TRUE`, summarizes
#'   across all units for each unique date to return the total area flooded.
#'
#' @param df Input tibble from [estimate_flood_extent()]
#' @param interval one of "day", "week", "month", "quarter" or "year"; see
#'   [seq.Date()]
#' @param wateryear numeric; water years to interpolate over
#' @param sum Logical; if TRUE, summarize by date across all units
#'
#' @return tibble with fields: `date`, `year`, `month`, `wateryear`,
#'   `AreaWater_ha`, and `AreaWater_ac`
#' @export
#' @importFrom zoo zoo
#' @importFrom lubridate leap_year
#' @importFrom zoo na.spline
#' @importFrom tibble deframe
#'
#' @examples
#' df = format_watertracker(sampledat) |> estimate_flood_extent()
#' interpolate_flooding(df, wateryear = c(2015, 2016))

interpolate_flooding = function(df, interval = 'week', wateryear, sum = FALSE) {

  sq = purrr::map(
    wateryear,
    function(x) {
      sq = seq.Date(
        as.Date(paste(x - 1, '10', '01', sep = '-')),
        as.Date(paste(x, '09', '30', sep = '-')), by = interval)

      if (lubridate::leap_year(x) & interval %in% c('day', 'week')) {
        sq = dplyr::tibble(sq) |>
          dplyr::mutate(
            sq = dplyr::if_else(sq > as.Date(paste0(x, '-02-29')),
                                sq + 1, sq)) |>
          tibble::deframe()
      }
      return(as.character(sq))
    }) |> unlist()

  n_interval = length(
    seq.Date(as.Date(paste(wateryear[1] - 1, '10', '01', sep = '-')),
             as.Date(paste(wateryear[1], '09', '30', sep = '-')), by = interval))

  res = df |>
    split(df$unit) |>
    purrr::map_df(
      ~zoo::zoo(.x$ObservedAreaWater_adjust, order.by = .x$obsdate) |>
        zoo::na.spline(x = as.Date, xout = as.Date(sq), na.rm = FALSE) |>
        tibble::as_tibble(rownames = 'date') |>
        dplyr::mutate(interval = rep(c(1:n_interval), length(wateryear))),
      .id = 'unit') |>
    dplyr::mutate(
      value = dplyr::if_else(.data$value < 0, 0, .data$value),
      date = as.Date(.data$date),
      year = format(.data$date, '%Y') |> as.numeric(),
      month = format(.data$date, '%m') |> as.numeric(),
      wateryear = dplyr::if_else(.data$month > 9,
                                 .data$year + 1, .data$year),
      AreaWater_ha = .data$value,
      AreaWater_ac = .data$AreaWater_ha * 2.47105) |>
    dplyr::select('wateryear', 'year', 'month', 'date', 'interval',
                  'AreaWater_ha', 'AreaWater_ac')

  if (sum) {
    res = res |>
      dplyr::group_by(.data$wateryear, .data$year, .data$month, .data$date, .data$interval) |>
      dplyr::summarize(AreaWater_ha = sum(.data$AreaWater_ha, na.rm = TRUE),
                       AreaWater_ac = .data$AreaWater_ha * 2.47105,
                       .groups = 'drop')
  }
  return(res)

}
