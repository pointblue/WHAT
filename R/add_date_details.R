#' Extract observation date details
#'
#' @details Internal to [format_watertracker()]. Not meant to be called
#'   separately.
#'
#' @param df Input data frame from water tracker with added field "obsdate" from
#'   [estimate_obsdate()]
#'
#' @return tibble with added fields "year", "month", "wateryear", and
#'   "month_name".
#'
add_date_details = function(df) {
  # get year and month of obsdate; translate to water year
  res = df |>
    dplyr::mutate(
      year = format(.data$obsdate, '%Y') |> as.numeric(),
      month = format(.data$obsdate, '%m') |> as.numeric(),
      wateryear = dplyr::if_else(.data$month >= 10, .data$year + 1, .data$year),
      month_name = format(.data$obsdate, '%b') |>
        factor(levels = c('Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr',
                          'May', 'Jun', 'Jul', 'Aug', 'Sep'))) |>
    dplyr::select("wateryear", "month", "month_name", "year", "obsdate",
                  dplyr::everything())

  if (any(is.na(res$wateryear))) {
    warning('Warning: Dates are missing in some rows.')
  }

  return(res)
}
