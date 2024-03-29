#' Generalize monthly management modes
#'
#' Identify typical water management schedule across multiple water years of
#' data
#'
#' @details For each unit, identifies and returns the most frequent management
#'   mode in each month and calculates `weight` as the proportion of water years
#'   in which that mode was detected. Low values for weight reflect months with
#'   more annual variability in the monthly management schedules.
#'
#'   Note that ties are not returned, and in the case of ties, the function
#'   favors returning modes in the following order: F, D, I, M, H, N (see
#'   [estimate_wetland_mode()] for details). It is therefore possible for the
#'   function to return multiple months with values for F and D. Setting
#'   `clean=TRUE` will reclassify duplicate values of F and D to `fullmode`, but
#'   will also remove `weight` estimates since they may no longer be valid.
#'
#' @param df Input tibble from [estimate_wetland_mode()]
#' @param fullmode One of `M` or `H`; only necessary if `clean = TRUE`; see
#'   Details
#' @param clean Logical; determines whether to correct for repeat values of F
#'   and D
#'
#' @return Input tibble with only one entry for each month for each unit, the
#'   most frequent value for "mode", and an additional field `weight` giving the
#'   proportion of years observed containing that management mode in that month.
#'
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' df = format_watertracker(sampledat) |> estimate_flood_extent() |>
#'   estimate_flood_delta() |> estimate_wetland_mode()
#' generalize_wetland_mode(df)
#'
generalize_wetland_mode = function(df, fullmode = NULL, clean = FALSE) {
  df = df |>
    #set order of importance
    dplyr::mutate(mode = factor(.data$mode, levels = c('F', 'D', 'I', 'M', 'H', 'N'))) |>
    dplyr::group_by(.data$WETLAND, .data$unit, .data$CLASS, .data$AREA_HA,
                    .data$AREA_AC, .data$AREA_AC_WETTED,
                    .data$month_name, .data$month) |>
    # frequency/prop of each mode in each month
    dplyr::count(mode) |>
    dplyr::mutate(total = sum(.data$n),
                  weight = .data$n / .data$total) |>
    dplyr::arrange(mode) |> #sort by factor level = order of importance
    # most frequent: with_ties = FALSE, means if ties, will return first one
    dplyr::slice_max(n = 1, with_ties = FALSE, order_by = .data$n) |>
    dplyr::ungroup() |>
    dplyr::select(-.data$total, -.data$n)

  if (clean) {
    df = df |>
      #handle repeat F and D, or lack of F and D
      dplyr::mutate(
        mode = dplyr::case_when(
          .data$mode == 'D' & dplyr::lead(.data$mode) == 'D' ~ fullmode,
          .data$mode == 'F' & dplyr::lag(.data$mode) == 'F' ~ fullmode,
          .data$mode == fullmode & dplyr::lag(.data$mode) == 'N' ~ 'F',
          .data$mode == fullmode & dplyr::lead(.data$mode) == 'N' ~ 'D',
          TRUE ~ mode
        )
      ) |>
      dplyr::select(-.data$weight)
  }

  return(df)
}
