#' Estimate monthly wetland operational modes
#'
#' Interprets results of [estimate_flood_delta()] to estimate assign monthly
#' water management modes for use with the `Wetland Water Budget Tool` a simple
#' spread-sheet based tool allowing non-specialists to estimate wetland water
#' budgets from a combination of management unit size and schedule of monthly
#' operational modes.
#'
#' @details The sequence of Water Tracker observations for each unit is analyzed
#'   to interpret monthly operational mode codes, as defined in the Wetland
#'   Water Budget Tool (WWBT):
#'   * F (flood up): water applied to fully flood the unit
#'   * M (maintenance): unit is fully flooded and being actively maintained at full to compensate for evapotranspiration and soil infiltration
#'   * H (hold): alternative to maintenance mode in which the unit is (or was) fully flooded but is not being actively maintained at full; evapotranspiration and soil infiltration may result in declines in depth
#'   * D (drawdown): unit is being drained
#'   * I (irrigation): short-term flooding
#'   * N (no water supply): no water added to the unit, though water may be present from management in prior months (e.g., drawdown still in progress)
#'   * C (cropped): no water supply, but actively growing vegetation is removing water from the soil through evapotranspiration
#'
#'   Using the output from [estimate_flood_delta()], the function first
#'   identifies months in which the `flood_status` of all observations is
#'   "full", interpreted as M or H depending on the `fullmode` provided, or
#'   "dry", interpreted as N. Months in which observations differ in the extent
#'   of flooding are considered transitional, and are further interpreted by
#'   examining the `flood_trend`, or the direction and magnitude of change in
#'   the proportion flooded from the previous observation.
#'
#'   A substantial increase in the proportion of a unit flooded from one
#'   observation date to the next is interpreted as new water applied to the
#'   unit, and should normally be interpreted as F or I. The distinction between
#'   F and I is interpreted from the duration of flooding detected, with F
#'   requiring subsequent observation(s) interpreted as M or H. Exceptions
#'   include: (1) increases that represent (sometimes substantial) fluctuations
#'   in the extent of flooding during extended periods of M or H (which are
#'   instead interpreted as continued M or H), or (2) the previous observation
#'   was already interpreted as F. Only one month in each water year should
#'   normally be interpreted as F, even if flood up is slow and the extent of
#'   flooding continues to increase in the following month. However, it is
#'   important to note that floodup may begin at the end of the previous water
#'   year (e.g., in September). There is no limit to the number of months that
#'   can be interpreted as I, but multiple irrigation events will only be
#'   detected if there are gaps in flooding between them.
#'
#'   A substantial decrease in the proportion of a unit flooded from one
#'   observation to the next is interpreted as D, provided the previous
#'   observation(s) were interpreted as M or H and not I or D. Only one month in
#'   each water year should normally be designated as D, even if drawdown is
#'   slow and there is still surface water present in the following month. If a
#'   previous observation was already designated as D and the extent of flooding
#'   continues to decline, it will be interpreted as N.
#'
#'   Relatively small fluctuations in the extent of flooding are ignored and
#'   assumed to reflect noise in the remote sensing data. In addition,
#'   WaterTracker data are not yet capable of automatically detecting cropped
#'   status. Values for `N` may be changed manually to `C` as needed.
#'
#' @param df Input tibble from [estimate_flood_delta()]
#' @param fullmode One of `M` or `H`; default operational mode for units
#'   considered fully flooded
#'
#' @return tibble with additional field "mode"
#' @export
#' @importFrom rlang .data
#' @importFrom tidyr complete
#' @importFrom tidyr nesting
#' @importFrom stringr str_c
#' @examples
#' df = format_watertracker(sampledat) |> estimate_flood_extent() |> estimate_flood_delta()
#' estimate_wetland_mode(df)

estimate_wetland_mode = function(df, fullmode = 'M') {

  df2 <- df |>
    dplyr::select("unit", "wateryear", "month", "month_name", "obsdate",
                  "flood_status", "flood_delta", "flood_trend") |>
    # order matters!
    dplyr::arrange(.data$unit, .data$obsdate) |>
    # recode brief gaps in full flooding as full
    dplyr::group_by(.data$unit) |>
    dplyr::mutate(
      month_status = dplyr::case_when(
        .data$flood_status != 'full' &
          dplyr::lead(.data$flood_status) == 'full' &
          dplyr::lag(.data$flood_status) == 'full' ~ 'full',
        TRUE ~ flood_status)
    ) |>
    # SUMMARIZE STRAIGHTFORWARD MONTHS: N, fullmode, or transition
    dplyr::group_by(.data$unit, .data$wateryear, .data$month) |>
    dplyr::mutate(
      mode = dplyr::case_when(
        all(.data$month_status %in% c('dry', 'trace')) ~ 'N',
        all(.data$month_status == 'full') ~ fullmode,
        TRUE ~ 'transition')) |>
    # ALSO TREAT FIRST FULL OF wateryear AS fullmode by default
    dplyr::group_by(.data$unit, .data$wateryear) |>
    dplyr::mutate(
      mode = dplyr::if_else(.data$mode == 'transition' &
                              .data$flood_status == 'full' &
                              !duplicated(.data$flood_status == 'full'),
                     fullmode,
                     .data$mode)) |>
    dplyr::ungroup()

  # summarize monthly modes
  df_month = df2 |>
    # collapse multiple observations in a single month
    dplyr::group_by(.data$unit, .data$wateryear, .data$month, .data$month_name) |>
    dplyr::summarize(
      flood_trend = stringr::str_c(.data$flood_trend, collapse = ','),
      flood_status = stringr::str_c(.data$flood_status, collapse = ','),
      mode = dplyr::case_when(
        any(.data$mode == fullmode) ~ fullmode,
        all(.data$mode == 'N') ~ 'N',
        all(.data$mode == 'transition') ~ 'transition'),
      .groups = 'drop') |>
    # order matters!
    dplyr::arrange(.data$unit, .data$wateryear, .data$month_name)

  # for each unit, interpret sequence of observations:
  df_interpret = df_month |>
    dplyr::group_by(.data$unit) |>
    # INTERPRET SIMPLE TRANSITIONS: INCREASES AS FLOODUP OR IRRIGATION
    # (unless increases immediately follow full flooding)
    dplyr::mutate(
      mode = dplyr::case_when(
        # substantial increase and previous month was already full = still full
        .data$mode == 'transition' &
          grepl('INCREASE', .data$flood_trend) &
          dplyr::lag(.data$mode) == fullmode ~ fullmode,
        # substantial increase (and previous month was not already full)
        # and next month is full = floodup
        .data$mode == 'transition' &
          grepl('INCREASE', .data$flood_trend) &
          !grepl('DECREASE', .data$flood_trend) &
          dplyr::lead(.data$mode) == fullmode ~ 'F',
        # substantial increase, next month is dry = irrigation
        .data$mode == 'transition' &
          grepl('INCREASE', .data$flood_trend) &
          dplyr::lead(.data$mode) == 'N' ~ 'I',
        TRUE ~ mode),
      # INTERPRET DECREASES AS DRAWDOWNS & ADD FLOODUP BETWEEN DRY AND FULL
      # WHERE NONE YET IDENTIFIED
      mode = dplyr::case_when(
        # decrease and last month was full = drawdown
        # (do this after previous step of identifying increases after full = still full)
        .data$mode == 'transition' &
          grepl('DECREASE|down', .data$flood_trend) &
          dplyr::lag(.data$mode) == fullmode ~ 'D',
        # full but previous month was dry with no floodup identified = floodup
        .data$mode == fullmode &
          dplyr::lag(.data$mode) == 'N' ~ 'F',
        # floodup but previous month already identified as floodup = full
        # (do this after previous step of identifying floodups)
        .data$mode == 'F' &
          dplyr::lag(.data$mode) == 'F' ~ fullmode,
        TRUE ~ mode),
      # CORE FLOOD CURVE SHOULD BE IDENTIFIED -- CHECK FOR REMAINING UNIDENTIFIED:
      # increases with no floodup, no drawdown identified, or wet after drawdown
      mode = dplyr::case_when(
        # wet after drawdown or irrigation and not a substantial increase = no supply
        .data$mode == 'transition' &
          dplyr::lag(.data$mode) %in% c('D', 'I') &
          !grepl('INCREASE', .data$flood_trend) ~ 'N',
        # dry but previous was full, decline detected, and no drawdown identified = drawdown
        .data$mode == 'N' &
          dplyr::lag(.data$mode) == fullmode &
          grepl('DECREASE|down', .data$flood_trend) ~ 'D',
        # wet just after floodup = treat as full
        .data$mode == 'transition' &
          dplyr::lag(.data$mode) == 'F' &
          dplyr::lead(.data$mode) == fullmode ~ fullmode,
        # floodup sandwiched by fullmode = presumed full
        .data$mode == 'F' &
          dplyr::lag(.data$mode) %in% c(fullmode, 'F') &
          dplyr::lead(.data$mode) == fullmode ~ fullmode,
        # other increases = treat as irrigation (e.g. due to lull after partial floodup)
        .data$mode == 'transition' &
          (grepl('INCREASE', .data$flood_trend) |
             (grepl('up', .data$flood_trend) & grepl('wet', .data$flood_status))) ~ 'I',
        .data$mode == 'transition' ~ 'UNKNOWN',
        TRUE ~ mode)
      ) |>
    dplyr::ungroup()

  # unit metadata - expand to cover all months and water years
  df_expand = tidyr::expand_grid(
    df |> dplyr::select('WETLAND', 'unit', 'CLASS', 'AREA_HA', 'AREA_AC') |>
      dplyr::distinct() |>
      dplyr::arrange(.data$WETLAND, .data$unit),
    df |> dplyr::select('wateryear', 'month', 'month_name') |>
      dplyr::distinct() |>
      dplyr::arrange(.data$wateryear, .data$month_name)) |>
    dplyr::left_join(df_interpret |>
                       dplyr::select('unit', 'wateryear', 'month_name', 'mode'),
                     by = c('unit', 'wateryear', 'month_name')) |>
    #fix lingering transitions/unidentified/gaps created by "complete"
    dplyr::mutate(
      mode = dplyr::case_when(
        # full but next is irrigation or dry, and no drawdown identified = drawdown
        .data$mode == fullmode &
          dplyr::lead(.data$mode) %in% c('I', 'N') ~ 'D',
        # full but previous is irrigation or dry, and no floodup identified = floodup
        .data$mode == fullmode &
          dplyr::lag(.data$mode) %in% c('I', 'N', 'UNKNOWN') ~ 'F',
        # wet lingers after drawdown or irrigation = no supply
        .data$mode == 'UNKNOWN' &
          dplyr::lag(.data$mode) %in% c('I', 'D', 'UNKNOWN', 'N') ~ 'N',
        # NA sandwiched by fullmode/D/F = presumed full
        is.na(.data$mode) &
          dplyr::lag(.data$mode) %in% c(fullmode, 'F') &
          dplyr::lead(.data$mode) %in% c(fullmode, 'D') ~ fullmode,
        # NA sandwiched by no supply = no supply
        is.na(.data$mode) &
          dplyr::lag(.data$mode) %in% c('N') &
          dplyr::lead(.data$mode) %in% c('N', 'F') ~ 'N',
        TRUE ~ .data$mode)
    )

  # add metadata back in to finalize
  res = df |>
    dplyr::select("wateryear", "month", "month_name", "year", "unit", "WETLAND",
                  "CLASS", "AREA_HA", "AREA_AC", "ObservedAreaWaterHa_pq",
                  'flood_prop') |>
    dplyr::mutate(AREA_AC_WETTED = .data$ObservedAreaWaterHa_pq * 2.47105) |>
    dplyr::group_by(.data$WETLAND, .data$unit, .data$CLASS, .data$AREA_HA,
                    .data$AREA_AC, .data$AREA_AC_WETTED, .data$wateryear,
                    .data$year, .data$month, .data$month_name) |>
    dplyr::summarize(flood_prop_max = suppressWarnings(max(.data$flood_prop, na.rm = TRUE)),
                     .groups = 'drop') |>
    dplyr::arrange(.data$unit, .data$wateryear, .data$month_name) |>
    dplyr::left_join(df_expand,
                      by = c('WETLAND', 'unit', 'CLASS', 'AREA_HA', 'AREA_AC',
                             'wateryear', 'month', 'month_name'))


  return(res)
}

