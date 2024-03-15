#' Estimate monthly wetland operational modes
#'
#' Internal to [analyze_watertracker()]. Not meant to be called separately.
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
#'   Using the output from [estimate_floodstatus()], the function first
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
#' @param df Input tibble from [format_watertracker()]
#' @param fullmode One of `M` or `H`; default operational mode for units
#'   considered fully flooded
#'
#' @return tibble with additional field "mode"
#' @importFrom rlang .data

estimate_wetlandmode = function(df, fullmode) {

  # find first full flood date of each year; assume = fullmode
  firstfull = df |>
    dplyr::arrange(.data$unit, .data$obsdate) |> # order matters!
    dplyr::group_by(.data$unit, .data$wateryear) |>
    filter(.data$flood_status == 'full') |>
    slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(wateryear, obsdate, unit) |>
    dplyr::mutate(modefirst = fullmode)


  df2 <- df |>
    dplyr::arrange(.data$unit, .data$obsdate) |> # order matters!
    dplyr::mutate(
      month_date = paste(.data$year, .data$month, '15', sep = '-') #for plotting
    ) |>
    # first recode brief gaps in full flooding as full
    dplyr::group_by(.data$unit) |>
    dplyr::mutate(
      month_status = case_when(
        .data$flood_status != 'full' &
          lead(.data$flood_status) == 'full' &
          lag(.data$flood_status) == 'full' ~ 'full',
        TRUE ~ flood_status)
      ) |>
    # SUMMARIZE STRAIGHTFORWARD MONTHS: N, fullmode, or transition
    dplyr::group_by(.data$unit, .data$wateryear, .data$month_name) |>
    dplyr::mutate(
      mode = dplyr::case_when(
        all(.data$month_status %in% c('dry', 'trace')) ~ 'N',
        all(.data$month_status == 'full') ~ fullmode,
        TRUE ~ 'transition')) |>
    # TREAT FIRST FULL OF wateryear AS fullmode by default
    dplyr::left_join(firstfull, by = c('wateryear', 'obsdate', 'unit')) |>
    dplyr::mutate(
      mode = dplyr::case_when(
        all(.data$mode == 'transition') & any(.data$modefirst == fullmode) ~ fullmode,
        TRUE ~ .data$mode)) |>
    dplyr::ungroup()

  # INTERPRET SIMPLE TRANSITIONS: INCREASES AS FLOODUP OR IRRIGATION
  # (unless increases immediately follow full flooding)
  tmp_month = df2 |>
    dplyr::group_by(.data$unit, .data$wateryear, .data$month) |>
    dplyr::mutate(trend = case_when(
                    any(.data$flood_trend %in% c('-', '(-)')) ~ '-',
                    any(.data$flood_trend == '+') ~ '+'
                  )) |>
    dplyr::ungroup() |>
    dplyr::select(.data$unit, .data$wateryear, .data$month, .data$mode, .data$trend) |>
    dplyr::distinct() |>
    dplyr::mutate(mode_next = lead(.data$mode),
                  mode_last = lag(.data$mode),
                  trend_next = lead(.data$trend))
  df3 = df2 |>
    # add mode_next and mode_last
    left_join(tmp_month, by = c('unit', 'wateryear', 'month', 'mode')) |>
    dplyr::group_by(.data$unit, .data$wateryear, .data$month_name) |>
    dplyr::mutate(
      mode = case_when(
        # substantial increase and previous month was already full = still full
        .data$mode == 'transition' &
          any(.data$flood_trend == '+') &
          .data$mode_last == fullmode ~ fullmode,
        # substantial increase (and previous month was not already full)
        # and next month is full = floodup
        .data$mode == 'transition' &
          any(.data$flood_trend == '+') & !any(.data$flood_trend == '-') &
          any(.data$mode_next == fullmode) ~ 'F',
        # substantial increase, next month is dry = irrigation
        .data$mode == 'transition' &
          any(.data$flood_trend == '+') &
          .data$mode_next == 'N' ~ 'I',
        TRUE ~ mode)) |>
    dplyr::ungroup()

  # INTERPRET DECREASES AS DRAWDOWNS &
  # ADD FLOODUP BETWEEN DRY AND FULL WHERE NONE YET IDENTIFIED
  # update tmp_month
  tmp_month3 = df3 |>
    dplyr::select(.data$unit, .data$wateryear, .data$month, .data$mode) |>
    dplyr::distinct() |>
    dplyr::mutate(mode_next = lead(.data$mode),
                  mode_last = lag(.data$mode))
  df4 = df3 |> select(-.data$mode_next, -.data$mode_last) |>
    left_join(tmp_month3, by = c('unit', 'wateryear', 'month', 'mode')) |>
    dplyr::group_by(.data$unit, .data$wateryear, .data$month_name) |>
    dplyr::mutate(
      mode = case_when(
        # decrease and last month was full = drawdown
        # (do this after previous step of identifying increases after full = still full)
        .data$mode == 'transition' & any(.data$flood_trend %in% c('-', '(-)')) &
          .data$mode_last == fullmode ~ 'D',
        # full but previous month was dry with no floodup identified = floodup
        .data$mode == fullmode & .data$mode_last == 'N' ~ 'F',
        # floodup but previous month already identified as floodup = full
        # (do this after previous step of identifying floodups)
        .data$mode == 'F' & .data$mode_last == 'F' ~ fullmode,
        TRUE ~ mode)) |>
    dplyr::ungroup()

  # CORE FLOOD CURVE SHOULD BE IDENTIFIED -- CHECK FOR REMAINING UNIDENTIFIED:
  # increases with no floodup, no drawdown identified, or wet after drawdown
  tmp_month4 = df4 |>
    dplyr::select(.data$unit, .data$wateryear, .data$month, .data$mode) |>
    dplyr::distinct() |>
    dplyr::mutate(mode_next = lead(.data$mode),
                  mode_last = lag(.data$mode))
  df5 = df4 |>
    dplyr::select(-.data$mode_next, -.data$mode_last) |>
    dplyr::left_join(tmp_month4, by = c('unit', 'wateryear', 'month', 'mode')) |>
    dplyr::group_by(.data$unit, .data$wateryear, .data$month_name) |>
    dplyr::mutate(
      mode = dplyr::case_when(
        # wet after drawdown or irrigation and not a substantial increase = no supply
        .data$mode == 'transition' & .data$mode_last %in% c('D', 'I') &
          !any(.data$flood_trend == '+') ~ 'N',
        # dry but previous was full, decline detected, and no drawdown identified = drawdown
        .data$mode == 'N' & .data$mode_last == fullmode &
           any(.data$flood_trend %in% c('-', '(-)')) ~ 'D',
        # wet just after floodup = treat as full
        .data$mode == 'transition' & .data$mode_last == 'F' &
          .data$mode_next == fullmode ~ fullmode,
        # floodup sandwiched by fullmode = presumed full
        .data$mode == 'F' &
          .data$mode_last %in% c(fullmode, 'F') &
          .data$mode_next == fullmode ~ fullmode,
        # other increases = treat as irrigation (e.g. due to lull after partial floodup)
        .data$mode == 'transition' &
          (any(.data$flood_trend == '+') |
             (any(.data$flood_trend == '(+)') & any(.data$flood_status == 'wet'))) ~ 'I',
        .data$mode == 'transition' ~ 'UNKNOWN',
        TRUE ~ mode)
    ) |> dplyr::ungroup()

  # finalize/simplify and fix unidentified drawdowns
  res = df5 |>
    dplyr::group_by(WETLAND, unit, CLASS) |>
    dplyr::mutate(Acres = max(ObservedAreaHa)*2.47105,
                  Acres_pq = max(ObservedAreaWaterHa_pq)*2.47105) |>
    dplyr::group_by(WETLAND, unit, CLASS, Acres, Acres_pq, wateryear, year, month, month_name, mode) |>
    dplyr::summarize(flood_prop_max = max(flood_prop), .groups = 'drop') |>
    tidyr::complete(nesting(WETLAND, unit, CLASS, Acres, Acres_pq),
                    nesting(wateryear, year, month_name, month)) |>
    dplyr::arrange(unit, wateryear, month_name) |>
    dplyr::mutate(
      mode = dplyr::case_when(
        # full but next is irrigation or dry, and no drawdown identified = drawdown
        .data$mode == fullmode & dplyr::lead(.data$mode) %in% c('I', 'N') ~ 'D',
        # full but previous is irrigation or dry, and no floodup identified = floodup
        .data$mode == fullmode & dplyr::lag(.data$mode) %in% c('I', 'N', 'UNKNOWN') ~ 'F',
        # wet lingers after drawdown or irrigation = no supply
        .data$mode == 'UNKNOWN' &
          dplyr::lag(.data$mode) %in% c('I', 'D', 'UNKNOWN', 'N') ~ 'N',
        # NA sandwiched by fullmode/D/F = presumed full
        is.na(.data$mode) & dplyr::lag(.data$mode) %in% c(fullmode, 'F') &
          dplyr::lead(.data$mode) %in% c(fullmode, 'D') ~ fullmode,
        # NA sandwiched by no supply
        is.na(.data$mode) & dplyr::lag(.data$mode) %in% c('N') &
          dplyr::lead(.data$mode) %in% c('N', 'F') ~ fullmode,
        TRUE ~ .data$mode))

  return(res)
}

