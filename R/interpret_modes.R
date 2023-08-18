#' Interpret wetland operational modes
#'
#' Interpret flooding status on each date and monthly operational modes from
#' Water Tracker data
#'
#' @details This function evaluates the Water Tracker data to first determine
#'   whether a wetland management unit should be considered flooded or not on a
#'   given observation date, and then interpret the flooding status on one or
#'   more observation dates within each month to estimate the monthly
#'   operational mode.
#'
#'   Flooding status is evaluated by setting the `wetthreshold` to a value above
#'   which the unit will be considered flooded (`status = "wet"`) and below
#'   which the unit will be considered dry (`status = "dry"`). However, if
#'   PercentObserved is below the `obsthreshold`, the data will be considered
#'   unreliable and the status changed to `NA`.
#'
#'   Operational mode codes are derived from the `Wetland Water Budget Tool` a
#'   simple spread-sheet based tool allowing non-specialists to estimate wetland
#'   water budgets from a combination of management unit size and schedule of
#'   monthly operational modes.
#'
#'   Possible values include:
#'   * N: No water supply
#'   * F: Flood-up
#'   * D: Drawdown
#'   * M: Maintenance (actively maintained at full)
#'   * H: Hold (filled but not actively maintained)
#'   * I: Irrigation
#'   * C: Cropped (no water supply, but actively growing vegetation)
#'
#'   Monthly operational modes are evaluated by examining the flooding status
#'   for all dates within a single month for each management unit ("unit" by
#'   default) and water year. If all are `"dry"`, `mode = "N"` (no water
#'   supply)). If all are `"wet"`, the mode is set equal to the `wetmode`, which
#'   should be either "M" or "H". If the first date is `"dry"` and the last date
#'   is `"wet"`, `mode = "F"`, while for the reverse, `"mode = "D"`. Any other
#'   combinations will result in `mode = "UNKNOWN"` and should be manually
#'   inspected.
#'
#' @param df tibble resulting from [estimate_area()]
#' @param wetthreshold Threshold applied to the `PercentWater` field for
#'   considering a wetland unit to be flooded
#' @param obsthreshold Threshold applied to the `PercentObserved` field for
#'   considering data to be useful and included
#' @param wetmode Default operational mode for units determined to be flooded
#'
#' @return tibble with additional field "mode"
#' @export
#'
#' @examples
#' # interpret_modes(df)
interpret_modes = function(df, wetmode = 'M', wetthreshold = 50, obsthreshold = 80) {

  df |>
    # interpret flooding status as management mode on each scene date
    dplyr::mutate(
      status = dplyr::if_else(.data$PercentWater > wetthreshold, 'wet', 'dry'),
      status = dplyr::if_else(.data$PercentObserved < obsthreshold,
                              NA, .data$status)) |>
    # interpret set of flooding status in each month
    dplyr::group_by(.data$unit, .data$area_ac, .data$wateryear, .data$month_name) |>
    dplyr::summarize(
      mode = dplyr::case_when(
        all(.data$status == 'wet') ~ wetmode, #maintenance or hold,
        all(.data$status == 'dry') ~ 'N', #no supply
        # dry to wet: assume floodup
        .data$status[.data$MosaicDateStart == min(.data$MosaicDateStart)] == 'dry' &
          .data$status[.data$MosaicDateEnd == max(.data$MosaicDateEnd)] == 'wet' ~ 'F',
        # wet to dry: assume drawdown
        .data$status[.data$MosaicDateStart == min(.data$MosaicDateStart)] == 'wet' &
          .data$status[.data$MosaicDateEnd == max(.data$MosaicDateEnd)] == 'dry' ~ 'D',
        TRUE ~ 'UNKNOWN'),
      .groups = 'drop')
}
