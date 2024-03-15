#' Find pseudoreplicate/duplicate data in Water Tracker output
#'
#' @details Internal to [format_watertracker()]. Not meant to be called
#'   separately.
#'
#'   An observation is considered to be a likely duplicate of a previous
#'   observation (i.e. representing the same individual scene) if: (1) the date
#'   range of the mosaic starts on or before previous mosaic's end date, (2) the
#'   PercentWater and ObservedAreaHa stats match, and (3) PercentWater is either
#'   NA or greater than zero (because difficult to distinguish multiple dry
#'   images from duplicates).
#'
#' @param df Input data frame from water tracker
#'
#' @return tibble with added logical field "dup"
#'
find_duplicates = function(df) {
  df |>
    dplyr::mutate(
      dup = dplyr::if_else(
        .data$MosaicDateStart <= dplyr::lag(.data$MosaicDateEnd) &
          (.data$PercentWater == dplyr::lag(.data$PercentWater) |
             (is.na(.data$PercentWater) & (is.na(dplyr::lag(.data$PercentWater))))) &
          .data$ObservedAreaHa == dplyr::lag(.data$ObservedAreaHa) &
          (is.na(.data$PercentWater) | .data$PercentWater > 0),
        TRUE, FALSE))
}
