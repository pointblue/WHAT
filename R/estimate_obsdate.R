#' Estimate observation date of satellite imagery
#'
#' @details Internal to [format_watertracker()]. Not meant to be called
#'   separately.
#'
#'   Function first find the number of days of overlap period between the end
#'   date of each observation's mosaic date range and start of the next
#'   observation's date range. If the next row is marked as a duplicate, the
#'   observation date of the image is estimated as the midpoint of this overlap
#'   period. Otherwise, the observation date is estimated as the midpoint of the
#'   original mosaic start and end date range.
#'
#' @param df Input data frame from water tracker with added field "dup" from
#'   [find_duplicates()]
#'
#' @return tibble with added fields "overlap_days" and "obsdate".
#'
estimate_obsdate = function(df) {
  df |>
    dplyr::mutate(
      overlap_days = difftime(.data$MosaicDateEnd,
                              dplyr::lead(.data$MosaicDateStart),
                              units = 'days'),
      obsdate = dplyr::case_when(
        dplyr::lead(dup) ~ .data$MosaicDateEnd - .data$overlap_days/2,
        TRUE ~ .data$MosaicDateStart +
          difftime(.data$MosaicDateEnd, .data$MosaicDateStart, 'days')/2)
    )
}
