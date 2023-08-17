format_watertracker = function(df, wetthreshold = 50, obsthreshold = 80) {
  df %>%
    # find mosaic midpoint
    rowwise() %>%
    mutate(midpoint = mean(c(MosaicDateStart, MosaicDateEnd))) %>%
    tibble() %>%
    # get year and month; translate to water year
    mutate(year = format(midpoint, '%Y') %>% as.numeric(),
           month = format(midpoint, '%m') %>% as.numeric(),
           wateryear = if_else(month >= 10, year + 1, year),
           month_name = format(midpoint, '%b') %>%
             factor(levels = c('Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr',
                               'May', 'Jun', 'Jul', 'Aug', 'Sep'))) %>%
    # interpret flooding status as management mode on each scene date
    mutate(status = if_else(PercentWater > wetthreshold, 'wet', 'dry'),
           status = if_else(PercentObserved < obsthreshold, NA, status)) %>%
    select(year:status, everything())
}
