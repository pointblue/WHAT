df = sampledat |> dplyr::select(obsdate = MosaicDateStart) |> add_date_details()
df2 = sampledat |> dplyr::select(obsdate = MosaicDateStart)
df2$obsdate[6:10] = NA

test_that("no data loss", {
  expect_equal(nrow(sampledat), nrow(df))
})

test_that("wateryear field added", {
  expect_true('wateryear' %in% names(df))
})

test_that('all observations have a wateryear', {
  expect_false(any(is.na(df$obsdate)))
})

test_that('gives error if no obsdate field', {
  expect_error(add_date_details(sampledat))
})

test_that('gives warning if missing dates', {
  expect_warning(add_date_details(df2))
})
