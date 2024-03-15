df = sampledat |> find_duplicates() |> estimate_obsdate()

test_that("no data loss", {
  expect_equal(nrow(sampledat), nrow(df))
})

test_that("obsdate, overlap_days fields added", {
  expect_true('overlap_days' %in% names(df))
  expect_true('obsdate' %in% names(df))
})

test_that('all observations have a date', {
  expect_false(any(is.na(df$obsdate)))
})
