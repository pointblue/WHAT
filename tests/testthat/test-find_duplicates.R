df = sampledat |> find_duplicates()

test_that("no data loss", {
  expect_equal(nrow(sampledat), nrow(df))
})

test_that("dup field added", {
  expect_true('dup' %in% names(df))
})

test_that("PercentWater > 0 if dup", {
  expect_true(all(df$PercentWater[df$dup] > 0))
})
