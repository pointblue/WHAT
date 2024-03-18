df = find_duplicates(sampledat)

test_that("no data loss", {
  expect_equal(nrow(sampledat), nrow(df))
})

test_that("dup field added", {
  expect_true('dup' %in% names(df))
})

test_that("PercentWater > 0 if dup", {
  expect_true(
    all(df |> dplyr::filter(!is.na(PercentWater)) |> dplyr::filter(dup) |>
          dplyr::pull(PercentWater) > 0))
})
