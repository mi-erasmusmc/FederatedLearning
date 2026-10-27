test_that("peak memory is reported for the main process", {
  peak <- peakMemoryMbCpp()
  expect_true(is.finite(peak) && peak > 0)
  rows <- stampPeakMemory(data.frame(method = c("a", "b")), list(NULL))
  expect_true(all(rows$peakMemoryMbMain >= peakMemoryMbCpp() - 1e-6))
  expect_true(all(is.na(rows$peakMemoryMbWorkers)))
})

test_that("durations are formatted in readable units", {
  expect_identical(formatDuration(42), "42 s")
  expect_identical(formatDuration(90), "1.5 min")
  expect_identical(formatDuration(5400), "1.50 h")
  expect_identical(formatDuration(NA_real_), "NA")
})
