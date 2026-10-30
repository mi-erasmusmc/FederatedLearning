test_that("loading the package loads Matrix", {
  # Sparse matrices returned by workers need Matrix's methods for base generics
  # such as nrow(); without them nrow() is NULL when no Matrix:: call ran first.
  expect_true("Matrix" %in% names(getNamespaceImports("FederatedLearning")))
})
