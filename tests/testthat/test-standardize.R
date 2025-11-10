test_that("standardize() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("wooldridge", quietly = TRUE))
})

test_that("standardize()",{
  expect_snapshot({
    RregressPkg::standardize(
      df = wooldridge::gpa1,
      cols = c("colGPA","hsGPA", "ACT"),
      mul = 2
    )
  })
})
