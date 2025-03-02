test_that("standardize() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("wooldridge", quietly = TRUE))
})

test_that("standardize()",{
  expect_snapshot({
    cols <- c("colGPA", "hsGPA", "ACT")
    dt <- data.table::as.data.table(wooldridge::gpa1) |>
      _[, ..cols]

    stand_dt <- RregressPkg::standardize(
      df = dt,
      cols = c("hsGPA", "ACT"),
      mul = 2
    )
    head(stand_dt)
  })
})
