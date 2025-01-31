test_that("dataframe_report() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
})

test_that("dataframe_report()", {
  expect_snapshot({
    set.seed(123)

    dt <- data.frame(
      id = 1:100,
      category = sample(c("A","B","C",NA), 100, replace = T),
      value = c(rnorm(97), -10, 100, NA),
      date = c(seq.Date(from = as.Date("2020-01-01"), by = "day", length.out = 99), NaN),
      text = sample(c("Lorem","Ipsum","Dolor","Sit",NA), 100, replace = T)
    )
    RregressPkg::dataframe_report(dt)
  })
})
