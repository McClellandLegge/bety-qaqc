context("Find All")

source("find.all.missing.test.dat.R", local = TRUE)

test_that("finds all covariates", {
  expect_identical(
    find.all.missing(
      most.recent = FALSE,
      use.date    = FALSE,
      from.date   = NA,
      user_trait  = "All",
      tbl.ex      = FALSE,
      outfile     = NA,
      is.test     = TRUE,
      test.traits = test.traits,
      test.cov    = test.cov
    )$master$trait_id, expect1)
})

test_that("most recent finds latest update", {
  expect_identical(
    find.all.missing(
      most.recent = TRUE,
      use.date = FALSE,
      from.date = NA,
      user_trait = "All",
      tbl.ex = FALSE,
      outfile = NA,
      is.test = TRUE,
      test.traits = test.traits,
      test.cov = test.cov
    )$master$trait_id, expect2)
})

test_that("from date finds correct dates", {
  expect_identical(
    find.all.missing(
      most.recent = FALSE,
      use.date    = TRUE,
      from.date   = "1890-02-17",
      user_trait  = "All",
      tbl.ex      = FALSE,
      outfile     = NA,
      is.test     = TRUE,
      test.traits = test.traits,
      test.cov    = test.cov
    )$master$trait_id, expect3)
})
