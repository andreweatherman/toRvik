test_that("throws error for season before 2008", {
  expect_error(bart_ratings(2007), 'or later')
  expect_error(bart_factors(2007), 'or later')
  expect_error(bart_conf_factors(2007), 'or later')
  expect_error(bart_conf_stats(2007, conf='ACC'), 'valid year')
})

test_that("date range works with one end", {
  expect_output(str(bart_factors(start='2022-01-01')), 'toRvik_data')
  expect_output(str(bart_factors(end='2022-01-01')), 'toRvik_data')
  expect_output(str(bart_conf_factors(start='2022-01-01')), 'toRvik_data')
  expect_output(str(bart_conf_factors(end='2022-01-01')), 'toRvik_data')
})

test_that("runs without date range", {
  expect_output(str(bart_factors()), 'toRvik_data')
  expect_output(str(bart_conf_factors()), 'toRvik_data')
})

test_that("location values must be exact", {
  expect_message(bart_factors(location='Home'), 'went wrong')
  expect_message(bart_conf_factors(location='Home'), 'went wrong')
})

test_that("location values must be exact", {
  expect_message(bart_factors(type='NC'), 'went wrong')
  expect_message(bart_conf_factors(type='NC'), 'went wrong')
})

test_that("conf code must be exact", {
 expect_error(bart_conf_stats(), 'valid')
 expect_error(bart_conf_stats(conf='acc'), 'valid')
})
