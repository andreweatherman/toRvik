test_that("throws error for season before 2008", {
  expect_error(bart_ratings(2007), 'valid year')
  expect_error(bart_factors(2007), 'valid year')
  expect_error(bart_conf_factors(2007), 'valid year')
  expect_error(bart_conf_stats(2007, conf='ACC'), 'valid year')
})

test_that("archive cuts off before 2014-15", {
  expect_error(bart_archive('2014-01-01'), '2014')
})

test_that("quad must be character", {
  expect_error(bart_factors(quad=4), "character")
  expect_error(bart_conf_factors(quad=4), "character")
})

test_that("date range works with one end", {
  expect_output(str(bart_factors(start='20220101')), 'tibble')
  expect_output(str(bart_factors(end='20220101')), 'tibble')
  expect_output(str(bart_conf_factors(start='20220101')), 'tibble')
  expect_output(str(bart_conf_factors(end='20220101')), 'tibble')
})

test_that("runs without date range", {
  expect_output(str(bart_factors()), 'tibble')
  expect_output(str(bart_conf_factors()), 'tibble')
})

test_that("venue values must be exact", {
  expect_error(bart_factors(venue='Home'), "correct")
  expect_error(bart_conf_factors(venue='Home'), "correct")
})

test_that("type values must be exact", {
  expect_error(bart_factors(type='NC'), "correct")
  expect_error(bart_conf_factors(type='NC'), "correct")
})

test_that("quad values must be exact", {
  expect_error(bart_factors(quad='5'), "correct")
  expect_error(bart_conf_factors(quad='5'), "correct")
})

test_that("conf code must be exact", {
 expect_error(bart_conf_stats(), 'valid')
 expect_error(bart_conf_stats(conf='acc'), 'valid')
})
