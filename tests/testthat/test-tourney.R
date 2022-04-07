test_that("throws error for season before 2019", {
  expect_error(bart_tourney_sheets(2018), 'valid year')
})

test_that("throws error for season before 2018", {
  expect_error(bart_tourney_odds(2017), 'valid year')
})

test_that("throws error for season before 2000", {
  expect_error(bart_tourney_results(1999), 'valid year')
})

test_that("max. year cannot exceed current season", {
  max_year <- current_season() + 1
  expect_error(bart_tourney_results(max_year = max_year), 'valid year')
})

test_that("odds argument must match", {
  expect_error(bart_tourney_odds(odds='Pre'), 'valid odds')
})

test_that("team argument must match", {
  expect_error(bart_tourney_results(type='Team'), 'valid type')
})
