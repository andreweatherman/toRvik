test_that("throws error for season before 2008", {
  expect_error(bart_season_schedule(2007), 'valid year')
  expect_error(bart_team_schedule(2007), 'valid year')
  expect_error(bart_pregame(2007), 'valid year')
  expect_error(bart_game_box(2007), 'valid year')
  expect_error(bart_game_factors(2007), 'valid year')
})

test_that("all years can combine", {
  skip_on_cran()
  expect_output(str(map_dfr(c(2008:2022), .f=(function(year){bart_game_box(year)}))), 'tibble')
})
