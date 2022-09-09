test_that("throws error for season before 2008", {
  expect_error(bart_season_schedule(2007), 'or later')
  expect_error(bart_pregame(2007), 'or later')
  expect_error(bart_game_box(2007), 'or later')
  expect_error(bart_game_factors(2007), 'or later')
})
