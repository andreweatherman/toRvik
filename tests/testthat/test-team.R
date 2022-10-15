test_that("throws error for season before 2010", {
  expect_error(bart_team_shooting(2009), 'or later')
})

test_that("throws error for season before 2008", {
  expect_error(bart_team_box(year=2007), 'or later')
})

test_that('throws error for more than one space in team name', {
  expect_error(bart_team_history(team='North  Carolia'), 'spacing')
})
