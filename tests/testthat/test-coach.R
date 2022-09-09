test_that("throws error for season before 2008", {
  expect_error(bart_coach_change(2007), 'or later')
})

test_that('throws error for more than one space in coach name', {
  expect_error(bart_coach(coach='Mark  Few'), 'spacing')
})
