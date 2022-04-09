test_that("throws error for season before 2008", {
  expect_error(bart_player_season(2007), 'valid year')
  expect_error(bart_player_game(2007), 'valid year')
  expect_error(bart_poy(2007), 'valid year')
  expect_error(bart_injuryimpact(year=2007, team='Duke', player='Paolo Banchero'), 'valid year')
})

test_that("throws error for more than one space in name", {
  expect_error(bart_injuryimpact(team='Duke', player='Paolo  Banchero'), 'spacing')
  expect_error(bart_injuryimpact(team='North  Carolina', player='Caleb Love'), 'spacing')
})

test_that("stat input must match", {
  expect_error(bart_player_season(), 'valid')
  expect_error(bart_player_season(stat='Box'), 'valid')
  expect_error(bart_player_game(), 'valid')
  expect_error(bart_player_game(stat='Box'), 'valid')
  expect_error(bart_transfers(), 'valid')
  expect_error(bart_transfers(stat='Box'), 'valid')
})

test_that("conf code must be exact", {
  expect_error(bart_poy(conf='Acc'), 'valid')
})
