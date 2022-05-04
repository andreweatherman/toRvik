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
  expect_error(bart_player_season(stat='Box'), 'valid')
  expect_error(bart_player_game(stat='Box'), 'valid')
  expect_error(bart_transfers(stat='Box'), 'valid')
  expect_error(bart_pro(stat='Box'), 'valid')
})

test_that("conf code must be exact", {
  expect_error(bart_poy(conf='Acc'), 'valid')
})

test_that("github issue #4; rbind works", {
  skip_on_cran()
  expect_output(print(map_dfr(.x=seq(2008, toRvik::current_season(), 1),
                        .f=function(year) {
                        x <- bart_player_season(year)
                          return(x)
                        })),
                'tibble' )
})

test_that("all normal season splits return tibbles", {
  skip_on_cran()
  expect_output(str(bart_player_season()), 'tibble')
  expect_output(str(bart_player_season(stat='box')), 'tibble')
  expect_output(str(bart_player_season(stat='shooting')), 'tibble')
  expect_output(str(bart_player_season(stat='adv')), 'tibble')
})

test_that("all transfer splits return tibbles", {
  skip_on_cran()
  expect_output(str(bart_transfers()), 'tibble')
  expect_output(str(bart_transfers(stat='box')), 'tibble')
  expect_output(str(bart_transfers(stat='shooting')), 'tibble')
  expect_output(str(bart_transfers(stat='adv')), 'tibble')
})

test_that("all pro splits return tibbles", {
  skip_on_cran()
  expect_output(str(bart_pro()), 'tibble')
  expect_output(str(bart_pro(stat='box')), 'tibble')
  expect_output(str(bart_pro(stat='shooting')), 'tibble')
  expect_output(str(bart_pro(stat='adv')), 'tibble')
})

test_that("early returns tibbles", {
  skip_on_cran()
  expect_output(str(bart_pro(early=TRUE)), 'tibble')
  expect_output(str(bart_pro(early=FALSE)), 'tibble')
})

test_that("conf_only returns tibbles", {
  skip_on_cran()
  expect_output(str(bart_pro(conf_only=TRUE)), 'tibble')
  expect_output(str(bart_pro(conf_only=FALSE)), 'tibble')
})
