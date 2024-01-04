# A Barttorvik Companion

<!-- badges: start -->
[![CRAN
status](https://www.r-pkg.org/badges/version/toRvik)](https://CRAN.R-project.org/package=toRvik)
[![Lifecycle:
superseded](https://img.shields.io/badge/lifecycle-superseded-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#superseded)
[![R-CMD-check](https://github.com/andreweatherman/toRvik/workflows/R-CMD-check/badge.svg)](https://github.com/andreweatherman/toRvik/actions)
[![Codecov test
coverage](https://codecov.io/gh/andreweatherman/toRvik/branch/main/graph/badge.svg)](https://app.codecov.io/gh/andreweatherman/toRvik?branch=main)
<!-- badges: end -->

**IMPORTANT**: The package will no longer be maintained. I am in the process of developing a new API in [cbbdata](https://cbbdata.aweatherman.com/) and accompanying package [cbbplotR](https://cbbplotr.aweatherman.com). Will bring faster speeds and a more defined structure. Expected roll-out to coincide with the 2023-24 college basketball season. toRvik will remain operational in its current state until the new API and package are released.

[**`toRvik`**](https://github.com/andreweatherman/toRvik) is an R
package for working with and scraping men’s college basketball.

There are a lot of college basketball data out there, but most are
difficult to pull and clean or they are behind a paywall. With `toRvik`,
you have immediate access to some of the most detailed and extensive
college basketball statistics publicly available – all returned in tidy
format with just a single line of code! Best of all, no subscription is
required to access the data.  
  
Most of `toRvik`’s functions are powered by a dedicated Fast API
framework – delivering data at rapid speeds with dependable up-times.

As of version 1.0.3, the package includes nearly 30 functions for
pulling player and team data, game results, advanced metric splits,
play-by-play shooting, and more. Leveraging the same data and models as
[Barttorvik](www.barttorvik.com), the package now offers game and
tournament predictor functions, allowing you to simulate games between
any pair of teams on any date at any venue back to the 2014-15 season.
`toRvik` also offers extensive transfer histories for over 5,000 players
back to the 2011-12 season and detailed player recruiting rankings for
over 6,000 players back to 2007-08.

## Package Installation

Install the released version of `toRvik` from CRAN:

``` r
install.packages("toRvik")
```

Or install the development version from GitHub with:

``` r
if (!requireNamespace('devtools', quietly = TRUE)){
  install.packages('devtools')
}
devtools::install_github("andreweatherman/toRvik") 
```

## Package Highlights

- Detailed game-by-game + season-long statistics by player and split
- Extensive transfer + recruiting histories
- Custom game and tournament predictions
- Shooting splits + shares by team
- Game box scores for all D-1 games
- Team + conference four factors by split
- Game-by-game four factors
- NCAA committee-style team sheets
- D-1 head coaching changes

## Basic Uses

All `toRvik` functions fall into one of six categories:

- **Rating**, pulling and slicing T-Rank + four factor data
- **Player**, pulling player data and histories
- **Team**, pulling team statistics and histories
- **Game**, pulling game-by-game data and schedules
- **Tournament**, pulling raw and adjusted tournament performance
- **Miscellaneous**

### Pull T-Rank ratings:

Calling `bart_ratings` will return the current T-Rank ranks and ratings.

``` r
head(bart_ratings())
```

    ## ── Team Ratings: 2022 ────────────────────────────────────────── toRvik 1.1.0 ──

    ## ℹ Data updated: 2022-09-08 21:16:51 EDT

    ## # A tibble: 6 × 19
    ##   team     conf  barthag barth…¹ adj_o adj_o…² adj_d adj_d…³ adj_t adj_t…⁴   wab
    ##   <chr>    <chr>   <dbl>   <int> <dbl>   <int> <dbl>   <int> <dbl>   <int> <dbl>
    ## 1 Gonzaga  WCC     0.966       1  120.       4  89.9       9  72.6       5  6.71
    ## 2 Houston  Amer    0.959       2  117.      10  88.5       6  63.7     336  6.15
    ## 3 Kansas   B12     0.958       3  120.       5  91.3      13  69.1      71 10.4 
    ## 4 Texas T… B12     0.951       4  111.      41  85.4       1  66.3     223  6.57
    ## 5 Baylor   B12     0.949       5  118.       8  91.3      14  67.6     149  8.91
    ## 6 Duke     ACC     0.944       6  123.       1  96.0      53  67.4     161  7.19
    ## # … with 8 more variables: nc_elite_sos <int>, nc_fut_sos <dbl>,
    ## #   nc_cur_sos <dbl>, ov_elite_sos <int>, ov_fut_sos <dbl>, ov_cur_sos <dbl>,
    ## #   seed <dbl>, year <int>, and abbreviated variable names ¹​barthag_rk,
    ## #   ²​adj_o_rk, ³​adj_d_rk, ⁴​adj_t_rk

### Pull team statistics

Calling `bart_factors` will return four factor stats on a number of
splits. To filter by home games, set venue to ‘home.’

``` r
head(bart_factors(location='H'))
```

    ## ── Team Factors ──────────────────────────────────────────────── toRvik 1.1.0 ──

    ## ℹ Data updated: 2022-09-08 21:16:52 EDT

    ## # A tibble: 6 × 22
    ##   team      conf  rating  rank adj_o adj_o…¹ adj_d adj_d…² tempo off_ppp off_efg
    ##   <chr>     <chr>  <dbl> <dbl> <dbl>   <dbl> <dbl>   <dbl> <dbl>   <dbl>   <dbl>
    ## 1 Houston   Amer    32.7     1  116.      15  83.0       1  66.1    117.    54.2
    ## 2 Gonzaga   WCC     29.7     2  120.       5  89.9      18  72.7    123.    60.1
    ## 3 Baylor    B12     28.8     3  116.       9  87.6       9  69.3    116.    55.0
    ## 4 Villanova BE      28.8     4  123.       2  94.0      50  63.2    122.    57.6
    ## 5 Purdue    B10     28.4     5  124.       1  96.0      81  67.9    125.    58.2
    ## 6 Auburn    SEC     27.6     6  115.      17  87.5       8  72.9    113.    53.1
    ## # … with 11 more variables: off_to <dbl>, off_or <dbl>, off_ftr <dbl>,
    ## #   def_ppp <dbl>, def_efg <dbl>, def_to <dbl>, def_or <dbl>, def_ftr <dbl>,
    ## #   wins <int>, losses <int>, games <int>, and abbreviated variable names
    ## #   ¹​adj_o_rank, ²​adj_d_rank

Calling `bart_team_box` will return team box totals and per-game
averages by game type. To find how Duke performed during the month of
March:

``` r
bart_team_box(team='Duke', split='month') |>
  dplyr::filter(month=='March')
```

    ## ── Team Stats ────────────────────────────────────────────────── toRvik 1.1.0 ──

    ## ℹ Data updated: 2022-09-08 21:16:52 EDT

    ## # A tibble: 1 × 39
    ##   team  month   min   pos   fgm   fga fg_pct   tpm   tpa fg3_pct   ftm   fta
    ##   <chr> <chr> <int> <int> <int> <int>  <dbl> <int> <int>   <dbl> <int> <int>
    ## 1 Duke  March  1800   599   270   518  0.521    63   176   0.358   118   149
    ## # … with 27 more variables: ft_pct <dbl>, oreb <int>, dreb <int>, reb <int>,
    ## #   ast <int>, stl <int>, blk <int>, to <int>, pf <int>, pts <int>,
    ## #   second_chance_pts <dbl>, second_chance_fgm <dbl>, second_chance_fga <dbl>,
    ## #   second_change_fg_pct <dbl>, pts_in_paint <dbl>, pts_in_paint_fgm <dbl>,
    ## #   pts_in_paint_fga <dbl>, pts_in_paint_fg_pct <dbl>, fast_brk_pts <dbl>,
    ## #   fast_brk_fgm <dbl>, fast_brk_fga <dbl>, fast_brk_fg_pct <dbl>,
    ## #   bench_pts <dbl>, pts_tov <dbl>, games <int>, wins <int>, losses <int>

### Pull player statistics

Calling `bart_player_season` will return detailed season-long player
stats. To pull per-game averages for Duke players:

``` r
head(bart_player_season(team='Duke', stat='box'))
```

    ## ── Player Season Stats ───────────────────────────────────────── toRvik 1.1.0 ──

    ## ℹ Data updated: 2022-09-08 21:16:53 EDT

    ## # A tibble: 6 × 21
    ##   player      pos   exp   hgt   team  conf      g   mpg   ppg fg_pct  oreb  dreb
    ##   <chr>       <chr> <chr> <chr> <chr> <chr> <int> <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1 Paolo Banc… Wing… Fr    6-10  Duke  ACC      39  33.0 17.2   0.513 1.77   6.05
    ## 2 Wendell Mo… Comb… Jr    6-5   Duke  ACC      39  33.9 13.4   0.513 1.21   4.05
    ## 3 Trevor Kee… Comb… Fr    6-4   Duke  ACC      36  30.2 11.5   0.422 0.833  2.61
    ## 4 Mark Willi… C     So    7-0   Duke  ACC      39  23.6 11.2   0.782 2.59   4.87
    ## 5 AJ Griffin  Wing… Fr    6-6   Duke  ACC      39  24.3 10.4   0.503 0.769  3.15
    ## 6 Jeremy Roa… Comb… So    6-1   Duke  ACC      39  29    8.62  0.412 0.359  2.05
    ## # … with 9 more variables: rpg <dbl>, apg <dbl>, tov <dbl>, ast_to <dbl>,
    ## #   spg <dbl>, bpg <dbl>, num <dbl>, year <int>, id <int>

Calling `bart_player_game` will return detailed game-by-game player
stats. To pull advance splits by game for Duke players:

``` r
head(bart_player_game(team='Duke', stat='advanced'))
```

    ## ── Player Game Stats ─────────────────────────────────────────── toRvik 1.1.0 ──

    ## ℹ Data updated: 2022-09-08 21:16:53 EDT

    ## # A tibble: 6 × 25
    ##   date        year player exp   team  conf  opp   result   min   pts   usg  ortg
    ##   <chr>      <dbl> <chr>  <chr> <chr> <chr> <chr> <chr>  <dbl> <dbl> <dbl> <dbl>
    ## 1 2021-11-09  2022 Theo … Sr    Duke  ACC   Kent… W         22     5  14.3  98.7
    ## 2 2021-11-12  2022 Theo … Sr    Duke  ACC   Army  W         15     0  11.9  18.1
    ## 3 2021-11-13  2022 Theo … Sr    Duke  ACC   Camp… W         10     0   1.9 236  
    ## 4 2021-11-16  2022 Theo … Sr    Duke  ACC   Gard… W         15     4  19.1 100. 
    ## 5 2021-11-19  2022 Theo … Sr    Duke  ACC   Lafa… W         17     4  12.2 122. 
    ## 6 2021-11-22  2022 Theo … Sr    Duke  ACC   The … W         16     8  11.6 207. 
    ## # … with 13 more variables: or_pct <dbl>, dr_pct <dbl>, ast_pct <dbl>,
    ## #   to_pct <dbl>, stl_pct <dbl>, blk_pct <dbl>, bpm <dbl>, obpm <dbl>,
    ## #   dbpm <dbl>, net <dbl>, poss <dbl>, id <dbl>, game_id <chr>

### Pull transfer histories

Calling `transfer_portal` will return transfer histories with matching
player IDs to join with other statistics. To find all players who
transferred to Duke:

``` r
head(transfer_portal(to='Duke'))
```

    ## ── Transfer Portal ───────────────────────────────────────────── toRvik 1.1.0 ──

    ## ℹ Data updated: 2022-09-08 21:16:53 EDT

    ## # A tibble: 6 × 11
    ##      id player        from  to    exp    year imm_e…¹ source from_d1 to_d1 sit  
    ##   <int> <chr>         <chr> <chr> <chr> <int> <chr>   <chr>  <lgl>   <lgl> <lgl>
    ## 1 65776 Kale Catchin… Harv… Duke  Sr     2023 Yes     Josep… TRUE    FALSE NA   
    ## 2 66209 Ryan Young    Nort… Duke  Jr     2023 Yes     Jeff … TRUE    FALSE NA   
    ## 3 65826 Max Johns     Prin… Duke  Sr     2023 Yes     <NA>   TRUE    FALSE NA   
    ## 4 50593 Theo John     Marq… Duke  Sr     2022 Yes     <NA>   TRUE    TRUE  FALSE
    ## 5 51179 Bates Jones   Davi… Duke  Sr     2022 Yes     <NA>   TRUE    TRUE  FALSE
    ## 6 45926 Patrick Tape  Colu… Duke  Sr     2021 Yes     Evan … TRUE    TRUE  TRUE 
    ## # … with abbreviated variable name ¹​imm_elig

### Pull recruiting rankings

Calling `player_recruiting_rankings` will return extensive recruit
histories with matching player IDs. To find all 5-star players who
played high school basketball in North Carolina:

``` r
head(player_recruiting_rankings(stars=5, state='NC'))
```

    ## ── Recruiting Rankings ───────────────────────────────────────── toRvik 1.1.0 ──

    ## ℹ Data updated: 2022-09-08 21:16:54 EDT

    ## # A tibble: 6 × 31
    ##   position player  height weight team  conf  high_…¹ town  state tfs_c…² tfs_c…³
    ##   <chr>    <chr>   <chr>   <dbl> <chr> <chr> <chr>   <chr> <chr>   <dbl>   <int>
    ## 1 SF       Patric… 6-6       215 Flor… ACC   West C… Char… Nort…    99.1       5
    ## 2 PG       Devon … 6-2       185 Kans… B12   Provid… Char… Nort…    99.3       5
    ## 3 SF       Jaylen… 6-8       215 Wake… ACC   Wesley… High… Nort…    99.3       5
    ## 4 SG       Coby W… 6-5       185 Nort… ACC   Greenf… Wils… Nort…    99.1       5
    ## 5 PF       Harry … 6-10      240 Duke  ACC   Oak Hi… Wins… Nort…   100.        5
    ## 6 PG       Dennis… 6-3       190 Nort… ACC   Trinit… Faye… Nort…    99.7       5
    ## # … with 20 more variables: tfs_comp_national <dbl>, tfs_comp_position <dbl>,
    ## #   tfs_comp_state <dbl>, tfs_rating <dbl>, tfs_star <int>, espn_rating <dbl>,
    ## #   espn_grade <dbl>, espn_rank <dbl>, rivals_rating <dbl>, rivals_rank <dbl>,
    ## #   avg_rank <dbl>, num_offers <int>, announce_date <chr>, tfs_cb <chr>,
    ## #   tfs_cb_odds <dbl>, tfs_cb_alt <chr>, tfs_cb_alt_odds <dbl>, tfs_pid <int>,
    ## #   year <int>, id <int>, and abbreviated variable names ¹​high_school,
    ## #   ²​tfs_comp_rating, ³​tfs_comp_star

### Predict games and tournaments

Calling `bart_game_predictions` will returns expected points,
possessions, and win percentage for a given game on a given date. To
simulate North Carolina at Duke in mid-January:

``` r
bart_game_prediction('Duke', 'North Carolina', '20220113', location = 'H')
```

    ## ── Duke vs. North Carolina Prediction ────────────────────────── toRvik 1.1.0 ──

    ## ℹ Data updated: 2022-09-08 21:16:54 EDT

    ## # A tibble: 2 × 8
    ##   team           date          location tempo   ppp   pts win_per did_win
    ##   <chr>          <chr>         <chr>    <dbl> <dbl> <dbl>   <dbl> <lgl>  
    ## 1 Duke           Jan. 13, 2022 Home      73.4  1.15  84.2    73.1 TRUE   
    ## 2 North Carolina Jan. 13, 2022 Away      73.4  1.02  75.1    26.9 FALSE

Calling `bart_tournament_prediction` will simulate a single-elimination
tournament between a group of teams on a given date. To simulate the
2022 Final Four 25 times:

``` r
bart_tournament_prediction(teams = c('Duke', 'North Carolina', 'Kansas', 'Villanova'), '20220402', sims = 25, seed = 10)
```

    ## ── Tournament Prediction: 25 Sims ────────────────────────────── toRvik 1.1.0 ──

    ## ℹ Data updated: 2022-09-08 21:16:55 EDT

    ## # A tibble: 4 × 4
    ##   team            wins finals champ
    ##   <chr>          <int>  <int> <int>
    ## 1 Duke              31     21    10
    ## 2 Kansas            17     10     7
    ## 3 Villanova         21     15     6
    ## 4 North Carolina     6      4     2

## Documentation

For more information on the package and its functions, please see the
[**`toRvik`** reference](https://www.torvik.dev/).

## The Author

[Andrew Weatherman](https://www.linkedin.com/in/andrewweatherman/)

<a href="https://twitter.com/andreweatherman" target="blank"><img src="https://img.shields.io/twitter/follow/andreweatherman?color=blue&amp;label=%40andreweatherman&amp;logo=twitter&amp;style=for-the-badge" alt="@andreweatherman"/></a>
