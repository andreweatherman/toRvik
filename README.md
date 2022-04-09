
<!-- README.md is generated from README.Rmd. Please edit that file -->

\#toRvik
<a href="https://www.torvik.dev/"><img src="man/figures/logo.png" align="right" width="20%" min-width="100px"/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/andreweatherman/toRvik/workflows/R-CMD-check/badge.svg)](https://github.com/andreweatherman/toRvik/actions)
[![Codecov test
coverage](https://codecov.io/gh/andreweatherman/toRvik/branch/main/graph/badge.svg)](https://app.codecov.io/gh/andreweatherman/toRvik?branch=main)
<!-- badges: end -->

[**`toRvik`**](https://github.com/andreweatherman/toRvik) is an R
package for working with and scraping men’s college basketball data from
[Barttorvik](https://barttorvik.com/).

No subscription is required to access the data. The package includes
functions for pulling player and team data, game results, advanced
metric splits, play-by-play shooting, and more – all returned in tibble
format. As of version 1.0.1, `toRvik` ships with more than 20 functions.

## Package Installation

To install toRvik, run the following code inside your R session:

``` r
if (!requireNamespace('devtools', quietly = TRUE)){
  install.packages('devtools')
}
devtools::install_github("andreweatherman/toRvik")
```

    ## Skipping install of 'toRvik' from a github remote, the SHA1 (a9121f88) has not changed since last install.
    ##   Use `force = TRUE` to force installation

``` r
library(toRvik)
```

## Package Highlights

-   Game-by-game statistics by player for every D-1 game since 2008
    (box, shooting, advanced)
-   Season-long statistics for every D-1 player since 2008 (box,
    shooting, advanced)
-   Shooting splits + shares by team for every season since 2008
-   Game box scores for every D-1 game since 2008
-   Team and conference four factors on a variety of splits (date range,
    location, game type, opponent strength, and quad)
-   Game-by-game four factors by team
-   NCAA committee-style team sheets, including resume and quality
    metrics + quad records
-   Every D-1 head coaching change since 2008

## Basic Uses

All `toRvik` functions fall into one of five categories:

-   **Rating**, which pulls and slices T-Rank + four factor data on a
    variety of splits
-   **Game**, which pulls team and season schedules + results
-   **Stat**, which pulls and slices player, team, and conference stats
    on a variety of levels and splits
-   **Tournament**, which pulls raw and adjusted tournament
    performance + odds
-   **Miscellaneous**

### Pull T-Rank ratings:

Calling `bart_ratings` will return the current T-Rank ranks and ratings.

``` r
head(bart_ratings())
```

    ## # A tibble: 6 × 19
    ##   team     conf  barthag barthag_rk adj_o adj_o_rk adj_d adj_d_rk adj_t adj_t_rk
    ##   <chr>    <chr>   <dbl>      <int> <dbl>    <int> <dbl>    <int> <dbl>    <int>
    ## 1 Gonzaga  WCC     0.966          1  120.        4  89.9        9  72.6        5
    ## 2 Houston  Amer    0.959          2  117.       10  88.5        6  63.7      336
    ## 3 Kansas   B12     0.958          3  120.        5  91.3       13  69.1       71
    ## 4 Texas T… B12     0.951          4  111.       41  85.4        1  66.3      223
    ## 5 Baylor   B12     0.949          5  118.        8  91.3       14  67.6      149
    ## 6 Duke     ACC     0.944          6  123.        1  96.0       53  67.4      161
    ## # … with 9 more variables: wab <dbl>, nc_elite_sos <dbl>, nc_fut_sos <dbl>,
    ## #   nc_cur_sos <dbl>, ov_elite_sos <dbl>, ov_fut_sos <dbl>, ov_cur_sos <dbl>,
    ## #   seed <int>, year <dbl>

### Pull team statistics

Calling `bart_factors` will return four factor stats on a number of
splits. To filter by home games, set venue to ‘home.’

``` r
head(bart_factors(venue='home'))
```

    ## # A tibble: 6 × 21
    ##   team       conf  barthag rec    wins games adj_t adj_o off_efg off_to off_or
    ##   <chr>      <chr>   <dbl> <chr> <dbl> <dbl> <dbl> <dbl>   <dbl>  <dbl>  <dbl>
    ## 1 Houston    Amer    0.978 16–1     16    17  65.8  116.    54.3   16.5   39.1
    ## 2 Baylor     B12     0.968 15–2     15    17  69.3  118.    55.1   17.6   38.1
    ## 3 Gonzaga    WCC     0.966 16–0     16    16  73.1  121.    60     15.3   31.8
    ## 4 Texas Tech B12     0.965 18–0     18    18  68.2  117.    57     19.4   38.3
    ## 5 Auburn     SEC     0.961 16–0     16    16  72.5  116.    52.6   16.3   32.5
    ## 6 Tennessee  SEC     0.959 16–0     16    16  68.9  113.    53     18.3   37.5
    ## # … with 10 more variables: off_ftr <dbl>, adj_d <dbl>, def_efg <dbl>,
    ## #   def_to <dbl>, def_or <dbl>, def_ftr <dbl>, year <dbl>, venue <chr>,
    ## #   type <chr>, quad <chr>

Calling `bart_team_box` will return team box totals and per-game
averages by game type. To filter by in-conference games, set type to
‘conf.’

``` r
head(bart_team_box(type='conf'))
```

    ## # A tibble: 6 × 27
    ##   team     ast   blk  dreb   fga   fgm   fta   ftm  oreb    pf   pts   reb   stl
    ##   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 Gonza…   266    83   471   912   480   238   182   121   218  1271   592   101
    ## 2 South…   267    49   518  1024   562   361   286   106   271  1568   624   101
    ## 3 Oral …   214    77   570  1258   594   302   224   191   314  1619   761    85
    ## 4 Toledo   325    65   600  1256   626   349   270   202   300  1669   802   108
    ## 5 South…   200    49   336   795   383   360   279   115   242  1163   451   111
    ## 6 Bryant   249    68   507  1076   483   397   298   213   295  1411   720   105
    ## # … with 14 more variables: to <dbl>, tpa <dbl>, tpm <dbl>, fg_pct <dbl>,
    ## #   tp_pct <dbl>, ft_pct <dbl>, rpg <dbl>, apg <dbl>, spg <dbl>, bpg <dbl>,
    ## #   tpg <dbl>, fpg <dbl>, ppg <dbl>, games <int>

### Pull player statistics

Calling `bart_player_season` will return detailed season-long player
stats. To pull per-game averages, set stat to ‘box.’

``` r
head(bart_player_season(stat='box'))
```

    ## # A tibble: 6 × 20
    ##   player      pos   exp   hgt   team  conf      g   mpg   ppg fg_pct  oreb  dreb
    ##   <chr>       <chr> <chr> <chr> <chr> <chr> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1 Peter Kiss  Comb… Sr    6-5   Brya… NEC      27  35.7  25.2  0.484 1.44   4.33
    ## 2 Darius McG… Comb… Sr    5-9   Libe… ASun     30  33.7  24.6  0.445 0.454  4.03
    ## 3 Antoine Da… Scor… Sr    6-1   Detr… Horz     29  37.0  23.9  0.429 0.483  3.07
    ## 4 Keegan Mur… Stre… So    6-8   Iowa  B10      35  31.9  23.5  0.596 2.86   5.8 
    ## 5 Max Abmas   Comb… Jr    6-0   Oral… Sum      28  36.8  22.8  0.412 0.3    3.13
    ## 6 Bryce Hami… Wing… Sr    6-4   UNLV  MWC      31  32.3  21.9  0.439 0.688  4.31
    ## # … with 8 more variables: rpg <dbl>, apg <dbl>, ast_to <dbl>, spg <dbl>,
    ## #   bpg <dbl>, num <chr>, year <dbl>, id <dbl>

Calling `bart_player_game` will return detailed game-by-game player
stats. To pull advance splits by game, set stat to ‘adv.’

``` r
head(bart_player_game(stat='adv'))
```

    ## # A tibble: 6 × 24
    ##   date        year player       exp   team  opp   result   min   pts   usg  ortg
    ##   <date>     <dbl> <chr>        <chr> <chr> <chr> <chr>  <dbl> <dbl> <dbl> <dbl>
    ## 1 2021-11-09  2022 Jalen Colem… Sr    Kans… Mich… W         10     5  34.2  76.9
    ## 2 2021-11-12  2022 Jalen Colem… Sr    Kans… Tarl… W         12    10  11.7 144. 
    ## 3 2021-11-18  2022 Jalen Colem… Sr    Kans… Ston… W         15     7  12.4 186. 
    ## 4 2021-11-25  2022 Jalen Colem… Sr    Kans… Nort… W         10     0  10.3   0  
    ## 5 2021-11-26  2022 Jalen Colem… Sr    Kans… Dayt… L          8     0  16.3  25.5
    ## 6 2021-11-28  2022 Jalen Colem… Sr    Kans… Iona  W          5     0   3   215. 
    ## # … with 13 more variables: or_pct <dbl>, dr_pct <dbl>, ast_pct <dbl>,
    ## #   to_pct <dbl>, stl_pct <dbl>, blk_pct <dbl>, bpm <dbl>, obpm <dbl>,
    ## #   dbpm <dbl>, net <dbl>, poss <dbl>, id <dbl>, game_id <chr>

## Documentation

For more information on the package and its functions, please see the
[**`toRvik`** reference](https://www.torvik.dev/).

## The Author

[Andrew Weatherman](https://www.linkedin.com/in/andrewweatherman/)

<a href="https://twitter.com/andreweatherman" target="blank"><img src="https://img.shields.io/twitter/follow/andreweatherman?color=blue&label=%40andreweatherman&logo=twitter&style=for-the-badge" alt="@andreweatherman" /></a>
