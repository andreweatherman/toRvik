# **toRvik** <a href="https://www.torvik.dev/"><img src="man/figures/logo.png" align="right" width="20%" min-width="100px"/></a>
 
<!-- badges: start -->
[![R-CMD-check](https://github.com/andreweatherman/toRvik/workflows/R-CMD-check/badge.svg)](https://github.com/andreweatherman/toRvik/actions)
 [![Codecov test coverage](https://codecov.io/gh/andreweatherman/toRvik/branch/main/graph/badge.svg)](https://app.codecov.io/gh/andreweatherman/toRvik?branch=main)
  <!-- badges: end -->

[**`toRvik`**](https://github.com/andreweatherman/toRvik) is an R package for working with and scraping men's college basketball data from [Barttorvik](https://barttorvik.com/). 
 
No subscription is required to access the data. The package includes functions for pulling player and team data, game results, advanced metric splits, play-by-play shooting, and more -- all returned in tibble format. As of version 1.0.1, `toRvik` ships with more than 20 functions.
   
## Package Installation
To install toRvik, run the following code inside your R session:
```r
if (!requireNamespace('devtools', quietly = TRUE)){
  install.packages('devtools')
}
devtools::install_github("andreweatherman/toRvik")
library(toRvik)
```
## **Package Highlights**

- Game-by-game statistics by player for every D-1 game since 2008 (box, shooting, advanced) 
- Season-long statistics for every D-1 player since 2008 (box, shooting, advanced)
- Shooting splits + shares by team for every season since 2008
- Game box scores for every D-1 game since 2008
- Team and conference four factors on a variety of splits (date range, location, game type, opponent strength, and quad)
- Game-by-game four factors by team
- NCAA committee-style team sheets, including resume and quality metrics + quad records
- Every D-1 head coaching change since 2008

## **Basic Uses**

All `toRvik` functions fall into one of five categories: 

- **Rating**, which pulls and slices T-Rank + four factor data on a variety of splits
- **Game**, which pulls team and season schedules + results 
- **Stat**, which pulls and slices player, team, and conference stats on a variety of levels and splits
- **Tournament**, which pulls raw and adjusted tournament performance + odds
- **Miscellaneous**

### Pull T-Rank ratings:

Calling `bart_ratings` will return the current T-Rank ranks and ratings.
```r
head(bart_ratings())
```

### Pull team statistics

Calling `bart_factors` will return four factor stats on a number of splits. To filter by home games, set venue to 'home.'
```r
head(bart_factors(venue='home'))
```

Calling `bart_team_box` will return team box totals and per-game averages by game type. To filter by in-conference games, set type to 'conf.'
``` r
head(bart_team_box(type='conf'))
```

### Pull player statistics

Calling `bart_player_season` will return detailed season-long player stats. To pull per-game averages, set stat to 'box.'
``` r
head(bart_player_season(stat='box'))
```
Calling `bart_player_game` will return detailed game-by-game player stats. To pull shooting splits by game, set stat to 'shooting.'
``` r
head(bart_player_game(stat='shooting'))
```
 
## Documentation

For more information on the package and its functions, please see
the [**`toRvik`** reference](https://www.torvik.dev/).

## The Author

[Andrew Weatherman](https://www.linkedin.com/in/andrewweatherman/)

<a href="https://twitter.com/andreweatherman" target="blank"><img src="https://img.shields.io/twitter/follow/andreweatherman?color=blue&label=%40andreweatherman&logo=twitter&style=for-the-badge" alt="@andreweatherman" /></a>
