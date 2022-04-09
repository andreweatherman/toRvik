# A Barttorvik Companion

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
## Package Highlights

- Game-by-game statistics by player for every D-1 game since 2008 (box, shooting, advanced) 
- Season-long statistics for every D-1 player since 2008 (box, shooting, advanced)
- Shooting splits + shares by team for every season since 2008
- Game box scores for every D-1 game since 2008
- Team and conference four factors on a variety of splits (date range, location, game type, opponent strength, and quad)
- Game-by-game four factors by team
- NCAA committee-style team sheets, including resume and quality metrics + quad records
- Every D-1 head coaching change since 2008

## Documentation

For more information on the package and its functions, please see
the [**`toRvik`** reference](https://www.torvik.dev/reference/).

## The Author

[Andrew Weatherman](https://www.linkedin.com/in/andrewweatherman/)

<a href="https://twitter.com/andreweatherman" target="blank"><img src="https://img.shields.io/twitter/follow/andreweatherman?color=blue&label=%40andreweatherman&logo=twitter&style=for-the-badge" alt="@andreweatherman" /></a>
