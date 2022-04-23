# Initial CRAN Release 
## 1.0.2

The first [CRAN release of `toRvik`](https://cran.r-project.org/package=toRvik) went live on April 22, 2022, which followed the push of version 1.0.2. 

The updates for 1.0.2 were mostly cosmetic and in response to CRAN suggestions but included several minor code tweaks:

- Added support for pulling complete player statistics by setting stat argument to 'all' in `bart_player_season`, `bart_player_game`, and `bart_transfers` (new default)
- Added WAB (wins above bubble) column to `bart_factors` to keep consistent with output from analogous `bart_conf_factors` function
- Shifted to [`switch`](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/switch) for lookup values in several functions
- Switched neutral variable in `bart_season_schedule` to logical type

