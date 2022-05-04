# toRvik 1.0.3
**Patches:**

- Fixed [data issue](https://github.com/andreweatherman/toRvik/issues/4) that casued `bart_player_season` to fail on row binds across certain seasons

**Code Updates:**

- Added [`bart_pro`](https://www.torvik.dev/reference/bart_pro.php) to to pull statistics for draft-eligible players

**Other:**

- Moved package site to self-host on AWS

# toRvik 1.0.2 

**Addressed CRAN submission comments:**

- Omitted 'Functions' from package title
- Provided links to utilized web services in DESCRIPTION
- Changed logical variables package-wide from 'T' and 'F' to 'TRUE' and 'FALSE'
- Added `\values{}` field to function documentation

**Code Updates:**

- Added support for pulling complete player statistics by setting stat argument to 'all' in `bart_player_season`, `bart_player_game`, and `bart_transfers` (new default)
- Transitioned to using [`switch()`](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/switch) for lookup values in several functions
- Moved examples with >5s run time to `\donttest{}` to keep output present on site while passing CRAN checks
- Added WAB (wins above bubble) column to `bart_factors` to keep consistent with output from analogous `bart_conf_factors`
- Switched neutral variable in `bart_season_schedule` to logical type

**Other:**

- Site refresh for CRAN release

# toRvik 1.0.1
**Initial CRAN submission rejected; fixed with 1.0.2**

**Patches:**

- Fixed [package-breaking user agent issue](https://github.com/andreweatherman/toRvik/issues/1) that threw HTTP 403 error on Windows machines by using [`withr`](https://withr.r-lib.org)
- Fixed [function-breaking file issue](https://github.com/andreweatherman/toRvik/issues/3) that caused `bart_player_game` to fail

**Code Updates:**

- Added [`bart_team_box`](https://www.torvik.dev/reference/bart_team_box.php) to return team box statistics and per-game averages  
- Added argument to filter `bart_transfers` by [active portal players and committed ones](https://github.com/andreweatherman/toRvik/issues/2)

**Other:**

- Changed pkgdown site colors

# **toRvik 1.0.0**
Initial release.

For a detailed list of changes to `toRvik`, consult the commit history at https://github.com/andreweatherman/toRvik
