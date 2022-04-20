#' Get Team Shooting Splits
#'
#' Returns team shooting totals and shares by shot location. Data returns back
#' to 2010.
#'
#' `x_share` represents the percentage of made FGs that fall under that
#' category.
#'
#' @returns Returns a tibble with 20 columns:
#' \describe{
#'   \item{\code{team}}{character.}
#'   \item{\code{seed}}{double.}
#'   \item{\code{conf}}{character.}
#'   \item{\code{dunk_fg}}{character.}
#'   \item{\code{dunk_share}}{double.}
#'   \item{\code{dunk_fg_d}}{character.}
#'   \item{\code{dunk_share_d}}{double.}
#'   \item{\code{close_fg}}{character.}
#'   \item{\code{close_share}}{double.}
#'   \item{\code{close_fg_d}}{character.}
#'   \item{\code{close_share_d}}{double.}
#'   \item{\code{far_fg}}{character.}
#'   \item{\code{far_share}}{double.}
#'   \item{\code{far_fg_d}}{character.}
#'   \item{\code{far_share_d}}{double.}
#'   \item{\code{three_fg}}{character.}
#'   \item{\code{three_share}}{double.}
#'   \item{\code{three_fg_d}}{character.}
#'   \item{\code{three_share_d}}{double.}
#'   \item{\code{year}}{double.}
#'}
#' @param year Defaults to current season (YYYY).
#' @import dplyr
#' @import httr
#' @importFrom withr local_options
#' @importFrom cli cli_abort
#' @importFrom rvest read_html html_table
#' @importFrom purrr pluck
#' @importFrom tidyr separate
#' @importFrom magrittr %>%
#' @examples
#' bart_team_shooting(year=2019)
#' @export
bart_team_shooting <- function(year = current_season()) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    names <- c("team", "conf", "dunk_fg", "dunk_share", "dunk_fg_d", "dunk_share_d", "close_fg", "close_share", "close_fg_d", "close_share_d", "far_fg", "far_share", "far_fg_d", "far_share_d", "three_fg", "three_share", "three_fg_d", "three_share_d")
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
      2010)) {
      cli::cli_abort("Enter a valid year as a number (YYYY). Data only goes back to 2010!")
    } else {
      x <- httr::GET(paste0("https://barttorvik.com/teampbptot.php?year=", year)) %>%
        httr::content(as = "text") %>%
        rvest::read_html() %>%
        rvest::html_table() %>%
        purrr::pluck(1) %>%
        subset(select = -c(1, seq(7, 22, 5)))
      colnames(x) <- names
      x <- x[!(x$team == ""), ]
      x <- x %>%
        tidyr::separate(team, into = c("team", "seed"), sep = "(?<=\\D) (?=[0-9])") %>%
        dplyr::mutate(across(c(2, seq(5, 19, 2)), as.numeric), year = year)
      return(x)
    }
  })
}

#' Get Team History
#'
#' Returns efficiency and four factor finish, conference and overall records,
#' and basic shooting percentages by year back to 2008.
#'
#' `team` input must be an exact match: 'State' is abbreviated to 'St.' and full
#' names are used where applicable (e.g. 'North Carolina'). For complete list,
#' see `team` column of \code{\link{bart_ratings}}.
#'
#' @returns Returns a tibble with 23 columns:
#' \describe{
#'   \item{\code{year}}{double.}
#'   \item{\code{t_rk}}{double.}
#'   \item{\code{coach}}{character.}
#'   \item{\code{seed}}{double.}
#'   \item{\code{finish}}{character.}
#'   \item{\code{conf}}{character.}
#'   \item{\code{ov_rec}}{character.}
#'   \item{\code{conf_rec}}{character.}
#'   \item{\code{adj_o}}{double.}
#'   \item{\code{adj_d}}{double.}
#'   \item{\code{efg_o}}{double.}
#'   \item{\code{efg_d}}{double.}
#'   \item{\code{to_percent}}{double.}
#'   \item{\code{tod_percent}}{double.}
#'   \item{\code{or_percent}}{double.}
#'   \item{\code{dr_percent}}{double.}
#'   \item{\code{ftr}}{double.}
#'   \item{\code{ftrd}}{double.}
#'   \item{\code{ft_percent}}{double.}
#'   \item{\code{two_pct}}{double.}
#'   \item{\code{two_pct_d}}{double.}
#'   \item{\code{three_pct}}{double.}
#'   \item{\code{adj_t}}{double.}
#' }
#' @param team Indicates team to return.
#' @import dplyr
#' @import httr
#' @import janitor
#' @import  readr
#' @importFrom withr local_options
#' @importFrom rvest read_html html_table
#' @importFrom purrr pluck
#' @importFrom tidyr separate
#' @importFrom magrittr %>%
#' @examples
#' bart_team_history(team='Charlotte')
#' @export
bart_team_history <- function(team) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if(grepl('  ', team)){
      cli::cli_abort("Check spacing in team name; use just one space to separate.")
    }
    team <- gsub(" ", "+", team)
    x <- httr::GET(paste0("https://barttorvik.com/team-history.php?team=", team)) %>%
      httr::content(as = "text") %>%
      rvest::read_html() %>%
      rvest::html_table() %>%
      purrr::pluck(1) %>%
      janitor::clean_names() %>%
      tidyr::separate(coach, into = c("coach", "more"), sep = "(?<=[a-z.])\\s*(?=[0-9])") %>%
      tidyr::separate(more, into = c("seed", "finish"), sep = ",") %>%
      tidyr::separate(rec, into = c("ov_rec", "conf_rec"), sep = "[\\(\\)]") %>%
      dplyr::mutate_at(c(5, 7), funs(trimws(.))) %>%
      dplyr::mutate(
        seed = readr::parse_number(seed),
        across(c(1, 2, 4, 9:22), as.numeric)
      ) %>%
      dplyr::rename(
        "two_pct" = 20,
        "two_pct_d" = 21,
        "three_pct" = 22
      )
    return(x)
  })
}


#' Get Team Box Stats
#'
#' Returns team box totals and per-game averages by game type back to 2008.
#'
#' Columns ending in 'pg' indicate a per-game average. Unless specified by
#' `type=d1`, results include games against non-Division 1 opponents.
#'
#' The `type` argument splits the results by game type, explained below:
#' \describe{ \item{all}{All games played.} \item{nc}{Non-conference games.}
#' \item{conf}{In-conference games.} \item{post}{Post-conference tournament
#' games.} \item{d1}{Games against D-1 teams only.} \item{nond1}{Games involving
#' one non-D1 team.} }
#'
#' @returns Returns a tibble of team box totals and per-game averages
#' @param year Defaults to current season (YYYY).
#' @param type Filters by game type; defaults to `all`.
#' @import dplyr
#' @importFrom tidyr gather spread
#' @importFrom magrittr %>%
#' @importFrom cli cli_abort
#' @examples
#' \donttest{bart_team_box(type='conf')}
#' @export
bart_team_box <- function(year=current_season(), type='all') {
  suppressMessages({
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
          2008)) {
      cli::cli_abort("Enter a valid year as a number (YYYY). Data only goes back to 2008!")
    }
    if (!(type %in% c('all', 'nc', 'conf', 'conf_t', 'post', 'd1', 'nond1'))) {
      cli::cli_abort("Please input a valid type value (see details)")
    }
  teams <- toRvik::bart_season_schedule(year=year) %>%
            dplyr::filter(type != 'nond1') %>%
            dplyr::select(home) %>%
            dplyr::distinct()
  if (type=='all') {
    x <- toRvik::bart_game_box(year=current_season()) %>%
      dplyr::select(3:34, 38)
    x <- tidyr::gather(x, key='key', value='value', -team1, -team2, -game_id) %>%
      dplyr::mutate(team = dplyr::case_when(grepl("team1", key)~team1,
                                            grepl("team2", key)~team2),
                    key=gsub(".*_", "", key)) %>%
      dplyr::select(3:6) %>%
      tidyr::spread(key, value) %>%
      dplyr::group_by(team) %>%
      dplyr::summarize(dplyr::across(where(is.double), sum, na.rm=TRUE))
    x <- dplyr::inner_join(x, teams, by=c('team'='home'))
    y <- toRvik::bart_season_schedule(year=year) %>%
      dplyr::select(home, away) %>%
      tidyr::gather("loc", "team", home, away) %>%
      dplyr::count(team, name='games')
  }
  else if (type=='d1') {
    x <- dplyr::inner_join(toRvik::bart_game_box(year=year), (toRvik::bart_season_schedule(year=year) %>% dplyr::filter(type != 'nond1')), by='game_id') %>%
      dplyr::select(3:34, 38)
    x <- tidyr::gather(x, key='key', value='value', -team1, -team2, -game_id) %>%
      dplyr::mutate(team = dplyr::case_when(grepl("team1", key)~team1,
                                            grepl("team2", key)~team2),
                    key=gsub(".*_", "", key)) %>%
      dplyr::select(3:6) %>%
      tidyr::spread(key, value) %>%
      dplyr::group_by(team) %>%
      dplyr::summarize(dplyr::across(where(is.double), sum, na.rm=TRUE))
    x <- dplyr::inner_join(x, teams, by=c('team'='home'))
    y <- toRvik::bart_season_schedule(year=year) %>%
      dplyr::filter(type != 'nond1') %>%
      dplyr::select(home, away) %>%
      tidyr::gather("loc", "team", home, away) %>%
      dplyr::count(team, name='games')
  }
  else {
    x <- dplyr::inner_join(toRvik::bart_game_box(year=year), (toRvik::bart_season_schedule(year=year) %>% dplyr::filter(type== !!type)), by='game_id') %>%
          dplyr::select(3:34, 38)
    x <- tidyr::gather(x, key='key', value='value', -team1, -team2, -game_id) %>%
      dplyr::mutate(team = dplyr::case_when(grepl("team1", key)~team1,
                                            grepl("team2", key)~team2),
                    key=gsub(".*_", "", key)) %>%
      dplyr::select(3:6) %>%
      tidyr::spread(key, value) %>%
      dplyr::group_by(team) %>%
      dplyr::summarize(dplyr::across(where(is.double), sum, na.rm=TRUE))
    x <- dplyr::inner_join(x, teams, by=c('team'='home'))
    y <- toRvik::bart_season_schedule(year=year) %>%
      dplyr::filter(type== !!type) %>%
      dplyr::select(home, away) %>%
      tidyr::gather("loc", "team", home, away) %>%
      dplyr::count(team, name='games')
  }
  x <- dplyr::inner_join(x, y, by='team')
  x <- x %>% dplyr::mutate(fg_pct=fgm/fga,
                           tp_pct=tpm/tpa,
                           ft_pct=ftm/fta,
                           rpg=(oreb+dreb)/games,
                           apg=ast/games,
                           spg=stl/games,
                           bpg=blk/games,
                           tpg=to/games,
                           fpg=pf/games,
                           ppg=pts/games, .before=games) %>%
            dplyr::arrange(desc(ppg))

  return(x)
  })}

