#' Get Full Season Schedule
#'
#' Returns full season schedule for all D-1 teams by year
#'
#' Function pulls all games involving at least one D-1 team. Under the `type`
#' column, games are classified by one of four indicators, explained below:
#' \describe{ \item{nc}{Non-conference games between two D-1 teams.}
#' \item{conf}{In-conference games.} \item{post}{Post-conference tournament
#' games.} \item{nond1}{Games involving one non-D1 team.} }
#'
#' @param year Defaults to current season (YYYY).
#' @import dplyr
#' @import readr
#' @import lubridate
#' @importFrom withr local_options
#' @importFrom magrittr %>%
#' @importFrom cli cli_abort
#' @examples
#' \dontrun{bart_season_schedule(year=2022)}
#'
#' @export
bart_season_schedule <- function(year = current_season()) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
      2008)) {
      cli::cli_abort("Enter a valid year as a number (YYYY). Data only goes back to 2008!")
    } else {
      names <- c("date", "type", "neutral", "home", "away", "game_id")
      x <- readr::read_csv(paste0("https://barttorvik.com/", year, "_master_sked.csv"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr::select(c(2, 3, 4, 6, 5, 3, 1))
      colnames(x) <- names
      x <- x %>% dplyr::mutate(
        date = lubridate::mdy(date),
        type = dplyr::case_when(
          type == 0 ~ "nc",
          type == 1 ~ "conf",
          type == 2 ~ "conf_t",
          type == 3 ~ "post",
          TRUE ~ "nond1"
        ),
        neutral = dplyr::case_when(
          neutral == 1 ~ "yes",
          TRUE ~ "no"
        )
      )
      return(x)
    }
  })
}


#' Get Team Schedule
#'
#' Returns full team schedule and game-by-game four factor statistics by year
#'
#' For a brief explanation of each factor and its computation, please visit
#' \href{https://kenpom.com/blog/four-factors/}{KenPom's blog}. `lead_diff` is
#' the average lead or deficit during a game for the team.
#'
#' @param year Defaults to current season (YYYY).
#' @param team Team to return.
#' @import dplyr
#' @import jsonlite
#' @import lubridate
#' @importFrom withr local_options
#' @importFrom cli cli_abort
#' @importFrom tidyr separate
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{bart_team_schedule(year=2022, team='Duke')}
#'
#' @export
bart_team_schedule <- function(year=current_season(), team=NULL) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if(!(is.numeric(year) && nchar(year) == 4 && year >=
         2008)){
      cli::cli_abort('Enter a valid year as a number. Data only goes back to 2008!')
    }
    else {
      team <- gsub(" ","+", team)
      names <- c('date','type','team','conf','opp','loc','result','adj_o','adj_d','ppp','efg',
                 'to','or','ftr','def_ppp','def_efg','def_to','def_or','def_ftr','game_score',
                 'opp_conf','year','poss','game_id','coach','opp_coach','lead_diff')
      x <- jsonlite::fromJSON(paste0('https://barttorvik.com/getgamestats.php?year=', year,'&tvalue=',team)) %>%
        dplyr::as_tibble() %>%
        dplyr::select(-c(22, 28:30))
      colnames(x) <- names
      x <- x %>% dplyr::mutate(type=case_when(type==0~'nc',
                                              type==1~'conf',
                                              type==2~'conf_t',
                                              type==3~'post'),
                               date=lubridate::mdy(date)) %>%
        tidyr::separate(result, into=c('result','score'), sep=',') %>%
        tidyr::separate(score, into=c('win','loss'), sep='-') %>%
        dplyr::mutate(points=case_when(result=='W'~win,
                                       TRUE~loss),
                      opp_points=case_when(result=='W'~loss,
                                           TRUE~win),
                      across(c(8:22, 24:25, 29:31), as.numeric)) %>%
        dplyr::select(-c(8:9)) %>%
        dplyr::relocate(points, .after=result) %>%
        dplyr::relocate(opp_points, .after=points) %>%
        dplyr::relocate(opp_conf, .before=loc) %>%
        dplyr::relocate(lead_diff, .after=opp_points) %>%
        relocate(game_id, .after=last_col()) %>%
        arrange(desc(date))
      return(x) }
  }
  )}


#' Get Pregame Probabilities
#'
#' Returns pregame win probabilities, expected scores, and thrill quotients for
#' all games.
#'
#' Under the `type` column, games are classified by one of five indicators,
#' explained below: \describe{ \item{nc}{Non-conference games between two D-1
#' teams.} \item{conf}{In-conference games.} \item{conf_t}{Conference tournament
#' games.} \item{post}{Post-conference tournament games.} \item{nond1}{Games
#' involving one non-D1 team.} }
#'
#' @param year Defaults to current season (YYYY).
#' @import dplyr
#' @import readr
#' @import lubridate
#' @importFrom withr local_options
#' @importFrom cli cli_abort
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{bart_pregame(year=2022)}
#'
#' @export
bart_pregame <- function(year=current_season()) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if(!(is.numeric(year) && nchar(year) == 4 && year >=
         2008)){
      cli::cli_abort('Enter a valid year as a number. Data only goes back to 2008!')
    }
    else {
      names <- c('date','conf','line','ttq','type', 'team1','team1_wp','team1_pts','team2','team2_wp','team2_pts','game_id')
      x <- readr::read_csv(paste0('https://barttorvik.com/',year,'_super_sked.csv'), col_names=FALSE, show_col_types = FALSE) %>%
        dplyr::select(2,3,5:7,9,53,14,15,54,20,1)
      colnames(x) <- names
      x <- x %>% dplyr::mutate(type=case_when(type==0~'nc',
                                              type==1~'conf',
                                              type==2~'conf_t',
                                              type==3~'post',
                                              type==99~'nond1'),
                               date=lubridate::mdy(date),
                               across(c(4,7,8,10,11), as.numeric),
                               year=year)
      return(x) }
  })}

#' Get Team Game Stats
#'
#' Returns box score data by team for all games.
#'
#' The home team is coded as `team2`. Neutral site games may contain errors as
#' to whom is the home team.
#'
#' @param year Defaults to current season (YYYY).
#' @import dplyr
#' @import jsonlite
#' @import lubridate
#' @importFrom withr local_options
#' @importFrom cli cli_abort
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{bart_game_box(year=2022)}
#'
#' @export
bart_game_box <- function(year = current_season()) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    abbrev <- c("fgm", "fga", "tpm", "tpa", "ftm", "fta", "oreb", "dreb", "reb", "ast", "stl", "blk", "to", "pf", "pts")
    names <- c("game_id", "date", "min", "team1", "team2", "pos", "win", "loss")
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
      2008)) {
      cli::cli_abort("Enter a valid year as a number. Data only goes back to 2008!")
    }
    if (dplyr::between(year, 2009, 2014)) {
      x <- jsonlite::fromJSON(paste0("https://barttorvik.com/", year, "_season.json"))
      x <- dplyr::as_tibble(t(sapply(x, `length<-`, max(lengths(x)))))
      x <- x %>%
        dplyr::select(-c(37, 40)) %>%
        dplyr::rename_at(c(1:5, 36:38), ~ paste0(names)) %>%
        dplyr::rename_at(6:20, ~ paste0("team1_", abbrev)) %>%
        dplyr::rename_at(21:35, ~ paste0("team2_", abbrev)) %>%
        dplyr::relocate(game_id, .after = loss) %>%
        dplyr::mutate(
          date = lubridate::mdy(date),
          across(c(2, 5:35), as.numeric)
        ) %>%
        dplyr::arrange(desc(date))
      return(x)
    } else {
      x <- jsonlite::fromJSON(paste0("https://barttorvik.com/", year, "_season.json")) %>%
        dplyr::as_tibble()
      x <- x %>%
        dplyr::select(-37) %>%
        dplyr::rename_at(c(1:5, 36:38), ~ paste0(names)) %>%
        dplyr::rename_at(6:20, ~ paste0("team1_", abbrev)) %>%
        dplyr::rename_at(21:35, ~ paste0("team2_", abbrev)) %>%
        dplyr::relocate(game_id, .after = loss) %>%
        dplyr::mutate(
          date = lubridate::mdy(date),
          across(c(2, 5:35), as.numeric)
        ) %>%
        dplyr::arrange(desc(date))
      return(x)
    }
  })
}

#' Get Game Four Factors
#'
#' Returns game-by-game four factor statistics.
#'
#' For a brief explanation of each factor and its computation, please visit
#' \href{https://kenpom.com/blog/four-factors/}{KenPom's blog}. `avg_marg` and
#' `opp_avg_marg` is the the average lead or deficit during a game.
#'
#' @param year Defaults to current season (YYYY).
#' @import dplyr
#' @import readr
#' @import lubridate
#' @importFrom withr local_options
#' @importFrom cli cli_abort
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{bart_game_factors(year=2022)}
#'
#' @export
bart_game_factors <- function(year = current_season()) {
  suppressWarnings({
  withr::local_options(HTTPUserAgent='toRvik Package')
  if (!(is.numeric(year) && nchar(year) == 4 && year >=
    2008)) {
    cli::cli_abort("Enter a valid year as a number (YYYY). Data only goes back to 2008!")
  } else {
    names <- c(
      "date", "type", "team", "conf", "opp", "loc", "result", "adj_o", "adj_d", "off_ppp", "off_efg", "off_to", "off_or", "off_ftr", "def_ppp",
      "def_efg", "def_to", "def_or", "def_ftr", "game_score", "opp_conf", "season", "tempo", "game_id", "coach", "opp_coach", "avg_marg", "opp_avg_marg"
    )
    x <- readr::read_csv(paste0("https://barttorvik.com/getgamestats.php?year=", year, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>% dplyr::select(-c(22, 30:31))
    colnames(x) <- names
    x <- x %>%
      dplyr::mutate(
        date = lubridate::mdy(date),
        type = dplyr::case_when(
          type == 0 ~ "nc",
          type == 1 ~ "conf",
          type == 2 ~ "conf_t",
          type == 3 ~ "post",
          TRUE ~ "nond1"
        )
      ) %>%
      dplyr::relocate("opp_conf", .after = "opp") %>%
      dplyr::relocate("avg_marg", .after = "result") %>%
      dplyr::relocate("opp_avg_marg", .after = "avg_marg") %>%
      dplyr::arrange(desc(date))
    return(x)
  }
})}
