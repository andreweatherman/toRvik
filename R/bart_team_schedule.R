#' Get Team Schedule
#'
#' Returns full team schedule and game-by-game four factor statistics by year
#'
#' For a brief explanation of each factor and its computation, please visit
#' \href{https://kenpom.com/blog/four-factors/}{KenPom's blog}. `lead_diff` is
#' the average lead or deficit during a game for the team.
#'
#' @returns Returns a tibble of game-by-game four factors by team
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
#' bart_team_schedule(year=2022, team='Duke')
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
