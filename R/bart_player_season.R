#' Get Player Season Stats
#'
#' Returns detailed, season-long player statistics on a variety of splits.
#'
#' Data is split on three statistical types, explained below: \describe{
#' \item{box}{Returns basic box score stats; sorts by ppg.}
#' \item{shooting}{Returns play-by-play shooting splits; sorts by ppg.}
#' \item{advanced}{Returns advanced metrics and possession-adjusted box score
#' statistics; sorts by recruiting rank.}}
#'
#' @returns Returns a tibble with the number of columns dependent on the value
#'   supplied to the `stat` argument.
#' @param year Defaults to current season (YYYY).
#' @param stat Indicates statistical split (see details).
#' @param conf_only Logical. Filters data by conference-only play; defaults to
#'   `FALSE`.
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @examples
#' \donttest{bart_player_season(year=2019, stat='adv')}
#'
#' @export
bart_player_season <- function(year = NULL, id = NULL, team = NULL, stat = NULL) {

    base_url <- 'https://api.cbbstat.com/players/season/stats?'
    parsed <- httr::modify_url(
      base_url,
      query = list(
        year = year,
        id = id,
        team = team,
        type = stat
      )
    )
    data <- jsonlite::fromJSON(parsed) %>% dplyr::as_tibble()
    return(data)
  }
