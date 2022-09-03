#' Get Player Game Stats
#'
#' Returns detailed game-by-game player statistics on a variety of splits.
#'
#' Data is split on three statistical types, explained below: \describe{
#' \item{box}{Returns basic box score stats; sorts by ppg.}
#' \item{shooting}{Returns play-by-play shooting splits; sorts by ppg.}
#' \item{advanced}{Returns advanced metrics and possession-adjusted box score
#' statistics; sorts by recruiting rank.}}
#'
#' @returns Returns a tibble with the number of columns dependent on the value
#'   supplied to the `stat` argument.
#' @param year Year to filter.
#' @param stat Indicates statistical split (see details).
#' @param game_id Game to filter.
#' @param player_id Player to filter.
#' @param exp Player experience to filter.
#' @param team Team to filter.
#' @param conf Conference to filter.
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @examples
#' \donttest{bart_player_game(year=2022, stat='box')}
#'
#' @export
bart_player_game <- function(year = NULL, stat = NULL, game_id = NULL, player_id = NULL, exp = NULL, team = NULL, conf = NULL) {
  base_url <- 'https://api.cbbstat.com/players/game/stats?'
  parsed <- httr::modify_url(
    base_url,
    query = list(
      year = year,
      type = stat,
      game_id = game_id,
      player_id = player_id,
      exp = exp,
      team = team,
      conf = conf
    )
  )
  data <- jsonlite::fromJSON(parsed) %>% dplyr::as_tibble()
  return(data)
}
