#' Get Player Season Splits
#'
#' Returns season-long player statistics on a variety of splits.
#'
#' Data is split on three statistical types, explained below: \describe{
#' \item{box}{Returns basic box score stats; sorts by ppg.}
#' \item{shooting}{Returns play-by-play shooting splits; sorts by ppg.}
#' \item{advanced}{Returns advanced metrics and possession-adjusted box score
#' statistics; sorts by recruiting rank.}} and data can be split by any
#' combination of three variables, explained below: \describe{
#' \item{result} \item{month} \item{game}{Split by game type}}
#'
#' Function pulls detailed season-long player statistic by a variety
#' of splits.
#'
#' @returns Returns a tibble with the number of columns dependent on the value
#'   supplied to the `stat` argument.
#' @param year Year to filter.
#' @param split Split to filter (see details).
#' @param team Team to filter.
#' @param conf Conference to filter.
#' @param exp Experience to filter.
#' @param player_id Unique player ID to filter.
#' @param type Data type to return (see details).
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @examples
#' \donttest{bart_player_splits(year=2022, conf='ACC')}
#'
#' @export
bart_player_splits <- function(year = NULL, split = NULL, team = NULL, conf = NULL, exp = NULL, player_id = NULL, type = NULL) {
  base_url <- 'https://api.cbbstat.com/players/season/stats/splits?'
  parsed <- httr::modify_url(
    base_url,
    query = list(
      year = year,
      split = split,
      team = team,
      conf = conf,
      exp = exp,
      player_id = player_id,
      type = type
    )
  )
  data <- jsonlite::fromJSON(parsed) %>% dplyr::as_tibble()
  return(data)
}
