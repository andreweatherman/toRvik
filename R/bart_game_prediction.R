#' T-Rank Game Prediction
#'
#' Returns game predictions using Barttorvik's model
#'
#' Function returns projected score, points per possession, tempo, and win
#' percentage for a given match-up on a given day
#'
#' @returns Returns a tibble with eight columns: \describe{
#'   \item{\code{team}}{character.} \item{\code{date}}{date.}
#'   \item{\code{location}}{character.} \item{\code{tempo}}{double.}
#'   \item{\code{ppp}}{double.} \item{\code{pts}}{double.}
#'   \item{\code{win_per}}{double.} \item{\code{did_win}}{logical.} }
#' @param team Team for game
#' @param opp Opponent for game
#' @param date Date for game; YYYYMMDD format
#' @param location Location for game ('N', 'H', or 'A')
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @examples
#' bart_game_prediction(team='Duke', opp='North Carolina', date='20220402')
#'
#' @export

bart_game_prediction <- function(team = NULL, opp = NULL, date = NULL, location='N') {
  base_url <- 'https://api.cbbstat.com/games/predictions?'
  parsed <- httr::modify_url(
    base_url,
    query = list(
      team = team,
      opp = opp,
      date = date,
      location = location
    )
  )
  data <- jsonlite::fromJSON(parsed) %>% dplyr::as_tibble()
  return(data)
}
