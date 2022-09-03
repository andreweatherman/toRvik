#' Transfer Portal
#'
#' Returns transfer portal decisions by year
#'
#' Function pulls destination decisions by players in the transfer portal back
#' to 2012
#'
#' @returns Returns a tibble with 31 columns: \describe{
#'   \item{\code{id}}{integer.} \item{\code{player}}{character.}
#'   \item{\code{from}}{character.} \item{\code{to}}{character.}
#'   \item{\code{exp}}{character.} \item{\code{year}}{integer.}
#'   \item{\code{imm_elig}}{character.} \item{\code{source}}{character.}
#'   \item{\code{from_d1}}{logical.} \item{\code{to_d1}}{logical}
#'   \item{\code{sit}}{logical.}}
#' @param year Year to filter
#' @param from Team player is transfering from
#' @param to Team player is transfering to
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @examples
#' transfer_portal(to='Duke')
#'
#' @export

transfer_portal <- function(year = NULL, from = NULL, to = NULL) {
  base_url <- 'https://api.cbbstat.com/players/transfers?'
  parsed <- httr::modify_url(
    base_url,
    query = list(
      year = year,
      from = from,
      to = to
    )
  )
  data <- jsonlite::fromJSON(parsed) %>% dplyr::as_tibble()
  return(data)
}
