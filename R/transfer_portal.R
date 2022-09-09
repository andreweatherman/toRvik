#' Get Transfer Portal
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
#' @param from Team player is transferring from
#' @param to Team player is transferring to
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_abort
#' @examples
#' transfer_portal(to='Charlotte')
#'
#' @export

transfer_portal <- function(year = NULL, from = NULL, to = NULL) {

  # test passed year
  if (!is.null(year) & !(is.numeric(year) && nchar(year) == 4 && year >= 2013)) {
    cli::cli_abort(c(
      "{.var year} must be 2013 or later",
      "x" = "You passed through {year}"
    ))
  }

  base_url <- 'https://api.cbbstat.com/players/transfers?'
  parsed <- httr::modify_url(
    base_url,
    query = list(
      year = year,
      from = from,
      to = to
    )
  )
  data <- data.frame()

  tryCatch(
    expr = {
      data  <- jsonlite::fromJSON(parsed) %>%
        make_toRvik_data('Transfer Portal', Sys.time())
    },
    error = function(e) {
      check_docs_error()
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(data)
}
