#' Get Recruiting Rankings
#'
#' Returns player rankings for major recruiting services
#'
#' Function pulls high school meta data and recruiting rankings for individual
#' players from 247Sports, ESPN, and Rivals back to 2008
#'
#' @returns Returns a tibble with 31 columns: \describe{
#'   \item{\code{position}}{character.} \item{\code{player}}{character.}
#'   \item{\code{height}}{character.} \item{\code{weight}}{double.}
#'   \item{\code{team}}{character.} \item{\code{conf}}{character.}
#'   \item{\code{high_school}}{character.} \item{\code{town}}{character.}
#'   \item{\code{state}}{character.} \item{\code{tfs_comp_rating}}{double.}
#'   \item{\code{tfs_comp_star}}{integer.} \item{\code{tfs_comp_national}}{double.}
#'   \item{\code{tfs_comp_position}}{double.} \item{\code{tfs_comp_state}}{double.}
#'   \item{\code{tfs_rating}}{double.} \item{\code{tfs_star}}{integer.}
#'   \item{\code{espn_rating}}{double.} \item{\code{espn_grade}}{double.}
#'   \item{\code{espn_rank}}{double.} \item{\code{rivals_rating}}{double.}
#'   \item{\code{rivals_rank}}{double.} \item{\code{avg_rank}}{logical.}
#'   \item{\code{num_offers}}{integer,} \item{\code{announce_date}}{character.}
#'   \item{\code{tfs_cb}}{character.} \item{\code{tfs_cb_odds}}{double.}
#'   \item{\code{tfs_cb_alt}}{character.} \item{\code{tfs_cb_alt_odds}}{double.}
#'   \item{\code{tfs_pid}}{integer.} \item{\code{year}}{integer.}
#'   \item{\code{id}}{integer.}}
#' @param year Freshman season for class (class year + 1)
#' @param stars 247Sports Composite stars
#' @param state State abbreviation of recruit; `domestic` or `international`
#'   also taken
#' @param conf Conference of school committed to
#' @param team Team player committed to
#' @param top Top x in 247Sports Composite national rankings
#' @param pos 247Sports player position to filter for
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_abort
#' @examples
#' player_recruiting_rankings(year=2019, stars=5)
#'
#' @export

player_recruiting_rankings <- function(year = NULL, stars = NULL, state = NULL, conf = NULL, team = NULL, top = NULL, pos = NULL) {

  # test passed year
  if (!is.null(year) & !(is.numeric(year) && nchar(year) == 4 && year >= 2009)) {
    cli::cli_abort(c(
      "{.var year} must be 2009 or later",
      "x" = "You passed through {year}"
    ))
  }

  base_url <- 'https://api.cbbstat.com/players/recruits?'
  parsed <- httr::modify_url(
    base_url,
    query = list(
      year = year,
      stars = stars,
      state = state,
      conf = conf,
      to = team,
      top = top,
      position = pos
    )
  )
  data <- data.frame()

  tryCatch(
    expr = {
      data  <- jsonlite::fromJSON(parsed) %>%
        make_toRvik_data('Recruiting Rankings', Sys.time())
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
