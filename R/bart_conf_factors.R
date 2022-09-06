#' Get Conference Four Factor Statistics
#'
#' Returns conference-wide four factor data on a variety of splits, including
#' date range, quadrant level, opponent ranking, game location, and game type.
#'
#' For a brief explanation of each factor and its computation, please visit
#' \href{https://kenpom.com/blog/four-factors/}{KenPom's blog}. Data can be
#' split on five variables: \describe{ \item{venue}{Splits on game location;
#' 'all', 'home', 'away', 'neutral', and 'road' (away + neutral).}
#' \item{type}{Splits on game type; 'all', 'nc' (non-conference), 'conf'
#' (conference), 'reg' (regular season), 'post' (post-season tournaments),
#' 'ncaa' (NCAA tournament).} \item{quad}{Splits by quadrant level; 1-4 with 0
#' indicating 1-A games.} \item{top}{Splits by opponent T-Rank position,
#' adjusted for game location.} \item{start/end}{Splits by date range
#' (YYYYMMDD).}}
#'
#' @returns Returns a tibble with 22 columns:
#' \describe{
#'   \item{\code{conf}}{character.}
#'   \item{\code{barthag}}{double. The estimation of a team's win probability
#'   against the average Division 1 team on a neutral court.}
#'   \item{\code{rec}}{character.}
#'   \item{\code{wins}}{double.}
#'   \item{\code{games}}{double.}
#'   \item{\code{adj_t}}{double.}
#'   \item{\code{adj_o}}{double.}
#'   \item{\code{off_efg}}{double.}
#'   \item{\code{off_to}}{double.}
#'   \item{\code{off_or}}{double.}
#'   \item{\code{off_ftr}}{double.}
#'   \item{\code{adj_d}}{double.}
#'   \item{\code{def_efg}}{double.}
#'   \item{\code{def_to}}{double.}
#'   \item{\code{def_or}}{double.}
#'   \item{\code{def_ftr}}{double.}
#'   \item{\code{wab}}{double. The number of wins above or below the expected
#'   total from a bubble team against the same schedule.}
#'   \item{\code{year}}{double.}
#'   \item{\code{venue}}{character. Split supplied to the venue argument.}
#'   \item{\code{type}}{character. Split supplied to the type argument.}
#'   \item{\code{top}}{double. Split supplied to the top argument.}
#'   \item{\code{quad}}{character. Split supplied to the quad argument.}
#' }
#' @param year Defaults to current season (YYYY).
#' @param venue Filters by venue; defaults to `all`.
#' @param type Filters by game type; defaults to `all`.
#' @param quad Filters by quadrant level; defaults to `4`.
#' @param top Filters by opponent T-Rank position; defaults to NULL (all).
#' @param start Filters by start date; defaults to NULL (full season).
#' @param end Filters by end date; defaults to NULL (full season).
#' @import dplyr
#' @import readr
#' @importFrom withr local_options
#' @importFrom cli cli_abort
#' @importFrom magrittr %>%
#' @examples
#' \donttest{bart_conf_factors(type='nc')}
#'
#' @export
bart_conf_factors <- function(year = current_season(), venue = "all", type = "all", quad = "4", top=0, start = NULL, end = NULL) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
          2008)) {
      cli::cli_abort("Enter a valid year as a number (YYYY). Data only goes back to 2008!")
    }
    if (!(is.character(quad))) {
      cli::cli_abort("Please enter quadrant cutoff as a character value (e.g. '4')")
    }
    if(!(venue %in% c('all', 'home', 'away', 'neutral', 'road'))) {
      cli::cli_abort("Please input correct venue value (see details)")
    }
    if(!(type %in% c('all', 'nc', 'conf', 'reg', 'post', 'ncaa'))) {
      cli::cli_abort("Please input correct type value (see details)")
    }
    if(!(quad %in% c('0', '1', '2', '3', '4'))) {
      cli::cli_abort("Please input correct quad value (see details)")
    }
    x_names <- c(
      "conf", "barthag", "rec", "wins", "games", "adj_t", "adj_o", "off_efg", "off_to", "off_or", "off_ftr", "adj_d", "def_efg",
      "def_to", "def_or", "def_ftr", "wab"
    )
    v <- switch(venue,
                "all" = "All",
                "home" = "H",
                "away" = "A",
                "neutral" = "N",
                "road" = "A-N"
    )
    t <- switch(type,
                "all" = "All",
                "nc" = "N",
                "conf" = "C",
                "reg" = "R",
                "post" = "P",
                "ncaa" = "T"
    )
    q <- switch(quad,
                "0" = "1",
                "1" = "2",
                "2" = "3",
                "3" = "4",
                "4" = "5" )
    if (is.null(start) && is.null(end)) {
      x <- readr::read_csv(paste0("https://barttorvik.com/trank.php?year=", year, "&conyes=1&revquad=0&quad=", q, "&venue=", v, "&type=", t, "&top=", top, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr::select(1, 4, 5, 6, 7, 27, 2, 8, 12, 14, 10, 3, 9, 13, 15, 11, 35)
      colnames(x) <- x_names
      x <- x %>%
        dplyr::mutate(
          year = year,
          venue = venue,
          type = type,
          top=top,
          quad = paste0(quad, "+")
        ) %>%
        dplyr::arrange(desc(barthag))
    }
    if (!is.null(start) && !is.null(end)) {
      x <- readr::read_csv(paste0("https://barttorvik.com/trank.php?year=", year, "&conyes=1&sort=&hteam=&t2value=&begin=", start, "&end=", end, "&revquad=0&quad=", q, "&top=", top, "&venue=", v, "&type=", t, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr::select(1, 4, 5, 6, 7, 27, 2, 8, 12, 14, 10, 3, 9, 13, 15, 11, 35)
      colnames(x) <- x_names
      x <- x %>%
        dplyr::mutate(
          year = year,
          venue = venue,
          type = type,
          top=top,
          quad = paste0(quad, "+"),
          start = start,
          end = end
        ) %>%
        dplyr::arrange(desc(barthag))
    }
    if(!is.null(start) && is.null(end)) {
      x <- readr::read_csv(paste0("https://barttorvik.com/trank.php?year=", year, "&conyes=1&sort=&hteam=&t2value=&begin=", start, "&revquad=0&quad=", q, "&top=", top, "&venue=", v, "&type=", t, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr::select(1, 4, 5, 6, 7, 27, 2, 8, 12, 14, 10, 3, 9, 13, 15, 11, 35)
      colnames(x) <- x_names
      x <- x %>%
        dplyr::mutate(
          year = year,
          venue = venue,
          type = type,
          top=top,
          quad = paste0(quad, "+"),
          start = start,
          end = end
        ) %>%
        dplyr::arrange(desc(barthag))
    }
    if(is.null(start) && !is.null(end)) {
      x <- readr::read_csv(paste0("https://barttorvik.com/trank.php?year=", year, "&conyes=1&sort=&hteam=&t2value=&end=", end, "&revquad=0&quad=", q, "&top=", top, "&venue=", v, "&type=", t, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr::select(1, 4, 5, 6, 7, 27, 2, 8, 12, 14, 10, 3, 9, 13, 15, 11, 35)
      colnames(x) <- x_names
      x <- x %>%
        dplyr::mutate(
          year = year,
          venue = venue,
          type = type,
          top=top,
          quad = paste0(quad, "+"),
          start = start,
          end = end
        ) %>%
        dplyr::arrange(desc(barthag))
    }
    return(x)
  })
}
