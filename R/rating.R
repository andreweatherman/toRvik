#' Get T-Rank Ratings
#'
#' Returns current T-Rank ratings and two forms of strength of schedule.
#'
#' \itemize{\item `x_elite_sos` is the percentage of games that an 'elite' team
#' would project to lose against this team's non-conference or overall schedule.
#' \item `x_cur_sos` is the current average Barthag rating of opponents. \item
#' `x_fut_sos` is the projected average Barthag rating of opponents.}
#'
#' @returns Returns a tibble with 19 columns:
#' \describe{
#'   \item{\code{team}}{character.}
#'   \item{\code{conf}}{character.}
#'   \item{\code{barthag}}{double. The estimation of a team's win probability
#'   against the average Division 1 team on a neutral court.}
#'   \item{\code{barthag_rk}}{integer.}
#'   \item{\code{adj_o}}{double.}
#'   \item{\code{adj_o_rk}}{integer.}
#'   \item{\code{adj_d}}{double.}
#'   \item{\code{adj_d_rk}}{integer.}
#'   \item{\code{adj_t}}{double.}
#'   \item{\code{adj_t_rk}}{integer.}
#'   \item{\code{wab}}{double. The number of wins above or below the expected
#'   total from a bubble team against the same schedule.}
#'   \item{\code{nc_elite_sos}}{double.}
#'   \item{\code{nc_fut_sos}}{double.}
#'   \item{\code{nc_cur_sos}}{double.}
#'   \item{\code{ov_elite_sos}}{double.}
#'   \item{\code{ov_fut_sos}}{double.}
#'   \item{\code{ov_cur_sos}}{double.}
#'   \item{\code{seed}}{integer.}
#'   \item{\code{year}}{double.}
#' }
#' @param year Defaults to current season (YYYY).
#' @import dplyr
#' @import readr
#' @import httr
#' @import janitor
#' @importFrom withr local_options
#' @importFrom cli cli_abort
#' @importFrom rvest read_html html_table
#' @importFrom purrr pluck
#' @importFrom tidyr separate
#'
#' @importFrom magrittr %>%
#' @examples
#' bart_ratings(year=2022)
#'
#' @export
bart_ratings <- function(year = current_season()) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
      2008)) {
      cli::cli_abort("Enter a valid year as a number (YYYY). Data only goes back to 2008!")
    } else {
      x_names <- c("team", "barthag", "adj_o", "adj_d", "adj_t", "wab")
      y_names <- c(
        "team", "seed", "conf", "nc_elite_sos", "nc_fut_sos", "nc_cur_sos",
        "ov_elite_sos", "ov_fut_sos", "ov_cur_sos"
      )
      x <- readr::read_csv(paste0("https://barttorvik.com/trank.php?year=", year, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr::select(1, 4, 2, 3, 27, 35)
      colnames(x) <- x_names
      y <- httr::GET(paste0("https://barttorvik.com/sos.php?year=", year)) %>%
        httr::content(as = "text") %>%
        rvest::read_html() %>%
        rvest::html_table(header = FALSE) %>%
        purrr::pluck(1) %>%
        janitor::row_to_names(row = 2) %>%
        janitor::clean_names() %>%
        select(-1) %>%
        dplyr::mutate_at(3:8, funs(readr::parse_number(.))) %>%
        tidyr::separate(team,
          into = c("team", "seed"), sep = "(?<=[A-Za-z.]) (?=[0-9])",
          convert = TRUE
        )
      colnames(y) <- y_names
      x <- dplyr::left_join(x, y, by = "team") %>%
        dplyr::relocate(conf, .before = barthag) %>%
        dplyr::relocate(seed, .after = last_col()) %>%
        dplyr::mutate(year = year) %>%
        dplyr::arrange(desc(barthag)) %>%
        dplyr::mutate(barthag_rk = row_number(), .after = barthag) %>%
        dplyr::arrange(desc(adj_o)) %>%
        dplyr::mutate(adj_o_rk = row_number(), .after = adj_o) %>%
        dplyr::arrange(adj_d) %>%
        dplyr::mutate(adj_d_rk = row_number(), .after = adj_d) %>%
        dplyr::arrange(desc(adj_t)) %>%
        dplyr::mutate(adj_t_rk = row_number(), .after = adj_t) %>%
        arrange(desc(barthag))
      return(x)
    }
  })
}

#' Get Four Factor Statistics
#'
#' Returns four factor data and team records on a variety of splits, including
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
#'   \item{\code{team}}{character.}
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
#'}
#' @param year Defaults to current season (YYYY).
#' @param venue Filters by venue; defaults to `all`.
#' @param type Filters by game type; defaults to `all`.
#' @param quad Filters by quadrant level; defaults to `4`.
#' @param top Filters by opponent T-Rank position; defaults to NULL (all).
#' @param start Filters by start date; defaults to NULL (full season).
#' @param end Filters by end date; defaults to NULL (full season).
#' @import dplyr
#' @import httr
#' @import readr
#' @import janitor
#' @importFrom withr local_options
#' @importFrom cli cli_abort
#' @importFrom rvest read_html html_table
#' @importFrom purrr pluck
#' @importFrom tidyr separate
#' @importFrom magrittr %>%
#' @examples
#' \donttest{bart_factors(quad='3', venue='away', start='20220101')}
#'
#' @export
bart_factors <- function(year = current_season(), venue = "all", type = "all", quad = "4", top=0, start = NULL, end = NULL) {
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
      "team", "barthag", "rec", "wins", "games", "adj_t", "adj_o", "off_efg", "off_to", "off_or", "off_ftr", "adj_d", "def_efg",
      "def_to", "def_or", "def_ftr", "wab"
    )
    y_names <- c("team", "conf")
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
                "4" = "5"
    )
    y <- httr::GET(paste0("https://barttorvik.com/sos.php?year=", year, "&csv=1")) %>%
      httr::content(as = "text") %>%
      rvest::read_html() %>%
      rvest::html_table(header = FALSE) %>%
      purrr::pluck(1) %>%
      janitor::row_to_names(row = 2) %>%
      janitor::clean_names() %>%
      dplyr::select(2:3) %>%
      tidyr::separate(team, into = c("team", NA), sep = "(?<=[A-Za-z.]) (?=[0-9])")
    colnames(y) <- y_names
    if (is.null(start) && is.null(end)) {
      x <- readr::read_csv(paste0("https://barttorvik.com/trank.php?year=", year, "&revquad=0&quad=", q, "&venue=", v, "&type=", t, "&top=", top, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr::select(1, 4, 5, 6, 7, 27, 2, 8, 12, 14, 10, 3, 9, 13, 15, 11, 35)
      colnames(x) <- x_names
      x <- x %>% dplyr::mutate(across(c(2, 4:13), as.numeric),
        year = year,
        venue = venue,
        type = type,
        top=top,
        quad = paste0(quad, "+")
      )
      x <- dplyr::left_join(x, y, by = "team") %>%
        dplyr::relocate(conf, .after = team) %>%
        dplyr::arrange(desc(barthag))
    }
    if (!is.null(start) && !is.null(end)) {
      x <- readr::read_csv(paste0("https://barttorvik.com/trank.php?year=", year, "&sort=&hteam=&t2value=&begin=", start, "&end=", end, "&revquad=0&quad=", q, "&venue=", v, "&type=", t, "&top=", top, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr::select(1, 4, 5, 6, 7, 27, 2, 8, 12, 14, 10, 3, 9, 13, 15, 11, 35)
      colnames(x) <- x_names
      x <- x %>% dplyr::mutate(across(c(2, 4:13), as.numeric),
        year = year,
        venue = venue,
        type = type,
        top=top,
        quad = paste0(quad, "+"),
        start = start,
        end = end
      )
      x <- dplyr::left_join(x, y, by = "team") %>%
        dplyr::relocate(conf, .after = team) %>%
        dplyr::arrange(desc(barthag))
    }
    if(!is.null(start) && is.null(end)) {
      x <- readr::read_csv(paste0("https://barttorvik.com/trank.php?year=", year, "&sort=&hteam=&t2value=&begin=", start, "&revquad=0&quad=", q, "&venue=", v, "&type=", t, "&top=", top, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr::select(1, 4, 5, 6, 7, 27, 2, 8, 12, 14, 10, 3, 9, 13, 15, 11, 35)
      colnames(x) <- x_names
      x <- x %>% dplyr::mutate(across(c(2, 4:13), as.numeric),
                               year = year,
                               venue = venue,
                               type = type,
                               top=top,
                               quad = paste0(quad, "+"),
                               start = start,
      )
      x <- dplyr::left_join(x, y, by = "team") %>%
        dplyr::relocate(conf, .after = team) %>%
        dplyr::arrange(desc(barthag))
    }
    if(is.null(start) && !is.null(end)) {
      x <- readr::read_csv(paste0("https://barttorvik.com/trank.php?year=", year, "&sort=&hteam=&t2value=&end=", end, "&revquad=0&quad=", q, "&venue=", v, "&type=", t, "&top=", top, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr::select(1, 4, 5, 6, 7, 27, 2, 8, 12, 14, 10, 3, 9, 13, 15, 11, 35)
      colnames(x) <- x_names
      x <- x %>% dplyr::mutate(across(c(2, 4:13), as.numeric),
                               year = year,
                               venue = venue,
                               type = type,
                               top=top,
                               quad = paste0(quad, "+"),
                               end = end,
      )
      x <- dplyr::left_join(x, y, by = "team") %>%
        dplyr::relocate(conf, .after = team) %>%
        dplyr::arrange(desc(barthag))

    }
    return(x)
  })
}

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

#' Get Conference Team Statistics
#'
#' Returns conference-only metrics, strengths of schedule, and bid probabilities
#' (auto and at-large) for all teams in a conference.
#'
#' Accepted conference abbreviations for the `conf` argument are: \itemize{\item ‘A10’, ‘ACC’,
#' ‘AE’, ‘ASun’, ‘Amer’, ‘B10’, ‘B12’, ‘BE’, ‘BSky’, ‘BSth’, ‘BW’, ‘CAA’,
#' ‘CUSA’, ‘Horz’, ‘Ivy’, ‘MAAC’, ‘MAC’, ‘MEAC’, ‘MVC’, ‘MWC’, ‘NEC’, ‘OVC’,
#' ‘P12’, ‘Pat’, ‘SB’, ‘SC’, ‘SEC’, ‘SWAC’, ‘Slnd’, ‘Sum’, ‘WAC’, ‘WCC’}
#'
#' @returns Returns a tibble with 23 columns:
#' \describe{
#'   \item{\code{rk}}{double.}
#'   \item{\code{team}}{character.}
#'   \item{\code{seed}}{double.}
#'   \item{\code{finish}}{character.}
#'   \item{\code{conf_rec}}{character.}
#'   \item{\code{adj_oe}}{double.}
#'   \item{\code{adj_de}}{double.}
#'   \item{\code{barthag}}{double. The estimation of a team's win probability
#'   against the average Division 1 team on a neutral court.}
#'   \item{\code{eff_marg}}{double.}
#'   \item{\code{con_oe}}{double.}
#'   \item{\code{con_oe_rk}}{double.}
#'   \item{\code{con_de}}{double.}
#'   \item{\code{con_de_rk}}{double.}
#'   \item{\code{conf_barthag}}{double.}
#'   \item{\code{proj_rec}}{character.}
#'   \item{\code{conf_cur_sos}}{double. The current average Barthag rating of
#'   conference opponents.}
#'   \item{\code{conf_cur_sos_rk}}{double.}
#'   \item{\code{conf_fut_sos}}{double. The projected average Barthag rating of
#'   conference opponents}
#'   \item{\code{conf_fut_sos_rk}}{double.}
#'   \item{\code{conf_sos}}{double.}
#'   \item{\code{conf_sos_rk}}{double.}
#'   \item{\code{auto_prob}}{double. Probability of winning the conference
#'   tournament (automatic bid).}
#'   \item{\code{bid_prob}}{double. Probability of securing a tournament bid
#'   (auto or at-large)}
#'}
#' @param year Defaults to current season (YYYY).
#' @param conf Indicates conference (see details).
#' @import dplyr
#' @import httr
#' @import janitor
#' @import readr
#' @importFrom withr local_options
#' @importFrom cli cli_abort
#' @importFrom rvest read_html html_table
#' @importFrom purrr pluck
#' @importFrom tidyr separate
#' @importFrom magrittr %>%
#' @examples
#' bart_conf_stats(year=2022, conf='ACC')
#'
#' @export
bart_conf_stats <- function(year = current_season(), conf = NULL) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if (is.null(conf) || !(conf %in% c('A10', 'ACC', 'AE', 'ASun', 'Amer', 'B10', 'B12', 'BE', 'BSky', 'BSth', 'BW',
                                      'CAA', 'CUSA', 'Horz', 'Ivy', 'MAAC', 'MAC', 'MEAC', 'MVC', 'MWC', 'NEC', 'OVC',
                                      'P12', 'Pat', 'SB', 'SC', 'SEC', 'SWAC', 'Slnd', 'Sum', 'WAC', 'WCC'))) {
      cli::cli_abort("Please enter valid conference code (see details)")
    }
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
      2008)) {
      cli::cli_abort("Enter a valid year as a number. Data only goes back to 2008!")
    }
    if(year==current_season()) {
      x <- httr::GET(paste0("https://barttorvik.com/conf.php?conf=", conf, "&year=", year)) %>%
        httr::content(as = "text") %>%
        rvest::read_html() %>%
        rvest::html_table() %>%
        purrr::pluck(1) %>%
        janitor::row_to_names(row_number = 1) %>%
        janitor::clean_names() %>%
        tidyr::separate(team, into = c("team", "more"), sep = "(?<=[a-zA-QS-Z.])\\s*(?=[0-9])") %>%
        tidyr::separate(more, into = c("seed", "finish"), sep = ",") %>%
        dplyr::mutate_at(4, funs(trimws(.))) %>%
        dplyr::select(-9) %>%
        dplyr::rename(
          "conf_rec" = 5,
          "eff_marg"=9,
          "con_oe_rk" = 11,
          "con_de_rk" = 13,
          "conf_barthag" = 14,
          "conf_cur_sos" = 16,
          "conf_cur_sos_rk" = 17,
          "conf_fut_sos" = 18,
          "conf_fut_sos_rk" = 19,
          "conf_sos" = 20,
          "conf_sos_rk" = 21,
          "auto_prob"=22,
          "bid_prob"=23
        ) %>%
        dplyr::mutate(
          year=year,
          seed = readr::parse_number(seed),
          auto_prob=readr::parse_number(auto_prob),
          bid_prob=readr::parse_number(bid_prob),
          across(c(1, 6:14, 16:23), as.numeric)
        )
      return(x)
    }
  else {
    x <- httr::GET(paste0("https://barttorvik.com/conf.php?conf=", conf, "&year=", year)) %>%
      httr::content(as = "text") %>%
      rvest::read_html() %>%
      rvest::html_table() %>%
      purrr::pluck(1) %>%
      janitor::row_to_names(row_number = 1) %>%
      janitor::clean_names() %>%
      tidyr::separate(team, into = c("team", "more"), sep = "(?<=[a-zA-QS-Z.])\\s*(?=[0-9])") %>%
      tidyr::separate(more, into = c("seed", "finish"), sep = ",") %>%
      dplyr::mutate_at(4, funs(trimws(.))) %>%
      dplyr::select(-9) %>%
      dplyr::rename(
        "conf_rec" = 5,
        "eff_marg"=9,
        "con_oe_rk" = 11,
        "con_de_rk" = 13,
        "conf_barthag" = 14,
        "conf_cur_sos" = 16,
        "conf_cur_sos_rk" = 17,
        "conf_fut_sos" = 18,
        "conf_fut_sos_rk" = 19,
        "conf_sos" = 20,
        "conf_sos_rk" = 21
      ) %>%
      dplyr::mutate(
        year=year,
        seed = readr::parse_number(seed),
        across(c(1, 6:14, 16:21), as.numeric)
      )
    return(x)
  }
  })
}

#' Get T-Rank Archive Ratings
#'
#' Returns T-Rank ratings and efficiency metrics from the morning of the
#' specified day. Data goes back to 2014-15.
#'
#' @returns Returns a tibble with 16 columns:
#' #' \describe{
#'   \item{\code{rk}}{double.}
#'   \item{\code{team}}{character.}
#'   \item{\code{conf}}{character.}
#'   \item{\code{rec}}{character.}
#'   \item{\code{barthag}}{double. The estimation of a team's win probability
#'   against the average Division 1 team on a neutral court.}
#'   \item{\code{adj_o}}{double.}
#'   \item{\code{adj_o_rk}}{double.}
#'   \item{\code{adj_d}}{double.}
#'   \item{\code{adj_d_rk}}{double.}
#'   \item{\code{adj_tempo}}{double.}
#'   \item{\code{adj_tempo_rk}}{double.}
#'   \item{\code{proj_rec}}{character.}
#'   \item{\code{proj_conf_rec}}{character.}
#'   \item{\code{wab}}{double. The number of wins above or below the expected
#'   total from a bubble team against the same schedule.}
#'   \item{\code{wab_rk}}{double.}
#'   \item{\code{date}}{double.}
#' }
#' @param date Date to pull ratings (YYYYMMDD).
#' @import dplyr
#' @import lubridate
#' @importFrom withr local_options
#' @importFrom cli cli_abort
#' @importFrom tidyr separate
#' @importFrom curl curl_download
#' @importFrom magrittr %>%
#' @examples
#' bart_archive('20220113')
#' @export
bart_archive <- function(date) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    t_date <- lubridate::ymd(date)
    if (t_date < as.Date("2014-11-01")) {
      cli::cli_abort("Data only goes back to 2014-11-01!")
    }
    if (isTRUE(grepl("-", date))) {
      cli::cli_abort("Please enter a date in YYYYMMDD format with no hyphens")
    }
    curl::curl_download(paste0('https://barttorvik.com/timemachine/team_results/', date, '_team_results.json.gz'), 'archive.json')
    names <- c('rank', 'team', 'conf', 'record', 'barthag', 'adj_o', 'adj_o_rk', 'adj_d', 'adj_d_rk',
               'adj_tempo', 'adj_tempo_rk', 'proj_record', 'proj_conf_record',
               'wab', 'wab_rk', 'date')
    x <-  jsonlite::fromJSON('archive.json') %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(dplyr::across(c(1, 5:8, 45, 9:14, 42, 43), as.numeric),
                    dplyr::across(11:14, round, 1),
                    adj_tempo_rk = dplyr::dense_rank(desc(V45)), .after = V45,
                    date = lubridate::ymd(date)) %>%
      tidyr::unite('proj_record', 11:12, sep = '-', remove = TRUE) %>%
      tidyr::unite('proj_conf_record', 12:13, sep = '-', remove = TRUE) %>%
      dplyr::select(c(1:4, 9, 5:8, 43:44, 11:12, 40, 41, 45))
    colnames(x) <- names
    unlink('archive.json')
    return(x) }
  )
}
