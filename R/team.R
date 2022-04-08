#' Get Team Shooting Splits
#'
#' Returns team shooting totals and shares by shot location. Data returns back
#' to 2010.
#'
#' `x_share` represents the percentage of made FGs that fall under that
#' category.
#'
#' @param year Defaults to current season (YYYY).
#' @import dplyr
#' @import httr
#' @importFrom withr local_options
#' @importFrom cli cli_abort
#' @importFrom rvest read_html html_table
#' @importFrom purrr pluck
#' @importFrom tidyr separate
#' @importFrom magrittr %>%
#' @examples
#' bart_team_shooting(year=2019)
#' @export
bart_team_shooting <- function(year = current_season()) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    names <- c("team", "conf", "dunk_fg", "dunk_share", "dunk_fg_d", "dunk_share_d", "close_fg", "close_share", "close_fg_d", "close_share_d", "far_fg", "far_share", "far_fg_d", "far_share_d", "three_fg", "three_share", "three_fg_d", "three_share_d")
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
      2010)) {
      cli::cli_abort("Enter a valid year as a number (YYYY). Data only goes back to 2010!")
    } else {
      x <- httr::GET(paste0("https://barttorvik.com/teampbptot.php?year=", year)) %>%
        httr::content(as = "text") %>%
        rvest::read_html() %>%
        rvest::html_table() %>%
        purrr::pluck(1) %>%
        subset(select = -c(1, seq(7, 22, 5)))
      colnames(x) <- names
      x <- x[!(x$team == ""), ]
      x <- x %>%
        tidyr::separate(team, into = c("team", "seed"), sep = "(?<=\\D) (?=[0-9])") %>%
        dplyr::mutate(across(c(2, seq(5, 19, 2)), as.numeric), year = year)
      return(x)
    }
  })
}

#' Get Team History
#'
#' Returns efficiency and four factor finish, conference and overall records,
#' and basic shooting percentages by year back to 2008.
#'
#' `team` input must be an exact match: 'State' is abbreviated to 'St.' and full
#' names are used where applicable (e.g. 'North Carolina'). For complete list,
#' see `team` column of \code{\link{bart_ratings}}.
#'
#' @param team Indicates team to return.
#' @import dplyr
#' @import httr
#' @import janitor
#' @import  readr
#' @importFrom withr local_options
#' @importFrom rvest read_html html_table
#' @importFrom purrr pluck
#' @importFrom tidyr separate
#' @importFrom magrittr %>%
#' @export
bart_team_history <- function(team) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if(grepl('  ', team)){
      cli::cli_abort("Check spacing in team name; use just one space to separate.")
    }
    team <- gsub(" ", "+", team)
    x <- httr::GET(paste0("https://barttorvik.com/team-history.php?team=", team)) %>%
      httr::content(as = "text") %>%
      rvest::read_html() %>%
      rvest::html_table() %>%
      purrr::pluck(1) %>%
      janitor::clean_names() %>%
      tidyr::separate(coach, into = c("coach", "more"), sep = "(?<=[a-z.])\\s*(?=[0-9])") %>%
      tidyr::separate(more, into = c("seed", "finish"), sep = ",") %>%
      tidyr::separate(rec, into = c("ov_rec", "conf_rec"), sep = "[\\(\\)]") %>%
      dplyr::mutate_at(c(5, 7), funs(trimws(.))) %>%
      dplyr::mutate(
        seed = readr::parse_number(seed),
        across(c(1, 2, 4, 9:22), as.numeric)
      ) %>%
      dplyr::rename(
        "two_pct" = 20,
        "two_pct_d" = 21,
        "three_pct" = 22
      )
    return(x)
  })
}
