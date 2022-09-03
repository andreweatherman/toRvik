#' Get Transfer Portal Statistics
#'
#' Returns detailed, season-long player statistics on a variety of splits for
#' all portal players for 2022-23. Transfer information is sourced from
#' \href{https://verbalcommits.com/transfers/2022}{Verbal Commits} and is
#' updated every 20 minutes. Commit information is updated roughly every 18
#' hours.
#'
#' Data is split on three statistical types, explained below: \describe{
#' \item{box}{Returns basic box score stats.} \item{shooting}{Returns
#' play-by-play shooting splits.} \item{adv}{Returns advanced metrics and
#' possession-adjusted box score statistics.}}
#'
#' @returns Returns a tibble with the number of columns dependent on the value
#'   supplied to the `stat` argument.
#' @param stat Indicates statistical split (see details).
#' @param conf_only Logical. Filters data by conference-only play; defaults to
#'   `FALSE`.
#' @param active Logical. Filters players by portal status -- active vs.
#'   committed; defaults to TRUE (active).
#' @import dplyr
#' @import jsonlite
#' @import readr
#' @import httr
#' @importFrom withr local_options
#' @importFrom cli cli_abort
#' @importFrom stringr str_match
#' @importFrom magrittr %>%
#' @examples
#' bart_transfers(stat='box')
#'
#' @export
bart_transfers <- function(stat = 'all', conf_only = FALSE, active=TRUE) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if (is.null(stat) || !(stat %in% c("box", "shooting", "adv", "all"))) {
      cli::cli_abort("Please input a valid stat command ('box', 'shooting', or 'adv')")
    }
    c_only <- as.integer(conf_only)
    txt <- httr::GET("https://barttorvik.com/playerstat.php?link=y&xvalue=trans&year=2022")
    txt <- httr::content(txt)
    t <- stringr::str_match(txt, "var transfers\\s*(.*?)\\s*var trandict")[1, 2]
    t <- gsub("=", "", t)
    t <- gsub(";", "", t)
    all <- jsonlite::fromJSON(t) %>%
      dplyr::as_tibble()
    portal <- all %>%
              dplyr::filter(V3 == "") %>%
              dplyr::select(1, 2) %>%
              dplyr::rename("player" = 1,
                            "team"=2)
    commit <- all %>%
              dplyr::filter(V3 != "") %>%
              dplyr::select(1, 2, 3) %>%
              dplyr::rename("player" = 1,
                            "team"=2,
                            "new_school"=3)
    if (stat == "box") {
      names <- c(
        "player", "pos", "exp", "hgt", "team", "conf", "g", "mpg", "ppg", "oreb",
        "dreb", "rpg", "apg", "ast_to", "spg", "bpg", "num", "year", "id"
      )
      x <- readr::read_csv(paste0("https://barttorvik.com/getadvstats.php?year=2022&conyes=", c_only, "&csv=1"), col_names = FALSE, show_col_types = FALSE)
      y <- x %>% dplyr::mutate(across(c(17, 18, 20, 21, 43, 44), as.numeric))
      y <- y %>%
        dplyr::group_by(X33) %>%
        dplyr::summarize(
          fga = sum(X18, X21, X44, na.rm = TRUE),
          fgm = sum(X17, X20, X43, na.rm = TRUE),
          fg_pct = fgm / fga
        ) %>%
        dplyr::rename("id" = 1)
      x <- x %>% dplyr::select(1, 65, 26, 27, 2:4, 55, 64, 58:61, 36, 62, 63, 28, 32, 33)
      colnames(x) <- names
      x <- dplyr::left_join(x, (y %>% dplyr::select(1, 4)), by = "id") %>%
        dplyr::relocate(fg_pct, .before = oreb) %>%
        dplyr::arrange(desc(ppg))
          if(active==TRUE) {
              x <- merge(x, portal, by = c("player", "team")) %>%
                dplyr::as_tibble() }
          if(active==FALSE) {
              x <- merge(x, commit, by = c("player", "team")) %>%
                  dplyr::as_tibble() }
      }
      if (stat == "shooting") {
      names <- c(
        "player", "pos", "exp", "team", "conf", "g", "mpg", "ppg", "usg", "ortg", "efg", "ts",
        "ftm", "fta", "ft_pct", "two_m", "two_a", "two_pct", "three_m", "three_a",
        "three_pct", "dunk_m", "dunk_a", "dunk_pct", "rim_m", "rim_a", "rim_pct",
        "mid_m", "mid_a", "mid_pct", "id"
      )
      x <- readr::read_csv(paste0("https://barttorvik.com/getadvstats.php?year=2022", "&conyes=", c_only, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr::select(
          1, 65, 26, 2:4, 55, 64, 7, 6, 8, 9, 14:22, 43:45, 37, 38, 41, 39, 40,
          42, 33
        )
      colnames(x) <- names
      x <- x %>%
        dplyr::mutate(p_per = ((40 * ppg) / mpg), .after = ppg) %>%
        dplyr::arrange(desc(ppg))
            if(active==TRUE) {
                x <- merge(x, portal, by = c("player", "team")) %>%
                     dplyr::as_tibble() }
            if(active==FALSE) {
                x <- merge(x, commit, by = c("player", "team")) %>%
                     dplyr::as_tibble() }
      }
    if (stat == "adv") {
      names <- c(
        "player", "pos", "exp", "team", "conf", "g", "min_rate", "porpag", "dporpag", "ortg", "adj_oe", "drtg", "adj_de",
        "stops", "obpm", "dbpm", "bpm", "oreb", "dreb", "ast", "to", "blk", "stl", "ftr", "pfr",
        "rec", "id"
      )
      x <- readr::read_csv(paste0("https://barttorvik.com/getadvstats.php?year=2022", "&conyes=", c_only, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr::select(1, 65, 26, 2:5, 29, 49, 6, 30, 47, 48, 50, 56, 57, 54, 10:13, 23:25, 31, 35, 33)
      colnames(x) <- names
      x <- x %>%
        dplyr::arrange(desc(rec))
          if(active==TRUE) {
              x <- merge(x, portal, by = c("player", "team")) %>%
                   dplyr::as_tibble() }
          if(active==FALSE) {
             x <- merge(x, commit, by = c("player", "team")) %>%
                  dplyr::as_tibble() }
    }
    if(stat=='all') {
      names <- c("player", "pos", "exp", "num", "hgt", "team", "conf", "g", "min", "mpg", "ppg", "oreb",
                 "dreb", "rpg", "apg", "ast_to", "spg", "bpg", "usg", "ortg", "efg", "ts",
                 "ftm", "fta", "ft_pct", "two_m", "two_a", "two_pct", "three_m", "three_a",
                 "three_pct", "dunk_m", "dunk_a", "dunk_pct", "rim_m", "rim_a", "rim_pct",
                 "mid_m", "mid_a", "mid_pct", "min_rate", "porpag", "dporpag", "adj_oe", "drtg", "adj_de",
                 "stops", "obpm", "dbpm", "bpm", "oreb_rate", "dreb_rate", "ast", "to", "blk", "stl", "ftr", "pfr",
                 "rec", "year", "id")
      x <- readr::read_csv(paste0("https://barttorvik.com/getadvstats.php?year=2022&conyes=", c_only, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr::select(1, 65, 26, 28, 27, 2:5, 55, 64, 58:61, 36, 62, 63, 7, 6, 8, 9, 14:22, 43:45, 37, 38, 41, 39, 40,
                      42, 29, 49, 6, 30, 47, 48, 50, 56, 57, 54, 10:13, 23:25, 31, 35, 46, 32, 33)
      colnames(x) <- names
      y <- x %>%
        dplyr::group_by(id) %>%
        dplyr::summarize(fgm=sum(two_m, three_m, na.rm=TRUE),
                         fga=sum(two_a, three_a, na.rm=TRUE),
                         fg_pct=fgm/fga)
      x <- dplyr::left_join(x, y, by='id')
      x <- x %>%
        dplyr::relocate(c(61:63), .after=ts) %>%
        dplyr::arrange(desc(ppg))
      if(active==TRUE) {
        x <- merge(x, portal, by = c("player", "team")) %>%
          dplyr::as_tibble() }
      if(active==FALSE) {
        x <- merge(x, commit, by = c("player", "team")) %>%
          dplyr::as_tibble() }
    }
    return(x)
}
  )
}

#' Get Pro Prospect Player Data
#'
#' Returns detailed, season-long player statistics on a variety of splits for
#' all eligible or declared players.
#'
#' Data is split on three statistical types, explained below: \describe{
#' \item{box}{Returns basic box score stats.} \item{shooting}{Returns
#' play-by-play shooting splits.} \item{adv}{Returns advanced metrics and
#' possession-adjusted box score statistics.}}
#'
#' @returns Returns a tibble with the number of columns dependent on the value
#'   supplied to the `stat` argument.
#' @param stat Indicates statistical split (see details).
#' @param conf_only Logical. Filters data by conference-only play; defaults to
#'   `FALSE`.
#' @param early Logical. Filters by early entrance status; defaults to FALSE
#'   (all players).
#' @import dplyr
#' @import jsonlite
#' @import readr
#' @import httr
#' @import stringr
#' @importFrom withr local_options
#' @importFrom cli cli_abort
#' @importFrom magrittr %>%
#' @examples
#' bart_pro(stat='box')
#'
#' @export
bart_pro <- function(stat='all', conf_only=FALSE, early=FALSE) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if (is.null(stat) || !(stat %in% c("box", "shooting", "adv", "all"))) {
      cli::cli_abort("Please input a valid stat command ('box', 'shooting', or 'adv')")
    }
    c_only <- as.integer(conf_only)
    txt <- httr::GET("https://barttorvik.com/playerstat.php?link=y&xvalue=pros&year=2022&start=20211101&end=20220501")
    txt <- httr::content(txt)
    pro <- stringr::str_match(txt, "var prodict\\s*(.*?)\\s*var ncaaregions")[1,2]
    pro <- gsub("=", "", pro)
    pro <- gsub(";", "", pro)
    pro <- gsub(':', '', pro)
    pro <- gsub("\\{|\\}", "", pro)
    pro <- gsub('[[:digit:]]+', '', pro)
    pro <- gsub("([a-z.]{2,})(?=[A-Z])", "\\1 \\2", pro, perl = T)
    pro <- gsub("(I{2,})(?=[A-Z])", "\\1 \\2", pro, perl = T)
    pro <- stringr::str_extract_all(pro, "[\"].*?[\"]")
    pro <- dplyr::as_tibble(pro, .name_repair = 'minimal') %>%
          dplyr::rename('player'=1)
    pro <- gsub("[^a-zA-Z.&'']", " ", pro$player)
    pro <- dplyr::as_tibble(pro) %>%
          dplyr::rename('player'=1) %>%
          dplyr::mutate(team=stringr::str_match(as.character(player), paste(toRvik:::team, collapse='|')),
                        player=stringr::str_remove_all(as.character(player), paste(toRvik:::team, collapse='|')),
                        abrev=stringr::str_extract(player, "  +.*"),
                        player=stringr::str_remove_all(as.character(player), "  .*+"),
                        player=trimws(player),
                        abrev=trimws(abrev),
                        team=stringr::str_c(team, ' ', abrev),
                        team=trimws(team)) %>%
          dplyr::select(-c(3))
    if (stat == "box") {
      names <- c(
        "player", "pos", "exp", "hgt", "team", "conf", "g", "mpg", "ppg", "oreb",
        "dreb", "rpg", "apg", "ast_to", "spg", "bpg", "num", "year", "id"
      )
      x <- readr::read_csv(paste0("https://barttorvik.com/getadvstats.php?year=2022&conyes=", c_only, "&csv=1"), col_names = FALSE, show_col_types = FALSE)
      y <- x %>% dplyr::mutate(across(c(17, 18, 20, 21, 43, 44), as.numeric))
      y <- y %>%
        dplyr::group_by(X33) %>%
        dplyr::summarize(
          fga = sum(X18, X21, X44, na.rm = TRUE),
          fgm = sum(X17, X20, X43, na.rm = TRUE),
          fg_pct = fgm / fga
        ) %>%
        dplyr::rename("id" = 1)
      x <- x %>% dplyr::select(1, 65, 26, 27, 2:4, 55, 64, 58:61, 36, 62, 63, 28, 32, 33)
      colnames(x) <- names
      x <- dplyr::left_join(x, (y %>% dplyr::select(1, 4)), by = "id") %>%
        dplyr::relocate(fg_pct, .before = oreb) %>%
        dplyr::arrange(desc(ppg))
      if(early==TRUE) {
      x <- merge(x, pro, by = c("player", "team")) %>%
          dplyr::as_tibble() %>%
          dplyr::filter(exp != 'Sr') }
      if(early==FALSE) {
        x <- merge(x, pro, by = c("player", "team")) %>%
          dplyr::as_tibble() }
      }
    if (stat == "shooting") {
      names <- c(
        "player", "pos", "exp", "team", "conf", "g", "mpg", "ppg", "usg", "ortg", "efg", "ts",
        "ftm", "fta", "ft_pct", "two_m", "two_a", "two_pct", "three_m", "three_a",
        "three_pct", "dunk_m", "dunk_a", "dunk_pct", "rim_m", "rim_a", "rim_pct",
        "mid_m", "mid_a", "mid_pct", "id"
      )
      x <- readr::read_csv(paste0("https://barttorvik.com/getadvstats.php?year=2022", "&conyes=", c_only, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr::select(
          1, 65, 26, 2:4, 55, 64, 7, 6, 8, 9, 14:22, 43:45, 37, 38, 41, 39, 40,
          42, 33
        )
      colnames(x) <- names
      x <- x %>%
        dplyr::mutate(p_per = ((40 * ppg) / mpg), .after = ppg) %>%
        dplyr::arrange(desc(ppg))
      if(early==TRUE) {
        x <- merge(x, pro, by = c("player", "team")) %>%
          dplyr::as_tibble() %>%
          dplyr::filter(exp != 'Sr') }
      if(early==FALSE) {
        x <- merge(x, pro, by = c("player", "team")) %>%
          dplyr::as_tibble() }
    }
    if (stat == "adv") {
      names <- c(
        "player", "pos", "exp", "team", "conf", "g", "min_rate", "porpag", "dporpag", "ortg", "adj_oe", "drtg", "adj_de",
        "stops", "obpm", "dbpm", "bpm", "oreb", "dreb", "ast", "to", "blk", "stl", "ftr", "pfr",
        "rec", "id"
      )
      x <- readr::read_csv(paste0("https://barttorvik.com/getadvstats.php?year=2022", "&conyes=", c_only, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr::select(1, 65, 26, 2:5, 29, 49, 6, 30, 47, 48, 50, 56, 57, 54, 10:13, 23:25, 31, 35, 33)
      colnames(x) <- names
      x <- x %>%
        dplyr::arrange(desc(rec))
      if(early==TRUE) {
        x <- merge(x, pro, by = c("player", "team")) %>%
          dplyr::as_tibble() %>%
          dplyr::filter(exp != 'Sr') }
      if(early==FALSE) {
        x <- merge(x, pro, by = c("player", "team")) %>%
          dplyr::as_tibble() }
    }
    if(stat=='all') {
      names <- c("player", "pos", "exp", "num", "hgt", "team", "conf", "g", "min", "mpg", "ppg", "oreb",
                 "dreb", "rpg", "apg", "ast_to", "spg", "bpg", "usg", "ortg", "efg", "ts",
                 "ftm", "fta", "ft_pct", "two_m", "two_a", "two_pct", "three_m", "three_a",
                 "three_pct", "dunk_m", "dunk_a", "dunk_pct", "rim_m", "rim_a", "rim_pct",
                 "mid_m", "mid_a", "mid_pct", "min_rate", "porpag", "dporpag", "adj_oe", "drtg", "adj_de",
                 "stops", "obpm", "dbpm", "bpm", "oreb_rate", "dreb_rate", "ast", "to", "blk", "stl", "ftr", "pfr",
                 "rec", "year", "id")
      x <- readr::read_csv(paste0("https://barttorvik.com/getadvstats.php?year=2022&conyes=", c_only, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr::select(1, 65, 26, 28, 27, 2:5, 55, 64, 58:61, 36, 62, 63, 7, 6, 8, 9, 14:22, 43:45, 37, 38, 41, 39, 40,
                      42, 29, 49, 6, 30, 47, 48, 50, 56, 57, 54, 10:13, 23:25, 31, 35, 46, 32, 33)
      colnames(x) <- names
      y <- x %>%
        dplyr::group_by(id) %>%
        dplyr::summarize(fgm=sum(two_m, three_m, na.rm=TRUE),
                         fga=sum(two_a, three_a, na.rm=TRUE),
                         fg_pct=fgm/fga)
      x <- dplyr::left_join(x, y, by='id')
      x <- x %>%
        dplyr::relocate(c(61:63), .after=ts) %>%
        dplyr::arrange(desc(ppg))
      if(early==TRUE) {
        x <- merge(x, pro, by = c("player", "team")) %>%
          dplyr::as_tibble() %>%
          dplyr::filter(exp != 'Sr') }
      if(early==FALSE) {
        x <- merge(x, pro, by = c("player", "team")) %>%
          dplyr::as_tibble() }
    }
    return(x) })}


#' Get Player of the Year Ratings
#'
#' Returns Barttorvik Player of the Year ratings on a variety of splits.
#'
#' Accepted conference abbreviations for the `conf` argument are: \itemize{\item
#' ‘A10’, ‘ACC’, ‘AE’, ‘ASun’, ‘Amer’, ‘B10’, ‘B12’, ‘BE’, ‘BSky’, ‘BSth’, ‘BW’,
#' ‘CAA’, ‘CUSA’, ‘Horz’, ‘Ivy’, ‘MAAC’, ‘MAC’, ‘MEAC’, ‘MVC’, ‘MWC’, ‘NEC’,
#' ‘OVC’, ‘P12’, ‘Pat’, ‘SB’, ‘SC’, ‘SEC’, ‘SWAC’, ‘Slnd’, ‘Sum’, ‘WAC’, ‘WCC’ }
#'
#' @returns Returns a tibble with four columns:
#' \describe{
#'   \item{\code{rk}}{integer.}
#'   \item{\code{player}}{character.}
#'   \item{\code{team}}{character.}
#'   \item{\code{score}}{double.}
#'   }
#' @param year Defaults to current season (YYYY).
#' @param conf Filters results by conference; defaults to all (see details).
#' @param class Filters results by class ('fr', 'so', 'jr', 'sr'); defaults to
#'   no filter.
#' @param conf_only Logical. Filters data by conference-only play; defaults to
#'   `FALSE`.
#' @import dplyr
#' @import httr
#' @import janitor
#' @importFrom withr local_options
#' @importFrom cli cli_abort
#' @importFrom rvest read_html html_table
#' @importFrom purrr pluck
#' @importFrom magrittr %>%
#' @examples
#' bart_poy(year=2019, class='fr')
#'
#' @export
bart_poy <- function(year = current_season(), conf = "All", class = NULL, conf_only = FALSE) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
  if (!(is.numeric(year) && nchar(year) == 4 && year >=
    2008)) {
    cli::cli_abort("Enter a valid year as a number (YYYY). Data only goes back to 2008!")
  }
  if(!is.null(class) && !(class %in% c('fr', 'so', 'jr', 'sr'))) {
    cli::cli_abort("Please input correct class value (see details)")
  }
  if (!(conf %in% c('All', 'A10', 'ACC', 'AE', 'ASun', 'Amer', 'B10', 'B12', 'BE', 'BSky', 'BSth', 'BW',
                                     'CAA', 'CUSA', 'Horz', 'Ivy', 'MAAC', 'MAC', 'MEAC', 'MVC', 'MWC', 'NEC', 'OVC',
                                     'P12', 'Pat', 'SB', 'SC', 'SEC', 'SWAC', 'Slnd', 'Sum', 'WAC', 'WCC'))) {
    cli::cli_abort("Please enter valid conference code (see details)")
  }

  class_lookup <- list(
    "fr" = "Fr",
    "so" = "So",
    "jr" = "Jr",
    "sr" = "Sr"
  )
  class <- class_lookup[class]
  if (conf_only == FALSE) {
    x <- httr::GET(paste0("https://barttorvik.com/poy.php?conlimit=", conf, "&year=", year, "&yr=", class)) %>%
      httr::content(as = "text") %>%
      rvest::read_html() %>%
      rvest::html_table() %>%
      purrr::pluck(1) %>%
      janitor::clean_names()
    return(x)
  }
  if (conf_only == TRUE) {
    x <- httr::GET(paste0("https://barttorvik.com/conpoy.php?conlimit=", conf, "&year=", year, "&yr=", class)) %>%
      httr::content(as = "text") %>%
      rvest::read_html() %>%
      rvest::html_table() %>%
      purrr::pluck(1) %>%
      janitor::clean_names()
    return(x)
  }
})}

#' Get Estimated Injury Impact
#'
#' Returns estimated adjusted ratings in a one-player injury scenario by team.
#'
#' Estimations are experimental and based on
#' \href{https://twitter.com/totally_t_bomb/status/973731719479201792}{'highly
#' dubious analysis.'}
#'
#' @returns Returns a tibble with five columns:
#' \describe{
#'   \item{\code{situation}}{character.}
#'   \item{\code{adj_oe}}{double.}
#'   \item{\code{adj_de}}{double.}
#'   \item{\code{barthag}}{double.}
#'   \item{\code{rk}}{double.}
#'   }
#' @param year Defaults to current season (YYYY).
#' @param team Indicates team.
#' @param player Indicates player to remove.
#' @import dplyr
#' @import httr
#' @import janitor
#' @importFrom withr local_options
#' @importFrom rvest read_html html_table
#' @importFrom cli cli_abort
#' @importFrom purrr pluck
#' @importFrom magrittr %>%
#' @examples
#' bart_injuryimpact(year=2019, team='Duke', player='Zion Williamson')
#'
#' @export
bart_injuryimpact <- function(year = current_season(), team = NULL, player = NULL) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if (is.null(team) | is.null(player)) {
      cli::cli_abort("Please enter a team or player value!")
    }
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
          2008)) {
      cli::cli_abort("Enter a valid year as a number (YYYY). Data only goes back to 2008!")
    }
    if(grepl('  ', team) | grepl('  ', player)){
      cli::cli_abort("Check spacing in name; use just one space to separate.")
    }
    else {
      player <- gsub(" ", "+", player)
      team <- gsub(" ", "+", team)
      x <- httr::GET(paste0("https://barttorvik.com/missing_player.php?team=", team, "&player=", player, "&year=", year)) %>%
        httr::content(as = "text") %>%
        rvest::read_html() %>%
        rvest::html_table(header = FALSE) %>%
        purrr::pluck(1) %>%
        janitor::row_to_names(row = 1) %>%
        janitor::clean_names() %>%
        dplyr::rename("situation" = 1) %>%
        dplyr::mutate(across(c(2:5), as.numeric))
      return(x)
    }
  })
}
