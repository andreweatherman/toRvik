#' Current Season
#'
#' Returns current season in YYYY format; used as default season in package
#' functions.
#'
#' @export
current_season <- function() {
  dplyr::if_else(
    Sys.Date() >= as.Date('2022-11-15'),
    2023,
    2022
  )
}

my_time <- function() strftime(Sys.time(), format = "%H:%M:%S")

# custom operator for use in filter with NULL variables
`%==%` <- function (e1, e2) {
  if (is.null(e2)) {
    return(TRUE)
  } else {
    return(e1 == e2)
  }
}

# Helper function to match GitHub url path
gh_data_path <- function(stat, load_all = FALSE, year = NULL, ...) {

  if (load_all) {

    params <- c('pg_box', 'pg_shooting', 'pg_adv', 'pg_all', 'ps_box', 'ps_shooting',
                'ps_adv', 'ps_all')

    endpoint <- c('player_game/box.parquet', 'player_game/shooting.parquet',
                  'player_game/advanced.parquet', 'player_game/all.parquet',
                  'player_season/box.parquet','player_season/shooting.parquet',
                  'player_season/advanced.parquet', 'player_season/all.parquet')
  }

  else {

  params <- c('pg_box', 'pg_shooting', 'pg_adv', 'pg_all', 'ps_box', 'ps_shooting',
              'ps_adv', 'ps_all', 'current_ratings')

  endpoint <- c(glue('player_game/{year}/box_{year}.parquet'), glue('player_game/{year}/shooting_{year}.parquet'),
                glue('player_game/{year}/advanced_{year}.parquet'), glue('player_game/{year}/all_{year}.parquet'),
                glue('player_season/{year}/box_{year}.parquet'), glue('player_season/{year}/shooting_{year}.parquet'),
                glue('player_season/{year}/advanced_{year}.parquet'), glue('player_season/{year}/all_{year}.parquet'),
                glue('ratings/ratings_{year}.csv'))
  }

  check <- setNames(endpoint, params)

  link <- check[stat]

}

# Function to load full data from GitHub
# This function is adapted from hoopR

load_gh_data <- function(stat = NULL, dbConnection = NULL, tablename = NULL, ...) {
  if (!is.null(dbConnection) && !is.null(tablename))
    in_db <- TRUE
  else in_db <- FALSE
  url <- paste0('https://github.com/andreweatherman/toRvik-data/raw/main/', gh_data_path(stat, ...))
  if (endsWith(url, '.rds')) {
    out <- rds_from_url(url)
  }
  if (endsWith(url, '.parquet')) {
    out <- parquet_from_url(url)
  }
  else {
    out <- read.csv(url)
  }
  if (in_db) {
    DBI::dbWriteTable(dbConnection, tablename, out, append = TRUE)
    out <- NULL
  }
  else {
    class(out) <- c("toRvik_data", "tbl_df", "tbl", "data.table", "data.frame")
  }
  out
}


# helper function to download .rds from url
rds_from_url <- function(url) {
  con <- url(url)
  on.exit(close(con))
  load <- try(readRDS(con), silent = TRUE)

  if (inherits(load, "try-error")) {
    warning('Failed to load data', call. = FALSE)
    return(data.table::data.table())
  }

  data.table::setDT(load)
  return(load)
}

# helper function to download .parquet from url
# h/t to Tan and {nflreadr}
parquet_from_url <- function(url) {
  rlang::check_installed("arrow")
  # cache_message()
  load <- try(curl::curl_fetch_memory(url), silent = TRUE)

  if (inherits(load, "try-error")) {
    cli::cli_warn("Failed to retrieve data from {.url {url}}")
    return(data.table::data.table())
  }

  content <- try(arrow::read_parquet(load$content), silent = TRUE)

  if (inherits(content, "try-error")) {
    cli::cli_warn("Failed to parse file with {.fun arrow::read_parquet()} from {.url {url}}")
    return(data.table::data.table())
  }

  data.table::setDT(content)
  return(content)
}



# Functions for custom class
# turn a data.frame into a tibble/toRvik_data
make_toRvik_data <- function(df,type,timestamp){
  out <- df %>%
    tidyr::as_tibble()

  class(out) <- c("toRvik_data","tbl_df","tbl","data.table","data.frame")
  attr(out,"toRvik_timestamp") <- timestamp
  attr(out,"toRvik_type") <- type
  return(out)
}

# The function `message_completed` to create the green "...completed" message
# only exists to hide the option `in_builder` in dots
message_completed <- function(x, in_builder = FALSE) {
  if (isFALSE(in_builder)) {
    str <- paste0(my_time(), " | ", x)
    cli::cli_alert_success("{{.field {str}}}")
  } else if (in_builder) {
    cli::cli_alert_success("{my_time()} | {x}")
  }
}

check_docs_error <- function() {
  cli::cli_alert_warning('Something went wrong. Check documentation.')
}

# helper function to define year errors
year_out_of_bounds <- function(year) {
  if (!is.null(year) & !(is.numeric(year) && nchar(year) == 4 && year >= 2008)) {
  cli::cli_abort(c(
    "{.var year} must be 2008 or later",
    "x" = "You passed through {year}"
  ))
  }
}


rule_header <- function(x) {
  rlang::inform(
    cli::rule(
      left = ifelse(is_installed("crayon"), crayon::bold(x), glue::glue("\033[1m{x}\033[22m")),
      right = paste0("toRvik version ", utils::packageVersion("toRvik")),
      width = getOption("width")
    )
  )
}

rule_footer <- function(x) {
  rlang::inform(
    cli::rule(
      left = ifelse(is_installed("crayon"), crayon::bold(x), glue::glue("\033[1m{x}\033[22m")),
      width = getOption("width")
    )
  )
}

#' @export
#' @noRd
print.toRvik_data <- function(x, ...) {
  cli::cli_rule(left = "{attr(x,'toRvik_type')}",right = "{.emph toRvik {utils::packageVersion('toRvik')}}")

  if(!is.null(attr(x,'toRvik_timestamp'))) {
    cli::cli_alert_info(
      "Data updated: {.field {format(attr(x,'toRvik_timestamp'), tz = Sys.timezone(), usetz = TRUE)}}"
    )
  }

  NextMethod(print,x)
  invisible(x)
}

# rbindlist but maintain attributes of last file, taken from nflreadr
rbindlist_with_attrs <- function(dflist){

  toRvik_timestamp <- attr(dflist[[length(dflist)]], "toRvik_timestamp")
  toRvik_type <- attr(dflist[[length(dflist)]], "toRvik_type")
  out <- data.table::rbindlist(dflist, use.names = TRUE, fill = TRUE)
  attr(out,"toRvik_timestamp") <- toRvik_timestamp
  attr(out,"toRvik_type") <- toRvik_type
  out

}

# define function to drop index if one is detected
drop_index <- function(data) {
  if (colnames(data)[1] %in% c('X', 'Unnamed: 0')) {
    data %>%
      dplyr::select(-1)
  }
  else {
    data
  }
}

# define list unchop from vctrs
list_unchop <- function(x,
                        ...,
                        indices = NULL,
                        ptype = NULL,
                        name_spec = NULL,
                        name_repair = c("minimal", "unique", "check_unique", "universal", "unique_quiet", "universal_quiet"),
                        error_arg = "x",
                        error_call = current_env()) {
  check_dots_empty0(...)
  .Call(ffi_list_unchop, x, indices, ptype, name_spec, name_repair, environment())
}
