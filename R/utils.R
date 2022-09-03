#' Current Season
#'
#' Returns current season in YYYY format; used as default season in package
#' functions.
#'
#' @export
current_season <- function() {
  dplyr::if_else(as.double(substr(Sys.Date(), 6, 7)) >= 10,
                 as.double(substr(Sys.Date(), 1, 4)) + 1, as.double(substr(
                   Sys.Date(),
                   1, 4
                 ))
  )
}

#' Negate the base `%in%` operator
#'
#' Reverses the base `%in%` operator and returns a boolean; TRUE if the
#' element(s) is not in the compared item
#'
#' @export
`%!in%` <- Negate(`%in%`)
