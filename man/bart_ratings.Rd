% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bart_ratings.R
\name{bart_ratings}
\alias{bart_ratings}
\title{Get T-Rank Ratings}
\usage{
bart_ratings(year = current_season())
}
\arguments{
\item{year}{Defaults to current season (YYYY).}
}
\value{
Returns a tibble with 19 columns:
\describe{
  \item{\code{team}}{character.}
  \item{\code{conf}}{character.}
  \item{\code{barthag}}{double. The estimation of a team's win probability
  against the average Division 1 team on a neutral court.}
  \item{\code{barthag_rk}}{integer.}
  \item{\code{adj_o}}{double.}
  \item{\code{adj_o_rk}}{integer.}
  \item{\code{adj_d}}{double.}
  \item{\code{adj_d_rk}}{integer.}
  \item{\code{adj_t}}{double.}
  \item{\code{adj_t_rk}}{integer.}
  \item{\code{wab}}{double. The number of wins above or below the expected
  total from a bubble team against the same schedule.}
  \item{\code{nc_elite_sos}}{double.}
  \item{\code{nc_fut_sos}}{double.}
  \item{\code{nc_cur_sos}}{double.}
  \item{\code{ov_elite_sos}}{double.}
  \item{\code{ov_fut_sos}}{double.}
  \item{\code{ov_cur_sos}}{double.}
  \item{\code{seed}}{integer.}
  \item{\code{year}}{double.}
}
}
\description{
Returns current T-Rank ratings and two forms of strength of schedule.
}
\details{
\itemize{\item `x_elite_sos` is the percentage of games that an 'elite' team
would project to lose against this team's non-conference or overall schedule.
\item `x_cur_sos` is the current average Barthag rating of opponents. \item
`x_fut_sos` is the projected average Barthag rating of opponents.}
}
\examples{
\donttest{try(bart_ratings(year=2020))}

}
