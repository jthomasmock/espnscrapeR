% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_nfl_win_prob.R
\name{get_espn_win_prob}
\alias{get_espn_win_prob}
\title{Get NFL in-game win probabilities for a specific game from ESPN}
\usage{
get_espn_win_prob(game_id)
}
\arguments{
\item{game_id}{Character string - can be acquired from the website of a specific game, or from espnscrapeR::get_nfl_schedule()}
}
\value{
Returns a tibble
}
\description{
Get NFL in-game win probabilities for a specific game from ESPN
}
\examples{
# Get win prob from specific game
get_espn_win_prob(game_id = "401030956")
}
