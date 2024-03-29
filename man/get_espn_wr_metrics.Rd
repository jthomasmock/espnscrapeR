% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_espn_wr_metrics.R
\name{get_espn_wr_metrics}
\alias{get_espn_wr_metrics}
\title{Get ESPN's WR metrics from FiveThirtyEight}
\usage{
get_espn_wr_metrics()
}
\value{
tibble
}
\description{
Get ESPN's WR metrics from FiveThirtyEight
}
\examples{
raw_metrics <- espnscrapeR::get_espn_wr_metrics()

dplyr::glimpse(raw_metrics)
}
