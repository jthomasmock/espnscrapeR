#' Get ESPN QBR for College football
#'
#' @param season Numeric or character - greater than 2004
#' @param week Numeric or character - typically 1 to 15 or Bowls
#' @import jsonlite tidyr dplyr purrr glue
#' @return tibble
#' @export
#'
#' @examples
#'
#' get_college_qbr(season = 2011, week = NA)
#'
#' get_espn_college_qbr(2019, 1)
#'
get_espn_college_qbr <- function(season = 2019, week = NA) {

  current_year <- as.double(substr(Sys.Date(), 1, 4))

  # Small error handling to guide the limits on years
  if (!between(as.numeric(season), 2004, current_year)) {
    stop(paste("Please choose season between 2004 and", current_year))
  }

  # Add message according to totals or weeks
  message(
    dplyr::if_else(
      is.na(week),
      glue::glue("Scraping QBR totals for {season}!"),
      glue::glue("Scraping QBR for week {week} of {season}!")
      )
  )

  # Build base url
  base_url <- "http://site.web.api.espn.com/apis/fitt/v3/sports/football/college-football/qbr?qbrType="

  # add base url to the weeks vs totals
  url <- if_else(!is.na(week),
    glue::glue("{base_url}weeks&conference=80&season={season}&page=1&sort=schedAdjQBR%3Adesc&week={week}&limit=200"),
    glue::glue("{base_url}seasons&conference=80&season={season}&page=1&sort=schedAdjQBR%3Adesc&limit=200")
  )

  raw_json <- fromJSON(url)

  # Get the specific player's QBR stats
  get_qbr_data <- function(row_n) {
    purrr::pluck(raw_json, "athletes", "categories", row_n, "totals", 1)
  }

  # unnest_wider() name repair is noisy
  # Let's make it quiet with purrr::quietly()
  quiet_unnest_wider <- purrr::quietly(tidyr::unnest_wider)

  purrr::pluck(raw_json, "athletes", "athlete") %>%
    dplyr::as_tibble() %>%
    dplyr::select(firstName:shortName, teamName:teamShortName, age) %>%
    dplyr::mutate(row_n = dplyr::row_number()) %>%
    dplyr::mutate(data = map(row_n, get_qbr_data)) %>%
    # lots of name_repair here that I am silencing
    quiet_unnest_wider(data) %>%
    purrr::pluck("result") %>%
    purrr::set_names(nm = c(
      "first_name", "last_name", "name", "short_name",
      "team_name", "team_short_name", "age", "row_n", "qbr_total",
      "points_added", "qb_plays", "total_epa", "pass", "run",
      "exp_sack", "penalty", "raw_qbr", "sack"
    )) %>%
    dplyr::mutate(season = season, week = week) %>%
    dplyr::mutate_at(vars(qbr_total:sack), as.double) %>%
    dplyr::select(season, week, dplyr::everything())
}
