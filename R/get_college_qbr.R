#' Get ESPN QBR for College football
#'
#' @param season Numeric or character - greater than 2004
#' @param week Numeric or character - typically 1 to 15 or Bowls
#' @import tidyr dplyr purrr httr
#' @importFrom dplyr %>%
#' @importFrom jsonlite fromJSON
#' @importFrom tibble enframe
#' @importFrom glue glue
#' @return tibble
#' @export
#'
#' @examples
#'
#' # Get college QBR from 2011 season as a whole
#' get_college_qbr(season = 2011, week = NA)
#'
#' # Get college QBR from 2019 season week 1
#' get_college_qbr(2019, 1)
get_college_qbr <- function(season = 2020, week = NA) {

  current_year <- as.double(substr(Sys.Date(), 1, 4))

  # Small error handling to guide the limits on years
  if (!dplyr::between(as.numeric(season), 2004, current_year)) {
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
  base_url <- "https://site.web.api.espn.com/apis/fitt/v3/sports/football/college-football/qbr"

  query_type <-
    if(!is.na(week)){
      list(
        qbrType = "weeks",
        limit = 200,
        season = season,
        week = week
      )
    } else {
      list(
        qbrType = "seasons",
        limit = 200,
        season = season
      )
    }


  raw_get <- httr::GET(
    url = base_url,
    query = list(
      qbrType = "seasons",
      limit = 200,
      season = 2020
    )
  )

  httr::stop_for_status(raw_get)

  raw_json <- httr::content(raw_get)


  # qbr names
  qbr_names <- c(
    "qbr_total",
    "pts_added",
    "qb_plays",
    "epa_total",
    "pass",
    "run",
    "exp_sack",
    "penalty",
    "qbr_raw",
    "sack"
  )

  purrr::pluck(raw_json, "athletes") %>%
    tibble::enframe() %>%
    tidyr::unnest_wider("value") %>%
    tidyr::unnest_wider(athlete) %>%
    tidyr::hoist(
      categories,
      qbr_values = list(1, "totals")
    ) %>%
    tidyr::hoist(
      headshot,
      headshot_href = "href"
    ) %>%
    dplyr::select(-name, -teams, -categories, -headshot, -position, -status, -links, -type) %>%
    dplyr::mutate(qbr_names = list(qbr_names)) %>%
    # select(id, shortName, qbr_values, qbr_names) %>%
    tidyr::unchop(qbr_values:qbr_names) %>%
    tidyr::unchop(qbr_values) %>%
    tidyr::pivot_wider(names_from = qbr_names, values_from = qbr_values) %>%
    janitor::clean_names() %>%
    dplyr::rename(player_id = id, player_uid = uid, player_guid = guid) %>%
    dplyr::mutate(season = as.integer(season), week = as.integer(week)) %>%
    # dplyr::mutate_at(vars(qbr_total:sack), as.double) %>%
    dplyr::select(season, week, dplyr::everything())
}
