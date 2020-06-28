#' Get ESPN QBR for NFL football
#'
#' @param season Either numeric or character
#' @param week Either NA to return season or week 1 to 4 for playoffs or 1 to 17 for regular
#' @param season_type Character - either "Regular" or "Playoffs"
#' @return Returns a tibble
#' @export
#' @import tidyr dplyr purrr
#' @importFrom dplyr %>%
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @examples
#' # Get ALL Playoff QBR from 2016 season
#' get_nfl_qbr("2016", season_type = "Playoffs", week = NA)
#'
#' # Get Regular season QBR for week 4 of 2019
#' get_nfl_qbr("2019", season_type = "Regular", week = 4)
get_nfl_qbr <- function(season = 2019, week = NA, season_type = "Regular") {
  current_year <- as.double(substr(Sys.Date(), 1, 4))

  # Error handling to correct season type
  if (!season_type %in% c("Regular", "Playoffs")) {
    stop("Please choose season_type of 'Regular' or 'Playoffs'")
  }

  # Error handling for limits on season
  if (!dplyr::between(as.numeric(season), 2006, current_year)) {
    stop(paste("Please choose season between 2006 and", current_year))
  }

  # Error handling for limits on regular season weeks
  if (!is.na(week) & season_type == "Regular" & !dplyr::between(as.numeric(week), 1, 17)) {
    stop("Please choose regular season week between 1 and 17")
  }

  # Error handling for limits on playoff weeks
  if (!is.na(week) & season_type == "Playoffs" & !dplyr::between(as.numeric(week), 1, 4)) {
    stop("Please choose Playoff week between 1 and 4")
  }

  # Error handling for missing data from ESPN
  if (!is.na(week) & season_type == "Playoffs" & as.numeric(season) == 2017) {
    stop("ESPN has missing Playoff data for 2017")
  }

  week_current <- dplyr::if_else(
    # Logic check to fix years where superbowl = 5
    season_type == "Playoffs" & as.numeric(week) == 4 & season >= 2009,
    # outcome = 5
    5,
    # default to normal week
    as.numeric(week)
  )

  # Add useful messages - separated by week
  message(dplyr::if_else(
    is.na(week),
    glue::glue("Scraping QBR totals for {season}!"),
    glue::glue("Scraping weekly QBR for week {week} of {season}!")
  ))

  # Build up URL - not sure if this makes it easier to read, but it makes it
  # closer to 80ish characters per line
  url_start <- "https://site.web.api.espn.com/apis/fitt/v3/sports/football/nfl/qbr?region=us&lang=en&qbrType="
  url_body <- "&isqualified=true&sort=schedAdjQBR%3Adesc&season="

  # Each of the urls build up for season type
  # seasontype == seasons or weeks
  # If weeks, week = week number
  url_1 <- glue::glue("{url_start}seasons&seasontype=2{url_body}{season}")
  url_2 <- glue::glue("{url_start}weeks&seasontype=2{url_body}{season}&week={week}")
  url_3 <- glue::glue("{url_start}seasons&seasontype=3{url_body}{season}")
  url_4 <- glue::glue("{url_start}weeks&seasontype=3{url_body}{season}&week={week_current}")

  # Assign the specific urls to the specific type of input
  url <- dplyr::case_when(
    is.na(week) & season_type == "Regular" ~ url_1,
    !is.na(week) & season_type == "Regular" ~ url_2,
    is.na(week) & season_type == "Playoffs" ~ url_3,
    !is.na(week) & season_type == "Playoffs" ~ url_4,
    TRUE ~ NA_character_
  )

  # Read in the raw json from ESPN
  raw_json <- jsonlite::fromJSON(url)

  # Get the QBR stats by each player (row_n = player)
  get_qbr_data <- function(row_n) {
    purrr::pluck(raw_json, "athletes", "categories", row_n, "totals", 1)
  }

  # unnest_wider() has noisy name repair
  # We'll wrap it in purrr::quietly() and pluck the "result"
  quiet_unnest_wider <- purrr::quietly(tidyr::unnest_wider)

  purrr::pluck(raw_json, "athletes", "athlete") %>%
    dplyr::as_tibble() %>%
    dplyr::select(firstName:shortName, headshot, teamName:teamShortName) %>%
    dplyr::mutate(row_n = dplyr::row_number()) %>%
    dplyr::mutate(data = purrr::map(row_n, get_qbr_data)) %>%
    # lots of name_repair here that I am silencing
    quiet_unnest_wider(data) %>%
    purrr::pluck("result") %>%

    # Names we need to add from the unnest_wider()
    purrr::set_names(nm = c(
      "first_name", "last_name", "name",
      "short_name", "headshot_href", "team_name",
      "team_short_name", "row_n", "qbr_total",
      "points_added", "qb_plays", "total_epa",
      "pass", "run", "exp_sack", "penalty", "raw_qbr", "sack"
    )) %>%
    # Add season and season type back to the main tibble
    dplyr::mutate(
      season = as.double(season),
      season_type = season_type,
      # Get the headshot url
      headshot_href = dplyr::pull(headshot_href, href),

      # Clean up the week numbers if playoffs or regular season
      game_week = dplyr::case_when(
        !is.na(week_current) ~ as.character(week_current),
        !is.na(week_current) & season_type == "Playoffs" ~ as.character(
          factor(week_current,
            levels = c(1:5),
            labels = c(
              "Wild Card",
              "Divisional",
              "Conference Championship",
              "Super Bowl",
              "Super Bowl"
            )
          )
        ),
        is.na(week_current) & season_type == "Playoffs" ~ "Playoff Total",
        is.na(week_current) & season_type == "Regular" ~ "Season Total",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(season:game_week,
      rank = row_n, first_name:short_name,
      team_name, team_short_name, qbr_total:sack, headshot_href
    ) %>%
    dplyr::mutate_at(vars(qbr_total:sack), as.double)
}
