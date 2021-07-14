#' Get ESPN QBR for NFL football
#'
#' @param season Either numeric or character
#' @param week Either NA to return season or week 1 to 4 for playoffs or 1 to 17 for regular
#' @param season_type Character - either "Regular" or "Playoffs"
#' @return Returns a tibble
#' @export
#' @import tidyr dplyr purrr httr
#' @importFrom dplyr %>%
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @examples
#' # Get ALL Playoff QBR from 2016 season
#' get_nfl_qbr("2016", season_type = "Playoffs", week = NA)
#'
#' # Get Regular season QBR for week 4 of 2019
#' get_nfl_qbr("2019", season_type = "Regular", week = 4)
get_nfl_qbr <- function(season = 2020, week = NA, season_type = "Regular") {
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
  if (!is.na(week) & season_type == "Regular" & !dplyr::between(as.numeric(week), 1, 18)) {
    stop("Please choose regular season week between 1 and 18")
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
    5L,
    # default to normal week
    as.integer(week)
  )

  # Add useful messages - separated by week
  message(dplyr::if_else(
    is.na(week),
    glue::glue("Scraping QBR totals for {season}!"),
    glue::glue("Scraping weekly QBR for week {week} of {season}!")
  ))

  # Build up URL - not sure if this makes it easier to read, but it makes it
  # closer to 80ish characters per line
  url_start <- "https://site.web.api.espn.com/apis/fitt/v3/sports/football/nfl/qbr"

  query_type <- if (is.na(week) & season_type == "Regular") {
    list(
      qbrType = "seasons",
      seasonType = 2,
      isqualified = "true",
      season = season
    )
  } else if (!is.na(week) & season_type == "Regular") {
    list(
      qbrType = "weeks",
      seasontype = 2,
      isqualified = "true",
      season = season,
      week = week
    )
  } else if (is.na(week) & season_type == "Playoffs") {
    list(
      qbrType = "seasons",
      seasontype = 3,
      isqualified = "true",
      season = season
    )
  } else if (!is.na(week) & season_type == "Playoffs") {
    list(
      qbrType = "weeks",
      seasontype = 3,
      isqualified = "true",
      season = season,
      week = week_current
    )
  }

  raw_get <- httr::GET(
    url = url_start,
    query = query_type
  )

  httr::stop_for_status(raw_get)

  raw_json <- httr::content(raw_get)

  in_nm <- c(
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

  if (is.na(week)) {
    final_df <- raw_json[["athletes"]] %>%
      tibble::enframe() %>%
      tidyr::unnest_wider(value) %>%
      tidyr::hoist(categories, vals = list(1, "totals")) %>%
      dplyr::select(-name, -categories) %>%
      tidyr::unnest_wider(athlete) %>%
      tidyr::hoist(
        headshot,
        headshot_href = "href"
      ) %>%
      dplyr::select(
        team_abb = teamShortName,
        player_id = id,
        short_name = shortName,
        vals,
        first_name = firstName,
        last_name = lastName,
        name = displayName,
        headshot_href,
        team = teamName
      ) %>%
      dplyr::mutate(vals = purrr::map(vals, ~ purrr::set_names(.x, in_nm))) %>%
      tidyr::unnest_wider(vals) %>%
      dplyr::mutate(dplyr::across(qbr_total:sack, as.double)) %>%
      dplyr::mutate(
        rank = rank(desc(qbr_total)),
        game_week = "Season Total",
        season = season,
        season_type = season_type
      ) %>%
      dplyr::select(
        season,
        season_type,
        game_week,
        team_abb,
        player_id,
        short_name,
        rank,
        qbr_total:sack,
        first_name,
        last_name,
        name,
        headshot_href,
        team
      )
  } else {
    final_df <- raw_json[["athletes"]] %>%
      tibble::enframe() %>%
      tidyr::unnest_wider(value) %>%
      tidyr::hoist(categories, vals = list(1, "totals")) %>%
      tidyr::hoist(
        game,
        game_id = "id",
        game_date = "date",
        game_score = "score",
        home_away = "homeAway",
        week_num = "weekNumber",
        week_text = "weekText",
        opp_id = list("teamOpponent", "id"),
        opp_abb = list("teamOpponent", "abbreviation"),
        opp_team = list("teamOpponent", "displayName"),
        opp_name = list("teamOpponent", "name")
      ) %>%
      dplyr::select(-name, -categories, -game) %>%
      tidyr::unnest_wider(athlete) %>%
      tidyr::hoist(
        headshot,
        headshot_href = "href"
      ) %>%
      dplyr::select(
        game_id,
        week_num,
        week_text,
        team_abb = teamShortName,
        player_id = id,
        short_name = shortName,
        vals,
        first_name = firstName,
        last_name = lastName,
        name = displayName,
        headshot_href,
        team = teamName,
        opp_id:opp_name,
      ) %>%
      dplyr::mutate(vals = purrr::map(vals, ~ purrr::set_names(.x, in_nm))) %>%
      tidyr::unnest_wider(vals) %>%
      dplyr::mutate(dplyr::across(qbr_total:sack, as.double)) %>%
      dplyr::mutate(
        rank = rank(desc(qbr_total)),
        game_week = as.integer(week),
        season = season,
        season_type = season_type
      ) %>%
      dplyr::select(
        season,
        season_type,
        game_id,
        game_week,
        week_text,
        team_abb,
        player_id,
        short_name,
        rank,
        qbr_total:sack,
        first_name,
        last_name,
        name,
        headshot_href,
        team,
        opp_id:opp_name,
        week_num
      )
  }

  final_df
}
