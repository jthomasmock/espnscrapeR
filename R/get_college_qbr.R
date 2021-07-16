#' Get ESPN QBR for College football
#'
#' @param season Numeric or character - greater than 2004
#' @param type character - "season" or "weekly"
#' @import tidyr dplyr purrr httr
#' @importFrom dplyr %>%
#' @importFrom tibble enframe
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @return tibble
#' @export
#'
#' @examples
#'
#' # Get college QBR from 2014 for players at season level
#' get_college_qbr(season = 2014, type = "season")
#'
#' # Get weekly college QBR from 2019
#' # Note the mix of playoffs/bowls and regular season
#' get_college_qbr(2019, type = "weekly")
get_college_qbr <- function(season = 2020, type = "season") {
  current_year <- as.double(substr(Sys.Date(), 1, 4))

  # Small error handling to guide the limits on years
  if (!dplyr::between(as.numeric(season), 2004, current_year)) {
    stop(paste("Please choose season between 2004 and", current_year))
  }

  if (!(type %in% c("season", "weekly"))) {
    stop("Please choose `season` or `weekly` for the `type`")
  }

  # Add message according to totals or weeks
  message(
    dplyr::if_else(
      type == "season",
      glue::glue("Scraping QBR totals for {season}!"),
      glue::glue("Scraping QBR for all weeks of {season}!")
    )
  )

  # Build base url
  base_url <- "https://site.web.api.espn.com/apis/fitt/v3/sports/football/college-football/qbr"

  query_type <-
    if (type == "weekly") {
      list(
        qbrType = "weeks",
        season = season,
        limit = 200
      )
    } else if (type == "season") {
      list(
        qbrType = "seasons",
        season = season,
        limit = 200
      )
    }

  raw_get <- httr::GET(
    url = base_url,
    query = query_type
  )

  httr::stop_for_status(raw_get)

  raw_json <- httr::content(raw_get)

  if (!("athletes" %in% names(raw_json))) {
    stop("ESPN has missing data")
  }

  # check pagination
  n_pages <- raw_json[["pagination"]][["pages"]]

  if (type == "weekly") {

    # loop through pages
    raw_data <- 1:n_pages %>%
      paste0(raw_get$url, "&page=", .) %>%
      purrr::map(~ httr::GET(.x) %>% httr::content()) %>%
      purrr::map("athletes") %>%
      tibble::enframe() %>%
      tidyr::unnest_longer(value)
  } else if (type == "season") {
    raw_data <- raw_json[["athletes"]] %>%
      tibble::enframe()
  }

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

  if (type == "weekly") {
    raw_data %>%
      tidyr::unnest_wider(value) %>%
      tidyr::unnest_wider(athlete) %>%
      tidyr::hoist(
        categories,
        qbr_values = list(1, "totals")
      ) %>%
      tidyr::hoist(
        headshot,
        headshot_href = "href"
      ) %>%
      tidyr::hoist(
        game,
        game_id = "id",
        game_date = "date",
        week = "weekNumber",
        week_text = "weekText",
        player_home_away = "homeAway",
        score = "score",
        opp_team_id =         list("teamOpponent", "id"),
        opp_team_name =       list("teamOpponent", "name"),
        opp_team_short_name = list("teamOpponent", "abbreviation"),
      ) %>%
      janitor::clean_names() %>%
      dplyr::rename(
        player_id = id,
        player_uid = uid,
        player_guid = guid,
        team_uid = team_u_id
      ) %>%
      dplyr::select(-name, -teams, -categories, -headshot, -position, -status, -links, -type) %>%
      dplyr::mutate(qbr_names = list(qbr_names)) %>%
      tidyr::unchop(c(qbr_values, qbr_names)) %>%
      tidyr::unchop(qbr_values) %>%
      tidyr::pivot_wider(names_from = qbr_names, values_from = qbr_values) %>%
      janitor::clean_names() %>%
      dplyr::mutate(season = as.integer(season)) %>%
      dplyr::mutate_at(vars(qbr_total:sack), as.double) %>%
      dplyr::mutate(
        week_type = dplyr::if_else(
          grepl(x = week_text, pattern = "Bowl"),
          "Bowls",
          "Regular"
        )
      ) %>%
      dplyr::select(season, contains("week"), dplyr::everything(), -game) %>%
      dplyr::arrange(desc(week_type), week, desc(qbr_total))

  } else if (type == "season") {
    raw_data %>%
      tidyr::unnest_wider(value) %>%
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
      tidyr::unchop(qbr_values:qbr_names) %>%
      tidyr::unchop(qbr_values) %>%
      tidyr::pivot_wider(names_from = qbr_names, values_from = qbr_values) %>%
      janitor::clean_names() %>%
      dplyr::rename(
        player_id = id,
        player_uid = uid,
        player_guid = guid,
        team_uid = team_u_id
      ) %>%
      dplyr::mutate(season = as.integer(season), week_text = "Season Level", week = NA) %>%
      dplyr::mutate_at(vars(qbr_total:sack), as.double) %>%
      dplyr::select(season, week, week_text, dplyr::everything())
  }
}
