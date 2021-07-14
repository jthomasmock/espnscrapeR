#' Get NFL standings for a specific season from ESPN's API
#'
#' @param season Either numeric or character
#'
#' @return Returns a tibble
#' @export
#' @import tidyr dplyr purrr httr
#' @importFrom dplyr %>%
#' @importFrom jsonlite fromJSON
#' @examples
#' # Get standings from 2018 season
#' get_nfl_standings(season = "2018")
#'
#' # Get standings from 2010 season
#' get_nfl_standings(2010)
#'
#'
get_nfl_standings <- function(season = 2019, quiet = FALSE) {
  current_year <- as.double(substr(Sys.Date(), 1, 4))

  # Small error handling to guide the limits on years
  if (!between(as.numeric(season), 1990, current_year)) {
    stop(paste("Please choose season between 1990 and", current_year))
  }

  if(isFALSE(quiet)){
    message(glue::glue("Returning {season}"))
  }



  # Working version (no choosing season though)
  raw_url <- "https://site.api.espn.com/apis/v2/sports/football/nfl/standings"

  request <- httr::GET(
    raw_url,
    query = list(
      season = season
    )
  )

  if (httr::http_error(request)) {
    stop(
      sprintf(
        "ESPN API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  resp <- httr::content(request, as = "text", encoding = "UTF-8")

  raw_standings <- jsonlite::parse_json(resp)

  names_fix <- tibble(
    type = c(
      "playoffseed",
      "wins",
      "losses",
      "winpercent",
      "gamesbehind",
      "ties",
      "pointsfor",
      "pointsagainst",
      "differential",
      "streak",
      "clincher",
      "divisionrecord",
      "divisionwins",
      "divisionties",
      "divisionlosses",
      "total",
      "home",
      "road",
      "vsdiv",
      "vsconf"
    ),
    abb = c(
      "seed",
      "wins",
      "losses",
      "win_pct",
      "g_behind",
      "ties",
      "pts_for",
      "pts_against",
      "pts_diff",
      "streak",
      "div_clincher",
      "record_div",
      "div_wins",
      "div_ties",
      "div_losses",
      "record",
      "record_home",
      "record_away",
      "record_div",
      "record_conf"
    )
  )

  full_stand <- raw_standings[["children"]] %>%
    tibble(data = .) %>%
    unnest_wider(data) %>%
    select(conf = abbreviation, standings) %>%
    unnest_wider(standings) %>%
    unnest_longer(entries) %>%
    unnest_wider(entries) %>%
    select(conf, season, team, stats) %>%
    unnest_wider(team) %>%
    hoist(logos, team_logo = list(1, "href")) %>%
    select(
      conf,
      season,
      team_id = id,
      team_location = location,
      team_name = name,
      team_abb = abbreviation,
      team_full = displayName,
      team_logo,
      stats
    ) %>%
    unnest_longer(stats) %>%
    unnest_wider(stats) %>%
    mutate(value = as.character(value)) %>%
    mutate(value = if_else(is.na(value), displayValue, value)) %>%
    select(conf:team_logo, type, value) %>%
    left_join(names_fix, by = "type") %>%
    filter(!(abb == "record_div" & str_length(value) < 2)) %>%
    filter(!is.na(abb), abb != "NA") %>%
    filter(abb != "div_clincher") %>%
    select(-type) %>%
    pivot_wider(
      names_from = abb,
      values_from = value,
      id_cols = conf:team_logo
    ) %>%
    separate(record_home, c("home_wins", "home_losses"), convert = TRUE, extra = "drop") %>%
    separate(record_away, c("away_wins", "away_losses"), convert = TRUE, extra = "drop") %>%
    separate(record_div, c("div_wins", "div_losses"), convert = TRUE, extra = "drop") %>%
    separate(record_conf, c("conf_wins", "conf_losses"), convert = TRUE, extra = "drop") %>%
    mutate(across(c(seed:div_losses, -record), as.double)) %>%
    group_by(conf) %>%
    arrange(conf, desc(win_pct), g_behind) %>%
    mutate(seed = row_number()) %>%
    ungroup()

  full_stand
}
