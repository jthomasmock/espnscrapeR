#' Get NFL schedule for a specific year from ESPN's API
#'
#' @param season Either numeric or character
#'
#' @return Returns a tibble
#' @export
#' @import tidyr dplyr purrr
#' @importFrom dplyr %>%
#' @importFrom httr stop_for_status GET content
#' @importFrom glue glue
#' @examples
#' # Get all games from 2018 season, note that this will have some overlap
#' # between seasons, for example 2018 returns 2018-01-01 to 2018-12-31
#' get_nfl_schedule(season = "2018")

get_nfl_schedule <- function(season){

  message(glue::glue("Returning data for {season}!"))

  max_year <- substr(Sys.Date(), 1,4)

  if(!(as.integer(substr(season, 1, 4)) %in% c(1969:max_year))){
    message(paste("Error: Season must be between 1969 and", max_year))
  }

  # year > 1969
  season <- as.character(season)
  if(nchar(season) > 4){
    season_dates <- season
  } else {
    season_dates <- glue::glue("{season}0101-{season}1231")
  }

  raw_url <- "http://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard"

  raw_get <- httr::GET(
    raw_url,
    query = list(
      limit = 1000,
      dates = season_dates
    )
  )

  httr::stop_for_status(raw_get)

  raw_sched <- httr::content(raw_get)

  nfl_data <- raw_sched[["events"]] %>%
    tibble(data = .) %>%
    unnest_wider(data) %>%
    unchop(competitions) %>%
    select(-id, -uid, -date, -status) %>%
    unnest_wider(competitions) %>%
    rename(matchup = name, matchup_short = shortName, game_id = id, game_uid = uid, game_date = date) %>%
    hoist(status,
          status_name = list("type", "name")) %>%
    select(!any_of(c("timeValid", "neutralSite", "conferenceCompetition","recent", "type"))) %>%
    unnest_wider(season) %>%
    rename(season = year) %>%
    unest_wider(week) %>%
    rename(week = number) %>%
    select(-any_of("status")) %>%
    hoist(
      competitors,
      home_team_name = list(1, "team", "name"),
      home_team_logo = list(1, "team", "logo"),
      home_team_abb = list(1, "team", "abbreviation"),
      home_team_id = list(1, "team", "id"),
      home_team_location = list(1, "team", "location"),
      home_team_full = list(1, "team", "displayName"),
      home_team_color = list(1, "team", "color"),
      home_score = list(1, "score"),
      home_win = list(1, "winner"),
      home_record = list(1, "records", 1, "summary"),
      # away team
      away_team_name = list(2, "team", "name"),
      away_team_logo = list(2, "team", "logo"),
      away_team_abb = list(2, "team", "abbreviation"),
      away_team_id = list(2, "team", "id"),
      away_team_location = list(2, "team", "location"),
      away_team_full = list(2, "team", "displayName"),
      away_team_color = list(2, "team", "color"),
      away_score = list(2, "score"),
      away_win = list(2, "winner"),
      away_record = list(2, "records", 1, "summary")
    ) %>%
    mutate(home_win = as.integer(home_win),
           away_win = as.integer(away_win),
           home_score = as.integer(home_score),
           away_score = as.integer(away_score))

  if("leaders" %in% names(nfl_data)){
    schedule_out <- nfl_data %>%
      hoist(
        leaders,
        pass_leader_yards = list(1, "leaders", 1, "value"),
        pass_leader_stat = list(1, "leaders", 1, "displayValue"),
        pass_leader_name = list(1, "leaders", 1, "athlete", "displayName"),
        pass_leader_shortname = list(1, "leaders", 1, "athlete", "shortName"),
        pass_leader_headshot = list(1, "leaders", 1, "athlete", "headshot"),
        pass_leader_team_id = list(1, "leaders", 1, "team", "id"),
        pass_leader_pos = list(1, "leaders", 1, "athlete", "position", "abbreviation"),
        # rushing
        rush_leader_yards = list(2, "leaders", 1, "value"),
        rush_leader_stat = list(2, "leaders", 1, "displayValue"),
        rush_leader_name = list(2, "leaders", 1, "athlete", "displayName"),
        rush_leader_shortname = list(2, "leaders", 1, "athlete", "shortName"),
        rush_leader_headshot = list(2, "leaders", 1, "athlete", "headshot"),
        rush_leader_team_id = list(2, "leaders", 1, "team", "id"),
        rush_leader_pos = list(2, "leaders", 1, "athlete", "position", "abbreviation"),
        # receiving
        rec_leader_yards = list(3, "leaders", 1, "value"),
        rec_leader_stat = list(3, "leaders", 1, "displayValue"),
        rec_leader_name = list(3, "leaders", 1, "athlete", "displayName"),
        rec_leader_shortname = list(3, "leaders", 1, "athlete", "shortName"),
        rec_leader_headshot = list(3, "leaders", 1, "athlete", "headshot"),
        rec_leader_team_id = list(3, "leaders", 1, "team", "id"),
        rec_leader_pos = list(3, "leaders", 1, "athlete", "position", "abbreviation")
      )

    if('venue' %in% names(schedule_out)){
      schedule_out = schedule_out %>%
        hoist(
          venue,
          venue_id = 'id',
          venue_name = 'fullName',
          venue_city = list('address', 'city'),
          venue_state = list('address', 'state'),
          capacity = 'capacity',
          indoor = 'indoor'
        ) %>%
        select(-venue)
    }
    if("broadcasts" %in% names(schedule_out)) {
      schedule_out %>%
        hoist(
          broadcasts,
          broadcast_market = list(1, "market"),
          broadcast_name = list(1, "names", 1)
        ) %>%
        select(!where(is.list))
    } else {
      schedule_out
    }
  } else {
    nfl_data %>% select(!where(is.list))
  }

}


