#' Get NFL schedule for a specific year from ESPN's API
#'
#' @param season Either numeric or character
#'
#' @return Returns a tibble
#' @export
#' @import tidyr dplyr purrr
#' @importFrom dplyr %>%
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @examples
#' # Get standings from 2018 season
#' get_nfl_schedule(season = "2018")

get_nfl_schedule <- function(season){

  message(glue::glue("Returning data for {season}!"))

  # year > 2000
  schedule_api <- glue::glue("http://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard?lang=en&region=us&calendartype=whitelist&limit=1000&dates={season}")

  raw_sched <- fromJSON(schedule_api, simplifyDataFrame = FALSE, simplifyVector = FALSE, simplifyMatrix = FALSE)

  suppressMessages(all_games <- 1:length(raw_sched$events) %>%
    map_dfr(~{raw_sched$events[[.x]] %>%
        enframe() %>%
        pivot_wider(names_from = name, values_from = value) %>%
        unnest(cols = id:shortName) %>%
        unnest_wider(season) %>%
        rename(seasonType = type) %>%
        mutate(competitions = chuck(competitions, 1)) %>%
        hoist(competitions, competitors = "competitors") %>%
        unnest_wider(competitors) %>%
        mutate(home_team_id = map_chr(...1, 1)) %>%
        mutate(away_team_id = map_chr(...2, 1)) %>%
        select(game_id = id, home_team_id, away_team_id, uid:seasonType)
    }) %>%
    mutate(
      home_team = word(shortName, -1),
      away_team = word(shortName, 1),
      home_team_id = as.integer(home_team_id),
      away_team_id = as.integer(away_team_id)
      )
  )

  all_games
}

get_nfl_schedule(2019)

