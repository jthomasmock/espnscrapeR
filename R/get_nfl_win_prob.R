#' Get NFL in-game win probabilities for a specific game from ESPN
#'
#' @param game_id Character string - can be acquired from the website of a specific game, or from espnscrapeR::get_nfl_schedule()
#'
#' @return Returns a tibble
#' @export
#' @import tidyr dplyr purrr
#' @importFrom dplyr %>%
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @importFrom tibble enframe
#' @examples
#' # Get win prob from specific game
#' get_espn_win_prob(game_id = "401030956")


get_espn_win_prob <- function(game_id){

  current_year <- as.double(substr(Sys.Date(), 1, 4))

  # Error handling for limits on season
  if (!dplyr::between(as.numeric(season), 2015, current_year)) {
    stop(paste("Please choose season between 2015 and", current_year))
  }

  raw_url <-glue::glue("https://www.espn.com/nfl/game?gameId={game_id}")

  raw_html <- raw_url %>%
    read_html()

  raw_text <- raw_html %>%
    html_nodes("script") %>%
    .[23] %>%
    html_text()

  raw_json <- raw_text %>%
    gsub(".*(\\[\\{)", "\\1", .) %>%
    gsub("(\\}\\]).*", "\\1", .)

  parsed_json <- jsonlite::parse_json(raw_json)

  raw_df <- parsed_json %>%
    enframe() %>%
    rename(row_id = name) %>%
    unnest_wider(value) %>%
    unnest_wider(play) %>%
    hoist(period, quarter = "number") %>%
    unnest_wider(start) %>%
    hoist(team, pos_team_id = "id") %>%
    hoist(clock, clock = "displayValue") %>%
    hoist(type, play_type = "text") %>%
    select(-type) %>%
    janitor::clean_names() %>%
    mutate(game_id = game_id)

  raw_df

}
