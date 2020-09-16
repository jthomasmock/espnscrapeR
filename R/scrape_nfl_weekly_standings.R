#' Scrape NFL weekly outcomes by week
#'
#' @param season character or numeric - greater than 1990
#' @param tidy logical - either TRUE to stack data by game/week or FALSE to return table as is
#' @import purrr tidyr dplyr stringr
#' @importFrom dplyr %>%
#' @importFrom rvest html_table html_node
#' @importFrom xml2 read_html
#' @importFrom glue glue
#' @return tibble
#' @export
#'
#' @examples
#'
#' # Here we run w/ tidy = FALSE to get the exact table from PFR
#' scrape_nfl_weekly_standings(season = 2020, tidy = FALSE)
#'
#' # Here we scrape the outcome and stack the games on top of eachother
#' scrape_nfl_weekly_standings(season = 2020, tidy = TRUE)

scrape_nfl_weekly_standings <- function(season = 2020, tidy = FALSE) {

  current_year <- as.double(substr(Sys.Date(), 1, 4))

  if (!dplyr::between(as.numeric(season), 1990, current_year)) {
    stop(paste("Please choose season between 1990 and", current_year))
  }

  url <- glue::glue("https://www.pro-football-reference.com/years/{season}/games.htm")

  message(glue::glue("Scraping standings from {season}!"))

  raw_html <- read_html(url)

  replace_names <- c(
    "week", "day", "date", "time", "winner_tie", "home_team", "loser_tie",
    "boxscore", "pts_winner", "pts_loser", "yds_winner", "turnovers_winner",
    "yds_loser", "turnovers_loser"
  )


  raw_df <- raw_html %>%
    html_node("#games") %>%
    html_table() %>%
    set_names(nm = replace_names) %>%
    tibble()

  playoff_teams <- c(
    raw_df %>% filter(grepl(x = week, pattern = "Wild|Div|Conf|Super")) %>%
      pull(winner_tie),
    raw_df %>% filter(grepl(x = week, pattern = "Wild|Div|Conf|Super")) %>%
      pull(loser_tie)
  ) %>%
    unique()

  clean_df <- raw_df %>%
    filter(!week %in% c("", "Week")) %>%
    mutate(season = as.integer(season)) %>%
    select(season, everything(), -boxscore) %>%
    mutate_at(vars(pts_winner:turnovers_loser), as.double) %>%
    group_by(pts_winner) %>%
    mutate(game_num = row_number()) %>%
    ungroup() %>%
    mutate(
      winner = case_when(
        pts_winner > pts_loser ~ 1,
        pts_winner < pts_loser ~ 0,
        pts_winner == pts_loser ~ "TIE",
        TRUE ~ NA_real_
        )
      ) %>%
    select(game_num, everything())


  tidy_df <- clean_df %>%
    select(game_num, season:winner_tie, home_team, contains("winner")) %>%
    mutate(home_team = if_else(home_team == "@", 0, 1)) %>%
    set_names(
      nm = c(
        "game_num", "season", "week", "day", "date", "time", "team", "home_team",
        "points", "yards", "turnovers", "winner"
        )
      ) %>%
    bind_rows(
      clean_df %>%
        select(game_num, season:time, loser_tie, home_team, contains("loser"), winner) %>%
        mutate(home_team = if_else(home_team == "@", 1, 0)) %>%
        mutate(winner = if_else(!is.na(winner), 0, NA_real_)) %>%
        set_names(
          nm = c(
            "game_num", "season", "week", "day", "date", "time", "team", "home_team",
            "points", "yards", "turnovers", "winner"
          )
        )
    ) %>%
    mutate(playoffs = if_else(team %in% c(playoff_teams), 1, 0))


  if (tidy == TRUE) {
    tidy_df
  } else if (tidy == FALSE) {
    clean_df
  }
}
