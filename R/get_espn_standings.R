#' Collect NFL standings for a specific season from ESPN's API
#'
#' @param season Either numeric or character
#'
#' @return Returns a tibble
#' @export
#' @import jsonlite tidyr dplyr purrr glue
get_espn_standings <- function(season = 2019){

  current_year <- as.double(substr(Sys.Date(), 1, 4))

  # Small error handling to guide the limits on years
  if (!between(as.numeric(season), 1990, current_year)) {
    stop(paste("Please choose season between 1990 and", current_year))
  }

  stand_url <- paste0("https://site.api.espn.com/apis/v2/sports/football/nfl/standings?&season=", season)

  raw_stand <- fromJSON(stand_url)

  cleaned_names <-
    c("id", "uid", "location", "name", "team_code", "full_name", "logo",
      "playoff_seed", "wins", "losses", "win_percent", "games_behind", "ties",
      "points_for", "points_against", "differential", "streak", "clincher",
      "league_win_percent", "division_record", "division_wins", "division_ties",
      "division_losses", "total", "home", "road", "vs_div", "vs_conf", "division_win_percent")

  suppressWarnings(
    clean_df <- raw_stand %>%
      pluck("children", "standings", "entries", 2, "team") %>%
      as_tibble() %>%
      mutate(logos = pull(logos[[1]], 1),
             group_num = row_number()) %>%
      left_join(
        raw_stand %>%
          pluck("children", "standings", "entries", 2, "stats") %>%
          bind_rows(.id = "group_num") %>% as_tibble() %>%
          mutate(group_num = as.double(group_num)) %>%
          select(group_num, type, displayValue) %>%
          group_by(group_num) %>%
          pivot_wider(names_from = type, values_from = displayValue),
        by = c("group_num")
      ) %>%
      select(-links, -group_num, -isActive, -shortDisplayName) %>%
      set_names(nm = cleaned_names) %>%
      mutate_at(
        vars(playoff_seed:differential, league_win_percent,
             division_wins:division_losses, division_win_percent),
        readr::parse_double)
  )

  suppressWarnings(
    clean_df2 <- raw_stand %>%
      pluck("children", "standings", "entries", 1, "team") %>%
      as_tibble() %>%
      mutate(logos = pull(logos[[1]], 1),
             group_num = row_number()) %>%
      left_join(
        raw_stand %>%
          pluck("children", "standings", "entries", 1, "stats") %>%
          bind_rows(.id = "group_num") %>% as_tibble() %>%
          mutate(group_num = as.double(group_num)) %>%
          select(group_num, type, displayValue) %>%
          group_by(group_num) %>%
          pivot_wider(names_from = type, values_from = displayValue),
        by = c("group_num")
      ) %>%
      select(-links, -group_num, -isActive, -shortDisplayName) %>%
      set_names(nm = cleaned_names) %>%
      mutate_at(
        vars(playoff_seed:differential, league_win_percent,
             division_wins:division_losses, division_win_percent),
        readr::parse_double)
  )


  bind_rows(clean_df, clean_df2) %>%
    add_column(season = season, .after = "uid")
  }
