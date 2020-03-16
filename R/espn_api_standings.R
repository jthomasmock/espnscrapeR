#' Get NFL standings for a specific season from ESPN's API
#'
#' @param season Either numeric or character
#'
#' @return Returns a tibble
#' @export get_espn_nfl_standings
#' @import tidyr dplyr purrr
#' @importFrom dplyr %>%
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @examples
#' get_espn_nfl_standings(season = "2018")
#'
#' get_espn_nfl_standings(2010)
#'
get_espn_nfl_standings <- function(season = 2019){

  current_year <- as.double(substr(Sys.Date(), 1, 4))

  # Small error handling to guide the limits on years
  if (!between(as.numeric(season), 1990, current_year)) {
    stop(paste("Please choose season between 1990 and", current_year))
  }

  message(glue::glue("Scraping {season}"))

  # Working version (no choosing season though)
  raw_standings <- jsonlite::fromJSON(glue::glue("https://site.api.espn.com/apis/v2/sports/football/nfl/standings?region=us&lang=en&season={season}"))

  get_data_nfc <- function(row_n){
    purrr::pluck(raw_standings, "children", "standings", "entries", 2, "stats", row_n, "value")
  }

  get_data_afc <- function(row_n){
    purrr::pluck(raw_standings, "children", "standings", "entries", 1, "stats", row_n, "value")
  }

  clean_names_df <- purrr::pluck(raw_standings, "children", "standings", "entries", 2, "stats", 1) %>%
    dplyr::pull(name) %>% janitor::make_clean_names()

  unnest_wider_quiet <- purrr::quietly(tidyr::unnest_wider)


  afc <- purrr::pluck(raw_standings, "children", "standings", "entries") %>%
    .[[1]] %>%
    purrr::pluck("team") %>%
    as_tibble() %>%
    dplyr::mutate(row_n = row_number()) %>%
    dplyr::mutate(data = map(row_n, get_data_afc)) %>%
    dplyr::select(location:displayName, logos, data) %>%
    purrr::set_names(nm = c("city", "team_name", "abb_name", "full_name", "logos", "data")) %>%
    unnest_wider_quiet(data) %>%
    purrr::pluck("result") %>%
    purrr::set_names(nm = c("city", "team_name", "abb_name", "full_name", "logos", clean_names_df)) %>%
    dplyr::arrange(playoff_seed) %>%
    dplyr::mutate(logos = dplyr::pull(logos[[1]], 1))


  nfc <- purrr::pluck(raw_standings, "children", "standings", "entries") %>%
    .[[2]] %>% purrr::pluck("team") %>% as_tibble() %>%
    dplyr::mutate(row_n = row_number()) %>%
    dplyr::mutate(data = map(row_n, get_data_nfc)) %>%
    dplyr::select(location:displayName, logos, data) %>%
    purrr::set_names(nm = c("city", "team_name", "abb_name", "full_name", "logos", "data")) %>%
    unnest_wider_quiet(data) %>%
    purrr::pluck("result") %>%
    purrr::set_names(nm = c("city", "team_name", "abb_name", "full_name", "logos", clean_names_df)) %>%
    dplyr::arrange(playoff_seed) %>%
    dplyr::mutate(logos = dplyr::pull(logos[[1]], 1))

  dplyr::bind_rows(afc, nfc) %>%
    dplyr::mutate(year = season)

  }


