#' Return all NFL teams with their name, abbreviation,
#'
#' @return tibble
#' @export
#' @importFrom httr content GET
#' @import dplyr purrr
#' @examples
#'
#' get_nfl_teams()
get_nfl_teams <- function() {
  message("Getting NFL teams!")

  team_url <- "https://site.api.espn.com/apis/site/v2/sports/football/nfl/teams"

  raw_teams <- httr::GET(team_url, query = list(limit = "50")) %>%
    httr::content()

  purrr::pluck(raw_teams, "sports", 1, "leagues", 1, "teams") %>%
    dplyr::tibble(value = .) %>%
    tidyr::unnest_wider(value) %>%
    tidyr::unnest_wider(team) %>%
    tidyr::hoist(
      logos,
      logo = list(1, "href")
    ) %>%
    dplyr::select(id, name, nickname, abbreviation, displayName, color, alternateColor, logo) %>%
    purrr::set_names(
      nm = c(
        "team_id", "team_name", "team_nickname", "team_abb", "team_full_name", "team_color",
        "team_alt_color", "logo"
      )
    ) %>%
    dplyr::mutate(
      team_color = paste0("#", team_color),
      team_alt_color = paste0("#", team_alt_color)
    )
}
