#' Return all NFL teams with their name, abbreviation,
#'
#' @return
#' @export
#' @importFrom jsonlite fromJSON
#' @import dplyr purrr
#' @examples
#'
#' get_nfl_teams()
get_nfl_teams <- function() {
  message("Getting NFL teams!")

  team_url <- "https://site.api.espn.com/apis/site/v2/sports/football/nfl/teams?&limit=50"
  raw_teams <- jsonlite::fromJSON(team_url)

  purrr::pluck(raw_teams, "sports", "leagues", 1, "teams", 1, "team") %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(logos = purrr::map_chr(logos, function(df) df[1, 1])) %>%
    dplyr::select(id, name:alternateColor, logos, -shortDisplayName) %>%
    purrr::set_names(
      nm = c(
        "uid", "team_name", "team_nickname", "team_short_name", "full_name", "team_color",
        "alternate_color", "logo"
      )
    ) %>%
    dplyr::mutate(
      team_color = paste0("#", team_color),
      alternate_color = paste0("#", alternate_color)
    )
}
