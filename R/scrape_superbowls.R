#' Scrape NFL superbowl winners for all years
#' @return Returns a tibble
#' @export
#' @import dplyr
#' @importFrom dplyr %>%
#' @importFrom xml2 read_html
#' @importFrom rvest html_node html_nodes html_text html_table html_attr
#' @examples
#' # Get superbowls
#' scrape_superbowls()
scrape_superbowls <- function() {

  # Table of all SB winners
  sb_url <- "https://en.wikipedia.org/wiki/List_of_Super_Bowl_champions"

  raw_wiki_sb <- sb_url %>%
    xml2::read_html()

  win_team <- raw_wiki_sb %>%
    rvest::html_nodes("td:nth-child(3)") %>%
    rvest::html_node("[title]") %>%
    rvest::html_attr("title") %>%
    gsub(" season", "", .)


  lose_team <- raw_wiki_sb %>%
    rvest::html_nodes("td:nth-child(5)") %>%
    rvest::html_node("[title]") %>%
    rvest::html_attr("title") %>%
    gsub(" season", "", .)

  raw_scores <- raw_wiki_sb %>%
    rvest::html_nodes("#mw-content-text > div > table:nth-child(16) > tbody > tr") %>%
    rvest::html_nodes("td:nth-child(4) > span") %>%
    rvest::html_text()

  sb_scores <- raw_scores[grepl("[1-9]", raw_scores)]

  win_df <- dplyr::tibble(team = win_team[grepl("[1-9]", win_team)]) %>%
    dplyr::mutate(
      season = substr(team, 1, 4),
      team = substr(team, 6, nchar(team)),
      superbowl = "Won Superbowl",
      sb_points = as.integer(substr(sb_scores, 1, 2))
    )

  lose_df <- dplyr::tibble(team = lose_team[grepl("[1-9]", lose_team)]) %>%
    dplyr::mutate(
      season = substr(team, 1, 4),
      team = substr(team, 6, nchar(team)),
      superbowl = "Lost Superbowl",
      sb_points = as.integer(substr(sb_scores, 4, 5))
    )

  all_sb <- dplyr::bind_rows(win_df, lose_df) %>%
    dplyr::mutate(season = as.integer(season))

  all_sb
}
