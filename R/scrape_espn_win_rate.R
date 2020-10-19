#' Scrape ESPN Pass/Run Block/Rush Win Rates ratings for a specific season from ESPN's site
#'
#' @return Returns a tibble
#' @export
#' @import dplyr stringr
#' @importFrom dplyr %>%
#' @importFrom readr parse_number
#' @importFrom tidyr separate
#' @importFrom xml2 read_html
#' @importFrom purrr map2_dfr
#' @importFrom rvest html_node html_text
#' @examples
#' # Get off and def pass/run win rates
#' scrape_espn_win_rate()

scrape_espn_win_rate <- function(){

  pbwr_url <- "https://www.espn.com/nfl/story/_/id/29939464/2020-nfl-pass-rushing-run-stopping-blocking-leaderboard-win-rate-rankings"

  stats_in <- c(
    "Pass Rush Win Rate",
    "Run Stop Win Rate",
    "Pass Block Win Rate",
    "Run Block Win Rate"
  )

  raw_html <- read_html(pbwr_url)

  raw_text <- raw_html %>%
    html_nodes("#article-feed > article:nth-child(1) > div > div.article-body > p") %>%
    html_text()

  enframe(raw_text) %>%
    filter(str_detect(value, "1. ")) %>%
    mutate(name = c(
      "Pass Rush Win Rate",
      "Run Stop Win Rate",
      "Pass Block Win Rate",
      "Run Block Win Rate"
    )) %>%
    mutate(value = str_split(value, "\n")) %>%
    unnest_longer(value) %>%
    separate(value, into = c("rank", "team", "win_pct"), sep = "\\. |, ") %>%
    mutate(
      rank = as.integer(rank),
      win_pct = str_remove(win_pct, "%"),
      win_pct = as.double(win_pct)
      ) %>%
    rename(stat = name, stat_rank = rank)

}

scrape_espn_win_rate()

