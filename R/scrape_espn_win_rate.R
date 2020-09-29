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

  nodes_in <- c(
    "#article-feed > article:nth-child(1) > div > div.article-body > p:nth-child(39)",
    "#article-feed > article:nth-child(1) > div > div.article-body > p:nth-child(43)",
    "#article-feed > article:nth-child(1) > div > div.article-body > p:nth-child(47)",
    "#article-feed > article:nth-child(1) > div > div.article-body > p:nth-child(52)"
  )

  raw_html <- read_html(pbwr_url)

  get_table <- function(node_in, measure){

    raw_html %>%
      html_node(node_in) %>%
      html_text() %>%
      str_split("\n") %>%
      .[[1]] %>%
      tibble(data = .) %>%
      separate(data, into = c("rank", "team", "win_pct"), sep = "\\. |, ") %>%
      mutate(
        rank = as.integer(rank),
        win_pct = parse_number(win_pct)/100,
        stat = measure,
        role = if_else(str_detect(measure, "Block"), "Offense", "Defense")
      )
  }

  df_out <- map2_dfr(nodes_in, stats_in, ~get_table(.x, .y))

  df_out

}

