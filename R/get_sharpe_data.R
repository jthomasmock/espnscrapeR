#' Get some of Lee Sharpe's datasets from GitHub
#'
#' @param dataset Character only, one of "airports", "closing_lines", "draft_picks", "draft_values", "games", "logos", "pff_pfr_map_v1", "positions", "rosters", "sc_lines", "standings", "teamcolors", "teams", "trades", "win_totals"
#' @importFrom glue glue
#' @importFrom readr read_csv
#' @return tibble
#' @export
#'
#' @examples
#'
#' # Get NFL games and their outcomes
#' get_sharpe_data(dataset = "games")
#'
#' # Get team abbreviations/names/etc by year
#' get_sharpe_data(dataset = "teams")

get_sharpe_data <- function(dataset = "games"){

  datasets <- c(
    "airports",
    "closing_lines",
    "draft_picks",
    "draft_values",
    "games",
    "logos",
    "pff_pfr_map_v1",
    "positions",
    "rosters",
    "sc_lines",
    "standings",
    "teamcolors",
    "teams",
    "trades",
    "win_totals"
  )

  collapsed_data <- paste0(datasets, collapse = ", ")

  if(!(dataset %in% datasets)){
    stop(c("Dataset not found, please use one of: ", collapsed_data))
  }

  read_url <- glue::glue("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/{dataset}.csv")

  raw_df <- read_csv(read_url, guess_max = 10000)

  raw_df
  }
