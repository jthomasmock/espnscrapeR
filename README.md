
<!-- README.md is generated from README.Rmd. Please edit that file -->

# espnscrapeR

<!-- badges: start -->

<!-- badges: end -->

The goal of espnscrapeR is to collect or scrape QBR, NFL standings, and
stats from ESPN.

## Installation

The development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("jthomasmock/espnscrapeR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(espnscrapeR)
```

``` r
# Get NFL QBR for the 2019 regular season week 4
get_nfl_qbr("2019", season_type = "Regular", week = 4)
#> Scraping weekly QBR for week 4 of 2019!
#> # A tibble: 31 x 20
#>    season season_type game_week  rank first_name last_name name  short_name
#>     <dbl> <chr>       <chr>     <int> <chr>      <chr>     <chr> <chr>     
#>  1   2019 Regular     4             1 Jameis     Winston   Jame… J. Winston
#>  2   2019 Regular     4             2 Carson     Wentz     Cars… C. Wentz  
#>  3   2019 Regular     4             3 Mason      Rudolph   Maso… M. Rudolph
#>  4   2019 Regular     4             4 Philip     Rivers    Phil… P. Rivers 
#>  5   2019 Regular     4             5 Chase      Daniel    Chas… C. Daniel 
#>  6   2019 Regular     4             6 Dak        Prescott  Dak … D. Presco…
#>  7   2019 Regular     4             7 Matthew    Stafford  Matt… M. Staffo…
#>  8   2019 Regular     4             8 Joe        Flacco    Joe … J. Flacco 
#>  9   2019 Regular     4             9 Aaron      Rodgers   Aaro… A. Rodgers
#> 10   2019 Regular     4            10 Baker      Mayfield  Bake… B. Mayfie…
#> # … with 21 more rows, and 12 more variables: team_name <chr>,
#> #   team_short_name <chr>, qbr_total <dbl>, points_added <dbl>, qb_plays <dbl>,
#> #   total_epa <dbl>, pass <dbl>, run <dbl>, exp_sack <dbl>, penalty <dbl>,
#> #   raw_qbr <dbl>, sack <dbl>
```

``` r
# Get NFL standings for 2010
get_espn_nfl_standings(2010)
#> Scraping 2010
#> # A tibble: 32 x 27
#>    city  team_name abb_name full_name logos playoff_seed  wins losses
#>    <chr> <chr>     <chr>    <chr>     <chr>        <dbl> <dbl>  <dbl>
#>  1 Pitt… Steelers  PIT      Pittsbur… http…            0    12      4
#>  2 Indi… Colts     IND      Indianap… http…            0    10      6
#>  3 Kans… Chiefs    KC       Kansas C… http…            0    10      6
#>  4 Balt… Ravens    BAL      Baltimor… http…            0    12      4
#>  5 New … Jets      NYJ      New York… http…            0    11      5
#>  6 New … Patriots  NE       New Engl… http…            0    14      2
#>  7 San … Chargers  SD       San Dieg… http…            0     9      7
#>  8 Jack… Jaguars   JAX      Jacksonv… http…            0     8      8
#>  9 Oakl… Raiders   OAK      Oakland … http…            0     8      8
#> 10 Miami Dolphins  MIA      Miami Do… http…            0     7      9
#> # … with 22 more rows, and 19 more variables: win_percent <dbl>,
#> #   games_behind <dbl>, ties <dbl>, points_for <dbl>, points_against <dbl>,
#> #   differential <dbl>, streak <dbl>, clincher <dbl>, league_win_percent <dbl>,
#> #   division_record <dbl>, division_wins <dbl>, division_ties <dbl>,
#> #   division_losses <dbl>, all_splits <dbl>, home <dbl>, road <dbl>,
#> #   vs_div <dbl>, vs_conf <dbl>, year <dbl>
```

``` r
# Get NFL 
scrape_espn_stats(2019, stat = "rushing")
#> Scraping rushing stats from 2019 Regular season!
#> # A tibble: 335 x 17
#>    season season_type rush_rank name  team  pos   games_played rush_att
#>     <dbl> <chr>           <int> <chr> <chr> <chr>        <int>    <int>
#>  1   2019 Regular             1 Derr… TEN   RB              15      303
#>  2   2019 Regular             2 Nick… CLE   RB              16      298
#>  3   2019 Regular             3 Chri… CAR   RB              16      287
#>  4   2019 Regular             4 Ezek… DAL   RB              16      301
#>  5   2019 Regular             5 Chri… SEA   RB              15      278
#>  6   2019 Regular             6 Lama… BAL   QB              15      176
#>  7   2019 Regular             7 Leon… JAX   RB              15      265
#>  8   2019 Regular             8 Josh… OAK   RB              13      242
#>  9   2019 Regular             9 Joe … CIN   RB              16      278
#> 10   2019 Regular            10 Dalv… MIN   RB              14      250
#> # … with 325 more rows, and 9 more variables: rush_yards <dbl>, rush_avg <dbl>,
#> #   rush_long <int>, rush_20plus <int>, rush_td <int>, rush_yards_game <dbl>,
#> #   fumble <int>, fumble_lost <int>, rush_first_down <int>
```

``` r
# Get college QBR for 2014 week 5
get_espn_college_qbr(season = 2014, week = 5)
#> Scraping QBR for week 5 of 2014!
#> # A tibble: 106 x 20
#>    season  week first_name last_name name  short_name team_name team_short_name
#>     <dbl> <dbl> <chr>      <chr>     <chr> <chr>      <chr>     <chr>          
#>  1   2014     5 Brandon    Harris    Bran… B. Harris  Tigers    LSU            
#>  2   2014     5 Brett      Hundley   Bret… B. Hundley Bruins    UCLA           
#>  3   2014     5 Gary       Nova      Gary… G. Nova    Scarlet … RUTG           
#>  4   2014     5 Deshaun    Watson    Desh… D. Watson  Tigers    CLEM           
#>  5   2014     5 A.J.       Schurr    A.J.… A.J. Schu… Black Kn… ARMY           
#>  6   2014     5 Nick       Marshall  Nick… N. Marsha… Tigers    AUB            
#>  7   2014     5 Justin     Worley    Just… J. Worley  Voluntee… TENN           
#>  8   2014     5 C.J.       Brown     C.J.… C.J. Brown Terrapins MD             
#>  9   2014     5 Gunner     Kiel      Gunn… G. Kiel    Bearcats  CIN            
#> 10   2014     5 Caleb      Rowe      Cale… C. Rowe    Terrapins MD             
#> # … with 96 more rows, and 12 more variables: age <int>, row_n <int>,
#> #   qbr_total <dbl>, points_added <dbl>, qb_plays <dbl>, total_epa <dbl>,
#> #   pass <dbl>, run <dbl>, exp_sack <dbl>, penalty <dbl>, raw_qbr <dbl>,
#> #   sack <dbl>
```
