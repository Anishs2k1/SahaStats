library(espnscrapeR)
> grid %>%
    ggplot( aes(x=years, y=wins)) +
    geom_line( color="grey") +
    geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
    theme_ipsum() +
    ggtitle("Bucs 2015-2020")

ninebucs <- get_nfl_standings(2019)
twentybucs <- get_nfl_standings(2020)
eightbucs <- get_nfl_standings(2018)
sevenbucs <- get_nfl_standings(2017)
sixbucs <- get_nfl_standings(2016)

ninebucs <- ninebucs %>%
  + filter(abb_name == 'TB')
ninebucs <- ninebucs$wins

rbdatanine <-  readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))
rbdatanine <- rbdatanine %>% filter(posteam == 'TB', rush == 1)%>%
  group_by(rusher) %>%
  summarize(
    mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()
  ) %>%
  arrange(-mean_epa) %>%
  filter(plays > 20)

rodgerseight <- filter(rbdataeight, rusher == 'J.Rodgers')
rjonesnine <- filter(rbdatanine, rusher == 'R.Jones')
rjoneseight <- filter(rbdataeight, rusher == 'R.Jones')
ltwenty <- filter(rbdatatwenty, rusher == 'L.Fournette')
kvaughntwenty <- filter(rbdatatwenty, rusher == 'K. Vaughn')
pbarbernine <- filter(rbdatanine, rusher == 'P.Barber')
jonestable <- rbindlist(list(rjoneseight, rjonesnine, rjonestwenty), use.names = TRUE, fill = TRUE)


jonestable %>%
       tail(10) %>%
       ggplot( aes(x= years, y= mean_epa)) +
       geom_line( color="grey") +
       geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
       theme_ipsum() +
       ggtitle("R. Jones")
plot(jonestable$mean_epa~jonestable$years , type="b" , lwd=3 , col=rgb(0.1,0.7,0.1,0.8) , ylab='mean_epa' , xlab="Years" , xtitle = 'R.Jones', bty="l" , pch=20 , cex=4)
rbdatatwenty %>%
            ggplot( aes(x=rusher, y=mean_epa)) +
            geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
            xlab("") +
            theme_bw() +
scale_y_reverse()
> ninebucs
# A tibble: 32 x 28
city  team_name abb_name full_name logos playoff_seed  wins losses win_percent games_behind  ties
<chr> <chr>     <chr>    <chr>     <chr>        <dbl> <dbl>  <dbl>       <dbl>        <dbl> <dbl>
  1 Balt… Ravens    BAL      Baltimor… http…            1    14      2       0.875            0     0
2 Kans… Chiefs    KC       Kansas C… http…            2    12      4       0.75             2     0
3 New … Patriots  NE       New Engl… http…            3    12      4       0.75             2     0
4 Hous… Texans    HOU      Houston … http…            4    10      6       0.625            4     0
5 Buff… Bills     BUF      Buffalo … http…            5    10      6       0.625            4     0
6 Tenn… Titans    TEN      Tennesse… http…            6     9      7       0.562            5     0
7 Pitt… Steelers  PIT      Pittsbur… http…            7     8      8       0.5              6     0
8 Denv… Broncos   DEN      Denver B… http…            8     7      9       0.438            7     0
9 Oakl… Raiders   OAK      Oakland … http…            9     7      9       0.438            7     0
10 Indi… Colts     IND      Indianap… http…           10     7      9       0.438            7     0
# … with 22 more rows, and 17 more variables: points_for <dbl>, points_against <dbl>, differential <dbl>,
#   streak <dbl>, clincher <dbl>, league_win_percent <dbl>, division_record <dbl>, division_wins <dbl>,
#   division_ties <dbl>, division_losses <dbl>, all_splits <dbl>, home <dbl>, road <dbl>, vs_div <dbl>,
#   vs_conf <dbl>, conf <chr>, year <dbl>
> ninebucs <- ninebucs %>%
  + filter(abb_name == 'TB')
> ninebucs
# A tibble: 1 x 28
city  team_name abb_name full_name logos playoff_seed  wins losses win_percent games_behind  ties points_for
<chr> <chr>     <chr>    <chr>     <chr>        <dbl> <dbl>  <dbl>       <dbl>        <dbl> <dbl>      <dbl>
  1 Tamp… Buccanee… TB       Tampa Ba… http…           11     7      9       0.438            6     0        458
# … with 16 more variables: points_against <dbl>, differential <dbl>, streak <dbl>, clincher <dbl>,
#   league_win_percent <dbl>, division_record <dbl>, division_wins <dbl>, division_ties <dbl>,
#   division_losses <dbl>, all_splits <dbl>, home <dbl>, road <dbl>, vs_div <dbl>, vs_conf <dbl>, conf <chr>,
#   year <dbl>
> ninebucs <- ninebucs %>%
  + filter(wins)
Error: Problem with `filter()` input `..1`.
x Input `..1` must be a logical vector, not a double.
ℹ Input `..1` is `wins`.
Run `rlang::last_error()` to see where the error occurred.
> ninebucs <- ninebucs$wins
> ninebucs
[1] 7
> library(nflfastR)
> rbdata <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))
> rbdata %>%
  +      filter(posteam == 'TB', rush == 1) %>%
  +      group_by(rusher) %>%
  +      summarize(
    +              mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()
    +          ) %>%v
Error in v(.) : could not find function "v"
>      arrange(-mean_epa) %>%
  +          filter(plays > 20)
Error in arrange(-mean_epa) : object 'mean_epa' not found
> rbdata <- rbdata %>%
  + filter(rush == 1, !is.na(epa))
> rbdata <- filter(posteam == 'TB', rush == 1) %>%
  +     group_by(rusher) %>%
  +      summarize(
    +              mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()
    +          ) %>%v
Error in v(.) : could not find function "v"
>      arrange(-mean_epa) %>%
  +          filter(plays > 20)
Error in arrange(-mean_epa) : object 'mean_epa' not found
> rbdata <- filter(posteam == 'CLE', rush == 1) %>%
  +      group_by(rusher) %>%
  +      summarize(
    +              mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()
    +     ) %>%
  +          arrange(-mean_epa) %>%
  +          filter(plays > 20)
Error in filter(posteam == "CLE", rush == 1) : object 'posteam' not found
> rbdata
# A tibble: 13,134 x 340
play_id game_id old_game_id home_team away_team season_type  week posteam posteam_type defteam
<dbl> <chr>   <chr>       <chr>     <chr>     <chr>       <int> <chr>   <chr>        <chr>  
  1      79 2019_0… 2019090804  MIN       ATL       REG             1 ATL     away         MIN    
2     148 2019_0… 2019090804  MIN       ATL       REG             1 MIN     home         ATL    
3     277 2019_0… 2019090804  MIN       ATL       REG             1 ATL     away         MIN    
4     298 2019_0… 2019090804  MIN       ATL       REG             1 ATL     away         MIN    
5     392 2019_0… 2019090804  MIN       ATL       REG             1 MIN     home         ATL    
6     472 2019_0… 2019090804  MIN       ATL       REG             1 MIN     home         ATL    
7     525 2019_0… 2019090804  MIN       ATL       REG             1 ATL     away         MIN    
8     663 2019_0… 2019090804  MIN       ATL       REG             1 ATL     away         MIN    
9     713 2019_0… 2019090804  MIN       ATL       REG             1 ATL     away         MIN    
10     832 2019_0… 2019090804  MIN       ATL       REG             1 MIN     home         ATL    
# … with 13,124 more rows, and 330 more variables: side_of_field <chr>, yardline_100 <dbl>, game_date <chr>,
#   quarter_seconds_remaining <dbl>, half_seconds_remaining <dbl>, game_seconds_remaining <dbl>,
#   game_half <chr>, quarter_end <dbl>, drive <dbl>, sp <dbl>, qtr <dbl>, down <dbl>, goal_to_go <dbl>,
#   time <chr>, yrdln <chr>, ydstogo <dbl>, ydsnet <dbl>, desc <chr>, play_type <chr>, yards_gained <dbl>,
#   shotgun <dbl>, no_huddle <dbl>, qb_dropback <dbl>, qb_kneel <dbl>, qb_spike <dbl>, qb_scramble <dbl>,
#   pass_length <chr>, pass_location <chr>, air_yards <dbl>, yards_after_catch <dbl>, run_location <chr>,
#   run_gap <chr>, field_goal_result <chr>, kick_distance <dbl>, extra_point_result <chr>,
#   two_point_conv_result <chr>, home_timeouts_remaining <dbl>, away_timeouts_remaining <dbl>, timeout <dbl>,
#   timeout_team <chr>, td_team <chr>, posteam_timeouts_remaining <dbl>, defteam_timeouts_remaining <dbl>,
#   total_home_score <dbl>, total_away_score <dbl>, posteam_score <dbl>, defteam_score <dbl>,
#   score_differential <dbl>, posteam_score_post <dbl>, defteam_score_post <dbl>,
#   score_differential_post <dbl>, no_score_prob <dbl>, opp_fg_prob <dbl>, opp_safety_prob <dbl>,
#   opp_td_prob <dbl>, fg_prob <dbl>, safety_prob <dbl>, td_prob <dbl>, extra_point_prob <dbl>,
#   two_point_conversion_prob <dbl>, ep <dbl>, epa <dbl>, total_home_epa <dbl>, total_away_epa <dbl>,
#   total_home_rush_epa <dbl>, total_away_rush_epa <dbl>, total_home_pass_epa <dbl>,
#   total_away_pass_epa <dbl>, air_epa <dbl>, yac_epa <dbl>, comp_air_epa <dbl>, comp_yac_epa <dbl>,
#   total_home_comp_air_epa <dbl>, total_away_comp_air_epa <dbl>, total_home_comp_yac_epa <dbl>,
#   total_away_comp_yac_epa <dbl>, total_home_raw_air_epa <dbl>, total_away_raw_air_epa <dbl>,
#   total_home_raw_yac_epa <dbl>, total_away_raw_yac_epa <dbl>, wp <dbl>, def_wp <dbl>, home_wp <dbl>,
#   away_wp <dbl>, wpa <dbl>, home_wp_post <dbl>, away_wp_post <dbl>, vegas_wp <dbl>, vegas_home_wp <dbl>,
#   total_home_rush_wpa <dbl>, total_away_rush_wpa <dbl>, total_home_pass_wpa <dbl>,
#   total_away_pass_wpa <dbl>, air_wpa <dbl>, yac_wpa <dbl>, comp_air_wpa <dbl>, comp_yac_wpa <dbl>,
#   total_home_comp_air_wpa <dbl>, total_away_comp_air_wpa <dbl>, total_home_comp_yac_wpa <dbl>, …
> rbdata <- filter(posteam == 'TB')
Error in filter(posteam == "TB") : object 'posteam' not found
> rbdatatwenty <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))
> rbdatatwenty <- rbdatatwenty %>%
  + filter(posteam == 'TB', rush == 1)%>%
  +     group_by(rusher) %>%
  +     summarize(
    +         mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()
    +     ) %>%
  +     arrange(-mean_epa) %>%
  +     filter(plays > 20)
> rbdatatwenty
# A tibble: 3 x 5
rusher      mean_epa success_rate   ypc plays
<chr>          <dbl>        <dbl> <dbl> <int>
  1 R.Jones      -0.0248        0.407  4.78   221
2 L.Fournette  -0.0269        0.397  3.83   151
3 K.Vaughn     -0.264         0.344  4.06    32
> rbdatanine <-  readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))
> rbdatanine <- rbdatanine %>% filter(posteam == 'TB', rush == 1)%>%
  +     group_by(rusher) %>%
  +     summarize(
    +         mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()
    +     ) %>%
  +     arrange(-mean_epa) %>%
  +     filter(plays > 20)
> rbdatanine
# A tibble: 2 x 5
rusher   mean_epa success_rate   ypc plays
<chr>       <dbl>        <dbl> <dbl> <int>
  1 R.Jones    -0.149        0.339  4.09   177
2 P.Barber   -0.233        0.3    2.95   160
> rbdatanine <-  readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2018.rds'))
> rbdatanine <-  readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))
> rbdataeight <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2018.rds'))
> rbdataeight <- rbdataeight %>% filter(posteam == 'TB', rush == 1)%>%
  +     group_by(rusher) %>%
  +     summarize(
    +         mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()
    +     ) %>%
  +     arrange(-mean_epa) %>%
  +     filter(plays > 20)
> rbdataeight
# A tibble: 3 x 5
rusher    mean_epa success_rate   ypc plays
<chr>        <dbl>        <dbl> <dbl> <int>
  1 J.Rodgers   -0.148        0.303  3.21    33
2 P.Barber    -0.234        0.315  3.51   248
3 R.Jones     -0.315        0.261  1.91    23
> rodgers <- rbdataeight(rusher == 'J.Rodgers')
Error in rbdataeight(rusher == "J.Rodgers") : 
  could not find function "rbdataeight"
> rodgers <- rbdataeight[rusher == 'J.Rodgers']
Error in `[.tbl_df`(rbdataeight, rusher == "J.Rodgers") : 
  object 'rusher' not found
> rodgers <- rbdataeight[rusher == 'J.Rodgers']
Error in `[.tbl_df`(rbdataeight, rusher == "J.Rodgers") : 
  object 'rusher' not found
> rodgers <- filter(rbdataeight, rusher == 'J.Rodgers')
> rodgers
# A tibble: 1 x 5
rusher    mean_epa success_rate   ypc plays
<chr>        <dbl>        <dbl> <dbl> <int>
  1 J.Rodgers   -0.148        0.303  3.21    33
> rodgerseight <- filter(rbdataeight, rusher == 'J.Rodgers)
+ exit
+ exit()
+ /
+ \
+ 

> rodgerseight <- filter(rbdataeight, rusher == 'J.Rodgers')
> rodgerseight
# A tibble: 1 x 5
  rusher    mean_epa success_rate   ypc plays
  <chr>        <dbl>        <dbl> <dbl> <int>
1 J.Rodgers   -0.148        0.303  3.21    33
> rjonesnine <- filter(rbdatanine, rusher == 'R.Jones')
> rjonestwenty <- filter(rbdatatwenty, rusher == 'R.Jones')
> rjoneseight <- filter(rbdataeight, rusher == 'R.Jones')
> ltwenty <- filter(rbdatatwenty, rusher == 'L.Fournette')
> kvaughntwenty <- filter(rbdatatwenty, rusher == 'K. Vaughn')
> pbarbernine <- filter(rbdatanine, rusher == 'P.Barber')
> kvaughntwenty <- filter(rbdatatwenty, rusher == 'K.Vaughn')
> pbarbereight <- filter(rbdataeight, rusher == 'P.Barber')
> jonestable <- rbindlist(list(rjoneseight, rjonesnine, rjonestwenty), use.names = TRUE, fill = TRUE)
Error in rbindlist(list(rjoneseight, rjonesnine, rjonestwenty), use.names = TRUE,  : 
  could not find function "rbindlist"
> library(data.table)
data.table 1.13.6 using 1 threads (see ?getDTthreads).  Latest news: r-datatable.com
**********
This installation of data.table has not detected OpenMP support. It should still work but in single-threaded mode.
This is a Mac. Please read https://mac.r-project.org/openmp/. Please engage with Apple and ask them for support. Check r-datatable.com for updates, and our Mac instructions here: https://github.com/Rdatatable/data.table/wiki/Installation. After several years of many reports of installation problems on Mac, it's time to gingerly point out that there have been no similar problems on Windows or Linux.
                         **********
                           
                           Attaching package: ‘data.table’
                         
                         The following objects are masked from ‘package:dplyr’:
                           
                           between, first, last
                         
                         The following object is masked from ‘package:purrr’:
                           
                           transpose
                         
                         > jonestable <- rbindlist(list(rjoneseight, rjonesnine, rjonestwenty), use.names = TRUE, fill = TRUE)
                         > jonestable
                         rusher    mean_epa success_rate      ypc plays play_id        game_id old_game_id home_team away_team
                         1: R.Jones -0.31515237    0.2608696 1.913043    23      NA           <NA>        <NA>      <NA>      <NA>
                           2: R.Jones          NA           NA       NA    NA     576  2019_01_SF_TB  2019090811        TB        SF
                         season_type week posteam posteam_type defteam side_of_field yardline_100  game_date
                         1:        <NA>   NA    <NA>         <NA>    <NA>          <NA>           NA       <NA>
                           2:         REG    1      TB         home      SF            SF           46 2019-09-08
                         quarter_seconds_remaining half_seconds_remaining game_seconds_remaining game_half quarter_end drive sp
                         1:                        NA                     NA                     NA      <NA>          NA    NA NA
                         2:                       526                   1426                   3226     Half1           0     3  0
                         qtr down goal_to_go  time  yrdln ydstogo ydsnet
                         1:  NA   NA         NA  <NA>   <NA>      NA     NA
                         2:   1    2          0 08:46  SF 46      10     34
                         desc
                         1:                                                                                                                 <NA>
                           2:                                     (8:46) (Shotgun) 27-R.Jones up the middle to SF 43 for 3 yards (56-K.Alexander).
                         play_type yards_gained shotgun no_huddle qb_dropback qb_kneel qb_spike qb_scramble pass_length
                         1:      <NA>           NA      NA        NA          NA       NA       NA          NA        <NA>
                           2:       run            3       1         0           0        0        0           0        <NA>
                           pass_location air_yards yards_after_catch run_location run_gap field_goal_result kick_distance
                         1:          <NA>        NA                NA         <NA>    <NA>              <NA>            NA
                         2:          <NA>        NA                NA       middle    <NA>              <NA>            NA
                         extra_point_result two_point_conv_result home_timeouts_remaining away_timeouts_remaining timeout
                         1:               <NA>                  <NA>                      NA                      NA      NA
                         2:               <NA>                  <NA>                       3                       3       0
                         timeout_team td_team posteam_timeouts_remaining defteam_timeouts_remaining total_home_score
                         1:         <NA>    <NA>                         NA                         NA               NA
                         2:         <NA>    <NA>                          3                          3                0
                         total_away_score posteam_score defteam_score score_differential posteam_score_post defteam_score_post
                         1:               NA            NA            NA                 NA                 NA                 NA
                         2:                3             0             3                 -3                  0                  3
                         score_differential_post no_score_prob opp_fg_prob opp_safety_prob opp_td_prob   fg_prob  safety_prob
                         1:                      NA            NA          NA              NA          NA        NA           NA
                         2:                      -3   0.008804570  0.08963161    0.0012437826  0.15586904 0.2770226 0.0047040242
                         td_prob extra_point_prob two_point_conversion_prob         ep         epa total_home_epa
                         1:        NA               NA                        NA         NA          NA             NA
                         2: 0.4627244                0                         0  2.7170810 -0.66232528     -3.8530816
                         total_away_epa total_home_rush_epa total_away_rush_epa total_home_pass_epa total_away_pass_epa air_epa
                         1:             NA                  NA                  NA                  NA                  NA      NA
                         2:      3.8530816         -0.03123181          0.03123181           -4.514648            4.514648      NA
                         yac_epa comp_air_epa comp_yac_epa total_home_comp_air_epa total_away_comp_air_epa
                         1:      NA           NA           NA                      NA                      NA
                         2:      NA            0            0              -0.3539041               0.3539041
                         total_home_comp_yac_epa total_away_comp_yac_epa total_home_raw_air_epa total_away_raw_air_epa
                         1:                      NA                      NA                     NA                     NA
                         2:               -1.319730                1.319730            -0.91704352             0.91704352
                         total_home_raw_yac_epa total_away_raw_yac_epa        wp    def_wp   home_wp   away_wp          wpa
                         1:                     NA                     NA        NA        NA        NA        NA           NA
                         2:            -3.59760442             3.59760442 0.4725588 0.5274412 0.4725588 0.5274412 -0.017488927
                         home_wp_post away_wp_post  vegas_wp vegas_home_wp total_home_rush_wpa total_away_rush_wpa
                         1:           NA           NA        NA            NA                  NA                  NA
                         2:    0.4550698    0.5449302 0.4722185     0.4722185        0.0004483163       -0.0004483163
                         total_home_pass_wpa total_away_pass_wpa air_wpa yac_wpa comp_air_wpa comp_yac_wpa
                         1:                  NA                  NA      NA      NA           NA           NA
                         2:         -0.11864823          0.11864823      NA      NA            0            0
                         total_home_comp_air_wpa total_away_comp_air_wpa total_home_comp_yac_wpa total_away_comp_yac_wpa
                         1:                      NA                      NA                      NA                      NA
                         2:            -0.038757026             0.038757026            -0.003744841             0.003744841
                         total_home_raw_air_wpa total_away_raw_air_wpa total_home_raw_yac_wpa total_away_raw_yac_wpa punt_blocked
                         1:                     NA                     NA                     NA                     NA           NA
                         2:           -0.007920116            0.007920116            -0.11072811             0.11072811            0
                         first_down_rush first_down_pass first_down_penalty third_down_converted third_down_failed
                         1:              NA              NA                 NA                   NA                NA
                         2:               0               0                  0                    0                 0
                         fourth_down_converted fourth_down_failed incomplete_pass touchback interception punt_inside_twenty
                         1:                    NA                 NA              NA        NA           NA                 NA
                         2:                     0                  0               0         0            0                  0
                         punt_in_endzone punt_out_of_bounds punt_downed punt_fair_catch kickoff_inside_twenty kickoff_in_endzone
                         1:              NA                 NA          NA              NA                    NA                 NA
                         2:               0                  0           0               0                     0                  0
                         kickoff_out_of_bounds kickoff_downed kickoff_fair_catch fumble_forced fumble_not_forced
                         1:                    NA             NA                 NA            NA                NA
                         2:                     0              0                  0             0                 0
                         fumble_out_of_bounds solo_tackle safety penalty tackled_for_loss fumble_lost own_kickoff_recovery
                         1:                   NA          NA     NA      NA               NA          NA                   NA
                         2:                    0           1      0       0                0           0                    0
                         own_kickoff_recovery_td qb_hit rush_attempt pass_attempt sack touchdown pass_touchdown rush_touchdown
                         1:                      NA     NA           NA           NA   NA        NA             NA             NA
                         2:                       0      0            1            0    0         0              0              0
                         return_touchdown extra_point_attempt two_point_attempt field_goal_attempt kickoff_attempt punt_attempt
                         1:               NA                  NA                NA                 NA              NA           NA
                         2:                0                   0                 0                  0               0            0
                         fumble complete_pass assist_tackle lateral_reception lateral_rush lateral_return lateral_recovery
                         1:     NA            NA            NA                NA           NA             NA               NA
                         2:      0             0             0                 0            0              0                0
                         passer_player_id passer_player_name receiver_player_id receiver_player_name
                         1:             <NA>               <NA>               <NA>                 <NA>
                           2:             <NA>               <NA>               <NA>                 <NA>
                           rusher_player_id rusher_player_name lateral_receiver_player_id
                         1:                                 <NA>               <NA>                       <NA>
                           2: 32013030-2d30-3033-3438-313686f35185            R.Jones                       <NA>
                           lateral_receiver_player_name lateral_rusher_player_id lateral_rusher_player_name lateral_sack_player_id
                         1:                         <NA>                     <NA>                       <NA>                   <NA>
                           2:                         <NA>                     <NA>                       <NA>                   <NA>
                           lateral_sack_player_name interception_player_id interception_player_name lateral_interception_player_id
                         1:                     <NA>                   <NA>                     <NA>                           <NA>
                           2:                     <NA>                   <NA>                     <NA>                           <NA>
                           lateral_interception_player_name punt_returner_player_id punt_returner_player_name
                         1:                             <NA>                    <NA>                      <NA>
                           2:                             <NA>                    <NA>                      <NA>
                           lateral_punt_returner_player_id lateral_punt_returner_player_name kickoff_returner_player_name
                         1:                            <NA>                              <NA>                         <NA>
                           2:                            <NA>                              <NA>                         <NA>
                           kickoff_returner_player_id lateral_kickoff_returner_player_id lateral_kickoff_returner_player_name
                         1:                       <NA>                               <NA>                                 <NA>
                           2:                       <NA>                               <NA>                                 <NA>
                           punter_player_id punter_player_name kicker_player_name kicker_player_id own_kickoff_recovery_player_id
                         1:             <NA>               <NA>               <NA>             <NA>                           <NA>
                           2:             <NA>               <NA>               <NA>             <NA>                           <NA>
                           own_kickoff_recovery_player_name blocked_player_id blocked_player_name
                         1:                             <NA>              <NA>                <NA>
                           2:                             <NA>              <NA>                <NA>
                           tackle_for_loss_1_player_id tackle_for_loss_1_player_name tackle_for_loss_2_player_id
                         1:                                 <NA>                          <NA>                        <NA>
                           2:                                 <NA>                          <NA>                        <NA>
                           tackle_for_loss_2_player_name qb_hit_1_player_id qb_hit_1_player_name qb_hit_2_player_id
                         1:                          <NA>               <NA>                 <NA>               <NA>
                           2:                          <NA>               <NA>                 <NA>               <NA>
                           qb_hit_2_player_name forced_fumble_player_1_team     forced_fumble_player_1_player_id
                         1:                 <NA>                        <NA>                                 <NA>
                           2:                 <NA>                        <NA>                                 <NA>
                           forced_fumble_player_1_player_name forced_fumble_player_2_team forced_fumble_player_2_player_id
                         1:                               <NA>                        <NA>                             <NA>
                           2:                               <NA>                        <NA>                             <NA>
                           forced_fumble_player_2_player_name solo_tackle_1_team solo_tackle_2_team
                         1:                               <NA>               <NA>               <NA>
                           2:                               <NA>                 SF               <NA>
                           solo_tackle_1_player_id solo_tackle_2_player_id solo_tackle_1_player_name
                         1:                                 <NA>                    <NA>                      <NA>
                           2: 32013030-2d30-3033-3231-3039fd009952                    <NA>               K.Alexander
                         solo_tackle_2_player_name            assist_tackle_1_player_id assist_tackle_1_player_name
                         1:                      <NA>                                 <NA>                        <NA>
                           2:                      <NA>                                 <NA>                        <NA>
                           assist_tackle_1_team            assist_tackle_2_player_id assist_tackle_2_player_name
                         1:                 <NA>                                 <NA>                        <NA>
                           2:                 <NA>                                 <NA>                        <NA>
                           assist_tackle_2_team assist_tackle_3_player_id assist_tackle_3_player_name assist_tackle_3_team
                         1:                 <NA>                      <NA>                        <NA>                 <NA>
                           2:                 <NA>                      <NA>                        <NA>                 <NA>
                           assist_tackle_4_player_id assist_tackle_4_player_name assist_tackle_4_team pass_defense_1_player_id
                         1:                      <NA>                        <NA>                 <NA>                     <NA>
                           2:                      <NA>                        <NA>                 <NA>                     <NA>
                           pass_defense_1_player_name pass_defense_2_player_id pass_defense_2_player_name fumbled_1_team
                         1:                       <NA>                     <NA>                       <NA>           <NA>
                           2:                       <NA>                     <NA>                       <NA>           <NA>
                           fumbled_1_player_id fumbled_1_player_name fumbled_2_player_id fumbled_2_player_name
                         1:                                 <NA>                  <NA>                <NA>                  <NA>
                           2:                                 <NA>                  <NA>                <NA>                  <NA>
                           fumbled_2_team fumble_recovery_1_team fumble_recovery_1_yards fumble_recovery_1_player_id
                         1:           <NA>                   <NA>                      NA                        <NA>
                           2:           <NA>                   <NA>                      NA                        <NA>
                           fumble_recovery_1_player_name fumble_recovery_2_team fumble_recovery_2_yards fumble_recovery_2_player_id
                         1:                          <NA>                   <NA>                      NA                        <NA>
                           2:                          <NA>                   <NA>                      NA                        <NA>
                           fumble_recovery_2_player_name return_team return_yards penalty_team penalty_player_id
                         1:                          <NA>        <NA>           NA         <NA>              <NA>
                           2:                          <NA>        <NA>            0         <NA>              <NA>
                           penalty_player_name penalty_yards replay_or_challenge replay_or_challenge_result penalty_type
                         1:                <NA>            NA                  NA                       <NA>         <NA>
                           2:                <NA>            NA                   0                       <NA>         <NA>
                           defensive_two_point_attempt defensive_two_point_conv defensive_extra_point_attempt
                         1:                          NA                       NA                            NA
                         2:                           0                        0                             0
                         defensive_extra_point_conv season cp cpoe series series_success     series_result order_sequence
                         1:                         NA     NA NA   NA     NA             NA              <NA>             NA
                         2:                          0   2019 NA   NA      7              0              Punt            576
                         start_time time_of_day               stadium                                             weather
                         1:       <NA>        <NA>                  <NA>                                                <NA>
                           2:   16:25:00    20:45:13 Raymond James Stadium   Cloudy Temp: 93° F, Humidity: 52%, Wind: SW 9 mph
                         nfl_api_id play_clock play_deleted play_type_nfl special_teams_play
                         1:                                 <NA>       <NA>           NA          <NA>                 NA
                         2: 10160000-0579-12d8-bb4a-d7a447161b3e          5            0          RUSH                  0
                         st_play_type end_clock_time end_yard_line fixed_drive fixed_drive_result drive_real_start_time
                         1:         <NA>           <NA>          <NA>          NA               <NA>                  <NA>
                           2:         <NA>           <NA>         SF 43           3               Punt                  <NA>
                           drive_play_count drive_time_of_possession drive_first_downs drive_inside20 drive_ended_with_score
                         1:               NA                     <NA>                NA             NA                     NA
                         2:                8                     3:36                 3              0                      0
                         drive_quarter_start drive_quarter_end drive_yards_penalized drive_start_transition drive_end_transition
                         1:                  NA                NA                    NA                   <NA>                 <NA>
                           2:                   1                 1                    16                KICKOFF                 PUNT
                         drive_game_clock_start drive_game_clock_end drive_start_yard_line drive_end_yard_line
                         1:                   <NA>                 <NA>                  <NA>                <NA>
                           2:                  11:08                07:32                 TB 18               SF 48
                         drive_play_id_started drive_play_id_ended away_score home_score location result total spread_line
                         1:                    NA                  NA         NA         NA     <NA>     NA    NA          NA
                         2:                   328                 642         31         17     Home    -14    48           1
                         total_line div_game     roof surface temp wind   home_coach    away_coach stadium_id
                         1:         NA       NA     <NA>    <NA>   NA   NA         <NA>          <NA>       <NA>
                           2:         51        0 outdoors   grass   93    9 Bruce Arians Kyle Shanahan      TAM00
                         game_stadium success passer passer_jersey_number rusher_jersey_number receiver
                         1:                  <NA>      NA   <NA>                   NA                   NA     <NA>
                           2: Raymond James Stadium       0   <NA>                   NA                   27     <NA>
                           receiver_jersey_number pass rush first_down aborted_play special play passer_id
                         1:                     NA   NA   NA         NA           NA      NA   NA      <NA>
                           2:                     NA    0    1          0            0       0    1      <NA>
                           rusher_id receiver_id    name jersey_number
                         1:                                 <NA>        <NA>    <NA>            NA
                         2: 32013030-2d30-3033-3438-313686f35185        <NA> R.Jones            27
                         id      qb_epa xyac_epa xyac_mean_yardage xyac_median_yardage
                         1:                                 <NA>          NA       NA                NA                  NA
                         2: 32013030-2d30-3033-3438-313686f35185 -0.66232528       NA                NA                  NA
                         xyac_success xyac_fd
                         1:           NA      NA
                         2:           NA      NA
                         [ reached getOption("max.print") -- omitted 9 rows ]
                         > jonestable <- jonestable[ ,c('rusher', 'mean_epa', 'success_rate')]
                         > jonestable
                         rusher    mean_epa success_rate
                         1: R.Jones -0.31515237    0.2608696
                         2: R.Jones          NA           NA
                         3: R.Jones          NA           NA
                         4: R.Jones          NA           NA
                         5: R.Jones          NA           NA
                         ---                                 
                           175: R.Jones          NA           NA
                         176: R.Jones          NA           NA
                         177: R.Jones          NA           NA
                         178: R.Jones          NA           NA
                         179: R.Jones -0.02483251    0.4072398
                         > jonestable <- rbindlist(list(rjoneseight, rjonesnine, rjonestwenty), use.names = TRUE, fill = TRUE)
                         > jonestable %>% select(rusher, mean_epa, success_rate)
                         rusher    mean_epa success_rate
                         1: R.Jones -0.31515237    0.2608696
                         2: R.Jones          NA           NA
                         3: R.Jones          NA           NA
                         4: R.Jones          NA           NA
                         5: R.Jones          NA           NA
                         ---                                 
                           175: R.Jones          NA           NA
                         176: R.Jones          NA           NA
                         177: R.Jones          NA           NA
                         178: R.Jones          NA           NA
                         179: R.Jones -0.02483251    0.4072398
                         > jonestable 
                         rusher    mean_epa success_rate      ypc plays play_id        game_id old_game_id home_team away_team
                         1: R.Jones -0.31515237    0.2608696 1.913043    23      NA           <NA>        <NA>      <NA>      <NA>
                           2: R.Jones          NA           NA       NA    NA     576  2019_01_SF_TB  2019090811        TB        SF
                         season_type week posteam posteam_type defteam side_of_field yardline_100  game_date
                         1:        <NA>   NA    <NA>         <NA>    <NA>          <NA>           NA       <NA>
                           2:         REG    1      TB         home      SF            SF           46 2019-09-08
                         quarter_seconds_remaining half_seconds_remaining game_seconds_remaining game_half quarter_end drive sp
                         1:                        NA                     NA                     NA      <NA>          NA    NA NA
                         2:                       526                   1426                   3226     Half1           0     3  0
                         qtr down goal_to_go  time  yrdln ydstogo ydsnet
                         1:  NA   NA         NA  <NA>   <NA>      NA     NA
                         2:   1    2          0 08:46  SF 46      10     34
                         desc
                         1:                                                                                                                 <NA>
                           2:                                     (8:46) (Shotgun) 27-R.Jones up the middle to SF 43 for 3 yards (56-K.Alexander).
                         play_type yards_gained shotgun no_huddle qb_dropback qb_kneel qb_spike qb_scramble pass_length
                         1:      <NA>           NA      NA        NA          NA       NA       NA          NA        <NA>
                           2:       run            3       1         0           0        0        0           0        <NA>
                           pass_location air_yards yards_after_catch run_location run_gap field_goal_result kick_distance
                         1:          <NA>        NA                NA         <NA>    <NA>              <NA>            NA
                         2:          <NA>        NA                NA       middle    <NA>              <NA>            NA
                         extra_point_result two_point_conv_result home_timeouts_remaining away_timeouts_remaining timeout
                         1:               <NA>                  <NA>                      NA                      NA      NA
                         2:               <NA>                  <NA>                       3                       3       0
                         timeout_team td_team posteam_timeouts_remaining defteam_timeouts_remaining total_home_score
                         1:         <NA>    <NA>                         NA                         NA               NA
                         2:         <NA>    <NA>                          3                          3                0
                         total_away_score posteam_score defteam_score score_differential posteam_score_post defteam_score_post
                         1:               NA            NA            NA                 NA                 NA                 NA
                         2:                3             0             3                 -3                  0                  3
                         score_differential_post no_score_prob opp_fg_prob opp_safety_prob opp_td_prob   fg_prob  safety_prob
                         1:                      NA            NA          NA              NA          NA        NA           NA
                         2:                      -3   0.008804570  0.08963161    0.0012437826  0.15586904 0.2770226 0.0047040242
                         td_prob extra_point_prob two_point_conversion_prob         ep         epa total_home_epa
                         1:        NA               NA                        NA         NA          NA             NA
                         2: 0.4627244                0                         0  2.7170810 -0.66232528     -3.8530816
                         total_away_epa total_home_rush_epa total_away_rush_epa total_home_pass_epa total_away_pass_epa air_epa
                         1:             NA                  NA                  NA                  NA                  NA      NA
                         2:      3.8530816         -0.03123181          0.03123181           -4.514648            4.514648      NA
                         yac_epa comp_air_epa comp_yac_epa total_home_comp_air_epa total_away_comp_air_epa
                         1:      NA           NA           NA                      NA                      NA
                         2:      NA            0            0              -0.3539041               0.3539041
                         total_home_comp_yac_epa total_away_comp_yac_epa total_home_raw_air_epa total_away_raw_air_epa
                         1:                      NA                      NA                     NA                     NA
                         2:               -1.319730                1.319730            -0.91704352             0.91704352
                         total_home_raw_yac_epa total_away_raw_yac_epa        wp    def_wp   home_wp   away_wp          wpa
                         1:                     NA                     NA        NA        NA        NA        NA           NA
                         2:            -3.59760442             3.59760442 0.4725588 0.5274412 0.4725588 0.5274412 -0.017488927
                         home_wp_post away_wp_post  vegas_wp vegas_home_wp total_home_rush_wpa total_away_rush_wpa
                         1:           NA           NA        NA            NA                  NA                  NA
                         2:    0.4550698    0.5449302 0.4722185     0.4722185        0.0004483163       -0.0004483163
                         total_home_pass_wpa total_away_pass_wpa air_wpa yac_wpa comp_air_wpa comp_yac_wpa
                         1:                  NA                  NA      NA      NA           NA           NA
                         2:         -0.11864823          0.11864823      NA      NA            0            0
                         total_home_comp_air_wpa total_away_comp_air_wpa total_home_comp_yac_wpa total_away_comp_yac_wpa
                         1:                      NA                      NA                      NA                      NA
                         2:            -0.038757026             0.038757026            -0.003744841             0.003744841
                         total_home_raw_air_wpa total_away_raw_air_wpa total_home_raw_yac_wpa total_away_raw_yac_wpa punt_blocked
                         1:                     NA                     NA                     NA                     NA           NA
                         2:           -0.007920116            0.007920116            -0.11072811             0.11072811            0
                         first_down_rush first_down_pass first_down_penalty third_down_converted third_down_failed
                         1:              NA              NA                 NA                   NA                NA
                         2:               0               0                  0                    0                 0
                         fourth_down_converted fourth_down_failed incomplete_pass touchback interception punt_inside_twenty
                         1:                    NA                 NA              NA        NA           NA                 NA
                         2:                     0                  0               0         0            0                  0
                         punt_in_endzone punt_out_of_bounds punt_downed punt_fair_catch kickoff_inside_twenty kickoff_in_endzone
                         1:              NA                 NA          NA              NA                    NA                 NA
                         2:               0                  0           0               0                     0                  0
                         kickoff_out_of_bounds kickoff_downed kickoff_fair_catch fumble_forced fumble_not_forced
                         1:                    NA             NA                 NA            NA                NA
                         2:                     0              0                  0             0                 0
                         fumble_out_of_bounds solo_tackle safety penalty tackled_for_loss fumble_lost own_kickoff_recovery
                         1:                   NA          NA     NA      NA               NA          NA                   NA
                         2:                    0           1      0       0                0           0                    0
                         own_kickoff_recovery_td qb_hit rush_attempt pass_attempt sack touchdown pass_touchdown rush_touchdown
                         1:                      NA     NA           NA           NA   NA        NA             NA             NA
                         2:                       0      0            1            0    0         0              0              0
                         return_touchdown extra_point_attempt two_point_attempt field_goal_attempt kickoff_attempt punt_attempt
                         1:               NA                  NA                NA                 NA              NA           NA
                         2:                0                   0                 0                  0               0            0
                         fumble complete_pass assist_tackle lateral_reception lateral_rush lateral_return lateral_recovery
                         1:     NA            NA            NA                NA           NA             NA               NA
                         2:      0             0             0                 0            0              0                0
                         passer_player_id passer_player_name receiver_player_id receiver_player_name
                         1:             <NA>               <NA>               <NA>                 <NA>
                           2:             <NA>               <NA>               <NA>                 <NA>
                           rusher_player_id rusher_player_name lateral_receiver_player_id
                         1:                                 <NA>               <NA>                       <NA>
                           2: 32013030-2d30-3033-3438-313686f35185            R.Jones                       <NA>
                           lateral_receiver_player_name lateral_rusher_player_id lateral_rusher_player_name lateral_sack_player_id
                         1:                         <NA>                     <NA>                       <NA>                   <NA>
                           2:                         <NA>                     <NA>                       <NA>                   <NA>
                           lateral_sack_player_name interception_player_id interception_player_name lateral_interception_player_id
                         1:                     <NA>                   <NA>                     <NA>                           <NA>
                           2:                     <NA>                   <NA>                     <NA>                           <NA>
                           lateral_interception_player_name punt_returner_player_id punt_returner_player_name
                         1:                             <NA>                    <NA>                      <NA>
                           2:                             <NA>                    <NA>                      <NA>
                           lateral_punt_returner_player_id lateral_punt_returner_player_name kickoff_returner_player_name
                         1:                            <NA>                              <NA>                         <NA>
                           2:                            <NA>                              <NA>                         <NA>
                           kickoff_returner_player_id lateral_kickoff_returner_player_id lateral_kickoff_returner_player_name
                         1:                       <NA>                               <NA>                                 <NA>
                           2:                       <NA>                               <NA>                                 <NA>
                           punter_player_id punter_player_name kicker_player_name kicker_player_id own_kickoff_recovery_player_id
                         1:             <NA>               <NA>               <NA>             <NA>                           <NA>
                           2:             <NA>               <NA>               <NA>             <NA>                           <NA>
                           own_kickoff_recovery_player_name blocked_player_id blocked_player_name
                         1:                             <NA>              <NA>                <NA>
                           2:                             <NA>              <NA>                <NA>
                           tackle_for_loss_1_player_id tackle_for_loss_1_player_name tackle_for_loss_2_player_id
                         1:                                 <NA>                          <NA>                        <NA>
                           2:                                 <NA>                          <NA>                        <NA>
                           tackle_for_loss_2_player_name qb_hit_1_player_id qb_hit_1_player_name qb_hit_2_player_id
                         1:                          <NA>               <NA>                 <NA>               <NA>
                           2:                          <NA>               <NA>                 <NA>               <NA>
                           qb_hit_2_player_name forced_fumble_player_1_team     forced_fumble_player_1_player_id
                         1:                 <NA>                        <NA>                                 <NA>
                           2:                 <NA>                        <NA>                                 <NA>
                           forced_fumble_player_1_player_name forced_fumble_player_2_team forced_fumble_player_2_player_id
                         1:                               <NA>                        <NA>                             <NA>
                           2:                               <NA>                        <NA>                             <NA>
                           forced_fumble_player_2_player_name solo_tackle_1_team solo_tackle_2_team
                         1:                               <NA>               <NA>               <NA>
                           2:                               <NA>                 SF               <NA>
                           solo_tackle_1_player_id solo_tackle_2_player_id solo_tackle_1_player_name
                         1:                                 <NA>                    <NA>                      <NA>
                           2: 32013030-2d30-3033-3231-3039fd009952                    <NA>               K.Alexander
                         solo_tackle_2_player_name            assist_tackle_1_player_id assist_tackle_1_player_name
                         1:                      <NA>                                 <NA>                        <NA>
                           2:                      <NA>                                 <NA>                        <NA>
                           assist_tackle_1_team            assist_tackle_2_player_id assist_tackle_2_player_name
                         1:                 <NA>                                 <NA>                        <NA>
                           2:                 <NA>                                 <NA>                        <NA>
                           assist_tackle_2_team assist_tackle_3_player_id assist_tackle_3_player_name assist_tackle_3_team
                         1:                 <NA>                      <NA>                        <NA>                 <NA>
                           2:                 <NA>                      <NA>                        <NA>                 <NA>
                           assist_tackle_4_player_id assist_tackle_4_player_name assist_tackle_4_team pass_defense_1_player_id
                         1:                      <NA>                        <NA>                 <NA>                     <NA>
                           2:                      <NA>                        <NA>                 <NA>                     <NA>
                           pass_defense_1_player_name pass_defense_2_player_id pass_defense_2_player_name fumbled_1_team
                         1:                       <NA>                     <NA>                       <NA>           <NA>
                           2:                       <NA>                     <NA>                       <NA>           <NA>
                           fumbled_1_player_id fumbled_1_player_name fumbled_2_player_id fumbled_2_player_name
                         1:                                 <NA>                  <NA>                <NA>                  <NA>
                           2:                                 <NA>                  <NA>                <NA>                  <NA>
                           fumbled_2_team fumble_recovery_1_team fumble_recovery_1_yards fumble_recovery_1_player_id
                         1:           <NA>                   <NA>                      NA                        <NA>
                           2:           <NA>                   <NA>                      NA                        <NA>
                           fumble_recovery_1_player_name fumble_recovery_2_team fumble_recovery_2_yards fumble_recovery_2_player_id
                         1:                          <NA>                   <NA>                      NA                        <NA>
                           2:                          <NA>                   <NA>                      NA                        <NA>
                           fumble_recovery_2_player_name return_team return_yards penalty_team penalty_player_id
                         1:                          <NA>        <NA>           NA         <NA>              <NA>
                           2:                          <NA>        <NA>            0         <NA>              <NA>
                           penalty_player_name penalty_yards replay_or_challenge replay_or_challenge_result penalty_type
                         1:                <NA>            NA                  NA                       <NA>         <NA>
                           2:                <NA>            NA                   0                       <NA>         <NA>
                           defensive_two_point_attempt defensive_two_point_conv defensive_extra_point_attempt
                         1:                          NA                       NA                            NA
                         2:                           0                        0                             0
                         defensive_extra_point_conv season cp cpoe series series_success     series_result order_sequence
                         1:                         NA     NA NA   NA     NA             NA              <NA>             NA
                         2:                          0   2019 NA   NA      7              0              Punt            576
                         start_time time_of_day               stadium                                             weather
                         1:       <NA>        <NA>                  <NA>                                                <NA>
                           2:   16:25:00    20:45:13 Raymond James Stadium   Cloudy Temp: 93° F, Humidity: 52%, Wind: SW 9 mph
                         nfl_api_id play_clock play_deleted play_type_nfl special_teams_play
                         1:                                 <NA>       <NA>           NA          <NA>                 NA
                         2: 10160000-0579-12d8-bb4a-d7a447161b3e          5            0          RUSH                  0
                         st_play_type end_clock_time end_yard_line fixed_drive fixed_drive_result drive_real_start_time
                         1:         <NA>           <NA>          <NA>          NA               <NA>                  <NA>
                           2:         <NA>           <NA>         SF 43           3               Punt                  <NA>
                           drive_play_count drive_time_of_possession drive_first_downs drive_inside20 drive_ended_with_score
                         1:               NA                     <NA>                NA             NA                     NA
                         2:                8                     3:36                 3              0                      0
                         drive_quarter_start drive_quarter_end drive_yards_penalized drive_start_transition drive_end_transition
                         1:                  NA                NA                    NA                   <NA>                 <NA>
                           2:                   1                 1                    16                KICKOFF                 PUNT
                         drive_game_clock_start drive_game_clock_end drive_start_yard_line drive_end_yard_line
                         1:                   <NA>                 <NA>                  <NA>                <NA>
                           2:                  11:08                07:32                 TB 18               SF 48
                         drive_play_id_started drive_play_id_ended away_score home_score location result total spread_line
                         1:                    NA                  NA         NA         NA     <NA>     NA    NA          NA
                         2:                   328                 642         31         17     Home    -14    48           1
                         total_line div_game     roof surface temp wind   home_coach    away_coach stadium_id
                         1:         NA       NA     <NA>    <NA>   NA   NA         <NA>          <NA>       <NA>
                           2:         51        0 outdoors   grass   93    9 Bruce Arians Kyle Shanahan      TAM00
                         game_stadium success passer passer_jersey_number rusher_jersey_number receiver
                         1:                  <NA>      NA   <NA>                   NA                   NA     <NA>
                           2: Raymond James Stadium       0   <NA>                   NA                   27     <NA>
                           receiver_jersey_number pass rush first_down aborted_play special play passer_id
                         1:                     NA   NA   NA         NA           NA      NA   NA      <NA>
                           2:                     NA    0    1          0            0       0    1      <NA>
                           rusher_id receiver_id    name jersey_number
                         1:                                 <NA>        <NA>    <NA>            NA
                         2: 32013030-2d30-3033-3438-313686f35185        <NA> R.Jones            27
                         id      qb_epa xyac_epa xyac_mean_yardage xyac_median_yardage
                         1:                                 <NA>          NA       NA                NA                  NA
                         2: 32013030-2d30-3033-3438-313686f35185 -0.66232528       NA                NA                  NA
                         xyac_success xyac_fd
                         1:           NA      NA
                         2:           NA      NA
                         [ reached getOption("max.print") -- omitted 9 rows ]
                         > rjonesnine
                         # A tibble: 177 x 340
                         play_id game_id old_game_id home_team away_team season_type  week posteam posteam_type defteam
                         <dbl> <chr>   <chr>       <chr>     <chr>     <chr>       <int> <chr>   <chr>        <chr>  
                           1     576 2019_0… 2019090811  TB        SF        REG             1 TB      home         SF     
                         2     776 2019_0… 2019090811  TB        SF        REG             1 TB      home         SF     
                         3     797 2019_0… 2019090811  TB        SF        REG             1 TB      home         SF     
                         4    1140 2019_0… 2019090811  TB        SF        REG             1 TB      home         SF     
                         5    2661 2019_0… 2019090811  TB        SF        REG             1 TB      home         SF     
                         6    2950 2019_0… 2019090811  TB        SF        REG             1 TB      home         SF     
                         7    2971 2019_0… 2019090811  TB        SF        REG             1 TB      home         SF     
                         8    2992 2019_0… 2019090811  TB        SF        REG             1 TB      home         SF     
                         9    3013 2019_0… 2019090811  TB        SF        REG             1 TB      home         SF     
                         10    3459 2019_0… 2019090811  TB        SF        REG             1 TB      home         SF     
                         # … with 167 more rows, and 330 more variables: side_of_field <chr>, yardline_100 <dbl>, game_date <chr>,
                         #   quarter_seconds_remaining <dbl>, half_seconds_remaining <dbl>, game_seconds_remaining <dbl>,
                         #   game_half <chr>, quarter_end <dbl>, drive <dbl>, sp <dbl>, qtr <dbl>, down <dbl>, goal_to_go <dbl>,
                         #   time <chr>, yrdln <chr>, ydstogo <dbl>, ydsnet <dbl>, desc <chr>, play_type <chr>, yards_gained <dbl>,
                         #   shotgun <dbl>, no_huddle <dbl>, qb_dropback <dbl>, qb_kneel <dbl>, qb_spike <dbl>, qb_scramble <dbl>,
                         #   pass_length <chr>, pass_location <chr>, air_yards <dbl>, yards_after_catch <dbl>, run_location <chr>,
                         #   run_gap <chr>, field_goal_result <chr>, kick_distance <dbl>, extra_point_result <chr>,
                         #   two_point_conv_result <chr>, home_timeouts_remaining <dbl>, away_timeouts_remaining <dbl>, timeout <dbl>,
                         #   timeout_team <chr>, td_team <chr>, posteam_timeouts_remaining <dbl>, defteam_timeouts_remaining <dbl>,
                         #   total_home_score <dbl>, total_away_score <dbl>, posteam_score <dbl>, defteam_score <dbl>,
                         #   score_differential <dbl>, posteam_score_post <dbl>, defteam_score_post <dbl>,
                         #   score_differential_post <dbl>, no_score_prob <dbl>, opp_fg_prob <dbl>, opp_safety_prob <dbl>,
                         #   opp_td_prob <dbl>, fg_prob <dbl>, safety_prob <dbl>, td_prob <dbl>, extra_point_prob <dbl>,
                         #   two_point_conversion_prob <dbl>, ep <dbl>, epa <dbl>, total_home_epa <dbl>, total_away_epa <dbl>,
                         #   total_home_rush_epa <dbl>, total_away_rush_epa <dbl>, total_home_pass_epa <dbl>,
                         #   total_away_pass_epa <dbl>, air_epa <dbl>, yac_epa <dbl>, comp_air_epa <dbl>, comp_yac_epa <dbl>,
                         #   total_home_comp_air_epa <dbl>, total_away_comp_air_epa <dbl>, total_home_comp_yac_epa <dbl>,
                         #   total_away_comp_yac_epa <dbl>, total_home_raw_air_epa <dbl>, total_away_raw_air_epa <dbl>,
                         #   total_home_raw_yac_epa <dbl>, total_away_raw_yac_epa <dbl>, wp <dbl>, def_wp <dbl>, home_wp <dbl>,
                         #   away_wp <dbl>, wpa <dbl>, home_wp_post <dbl>, away_wp_post <dbl>, vegas_wp <dbl>, vegas_home_wp <dbl>,
                         #   total_home_rush_wpa <dbl>, total_away_rush_wpa <dbl>, total_home_pass_wpa <dbl>,
                         #   total_away_pass_wpa <dbl>, air_wpa <dbl>, yac_wpa <dbl>, comp_air_wpa <dbl>, comp_yac_wpa <dbl>,
                         #   total_home_comp_air_wpa <dbl>, total_away_comp_air_wpa <dbl>, total_home_comp_yac_wpa <dbl>, …
                         > rjonesnine <- select(rjonesnine, rusher, mean_epa, success_rate)
                         Error: Can't subset columns that don't exist.
                         x Column `mean_epa` doesn't exist.
Run `rlang::last_error()` to see where the error occurred.
> rjonesnine
# A tibble: 177 x 340
   play_id game_id old_game_id home_team away_team season_type  week posteam posteam_type defteam
     <dbl> <chr>   <chr>       <chr>     <chr>     <chr>       <int> <chr>   <chr>        <chr>  
 1     576 2019_0… 2019090811  TB        SF        REG             1 TB      home         SF     
 2     776 2019_0… 2019090811  TB        SF        REG             1 TB      home         SF     
 3     797 2019_0… 2019090811  TB        SF        REG             1 TB      home         SF     
 4    1140 2019_0… 2019090811  TB        SF        REG             1 TB      home         SF     
 5    2661 2019_0… 2019090811  TB        SF        REG             1 TB      home         SF     
 6    2950 2019_0… 2019090811  TB        SF        REG             1 TB      home         SF     
 7    2971 2019_0… 2019090811  TB        SF        REG             1 TB      home         SF     
 8    2992 2019_0… 2019090811  TB        SF        REG             1 TB      home         SF     
 9    3013 2019_0… 2019090811  TB        SF        REG             1 TB      home         SF     
10    3459 2019_0… 2019090811  TB        SF        REG             1 TB      home         SF     
# … with 167 more rows, and 330 more variables: side_of_field <chr>, yardline_100 <dbl>, game_date <chr>,
#   quarter_seconds_remaining <dbl>, half_seconds_remaining <dbl>, game_seconds_remaining <dbl>,
#   game_half <chr>, quarter_end <dbl>, drive <dbl>, sp <dbl>, qtr <dbl>, down <dbl>, goal_to_go <dbl>,
#   time <chr>, yrdln <chr>, ydstogo <dbl>, ydsnet <dbl>, desc <chr>, play_type <chr>, yards_gained <dbl>,
#   shotgun <dbl>, no_huddle <dbl>, qb_dropback <dbl>, qb_kneel <dbl>, qb_spike <dbl>, qb_scramble <dbl>,
#   pass_length <chr>, pass_location <chr>, air_yards <dbl>, yards_after_catch <dbl>, run_location <chr>,
#   run_gap <chr>, field_goal_result <chr>, kick_distance <dbl>, extra_point_result <chr>,
#   two_point_conv_result <chr>, home_timeouts_remaining <dbl>, away_timeouts_remaining <dbl>, timeout <dbl>,
#   timeout_team <chr>, td_team <chr>, posteam_timeouts_remaining <dbl>, defteam_timeouts_remaining <dbl>,
#   total_home_score <dbl>, total_away_score <dbl>, posteam_score <dbl>, defteam_score <dbl>,
#   score_differential <dbl>, posteam_score_post <dbl>, defteam_score_post <dbl>,
#   score_differential_post <dbl>, no_score_prob <dbl>, opp_fg_prob <dbl>, opp_safety_prob <dbl>,
#   opp_td_prob <dbl>, fg_prob <dbl>, safety_prob <dbl>, td_prob <dbl>, extra_point_prob <dbl>,
#   two_point_conversion_prob <dbl>, ep <dbl>, epa <dbl>, total_home_epa <dbl>, total_away_epa <dbl>,
#   total_home_rush_epa <dbl>, total_away_rush_epa <dbl>, total_home_pass_epa <dbl>,
#   total_away_pass_epa <dbl>, air_epa <dbl>, yac_epa <dbl>, comp_air_epa <dbl>, comp_yac_epa <dbl>,
#   total_home_comp_air_epa <dbl>, total_away_comp_air_epa <dbl>, total_home_comp_yac_epa <dbl>,
#   total_away_comp_yac_epa <dbl>, total_home_raw_air_epa <dbl>, total_away_raw_air_epa <dbl>,
#   total_home_raw_yac_epa <dbl>, total_away_raw_yac_epa <dbl>, wp <dbl>, def_wp <dbl>, home_wp <dbl>,
#   away_wp <dbl>, wpa <dbl>, home_wp_post <dbl>, away_wp_post <dbl>, vegas_wp <dbl>, vegas_home_wp <dbl>,
#   total_home_rush_wpa <dbl>, total_away_rush_wpa <dbl>, total_home_pass_wpa <dbl>,
#   total_away_pass_wpa <dbl>, air_wpa <dbl>, yac_wpa <dbl>, comp_air_wpa <dbl>, comp_yac_wpa <dbl>,
#   total_home_comp_air_wpa <dbl>, total_away_comp_air_wpa <dbl>, total_home_comp_yac_wpa <dbl>, …
> rodgers
# A tibble: 1 x 5
  rusher    mean_epa success_rate   ypc plays
  <chr>        <dbl>        <dbl> <dbl> <int>
1 J.Rodgers   -0.148        0.303  3.21    33
> rbdataeight
# A tibble: 3 x 5
  rusher    mean_epa success_rate   ypc plays
  <chr>        <dbl>        <dbl> <dbl> <int>
1 J.Rodgers   -0.148        0.303  3.21    33
2 P.Barber    -0.234        0.315  3.51   248
3 R.Jones     -0.315        0.261  1.91    23
> rbdatanine
# A tibble: 48,034 x 340
   play_id game_id old_game_id home_team away_team season_type  week posteam posteam_type defteam
     <dbl> <chr>   <chr>       <chr>     <chr>     <chr>       <int> <chr>   <chr>        <chr>  
 1       1 2019_0… 2019090804  MIN       ATL       REG             1 NA      NA           NA     
 2      36 2019_0… 2019090804  MIN       ATL       REG             1 ATL     away         MIN    
 3      51 2019_0… 2019090804  MIN       ATL       REG             1 ATL     away         MIN    
 4      79 2019_0… 2019090804  MIN       ATL       REG             1 ATL     away         MIN    
 5     100 2019_0… 2019090804  MIN       ATL       REG             1 ATL     away         MIN    
 6     121 2019_0… 2019090804  MIN       ATL       REG             1 ATL     away         MIN    
 7     148 2019_0… 2019090804  MIN       ATL       REG             1 MIN     home         ATL    
 8     185 2019_0… 2019090804  MIN       ATL       REG             1 MIN     home         ATL    
 9     214 2019_0… 2019090804  MIN       ATL       REG             1 MIN     home         ATL    
10     239 2019_0… 2019090804  MIN       ATL       REG             1 MIN     home         ATL    
# … with 48,024 more rows, and 330 more variables: side_of_field <chr>, yardline_100 <dbl>, game_date <chr>,
#   quarter_seconds_remaining <dbl>, half_seconds_remaining <dbl>, game_seconds_remaining <dbl>,
#   game_half <chr>, quarter_end <dbl>, drive <dbl>, sp <dbl>, qtr <dbl>, down <dbl>, goal_to_go <dbl>,
#   time <chr>, yrdln <chr>, ydstogo <dbl>, ydsnet <dbl>, desc <chr>, play_type <chr>, yards_gained <dbl>,
#   shotgun <dbl>, no_huddle <dbl>, qb_dropback <dbl>, qb_kneel <dbl>, qb_spike <dbl>, qb_scramble <dbl>,
#   pass_length <chr>, pass_location <chr>, air_yards <dbl>, yards_after_catch <dbl>, run_location <chr>,
#   run_gap <chr>, field_goal_result <chr>, kick_distance <dbl>, extra_point_result <chr>,
#   two_point_conv_result <chr>, home_timeouts_remaining <dbl>, away_timeouts_remaining <dbl>, timeout <dbl>,
#   timeout_team <chr>, td_team <chr>, posteam_timeouts_remaining <dbl>, defteam_timeouts_remaining <dbl>,
#   total_home_score <dbl>, total_away_score <dbl>, posteam_score <dbl>, defteam_score <dbl>,
#   score_differential <dbl>, posteam_score_post <dbl>, defteam_score_post <dbl>,
#   score_differential_post <dbl>, no_score_prob <dbl>, opp_fg_prob <dbl>, opp_safety_prob <dbl>,
#   opp_td_prob <dbl>, fg_prob <dbl>, safety_prob <dbl>, td_prob <dbl>, extra_point_prob <dbl>,
#   two_point_conversion_prob <dbl>, ep <dbl>, epa <dbl>, total_home_epa <dbl>, total_away_epa <dbl>,
#   total_home_rush_epa <dbl>, total_away_rush_epa <dbl>, total_home_pass_epa <dbl>,
#   total_away_pass_epa <dbl>, air_epa <dbl>, yac_epa <dbl>, comp_air_epa <dbl>, comp_yac_epa <dbl>,
#   total_home_comp_air_epa <dbl>, total_away_comp_air_epa <dbl>, total_home_comp_yac_epa <dbl>,
#   total_away_comp_yac_epa <dbl>, total_home_raw_air_epa <dbl>, total_away_raw_air_epa <dbl>,
#   total_home_raw_yac_epa <dbl>, total_away_raw_yac_epa <dbl>, wp <dbl>, def_wp <dbl>, home_wp <dbl>,
#   away_wp <dbl>, wpa <dbl>, home_wp_post <dbl>, away_wp_post <dbl>, vegas_wp <dbl>, vegas_home_wp <dbl>,
#   total_home_rush_wpa <dbl>, total_away_rush_wpa <dbl>, total_home_pass_wpa <dbl>,
#   total_away_pass_wpa <dbl>, air_wpa <dbl>, yac_wpa <dbl>, comp_air_wpa <dbl>, comp_yac_wpa <dbl>,
#   total_home_comp_air_wpa <dbl>, total_away_comp_air_wpa <dbl>, total_home_comp_yac_wpa <dbl>, …
> rbdatanine <- rbdatanine %>% filter(posteam == 'TB', rush == 1)%>%
+     group_by(rusher) %>%
+     summarize(
+         mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()
+     ) %>%
+     arrange(-mean_epa) %>%
+     filter(plays > 20) 
> rbdatatwenty <- rbdatatwenty %>% filter(posteam == 'TB', rush == 1)%>%
+     group_by(rusher) %>%
+     summarize(
+         mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()
+     ) %>%
+     arrange(-mean_epa) %>%
+     filter(plays > 20)
Error: Problem with `filter()` input `..1`.
x object 'posteam' not found
ℹ Input `..1` is `posteam == "TB"`.
Run `rlang::last_error()` to see where the error occurred.
> rbdatatwenty <- rbdatatwenty %>% filter(posteam == 'TB', rush == 1)%>%
+     group_by(rusher) %>%
+     summarize(
+         mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()
+     ) %>%
+     arrange(-mean_epa) %>%
+     filter(plays > 20)
Error: Problem with `filter()` input `..1`.
x object 'posteam' not found
ℹ Input `..1` is `posteam == "TB"`.
Run `rlang::last_error()` to see where the error occurred.
> rbdatanine <-  readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))
> rbdatatwenty <-  readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))
> rbdatanine <-  readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))
> rbdatanine <- rbdatanine %>% filter(posteam == 'TB', rush == 1)%>%
+     group_by(rusher) %>%
+     summarize(
+         mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()
+     ) %>%
+     arrange(-mean_epa) %>%
+     filter(plays > 20)
> rbdatatwenty <- rbdatatwenty %>% filter(posteam == 'TB', rush == 1)%>%
+     group_by(rusher) %>%
+     summarize(
+         mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()
+     ) %>%
+     arrange(-mean_epa) %>%
+     filter(plays > 20)
> rbdataeight
# A tibble: 3 x 5
  rusher    mean_epa success_rate   ypc plays
  <chr>        <dbl>        <dbl> <dbl> <int>
1 J.Rodgers   -0.148        0.303  3.21    33
2 P.Barber    -0.234        0.315  3.51   248
3 R.Jones     -0.315        0.261  1.91    23
> 
> rodgerseight <- filter(rbdataeight, rusher == 'J.Rodgers')
> rjoneseight <- filter(rbdataeight, rusher == 'R.Jones')
> rjonesnine <- filter(rbdatanine, rusher == 'R.Jones')
> rjonesnine
# A tibble: 1 x 5
  rusher  mean_epa success_rate   ypc plays
  <chr>      <dbl>        <dbl> <dbl> <int>
1 R.Jones   -0.149        0.339  4.09   177
> ltwenty <- filter(rbdatatwenty, rusher == 'L.Fournette')
> kvaughntwenty <- filter(rbdatatwenty, rusher == 'K. Vaughn')
> pbarbernine <- filter(rbdatanine, rusher == 'P.Barber')
> rjonestwenty <- filter(rbdatatwenty, rusher == 'R.Jones')
> jonestable <- rbindlist(list(rjoneseight, rjonesnine, rjonestwenty), use.names = TRUE, fill = TRUE)
> jonestable
    rusher    mean_epa success_rate      ypc plays
1: R.Jones -0.31515237    0.2608696 1.913043    23
2: R.Jones -0.14888128    0.3389831 4.090395   177
3: R.Jones -0.02483251    0.4072398 4.778281   221
> head(teams_colors_logos)
# A tibble: 6 x 11
  team_abbr team_name team_id team_nick team_color team_color2 team_color3 team_color4 team_logo_wikip…
  <chr>     <chr>     <chr>   <chr>     <chr>      <chr>       <chr>       <chr>       <chr>           
1 ARI       Arizona … 3800    Cardinals #97233f    #000000     #ffb612     #a5acaf     https://upload.…
2 ATL       Atlanta … 0200    Falcons   #a71930    #000000     #a5acaf     #a30d2d     https://upload.…
3 BAL       Baltimor… 0325    Ravens    #241773    #000000     #9e7c0c     #c60c30     https://upload.…
4 BUF       Buffalo … 0610    Bills     #00338d    #c60c30     #0c2e82     #d50a0a     https://upload.…
5 CAR       Carolina… 0750    Panthers  #0085ca    #000000     #bfc0bf     #0085ca     https://upload.…
6 CHI       Chicago … 0810    Bears     #0b162a    #c83803     #0b162a     #c83803     https://upload.…
# … with 2 more variables: team_logo_espn <chr>, team_wordmark <glue>
> jonestable <- jonestable %>%
+     left_join(teams_colors_logos, by = c('team' = 'team_abbr'))
Error: Join columns must be present in data.
x Problem with `team`.
Run `rlang::last_error()` to see where the error occurred.
> team <- c('TB', 'TB', 'TB')
> jonestable$team = team
> jonestable
    rusher    mean_epa success_rate      ypc plays team
1: R.Jones -0.31515237    0.2608696 1.913043    23   TB
2: R.Jones -0.14888128    0.3389831 4.090395   177   TB
3: R.Jones -0.02483251    0.4072398 4.778281   221   TB
> jonestable <- jonestable %>%
+          left_join(teams_colors_logos, by = c('team' = 'team_abbr'))
> ggplot(jonestable, aes(x= rusher, y = mean_epa)) + geom_bar(stat = 'identity') +  geom_image(aes(image = team_logo_espn), size = 21000, asp = 16 / 9) + coord_flip()
> jonestable$years = c(2018, 2019, 2020)
> jonestable
    rusher    mean_epa success_rate      ypc plays team            team_name team_id  team_nick team_color
1: R.Jones -0.31515237    0.2608696 1.913043    23   TB Tampa Bay Buccaneers    4900 Buccaneers    #d50a0a
2: R.Jones -0.14888128    0.3389831 4.090395   177   TB Tampa Bay Buccaneers    4900 Buccaneers    #d50a0a
3: R.Jones -0.02483251    0.4072398 4.778281   221   TB Tampa Bay Buccaneers    4900 Buccaneers    #d50a0a
   team_color2 team_color3 team_color4
1:     #34302b     #000000     #ff7900
2:     #34302b     #000000     #ff7900
3:     #34302b     #000000     #ff7900
                                                                                                          team_logo_wikipedia
1: https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/Tampa_Bay_Buccaneers_logo.svg/100px-Tampa_Bay_Buccaneers_logo.svg.png
2: https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/Tampa_Bay_Buccaneers_logo.svg/100px-Tampa_Bay_Buccaneers_logo.svg.png
3: https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/Tampa_Bay_Buccaneers_logo.svg/100px-Tampa_Bay_Buccaneers_logo.svg.png
                                     team_logo_espn
1: https://a.espncdn.com/i/teamlogos/nfl/500/tb.png
2: https://a.espncdn.com/i/teamlogos/nfl/500/tb.png
3: https://a.espncdn.com/i/teamlogos/nfl/500/tb.png
                                                           team_wordmark years
1: https://github.com/guga31bb/nflfastR-data/raw/master/wordmarks/TB.png  2018
2: https://github.com/guga31bb/nflfastR-data/raw/master/wordmarks/TB.png  2019
3: https://github.com/guga31bb/nflfastR-data/raw/master/wordmarks/TB.png  2020
> ggplot2(jonestable, aes(x = years, y = mean_epa)) + geom_bar(stat = 'identity') + coord_flip()
Error in ggplot2(jonestable, aes(x = years, y = mean_epa)) : 
  could not find function "ggplot2"
> ggplot(jonestable, aes(x = years, y = mean_epa)) + geom_bar(stat = 'identity') + coord_flip()
> ggplot(jonestable, aes(x = years, y = mean_epa)) + geom_bar(stat = 'identity')
> ggplot(jonestable, aes(x= years, y= mean_epa)) +
+     geom_line()
> jonestable %>%
+     tail(10) %>%
+     ggplot( aes(x= years, y= mean_epa)) +
+     geom_line( color="grey") +
+     geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
+     theme_ipsum() +
+     ggtitle("R. Jones")
Error in theme_ipsum() : could not find function "theme_ipsum"
> library(hrbrthemes)
> jonestable %>%
+     tail(10) %>%
+     ggplot( aes(x= years, y= mean_epa)) +
+     geom_line( color="grey") +
+     geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
+     theme_ipsum() +
+     ggtitle("R. Jones")
> plot(jonestable$years~jonestable$mean_epa , type="b" , lwd=3 , col=rgb(0.1,0.7,0.1,0.8) , ylab="value of ..." , xlab="date" , bty="l" , pch=20 , cex=4)
> abline(h=seq(0,100,10) , col="grey", lwd=0.8)
> plot(jonestable$mean_epa~jonestable$years , type="b" , lwd=3 , col=rgb(0.1,0.7,0.1,0.8) , ylab="value of ..." , xlab="date" , bty="l" , pch=20 , cex=4)
> plot(jonestable$mean_epa~jonestable$years , type="b" , lwd=3 , col=rgb(0.1,0.7,0.1,0.8) , ylab='mean_epa' , xlab="Years" , xtitle = 'R.Jones', bty="l" , pch=20 , cex=4)
Warning messages:
1: In plot.window(...) : "xtitle" is not a graphical parameter
2: In plot.xy(xy, type, ...) : "xtitle" is not a graphical parameter
3: In axis(side = side, at = at, labels = labels, ...) :
  "xtitle" is not a graphical parameter
4: In axis(side = side, at = at, labels = labels, ...) :
  "xtitle" is not a graphical parameter
5: In box(...) : "xtitle" is not a graphical parameter
6: In title(...) : "xtitle" is not a graphical parameter
> 
> plot(jonestable$mean_epa~jonestable$years , type="b" , lwd=3 , col=rgb(0.1,0.7,0.1,0.8) , ylab='mean_epa' , xlab="Years", bty="l" , pch=20 , cex=4)
There were 12 warnings (use warnings() to see them)
> plot(jonestable$mean_epa~jonestable$years , type="b" , lwd=3 , col=rgb(0.1,0.7,0.1,0.8) , ylab="value of ..." , xlab="date" , bty="l" , pch=20 , cex=4)
> plot(jonestable$mean_epa~jonestable$years , type="b" , lwd=3 , col=rgb(0.1,0.7,0.1,0.8) , ylab="mean_epa" , xlab="years" , sub = 'R.Jones', bty="l" , pch=20 , cex=4)
> plot(jonestable$mean_epa~jonestable$years , type="b" , lwd=3 , col=rgb(0.1,0.7,0.1,0.8) , ylab="mean_epa" , xlab="years" , main = 'R.Jones', bty="l" , pch=20 , cex=4)
> rbdatatwenty %>% tail(10) %>%
+     ggplot( aes(x= years, y= mean_epa)) +
+     geom_line( color="grey") +
+     geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
+     theme_ipsum() +
+     ggtitle("@020 Running Backs")
Error in FUN(X[[i]], ...) : object 'years' not found
> rbdatatwenty %>%
+     tail(10) %>%
+     ggplot( aes(x= rusher, y= mean_epa)) +
+     geom_line( color="grey") +
+     geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
+     theme_ipsum() +
+     ggtitle("2020 Running Backs")
geom_path: Each group consists of only one observation. Do you need to adjust the group aesthetic?
> rbdatatwenty %>%
+     ggplot( aes(x= rusher, y= mean_epa)) +
+     geom_bar(stat = 'identity') +
+     ggtitle("R. Jones")
> ggplot(rbdatatwenty, aes(x=rusher, y=mean_epa)) +
+     geom_point() + 
+     geom_segment( aes(x=x, xend=x, y=0, yend=y))
Error: Aesthetics must be either length 1 or the same as the data (3): yend
Run `rlang::last_error()` to see where the error occurred.
> 
> library(fforcats)
Error in library(fforcats) : there is no package called ‘fforcats’
> library(forcats)
> rbdatatwenty %>%
+     ggplot( aes(x=rusher, y=mean_epa)) +
+     geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
+     coord_flip() +
+     xlab("") +
+     theme_bw()
> rbdatatwenty %>%
+          ggplot( aes(x=rusher, y=mean_epa)) +
+          geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
+          xlab("") +
+          theme_bw()
> rbdatatwenty %>%
+               ggplot( aes(x=rusher, y=mean_epa)) +
+               geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
+               xlab("") +
+               theme_bw() +
+     scale_y_reverse()
> rbdatatwenty %>%
+     ggplot( aes(x=rusher, y=mean_epa)) +
+     geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
+     xlab("") +
+     theme_bw() +
+     scale_y_reverse() + 
+ ggtitle('2020 Runningbacks')
> rbdatatwenty %>%
+      ggplot( aes(x=rusher, y=mean_epa)) +
+      geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
+      xlab("") +
+      theme_bw() +
+      scale_y_reverse() + 
+  ggtitle('2020 Running backs')
