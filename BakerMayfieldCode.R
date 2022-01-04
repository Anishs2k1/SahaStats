library(tidyverse)
library(ggrepel)
library(ggplot2)
library(ggimage)
library(nflfastR)
data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))
secdata <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2018.rds'))
brown <- data %>%
    filter(rush == 1 | pass == 1, !is.na(epa))
brown %>%
   filter(posteam == 'CLE', rush == 1) %>%
   group_by(rusher) %>%
   summarize(
         mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()
     ) %>%
       arrange(-mean_epa) %>%
       filter(plays > 20)
secdata %>%
       filter(posteam == 'CLE', rush == 1) %>%
             group_by(rusher) %>%
             summarize(
                           mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()
                      ) %>%
                 arrange(-mean_epa) %>%
                 filter(plays > 20)
twentydata <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))
twentydata %>% filter(posteam == 'CLE', rush == 1) %>%
                       group_by(rusher) %>%
                       summarize(
                                             mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()
                                         ) %>%
                           arrange(-mean_epa) %>%
                           filter(plays > 20)
url <- "https://www.pro-football-reference.com/teams/cle/2020/gamelog/"
colnames(pfr_raw) <- make.names(pfr_raw[1, ], unique = TRUE, allow_ = TRUE)
pfr <- pfr_raw %>%
      slice(-1) %>%
      select(Week, Opp, Tm, Opp.1)
schedule_table2020 <- newpfr %>% gt() %>% 
                 tab_style(
                                             style = list(
                                                                   cell_text(weight = "bold")
                                                               ),
                                             locations = cells_column_labels(everything())
                                        ) %>%
                  tab_header(
                                    title = "Browns Schedule till Week 17",
                                    subtitle = "2020"
                                )

url <- "https://www.pro-football-reference.com/teams/cle/2020.htm#all_games"
 pfr_raw <- url %>%
  +                read_html() %>%
  +                html_table() %>%
  +       as.data.frame()
 pfr <- pfr_raw %>%
  + slice(-1) %>%
  + select(Week, Opp, Tm, Opp)
colnames(pfr_raw) <- make.names(pfr_raw[1, ], unique = TRUE, allow_ = TRUE)
url <- "https://www.pro-football-reference.com/teams/cle/2020.htm#all_games"
 pfr_raw <- url %>%
                  read_html() %>%
                  html_table() %>%
         as.data.frame()
 pfr <- pfr_raw %>%
  + slice(-1) %>%
  + select(Week, Opp, Tm, Opp)

qbs <- pbp %>%
  filter(week <= 17, !is.na(epa)) %>%
  group_by(id, name) %>%
  summarize(
    epa = mean(qb_epa),
    cpoe = mean(cpoe, na.rm = T),
    n_dropbacks = sum(pass),
    n_plays = n(),
    team = last(posteam)
  ) %>%
  ungroup() %>%
  filter(n_dropbacks > 100 & n_plays > 1000)
bakertable <- rbindlist(list(bakerrookie, bakernine,bakertwent), use.names = TRUE, fill = TRUE)
years = c(2018,2019,2020)
 bakertable$year = years
qbtable %>% ggplot(aes(x = epa, y = cpoe)) +
  geom_point() + 
  geom_text_repel(aes(label=names)) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm') +
  labs(x = "Completion % above expected (CPOE)",y = "EPA per play (passes, rushes, and penalties)",  title = "Rookie QB Efficienty, 2018", caption = "Data: @nflfastR") +
  theme_bw() + theme(
            aspect.ratio = 9 / 16,
            plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
       ) +
  geom_image(aes(image = team_logo_espn), size = .07, asp = 16 / 9)
bakertable %>%
       tail(10) %>%
       ggplot( aes(x=epa, y=years)) +
       geom_line( color="grey") +
       geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
       theme_ipsum() +
       ggtitle("Evolution of Baker's Play") 
qbs %>%
            ggplot(aes(x = cpoe, y = epa)) +
            
            geom_hline(yintercept = mean(qbs$epa), color = "red", linetype = "dashed", alpha=0.5) +
           
            geom_vline(xintercept =  mean(qbs$cpoe), color = "red", linetype = "dashed", alpha=0.5) +
            
  
            geom_image(aes(image = team_logo_espn), size = qbs$n_plays / 15000, asp = 16 / 9) +
            
            geom_text_repel(aes(label=name)) +
           
            stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
            
            labs(x = "Completion % above expected (CPOE)",
                                            y = "EPA per play (passes, rushes, and penalties)",
                                            title = "Quarterback Efficiency in 2020",
                                            caption = "Data: @nflfastR") +
            
            theme_bw() +
            
            theme(
                      plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
                  ) +
           
            
            scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
            scale_x_continuous(breaks = scales::pretty_breaks(n = 10))



get_nfl_qbr <- function(season = 2019, week = NA, season_type = "Regular") {
  current_year <- as.double(substr(Sys.Date(), 1, 4))
  
  # Error handling to correct season type
  if (!season_type %in% c("Regular", "Playoffs")) {
    stop("Please choose season_type of 'Regular' or 'Playoffs'")
  }
  
  # Error handling for limits on season
  if (!between(as.numeric(season), 2018, current_year)) {
    stop(paste("Please choose season between 2006 and", current_year))
  }
  
  # Error handling for limits on regular season weeks
  if (!is.na(week) & season_type == "Regular" & !between(as.numeric(week), 1, 17)) {
    stop("Please choose regular season week between 1 and 17")
  }
  
  # Error handling for limits on playoff weeks
  if (!is.na(week) & season_type == "Playoffs" & !between(as.numeric(week), 1, 4)) {
    stop("Please choose Playoff week between 1 and 4")
  }
  
  # Error handling for missing data from ESPN
  if (!is.na(week) & season_type == "Playoffs" & as.numeric(season) == 2017) {
    stop("ESPN has missing Playoff data for 2017")
  }
  week_current <- if_else(
    # Logic check to fix years where superbowl = 5
    season_type == "Playoffs" & as.numeric(week) == 4 & season >= 2018, 
    # outcome = 5  
    5, 
    # default to normal week
    as.numeric(week)
  )
  
  # Add useful messages - separated by week
  message(if_else(is.na(week),
                  glue("Scraping QBR totals for {season}!"),
                  glue("Scraping weekly QBR for week {week} of {season}!")
  ))
  url_start <- "https://site.web.api.espn.com/apis/fitt/v3/sports/football/nfl/qbr?region=us&lang=en&qbrType="
  url_body <- "&isqualified=true&sort=schedAdjQBR%3Adesc&season="
  url_1 <- glue("{url_start}seasons&seasontype=2{url_body}{season}")
  url_2 <- glue("{url_start}weeks&seasontype=2{url_body}{season}&week={week}")
  url_3 <- glue("{url_start}seasons&seasontype=3{url_body}{season}")
  url_4 <- glue("{url_start}weeks&seasontype=3{url_body}{season}&week={week_current}")
  url <- case_when(
    is.na(week) & season_type == "Regular" ~ url_1,
    !is.na(week) & season_type == "Regular" ~ url_2,
    is.na(week) & season_type == "Playoffs" ~ url_3,
    !is.na(week) & season_type == "Playoffs" ~ url_4,
    TRUE ~ NA_character_
  )
  raw_json <- fromJSON(url)
  get_qbr_data <- function(row_n) {
    pluck(raw_json, "athletes", "categories", row_n, "totals", 1)
  }
  quiet_unnest_wider <- quietly(unnest_wider)
  
  pluck(raw_json, "athletes", "athlete") %>%
    as_tibble() %>%
    select(firstName:shortName, headshot, teamName:teamShortName) %>%
    mutate(row_n = row_number()) %>%
    mutate(data = map(row_n, get_qbr_data)) %>%
    # lots of name_repair here that I am silencing
    quiet_unnest_wider(data) %>%
    pluck("result") %>%
    
    # Names we need to add from the unnest_wider()
    set_names(nm = c(
      "first_name", "last_name", "name",
      "short_name", "headshot_href", "team_name",
      "team_short_name", "row_n", "qbr_total",
      "points_added", "qb_plays", "total_epa",
      "pass", "run", "exp_sack", "penalty", "raw_qbr", "sack"
    )) %>%
    # Add season and season type back to the main tibble
    mutate(
      season = as.double(season),
      season_type = season_type,
      # Get the headshot url
      headshot_href = pull(headshot_href, href),
      
      # Clean up the week numbers if playoffs or regular season
      game_week = case_when(
        !is.na(week_current) ~ as.character(week_current),
        !is.na(week_current) & season_type == "Playoffs" ~ as.character(
          factor(week_current,
                 levels = c(1:5),
                 labels = c(
                   "Wild Card",
                   "Divisional",
                   "Conference Championship",
                   "Super Bowl",
                   "Super Bowl"
                 )
          )
        ),
        is.na(week_current) & season_type == "Playoffs" ~ "Playoff Total",
        is.na(week_current) & season_type == "Regular" ~ "Season Total",
        TRUE ~ NA_character_
      )
    ) %>%
    select(season:game_week, rank = row_n, first_name:short_name, 
           team_name, team_short_name, qbr_total:sack) %>%
    mutate_at(vars(qbr_total:sack), as.double)
}
br_all <- crossing(season = 2018:2020, season_type = "Regular", week = NA) %>%
  purrr::pmap_dfr(get_nfl_qbr)
qbr_all <- merge(x = qbr_all, y = df_logo_colors, by.x = "team_short_name", by.y = "team_abbr", all.x = TRUE) %>%
  dplyr::rename(
    color = team_color,
    color2 = team_color2
  )
qbr_plot <- function(qbr, qb) {
  
  # Filter abr to qb's dropbacks
  df <- qbr %>% 
    filter(short_name == qb)
  
  # Find beg and end seasons. Find first and last name
  beg_szn <- as.integer(min(df$season))
  end_szn <- as.integer(max(df$season))
  first_name <- df$first_name
  last_name <- df$last_name
  
  # Plot
  p <- df %>% 
    ggplot(aes(season, qbr_total)) +
    geom_line(size = 2, color = df$color2) +
    geom_point(size = 5, color = "white") +
    # geom_ribbon(aes(ymin = 50, ymax = qbr_total, fill = team_name),
    #              show.legend = FALSE) +
    # scale_fill_manual(values = df$color2) + 
    labs(caption = paste("Data from espn.com/nfl/qbr"),
         title = paste(first_name, last_name, "QBR")) +
    theme(panel.background = element_rect(fill = df$color),
          plot.background = element_rect(fill = df$color),
          panel.grid = element_blank(),
          axis.text.y = element_text(color = "white", size = 14),
          axis.title.y = element_text(color = "white", size = 16),
          axis.text.x = element_text(color = "white", size = 14),
          axis.title.x = element_blank(),
          axis.line.y = element_line(color = df$color2, size = 2),
          axis.line.x = element_line(color = df$color2, size = 2),
          axis.ticks = element_blank(),
          plot.title = element_text(size = 20, 
                                    hjust = 0.5,
                                    face = "bold",
                                    color = df$color2),
          plot.caption = element_text(size = 10, 
                                      color = df$color2)) +
    scale_y_continuous(name = "QBR") +
    scale_x_continuous(limits = c(beg_szn, end_szn), breaks = c(beg_szn:end_szn))
  
  # Show plot
  p
  
  # Save plot as png
  #  ggsave(p, filename = paste0(qb,".png"), dpi = 600, width = 14, height = 8)
}
p5 <- qbr_plot(qbr_all, "B. Mayfield")
p5 
get_nfl_qbr <- function(season = 2020, week = NA, season_type = "Regular") {
  current_year <- as.double(substr(Sys.Date(), 1, 4))
  if (!season_type %in% c("Regular", "Playoffs")) {
    stop("Please choose season_type of 'Regular' or 'Playoffs'")
  }
  if (!between(as.numeric(season), 2018, current_year)) {
    stop(paste("Please choose season between 2006 and", current_year))
  }
  if (!is.na(week) & season_type == "Regular" & !between(as.numeric(week), 1, 17)) {
    stop("Please choose regular season week between 1 and 17")
  }
  if (!is.na(week) & season_type == "Playoffs" & !between(as.numeric(week), 1, 4)) {
    stop("Please choose Playoff week between 1 and 4")
  }
  if (!is.na(week) & season_type == "Playoffs" & as.numeric(season) == 2018) {
    stop("ESPN has missing Playoff data for 2017")
  }
  week_current <- if_else(
    season_type == "Playoffs" & as.numeric(week) == 4 & season >= 2018, 
    5,
    as.numeric(week)
  )
  message(if_else(is.na(week),
                  glue("Scraping QBR totals for {season}!"),
                  glue("Scraping weekly QBR for week {week} of {season}!")
  ))
  url_start <- "https://site.web.api.espn.com/apis/fitt/v3/sports/football/nfl/qbr?region=us&lang=en&qbrType="
  url_body <- "&isqualified=true&sort=schedAdjQBR%3Adesc&season="
  url_1 <- glue("{url_start}seasons&seasontype=2{url_body}{season}")
  url_2 <- glue("{url_start}weeks&seasontype=2{url_body}{season}&week={week}")
  url_3 <- glue("{url_start}seasons&seasontype=3{url_body}{season}")
  url_4 <- glue("{url_start}weeks&seasontype=3{url_body}{season}&week={week_current}")
  url <- case_when(
    is.na(week) & season_type == "Regular" ~ url_1,
    !is.na(week) & season_type == "Regular" ~ url_2,
    is.na(week) & season_type == "Playoffs" ~ url_3,
    !is.na(week) & season_type == "Playoffs" ~ url_4,
    TRUE ~ NA_character_
  )
  raw_json <- instfromJSON(url)
  get_qbr_data <- function(row_n) {
    pluck(raw_json, "athletes", "categories", row_n, "totals", 1)
  } 
  quiet_unnest_wider <- quietly(unnest_wider)
  pluck(raw_json, "athletes", "athlete") %>%
    as_tibble() %>%
    select(firstName:shortName, headshot, teamName:teamShortName) %>%
    mutate(row_n = row_number()) %>%
    mutate(data = map(row_n, get_qbr_data)) %>%
    quiet_unnest_wider(data) %>%
    pluck("result") %>%
    set_names(nm = c(
      "first_name", "last_name", "name",
      "short_name", "headshot_href", "team_name",
      "team_short_name", "row_n", "qbr_total",
      "points_added", "qb_plays", "total_epa",
      "pass", "run", "exp_sack", "penalty", "raw_qbr", "sack"
    )) %>%
    mutate(
      season = as.double(season),
      season_type = season_type,
      headshot_href = pull(headshot_href, href),
      game_week = case_when(
        !is.na(week_current) ~ as.character(week_current),
        !is.na(week_current) & season_type == "Playoffs" ~ as.character(
          factor(week_current,
                 levels = c(1:5),
                 labels = c(
                   "Wild Card",
                   "Divisional",
                   "Conference Championship",
                   "Super Bowl",
                   "Super Bowl"
                 )
          )
        ),
        is.na(week_current) & season_type == "Playoffs" ~ "Playoff Total",
        is.na(week_current) & season_type == "Regular" ~ "Season Total",
        TRUE ~ NA_character_
      )
    ) %>%
    select(season:game_week, rank = row_n, first_name:short_name, 
           team_name, team_short_name, qbr_total:sack) %>%
    mutate_at(vars(qbr_total:sack), as.double)
}
qbr_all <- crossing(season = 2018:2020, season_type = "Regular", week = NA) %>%
  purrr::pmap_dfr(get_nfl_qbr)

qbr_plot <- function(qbr, qb) {
  
  
  df <- qbr %>% 
    filter(short_name == qb)
  
  
  beg_szn <- as.integer(min(df$season))
  end_szn <- as.integer(max(df$season))
  first_name <- df$first_name
  last_name <- df$last_name
  
  
  p <- df %>% 
    ggplot(aes(season, qbr_total)) +
    geom_line(size = 2, color = df$color2) +
    geom_point(size = 5, color = "white") +
    labs(caption = paste("Data from espn.com/nfl/qbr"),
         title = paste(first_name, last_name, "QBR")) +
    theme(panel.background = element_rect(fill = df$color),
          plot.background = element_rect(fill = df$color),
          panel.grid = element_blank(),
          axis.text.y = element_text(color = "white", size = 14),
          axis.title.y = element_text(color = "white", size = 16),
          axis.text.x = element_text(color = "white", size = 14),
          axis.title.x = element_blank(),
          axis.line.y = element_line(color = df$color2, size = 2),
          axis.line.x = element_line(color = df$color2, size = 2),
          axis.ticks = element_blank(),
          plot.title = element_text(size = 20, 
                                    hjust = 0.5,
                                    face = "bold",
                                    color = df$color2),
          plot.caption = element_text(size = 10, 
                                      color = df$color2)) +
    scale_y_continuous(name = "QBR") +
    scale_x_continuous(limits = c(beg_szn, end_szn), breaks = c(beg_szn:end_szn))
  ggsave(p, filename = paste0(qb,".png"), dpi = 600, width = 14, height = 8)
  p
}
url
[1] "https://www.pro-football-reference.com/years/2020/passing_advanced.htm"
pfr_raw <- url %>%
  +     read_html() %>%
  +     html_table() %>%
  +     as.data.frame()

pfr <- pfr_raw %>%
  +     slice(-1) %>%
  +     select(Player, Tm, IAY.PA, Bad., Att) %>%
  +     rename(team = Tm) %>%
  +     mutate(
    +         
      +         team = case_when(
        +             team == "GNB" ~ "GB",
        +             team == "KAN" ~ "KC",
        +             team == "NOR" ~ "NO",
        +             team == "NWE" ~ "NE",
        +             team == "SFO" ~ "SF",
        +             team == "TAM" ~ "TB",
        +             TRUE ~ team
        +         ),
    +         # repair player names
      +         Player = str_replace(Player, "\\*", ""),
    +         Player = str_replace(Player, "\\+", ""),
    +         
      +         # make interesting columns numeric
      +         IAY.PA = as.numeric(IAY.PA),
    +         Bad. = as.numeric(str_replace(Bad., "%", "")),
    +         Passattempts = as.numeric(Att)
    +     ) %>%
  +     left_join(nflfastR::teams_colors_logos, by = c("team" = "team_abbr"))
saveRDS(pfr, file = "pfr_bad_throws.rds")
chart_data <- readRDS("pfr_bad_throws.rds") %>% filter(Passattempts > 180)
chart_data %>% 
  +     ggplot(aes(x = IAY.PA, y = Bad./100)) +
  +     geom_hline(aes(yintercept = mean(Bad./100)), color = "red", linetype = "dotted") +
  +     geom_vline(aes(xintercept =  mean(IAY.PA)), color = "red", linetype = "dotted") +
  +     geom_smooth(method = "lm", se = FALSE, color="black", size=0.3) +
  +     geom_point(color = chart_data$team_color, aes(cex=Passattempts), alpha=1/4) +
  +     ggrepel::geom_text_repel(aes(label=Player), force=1, point.padding=0, segment.size=0.1, size = 3) +
  +     scale_y_continuous(labels=scales::percent) +
  +     scale_size_area(max_size = 6) +
  +     labs(x = "Average Depth of Target in Yards",
             +          y = "Bad Throw Percentage",
             +          caption = "Bad Throw Percentage = Percentage of throws that weren't catchable\nFigure: @mrcaseb | @AnishStats ",
             +          title = 'QB Passing Performance 2020',
             +          subtitle = "Focus on Baker") +
  +     ggthemes::theme_stata(scheme = "sj", base_size = 8) +
  +     theme(
    +         plot.title = element_text(face = 'bold'),
    +         plot.caption = element_text(hjust = 1),
    +         axis.text.y = element_text(angle = 0, vjust = 0.5),
    +         legend.title = element_text(size = 8, hjust = 0, vjust = 0.5, face = 'bold'),
    +         legend.position = "top",
    +         aspect.ratio = 1/1.618
    +     ) +
  +     NULL

 