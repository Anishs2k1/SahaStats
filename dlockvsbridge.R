play_by_play2018 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2018.csv"))
play_by_play2019 <- readRDS(url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds"))
play_by_play2020 <- readRDS(url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds"))

qu

quarterbacks <- quarterbacks %>% filter(!is.na(epa)) %>%
           group_by(passer_player_id, passer_player_name) %>%
           select(epa))

quarterbacks2019 %>% filter(!is.na(passer_player_name)) %>% group_by(passer_player_name) %>% summarize(
        epa = mean(epa), passinter = (count(pass_touchdown)/ count(interception)) , plays = n() ) %>% filter(plays > 50)
, passinter = (count(pass_touchdown)/ count(interception))

quarterbacks2019 %>% filter(!is.na(passer_player_name)) %>% group_by(passer_player_name) %>% summarize(
   epa = mean(epa), passinter = (as.integer(count((pass_touchdown)))/ as.integer(count(interception)) ), plays = n() ) %>% filter(plays > 50)

tbridge2020 <- qb2020 %>% filter(passer_player_name == "T.Bridgewater")
dlock2020 <- qb2020 %>% filter(passer_player_name == "D.Lock")
dlock2020 <- dlock2020["mean_epa"]
dlock2020 <- dlock2020[, c(2, 1)]
dlock2020 <- as.matrix(dlock2020)
data.combined <- data.frame()
data.combined <- rbind(dlock, dlock2020)
dlocktable <- as.data.frame(data.combined)

quarterbackselection <- quarterbacks2019 %>% filter(!is.na(passer_player_name)) %>% group_by(passer_player_name) %>% summarise(
 epa = mean(epa), passinter = (sum(interception) / sum(pass_touchdown)), plays = n() ) %>% filter(plays > 100)

dlock <- quarterbackselection %>% filter(passer_player_name == "D.Lock"
tbridge <- quarterbackselection %>% filter(passer_player_name == "T.Bridgewater")

denverqb <- data.frame(dlock, dlock20, tbridge19, tbridge)
combine(dlock, dlock20, tbridge19, tbridge)
library(gdata)


ggplot(data=denverqb, aes(x=passer_player_name, y=epa, ymin = -1, ymax = 1, fill=type, linetype=type)) + 
            geom_line() + 
            geom_ribbon(alpha=0.5) + 
            xlab("Lock vs Bridgewater") + 
            ylab("EPA")

dlock$type <- "lock"
> dlock20$type <- "lock"
> tbridge19$type <- "lock"
> tbridge$type <- "lock"
> denverqb <- combine(dlock, dlock20, tbridge19, tbridge)

ggplot(data=denverqb, aes(x=passer_player_name, y=epa, fill=type, linetype=type)) + 
  +     geom_line() + 
  +     geom_ribbon(alpha=0.5) + 
  +     xlab("Lock vs Bridgewater") + 
  +     ylab("EPA")

ggplot(data=denverqb, aes(x=passer_player_name, y=epa, ymin = -1, ymax = 1, fill=type, linetype=type)) + 
  +     geom_line() + 
  +     geom_ribbon(alpha=0.5) + 
  +     scale_x_log10() + 
  +     scale_y_log10() + 
  +     xlab("Lock vs Bridgewater") + 
  
  
  ggplot(data=denverqb, aes(x=passer_player_name, y=epa, ymin = -1, ymax = 1, fill=type, linetype=type)) + 
  +     geom_line() + 
  +     geom_ribbon(alpha=0.5) + 
  +     xlab("Lock vs Bridgewater") + 
  +     ylab("EPA")
  +     ylab("EPA")
> 
  
  years <- c(2019 LOCK, 2020 LOCK, 2019 Bridge, 2020 Bridge)
denverqb$years <- years

years <- c("2019 LOCK", "2020 LOCK", "2019 Bridge", "2020 Bridge")


denverqb %>%
  +     ggplot( aes(x=years, y=epa, group=passer_player_name, color=passer_player_name)) +
  +     geom_line() +
  +     scale_color_viridis(discrete = TRUE) +
  +     ggtitle("Denver QB Battle") +
  +     theme_ipsum() +
  +     ylab("EPA")

ggplot(data=denverqb, aes(x=passer_player_name, y=epa, ymin = -.5, ymax = .5, fill=type, linetype=type)) + 
  +          geom_line() + 
  +          geom_ribbon(alpha=0.5) + 
  +          xlab("Lock vs Bridgewater") + 
  +          ylab("EPA")

#https://www.pro-football-reference.com/teams/
#den
#2020.htm#all_games


ggplot(denverqb, aes(plays, passinter, colour = passer_player_name)) +
  +     geom_point() +
  +     geom_smooth(se = FALSE, method = lm)
DrewLockCollege %>% ggplot( aes (x = Years, y = CmpPerc, group = Years, color = Years)) +
            geom_line() +
            geom_point() +
  scale_color_viridis(discrete = TRUE)
             ggtitle("CmpPerc over Drew Locks Career") + 
             theme_ipsum()+
         ylab("Cmp%")+
            transition_reveal(Years)

             DrewLockCollege %>% ggplot(aes(x = Years, y = CmpPerc, group = Years, color = Years)) + 
                     geom_line()+
                     geom_point(alpha = .5, aes(group=1))+
                     scale_color_viridis_c()+
                     theme_ipsum()+
                     transition_reveal(Years)
             
             
  p <- ggplot(DrewLockCollege, aes( x= Years, y= CmpPerc))+
    geom_point(aes(group = seq_along(Years)), color = "Yellow", size = 4)+
    geom_line(alpha = .5, aes(group = 1))+
    scale_color_viridis_c()+
    ggtitle("CmpPerc over D.Lock Mizzou Career")+
    theme_ipsum()+
    transition_reveal(Years)
  
  
  passingplot2015 <- ggplot(passes, aes(x=passer_player_name, y=meanEPA, label=passer_player_name)) + geom_text_repel(aes(label=passer_player_name)) + 
     geom_hline(yintercept = 0, color = "red", linetype = "dashed", alpha=0.5) +
     geom_vline(xintercept =  0, color = "red", linetype = "dashed", alpha=0.5) +
     labs(x = "2015 Quarterbacks", y="EPA")
   
  + geom_image(aes(image = team_logo_espn))
  
  
  passes2020 <- play_by_play2020 %>% filter(!is.na(passer_player_name)) %>% group_by(passer_player_name) %>% summarize(
    epa = mean(epa), passinter = (sum(touchdown) / sum(interception)), dropbackEPA = sum(qb_dropback) / mean(epa), plays = n() ) %>% filter(plays > 150)
  
  formattable(devnertable, 
                                                     align =c("l","c","c","c","c", "r"), 
                                                     list(`passer_player_name` = formatter(
                                                                   "span", style = ~ style(color = "grey",font.weight = "bold")), 
                                                          `epa`= color_tile(customGreen, customGreen0),
                                                          'passinter' = color_tile(customGreen, customGreen0),
                                                          'dropbackEPA' = color_tile(customGreen, customGreen0),
                                                          'plays' = color_tile(customGreen, customGreen0),
                                                          `Improvement` = improvement_formatter
                                              
                                                             ))
  
  
  
  ggplot(devnertable, aes(x=passer_player_name, y=epa, fill=passer_player_name)) + 
    +          geom_bar(stat='identity') +
    +          theme_bw() +
    +          transition_states(
      +                  source,
      +                  transition_length = 2,
      +                  state_length = 1
      +              ) +
    +         ease_aes('sine-in-out') 
  
  
  ggplot(devnertable, aes(x=plays, y=passinter, color = passer_player_name)) +
         geom_point() +
          geom_line() +# Show dots
         geom_text(
               label= passer_player_name, 
               nudge_x = 0.25, nudge_y = 0.25, 
               check_overlap = T
           )
  
