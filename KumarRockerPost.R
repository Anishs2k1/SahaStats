#Kumar Rocker

library(dplyr)
library(xml2)
library(rvest)

#Starting Pitcher
kumar20 <- ncaa_scrape(736, 2020, "pitching") %>% filter(Player == "Rocker, Kumar")

#Webscraping andFormatting NCAA Data into specific conferences

teams <- function(year, conference, div) {
       url <- paste0("http://stats.ncaa.org/team/inst_team_list?academic_year=", year, "&conf_id=", conference, "&division=", div, "&sport_code=MBA")
       read <- read_html(url)
       links <- html_nodes(read, "a") %>%
           html_attr("href")
       link_names <- html_nodes(read, "a") %>%
           html_text()
       table <- as.data.frame(cbind(link_names, links))
       table$links <- as.character(table$links)
       table$link_names <- as.character(table$link_names)
       table <- table %>%
            filter(grepl("team", links)) %>%
            filter(!grepl("inst_team", links)) %>%
            filter(!grepl("schedule", links))
       table$links <- gsub("/team/", "", table$links)
       table$links <- sub("/.*", "", table$links)
       table$year <- year
       table$division <- div
       table$conference_id <- conference
       names(table) <- c("school", "school_id", "year", "division", "conference_id")
       table
   }

#arranging the schools from the previous tables
master_ncaa_team_lu <- conference_code_lu %>% 
  +     group_by(year, division, conference, conference_id) %>% 
  +     do(teams(.$year, .$conference_id, .$division)) %>%
  +     ungroup() %>%
  +     select(school, conference, everything()) %>%
  +     mutate(school_id = as.numeric(school_id)) %>%
  +     arrange(school)

#finding specific players - Kumar Rocker
ncaa_scrape(736, 2020, "batting")
ncaa_scrape(430, 2020, "pitching")
kumartable <- kumartable[,c("school","Player",  "GP", "ERA", "IP", "H", "R", "ER", "BB", "SO", "SHO")]

#order pitchers of top 25
#Earned run Average
MS 430
ms <- ncaa_scrape(430, 2021, "Pitching")
Vander 736
vanderbilt <- ncaa_scrape(736, 2021, "Pitching")
TX 703
texas <- ncaa_scrape(703, 2021, "Pitching")
NC State 490
ncstate <- ncaa_scrape(490, 2021, "Pitching")
ARK 31
arkansas <- ncaa_scrape(31, 2021, "Pitching")
Warning messages:
Tennessee 694
tennesse <- ncaa_scrape(694, 2021, "Pitching")
Arizona 29
arizona <- ncaa_scrape(29, 2021, "Pitching")
Stanford 674
stanford <- ncaa_scrape(674, 2021, "Pitching")
Virginia 746
virgina <-  ncaa_scrape(746, 2021, "Pitching")
Notre Dame 513
notredame <- ncaa_scrape(513, 2021, "Pitching")
Texas Tech 700
ttech <- ncaa_scrape(700, 2021, "Pitching")
ECU 196
ECU <- ncaa_scrape(196, 2021, "Pitching")
Ole Miss 433
olemiss <- ncaa_scrape(433, 2021, "Pitching")
Dallas Baptist 1045
dbu <- ncaa_scrape(1045, 2021, "Pitching")
TCU 698
tcu <- ncaa_scrape(698, 2021, "Pitching")
Old Dominion 523
olddominion <- ncaa_scrape(523, 2021, "Pitching")
Nebraska 463
nebraska <- ncaa_scrape(463, 2021, "Pitching")
LSU 365
lsu <- ncaa_scrape(365, 2021, "Pitching")
Oregon 529
oregon <- ncaa_scrape(529, 2021, "Pitching")
South Floridia 652
southflorida <- ncaa_scrape(652, 2021, "Pitching")
UCI 109
uci <- ncaa_scrape(109, 2021, "Pitching")
LA Tech 366
latech <- ncaa_scrape(366, 2021, "Pitching")
Southern Mississippi 664
southermiss <- ncaa_scrape(664, 2021, "Pitching")
UCLA 110
ucla <- ncaa_scrape(110, 2021, "Pitching")
Maryland 392
maryland <- ncaa_scrape(392, 2021, "Pitching")

> data.combined <- data.frame()
> data.combined <- rbind(kumar19, kumar20, kumar21)
> kumartable <- as.data.frame(data.combined)
#first plotting a graph with just Kumar Rocker

collegeplot <- ggplot(top2021, aes(x= IP, y = ERA)) + geom_point() + geom_text(x = 15.0, y = 1.80, label = "Kumar Rocker")

#combining other players as well
data.combined <- rbind (ms, vanderbilt, texas, ncstate, arkansas, tennesse, arizona, stanford, virgina, notredame, ttech, ECU, olemiss,dbu, tcu, olddominion, nebraska, lsu, oregon, southflorida, uci, latech, southermiss, ucla, maryland)
top2021 <-  as.data.frame(data.combined)
top2021 <- top2021[,c("school","Player", "Pos", "GP", "ERA", "IP", "H", "R", "ER", "BB", "SO", "SHO")]
> top2021 <- top2021 %>% filter(Pos == "P")
top20211 <- top2021[,-1]
> rownames(top20211) <- top2021[,2]
school        Player Pos GP  ERA  IP  H  R ER BB  SO SHO
<NA> Rocker, Kumar   P 20 2.73 122 75 43 37 39 179  NA


top2021filter <- top2021 %>% filter(GP > 15, ERA > 2, IP > 80)
> top2021filter1 <- top2021filter[,-1]
> rownames(top2021filter1) <- top2021filter[,2]

collegeplot <- ggplot(top2021filter, aes(x= IP, y = ERA)) + geom_point() + geom_label(
  +     label=rownames(top2021filter1), 
  +     nudge_x = 0.25, nudge_y = 0.25, 
  +     check_overlap = T
  + )

#factoring in EPA and IP for each other the players 
collegeplot + labs(title = "Top College Pitchers EPA vs IP",
                   +                                        subtitle = "Innings Played",
                   +                                        caption = "Pitchers from Top 25 teams")

top2022 <- top20211 %>% select(-Player)

top2022 <- top2022 %>% select(-Pos)
top2021filter1 <- subset(top2021filter1, select = -Player)
> top2021filter1 <- subset(top2021filter1, select = -Pos)
> km <- kmeans(top2021filter1, centers = 4, nstart = 30)
x = top2021filter1[,2:3]

#using clustering to create groups of players to see which players fall above others
library(cluster)
> clusplot(x,model$cluster)
> km <- kmeans(x, centers = 4, nstart = 30)
> fviz_cluster(km, data = x)
