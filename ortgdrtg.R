

library(tidyverse)
library(readxl)
library(ggimage)
# library(ggpubr)
# library(png)

nba_records <- read_excel("NBALast10_12.4.xlsx")

path <- "C:/Users/Ryan Kyaw/Desktop/NBA/Pythagorean Expectation/"

atlanta <- paste0(path, "Atlanta Hawks.png")
boston <- paste0(path, "Boston Celtics.png")
brooklyn <- paste0(path, "Brooklyn Nets.png")
charlotte <- paste0(path, "Charlotte Hornets.png")
chicago <- paste0(path, "Chicago Bulls.png")
cleveland <- paste0(path, "Cleveland Cavaliers.png")
dallas <- paste0(path, "Dallas Mavericks.png")
denver <- paste0(path, "Denver Nuggets.png")
detroit <- paste0(path, "Detroit Pistons.png")
goldenstate <- paste0(path, "Golden State Warriors.png")
houston <- paste0(path, "Houston Rockets.png")
indiana <- paste0(path, "Indiana Pacers.png")
clippers <- paste0(path, "Los Angeles Clippers.png")
lakers <- paste0(path, "Los Angeles Lakers.png")
memphis <- paste0(path, "Memphis Grizzlies.png")
miami <- paste0(path, "Miami Heat.png")
milwaukee <- paste0(path, "Milwaukee Bucks.png")
minnesota <- paste0(path, "Minnesota Timberwolves.png")
neworleans <- paste0(path, "New Orleans Pelicans.png")
newyork <- paste0(path, "New York Knicks.png")
okc <- paste0(path, "Oklahoma City Thunder.png")
orlando <- paste0(path, "Orlando Magic.png")
philadelphia <- paste0(path, "Philadelphia 76ers.png")
phoenix <- paste0(path, "Phoenix Suns.png")
portland <- paste0(path, "Portland Trail Blazers.png")
sacramento <- paste0(path, "Sacramento Kings.png")
sanantonio <- paste0(path, "San Antonio Spurs.png")
toronto <- paste0(path, "Toronto Raptors.png")
utah <- paste0(path, "Utah Jazz.png")
washington <- paste0(path, "Washington Wizards.png")

images <- data.frame(Team = c("Atlanta Hawks", 
                              "Boston Celtics", 
                              "Brooklyn Nets", 
                              "Charlotte Hornets",
                              "Chicago Bulls",
                              "Cleveland Cavaliers", 
                              "Dallas Mavericks",
                              "Denver Nuggets",
                              "Detroit Pistons",
                              "Golden State Warriors",
                              "Houston Rockets",
                              "Indiana Pacers",
                              "Los Angeles Clippers",
                              "Los Angeles Lakers",
                              "Memphis Grizzlies",
                              "Miami Heat",
                              "Milwaukee Bucks",
                              "Minnesota Timberwolves",
                              "New Orleans Pelicans",
                              "New York Knicks",
                              "Oklahoma City Thunder",
                              "Orlando Magic",
                              "Philadelphia 76ers",
                              "Phoenix Suns",
                              "Portland Trail Blazers",
                              "Sacramento Kings",
                              "San Antonio Spurs",
                              "Toronto Raptors",
                              "Utah Jazz",
                              "Washington Wizards"),
                     ImagePath = c(atlanta, 
                                   boston,
                                   brooklyn,
                                   charlotte,
                                   chicago,
                                   cleveland,
                                   dallas,
                                   denver,
                                   detroit,
                                   goldenstate,
                                   houston,
                                   indiana,
                                   clippers,
                                   lakers,
                                   memphis,
                                   miami,
                                   milwaukee,
                                   minnesota,
                                   neworleans,
                                   newyork,
                                   okc,
                                   orlando,
                                   philadelphia,
                                   phoenix,
                                   portland,
                                   sacramento,
                                   sanantonio,
                                   toronto,
                                   utah,
                                   washington))


nba_ratings <- nba_records %>%
  select(c("Team", "ORTG", "DRTG")) %>%
  left_join(images, by = c("Team"))

avg_ORtg <- mean(nba_ratings$ORTG)
avg_DRtg <- mean(nba_ratings$DRTG)

scatter2 <- ggplot(data = nba_ratings) +
  geom_image(aes(x = ORTG, y = DRTG, image = ImagePath), 
             size = 0.05,
             asp = 1.618) +
  geom_hline(yintercept = avg_DRtg) +
  geom_vline(xintercept = avg_ORtg) +
  labs(x = "Offensive Rating", y = "Defensive Rating",
       title = "NBA Team ORTG vs DRTG From the Last 10 Games",
       subtitles = "Horizontal Line is Average DRTG | Vertical Line is Average ORTG") +
  geom_text(x = 120, y = 105, label = "Good Offense, Good Defense") +
  geom_text(x = 108, y = 122, label = "Bad Offense, Bad Defense") +
  geom_text(x = 108, y = 105, label = "Bad Offense, Good Defense") +
  geom_text(x = 120, y = 122, label = "Good Offense, Bad Defense")
  
scatter2
ggsave("scatter12.4.png", width = 10, height = 8)

# scatter1 <- ggplot(data = nba_totals) +
#   geom_abline(intercept = 0, slope = 1, color = "blue") +
#   xlim(0, 1) +
#   ylim(0, 1) +
#   geom_image(aes(x = ActualWL, y = PythWL, image = ImagePath), 
#              size = 0.05,
#              asp = 1.618) +
#   labs(x = "Current Win/Loss", y = "Pythagorean Win/Loss",
#        title = "22-23 NBA Actual WL vs Pythagorean WL",
#        subtitle = "Data as of 10/30/22 | Blue Line indicates where Actual WL matches Pythagorean WL", 
#        caption = "Data from Basketball Reference | Twitter: @ryank_08 | Instagram: @ryan8kyaw") +
#   geom_text(x = 0.1, y = 0.9, label = "Underachievers") +
#   geom_text(x = 0.9, y = 0.1, label = "Overachievers")
# scatter1
# ggsave("scatter.png", width = 10, height = 8)

