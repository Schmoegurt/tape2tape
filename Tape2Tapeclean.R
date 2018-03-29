library(tidyverse)
library(lubridate)
################################################################################
##This code written by Prashanth Iyer who can be found on twitter          #####
##@iyer_prashanth and is a really good follow                              #####
################################################################################
source('~/graphautomation/RinkFunction.R')
rink <- fun.draw_rink() + coord_fixed()
#convert player ids to actual player names from the player dataframe
convert_ids <- function(column, player_df){
    column <- player_df[match(column, player_df$playerId, nomatch = column), 
                        c('fullName')]
}

file_name <- c('~/HockeyStuff/Tape2TapeData/Tape2Tape/11_10_plays.csv')
players_file_name <- c('~/HockeyStuff/Tape2TapeData/Tape2Tape/11_10_roster.csv')

pbp <- read_csv(file_name)
players <- read_csv(players_file_name)

#Fix game time and turn it into integer for seconds
pbp$periodTime <- as.character(pbp$periodTime)
pbp$periodTime <- substr(pbp$periodTime,1,5)
pbp$periodTime <- paste0('00:', pbp$periodTime)
pbp$periodTime <- hms(pbp$periodTime)
pbp$periodTime <- as.numeric(pbp$periodTime)

#convert periodTime to a cumsum of total seconds elapsed
pbp$periodTime <- ifelse(pbp$period == 2, pbp$periodTime + 1200, pbp$periodTime)
pbp$periodTime <- ifelse(pbp$period == 3, pbp$periodTime + 2400, pbp$periodTime)
pbp$periodTime <- ifelse(pbp$period == 4, pbp$periodTime + 3600, pbp$periodTime)

#converting playerids to playernames
pbp[4:15] <- pbp %>% select(awayPlayer0Id:homePlayer5Id) %>%
    sapply(convert_ids, player_df = players)

pbp[,c('player0Id', 'player1Id', 'pass0player0Id', 'pass0player1Id', 
       'pass1player0Id', 'pass1player1Id', 'pass2player0Id', 'pass2player1Id',
       'pass3player0Id', 'pass3player1Id', 
       'pass4player0Id', 'pass4player1Id')] <- 
    pbp[,c('player0Id', 'player1Id', 'pass0player0Id', 'pass0player1Id', 
           'pass1player1Id', 'pass1player1Id', 'pass2player0Id', 'pass2player1Id',
           'pass3player0Id', 'pass3player1Id', 'pass4player0Id', 
           'pass4player1Id')] %>% sapply(convert_ids, player_df = players)

team_colors <- c('black', 'darkred', 'black', 'royalblue4', 'red3', 'firebrick1',
                 'black', 'navy', 'maroon', 'red', 'darkgreen', 'navyblue',
                 'orange', 'mediumblue', 'slategrey', 'limegreen', 'darkgreen',
                 'darkorange1', 'yellow2', 'mediumblue', 'steelblue4', 'red2',
                 'lightseagreen', 'darkorange1', 'dodgerblue4', 'gold', 'gold',
                 'dodgerblue3', 'navyblue', 'navyblue', 'red1')
names(team_colors) <- c('ANA', 'ARI','BOS', 'BUF', 'CGY', 'CAR', 'CHI', 'CBJ',
                        'COL', 'DET', 'DAL', 'FLA', 'EDM', 'MTL', 'L.A', 'N.J',
                        'MIN', 'NYI', 'NSH', 'NYR', 'STL', 'OTT', 'S.J', 'PHI',
                        'VAN', 'PIT', 'VGK', 'T.B', 'TOR', 'WPG', 'WSH')

pbp_graph <- subset(pbp, pbp$event %in% c('Shot'))
index <- c(1:nrow(pbp))
score <- c(0, 0)
names(score) <- c(unique(pbp$eventTeam)[1], unique(pbp$eventTeam)[2])
for (number in index){
    pbp_graph <- pbp[number,]
    if (pbp_graph$event == 'Goal') {
        score[pbp_graph$eventTeam] <- score[pbp_graph$eventTeam] + 1
    }
    
    
    plot <-rink +  geom_point(aes(x = x0, y = y0), 
                              color = team_colors[pbp_graph$eventTeam],
                              data = pbp_graph,
                              size = 4) + 
        geom_segment(aes(x = x0, y = y0, xend = x1, yend = y1 
                                   ), color = team_colors[pbp_graph$eventTeam],
                               arrow = arrow(length = unit(0.1,"cm")), data = pbp_graph) +
        geom_segment(aes(x = pass0x1, y = pass0y1, xend = x0, yend = y0), 
                         color = team_colors[pbp_graph$eventTeam], linetype = 2,
                     arrow = arrow(length = unit(0.1,"cm")), data = pbp_graph) +
        geom_segment(aes(x = pass1x1, y = pass1y1, xend = pass0x0, yend = pass0y0), 
                         color = team_colors[pbp_graph$eventTeam], linetype = 2,
                     arrow = arrow(length = unit(0.1,"cm")), data = pbp_graph) +
        geom_segment(aes(x = pass1x0, y = pass1y0, xend = pass2x1, yend = pass2y1), 
                         color = team_colors[pbp_graph$eventTeam], linetype = 2,
                     arrow = arrow(length = unit(0.1,"cm")), data = pbp_graph) +
        geom_segment(aes(x = pass2x0, y = pass2y0, xend = pass3x1, yend = pass3y1), 
                         color = team_colors[pbp_graph$eventTeam], linetype = 2,
                     arrow = arrow(length = unit(0.1,"cm")), data = pbp_graph) +
        geom_segment(aes(x = pass3x0, y = pass3y0, xend = pass4x1, yend = pass4y1), 
                         color = team_colors[pbp_graph$eventTeam], linetype = 2,
                     arrow = arrow(length = unit(0.1,"cm")), data = pbp_graph) +
        geom_segment(aes(x = pass0x0, y = pass0y0, xend = pass0x1, yend = pass0y1), 
                         color = team_colors[pbp_graph$eventTeam], 
                     arrow = arrow(length = unit(0.1,"cm")), data = pbp_graph) +
        geom_segment(aes(x = pass1x0, y = pass1y0, xend = pass1x1, yend = pass1y1), 
                         color = team_colors[pbp_graph$eventTeam], 
                     arrow = arrow(length = unit(0.1,"cm")), data = pbp_graph) +
        geom_segment(aes(x = pass2x0, y = pass2y0, xend = pass2x1, yend = pass2y1), 
                         color = team_colors[pbp_graph$eventTeam], 
                     arrow = arrow(length = unit(0.1,"cm")), data = pbp_graph) +
        geom_segment(aes(x = pass3x0, y = pass3y0, xend = pass3x1, yend = pass3y1), 
                         color = team_colors[pbp_graph$eventTeam], 
                     arrow = arrow(length = unit(0.1,"cm")), data = pbp_graph) +
        geom_segment(aes(x = pass4x0, y = pass4y0, xend = pass4x1, yend = pass4y1), 
                         color = team_colors[pbp_graph$eventTeam], 
                     arrow = arrow(length = unit(0.1,"cm")), data = pbp_graph)  + 
        labs(title = paste0(unique(pbp$eventTeam)[1], ' ',
                            score[[unique(pbp$eventTeam)[1]]],' ',
                            unique(pbp$eventTeam)[2], ' ',
                            score[[unique(pbp$eventTeam)[2]]], ' ',
                            pbp_graph$periodTimeRemaining),
            caption = paste0(pbp_graph$eventTeam, ' ', 
                              pbp_graph$event, ' ',
                              ifelse(!is.na(pbp_graph$eventType), 
                                     pbp_graph$eventType, ''),
                              ' ',
                              ifelse(!is.na(pbp_graph$eventResult), 
                                     pbp_graph$eventResult, '')))
    
    ggsave(paste0('00', as.character(number), '.png'), plot = plot, height = 6,
           width = 8)
}
