library(tidyverse)
library(lubridate)
################################################################################
##This code written by Prashanth Iyer who can be found on twitter          #####
##@iyer_prashanth and is a really good follow                              #####
################################################################################
fun.draw_rink <- function() {
    
    
    
    xseq <- seq(-4, 4, length = 100)
    theta1 <- seq(0, 2 * pi, length = 300)
    theta <- seq(0, 2 * pi, length = 300)
    dd <- (5 + 7 / 12) / 2
    
    ## Blank NHL Rink
    
    rink <- ggplot(data = data.frame(x = 1, y = 1), aes(x, y)) + 
        
        geom_path(data = data.frame(
            x = c(15, 87 + 13 * sin(seq(0, pi / 2, length = 20)), 
                  87 + 13 * sin(seq(pi / 2, 0, length = 20)), 15), 
            y = c(-42.5, -42.5 + 15 - 15 * cos(seq(0, pi / 2, length = 20)), 
                  42.5 - 15 + 15 * cos(seq(pi / 2, 0, length = 20)), 42.5))) + 
        geom_path(data = data.frame(
            x = c(15, -87 - 13 * sin(seq(0, pi / 2, length = 20)), 
                  -87 - 13 * sin(seq(pi / 2, 0, length = 20)), 15), 
            y = c(-42.5, -42.5 + 15 - 15 * cos(seq(0, pi / 2, length = 20)), 
                  42.5 - 15 + 15 * cos(seq(pi / 2, 0, length = 20)), 42.5))) + 
        ## Goal Lines
        geom_path(data = data.frame(x = c(89),
                                    y = c(42.5 - 15 + sqrt(15^2 - (15 - 11)^2), 
                                          -(42.5 - 15 + sqrt(15^2 - (15 - 11)^2)))), 
                  color = 'red') + 
        geom_path(data = data.frame(x = c(-89), 
                                    y = c(42.5 - 15 + sqrt(15^2 - (15 - 11)^2), 
                                          -(42.5 - 15 + sqrt(15^2 - (15 - 11)^2)))), 
                  color = 'red') +
        ## Nets
        geom_path(data = data.frame(x = c(90, 92, 92, 90)), y = c(-3, -3, 3, 3)) + 
        geom_path(data = data.frame(x = c(-90, -92, -92, -90), y = c(-3,-3, 3, 3))) +
        
        ## Restricted Area
        geom_segment(aes(x = 89, y = -11, xend = 100, yend = -14), color = 'red') + 
        geom_segment(aes(x = 89, y = 11, xend = 100, yend = 14), color = 'red') + 
        geom_segment(aes(x = -89, y = -11, xend = -100, yend = -14), color = 'red') + 
        geom_segment(aes(x = -89, y = 11, xend =-100, yend = 14), color = 'red') +
        
        ## Red Line (Center Ice)
        geom_segment(aes(x = 0, y = -42.5, xend = 0, yend = 42.5), color = 'red', size = 1) +
        
        ## Blue Lines
        geom_segment(aes(x = 25, y = -42.5, xend = 25,  yend = 42.5), color = 'blue', size = 1) + 
        geom_segment(aes(x = -25, y = -42.5, xend = -25,  yend = 42.5), color = 'blue', size = 1) +
        
        ## Crease
        geom_polygon(data = data.frame(x = 1 * c(89, 83+xseq^2 / 4^2 * 1.5, 89),
                                       y = c(-4, xseq, 4)), 
                     color = 'red', fill = 'deepskyblue2') + 
        geom_polygon(data = data.frame(x = -1 * c(89, 83 + xseq^2 / 4^2 * 1.5, 89),
                                       y = c(-4, xseq, 4)), 
                     color = 'red', fill = 'deepskyblue2') +
        
        ## Center Ice Circle
        geom_path(data = data.frame(x = 15 * sin(theta1)), 
                  y = 15 * cos(theta1), color = 'deepskyblue2') +
        
        ## Faceoff Dots
        geom_polygon(data = data.frame(y = 22 + 1 * cos(theta), 
                                       x = 20 + 1 * sin(theta)), 
                     color = "red", fill = "red") + 
        geom_polygon(data = data.frame(y = 22 + 1 * cos(theta), 
                                       x = -20 + 1 * sin(theta)), 
                     color = "red", fill = 'red') + 
        geom_polygon(data = data.frame(y = -22 + 1 * cos(theta), 
                                       x = -20 + 1 * sin(theta)), 
                     color = 'red', fill = 'red') + 
        geom_polygon(data = data.frame(y = -22 + 1 * cos(theta), 
                                       x = 20 + 1 * sin(theta)), 
                     color = 'red', fill = 'red') + 
        geom_polygon(data = data.frame(y = 22 + 1 * cos(theta), 
                                       x = -69 + 1 * sin(theta)), 
                     color = 'red', fill = 'red') + 
        geom_polygon(data = data.frame(y = 22 + 1 * cos(theta), 
                                       x = 69 + 1 * sin(theta)), 
                     color = 'red', fill = 'red') + 
        geom_polygon(data = data.frame(y = -22 + 1 * cos(theta), 
                                       x = -69 + 1 * sin(theta)), 
                     color = 'red', fill = 'red') + 
        geom_polygon(data = data.frame(y = -22 + 1 * cos(theta), 
                                       x = 69 + 1 * sin(theta)), 
                     color = 'red', fill = 'red') +
        
        ## Faceoff Circles
        geom_segment(aes(y = 22 - 0.75, x = 69 - 2, 
                         yend = 22 - 0.75, xend = 69 - 6), color = 'red') + 
        geom_segment(aes(y = 22 + 0.75, x = 69 - 2, 
                         yend = 22 + 0.75, xend = 69 - 6), color = 'red') + 
        geom_segment(aes(y = 22 + 0.75, x = 69 + 2, 
                         yend = 22 + 0.75, xend = 69 + 6), color= 'red') + 
        geom_segment(aes(y = 22 - 0.75, x = 69 - 2, 
                         yend = 22 - 0.75, xend = 69 - 6), color = 'red') + 
        geom_segment(aes(y = -22 + 0.75, x = 69 - 2, 
                         yend = -22 + 0.75, xend = 69 - 6), color= 'red') + 
        geom_segment(aes(y = -22 + 0.75, x = 69 + 2, 
                         yend = -22 + 0.75, xend = 69 + 6), color= 'red') + 
        geom_segment(aes(y = -22 - 0.75, x = 69 - 2, 
                         yend = -22 - 0.75, xend = 69 - 6), color = 'red') + 
        geom_segment(aes(y = -22 - 0.75, x = 69 + 2, 
                         yend = -22 - 0.75, xend = 69 + 6), color = 'red') + 
        geom_segment(aes(y = 22 - 0.75, x = 69 + 2, 
                         yend = 22 - 0.75, xend = 69 + 6), color = 'red') + 
        geom_segment(aes(y = 22 + 0.75, x = -69 - 2, 
                         yend = 22 + 0.75, xend = -69 - 6), color = 'red') + 
        geom_segment(aes(y = 22 - 0.75, x = -69 - 2, 
                         yend = 22 - 0.75, xend = -69 - 6), color = 'red') + 
        geom_segment(aes(y = 22 + 0.75, x = -69 + 2, 
                         yend = 22 + 0.75, xend = -69 + 6), color = 'red') + 
        geom_segment(aes(y = -22 + 0.75, x = -69 - 2, 
                         yend = -22 + 0.75, xend = -69 - 6), color = 'red') + 
        geom_segment(aes(y = 22 - 0.75, x = -69 + 2, 
                         yend = 22 - 0.75, xend = -69 + 6), color = 'red') + 
        geom_segment(aes(y = -22 + 0.75, x = -69 + 2, 
                         yend = -22 + 0.75, xend = -69 + 6), color= 'red') + 
        geom_segment(aes(y = -22 - 0.75, x = -69 - 2, 
                         yend = -22 - 0.75, xend = -69 - 6), color = 'red') + 
        geom_segment(aes(y = -22 - 0.75, x = -69 + 2, 
                         yend = -22 - 0.75, xend = -69 + 6), color = 'red') + 
        geom_segment(aes(y = 22 - 15, x = 69 - dd, 
                         yend = 22 - 17, xend = 69 - dd), color = 'red') + 
        geom_segment(aes(y = 22 - 15, x = 69 + dd, 
                         yend = 22 - 17, xend = 69 + dd), color = 'red') + 
        geom_segment(aes(y = 22 + 15, x = 69 + dd, 
                         yend = 22+17, xend = 69 + dd), color = 'red') + 
        geom_segment(aes(y = 22 + 15, x = 69 - dd, 
                         yend = 22 + 17, xend = 69 - dd), color = 'red') + 
        geom_segment(aes(y = -22 + 15, x = 69 - dd, 
                         yend = -22 + 17, xend = 69 - dd), color = 'red') + 
        geom_segment(aes(y = -22 + 15, x = 69 + dd, 
                         yend = -22 + 17, xend = 69 + dd), color = 'red') + 
        geom_segment(aes(y = -22 - 15, x = 69 - dd, 
                         yend = -22 - 17, xend = 69 - dd), color= 'red') + 
        geom_segment(aes(y = -22 - 15, x = 69 + dd, 
                         yend = -22 - 17, xend = 69 + dd), color = 'red') + 
        geom_segment(aes(y = -22 + 15, x = -69 + dd, 
                         yend = -22 + 17, xend = -69 + dd), color = 'red') + 
        geom_segment(aes(y = -22 - 15, x = -69 - dd, 
                         yend = -22 - 17, xend = -69 - dd), color = 'red') + 
        geom_segment(aes(y = -22 - 15, x = -69 + dd, 
                         yend = -22 - 17, xend = -69 + dd), color = 'red') + 
        geom_segment(aes(y = -22 + 15, x = -69 - dd, 
                         yend = -22 + 17, xend = -69 - dd), color = 'red') + 
        geom_segment(aes(y = 22 - 15, x = -69 + dd, 
                         yend = 22 - 17, xend = -69 + dd), color = 'red') + 
        geom_segment(aes(y = 22 - 15, x = -69 - dd, 
                         yend = 22 - 17, xend = -69 - dd), color = 'red') + 
        geom_segment(aes(y = 22 + 15, x = -69 - dd, 
                         yend = 22 + 17, xend = -69 - dd), color = 'red') + 
        geom_segment(aes(y = 22 + 15, x = -69 + dd, 
                         yend = 22 + 17, xend = -69 + dd), color = 'red') + 
        geom_segment(aes(y = 22 + 0.75, x = 69 + 2, 
                         yend = 22 + 3.75, xend = 69 + 2), color = 'red') + 
        geom_segment(aes(y = 22 + 0.75, x = 69 - 2, 
                         yend = 22 + 3.75, xend = 69 - 2), color = 'red') + 
        geom_segment(aes(y = 22 - 0.75, x = 69 + 2, 
                         yend = 22 - 3.75, xend = 69 + 2), color = 'red') + 
        geom_segment(aes(y = 22 - 0.75, x = 69 - 2, 
                         yend = 22 - 3.75, xend = 69 - 2), color = 'red') + 
        geom_segment(aes(y = 22 + 0.75, x = -69 + 2, 
                         yend = 22 + 3.75, xend = -69 + 2), color = 'red') + 
        geom_segment(aes(y = 22 + 0.75, x = -69 - 2, 
                         yend = 22 + 3.75, xend = -69 - 2), color = 'red') + 
        geom_segment(aes(y = 22 - 0.75, x = -69 + 2, 
                         yend = 22 - 3.75, xend = -69 + 2), color = 'red') + 
        geom_segment(aes(y = 22 - 0.75, x = -69 - 2, 
                         yend = 22 - 3.75, xend = -69 - 2), color = 'red') + 
        geom_segment(aes(y = -22 - 0.75, x = -69 + 2, 
                         yend = -22 - 3.75, xend = -69 + 2), color = 'red') + 
        geom_segment(aes(y = -22 - 0.75, x = -69 - 2, 
                         yend = -22 - 3.75, xend = -69 - 2), color = 'red') + 
        geom_segment(aes(y = -22 + 0.75, x = -69 + 2, 
                         yend = -22 + 3.75, xend = -69 + 2), color = 'red') + 
        geom_segment(aes(y = -22 + 0.75, x = -69 - 2, 
                         yend = -22 + 3.75, xend = -69 - 2), color = 'red') + 
        geom_segment(aes(y = -22 + 0.75, x = 69 + 2, 
                         yend = -22 + 3.75, xend = 69 + 2), color = 'red') + 
        geom_segment(aes(y = -22 - 0.75, x = 69 - 2, 
                         yend = -22 - 3.75, xend = 69 - 2), color = 'red') + 
        geom_segment(aes(y = -22 + 0.75, x = 69 - 2, 
                         yend = -22 + 3.75, xend = 69 - 2), color = 'red') + 
        geom_segment(aes(y = -22 - 0.75, x = 69 + 2, 
                         yend = -22 - 3.75, xend = 69 + 2), color = 'red') + 
        geom_path(data = data.frame(y = 22 + 15 * cos(theta), 
                                    x = 69 + 15 * sin(theta)), color = 'red') + 
        geom_path(data = data.frame(y = 22 + 15 * cos(theta), 
                                    x = -69 + 15 * sin(theta)), color = 'red') + 
        geom_path(data = data.frame(y = -22 + 15 * cos(theta), 
                                    x = -69 + 15 * sin(theta)), color = 'red') + 
        geom_path(data = data.frame(y = -22 + 15 * cos(theta), 
                                    x = 69 + 15 * sin(theta)), color = 'red') + 
        
        theme_void()
}

rink <- fun.draw_rink() + coord_fixed()
#convert player ids to actual player names from the player dataframe
convert_ids <- function(column, player_df){
    column <- player_df[match(column, player_df$playerId, nomatch = column), 
                        c('fullName')]
}

file_name <- c('~/HockeyStuff/Tape2TapeData/11_10_plays.csv')
players_file_name <- c('~/HockeyStuff/Tape2TapeData/11_10_roster.csv')

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

pbp_graph <- subset(pbp, pbp$event %in% c('Shot'))
index <- c(1:nrow(pbp))
for (number in index){
    pbp_graph <- pbp[number,]
    
    
    plot <-rink + geom_segment(aes(x = x0, y = y0, xend = x1, yend = y1, 
                                   color = eventTeam), 
                               arrow = arrow(length = unit(0.1,"cm")), data = pbp_graph) +
        geom_segment(aes(x = pass0x1, y = pass0y1, xend = x0, yend = y0, 
                         color = eventTeam), linetype = 2,
                     arrow = arrow(length = unit(0.1,"cm")), data = pbp_graph) +
        geom_segment(aes(x = pass1x1, y = pass1y1, xend = pass0x0, yend = pass0y0, 
                         color = eventTeam), linetype = 2,
                     arrow = arrow(length = unit(0.1,"cm")), data = pbp_graph) +
        geom_segment(aes(x = pass1x0, y = pass1y0, xend = pass2x1, yend = pass2y1, 
                         color = eventTeam), linetype = 2,
                     arrow = arrow(length = unit(0.1,"cm")), data = pbp_graph) +
        geom_segment(aes(x = pass2x0, y = pass2y0, xend = pass3x1, yend = pass3y1, 
                         color = eventTeam), linetype = 2,
                     arrow = arrow(length = unit(0.1,"cm")), data = pbp_graph) +
        geom_segment(aes(x = pass3x0, y = pass3y0, xend = pass4x1, yend = pass4y1, 
                         color = eventTeam), linetype = 2,
                     arrow = arrow(length = unit(0.1,"cm")), data = pbp_graph) +
        geom_segment(aes(x = pass0x0, y = pass0y0, xend = pass0x1, yend = pass0y1, 
                         color = eventTeam), 
                     arrow = arrow(length = unit(0.1,"cm")), data = pbp_graph) +
        geom_segment(aes(x = pass1x0, y = pass1y0, xend = pass1x1, yend = pass1y1, 
                         color = eventTeam), 
                     arrow = arrow(length = unit(0.1,"cm")), data = pbp_graph) +
        geom_segment(aes(x = pass2x0, y = pass2y0, xend = pass2x1, yend = pass2y1, 
                         color = eventTeam), 
                     arrow = arrow(length = unit(0.1,"cm")), data = pbp_graph) +
        geom_segment(aes(x = pass3x0, y = pass3y0, xend = pass3x1, yend = pass3y1, 
                         color = eventTeam), 
                     arrow = arrow(length = unit(0.1,"cm")), data = pbp_graph) +
        geom_segment(aes(x = pass4x0, y = pass4y0, xend = pass4x1, yend = pass4y1, 
                         color = eventTeam), 
                     arrow = arrow(length = unit(0.1,"cm")), data = pbp_graph) 
    ggsave(paste0('plot', as.character(number), '.png'), plot = plot, height = 4)
    
}