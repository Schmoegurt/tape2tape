library(tidyverse)

#convert player ids to actual player names from the player dataframe
convert_ids <- function(column, player_df){
    column <- player_df[match(column, player_df$playerId, nomatch = column), 
                        c('fullName')]
}

file_name <- c('~/HockeyStuff/Tape2TapeData/11_10_plays.csv')
players_file_name <- c('~/HockeyStuff/Tape2TapeData/11_10_roster.csv')

pbp <- read_csv(file_name)

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

