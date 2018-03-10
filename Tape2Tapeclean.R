library(tidyverse)

file_name <- c('')

pbp <- read_csv(file_name)

#Fix game time and turn it into integer for seconds
pbp$periodTime <- as.character(pbp$periodTime)
pbp$periodTime <- substr(pbp$periodTime,1,5)
pbp$periodTime <- paste0('00:', pbp$periodTime)
pbp$periodTime <- hms(pbp$periodTime)
pbp$periodTime <- as.numeric(pbp$periodTime)

#convert player ids to actual player names from the player dataframe
convert_ids <- function(column, player_df){
    column <- player_df[match(column, player_df$playerId, nomatch = column), 
                        c('fullName')]
}

pbp[4:15] <- sapply(pbp[4:15], convert_ids, player_df = players)

pbp$awayPlayer0Id <- players[match(pbp$awayPlayer0Id, players$playerId), 4]