library(StatsBombR)
library(tidyverse)
library(ggsoccer)
library(ggplot2)
comps <- FreeCompetitions()
worldcup <- comps[comps$competition_id == 43 & comps$season_id == 3, ] ## saving the data of FIFA 2014 World Cup to variable
worldcupmatches <- FreeMatches(worldcup)
worldcupevents <- free_allevents(MatchesDF = worldcupmatches) ## the free_allevents function needs match data passed
head(worldcupevents)
View(worldcupevents)
colnames(worldcupevents)
unique(worldcupevents$team.name) ## inspecting the data to see what I'm working with
worldcupevents <- worldcupevents %>% 
  mutate(
    location_x = map_dbl(location, ~ if(length(.) >= 1) .[[1]] else NA_real_),
    location_y = map_dbl(location, ~ if(length(.) >= 2) .[[2]] else NA_real_),
    pass_end_x = map_dbl(pass.end_location, ~ if(length(.) >= 1) .[[1]] else NA_real_),
    pass_end_y = map_dbl(pass.end_location, ~ if(length(.) >= 2) .[[2]] else NA_real_))  ## adding columns for pass location start and endpoints
sterling_passes <- worldcupevents %>% filter(type.name == "Pass", player.name == "Raheem Sterling") ## dataset for sterling pass map
ggplot(sterling_passes) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "gray70", fill = "white") +
  geom_segment(aes(x = location_x, y = location_y,
                   xend = pass_end_x, yend = pass_end_y),
               arrow = arrow(length = unit(0.1, "inches")),
               color = "red", alpha = 0.6) +
  coord_fixed() +
  labs(title = "Raheem Sterling - Pass Map") ## this was just a trial to see what I could do with the dataset
worldcuppasses <- worldcupevents %>% filter(type.name == "Pass") ## filtering out everything other than passes so I can make other maps
messivsronaldo <- worldcuppasses %>% filter(player.name == "Lionel Andrés Messi Cuccittini" | player.name == "Cristiano Ronaldo dos Santos Aveiro")
ggplot(messivsronaldo) + geom_segment(aes(x = location_x, y = location_y, xend = pass_end_x, yend = pass_end_y), arrow = arrow(length = unit(0.1, "inches")), color = "red", alpha = 0.6) + coord_fixed() + facet_wrap(~ player.name) ## another experiment to try two players pass maps against each other