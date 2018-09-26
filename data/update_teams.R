library(tidyverse)

t <- read_csv("fantasy_teams.csv")

coaches <- t %>% group_by(Coach) %>% summarise(m=max(Round)) %>% filter(m != max(m)) %>% .$Coach

update_c <- function(coach) {
	filter(t, Coach == coach) %>% filter(Round == max(Round)) %>% mutate(Round = Round + 1) %>% write_csv("fantasy_teams.csv", append = T)
}

walk(coaches, update_c)
