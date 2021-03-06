suppressMessages(require(tidyverse))
suppressMessages(require(purrrlyr))
suppressMessages(require(magrittr))
suppressMessages(require(httr))
suppressMessages(require(rvest))
suppressMessages(require(stringr))
suppressMessages(require(nufflytics))
suppressMessages(require(RSQLite))
suppressMessages(require(dbplyr))

api_key <- readRDS("api.key")
last_game = read_file("last_game.uuid")
leagues <- c("REBBL - Big O", "REBBL - Gman", "REBBL - REL")

uuid_to_id <- function(uuid) {
  if(is.na(uuid)) return(0)
  uuid %>% str_sub(3) %>% as.hexmode() %>% as.integer()
}

get_contests <- function(league_name) {
  api_contests(api_key, league_name, status = "played", limit = 200)$upcoming_matches %>% 
    map_dfr(
      ~data.frame(uuid = .$match_uuid, id = .$match_id, round = .$round)
      ) %>% 
    filter(id > uuid_to_id(last_game))
}

matches <- map_dfr(leagues, get_contests)

calc_FP <- function(player_result, own_race, own_team, opp_race, opp_team, round, uuid, league, comp, result) {
  
  old_injuries <- map_chr(
    player_result$casualties_state_id[-match(player_result$casualties_sustained_id,player_result$casualties_state_id)], 
    ~map_chr(., ~nufflytics::id_to_casualty(.))
    )
  new_injuries <- map_chr(player_result$casualties_sustained_id, ~map_chr(., ~nufflytics::id_to_casualty(.)))
  skills <- player_result$skills
  
  #browser()
  
  data_frame(
    match_uuid = uuid,
    league = league,
    comp = comp,
    Round = round,
    Team = own_team,
    Race = own_race,
    Name = player_result$name,
    Type = player_result$type %>% stringr::str_replace_all(c(".*_(.*)" = "\\1", "([a-z])([A-Z])" = "\\1 \\2")),
    Level = player_result$level,
    SPP_gain = player_result$xp_gain,
    playerID = fill_nulls(player_result$id, 0),
    Opponent = opp_team,
    Opponent_Race = opp_race,
    BLK = player_result$stats$inflictedtackles,
    AVBr = player_result$stats$inflictedinjuries,
    KO = player_result$stats$inflictedko,
    CAS = player_result$stats$inflictedcasualties,
    Kills = player_result$stats$inflicteddead,
    TD = player_result$stats$inflictedtouchdowns,
    Pass = player_result$stats$inflictedpasses,
    Pass_m = player_result$stats$inflictedmeterspassing,
    Catch = player_result$stats$inflictedcatches,
    Int = player_result$stats$inflictedinterceptions,
    Carry_m = player_result$stats$inflictedmetersrunning,
    Surf = player_result$stats$inflictedpushouts,
    FP = ceiling(BLK/5)+ceiling(AVBr/2)+KO+CAS+(2*Kills)+(2*Surf)+(3*TD)+(2*Pass)+ceiling(Pass_m/20)+(2*Catch)+(5*Int)+ceiling(Carry_m/50),
    old_injuries = ifelse(is_empty(old_injuries), list(), list(old_injuries)),
    new_injuries = ifelse(is_empty(new_injuries), list(), list(new_injuries)),
    skills = ifelse(is_empty(skills), list(), list(skills)),
    result = result
  )
}

match_FP <- function(uuid, round = NULL) {
  match_data = api_match(api_key, uuid)
  
  if (is.null(round)) {round = match_data$match$round}
  
  if (match_data$match$teams[[1]]$nbsupporters == 0) return(NULL) 
  home = match_data$match$teams[[1]]$roster
  away = match_data$match$teams[[2]]$roster
  
  h_score <- match_data$match$teams[[1]]$score
  a_score <- match_data$match$teams[[2]]$score
  
  result <- case_when(
    h_score > a_score ~ c("W","L"),
    h_score == a_score ~ c("T","T"),
    h_score < a_score ~ c("L","W")
  )
  
  pmap_df(
    list(
      c(home,away),
      rep(c(nufflytics::id_to_race(match_data$match$teams[[1]]$idraces),nufflytics::id_to_race(match_data$match$teams[[2]]$idraces)), c(length(home),length(away))),
      rep(c(match_data$match$teams[[1]]$teamname,match_data$match$teams[[2]]$teamname), c(length(home),length(away))),
      rep(c(nufflytics::id_to_race(match_data$match$teams[[2]]$idraces),nufflytics::id_to_race(match_data$match$teams[[2]]$idraces)), c(length(home),length(away))),
      rep(c(match_data$match$teams[[2]]$teamname,match_data$match$teams[[1]]$teamname), c(length(home),length(away))),
      round,
      uuid,
      match_data$match$leaguename,
      match_data$match$competitionname,
      rep(result, c(length(home),length(away)))
      ),
    calc_FP
    )
}

#Get data for leagues
# league_html_response <- map2(league_search_strings, platform, api_query)

# league_data <- map(league_html_response, ~ map_df(.,get_league_data)) %>% bind_rows(.id = "League")

# new_games <- league_data %>% filter(ID> strtoi(last_game, base=16))

#Calculate fantasy stats

add_stats <- map2_df(matches$uuid, matches$round, match_FP)

#Write new stats and update last recorded game
stats <- read_rds("player_stats.rds")

new_stats <- bind_rows(stats,add_stats) 

if(nrow(add_stats) > 0) {
  #update uuid
  write_file(filter(matches, id == max(id))$uuid,"last_game.uuid")
  
  #update stats for site
  write_rds(new_stats %>% select(-result), path = "player_stats.rds")
  
  #update db for external
  con <- dbConnect(RSQLite::SQLite(), "../www/db/player_stats.db")
  copy_to(con, new_stats %>% select(-old_injuries, -new_injuries, -skills), "stats", overwrite = T, temporary = F)
  dbDisconnect(con)
  
}
