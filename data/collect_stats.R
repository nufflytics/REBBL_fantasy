suppressMessages(require(tidyverse))
suppressMessages(require(purrrlyr))
suppressMessages(require(magrittr))
suppressMessages(require(httr))
suppressMessages(require(rvest))
suppressMessages(require(stringr))
suppressMessages(require(nufflytics))

#load webhook info and API calls from file
load("api.Rda")
load("RFBBL_parameters.Rda")
last_game = read_file("last_game.uuid")

get_league_data <- function(league_response) {
  response_content <- content(league_response) 
  
  #Parse basic table information
  league_games <- response_content %>% 
    html_table %>% 
    extract2(1) %>% # Get first html table in response
    set_colnames(c("comp","round","h_coach","h_team","h_img","score","a_img","a_team","a_coach")) %>% 
    separate(score,c("h_score","a_score")) %>% 
    filter(a_coach != "Coach 2")
  
  if(nrow(league_games)==0) return(NULL) # No games, don't process further
  
  #Add uuids from the [data] attribute of html nodes
  league_games$uuid <- response_content %>% 
    html_nodes("[data]") %>% 
    html_attr("data") %>% 
    magrittr::extract(seq(1,length(.),by=10)) %>% # have the uuid listed 10 times per table row, so just take one
    str_replace_all("^1[012]","") # strip initial 1<platform_code> from uuid so unrecorded games have ID = 0
  
  # add numeric ID for easy comparison and remove concedes (a_score is NA after above processing)
  league_games %>% 
    mutate(ID = strtoi(uuid, base = 16)) #%>% 
  #filter(!is.na(a_score))
}

calc_FP <- function(player_result, own_race, own_team, opp_race, opp_team, round, uuid) {
  data_frame(
    match_uuid = uuid,
    Round = round,
    Team = own_team,
    Race = own_race,
    Name = player_result$playerData$name,
    Type = nufflytics::id_to_playertype(player_result$playerData$idPlayerType),
    Level = player_result$playerData$level,
    SPP_gain = player_result$xp,
    playerID = player_result$statistics$idPlayerListing,
    Opponent = opp_team,
    Opponent_Race = opp_race,
    BLK = player_result$statistics$inflictedTackles,
    AVBr = player_result$statistics$inflictedInjuries,
    KO = player_result$statistics$inflictedKO,
    CAS = player_result$statistics$inflictedCasualties,
    Kills = player_result$statistics$inflictedDead,
    TD = player_result$statistics$inflictedTouchdowns,
    Pass = player_result$statistics$inflictedPasses,
    Pass_m = player_result$statistics$inflictedMetersPassing * (Pass>0), # Odd bug where players could get pass yards without passing
    Catch = player_result$statistics$inflictedCatches,
    Int = player_result$statistics$inflictedInterceptions,
    Carry_m = player_result$statistics$inflictedMetersRunning,
    Surf = player_result$statistics$inflictedPushOuts,
    FP = ceiling(BLK/5)+ceiling(AVBr/2)+KO+CAS+(2*Kills)+(2*Surf)+(3*TD)+(2*Pass)+ceiling(Pass_m/20)+(2*Catch)+(5*Int)+ceiling(Carry_m/50)
  )
}

match_FP <- function(uuid, round) {
  stats = get_game_stats(uuid, "pc")
  
  if (stats$RowMatch$homeNbSupporters == 0 | stats$RowMatch$idMatchCompletionStatus != 0) return(NULL) 
  home = stats$MatchResultDetails$coachResults[[1]]$teamResult$playerResults
  away = stats$MatchResultDetails$coachResults[[2]]$teamResult$playerResults
  
  pmap_df(
    list(
      c(home,away),
      rep(c(nufflytics::id_to_race(stats$RowMatch$idRacesHome),nufflytics::id_to_race(stats$RowMatch$idRacesAway)), c(length(home),length(away))),
      rep(c(stats$RowMatch$teamHomeName,stats$RowMatch$teamAwayName), c(length(home),length(away))),
      rep(c(nufflytics::id_to_race(stats$RowMatch$idRacesAway),nufflytics::id_to_race(stats$RowMatch$idRacesHome)), c(length(home),length(away))),
      rep(c(stats$RowMatch$teamAwayName,stats$RowMatch$teamHomeName), c(length(home),length(away))),
      round,
      uuid
      ),
    calc_FP
    )
}

#Get data for leagues
league_html_response <- map2(league_search_strings, platform, api_query)

league_data <- map(league_html_response, ~ map_df(.,get_league_data)) %>% bind_rows(.id = "League")

new_games <- league_data %>% filter(ID> strtoi(last_game, base=16))

#Calculate fantasy stats

new_stats <- map2_df(new_games$uuid, new_games$round, match_FP)

#Write new stats and update last recorded game
write_csv(new_stats, "data/player_stats.csv", append = TRUE)

write_file(filter(league_data, ID == max(ID))$uuid,"data/last_game.uuid")
