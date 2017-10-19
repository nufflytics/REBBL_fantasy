
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tidyverse)
library(DT)
library(lubridate)
library(hrbrthemes)

start_time = lubridate::dmy_hm("051017 0000", tz = "EST")

theme_fantasy <- function() {
  theme_ipsum_rc(base_size = 12, axis_title_just = "m", axis_title_size = 14, grid = "Yy") + theme(legend.position = "bottom")
}


shinyServer(function(input, output, session) {
  #Setup ------
  session$allowReconnect(TRUE)
  teams <- read_csv("data/fantasy_teams.csv") %>% mutate(Race = stringr::str_replace_all(Race, "_"," "))
  stats <- read_csv("data/player_stats.csv") %>% filter(!Type %in% c("Star Player", "Unknown playertype"))
  costs <- read_csv("data/costs.csv")
  regions <- read_csv("data/regions.csv")
  
  stats <- stats %>% left_join(regions) %>% left_join(costs) %>% mutate(`Total Cost` = Cost + (Level-1)*10)
  
  points = left_join(teams, stats, by=c("Player" = "Name", "Team", "Race", "Type","Round"))
  
  
  #General data -----
  gameweek = difftime(now("UTC"), start_time, units = "weeks") %>% ceiling()
  
  observeEvent(
    input$tabs,
    updateNumericInput(session, "selected_round", value = as.numeric(gameweek)),
    once = T
  )
  
  #Leaderboard tab ------
  
  weekly_points <- points %>% 
    group_by(Coach, Round) %>% 
    mutate(
      FP = ifelse( Special %in% c("c"), 2*FP, FP), # Double captain's points
      FP = ifelse( Special %in% c("r") & max(row_number() > 11) & !is.na(FP), 0, FP) # Remove reserve's points if more than 11 points recorded
    )  %>% 
    select(Coach,Round,Player,FP,`Total Cost`) %>% 
    summarise(Tot_points = sum(FP, na.rm=T), played = sum(!is.na(FP)), cost = sum(`Total Cost`, na.rm=T), team_size = n()) %>% 
    mutate(PCR = Tot_points*10/cost)
  
  leaders <- weekly_points %>% 
    summarise(`Scoring players this gameweek` = paste0(played[gameweek],"/",team_size[gameweek]), `Team Efficiency` = sum(PCR, na.rm=T), Points = sum(Tot_points)) %>% 
    arrange(desc(Points))
  
  output$leaderboard <- DT::renderDataTable(
    DT::datatable(leaders,
    options = list(
      pageLength = 100,
      dom = 't',
      class = 'compact'
    ),
    rownames = FALSE,
    selection = list(mode = "multiple", selected = 1:4)
  ) %>% DT::formatRound(3)
  )
  
  output$weekly_bars <- renderPlot({
    validate(need(input$leaderboard_rows_selected, message = F))
    
    coaches = leaders[input$leaderboard_rows_selected,]$Coach
    weekly_points %>% 
      ungroup %>% 
      filter(Coach %in% coaches, played > 0) %>% 
      mutate(Coach = factor(Coach, levels=coaches)) %>% 
      ggplot(aes(x=Round, y = Tot_points, fill = Coach)) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_fill_brewer(palette="Paired") +
      ylab("Points") +
      scale_x_continuous(breaks = 1:max(weekly_points$Round)) +
      theme_fantasy() +
      ggtitle("Points per week")
  })
  
  output$weekly_lines <- renderPlot({
    validate(need(input$leaderboard_rows_selected, message = F))
    
    coaches = leaders[input$leaderboard_rows_selected,]$Coach
    weekly_points %>% 
      mutate(cum_points = cumsum(Tot_points)) %>% 
      ungroup %>% 
      filter(Coach %in% coaches, played > 0) %>% 
      mutate(Coach = factor(Coach, levels=coaches)) %>% 
      ggplot(aes(x=Round, y = cum_points, colour = Coach)) +
      geom_point(size = 2, alpha = 0.8) +
      geom_line(size = 1.2, alpha = 0.8) +
      scale_colour_brewer(palette="Paired") +
      ylab("Points") +
      scale_y_continuous(limits = c(0,NA)) +
      scale_x_continuous(breaks = 1:max(weekly_points$Round)) +
      theme_fantasy() +
      ggtitle("Cumulative points")
  })
  
  #Teams tab ------
  
  output$coach_select <- renderUI({
    selectizeInput("selected_coach", NULL, teams$Coach %>% unique %>% sort)
  })
  
  output$team_name <- renderUI(h3(filter(teams,Coach == input$selected_coach) %>% .$FTeam %>% unique))
  
  team_table <- reactive({points %>% 
      filter(Round == input$selected_round) %>% 
      filter(Coach == input$selected_coach) %>% 
      select(Special,Player:Type,Cost = `Total Cost`, Points = FP) %>% 
      mutate(Special = ifelse(
        Special == "c", 
        "<i class='fa fa-copyright' title='Captain'></i>",
        ifelse(
          Special=="r", 
          "<i class='fa fa-registered' title='Reserve'></i>", 
          NA
        )
      ),
      `Player efficiency` = Points*10/Cost 
      )
  })
  output$team_summary <- DT::renderDataTable(
    DT::datatable(team_table(),
    options = list(
      pageLength = 100,
      dom = 't',
      class = 'compact'
    ),
    rownames = FALSE,
    colnames = c(" " = "Special"),
    escape = FALSE,
    selection = "single"
  ) %>% DT::formatRound(8)
  )
  
  
  #Stats tab ---------
  
  #data setup
  summarised_stats <- stats %>%
    group_by(Region, Name, Team, Race, Type, playerID) %>% 
    summarise(`Current Cost` = last(`Total Cost`), Games = n(), Points = mean(FP), BLK = mean(BLK), AVBr = mean(AVBr), KO = mean(KO), CAS = mean(CAS), Kills = mean(Kills), TD = mean(TD), Pass = mean(Pass), `Pass(m)` = mean(Pass_m), Catch = mean(Catch), Int = mean(Int), `Carry(m)` = mean(Carry_m), Surf = mean(Surf)) %>% 
    arrange(desc(Points))
  
  sorted_stats <- stats %>% arrange(desc(FP))
  
  #reactive values
  stats_selected_player <- reactive({
    validate(need(input$stats_tab, message = F))
    switch(
      input$stats_tab,
      "Averaged" = summarised_stats[input$averaged_stats_table_rows_selected,],
      "All" = sorted_stats[input$stats_table_rows_selected,],
      NULL
    )
  })
  
  stats_selected_current_tab <- reactive({
    validate(need(input$stats_tab, message = F))
    switch(
      input$stats_tab,
      "Averaged" = length(input$averaged_stats_table_rows_selected)>0,
      "All" = length(input$stats_table_rows_selected)>0,
      NULL
    )
  })
  
  output$averaged_stats_table <- DT::renderDataTable(
    DT::datatable(summarised_stats %>% select(-playerID),
                  extensions = "Scroller",
                  options = list(
                    dom = 'tip',
                    scrollX = TRUE,
                    scrollY = 300,
                    scroller= list(loadingIndicator = TRUE)
                  ),
                  selection = "single",
                  filter = "top",
                  rownames = F
    ) %>% DT::formatRound(8:19)
  )
  
  output$stats_table <- DT::renderDataTable(
    DT::datatable(sorted_stats %>% 
                    select(Region, Name, Team, Race, Type, Round, Cost = `Total Cost`, Points = "FP", BLK, AVBr, KO, CAS, Kills, TD, Pass, `Pass(m)`= "Pass_m", Catch, Int, `Carry(m)` = Carry_m, Surf),
                  extensions = "Scroller",
                  options = list(
                    dom = 'tip',
                    scrollX = TRUE,
                    scrollY = 300,
                    scroller= list(loadingIndicator = TRUE)
                  ),
                  selection = "single",
                  filter = "top",
                  rownames = F
    )
  )
  
  output$best_game_stats <- renderInfoBox({
    validate(
      need(stats_selected_player(), message = F),
      need(stats_selected_current_tab(), message = F)
    )
    
    best_match = stats %>%  
      filter(playerID == stats_selected_player()$playerID, Name == stats_selected_player()$Name) %>% 
      filter(FP == max(FP)) # separate filter to not filter on global max points
    
    infoBox(
      title = "Best Match",
      value = best_match$FP,
      subtitle = paste0("Against ", best_match$Opponent), #CHANGE TO OPP_TEAM WHEN USING REAL DATA
      icon = icon("thumbs-up"),
      color = "green"
    )
  })
  
  output$points_bar_stats <- renderPlot({
    validate(
      need(stats_selected_player(), message = F),
      need(stats_selected_current_tab(), message = F)
    )
    
    #Add data for weeks where the player didn't play
    player_points <- stats %>%  
      filter(playerID == stats_selected_player()$playerID, Name == stats_selected_player()$Name) %>%
      arrange(Round) %>% 
      right_join(data.frame(Round = 1:max(.$Round))) %>% 
      replace_na(replace = list(Opponent = "DNP", FP = 0))
    
    player_points %>% 
      ggplot(aes(x=Round,y=FP)) +
      geom_bar(stat="identity") +
      theme_fantasy() +
      ggtitle(stats_selected_player()$Name) +
      scale_x_continuous(breaks = 1:max(player_points$Round), labels = player_points$Opponent) +
      ylab("Points") +
      xlab("Opponent") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$worst_game_stats <- renderInfoBox({
    validate(
      need(stats_selected_player(), message = F),
      need(stats_selected_current_tab(), message = F)
    )
    
    worst_match = stats %>%  
      filter(playerID == stats_selected_player()$playerID, Name == stats_selected_player()$Name) %>% 
      filter(FP == min(FP)) 
    
    infoBox(
      title = "Worst Match",
      value = worst_match$FP,
      subtitle = paste0("Against ", worst_match$Opponent), #CHANGE TO OPP_TEAM WHEN USING REAL DATA
      icon = icon("thumbs-down"),
      color="red"
    )
  })
  
  #output$debug = renderText(c(stats_selected_player()$Name, stats_selected_player()$playerID,input$stats_tab, stats_selected_current_tab()))
})
