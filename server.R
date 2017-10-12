
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tidyverse)
library(DT)
library(lubridate)
library(nufflytics)
extrafont::loadfonts(quiet = T)

start_time = lubridate::dmy_hm("051017 0000", tz = "EST")

teams <- read_csv("data/fantasy_teams.csv") %>% mutate(Race = stringr::str_replace_all(Race, "_"," "))

stats <- read_csv("data/player_stats.csv") %>% filter(!Type %in% c("Star Player", "Unknown playertype"))

points = left_join(teams, stats, by=c("Player" = "Name", "Team", "Race", "Type","Round"))



shinyServer(function(input, output, session) {
  session$allowReconnect(TRUE)
  
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
    filter(!is.na(FP)) %>% 
    mutate(
      FP = ifelse( Special %in% c("c"), 2*FP, FP), # Double captain's points
      FP = ifelse( Special %in% c("r") & max(row_number() > 11), 0, FP) # Remove reserve's points if more than 11 points recorded
      ) %>% 
    summarise(FP = sum(FP))
  
  leaders <- weekly_points %>% summarise(Points = sum(FP)) %>% arrange(desc(Points))
  
  output$leaderboard <- renderDataTable(
    leaders,
    options = list(
      pageLength = 100,
      dom = 't',
      class = 'compact',
      selected = "all"
    ),
    rownames = FALSE
  )
  
  output$weekly_bars <- renderPlot({
    validate(need(input$leaderboard_rows_selected, message = F))
    
    coaches = leaders[input$leaderboard_rows_selected,]$Coach
    weekly_points %>% 
      ungroup %>% 
      filter(Coach %in% coaches) %>% 
      mutate(Coach = factor(Coach, levels=coaches)) %>% 
      ggplot(aes(x=Round, y = FP, fill = Coach)) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_fill_brewer(palette="Paired") +
      ylab("Points") +
      scale_x_continuous(breaks = 1:max(weekly_points$Round)) +
      theme_nufflytics() +
      ggtitle("Points per week")
  })
  
  output$weekly_lines <- renderPlot({
    validate(need(input$leaderboard_rows_selected, message = F))
    
    coaches = leaders[input$leaderboard_rows_selected,]$Coach
    weekly_points %>% 
      mutate(cum_points = cumsum(FP)) %>% 
      ungroup %>% 
      filter(Coach %in% coaches) %>% 
      mutate(Coach = factor(Coach, levels=coaches)) %>% 
      ggplot(aes(x=Round, y = cum_points, colour = Coach)) +
      geom_point(size = 2, alpha = 0.8) +
      geom_line(size = 1.2, alpha = 0.8) +
      scale_colour_brewer(palette="Paired") +
      ylab("Points") +
      scale_y_continuous(limits = c(0,NA)) +
      scale_x_continuous(breaks = 1:max(weekly_points$Round)) +
      theme_nufflytics() +
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
    select(Special,Player:Type, Points = FP) %>% 
    mutate(Special = ifelse(
      Special == "c", 
      "<i class='fa fa-copyright' title='Captain'></i>",
      ifelse(
        Special=="r", 
        "<i class='fa fa-registered' title='Reserve'></i>", 
        NA
      )
    )
    )
  })
  output$team_summary <- DT::renderDataTable(
   team_table(),
    options = list(
      pageLength = 100,
      dom = 't',
      class = 'compact'
    ),
    rownames = FALSE,
    colnames = c(" " = "Special"),
    escape = FALSE,
    selection = "single"
  )
  
  
  #Stats tab ---------
  summarised_stats <- stats %>% 
    group_by(Name,Team,Race,Type,playerID) %>% #add playerID when using real data
    summarise(Games = n(), Points = mean(FP), BLK = mean(BLK), AVBr = mean(AVBr), KO = mean(KO), CAS = mean(CAS), Kills = mean(Kills), TD = mean(TD)) %>% 
    arrange(desc(Points))
  
  output$stats_table <- DT::renderDataTable(
    DT::datatable(summarised_stats %>% select(-playerID),
                  options = list(
                    lengthMenu = c(5,10,20,50),
                    pageLength = 10,
                    dom = 'ltp'
                  ),
                  selection = "single",
                  filter = "top",
                  rownames = F
    ) %>% DT::formatRound(5:11)
  )
  
  output$best_game_stats <- renderInfoBox({
    validate(need(input$stats_table_rows_selected, message = F))
    
    best_match = stats %>%  
      filter(playerID %in% summarised_stats[input$stats_table_rows_selected,]$playerID) %>% 
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
    validate(need(input$stats_table_rows_selected, message = F))
    
    player_points <- stats %>%  
      filter(playerID %in% summarised_stats[input$stats_table_rows_selected,]$playerID) %>%
      arrange(Round)
    
    player_points %>% 
      ggplot(aes(x=Round,y=FP)) +
      geom_bar(stat="identity") +
      theme_nufflytics() +
      ggtitle(summarised_stats[input$stats_table_rows_selected,]$Name) +
      scale_x_continuous(breaks = 1:max(player_points$Round), labels = player_points$Opponent) +
      ylab("Points") +
      xlab("Opponent") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$worst_game_stats <- renderInfoBox({
    validate(need(input$stats_table_rows_selected, message = F))
    
    worst_match = stats %>%  
      filter(playerID %in% summarised_stats[input$stats_table_rows_selected,]$playerID) %>% 
      filter(FP == min(FP)) 
    
    infoBox(
      title = "Worst Match",
      value = worst_match$FP,
      subtitle = paste0("Against ", worst_match$Opponent), #CHANGE TO OPP_TEAM WHEN USING REAL DATA
      icon = icon("thumbs-down"),
      color="red"
    )
  })
})
