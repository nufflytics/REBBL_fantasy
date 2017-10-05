
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tidyverse)
library(DT)
library(lubridate)

cat(file=stderr(), "starting.....","\n")
start_time = lubridate::dmy_hm("051017 0000", tz = "EST")

teams <- read_csv("data/fantasy_teams.csv") %>% mutate(Race = str_replace_all(Race, "_"," "))
cat(file=stderr(), "teams loaded","\n")


stats <- read_csv("data/player_stats.csv") %>% filter(!Type %in% c("Star Player", "Unknown playertype"))
cat(file=stderr(), "stats loaded","\n")

points = left_join(teams, stats, by=c("Player" = "Name", "Race", "Type"))
cat(file=stderr(), "points loaded","\n")



shinyServer(function(input, output, session) {
  #General data -----
  gameweek = difftime(now("UTC"), start_time, units = "weeks") %>% ceiling()
  
  #Teams tab ------
  
  output$coach_select <- renderUI({
    selectizeInput("selected_coach", NULL, teams$Coach %>% unique %>% sort)
  })
  
  
  output$team_name <- renderUI(h3(filter(teams,Coach == input$selected_coach) %>% .$FTeam %>% unique))
  
  output$team_summary <- DT::renderDataTable(
    points %>% 
      #filter(round == input$selected_round) %>% 
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
      ),
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
    group_by(Name,Race,Type) %>% #add playerID when using real data
    summarise(Games = n(), Points = mean(FP), BLK = mean(BLK), AVBr = mean(AVBr), KO = mean(KO), CAS = mean(CAS), Kills = mean(Kills), TD = mean(TD)) %>% 
    arrange(desc(Points))
  
  output$stats_table <- DT::renderDataTable(
    DT::datatable(summarised_stats,
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
  
  output$best_game <- renderInfoBox({
    validate(need(input$stats_table_rows_selected, message = F))
    
    best_match = stats %>%  
      filter(Name %in% summarised_stats[input$stats_table_rows_selected,]$Name) %>% 
      filter(FP == max(FP)) # separate filter to not filter on global max points
    
    infoBox(
      title = "Best Match",
      value = best_match$FP,
      subtitle = paste0("Against ", best_match$Name), #CHANGE TO OPP_TEAM WHEN USING REAL DATA
      icon = icon("thumbs-up"),
      color = "green"
    )
  })
  
  output$points_bar <- renderPlot({
    validate(need(input$stats_table_rows_selected, message = F))
    
    stats %>%  
      filter(Name %in% summarised_stats[input$stats_table_rows_selected,]$Name) %>% 
      .$FP %>% 
      barplot()
  })
  
  output$worst_game <- renderInfoBox({
    validate(need(input$stats_table_rows_selected, message = F))
    
    worst_match = stats %>%  
      filter(Name %in% summarised_stats[input$stats_table_rows_selected,]$Name) %>% 
      filter(FP == min(FP)) 
    
    infoBox(
      title = "Worst Match",
      value = worst_match$FP,
      subtitle = paste0("Against ", worst_match$Name), #CHANGE TO OPP_TEAM WHEN USING REAL DATA
      icon = icon("thumbs-down"),
      color="red"
    )
  })
})
