
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
#library(tidyverse)
library(nufflytics)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(ggplot2)
library(DT)
library(hrbrthemes)
library(bcrypt)
library(shinyjs)
library(shinycssloaders)

source("global.R")

min_players <- 6
start_treasury <- 600

start_time = lubridate::dmy_hm("240618 0000", tz = "UTC")

theme_fantasy <- function() {
  theme_ipsum_rc(base_size = 12, axis_title_just = "m", axis_title_size = 14, grid = "Yy") + theme(legend.position = "bottom")
}

initial_player_pool <- read_rds("data/api_team_output.rds") %>% 
  map_dfr(~data_frame(
    coach = .$coach$name, 
    team = .$team$name, 
    name = map_chr(.$roster,"name"), 
    race = id_to_race(.$team$idraces), 
    type = map_chr(.$roster, "type") %>% str_replace_all(c(".*_(.*)"="\\1", "([a-z])([A-Z])"="\\1 \\2")), 
    level = map_int(.$roster,"level"), 
    skills = map(.$roster, "skills"), 
    injuries = map(.$roster, ~map_chr(.$casualties_state, state_to_casualty)),
    playerID = map_int(.$roster,"id")
  ))

pretty_skills <- function(skill) {
  str_replace_all(skill, c(
    "IncreaseStrength" = "+ST",
    "IncreaseAgility" = "+AG",
    "IncreaseMovement" = "+MA",
    "IncreaseArmour" = "+AV",
    "([a-z])([A-Z])" = "\\1 \\2"
  )
  )
}

check_player_numbers <- function(df) {
  if(nrow(df)==0) return(FALSE)
  
  df <- df %>% 
    group_by(race, type) %>% 
    summarise(n = n())
  
  max_doubles = max(df$n) <= 2
  ww_lt_one = ifelse("Werewolf" %in% df$type, filter(df, type=="Werewolf") %>% .$n  %>% magrittr::equals(1), TRUE)
  
  max_doubles & ww_lt_one
}

check_regions <- function(df) {
  if(nrow(df)==0) return(FALSE)
  
  df <- df %>% 
    group_by(Region) %>% 
    summarise(n = n())
  
  (nrow(df) >= 3) & (min(df$n) >= 2)
}

shinyServer(function(input, output, session) {
  #Setup ------
  session$allowReconnect(TRUE)
  gameweek = difftime(now("UTC"), start_time, units = "weeks") %>% ceiling()
  
  coaches <- read_lines("data/coaches.txt")
  teams <- read_csv("data/fantasy_teams.csv", col_types = "cciccccic")
  treasury <- read_csv("data/treasury.csv", col_types = "ci")
  treasury <- as.list(treasury$Cash) %>% set_names(treasury$Coach)
  stats <- read_csv("data/OI_player_stats.csv") %>% filter(!Type %in% c("Star Player"))
  costs <- read_csv("data/costs.csv")
  regions <- select(stats, Team, league) %>% unique %>% rename(Region = league)
  stats <- stats %>% rename(Region = "league") %>% left_join(costs) %>% mutate(`Total Cost` = Cost + (Level-1)*10, Efficiency = FP*10/`Total Cost`)
  points = left_join(teams, stats, by=c("Player" = "Name", "Team", "Race", "Type","Round"))
  
  observeEvent( # sets the selected round on the 
    input$tabs,
    updateNumericInput(session, "selected_round", value = as.numeric(gameweek)),
    once = T
  )
  #Login stuff ------
  user = reactiveVal("")
  pwd_err <- reactiveVal()
  
  output$username <- renderText(user())
  output$pwd_err <- renderText(pwd_err())
  observeEvent(input$login, {
    if(user() != "") {
      user("")
      pwd_err(NULL)
      updateActionButton(session, "login", "Login", icon = icon("user-o", class = "fa-lg fa-fw"))
    } else {
      showModal(modalDialog(
        title = "Login",
        easyClose = T,
        selectizeInput("bb2_coach","BB2 coach name", choices = c("Select Coach" = "", coaches), multiple=F, selected = NA),
        passwordInput("pwd", "Password"),
        textOutput("pwd_err", inline=T),
        footer = tagList(modalButton("Cancel", icon = icon("ban")),actionButton("login_btn", "Login", icon = icon("user")))
      )) 
    }
  })
  
  observeEvent(input$login_btn, {
    pwd_err(NULL)
    coach <- isolate(input$bb2_coach)
    pass <- isolate(input$pwd)
    
    check = read_csv("data/login.csv") %>% filter(user == coach) %>% .$pwd
    
    #new user
    if(is_empty(check)) {
      if(pass != "") {
        d = data_frame(user = coach, pwd = bcrypt::hashpw(pass, gensalt(12)))
        write_csv(d , "data/login.csv", append = T)
        user(input$bb2_coach)
        removeModal()
      } else{
        pwd_err("Please choose a password to create a team")
      }
    } else {
      #check pwd
      if(bcrypt::checkpw(input$pwd, check)) {
        user(coach)
        pwd_err(NULL)
        removeModal()
      } else {
        updateTextInput(session, "pwd", value = "")
        pwd_err("Incorrect password, please try again.")
      }  
    }
    updateActionButton(session, "login", "Logout", icon = icon(ifelse(user()=="","user-o","user"), class = "fa-lg fa-fw"))
  })
  
  
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
      mutate(
        Special = ifelse(Special=="r", "<i class='fa fa-registered' title='Reserve'></i>", NA),
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
    summarise(Cost = last(`Total Cost`), Games = n(), Points = mean(FP), Efficiency = mean(Efficiency), BLK = mean(BLK), AVBr = mean(AVBr), KO = mean(KO), CAS = mean(CAS), Kills = mean(Kills), TD = mean(TD), Pass = mean(Pass), `Pass(m)` = mean(Pass_m), Catch = mean(Catch), Int = mean(Int), `Carry(m)` = mean(Carry_m), Surf = mean(Surf)) %>% 
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
    ) %>% DT::formatRound(8:21)
  )
  
  output$stats_table <- DT::renderDataTable(
    DT::datatable(sorted_stats %>% 
                    select(Region, Name, Team, Race, Type, Round, Cost = `Total Cost`, Points = "FP", Efficiency, BLK, AVBr, KO, CAS, Kills, TD, Pass, `Pass(m)`= "Pass_m", Catch, Int, `Carry(m)` = Carry_m, Surf),
                  extensions = "Scroller",
                  options = list(
                    dom = 'tip',
                    scrollX = TRUE,
                    scrollY = 300,
                    scroller = list(loadingIndicator = TRUE)
                  ),
                  selection = "single",
                  filter = "top",
                  rownames = F
    ) %>% DT::formatRound(9)
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
  
  #Team Builder ------
  output$team_builder_menu <- renderMenu({
    validate(need(user(), message = F), need(gameweek < 3, message = F))
    
    menuItem("Create Team", tabName = "create", icon = icon("pencil-alt", class = "fa-fw fa-lg"))
  })
  
  selectable_players <- reactive({
    initial_player_pool %>% 
      filter(coach != user()) %>% 
      mutate(
        skills = map_chr(
          skills, ~ifelse(
            is_empty(.), 
            NA, 
            map_chr(., ~glue::glue("<img class='skillimg' src='img/skills/{.}.png' title='{pretty_skills(.)}' />")) %>% glue::collapse()
          )
        ), 
        injuries = map_chr(
          injuries, ~ifelse(
            is_empty(.), 
            NA, 
            map_chr(., ~glue::glue("<img class='skillimg' src='img/skills/{.}.png' title='{.}' />")) %>% glue::collapse()
          )
        )
      ) %>% 
      left_join(costs, by = c(race="Race",type="Type")) %>% 
      left_join(regions, by = c(team="Team")) %>% 
      mutate(Cost = Cost + (level-1)*10) %>% 
      select(Region, coach,team,name,race,type,level, Cost, everything())
  })
  
  output$player_pool <- DT::renderDataTable({
    DT::datatable(
      selectable_players() %>% select(-playerID) %>% mutate_at(vars("Region", "race", "Cost"), as.factor),
      class = "display compact new-filter",
      escape = c(-9,-10),
      filter = "top",
      rownames = F,
      colnames = c("Region", "Coach","Team", "Player", "Race", "Type", "Level", "Cost", "Acquired Skills", "Current Injuries"),
      selection = "single",
      options = list(
        dom = "tlip",
        search = list(regex=T),
        scrollX = T,
        pageLength = 5,
        lengthMenu = c(5, 10, 20),
        columns = list(
          list( #Region
            width = '5%'
          ),
          NULL,NULL,NULL,NULL,
          list( #Type
            width = '10%'
          ),
          list( #Level
            width = '5%'
          ),
          list( #Cost
            width = '5%'
          ),
          NULL,NULL
        )
      ) 
    )
  })
  
  user_created_team <- reactiveValues()
  
  observeEvent(nchar(input$teamname), { # Reload team data if coach already submitted something
    if(filter(teams, Coach == user()) %>% nrow > 0) {
      
      for (id in filter(teams, Coach == user()) %>% .$playerID) {
        user_created_team[[as.character(id)]] <- filter(selectable_players(), playerID == id)
      }
      
      updateTextInput(session, "teamname", value = filter(teams, Coach == user()) %>% head(1) %>% .$FTeam, placeholder = NULL)
    }
  }, ignoreNULL = T, ignoreInit = T, once = T)
  
  output$user_team_table <- DT::renderDataTable({
    validate(need(length(reactiveValuesToList(user_created_team) %>% compact) > 0, message = F))
    DT::datatable(
      data = reactiveValuesToList(user_created_team) %>% 
        bind_rows() %>% 
        select(-playerID) %>% 
        arrange(desc(Cost)),
      class = "display compact",
      escape = c(-10,-11),
      filter = "none",
      colnames = c("Region", "Coach","Team", "Player", "Race", "Type", "Level", "Cost", "Acquired Skills", "Current Injuries"),
      selection = "single",
      options = list(
        dom = "t",
        scrollX = T,
        pageLength = 100,
        ordering = FALSE,
        columns = list(NULL,
                       list( #Region
                         width = '5%'
                       ),
                       NULL,NULL,NULL,NULL,
                       list( #Type
                         width = '10%'
                       ),
                       list( #Level
                         width = '5%'
                       ),
                       list( #Cost
                         width = '5%'
                       ),
                       NULL,NULL
        )
      )
    )
  })
  
  creation_cash_left <- reactive({ start_treasury - (map_dbl(reactiveValuesToList(user_created_team) %>% compact, 'Cost') %>% sum) })
  
  output$cash_remaining = renderUI({
    span(class=glue::glue("treasury pull-right {ifelse(creation_cash_left() >= 0, 'text-success', 'text-danger')}"), glue::glue("Treasury remaining: ${prettyNum(creation_cash_left(), big.mark=',')}"))
  })
  
  
  # Team validation and infoBoxes -----
  validation <- reactiveValues(
    bank = F,
    num_players = F,
    positions = F,
    regions = F
  )
  
  observeEvent(reactiveValuesToList(user_created_team), {
    team <- isolate(reactiveValuesToList(user_created_team) %>% compact())
    
    validation$bank <- (start_treasury - (map_dbl(team, 'Cost') %>% sum)) >= 0
    validation$num_players <- length(team) >= min_players
    validation$positions <- check_player_numbers(team %>% bind_rows())
    validation$regions <- team %>% bind_rows() %>% check_regions()
  }, ignoreNULL = T, ignoreInit = T)
  
  output$bank <- renderInfoBox({
    validate(need(length(reactiveValuesToList(user_created_team)) > 0, message = F))
    infoBox("Bank", value = "Max budget $1,200", icon = icon(ifelse(validation$bank, "thumbs-up", "thumbs-down")), color = ifelse(validation$bank, "green","red"), fill = T)
  })
  output$num_players <- renderInfoBox({
    validate(need(length(reactiveValuesToList(user_created_team)) > 0, message = F))
    infoBox("Player Numbers", value = "Min 12 players", icon = icon(ifelse(validation$num_players, "thumbs-up", "thumbs-down")), color = ifelse(validation$num_players, "green","red"), fill = T)
  })
  output$positions <- renderInfoBox({
    validate(need(length(reactiveValuesToList(user_created_team)) > 0, message = F))
    infoBox("Player Diversity", value = "Max 2 of each positional", icon = icon(ifelse(validation$positions, "thumbs-up", "thumbs-down")), color = ifelse(validation$positions, "green","red"), fill = T)
  })
  output$regions <- renderInfoBox({
    validate(need(length(reactiveValuesToList(user_created_team)) > 0, message = F))
    infoBox("Region Diversity", value = "Min 2 from each region", icon = icon(ifelse(validation$regions, "thumbs-up", "thumbs-down")), color = ifelse(validation$regions, "green","red"), fill = T)
  })  
  # End team validation ------
  
  output$team_builder <- renderUI({
    validate(need(user(), message = F), need(gameweek < 3, message = F))
    
    fluidRow(
      box(
        title = "Player Pool",
        width = 12,
        collapsible = T,
        status = "primary",
        solidHeader = T,
        fluidRow(class = "vertical-align", 
                 column(3, p(
                   "Select players for your fantasy team"
                 )), 
                 column(9,span(class = "pull-right", actionButton("add_player", "Hire player", icon = icon("plus-circle"))))
        ),
        withSpinner(DT::dataTableOutput("player_pool"))
      ),
      box(
        title=glue::glue("{user()}'s team:"),
        width = 12,
        status = "success",
        solidHeader = T,
        collapsible = T, 
        fluidRow(class = "vertical-align",
                 column(3,textInput("teamname", label = NULL, placeholder = "Enter a team name")),
                 column(3, 
                        actionButton("submit_team", "Submit team", icon = icon("upload")), 
                        actionButton("remove_player", "Fire player", icon = icon("minus-circle"))
                 ),
                 column(6,uiOutput("cash_remaining"))
        ),
        DT::dataTableOutput("user_team_table"),
        fluidRow(class = "short-infobox", style = "padding-top: 0.5em",
                 column(3,infoBoxOutput("bank")),
                 column(3,infoBoxOutput("num_players")),
                 column(3,infoBoxOutput("positions")),
                 column(3,infoBoxOutput("regions"))
        )
      )
    )
  })
  
  # Button toggling
  observe({
    validate(need(nchar(input$teamname), message=F))
    
    if(is.null(input$player_pool_rows_selected)) {
      disable("add_player")
      removeClass("add_player", class = "btn-success")
    } else {
      enable("add_player")
      addClass("add_player", class = "btn-success")
    }
    
    if(is.null(input$user_team_table_rows_selected) | length(reactiveValuesToList(user_created_team) %>% compact)==0 ) {
      disable("remove_player")
      removeClass("remove_player", class = "btn-danger")
    } else {
      enable("remove_player")
      addClass("remove_player", class = "btn-danger")
    }
    
    if( (all(as.logical(reactiveValuesToList(validation)))) & nchar(input$teamname)>0 ) {
      enable("submit_team")
      addClass("submit_team", "btn-success")
    } else {
      disable("submit_team")
      removeClass("submit_team", "btn-success")
    }
    
    if((length(input$reserve_selection) %>% magrittr::add(min_players-1)) == (reactiveValuesToList(user_created_team) %>% compact %>% length)) {
      enable("confirm_submission")
      addClass("confirm_submission", "btn-success")
    } else {
      disable("confirm_submission")
      removeClass("confirm_submission", "btn-success")
    }
  })
  
  # Button actions
  observeEvent(input$add_player, {
    selected <- isolate(selectable_players())[isolate(input$player_pool_rows_selected), ] %>% as.list()
    
    user_created_team[[as.character(selected$playerID)]] <- as_data_frame(selected) 
  })
  
  observeEvent(input$remove_player, {
    cat(paste0(user(), " is firing a player\n"))
    deleted_id <- isolate(reactiveValuesToList(user_created_team) %>% compact %>% magrittr::extract(order(map_dbl(., "Cost"), decreasing = T)) %>% names)[input$user_team_table_rows_selected]
    
    cat(paste0("Firing ", user_created_team[[deleted_id]]$name, " (", deleted_id, ")\n"))
    user_created_team[[deleted_id]] <- NULL
  })
  
  observeEvent(input$submit_team, {
    ordered_team <- isolate(reactiveValuesToList(user_created_team) %>% compact %>% magrittr::extract(order(map_dbl(., "Cost"), decreasing = T)) %>% bind_rows)
    
    cash_text <- ""
    if(creation_cash_left() > 0) cash_text <- glue::glue("<p>You will have ${creation_cash_left() %>% prettyNum(big.mark=',')} remaining in your treasury for future trades</p>")
    
    showModal(modalDialog(
      title = "Confirm team and select reserves",
      easyClose = T,
      div(class = "bg-info", 
          HTML(paste0("<p>", strong(user()), glue::glue(", please select a captain and {nrow(ordered_team) - (min_players-1)} reserve players for "),tags$strong(input$teamname), "</p>", cash_text))
          ),
      selectInput("captain_picker", "Captain:", choices = ordered_team %>% filter(type != "Werewolf") %>% .$name),
      uiOutput("reserve_picker"),
      footer = tagList(modalButton("Cancel", icon = icon("ban")), actionButton("confirm_submission", "Confirm team", icon = icon("upload")))
    ))
    
    output$reserve_picker = renderUI({
      checkboxGroupInput("reserve_selection", width = "100%", "Reserves", choiceValues = ordered_team$name[!ordered_team$name %in% input$captain_picker], choiceNames = glue::glue_data(ordered_team %>% filter(!name %in% input$captain_picker), "{name} - ({race} {type}, Level {level})"), selected = ordered_team$name[!ordered_team$name %in% input$captain_picker][-c(1:(min_players-2))])
    })
  })
  
  
  observeEvent(input$confirm_submission, {
    Coach <- user()
    FTeam <- input$teamname
    Round <- max(as.numeric(gameweek) + 1, 2)
    
    team <- reactiveValuesToList(user_created_team) %>% compact %>% magrittr::extract(order(map_dbl(., "Cost"), decreasing = T)) %>% bind_rows %>% 
      select(name, team, race, type, playerID) %>% 
      rename(Player = "name", Team = "team", Race = "race", Type="type")
    reserve <- team$Player %in% input$reserve_selection
    captain <- team$Player %in% input$captain_picker
    
    store <- cbind(Coach = Coach, FTeam = FTeam, Round = Round, team, Special = case_when(captain ~ "c", reserve ~ "r", T ~ ""))
    
    #reread data to minimise collision
    cat(paste0(now(tzone="UTC"), "\t", user(), " submitting team ", FTeam, " with players ", glue::collapse(team$Player, ", "), " and IDs ", glue::collapse(team$playerID, ", ") ,"\n"), file = "data/logs/submission.log", append = T)
    read_csv("data/fantasy_teams.csv", col_types = "cciccccic") %>% 
      filter(Coach != user()) %>% 
      bind_rows(store) %>% 
      write_csv("data/fantasy_teams.csv")
    
    removeModal()
    
    treasury[[user()]] <- creation_cash_left()
    
    cat(paste0(now(tzone="UTC"), "\t", user(), " treasury set to ", creation_cash_left(), "\n"), file = "data/logs/treasury.log", append = T)
    read_csv("data/treasury.csv", col_types = "ci") %>% 
      filter(Coach != user()) %>% 
      bind_rows(data_frame(Coach = user(), Cash = creation_cash_left())) %>% 
      write_csv("data/treasury.csv")
    
    shiny::showNotification(id = "team_submitted", ui = span(icon("check-circle") , "Team submitted"), duration = 2, type = "warning", closeButton = F)
    addClass("shiny-notification-team_submitted", "animated rubberBand")
  })
  
  #Trade helper ------
  output$team_management_menu <- renderMenu({
    validate(need(user(), message = F), need(gameweek > 2 | user() == "Schlice", message = F))
    
    menuItem("Team Management", tabName = "manage", icon = icon("clipboard-list", class = "fa-fw fa-lg"))
  })
  
  # Fetch current team for user (filter teams for user/max(round))
  
  # Create score history for players on the team?
  
  # Top display of team's performance (players, points-per-round (w/ pretty formatting?))
  # Alter reserves here?
  
  # Bottom selectize (max 2) of players in team
  # filter list of potential trades (filter stats by this and last round, unique players (last row if multiple), removing ones in team already and ones that cost too much (cost of traded players + treasury))
  # Display trade summary w/ request button
  # Send email? about trade, notify that it will be processed for next round
  
  output$team_management <- renderUI({
    validate(need(user(), message = F), need(gameweek > 2 | user() == "Schlice", message = F))
    
    fluidRow(
      box(
        title = "Team viewer",
        width = 12,
        h2("Show team's performance")
      ),
      box(
        width = 12,
        h2("Trade assistant UI")
      )
    )
  })
  
  
  #output$debug = renderText(c(names(user_created_team), map_lgl(reactiveValuesToList(user_created_team), is.null)))
  #output$debug2 = renderTable(reactiveValuesToList(user_created_team) %>% compact %>% bind_rows)
})
