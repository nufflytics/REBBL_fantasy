
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
#library(tidyverse)
library(nufflytics)
library(readr)
library(tidyr)
library(dplyr)
library(magrittr)
library(stringr)
library(lubridate)
library(purrr)
library(ggplot2)
library(DT)
library(hrbrthemes)
library(bcrypt)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)

source("global.R")

min_players <- 12
start_treasury <- 1200

admin_webhook <- "https://discordapp.com/api/webhooks/465757115349467156/railu4TRPS1W6o7THZGNr6NbhFnAhSDAcfYS_dMxeLfyj4uUylfZGR_SHTO9MD2VeK7i"

start_time = lubridate::dmy_hm("040718 2359", tz = "Australia/Sydney")

theme_fantasy <- function() {
  theme_ipsum_rc(base_size = 12, axis_title_just = "m", axis_title_size = 14, grid = "Yy") + theme(legend.position = "bottom")
}

initial_player_pool <- read_rds("data/all_rebbl_api_team_output.rds") %>% 
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
  gameweek = difftime(now(), start_time, units = "weeks") %>% ceiling()
  
  output$countdown <- renderUI({
    validate(need(user(), message = F))
    invalidateLater(1000*60)
    
    td <- difftime(start_time + weeks(gameweek), now()) %>% as.period()
    
    daysep <- ifelse(td@day > 1, " days ", " day ")
    
    time_left <- paste0(td@day, daysep, td@hour,":" , sprintf("%02d", td@minute), " left")
    
    start <- regexpr("[-1-9]|(0\\.)", time_left) # remove leading 0 values
    time_left <- ifelse(start > 0, substr(time_left, start, nchar(time_left)),"")
    
    class = "text-info"
    
    if(td@day == 0) {
      class = "text-warning"
      if(td@hour == 0) {
        class = "text-danger"
      }
    }
    
    HTML(glue::glue("<div class='vertical-align'><div>Round {gameweek}:&nbsp;</div><div class='{class}'>{time_left}</div></span>"))
    
  })
  
  coaches <- read_lines("data/coaches.txt")
  teams <- read_csv("data/fantasy_teams.csv", col_types = "cciccccic", trim_ws = F)
  treasury <- read_csv("data/treasury.csv", col_types = "ci", trim_ws = F)
  treasury <- as.list(treasury$Cash) %>% set_names(treasury$Coach)
  stats <- read_rds("data/player_stats.rds") %>% filter(!Type %in% c("Star Player")) %>% mutate_at(vars(league), str_remove, "REBBL - ")
  trades <- read_rds("data/trades.rds")
  costs <- read_csv("data/costs.csv", trim_ws = F)
  #regions <- select(stats, Team, league) %>% unique %>% rename(Region = "league")
  regions <- read_csv("data/regions.csv", trim_ws = F)
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
      user_created_team <- reactiveValues()
      
      updateActionButton(session, "login", "Login", icon = icon("user", class = "fa-lg fa-fw", type = "regular"))
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
    updateActionButton(session, "login", "Logout", icon = icon("user", type = ifelse(user()=="","regular","solid"), class = "fa-lg fa-fw"))
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
    filter(Round == gameweek) %>% 
    summarise(`Scoring players this gameweek` = paste0(played,"/",team_size), `Team Efficiency` = sum(PCR, na.rm=T), Points = sum(Tot_points)) %>% 
    arrange(desc(Points))
  
  output$leaderboard <- DT::renderDataTable({
    DT::datatable(leaders,
                  options = list(
                    pageLength = 5,
                    lengthMenu = c(5, 10, 23),
                    dom = 'plt',
                    class = 'display compact'
                  ),
                  rownames = FALSE,
                  selection = list(mode = "multiple", selected = 1:5)
    ) %>% DT::formatRound(3)
  })
  
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
  
  team_table <- reactive({
    validate(need(input$selected_coach, message = F))
    
    points %>% 
      filter(Round == input$selected_round) %>% 
      filter(Coach == input$selected_coach) %>% 
      select(Special,Player:Type,Cost = `Total Cost`, Points = FP) %>% 
      mutate(
        Special = case_when(
          Special=="c" ~ "<i class='far fa-copyright' title='Captain'></i>",
          Special=="r" ~ "<i class='far fa-registered' title='Reserve'></i>", 
          T ~ ''
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
    DT::datatable(summarised_stats %>% select(-playerID) %>% ungroup() %>% mutate_at(vars(Region), as.factor),
                  extensions = "Scroller",
                  options = list(
                    dom = 'tip',
                    deferRender = T,
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
                    select(Region, Name, Team, Race, Type, Round, Cost = `Total Cost`, Points = "FP", Efficiency, BLK, AVBr, KO, CAS, Kills, TD, Pass, `Pass(m)`= "Pass_m", Catch, Int, `Carry(m)` = Carry_m, Surf) %>% 
                    mutate_at(vars(Region), as.factor),
                  extensions = "Scroller",
                  options = list(
                    dom = 'tip',
                    deferRender=T,
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
  
  observeEvent(c(nchar(input$teamname)), { # Reload team data if coach already submitted something
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
  
  
    #Team validation and infoBoxes -----
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
    #End team validation ------
  
  output$team_builder <- renderUI({
    validate(need(user(), message = F))
    
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
                 column(9,span(class = "pull-right", actionButton("add_player", "Hire player", icon = icon("plus-circle")) %>% remove_default()))
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
                        actionButton("submit_team", "Submit team", icon = icon("upload")) %>% remove_default(), 
                        actionButton("remove_player", "Fire player", icon = icon("minus-circle")) %>% remove_default()
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
      selectInput("captain_picker", "Captain:", choices = ordered_team %>% filter(type != "Werewolf") %>% .$name %>% set_names(glue::glue_data(ordered_team %>% filter(type != "Werewolf"), "{name} - ({race} {type}, Level {level})"))),
      uiOutput("reserve_picker"),
      footer = tagList(modalButton("Cancel", icon = icon("ban")), actionButton("confirm_submission", "Confirm team", icon = icon("upload")))
    ))
    
    output$reserve_picker = renderUI({
      checkboxGroupInput("reserve_selection", width = "100%", "Reserves:", choiceValues = ordered_team$name[!ordered_team$name %in% input$captain_picker], choiceNames = glue::glue_data(ordered_team %>% filter(!name %in% input$captain_picker), "{name} - ({race} {type}, Level {level})"), selected = ordered_team$name[!ordered_team$name %in% input$captain_picker][-c(1:(min_players-2))])
    })
  })
  
  
  observeEvent(input$confirm_submission, {
    Coach <- user()
    FTeam <- input$teamname
    Round <- 2
    
    team <- reactiveValuesToList(user_created_team) %>% compact %>% magrittr::extract(order(map_dbl(., "Cost"), decreasing = T)) %>% bind_rows %>% 
      select(name, team, race, type, playerID) %>% 
      rename(Player = "name", Team = "team", Race = "race", Type="type")
    reserve <- team$Player %in% input$reserve_selection
    captain <- team$Player %in% input$captain_picker
    
    store <- cbind(Coach = Coach, FTeam = FTeam, Round = Round, team, Special = case_when(captain ~ "c", reserve ~ "r", T ~ ""))
    
    #reread data to minimise collision
    cat(paste0(now(tzone="UTC"), "\t", user(), " submitting team ", FTeam, " with players ", glue::collapse(team$Player, ", "), " and IDs ", glue::collapse(team$playerID, ", ") ,"\n"), file = "data/logs/submission.log", append = T)
    
    httr::POST(
      url = admin_webhook,
      body = list(
        username = "Team submitted",
        content = glue::glue("**User:** {user()}\n**Team:** {input$teamname}\n```{knitr::kable(store)}```")
      ),
      encode = "json"
      )
    
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
    
    shiny::showNotification(id = "team_submitted", ui = span(icon("check-circle") , "Team submitted"), duration = 3, type = "warning", closeButton = F)
    addClass("shiny-notification-team_submitted", "animated rubberBand")
  })
  
  #Trade helper ------
  output$team_management_menu <- renderMenu({
    validate(need(user(), message = F), need(user_team(), message = F))
    
    menuItem("Team Management", tabName = "manage", icon = icon("clipboard-list", class = "fa-fw fa-lg"))
  })
  
  # Fetch current team for user (filter teams for user/max(round))
  # Create score history for players on the team?
  
  react_teams <- reactiveFileReader(1000, session, "data/fantasy_teams.csv", read_csv)
  
  split_teams <- reactive({
    split(react_teams(), react_teams()$Coach) %>% 
      map(~split(., .$Round))
  })
  
  user_team <- reactive({
    split_teams()[[user()]][[ifelse(gameweek<2, "2", as.character(gameweek))]]
  })
  
  next_round_team <- reactive({
    # browser()
    next_gameweek <- ifelse(gameweek <= 2, "3", as.character(gameweek + 1))
    ret = NA
    if(next_gameweek %in% names(split_teams()[[user()]])) {
      ret = split_teams()[[user()]][[ifelse(gameweek<2, "3", as.character(gameweek + 1))]]
    } else {
      ret = user_team() %>% mutate(Round = Round +1)
    }
    
    ret
  }) 
  
  
  player_status <- reactive({
    validate(need(next_round_team(), message = F))
    filter(stats, playerID %in% next_round_team()$playerID) %>% 
      filter(Round <= max(as.numeric(gameweek), 1)) %>% 
      group_by(playerID) %>% 
      summarise(Level = last(Level), `Total Cost` = last(`Total Cost`), isDead = "Dead" %in% new_injuries) %>% 
      left_join(next_round_team()) %>% 
      select(playerID, Level, `Total Cost`, isDead)
  })
  
  team_overview <- reactive({
    #browser()
    validate(need(user_team(), message = F), need(player_status(), message = F))
    
    next_round_team() %>% 
      select(-Round) %>% 
      left_join(stats) %>% 
      select(playerID, Player, Race, Type, Round, FP, Special) %>% 
      mutate(Round = paste0("R",Round)) %>% 
      spread(Round, FP) %>% 
      left_join(player_status())
  })
  
  # Top display of team's performance (players, points-per-round (w/ pretty formatting?))
  
  output$team_overview_table <- DT::renderDataTable({
    DT::datatable({
      team_overview() %>% 
        mutate(Special = case_when(
          Special=="c" ~ "<i class='far fa-copyright' title='Captain'></i>",
          Special=="r" ~ "<i class='far fa-registered' title='Reserve'></i>",
          is.na(Special) ~ ''
        ),
        isDead = ifelse(isDead, "<img class='skillimg' src='img/skills/Dead.png' title='Dead' />", "")
        ) %>% 
        select(Special, Player, Race, Type, Level, `Total Cost`, matches("R[123456789]"), isDead) %>% 
        arrange(desc(`Total Cost`))
    },
    rownames = F,
    class="display compact",
    colnames = c(' ' = 'Special', ' ' = 'isDead'),
    filter = "none",
    escape = c("Player"),
    options = list(
      dom = "t",
      scrollX = T,
      pageLength = 20,
      ordering = F,
      #autoWidth = T,
      columnDefs = list(list(targets = c(0), width = "20px"), list(targets = c(1:5), width = "10%"), list(targets = c(6:(gameweek+5)), width = "15%"))
    ) 
    ) %>% 
      DT::formatStyle(colnames(.$x$data)[grep("R[1234567890]",colnames(.$x$data))],
                      background = styleColorBar(c(0,20), "#cce5ff"),
                      backgroundSize = '90% 90%',
                      backgroundRepeat = 'no-repeat',
                      backgroundPosition = 'center')
  })
  
  # filter list of potential trades (filter stats by this and last round, unique players (last row if multiple), removing ones in team already and ones that cost too much (cost of traded players + treasury))
  team_trade_value <- reactive({
    #browser()
    stats %>% 
      filter(playerID %in% next_round_team()$playerID) %>% 
      group_by(playerID) %>% 
      mutate_at(vars(old_injuries, new_injuries, skills), as.character) %>% 
      summarise_all(last) %>% 
      ungroup() %>% 
      select(playerID, Name, `Total Cost`) %>% 
      arrange(desc(`Total Cost`))
  })
  
  treasury_change <- reactive({
    validate(need(input$trade_out, message = F))
    
    cost_sold <- filter(team_overview(), playerID %in% input$trade_out) %>% use_series(`Total Cost`) %>% sum
    
    cost_bought <- 0
    
    if(!is.null(input$trade_in)) cost_bought <-filter(trade_pool(), playerID %in% input$trade_in) %>% use_series(`Total Cost`) %>% sum
    
    cost_sold - cost_bought - trade_fee()
  })
  
  trade_pool <- reactive({
    stats %>% 
      filter(Round %in% c(gameweek-1, gameweek)) %>% 
      group_by(playerID) %>% 
      #mutate_at(vars(old_injuries, new_injuries, skills), as.character) %>% 
      summarise_all(~(if('list' %in% class(.)) .[length(.)] else last(.))) %>% 
      ungroup() %>% 
      filter(!playerID %in% team_trade_value()$playerID) %>% 
      arrange(desc(`Total Cost`))
  })
  
  trade_fee <- reactive({
    validate(need(!is.null(input$trade_out), message = F))

    free_trade <- team_overview() %>% 
      filter(playerID %in% input$trade_out) %>% 
      select(playerID, matches("R[1234567890]"), isDead) %>% 
      gather(round, points, -playerID, -isDead) %>% 
      mutate(round = str_remove(round,"R") %>% as.integer()) %>% 
      group_by(playerID) %>% 
      summarise(round = max(round), isDead = all(isDead)) %>% 
      mutate(fired = (gameweek - round) >= 3, free = isDead|fired) %>% 
      use_series(free) %>% 
      all
    
    ifelse(free_trade | input$request_trade_waiver, 0, 10)
  })
  
  available_trades <- reactive({
    validate(need(trade_fee(), message = F))
    
    traded_value <- filter(team_trade_value(), playerID %in% input$trade_out) %>% use_series(`Total Cost`) %>% sum()
    
    users_team <- filter(initial_player_pool, coach == user())$team %>% unique
    
    trade_pool() %>% 
      filter(`Total Cost` <= (treasury[[user()]] + traded_value - trade_fee()), Team != users_team, playerID > 100)
  })
  
  output$trade_summary <- renderUI({
    validate(need(input$trade_out, message = F))
    
    div(class = "vertical-align",
                     column(7,
                            uiOutput("trade_tally")
                     ),
                     column(3,class = "text-center",
                            actionButton("confirm_trade", "Process Trade", icon = icon("check-circle", type = "regular", class = "fa-lg"), class = "disabled", style="margin-bottom:0px")%>% remove_default()
                     ),
                     column(2,class = "bg-info",id = "waiver_box",
                            checkboxInput("request_trade_waiver","Request trade fee waiver"), tags$small("Select if you believe a trade fee has been applied incorectly to this trade"))
                     )
  })
  
  output$trade_tally <- renderUI({
    tagList(
      h4("Trade Summary:"),
      map(input$trade_out, ~div(icon("arrow-left", class="fa-lg fa-fw text-danger"), span(class="text-danger", "Trading out:"), span(HTML(glue::glue_data(filter(team_overview(), playerID == .), "{Player} - {Race} {Type} {span(class = 'pull-right', '$',`Total Cost`)}"))))),
      map(input$trade_in, ~div(icon("arrow-right", class="fa-lg fa-fw text-success"), span(class="text-success", "Trading in:"), span(HTML(glue::glue_data(filter(available_trades(), playerID == .), "{Name} - {Race} {Type} {span(class = 'pull-right', '$',`Total Cost`)}"))))),
      div(icon("money-bill-wave", class = "fa-lg fa-fw"), "Trade fee", span(class = "pull-right", '$', trade_fee())),
      div("Net treasury", span(class = paste("pull-right total", case_when(treasury_change() > 0 ~ 'text-success', treasury_change() < 0 ~ 'text-danger', T ~ '')), "$", treasury_change()))
    )
  })

  observe({
    validate(need(treasury_change(), message = F))
    #browser()
    trade_out_num <- length(input$trade_out) <= 2
    trade_in_num <- ifelse("trade_in" %in% names(input), length(input$trade_in) <= 2 & length(input$trade_out) == length(input$trade_in), FALSE)
    good_value <- treasury_change() <= treasury[[user()]]
    
    if(trade_out_num & trade_in_num & good_value) {
      enable("confirm_trade")
      addClass("confirm_trade", "btn-success")
    } else {
      disable("confirm_trade")
      removeClass("confirm_trade", "btn-success")
    }
    
    
  })
  
  observeEvent(input$confirm_trade, {
    
    trade_in_players <- trade_pool() %>% filter(playerID %in% input$trade_in) %>% select(Name, Team, Race, Type, playerID)
    prospective_team <- next_round_team() %>% inset(.$playerID %in% input$trade_out, 4:8, trade_in_players)
    checker <- prospective_team %>% rename(race=Race,type=Type) %>% left_join(regions)
    
    prior_trade <- user() %in% names(trades[[max(as.integer(gameweek) - 1, 1)]]) 
    prior_trade_check <- T
    
    if(prior_trade) {prior_trade_check <- length(input$trade_out) == 1}
    this_week_trade <- user() %in% names(trades[[as.integer(gameweek)]])
    
    
    if(check_player_numbers(checker) & check_regions(checker) & !this_week_trade & prior_trade_check) {
      
      tmp = showModal(modalDialog(
        title = "Trade confirmation",
        tagList(
          map(input$trade_out, ~div(icon("arrow-left", class="fa-lg fa-fw text-danger"), span(class="text-danger", "Trading out:"), span(HTML(glue::glue_data(filter(team_overview(), playerID == .), "{Player} - {Race} {Type} {span(class = 'pull-right', '$',`Total Cost`)}"))))),
          map(input$trade_in, ~div(icon("arrow-right", class="fa-lg fa-fw text-success"), span(class="text-success", "Trading in:"), span(HTML(glue::glue_data(filter(available_trades(), playerID == .), "{Name} - {Race} {Type} {span(class = 'pull-right', '$',`Total Cost`)}"))))),
          div(icon("money-bill-wave", class = "fa-lg fa-fw"), "Trade fee", span(class = "pull-right", '$', trade_fee())),
          div("Net treasury", span(style = "border-top: 1px solid rgb(128,128,128); margin-bottom: -1px;", class = paste("pull-right", case_when(treasury_change() > 0 ~ 'text-success', treasury_change() < 0 ~ 'text-danger', T ~ '')), "$", treasury_change())),
          div("Start treasury", span(class = "pull-right", "$", treasury[[user()]])),
          div("End treasury", span(class = paste("pull-right total"), "$", treasury[[user()]] + treasury_change())),
          conditionalPanel("input.request_trade_waiver == true", 
            hr(),
            textAreaInput("waiver_reason", "Reason for fee waiver request:")
          )
        ),
        footer = tagList(modalButton("Cancel", icon = icon("ban")),actionButton("doubleconfirm_trade", "Trade", icon = icon("retweet")))
      ))
      
    } else { #invalid trade
      reason = tagList()
      
      if (!check_player_numbers(checker)) {
        reason <- append(reason, "Too many players of a single type.")
      }
      
      if (!check_regions(checker)) {
        reason <- append(reason, "Fewer than two players from each region.")
      }
      
      if (!prior_trade_check) {
        reason <- append(reason, "Can only trade two players if you didn't trade last week.")
      }
      
      if(this_week_trade) {
        reason <- append(reason, "Trade already completed this week.")
      }
      
      showModal(modalDialog(
        title = "Invalid trade",
        div(class = "text-center",
          div(icon("warning", class = "fa-4x text-warning")),
          div("Trade cannot be completed because:"),
          strong(reason)
          ),
        easyClose = TRUE
      ))
    }
    
    observeEvent(input$doubleconfirm_trade, {
      cat(paste0(now(tzone="UTC"), "\t", user(), " making a trade ", glue::collapse(input$trade_out, ", "), " for ", glue::collapse(input$trade_in, ", "), "\n"), file = "data/logs/trades.log", append = T)
      
      out_trade <- user_team() %>% left_join(team_trade_value(), by = c("Player"="Name", "playerID"="playerID")) %>% filter(playerID %in% input$trade_out) %>% glue::glue_data("{Player}, {Team}, {Race}, {Type}, ${`Total Cost`}, {playerID}") %>% glue::collapse("|")
      in_trade <- available_trades() %>% filter(playerID %in% input$trade_in) %>% glue::glue_data("{Name}, {Team}, {Race}, {Type}, ${`Total Cost`}, {playerID}") %>% glue::collapse("|")
      
      trades <- read_rds("data/trades.rds")
      trades[[gameweek]][[user()]] <- list(list(out = out_trade, `in` = in_trade, net_cash = treasury_change()))
      write_rds(trades, "data/trades.rds")
      
      message = glue::glue("**User:** {user()}\n**Team:** {user_team()$FTeam %>% unique()}\n**Trade out:** {out_trade}\n**Trade in:** {in_trade}\n**Waiver request:** {input$request_trade_waiver}")
      
      if(input$request_trade_waiver) {message <- paste(message, glue::glue("**Waiver reason:** {input$waiver_reason}"), sep = "\n")}
      
      httr::POST(
        url = admin_webhook,
        body = list(
          username = "Trade performed",
          content = message
        ),
        encode = "json"
      )
      
      read_csv("data/fantasy_teams.csv") %>% 
        filter(Coach != user() | Round != ifelse(gameweek < 2, "3", as.character(gameweek + 1))) %>% 
        bind_rows(prospective_team) %>% 
        write_csv("data/fantasy_teams.csv")
      
      removeModal()
      
      shiny::showNotification(id = "trade_submitted", ui = div(span(icon("check-circle") , "Trade completed."), div("Traded players will be active from next gameweek.")), duration = 4, type = "warning", closeButton = T)
      addClass("shiny-notification-trade_submitted", "animated rubberBand")
    })
    
  })
  
  
  output$FTeamName <- renderText(paste(user_team()$FTeam %>% unique(), "Overview"))
  
  output$team_management <- renderUI({
    validate(need(user(), message = F), need(user_team(), message = F))
    
    fluidRow(
      box(
        title = textOutput("FTeamName"),
        width = 12,
        status = "primary",
        solidHeader = T,
        collapsible = T,
        #p("Interface for making trades and swapping captain/reserves will appear here once Round Two has commenced.")
        DT::dataTableOutput("team_overview_table"),
        fluidRow(class = "vertical-align", style = "padding-top: 0.7em",
                 column(3, radioGroupButtons("change_special", "Reassign:", choices = c("Captain", "Reserves"), justified = T, selected = NULL, status = "primary", individual = T)),
                 column(6, selectizeInput("special_picker", "Player", choices = team_overview() %>% filter(Special %in% c(NA, NA_character_), Type != "Werewolf") %>% arrange(desc(`Total Cost`)) %>% use_series(Player),   multiple = T)),
                 column(3, br(), actionButton("special_confirm","Confirm change", icon = icon("check-circle", type="regular"), class = "btn-success", width = "100%") %>% remove_default())
        )
        
      ),
      box(
        title = "Trade assistant",
        width = 12,
        status = "warning",
        solidHeader = T,
        collapsible = T,
        fluidRow(class = "vertical-align",
                 column(4, selectizeInput("trade_out", "Trading out:", choices = team_trade_value()$playerID %>% set_names(glue::glue_data(team_trade_value(), "{Name} - ${`Total Cost`}")), multiple = T)),
                 column(4, class = "text-center", div(class = "treasury", glue::glue("Treasury: ${treasury[[user()]]}"))),
                 column(4, selectizeInput("trade_in", "Trading in:", choices = NULL, multiple = T))
        ),
        uiOutput("trade_summary"),
        conditionalPanel('input.trade_out != null', h3("Potential Trades"), withSpinner(DT:::dataTableOutput("trade_scouter")))
      )
    )
  })
  
  output$trade_scouter <- DT::renderDataTable({
    validate(need(available_trades(), message = F))
    
    DT::datatable(
      available_trades() %>% 
        select(Region, Division = comp, Name, Team, Race, Type, Level, FP, `Total Cost`, skills, old_injuries, new_injuries) %>% 
        mutate(
          Division = str_remove(Division, "Season 9 - Division "),
          skills = map_chr(
            skills, ~ifelse(
              is_empty(.), 
              NA, 
              map_chr(., ~glue::glue("<img class='skillimg' src='img/skills/{.}.png' title='{pretty_skills(.)}' />")) %>% glue::collapse()
            )
          ),
          old_injuries = map_chr(
            old_injuries, ~ifelse(
              is_empty(.),
              NA,
              map_chr(., ~glue::glue("<img class='skillimg' src='img/skills/{.}.png' title='{.}' />")) %>% glue::collapse()
            )
          ),
          new_injuries = map_chr(
            new_injuries, ~ifelse(
              is_empty(.),
              NA,
              map_chr(.[!"BH"%in%.], ~glue::glue("<img class='skillimg' src='img/skills/{.}.png' title='{.}' />")) %>% glue::collapse()
            )
          )
        ),
      rownames = F,
      class="display compact",
      escape = c(-10,-11,-12),
      extensions = "Scroller",
      options = list(
        deferRender=T,
        dom = "tip",
        scrollX = T,
        scrollY = 200,
        pageLength = 20,
        scroller = list(loadingIndicator=T),
        ordering = F
        #autoWidth = T,
        #columnDefs = list(list(targets = c(0), width = "20px"), list(targets = c(1:5), width = "10%"), list(targets = c(6:(gameweek+5)), width = "15%"))
      )
    )
  })
  
  
  observeEvent(available_trades(), {
    updateSelectizeInput(session, "trade_in", choices = available_trades()$playerID %>% set_names(glue::glue_data(available_trades(), "{Name} - ${`Total Cost`}")), selected = input$trade_in)
  })
  
  observeEvent(c(input$change_special, input$special_confirm), {
    if(input$change_special == "Captain") {
      updateSelectizeInput(session, "special_picker", label = "New Captain:", choices = team_overview() %>% filter(Special %in% c(NA, NA_character_), Type != "Werewolf") %>% arrange(desc(`Total Cost`)) %>% use_series(Player))
    } else {
      updateSelectizeInput(session, "special_picker", label = "New Reserve(s):", choices = team_overview() %>% filter(Special %in% c(NA,'r')) %>% arrange(desc(`Total Cost`)) %>%  use_series(Player))
    }
  })
  
  observeEvent(input$special_confirm, {
    if(input$change_special == "Captain") {
      
      changes <- next_round_team() %>% 
        mutate(Special = case_when(
          Special == 'c' ~ NA_character_,
          Player %in% input$special_picker ~ "c",
          T ~ Special
        ))
      cat(paste0(now(tzone="UTC"), "\t", user(), " changing captain ", next_round_team() %>% filter(Special == "c") %>% .$Player, " for ", input$special_picker ,"\n"), file = "data/logs/special_changes.log", append = T)
      read_csv("data/fantasy_teams.csv") %>% 
        filter(Coach != user() | Round != ifelse(gameweek < 2, "3", as.character(gameweek + 1))) %>% 
        bind_rows(changes) %>% 
        write_csv("data/fantasy_teams.csv")
    } else {
      changes <- next_round_team() %>% 
        mutate(Special = case_when(
          Special == 'r' & !Player %in% input$special_picker ~ NA_character_,
          Player %in% input$special_picker ~ "r",
          T ~ Special
        ))
      
      cat(paste0(now(tzone="UTC"), "\t", user(), " changing reserves ", next_round_team() %>% filter(Special == "r") %>% .$Player %>% glue::collapse(", "), " for ", glue::collapse(input$special_picker, ", "), "\n"), file = "data/logs/special_changes.log", append = T)
      
      read_csv("data/fantasy_teams.csv") %>% 
        filter(Coach != user() | Round != ifelse(gameweek < 2, "3", as.character(gameweek + 1))) %>% 
        bind_rows(changes) %>% 
        write_csv("data/fantasy_teams.csv")
    }
  })
  
  observe({
    validate(need(input$change_special, message = F), need(user_team(), message = F))
    
    if(input$change_special == "Captain") {
      if(length(input$special_picker) != 1) {
        removeClass("special_confirm", "btn-success")
        disable("special_confirm")
      } else {
        addClass("special_confirm", "btn-success")
        enable("special_confirm")
      }
    }
    
    if(input$change_special == "Reserves") {
      if(length(input$special_picker) != nrow(next_round_team()) - 11) {
        removeClass("special_confirm", "btn-success")
        disable("special_confirm")
      } else {
        addClass("special_confirm", "btn-success")
        enable("special_confirm")
      }
    }
  })
  
  #output$debug = renderText(input$trade_out)
  #output$debug2 = renderTable(reactiveValuesToList(user_created_team) %>% compact %>% bind_rows)
})
