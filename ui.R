# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(shinycssloaders)

options(spinner.type=8, spinner.color = "#333333")

# Leaderboard content ----
leaderboard <- tabItem(tabName = "leaderboard",
                       fluidRow(
                         box(
                           title = "Current Standings",
                           width = 12,
                           DT::dataTableOutput("leaderboard")
                         ),
                         conditionalPanel(
                           "input.leaderboard_rows_selected != ''",
                           box(
                             width = 6,
                             plotOutput("weekly_bars")
                           ),
                           box(
                             width = 6,
                             plotOutput("weekly_lines")
                           )
                         )
                       )
)

# Teams tab content -----
team_summary <- tabItem(tabName = "teams",
                        fluidRow(
                          box(
                            title = "Coach:",
                            width = 6,
                            height = 100,
                            uiOutput("coach_select")
                          ),
                          box(
                            title = "Round:",
                            width = 6,
                            height = 100,
                            numericInput("selected_round", NULL, 1, min = 1, max = 13, step = 1)
                          )
                        ),
                        fluidRow(
                          box(
                            width = 12,
                            uiOutput("team_name"),
                            DT::dataTableOutput("team_summary")
                          )
                        )
)

# All stats tab content -----
overall_stats <- tabItem(tabName = "stats",
                         fluidRow(
                           tabBox(
                             title = "Player statistics",
                             id = "stats_tab",
                             width = 12,
                             selected = "Averaged",
                             tabPanel("Averaged", withSpinner(DT::dataTableOutput("averaged_stats_table"))),
                             tabPanel("All", withSpinner(DT::dataTableOutput("stats_table")))
                           )
                          ) #, #Removed for testing
                         # fluidRow(
                         #   infoBoxOutput("best_game_stats", width = 3),
                         #   conditionalPanel(
                         #     "(input.stats_tab == 'Averaged' & input.averaged_stats_table_rows_selected != '') | (input.stats_tab == 'All' & input.stats_table_rows_selected != '')",
                         #     box(
                         #       width = 6,
                         #       title = "Week by week",
                         #       plotOutput("points_bar_stats")
                         #     )
                         #   ),
                         #   infoBoxOutput("worst_game_stats", width = 3)
                         # )
)

# Team creation page --------
team_builder <- tabItem(tabName = "create", uiOutput("team_builder"))

# Trade helper page -----
trade_helper <- tabItem(tabName = "trade", uiOutput("trade_helper"))

# Page scaffold ----
dashboardPage(title = "REBBL Fantasy League",
              skin="black",
              dashboardHeader(
                title = span(tagList(a(href="https://www.reddit.com/r/rebbl", img(src = "img/ReBBL_logo_800px_72dpi.png", width = "70px")),"Fantasy")),
                tags$li(class = "dropdown",
                        tags$li(class="dropdown username", textOutput("username")),
                        tags$li(class="dropdown", actionLink("login", "Login", icon = icon("user-o", class = "fa-lg fa-fw")))
                )
              ),
              dashboardSidebar(
                sidebarMenu(
                  id = "tabs",
                  #menuItem("Leaderboard", tabName = "leaderboard", icon = icon("trophy", class = "fa-fw")),
                  #menuItem("Team Performance", tabName = "teams", icon = icon("user", class = "fa-fw")),
                  menuItem("Player Stats", tabName = "stats", icon = icon("bar-chart", class = "fa-fw")),
                  menuItemOutput("team_builder_menu"),
                  menuItemOutput("trade_helper_menu")
                )
              ),
              dashboardBody(
                includeCSS("www/css/google-font.css"),
                tags$head(
                  tags$link(rel="stylesheet", href="css/fantasy.css"),
                  tags$link(rel="stylesheet", href="css/google-font.css"),
                  tags$link(rel="stylesheet", href="css/animate.css")
                  ),
                useShinyjs(),
                tabItems(
                  overall_stats,
                  #leaderboard,
                  #team_summary,
                  team_builder,
                  trade_helper
                )
              )
)