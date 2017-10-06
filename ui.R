# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(DT)

dashboardPage(skin="black",
              dashboardHeader(title = span(tagList(a(href="https://www.reddit.com/r/rebbl", img(src = "img/ReBBL_logo_800px_72dpi.png", width = "70px")),"Fantasy"))),
              dashboardSidebar(
                sidebarMenu(
                  id = "tabs",
                  menuItem("Leaderboard", tabName = "leaderboard", icon = icon("trophy", class = "fa-fw")),
                  menuItem("Teams", tabName = "teams", icon = icon("user", class = "fa-fw")),
                  menuItem("Stats", tabName = "stats", icon = icon("bar-chart", class = "fa-fw"))
                )
              ),
              dashboardBody(
                tabItems(
                  # First tab content
                  tabItem(tabName = "leaderboard",
                          box(
                            title = "Current Standings",
                            width = 12,
                            h3("Leaderboard coming soon")
                          )
                  ),
                  
                  # Second tab content
                  tabItem(tabName = "teams",
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
                          )#,
                          # fluidRow(
                          #   infoBoxOutput("best_game", width = 3),
                          #   conditionalPanel(
                          #     "input.team_summary_table_rows_selected != ''",
                          #     box(
                          #       width = 6,
                          #       title = "Week by week",
                          #       plotOutput("points_bar")
                          #     )
                          #   ),
                          #   infoBoxOutput("worst_game", width = 3)
                          # )
                  ),
                  # Third tab content 
                  tabItem(tabName = "stats",
                          fluidRow(
                            box(
                              title = "Averaged statistics",
                              width = 12,
                              collapsible = T,
                              DT::dataTableOutput("stats_table")
                            )
                          ),
                          fluidRow(
                            infoBoxOutput("best_game", width = 3),
                            conditionalPanel(
                              "input.stats_table_rows_selected != ''",
                              box(
                                width = 6,
                                title = "Week by week",
                                plotOutput("points_bar")
                              )
                            ),
                            infoBoxOutput("worst_game", width = 3)
                          )
                  )
                )
              )
)