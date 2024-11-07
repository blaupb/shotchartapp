# install/load libraries
# install.packages('shiny')
library(shiny)
# install.packages('shinydashboard')
library(shinydashboard)
# install.packages('tidyverse')
library(tidyverse)
# install.packages('nbastatR')
library(nbastatR)
# install.packages('ggthemes')
library(ggthemes)
# install.packages('plotly')
library(plotly)

# load data
load('shotdata.RData')

# RUN TO MAKE APP WORK
Sys.setenv('VROOM_CONNECTION_SIZE' = 131072 * 2)

# Set up the dashboard UI
ui <- dashboardPage(
  # theme
  skin = 'black',
  
  # title
  dashboardHeader(title = 'NBA Shot Analysis App'),
  
  # create sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem('Player Shot Charts', tabName = 'player_shots', icon = icon('basketball-ball'))
    ),
    
    # team input
    selectInput(
      inputId = 'team_pick',
      label = 'Select Team',
      choices = nba_teams$nameTeam
    ),
    
    # season input
    selectInput(
      inputId = 'szn',
      label = 'Select Season',
      choices = 1997:2025,
      selected = 2025
    ),
    
    # player output determined on team, season
    uiOutput(outputId = 'player'),
    
    # game date outcome based on player
    uiOutput(outputId = 'date')
  ),
  
  # dashboard body --> plot output
  dashboardBody(
    tabItems(
      # plotly output; interactive graph
      tabItem(tabName = 'player_shots', 
              plotlyOutput('shot_chart'), 
              tableOutput('summary'),
              tableOutput('summary2'))
      )
    )
  )


# server logic
server <- function(input, output) {
  
  # player UI
  output$player <- renderUI({
    
    # filter names based on team, season input
    names <- teams_shots(teams = as.character(input$team_pick), 
                         seasons = as.numeric(input$szn)) %>%
      dplyr::select(namePlayer) %>%
      unique() %>%
      arrange(namePlayer)
    
    # player input
    selectInput(
      inputId = 'player',
      label = 'Select Player',
      choices = names$namePlayer
    )
  })
  
  # date UI
  output$date <- renderUI({
    
    # filter dates based on team, season, player input
    dates <- teams_shots(teams = as.character(input$team_pick),
                         seasons = as.numeric(input$szn)) %>%
      filter(namePlayer == as.character(input$player)) %>%
      dplyr::select(dateGame) %>%
      mutate(dateGame = ymd(dateGame)) %>%
      arrange(dateGame)
    
    # date range input
    dateRangeInput(
      inputId = 'date',
      label = 'Date Range',
      start = min(dates$dateGame),
      end = max(dates$dateGame),
      min = min(dates$dateGame),
      max = max(dates$dateGame),
      actionButton('submit', 'Submit')
    )
    

  })
  
  # plotly object of shot chart
  output$shot_chart <- renderPlotly({
    
    # shots data using nbastatR
    tm_shots <- teams_shots(teams = as.character(input$team_pick), 
                            seasons = as.numeric(input$szn)) %>%
      mutate(dateGame = ymd(dateGame))
    
    # final table, scaled x/y for mapping + mutations
    final_shots <- tm_shots %>%
      filter(namePlayer == as.character(input$player) & 
               dateGame >= as.Date(input$date[1]) &
               dateGame <= as.Date(input$date[2])) %>%
      mutate(x = as.numeric(as.character(locationX)) / 10,
             y = as.numeric(as.character(locationY)) / 10 + hoop_center_y)
    final_shots$x <- final_shots$x * -1
    
    # ggplot object of court, including interactive text
    p1 <- plot_court(court_themes$light, use_short_three = FALSE) +
      geom_point(data = final_shots, aes(x = x, y = y, color = isShotMade, fill = isShotMade,
                                         text = paste('Date: ',
                                                      format(dateGame, '%B %d'),
                                                      '<br>',
                                                      'Game:',
                                                      slugTeamAway,
                                                      'at',
                                                      slugTeamHome,
                                                      '<br>',
                                                      'Made Shot:', 
                                                      isShotMade, 
                                                      '<br>',
                                                      'Shot Type:', 
                                                      typeAction)),
                 size = 3, shape = 21, stroke = 0.5) +
      scale_color_manual(values = c('green4', 'red3'), 
                         breaks = c('TRUE', 'FALSE'), 
                         labels = c('Made', 'Missed'),
                         name = NULL) +
      scale_fill_manual(values = c('green3', 'red4'), 
                        breaks = c('TRUE', 'FALSE'), 
                        labels = c('Made', 'Missed'),
                        name = NULL) +
      scale_x_continuous(limits = c(-27.5, 27.5)) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'),
            plot.subtitle = element_text(hjust = 0.5, size = 14),
            legend.position = 'none',
            plot.caption = element_text(hjust = 0.5, size = 8, face = 'bold', color = 'black')) +
      ggtitle(label = paste0(as.character(input$player), ' Shot Chart from ', 
                             as.character(format(min(final_shots$dateGame), '%B %d, %Y')),
                             ' to ',
                             as.character(format(max(final_shots$dateGame), '%B %d, %Y'))))
    
    # convert to plotly
    interactive_plot <- ggplotly(p1, tooltip = 'text', source = 'select')
    
    # render/return object
    interactive_plot
  })
  
  # shot type field goal percentage table
  output$summary <- renderTable({
    
    # summary statistics from original data filtering
    tm_shots <- teams_shots(teams = as.character(input$team_pick), 
                            seasons = as.numeric(input$szn)) %>%
      mutate(dateGame = ymd(dateGame))
    
    # final_shots from above, minus the x/y mutation
    # since we are covering summary statistics
    final_shots <- tm_shots %>%
      filter(namePlayer == as.character(input$player) & 
               dateGame >= as.Date(input$date[1]) &
               dateGame <= as.Date(input$date[2])) 
    
    # summary stats manipulation
    shots_summary <- final_shots %>%
      dplyr::select(namePlayer, isShotAttempted, isShotMade, typeAction)
    
    # find total shots made
    shots_summary1 <- shots_summary %>%
      group_by(namePlayer, isShotMade, typeAction) %>%
      summarise(total = n())
    
    # find shots attempted
    shots_summary2 <- shots_summary %>%
      group_by(namePlayer, typeAction) %>%
      summarise(totalAll = n())
    
    # join makes, attempts tables
    sum_tbl <- shots_summary1 %>%
      left_join(
        shots_summary2,
        by = c('namePlayer', 'typeAction')
      )
    
    # find shooting percentage
    final_sum <- sum_tbl %>%
      filter(isShotMade == T) %>%
      mutate(pct = paste0(round((total/totalAll) * 100, 2), '%'))
    
    # create final table
    final_summary <- final_sum %>%
      dplyr::ungroup() %>%
      dplyr::select(typeAction, total, totalAll, pct)
    
    # make names readable in app
    names(final_summary) <- c('Shot Type', 'Makes', 'Attempts', 'Shot Percentage')
    
    # render table
    final_summary
    
  })
  
  # 2pt/3pt field goal percentage table
  output$summary2 <- renderTable({
    
    # summary statistics from original data filtering
    tm_shots <- teams_shots(teams = as.character(input$team_pick), 
                            seasons = as.numeric(input$szn)) %>%
      mutate(dateGame = ymd(dateGame))
    
    # final_shots from above, minus the x/y mutation
    # since we are covering more summary statistics
    final_shots <- tm_shots %>%
      filter(namePlayer == as.character(input$player) & 
               dateGame >= as.Date(input$date[1]) &
               dateGame <= as.Date(input$date[2])) 
    
    # find shots made, missed
    sum_2 <- final_shots %>%
      dplyr::select(namePlayer, typeShot, isShotMade) %>%
      mutate(made = ifelse(isShotMade == T, 1, 0),
             miss = ifelse(isShotMade == F, 1, 0))
    
    # create final summary table, find shooting percentage
    sum_2_final <- sum_2 %>%
      group_by(namePlayer, typeShot) %>%
      summarise(total_make = as.integer(sum(made)),
                total_miss = as.integer(sum(miss))) %>%
      mutate(attempts = total_make + total_miss,
             pct = paste0(round((total_make/attempts)*100, 2), '%')) %>%
      dplyr::ungroup() %>%
      select(typeShot, total_make, attempts, pct)
    
    # change names to make readable in app
    names(sum_2_final) <- c('FG Type', 'Makes', 'Attempts', 'Shot Percentage')
    
    # render table
    sum_2_final
    
  })
}

# app
shinyApp(ui, server)
