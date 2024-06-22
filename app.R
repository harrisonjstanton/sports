#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(dplyr)
library(shinyFeedback)
library(rsconnect)
library(shinymeta)
library(kableExtra)


hvt_tt_long_df <- read_csv("hvt_long.csv")
hvt_tt_df <- read_csv("hvt.csv")
teams <- unique(hvt_tt_long_df$team)
teams <- sort(teams)
rb_stats_df <- read_csv("RBstats.csv")
column_names <- c("Total_touches", "Total_HVT", "TRAP", "total_fantasy_points", "total_fantasy_points_exp", "games", "ppg", "xppg", "HVT_perc", "HVT_per_game", "Touches_per_game")



ui <- fluidPage(
  
  # Application title
  titlePanel("Fantasy Running Backs Analysis"),
  
  #add fluid row here
  #need to get the text to work

  tabsetPanel(
    tabPanel("Introduction",
             fluidRow(
               column(width = 10, offset = 1, 
                      tags$style(".custom-padding { padding-top: 20px; }"),  # Define a custom CSS class for padding-top
                      tags$div(class = "custom-padding"),
                      tags$p(style = "text-align: left; font-size: 16px; line-height: 1.5;",  # Center and style
                             "Trivial Rush Attempt Percentage (TRAP) is a stat invented by Ben Gretch that is used to help managers target running backs in fantasy football. The stat tells a manager what percentage of touches a running back had that were not high value touches, meaning not carries inside the 10 yard line or receptions. 
                             There are 2 effective ways in which this stat is used: first, TRAP can help identify which running back in a split backfield is worth having or which handcuff running back to roster. 
                             The second way is that it can help you differentiate between backs who get the same amount of touches per week."
                      ),
                      tags$p(style = "text-align: left; font-size: 16px; line-height: 1.5;",  # Style the second paragraph
                             "The purpose of this app is to help people to interact with this data visually, and more effectively target which running backs they should roster."
                      ),
                      tags$p(style = "text-align: left; font-size: 16px; line-height: 1.5;",  # Style the second paragraph
                             "The most effective way, in my opinion, to use the TRAP Bar Chart tab is to filter by team. This helps you to identify which running backs in a split backfield, or which handcuff, to roster.
                              For example, if you filter to the Steelers, you will see that Jaylen Warren and Najee Harris were the two primary backs for the Steelers this year.
                              And, while Najee Harris received more touches per game than Jaylen Warren did, he received 1 fewer high value touch per game.
                              This is reflected in their total points per game outputs for the season."
                      ),
                      tags$p(style = "text-align: left; font-size: 16px; line-height: 1.5;",  # Style the second paragraph
                             "The most effective way to use the Running Back Scatterplots tab is to look at 3 graphs.
                              The first is HVT per game vs. Touches per game. This graph is a visual demonstration of why not all touches are built the same.
                              If you select a range of running backs who all have similar touches per game but different high value touches per game using the brush tool, and then view
                              their stats in the table below, you will see that the running backs who consistently get more high value touches per game are the ones who score more points.
                              This would lead you to value the guys who are above the line more highly than the guys who are below the line, as long as they are getting the same touches per game.
 "
                      ),
                      tags$p(style = "text-align: left; font-size: 16px; line-height: 1.5;",  # Style the second paragraph
                             "The two other graphs to look at are PPG vs. HVT per game and PPG vs. Touches per game. The most important piece of information from these two graphs actually
                              comes from the slope of the best fit line. As you will see, the slope of the best fit line for the first graph is 3.19, and the slope for the second is 0.83.
                              This is a great way to see why high value touches are important, because they lead to roughly 3 times more fantasy points than touches per game. Additionally,
                              another important takeaway is that touches per game is not unimportant. Volume can still drive scoring."
                      ),
                       tags$div(class = "custom-padding"),
               )),
             ),
    tabPanel("TRAP Bar Chart",
       #     fluidRow(
       #       column(width = 10, offset = 1, 
       #        tags$style(".custom-padding { padding-top: 20px; }"),  # Define a custom CSS class for padding-top
       #        tags$div(class = "custom-padding"),
       #        tags$p(style = "text-align: left; font-size: 16px; line-height: 1.5;",  # Style the second paragraph
       #               "The most effective way, in my opinion, to use the TRAP Bar Chart tab is to filter by team. This helps you to identify which running backs in a split backfield, or which handcuff, to roster.
       #                For example, if you filter to the Steelers, you will see that Jaylen Warren and Najee Harris were the two primary backs for the Steelers this year.
       #                And, while Najee Harris received more touches per game than Jaylen Warren did, he received 1 fewer high value touch per game.
       #                This is reflected in their total points per game outputs for the season."
       #     ),
       #     tags$div(class = "custom-padding"),
       #   ), 
       # ),
           fluidRow(
             sidebarLayout(
               sidebarPanel(
                 selectInput("user_choice", "Choose between Tier and Team", choices = c("Tier", "Team"), selected = "Tier"),
                 uiOutput("user_rank_input"),
                 checkboxInput("data", "View RBs season stats"),
               ),


               mainPanel(
                 plotOutput("histogram"),
               )
             ),
           ),
           fluidRow(
             column(10,
              tableOutput("table")
             )
           ),
    ),

    #need to get text to work for this as well
    tabPanel("Running Back Scatterplots",
    #   fluidRow(
    #     column(width = 10, offset = 1,
    #        tags$style(".custom-padding { padding-top: 20px; }"),  # Define a custom CSS class for padding-top
    #        tags$div(class = "custom-padding"),
    #        tags$p(style = "text-align: left; font-size: 16px; line-height: 1.5;",  # Style the second paragraph
    #          "The most effective way to use the Running Back Scatterplots tab is to look at 3 graphs.
    #           The first is HVT per game vs. Touches per game. This graph is a visual demonstration of why not all touches are built the same.
    #           If you select a range of running backs who all have similar touches per game but different high value touches per game using the brush tool, and then view
    #           their stats in the table below, you will see that the running backs who consistently get more high value touches per game are the ones who score more points.
    #           This would lead you to value the guys who are above the line more highly than the guys who are below the line, as long as they are getting the same touches per game.
    # "
    #       ),
    #       tags$p(style = "text-align: left; font-size: 16px; line-height: 1.5;",  # Style the second paragraph
    #          "The two other graphs to look at are PPG vs. HVT per game and PPG vs. Touches per game. The most important piece of information from these two graphs actually
    #           comes from the slope of the best fit line. As you will see, the slope of the best fit line for the first graph is 3.19, and the slope for the second is 0.83.
    #           This is a great way to see why high value touches are important, because they lead to roughly 4 times more fantasy points than touches per game. Additionally,
    #           another important takeaway is that touches per game is not unimportant. Volume can still drive scoring."
    #     ),
    #     tags$div(class = "custom-padding"),
    #   ),
    #   ),
      fluidRow(
        sidebarLayout(
         sidebarPanel(
           selectInput("x", "Choose x-axis", choices = column_names, selected = "Touches_per_game"),
           selectInput("y", "Choose y-axis", choices = column_names, selected = "HVT_per_game"),
           checkboxInput("user_equation", "Show equation of line"),
           checkboxInput("see_stats", "Show season stats")
         ),
         
         
         mainPanel(
           plotOutput("scatter", brush = "plot_brush"),
           textOutput("equation"),
           textOutput("rsquared")
         ))
       ), 
      fluidRow(
        tableOutput("data")
      )
    ),
    tabPanel("Glossary", 
             fluidRow(4, offset = 4,
               #might want to center this... that's the last thing I want to change on this tab
               uiOutput("glossaryTable")
             ),
    ),
  ),
)



server <- function(input, output) {
  
  #text for first tab
  output$trapExplanation <- renderText({
    
    "The stat tells a manager what percentage of touches a running back had that were not high value touches, meaning not carries inside the 10 yard line or receptions. 
    There are 2 effective ways in which this stat is used: first, TRAP can help identify which running back in a split backfield is worth having or which handcuff running back to roster. 
    The second way is that it can help you differentiate between backs who get the same amount of touches per week.

  The purpose of this app is to help people to interact with this data visually, and more effectively target which running backs they should roster.

  The most effective way, in my opinion, to use the TRAP Bar Chart tab is to filter by team. This helps you to identify which running backs in a split backfield, or which handcuff, to roster. 
  For example, if you filter to the Steelers, you will see that Jaylen Warren and Najee Harris were the two primary backs for the Steelers this year. 
  And, while Najee Harris received more touches per game than Jaylen Warren did, he received 1 fewer high value touch per game. 
  This is reflected in their total points per game outputs for the season.

  The most effective way to use the Running Back Scatterplots tab is to look at 3 graphs. 
  The first is HVT per game vs. Touches per game. This graph is a visual demonstration of why not all touches are built the same. 
  If you select a range of running backs who all have similar touches per game but different high value touches per game using the brush tool, and then view 
  their stats in the table below, you will see that the running backs who consistently get more high value touches per game are the ones who score more points. 
  This would lead you to value the guys who are above the line more highly than the guys who are below the line, as long as they are getting the same touches per game. 
 
The two other graphs to look at are PPG vs. HVT per game and PPG vs. Touches per game. The most important piece of information from these two graphs actually 
    comes from the slope of the best fit line. As you will see, the slope of the best fit line for the first graph is 3.19, and the slope for the second is 0.83.
    This is a great way to see why high value touches are important, because they lead to roughly 3 times more fantasy points than touches per game. Additionally, 
    another important takeaway is that touches per game is not unimportant. Volume can still drive scoring. "

  })
  
  styled_table_content <- reactive({
    data <- data.frame(
      Term = c("PPG", "xPPG", "pos_rank", "ATT", "rush_yds", "Y/A", "LG", "20+", 
               "rush_TD", "REC", "TGT", "rec_yds", "Y/R", "rec_TD", "FL", "HVT", 
               "Total_touches", "TRAP"),
      Definition = c("Fantasy points per game", "Expected fantasy points pergame",
                     "Position Rank: sorted by total points, sorted by position", 
                     "Rushing attempts on the season", "Rushing yards on the season", 
                     "Yards per attempt", "Longest rush of the season", 
                     "Number of rushes over 20 yards", "Rushing Touchdowns", "Receptions", 
                     "Targets", "Receiving yards", "Yards per reception", 
                     "Receiving Touchdowns", "Fumbles lost", 
                     "High value touches: rushes within the 10 yard line plus receptions", 
                     "Receptions plus carries", 
                     "Trivial Rush Attempt Percentage: (Total touches - HVT) / Total touches")
    )
    
    # Create the HTML table with alternating row colors
    styled_table <- kable(data, "html") %>%
      kable_styling("striped", full_width = FALSE) %>%
      column_spec(1, bold = TRUE)  # Make the first column bold
    return(styled_table)
  })
  
  output$glossaryTable <- renderUI({
    HTML(styled_table_content())
  })
  
  #warning when both inputs are same on second graph
  observe({
    if(input$x == input$y){
      showNotification("You have selected the same variable for x-axis and y-axis", type = "warning")
    }
  })
  
  #conditional drop down tab on first tab
  output$user_rank_input <- renderUI({
    if(input$user_choice == "Team"){
      selectInput("user_team", "Choose Team", choices = teams, selected = "ARI")
    }
    else if(input$user_choice == "Tier"){
      selectInput("user_rank", "Choose RB Tier", choices = c("RB1", "RB2", "RB3"), selected = "RB1")
    }
    
  })
  
  #equation of scatterplot
  output$equation <- renderText({
    req(input$user_equation)
    lm_model <- lm(rb_stats_df[[input$y]] ~ rb_stats_df[[input$x]])
    
    slope <- coef(lm_model)[2]
    intercept <- coef(lm_model)[1]
    
    equation_string <- paste0("y = ", round(slope, 2), "x + ", round(intercept, 2))
    return(equation_string)
  })
  
  output$rsquared <- renderText({
    if (input$user_equation) {
      model <- lm(rb_stats_df[[input$y]] ~ rb_stats_df[[input$x]])
      paste("R-squared is:", round(summary(model)$r.squared, 3))
    }
  })
  

  #graph for first tab
  output$histogram <- renderPlot({
    

    if (input$user_choice == "Tier") {
      names <- hvt_tt_long_df %>%
        filter(Rank == input$user_rank) %>%
        distinct(gsis_id, .keep_all = TRUE) %>%
        pull(full_name)

        names <- rev(names)
        
      #table for rank input, first tab
      hvt_tt_long_df %>%
        filter(Rank == input$user_rank) %>%
        ggplot(aes(x = full_name, y = percentage, fill = Touch_type)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual(values = c("springgreen4", "brown2"), name = "Touch Type",
                          labels = c("High Value Touches", "Trivial Rushes")) +
        labs(x = "", y = "Percentage") +
        theme(
          plot.title = element_text(size = 24),
          axis.title = element_text(size = 18), 
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.title = element_text(size = 16), 
          legend.text = element_text(size = 14),
        ) +
        scale_x_discrete(limits = names) +
        ggtitle(paste0(input$user_rank, "'s TRAP for 2023")) +
        geom_text(aes(y = label_y, label = scales::percent(percentage, accuracy = .01)), color = 'white') +
        scale_y_continuous(labels = scales::percent)
    }
    else if(input$user_choice == "Team"){
      names <- hvt_tt_long_df %>%
        filter(team == input$user_team) %>%
        distinct(gsis_id, .keep_all = TRUE) %>%
        pull(full_name)

       names <- rev(names)
       
      #table for teams of first tab
       hvt_tt_long_df %>%
        filter(team == input$user_team) %>%
        ggplot(aes(x = full_name, y = percentage, fill = Touch_type)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual(values = c("springgreen4", "brown2"), name = "Touch Type",
                          labels = c("High Value Touches", "Trivial Rushes")) +
        labs(x = "", y = "Percentage") +
         theme(
           plot.title = element_text(size = 24),
           axis.title = element_text(size = 18), 
           axis.text.x = element_text(size = 14),
           axis.text.y = element_text(size = 14),
           legend.title = element_text(size = 16), 
           legend.text = element_text(size = 14),
         ) +
        scale_x_discrete(limits = names) +
        ggtitle(paste0(input$user_team, "'s TRAP for 2023")) +
        geom_text(aes(y = label_y, label = scales::percent(percentage, accuracy = .01)), color = 'white') +
        scale_y_continuous(labels = scales::percent)
    }
    
    
  })
  
  #data for first tab
  output$table <- renderTable({
    if(input$data){
      if(input$user_choice == "Tier"){
        rb_stats_df %>%
          filter(rank == input$user_rank) %>%
          select(full_name, team, games, ppg, xppg, pos_rank, ATT, rush_yds, "Y/A", LG, "20+", rush_TD, REC, TGT, rec_yds, "Y/R", rec_TD, HVT_per_game, Touches_per_game)
      }
      else if(input$user_choice == "Team"){
        rb_stats_df %>%
          filter(team == input$user_team) %>%
          select(full_name, team, games, ppg, xppg, pos_rank, ATT, rush_yds, "Y/A", LG, "20+", rush_TD, REC, TGT, rec_yds, "Y/R", rec_TD, HVT_per_game, Touches_per_game)
      }
    }
  })
  
  #data for second tab
  output$data <- renderTable({
    req(input$see_stats)
    brushedPoints(rb_stats_df %>% select(full_name, team, games, ppg, xppg, pos_rank, ATT, rush_yds, "Y/A", "20+", rush_TD, REC, TGT, rec_yds, "Y/R", rec_TD, FL, input$x, input$y), input$plot_brush)
    
  })
  
  #scatterplot for second tab
  output$scatter <- renderPlot({
    rb_stats_df %>%
      ggplot(aes_string(x = input$x, y = input$y)) +
        geom_point() +
        xlab(input$x) +
        ylab(input$y) +
        ggtitle(paste0(input$y, " vs. ", input$x)) +
        geom_smooth(method = "lm", se = FALSE) +
      theme(
        plot.title = element_text(size = 24),
        axis.title = element_text(size = 18), 
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14),
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

