library(tidyverse
        , shiny
        )

Income_Calc <- function(Wage
                        , Hours) {
  Wage * Hours
}

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  tabsetPanel(
    tabPanel("Introduction",
             titlePanel("Social Assistance Income Calculator"),
             textOutput("Intro")
             ),
    tabPanel("About Me",
             fluidRow(
               column(3
                      , textOutput("Scenario_1")
                      , numericInput("Wage_1"
                                     , "Wage"
                                     , value = 17.20
                                     , min = 17.20
                                     , max = 100
                      )
                      , numericInput("Hours_1"
                                     , "You can change hours here to see another employment scenario"
                                     , value = 0
                                     , min = 0
                                     , max = 60
                      )
               )
               , column(3
                        , textOutput("Scenario_2")
                        , numericInput("Wage_2"
                                       , "Wage"
                                       , value = 17.20
                                       , min = 17.20
                                       , max = 100
                        )
                        , numericInput("Hours_2"
                                       , "Part-time hours could me more or less than 17 per week"
                                       , value = 17
                                       , min = 0
                                       , max = 60
                        )
               )
               , column(3
                        , textOutput("Scenario_3")
                        , numericInput("Wage_3"
                                       , "Wage"
                                       , value = 17.20
                                       , min = 17.20
                                       , max = 100
                        )
                        , numericInput("Hours_3"
                                       , "Full-time typically starts at 30 hours per week. Overtime starts after 44 hours per week"
                                       , value = 35
                                       , min = 0
                                       , max = 60
                        )
               )
             )
             , actionButton("Calculate"
                            , "Calculate"
             ) 
             ),
    tabPanel("Income Results",
             fluidRow(
               column(9
                      , plotOutput("Income_Plot"
                      )
               )
             )
             )
  )
)
server <- function(input, output, session) {
  output$Scenario_1 <- renderText("Unemployed")
  output$Scenario_2 <- renderText("Employed Part Time")
  output$Scenario_3 <- renderText("Employed Full Time")
  Calcd_Income_1 <- eventReactive(input$Calculate, {
    Income_Calc(input$Wage_1
                 , input$Hours_1
    )
    })
    Calcd_Income_2 <- eventReactive(input$Calculate, {
      Income_Calc(input$Wage_2
                  , input$Hours_2
                  )
                                                      }
                                    )
    Calcd_Income_3 <- eventReactive(input$Calculate, {
      Income_Calc(input$Wage_3
                  , input$Hours_3
                   )
    })
    Income_Tibble <- reactive(tibble(Calcd_Income = c(Calcd_Income_1()
                                                          , Calcd_Income_2()
                                                          , Calcd_Income_3()
                                                          )
                                     , Scenario = c(1:3)
                                     )
                              )
    output$Income_Plot <- renderPlot({
      ggplot(Income_Tibble(), aes(x = Scenario, y = Calcd_Income)) +
        geom_col()
    }, res = 96
    )
}
shinyApp(ui, server)