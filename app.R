# Packages
library(tidyverse,
        readr)
library(shiny, 
        shinyFeeedback)
library(scales)
library(shinycssloaders)

# Constants
Min_Wage <- 17.2
# Functions
Income_Calc <- function(Wage
                        , Hours) {
  Wage * Hours
}

ui <- fluidPage(
  # Setting the look and feel of the app.
  theme = bslib::bs_theme(bootswatch = "united"),
  # Setting up the ability to provide dynamic user feedback.
  shinyFeedback::useShinyFeedback(),
  # Creating tabs to organize the content of the shiny app. 
  navbarPage(
    titlePanel("Social Assistance Income Calculator"),
    tabPanel("Introduction",
             fluidPage(
             textOutput("App_Intro")
             )
    ),
    tabPanel("About Me",
             fluidPage(
               fluidRow(
                 column(5,
                        selectInput("Program",
                                    "Which social assistance program do you want to use for calculations?",
                                    c("Ontario Works", "Ontario Disability Support Program (ODSP)")
                                    )
                        )
               ),
               fluidRow(
                 column(5, 
                        selectInput("Spouse",
                                      "Do you have a spouse or common law partner?",
                                      c("No", "Yes")
                        )
                 )
               ),
               fluidRow(
                 column(6,
                        conditionalPanel(condition = "input.Program == 'Ontario Disability Support Program (ODSP)' && input.Spouse == 'Yes'",
                        selectInput("User_Disability",
                                    "Has ODSP determined that you have a disability?",
                                    c("Yes", "No")
                                    )
                        )
                        ),
                 column(6,
                        conditionalPanel(
                          condition = "input.Program == 'Ontario Disability Support Program (ODSP)' && input.Spouse == 'Yes'",
                          selectInput("Spouse_Disability",
                                      "Has ODSP determined that your spouse has a disability?",
                                      c("No", "Yes")
                          )
                   )
                 )
               )
             )
    ),
    tabPanel("Work Scenarios",
             fluidRow(
               column(3
                      , textOutput("Scenario_1")
                      , numericInput("Wage_1"
                                     , "Wage"
                                     , value = 17.20
                                     , min = 0
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
                                       , min = 0
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
                                       , min = 0
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
             # , actionButton("Calculate"
             #                , "Calculate"
             # ) 
             ),
    tabPanel("Income Results",
             fluidRow(
               column(9
                      , withSpinner(plotOutput("Income_Plot"
                                               )
                                    )
                      )
                      )
             ),
    tabPanel("More Information")
  )
)
server <- function(input, output, session) {
  # Error messages
  observeEvent(input$Wage_1, {
    shinyFeedback::feedbackWarning("Wage_1",
                                   input$Wage_1 < Min_Wage,
                                   paste0("Ontario minimum wage is ",
                                          dollar(Min_Wage),
                                         " per hour."
                                          )
                                   )
  })
  observeEvent(input$Wage_2, {
    shinyFeedback::feedbackWarning("Wage_2",
                                   input$Wage_2 < Min_Wage,
                                   paste0("Ontario minimum wage is ",
                                          dollar(Min_Wage),
                                          " per hour."
                                   )
    )
  })
  observeEvent(input$Wage_3, {
    shinyFeedback::feedbackWarning("Wage_3",
                                   input$Wage_3 < Min_Wage,
                                   paste0("Ontario minimum wage is ",
                                          dollar(Min_Wage),
                                          " per hour."
                                   )
    )
  })
  output$Intro <- renderText("This app is designed to help you understand how your income changes based on your employment status.")
  output$Scenario_1 <- renderText("Unemployed")
  output$Scenario_2 <- renderText("Employed Part Time")
  output$Scenario_3 <- renderText("Employed Full Time")
  Calcd_Income_1 <- reactive({
    # Legal_Wage <- input$Wage_1 >= 17.2
    # shinyFeedback::feedbackWarning("Wage_1", 
    #                                !Legal_Wage, 
    #                                "Ontario minimum wage is $17.20 per hour."
    #                                )
    # req(Legal_Wage, cancelOutput = TRUE)
    
    Income_Calc(input$Wage_1,
                input$Hours_1
    )
    })
    Calcd_Income_2 <- reactive({
      Income_Calc(input$Wage_2
                  , input$Hours_2
                  )
                                                      }
                                    )
    Calcd_Income_3 <- reactive({
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