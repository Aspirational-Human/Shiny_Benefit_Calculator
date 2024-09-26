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
               h2("Welcome to the Ontario Social Assistance Income Calculator"),
               HTML("This online calculator can help you estimate how income from work will affect:
                    <ul>
                    <li>social assistance payments from <strong>Ontario Works</strong> or <strong>Ontario Disability Support Program (ODSP)</strong></li>
                    <li>income tax refunds</li>
                    <li>other benefit payments from federal and provincial programs</li>
                    </ul>
                    This calculator is <strong>free</strong> to use and does <strong>not</strong> collect or store your personal information. This calculator is made avaiable without any warranty as to the accuracy of its calculations.
                    <br>
                    <br>
                    There are three steps to using this calculator:
                    <ol>
                    <li> Fill out the information in the <strong>About Me</strong> tab to make the calculations relevant to you and your family.</li>
                    <li> Provide three <strong>Work Scenarios</strong> for which you would like income, social assistance, tax and other benefit estimates.</li>
                    <li> Review the <strong>Income Results</strong> tab to see estimates for each work scenario and go back and adjust the work scenarios as needed.</li>"
                    )
             )
    ),
    tabPanel("About Me",
             fluidPage(
               h2("About Me"),
               HTML("Please answer all of the questions on this page to make the calculator results relevant to your situation.<br>"),
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
               ),
               fluidRow(
                 column(6,
                        numericInput("Dependents",
                                    "How many dependents are in your household?",
                                    value = 0
                                    , min = 0
                                    , max = 12
                                    )
                        )
               )
             )
    ),
    tabPanel("Work Scenarios",
             fluidPage(
               h2("Work Scenarios"),
               HTML("On this page you can customize three work scenarios to see how different employment situations will affect your income, social assistance, tax and other benefit payments.
                    <ul>
                    <li><strong>Scenario 1</strong> has been set to your current situation.</li>
                    <li><strong>Scenario 2</strong> has been set to part-time employment at minimum wage.</li>
                    <li><strong>Scenario 3</strong> has been set to full-time employment at minimum wage.</li>
                    </ul>"
                    ),
             fluidRow(
               column(4, 
                      h3("Scenario 1"),
                      radioButtons("Format_1"
                                   , "Prefered income format"
                                   , c("Hourly Wage", "Monthly take-home pay")
                                   )
                      ),
               column(4, h3("Scenario 2")),
               column(4, h3("Scenario 3"))
               ),
             # fluidRow(
             #   column(2, actionButton("Wage_Button_1", "Hourly Wage")),
             #   column(2, actionButton("Income_Button_1", "Take Home Pay"))
             # ),
             fluidRow(
               column(4,
                      # , h3("Scenario 1")
                      numericInput("Wage_1"
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
               , column(4
                        # , h3("Scenario 2")
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
               , column(4
                        # , h3("Scenario 3")
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
             )
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
  observeEvent(input$Spouse, {
    label <- if(input$Spouse == "Yes") {
      "How many dependents (not including your spouse) are in your household?"
    } else {
      "How many dependents are in your household?"
    }
    updateNumericInput(session = getDefaultReactiveDomain(), "Dependents", label = label)
  })
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