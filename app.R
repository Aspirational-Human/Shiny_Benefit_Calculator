## Task for tomorrow:
# Change all income names to either Take_Home_Pay or Gross_Inc



# Packages
library(tidyverse)
library(shiny) 
library(shinyFeedback)
library(scales)
library(shinyWidgets)
library(shinycssloaders)
library(bslib)
library(bsicons)
library(thematic)
library(bsplus)
library(htmltools)
library(shinythemes)

# Income constants ----
# Ontario minimum wage as of October 2024
Min_Wage <- 17.2
# EI deduction rate for 2024
EI_Rate <- 0.0166
# Annual deduction cap for EI in 2024
EI_Cap_Y <- 1049.12
# CPP is deducted at a rate of 5.95% in 2024. This is composed of base and enhanced contributions.
CPP_Base_Rate <- 0.0495
CPP_Enhanced_Rate <- 0.01
# Employees don't start paying CPP until their annualized income exceeds $3,500 in 2024.
CPP_Threshold_Y <- 3500
# Annual CPP deductions have a maximum cap based on YMPE - earnings threshold * deduction rate. 2024 YMPE is $68,500.
CPP_Base_Income_Deduction_Cap_Y <- (68500 - CPP_Threshold_Y) * CPP_Base_Rate
CPP_Enhanced_Income_Deduction_Cap_Y <- (68500 - CPP_Threshold_Y) * CPP_Enhanced_Rate

# UI constants ----
Primary_Color <- "#2039FF"
Scenario_1_Color <- "#FB89B0"
Scenario_2_Color <- "#89B0FB"
Scenario_3_Color <- "#B0FB89"

# Income Functions ----
# Calculating gross monthly income from hourly wage and hours per week.
Wage_to_Gross_Inc <- function(Wage, Hours_W) {
  (Wage * Hours_W) * 52 / 12
}
EI_Deduction <- function(Gross_Income_M) {
  (Gross_Income_M * EI_Rate) %>%
    pmin(EI_Cap_Y / 12)
}
# UI functions
Help_Icon <- function(AODA_Title) {
  bs_icon("question-circle-fill"
          , class = "text-primary fa-pull-right"
          , title = AODA_Title
  )
}

custom_theme <- bs_theme(preset = "shiny") %>%
  bs_add_variables(
     "primary" = Primary_Color,
  #   # "$secondary" = "#89B0FB",
  #   # "$success" = "#B0FB89",
  #   # "$info" = "#FB89B0",
  #   # "$warning" = "#89B0FB",
     "danger" = Scenario_1_Color,
     "light" = Scenario_2_Color,
     "dark" = Scenario_3_Color
  #   #"gray-100" = "#B0FB89"
  #   # "$gray-200" = "#FB89B0",
  #   # "$gray-300" = "#89B0FB",
  #   # "$gray-400" = "#B0FB89",
  #   # "$gray-500" = "#FB89B0",
  #   # "$gray-600" = "#89B0FB",
  #   # "$gray-700" = "#B0FB89",
  #   # "$gray-800" = "#FB89B0",
  #   # "$gray-900" = "#89B0FB"
  )

ui <- fluidPage(
  # Setting the look and feel of the app.
  #theme = bslib::bs_theme(bootswatch = "united"),
  theme = custom_theme,
  # theme = shinytheme("yeti"),
  # Setting up the ability to provide dynamic user feedback.
  shinyFeedback::useShinyFeedback(),
  tags$style(
    paste0(".fa-circle-question {color:", Primary_Color, "}"
           )
    ),
  use_bs_tooltip(),
  # Creating tabs to organize the content of the shiny app. 
  navbarPage(
    titlePanel("Social Assistance Income Calculator"),
    tabPanel("Introduction",
             # fluidPage(
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
             # )
    ),
    # About me tab  -------------
    tabPanel("About Me",
               h2("About Me"),
               HTML("Please answer all of the questions on this page to make the calculator results relevant to your situation.<br><br><br>"),
               fluidRow(
                 column(6,
                        selectInput("Program",
                                    "Which social assistance program do you want to use for calculations?",
                                    c("Ontario Works", "Ontario Disability Support Program (ODSP)")
                                    )
                        ),
                 column(2,
                        autonumericInput(
                          "SA_Payment",
                          label = "What is your typical monthly Ontario Works payment?",
                          value = "",
                          min = 0,
                          currencySymbol = "$"
                          )
                          ),
                 column(1,
                        tooltip(
                          trigger = bs_icon(
                            "question-circle-fill",
                            class = "text-primary",
                            title = "Info about social assistance payments"
                            ),
                          "Your most recent payment can be found on Ontario.ca/MyBenefits, your bank statement, or a paper cheque from the program",
                          placement = "top",
                          id = "SA_Pay_Tip"
                        )
                        )
                        ),
               fluidRow(
                 column(5, 
                        tags$div(selectInput("Spouse",
                                      "Do you have a spouse or common law partner?",
                                      c("No", "Yes")),
                                    id = "Spouse_Tip"
                        )
                 )
               ),
               fluidRow(
                 column(6,
                        conditionalPanel(condition = "input.Program == 'Ontario Disability Support Program (ODSP)' && input.Spouse == 'Yes'",
                        selectInput("Disability_P",
                                    "Has ODSP determined that you have a disability?",
                                    c("Yes", "No")
                                    )
                        )
                        ),
                 column(6,
                        conditionalPanel(
                          condition = "input.Program == 'Ontario Disability Support Program (ODSP)' && input.Spouse == 'Yes'",
                          selectInput("Disability_S",
                                      "Has ODSP determined that your spouse has a disability?",
                                      c("No", "Yes")
                          )
                   )
                 )
               ),
               fluidRow(
                 column(6,
                        autonumericInput("Take_Home_Pay_1_PM",
                                         label = tooltip(
                                           trigger = list(
                                             "What is your typical monthly take-home pay from work?",
                                             bs_icon("question-circle-fill"
                                                     , class = "text-primary fa-pull-right"
                                                     , title = "Info about take-home pay"
                                                     )
                                           ),
                                           "Take-home pay is the amount paid to you by your employer after payroll deductions, like taxes and CPP. If you are currently unemployed, put $0."
                                           , placement = "top"
                                         ),
                                         value = 0,
                                         min = 0,
                                         currencySymbol = "$"
                                         )
                        ),
                 column(6,
                        conditionalPanel(
                          condition = "input.Spouse == 'Yes'",
                          autonumericInput("Take_Home_Pay_1_SM",
                                           label = tooltip(
                                             trigger = list(
                                               "What is your spouse's typical monthly take-home pay from work?",
                                               Help_Icon("Info about spousal income")
                                               ),
                                             "Take-home pay is the amount paid to your spouse by their employer after payroll deductions, like taxes and CPP. If your spouse is currently unemployed, put $0."
                                               ),
                                           value = 0,
                                           min = 0,
                                           currencySymbol = "$"
                                          )
                        )
                        )
               ),
               fluidRow(
                 column(6,
                        numericInput("Dependents",
                                    label = tooltip(
                                      trigger = list(
                                        "How many dependents are in your household?",
                                        bs_icon(
                                          "question-circle-fill",
                                          class = "text-primary fa-pull-right",
                                          title = "Info about dependents"
                                          )
                                      ),
                                      "Dependents include children or others who live with you and are included in your Ontario Works unit. Do not include yourself in this number.",
                                      placement = "top",
                                      id = "Dep_Tip"
                                    ),
                                    value = 0
                                    , min = 0
                                    , max = 12
                                    )
                        )
               )
    ),
    # Work scenarios tab ----------------
    tabPanel("Work Scenarios",
             # fluidPage(
               h2("Work Scenarios"),
               HTML(paste0("On this page you can customize three work scenarios to see how different employment situations will affect your income, social assistance, tax and other benefit payments.
                    <ul>
                    <li><span style='background-color:", Scenario_1_Color, ";'><strong>Scenario 1</strong></span> has been set to your current situation.</li>
                    <li><span style='background-color:", Scenario_2_Color, ";'><strong>Scenario 2</strong></span> has been set to part-time employment at minimum wage.</li>
                    <li><span style='background-color:", Scenario_3_Color, ";'><strong>Scenario 3</strong></span> has been set to full-time employment at minimum wage.</li>
                    </ul>"
                    )
                    ),
               layout_column_wrap(
                 width = NULL,
                 style = css(grid_template_columns = "28% 35% 35%"),
                 # Scenario 1 Card.
                 card(card_title("Scenario 1 - Current Situation"),
                      class = "bg-danger",
                      htmlOutput("Scen_1_Descript")
                      ),
                 # Scenario 2 Card.
                 card(card_title("Scenario 2"),
                      class = "bg-light",
                      radioButtons(
                        "Format_2",
                        "Prefered income format",
                        c("Hourly Wage", "Monthly take-home pay")
                        ),
                      uiOutput("Scen_2_Parameters")
                      ),
                 # Scenario 3 Card.
                 card(card_title("Scenario 3"),
                      class = "bg-dark",
                      radioButtons(
                        "Format_3",
                        "Prefered income format",
                        c("Hourly Wage", "Monthly take-home pay")
                        ),
                      uiOutput("Scen_3_Parameters")
                      )
                 )
             ),
    # Income results tab -----
    tabPanel("Income Results",
             fluidRow(
               column(
                 9,
                 # withSpinner(plotOutput("Income_Plot"
                 #                               )
                 #                    )
                 tableOutput("Income_Table"),
                 textOutput("Gross_Output_1_PM"),
                 textOutput("Gross_Output_2_PM"),
                 textOutput("Gross_Output_3_PM"),
                 textOutput("Gross_Output_1_SM"),
                 textOutput("Gross_output_2_SM"),
                 textOutput("Gross_output_3_SM")
                      )
                      )
             ),
    tabPanel("More Information")
               )
  )
server <- function(input, output, session) {
  # thematic::thematic_shiny()
  shiny_session <- getDefaultReactiveDomain()
  # Error messages don't seem to be compatible with cards in which these inputs are contained.
  # observeEvent(input$Wage_1, {
  #   shinyFeedback::feedbackWarning("Wage_1",
  #                                  input$Wage_1 < Min_Wage,
  #                                  paste0("Ontario minimum wage is ",
  #                                         dollar(Min_Wage),
  #                                        " per hour."
  #                                         )
  #                                  )
  # })
  # observeEvent(input$Wage_2, {
  #   shinyFeedback::feedbackWarning("Wage_2",
  #                                  input$Wage_2 < Min_Wage,
  #                                  paste0("Ontario minimum wage is ",
  #                                         dollar(Min_Wage),
  #                                         " per hour."
  #                                  )
  #   )
  # })
  # observeEvent(input$Wage_3, {
  #   shinyFeedback::feedbackWarning("Wage_3",
  #                                  input$Wage_3 < Min_Wage,
  #                                  paste0("Ontario minimum wage is ",
  #                                         dollar(Min_Wage),
  #                                         " per hour."
  #                                  )
  #   )
  # })
  # About Me tab server ---------
  Program_Name <- reactive({
    if(input$Program == "Ontario Works") {
      "Ontario Works"
    } else {
      "ODSP"
    }
  })
  observeEvent(list(input$Spouse, input$Program), {
    Spouse_Instruct <- if(input$Spouse == "Yes") {
      "or your spouse"
    } else {
      ""
    }
    update_tooltip(
      session = shiny_session,
      id = "Dep_Tip",
      label = paste0("Dependents include children or others who live with you and are included in your ",
                     Program_Name(),
                     " unit. Do not include yourself ",
                     Spouse_Instruct,
                     " in this number.")
      )
  })
  observeEvent(input$Program, {
    label <- paste0(
      "What is your typical monthly ",
      Program_Name(),
      " payment?"
      )
    tip <- paste0(
      "Your most recent ",
      Program_Name(),
      " payment can be found on Ontario.ca/MyBenefits, your bank statement, or an ",
      Program_Name(),
      " cheque"
    )
    update_tooltip(
      session = shiny_session,
      id = "SA_Pay_Tip",
      tip
    )
    updateAutonumericInput(
      session = shiny_session,
      "SA_Payment",
      label = label
                           )
  })
  # Work scenarios tab server ---------------
  # Dynamic description of current situation displayed in Scenario 1 card.
  output$Scen_1_Descript <- renderText({
    Spousal_Descript <- if(input$Spouse == "Yes"){
      paste0(
        "<li>and ",
        dollar(input$Take_Home_Pay_1_SM),
        " as your spouse's current monthly take-home pay from work"
        )}
      else ""
    paste0(
      "On the previous page you put <ul><br><li>",
      dollar(input$Take_Home_Pay_1_PM),
      " as your current monthly take-home pay from work",
      Spousal_Descript,
      ".", 
      "</ul>You can return to the <strong>About Me</strong> tab if you would like to change the work income for Scenario 1."
      )
  })
  # Dynamically sized Scenario 2 card elements to accommodate spouse items.
  Spouse_Denom <- eventReactive(input$Spouse, {
    if(input$Spouse == "Yes") {
      2
    } else {
      1
    }
  })
  Spouse_Break <- eventReactive(input$Spouse, {
    if(input$Spouse == "Yes") {
      "<br>"
    } else {
      ""
    }
  })
  # Scenario 2 inputs.
  # Inputs for hourly wage option.
  output$Scen_2_Parameters <- renderUI( {
    if(input$Format_2 == "Hourly Wage") {
      layout_column_wrap(
        width = 1/Spouse_Denom(),
        fill = FALSE,
        heights_equal = "row",
        h5("What if I worked...?"),
        conditionalPanel(
          condition = "input.Spouse == 'Yes'",
          h5("What if my spouse worked...?")
        ),
        autonumericInput("Hours_2_PW"
                         , HTML(paste0(
                           "My weekly work ",
                           Spouse_Break(),
                           "hours"
                         ))
                         , value = 17
                         , min = 0
                         , max = 60
                         , decimalPlaces = 0
        ),
        conditionalPanel(
          condition = "input.Spouse == 'Yes'",
          autonumericInput(
            "Hours_2_SW",
            "Spouse's weekly work hours",
            value = 17,
            min = 0,
            max = 60,
            decimalPlaces = 0
          )
        ),
        autonumericInput("Wage_2_P"
                         , "My hourly wage"
                         , value = 17.20
                         , min = 17.20
                         , max = 100
                         , currencySymbol = "$"
        ),
        conditionalPanel(
          condition = "input.Spouse == 'Yes'",
          autonumericInput(
            "Wage_2_S",
            "Spouse's hourly wage",
            value = 17.20,
            min = 17.20,
            max = 100,
            currencySymbol = "$"
          )
        )
      )
      # Input for monthly take-home pay option.
    } else {
      layout_column_wrap(
        width = 1/Spouse_Denom(),
        fill = FALSE,
        heights_equal = "row",
        h5("What if I worked...?"),
        conditionalPanel(
          condition = "input.Spouse == 'Yes'",
          h5("What if my spouse worked...?")
        ),
        autonumericInput(
          "Take_Home_Pay_2_PM",
          label = tooltip(
            trigger = list(
              "My monthly take-home pay from work",
              Help_Icon("Info about take-home pay")
              ),
            "Take-home pay is the amount paid to you by your employer after payroll deductions, like taxes and CPP. To show yourself as unemployed in this scenario, put $0."
            ),
          value = 1170,
          min = 0,
          currencySymbol = "$"
        ),
        conditionalPanel(
          condition = "input.Spouse == 'Yes'",
          autonumericInput(
            "Take_Home_Pay_2_SM",
            label = tooltip(
              trigger = list(
                "Spouse's monthly take-home pay from work",
                Help_Icon("Info about spousal income")
                ),
              "Take-home pay is the amount paid to your spouse by their employer after payroll deductions, like taxes and CPP. To show your spouse as unemployed in this scenario, put $0."
              ),
            value = 1170,
            min = 0,
            currencySymbol = "$"
          )
        )
      )
    }
  })
  # Scenario 3 inputs.
  output$Scen_3_Parameters <- renderUI( {
    if(input$Format_3 == "Hourly Wage") {
      layout_column_wrap(
        width = 1/Spouse_Denom(),
        fill = FALSE,
        heights_equal = "row",
        h5("What if I worked...?"),
        conditionalPanel(
          condition = "input.Spouse == 'Yes'",
          h5("What if my spouse worked...?")
        ),
        autonumericInput("Hours_3_PW"
                         , HTML(paste0(
                           "My weekly work ",
                           Spouse_Break(),
                           "hours"
                         ))
                         , value = 35
                         , min = 0
                         , max = 60
                         , decimalPlaces = 0
        ),
        conditionalPanel(
          condition = "input.Spouse == 'Yes'",
          autonumericInput(
            "Hours_3_SW",
            "Spouse's weekly work hours",
            value = 35,
            min = 0,
            max = 60,
            decimalPlaces = 0
          )
        ),
        autonumericInput("Wage_3_P"
                         , "My hourly wage"
                         , value = 17.20
                         , min = 17.20
                         , max = 100
                         , currencySymbol = "$"
        ),
        conditionalPanel(
          condition = "input.Spouse == 'Yes'",
          autonumericInput(
            "Wage_3_S",
            "Spouse's hourly wage",
            value = 17.20,
            min = 17.20,
            max = 100,
            currencySymbol = "$"
          )
        )
      )
    } else {
      layout_column_wrap(
        width = 1/Spouse_Denom(),
        fill = FALSE,
        heights_equal = "row",
        h5("What if I worked...?"),
        conditionalPanel(
          condition = "input.Spouse == 'Yes'",
          h5("What if my spouse worked...?")
        ),
        autonumericInput(
          "Take_Home_Pay_3_PM",
          label = tooltip(
            trigger = list(
              "My monthly take-home pay from work",
              Help_Icon("Info about take-home pay")
              ),
            "Take-home pay is the amount paid to you by your employer after payroll deductions, like taxes and CPP. To show yourself as unemployed in this scenario, put $0."
            ),
          value = 2400,
          min = 0,
          currencySymbol = "$"
          ),
        conditionalPanel(
          condition = "input.Spouse == 'Yes'",
          autonumericInput(
            "Take_Home_Pay_3_SM",
            label = tooltip(
              trigger = list(
                "Spouse's monthly take-home pay from work",
                Help_Icon("Info about spousal income")
                ),
              "Take-home pay is the amount paid to your spouse by their employer after payroll deductions, like taxes and CPP. To show your spouse as unemployed in this scenario, put $0."
              ),
            value = 2400,
            min = 0,
            currencySymbol = "$"
          )
        )
      )
    }
  })
  # Income results tab server ---------------
  # Gross income formulas
  Gross_Income_2_PM <- reactive( {
    if (input$Format_2 == "Hourly Wage") {
      Wage_to_Gross_Inc(
        input$Wage_2_P,
        input$Hours_2_PW
      )
      } else {
        # Need to develop the formula to convert take-home pay into gross income.
        NA
      }
  })
  Gross_Income_2_SM <- eventReactive(list(input$Format_2, input$Spouse), {
    if (input$Format_2 == "Hourly Wage" & input$Spouse == "Yes") {
      Wage_to_Gross_Inc(
        input$Wage_2_S,
        input$Hours_2_SW
      )
      } else {
        # Need to develop the formula to convert take-home pay into gross income.
        NA
      }
  })
  Gross_Income_3_PM <- reactive( {
    if (input$Format_3 == "Hourly Wage") {
      Wage_to_Gross_Inc(
        input$Wage_3_P,
        input$Hours_3_PW
      )
      } else {
        # Need to develop the formula to convert take-home pay into gross income.
        NA
      }
  })
  Gross_Income_3_SM <- eventReactive(list(input$Format_3, input$Spouse), {
    if (input$Format_3 == "Hourly Wage" & input$Spouse == "Yes") {
      Wage_to_Gross_Inc(
        input$Wage_3_S,
        input$Hours_3_SW
      )
      } else {
        # Need to develop the formula to convert take-home pay into gross income.
        NA
      }
  })
  # Computing take home pay.
  # Take_Home_Pay_1_BM <- reactive( {
  #   sum(
  #     input$Take_Home_Pay_1_PM,
  #     input$Take_Home_Pay_1_SM,
  #     na.rm = TRUE
  #   )
  # })
  Take_Home_Pay_2_PM <- eventReactive(input$Format_2, {
    if (input$Format_2 == "Monthly take-home pay") {
      input$Take_Home_Pay_2_PM
    } else {
      # Again, need to develop the formula to convert gross income into take-home pay.
      NA
    }
  })
  Take_Home_Pay_2_SM <- eventReactive(list(input$Format_2, input$Spouse), {
    if (input$Format_2 == "Monthly take-home pay" & input$Spouse == "Yes") {
      input$Take_Home_Pay_2_SM
    } else {
      # Again, need to develop the formula to convert gross income into take-home pay.
      NA
    }
  })
  Take_Home_Pay_3_PM <- eventReactive(input$Format_3, {
    if (input$Format_3 == "Monthly take-home pay") {
      input$Take_Home_Pay_3_PM
    } else {
      # Again, need to develop the formula to convert gross income into take-home pay.
      NA
    }
  })
  Take_Home_Pay_3_SM <- eventReactive(list(input$Format_3, input$Spouse), {
    if (input$Format_3 == "Monthly take-home pay" & input$Spouse == "Yes") {
      input$Take_Home_Pay_3_SM
    } else {
      # Again, need to develop the formula to convert gross income into take-home pay.
      NA
    }
  })
      
  Income_Tibble <- reactive( {
    tibble(
      Scenario = as.factor(c(1:3)),
      Wage_P = c(
        NA,
        case_when(
          input$Format_2 == "Hourly Wage" ~ input$Wage_2_P
          ),
        case_when(
          input$Format_3 == "Hourly Wage" ~ input$Wage_3_P
          )
        ),
      Wage_S = c(
        NA,
        case_when(
          input$Format_2 == "Hourly Wage" & input$Spouse == "Yes" ~ input$Wage_2_S
        ),
        case_when(
          input$Format_3 == "Hourly Wage" & input$Spouse == "Yes" ~ input$Wage_3_S
        )
      ),
      Hours_PW = c(
        NA,
        case_when(
          input$Format_2 == "Hourly Wage" ~ input$Hours_2_PW
          ),
        case_when(
          input$Format_3 == "Hourly Wage" ~ input$Hours_3_PW
          )
        ),
      Hours_SW = c(
        NA,
        case_when(
          input$Format_2 == "Hourly Wage" & input$Spouse == "Yes" ~ input$Hours_2_SW
          ),
        case_when(
          input$Format_3 == "Hourly Wage" & input$Spouse == "Yes" ~ input$Hours_3_SW
          )
        )
      )
    })
  
  output$Income_Table <- renderTable(Income_Tibble())
   # output$Income_Plot <- renderPlot({
   #    ggplot(Income_Tibble(), aes(x = Scenario, y = Calcd_Income, fill = Scenario)) +
   #      geom_col() +
   #      scale_fill_manual(values = c(Scenario_1_Color, Scenario_2_Color, Scenario_3_Color))
   #  }, res = 96
   #  )

}
shinyApp(ui, server)