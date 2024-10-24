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
library(shinyvalidate)

# Income constants ----
# Ontario minimum wage as of October 2024
Min_Wage <- 17.2
Part_Time_Hours <- 17
Full_Time_Hours <- 40
Part_Time_Take_Home_Pay <- 1170
Full_Time_Take_Home_Pay <- 2400
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

# Tax constants ----
# Progressive federal income tax rates for 2024
Fed_Tax_Rates <- c(0.15, 0.205, 0.26, 0.29, 0.33)
# Progressive federal tax brackets for 2024
Fed_Tax_Brackets <- c(0, 55867, 111733, 173205, 246752)
# Progressive Ontario income tax rates for 2024
ON_Tax_Rates <- c(0.0505, 0.0915, 0.1116, 0.1216, 0.1316)
# Progressive Ontario income tax brackets for 2024
ON_Tax_Brackets <- c(0, 51446, 102894, 150000, 220000)
# Ontario surtax rates from ON428 for 2024
ON_Surtax_Rates <- c(0.2, 0.36)
# Ontario surtax brackets from ON428 for 2024
ON_Surtax_Brackets <- c(0, 5554, 7108)
# The basic personal amount non-refundable tax credit for federal and Ontario in 2024.
# Given the social assistance population, it seems reasonable to assume net incomes will be below $173,205, and therefore give everyone the basic personal amount supplement.
Fed_Basic_NRTC <- 15705
ON_Basic_NRTC <- 12399
# These parameters come from the Federal Worksheet and are updated for 2024.
Fed_Age_Amount_NRTC_Max <- 8790
Fed_Age_Amount_Income_Threshold <- 44325
Fed_Disabled_Child_NRTC_Supplement <- 5758
Disabled_Child_NRTC_Max <- 15630
Fed_Disabled_Childcare_Threshold <- 3373
# These parameters come from Schedule 5 and have been updated to 2024.
Canada_Caregiver_Amount <- 2616
Canada_Caregiver_Max <- 8375
Canada_Caregiver_Base <- 28041
# These parameters come from Step 5B of the T1 form and have been updated to 2024.
EI_NRTC_Max <- 1061
Canada_Employment_NRTC_Max <- 1433
Fed_Disability_NRTC_Self <- 9872
# These parameters come from ON428 and are updated to 2024.
ON_Dep_Base_Amount <- 11582
ON_Dep_NRTC_Max <- 10528
ON_Disability_NRTC_Self <- 10017
ON_Basic_Tax_Reduction <- 286
ON_Dep_Tax_Reduction <- 529
ON_Health_Premium_Bracket_1_bottom <- 20000
ON_Health_Premium_Bracket_1_top <- 25000
ON_Health_Premium_Bracket_2_bottom <- 36000
ON_Health_Premium_Bracket_2_top <- 38500
ON_Health_Premium_Bracket_3_bottom <- 48000
ON_Health_Premium_Bracket_3_top <- 48600
ON_Health_Premium_Bracket_4_bottom <- 72000
ON_Health_Premium_Bracket_4_top <- 72600
ON_Health_Premium_Bracket_5_bottom <- 200000
ON_Health_Premium_Bracket_5_top <- 200600
ON_Health_Premium_Rate_1 <- .06
ON_Health_Premium_Rate_2 <- .25
# These parameters come from the Ontario Worksheet updated to 2024.
ON_Age_Amount_NRTC_Max <- 6054
ON_Age_Amount_Income_Threshold <- 45068
ON_Caregiver_Base_Amount <- 25840
ON_Disabled_Child_NRTC_Supplement <- 5843
ON_Disabled_Childcare_Threshold <- 3422
# These parameters come from Schedule ON428-A updated to 2024.
LIFT_Credit_Max <- 875
LIFT_Applicable_Rate <- .05
LIFT_Income_Threshold <- 32500
# These parameters come from Schedule 6 for the Canada Workers Benefit updated to 2024.
CWB_Basic_Start_Point <- 3000
CWB_Disability_Supplement_Start_Point <- 1150
CWB_Max_Basic_Single <- 1589
CWB_Max_Basic_Spouse_Or_Dep <- 2739
CWB_Max_Disability_Supplement <- 821
CWB_Rate <- .27
CWB_Reduction_Rate <- .15
CWB_Max_Secondary_Earner_Exemption <- 15955
CWB_Basic_Reduction_Point_Single <- 26149
CWB_Basic_Reduciton_Point_Spouse_Or_Dep <- 29833
CWB_Disability_Supplement_Reduction_Point_Single <- 36748
CWB_Disability_Supplement_Reduction_Point_Spouse_Or_Dep <- 48091
# These parameters come from the 2024 OEPTC Calculation sheets.
OEPTC_Occupancy_Cost_Rate <- .2
OEPTC_Energy_Max <- 277
OEPTC_Property_Tax_Rate <- .1
OEPTC_Property_Tax_Max_Juniors <- 971
OEPTC_Property_Tax_Max_Seniors <- 1144
OEPTC_Property_Tax_Supplement_Juniors <- 69
OEPTC_Property_Tax_Supplement_Seniors <- 589
OEPTC_Reduction_Threshold_Family <- 34661
OEPTC_Reduction_Threshold_Single <- 27729
OEPTC_Reduction_Rate <- .02
# These parameters come from the CRA's webpage on the Trillium Benefit updated for July 2024.
OSTC_Credit <- 360
OSTC_Reduction_Threshold_Family <- 34661
OSTC_Reduction_Threshold_Single <- 27729
OSTC_Reduction_Rate <- .04
# These parameters come from the CRA page on the Canada Child Benefit, updated for 2024. 
CCB_Credit_Under_6 <- 7787
CCB_Credit_6_to_17 <- 6570
CCB_Credit_Disabled <- 3322
CCB_Lower_Reduction_Threshold <- 36502
CCB_Upper_Reduction_Threshold <- 79087
CCB_Lower_Reduction_Rate_1_Kid <- .07
CCB_Upper_Reduction_Rate_1_Kid <- .032
CCB_Lower_Reduction_Rate_2_Kids <- .135
CCB_Upper_Reduction_Rate_2_Kids <- .057
CCB_Lower_Reduction_Rate_3_Kids <- .19
CCB_Upper_Reduction_Rate_3_Kids <- .08
CCB_Lower_Reduction_Rate_4_Kids <- .23
CCB_Upper_Reduction_Rate_4_Kids <- .095
CDB_Reduction_Rate_1_Kid <- .032
CDB_Reduction_Rate_2_Kids <- .057
# These parameters come from the CCB and related provincial and territorial programs booklet (t4144), updated for July 2024.
OCB_Credit <- 1680
OCB_Reduction_Threshold <- 25646
# The OCB Reduction rate is not published, so it was determined algebraically from output from the CRA's Child and Family Benefits Calculator
OCB_Reduction_Rate <- .08
# These parameters are taken from HST Credit Calculation Sheets, updated for July 2024.
HST_Credit_Adult <- 340
Additional_HST_Credit_Threshold <- 11039
Additional_HST_Credit <- 179
Additional_HST_Credit_Rate <- 0.02
HST_Credit_Reduction_Threshold <- 44324
HST_Credit_Reduction_Rate <- 0.05
# These parameters are taken from the CRA's policy directive on the Climate Action Incentive Payments 2023.
CAIP_Individual <- 488
CAIP_Spouse <- 244
CAIP_Children <- 122
CAIP_First_Child_Single_Parent <- 244
CAIP_Rural_Supplement <- .1

# UI constants ----
Primary_Color <- "#2039FF"
Scenario_1_Color <- "#FB89B0"
Scenario_2_Color <- "#89B0FB"
Scenario_3_Color <- "#B0FB89"

# Default scenarios -----
# This is a tibble containing all of the default settings that gets updated when users change inputs in the UI



# Income Functions ----
# Calculating gross monthly income from hourly wage and hours per week.
Wage_To_Gross_Formula <- function(Wage, Hours_W) {
  (Wage * Hours_W) * 52 / 12
}
# Annual EI deductions.
EI_Deduct_Formula <- function(Gross_Income_Y) {
  (Gross_Income_Y * EI_Rate) %>%
    # EI deductions cannot exceed an annual cap.
    pmin(EI_Cap_Y)
}
# These formulas calculate benefit unit exemplars' annual base and enhanced CPP deductions.
# The CCP formulas are taken from Schedule 8.
CPP_Base_Deduct_Formula <- function(Gross_Income_Y) {
  ((Gross_Income_Y - CPP_Threshold_Y) * CPP_Base_Rate) %>% 
    # Preventing negative values.
    pmax(0) %>%
    # CPP deductions have a cap.
    pmin(CPP_Base_Income_Deduction_Cap_Y)
}
CPP_Enhanced_Deduct_Formula <- function(Gross_Income_Y) {
  ((Gross_Income_Y - CPP_Max_Exemption_Y) * CPP_Enhanced_Rate) %>%
    pmax(0) %>%
    pmin(CPP_Enhanced_Income_Deduction_Cap_Y)
}
# Function to calculate taxable income. 
# If the employee does not fill out all fields in TD1 or TD1ON, then the only reduction to taxable income is CPP Enhanced contributions.
Taxable_Income_Formula <- function(Gross_Income_Y) {
  Gross_Income_Y - CPP_Enhanced_Deduct_Formula(Gross_Income_Y)
}
# Function to calculate federal income tax
Fed_Tax_Formula <- function(Taxable_Income_Y) {
  Tax <- 0
  for (i in seq_along(Fed_Tax_Rates)) {
    if (Taxable_Income_Y > Fed_Tax_Brackets[i]) {
      Marginal_Income <- min(Taxable_Income_Y, Fed_Tax_Brackets[i + 1]) - Fed_Tax_Brackets[i]
      Tax <- Tax + Marginal_Income * Fed_Tax_Rates[i]
    }
  }
  return(Tax)
}
# Function to calculate gross income from net income
Take_Home_To_Gross_Formula <- function(Take_Home_Pay_Y) {
  # Iterative approach to estimate gross income
  Gross_Income_Y <- Take_Home_Pay_Y
  Results <- data.frame(Iteration = integer(), GrossIncome = numeric())
  Iteration <- 0
  Difference <- Inf
  # Iterate until difference between iterations in gross income calculations is less than 10 cents.
  while (difference >= 0.1) {
    Iteration <- Iteration + 1
    Previous_Gross_Income <- Gross_Income_Y
    EI_Deduction_Y <- EI_Deduct_Formula(Gross_Income_Y)
    CPP_Base_Deduct_Y <- CPP_Base_Deduct_Formula(Gross_Income_Y)
    CPP_Enhanced_Deduct_Y <- CPP_Enhanced_Deduct_Formula(Gross_Income_Y)
    Taxable_Income_Y <- Taxable_Income_Formula(Gross_Income_Y)
    Fed_Tax_Y <- Fed_Tax_Formula(Taxable_Income_Y)
    Deductions <- EI_Deduction_Y + CPP_Base_Deduct_Y + CPP_Enhanced_Deduct_Y + Fed_Tax_Y
    Gross_Income_Y <- Take_Home_Pay_Y + Deductions
    difference <- abs(Gross_Income_Y - Previous_Gross_Income_Y)
    results <- rbind(results, data.frame(Iteration = iteration, GrossIncome = gross_income))
  }
  
  # Print the results in a table format
  kable(results, format = "markdown", col.names = c("Iteration", "Gross Income"))
  
  return(Gross_Income_Y)
}
# UI functions ----
Help_Icon <- function(AODA_Title) {
  bs_icon("question-circle-fill"
          , class = "text-primary fa-pull-right"
          , title = AODA_Title
  )
}

#UI ----
custom_theme <- bs_theme(preset = "shiny") %>%
  bs_add_variables(
     "primary" = Primary_Color,
  #   # "$secondary" = "#89B0FB",
  #   # "$success" = "#B0FB89",
  #   # "$info" = "#FB89B0",
     # "warning" = "#c10000",
     "info" = Scenario_1_Color,
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
  theme = custom_theme,
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
                                    c("",
                                      "Ontario Works",
                                      "Ontario Disability Support Program (ODSP)"
                                      )
                                    )
                        ),
                 column(2,
                        textInput(
                          "SA_Payment",
                          label = "What is your typical monthly social assistance payment?",
                          value = "",
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
                                      c("", "No", "Yes")),
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
                                    value = NULL
                                    , min = 0
                                    , max = 12
                                    )
                        )
               ),
             fluidRow(
               column(
                 6,
                 numericInput(
                   "Age_P",
                   "How old are you?",
                   value = NULL,
                   min = 0,
                   max = 99
                 )
               ),
               column(
                 2,
                 conditionalPanel(
                   condition = "input.Spouse == 'Yes'",
                   numericInput(
                      "Age_S",
                      "How old is your spouse?",
                      value = NULL,
                      min = 0,
                      max = 99
                    )
                   )
               ),
               column(
                 6,
                 conditionalPanel(
                   condition = "input.Dependents >= 1",
                   numericInput(
                     "Age_D_1",
                     "How old is your dependent?",
                     value = NULL,
                     min = 0,
                     max = 99
                   )
                 )
               ),
               column(
                 2,
                 conditionalPanel(
                   condition = "input.Dependents >= 2",
                   numericInput(
                     "Age_D_1",
                     "How old is your second dependent?",
                     value = NULL,
                     min = 0,
                     max = 99
                   )
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
                      class = "bg-info",
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
    # More information tab ----
    tabPanel("More Information",
             fluidRow(
               column(
                 9,
                 verbatimTextOutput("Test")
               )
             ))
               )
  )
server <- function(input, output, session) {
  # thematic::thematic_shiny()
  shiny_session <- getDefaultReactiveDomain()
  
  # About Me tab server ---------
  # Creating a variable that tracks the name of the program to which user belongs.
  Program_Name <- reactive({
    if(input$Program == "Ontario Works") {
      "Ontario Works"
    } else {
      "ODSP"
    }
  })
  
  # Hiding missing input feedback once input detected.
  observeEvent(input$Program, {
    if (input$Program != "") {
      hideFeedback("Program")
      removeNotification("Missing_About_Me")
    }
  })
  observeEvent(input$Spouse, {
    if (input$Spouse != "") {
      hideFeedback("Spouse")
      removeNotification("Missing_About_Me")
    }
  })
  observeEvent(input$SA_Payment, {
    if (input$SA_Payment != "") {
      hideFeedback("SA_Payment")
      removeNotification("Missing_About_Me")
    }
  })
  
  # Formatting a textInput field as a currency field
  SA_Pay_Value <- reactiveVal(NULL)
  observeEvent(input$SA_Payment, {
    Out_Num <- as.numeric(str_extract(
      input$SA_Payment,
      "\\d*\\.?\\d+"
        )
      )
    if (!is.na(Out_Num)) {
      Formatted_Value <- paste0(
        "$",
        format(Out_Num, big.mark = ",", digits = 2, nsmall = 2
               )
      )
      updateTextInput(
        session = shiny_session,
        "SA_Payment",
        value = Formatted_Value
      )
      SA_Pay_Value(Out_Num) # making a numeric output available for calculations
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
                         , min = 0
                         , max = 100
                         , currencySymbol = "$"
        ),
        conditionalPanel(
          condition = "input.Spouse == 'Yes'",
          autonumericInput(
            "Wage_2_S",
            "Spouse's hourly wage",
            value = 17.20,
            min = 0,
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
          value = Part_Time_Take_Home_Pay,
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
            value = Part_Time_Take_Home_Pay,
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
                         , min = 0
                         , max = 100
                         , currencySymbol = "$"
        ),
        conditionalPanel(
          condition = "input.Spouse == 'Yes'",
          autonumericInput(
            "Wage_3_S",
            "Spouse's hourly wage",
            value = 17.20,
            min = 0,
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
          value = Full_Time_Take_Home_Pay,
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
            value = Full_Time_Take_Home_Pay,
            min = 0,
            currencySymbol = "$"
          )
        )
      )
    }
  })

  
  # Income results tab server ---------------
  
  # Setting up a default values so that unrendered inputs don't throw errors
  Take_Home_Pay_2_PM_Default <- reactiveVal(Part_Time_Take_Home_Pay)
  # Making it possible to update the default value when rendered
  observeEvent(input$Take_Home_Pay_2_PM, {
    Take_Home_Pay_2_PM_Default(input$Take_Home_Pay_2_PM)
  }, ignoreInit = TRUE, ignoreNULL = TRUE
  )
  Take_Home_Pay_3_PM_Default <- reactiveVal(Full_Time_Take_Home_Pay)
  observeEvent(input$Take_Home_Pay_3_PM, {
    Take_Home_Pay_3_PM_Default(input$Take_Home_Pay_3_PM)
  }, ignoreInit = TRUE, ignoreNULL = TRUE
  )
  Take_Home_Pay_2_SM_Default <- reactiveVal(Part_Time_Take_Home_Pay)
  observeEvent(input$Take_Home_Pay_2_SM, {
    Take_Home_Pay_2_SM_Default(input$Take_Home_Pay_2_SM)
  }, ignoreInit = TRUE, ignoreNULL = TRUE
  )
  Take_Home_Pay_3_SM_Default <- reactiveVal(Full_Time_Take_Home_Pay)
  observeEvent(input$Take_Home_Pay_3_SM, {
    Take_Home_Pay_3_SM_Default(input$Take_Home_Pay_3_SM)
  }, ignoreInit = TRUE, ignoreNULL = TRUE
  )
  
  Income_Tibble <- reactive( {
    # Creating a notice on the results page if there are missing input values
    if (input$Spouse == "" || input$Program == "" || is.null(input$SA_Payment)) {
    Missing <- showNotification(
      HTML("Income results are not calculated until you have inputed all required information on the <strong>About Me</strong> tab"),
      duration = NULL,
      closeButton = TRUE,
      type = "error",
      id = "Missing_About_Me"
    )
    }
    # Marking the required inputs for user completion.
    feedbackDanger(
      "Spouse",
      input$Spouse == "",
      "Input requried"
    )
    feedbackDanger(
      "Program",
      input$Program == "",
      "Input required"
    )
    feedbackDanger(
      "SA_Payment",
      is.null(input$SA_Payment) | input$SA_Payment == "",
      "Input required"
    )
    # Do not perform the output until all the input is available.
    req(input$Spouse, input$Program, 
        # input$SA_Payment
        )
    Default_Scenarios <- tibble(
      Scenario = as.factor(c(1:3)),
      # Spouse = "No",
      # Dependents = 0,
      # Wage_P = c(NA, Min_Wage, Min_Wage),
      # Wage_S = NA,
      # Hours_PW = c(NA, Part_Time_Hours, Full_Time_Hours),
      # Hours_SW = NA,
      # Take_Home_Pay_PM = NA,
      # Take_Home_Pay_SM = NA
    )
    Default_Scenarios %>%
      mutate(
        Program = case_when(
          .env$input$Program == "Ontario Works"
          ~ "OW",
          .env$input$Program == "Ontario Disability Support Program (ODSP)"
          ~ "ODSP"
        ),
        SA_Pay = c(
          .env$SA_Pay_Value(),
          NA,
          NA
          ),
        Spouse = case_when(
          .env$input$Spouse == "Yes"
          ~ "Yes",
          .env$input$Spouse == "No"
          ~ "No"
        ),
        Wage_P = c(
          NA,
          case_when(
            .env$input$Format_2 == "Hourly Wage"
            ~ .env$input$Wage_2_P
          ),
          case_when(
            .env$input$Format_3 == "Hourly Wage"
            ~ .env$input$Wage_3_P
          )
        ),
        Wage_S = case_when(
          .env$input$Spouse == "Yes"
          ~ c(
            NA,
            case_when(
              .env$input$Format_2 == "Hourly Wage"
              ~ .env$input$Wage_2_S
              ),
            case_when(
              .env$input$Format_3 == "Hourly Wage"
              ~ .env$input$Wage_3_S
              )
            )
          ),
        Hours_PW = c(
          NA,
          case_when(
            .env$input$Format_2 == "Hourly Wage"
            ~ .env$input$Hours_2_PW
          ),
          case_when(
            .env$input$Format_3 == "Hourly Wage"
            ~ .env$input$Hours_3_PW
          )
        ),
        Hours_SW = case_when(
          .env$input$Spouse == "Yes"
          ~ c(
            NA,
            case_when(
              .env$input$Format_2 == "Hourly Wage"
              ~ .env$input$Hours_2_SW
            ),
            case_when(
              .env$input$Format_3 == "Hourly Wage"
              ~ .env$input$Hours_3_SW
            )
          )
        ),
        Gross_Income_PM = case_when(
          !is.na(.data$Wage_P) & !is.na(.data$Hours_PW)
          ~ Wage_To_Gross_Formula(
            .data$Wage_P,
            .data$Hours_PW
            )
          ),
        Gross_Income_SM = case_when(
          !is.na(.data$Wage_S) & !is.na(.data$Hours_SW)
          ~ Wage_To_Gross_Formula(
            .data$Wage_S,
            .data$Hours_SW
            )
          ),
        Take_Home_Pay_PM = case_when(
          is.na(.data$Wage_P) & is.na(.data$Hours_PW)
          ~ c(
            .env$input$Take_Home_Pay_1_PM,
            .env$Take_Home_Pay_2_PM_Default(),
            .env$Take_Home_Pay_3_PM_Default()
            )
          ),
        Take_Home_Pay_SM = case_when(
          is.na(.data$Wage_S) & is.na(.data$Hours_SW) & .data$Spouse == "Yes"
          ~ c(
            .env$input$Take_Home_Pay_1_SM,
            .env$Take_Home_Pay_2_SM_Default(),
            .env$Take_Home_Pay_3_SM_Default()
            )
          )
        )
    })

  
  output$Income_Table <- renderTable( {
    Income_Tibble()
    })
   # output$Income_Plot <- renderPlot({
   #    ggplot(Income_Tibble(), aes(x = Scenario, y = Calcd_Income, fill = Scenario)) +
   #      geom_col() +
   #      scale_fill_manual(values = c(Scenario_1_Color, Scenario_2_Color, Scenario_3_Color))
   #  }, res = 96
   #  )

}
shinyApp(ui, server)