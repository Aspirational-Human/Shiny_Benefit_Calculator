## Task for tomorrow:
# Update default scenario titles to reflect marital status.



# Packages
library(tidyverse)
library(shiny) 
# library(shinyFeedback)
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
library(plotly)
library(reactable)
# library(ggiraph)
# library(ggpattern)

# Income constants ----
# Ontario minimum wage as of October 2024
Min_Wage <- 17.2
Part_Time_Hours <- 17
Full_Time_Hours <- 40
Part_Time_Take_Home_Pay <- 1170
Full_Time_Take_Home_Pay <- 2400
Part_Time_Salary <- Min_Wage * Part_Time_Hours * 52
Full_Time_Salary <- Min_Wage * Full_Time_Hours * 52
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
# Ontario healh premium brackets from ON428 for 2024.
ON_Health_Premium_Brackets <- c(0, 20000, 36000, 48000, 72000, 200000)
# There are only two health premium rates, applied progressively to the brackets.
ON_Health_Premimum_Rates <- c(.06, .06, .25, .25, .25, .25)
# The maximum health premium for each bracket.
ON_Health_Premium_Bracket_Maxes <- c(0, 300, 450, 600, 750, 900)
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

# Ontario Works constants --------------------------------------------------
# OW clients who are parents to one or more dependents under the age of 18 and who do not have a spouse in their benefit unit receive a monthly sole support parent supplement.
OW_Sole_Parent_Supplement <- 17
# Creating parameter used for determining benefit units' basic needs for every additional dependent beyond the MCCSS provided look-up tables.
OW_Additional_Dep_Basic_Need <- 175
# Creating OW advanced age allowance as a constant.
OW_Advanced_Age_Allowance <- 44
# Unlicenced childcare allowance is capped at $600 per child per month for both OW & ODSP
Unlicenced_Childcare_Cap <- 600
# Licenced childcare costs have no cap. To estimate childcare costs for clients who have children but do not currently report childcare costs, we can take an average childcare cost from other clients. For now this parameter serves as a placeholder for that average.
Avg_SAMS_licensed_Childcare_cost <- 875
# Creating remote allowance parameter for determining benefit units' remote allowance when dependents exceed the number listed in MCCSS's provided look-up tables. This same value is used for both OW and ODSP.
Additional_Dep_Remote_Allowance <- 102
# Creating flat earnings exemption parameter for OW clients.
OW_Flat_Earning_Exemption <- 200
# Creating earnings exemption rate parameter for income greater than flat rate exemption.
OW_Earning_Exemption_Rate <- .5
# Creating a parameter for Rent Geared to Income's relationship to household income.
RGI_Rate <- .3
# Net income is exempt for both RGI applicants and their spouse up to this amount per person according to O.Reg 316/19.
RGI_Income_Exemption <- 75

# ODSP constants ---------------------------------------------------------------
# Sole Support parent supplement parameter.
ODSP_Sole_Parent_Supplement <- 143
# Creating parameter used for determining benefit units' basic needs for every additional dependent beyond the MCCSS provided look-up tables. Inflation adjusted for 01 July, 2024.
ODSP_Additional_Dep_Basic_Need <- 248
# Creating parameter used for double disabled benefit units' shelter allowance. Inflation adjusted for 01 July, 2024.
Double_Disabled_Shelter_Supplement <- 85
# Creating parameter used to cap double disabled benefit units' basic need and shelter allowances. Inflation adjusted for 01 July, 2024.
Double_Disabled_Cap <- 2305
# Creating flat earnings exemption parameter for disabled ODSP clients (non-disabled ODSP BU members receive OW earnings exemption).
ODSP_Flat_Earning_Exemption <- 1000
# Creating earnings exemption rate parametr for income greater than flat rate exemption.
ODSP_Earning_Exemption_Rate <- .25
# Every adult member of an ODSP benefit unit receives a work-related benefit for every month they report earnings.
ODSP_Work_Related_Benefit <- 100

# UI constants -----------------------------------------------------------------
Primary_Color <- "#2039FF"
Light_Green <- "#d3f7c6"
Light_Blue <- "#C6EBF7"
Light_Purple <- "#EAC6F7"
Light_Brown <- "#F7D2C6"
Light_Grey <- "#000000"
Scenario_1_Color <- "#EAC6F7"
Scenario_2_Color <- "#C6EBF7"
Scenario_3_Color <- "#d3f7c6"
Theme_Colours <- c(
  Light_Green,
  Light_Blue,
  Light_Purple,
  Light_Brown
)
# Custom CSS for scenario fill patterns
# css <- HTML(sprintf('
#   /* SVG Patterns */
#   .solid-card {
#     background-color: %s !important;
#   }
#   .stripe-card {
#     background-color: %s !important;
#     background-image: url("data:image/svg+xml;base64,%s") !important;
#     background-size: 12px 12px !important;
#   }
#   .dot-card {
#     background-color: %s !important;
#     background-image: url("data:image/svg+xml;base64,%s") !important;
#     background-size: 8px 8px !important;
#   }
#   .text-container {
#     background-color: rgba(255, 255, 255, 0.9);
#     padding: 10px;
#     border-radius: 4px;
#     margin: 5px 0;
#   }
# ',
#                     Light_Green,
#                     Light_Green,
#                     base64enc::base64encode(charToRaw(sprintf('<svg width="12" height="12" viewBox="0 0 12 12" xmlns="http://www.w3.org/2000/svg"><rect width="12" height="12" fill="%s"/><line x1="0" y1="0" x2="12" y2="12" stroke="%s" stroke-width="3"/></svg>', Light_Green, Light_Grey))),
#                     Light_Green,
#                     base64enc::base64encode(charToRaw(sprintf('<svg width="8" height="8" viewBox="0 0 8 8" xmlns="http://www.w3.org/2000/svg"><rect width="8" height="8" fill="%s"/><circle cx="4" cy="4" r="1.5" fill="%s"/></svg>', Light_Green, Light_Grey)))
# ))


# Income Functions -------------------------------------------------------------

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
  ((Gross_Income_Y - CPP_Threshold_Y) * CPP_Enhanced_Rate) %>%
    pmax(0) %>%
    pmin(CPP_Enhanced_Income_Deduction_Cap_Y)
}

# Tax functions ----
# Function to calculate taxable income. 
# If the employee does not fill out all fields in TD1 or TD1ON, then the only reduction to taxable income is CPP Enhanced contributions.
Taxable_Income_Formula <- function(Gross_Income_Y) {
  Gross_Income_Y - CPP_Enhanced_Deduct_Formula(Gross_Income_Y)
}

# Function to calculate annual federal income tax
Fed_Tax_Formula <- function(Taxable_Income_Y) {
  Tax <- 0
  if (is.na(Taxable_Income_Y)) {
    NA # NA handling for if statements to account for multiple user input options
  } else {
    for (i in seq_along(Fed_Tax_Rates)) {
      if (Taxable_Income_Y > Fed_Tax_Brackets[i]) {
        Marginal_Income <- pmin(
          Taxable_Income_Y,
          Fed_Tax_Brackets[i + 1],
          na.rm = TRUE # NA handling for people in top income bracket
          ) - Fed_Tax_Brackets[i]
        Tax <- Tax + Marginal_Income * Fed_Tax_Rates[i]
      }
    }
  }
  return(Tax)
}

# Function to calculate annual Ontario income tax
ON_Tax_Formula <- function(Taxable_Income_Y) {
  Tax <- 0
  if (is.na(Taxable_Income_Y)) {
    NA
  } else {
    for (i in seq_along(ON_Tax_Rates)) {
      if (Taxable_Income_Y > ON_Tax_Brackets[i]) {
        Marginal_Income <- pmin(
          Taxable_Income_Y,
          ON_Tax_Brackets[i + 1],
          na.rm = TRUE
          ) - ON_Tax_Brackets[i]
        Tax <- Tax + Marginal_Income * ON_Tax_Rates[i]
      }
    }
  }
  return(Tax)
}

# Ontario surtax is calculated on the basis of net tax (Ontario tax owed less NRTCs).
# The formula is similar to Ontario income tax, but with only two brackets.
ON_Surtax_Formula <- function(ON_Net_Tax_Y) {
  Tax <- 0
  if (is.na(ON_Net_Tax_Y)) {
    NA
  } else {
    for (i in seq_along(ON_Surtax_Rates)) {
      if (ON_Net_Tax_Y > ON_Surtax_Brackets[i]) {
        Marginal_Income <- pmin(
          ON_Net_Tax_Y,
          ON_Surtax_Brackets[i + 1],
          na.rm = TRUE
          ) - ON_Surtax_Brackets[i]
        Tax <- Tax + Marginal_Income * ON_Surtax_Rates[i]
      }
    }
  }
  return(Tax)
}

# Ontario health premimum is calculated on teh basis of taxable income.
# Although similar to the calculation of Ontario income tax, each bracket has a maximum that is less than the rate for that bracket applied to the marginal income span of that bracket.
ON_Health_Premium_Formula <- function(Taxable_Income_Y) {
  Tax <- 0
  if (is.na(Taxable_Income_Y)) {
    NA
  } else {
    for (i in seq_along(ON_Health_Premimum_Rates)) {
      if (Taxable_Income_Y > ON_Health_Premium_Brackets[i]) {
        Marginal_Income <- Taxable_Income_Y - ON_Health_Premium_Brackets[i]
        Tax <- min(
          Marginal_Income * ON_Health_Premimum_Rates[i] + ON_Health_Premium_Bracket_Maxes[i - 1], 
          ON_Health_Premium_Bracket_Maxes[i]
          )
      }
    }
  }
  return(Tax)
}

# Non-refundable tax credit formulas ----

Canada_Employment_NRTC_Formula <- function(Gross_Income_Y)
{
  (Gross_Income_Y) %>%
    # Canada Employment amount has a cap.
    pmin(Canada_Employment_NRTC_Max)
}

# Payroll deduction functions -----------------------------------------------

# This formula calculates benefit unit exemplars' annual payroll tax deduction. For simplicity and based on the limitations of SAMS data, it is assumed that social assistance recipients do not request changes to their tax deductions on the TD1 or TD1ON.
Tax_Deduct_Formula <- function(Gross_Income_Y) {
  Taxable_Income_Y <- Taxable_Income_Formula(Gross_Income_Y)
  # Federal tax is reduced by basic NRTC, EI premiums, CPP base contributions, and the Canada Employment Amount NRTC
  Fed_NRTCs_Y <- sum(
    Fed_Basic_NRTC,
    EI_Deduct_Formula(Gross_Income_Y), 
    CPP_Base_Deduct_Formula(Gross_Income_Y),
    Canada_Employment_NRTC_Formula(Gross_Income_Y)
  ) * Fed_Tax_Rates[1] # Value of deductions are multiplied by lowest fed tax rate
  # Net tax is tax owed less these basic deductions
  Net_Federal_Tax_Y <- (Fed_Tax_Formula(Taxable_Income_Y) - Fed_NRTCs_Y) %>%
    pmax(0) # Cannot be negative
  ON_NRTCs_Y <- sum(
    ON_Basic_NRTC,
    EI_Deduct_Formula(Gross_Income_Y),
    CPP_Base_Deduct_Formula(Gross_Income_Y)
  ) * ON_Tax_Rates[1]
  Net_ON_Tax_Y <- (ON_Tax_Formula(Taxable_Income_Y) - ON_NRTCs_Y) %>%
    pmax(0)
  ON_Surtax_Y <- ON_Surtax_Formula(Net_ON_Tax_Y)
  ON_Health_Premium_Y <- ON_Health_Premium_Formula(Taxable_Income_Y)
  # Ontario also has a tax reduction formula we need to calculate
  ON_Tax_Reduction <- (
    (ON_Basic_Tax_Reduction * 2) - (Net_ON_Tax_Y + ON_Surtax_Y)
  ) %>%
    pmax(0) %>%
    pmin(Net_ON_Tax_Y + ON_Surtax_Y)
  Payroll_Tax_Deduct_Y <- (
    Net_Federal_Tax_Y + Net_ON_Tax_Y + ON_Surtax_Y + ON_Health_Premium_Y - ON_Tax_Reduction
    ) %>%
    pmax(0)
  return(Payroll_Tax_Deduct_Y)
}

# Now integrating all of those formulas into a monthly take-home pay after all payroll deductions.
Take_Home_Pay_Formula <- function(Gross_Income_Y) {
  (Gross_Income_Y - EI_Deduct_Formula(Gross_Income_Y) - CPP_Base_Deduct_Formula(Gross_Income_Y) - CPP_Enhanced_Deduct_Formula(Gross_Income_Y) - Tax_Deduct_Formula(Gross_Income_Y)
   ) / 12 
}

# Function to calculate gross monthly income from monthly take-home pay
Take_Home_To_Gross_Formula <- function(Take_Home_Pay_M) {
  if (is.na(Take_Home_Pay_M)) {
    NA
  } else {
    # Iterative approach to estimate gross income
    Gross_Income_Y <- Take_Home_Pay_M * 12
    # Results <- data.frame(Iteration = integer(), GrossIncome = numeric())
    Iteration <- 0
    Difference <- Inf
    # Iterate until difference between iterations in gross income calculations is less than 10 cents.
    while (Difference >= 1) { 
      Iteration <- Iteration + 1
      Previous_Gross_Income_Y <- Gross_Income_Y
      EI_Deduct_Y <- EI_Deduct_Formula(Gross_Income_Y)
      CPP_Base_Deduct_Y <- CPP_Base_Deduct_Formula(Gross_Income_Y)
      CPP_Enhanced_Deduct_Y <- CPP_Enhanced_Deduct_Formula(Gross_Income_Y)
      # Taxable_Income_Y <- Taxable_Income_Formula(Gross_Income_Y)
      # Fed_Tax_Y <- Fed_Tax_Formula(Taxable_Income_Y)
      Tax_Deduct_Y <- Tax_Deduct_Formula(Gross_Income_Y)
      Deducts <- EI_Deduct_Y + CPP_Base_Deduct_Y + CPP_Enhanced_Deduct_Y + Tax_Deduct_Y
      Gross_Income_Y <- (Take_Home_Pay_M * 12) + Deducts
      Difference <- abs(Gross_Income_Y - Previous_Gross_Income_Y)
      # results <- rbind(results, data.frame(Iteration = iteration, GrossIncome = gross_income))
    }
  }
  return(Gross_Income_Y / 12)
}

# Social Assistance Functions -------------------------------
SA_Reduction_Formula <- function(Take_Home_Pay_M, Disability) {
  if (is.na(Take_Home_Pay_M) | is.na(Disability)) {
    0
  } else {
    if (Disability == "Yes") {
      Flat_Earnings_Exemption <- ODSP_Flat_Earning_Exemption
      Earning_Exemption_Rate <- ODSP_Earning_Exemption_Rate
    } else if (Disability == "No") {
      Flat_Earnings_Exemption <- OW_Flat_Earning_Exemption
      Earning_Exemption_Rate <- OW_Earning_Exemption_Rate
    }
    Partial_Earnings_Exemption <- ((Take_Home_Pay_M - Flat_Earnings_Exemption) * Earning_Exemption_Rate) %>%
      pmax(0)
    SA_Reduction <- (Take_Home_Pay_M - Flat_Earnings_Exemption - Partial_Earnings_Exemption) %>%
      pmax(0)
    return(SA_Reduction)
  } 
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
  # #   # "$secondary" = "#89B0FB",
  # #   # "$success" = "#B0FB89",
   "info" = Primary_Color,
  #    # "warning" = "#c10000",
  #    # "info" = Scenario_1_Color,
      "light" = "#9cbbff",
  #    # "dark" = Scenario_3_Color,
  #    # "green" = "#d3f7c6",
  #     # "blue" = "#2039FF",
  #    # "purple" = "#EAC6F7",
  #    # "brown" = "#F7D2C6"
  # #   #"gray-100" = "#B0FB89"
  # #   # "$gray-200" = "#FB89B0",
  # #   # "$gray-300" = "#89B0FB",
  # #   # "$gray-400" = "#B0FB89",
  # #   # "$gray-500" = "#FB89B0",
  # #   # "$gray-600" = "#89B0FB",
  # #   # "$gray-700" = "#B0FB89",
  # #   # "$gray-800" = "#FB89B0",
  # #   # "$gray-900" = "#89B0FB"
   )

ui <- page_fluid(
  # tags$head(
  #   # tags$style(css) # this is what makes the custom css available 
  # ),
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
                                    choices = c(
                                      " " = "social assistance",
                                      "Ontario Works" = "Ontario Works",
                                      "Ontario Disability Support Program (ODSP)" = "ODSP"
                                    ),
                                    selected = "social assistance"
                        )
                        ),
                 column(3,
                        autonumericInput(
                          "SA_Payment",
                          label = "What is your typical monthly social assistance payment?",
                          value = NULL,
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
                                      c("", "No", "Yes")),
                                    id = "Spouse_Tip"
                        )
                 )
               ),
               fluidRow(
                 column(6,
                        conditionalPanel(condition = "input.Program == 'ODSP' && input.Spouse == 'Yes'",
                        selectInput("Disability_P",
                                    "Has ODSP determined that you have a disability?",
                                    c("Yes", "No")
                                    )
                        )
                        ),
                 column(6,
                        conditionalPanel(
                          condition = "input.Program == 'ODSP' && input.Spouse == 'Yes'",
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
                    <br>
                    <br>
                    <ul>
                    <li><span style='background-color:", Scenario_1_Color, ";'><strong>Scenario 1</strong></span> has been set to your current situation.</li>
                    <li><span style='background-color:", Scenario_2_Color, ";'><strong>Scenario 2</strong></span> has been set to part-time employment at minimum wage ($17.20 an hour).</li>
                    <li><span style='background-color:", Scenario_3_Color, ";'><strong>Scenario 3</strong></span> has been set to full-time employment at minimum wage.</li>
                    </ul>"
                    )
                    ),
               layout_column_wrap(
                 width = NULL,
                 style = css(grid_template_columns = "28% 35% 35%"),
                 # Scenario 1 Card.
                 card(card_title(HTML("<strong>Scenario 1</strong> - Your Current Situation")),
                      style = paste0("background-color: ", Scenario_1_Color, ";"),
                      htmlOutput("Scen_1_Descript"),
                      ),
                 # Scenario 2 Card.
                 card(
                   # class = "stripe-card",
                   card_title(
                     # class = "text-container",
                     htmlOutput("Scen_2_Descript")
                     ),
                   style = paste0("background-color: ", Scenario_2_Color, ";"),
                   card_body(
                     div(
                       # class = "text-container",
                   textInput(
                     "Scen_2_Title",
                     label = tooltip(
                         trigger = list(
                           "Description of Scenario 2",
                           Help_Icon("Info about changing Scenario 2 description")
                          ),
                         "You can change the description of Scenario 2 to match the employment situation you want to calculate",
                         placement = "top"
                       ),
                      value = "Work Part-time at Minimum Wage"
                      )
                   ),
                   div(
                     # class = "text-container",
                   radioButtons(
                        "Format_2",
                        "Prefered income format",
                        choiceValues = c("wage", "pay stub", "salary"),
                        choiceNames = list(
                          tooltip(
                            trigger = list(
                              "Hourly wage",
                              Help_Icon("Info about hourly wage")
                              ),
                            "Enter pay per hour of work and hours worked per week to calculate work earnings for this scenario",
                            placement = "top"
                            ),
                          tooltip(
                            trigger = list(
                              "Monthly take-home pay",
                              Help_Icon("Info about take-home pay")
                            ),
                            "Enter monthly pay received from employer after payroll deductions like taxes, EI, and CPP to calculate work earnings for this scenario",
                            placement = "top"
                          ),
                          tooltip(
                            trigger = list(
                              "Annual salary",
                              Help_Icon("Info about annual salary")
                            ),
                            "Enter annual salary received from employer before payroll deductions (i.e., yearly pay written on job add or employment contract) to calculate work earnings for this scenario",
                            placement = "bottom"
                          )
                        )
                      )
                   ),
                   div(
                     # class = "text-container",
                      uiOutput(
                        "Scen_2_Parameters"
                        )
                      )
                   )
                    ),
                 # Scenario 3 Card.
                 card(
                   # class = "dot-card",
                   card_title(
                     # class = "text-container",
                     htmlOutput("Scen_3_Descript")
                     ),
                   style = paste0("background-color: ", Scenario_3_Color, ";"),
                   card_body(
                     div(
                       # class = "text-container",
                       textInput(
                         "Scen_3_Title",
                         label = tooltip(
                           trigger = list(
                             "Description of Scenario 3",
                             Help_Icon("Info about changing Scenario 3 description")
                           ),
                           "You can change the description of Scenario 3 to match the employment situation you want to calculate",
                           placement = "top"
                         ),
                         value = "Work Full-time at Minimum Wage"
                       ) 
                     ),
                     div(
                       # class = "text-container",
                       radioButtons(
                         "Format_3",
                         "Prefered income format",
                         choiceValues = c("wage", "pay stub", "salary"),
                         choiceNames = list(
                           tooltip(
                             trigger = list(
                               "Hourly wage",
                               Help_Icon("Info about hourly wage")
                             ),
                             "Enter pay per hour of work and hours worked per week to calculate work earnings for this scenario",
                             placement = "top"
                           ),
                           tooltip(
                             trigger = list(
                               "Monthly take-home pay",
                               Help_Icon("Info about take-home pay")
                             ),
                             "Enter monthly pay received from employer after payroll deductions like taxes, EI, and CPP to calculate work earnings for this scenario",
                             placement = "top"
                           ),
                           tooltip(
                             trigger = list(
                               "Annual salary",
                               Help_Icon("Info about annual salary")
                             ),
                             "Enter annual salary received from employer before payroll deductions (i.e., yearly pay written on job add or employment contract) to calculate work earnings for this scenario",
                             placement = "bottom"
                           )
                         )
                       ) 
                     ),
                     div(
                       # class = "text-container",
                       uiOutput("Scen_3_Parameters") 
                     )
                   )
                    )
                 )
             ),
    # Income results tab -----------------------------------------------------
    tabPanel("Income Results",
             selectInput(
               inputId = "Plot_Choice",
               label = "Choose a calculation to display",
               choices = c(
                 "Monthly household income",
                 "Total annual household income",
                 "Total household income by month"
               ),
               selected = "Monthly household income"
             ),
             layout_column_wrap(
               width = 1,
               card(
                 # card_header("Comparing income across scenarios"),
                 card_body(
                   layout_column_wrap(
                     width = NULL,  # Creates 6 columns total
                     heights_equal = "row",
                     style = css(grid_template_columns = "1fr 5fr"),
                     # Legend takes up 1/6 of the space
                     div(
                       uiOutput("pattern_legend")
                     ),
                     # Plot takes up 5/6 of the space
                     div(
                       withSpinner(
                         plotlyOutput("Selected_Income_Plot")
                       )
                     )
                   ),
                   uiOutput("Plot_Description")
                 )
               ),
               card(
                 card_header("Income and other benefits"),
                 reactableOutput("Benefits_Table")
               )
             )
    ),
    
    # Data Validation tab -----------------------------------------------------
    # Turn on this panel to see data calculations for the plots.
    # tabPanel("Data Validation",
    #          fluidRow(
    #            column(
    #              9,
    #              # withSpinner(plotOutput("Income_Plot"
    #              #                               )
    #              #                    )
    #              tableOutput("Income_Table")
    #              # textOutput("Gross_Output_1_PM"),
    #              # textOutput("Gross_Output_2_PM"),
    #              # textOutput("Gross_Output_3_PM"),
    #              # textOutput("Gross_Output_1_SM"),
    #              # textOutput("Gross_output_2_SM"),
    #              # textOutput("Gross_output_3_SM")
    #                   )
    #                   )
    #          ),
    # More information tab ----
    tabPanel("More Information",
             fluidRow(
               column(
                 9,
                 HTML("This page is currently blank. It may be filled in the future with additional information or FAQ.")
               )
             ))
               )
  )
server <- function(input, output, session) {
  # thematic::thematic_shiny()
  shiny_session <- getDefaultReactiveDomain()
  
  # enabling validation error messages
  IV <- InputValidator$new()
  IV$add_rule("Program", sv_required())
  IV$add_rule("Program", ~ if (. == "social assistance") "Required")
  IV$add_rule("SA_Payment", sv_required())
  IV$add_rule("Spouse", sv_required())
  
  # About Me tab server ---------
  # Creating a variable that tracks the name of the program to which user belongs.
  # Program_Name <- reactiveVal("social assistance")
  # observeEvent(input$Program, {
  #   if(input$Program == "Ontario Works") {
  #     Program_Name("Ontario Works")
  #   } else {
  #     Program_Name("ODSP")
  #   }
  # }, ignoreInit = TRUE)
  
  # Hiding missing input feedback once input detected.
  observeEvent(input$Program, {
    if (input$Program != "social assistance") {
      # hideFeedback("Program")
      removeNotification("Missing_About_Me")
    }
  })
  observeEvent(input$Spouse, {
    if (input$Spouse != "") {
      # hideFeedback("Spouse")
      removeNotification("Missing_About_Me")
    }
  })
  observeEvent(input$SA_Payment, {
    if (!is.null(input$SA_Payment)) {
      # hideFeedback("SA_Payment")
      removeNotification("Missing_About_Me")
    }
  })

  # Making some of the UI instructions responsive to user input.
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
                     input$Program,
                     " unit. Do not include yourself ",
                     Spouse_Instruct,
                     " in this number.")
      )
  })
  observeEvent(input$Program, {
    label <- paste0(
      "What is your typical monthly ",
      input$Program,
      " payment?"
      )
    tip <- paste0(
      "Your most recent ",
      input$Program,
      " payment can be found on Ontario.ca/MyBenefits, your bank statement, or an ",
      input$Program,
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
  
  # Titles of cards 2 and 3 supplied by user to reflect the scenarios they want calculated.
  output$Scen_2_Descript <- renderText({
    paste("<strong>Scenario 2</strong> -", input$Scen_2_Title)
  })
  output$Scen_3_Descript <- renderText({
    paste("<strong>Scenario 3</strong> -", input$Scen_3_Title)
  })
  
  # Want to make the default scenario descriptions clear in that they assume both spouses work.
  # observeEvent(input$Spouse, {
  #   if (input$Spouse == "Yes") {
  #     updateTextInput(
  #       session = shiny_session,
  #       inputId = "Scen_2_Title",
  #       value = "Both spouses work full-time at minimum wage"
  #     )
  #   } else {
  #     updateTextInput(
  #       session = shiny_session,
  #       inputId = "Scen_2_Title",
  #       value = "Work full-time at minimum wage"
  #     )
  #   }
  # })
  # observeEvent(input$Spouse, {
  #   if (input$Spouse == "Yes") {
  #     updateTextInput(
  #       session = shiny_session,
  #       inputId = "Scen_3_Title",
  #       value = "Both spouses work full-time at minimum wage"
  #     )
  #   } else {
  #     updateTextInput(
  #       session = shiny_session,
  #       inputId = "Scen_3_Title",
  #       value = "Work full-time at minimum wage"
  #     )
  #   }
  # })
  
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
    if(input$Format_2 == "wage") {
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
      # )
      # Input for monthly take-home pay option.
    } else if (input$Format_2 == "pay stub") {
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
          label = "My monthly take-home pay from work",
          value = Part_Time_Take_Home_Pay,
          min = 0,
          currencySymbol = "$"
        ),
        conditionalPanel(
          condition = "input.Spouse == 'Yes'",
          autonumericInput(
            "Take_Home_Pay_2_SM",
            label = "Spouse's monthly take-home pay from work",
            value = Part_Time_Take_Home_Pay,
            min = 0,
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
          "Gross_Salary_2_PY",
          label = "My annual work salary",
          value = Part_Time_Salary,
          min = 0,
          currencySymbol = "$"
        ),
        conditionalPanel(
          condition = "input.Spouse == 'Yes'",
          autonumericInput(
            "Gross_Salary_2_SY",
            label = "Spouse's annual work salary",
            value = Part_Time_Salary,
            min = 0,
            currencySymbol = "$"
          )
        )
      )
    }
  })
  # Scenario 3 inputs.
  output$Scen_3_Parameters <- renderUI( {
    if(input$Format_3 == "wage") {
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
    } else if (input$Format_3 == "pay stub") {
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
          label = "My monthly take-home pay from work",
          value = Full_Time_Take_Home_Pay,
          min = 0,
          currencySymbol = "$"
          ),
        conditionalPanel(
          condition = "input.Spouse == 'Yes'",
          autonumericInput(
            "Take_Home_Pay_3_SM",
            label = "Spouse's monthly take-home pay from work",
            value = Full_Time_Take_Home_Pay,
            min = 0,
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
          "Gross_Salary_3_PY",
          label = "My annual work salary",
          value = Full_Time_Salary,
          min = 0,
          currencySymbol = "$"
        ),
        conditionalPanel(
          condition = "input.Spouse == 'Yes'",
          autonumericInput(
            "Gross_Salary_3_SY",
            label = "Spouse's annual work salary",
            value = Full_Time_Salary,
            min = 0,
            currencySymbol = "$"
          )
        )
      )
    }
  })

  
  # Income results tab server -------------------------------------------------
  
  # Description of the income results plots
  output$Plot_Description <- renderUI({
    if (input$Plot_Choice == "Monthly household income") {
      HTML(paste(
        "The <b>Monthly household income</b> calculation shows each work scenario's typical income from working and",
        input$Program,
        "after all deductions. Tax refunds and other benefits are not shown in this calculation because they are not paid monthly. See the other two calculations for estimates of those payments."
        ))
    } else if (input$Plot_Choice == "Total annual household income") {
      HTML(paste0(
        "The <b>Total annual household income</b> calculation shows each work scenario's typical income from all sources, including working, ",
        input$Program,
        ", tax refunds, and other benefits. Tax refunds and other benefits are paid periodically, and an estimated schedule of these payments can be seen in the <b>Total household income by month</b>."
        ))
    }
  })
  
  # Creating colors for scenarios
  Scenario_Colors <- c(Scenario_1_Color, Scenario_2_Color, Scenario_3_Color)
  
  # Create reactive patterns for income sources
  Income_Patterns <- reactiveVal({
    list(
      "Earnings after payroll deductions" = list(
        shape = ".",       # dots
        fillmode = "overlay",
        size = 8,
        solidity = 0.1,
        fgcolor = Light_Grey,
        spacing = 2
      ),
      "SA payments" = list(
        shape = "\\",      # diagonal lines
        fillmode = "overlay",
        size = 4,
        solidity = 0.2,
        fgcolor = Light_Grey
      ),
      "Tax refund" = list(
        shape = "-",      # diagonal lines
        fillmode = "overlay",
        size = 6,
        solidity = 0.2,
        fgcolor = Light_Grey
      ),
      "Other benefits" = list(
        shape = "x",       # cross hatch
        fillmode = "overlay",
        size = 7,
        solidity = 0.6,
        fgcolor = Light_Grey
        # shape = "",        # solid fill
        # solidity = 1
      )
    )
  })
  
  Dynamic_Income_Patterns <- reactive({ 
    patterns <- Income_Patterns() # Store the SA payments pattern 
    sa_pattern <- patterns[["SA payments"]] 
    # Remove the old SA payments entry 
    patterns[["SA payments"]] <- NULL 
    # Add the new entry with dynamic name 
    patterns[[paste(input$Program, "payments")]] <- sa_pattern 
    patterns 
    })
  
  output$Selected_Income_Plot <- renderPlotly({
    
    # Modify the plot data to match the requested view
    if (input$Plot_Choice == "Monthly household income") {
      Selected_Plot_Data <- Plot_Data() %>%
        filter(Income_Source != "Tax refund" & Income_Source != "Other benefits")
    } else if (input$Plot_Choice == "Total annual household income") {
      Selected_Plot_Data <- Plot_Data() %>%
        mutate(Income = Income * 12)
    } else if (input$Plot_Choice == "Total household income by month") {
      Selected_Plot_Data <- Plot_Data() %>%
        slice(rep(1:n(), each = 12)) %>%
        mutate(
          Month = rep(1:12, length.out = n()),
          Month_Label = factor(month.name[Month], levels = month.name),
          Income = case_when(
            Income_Source == "Tax refund" & Month != 4 ~ 0,
            Income_Source == "Tax refund" & Month == 4 ~ Income * 12,
            Income_Source == "Other benefits" & !Month %in% c(1, 4, 7, 10) ~ 0,
            Income_Source == "Other benefits" & Month %in% c(1, 4, 7, 10) ~ Income * 3,
            TRUE ~ Income
          ),
          Position = Month * 4 + case_when(
            Scenario == 1 ~ 0,
            Scenario == 2 ~ 1,
            Scenario == 3 ~ 2
          ) - 3
        )
    }
    
    # Calculate totals based on plot type
    if(input$Plot_Choice == "Total household income by month") {
      Totals <- Selected_Plot_Data %>%
        group_by(Position, Scenario_Label) %>%
        summarise(Total = sum(Income), .groups = 'drop')
    } else {
      Totals <- Selected_Plot_Data %>%
        group_by(Scenario_Label) %>%
        summarise(Total = sum(Income))
    }
    
    # Generate the dynamic order of income sources
    Dynamic_Income_Patterns <- Dynamic_Income_Patterns()
    Income_Order <- names(Dynamic_Income_Patterns)
    
    # Sort the data according to the desired order of income sources
    Selected_Plot_Data <- Selected_Plot_Data %>%
      mutate(Income_Source = factor(Income_Source, levels = Income_Order))
    
    if(input$Plot_Choice == "Total household income by month") {
      # Monthly view with grouped bars
      Plot <- plot_ly()
      
      # Loop through scenarios
      for(Scenario_Idx in 1:3) {
        # Filter data for this scenario
        Scenario_Data <- Selected_Plot_Data %>%
          filter(Scenario == Scenario_Idx)
        
        # Add traces in reverse order for stacking, but keep original names
        for(i in length(Income_Order):1) {
          Source <- Income_Order[i]
          Source_Data <- Scenario_Data %>%
            filter(Income_Source == Source)
          
          Show_Legend <- Scenario_Idx == 1
          
          Plot <- Plot %>%
            add_trace(
              data = Source_Data,
              x = ~Position,
              y = ~Income,
              name = Source,
              type = "bar",
              legendgroup = Source,
              showlegend = FALSE,
              stackgroup = paste("stack", Scenario_Idx),
              marker = list(
                color = Scenario_Colors[Scenario_Idx],
                pattern = Dynamic_Income_Patterns[[Source]]
              ),
              hovertemplate = paste0(
                "<b>$%{y:,.0f}</b><br>",
                "<b>Scenario:</b> ", Source_Data$Scenario_Label, "<br>",
                "<b>Income Source:</b> ", Source, "<br>",
                "<extra></extra>"
              )
            )
        }
      }
      
      # Layout for monthly view
      Plot <- Plot %>%
        layout(
          barmode = "stack",
          bargap = 0.2,
          bargroupgap = 0,
          title = paste(input$Plot_Choice, "sources"),
          xaxis = list(
            title = "Month",
            tickangle = 45,
            ticktext = month.name,
            tickvals = seq(1, 45, by = 4),
            tickmode = "array"
          ),
          yaxis = list(
            title = "Monthly Income",
            tickprefix = "$",
            tickformat = ",.0f"
          ),
          legend = list(traceorder = "reversed")
        ) %>%
        add_annotations(
          x = ~Position,
          y = ~Total,
          text = ~paste0("$", formatC(Total, format="f", big.mark=",", digits=0)),
          data = Totals,
          showarrow = FALSE,
          yshift = 10
        )
    } else {
      # Initialize plot
      Plot <- plot_ly()
      
      # Adding traces for each scenario-income_source combination
      for(Scenario_Idx in 1:3) {
        # Add traces in reverse order for stacking, but keep original names
        for(i in length(Income_Order):1) {
          Source <- Income_Order[i]
          Plot_Data_Trace <- Selected_Plot_Data %>%
            filter(Income_Source == Source, Scenario == !!Scenario_Idx)
          Show_Legend <- Scenario_Idx == 1
          
          Plot <- Plot %>%
            add_trace(
              data = Plot_Data_Trace,
              x = ~Scenario_Label,
              y = ~Income,
              name = Source,
              type = "bar",
              legendgroup = Source,
              showlegend = FALSE,
              stackgroup = paste("stack", Scenario_Idx),
              marker = list(
                color = Scenario_Colors[Scenario_Idx],
                pattern = Dynamic_Income_Patterns[[Source]]
              ),
              hovertemplate = paste0(
                "<b>$%{y:,.0f}</b><br>",
                Source,
                "<extra></extra>"
              )
            )
        }
      }
      
      # Layout for original views
      Plot <- Plot %>%
        layout(
          barmode = "stack",
          title = paste(input$Plot_Choice, "sources"),
          xaxis = list(title = "Your Work Scenarios"),
          yaxis = list(
            title = "Monthly Income",
            tickprefix = "$",
            tickformat = ",.0f"
          ),
          legend = list(traceorder = "reversed"),
          annotations = lapply(1:nrow(Totals), function(i) {
            list(
              x = Totals$Scenario_Label[i],
              y = Totals$Total[i],
              text = paste0("<b>$", format(round(Totals$Total[i]), big.mark = ","), "</b>"),
              showarrow = FALSE,
              yshift = 15,
              font = list(size = 14),
              bgcolor = "white",
              bordercolor = "white",
              borderwidth = 2,
              borderpad = 2
            )
          })
        )
    }
    
    Plot
  })
  
  # Creating a custom legend table
  output$pattern_legend <- renderUI({
    # Define all legend items in a list
    all_legend_items <- list(
      earnings = div(
        style = "display: flex; align-items: flex-start; margin-bottom: 10px;",
        div(
          style = "min-width: 30px; width: 30px; height: 20px; margin-right: 10px; background-color: white; 
              background-image: radial-gradient(black 1px, transparent 1px);
              background-size: 4px 4px; flex-shrink: 0;",
        ),
        div(
          style = "width: calc(100% - 40px);",
          "Earnings after payroll deductions"
        )
      ),
      
      tax_refund = div(
        style = "display: flex; align-items: flex-start; margin-bottom: 10px;",
        div(
          style = "min-width: 30px; width: 30px; height: 20px; margin-right: 10px; background-color: white;
              background-image: repeating-linear-gradient(0deg, black 0, black 1px, transparent 1px, transparent 4px); flex-shrink: 0;",
        ),
        div(
          style = "width: calc(100% - 40px);",
          "Tax refund"
        )
      ),
      
      other_benefits = div(
        style = "display: flex; align-items: flex-start; margin-bottom: 10px;",
        div(
          style = "min-width: 30px; width: 30px; height: 20px; margin-right: 10px; background-color: white;
              background-image: repeating-linear-gradient(45deg, black 0, black 1px, transparent 1px, transparent 4px),
                              repeating-linear-gradient(-45deg, black 0, black 1px, transparent 1px, transparent 4px); flex-shrink: 0;",
        ),
        div(
          style = "width: calc(100% - 40px);",
          "Other benefits"
        )
      ),
      
      program_payments = div(
        style = "display: flex; align-items: flex-start; margin-bottom: 10px;",
        div(
          style = "min-width: 30px; width: 30px; height: 20px; margin-right: 10px; background-color: white;
              background-image: repeating-linear-gradient(45deg, black 0, black 1px, transparent 1px, transparent 4px); flex-shrink: 0;",
        ),
        div(
          style = "width: calc(100% - 40px);",
          paste(input$Program, "payments")
        )
      )
    )
    
    # Select which items to show based on input$Plot_Choice
    selected_items <- if (input$Plot_Choice == "Monthly household income") {
      # Only show earnings and program payments
      list(
        all_legend_items$earnings,
        all_legend_items$program_payments
      )
    } else {
      # Show all items
      all_legend_items
    }
    
    # Create the final legend output
    div(
      style = "border: 1px solid #ddd; padding: 15px; margin-left: 20px;",
      h6(HTML("<strong>Income Sources</strong>")),
      tagList(selected_items)
    )
  })
  
  # Create reactive data
  table_data <- reactive({
    df <- data.frame(
      RowNames = paste("Row", 1:5),  # Add row names as first column
      Column1 = c(1:5), #* input$multiplier,
      Column2 = c(2:6), #* input$multiplier,
      Column3 = c(3:7) #* input$multiplier
    )
    df
  })
  
  output$Benefits_Table <- renderReactable({
    reactable(
      table_data(),
      columns = list(
        RowNames = colDef(  # Row names column
          name = " ",
          style = list(
            background = "#e6e6e6",
            fontWeight = "bold",
            borderRight = "2px solid #555"
            # borderLeft = "1px solid #ffffff",
             # Remove left border
          ),
          headerStyle = list(
            background = "white",  # Make top-left cell white
            borderBottom = "0px"
            # borderLeft = "0px"    # Remove left border in header
          )
        ),
        Column1 = colDef(
          name = "Your current situation",
          style = list(background = Scenario_1_Color),  # Light blue
          headerStyle = list(
            background = Scenario_1_Color,  # Match column color
            fontWeight = "bold",
            borderBottom = "2px solid #555"
          )
        ),
        Column2 = colDef(
          name = input$Scen_2_Title,
          style = list(background = Scenario_2_Color),  # Light pink
          headerStyle = list(
            background = Scenario_2_Color,  # Match column color
            fontWeight = "bold",
            borderBottom = "2px solid #555"
          )
        ),
        Column3 = colDef(
          name = input$Scen_3_Title,
          style = list(background = Scenario_3_Color),  # Light green
          headerStyle = list(
            background = Scenario_3_Color,  # Match column color
            fontWeight = "bold",
            borderBottom = "2px solid #555"
          )
        )
      ),
      theme = reactableTheme(
        borderColor = "#555",
        style = list(
          height = "auto"  # Prevent table from filling card height
        )
      ),
      bordered = FALSE,
      highlight = TRUE
    )
  })
  
  # Data processing server ----------------------------------------------------
  
  # Setting up default values so that unrendered inputs from the work scenarios page don't throw errors
  Take_Home_Pay_2_PM_Default <- reactiveVal(Part_Time_Take_Home_Pay)
  # Making it possible to update the default value when rendered
  observeEvent(input$Take_Home_Pay_2_PM, {
    Take_Home_Pay_2_PM_Default(input$Take_Home_Pay_2_PM)
  }, ignoreInit = TRUE, ignoreNULL = TRUE
  )
  # Reapeating this process for all the work scenario inputs
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
  Gross_Salary_2_PY_Default <- reactiveVal(Part_Time_Salary)
  observeEvent(input$Gross_Salary_2_PY, {
    Gross_Salary_2_PY_Default(input$Gross_Salary_2_PY)
  }, ignoreInit = TRUE, ignoreNULL = TRUE
  )
  Gross_Salary_3_PY_Default <- reactiveVal(Full_Time_Salary)
  observeEvent(input$Gross_Salary_3_PY, {
    Gross_Salary_3_PY_Default(input$Gross_Salary_3_PY)
  }, ignoreInit = TRUE, ignoreNULL = TRUE
  )
  Gross_Salary_2_SY_Default <- reactiveVal(Part_Time_Salary)
  observeEvent(input$Gross_Salary_2_SY, {
    Gross_Salary_2_SY_Default(input$Gross_Salary_2_SY)
  }, ignoreInit = TRUE, ignoreNULL = TRUE
  )
  Gross_Salary_3_SY_Default <- reactiveVal(Full_Time_Salary)
  observeEvent(input$Gross_Salary_3_SY, {
    Gross_Salary_3_SY_Default(input$Gross_Salary_3_SY)
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
    IV$enable()
    # Do not perform the output until all the input is available.
    req(IV$is_valid())
    Scenarios <- tibble(
      Scenario = c(1:3),
      Scenario_Label = factor( # factor with levels controls order of scenarios in subsequent plots
        c(
          "Your Current Situation",
          .env$input$Scen_2_Title,
          .env$input$Scen_3_Title
          ),
        levels = c(
          "Your Current Situation",
          .env$input$Scen_2_Title,
          .env$input$Scen_3_Title
        )
      )
    )
    Calculated_Scenarios <- Scenarios %>% # Extended mutate command to calculate all of the variables needed for the plot.
      mutate(
        Program = .env$input$Program,
        Spouse = .env$input$Spouse,
        Disability_P = case_when(
          .data$Program == "ODSP"
          ~ .env$input$Disability_P,
          .default = "No"
        ),
        Disability_S = case_when(
          .data$Spouse == "Yes"
          ~ .env$input$Disability_S
          ), 
        Income_Format = c(
          "pay stub",
          case_when(
            .env$input$Format_2 == "wage"
            ~ "wage",
            .env$input$Format_2 == "pay stub"
            ~ "pay stub",
            .env$input$Format_2 == "salary"
            ~ "salary"
            ),
          case_when(
            .env$input$Format_3 == "wage"
            ~ "wage",
            .env$input$Format_3 == "pay stub"
            ~ "pay stub",
            .env$input$Format_3 == "salary"
            ~ "salary"
          )
        ),
        Wage_P = c(
          NA,
          case_when(
            .env$input$Format_2 == "wage"
            ~ .env$input$Wage_2_P
          ),
          case_when(
            .env$input$Format_3 == "wage"
            ~ .env$input$Wage_3_P
          )
        ),
        Wage_S = case_when(
          .env$input$Spouse == "Yes"
          ~ c(
            NA,
            case_when(
              .env$input$Format_2 == "wage"
              ~ .env$input$Wage_2_S
              ),
            case_when(
              .env$input$Format_3 == "wage"
              ~ .env$input$Wage_3_S
              )
            )
          ),
        Hours_PW = c(
          NA,
          case_when(
            .env$input$Format_2 == "wage"
            ~ .env$input$Hours_2_PW
          ),
          case_when(
            .env$input$Format_3 == "wage"
            ~ .env$input$Hours_3_PW
          )
        ),
        Hours_SW = case_when(
          .env$input$Spouse == "Yes"
          ~ c(
            NA,
            case_when(
              .env$input$Format_2 == "wage"
              ~ .env$input$Hours_2_SW
            ),
            case_when(
              .env$input$Format_3 == "wage"
              ~ .env$input$Hours_3_SW
            )
          )
        ),
        Gross_Income_PM = case_when(
          # !is.na(.data$Wage_P) & !is.na(.data$Hours_PW)
          .data$Income_Format == "wage"
          ~ Wage_To_Gross_Formula(
            .data$Wage_P,
            .data$Hours_PW
            ),
          .data$Income_Format == "pay stub"
          # is.na(.data$Wage_P) & is.na(.data$Hours_PW)
          ~ map_dbl(
            c(
              .env$input$Take_Home_Pay_1_PM,
              .env$Take_Home_Pay_2_PM_Default(),
              .env$Take_Home_Pay_3_PM_Default()
            ),
            Take_Home_To_Gross_Formula
            ),
          .data$Income_Format == "salary"
          ~ map_dbl(
            c(
              NA,
              .env$Gross_Salary_2_PY_Default(),
              .env$Gross_Salary_3_PY_Default()
              ),
            ~{.x / 12} # converting annual salary to monthly gross pay
            )
          ),
        Gross_Income_SM = case_when(
          .data$Income_Format == "wage" & .data$Spouse == "Yes"
          ~ Wage_To_Gross_Formula(
            .data$Wage_S,
            .data$Hours_SW
            ),
          .data$Income_Format == "pay stub" & .data$Spouse == "Yes"
          # is.na(.data$Wage_S) & is.na(.data$Hours_SW)
          ~ map_dbl(
            c(
              .env$input$Take_Home_Pay_1_SM,
              .env$Take_Home_Pay_2_SM_Default(),
              .env$Take_Home_Pay_3_SM_Default()
            ),
            Take_Home_To_Gross_Formula
            ),
          .data$Income_Format == "salary" & .data$Spouse == "Yes"
          ~ map_dbl(
            c(
              NA,
              .env$Gross_Salary_2_SY_Default(),
              .env$Gross_Salary_3_SY_Default()
            ),
            ~{.x / 12}
          )
          ),
        Take_Home_Pay_PM = case_when(
          # is.na(.data$Wage_P) & is.na(.data$Hours_PW)
          .data$Income_Format == "pay stub"
          ~ c(
            .env$input$Take_Home_Pay_1_PM,
            .env$Take_Home_Pay_2_PM_Default(),
            .env$Take_Home_Pay_3_PM_Default()
            ),
          .data$Income_Format == "wage" | .data$Income_Format == "salary"
          # !is.na(.data$Wage_P) & !is.na(.data$Hours_PW)
          ~ map_dbl( # map function on account of if statements to handle vector inputs
            .data$Gross_Income_PM * 12, # Annualizing the monthly gross income
            Take_Home_Pay_Formula
            )
          ),
        Take_Home_Pay_SM = case_when(
          .data$Income_Format == "pay stub" & .data$Spouse == "Yes"
          ~ c(
            .env$input$Take_Home_Pay_1_SM,
            .env$Take_Home_Pay_2_SM_Default(),
            .env$Take_Home_Pay_3_SM_Default()
            ),
          (.data$Income_Format == "wage" | .data$Income_Format == "salary") & .data$Spouse == "Yes"
          ~ map_dbl(
            .data$Gross_Income_SM * 12,
            Take_Home_Pay_Formula
            )
          ),
        Unreduced_SA_Pay = .env$input$SA_Payment + SA_Reduction_Formula(
          .env$input$Take_Home_Pay_1_PM, .data$Disability_P[1] # reduction in SA payment due to principal's earnings
        ) + SA_Reduction_Formula(
          .env$input$Take_Home_Pay_1_SM, .data$Disability_S[1] # reduction in SA payment due to spouse's earnings
        ),
        Reduced_SA_Pay_BM = (.data$Unreduced_SA_Pay - map2_dbl(
          .data$Take_Home_Pay_PM,
          .data$Disability_P,
          SA_Reduction_Formula
        ) - map2_dbl(
          .data$Take_Home_Pay_SM,
          .data$Disability_S,
          SA_Reduction_Formula
        )
        ) %>%
          pmax(0)
        )
    Calculated_Scenarios <- Calculated_Scenarios %>%
      rowwise() %>%
      mutate(
        Take_Home_Pay_BM = sum(
          .data$Take_Home_Pay_PM,
          .data$Take_Home_Pay_SM,
          na.rm = TRUE
        )
      )
    Calculated_Scenarios
    })
  
  # Getting the data in shape for plotting
  Plot_Data <- reactive({
    
    # REPLACE these temporary values once they are properly calculated
    Other_Benefits = c(100, 200, 125)
    
    #Tibble transformation
    Income_Tibble() %>%
      mutate(
        Tax_Refund_PY = map_dbl( # This is a placeholder for the purpose of building prototype plots and must be REPLACED once all income tax formulas are written.
          Gross_Income_PM * 12,
          Tax_Deduct_Formula
        ),
        Tax_Refund_SY = map_dbl( # REPLACE
          Gross_Income_SM * 12,
          Tax_Deduct_Formula
        ),
        # sum Tax_Refund_PY and Tax_Refund_SY, accounting for the fact that Tax_Refund_SY may be NA
        Tax_Refund_BM = (Tax_Refund_PY + coalesce(Tax_Refund_SY, 0)) / 12,
        Other_Benefits_BM = Other_Benefits[row_number()] # REPLACE
      ) %>%
      select( # Reducing the data to just the columns needed
        Scenario,
        Scenario_Label,
        Reduced_SA_Pay_BM,
        Take_Home_Pay_BM,
        Tax_Refund_BM,
        Other_Benefits_BM
      ) %>%
      rename( # Alphabetizing the order in which we want stacked bars to appear vertically in the graph
        Earnings_after_payroll_deductions = Take_Home_Pay_BM,
        !!paste0(input$Program, "_payments") := Reduced_SA_Pay_BM,
        Tax_refund = Tax_Refund_BM,
        Other_benefits = Other_Benefits_BM
      ) %>%
      pivot_longer( # stacked plots require long-form data
        # Do not pivot Scenario and Scenario_Label
        cols = -c(Scenario, Scenario_Label),
        names_to = "Income_Source",
        values_to = "Income"
      ) %>%
      mutate(
        # Making human readable plot labels
        Income_Source = str_replace_all(Income_Source, "_", " ")
      )
  }) 
  
  # Putting the calculated values in a table for viewing while troubleshooting
  output$Income_Table <- renderTable( {
    Income_Tibble()
  })
}
shinyApp(ui, server)