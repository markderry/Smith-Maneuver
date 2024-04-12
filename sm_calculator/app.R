#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(data.table)
source("smith_analysis.R")

employment_income <- c(50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000)
home_value <- c(500000, 1000000, 1500000, 2000000, 2500000, 3000000)
mortgage_balance <- c(100000, 200000, 300000, 400000, 500000, 1000000, 1500000, 2000000, 2500000)
loc_interest_rate <- c(0.03, 0.04, 0.05, 0.06, 0.07, 0.08)
dividend_yeild <- c(0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10, 0.11, 0.12)
dividend_type_split <- c(0, 0.25, 0.5, 0.75, 1)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Smith Maneuver"),
  tabPanel(
    "Smith Maneuver Analysis",
    # Sidebar with a slider input for number of bins 
    # create sidebar
    sidebarLayout(
      sidebarPanel(
                   # Input variables
                   
                   numericInput("value_of_home", label = "Value of Home:", value = 1000000, step = 100000),
                   numericInput("mortage", label = "Mortgage:", value = 500000, step = 50000),
                   numericInput("line_of_credit_interest", label = "Line of Credit Interest:", value = 0.07, step = 0.005),
                   numericInput("investment_yeild", label = "Investment Yeild:", value = 0.09, step = 0.005),
                   numericInput("income", label = "Employment Income:", value = 100000, step = 10000)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tableOutput("results_table")
      )
    )
  ),
  # tabPanel(
  #   "Solution Space",
  #   # filter panel
  #   sidebarLayout(
  #     sidebarPanel(
  #                  selectInput(
  #                    "employment_income_selection",
  #                    label = "Employment Income",
  #                    choices = employment_income
  #                  ),
  #                  selectInput(
  #                    "home_value_selection",
  #                    label = "Home Value",
  #                    choices = home_value
  #                  ),
  #                  selectInput(
  #                    "mortgage_balance_selection",
  #                    label = "Mortgage Balance",
  #                    choices = mortgage_balance
  #                  ),
  #                  selectInput(
  #                    "loc_interest_rate_selection",
  #                    label = "LOC Interest Rate",
  #                    choices = loc_interest_rate
  #                  ),
  #                  selectInput(
  #                    "dividend_yeild_selection",
  #                    label = "Dividend Yeild",
  #                    choices = dividend_yeild
  #                  ),
  #                  selectInput(
  #                    "dividend_type_split_selection",
  #                    label = "Dividend Type Split",
  #                    choices = dividend_type_split
  #                  )
  #     ),
  #     
  #     # Show a plot of the generated distribution
  #     mainPanel(
  #     
  #     )
  #   )
  # )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observe(
    input$run
  )
  
  output$value_of_home <- renderText({as.character(input$value_of_home)})
  output$mortage <- renderText({ input$mortage })
  output$mortgage_interest <- renderText({ input$mortgage_interest })
  output$mortgage_years <- renderText({ input$mortgage_years })
  output$line_of_credit_interest <- renderText({ input$line_of_credit_interest })
  output$investment_yeild <- renderText({ input$investment_yeild })
  output$income <- renderText({ input$income })
  # output$yearly_prepayment <- renderText({ input$yearly_prepayment })
  
  
  # create table
  output$results_table <- renderTable({
    employment_income <- input$income
    home_value <- input$value_of_home
    mortgage_balance <- input$mortage
    loc_interest_rate <- input$line_of_credit_interest
    loc_ballance <- 0.65 * (home_value - mortgage_balance)
    dividend_yeild <- input$investment_yeild
    
    for (i in 1:2) {
      if (i == 1) {
        interest_expense <- loc_interest_rate * loc_ballance
        dividend_income <- loc_ballance * dividend_yeild
        scenarios <- calculate_tax_liability_sm(employment_income = employment_income, 
                                                dividend_income = 0, 
                                                interest_expense = 0, 
                                                dividend_type_split = 0.5)
      } else {
        # loc_ballance <- loc_ballance + scenarios$sm_effective_value[i - 1]
        # if (loc_ballance > 0.65 * (home_value)) {
        #   loc_ballance <- 0.65 * (home_value)
        # }
        interest_expense <- loc_interest_rate * loc_ballance
        dividend_income <- loc_ballance * dividend_yeild
        scenarios <- rbind(scenarios, calculate_tax_liability_sm(employment_income = employment_income, 
                                                                 dividend_income = dividend_income, 
                                                                 dividend_type_split = 0.5, 
                                                                 interest_expense = interest_expense)
        )
      }
    }
    # convert all columns to dollar format
    scenarios[, employment_income := dollar(employment_income, big.mark = ",", accuracy = 1)]
    scenarios[, dividend_income := dollar(dividend_income, big.mark = ",", accuracy = 1)]
    scenarios[, interest_expense := dollar(interest_expense, big.mark = ",", accuracy = 1)]
    scenarios[, income_tax := dollar(income_tax, big.mark = ",", accuracy = 1)]
    scenarios[, div_tax := dollar(div_tax, big.mark = ",", accuracy = 1)]
    scenarios[, tax_savings_from_interest := dollar(tax_savings_from_interest, big.mark = ",", accuracy = 1)]
    scenarios[, net_income := dollar(net_income, big.mark = ",", accuracy = 1)]
    scenarios[, sm_effective_value := dollar(sm_effective_value, big.mark = ",", accuracy = 1)]
    scenarios_out <- data.table::transpose(scenarios, keep.names = "Variable")
    setnames(scenarios_out, c("V1", "V2"), c("No Maneuver", "Smith Maneuver"))
  },
  striped = TRUE,
  hover = TRUE,
  width = "100%"
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
