source("sm_calculator/smith_analysis.R")

employment_income <- c(50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000)
home_value <- c(500000, 1000000, 1500000, 2000000, 2500000, 3000000)
mortgage_balance <- c(100000, 200000, 300000, 400000, 500000, 1000000, 1500000, 2000000, 2500000)
loc_interest_rate <- c(0.03, 0.04, 0.05, 0.06, 0.07, 0.08)
dividend_yeild <- c(0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10, 0.11, 0.12)
dividend_type_split <- c(0, 0.25, 0.5, 0.75, 1)
data.table::expand.grid(employment_income, home_value, loc_interest_rate, dividend_yeild, mortgage_balance, dividend_type_split)
test_cases <- expand.grid(employment_income, home_value, loc_interest_rate, dividend_yeild, mortgage_balance, dividend_type_split)

# tax function tests
calculate_tax_liability_sm(employment_income = 100000, 
                           mortgage_balance = 500000, 
                           home_value = 1000000, 
                           dividend_yeild = 0.02, 
                           dividend_type_split = 0.5, 
                           loc_balance = 100000, 
                           loc_interest_rate = 0.05)

employment_income, 
dividend_income, 
dividend_type_split, 
interest_expense