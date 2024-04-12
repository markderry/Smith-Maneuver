library(data.table)
library(snakecase)
library(scales)

tax_rates <- fread("2024_marginal_tax_rates.csv")
colnames(tax_rates) <- to_snake_case(colnames(tax_rates))
tax_rates[, income_tax_rate := as.numeric(gsub(pattern = "%", x = income_tax_rate, replacement = "", fixed = TRUE)) / 100]
tax_rates[, capital_gains_rate := as.numeric(gsub(pattern = "%", x = capital_gains_rate, replacement = "", fixed = TRUE)) / 100]
tax_rates[, eligible_canadian_dividends_rate := as.numeric(gsub(pattern = "%", x = eligible_canadian_dividends_rate, replacement = "", fixed = TRUE)) / 100]
tax_rates[, non_eligible_canadian_dividends_rate := as.numeric(gsub(pattern = "%", x = non_eligible_canadian_dividends_rate, replacement = "", fixed = TRUE)) / 100]



# tax_rates = Combined Federal & Alberta Tax Brackets and Tax Rates
# Marginal tax rate for dividends is a % of actual dividends received (not grossed-up taxable amount).
# Marginal tax rate for capital gains is a % of total capital gains (not taxable capital gains).
# Gross-up rate for eligible dividends is 38%, and for non-eligible dividends is 15%.
# calculate tax liability for employment income
income_tax <- function(tax_rates, employment_income) {
  ptax_rates <- tax_rates[, .(lower_limit, upper_limit, income_tax_rate, eligible_canadian_dividends_rate, non_eligible_canadian_dividends_rate)]
  # ptax_rates[upper_limit < employment_income]
  ptax_rates[upper_limit < employment_income, 
             tax_by_bracket := (upper_limit - lower_limit) * income_tax_rate]
  # ptax_rates[upper_limit >= employment_income & employment_income - lower_limit > 0]
  ptax_rates[upper_limit >= employment_income & employment_income - lower_limit > 0, 
             tax_by_bracket := (employment_income - lower_limit) * income_tax_rate]
  
  ptax_rates[is.na(tax_by_bracket), tax_by_bracket := 0]
  return(ptax_rates)
}

# calculate tax liability for dividend income
dividend_tax <- function(tax_rates, employment_income, dividend_income, dividend_type_split) {
  ptax_rates <- income_tax(tax_rates, employment_income)
  # find lower and upper limits for dividend income
  # upper limit
  # ptax_rates[lower_limit <= (employment_income + dividend_income) & upper_limit > (employment_income + dividend_income)]
  ptax_rates[lower_limit <= (employment_income + dividend_income) & upper_limit > (employment_income + dividend_income),
             div_tax_by_bracket := (employment_income + dividend_income - lower_limit) * 
               eligible_canadian_dividends_rate * dividend_type_split +
               (employment_income + dividend_income - lower_limit) * 
               non_eligible_canadian_dividends_rate * (1 - dividend_type_split)]
  
  # Find the inbetween dividend brackets
  # ptax_rates[lower_limit >= employment_income & upper_limit < (employment_income + dividend_income)]
  ptax_rates[lower_limit >= employment_income & upper_limit < (employment_income + dividend_income), 
             div_tax_by_bracket := (upper_limit - lower_limit) * eligible_canadian_dividends_rate * dividend_type_split +
               (upper_limit - lower_limit) * non_eligible_canadian_dividends_rate * (1 - dividend_type_split)]
  
  # Find the lower limit dividend brackets
  # ptax_rates[lower_limit <= employment_income & upper_limit > employment_income & upper_limit < (employment_income + dividend_income)]
  ptax_rates[lower_limit <= employment_income & upper_limit > employment_income & upper_limit < (employment_income + dividend_income), 
             div_tax_by_bracket := (upper_limit - employment_income) * 
               eligible_canadian_dividends_rate * dividend_type_split +
               (upper_limit - employment_income) * 
               non_eligible_canadian_dividends_rate * (1 - dividend_type_split)]
  ptax_rates[is.na(div_tax_by_bracket), div_tax_by_bracket := 0]
  return(ptax_rates)
}


calculate_tax_liability_sm <- function(employment_income = 100000,
                                       mortgage_balance = 200000,
                                       home_value = 1000000, 
                                       dividend_yeild = 0.2, 
                                       dividend_type_split = 0.75, 
                                       loc_interest_rate = 0.06) {

  # calculate amount you could borrow
  loc_balance <- 0.65 * (home_value - mortgage_balance)
  
  # calculate interest expense
  interest_expense <- loc_interest_rate * loc_balance
  
  # calculate dividend income
  dividend_income <- loc_balance * dividend_yeild
  
  # calculate tax liability for employment income
  ptax_rates <- income_tax(tax_rates, employment_income)
  ptax_rates[, div_tax_by_bracket := 0]
  # if (dividend_income > 0) {
  #   ptax_rates <- dividend_tax(tax_rates, employment_income, dividend_income, dividend_type_split)
  # } else {
  #   ptax_rates[, div_tax_by_bracket := 0]
  # }
  
  # filter to tax brackets that are relevant
  tax <- ptax_rates[tax_by_bracket > 0]
  original_income_tax <- tax[, .(sum(tax_by_bracket))]
  original_income <- employment_income - original_income_tax
  
  # calculate tax savings from investment interest expense
  # assume that investment interest expense is deductible from employment income first
  if (interest_expense > 0 & employment_income > interest_expense) {
    income_after_expenses <- employment_income - interest_expense
    new_income_tax <- dividend_tax(tax_rates, income_after_expenses, dividend_income, dividend_type_split)
    sm_income_tax <- new_income_tax[, .(sum(tax_by_bracket))]
    sm_div_tax <- new_income_tax[, .(sum(div_tax_by_bracket))]
    sm_income <- employment_income + dividend_income - sm_income_tax - sm_div_tax
    tax_savings_from_interest <- sm_income - original_income
  } else if (interest_expense > 0 & employment_income < interest_expense) {
    income_after_expenses <- 0
    div_after_expenses <- dividend_income - (interest_expense - employment_income)
    new_income_tax <- dividend_tax(tax_rates, income_after_expenses, div_after_expenses, dividend_type_split)
    sm_income_tax <- new_income_tax[, .(sum(tax_by_bracket))]
    sm_div_tax <- new_income_tax[, .(sum(div_tax_by_bracket))]
    sm_income <- employment_income + dividend_income - sm_income_tax - sm_div_tax
    tax_savings_from_interest <- sm_income - original_income
  } else {
    tax_savings_from_interest <- 0
    sm_income_tax <- original_income_tax
    sm_div_tax <- 0
    sm_income <- original_income
  }
  
  # calculate total tax liability and other factors
  totals <- data.table(
    employment_income = employment_income, 
    mortgage_balance = mortgage_balance,
    home_value = home_value,
    dividend_yeild = dividend_yeild,
    dividend_type_split = dividend_type_split,
    loc_balance = loc_balance,
    loc_interest_rate = loc_interest_rate,
    interest_expense = -interest_expense,
    original_net_income = original_income[, V1],
    original_income_tax = -original_income_tax[, V1], 
    sm_net_income = sm_income[, V1],
    dividend_income = dividend_income, 
    sm_income_tax = -sm_income_tax[, V1],
    sm_div_tax = -sm_div_tax[, V1],
    tax_savings_from_interest = tax_savings_from_interest[, V1]
  )

  totals[, sm_effective_value := sm_net_income - original_net_income]
  
  return(totals)
}
