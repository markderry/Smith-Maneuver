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

# function to calculate tax liability based on employment income and dividend income with smith maneuver
calculate_tax_liability_sm <- function(employment_income, dividend_income, dividend_type_split, interest_expense) {
  # calculate tax liability for employment income
  ptax_rates <- tax_rates[, .(lower_limit, upper_limit, income_tax_rate, eligible_canadian_dividends_rate, non_eligible_canadian_dividends_rate)]
  
  
  ptax_rates[upper_limit < employment_income, 
             tax_by_bracket := (upper_limit - lower_limit) * income_tax_rate]
  ptax_rates[upper_limit >= employment_income & employment_income - lower_limit > 0, 
             tax_by_bracket := (employment_income - lower_limit) * income_tax_rate]
  ptax_rates[is.na(tax_by_bracket), tax_by_bracket := 0]
  if (dividend_income > 0) {
    ptax_rates[lower_limit <= employment_income + dividend_income & upper_limit > employment_income + dividend_income,
               div_tax_by_bracket := (employment_income + dividend_income - lower_limit) * 
                 eligible_canadian_dividends_rate * dividend_type_split +
                 (employment_income + dividend_income - lower_limit) * 
                 non_eligible_canadian_dividends_rate * (1 - dividend_type_split)]
    
    ptax_rates[lower_limit >= employment_income & upper_limit < employment_income + dividend_income, 
               div_tax_by_bracket := (upper_limit - lower_limit) * eligible_canadian_dividends_rate * dividend_type_split +
                 (upper_limit - lower_limit) * non_eligible_canadian_dividends_rate * (1 - dividend_type_split)]
    
    ptax_rates[lower_limit <= employment_income & upper_limit > employment_income & upper_limit < employment_income + dividend_income, 
               div_tax_by_bracket := (upper_limit - employment_income) * 
                 eligible_canadian_dividends_rate * dividend_type_split +
                 (upper_limit - employment_income) * 
                 non_eligible_canadian_dividends_rate * (1 - dividend_type_split)]
    ptax_rates[is.na(div_tax_by_bracket), div_tax_by_bracket := 0]
  } else {
    ptax_rates[, div_tax_by_bracket := 0]
  }
  
  # calculate tax savings from investment interest expense
  # assume that investment interest expense is deductible from employment income first
  if (interest_expense > 0) {
    ptax_rates[tax_by_bracket > 0 & 
                 lower_limit < employment_income & 
                 upper_limit > employment_income & 
                 employment_income - interest_expense < lower_limit, 
               tax_savings_by_bracket := (employment_income - lower_limit) * income_tax_rate] 
    ptax_rates[lower_limit < employment_income - interest_expense & 
                 upper_limit > employment_income - interest_expense,
               tax_savings_by_bracket := (upper_limit - (employment_income - interest_expense)) * income_tax_rate]
    ptax_rates[lower_limit > (employment_income - interest_expense) &
                 upper_limit < employment_income,
               tax_savings_by_bracket := (upper_limit - lower_limit) * income_tax_rate]
    ptax_rates[is.na(tax_savings_by_bracket), tax_savings_by_bracket := 0]
  } else {
    ptax_rates[, tax_savings_by_bracket := 0]
  }
  
  
  totals <- ptax_rates[, .(employment_income = employment_income, 
                           dividend_income = dividend_income, 
                           # gross_income = employment_income + dividend_income, 
                           interest_expense = -interest_expense,
                           income_tax = -sum(tax_by_bracket), 
                           div_tax = -sum(div_tax_by_bracket), 
                           tax_savings_from_interest = sum(tax_savings_by_bracket))
  ]
  totals[, net_income := employment_income + dividend_income + interest_expense + income_tax + div_tax + tax_savings_from_interest]
  totals[, sm_effective_value := dividend_income + interest_expense + div_tax + tax_savings_from_interest]
  # totals[tax_savings_from_interest > abs(income_tax + div_tax), tax_savings_from_interest := abs(income_tax + div_tax)]
  return(totals)
}

