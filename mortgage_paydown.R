library(data.table)
library(capitalR)

# Input variables
value_of_home <- 1300000
mortage <- 700000
mortgage_interest <- 0.0612
mortgage_years_left <- 14
mortgage_periods <- mortgage_years_left * 12
line_of_credit_interest <- 0.072
investment_yeild <- 0.11
income <- 250000

prepayment_schedule <- data.table(Year = c(2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033),
                                  Prepayment = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

# Calculate original mortgage schedule
mortgage_org <- data.table(schedule(r = mortgage_interest/12, n = mortgage_periods, pv = mortage, fv = 0, end = TRUE))
mortgage_org 


# mortgage prepayment function
morgtage_prepayment_schedule <- function(org_mortgage, yearly_prepayment, mortgage_interest) {
  n_periods_left <- org_mortgage[Period == max(Period), Period]
  mortgage_schedule <- org_mortgage
  year <- 1
  
  scenarios <- org_mortgage[Period == max(Period), 
                            .(Year = 2024, 
                              Years = Period/12, 
                              Cumulative_Interest = Cum.Int, 
                              Cumulative_Principal = Cum.Prin)]
  
  
  while (trunc(n_periods_left / 12) > 0) {
    years_left <- trunc(n_periods_left / 12)
    new_balance <- mortgage_schedule[Period == year*12, Rem.Bal] - yearly_prepayment
    monthly_mortgage_payment <- mortgage_schedule[Period == year*12, Payment]
    
    print(paste0("new ballance: ", new_balance))
    new_periods <- annuity(type = 'nper', 
                           r = mortgage_interest/12, 
                           pv = new_balance, 
                           fv = 0, 
                           pmt = -monthly_mortgage_payment, 
                           end = TRUE)
    print(new_periods)
    recalc_mortgage <- data.table(schedule(r = mortgage_interest/12, 
                                           n = new_periods, 
                                           pv = new_balance, 
                                           fv = 0, 
                                           end = TRUE))
    recalc_mortgage[Period == 0, Payment := yearly_prepayment]
    recalc_mortgage[Period == 0, Principal := yearly_prepayment]
    
    recalc_mortgage[, Period := Period + year*12]
    mortgage_schedule <- rbind(mortgage_schedule[Period <= year*12], recalc_mortgage[Period >= year*12])
    
    # recalculate cumulative interest and principal
    mortgage_schedule[, Cum.Int := cumsum(Interest)]
    mortgage_schedule[, Cum.Prin := cumsum(Principal)]
    mortgage_schedule[, Cum.Pmt := cumsum(Payment)]
    
    scenarios <- rbind(scenarios, 
                       mortgage_schedule[Period == max(Period) - 1, 
                                         .(Year = year(Sys.Date()) + year, 
                                           Years = Period/12, 
                                           Cumulative_Interest = Cum.Int, 
                                           Cumulative_Principal = Cum.Prin)])
    print(scenarios)
    
    # set var for next loop
    year = year + 1
    n_periods_left <- mortgage_schedule[Period == max(Period), Period] - year*12
  }
  return(list(mortgage_schedule, scenarios))
}


mortgage_prepayments <- morgtage_prepayment_schedule(mortgage_org, yearly_prepayment, mortgage_interest)

sc <- mortgage_prepayments[[2]]
print(paste0("Intrest Savings: ", sc[Year == 2024, as.numeric(Cumulative_Interest)] - sc[Year == 2028, as.numeric(Cumulative_Interest)]))


# function to calculate the interest component of the mortgage payment and the principal component
mortgage_payment <- function(mortgage, interest_rate) {
  interest <- mortgage * interest_rate / 12
  principal <- mortgage - interest
  return(list(interest, principal))
}

