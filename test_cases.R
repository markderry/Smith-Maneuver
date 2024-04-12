source("smith_analysis.R")
# test cases
employment_income = c(50000, 100000, 150000, 200000, 250000, 300000, 350000)
home_value = c(500000, 1000000, 1500000, 2000000, 2500000, 3000000)
mortgage_balance = c(100000, 200000, 300000, 400000, 500000, 1000000, 1500000, 2000000)
loc_interest_rate = c(0.03, 0.04, 0.05, 0.06, 0.07, 0.08)
dividend_yeild = c(0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10, 0.11, 0.12)
dividend_type_split = c(0, 0.25, 0.5, 0.75, 1)
test_cases <- data.table(expand.grid(employment_income, home_value, loc_interest_rate, dividend_yeild, mortgage_balance, dividend_type_split))
setnames(test_cases, c("employment_income", "home_value", "loc_interest_rate", "dividend_yeild", "mortgage_balance", "dividend_type_split"))
rm(employment_income, home_value, loc_interest_rate, dividend_yeild, mortgage_balance, dividend_type_split)


# check that the test cases are correct
test_cases <- test_cases[home_value > mortgage_balance, ]
test_cases[, loc_balance := 0.65 * (home_value - mortgage_balance)]
test_cases[, dividend_income := loc_balance * dividend_yeild]
test_cases[, interest_expense := loc_interest_rate * loc_balance]
test_cases

test <- calculate_tax_liability_sm(employment_income = test_cases$employment_income[1], 
                                   home_value = test_cases$home_value[1],
                                   mortgage_balance = test_cases$mortgage_balance[1],
                                   dividend_yeild = test_cases$dividend_yeild[1],
                                   dividend_type_split = test_cases$dividend_type_split[1],
                                   loc_interest_rate = test_cases$loc_interest_rate[1])

test
# calculate tax liability for each test case
library(tictoc)
tic()
output <- data.table()
for (i in 1:nrow(test_cases)) {
# for (i in 1:1000) {
  test_case_i = calculate_tax_liability_sm(employment_income = test_cases$employment_income[i], 
                                           home_value = test_cases$home_value[i],
                                           mortgage_balance = test_cases$mortgage_balance[i],
                                           dividend_yeild = test_cases$dividend_yeild[i],
                                           dividend_type_split = test_cases$dividend_type_split[i],
                                           loc_interest_rate = test_cases$loc_interest_rate[i]
  )
  if (i == 1) {
    output <- test_case_i
  } else {
    output <- rbind(output, test_case_i, use.names = FALSE)
  }
  print(i)
}
toc()

# write output to rds
saveRDS(output, "output.rds")

out <- cbind(test_cases, output)
out[, which(duplicated(names(out))) := NULL]
# plot results 
library(ggplot2)
plot <- out[employment_income == 350000 & 
                 home_value == 1000000 &
                 dividend_yeild == 0.11 & 
                 dividend_type_split == 0.25, ]

plot
# plot 1 group by loc_interest_rate

ggplot(plot, aes(x = loc_balance, y = sm_effective_value, group = loc_interest_rate)) + 
  geom_point(aes(color=loc_interest_rate)) +
  geom_line(aes(color=loc_interest_rate)) 
