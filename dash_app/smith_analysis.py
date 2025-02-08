import pandas as pd

# Load tax rates
tax_rates = pd.read_csv('2024_marginal_tax_rates.csv')
tax_rates.columns = tax_rates.columns.str.lower().str.replace(' ', '_')
tax_rates['income_tax_rate'] = tax_rates['income_tax_rate'].str.rstrip('%').astype('float') / 100
tax_rates['capital_gains_rate'] = tax_rates['capital_gains_rate'].str.rstrip('%').astype('float') / 100
tax_rates['eligible_canadian_dividends_rate'] = tax_rates['eligible_canadian_dividends_rate'].str.rstrip('%').astype('float') / 100
tax_rates['non_eligible_canadian_dividends_rate'] = tax_rates['non_eligible_canadian_dividends_rate'].str.rstrip('%').astype('float') / 100

def calculate_tax_liability_sm(employment_income, dividend_income, dividend_type_split, interest_expense):
    # Calculate tax liability for employment income
    ptax_rates = tax_rates[['lower_limit', 'upper_limit', 'income_tax_rate', 'eligible_canadian_dividends_rate', 'non_eligible_canadian_dividends_rate']].copy()
    
    ptax_rates.loc[ptax_rates['upper_limit'] < employment_income, 'tax_by_bracket'] = (ptax_rates['upper_limit'] - ptax_rates['lower_limit']) * ptax_rates['income_tax_rate']
    ptax_rates.loc[(ptax_rates['upper_limit'] >= employment_income) & (employment_income - ptax_rates['lower_limit'] > 0), 'tax_by_bracket'] = (employment_income - ptax_rates['lower_limit']) * ptax_rates['income_tax_rate']
    ptax_rates['tax_by_bracket'].fillna(0, inplace=True)
    
    if dividend_income > 0:
        ptax_rates.loc[(ptax_rates['lower_limit'] <= employment_income + dividend_income) & (ptax_rates['upper_limit'] > employment_income + dividend_income), 'div_tax_by_bracket'] = (employment_income + dividend_income - ptax_rates['lower_limit']) * ptax_rates['eligible_canadian_dividends_rate'] * dividend_type_split + (employment_income + dividend_income - ptax_rates['lower_limit']) * ptax_rates['non_eligible_canadian_dividends_rate'] * (1 - dividend_type_split)
        ptax_rates.loc[(ptax_rates['lower_limit'] >= employment_income) & (ptax_rates['upper_limit'] < employment_income + dividend_income), 'div_tax_by_bracket'] = (ptax_rates['upper_limit'] - ptax_rates['lower_limit']) * ptax_rates['eligible_canadian_dividends_rate'] * dividend_type_split + (ptax_rates['upper_limit'] - ptax_rates['lower_limit']) * ptax_rates['non_eligible_canadian_dividends_rate'] * (1 - dividend_type_split)
        ptax_rates.loc[(ptax_rates['lower_limit'] <= employment_income) & (ptax_rates['upper_limit'] > employment_income) & (ptax_rates['upper_limit'] < employment_income + dividend_income), 'div_tax_by_bracket'] = (ptax_rates['upper_limit'] - employment_income) * ptax_rates['eligible_canadian_dividends_rate'] * dividend_type_split + (ptax_rates['upper_limit'] - employment_income) * ptax_rates['non_eligible_canadian_dividends_rate'] * (1 - dividend_type_split)
        ptax_rates['div_tax_by_bracket'].fillna(0, inplace=True)
    else:
        ptax_rates['div_tax_by_bracket'] = 0
    
    # Calculate tax savings from investment interest expense
    if interest_expense > 0:
        ptax_rates.loc[(ptax_rates['tax_by_bracket'] > 0) & (ptax_rates['lower_limit'] < employment_income) & (ptax_rates['upper_limit'] > employment_income) & (employment_income - interest_expense < ptax_rates['lower_limit']), 'tax_savings_by_bracket'] = (employment_income - ptax_rates['lower_limit']) * ptax_rates['income_tax_rate']
        ptax_rates.loc[(ptax_rates['lower_limit'] < employment_income - interest_expense) & (ptax_rates['upper_limit'] > employment_income - interest_expense), 'tax_savings_by_bracket'] = (ptax_rates['upper_limit'] - (employment_income - interest_expense)) * ptax_rates['income_tax_rate']
        ptax_rates.loc[(ptax_rates['lower_limit'] > (employment_income - interest_expense)) & (ptax_rates['upper_limit'] < employment_income), 'tax_savings_by_bracket'] = (ptax_rates['upper_limit'] - ptax_rates['lower_limit']) * ptax_rates['income_tax_rate']
        ptax_rates['tax_savings_by_bracket'].fillna(0, inplace=True)
    else:
        ptax_rates['tax_savings_by_bracket'] = 0
    
    totals = ptax_rates[['tax_by_bracket', 'div_tax_by_bracket', 'tax_savings_by_bracket']].sum()
    totals['employment_income'] = employment_income
    totals['dividend_income'] = dividend_income
    totals['interest_expense'] = -interest_expense
    totals['income_tax'] = -totals['tax_by_bracket']
    totals['div_tax'] = -totals['div_tax_by_bracket']
    totals['tax_savings_from_interest'] = totals['tax_savings_by_bracket']
    totals['net_income'] = employment_income + dividend_income + totals['interest_expense'] + totals['income_tax'] + totals['div_tax'] + totals['tax_savings_from_interest']
    totals['sm_effective_value'] = totals['dividend_income'] + totals['interest_expense'] + totals['div_tax'] + totals['tax_savings_from_interest']
    
    return totals
