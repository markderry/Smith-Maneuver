import pandas as pd

def mortgage_schedule(r, n, pv, fv=0, end=True):
    schedule = []
    pmt = -pv * r / (1 - (1 + r) ** -n)
    for i in range(1, n + 1):
        interest = pv * r
        principal = pmt - interest
        pv -= principal
        schedule.append((i, pmt, interest, principal, pv))
    return pd.DataFrame(schedule, columns=['Period', 'Payment', 'Interest', 'Principal', 'Rem.Bal'])

def mortgage_prepayment_schedule(org_mortgage, yearly_prepayment, mortgage_interest):
    n_periods_left = org_mortgage.loc[org_mortgage['Period'] == org_mortgage['Period'].max(), 'Period'].values[0]
    mortgage_schedule = org_mortgage.copy()
    year = 1

    scenarios = org_mortgage.loc[org_mortgage['Period'] == org_mortgage['Period'].max(), 
                                 ['Period', 'Cum.Int', 'Cum.Prin']].copy()
    scenarios['Year'] = 2024
    scenarios['Years'] = scenarios['Period'] / 12
    scenarios.rename(columns={'Cum.Int': 'Cumulative_Interest', 'Cum.Prin': 'Cumulative_Principal'}, inplace=True)

    while n_periods_left // 12 > 0:
        years_left = n_periods_left // 12
        new_balance = mortgage_schedule.loc[mortgage_schedule['Period'] == year * 12, 'Rem.Bal'].values[0] - yearly_prepayment
        monthly_mortgage_payment = mortgage_schedule.loc[mortgage_schedule['Period'] == year * 12, 'Payment'].values[0]

        new_periods = -np.log(1 - new_balance * r / monthly_mortgage_payment) / np.log(1 + r)
        recalc_mortgage = mortgage_schedule(r, new_periods, new_balance, fv=0, end=True)
        recalc_mortgage.loc[0, 'Payment'] = yearly_prepayment
        recalc_mortgage.loc[0, 'Principal'] = yearly_prepayment

        recalc_mortgage['Period'] += year * 12
        mortgage_schedule = pd.concat([mortgage_schedule[mortgage_schedule['Period'] <= year * 12], recalc_mortgage[recalc_mortgage['Period'] >= year * 12]])

        mortgage_schedule['Cum.Int'] = mortgage_schedule['Interest'].cumsum()
        mortgage_schedule['Cum.Prin'] = mortgage_schedule['Principal'].cumsum()
        mortgage_schedule['Cum.Pmt'] = mortgage_schedule['Payment'].cumsum()

        scenario = mortgage_schedule.loc[mortgage_schedule['Period'] == mortgage_schedule['Period'].max() - 1, 
                                         ['Period', 'Cum.Int', 'Cum.Prin']].copy()
        scenario['Year'] = 2024 + year
        scenario['Years'] = scenario['Period'] / 12
        scenario.rename(columns={'Cum.Int': 'Cumulative_Interest', 'Cum.Prin': 'Cumulative_Principal'}, inplace=True)
        scenarios = pd.concat([scenarios, scenario])

        year += 1
        n_periods_left = mortgage_schedule.loc[mortgage_schedule['Period'] == mortgage_schedule['Period'].max(), 'Period'].values[0] - year * 12

    return mortgage_schedule, scenarios

def mortgage_payment(mortgage, interest_rate):
    interest = mortgage * interest_rate / 12
    principal = mortgage - interest
    return interest, principal
