import dash
from dash import dcc, html
from dash.dependencies import Input, Output
import pandas as pd
import dash_table

# Load data
tax_rates = pd.read_csv('2024_marginal_tax_rates.csv')

# Initialize the Dash app
app = dash.Dash(__name__)

# Define the layout of the app
app.layout = html.Div([
    dcc.Tabs([
        dcc.Tab(label='Smith Maneuver Analysis', children=[
            html.Div([
                html.Label('Value of Home:'),
                dcc.Input(id='value_of_home', type='number', value=1000000, step=100000),
                html.Label('Mortgage:'),
                dcc.Input(id='mortage', type='number', value=500000, step=50000),
                html.Label('Line of Credit Interest:'),
                dcc.Input(id='line_of_credit_interest', type='number', value=0.07, step=0.005),
                html.Label('Investment Yield:'),
                dcc.Input(id='investment_yeild', type='number', value=0.09, step=0.005),
                html.Label('Employment Income:'),
                dcc.Input(id='income', type='number', value=100000, step=10000),
                html.Button('Run', id='run', n_clicks=0)
            ]),
            dash_table.DataTable(id='results_table')
        ]),
        dcc.Tab(label='Solution Space', children=[
            html.Div([
                html.Label('Value of Home:'),
                dcc.Input(id='value_of_home', type='number', value=1000000, step=100000),
                html.Label('Mortgage:'),
                dcc.Input(id='mortage', type='number', value=500000, step=50000),
                html.Label('Line of Credit Interest:'),
                dcc.Input(id='line_of_credit_interest', type='number', value=0.07, step=0.005),
                html.Label('Investment Yield:'),
                dcc.Input(id='investment_yeild', type='number', value=0.09, step=0.005),
                html.Label('Employment Income:'),
                dcc.Input(id='income', type='number', value=100000, step=10000),
                html.Button('Run', id='run', n_clicks=0)
            ]),
            dash_table.DataTable(id='results_table'),
            dcc.Graph(id='solution_space')
        ])
    ])
])

# Define the callback to update the results table
@app.callback(
    Output('results_table', 'data'),
    Input('run', 'n_clicks'),
    [Input('value_of_home', 'value'),
     Input('mortage', 'value'),
     Input('line_of_credit_interest', 'value'),
     Input('investment_yeild', 'value'),
     Input('income', 'value')]
)
def update_results_table(n_clicks, value_of_home, mortage, line_of_credit_interest, investment_yeild, income):
    if n_clicks > 0:
        loc_ballance = 0.65 * (value_of_home - mortage)
        interest_expense = line_of_credit_interest * loc_ballance
        dividend_income = loc_ballance * investment_yeild

        scenarios = calculate_tax_liability_sm(income, 0, 0, 0.5)
        scenarios = scenarios.append(calculate_tax_liability_sm(income, dividend_income, 0.5, interest_expense))

        scenarios['employment_income'] = scenarios['employment_income'].apply(lambda x: f"${x:,.2f}")
        scenarios['dividend_income'] = scenarios['dividend_income'].apply(lambda x: f"${x:,.2f}")
        scenarios['interest_expense'] = scenarios['interest_expense'].apply(lambda x: f"${x:,.2f}")
        scenarios['income_tax'] = scenarios['income_tax'].apply(lambda x: f"${x:,.2f}")
        scenarios['div_tax'] = scenarios['div_tax'].apply(lambda x: f"${x:,.2f}")
        scenarios['tax_savings_from_interest'] = scenarios['tax_savings_from_interest'].apply(lambda x: f"${x:,.2f}")
        scenarios['net_income'] = scenarios['net_income'].apply(lambda x: f"${x:,.2f}")
        scenarios['sm_effective_value'] = scenarios['sm_effective_value'].apply(lambda x: f"${x:,.2f}")

        scenarios_out = scenarios.transpose().reset_index()
        scenarios_out.columns = ['Variable', 'No Maneuver', 'Smith Maneuver']
        return scenarios_out.to_dict('records')
    return []

# Define the callback to update the solution space graph
@app.callback(
    Output('solution_space', 'figure'),
    Input('run', 'n_clicks'),
    [Input('value_of_home', 'value'),
     Input('mortage', 'value'),
     Input('line_of_credit_interest', 'value'),
     Input('investment_yeild', 'value'),
     Input('income', 'value')]
)
def update_solution_space(n_clicks, value_of_home, mortage, line_of_credit_interest, investment_yeild, income):
    if n_clicks > 0:
        loc_ballance = 0.65 * (value_of_home - mortage)
        interest_expense = line_of_credit_interest * loc_ballance
        dividend_income = loc_ballance * investment_yeild

        scenarios = calculate_tax_liability_sm(income, 0, 0, 0.5)
        scenarios = scenarios.append(calculate_tax_liability_sm(income, dividend_income, 0.5, interest_expense))

        fig = {
            'data': [
                {'x': scenarios['loc_balance'], 'y': scenarios['sm_effective_value'], 'type': 'line', 'name': 'Smith Maneuver'}
            ],
            'layout': {
                'title': 'Solution Space'
            }
        }
        return fig
    return {'data': [], 'layout': {'title': 'Solution Space'}}

# Run the app
if __name__ == '__main__':
    app.run_server(debug=True)
