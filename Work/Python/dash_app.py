import dash
import dash_table
import dash_core_components as dcc
import dash_html_components as html
import plotly.graph_objs as go
import datetime
import cx_Oracle as ora
import pandas as pd

db_address = 'system/1234@localhost:1521/xe'
connection = ora.connect(db_address)


def fetch_data(start_date, end_date):
    query = """
    SELECT DA.AGENT_NAME,DW.WEEK_DAY,DS.SHIFT_TYPE,DF.CONTACT_DATE,TOTAL_CONNECTED_IN_HRS,
    TOTAL_WAITING_IN_HRS, TOTAL_PAUSED_IN_HRS, TOTAL_DEASSIGN_IN_HRS,
    TOTAL_SUMMARY_IN_HRS, TOTAL_CONNECTED_IN_MINS, TOTAL_WAITING_IN_MINS,
    TOTAL_PAUSED_IN_MINS, TOTAL_DEASSIGN_IN_MINS, TOTAL_SUMMARY_IN_MINS
    FROM
    DIM_AGENT DA,
    DIM_SHIFTS DS,
    DIM_WEEKS DW,
    DAILY_NOBLE_DATA_FACT DF
    WHERE
    DF.AGENT_ID =DA.AGENT_ID
    AND DS.SHIFT_ID=DF.SHIFT_ID
    AND DW.WEEK_ID = DF.WEEK_ID
    AND TO_DATE(DF.CONTACT_DATE , 'MM/DD/YYYY') BETWEEN TO_DATE('""" + start_date + """', 'YYYY-MM-DD')
    AND TO_DATE('""" + end_date + """', 'YYYY-MM-DD')"""
    sql_data = pd.read_sql(query, con=connection)
    sql_data['CONTACT_DATE'] = pd.to_datetime(sql_data['CONTACT_DATE'])

    left_pane_data = sql_data[['AGENT_NAME', 'TOTAL_SUMMARY_IN_HRS', 'CONTACT_DATE']].groupby(
        by=['AGENT_NAME', 'CONTACT_DATE'], as_index=False)[['TOTAL_SUMMARY_IN_HRS']].sum()

    right_pane_date = sql_data[[
        'SHIFT_TYPE',
        'WEEK_DAY',
        'AGENT_NAME',
        'TOTAL_CONNECTED_IN_HRS',
        'TOTAL_WAITING_IN_HRS',
        'TOTAL_PAUSED_IN_HRS',
        'TOTAL_DEASSIGN_IN_HRS'
    ]].groupby(by=['SHIFT_TYPE', 'WEEK_DAY', 'AGENT_NAME'], as_index=False).sum()

    return (left_pane_data, right_pane_date)


external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']
app = dash.Dash(__name__, external_stylesheets=external_stylesheets)

app.layout = html.Div(children=[
    html.H4(children='Weekly performance report'),
    html.Div(children=[
        # Left pane
        html.Div(children=[
            dcc.DatePickerRange(
                id='date-picker',
                start_date=(datetime.datetime.today() - \
                            datetime.timedelta(365 / 12)),
                end_date=datetime.datetime.today()
            ),
            dcc.Graph(id='scatter-plot')
        ], style={
            'flex': '1 0 70%',
            'display': 'flex',
            'flex-direction': 'column'
        }),
        # Right pane
        html.Div()
    ], style={
        'display': 'flex',
        'flex-direction': 'row',
        'flex-wrap': 'nowrap'
    }),
])


@app.callback(
    dash.dependencies.Output('scatter-plot', 'figure'),
    [dash.dependencies.Input('date-picker', 'start_date'),
     dash.dependencies.Input('date-picker', 'end_date')]
)
def update_plot1(start_date, end_date):
    date1 = datetime.datetime.fromisoformat(start_date)
    date2 = datetime.datetime.fromisoformat(end_date)
    left_data, right_data = fetch_data(
        date1.strftime('%Y-%m-%d'),
        date2.strftime('%Y-%m-%d')
    )
    return {
        'data': [go.Scatter(
            x=left_data['CONTACT_DATE'],
            y=left_data['TOTAL_SUMMARY_IN_HRS'],
            mode='markers'
        )],
        'layout': go.Layout(title='')
    }


if __name__ == '__main__':
    app.run_server(debug=True)
