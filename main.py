import plotly.express as px
import pandas as pd
from dash import Dash, dcc, html, Input, Output

df = pd.read_csv("pitching.csv")
df.sort_values(by="xwoba", ascending=False, inplace=True)

good_percentiles = ["groundballs_percent", "whiff_percent", "k_percent"]
bad_percentiles = ["bb_percent", "popups_percent", "linedrives_percent", "avg_hyper_speed"]
labels = good_percentiles+bad_percentiles
for p in good_percentiles:
    df[f"{p}_p"] = (df[p].rank(pct=True)*100).round(0).astype(int)
for p in bad_percentiles:
    df[f"{p}_p"] = (df[p].rank(pct=True, ascending=False)*100).round(0).astype(int)

df['percents'] = df[["groundballs_percent_p", "whiff_percent_p", "k_percent_p", "bb_percent_p",
                     "popups_percent_p", "linedrives_percent_p", "avg_hyper_speed_p"]].values.tolist()
df['labels'] = [labels]*len(df)

app = Dash(__name__)

app.layout = html.Div([
    html.H1("Pitching Analytics Dashboard"),

    html.Div([
        dcc.Graph(
            id='bar-chart',
            figure=px.bar(df, x="xwoba", y="last_name, first_name", title="Player by xwOBA",
                          orientation="h", labels={"xwoba": "Expected WOBA", "last_name, first_name": "Pitcher Name"},
                          color="xwoba", color_continuous_scale="amp").update_layout(plot_bgcolor="rgba(0,0,0,0)")
        ),
    ], style={'width': '50%', 'display': 'inline-block'}),

    html.Div([
        dcc.Graph(id='radar-chart'),
    ], style={'width': '50%', 'display': 'inline-block'})
])

@app.callback(
    Output('radar-chart', 'figure'),
    Input('bar-chart', 'clickData')
)
def update(clickData):
    player_name = "Milner, Hoby"
    if clickData:
        player_name = clickData['points'][0]['y']
    player_df = df[df["last_name, first_name"] == player_name].iloc[0]
    radar_df = pd.DataFrame({
        "r": [player_df[f"{l}_p"] for l in labels],
        "theta": labels
    })

    fig = px.line_polar(radar_df, r="r", theta="theta", line_close=True)
    fig.update_layout(
        polar=dict(
            radialaxis=dict(
                visible=True, range=[0, 100])),
                showlegend=False,
                title=f"Stats for {player_name}")
    return fig


if __name__ == '__main__':
    app.run(debug=True)
