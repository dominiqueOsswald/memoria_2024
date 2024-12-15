# External imports
from paretoset import paretoset
import plotly.graph_objects as go
from scipy.spatial import ConvexHull
from scipy.interpolate import interp1d
import numpy as np
import sys

# Sys instructions
sys.path.append("..")

# Internal imports
from common import config, helpers

# sys.argv[1] -> dea_te, dea_te_grd, dea_te_region

years = config.YEARS_RANGE
orientations = ['CRS', 'NIRS', 'VRS']
te_models = ['input', 'output', 'directional']

for year in years:
  for orientation in orientations:
    for model in te_models:
      results_df = helpers.dataframe_read_csv_file(path=f'../../../results/{sys.argv[1]}/{year}_{orientation}.csv')
      selected_columns = ['hospital_discharge_normalized', f'te_{model}']
      column_filtered_df = helpers.dataframe_filter_columns(data_frame=results_df,
                                                            columns=selected_columns)

      mask = paretoset(column_filtered_df, sense=["max", "max"])
      paretoset_hospitals = column_filtered_df[mask]

      # Scatter plot for dominated hospitals
      trace_dominated = go.Scatter(
          x=column_filtered_df['hospital_discharge_normalized'],
          y=column_filtered_df[f'te_{model}'],
          mode='markers',
          marker=dict(color='blue'),
          name='Hospitales dominados'
      )

      # Scatter plot for non-dominated hospitals
      trace_nondominated = go.Scatter(
          x=paretoset_hospitals['hospital_discharge_normalized'],
          y=paretoset_hospitals[f'te_{model}'],
          mode='markers',
          marker=dict(color='red'),
          name='Hospitales no dominados'
      )

      # Sort the red dots by the 'hospital_discharge_normalized' coordinate
      sorted_nondominated = paretoset_hospitals.sort_values(by='hospital_discharge_normalized')

      # Create a trace for the Pareto front line
      pareto_front_line = go.Scatter(
          x=sorted_nondominated['hospital_discharge_normalized'],
          y=sorted_nondominated[f'te_{model}'],
          mode='lines',
          name='Frontera',
          line=dict(color='red'),
                              
      )

      # Layout
      layout = go.Layout(
          title=f'Frontera de Pareto - {year} {orientation} {model.title()}',
          xaxis=dict(title='Egreso Hospitalario'),
          yaxis=dict(title='Eficiencia')
      )

      # Create figure
      fig = go.Figure(data=[trace_dominated, trace_nondominated, pareto_front_line], layout=layout)

      # Save the figure as PNG
      fig.write_image(f'../../../results/images/{sys.argv[1]}/pareto_front_{year}_{orientation}_{model}.png')
