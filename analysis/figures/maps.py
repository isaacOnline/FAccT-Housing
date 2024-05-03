import os
import math

import matplotlib
import geopandas as gpd
import matplotlib.pyplot as plt
from shapely.ops import unary_union

font = {'family' : 'Times New Roman',
        'weight' : 'bold',
        'size'   : 22}

matplotlib.rc('font', **font)
matplotlib.rc('legend', handletextpad=-0.5)

class DataReader():
    def __init__(self):
        self._read()
        self._clean()
        self._aggregate()

    def _read(self):
        self.sales = pd.read_csv(os.path.join('data', 'analysis', 'complete_transfer_info_20240111.csv'),
                                 low_memory=False)

        # This file will need to be downloaded
        self._npa_shapes = gpd.read_file(os.path.join('data', 'neighborhood', 'QOL_NPA_2020_shapefile'))

        # This file will need to be downloaded
        self.npas = pd.read_csv(os.path.join('data', 'neighborhood', 'Data_Download_20231016.csv'))

    def _clean(self):
        # convert to right types
        self.sales['npa_id'] = self.sales['npa_id'].astype(int)

        # Clean up column names
        self._npa_shapes.columns = [col.lower() for col in self._npa_shapes.columns]
        self.npas.columns = [col.lower() for col in self.npas.columns]

        # Filter to 2020
        self.npas = self.npas[self.npas['data_year'] == 2020]

        raw = self.npas[~self.npas['raw'].isna()]
        raw = raw.sort_values(['npa','data_year'])
        raw = raw.drop_duplicates(
            subset=['variable_id','raw_data_name','npa'],
            keep='last'
        ).drop(columns=['data_year','variable_id'])

        # Pivot raw variables to wide format
        raw = raw.pivot_table(
            index=['npa'],
            columns=['raw_data_name'],
            values='raw'
        ).reset_index()

        # Filter to most recently available data for each column
        normalized = self.npas[~self.npas['normalized'].isna()]
        normalized = normalized.sort_values(['npa', 'data_year'])
        normalized = normalized.drop_duplicates(
            subset=['variable_id', 'normalized_data_name', 'npa'],
            keep='last'
        ).drop(columns=['data_year', 'variable_id'])

        normalized = normalized.pivot_table(
            index=['npa'],
            columns=['normalized_data_name'],
            values='normalized'
        ).reset_index()

        self.npas = pd.merge(raw, normalized, on='npa')

        self.npas.columns = [col.lower() for col in self.npas.columns]

    def _aggregate(self):
        self.sales['white_seller'] = self.sales['race_desc_seller'] == 'White'
        self.sales['black_seller'] = self.sales['race_desc_seller'] == 'Black/African American'
        self.sales['white_buyer'] = self.sales['race_desc_buyer'] == 'White'
        self.sales['black_buyer'] = self.sales['race_desc_buyer'] == 'Black/African American'


        aggregated = self.sales.groupby('npa_id').agg({
            'sfr_buyer':'sum',
            'sfr_seller':'sum',
            'ibuyer_buyer':'sum',
            'ibuyer_seller': 'sum',
            'white_seller':'sum',
            'black_seller':'sum',
            'white_buyer':'sum',
            'black_buyer':'sum',
            'deed_file_name':'count'
        }).rename(columns={'deed_file_name':'sale_count'})

        aggregated_columns = aggregated.columns
        for col in aggregated_columns:
            if col != 'sale_count':
                aggregated[f'percent_{col}'] = aggregated[col] / aggregated['sale_count'] * 100

        self.npas = aggregated.merge(self.npas, left_index=True, right_on='npa')


        self.npas = self._npa_shapes.merge(self.npas, left_on='npa2020_no', right_on='npa', how='left')

        for col in aggregated_columns:
            self.npas[col] = self.npas[col].fillna(0)
            if col != 'sale_count':
                self.npas[f'percent_{col}'] = self.npas[f'percent_{col}'].fillna(0)

        self.npas = self.npas.drop(columns=['npa2020_no','npa_id'])



    def read_sales(self):
        return self.sales

    def read_npas(self):
        return self.npas


import numpy as np
import pandas as pd
import math

def create_even_bins(data, num_bins=4):
    min_val = data.min()
    max_val = data.max()

    # Calculate the raw bin width
    raw_bin_width = (max_val - min_val) / num_bins

    # Round the bin width to a convenient whole number
    rounded_bin_width = round(raw_bin_width / 10) * 10 if raw_bin_width > 10 else round(raw_bin_width)
    if rounded_bin_width == 0:
        rounded_bin_width = 1

    # Round the min and max values to the nearest whole number
    min_val = math.floor(min_val / rounded_bin_width) * rounded_bin_width
    max_val = math.ceil(max_val / rounded_bin_width) * rounded_bin_width

    # Create bin edges
    bins = np.arange(min_val, max_val + rounded_bin_width, rounded_bin_width)

    custom_bins = {
        'sale_count': [0, 200, 400, 600, 800, 1000],
        'black_population': [0, 25, 50, 75, 100],
        'percent_ibuyer_buyer': [0, 10, 20, 30, 40, 50],
        'percent_sfr_buyer': [0, 10, 20, 30, 40, 50],
        'percent_black_seller': np.arange(0, 100, 5)
    }
    if data.name in custom_bins:
        bins = custom_bins[data.name]

    # Ensure the last bin covers the max value
    if bins[-1] < max_val:
        bins = np.append(bins, max_val)

    # Bin the data
    binned_data = pd.cut(data, bins, include_lowest=True, right=False)


    return binned_data


if __name__ == '__main__':
    reader = DataReader()
    sales = reader.read_sales()
    npas = reader.read_npas().copy()


    for col in ['black_population', 'sale_count','percent_ibuyer_buyer', 'percent_black_seller', 'percent_sfr_buyer']:
        if npas[col].dtype == 'float64':

            # Create bins
            npas[col] = create_even_bins(npas[col], num_bins=4)

            # Get bins to sort through
            categories = npas[col].dropna().unique()
            categories = sorted(categories, key=lambda x: x.left)
            n_categories = len(categories)

            # Create a colormap
            colormap = plt.cm.viridis(np.linspace(0, 1, n_categories))
            color_mapping = {str(category): tuple(color) for category, color in zip(categories, colormap.tolist())}

            # Create the plot
            plt.clf()
            fig, ax = plt.subplots(figsize=(6, 6))
            npas[npas[col].isna()].plot(ax=ax, color='white', legend=False)

            legend_handles = []
            groups = []
            for interval in categories:
                col_value = str(interval)
                color = color_mapping[col_value]
                group = npas[npas[col] == interval]
                group.plot(ax=ax, color=color, edgecolor=color, label=col_value, linewidth=0)
                legend_handles.append(plt.Line2D([0], [0], marker='o', color='w', label=col_value,
                                                 markersize=10, markerfacecolor=color))

            # Remove the black border
            ax.spines['top'].set_visible(False)
            ax.spines['right'].set_visible(False)
            ax.spines['bottom'].set_visible(False)
            ax.spines['left'].set_visible(False)

            # Remove the tick marks
            ax.set_xticks([])
            ax.set_yticks([])

            # Remove the tick labels
            ax.set_xticklabels([])
            ax.set_yticklabels([])

            # Move legend to the top left
            legend = ax.legend(handles=legend_handles, bbox_to_anchor=(0.175, 1))
            legend.get_frame().set_edgecolor('none')

            # Merge all geometries into a single geometry
            unified_geometry = unary_union(npas.geometry)

            # Create a new GeoDataFrame from the unified geometry
            unified_mkl = gpd.GeoDataFrame(geometry=[unified_geometry])

            # Overlay the unified geometry with a gray border
            unified_mkl.boundary.plot(ax=ax, color='black', linewidth=1)

            # Remove white space
            plt.tight_layout()

            # fig.show()
            plt.savefig(os.path.join('analysis', 'figures', 'maps', f'{col}.pdf'))

            # Close
            plt.close()

