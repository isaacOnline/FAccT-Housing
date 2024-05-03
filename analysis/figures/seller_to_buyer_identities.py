import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib

from analysis.create_data_for_analysis import ResaleDataLoader


# Use ggplot style
# plt.style.use('ggplot')

font = {'family' : 'Times New Roman',
        'weight' : 'bold',
        'size'   : 12}

axes = {'titlesize' : 14,
        'labelweight' : 'bold',
        'labelsize' : 14}


matplotlib.rc('font', **font)
matplotlib.rc('axes', **axes)
fig_size = plt.rcParams["figure.figsize"]
fig_size[0] = 6 #fig_size[0] = 11.69
fig_size[1] = 4 #fig_size[1] = 8.27
plt.rcParams["figure.figsize"] = fig_size


def plot_bars(bar_data, save_path):
    bar_data = get_pcts_by_seller_type(bar_data)

    bar_data = bar_data[bar_data['bought_from'].isin(['White', 'Black/African American'])]

    plt.clf()


    #sort the bars into 'Black' 'White' 'Institutional' 'iBuyer' 'Other' order
    bar_data['bought_sort'] = bar_data['bought_from'].replace({
        'White': 0,
        'Black/African American': 1,
        'Unknown/Other/Multiple': 2,
        'Institutional': 3,
        'iBuyer': 4,
    })
    bar_data['sold_sort'] = bar_data['sold_to'].replace({
        'White': 0,
        'Black/African American': 1,
        'Unknown/Other/Multiple': 2,
        'Institutional': 3,
        'iBuyer': 4,
    })
    bar_data = bar_data.sort_values(['bought_sort', 'sold_sort'])

    # Use colorblind friendly colors
    sns.set_palette(sns.color_palette("colorblind"))
    bars = sns.barplot(data=bar_data, x='bought_from', y='pct', hue='sold_to')

    # Define some hatches
    hatches = ['-', 'O', '*', '\\',  '.', '/', '+', 'x', 'o', 'X']
    unique_colors = np.unique([patch.get_facecolor() for patch in bars.patches], axis=0)
    unique_colors = [tuple(color) for color in unique_colors]
    color_map = dict(zip(unique_colors, hatches[:len(unique_colors)]))

    # Loop over the bars
    for thisbar in bars.patches:
        # Set a different hatch for each bar
        thisbar.set_hatch(color_map[tuple(thisbar.get_facecolor())])

    plt.legend(title='End Buyer', loc='upper right')
    plt.xlabel('Original Seller Demographic(s)', fontdict={'size': 12, 'weight': 'bold'})
    plt.ylabel('Percent of Homes Sold by Group', fontdict={'size': 12, 'weight': 'bold'})

    plt.ylim(0, 100)

    # Set yticks to percent
    plt.yticks(np.arange(0, 101, 20), ['{}%'.format(x) for x in np.arange(0, 101, 20)])


    # Set xticks to angle
    # plt.xticks(rotation=12, ha='right')

    # Set legend title

    plt.tight_layout()



    plt.savefig(save_path, bbox_inches='tight')

    plt.close()



def get_pcts_by_seller_type(sales_data):
    sales_data['sold_to'] = np.where(
        sales_data['sold_to'] == 'Institutional',
        'Institutional',
        'Personal'
    )
    sales_data = sales_data.groupby(['bought_from', 'sold_to']).agg({'parcel_id': 'count'}).reset_index()
    sales_data.columns = ['bought_from', 'sold_to', 'count']

    totals = sales_data.groupby('bought_from').agg({'count': 'sum'}).reset_index().sort_values('count', ascending=False)
    totals.columns = ['bought_from', 'total']

    sales_data = sales_data.merge(totals, on='bought_from')

    sales_data['pct'] = sales_data['count'] / sales_data['total'] * 100

    return sales_data

def get_pcts_overall(sales_data):
    sales_data = sales_data.groupby(['sold_to']).agg({'parcel_id': 'count'}).reset_index()
    sales_data.columns = ['sold_to', 'count']

    totals = sales_data['count'].sum()

    sales_data['total'] = totals

    sales_data['pct'] = sales_data['count'] / sales_data['total'] * 100

    return sales_data


if __name__ == '__main__':
    loader = ResaleDataLoader()

    ib_resales = loader.load_resales()

    plot_bars(ib_resales, 'analysis/figures/seller_to_buyer_identities_ibuyer.pdf')

    direct_sales = loader.load_direct_sales()

    plot_bars(direct_sales, 'analysis/figures/seller_to_buyer_identities_direct.pdf')



    print('iBuyer resales by group')
    print(get_pcts_by_seller_type(ib_resales))

    print('\niBuyer resales overall')
    print(get_pcts_overall(ib_resales))

    print('\nDirect sales by group')
    print(get_pcts_by_seller_type(direct_sales))

    print('\nDirect sales overall')
    print(get_pcts_overall(direct_sales))
