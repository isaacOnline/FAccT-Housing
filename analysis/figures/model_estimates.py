import os
from dataclasses import dataclass

import pandas as pd
import matplotlib
import matplotlib.pyplot as plt


########################################################################################################################
# Set style

# Use ggplot style
# plt.style.use('ggplot')

########################################################################################################################
@dataclass
class PosteriorPredictedValue:
    identity_group: str
    entity: str
    mean: float
    std: float
    lower: float
    upper: float


@dataclass
class PosteriorEstimate:
    identity_group: str
    entity: str
    mean: float
    std: float
    lower: float
    upper: float


########################################################################################################################
# Set plotting parameters
font = {'family' : 'Times New Roman',
        'weight' : 'bold',
        'size'   : 22}
axes = {'titlesize' : 22,
        'labelweight' : 'bold',
        'labelsize' : 22}


matplotlib.rc('axes', **axes)
matplotlib.rc('font', **font)
matplotlib.rc('legend', handletextpad=-0.5)

########################################################################################################################
# Define groups to be plotted

identity_groups = {
    'Black/African American': 'black',
    'White': 'white'
}

entities = {
    'iBuyer': 'ibuyer',
    'Non-iBuyer (Personal)': 'personal',
    # 'Non-iBuyer (Institutional)': 'institutional'
}

########################################################################################################################
# Plotting functions

def plot_contrasts(model, error_type):
    means = []
    for identity, identity_string in identity_groups.items():
        for entity, entity_string in entities.items():
            means.append(
                PosteriorEstimate(
                    identity_group=identity,
                    entity=entity,
                    mean=model.loc[f'{identity_string}_to_{entity_string}_point'],
                    std=model.loc[f'{identity_string}_to_{entity_string}_sd'],
                    lower=model.loc[f'{identity_string}_to_{entity_string}_point']-model.loc[f'{identity_string}_to_{entity_string}_lower'],
                    upper=model.loc[f'{identity_string}_to_{entity_string}_upper']-model.loc[f'{identity_string}_to_{entity_string}_point']
                )
            )

    means = pd.DataFrame(means).sort_values('identity_group',ascending=False)

    target_dict = {
        'sp': 'Sale Price',
        'log_sp': 'Log Sale Price',
        'profit': 'Markup'
    }

    target = target_dict[model.loc['target']]

    plt.clf()

    # Create a colorblind-friendly color palette
    colors = ['#F8766D','#00BFC4']
    shapes = ['o', 's']
    color_mapping = {identity: (color, shape) for identity, color, shape in
                     zip(means['entity'].unique(), colors, shapes)}

    # Get unique entities and identity groups
    unique_entities = means['entity'].unique()
    unique_groups = means['identity_group'].unique()

    # Offset factor for x-axis
    offset_factor = 0.4
    num_entities = len(unique_groups)

    # Mapping each entity to a number
    entity_to_num = {entity: i for i, entity in enumerate(unique_groups)}

    # Plotting
    plt.figure(figsize=(10, 6))
    for i, entity in enumerate(unique_entities):
        group_data = means[means['entity'] == entity]
        x_values = [entity_to_num[entity] + (i - len(unique_entities) / 2) * offset_factor + offset_factor/2 for entity in
                    group_data['identity_group']]
        if error_type == 'std':
            error = group_data['std']
        elif error_type == 'ci':
            error = group_data[['lower', 'upper']].T
        elif error_type == 'none':
            error = None
        else:
            raise ValueError('Invalid error type')
        plt.errorbar(x_values, group_data['mean'], yerr=error, fmt='o', color=color_mapping[entity][0],
                        marker=color_mapping[entity][1],
                     label=entity,elinewidth=4, capsize=9, capthick=4, markersize=15)

    # Set x-axis labels to entity names
    plt.xticks(range(num_entities), unique_groups)

    # To avoid duplicate labels in the legend
    handles, labels = plt.gca().get_legend_handles_labels()
    by_label = dict(zip(labels, handles))

    # Add legend title and place legend to the right of plot

    plt.legend(by_label.values(), by_label.keys(), title='Home Buyer Type', bbox_to_anchor=(1, 0.8))

    # Format y axis ticks in dollars with commas
    plt.gca().get_yaxis().set_major_formatter(
        plt.FuncFormatter(lambda x, loc: "${:,}".format(int(x))))

    # Add soft gray horizontal lines
    plt.grid(axis='y', color='gray', linestyle='dashed', alpha=0.5)

    plt.xlabel('\nRace of Home Seller(s)')
    plt.ylabel('Difference in ' + target + '\n')



    plt.tight_layout()


    plt.savefig(os.path.join('analysis','figures','model_estimates', 'contrasts_' + model['save_name']))


def plot_predicted_prices(model, prediction_type, error_type):

    means = []
    for identity, identity_string in identity_groups.items():
        for entity, entity_string in entities.items():
            means.append(
                PosteriorPredictedValue(
                    identity_group=identity,
                    entity=entity,
                    mean=model.loc[f'{prediction_type}_value_{identity_string}_and_{entity_string}_point'],
                    std=model.loc[f'{prediction_type}_value_{identity_string}_and_{entity_string}_sd'],
                    lower=model.loc[f'{prediction_type}_value_{identity_string}_and_{entity_string}_point']-model.loc[f'{prediction_type}_value_{identity_string}_and_{entity_string}_lower'],
                    upper=model.loc[f'{prediction_type}_value_{identity_string}_and_{entity_string}_upper']-model.loc[f'{prediction_type}_value_{identity_string}_and_{entity_string}_point']
                )
            )

    means = pd.DataFrame(means).sort_values('identity_group',ascending=False)

    target_dict = {
        'sp': 'Sale Price',
        'log_sp': 'Log Sale Price',
        'profit': 'Markup'
    }

    target = target_dict[model.loc['target']]

    plt.clf()

    # Create a colorblind-friendly color palette
    colors = ['#F8766D', '#00BFC4']
    shapes = ['o', 's']
    color_mapping = {identity: (color, shape) for identity, color, shape in
                     zip(means['entity'].unique(), colors, shapes)}

    # Get unique entities and identity groups
    unique_entities = means['entity'].unique()
    unique_groups = means['identity_group'].unique()

    # Offset factor for x-axis
    offset_factor = 0.4
    num_entities = len(unique_groups)

    # Mapping each entity to a number
    entity_to_num = {entity: i for i, entity in enumerate(unique_groups)}

    # Plotting
    plt.figure(figsize=(10, 6))
    for i, entity in enumerate(unique_entities):
        group_data = means[means['entity'] == entity]
        x_values = [entity_to_num[entity] + (i - len(unique_entities) / 2) * offset_factor + offset_factor/2 for entity in
                    group_data['identity_group']]
        if error_type == 'std':
            error = group_data['std']
        elif error_type == 'ci':
            error = group_data[['lower', 'upper']].T
        elif error_type == 'none':
            error = None
        else:
            raise ValueError('Invalid error type')
        plt.errorbar(x_values, group_data['mean'], yerr=error, fmt='o', color=color_mapping[entity][0],
                        marker=color_mapping[entity][1],
                     label=entity,elinewidth=4, capsize=9, capthick=4, markersize=15)

    # Set x-axis labels to entity names
    plt.xticks(range(num_entities), unique_groups)

    # To avoid duplicate labels in the legend
    handles, labels = plt.gca().get_legend_handles_labels()
    by_label = dict(zip(labels, handles))

    # Add legend title and place legend to the right of plot

    plt.legend(by_label.values(), by_label.keys(), title='Home Buyer Type', bbox_to_anchor=(1, 0.8))

    # Format y axis ticks in dollars with commas
    plt.gca().get_yaxis().set_major_formatter(
        plt.FuncFormatter(lambda x, loc: "${:,}".format(int(x))))

    # Add soft gray horizontal lines
    plt.grid(axis='y', color='gray', linestyle='dashed', alpha=0.5)

    plt.xlabel('\nRace of Home Seller(s)')
    plt.ylabel('Predicted ' + target + '\n')



    plt.tight_layout()


    plt.savefig(os.path.join('analysis','figures','model_estimates', 'predictions_' + prediction_type + '_' + model['save_name']))


if __name__ == '__main__':
    # Read in data
    model_estimates = pd.read_csv(os.path.join('data','analysis','model_summaries.csv'))
    print(model_estimates)

    model_estimates['save_name'] = model_estimates['model_name'].str.replace('.RDS','.pdf')

    model_estimates = model_estimates[model_estimates['target'] == 'sp']

    for i, model in model_estimates.iterrows():
        plot_predicted_prices(model, 'mean', 'none')
        plot_predicted_prices(model, 'median', 'none')
        plot_contrasts(model, 'ci')