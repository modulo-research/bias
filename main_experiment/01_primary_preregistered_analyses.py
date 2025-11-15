import pandas as pd
from scipy.stats import ttest_ind

df = pd.read_csv('original_data/combined_data_anonymised.csv')

groups = ['intervention', 'control']
variables = ['mean_correct', 'mean_logit_final', 'mean_logit_diff']

results_df = pd.DataFrame(columns=[
    'Variable', 'Wave', 'Group 1 Mean (Intervention)', 'Group 1 Median', 'Group 1 Standard Deviation',
    'Group 2 Mean (Control)', 'Group 2 Median', 'Group 2 Standard Deviation', 
    't-statistic', 'Degrees of Freedom', 'p-value'
])

def perform_ttest(data, variable, wave_label='Overall'):
    group1 = data[data['group'] == 'intervention'][variable]
    group2 = data[data['group'] == 'control'][variable]
    t_stat, p_value = ttest_ind(group1, group2, equal_var=False)
    n1, n2 = len(group1), len(group2)
    dof = (group1.var()/n1 + group2.var()/n2)**2 / ((group1.var()**2 / (n1**2 * (n1 - 1))) + (group2.var()**2 / (n2**2 * (n2 - 1))))
    mean1, median1, sd1 = group1.mean(), group1.median(), group1.std()
    mean2, median2, sd2 = group2.mean(), group2.median(), group2.std()
    return {
        'Variable': variable,
        'Wave': wave_label,
        'Group 1 Mean (Intervention)': mean1,
        'Group 1 Standard Deviation': sd1,
        'Group 1 Median': median1,
        'Group 2 Mean (Control)': mean2,
        'Group 2 Standard Deviation': sd2,
        'Group 2 Median': median2,
        't-statistic': t_stat,
        'Degrees of Freedom': dof,
        'p-value': p_value
    }

for variable in variables:
    results_df.loc[len(results_df)] = perform_ttest(df, variable)
    wave1_data = df[df['wave'] == 1]
    results_df.loc[len(results_df)] = perform_ttest(wave1_data, variable, 'Wave 1')
    wave2_data = df[df['wave'] == 2]
    results_df.loc[len(results_df)] = perform_ttest(wave2_data, variable, 'Wave 2')

for index, row in results_df.iterrows():
    wave_label = f"({row['Wave']})" if row['Wave'] != 'Overall' else ""
    print(f"For {row['Variable']} {wave_label}:")
    print(f"Group 1 (Intervention) Mean = {row['Group 1 Mean (Intervention)']:.2f}, SD = {row['Group 1 Standard Deviation']:.2f} (Median = {row['Group 1 Median']:.2f})")
    print(f"Group 2 (Control) Mean = {row['Group 2 Mean (Control)']:.2f}, SD = {row['Group 2 Standard Deviation']:.2f} (Median = {row['Group 2 Median']:.2f})")
    p_value_formatted = f"{row['p-value']:.3f}" if row['p-value'] < 0.05 else f"{row['p-value']:.2f}"
    print(f"t({row['Degrees of Freedom']:.2f}) = {row['t-statistic']:.2f}, p = {p_value_formatted}\n")