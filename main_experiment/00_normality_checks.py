import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns
from scipy.stats import shapiro, kstest, norm, probplot, skew, kurtosis

df = pd.read_csv('original_data/combined_data_anonymised.csv')

groups = ['intervention', 'control']
variables = ['total_correct', 'mean_correct', 'mean_logit_final', 'mean_logit_diff']

for var in variables:
    plt.figure(figsize=(12, 8))

    plt.subplot(2, 2, 1)
    for group in groups:
        data = df[df['group'] == group][var]
        plt.hist(data, alpha=0.5, label=group, bins=20)
    plt.xlabel(var)
    plt.ylabel('Frequency')
    plt.legend()
    plt.title(f'Histogram of {var}')
    
    plt.subplot(2, 2, 2)
    for group in groups:
        data = df[df['group'] == group][var]
        probplot(data, dist="norm", plot=plt)
    plt.title(f'Q-Q Plot of {var}')
    
    plt.subplot(2, 2, 3)
    sns.boxplot(x='group', y=var, data=df)
    plt.title(f'Boxplot of {var}')
    
    stats_df = pd.DataFrame()
    for group in groups:
        data = df[df['group'] == group][var]
        skewness = skew(data, nan_policy='omit')
        kurt = kurtosis(data, nan_policy='omit')
        desc = data.describe()
        median = data.median()
        shapiro_test = shapiro(data)
        kstest_normal = kstest(data, 'norm', args=(np.mean(data), np.std(data)))

        stats_df[group] = pd.Series({
            'Mean': desc['mean'],
            'Median': median,
            'Std': desc['std'],
            'Skewness': skewness,
            'Kurtosis': kurt,
            'Shapiro-Wilk p-value': shapiro_test.pvalue,
            'K-S p-value': kstest_normal.pvalue
        })

    print(f'\nDescriptive Statistics for {var}:\n', stats_df)

    plt.tight_layout()
    plt.savefig(f'{var}_diagnostics.png')
    plt.close()

