import pandas as pd
import numpy as np

df = pd.read_csv('tidy_data.csv')

df = df[df['group'] != 'no assistance']

blank_prob = df['probability_assigned_to_a'].isna().sum()
blank_logit = df['logit'].isna().sum()

print(f"Number of blank/nan values in 'probability_assigned_to_a': {blank_prob}")
print(f"Number of blank/nan values in 'logit': {blank_logit}")

if blank_prob > 0 or blank_logit > 0:
    print("\nRows with blank/nan values:")
    print(df[(df['probability_assigned_to_a'].isna()) | (df['logit'].isna())])
