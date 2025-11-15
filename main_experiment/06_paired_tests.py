import pandas as pd
import numpy as np
from scipy import stats

df = pd.read_csv('aggregated_expanded_data.csv')

df = df[df['group'] != 'no assistance']

df_filtered = df[df['timepoint'].isin(['intermediate', 'after'])]

df_correct = df_filtered.pivot(index='participant_code', 
                             columns='timepoint',
                             values='mean_correct_answer_logit')

df_llm = df_filtered.pivot(index='participant_code',
                          columns='timepoint', 
                          values='mean_llm_answer_logit')

correct_diffs = df_correct['after'] - df_correct['intermediate']
llm_diffs = df_llm['after'] - df_llm['intermediate']

t_stat, p_value = stats.ttest_rel(correct_diffs, llm_diffs)

print("Mean difference in correct answer logit changes:", np.mean(correct_diffs))
print("Mean difference in LLM answer logit changes:", np.mean(llm_diffs))
print("\nPaired t-test results:")
print(f"t-statistic: {t_stat}")
print(f"p-value: {p_value}")

for group_name in ['intervention', 'control']:
    print(f"\n=== Analysis for {group_name} group ===")
    
    df_group = df[df['group'] == group_name]
    df_group_filtered = df_group[df_group['timepoint'].isin(['intermediate', 'after'])]
    
    df_group_correct = df_group_filtered.pivot(index='participant_code',
                                             columns='timepoint',
                                             values='mean_correct_answer_logit')
    
    df_group_llm = df_group_filtered.pivot(index='participant_code',
                                         columns='timepoint',
                                         values='mean_llm_answer_logit')
    
    group_correct_diffs = df_group_correct['after'] - df_group_correct['intermediate']
    group_llm_diffs = df_group_llm['after'] - df_group_llm['intermediate']
    
    group_t_stat, group_p_value = stats.ttest_rel(group_correct_diffs, group_llm_diffs)
    
    print(f"N = {len(group_correct_diffs)}")
    print(f"Mean difference in correct answer logit changes: {np.mean(group_correct_diffs)}")
    print(f"Mean difference in LLM answer logit changes: {np.mean(group_llm_diffs)}")
    print("\nPaired t-test results:")
    print(f"t-statistic: {group_t_stat}")
    print(f"p-value: {group_p_value}")
