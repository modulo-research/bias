import pandas as pd
import numpy as np

df = pd.read_csv('tidy_data_expanded.csv')

df_means = df.groupby(['participant_code', 'timepoint']).agg({
    'group': 'first',
    'medical_experience': 'first',
    'legal_experience': 'first', 
    'conlang_experience': 'first',
    'native_english': 'first',
    'probability_assigned_to_correct_answer': 'mean',
    'is_correct': 'mean',
    'favored_llm_answer': 'mean',
    'probability_assigned_to_llm_answer': 'mean',
    'llm_answer_logit': 'mean',
    'correct_answer_logit': 'mean'
}).reset_index()

df_means = df_means.rename(columns={
    'probability_assigned_to_correct_answer': 'mean_probability_assigned_to_correct_answer',
    'is_correct': 'accuracy',
    'favored_llm_answer': 'proportion_of_time_favoring_llm_answer',
    'probability_assigned_to_llm_answer': 'mean_probability_assigned_to_llm_answer',
    'llm_answer_logit': 'mean_llm_answer_logit',
    'correct_answer_logit': 'mean_correct_answer_logit'
})

df_means = df_means[[
    'participant_code',
    'group',
    'medical_experience',
    'legal_experience',
    'conlang_experience',
    'native_english',
    'timepoint',
    'mean_probability_assigned_to_correct_answer',
    'accuracy',
    'mean_probability_assigned_to_llm_answer',
    'proportion_of_time_favoring_llm_answer',
    'mean_correct_answer_logit',
    'mean_llm_answer_logit',
]]

df_means = df_means[~((df_means['timepoint'] == 'intermediate') & (df_means['group'] == 'no assistance'))]

df_means.to_csv('aggregated_expanded_data.csv', index=False)