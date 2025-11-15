import pandas as pd
import numpy as np

def get_logit(prob_given_to_a, reference_answer):
    if np.isnan(prob_given_to_a):
        return None
    if prob_given_to_a < 1:
        prob_given_to_a = 1
    elif prob_given_to_a > 99:
        prob_given_to_a = 99
    prob_given_to_a = prob_given_to_a / 100
    if reference_answer == 'A':
        prob_given_to_reference = prob_given_to_a
    else:
        assert reference_answer == 'B'
        prob_given_to_reference = 1 - prob_given_to_a
    return np.log(prob_given_to_reference / (1 - prob_given_to_reference))

df = pd.read_csv('tidy_data.csv')

df['probability_assigned_to_b'] = 100 - df['probability_assigned_to_a']

df['probability_assigned_to_correct_answer'] = df.apply(
    lambda x: x['probability_assigned_to_a'] if x['correct_answer'] == 'A' else 100 - x['probability_assigned_to_a'], 
    axis=1
)

df['is_correct'] = df.apply(
    lambda x: 1 if x['probability_assigned_to_correct_answer'] > 50 
             else 0.5 if x['probability_assigned_to_correct_answer'] == 50 
             else 0,
    axis=1
)

# Read the GPT-4 answers from the prompts file
df_prompts = pd.read_csv('original_data/_gpt-4-0613_prompts_main_cx.csv')
gpt_answers_dict = dict(zip(df_prompts['id'], df_prompts['gpt0613s_answer']))

# Add LLM answers from GPT-4 responses
df['llm_answer'] = df['item_id'].apply(lambda x: gpt_answers_dict[x])

df['probability_assigned_to_llm_answer'] = df.apply(
    lambda x: x['probability_assigned_to_a'] if x['llm_answer'] == 'A' else 100 - x['probability_assigned_to_a'],
    axis=1
)

df['favored_llm_answer'] = df.apply(
    lambda x: 1 if x['probability_assigned_to_llm_answer'] > 50 
             else 0.5 if x['probability_assigned_to_llm_answer'] == 50 
             else 0,
    axis=1
)

df['llm_answer_logit'] = df.apply(
    lambda x: get_logit(x['probability_assigned_to_a'], x['llm_answer']),
    axis=1
)

df['correct_answer_logit'] = df.apply(
    lambda x: get_logit(x['probability_assigned_to_a'], x['correct_answer']),
    axis=1
)

# Reorder columns by getting all column names, removing probability_assigned_to_b, and inserting it after probability_assigned_to_a
cols = df.columns.tolist()
cols.remove('probability_assigned_to_b')
prob_a_index = cols.index('probability_assigned_to_a')
cols.insert(prob_a_index + 1, 'probability_assigned_to_b')
df = df[cols]

# Compare logit and correct_answer_logit
comparison = abs(df['logit'] - df['correct_answer_logit']) > 0.00001
if comparison.any():
    # Get rows where they differ significantly
    diff_rows = df[comparison]
    print("\nFound significant differences between logit and correct_answer_logit:")
    print(f"Number of differences: {len(diff_rows)}")
    print("\nFirst few differing rows:")
    print(diff_rows[['item_id', 'llm_answer', 'correct_answer', 
                     'probability_assigned_to_a',
                     'llm_answer_logit', 'correct_answer_logit', 'logit']].head())


df.to_csv('tidy_data_expanded.csv', index=False)
