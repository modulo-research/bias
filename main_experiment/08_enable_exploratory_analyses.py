import pandas as pd
import numpy as np

df = pd.read_csv('tidy_data_expanded.csv')

group_counts = df.groupby(['participant_code', 'item_id'])['timepoint'].value_counts().unstack(fill_value=0)

valid_groups = (group_counts['before'] == 1) & (group_counts['intermediate'] == 1) & (group_counts['after'] == 1)
if not valid_groups.all():
    invalid_count = (~valid_groups).sum()
    print(f"WARNING: Found {invalid_count} participant_code/item_id pairs without exactly one row for each timepoint")
    print("First few invalid pairs:")
    print(group_counts[~valid_groups].head())
else:
    print("All participant_code/item_id pairs have exactly one row for each timepoint")

constant_columns = ['group', 'medical_experience', 'legal_experience', 
                    'conlang_experience', 'native_english', 'topic']

print("Checking consistency of values within groups (treating all-NaN as consistent)...")
inconsistent_groups = 0

for col in constant_columns:
    grouped = df.groupby(['participant_code', 'item_id'])[col]
    def is_consistent(values):
        non_nan = values.dropna().unique()
        if len(non_nan) == 0:
            return True
        return len(non_nan) == 1
    col_consistency = grouped.apply(is_consistent)

    if not col_consistency.all():
        inconsistent_count = (~col_consistency).sum()
        inconsistent_groups += inconsistent_count
        print(f"WARNING: Found {inconsistent_count} participant_code/item_id pairs with inconsistent '{col}' values (NaNs ignored)")

if inconsistent_groups == 0:
    print("All constant columns have consistent values within groups (NaN = NaN treated as consistent)")

print("Creating logit difference dataframe...")

pivot_df = df.pivot_table(
    index=['participant_code', 'item_id', 'group', 'medical_experience', 
           'legal_experience', 'conlang_experience', 'native_english', 'topic',
           'llm_answer', 'correct_answer'],
    columns='timepoint',
    values='llm_answer_logit'
).reset_index()

pivot_df['llm_logit_difference'] = pivot_df['after'] - pivot_df['before']

pivot_df['model_correct'] = (pivot_df['llm_answer'] == pivot_df['correct_answer']).astype(int)

result_df = pivot_df[['participant_code', 'group', 'medical_experience', 'legal_experience',
                      'conlang_experience', 'native_english', 'item_id', 'topic', 
                      'llm_answer', 'correct_answer', 'model_correct',
                      'llm_logit_difference']]

result_df.to_csv('tidy_data_logit_diffs.csv', index=False)

