import pandas as pd
import random
import numpy as np

def sanity_check(group_df, group_name):
    group_correctness_avg = group_df['correctness'].mean()
    print(f"{group_name} - Average Correctness:", group_correctness_avg)

    group_correctness_sum = group_df['correctness'].sum()
    print(f"{group_name} - Total Correctness:", group_correctness_sum)

    group_sum_cases = group_df[(group_df['logodds_assigned_to_correct'] > 0) |
                               ((group_df['logodds_assigned_to_correct'] == 0) &
                                group_df['q_id'].str.contains('key-b'))]['correctness'].sum()
    print(f"{group_name} - Total Cases with logodds_assigned_to_correct > 0 or logodds_assigned_to_correct == 0 and q_id contains 'key-b':", group_sum_cases)

def calculate_d_correct(group_df, group_name, csv_infix):
    result_list = []

    for d in np.arange(0, 2.01, 0.01):
        group_df_copy = group_df.copy()

        group_df_copy.loc[:, 'adjusted_logodds'] = group_df_copy['logodds_assigned_to_model_guess'] - 1 + d

        group_df_copy.loc[:, 'd_correct'] = np.where(
            ((group_df_copy['adjusted_logodds'] < 0) & (~group_df_copy['model_correct'])) |
            ((group_df_copy['adjusted_logodds'] > 0) & group_df_copy['model_correct']) |
            ((group_df_copy['adjusted_logodds'] == 0) & group_df_copy['q_id'].str.contains('key-b')), 1, 0)

        total_d_correct = group_df_copy['d_correct'].sum()
        d_accuracy = total_d_correct / len(group_df_copy)
        result_list.append([d, total_d_correct, d_accuracy])

    result_df = pd.DataFrame(result_list, columns=['d', 'total_d_correct', 'd_accuracy'])

    result_df.to_csv(f'{group_name}_{csv_infix}_d_correct.csv', index=False)

    max_accuracy_row = result_df.loc[result_df['d_accuracy'].idxmax()]
    print(f"\n{group_name} - Maximal D-Accuracy is {max_accuracy_row['d_accuracy']} at d = {max_accuracy_row['d']}\n")

    return result_df



np.random.seed(1)
random.seed(1)

def run_2fold_x_validation(df, csv_infix):
    participant_ids = df['participant_id'].unique()

    random.shuffle(participant_ids)
    group_size = len(participant_ids) // 2
    group_a = participant_ids[:group_size]
    group_b = participant_ids[group_size:]

    group_a_df = df[df['participant_id'].isin(group_a)]
    sanity_check(group_a_df, "Group A")
    calculate_d_correct(group_a_df, "Group A", csv_infix)

    group_b_df = df[df['participant_id'].isin(group_b)]
    sanity_check(group_b_df, "Group B")
    calculate_d_correct(group_b_df, "Group B", csv_infix)

data = pd.read_csv('data_format_1.csv')
data = data[data['timepoint'] == 1]
df_show_true = data[data['show_debate']]
df_show_false = data[~data['show_debate']]

print("**** 2-fold cross-validation for show_debate == True ****")
run_2fold_x_validation(df_show_true, 'show_debate')
print("**** 2-fold cross-validation for show_debate == False ****")
run_2fold_x_validation(df_show_false, 'hide_debate')
