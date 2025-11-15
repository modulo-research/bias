import pandas as pd
import os
import re
import math
import shutil
import numpy as np

PATH = os.path.dirname(os.path.realpath(__file__))
OUTPUT_PATH = PATH
ORIGINAL_DATA_PATH = PATH + '/original_data'

def logodds(probability):
    if (probability < 0.01):
        probability = 0.01
    elif (probability > 0.99):
        probability = 0.99
    return math.log(probability / (1 - probability))

def get_mean_of_absolute_differences(bins, confidence_categories):
    bin_means = []
    for bin in bins:
        if len(bin) > 0:
            bin_means.append(sum(bin) / len(bin))
        else:
            bin_means.append(None)

    absolute_differences = [abs(mean - confidence) if mean is not None else None 
                            for mean, confidence in zip(bin_means, confidence_categories)]
    
    # Return mean of absolute differences which are not None
    valid_values = [diff for diff in absolute_differences if diff is not None]
    return sum(valid_values) / len(valid_values)

def calculate_brier_score(scalar_confidence_list, accuracy_list):
    return sum([(confidence - accuracy) ** 2 for confidence, accuracy in zip(scalar_confidence_list, accuracy_list)]) / len(scalar_confidence_list)

def calculate_exploratory_calibration_1(likert_confidence_list, accuracy_list):
    if None in likert_confidence_list:
        return None
    
    confidence_categories = [0.5, 0.625, 0.75, 0.875, 1.0]
    bins = [[] for _ in range(5)]

    for bin_str, acc in zip(likert_confidence_list, accuracy_list):
        bin_index = int(bin_str) - 1
        bins[bin_index].append(acc)

    return get_mean_of_absolute_differences(bins, confidence_categories)


def calculate_exploratory_calibration_2(scalar_confidence_list, accuracy_list):
    confidence_categories = [0.5, 0.625, 0.75, 0.875, 1.0]
    bins = [[] for _ in range(5)]

    for conf, acc in zip(scalar_confidence_list, accuracy_list):
        differences = [abs(conf - category) for category in confidence_categories]
        bin_index = differences.index(min(differences))  # Finds the closest category
        bins[bin_index].append(acc)

    return get_mean_of_absolute_differences(bins, confidence_categories)

def check_typeform_present(x):
    if pd.isna(x):
        return np.nan
    else:
        return x

def check_lolo_attempted(lolo):
    if pd.isna(lolo):
        return np.nan
    else:
        return not lolo.endswith('DRAG USELESS ACTIONS BELOW THIS LINE')

def validate_lolo_actions_strict(action_string):
    if pd.isna(action_string):
        return np.nan
    actions = action_string.split(',')

    # Must have the set of 'useless' actions exactly right
    split_index = actions.index('DRAG USELESS ACTIONS BELOW THIS LINE')
    useless_actions = set(['Push the block at E7 to E5',
                       'Push the block at G7 to G5',
                       'Push the block at D6 to C6',
                       'Push the block at H6 to I6',
                       'Go to row 2 by way of column A and pick up the stars in row 2',
                       'Go to row 2 by way of column K and pick up the stars in row 2'])
    actions_after_split = set(actions[split_index+1:])
    if useless_actions != actions_after_split:
        return False
    
    # Chest must be last action before split
    chest_index = actions.index('Go to the chest (while avoiding the gaze of the medusas)')
    if chest_index != split_index - 1:
        return False
    
    # Must have pushed blocks to D5 and H5 before getting stars at F5 or in row 2
    d5_index = actions.index('Push the block at D6 to D5')
    h5_index = actions.index('Push the block at H6 to H5')
    f5_index = actions.index('Pick up the star at F5')
    row2_index = actions.index('Go to row 2 by way of column F and pick up the stars in row 2')
    if d5_index > f5_index or h5_index > f5_index or d5_index > row2_index or h5_index > row2_index:
        return False
    
    # Must have pushed block to C7 before getting star at A8, and block to I7 before getting star at K8
    c7_index = actions.index('Push the block at E7 to C7')
    i7_index = actions.index('Push the block at G7 to I7')
    a8_index = actions.index('Pick up the star at A8')
    k8_index = actions.index('Pick up the star at K8')
    if c7_index > a8_index or i7_index > k8_index:
        return False
    
    return True


def extract_chatlog_data(file_path):
    with open(file_path, 'r', encoding='utf-8') as file:
        content = file.read().split("------ NEW USER INPUT ------")

    user_data = {}
    for block in content[1:]:  # skipping the first block as it's empty
        lines = block.strip().split("\n")
        user_id = lines[0].split(": ")[1]  # extract user id
        user_content = "\n".join(lines[1:]).strip()  # join the rest of the lines

        if user_id in user_data:
            if user_data[user_id] != user_content:
                raise Exception(f'Two instances of {user_id}, with different content')
        else:
            user_data[user_id] = user_content

    return user_data



def extract_merged_data():
    df = pd.read_csv(ORIGINAL_DATA_PATH +  '/anonymised_merged_included.csv')

    with open(ORIGINAL_DATA_PATH + '/anonymised_merged_included_column_names.txt', 'r') as f:
        column_names = f.read().splitlines()
    assert len(column_names) == len(df.columns)
    df.columns = column_names

    df = df.drop(columns=['study_id', 'session_id', 'network_id', 'tags'])

    df['start_date'] = pd.to_datetime(df['start_date'])
    df['submit_date'] = pd.to_datetime(df['submit_date'])

    df = df.sort_values('submit_date').drop_duplicates('anonymised_id', keep='last')

    df['total_minutes'] = (df['submit_date'] - df['start_date']).dt.total_seconds() / 60

    print(len(df))

    df['filler_score'] = 0
    for col in df.columns:
        if re.match(r'filler_\d+', col):
            df['filler_score'] += (df[col] == 'Yes')

    df.replace({'NA': np.nan, '': np.nan}, inplace=True)

    df['core_score'] = 0

    for col in df.columns:
        if col.startswith('valid_'):
            df['core_score'] += (df[col] == 'Yes')
        elif col.startswith('invalid_'):
            df['core_score'] += (df[col] == 'No')

    for col in df.columns:
        if col.startswith('valid_') or col.startswith('invalid_'):
            df.loc[df[col].isna(), 'core_score'] = np.nan
            df.loc[df[col].isna(), 'filler_score'] = np.nan

    df['total_logic_score'] = df['filler_score'] + df['core_score']

    df['passed_both_attention_checks'] = (df['attention_check_1'] == 'dinosaur') & (df['attention_check_2'] == 'candle')

    df['lolo_attempted'] = df['lolo'].apply(check_lolo_attempted)
    df['lolo_correct'] = df['lolo'].apply(validate_lolo_actions_strict)

    df = df[['userid', 'extra_incentive', 'show_debate', 'passed_both_attention_checks', 'filler_score', 'core_score', 'total_logic_score', 'lolo_attempted', 'lolo_correct']]
    print(len(df))
    return df


def raw_df_from_raw_csv(csv_file_name, column_file_name):
    print(csv_file_name)

    print(os.getcwd())

    df = pd.read_csv(csv_file_name)
    
    with open(column_file_name, 'r') as f:
        column_names = f.read().splitlines()
    assert len(column_names) == len(df.columns)
    df.columns = column_names

    df = df.drop(columns=['study_id', 'session_id', 'network_id', 'tags'])

    df['start_date'] = pd.to_datetime(df['start_date'])
    df['submit_date'] = pd.to_datetime(df['submit_date'])

    df = df[df['start_date'] >= pd.Timestamp('2023-04-26 16:00:00')]

    df = df.sort_values('submit_date').drop_duplicates('anonymised_id', keep='last')

    df['total_minutes'] = (df['submit_date'] - df['start_date']).dt.total_seconds() / 60

    return df

def process_user_chatlog_data(text, user_merged_data, chatlog_vars, merged_vars):
    participant_id = re.search(r'Q1 CHAT LOG \((\w+)\)', text).group(1)
    control_variables_missing = False

    numeracy = None
    numeracy_match = re.search(r'Numeracy score: (\d+)', text)
    if numeracy_match:
        numeracy = numeracy_match.group(1)
    else:
        control_variables_missing = True

    crt2 = None
    crt2_match = re.search(r'CRT2 score: (\d+)', text)
    if crt2_match:
        crt2 = crt2_match.group(1)
    else:
        control_variables_missing = True
    
    
    additional_variable_string = ','.join([f'{user_merged_data[var].strip() if isinstance(user_merged_data[var], str) else user_merged_data[var]}' for var in merged_vars])

    q_sections = re.findall(r'(------ Q\d CHAT LOG)(.*?)(?=(------ Q\d CHAT LOG)|$)', text, re.DOTALL)
    rows = []
    likert_confidence_list = []
    scalar_confidence_list = []
    accuracy_list = []
    timepoint = None
    for section in q_sections:
        q_order = re.search(r'Q(\d)', section[0]).group(1)
        q_id = re.search(r'\((stim.*?)\)', section[1]).group(1)
        topic = re.search(r'\((Surgery|Evidence|Contracts|Machine-Generated)\)', section[1]).group(1)
        correct_answer = re.search(r'Correct answer: (\w)', section[1]).group(1)
        model_answer = re.search(r"Model's 'concise answer': (\w)", section[1]).group(1)
        for timepoint, regex in enumerate([r'Preferred answer before interacting with assistant: (\w) \(probability of A: (\d+\.\d+)%\)',
                                           r'Preferred answer after interacting with assistant: (\w) \(probability of A: (\d+\.\d+)%\)']):
            
            correctness = None
            logodds_assigned_to_correct = None
            logodds_assigned_to_model_guess = None
            model_correct = (correct_answer == model_answer)
            match = re.search(regex, section[1])
            if match:
                preferred_answer, probability_of_A = match.groups()
                probability_of_A = float(probability_of_A) / 100
                logodds_assigned_to_correct = logodds(probability_of_A if correct_answer == 'A' else 1 - probability_of_A)
                logodds_assigned_to_model_guess = logodds(probability_of_A if model_answer == 'A' else 1 - probability_of_A)
                correctness = int(preferred_answer == correct_answer)
            
            conf_match = re.search(r'Confidence ' + ('after' if timepoint else 'before') + r' interacting with assistant \[.*\]: (\d)', section[1])
            if conf_match:
                confidence = conf_match.group(1)
            else:
                confidence = None
            
            row_string = ''
            for var in chatlog_vars:
                row_string += str(locals()[var]) + ','
            row_string += additional_variable_string
            rows.append(row_string)
            if timepoint == 1:
                likert_confidence_list.append(confidence)
                scalar_confidence_list.append(abs(0.5 - probability_of_A))
                accuracy_list.append(correctness)
    calibration = {}
    calibration['exploratory_calibration_1'] = calculate_exploratory_calibration_1(likert_confidence_list, accuracy_list)
    calibration['exploratory_calibration_2'] = calculate_exploratory_calibration_2(scalar_confidence_list, accuracy_list)
    calibration['brier_score'] = calculate_brier_score(scalar_confidence_list, accuracy_list)
    return rows, control_variables_missing, calibration


def main():
    file_path = PATH + '/chat_logs.txt'
    chatlog_data = extract_chatlog_data(file_path)
    merged_data = extract_merged_data()

    chatlog_vars = ['participant_id', 'topic', 'q_order', 'q_id', 'timepoint', 'logodds_assigned_to_correct', 'logodds_assigned_to_model_guess', 'correctness', 'confidence', 'numeracy', 'crt2', 'model_correct']
    merged_vars = ['extra_incentive', 'show_debate', 'filler_score', 'core_score', 'total_logic_score', 'lolo_attempted', 'lolo_correct']

    ALL_PARTICIPANTS_FILENAME = 'participants_including_early_quitters.csv'
    with open(OUTPUT_PATH + '/other_subsets/' + ALL_PARTICIPANTS_FILENAME, 'w') as f_original:
        with open(OUTPUT_PATH + '/other_subsets/participants_with_nonmissing_control_and_logic_vars_only.csv', 'w') as f_nonmissing:
            with open(OUTPUT_PATH + '/calibration.csv', 'w') as f_calibration:
                header = ','.join(chatlog_vars + merged_vars) + '\n'
                f_original.write(header)
                f_nonmissing.write(header)
                f_calibration.write('participant_id,extra_incentive,show_debate,exploratory_calibration_1,exploratory_calibration_2,brier_score\n')

                for user, text in chatlog_data.items():
                    user_merged_row = merged_data.loc[merged_data['userid'] == user]
                    assert len(user_merged_row) <= 1

                    if len(user_merged_row) == 1:
                        user_merged_data = user_merged_row.to_dict('records')[0]
                    else:
                        user_merged_data = {var: None for var in merged_vars}

                    
                    user_chatlog_data, control_variables_missing, calibration = process_user_chatlog_data(text, user_merged_data, chatlog_vars, merged_vars)
                    out_block = '\n'.join(user_chatlog_data)  + '\n'
                    
                    f_original.write(out_block)
                    f_calibration.write(f"{user_merged_data['userid']},{user_merged_data['extra_incentive']},{user_merged_data['show_debate']},{calibration['exploratory_calibration_1']},{calibration['exploratory_calibration_2']},{calibration['brier_score']}\n")
                    
                    if len(user_merged_row) == 1 and (not control_variables_missing):
                        f_nonmissing.write(out_block)

    shutil.copyfile(OUTPUT_PATH + '/' + ALL_PARTICIPANTS_FILENAME, OUTPUT_PATH + '/data_format_1.csv')

main()

#print(user_data.values())
