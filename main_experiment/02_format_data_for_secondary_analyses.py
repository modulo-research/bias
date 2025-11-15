import pandas as pd
import numpy as np
import re

def get_logit(prob_given_to_a, correct_answer):
    if pd.isna(prob_given_to_a):
        return None
    if prob_given_to_a < 1:
        prob_given_to_a = 1
    elif prob_given_to_a > 99:
        prob_given_to_a = 99
    prob_given_to_a = prob_given_to_a / 100
    if correct_answer == 'A':
        prob_given_to_correct = prob_given_to_a
    else:
        assert correct_answer == 'B'
        prob_given_to_correct = 1 - prob_given_to_a
    return np.log(prob_given_to_correct / (1 - prob_given_to_correct))

def process_dataset(input_file, output_file_without_ids, questions_to_correct_answers):

    df = pd.read_csv(input_file)
    
    df['native_english'] = df['english_proficiency'].apply(lambda x: 1 if x == "Native or native-level (I am a native speaker, or I have attained a level of proficiency that is equivalent to that of a native speaker)" else ('' if pd.isna(x) else 0))

    experience_cols = ['medical_experience', 'legal_experience', 'conlang_experience']
    for col in experience_cols:
        df[col] = df[col].apply(lambda x: 1 if x == 'Yes' else (0 if x == 'No' else ''))

    prob_cols = [col for col in df.columns if re.match(r'[^q].+_prob1$', col)]
    question_ids = set([re.match(r'(.+)_prob1$', col).group(1) for col in prob_cols])

    unique_participants = df['participant_code'].unique()
    participant_code_map = {pid: f'P{i+1}' for i, pid in enumerate(unique_participants)}

    data_records = []

    for index, row in df.iterrows():
        participant_id = row['participant_code']
        participant_code = participant_code_map[participant_id]
        group = row['group']
        native_english = row['native_english']
        medical_experience = row['medical_experience']
        legal_experience = row['legal_experience']
        conlang_experience = row['conlang_experience']

        for question_id in question_ids:
            if question_id.startswith('Contracts'):
                topic = 'Contracts'
            elif question_id.startswith('Surgery'):
                topic = 'Surgery'
            elif question_id.startswith('Evidence'):
                topic = 'Evidence'
            elif question_id.startswith('jbo'):
                topic = 'jbo'
            else:
                topic = 'Unknown'

            correct_answer = questions_to_correct_answers.get(question_id, None)
            if correct_answer not in ['A', 'B']:
                raise ValueError(f"Correct answer for question {question_id} is not 'A' or 'B'")

            for timepoint, prob_col_suffix in zip(['before', 'intermediate', 'after'], ['prob1', 'prob2', 'prob3']):
                prob_col = f'{question_id}_{prob_col_suffix}'
                if not prob_col in df.columns:
                    raise ValueError(f"Probability column {prob_col} not found in dataframe")
                
                prob_assigned_to_a = row[prob_col]

                logit = get_logit(prob_assigned_to_a, correct_answer)

                data_records.append({
                    'participant_code': participant_code,
                    'group': group,
                    'medical_experience': medical_experience,
                    'legal_experience': legal_experience,
                    'conlang_experience': conlang_experience,
                    'native_english': native_english,
                    'item_id': question_id,
                    'topic': topic,
                    'timepoint': timepoint,
                    'probability_assigned_to_a': prob_assigned_to_a,
                    'correct_answer': correct_answer,
                    'logit': logit
                })

    long_df = pd.DataFrame(data_records)

    long_df = long_df[['participant_code', 'group', 'medical_experience', 'legal_experience', 'conlang_experience',
                       'native_english', 'item_id', 'topic', 'timepoint', 'probability_assigned_to_a',
                       'correct_answer', 'logit']]

    long_df.to_csv(output_file_without_ids, index=False)


questions_to_correct_answers = {}
df_questions = pd.read_csv('original_data/_gpt-4-0613_prompts_main_cx.csv')
for _, row in df_questions.iterrows():
    questions_to_correct_answers[row['id']] = row['key']

input_file = 'original_data/combined_data_with_demographics_anonymised.csv'
output_file = 'tidy_data.csv'

process_dataset(input_file, output_file, questions_to_correct_answers)
