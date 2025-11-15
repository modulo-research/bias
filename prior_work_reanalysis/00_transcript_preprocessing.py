import pandas as pd
import re

def process_text_file_to_csv_with_answers(file_title):
    with open(file_title, 'r', encoding='utf-8') as file:
        text = file.read()
    
    answer_pattern = r"Selected: ([A-E])\nCorrect: ([A-E])$"
    
    instances = text.split("------ NEW INSTANCE ------")[1:]
    
    results = pd.DataFrame(columns=['index', 'question', 'question_id', 'discussion', 'human_turn_count', 'conversation_count', 'selected_answer', 'correct_answer', 'is_correct'])
    
    question_ids = {}
    
    for index, instance in enumerate(instances):
        parts = instance.split("------ INITIAL CONVERSATION ------")
        question = parts[0].strip()
        discussion = "\n" + (parts[1].strip() if len(parts) > 1 else "")
        
        human_turn_count = discussion.count("\nHuman: ")
        
        conversation_count = discussion.count("------ RESET CONVERSATION ------") + 1 if discussion else 0
        
        match = re.search(answer_pattern, discussion, flags=re.MULTILINE)
        assert match, f"Discussion does not match required pattern at instance {index + 1}"
        
        selected_answer = match.group(1)
        correct_answer = match.group(2)
        is_correct = 1 if selected_answer == correct_answer else 0
        
        if question not in question_ids:
            question_ids[question] = len(question_ids) + 1
        question_id = question_ids[question]
        
        results.loc[len(results)] = [index + 1, question, question_id, discussion.strip(), human_turn_count, conversation_count, selected_answer, correct_answer, is_correct]
    
    results.to_csv(file_title.replace('.txt', '') + '_output.csv', index=False)

    results.drop(columns=['question', 'discussion']).to_csv(file_title.replace('.txt', '') + '_output_small.csv', index=False)



process_text_file_to_csv_with_answers('m-paper-all-transcripts.txt')
process_text_file_to_csv_with_answers('m-plus-all-transcripts.txt')
process_text_file_to_csv_with_answers('q-paper-all-transcripts.txt')
process_text_file_to_csv_with_answers('q-plus-all-transcripts.txt')
