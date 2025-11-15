import pandas as pd
import numpy as np
from scipy import stats

data = pd.read_csv('data_format_1.csv')
data = data[data['timepoint'] == 1]

questions_per_participant = data.groupby('participant_id').size()
assert (questions_per_participant == 4).all(), "Each participant should have exactly 4 questions"

participant_accuracy = (data.groupby(['participant_id', 'show_debate'])['correctness']
                      .agg(['sum', 'size'])
                      .assign(accuracy=lambda x: x['sum'] / x['size'] * 100))

show_debate_accuracy = participant_accuracy.loc[participant_accuracy.index.get_level_values('show_debate')]
hide_debate_accuracy = participant_accuracy.loc[~participant_accuracy.index.get_level_values('show_debate')]

show_debate_mean = show_debate_accuracy['accuracy'].mean()
hide_debate_mean = hide_debate_accuracy['accuracy'].mean()

t_stat, p_val = stats.ttest_ind(
    show_debate_accuracy['accuracy'],
    hide_debate_accuracy['accuracy']
)

print("\nAccuracy Analysis Results:")
print(f"Show Debate Mean Accuracy: {show_debate_mean:.2f}%")
print(f"Hide Debate Mean Accuracy: {hide_debate_mean:.2f}%")
print(f"Difference (Show - Hide): {(show_debate_mean - hide_debate_mean):.2f}%")
print(f"\nNumber of participants in Show Debate: {len(show_debate_accuracy)}")
print(f"Number of participants in Hide Debate: {len(hide_debate_accuracy)}")
print(f"\nStatistical Test:")
print(f"t-statistic: {t_stat:.3f}")
print(f"p-value: {p_val:.3f}")