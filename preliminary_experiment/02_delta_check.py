import pandas as pd
import numpy as np
from scipy import stats

data = pd.read_csv('data_format_1.csv')
data = data[data['timepoint'] == 1]

show_debate_correct = data[(data['show_debate']) & (data['model_correct'])]['logodds_assigned_to_correct'].mean()
show_debate_incorrect = data[(data['show_debate']) & (~data['model_correct'])]['logodds_assigned_to_correct'].mean()
hide_debate_correct = data[(~data['show_debate']) & (data['model_correct'])]['logodds_assigned_to_correct'].mean()
hide_debate_incorrect = data[(~data['show_debate']) & (~data['model_correct'])]['logodds_assigned_to_correct'].mean()

change_when_correct = show_debate_correct - hide_debate_correct
change_when_incorrect = show_debate_incorrect - hide_debate_incorrect

abs_change_correct = abs(change_when_correct)
abs_change_incorrect = abs(change_when_incorrect)

correct_values_show = data[data['show_debate'] == True][data['model_correct'] == True]['logodds_assigned_to_correct']
correct_values_hide = data[data['show_debate'] == False][data['model_correct'] == True]['logodds_assigned_to_correct']
incorrect_values_show = data[data['show_debate'] == True][data['model_correct'] == False]['logodds_assigned_to_correct']
incorrect_values_hide = data[data['show_debate'] == False][data['model_correct'] == False]['logodds_assigned_to_correct']

t_correct, p_correct = stats.ttest_ind(correct_values_show, correct_values_hide)
t_incorrect, p_incorrect = stats.ttest_ind(incorrect_values_show, incorrect_values_hide)

print("\nEffect of showing debate on log odds:")
print(f"When model was correct: {change_when_correct:.3f} (absolute value: {abs_change_correct:.3f})")
print(f"When model was incorrect: {change_when_incorrect:.3f} (absolute value: {abs_change_incorrect:.3f})")
print(f"\nDifference between absolute changes: {abs_change_correct - abs_change_incorrect:.3f}")

print("\nStatistical tests:")
print(f"T-test for effect when correct: t={t_correct:.3f}, p={p_correct:.3f}")
print(f"T-test for effect when incorrect: t={t_incorrect:.3f}, p={p_incorrect:.3f}")

correct_changes = correct_values_show - correct_values_hide.mean()
incorrect_changes = incorrect_values_show - incorrect_values_hide.mean()

abs_correct_changes = np.abs(correct_changes)
abs_incorrect_changes = np.abs(incorrect_changes)

t_abs_diff, p_abs_diff = stats.ttest_ind(abs_correct_changes, abs_incorrect_changes)

print(f"\nTest of difference in absolute changes:")
print(f"t={t_abs_diff:.3f}, p={p_abs_diff:.3f}")
