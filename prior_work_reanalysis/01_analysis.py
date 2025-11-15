import pandas as pd
import numpy as np
from scipy import stats
import os
import statsmodels.api as sm
import sys

def load_data(file_path):
    """
    Load a single CSV file
    
    Args:
        file_path (str): File path to load
    
    Returns:
        pd.DataFrame: Loaded dataframe
    """
    if os.path.exists(file_path):
        df = pd.read_csv(file_path)
        df['source'] = os.path.basename(file_path)
        return df
    else:
        raise FileNotFoundError(f"File {file_path} not found")

def run_ttest(data, groupby_col, measure_cols, output_file):
    """
    Conduct t-tests to compare means of measure_cols between groups defined by groupby_col
    
    Args:
        data (pd.DataFrame): Input data
        groupby_col (str): Column to group by (should be binary)
        measure_cols (list): List of columns to measure
        output_file: File to write results to
    """
    groups = sorted(data[groupby_col].unique())
    if len(groups) != 2:
        output_file.write(f"Warning: {groupby_col} has {len(groups)} groups, not 2 as expected for t-test\n")
        output_file.write(f"Groups found: {groups}\n")
        return
    
    output_file.write(f"\n=== T-Test Results: Does {groupby_col} predict {', '.join(measure_cols)}? ===\n")
    
    for measure in measure_cols:
        group1 = data[data[groupby_col] == groups[0]][measure]
        group2 = data[data[groupby_col] == groups[1]][measure]
        mean1 = group1.mean()
        mean2 = group2.mean()
        t_stat, p_val = stats.ttest_ind(group1, group2, equal_var=False)
        output_file.write(f"\nMeasure: {measure}\n")
        output_file.write(f"  Mean when {groupby_col}={groups[0]}: {mean1:.4f}\n")
        output_file.write(f"  Mean when {groupby_col}={groups[1]}: {mean2:.4f}\n")
        output_file.write(f"  t-statistic: {t_stat:.4f}\n")
        output_file.write(f"  p-value: {p_val:.4f}\n")
        output_file.write(f"  Significant at α=0.05: {'Yes' if p_val < 0.05 else 'No'}\n")

def create_binary_feature(data, source_col, threshold, comparison_type='greater_than'):
    """
    Create binary feature based on threshold comparison
    
    Args:
        data (pd.DataFrame): Input data
        source_col (str): Column to apply threshold to
        threshold (numeric): Threshold value
        comparison_type (str): Type of comparison ('greater_than' or 'greater_equal')
    
    Returns:
        pd.Series: Binary feature (0 or 1)
    """
    if comparison_type == 'greater_than':
        return (data[source_col] > threshold).astype(int)
    elif comparison_type == 'greater_equal':
        return (data[source_col] >= threshold).astype(int)
    else:
        raise ValueError("comparison_type must be 'greater_than' or 'greater_equal'")

def run_chi_square(data, feature_col, target_col, output_file):
    """
    Conduct chi-square test of independence between feature_col and target_col
    
    Args:
        data (pd.DataFrame): Input data
        feature_col (str): Feature column (independent variable)
        target_col (str): Target column (dependent variable)
        output_file: File to write results to
    """
    contingency = pd.crosstab(data[feature_col], data[target_col])
    chi2, p_val, dof, expected = stats.chi2_contingency(contingency)
    row_totals = contingency.sum(axis=1)
    percentage_table = contingency.div(row_totals, axis=0) * 100
    output_file.write(f"\n=== Chi-Square Test: Does {feature_col} predict {target_col}? ===\n")
    feature_counts = data[feature_col].value_counts()
    feature_percentages = data[feature_col].value_counts(normalize=True) * 100
    output_file.write(f"Note: {feature_col} = 0 occurs {feature_counts.get(0, 0)}/{len(data)} times ({feature_percentages.get(0, 0):.2f}%)\n")
    output_file.write(f"      {feature_col} = 1 occurs {feature_counts.get(1, 0)}/{len(data)} times ({feature_percentages.get(1, 0):.2f}%)\n")
    output_file.write("\nContingency Table (counts):\n")
    output_file.write(f"{contingency}\n")
    output_file.write("\nPercentage Table (% within each row):\n")
    output_file.write(f"{percentage_table.round(2)}\n")
    output_file.write(f"\nchi-square: {chi2:.4f}\n")
    output_file.write(f"p-value: {p_val:.4f}\n")
    output_file.write(f"degrees of freedom: {dof}\n")
    output_file.write(f"Significant at α=0.05: {'Yes' if p_val < 0.05 else 'No'}\n")

def run_logistic_regression(data, predictor_col, target_col, output_file):
    """
    Conduct logistic regression to predict target_col using predictor_col
    
    Args:
        data (pd.DataFrame): Input data
        predictor_col (str): Predictor column (independent variable)
        target_col (str): Target column (dependent variable, binary)
        output_file: File to write results to
    """
    X = sm.add_constant(data[predictor_col])
    y = data[target_col]
    model = sm.Logit(y, X)
    result = model.fit(disp=0)
    coef = result.params[predictor_col]
    p_val = result.pvalues[predictor_col]
    odds_ratio = np.exp(coef)
    output_file.write(f"\n=== Logistic Regression: Does {predictor_col} predict {target_col}? ===\n")
    output_file.write("\nModel Summary:\n")
    output_file.write(f"  Coefficient: {coef:.4f}\n")
    output_file.write(f"  Odds Ratio: {odds_ratio:.4f}\n")
    output_file.write(f"  p-value: {p_val:.4f}\n")
    output_file.write(f"  Significant at α=0.05: {'Yes' if p_val < 0.05 else 'No'}\n")
    output_file.write("\nInterpretation:\n")
    if coef > 0:
        output_file.write(f"  For each unit increase in {predictor_col}, the odds of {target_col}=1 increase by a factor of {odds_ratio:.4f}\n")
    else:
        output_file.write(f"  For each unit increase in {predictor_col}, the odds of {target_col}=1 decrease by a factor of {1/odds_ratio:.4f}\n")
    output_file.write("\nDetailed Model Summary:\n")
    output_file.write(f"{result.summary().tables[1]}\n")

def process_file(file_path, output_file):
    """
    Process a single file with all analyses
    
    Args:
        file_path (str): Path to the file to process
        output_file: File to write results to
    """
    output_file.write(f"\n\n{'='*50}\n")
    output_file.write(f"{os.path.basename(file_path)}\n")
    output_file.write(f"{'='*50}\n\n")
    
    try:
        df = load_data(file_path)
        output_file.write(f"Loaded {len(df)} rows from {file_path}\n")
        df['human_turn_count'] = pd.to_numeric(df['human_turn_count'], errors='coerce')
        df['conversation_count'] = pd.to_numeric(df['conversation_count'], errors='coerce')
        df['is_correct'] = pd.to_numeric(df['is_correct'], errors='coerce')
        df = df.dropna(subset=['human_turn_count', 'conversation_count', 'is_correct', 'correct_answer'])
        output_file.write(f"After cleaning: {len(df)} rows\n")
        run_ttest(df, 'is_correct', ['human_turn_count', 'conversation_count'], output_file)
        run_logistic_regression(df, 'human_turn_count', 'is_correct', output_file)
        run_logistic_regression(df, 'conversation_count', 'is_correct', output_file)
        df['human_turn_gt1'] = create_binary_feature(df, 'human_turn_count', 1)
        df['conv_count_gt1'] = create_binary_feature(df, 'conversation_count', 1)
        run_chi_square(df, 'human_turn_gt1', 'is_correct', output_file)
        run_chi_square(df, 'conv_count_gt1', 'is_correct', output_file)
    except Exception as e:
        output_file.write(f"Error processing {file_path}: {e}\n")

def main():
    # Define file paths
    # m- == MMLU
    # q- == QuALITY
    # -paper- == paper dataset
    # -plus- == replication with alternative model
    file_paths = [
        'm-paper-all-transcripts_output_small.csv',
        'q-paper-all-transcripts_output_small.csv',
        'm-plus-all-transcripts_output_small.csv',
        'q-plus-all-transcripts_output_small.csv'
    ]
    
    with open('01_analysis_output.txt', 'w', encoding='utf-8') as output_file:
        for file_path in file_paths:
            process_file(file_path, output_file)

if __name__ == "__main__":
    main()