import pandas as pd
import numpy as np

df = pd.read_csv('original_data/combined_data_with_demographics_anonymised.csv')

demographic_columns = ['gender', 'age', 'native_language', 'english_proficiency', 
                       'education', 'medical_experience', 'legal_experience', 'conlang_experience']

def analyze_column(df, column_name):
    print(f"\n=== {column_name} ===")
    
    blank_values = df[column_name].isna() | (df[column_name] == '') | (df[column_name] == 'Prefer not to say')
    
    if column_name == 'age':
        try:
            numeric_age = pd.to_numeric(df[column_name], errors='coerce')
            blank_values = blank_values | (numeric_age > 99)
        except:
            pass
    
    blank_count = blank_values.sum()
    non_blank_count = len(df) - blank_count
    
    print(f"Blank/NaN/Prefer not to say: {blank_count}")
    print(f"Provided a response: {non_blank_count}")
    
    if non_blank_count > 0:
        if column_name == 'education':
            valid_education = [
                'No qualification', 
                'Primary school', 
                'GCSE / O-Level / BTEC NVQ Level 2', 
                'A-Level / International Baccalaureate / BTEC NVQ Level 3', 
                'Bachelor\'s degree or equivalent', 
                'Higher National Certificates and Diplomas / Other vocational', 
                'Master\'s degree / Postgraduate qualification', 
                'Doctoral degree'
            ]
            
            education_values = df.loc[~blank_values, column_name].copy()
            education_values = education_values.apply(lambda x: x if x in valid_education else 'Other')
            value_counts = education_values.value_counts().sort_values(ascending=False)
        elif column_name == 'age':
            age_values = df.loc[~blank_values, column_name].copy()
            
            def categorize_age(age):
                if age == 'Over 60' or str(age).lower() == 'over 60':
                    return '60+'
                try:
                    age_num = int(float(age))
                    if age_num >= 18 and age_num <= 29:
                        return '18-29'
                    elif age_num >= 30 and age_num <= 39:
                        return '30-39'
                    elif age_num >= 40 and age_num <= 49:
                        return '40-49'
                    elif age_num >= 50 and age_num <= 59:
                        return '50-59'
                    elif age_num >= 60:
                        return '60+'
                    else:
                        return 'Other'
                except:
                    return 'Other'
            
            age_groups = age_values.apply(categorize_age)
            value_counts = age_groups.value_counts().sort_index()
        else:
            value_counts = df.loc[~blank_values, column_name].value_counts().sort_values(ascending=False)
        
        print("\nValue counts for those who provided a response:")
        for value, count in value_counts.items():
            print(f"  {value}: {count}")

print("DEMOGRAPHIC DATA ANALYSIS")
print("=" * 50)
for column in demographic_columns:
    analyze_column(df, column)
