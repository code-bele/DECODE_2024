import pandas as pd

adae_df=pd.read_excel("C:\\Users\\chira\\Desktop\\DECODE 2024\\decode_final_round_prog\\decode_final_round_prog\\adae.xlsx")
adsl_df=pd.read_excel("C:\\Users\\chira\\Desktop\\DECODE 2024\\decode_final_round_prog\\decode_final_round_prog\\adsl.xlsx")

#df_filtered = df[(df['DCREASCD'] == 'Adverse Event')]
#df_filtered.to_excel('filtered_to_AE.xlsx')

# Word to search for
'''
word_to_search = 'SUDDEN DEATH'
word_to_search_2='FALL'

# Count rows that contain the word
count = df['AETERM'].str.contains(word_to_search, case=False, na=False).sum()
count_2=df['AETERM'].str.contains(word_to_search_2, case=False, na=False).sum()
print(f"Number of rows containing the word '{word_to_search}': {count}")
print(f"Number of rows containing the word '{word_to_search_2}': {count}")
'''


adverse_event_subjid=adsl_df[(adsl_df['DCREASCD'] == 'Adverse Event')]
print(adverse_event_subjid)
#df_filtered =adsl_df[(adsl_df['DCREASCD'] == 'Adverse Event')]
#print(df_filtered)
values_list = adverse_event_subjid['USUBJID'].tolist()
print(values_list)
print("The length of the list is:"+str(len(values_list)))#shd be 92
filtered_df = adae_df[adae_df['USUBJID'].isin(values_list)]
filtered_df=filtered_df[(filtered_df['AEOUT']=='NOT RECOVERED/NOT RESOLVED')]
print(filtered_df.USUBJID.unique())
print(filtered_df.USUBJID.nunique(dropna=True))
print(filtered_df)


'''
import pandas as pd
import numpy as np


df=pd.read_excel("C:\\Users\\chira\\Desktop\\DECODE 2024\\decode_final_round_prog\\decode_final_round_prog\\adae.xlsx")
print(df.head())
print(df.shape)

print(df.USUBJID.unique())
df.columns = df.columns.str.strip()

print(df.USUBJID.nunique(dropna=True))#gives the unique subject id i.e 225present in this data
print(df.info())



# Assuming your dataframe is named df and the column is named 'AESEV'
df['AESEV'] = df['AESEV'].replace({
    'MILD': 1,
    'MODERATE': 2,
    'SEVERE': 3
})
'''

'''
import pandas as pd
import numpy as np

df=pd.read_excel("C:\\Users\\chira\\Desktop\\DECODE 2024\\decode_final_round_prog\\decode_final_round_prog\\adae.xlsx")
print(df.USUBJID.unique())
print(df.USUBJID.nunique(dropna=True))
print(df.info())


# Word to search for
word_to_search = 'Placebo'

# Count rows that contain the word
count = df['TRTA'].str.contains(word_to_search, case=False, na=False).sum()

print(f"Number of rows containing the word '{word_to_search}': {count}")


'''

'''
import pandas as pd

# Read the Excel file into a DataFrame
df = pd.read_excel("C:\\Users\\chira\\Desktop\\DECODE 2024\\decode_final_round_prog\\decode_final_round_prog\\adae.xlsx")

# Print the first few rows to check the data
print(df.head())

# Filter the DataFrame where both conditions are True
df_filtered = df[(df['TRTEMFL'] == 'Y') & (df['AOCC01FL'] == 'Y')]

# Save the filtered DataFrame to a new Excel file
df_filtered.to_excel('treatment_emergent_1.xlsx', index=False)


'''

'''
import pandas as pd

# Load the datasets
adsl_df = pd.read_excel("C:\\Users\\chira\\Desktop\\DECODE 2024\\decode_final_round_prog\\decode_final_round_prog\\adsl.xlsx")
adae_df = pd.read_excel("C:\\Users\\chira\\Desktop\\DECODE 2024\\decode_final_round_prog\\decode_final_round_prog\\adae.xlsx")
# Step 1: Filter the adsl DataFrame
adsl_filtered = adsl_df[adsl_df['DCREASCD'] == 'Adverse Event'][['USUBJID', 'RFENDT', 'DCREASCD']]

# Step 2: Filter the adae DataFrame based on USUBJID from the filtered adsl DataFrame
adae_filtered = adae_df[adae_df['USUBJID'].isin(adsl_filtered['USUBJID'])]

# Convert dates to datetime for comparison
adae_filtered['AENDT'] = pd.to_datetime(adae_filtered['AENDT'], format='%d-%b-%y', errors='coerce')
adsl_filtered['RFENDT'] = pd.to_datetime(adsl_filtered['RFENDT'], format='%d-%b-%y', errors='coerce')

# Step 3: Define a function to handle both cases
def get_selected_aeterm(sub_df, rfendt):
    # Check if there are any "NOT RECOVERED/NOT RESOLVED" cases
    not_resolved = sub_df[sub_df['AEOUT'] == "NOT RECOVERED/NOT RESOLVED"]
    if not_resolved.shape[0] > 0:
        return not_resolved.iloc[0]['AETERM']
    
    # For "RESOLVED", find the closest AENDT to RFENDT
    resolved = sub_df[sub_df['AEOUT'] == "RESOLVED"]
    if resolved.shape[0] > 0:
        resolved['date_diff'] = (rfendt - resolved['AENDT']).abs()
        closest_row = resolved.loc[resolved['date_diff'].idxmin()]
        return closest_row['AETERM']
    
    return None

# Step 4: Apply the function to each group of USUBJID
final_aeterms = []
for subj_id, group in adae_filtered.groupby('USUBJID'):
    rfendt = adsl_filtered.loc[adsl_filtered['USUBJID'] == subj_id, 'RFENDT'].values[0]
    aeterm = get_selected_aeterm(group, rfendt)
    if aeterm:
        final_aeterms.append({'USUBJID': subj_id, 'AETERM_SELECTED': aeterm})

# Convert the result to a DataFrame
final_adae = pd.DataFrame(final_aeterms)

# Merge with the filtered adsl DataFrame to create the final DataFrame
final_df = pd.merge(adsl_filtered, final_adae, on='USUBJID')

# Display the final DataFrame
print(final_df)
final_df.to_excel("bubble_2.xlsx")

'''


'''
import pandas as pd
import numpy as np


df=pd.read_excel("C:\\Users\\chira\\Desktop\\DECODE 2024\\decode_final_round_prog\\decode_final_round_prog\\adae.xlsx")
print(df.head())
print(df.shape)
#summary_adsl=df.describe()
#summary_adsl.to_csv("summary_adsl.csv")
'''
#In this example, we create a pandas DataFrame from a dictionary and then calculates and prints the number of
 #unique values in the ‘C’ column, excluding NaN values.
 #The result is 3, indicating there are three unique values in column ‘C’.

'''
print(df.AETERM.unique())
print(df.AETERM.nunique(dropna=True))
print(df.info())

#0 for rows and 1 for columns
df=df.drop(['STUDYID','USUBJID','TRT01P','TRT01A','TRT01AN','SAFFL','ITTFL','EDUCLVL'],axis=1)
print(df.head())
df['TRTSDT']=pd.to_datetime(df['TRTSDT'],format='%d-%b-%y')


df['TRTEDT']=pd.to_datetime(df['TRTEDT'],format='%d-%b-%y')

'''
are_columns_equal = (df['RFSTDTC'] == df['TRTSDT']).all()
scam=df['TRTSDT'].equals(df['RFSTDTC'])
print(are_columns_equal)
print(scam)


# Check data types
print(df['RFSTDTC'].dtype) #object
print(df['TRTSDT'].dtype)#datetime64

# Check for any NaN values
print(df['RFSTDTC'].isna().sum())
print(df['TRTSDT'].isna().sum())

# Check for any differences
print((df['RFSTDTC'] == df['TRTSDT']).value_counts())
'''
'''
#so what happened above was that when i used .all it compared the startdate and REFstartdate by implicitly converting the start date 
#to date time format. i.e it did not care about the type of data. it only cared about the similarity of the data. whereas equals()
#cares about the quality of the data hence it showed false. although it was true in the grammatical sense of the datetime
#moreover please note that the data STart data and end date are converted to strings whereas RFSTDTC and RFENDTC are objects'''
'''
are_start_date_equal=(df['RFSTDTC'] == df['TRTSDT']).all()
print(are_start_date_equal)
are_end_date_equal=(df['RFENDTC']==df['TRTEDT']).all()
print(are_end_date_equal)
'''
'''
print((df['RFENDTC'] == df['TRTEDT']).value_counts())

df['COMP8FL'] = df['COMP8FL'].replace('N', '')
df['COMP16FL'] = df['COMP16FL'].replace('N', '')
df['COMP24FL'] = df['COMP24FL'].replace('N', '')

print(df['COMP8FL'])
df.to_excel('filtered.xlsx')
'''

