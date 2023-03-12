# Coded by Alex IlyÃ©s
# Pipeline for PhD study

#%% Packages
import pandas as pd
import openpyxl
import os
import chardet

#%% Script
palt_folder = "../data/C_PALT/" 
smst_folder = "../data/A_semanticMST/" 
mst_folder = "../data/B_MST/" 

palt_clean = "../results/C_PALT/" 
smst_clean = "../results/A_semanticMST/" 
mst_clean = "../results/B_MST/" 
nback_clean = "../results/E_NBack/"


def label_correctness_smst(row):
    if row['stimtype'] == "TARG":
        if row['response'] == 'old':
            return '1'
        elif row['response'] == 'new':
            return '0'
        elif row['response'] == 'None':
            return 'none'
        
    elif row['stimtype'] == "CLOSE" or row['stimtype'] == "DISTANT" or row['stimtype'] == "FOIL":
        if row['response'] == 'old':
            return '0'
        elif row['response'] == 'new':
            return '1'
        elif row['response'] == 'None':
            return 'none'
        
def correct_word(row):
    if row['wordA'] == row['cueword']:
        return row['wordB']
    elif row['wordB'] == row['cueword']:
        return row['wordA']
    
def correctness_palt(row):
    if row['correct_word'] == row['test_ansbox.text']:
        return "1"
    else:
        return "0"

#%% sMST
    
enc_gyak_header = ['feladat', 'Lista száma:', 'itemno', 'noun', 'adjective', 'stimtype', 'enc_practice_loop.thisN', 'enc_keys.keys', 'enc_keys.rt', 'confid_rate_keys.keys', 'confid_rate_keys.rt']
enc_header = ['feladat', 'Lista száma:', 'itemno', 'noun', 'adjective', 'stimtype', 'enc_loop.thisN', 'enc_keys.keys', 'enc_keys.rt', 'confid_rate_keys.keys', 'confid_rate_keys.rt']
rec_gyak_header = ['feladat', 'Lista száma:', 'itemno', 'noun', 'adjective', 'stimtype', 'test_pract_loop.thisN', 'test_pract_keys.keys', 'test_pract_keys.rt', 'confid_rate_keys.keys', 'confid_rate_keys.rt']
rec_header = ['feladat', 'Lista száma:', 'itemno', 'noun', 'adjective', 'stimtype', 'test_loop.thisN', 'test_pract_keys.keys', 'test_pract_keys.rt', 'confid_rate_keys.keys', 'confid_rate_keys.rt']

smst_header = ['feladat_rész', 'lista', 'itemno', 'noun', 'adjective', 'stimtype', 'trialnum', 'response', 'reaction_time', 'confidence', 'confidence_rt']

enc_gyak_header_dict = {regi : uj for regi, uj in zip(enc_gyak_header, smst_header)}
enc_header_dict = {regi : uj for regi, uj in zip(enc_header, smst_header)}
rec_gyak_header_dict = {regi : uj for regi, uj in zip(rec_gyak_header, smst_header)}
rec_header_dict = {regi : uj for regi, uj in zip(rec_header, smst_header)}

nback_header = ['feladat_blokk', 'Betű', 'Answer_key', 'Answer RT', 'Response type', 'Correctness']

smst_header_all = ['ID', 'AgeGroup', 'feladat_rész', 'lista', 'itemno', 'noun', 'adjective', 'stimtype', 'trialnum', 'response', 'reaction_time', 'confidence', 'confidence_rt', 'correctness', 'enc_gyak_num', 'rec_gyak_num']
smst_all_table = pd.DataFrame(columns = smst_header_all)

nback_header_all = ['ID', 'AgeGroup', 'feladat_blokk', 'Betű', 'Answer_key', 'Answer RT', 'Response type', 'Correctness', 'pract_num_one', 'pract_num_two']
nback_all_table = pd.DataFrame(columns = nback_header_all)


smst_whole_header = ['ID', 'AgeGroup', 'task', 'list', 'itemno', 'noun', 'adjective', 'stimtype', 'trialnum', 'response', 'reaction_time', 'confidence', 'confidence_rt', 'correctness', 'enc_pract_num', 'rec_pract_num']
nback_header_export = ['ID', 'AgeGroup', 'block', 'letter', 'response', 'reaction_time', 'response_type', 'correctness', 'pract_num_one', 'pract_num_two']


for age_group in os.listdir(smst_folder):
    
    age_group_folder = smst_folder + age_group + "/"
    
    for smst_file in list(filter(lambda x: '.csv' in x, os.listdir(age_group_folder))):
 
        codename = smst_file[0:6]
        
        smst_filename = age_group_folder + smst_file
    
        smst_data = pd.read_csv(smst_filename)
        
        smst_enc_gyak = smst_data.loc[smst_data["enc_practice_loop.thisRepN"] == 0, enc_gyak_header[1:]]
        smst_enc = smst_data.loc[smst_data["enc_loop.thisRepN"] == 0, enc_header[1:]]
        smst_rec_gyak = smst_data.loc[smst_data["test_pract_loop.thisRepN"] == 0, rec_gyak_header[1:]]
        smst_rec = smst_data.loc[smst_data["test_loop.thisRepN"] == 0, rec_header[1:]]
        
        smst_enc_gyak.insert(loc=0, column='feladat', value=["ENC_GYAK"]*len(smst_enc_gyak))
        smst_enc.insert(loc=0, column='feladat', value=["ENC"]*len(smst_enc))
        smst_rec_gyak.insert(loc=0, column='feladat', value=["REC_GYAK"]*len(smst_rec_gyak))
        smst_rec.insert(loc=0, column='feladat', value=["REC"]*len(smst_rec))
        
        smst_enc_gyak = smst_enc_gyak.rename(columns=enc_gyak_header_dict)
        smst_enc = smst_enc.rename(columns=enc_header_dict)
        smst_rec_gyak = smst_rec_gyak.rename(columns=rec_gyak_header_dict)
        smst_rec = smst_rec.rename(columns=rec_header_dict)
        
        if "confid_break_gombok.rt" in list(smst_data.columns):
            smst_fx_cross_responses = smst_data.loc[(smst_data["test_loop.thisRepN"] == 0) & (smst_data["test_loop.thisRepN"] == 0), ["confid_break_gombok.keys", "confid_break_gombok.rt"]]

            for item_no in list(smst_fx_cross_responses.index.values):
                if smst_fx_cross_responses.loc[item_no, "confid_break_gombok.keys"] != 'None' and  smst_rec.at[item_no, "response"] == 'None':
                    smst_rec.at[item_no, "response"] = smst_fx_cross_responses.loc[item_no, "confid_break_gombok.keys"]
                    smst_rec.at[item_no, "reaction_time"] = 2.00 + smst_fx_cross_responses.loc[item_no, "confid_break_gombok.rt"]
                
        
        smst_enc_gyak["response"].replace({"2": "good", "j": "no-good"}, inplace=True)
        smst_enc["response"].replace({"2": "good", "j": "no-good"}, inplace=True)
        smst_rec_gyak["response"].replace({"2": "old", "j": "new"}, inplace=True)
        smst_rec["response"].replace({"2": "old", "j": "new"}, inplace=True)
        smst_rec_gyak["confidence"].replace({"2": "1", "4": "2", "j": "3"}, inplace=True)
        smst_rec["confidence"].replace({"2": "1", "4": "2", "j": "3"}, inplace=True)
        
        
        smst_whole = pd.concat([smst_enc_gyak, smst_enc, smst_rec_gyak, smst_rec])
        smst_whole.insert(loc=0, column='AgeGroup', value=[age_group]*len(smst_whole))
        smst_whole.insert(loc=0, column='ID', value=[codename]*len(smst_whole))
        smst_whole["trialnum"] = smst_whole["trialnum"] + 1
        
        smst_whole['correctness'] = smst_whole.apply (lambda row: label_correctness_smst(row), axis=1)
        
        enc_gyak_szam = smst_whole["feladat_rész"].value_counts().ENC_GYAK//6
        rec_gyak_szam = smst_whole["feladat_rész"].value_counts().REC_GYAK//6
        
        smst_whole["enc_gyak_num"] = [enc_gyak_szam]*len(smst_whole)
        smst_whole["rec_gyak_num"] = [rec_gyak_szam]*len(smst_whole)  
        
        smst_whole.to_excel(smst_clean + age_group + "/" + codename + "_smst.xlsx", engine = "openpyxl", index = False, header = smst_whole_header)
    
        smst_all_table = pd.concat([smst_all_table, smst_whole])
        
        # N-BACK
        nback_data = smst_data.loc[smst_data["feladat_blokk"].map(lambda x: type(x) == str), nback_header]
        nback_data = nback_data.dropna()
        nback_data.insert(loc=0, column='AgeGroup', value=[age_group]*len(nback_data))
        nback_data.insert(loc=0, column='ID', value=[codename]*len(nback_data))
        
        
        one_gyak_szam = nback_data["feladat_blokk"].value_counts().nback_1_practice//12
        two_gyak_szam = nback_data["feladat_blokk"].value_counts().nback_2_practice//12
        
        nback_data["pract_num_one"] = [one_gyak_szam]*len(nback_data)
        nback_data["pract_num_two"] = [two_gyak_szam]*len(nback_data)  
        
        
        nback_data.to_excel(nback_clean + age_group + "/" + codename + "_nback.xlsx", engine = "openpyxl", index = False, header = nback_header_export)
    
        nback_all_table = pd.concat([nback_all_table, nback_data])

smst_all_table.to_excel(smst_clean + "smst_all_longformat.xlsx", engine = "openpyxl", index = False, header = smst_whole_header)
nback_all_table.to_excel(nback_clean + "nback_all_longformat.xlsx", engine = "openpyxl", index = False, header = nback_header_export)


#%% MST

enc_gyak_mst_header = ['feladat', 'stimulus', 'cond', 'trial_index', 'rt', 'enc_response', 'correct', 'correct_response']
enc_mst_header = ['feladat', 'stimulus', 'condition', 'trial_index', 'rt', 'enc_response', 'correct', 'correct_response']
rec_mst_header = ['feladat', 'stimulus', 'condition', 'trial_index', 'rt', 'rec_response', 'correct', 'correct_response']

mst_header_all = ['ID', 'AgeGroup', 'feladat', 'stimulus', 'condition', 'trial_index', 'rt', 'response', 'correct', 'correct_response']
mst_all_table = pd.DataFrame(columns = mst_header_all)
mst_header_export = ['ID', 'AgeGroup', 'task', 'stimulus', 'condition', 'trialnum', 'reaction_time', 'response', 'correctness', 'correct_response']

mst_sum_header = ['ID', 'targ_old','targ_sim', 'targ_new', 'lure_old', 'lure_sim', 'lure_new', 'foil_old', 'foil_sim', 'foil_new', 'recognition_ratio', 'ldi']
mst_sum_all = pd.DataFrame(columns = mst_sum_header)

for age_group in os.listdir(mst_folder):
    
    age_group_folder = mst_folder + age_group + "/"
    
    for mst_file in list(filter(lambda x: '.csv' in x, os.listdir(age_group_folder))):
        codename = mst_file[0:6]
        
        mst_filename = age_group_folder + mst_file
        print(mst_filename)
        
        mst_data = pd.read_csv(mst_filename)
        
        mst_enc_gyak = mst_data.loc[mst_data["trial_index"].map(lambda x: x in [7,9,11,13,15,17]), enc_gyak_mst_header[1:]]
        mst_rec_gyak = mst_data.loc[mst_data["trial_index"].map(lambda x: x in [88,90,92,94,96,98]), rec_mst_header[1:]]
        mst_enc = mst_data.loc[21:84, enc_mst_header[1:]]
        mst_rec = mst_data.loc[102:197, rec_mst_header[1:]]
        
        mst_enc_gyak.insert(loc=0, column='feladat', value=["ENC_PRACT"]*len(mst_enc_gyak))
        mst_rec_gyak.insert(loc=0, column='feladat', value=["REC_PRACT"]*len(mst_rec_gyak))
        mst_enc.insert(loc=0, column='feladat', value=["ENC"]*len(mst_enc))
        mst_rec.insert(loc=0, column='feladat', value=["REC"]*len(mst_rec))
        
        mst_enc_gyak.rename(columns={'enc_response':'response', 'cond':'condition'}, inplace=True)
        mst_rec_gyak.rename(columns={'rec_response':'response'}, inplace=True)
        mst_enc.rename(columns={'enc_response':'response'}, inplace=True)
        mst_rec.rename(columns={'rec_response':'response'}, inplace=True)
    
        mst_rec_gyak["correct"].replace({True: "1", False: "0"}, inplace=True)    
        mst_rec["correct"].replace({True: "1", False: "0"}, inplace=True)
    
        mst_whole = pd.concat([mst_enc_gyak, mst_enc, mst_rec_gyak, mst_rec])
        mst_whole.insert(loc=0, column='AgeGroup', value=[age_group]*len(mst_whole))
        mst_whole.insert(loc=0, column='ID', value=[codename]*len(mst_whole))
        
        mst_all_table = pd.concat([mst_all_table, mst_whole])
        
        mst_whole.to_excel(mst_clean + age_group + "/" + codename + "_mst.xlsx", engine = "openpyxl", index = False, header = mst_header_export)
         
        mst_sum = mst_data.loc[198, mst_sum_header[1:]].values.flatten().tolist()
        mst_sum.insert(0, codename)
        mst_sum_all.loc[len(mst_sum_all)] = mst_sum        


mst_all_table.to_excel(mst_clean + "mst_all_longformat.xlsx", engine = "openpyxl", index = False, header = mst_header_export)
mst_sum_all.to_excel(mst_clean + "mst_summary.xlsx", engine = "openpyxl", index = False)

#%% palt
palt_header = ['repeat_task.thisRepN', 'itemno', 'wordA', 'wordB', 'cueword', 'test_ansbox.text', 'cue_resp.rt']
palt_excluded_header = ['repeat_task.thisRepN', 'itemno', 'wordA', 'wordB', 'cueword', 'test_ansbox.text', 'cue_resp.rt']

palt_header_all = ['ID', 'AgeGroup', 'trialno', 'repeat_task.thisRepN', 'itemno', 'cueword', 'response_word', 'RT', 'correct_word', 'correctness', 'task_version', 'enc_order']
palt_all_table = pd.DataFrame(columns = palt_header_all)
palt_header_export = ['ID', 'AgeGroup', 'trialno', 'which_rep', 'itemno', 'cueword', 'response_word', 'reaction_time', 'correct_word', 'correctness', 'task_version', 'enc_order']

for age_group in os.listdir(palt_folder):
    
    age_group_folder = palt_folder + age_group + "/"
    
    for palt_file in list(filter(lambda x: '.csv' in x, os.listdir(age_group_folder))):
        codename = palt_file[0:6]
        
        palt_filename = age_group_folder + palt_file
        print(palt_filename)
        
        with open(palt_filename, 'rb') as f:
            enc = chardet.detect(f.read())  # or readline if the file is large
         
        if enc["encoding"] == "UTF-8-SIG":   
            palt_data = pd.read_csv(palt_filename, encoding = enc['encoding'])
        else:
            palt_data = pd.read_csv(palt_filename, encoding = enc['encoding'], sep = "\t")
                
        if palt_file.split(".")[0].split("_")[-1] == "excluded":
            current_palt_header = palt_excluded_header
            palt_data.insert(loc=0, column='repeat_task.thisRepN', value=[0]*len(palt_data))

        else:    
            current_palt_header = palt_header
            
        palt_adat = palt_data.loc[palt_data["test_loop.thisRepN"] == 0, current_palt_header]
        palt_adat['test_ansbox.text'] = palt_adat['test_ansbox.text'].str.strip()
        
        palt_adat['correct_word'] = palt_adat.apply (lambda row: correct_word(row), axis=1)
        palt_adat['correctness'] = palt_adat.apply (lambda row: correctness_palt(row), axis=1)
        
        if palt_file.split(".")[0].split("_")[-1] == "excluded":
            #palt_adat.insert(loc=0, column='repeat_task.thisRepN', value=[0]*len(palt_adat))
            palt_adat['task_version'] = ["old_version"]*len(palt_adat)
        else:
            palt_adat['task_version'] = ["new_version"]*len(palt_adat)
            
        palt_adat.insert(loc=0, column='trialno', value=list(range(1,len(palt_adat)+1)))
        palt_adat.insert(loc=0, column='AgeGroup', value=[age_group]*len(palt_adat))
        palt_adat.insert(loc=0, column='ID', value=[codename]*len(palt_adat))
        
        palt_adat["repeat_task.thisRepN"] = palt_adat["repeat_task.thisRepN"] + 1
        
        for palt_id, palt_row in palt_adat.iterrows():
            enc_order = int(list(palt_data[(palt_data["repeat_task.thisRepN"] == palt_adat.at[palt_id, "repeat_task.thisRepN"]-1) & (palt_data["enc_loop.thisRepN"] == 0) & (palt_data["itemno"] == palt_adat.at[palt_id, "itemno"])]["enc_loop.thisTrialN"])[0])
            palt_adat.at[palt_id, "enc_order"] = enc_order + 1
        
        #palt_adat = palt_adat.drop('wordA', 1)
        #palt_adat = palt_adat.drop('wordB', 1)
        palt_adat.rename(columns={'test_ansbox.text':'response_word'}, inplace=True)
        palt_adat.rename(columns={'cue_resp.rt':'RT'}, inplace=True)
        
        palt_adat = palt_adat.drop(['wordA', 'wordB'], axis=1)
        palt_adat.to_excel(palt_clean + age_group + "/" + codename + "_palt.xlsx", engine = "openpyxl", index = False, header = palt_header_export)
    
        palt_all_table = pd.concat([palt_all_table, palt_adat])

palt_all_table.to_excel(palt_clean + "palt_all_longformat.xlsx", engine = "openpyxl", index = False, header = palt_header_export)


    
 
