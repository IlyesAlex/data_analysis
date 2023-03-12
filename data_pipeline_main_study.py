# Coded by Alex Ilyés
# Pipeline for PhD study

#%% Packages
import pandas as pd
import openpyxl

#%% Script

codename = "pilot2"

palt_fn = "data/" + codename + "_palt_2022_Feb_09_1815.csv"
smst_fn = "data/" + codename + "_encoding_smst_2022_Feb_09_1637.csv"
mst_fn = "data/mst_online_PARTICIPANT_SESSION_2022-02-09_18h12.39.375.csv"

#%% sMST
enc_gyak_header = ['feladat', 'Lista száma:', 'itemno', 'noun', 'adjective', 'stimtype', 'enc_practice_loop.thisN', 'enc_keys.keys', 'enc_keys.rt', 'confid_rate_keys.keys']
enc_header = ['feladat', 'Lista száma:', 'itemno', 'noun', 'adjective', 'stimtype', 'enc_loop.thisN', 'enc_keys.keys', 'enc_keys.rt', 'confid_rate_keys.keys']
rec_gyak_header = ['feladat', 'Lista száma:', 'itemno', 'noun', 'adjective', 'stimtype', 'test_pract_loop.thisN', 'test_pract_keys.keys', 'test_pract_keys.rt', 'confid_rate_keys.keys']
rec_header = ['feladat', 'Lista száma:', 'itemno', 'noun', 'adjective', 'stimtype', 'test_loop.thisN', 'test_pract_keys.keys', 'test_pract_keys.rt', 'confid_rate_keys.keys']

smst_header = ['feladat_rész', 'lista', 'itemno', 'noun', 'adjective', 'stimtype', 'trialnum', 'response', 'reaction_time', 'confidence']

enc_gyak_header_dict = {regi : uj for regi, uj in zip(enc_gyak_header, smst_header)}
enc_header_dict = {regi : uj for regi, uj in zip(enc_header, smst_header)}
rec_gyak_header_dict = {regi : uj for regi, uj in zip(rec_gyak_header, smst_header)}
rec_header_dict = {regi : uj for regi, uj in zip(rec_header, smst_header)}

smst_data = pd.read_csv(smst_fn)

smst_enc_gyak = smst_data.loc[4:9, enc_gyak_header[1:]]
smst_enc = smst_data.loc[11:210, enc_header[1:]]
smst_rec_gyak = smst_data.loc[345:350, rec_gyak_header[1:]]
smst_rec = smst_data.loc[352:476, rec_header[1:]]

smst_enc_gyak.insert(loc=0, column='feladat', value=["ENC_GYAK"]*len(smst_enc_gyak))
smst_enc.insert(loc=0, column='feladat', value=["ENC"]*len(smst_enc))
smst_rec_gyak.insert(loc=0, column='feladat', value=["REC_GYAK"]*len(smst_rec_gyak))
smst_rec.insert(loc=0, column='feladat', value=["REC"]*len(smst_rec))

smst_enc_gyak = smst_enc_gyak.rename(columns=enc_gyak_header_dict)
smst_enc = smst_enc.rename(columns=enc_header_dict)
smst_rec_gyak = smst_rec_gyak.rename(columns=rec_gyak_header_dict)
smst_rec = smst_rec.rename(columns=rec_header_dict)

smst_enc_gyak["response"].replace({"2": "good", "j": "no-good"}, inplace=True)
smst_enc["response"].replace({"2": "good", "j": "no-good"}, inplace=True)
smst_rec_gyak["response"].replace({"2": "old", "j": "new"}, inplace=True)
smst_rec["response"].replace({"2": "old", "j": "new"}, inplace=True)
smst_rec_gyak["confidence"].replace({"2": "1", "4": "2", "j": "3"}, inplace=True)
smst_rec["confidence"].replace({"2": "1", "4": "2", "j": "3"}, inplace=True)


smst_whole = pd.concat([smst_enc_gyak, smst_enc, smst_rec_gyak, smst_rec])
smst_whole.insert(loc=0, column='ID', value=[codename]*len(smst_whole))

smst_whole.to_excel("export/" + codename + "_smst.xlsx", engine = "openpyxl", index = False)


#%% MST
mst_data = pd.read_csv(mst_fn)

enc_mst_header = ['feladat', 'stimulus', 'condition', 'trial_index', 'enc_response', 'correct_response']
rec_mst_header = ['feladat', 'stimulus', 'condition', 'trial_index', 'rec_response', 'correct_response']

mst_enc = mst_data.loc[21:84, enc_mst_header[1:]]
mst_rec = mst_data.loc[102:197, rec_mst_header[1:]]
mst_enc.insert(loc=0, column='feladat', value=["ENC"]*len(mst_enc))
mst_rec.insert(loc=0, column='feladat', value=["REC"]*len(mst_rec))
mst_enc.rename(columns={'enc_response':'response'}, inplace=True)
mst_rec.rename(columns={'rec_response':'response'}, inplace=True)


mst_whole = pd.concat([mst_enc, mst_rec])
mst_whole.insert(loc=0, column='ID', value=[codename]*len(mst_whole))

mst_whole.to_excel("export/" + codename + "_mst.xlsx", engine = "openpyxl", index = False)


mst_sum_header = ['targ_old','targ_sim', 'targ_new', 'lure_old', 'lure_sim', 'lure_new', 'foil_old', 'foil_sim', 'foil_new', 'recognition_ratio', 'ldi']
mst_sum = mst_data.loc[198, mst_sum_header].values.flatten().tolist()
mst_sum.insert(0, codename)

#%% palt
palt_data = pd.read_csv(palt_fn)

