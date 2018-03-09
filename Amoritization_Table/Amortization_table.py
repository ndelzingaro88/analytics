# -*- coding: utf-8 -*-
"""
Created on Fri Aug  5 08:20:00 2016

@author: Nick Del Zingaro
"""
#Clears all data in the shell
import subprocess as sp
tmp=sp.call('cls',shell=True)

#Imports the modules
import numpy as np
import pandas as pd
from decimal import Decimal
import datetime 
#from datetime import date
from dateutil.relativedelta import relativedelta


#Creating a Variable
currentdate='2016/08/01'
year,month,day = currentdate.split('/')
date_test = datetime.date(int(year),int(month),int(day))
#test = date_test + relativedelta(months=1)
    
def payment(original_bal,rate,months):
    num = (original_bal*rate*(1+rate)**months)
    denom = ((1+rate)**months)-1
    pmt = (num/denom)
    return pmt

#Reads in unemployment csv
input_file = "unemployment_201604.csv"
unemploy = pd.read_csv (input_file, header =0)
unemploy['report_date'] = pd.to_datetime(unemploy['observation_date'])
unemploy['delta_unemp'] = (unemploy['UNRATE'].shift(0) + unemploy['UNRATE'].shift(1) + unemploy['UNRATE'].shift(2))/3 - (unemploy['UNRATE'].shift(12) + unemploy['UNRATE'].shift(13) + unemploy['UNRATE'].shift(14))/3 ;
#original_headers = list(unemploy.columns.values)
unemploy=unemploy[['delta_unemp','report_date']]

#Reads in VQI
input_file = "vqi.csv"
vqi = pd.read_csv (input_file, header =0)
vqi = vqi[['VQI','orig_dt']]
vqi['orig_dt'] = pd.to_datetime(vqi['orig_dt'],format='%Y-%m-%d')


#Reads in HPI
#ZIP HPI
input_file = "hpi_at_zip.csv"
zip_hpi = pd.read_csv (input_file, header =0)
zip_hpi = zip_hpi[['zip','year','hpidate','monthly_hpi','deltahpi']]
zip_hpi.columns = ['zip','year','date_zip','hpi_zip','deltahpizip']
zip_hpi['new_hpi_zip'] = zip_hpi.groupby(['zip'])['hpi_zip'].shift(1)

#MSA HPI
input_file = "hpi_at_msa.csv"
msa_hpi = pd.read_csv (input_file, header =0)
msa_hpi = msa_hpi[['cbsa','year','hpidate','monthly_hpi','deltahpi']]
msa_hpi.columns = ['msa','year','date_msa','hpi_msa','deltahpimsa']
msa_hpi['new_hpi_msa'] = msa_hpi.groupby(['msa'])['hpi_msa'].shift(1)

#state HPI
input_file = "hpi_at_state.csv"
state_hpi = pd.read_csv (input_file, header =0)
state_hpi = state_hpi[['state','year','hpidate','monthly_hpi','deltahpi']]
state_hpi.columns = ['state','year','date_state','hpi_state','deltahpistate']
state_hpi['new_hpi_state'] = state_hpi.groupby(['state'])['hpi_state'].shift(1)

#national HPI
input_file = "hpi_at_national.csv"
national_hpi = pd.read_csv (input_file, header =0)
national_hpi = national_hpi[['region','year','hpidate','monthly_hpi','deltahpi']]
national_hpi.columns = ['region','year','date_national','hpi_national','deltahpinat']
national_hpi['new_hpi_national'] = national_hpi.groupby(['region'])['hpi_national'].shift(1)

#ZIP HPI
hpi_zip3 = zip_hpi[['zip','date_zip','hpi_zip']]
hpi_zip3=hpi_zip3.rename(columns = {'hpi_zip':'orig_hpi_zip'})
hpi_zip3['date_zip'] = pd.to_datetime(hpi_zip3['date_zip'],format='%Y-%m-%d')
hpi_zip2 = zip_hpi[['zip','date_zip','new_hpi_zip','deltahpizip']]
#MSA HPI
hpi_msa3 = msa_hpi[['msa','date_msa','hpi_msa']]
hpi_msa3=hpi_msa3.rename(columns = {'hpi_msa':'orig_hpi_msa'})
hpi_msa3['date_msa'] = pd.to_datetime(hpi_msa3['date_msa'],format='%Y-%m-%d')
hpi_msa2 = msa_hpi[['msa','date_msa','new_hpi_msa','deltahpimsa']]
#State HPI
hpi_state3 = state_hpi[['state','date_state','hpi_state']]
hpi_state3=hpi_state3.rename(columns = {'hpi_state':'orig_hpi_state'})
hpi_state3['date_state'] = pd.to_datetime(hpi_state3['date_state'],format='%Y-%m-%d')
hpi_state2 = state_hpi[['state','date_state','new_hpi_state','deltahpistate']]
#National HPI
hpi_national3 = national_hpi[['date_national','hpi_national']]
hpi_national3=hpi_national3.rename(columns = {'hpi_national':'orig_hpi_national'})
hpi_national3['date_national'] = pd.to_datetime(hpi_national3['date_national'],format='%Y-%m-%d')
hpi_national2 = national_hpi[['date_national','new_hpi_national','deltahpinat']]


#Reads in coefficient csv
input_file = "Coeff.csv"
coeff = pd.read_csv (input_file, header =0)

#Reads in test loan dataset
input_file = "STACR.csv"
loans = pd.read_csv (input_file, header =0)
original_headers = list(loans.columns.values)
#First Payment in Date format
loans['first_date'] = pd.to_datetime(loans['first_pay_dt'],format='%Y%m')
#Today's date in Date Format
loans['today_date'] = pd.datetime(int(year),int(month),int(day))
#Caluculated loan age off first payment date and current date
loans['loan_age'] = (loans['today_date'].dt.year - loans['first_date'].dt.year) * 12 + (loans['today_date'].dt.month - loans['first_date'].dt.month)
#Calculate new loan terms
loans['terms'] = loans['orig_loan_term']-loans['loan_age']
#creating generic CPR
loans['cpr']=.6


#Merging Original HPI
loans=pd.merge(loans, hpi_zip3[['orig_hpi_zip','date_zip','zip']], how='left', left_on=['first_date','zip'],right_on=['date_zip','zip'])
loans=pd.merge(loans, hpi_msa3[['orig_hpi_msa','date_msa','msa']], how='left', left_on=['first_date','msa'],right_on=['date_msa','msa'])
loans=pd.merge(loans, hpi_state3[['orig_hpi_state','date_state','state']], how='left', left_on=['first_date','state'],right_on=['date_state','state'])
loans=pd.merge(loans, hpi_national3[['orig_hpi_national','date_national']], how='left', left_on='first_date',right_on='date_national')


#Original Value & Original Lien
loans['rs_origvalue'] = loans['orig_upb']*100/loans['orig_ltv']
loans['rs_origlien2'] = loans['orig_cltv']*loans['rs_origvalue']/100-loans['orig_upb']

#Merging VQI
loans=pd.merge(loans, vqi, how='left', left_on='first_date',right_on='orig_dt')

#Loan Purpose & occupancy variables
loans['rs_occ_N'] = np.where(loans['occ_status']!='O',1,0)
loans['rs_purpose__C'] = np.where(loans['loan_purpose'] =='C',1,0)
loans['rs_purpose__R'] = np.where(loans['loan_purpose'] =='N',1,0)

# Define column names
colNames = ('terms','loan_id','upb','interest','sched_prin','report_date','loan_age','orig_hpi_zip','orig_hpi_msa','orig_hpi_state','orig_hpi_national')

# Define a dataframe with the required column names
dfs = pd.DataFrame(columns = colNames)
#dfs = []


for i, row in loans.iterrows():
    org_bal=loans.orig_upb[i]
    mort_bal=loans.upb_iss[i]
    mort_rate = (loans.orig_int_rt[i]/100)/12
    org_term = loans.orig_loan_term[i]
    term = loans.terms[i]
    loan = loans.loan_id[i]
    dates = loans.today_date[i]
    age = loans.loan_age[i]
    loan_zip = loans.zip[i]
    loan_msa = loans.msa[i]
    loan_state = loans.state[i]
    hpi_zip = loans.orig_hpi_zip[i]
    hpi_msa = loans.orig_hpi_msa[i]
    hpi_state = loans.orig_hpi_state[i]
    hpi_national = loans.orig_hpi_national[i]
   # cpr_rate = 1-(1-loans.cpr[i])**(1/12)
   #Scheduled Payment Calculation
    mort_payment = payment(org_bal,mort_rate,org_term)
   #Creates the indexes for each variable
    terms = pd.Series(range(0,term+1,1))
    loan_id = ["XXXXX"] * (term+1)
    upb = [0.0] * (term+1)
    interest = [0.0] * (term+1)
    sched_prin = [0.0] * (term+1)
    prepay = [0.0] * (term +1)
    report_date = [0] * (term+1)
    loan_age=[0] * (term+1)
    orig_hpi_zip=[0] * (term+1)
    orig_hpi_msa=[0] * (term+1)
    orig_hpi_state=[0] * (term+1)
    orig_hpi_national=[0] * (term+1)
    zip_code=[0] * (term+1)
    msa=[0] * (term+1)
    state=[0] * (term+1)
    #inputs the first values
    upb[0] = mort_bal
    report_date[0] = dates
    loan_id[0] = loan
    zip_code[0] = loan_zip
    msa[0] = loan_msa
    state[0] = loan_state
    orig_hpi_zip[0] = hpi_zip
    orig_hpi_msa[0] = hpi_msa
    orig_hpi_state[0] = hpi_state
    orig_hpi_national[0] = hpi_national
    loan_age[0] = age
    upb = [int(x) for x in upb]
    interest = [int(x) for x in interest]
    sched_prin = [int(x) for x in sched_prin]
    prepay = [int(x) for x in prepay]
    for t in range (1,len(terms+1)):
            interest[t] = upb[t-1] * mort_rate
           # cpr[t] = cpr[t-1]
            loan_id[t] = loan_id[t-1]
            zip_code[t] = zip_code[t-1]
            msa[t] = msa[t-1]
            state[t] = state[t-1]
            orig_hpi_zip[t] = orig_hpi_zip[t-1]
            orig_hpi_msa[t] = orig_hpi_msa[t-1]
            orig_hpi_state[t] = orig_hpi_state[t-1]
            orig_hpi_national[t] = orig_hpi_national[t-1]
            loan_age[t] = loan_age[t-1] + 1
            report_date[t] = report_date[t-1] + relativedelta(months=1)
            sched_prin[t] = mort_payment - interest[t]
            if upb[t-1] < sched_prin[t]: 
                upb[t] = 0 
            else: 
                upb[t] =  upb[t-1] - sched_prin[t]
                #Creating the dataframe
                d = { "terms": terms,"loan_id": loan_id,
                    "upb": upb,"interest": interest,"sched_prin":sched_prin,
                     "report_date": report_date,"loan_age": loan_age,
                     "orig_hpi_zip": orig_hpi_zip,"orig_hpi_msa": orig_hpi_msa,
                     "orig_hpi_state": orig_hpi_state,"orig_hpi_national": orig_hpi_national}     
    d=pd.DataFrame(d)
    d=pd.merge(d,unemploy, on='report_date', how='left')
    loans=pd.merge(d, hpi_zip2, how='left', left_on=['report_date','zip'],right_on=['date_zip','zip'])
    loans=pd.merge(d, hpi_msa2, how='left', left_on=['report_date','msa'],right_on=['date_msa','msa'])
    loans=pd.merge(d, hpi_state2, how='left', left_on=['report_date','state'],right_on=['date_state','state'])
    loans=pd.merge(d, hpi_national2, how='left', left_on='report_date',right_on='date_national')
    dfs = pd.concat([dfs, d], axis=0)


#cols = ['terms','upb','interest','sched_prin','am_factor']
#dfs = dfs[cols]
#pd.DataFrame(dfs)

dfs.to_csv('test_unemploy3.csv')

loans.to_csv('test_hpi.csv')



