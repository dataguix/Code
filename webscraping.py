# -*- coding: utf-8 -*-
"""
Created on Mon Oct  1 13:24:55 2018

@author: Asus
"""

import bs4   
import urllib
import re
import pandas as pd
import numpy as np
import nltk
import requests

###############################################################################
###############################Get the CIK (SEC key identifier)of our companies

#Get our company list
from mechanize import Browser
from itertools import chain
import pickle
df= pd.read_csv('liste_firm.csv')
firm = df[u'NAME']



#Get the Central Index Key from company Names
CIK = []
for company in firm:
    print(company)
    print(float(firm.index(company))/float(len(firm))*100)
    browser = Browser()
    browser.open('https://www.sec.gov/edgar/searchedgar/cik.htm')
    browser.select_form(nr=2)
    browser['company'] = company
    response = browser.submit()
    content = response.read()
    ###On prend le plus redondant
    liste_cik = []
    for line in content.splitlines():
        #print(re.findall('CIK=(.*?)">',line))
        if 'CIK' in line:
            liste_cik.append(re.findall('CIK=(.*?)">',line))
    liste_cik = [y for x in liste_cik for y in x]
    if len(liste_cik) == 1:
        CIK.append(liste_cik[0])
    elif len(liste_cik) == 0:
        CIK.append('NOT FOUND')
        
    else:    
        if len(set(liste_cik))==len(liste_cik):
            get_inc_str = []
            for line in content.split('<a'):
                get_inc_str.append(line)
                
            get_inc = []
            for i in range(len(get_inc_str)):
                arr = [' INC',' CORP']
                if any(re.findall('|'.join(arr),str(get_inc_str[i]))):
                    get_inc.append(re.findall('CIK=(.*?)">',str(get_inc_str[i])))
            get_inc = [y for x in get_inc for y in x]
            if len(get_inc) == 0:
                CIK_full = []
                for line in content.splitlines():
                       if 'href' in line:
                           CIK_full.append(re.findall(r'CIK=(.*?)">',line))      
                CIK.append(CIK_full[0])
            
            else:
                CIK.append(get_inc[0])
        else:    
            max_cik = max(liste_cik,key = liste_cik.count)
            CIK.append(max_cik)
        
df['CIK'] = CIK
for item in range(len(df['CIK'])):
    if type(df['CIK'][item]) == list:
        df['CIK'][item] = df['CIK'][item][0]
    
###############################################################################
########################### Get the 10-K filings ##############################



CIK = df['CIK']
date = '20180101'  #Get 10-K until 2018
type_f = '10-K' 
count = 100   #Number of 10-K per website page
for c in range(len(CIK)):
    print(CIK[c])
    print(float(c)/float(len(CIK))*100)
    
    try:
        
        url = 'https://www.sec.gov/cgi-bin/browse-edgar?CIK='+str(CIK[c])+'&type='+str(type_f)+'&dateb='+str(date)+'&owner=exclude&count='+str(count)
        
        html_page = urllib.urlopen(url)
        soup = bs4.BeautifulSoup(html_page)
        try:
            companyName=re.findall('"companyName">(.*?)<acronym',str(soup))
        except:
            companyName = 'No Acronym'
        #print(soup)
        links=[]
        
        for link in soup.findAll('tr'):
            links.append(link)
            
        links2=[]
        for i in range(len(links)):
            links2.append(str(links[i]))
        
        dates = []
        links = []            
        for i in range(len(links2)):
            if '>10-K<'  in links2[i]:
                dates.append(re.findall('\d{4}-\d{2}-\d{2}',links2[i])[-1])
                links.append(re.findall('href=(.*?)id',links2[i])[0])
        
        for l in range(len(links)):
            
            
            links_cut = re.sub('"','',links[l])
            new_url = 'https://www.sec.gov/'+ str(links_cut)
            html_page = urllib.urlopen(new_url)
            soup = bs4.BeautifulSoup(html_page)
            #print(soup)
            links_totext = []
            for link in soup.findAll('a'):
                links_totext.append(link.get('href'))
            
            #links_totext = [i for i in links_totext if '.txt' in i ][0]  
            links_totext = [i for i in links_totext if 'Archives/edgar/data/' in i][0]
            if 'htm' not in links_totext:
                    links_totext = []
                    for link in soup.findAll('a'):
                        links_totext.append(link.get('href'))
                    links_totext= [i for i in links_totext if 'Archives/edgar/data/' and 'txt' in i][0]    
            url_text = 'https://www.sec.gov/'+ str(links_totext)
            html_page = requests.get(url_text)
            a=html_page.text
            a = re.sub('<.*?>','',a)
            a = re.sub('&nbsp','',a)
            a = str(a)
            #l=0
            with open("save_file/"+str(CIK[c])+'_'+str(dates[l])+".txt",'wb') as f:
                     f.write(a)
                
            
            print('Scrapped a 10-K for'+str(CIK[c])+str(companyName)+str(dates[l]))    
    except:
        print('No 10-K')
    


#Put the 10-K in a dataset
        

liste_text = []
liste_Code_CIK = []
liste_date = []
liste_date_abregee = []
liste_size = []
import os
for fn in os.listdir("save_file/"):
    print(float(os.listdir("save_file/"))*100)
    code_CIK = re.findall("(.*?)_",str(fn))
    date = re.findall("_(.*?)\.txt",str(fn))
    annee = re.findall('\d{4}',date[0])
    liste_Code_CIK.append(code_CIK[0])
    liste_date.append(date[0])
    liste_date_abregee.append(annee[0])
    liste_size.append(os.path.getsize("save_file/"+str(fn)))
    with open("save_file/"+str(fn), 'r') as f:
        text = f.read().strip()
        #text = cleanhtml(text)        
        liste_text.append(text)        
        
df_panel = pd.DataFrame({'CIK':liste_Code_CIK,
                         'date':liste_date,
                         'annee': liste_date_abregee,
                         'Text':liste_text,
                         'Size':liste_size
        })
    
df_panel.to_csv('save_file/data.csv')    
        


        