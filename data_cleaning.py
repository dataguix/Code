# -*- coding: utf-8 -*-
"""
Created on Mon Oct  1 13:33:49 2018

@author: Asus
"""

import pandas as pd
import csv
import sys
csv.field_size_limit(sys.maxint)


df = pd.read_csv('data_text.csv',engine = 'python')



import os
import unicodedata
import re
import pandas as pd
import nltk
from nltk.tokenize import RegexpTokenizer
from nltk.corpus import stopwords
from collections import Counter
import pickle
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfVectorizer
import nltk.stem.porter
import unicodedata
nltk.data.path = ['/NLTK_/packages']

def Remove_proper_nouns(string_split):
    string_split_t = nltk.tag.pos_tag(string_split.split())
    edited_sentence = [word for word,tag in string_split_t if tag != 'NNP' and tag != 'NNPS']
    string_split = ' '.join(edited_sentence)
    return string_split

def EncodeText(text):
    try:
        text = unicode(text, 'utf-8')
    except TypeError:
        pass
    text = unicodedata.normalize('NFD', text)
    text = text.encode('ascii', 'ignore')
    text = text.decode("utf-8")
    return str(text)

 

def CleanText(cleantext):
    cleantext = cleantext.lower()
    cleantext = EncodeText(cleantext)
    cleantext = re.sub(r"\n", " ",cleantext)
    cleantext = re.sub(r"\r", " ", cleantext)
    cleantext = re.sub(r"[^a-z ]+", " ", cleantext)
    cleantext = re.sub(r"[aeiou]{3,}.*?", " ", cleantext)
    cleantext = re.sub(r"[bcdfghjklmnpqrstvwxyz]{4,}", " ", cleantext)
    cleantext = re.sub(r" {2,}", " ", cleantext)
    return cleantext


def TokenAndLem(string):
    tokenizer = RegexpTokenizer(r'\w+')
    stopw = set(stopwords.words('english'))
    tokens = tokenizer.tokenize(string)
    tokens = [word for word in tokens if word not in stopw]
    stemmer = nltk.stem.porter.PorterStemmer()
    token_stem = [stemmer.stem(t) for t in tokens]
    token_stem = [x for x in token_stem if ((len(x) > 3) & (len(x) < 15))]
    #f_token_stem = ' '.join(token_stem)
    return token_stem

def from_string_to_token(string):
    string = Remove_proper_nouns(string)
    string = CleanText(string)
    string = TokenAndLem(string)    
    return string


for i in range(len(df)/2):
    print(float(i)/float(len(df)/2)*100)
    df['Text'][i] = from_string_to_token(df['Text'][i])
      


####Matrix of words
vectorizer = CountVectorizer(max_df=0.98,min_df=0.05)
x = vectorizer.fit_transform(df['Text'])
x2 = x.toarray()
x_name = vectorizer.get_feature_names()
bag_of_words_count = pd.DataFrame(x2,columns = x_name)



