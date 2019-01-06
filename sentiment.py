# -*- coding: utf-8 -*-
"""
Created on Mon Dec 31 20:50:21 2018

@author: Arijit
"""

import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

from textblob import TextBlob

#read xl data into a dataframe
df = pd.read_excel("WebData_downloaded.xlsx", "Data")

#perform the sentiment analysis
bloblist_desc = list()
df_descr_str=df['Shortstory'].astype(str)
for row in df_descr_str:
    blob = TextBlob(row)
    bloblist_desc.append((row,blob.sentiment.polarity, blob.sentiment.subjectivity))
    df_polarity_desc = pd.DataFrame(bloblist_desc, columns = ['sentence','polarity','subjectivity'])
    
def f(df_polarity_desc):
    if df_polarity_desc['polarity'] > 0:
        val = "Positive"
    elif df_polarity_desc['polarity'] == 0:
        val = "Neutral"
    else:
        val = "Negative"
    return val

df_polarity_desc['Sentiment_Type'] = df_polarity_desc.apply(f, axis=1)

#plot the sentiment analysis for all data
plt.figure(figsize=(10,10))
sns.set_style("whitegrid")
ax = sns.countplot(x="Sentiment_Type", data=df_polarity_desc)

#obtain the sentiment in xl
df_polarity_desc.to_excel("Sentiment.xlsx",sheet_name="sentiment")
