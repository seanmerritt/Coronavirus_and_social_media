# -*- coding: utf-8 -*-
"""
Created on Thu Jan  9 19:32:11 2020

@author: seanm
"""

import  GetOldTweets3 as got
import csv
import pandas as pd

tweetCriteria = got.manager.TweetCriteria().setQuerySearch('Coronavirus')\
                                           .setSince("2019-10-31")\
                                           .setUntil("2020-04-14")\
                                           .setMaxTweets(5000)
                                           
tweets = got.manager.TweetManager.getTweets(tweetCriteria)

# Creating list of chosen tweet data
user_tweets = [[tweet.date, tweet.text] for tweet in tweets]



##csv.writer(user_tweets, dialect='excel')

df = pd.DataFrame(user_tweets)

df.to_csv (r'C:\Users\seanm\Desktop\ec_dataframe.csv', index = False, header=True)




