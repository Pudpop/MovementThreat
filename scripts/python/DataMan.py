import numpy as np
import pandas as pd
import os

direc = "C:/Users/David/OneDrive/Documents/Work/Thesis/github/scripts/python/groundtruth"

#craete list of column names
vars = []
for i in range(1,24):
    #index for player
    b = [i]*8
    
    #each variable column j  is Vj + player index i
    next = ["{}{}".format(a_, b_) for a_, b_ in zip([f'V{i}' for i in range(1,9)], b)]
    
    #concat
    vars = np.concatenate([vars,next])
    
all_cols = np.concatenate([['matchNo','time','state','score_left','score_right','leftteam','rightteam'],vars])

#cols to turn into id later on for reformatting with wide_to_long
cols = ['matchNo','time','state','score_left','score_right','leftteam','rightteam']

#instantiate empty df to collect all games
df = pd.DataFrame(columns=np.concatenate([cols,['V1','V2','V3','V4']]))

for matchNo,filename in enumerate(os.listdir(direc)):
    #output for checking
    print(matchNo)
    
    #read in file
    data = pd.read_csv(direc + "/" + filename)
    
    #add in team names
    data.insert(4,'leftteam',filename.split('-')[1])
    data.insert(5,'rightteam',filename.split('-')[3])
    
    #add in dummy sesnsor values for the ball
    data.insert(10,'dummy1',0)
    data.insert(11,'dummy2',0)
    data.insert(12,'dummy3',0)
    data.insert(13,'dummy4',0) 
    
    #reaname columns, ignoring matchNo
    data.columns = all_cols[1:]
    
    #get rid of duplicates
    data = data.drop_duplicates(subset=cols[1:])
    
    #convert to long format using first 6 columns as, grouping other variables by their id after Vi
    data = pd.wide_to_long(data, i=cols[1:], j='id', stubnames=[f'V{i}' for i in range(1,9)], suffix='.*')
    
    #drop sensor data and reindex to format into row format
    data = (data.reset_index().drop(columns=[f'V{i}' for i in range(5,9)])).reset_index().sort_values(['id','time']) 
    
    #add matchNo
    data.insert(0,'matchNo',matchNo) 
    
    #add to other data
    df = pd.concat([df,data])
    

df.to_csv(path_or_buf = 'C:/Users/David/Desktop/test.csv',index=False)
