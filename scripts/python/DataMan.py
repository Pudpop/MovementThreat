import numpy as np
import pandas as pd
import os
import math

main_direc = "C:/Users/David/OneDrive/Documents/Work/Thesis/Code/Data"
gt_direc = main_direc + "/groundtruth"
matches_direc = main_direc + "/matches_formatted"

#create list of column names
vars = []
for i in range(1,24):
    #index for player
    b = [i]*8

    #each variable column j  is Vj + player index i
    next = ["{}{}".format(a_, b_) for a_, b_ in zip([f'V{i}' for i in range(1,9)], b)]

    #concat
    vars = np.concatenate([vars,next])

all_cols = np.concatenate([['matchNo','time','state','score_left','score_right','left_team','right_team'],vars])

#cols to turn into id later on for reformatting with wide_to_long
cols = ['matchNo','time','state','score_left','score_right','left_team','right_team']

#instantiate empty df to collect all games
df = pd.DataFrame(columns=np.concatenate([cols,['V1','V2','V3','V4']]))

#loop through all files in directory
print("Processing .csv files")
for matchNo,filename in enumerate(os.listdir(gt_direc)):
    #output for checking
    print(" - Processing match number: " + str(matchNo) + " with filename " + filename)

    

    left_team_name = ""
    right_team_name = ""
    left_team_score = 0
    right_team_score = 0
    
    #variables for filename
    left_team_name = filename.split('-')[1].split('_')[0]
    left_team_score = filename.split('-')[1].split('_')[1]        
    right_team_name = filename.split('-')[3].split('_')[0]
    right_team_score = filename.split('-')[3].split('_')[1]    
    
    new_file_name = matches_direc + '/' + str(matchNo) + '-'+left_team_name+'-'+left_team_score +'-'+right_team_name+'-'+right_team_score + '.csv'
    
    if (not os.path.isfile(new_file_name)):
        #read in file
        data = pd.read_csv(gt_direc + "/" + filename)        
        #add in team names
        data.insert(4,'leftteam',filename.split('-')[1].split('_')[0])
        data.insert(5,'rightteam',filename.split('-')[3].split('_')[0])
    
        #add in dummy sensor values for the ball
        data.insert(10,'dummy1',0)
        data.insert(11,'dummy2',0)
        data.insert(12,'dummy3',0)
        data.insert(13,'dummy4',0)
    
        #reaname columns, ignoring matchNo
        data.columns = all_cols[1:]
    
        #get rid of duplicates
        data = data.drop_duplicates(subset=cols[1:])
    
        #convert to long format using first 6 columns (cols[1:]) as id, grouping other variables by their id after Vi
        data = pd.wide_to_long(data, i=cols[1:], j='id', stubnames=[f'V{i}' for i in range(1,9)], suffix='.*')
    
        #drop sensor data and reindex to format into row format
        data = (data.reset_index().drop(columns=[f'V{i}' for i in range(5,9)])).reset_index().sort_values(['id','time'])
    
        #add matchNo
        data.insert(0,'matchNo',matchNo)
    
        #rename columns
        data.columns = ['matchNo', 'index', 'frame', 'state', 'score_left', 'score_right',
               'left_team', 'right_team', 'player','x','y','vX','vY']
        
        #change bounds of x and y
        data['x'] += 53.5
        data['y'] += 35
        
        #add speeds and group them into discrete speed categories
        #velocity function
        def getSpeed(row):
            return(math.sqrt(row['vX']**2 + row['vY']**2))    
        data['speed'] = data.apply(getSpeed,axis=1)
        bins= [0.000, 0.001,  0.051,  0.101,  0.151,  0.201,  0.251,  0.301,  0.351,  0.401,  0.451,  0.501, 10.000] 
        labels = list(range(1,13))
        data['speedGroup'] = pd.cut(data['speed'], bins=bins, labels=labels,include_lowest = True)
        
        #add time column
        data['time'] = data['frame'].apply(lambda x : x/10)
        
        #get teams
        data['team'] = pd.cut(data['player'],bins = [0,1,12,23],labels = ['b','l','r'],include_lowest=True)
        
        #fix team names and add score
        data['left_team'] = left_team_name
        data['right_team'] = right_team_name
        data['final_score_left'] = left_team_score
        data['final_score_right'] = right_team_score
        
        
    
        #check directory
        if not os.path.exists(matches_direc):
            os.mkdir(matches_direc)    
    
        #write match csv file
        data.to_csv(new_file_name,index=False)
    
        #append to file containing formatted data
        #with open(matches_direc + '/all_games.csv', 'a') as f:
        #    data.to_csv(f, header=False,index=False)