import numpy as np
import pandas as pd
import os
import math
import sys
pd.options.mode.chained_assignment = None  # default='warn'

main_direc = "C:/Users/David/OneDrive/Documents/Work/Thesis/Code/Data"
gt_direc = main_direc + "/groundtruth"
events_direc = main_direc + "/moving_cyrus/"
matches_direc = main_direc + "/matches_formatted"
match_name = "20170905233547-CYRUS_2-vs-HfutEngine2017_1"

#loop through all files in directory
#print("Processing .csv files")
#for matchNo,filename in enumerate(os.listdir(gt_direc)):
     #if (not os.path.isfile(new_file_name)):
     #check directory
     #if not os.path.exists(matches_direc):
     #    os.mkdir(matches_direc)    
     
     #write match csv file
     #data.to_csv(new_file_name,index=False)
 
     #append to file containing formatted data
     #with open(matches_direc + '/all_games.csv', 'a') as f:
     #    data.to_csv(f, header=False,index=False) 
     #new_file_name = matches_direc + '/' + str(matchNo) + '-'+left_team_name+'-'+left_team_score +'-'+right_team_name+'-'+right_team_score + '.csv'
        
def loadGroundtruth(matchNo,filename = "C:/Users/David/OneDrive/Documents/Work/Thesis/Code/Data/groundtruth/20170905233547-CYRUS_2-vs-HfutEngine2017_1-groundtruth.csv"):
    #output for checking
    print(" - Processing match number: " + str(matchNo) + " with filename " + filename)

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

    left_team_name = ""
    right_team_name = ""
    left_team_score = 0
    right_team_score = 0
    
    #variables for filename
    left_team_name = filename.split('-')[1].split('_')[0]
    left_team_score = filename.split('-')[1].split('_')[1]        
    right_team_name = filename.split('-')[3].split('_')[0]
    right_team_score = filename.split('-')[3].split('_')[1]    
    
    #read in file
    data = pd.read_csv(filename)        
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
    
    return(data)  

def extractPlayerTeam(player_number):
    if (player_number < 13):
        return("l")
    else:
        return("r")
    
def extractPlayerID(player_number):
    return((player_number-2) % 11 + 1)

def extractEvents(filename = "C:/Users/David/OneDrive/Documents/Work/Thesis/Code/Data/moving_cyrus/20170905233547-CYRUS_2-vs-HfutEngine2017_1-CYRUS_1-moving.csv"):
    #read in data
    events = pd.read_csv(filename,low_memory=False)
    
    #remove unknown columns, since we cant reliably extract information from them
    events = events.iloc[:,[0] + list(range(5,181))]
    #remove unnecessary information
    events = events.iloc[:,[0]+[x*8 for x in list(range(1,23))]]

    #rename columns
    events.columns = ["frame"] + ["p"+str(x) for x in range(1,23)]

    #remove duplicate frames
    events = events.drop_duplicates(subset=["frame"])

    #reshape data into long format.
    events = pd.wide_to_long(events, i="frame", j='id', stubnames="p")
    events = events.reset_index()
    events.columns = ["frame","player","action"]

    #remove NANs
    events = events.loc[events['action'].isin(["k","gk","gt","t","kg"])]
    
    #rename players
    events.player = events.player+1

    return(events)     

def classifyKicks(data):
    frames = data.loc[data['action'] == "k"]
    frames = frames['frame']
    frames = data.loc[data['frame'].isin(frames)]
    
    for level in frames.frame.unique():
        sub = frames.loc[frames.frame == level]
        kicker = sub.loc[frames['action'] == "k"]
        
        player_name = kicker.iloc[0,8]
        team = kicker.iloc[0,16]
        teammates = sub.loc[sub['player'].isin(list(range(13,24)))]
        opp = sub.loc[sub['player'].isin(list(range(2,13)))]
        opp_gk = sub.loc[sub['player'] == 2]
        goal_bot = [0,35-9.15]
        goal_top = [0,35+9.15]  
        kick_position = kicker.iloc[0,9:11]
        
        if (len(kicker) > 1):
            print("too many kickers")
            continue
        else:
            if (team == "l"):
                opp_gk = sub.loc[sub['player'] == 13]
                opp = teammates
                teammates = sub.loc[sub['player'].isin(list(range(2,13)))]
                goal_bot = [107,35-9.15]
                goal_top = [107,35+9.15]
                
        #check if player in final third
        final_third = (kick_position[0]>70)
        if (team == "r"):
            final_third = (kick_position[0]<35)
                
        #check if angle of ball trajectory is toward goal
        angle_top = math.atan2(goal_top[1]-kick_position[1],goal_top[0]-kick_position[0])
        angle_bot = math.atan2(goal_bot[1]-kick_position[1],goal_bot[0]-kick_position[0])
        frame_after = kicker.iloc[0,2]+1
        ball_direc = frames
        for i in range(0,10):
            ball_direc = data.loc[(data['frame']==frame_after+i) & (data['player'] == 1),['vX','vY']]
            if (len(ball_direc)==1):
                break
            if (len(ball_direc)>1):
                ball_direc = ball_direc.iloc[0,:]
                break
        if (ball_direc.empty):
            print("no more values to determine ball direction")
            continue

        angle_ball = math.atan2(ball_direc.iloc[0,1],ball_direc.iloc[0,0])
        goal_bound = (angle_top > angle_ball) & (angle_bot < angle_ball)

        #check if teammates in front of player
        asc = (team == "r")
        teammates['xrank'] = teammates['x'].rank(ascending=asc)
        rank = teammates.loc[teammates['player']==player_name].iloc[0,len(teammates.columns)-1]
        most_advanced = False
        close_teammate = False
        kind_of_close_teammate = False
        if (rank == teammates['xrank'].min()):
            most_advanced = True
        else:
            adv_teammates = teammates.loc[teammates['xrank'] <= rank].loc[teammates['player']!=player_name]
            angles_team = adv_teammates.apply(lambda mate: math.atan2(mate.y-kicker.y,mate.x-kicker.x),axis=1)
            if (any(pd.Series(angles_team).apply(lambda angle: angle-angle_ball < 0.1))):
                close_teammate = True
            if (any(pd.Series(angles_team).apply(lambda angle: angle-angle_ball < 1.2))):
                kind_of_close_teammate = True                
        
        if (not most_advanced):
            if (goal_bound and final_third and not (close_teammate)):
                data.loc[(data['frame'] == level) & (data['player'] == player_name),['action']] = "shot" 
                continue
            if (goal_bound and not (final_third) and not (kind_of_close_teammate)):
                data.loc[(data['frame'] == level) & (data['player'] == player_name),['action']] = "shot" 
                continue
        else:
            if (goal_bound):
                data.loc[(data['frame'] == level) & (data['player'] == player_name),['action']] = "shot" 
                continue
        if (sub.loc[sub['player'] == 1,['speed']].iloc[0,0] > 1):
            data.loc[(data['frame'] == level) & (data['player'] == player_name),['action']] = "pass"
        else:
            data.loc[(data['frame'] == level) & (data['player'] == player_name),['action']] = "dribble"
        

    return(data)
    

def processMatch(direc,matchNo,matchName):
    gt = loadGroundtruth(matchNo,direc + matchName + "-groundtruth.csv")
    
    print("Done with GT")
    left_team_name = matchName.split('-')[1].split('_')[0]
    right_team_name = matchName.split('-')[3].split('_')[0] 
    
    files = [direc + matchName + "-" + left_team_name + "_" + str(x) + "-moving.csv" for x in range(1,12)]+[direc + matchName + "-" + right_team_name + "_" + str(x) + "-moving.csv" for x in range(1,12)]
    #return(files)
    events = pd.concat(pd.Series(files).apply(extractEvents).values.tolist())
    events = events.drop_duplicates()

    combined = pd.merge(gt,events,on = ["frame","player"],how="left")
    
    combined['x'].astype('float64')
    combined['y'].astype('float64')
    combined['vX'].astype('float64')
    combined['vY'].astype('float64')
    combined['speed'].astype('float64')
    print("Done with Events")
    
    combined = combined.drop_duplicates(subset=['frame','player'])
    
    combined = classifyKicks(combined)
    
    return(combined)

if __name__ == "__main__":
    direc = sys.argv[0]
    matchName = sys.argv[1]
    match = sys.argv[2]
    out_direc = sys.argv[3]
    processMatch(direc,match,matchName).to_csv(out_direc)
    