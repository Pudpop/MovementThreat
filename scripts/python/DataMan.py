import numpy as np
import pandas as pd
import os
import time
import math
import sys
import zipfile
from scipy.spatial import distance_matrix
from itertools import compress
from fnmatch import fnmatch
pd.options.mode.chained_assignment = None  # default='warn'

MAIN_DIREC = "C:/Users/David/OneDrive/Documents/Work/Thesis/Data/"
EVENTS_FILE_NAME = MAIN_DIREC + "events.csv"

#util
def ifelse(boo, yes, no):
    if (boo):
        return(yes)
    return(no)

def loadGroundtruth(matchNo,
                    zp,
                    folderName,
                    roundNo,
                    matchName):
    #output for checking
    print(" - Processing match number: " + str(matchNo) + " " + matchName)
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

    temp = matchName
    matchName = matchName.replace("Ri-one","Rione")
    matchName = matchName.replace("FRA-UNIted","FRAUNIted")
    matchName = matchName.replace("CSU_Yunlu","CSUYunlu")

    left_team_name = ""
    right_team_name = ""
    left_team_score = 0
    right_team_score = 0

    #variables for filename
    left_team_name = matchName.split('-')[1].split('_')[0]
    left_team_score = matchName.split('-')[1].split('_')[1]
    right_team_name = matchName.split('-')[3].split('_')[0]
    right_team_score = matchName.split('-')[3].split('_')[1]

    matchName = temp
    #read in file
    data = pd.read_csv(zp.open(folderName + "round-" + str(roundNo) + "/" + matchName + "-groundtruth.csv"))
    #add in team names
    data.insert(4,'leftteam',matchName.split('-')[1].split('_')[0])
    data.insert(5,'rightteam',matchName.split('-')[3].split('_')[0])

    #add in dummy sensor values for the ball
    data.insert(10,'dummy1',0)
    data.insert(11,'dummy2',0)
    data.insert(12,'dummy3',0)
    data.insert(13,'dummy4',0)

    #reaname columns, ignoring matchNo
    data.columns = all_cols[1:]

    #get rid of duplicates
    data = data.drop_duplicates(subset=cols[1:],keep="last")

    #convert to long format using first 6 columns (cols[1:]) as id, grouping other variables by their id after Vi
    data = pd.wide_to_long(data, i=cols[1:], j='id', stubnames=[f'V{i}' for i in range(1,9)], suffix='.*')

    #drop sensor data and reindex to format into row format
    data = (data.reset_index().drop(columns=[f'V{i}' for i in range(5,9)])).reset_index().sort_values(['id','time'])

    #add matchNo
    data.insert(0,'matchNo',matchNo)

    #rename columns
    data.columns = ['matchNo', 'index', 'frame', 'state', 'score_left', 'score_right',
           'left_team', 'right_team', 'player','x','y','vX','vY']
    data = data.drop_duplicates(subset=['frame','player'],keep="last")

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

    #get direction of players
    def getAngle(row):
        angle = math.atan2(row['vY'],row['vX'])
        if (angle < 0):
            angle = math.pi - angle
        return(angle)
    data['angle'] = data.apply(getAngle,axis=1)
    bins= [0.000, math.pi/4, math.pi/2, 3*math.pi/4,math.pi,5*math.pi/4,3*math.pi/2,7*math.pi/4,2*math.pi]
    labels = list(range(1,9))
    data['angleGroup'] = pd.cut(data['angle'], bins=bins, labels=labels,include_lowest = True)

    data = data.sort_values(by=['frame'])
    list_of_df = [d for _, d in data.groupby(['frame'])]

    #show distance to ball
    def getDist(frame):
        distances = pd.DataFrame(distance_matrix(frame.iloc[:,9:11], frame.iloc[:,9:11]), index=frame['player'], columns=frame['player']).iloc[:,0]
        return(distances)

    #show who is in possession
    def getCurPos(frame):
        frame = frame.sort_values(by='player')
        frameNo = frame['frame'].values
        players = frame['player'].values
        mins = 'none'
        ball = frame.loc[frame['player']==1,['speed']].iloc[0,0]
        dist = getDist(frame)
        if (ball < 1.4):
            mins = np.argmin(dist.iloc[1:]) + 2
        df = pd.DataFrame({'possession' : [mins]*23, 'distToBall' : dist, 'frame' : frameNo,"player" : players})
        return(df)

    #extra information about ball
    def ballInf(frame):
        main = pd.DataFrame(np.array(np.tile(frame.loc[frame['player']==1,['x','y','speed','angle']].iloc[0,:],reps=23)).reshape(23,4),
                            columns = ['ballPosX','ballPosY','ballSpeed','ballAngle'])
        player = pd.DataFrame({'ballPosX' : main['ballPosX'].values, 'ballPosY' : main['ballPosY'].values,
                                'ballSpeed' : main['ballSpeed'].values, 'ballAngle' : main['ballAngle'].values,
                                'frame' : frame['frame'].values, 'player' : frame['player'].values})
        return(player)
    data = pd.merge(data,pd.concat(map(ballInf,list_of_df),ignore_index=True),on=['frame','player'],how='left')
    data = pd.merge(data,pd.concat(map(getCurPos,list_of_df),ignore_index=True),on=['frame','player'],how='left')
    return(data)

def extractPlayerTeam(player_number):
    if (player_number < 13):
        return("l")
    else:
        return("r")

def extractPlayerID(player_number):
    return((player_number-2) % 11 + 1)

def extractEvents(filename,zp):
    #read in data
    try:
        events = pd.read_csv(zp.open(filename),low_memory=False)
    except:
        print("error in events")
        return(None)

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
    events = events.loc[events['action'].isin(["k","gk","gt","t","kg","tg"])]

    #rename players
    events.player = events.player+1

    return(events)

def classifyKicks(data):
    #label goalkeepers
    gkevents = data.loc[data['action'].isin(["gk","gt","kg","tg"])]
    gks = gkevents.player.unique()
    data['isGK'] = data['player'].isin(gks)

    #check if both teams have a keeper
    teamLKeeper = data.loc[data['player'] < 13].loc[data['player']>1,['isGK']].all(axis=None)
    if (not teamLKeeper):
        avePos = data.loc[data['player'].isin([2,3,4,5,6,7,8,9,10,11,12]),['player','x']].groupby('player').mean()
        player_name = np.argmin(avePos)+2
        data.loc[data['player'] == player_name,['isGK']] = True
    teamRKeeper = data.loc[data['player'] > 12,['isGK']].all(axis=None)
    if (not teamRKeeper):
        avePos = data.loc[data['player'].isin([13,14,15,16,17,18,19,20,21,22,23]),['player','x']].groupby('player').mean()
        player_name = np.argmax(avePos)+13
        data.loc[data['player'] == player_name,['isGK']] = True

    #convert gk events to normal events
    data.loc[data['action'].isin(["kg"]),['action']] = 'k'
    data.loc[data['action'].isin(["tg"]),['action']] = 't'

    #get all events
    frames = data.loc[data['action'] == "k"]
    frames = frames['frame']
    frames = data.loc[data['frame'].isin(frames)]

    for level in frames.frame.unique():
        #get all data from this frame
        sub = frames.loc[frames.frame == level]
        #find who did the event
        kicker = sub.loc[frames['action'] == "k"]

        #if there are multiple kickersdon't classify
        if (len(kicker) > 1):
            continue

        #extract information
        player_name = kicker.iloc[0,8]

        #if player is too far away don't count as action
        #print([player_name,level,sub.loc[sub['player'] == player_name,['distToBall']].iloc[0,0]])
        if (sub.loc[sub['player'] == player_name,['distToBall']].iloc[0,0] > 5.0):
            data.loc[(data['frame'] == level) & (data['player'] == player_name),['action']] = ""
            continue

        #if ball travelling slowly then its a dribble
        if (sub.loc[sub['player'] == 1,['speed']].iloc[0,0] <= 1):
            data.loc[(data['frame'] == level) & (data['player'] == player_name),['action']] = "dribble"
            continue

        #more information
        team = kicker.iloc[0,16]
        leftteam = (team=='l')
        left = sub.loc[sub['player'].isin(list(range(2,13)))]
        right = sub.loc[sub['player'].isin(list(range(13,24)))]
        teammates = ifelse(leftteam,
                           left,
                           right)
        opp = ifelse(leftteam,
                     right,
                     left)
        opp_gk = opp.loc[opp['isGK'] == True]

        goal_allowance = 15
        goal_bot = ifelse(leftteam,
                          [107,35-goal_allowance],
                          [0,35-goal_allowance])
        goal_top = ifelse(leftteam,
                          [107,35+goal_allowance] ,
                          [0,35+goal_allowance])
        kick_position = kicker.iloc[0,9:11]

        #check if player in final third
        final_third = ifelse(leftteam,
                             kick_position[0]>70,
                             kick_position[0]<35)

        #check if angle of ball trajectory is toward goal
        angle_top = math.atan2(goal_top[1]-kick_position[1],goal_top[0]-kick_position[0])
        angle_bot = math.atan2(goal_bot[1]-kick_position[1],goal_bot[0]-kick_position[0])
        frame_after = int(level)+1
        ball_direc = 0
        for i in range(0,10):
            ball_direc = data.loc[(data['frame']==frame_after+i) & (data['player'] == 1),['vX','vY']]
            if (len(ball_direc)==1):
                break
            if (len(ball_direc)>1):
                ball_direc = ball_direc.iloc[0,:]
                break
        if (ball_direc.empty):
            continue

        angle_ball = math.atan2(ball_direc.iloc[0,1],ball_direc.iloc[0,0])
        goal_bound = (angle_top > angle_ball) & (angle_bot < angle_ball)

        if (not goal_bound):
            data.loc[(data['frame'] == level) & (data['player'] == player_name),['action']] = "pass"
            continue
        if (not final_third):
            data.loc[(data['frame'] == level) & (data['player'] == player_name),['action']] = "pass"
            continue

        #check if teammates in front of player
        asc = (team == "r")
        teammates['xrank'] = teammates['x'].rank(ascending=asc)
        try:
            rank = float(teammates.loc[teammates['player']==player_name]['xrank'])
        except:
            print(teammates)
            sys.exit()
        if (rank == teammates['xrank'].min()):
            data.loc[(data['frame'] == level) & (data['player'] == player_name),['action']] = "shot"
            continue
        else:
            adv_teammates = teammates.loc[teammates['xrank'] <= rank].loc[teammates['player']!=player_name]
            angles_team = adv_teammates.apply(lambda mate: math.atan2(mate.y-kicker.y,mate.x-kicker.x),axis=1)
            try:
                close_teammate = ifelse(any(pd.Series(angles_team).apply(lambda angle: angle-angle_ball < 0.1)),
                                        True,
                                        False)
            except:
                print(str(rank) +" : "+ str(teammates['xrank']))
                continue
            if (final_third and not (close_teammate)):
                data.loc[(data['frame'] == level) & (data['player'] == player_name),['action']] = "shot"
                continue
            #kind_of_close_teammate = ifelse(any(pd.Series(angles_team).apply(lambda angle: angle-angle_ball < 1.2)),
            #                                True,
            #                                False)
            #if (not (final_third) and not (kind_of_close_teammate)):
            #    data.loc[(data['frame'] == level) & (data['player'] == player_name),['action']] = "shot"
            #    continue

        #classify as pass if not shot or dribble
        data.loc[(data['frame'] == level) & (data['player'] == player_name),['action']] = "pass"


    return(data)


def processMatch(zp,matchNo,folder,roundNo,matchName,files):

    #preliminary formatting
    sta = time.process_time()
    gt = loadGroundtruth(matchNo,zp,folder,roundNo,matchName)

    print("Done with GT:\t" + str(time.process_time()-sta))

    #fix team names
    #print(matchName)
    temp = matchName
    matchName = matchName.replace("Ri-one","Rione")
    matchName = matchName.replace("FRA-UNIted","FRAUNIted")
    matchName = matchName.replace("CSU_Yunlu","CSUYunlu")

    eventsTime = time.process_time()
    #get event data
    left_team_name = matchName.split('-')[1].split('_')[0]
    right_team_name = matchName.split('-')[3].split('_')[0]

    matchName = temp

    #files = [direc + matchName + "-" + left_team_name + "_" + str(x) + "-moving.csv" for x in range(1,12)]+[direc + matchName + "-" + right_team_name + "_" + str(x) + "-moving.csv" for x in range(1,12)]
    evFiles = list(compress(files,[not any(s in filename for s in ["landmark","groundtruth","parameter"]) for filename in files]))

    def tempEx(f):
        return(extractEvents(f,zp))

    events = pd.Series(evFiles).apply(tempEx).values.tolist()
    events = [i for i in events if all(i)]
    events = pd.concat(events)
    events = events.drop_duplicates()

    #add events to groundtruth
    combined = pd.merge(gt,events,on = ["frame","player"],how="left")

    #format some things
    combined['x'].astype('float64')
    combined['y'].astype('float64')
    combined['vX'].astype('float64')
    combined['vY'].astype('float64')
    combined['speed'].astype('float64')

    combined = combined.drop_duplicates(subset=['frame','player'])

    combined = classifyKicks(combined)
    print("Classify Time:\t" + str(time.process_time() - eventsTime))


    #print(combined.loc[(combined['action'].isin(["pass","dribble","shot"])) & combined['frame'] == 1296])

    lastStart = time.process_time()


    #end position of each event
    evSub = combined.loc[combined['action'].isin(["pass","dribble","shot"])]
    evSub.x = evSub.x.shift(-1)
    evSub.y = evSub.y.shift(-1)
    evSub = evSub.loc[:,['frame','player','x','y']]
    evSub.columns = ['frame','player','eventEndPosX','eventEndPosY']
    combined = pd.merge(combined,evSub,on = ["frame","player"],how="left")

    #receiving player of event
    evSub = combined.loc[combined['action'].isin(["pass","dribble","shot"])]
    evSub['recPlayer'] = evSub.player.shift(-1)
    evSub = evSub.loc[:,['frame','player','recPlayer']]
    combined = pd.merge(combined,evSub,on = ["frame","player"],how="left")

    #fix dribbles based on recplayer
    evSub = combined.loc[combined['action'].isin(["pass","dribble","shot"])]
    for i in range(0,len(evSub)):
        if (evSub.iloc[i,:]['player'] == evSub.iloc[i,:]['recPlayer'] ):
            player_name = evSub.iloc[i,:]['player']
            level = evSub.iloc[i,:]['frame']
            combined.loc[(combined['frame'] == level) & (combined['player'] == player_name),['action']] = "dribble"

    #fix shots based on when goals occur
    evSub = combined.loc[combined['state'].isin([" goal_l"," goal_r"])]
    evEvs = combined.loc[combined['action'].isin(["pass","dribble","shot"])]
    for i in range(0,len(evSub)):
        frame = evSub.iloc[i,:]['frame']
        team = "r"
        if (evSub.iloc[i,:]['state'] == " goal_l"):
            team = "l"

        pre_ev = evEvs.loc[evEvs['frame'] < frame]
        for j in range(len(pre_ev)-1,0,-1):
            kick_position = pre_ev.iloc[j,:]['x']
            final_third = ifelse(team == "l",
                                 kick_position>70,
                                 kick_position<35)
            if (pre_ev.iloc[j,:]['team'] == team and final_third):
                shot_time = pre_ev.iloc[j,:]['frame']
                player = pre_ev.iloc[j,:]['player']
                combined.loc[(combined['frame'] == shot_time) & (combined['player'] == player),['action']] = "shot"
                combined.loc[(combined['frame'] == shot_time) & (combined['player'] == player),['recPlayer']] = "goal"
                break

    #add success of events
    combined['eventSuccess'] = combined.action
    evSub = combined.loc[combined['action'].isin(["pass","dribble","shot"])]
    for i in range(0,len(evSub)):
        frame = evSub.iloc[i,:]['frame']
        player  = evSub.iloc[i,:]['player']
        rec = evSub.iloc[i,:]['recPlayer']
        ev = evSub.iloc[i,:]['action']
        if (ev == "shot"):
            if (rec == "goal"):
                combined.loc[(combined['frame'] == frame) & (combined['player'] == player),['eventSuccess']] = True
                continue
            combined.loc[(combined['frame'] == frame) & (combined['player'] == player),['eventSuccess']] = False
            continue
        teammates = ifelse(int(player)<13,
                           list(range(2,13)),
                           list(range(13,24)))
        if (rec in teammates):
            combined.loc[(combined['frame'] == frame) & (combined['player'] == player),['eventSuccess']] = True
            continue
        combined.loc[(combined['frame'] == frame) & (combined['player'] == player),['eventSuccess']] = False




    print("Last Time:\t" + str(time.process_time()-lastStart))
    #print("Done with Events")

    return(combined)

def make_events_database():
    write_file_name = EVENTS_FILE_NAME

    def get_events(game,zp):
        df = pd.read_csv(zp.open(game))
        df = df.loc[df['action'].isin(['pass','dribble','shot','k','kg','gk'])]
        return(df)

    with zipfile.ZipFile(MAIN_DIREC + "matches_formatted.zip") as z:
        for name in z.namelist():
            if fnmatch(name,"*.csv"):
                print(name)
                if (os.path.isfile(write_file_name)):
                    with open(write_file_name,'a') as fd:
                        fd.write(get_events(name,z).to_csv(header=False, index = False,line_terminator='\n') )
                else:
                    with open(write_file_name,'a') as fd:
                        fd.write(get_events(name,z).to_csv(header=True, index = False,line_terminator='\n') )

def process_all_matches():
    count = 0
    f = []
    for path,dirnames,filenames in  os.walk(MAIN_DIREC + 'archive/'):
        f.extend(filenames)
    for zipf in  f:
        with zipfile.ZipFile(MAIN_DIREC + 'archive/' + zipf) as z:
            for filename in z.namelist():
                if len(filename.split("/"))<3:
                    #print(filename)
                    for j in range(1,26):
                        matchFiles = [i for i in z.namelist() if ((filename+"round-"+str(j)+"/") in i and not (filename+"round-"+str(j)+"/")  == i)]
                        if len(matchFiles)==0:
                            continue
                        #print([i for i in matchFiles if "-groundtruth.csv" in i][0].replace("-groundtruth.csv",""))
                        matchName = [i for i in matchFiles if "-groundtruth.csv" in i][0].replace("-groundtruth.csv","").split("/")[2]
                        folder = MAIN_DIREC + 'matches_formatted/' + matchFiles[0].split("/")[0] + "/"
                        if not os.path.exists(folder):
                            os.mkdir(folder)
                        #print(folder+filename +str(count)+"-" + matchName + ".csv")
                        if not os.path.isfile(folder+str(count)+"-" + matchName + ".csv"):
                            processMatch(z,count,filename,j,matchName,matchFiles).to_csv(folder+str(count)+"-"+matchName+".csv")
                        count+=1

if __name__ == "__main__":
    #process_all_matches()
    make_events_database()
