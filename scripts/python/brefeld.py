import math
import time
import pandas as pd
import os
import multiprocessing
import zipfile
import h5py
import threading
from multiprocessing.pool import ThreadPool
from collections import Counter

NUM_PROCS = 4
X_SEGMENTATION = 40
Y_SEGMENTATION = 28
NUM_CELLS = X_SEGMENTATION*Y_SEGMENTATION
PITCH_LENGTH = 107
PITCH_WIDTH = 70
X_BIN_WIDTH = PITCH_LENGTH/X_SEGMENTATION
Y_BIN_WIDTH = PITCH_WIDTH/Y_SEGMENTATION
X_BINS= [-5] + [X_BIN_WIDTH*i for i in range(1,X_SEGMENTATION)] + [PITCH_LENGTH+5]
X_LABELS = list(range(0,X_SEGMENTATION))
Y_BINS= [-5] + [Y_BIN_WIDTH*i for i in range(1,Y_SEGMENTATION)] + [PITCH_WIDTH + 5]
Y_LABELS = list(range(0,Y_SEGMENTATION))
PRE_TIMES = [0.2]
TIMES = [0.1,0.5,1,2,3,4,5]
PPC_TIMES = [0.1,0.5,1,2,3,4,5]
ANGLES = [1,2,3,4,5,6,7,8]
SPEEDS = [1,2,3,4,5,6,7,8,9,10,11,12]
PATH_PPC = "C:/Users/David/OneDrive/Documents/Work/Thesis/github/data/positional"
PATH_BRE = "C:/Users/David/OneDrive/Documents/Work/Thesis/github/data/brefeld"
TEAMS = ["Gliders2016","HELIOS2016","Rione","CYRUS","MT2017",
            "Oxsy","FRAUNIted","HELIOS2017","HfutEngine2017","CSUYunlu"]
FILE_DICT = {}

#util
def ifelse(boo, yes, no):
    if (boo):
        return(yes)
    return(no)

#Brefeld's tranformation function. Takes in 3 (x,y) coordinates and outputs a single coordinate
#referring to the transformed position of the player given origin as starting point
def transform(ps,pt,pu):
    r = math.sqrt((pt[0]-pu[0])**2+(pt[1]-pu[1])**2)
    #s = math.sqrt((pt[0]-ps[0])**2+(pt[1]-ps[1])**2)
    theta = math.atan2(pt[1]-ps[1],pt[0]-ps[0])-math.atan2(pu[1]-pt[1],pu[0]-pt[0])
    return([r*math.cos(theta) + PITCH_LENGTH/2,r*math.sin(theta) + PITCH_WIDTH/2])

#transforms a row in the data frame of triplets into the corresponding point p
def transformRowTriplets(row):
    #print([[float(row.psx),float(row.psy)],[float(row.ptx),float(row.pty)],[float(row.pux),float(row.puy)]])
    return(transform([float(row.psx),float(row.psy)],[float(row.ptx),float(row.pty)],[float(row.pux),float(row.puy)]))

class PPCTime(object):

    def __init__(self):
        self.lock = threading.Lock()
        self.file = FILE_DICT["time"]

    def process(self,position):
        def do_time(ind):
            #print("here")
            #big_mat = self.file["time_" + str(ind)][:]
            temp = position.loc[position['tD'] == TIMES[ind]]
            for pos in temp.groupby(['speedGroup','angleGroup']):
                pos=pos[1]
                sp = int(pos['speedGroup'].values[0])
                ang = int(pos['angleGroup'].values[0])
                x = [x for x in list(pos['xBlock'].values) if str(x) != 'nan']
                y = [x for x in list(pos['yBlock'].values) if str(x) != 'nan']
                xEnd = [x for x in list(pos['xEndBlock'].values) if str(x) != 'nan']
                yEnd = [x for x in list(pos['yEndBlock'].values) if str(x) != 'nan']
                b = [(a*Y_SEGMENTATION + b)*NUM_CELLS + c*Y_SEGMENTATION + d for a,b,c,d in zip(x,y,xEnd,yEnd)]
                b  = Counter(b)
                with self.lock:
                    big_mat = self.file["time_" + str(ind)][:]
                    for k in b.keys():
                        if (ind == 0 and sp  == 1 and ang  == 1):
                            if (math.floor(int(k)/NUM_CELLS) == 576):
                                print(str(int(k)%NUM_CELLS) + " " + str(sp) + " " + str(ang) + " " + str(Counter(big_mat[sp-1][ang-1][math.floor(int(k)/NUM_CELLS)])))
                            #print(Counter(big_mat[sp-1][ang-1][math.floor(int(k)/NUM_CELLS)]))
                        big_mat[sp-1][ang-1][math.floor(int(k)/NUM_CELLS)][int(k)%NUM_CELLS] += b[k]
                        #print(Counter(big_mat[sp-1][ang-1][math.floor(int(k)/NUM_CELLS)]))
                        if (ind == 0 and sp  == 1 and ang  == 1):
                            if (math.floor(int(k)/NUM_CELLS) == 576):
                                print(str(int(k)%NUM_CELLS) + " " + str(sp) + " " + str(ang) + " " + str(Counter(big_mat[sp-1][ang-1][math.floor(int(k)/NUM_CELLS)])))

            #with self.lock:
            #    if (ind == 0):
            #        print(Counter(self.file["time_" + str(ind)][0][0][576]))
            #        print("    " + str(Counter(big_mat[0][0][576])))
            #    self.file["time_" + str(ind)][:] = big_mat
            #    if (ind == 0):
            #        print("    "+str(Counter(self.file["time_" + str(ind)][4][0][576])))
        #for i in range(len(TIMES)):
        #    do_time(i)
        pool = multiprocessing.Pool(7)
        pool.apply_async(do_time,list(range(len(TIMES))))
        pool.close()
        pool.join()

class BrefeldPlayer(object):

    def __init__(self,f,given_time,given_player):
        self.file = f
        self.time = given_time
        self.player = given_player

    def update(self,model):
        self.file[str(model[0])][self.time] = self.file[str(model[0])][self.time] + model[1]

    def process(self,sp):
        sped = int(sp['speedGroup'].values[0])
        xEnd = [x for x in list(sp['xEndBlock'].values) if str(x) != 'nan']
        yEnd = [x for x in list(sp['yEndBlock'].values) if str(x) != 'nan']
        b = [a*Y_SEGMENTATION + b for a,b in zip(xEnd,yEnd)]
        b  = Counter(b)
        cnt = [0]*NUM_CELLS
        for k in b.keys():
            cnt[int(k)] = b[k]
        self.update([sped,cnt])

def write_folders():
    for team in TEAMS:
        if not os.path.exists(PATH_BRE + '/' + team):
            os.mkdir(PATH_BRE + '/' + team)

def extract_triplets_into_matrices(filename,zp) :
    start_time = time.process_time()
    print(filename)

    #read in file
    match = pd.read_csv(zp.open(filename))

    #remove ball
    match = match[match.player != 1]
    match = match.iloc[:,1:]
    match = match.loc[(match['state'] == ' play_on')]

    matchID_name = match.iloc[0,0]

    players = pd.Categorical(match['player']).categories

    for ind1 in range(0,len(PRE_TIMES)):
        t = PRE_TIMES[ind1]
        preGap = t/0.1
        for player in players:
            player_time = time.process_time()
            play_df = match.loc[(match['player'] == player)]
            rows = len(play_df)
            #get id variables
            player_id = play_df.iloc[0,8]
            player_name = (int(player_id)-2)%11 + 1
            team_name = ifelse(int(player_id) < 13,
                                               play_df.iloc[0,7],
                                               play_df.iloc[0,6])
            team_side = ifelse(int(player_id) < 13,
                                               "l",
                                               "r")
            print(team_name + " " + str(player_name))

            def form_temp(df):
                main = pd.DataFrame(columns=['matchID','team','side','player',
                                             'psx','psy',
                                             'ptx','pty',
                                             'pux','puy',
                                             'speed','speedGroup',
                                             'angle','angleGroup',
                                             'td','tD',
                                             'xBlock','yBlock',
                                             'xEndBlock','yEndBlock'])
                for ind2 in range(0,len(TIMES)):

                    T = TIMES[ind2]
                    gap = T/0.1
                    psx = df.iloc[0:int(rows-preGap-gap),:].x
                    psy = df.iloc[0:int(rows-preGap-gap),:].y
                    ptx = df.iloc[int(preGap):int(rows-gap),:].x
                    pty = df.iloc[int(preGap):int(rows-gap),:].y
                    pux = df.iloc[int(preGap+gap):int(rows),:].x
                    puy = df.iloc[int(preGap+gap):int(rows),:].y
                    speed = df.iloc[int(preGap):int(rows-gap),:].speed
                    speedGroup = df.iloc[int(preGap):int(rows-gap),:].speedGroup
                    angle = df.iloc[int(preGap):int(rows-gap),:].angle
                    angleGroup = df.iloc[int(preGap):int(rows-gap),:].angleGroup

                    #create id columns
                    x_len = len(psx)
                    td = [t]*x_len
                    tD = [T]*x_len
                    matchID = [matchID_name]*x_len
                    player = [player_name]*x_len
                    team = [team_name]*x_len
                    side = [team_side]*x_len

                    if (team_side == "r"):
                        psx = PITCH_LENGTH - psx
                        ptx = PITCH_LENGTH - ptx
                        pux = PITCH_LENGTH - pux
                        psy = PITCH_WIDTH - psy
                        pty = PITCH_WIDTH - pty
                        puy = PITCH_WIDTH - puy

                    #append temporary data frame
                    temp = pd.DataFrame(list(zip(matchID,team,side,player,
                                                psx,psy,ptx,pty,
                                                pux,puy,speed,speedGroup,
                                                angle,angleGroup,td,tD)),
                                        columns=['matchID','team','side','player',
                                                 'psx','psy',
                                                 'ptx','pty',
                                                 'pux','puy',
                                                 'speed','speedGroup',
                                                 'angle','angleGroup',
                                                 'td','tD'])

                    temp['xBlock'] = pd.cut(temp['ptx'], bins=X_BINS, labels=X_LABELS,include_lowest = True)
                    temp['yBlock'] = pd.cut(temp['pty'], bins=Y_BINS, labels=Y_LABELS,include_lowest = True)
                    temp['xEndBlock'] = pd.cut(temp['pux'], bins=X_BINS, labels=X_LABELS,include_lowest = True)
                    temp['yEndBlock'] = pd.cut(temp['puy'], bins=Y_BINS, labels=Y_LABELS,include_lowest = True)

                    main = pd.concat([main,temp])
                return(main)

            list_of_df = [d for _, d in play_df.groupby(play_df.frame - list(range(rows)))]
            temp = pd.DataFrame(columns=['matchID','team','side','player',
                                             'psx','psy',
                                             'ptx','pty',
                                             'pux','puy',
                                             'speed','speedGroup',
                                             'angle','angleGroup',
                                             'td','tD',
                                             'xBlock','yBlock',
                                             'xEndBlock','yEndBlock'])
            for df in list_of_df:
                df_temp  = form_temp(df)
                temp = pd.concat([temp,df_temp])
            if (not temp.empty):
                #f = FILE_DICT["time"]
                time_writer = PPCTime()
                time_writer.process(temp)

                for ind3 in range(len(TIMES)):
                    if (len(temp.loc[temp['tD'] == TIMES[ind3]])>0):
                        St = pd.DataFrame(temp.loc[temp['tD'] == TIMES[ind3]].apply(transformRowTriplets,axis=1).tolist())
                        St.columns = ['xEndBlock','yEndBlock']
                        St['yEndBlock'] = pd.cut(St['xEndBlock'], bins=X_BINS, labels=X_LABELS,include_lowest = True)
                        St['xEndBlock'] = pd.cut(St['yEndBlock'], bins=Y_BINS, labels=Y_LABELS,include_lowest = True)
                        St['xBlock'] = temp.loc[temp['tD'] == TIMES[ind3]]['xBlock'].tolist()
                        St['yBlock'] = temp.loc[temp['tD'] == TIMES[ind3]]['xBlock'].tolist()
                        St['speedGroup'] = temp.loc[temp['tD'] == TIMES[ind3]]['speedGroup'].tolist()
                        St.dropna()
                        fb = FILE_DICT[team_name + "/player_" + str(player_name)]
                        player_writer = BrefeldPlayer(fb,ind3,player_name)
                        for frame in  [d for _, d in St.groupby(['speedGroup'])]:
                            player_writer.process(frame)
            #print("\t" + str(time.process_time() - player_time))
    print("Time " + str(ind1) + ": " + str(time.process_time() - start_time))

def make_matrices(path = "C:/Users/David/OneDrive/Documents/Work/Thesis/Data/",wf=True):
    if (wf):
        write_folders()
    FILE_DICT['time'] = h5py.File(PATH_PPC + "/time.hdf5","a",libver='latest')
    if (len(list(FILE_DICT['time'].keys())) == 0):
        for ind in range(len(TIMES)):
            FILE_DICT['time'].create_dataset("time_"+str(ind), (len(SPEEDS),len(ANGLES),NUM_CELLS,NUM_CELLS), compression = "gzip", dtype='i2')
    for team in TEAMS:
        for i in range(1,12):
            FILE_DICT[team + '/player_' + str(i)] = h5py.File(PATH_BRE + "/" + team + "/player_" + str(i) + ".hdf5","a",libver='latest')
            if (len(list(FILE_DICT[team + '/player_' + str(i)].keys())) == 0):
                for j in range(1,13):
                    FILE_DICT[team + '/player_' + str(i)].create_dataset(str(j), (len(TIMES),NUM_CELLS), dtype='i2')

    with zipfile.ZipFile(path + "matches_formatted.zip") as z:
        for file in list(filter(lambda x:x.endswith(".csv"),z.namelist() )):
            extract_triplets_into_matrices(file,z)

if __name__ == '__main__':
    make_matrices()
