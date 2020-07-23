import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import os
sns.set_style('dark')

for filename in os.listdir(os.getcwd()+"/groundtruth"):
    data = pd.read_csv(filename)
    
    #and reshape the data that each player's variables are in one row
    game = np.array(data.iloc[:,:]).reshape(-1,8)
    
    #We'll take the last 4 columns of the first row and put them below the first 4 columns of the player's data
    game_xy = np.concatenate([frame[0,4:].reshape(-1,4), frame[1:,:4]]) #have to reshape into a row vector
    
    #add in game details
    
data = pd.read_csv("groundtruth.csv")

data.pivot(index="frame",columns=)
