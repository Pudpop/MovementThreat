import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import os
sns.set_style('dark')

data = pd.read_csv("groundtruth.csv")
#data.columns = data.columns.str.strip()
data.columns = ['time', 'playmode', 'scoreleft', 'scoreright', 'ball_x', 'ball_y', 'ball_vx', 'ball_vy', 'LG1 x', 'LG1 y', 'LG1 vx', 'LG1 vy', 'LG1 body', 'LG1 head', 'LG1 vieww', 'LG1 viewq', 'L2 x', 'L2 y', 'L2 vx', 'L2 vy', 'L2 body', 'L2 head', 'L2 vieww', 'L2 viewq', 'L3 x', 'L3 y', 'L3 vx', 'L3 vy', 'L3 body', 'L3 head', 'L3 vieww', 'L3 viewq', 'L4 x', 'L4 y', 'L4 vx', 'L4 vy', 'L4 body', 'L4 head', 'L4 vieww', 'L4 viewq', 'L5 x', 'L5 y', 'L5 vx', 'L5 vy', 'L5 body', 'L5 head', 'L5 vieww', 'L5 viewq', 'L6 x', 'L6 y', 'L6 vx', 'L6 vy', 'L6 body', 'L6 head', 'L6 vieww', 'L6 viewq', 'L7 x', 'L7 y', 'L7 vx', 'L7 vy', 'L7 body', 'L7 head', 'L7 vieww', 'L7 viewq', 'L8 x', 'L8 y', 'L8 vx', 'L8 vy', 'L8 body', 'L8 head', 'L8 vieww', 'L8 viewq', 'L9 x', 'L9 y', 'L9 vx', 'L9 vy', 'L9 body', 'L9 head', 'L9 vieww', 'L9 viewq', 'L10 x', 'L10 y', 'L10 vx', 'L10 vy', 'L10 body', 'L10 head', 'L10 vieww', 'L10 viewq', 'L11 x', 'L11 y', 'L11 vx', 'L11 vy', 'L11 body', 'L11 head', 'L11 vieww', 'L11 viewq', 'RG1 x', 'RG1 y', 'RG1 vx', 'RG1 vy', 'RG1 body', 'RG1 head', 'RG1 vieww', 'RG1 viewq', 'R2 x', 'R2 y', 'R2 vx', 'R2 vy', 'R2 body', 'R2 head', 'R2 vieww', 'R2 viewq', 'R3 x', 'R3 y', 'R3 vx', 'R3 vy', 'R3 body', 'R3 head', 'R3 vieww', 'R3 viewq', 'R4 x', 'R4 y', 'R4 vx', 'R4 vy', 'R4 body', 'R4 head', 'R4 vieww', 'R4 viewq', 'R5 x', 'R5 y', 'R5 vx', 'R5 vy', 'R5 body', 'R5 head', 'R5 vieww', 'R5 viewq', 'R6 x', 'R6 y', 'R6 vx', 'R6 vy', 'R6 body', 'R6 head', 'R6 vieww', 'R6 viewq', 'R7 x', 'R7 y', 'R7 vx', 'R7 vy', 'R7 body', 'R7 head', 'R7 vieww', 'R7 viewq', 'R8 x', 'R8 y', 'R8 vx', 'R8 vy', 'R8 body', 'R8 head', 'R8 vieww', 'R8 viewq', 'R9 x', 'R9 y', 'R9 vx', 'R9 vy', 'R9 body', 'R9 head', 'R9 vieww', 'R9 viewq', 'R10 x', 'R10 y', 'R10 vx', 'R10 vy', 'R10 body', 'R10 head', 'R10 vieww', 'R10 viewq', 'R11 x', 'R11 y', 'R11 vx', 'R11 vy', 'R11 body', 'R11 head', 'R11 vieww', 'R11 viewq']

#print (data.columns.tolist())

#df = data.pivot(index=list(data.columns)[1],columns=list(data.columns)[2:4],values=list(data.columns)[5:] )

#print(df)

game = np.array(data.iloc[1:3,:]).reshape(-1,8)

#We'll take the last 4 columns of the first row and put them below the first 4 columns of the player's data
#game_xy = np.concatenate([np.concatenate([game[0,:4].reshape(-1,4),game[0,4:].reshape(-1,4)],axis=1), np.concatenate([np.tile(game[0,:4].reshape(-1,4),(22,1)),game[1:,:4]],axis=1)]) #have to reshape into a row vector


print(game)