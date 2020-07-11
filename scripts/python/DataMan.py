import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
sns.set_style('dark')

data = pd.read_csv('Data/groundtruth.csv')

#So let's some frame
frame_nr = 100

#and reshape the data that each player's variables are in one row
frame = np.array(data.iloc[frame_nr,:]).reshape(-1,8)

#We'll take the last 4 columns of the first row and put them below the first 4 columns of the player's data
frame_xy = np.concatenate([frame[0,4:].reshape(-1,4), frame[1:,:4]], axis = 0) #have to reshape into a row vector

def drawfield(ax):
    '''
    Draws the pitch on an axes object
    '''
    plt.sca(ax)
    plt.axis([-53.5,53.5,-35,35])

    linecolor = 'w'

    circle = plt.Circle((0, 0), radius=9.15, fc='None', color = linecolor, lw=1)
    plt.gca().add_patch(circle)

    fiveyard1 = plt.Rectangle((-52.5, -9.12), 5.5, 18.32, fc='None', color = linecolor, lw = 1)
    plt.gca().add_patch(fiveyard1)

    fiveyard2 = plt.Rectangle((52.5-5.5, -9.12), 5.5, 18.32, fc='None', color = linecolor, lw =1)
    plt.gca().add_patch(fiveyard2)

    box1 = plt.Rectangle((-52.5, -20.12), 16.5, 40.24, fc='None', color = linecolor, lw = 1)
    plt.gca().add_patch(box1)

    box2 = plt.Rectangle((52.5-16.5, -20.12), 16.5, 40.24, fc='None', color = linecolor, lw =1)
    plt.gca().add_patch(box2)

    midline = plt.Line2D((0,0),(34,-34), c = linecolor, lw = 1)
    plt.gca().add_line(midline)

    leftgoalline = plt.Line2D((-52.5,-52.5),(34,-34), c = linecolor, lw = 1)
    plt.gca().add_line(leftgoalline)

    rightgoalline = plt.Line2D((52.5,52.5),(34,-34), c = linecolor, lw = 1)
    plt.gca().add_line(rightgoalline)

    topfieldline = plt.Line2D((-52.5,52.5),(34,34), c = linecolor, lw = 1)
    plt.gca().add_line(topfieldline)

    bottomfieldline = plt.Line2D((-52.5,52.5),(-34,-34), c = linecolor, lw = 1)
    plt.gca().add_line(bottomfieldline)

    goal1 = plt.Line2D((-52.5,-52.5),(3.66,-3.66), c='k', lw = 1)
    plt.gca().add_line(goal1)

    goal2 = plt.Line2D((52.5,52.5),(3.66,-3.66), c='k', lw = 1)
    plt.gca().add_line(goal2)

    penalty1 = plt.Circle((-41.5,0), radius = 0.6, fc = linecolor, color=linecolor)
    plt.gca().add_patch(penalty1)

    penalty2 = plt.Circle((41.5,0), radius = 0.6, fc = linecolor, color=linecolor)
    plt.gca().add_patch(penalty2)

    plt.tick_params(
        axis='both',          # changes apply to the x-axis
        which='both',      # both major and minor ticks are affected
        bottom='off',      # ticks along the bottom edge are off
        top='off',         # ticks along the top edge are off
        left='off',
        labelleft='off',
        labelbottom='off')

#define an array for the colors [Ball, 11*TeamL, 11*TeamR]
colors = ['k'] + 11*['r'] + 11*['SkyBlue']

def get_frame_xy_data(frame_nr):
    '''
    returns the xy-coordinates and xy-velocities for the 22 players and the ball
    '''
    frame = np.array(data.iloc[frame_nr,:]).reshape(-1,8) #reshape each player's variables into one row
    return np.concatenate([frame[0,4:].reshape(-1,4), frame[1:,:4]], axis = 0) #have to reshape into one row vector


def plot_frame(fig, ax, frame):
    '''
    plots the x- and y-coordinates of players and the ball
    '''
    #Clear the axes to get rid of the old objects
    plt.cla()

    #get the positional data for this frame
    frame_xy = get_frame_xy_data(frame)
    #draw the pitch, players and the ball
    drawfield(ax)
    sc = plt.scatter(frame_xy[:,0],
               frame_xy[:,1],
               c = 'w',
               edgecolors = colors,
               linewidth = 1.5)

fig, ax = plt.subplots(figsize=(8,6))



#Call our function to plot the frame
plot_frame(fig, ax, frame = 200)
plt.show()
print(test)
