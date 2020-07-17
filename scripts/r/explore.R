#set wd to where data is
setwd('C:/Users/David/OneDrive/Documents/Work/Thesis/github/data/groundtruth')

data = read.csv('../groundtruth.csv')

#general
library(dplyr) 

#plots
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(mapview)

#spatial
library(sf)
library(sp)
library(rgdal)

#format data as necessary

#choose number of frames to use
frames = nrow(data)

#intialize data frame
df = data.frame(frame = as.integer(),
                id = as.integer(),
                team = as.character(),
                x=as.numeric(),
                y=as.numeric(),
                vX=as.numeric(),
                vY=as.numeric())

#add data for each frame
for (i in 1:frames){
  temp = as.data.frame(matrix(data[i,], ncol=8, byrow=TRUE))
  temp[1,1:4] = temp[1,5:8]
  temp = temp[,1:4]
  temp = cbind(rep(i,23),seq(1,23),c('b',rep('h',11),rep('a',11)),temp)
  colnames(temp) =  c('frame','id',"team",'x','y','vX','vY')
  if (i > 1){
    df = rbind(df,temp)
  }
  else{
    df = temp
  }
}

#clean up
df$x<-as.numeric(df$x)
df$y<-as.numeric(df$y)
df$vX<-as.numeric(df$vX)
df$vY<-as.numeric(df$vY)
df$id<-as.factor(df$id)
df$team<-as.factor(df$team)
df$frame<-as.factor(df$frame)

#pitch variables
      lengthPitch = 107
      widthPitch = 70
      arrow = c("none", "r", "l")
      title = "Voronoi Test"
      subtitle = "Test"
      
      fill1 <- "#008000"
      fill2 <- "#328422"
      colPitch <- "grey85"
      arrowCol <- "white"
      colText <- "white"
      
      lwd <- 0.5
      border <- c(10, 6, 5, 6)

      # mowed grass lines
      lines <- (lengthPitch + border[2] + border[4]) / 13
      boxes <- data.frame(start = lines * 0:12 - border[4], end = lines * 1:13 - border[2])[seq(2, 12, 2),]

#df[,4] = df[,4]+53.5
#df[,5] = df[,5]+35

#box <- as.matrix(rbind(c(0,107),c(0, 70)))
#rownames(box) <- c("x","y")
#colnames(box) <- c("min","max")



plot_pos <- function(df, lengthPitch = 105, widthPitch = 68, fill1 = "red", 
                     col1 = NULL, fill2 = "blue", col2 = NULL, labelCol = "black", 
                     homeTeam = NULL, flipAwayTeam = TRUE, label = c("name", "number", "none"), 
                     labelBox = TRUE, shortNames = TRUE, nodeSize = 5, labelSize = 4, 
                     arrow = c("none", "r", "l"), theme = c("light", "dark", "grey", "grass"), 
                     title = NULL, subtitle = NULL, source = c("manual", "statsbomb"), 
                     x = "x", y = "y", id = "id", name = NULL, team = NULL) {
  
  # define colours by theme
  if(theme[1] == "grass") {
    colText <- "white"
  } else if(theme[1] == "light") {
    colText <- "black"
  } else if(theme[1] %in% c("grey", "gray")) {
    colText <- "black"
  } else {
    colText <- "white"
  }
  if(is.null(col1)) col1 <- fill1
  if(is.null(col2)) col2 <- fill2
  
  # plot
  p <- ##soccerPitch(theme = theme[1], title = title, subtitle = subtitle) +
    ggplot(data, aes( col = factor(team), fill = factor(team))) +
    #geom_polygon(data = df[1:23,], aes(x,y,group=team), fill=team, color=team) +
    #geom_path(data = df[1:23,],stat="voronoi", size=0.1, aes(x,y,color=team)) +
    #coord_quickmap() +
    # perimeter line
    #geom_rect(aes(x=NULL,y=NULL,xmin = 0, xmax = lengthPitch, ymin = 0, ymax = widthPitch), fill = NA, col = colPitch, lwd = lwd) +
    # centre circle
    #geom_circle(aes(x0 = lengthPitch/2, y0 = widthPitch/2, r = 9.15), col = colPitch, lwd = lwd) +
    # kick off spot
    #geom_circle(aes(x0 = lengthPitch/2, y0 = widthPitch/2, r = 0.25), fill = colPitch, col = colPitch, lwd = lwd) +
    # halfway line
    #geom_segment(aes(x = lengthPitch/2, y = 0, xend = lengthPitch/2, yend = widthPitch), col = colPitch, lwd = lwd) +
    # penalty arcs
    #geom_arc(aes(x0= 11, y0 = widthPitch/2, r = 9.15, start = pi/2 + 0.9259284, end = pi/2 - 0.9259284), col = colPitch, lwd = lwd) +
    #geom_arc(aes(x0 = lengthPitch - 11, y0 = widthPitch/2, r = 9.15, start = pi/2*3 - 0.9259284, end = pi/2*3 + 0.9259284), col = colPitch, lwd = lwd) +
    # penalty areas
    #geom_rect(aes(x=NULL,y=NULL,xmin = 0, xmax = 16.5, ymin = widthPitch/2 - 20.15, ymax = widthPitch/2 + 20.15), fill = NA, col = colPitch, lwd = lwd) +
    #geom_rect(aes(x=NULL,y=NULL,xmin = lengthPitch - 16.5, xmax = lengthPitch, ymin = widthPitch/2 - 20.15, ymax = widthPitch/2 + 20.15), fill = NA, col = colPitch, lwd = lwd) +
    # penalty spots
    #geom_circle(aes(x0 = 11, y0 = widthPitch/2, r = 0.25), fill = colPitch, col = colPitch, lwd = lwd) +
    #geom_circle(aes(x0 = lengthPitch - 11, y0 = widthPitch/2, r = 0.25), fill = colPitch, col = colPitch, lwd = lwd) +
    # six yard boxes
    #geom_rect(aes(x=NULL,y=NULL,xmin = 0, xmax = 5.5, ymin = (widthPitch/2) - 9.16, ymax = (widthPitch/2) + 9.16), fill = NA, col = colPitch, lwd = lwd) +
    #geom_rect(aes(x=NULL,y=NULL,xmin = lengthPitch - 5.5, xmax = lengthPitch, ymin = (widthPitch/2) - 9.16, ymax = (widthPitch/2) + 9.16), fill = NA, col = colPitch, lwd = lwd) +
    # goals
    #geom_rect(aes(x=NULL,y=NULL,xmin = -2, xmax = 0, ymin = (widthPitch/2) - 3.66, ymax = (widthPitch/2) + 3.66), fill = NA, col = colPitch, lwd = lwd) +
    #geom_rect(aes(x=NULL,y=NULL,xmin = lengthPitch, xmax = lengthPitch + 2, ymin = (widthPitch/2) - 3.66, ymax = (widthPitch/2) + 3.66), fill = NA, col = colPitch, lwd = lwd)+
    
    
    geom_point(data=df,aes(x,y),shape = 21, size = 6, stroke = 1.3) +
    transition_states(
      states = frame,
      transition_length = 0.01,
      state_length = 0,
      wrap = FALSE
    ) +
    scale_colour_manual(values = c("#355C7D", "#7d8a35","#F67280","#FFFFFF")) +
    scale_fill_manual(values = c("#355C7D", "#7d8a35","#F67280")) +
    guides(colour = FALSE, fill = FALSE)+
    coord_fixed()
    #theme_void(base_family="Roboto Condensed") 
  
  return(p)
  
}


p<-plot_pos(df,
            title = "test", 
            subtitle = "pos frame 1",
            team = "true",
            theme = c('grass'),
            homeTeam = 'h')
p
animate(p, fps = 13,nframes=frames)

ggplot(data = df,aes(x,y))+
geom_point(aes(fill = team, colour = team), shape = 21, size = 6, stroke = 1.3) +
  transition_states(
    states = frame,
    transition_length = 0.01,
    state_length = 0,
    wrap = FALSE
  )
#anim_save("single_frame", animation = last_animation())

#CTM

avPos <- function(param,df=ctm){
  minute = as.numeric(param[1])#
  range = as.numeric(param[2])
  #check if valid range minute combination
  if(minute-(range)<0 ||minute+(range)<0){
    return(NULL)
  }
  #initialize the count variables
  noH = 0
  noA = 0
  
  #for loop to check who has possession in all frames in the range
  for (i in 0:(range-1)){
    #first if statement checks in the period before the centre point
    if (df[minute+i]=="h"){#if home team has possesion i frames before minute
      noH = noH+1 #then increment home counter
    }
    else{#else increment away counter
      noA = noA+1
    }
    #second if statement checks in the period before the centre point
    if(i!=0){#dont double count midpoint
      if (df[minute-i]=="h"){#if home team has possesion i frames after minute
        noH = noH+1
      }
      else{
        noA = noA+1
      }
    }
    
  }
  if(noH>noA){
    return("h")
  }
  else{
    return("a")
  }
}


ctm_Plot<-function(team){
  ctm <- data.frame(cbind(team,seq(1,6329)))
  colnames(ctm) <- c("team","frame")
  sub <- cbind(seq(32,6331,by=62),rep(31,102))
  
  
  sum<-apply(FUN=avPos,MARGIN=1,X=as.matrix(sub),df=ctm$team)
  
  perm<-c()
  for (i in 1:50){
    perm <- c(perm,rep(i,times=i))
  }
  perm<-c(perm,rep(51,times=51),rep(52,times=51),103-rev(perm))
  
  len <- c()
  for (i in 1:50){
    len<- c(len,seq(1,i))
  }
  len<-c(len,seq(1,51),seq(1,51),rev(len))
  
  temp <- data.frame(cbind(perm,len))
  ctmRes<-apply(FUN=avPos,MARGIN=1,X=temp,df=sum)
  temp <- data.frame(cbind(perm,len,ctmRes))
  colnames(temp)<-c("time","range","team")
  temp$team <- as.factor(temp$team)
  temp$time <- as.numeric(temp$time)
  temp$range <- as.numeric(temp$range)
  
  return(ggplot(data = temp,aes(x=time,y=range,color=team))+
    geom_point())
}

#for loop to determine possession at given frame
team <- c()
for(i in 1:frames){
  temp <- subset(df, frame==i)
  distances<-as.matrix(dist(as.matrix(temp[,4:5]),method="euclidean"))[,1]
  index <- which.min(distances[2:23])+1
  team <- c(team,temp[index,3])
}

p1 <- ctm_Plot(team)

#determine centroid based territorial advanage at given frame
team1 <- c()
for(i in 1:frames){
  
  #remove ball and goalkeepers
    temp <- subset(df, frame==i)[-c(1,2,13),]
  
  #find centroid of home team and distance from centroid to right hand side of pitch
    aveHomeX <- mean(as.numeric(as.matrix(subset(temp,team=="h")[,4])))
    distHome<-53.5-aveHomeX
    #aveHome <- c(aveHomeX,mean(as.numeric(as.matrix(subset(temp,team=="h")[,5]))))
  
  #find centroid of away team and distance from centroid to left hand side of pitch
    aveAwayX <- mean(as.numeric(as.matrix(subset(temp,team=="a")[,4])))
    distAway<-53.5-aveAwayX
    #aveAway <- c(aveAwayX,mean(as.numeric(as.matrix(subset(temp,team=="a")[,5]))))
  
  #if home dist > away dist, then away advantage
    if (distHome > distAway){
      team1 <- c(team1,"a")
    }
    else{
      team1 <- c(team1,"h")
    }
}

p2 <- ctm_Plot(team1)
p2

