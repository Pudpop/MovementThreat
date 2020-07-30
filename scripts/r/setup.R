#general
  library(tidyverse)
  library(dplyr)
  library(foreach) #parallel
  library(doParallel) #parallel
  library(MASS) # kde

#plots
  library(ggplot2)
  library(ggforce)
  library(gganimate)
  library(RColorBrewer)
  library(ggpubr)
  library(wesanderson)
  library(extrafont)

#spatial
  library(sf)
  library(sp)
  library(rgdal)

pal <- c("#A6CEE3","#1F78B4","#b2df8a","#33a02c",
         "#fb9a99","#fb9a99","#fdbf6f","#ff7f00",
         "#cab2d6","#6a3d9a","#ffff99")

reader <- function(path = 'C:/Users/David/OneDrive/Documents/Work/Thesis/Code/data',
                  filename = 'ten_games_formatted.csv'){

    #format data as necessary
        #set wd to where data is
        setwd(path)

        df = read.csv(filename)
        df = df[,-c(12)]
        df = df[,c(1,12,seq(2,11))]
        #df <- cbind(cumsum(!duplicated(df[5:6])),df)
        colnames(df) = c("matchID","player","frame","state","score_left","score_right","left_team","right_team",
                           "x","y","vX","vY")

        #clean up
        df$x<-as.numeric(df$x)+53.5
        df$y<-as.numeric(df$y)+35
        df$vX<-as.numeric(df$vX)
        df$vY<-as.numeric(df$vY)
        df$speed <- sqrt((df$vX)^2+(df$vY)^2)
        df$state <- as.factor(df$state)
        #df$speedGroup <- as.numeric(cut(df$speed, c(0,0.03,0.073,0.13,0.24,0.48,0.6),include.lowest = T))
        df$speedGroup <- as.numeric(cut(df$speed, c(0,seq(0.001,0.55,by=0.05),10),include.lowest = T))
        df$time <- as.numeric(df$frame)/10
        df$team <- cut(df$player,c(0,1,12,23),include.lowest = T)
        levels(df$team) <- c("b","l","r")
        
        return(df)
}

fte_theme <- function(font = c("serif","serif"), pal = brewer.pal("Greys",n=9),sizes = c(8,12,14)){

  # Generate the colors for the chart procedurally with RColorBrewer
  color.background = pal[1]
  color.grid.major = pal[3]
  color.axis.text = pal[6]
  color.axis.title = pal[7]
  color.title = pal[9]

  # Begin construction of chart

  theme_bw(base_size=9) +

    # Set the entire chart region to a light gray color

    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +

    # Format the grid

    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +

    # Format the legend, but hide by defaut

    #theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title, family=font[[1]])) +

    # Set title and axis labels, and format these and tick marks

    theme(plot.title=element_text(color=color.title, size=sizes[[2]], vjust=1.25, family=font[[2]])) +
    theme(plot.subtitle=element_text(color=color.title, size=sizes[[3]], vjust=1.25, family=font[[1]])) +
    theme(legend.title =element_text(color=color.title, size=sizes[[2]], vjust=1.25, family=font[[1]])) +
    theme(axis.text.x=element_text(size=sizes[[1]],color=color.axis.text, family=font[[1]])) +
    theme(axis.text.y=element_text(size=sizes[[1]],color=color.axis.text, family=font[[1]])) +
    theme(axis.title.x=element_text(size=sizes[[1]],color=color.axis.title, vjust=0, family=font[[1]])) +
    theme(axis.title.y=element_text(size=sizes[[1]],color=color.axis.title, vjust=1.25, family=font[[1]])) +

    # Plot margins

    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

soccerPitch <- function(scale=c(-200,-180,10,-200,-180,10),linecolor = "grey85"){
  green = "#FFFFFF"
  lengthPitch = 107
  widthPitch = 70
  arrow = c("none", "r", "l")
  title = "Voronoi Test"
  subtitle = "Test"

  fill1 <- "#008000"
  fill2 <- "#328422"
  colPitch <- linecolor
  arrowCol <- "white"
  colText <- "white"

  lwd <- 0.5
  border <- c(10, 6, 5, 6)

  # mowed grass lines
  lines <- (lengthPitch + border[2] + border[4]) / 13
  boxes <- data.frame(start = lines * 0:12 - border[4], end = lines * 1:13 - border[2])[seq(2, 12, 2),]
  ggplot()+
    fte_theme(font = c("Segoe UI Light","Segoe UI"))+
    theme(panel.background=element_rect(fill=green, color=green)) +
    theme(plot.background=element_rect(fill=green, color=green)) +
    theme(panel.border=element_rect(color=green)) +
      scale_x_continuous(breaks = seq(scale[1],scale[2],by=scale[3])) +
      scale_y_continuous(breaks = seq(scale[4],scale[5],by=scale[6])) +
    #perimeter
    geom_rect(aes(xmax=107,xmin=0,ymax=70,ymin=0),fill=NA,color=linecolor)+
    # centre circle
    geom_circle(aes(x0 = lengthPitch/2, y0 = widthPitch/2, r = 9.15), col = colPitch, lwd = lwd) +
    # kick off spot
    geom_circle(aes(x0 = lengthPitch/2, y0 = widthPitch/2, r = 0.25), fill = colPitch, col = colPitch, lwd = lwd) +
    # halfway line
    geom_segment(aes(x = lengthPitch/2, y = 0, xend = lengthPitch/2, yend = widthPitch), col = colPitch, lwd = lwd) +
    # penalty arcs
    geom_arc(aes(x0= 11, y0 = widthPitch/2, r = 9.15, start = pi/2 + 0.9259284, end = pi/2 - 0.9259284), col = colPitch, lwd = lwd) +
    geom_arc(aes(x0 = lengthPitch - 11, y0 = widthPitch/2, r = 9.15, start = pi/2*3 - 0.9259284, end = pi/2*3 + 0.9259284), col = colPitch, lwd = lwd) +
    # penalty areas
    geom_rect(aes(x=NULL,y=NULL,xmin = 0, xmax = 16.5, ymin = widthPitch/2 - 20.15, ymax = widthPitch/2 + 20.15), fill = NA, col = colPitch, lwd = lwd) +
    geom_rect(aes(x=NULL,y=NULL,xmin = lengthPitch - 16.5, xmax = lengthPitch, ymin = widthPitch/2 - 20.15, ymax = widthPitch/2 + 20.15), fill = NA, col = colPitch, lwd = lwd) +
    # penalty spots
    geom_circle(aes(x0 = 11, y0 = widthPitch/2, r = 0.25), fill = colPitch, col = colPitch, lwd = lwd) +
    geom_circle(aes(x0 = lengthPitch - 11, y0 = widthPitch/2, r = 0.25), fill = colPitch, col = colPitch, lwd = lwd) +
    # six yard boxes
    geom_rect(aes(x=NULL,y=NULL,xmin = 0, xmax = 5.5, ymin = (widthPitch/2) - 9.16, ymax = (widthPitch/2) + 9.16), fill = NA, col = colPitch, lwd = lwd) +
    geom_rect(aes(x=NULL,y=NULL,xmin = lengthPitch - 5.5, xmax = lengthPitch, ymin = (widthPitch/2) - 9.16, ymax = (widthPitch/2) + 9.16), fill = NA, col = colPitch, lwd = lwd) +
    # goals
    geom_rect(aes(x=NULL,y=NULL,xmin = -2, xmax = 0, ymin = (widthPitch/2) - 3.66, ymax = (widthPitch/2) + 3.66), fill = NA, col = colPitch, lwd = lwd) +
    geom_rect(aes(x=NULL,y=NULL,xmin = lengthPitch, xmax = lengthPitch + 2, ymin = (widthPitch/2) - 3.66, ymax = (widthPitch/2) + 3.66), fill = NA, col = colPitch, lwd = lwd)
}
