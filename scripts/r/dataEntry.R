library(ggplot2)
library(gganimate)
library(transformr)
library(ggvoronoi)
library(ggforce)
library(gifski)
library(ggrepel)
library(deldir)
library(rgdal)
library(soccermatics)
library(kableExtra)
library(dplyr)    # alternatively, this also loads %>%

setwd('C:/Users/David/OneDrive/Documents/Work/Thesis/Code/Data')
data = read.csv('groundtruth.csv')

kable_styling(kableExtra::kable(head(data)) ,
              full_width = F,
              latex_options = c("striped","hold_position"))

frames = 100

df = data.frame(frame = as.integer(),
                id = as.integer(),
                team = as.character(),
                x=as.numeric(),
                y=as.numeric(),
                vX=as.numeric(),
                vY=as.numeric())
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

df$x= as.numeric(df$x)
df$y= as.numeric(df$y)
df$vX= as.numeric(df$vX)
df$vY= as.numeric(df$vY)

df[,4] = df[,4]+53.5
df[,5] = df[,5]+35

box <- as.matrix(rbind(c(0,107),c(0, 70)))
rownames(box) <- c("x","y")
colnames(box) <- c("min","max")


SPointsDF_to_voronoi_SPolysDF <- function(sp) {
  
  # tile.list extracts the polygon data from the deldir computation
  vor_desc <- tile.list(deldir(sp@coords[,1], sp@coords[,2], rw = box))
  
  lapply(1:(length(vor_desc)), function(i) {
    
    # tile.list gets us the points for the polygons but we
    # still have to close them, hence the need for the rbind
    tmp <- cbind(vor_desc[[i]]$x, vor_desc[[i]]$y)
    tmp <- rbind(tmp, tmp[1,])
    
    # now we can make the Polygon(s)
    Polygons(list(Polygon(tmp)), ID=i)
    
  }) -> vor_polygons
  
  # hopefully the caller passed in good metadata!
  sp_dat <- sp@data
  
  # this way the IDs _should_ match up w/the data & voronoi polys
  rownames(sp_dat) <- sapply(slot(SpatialPolygons(vor_polygons),
                                  'polygons'),
                             slot, 'ID')
  
  SpatialPolygonsDataFrame(SpatialPolygons(vor_polygons),
                           data=sp_dat)
  
}

temp = subset(df,frame==1)
#temp$x= as.numeric(temp$x)
#temp$y= as.numeric(temp$y)
#temp$vX= as.numeric(temp$vX)
#temp$vY= as.numeric(temp$vY)


vor_pts <- SpatialPointsDataFrame(cbind(temp$x,temp$y),temp[,c(2,4,5)], match.ID=TRUE)

vor <- SPointsDF_to_voronoi_SPolysDF(vor_pts)

vor <- fortify(vor)

vor <- cbind(vor,rep(1,times=nrow(vor)))
colnames(vor) = c("long","lat","order","hole","piece","id","group","frame")

#vor_pts <- SpatialPointsDataFrame(cbind(df$x,df$y),df[,c(2,4,5)], match.ID=TRUE, bbox = box)
for (i in 2:frames){
  temp = subset(df,frame==i)
  vor_pts <- SpatialPointsDataFrame(cbind(temp$x,temp$y),temp[,c(2,4,5)], match.ID=TRUE)
  
  vor_df <- SPointsDF_to_voronoi_SPolysDF(vor_pts)
  
  vor_df <- fortify(vor_df)
  
  vor_df <- cbind(vor_df,rep(i,times=nrow(vor_df)))
  vor_df
  colnames(vor_df) = c("long","lat","order","hole","piece","id","group","frame")  
  vor = rbind(vor,vor_df)
}

vor = cbind(vor,rep("h",times = nrow(vor)))
colnames(vor) = c("long","lat","order","hole","piece","id","group","frame","team")  
vor$team = as.character(vor$team)
for (i in 1:nrow(vor)){
  if (as.numeric(vor[i,]$id) > 12){
    vor[i,]$team = "a"
  }
    
  if (vor[i,]$id == "1"){
    vor[i,]$team = "b"
  }
    
}
vor$team = as.factor(vor$team)

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
    ggplot(vor, aes(x=long,y=lat, col = factor(team), fill = factor(team))) +
    #geom_polygon(data = df[1:23,], aes(x,y,group=team), fill=team, color=team) +
    #geom_path(data = df[1:23,],stat="voronoi", size=0.1, aes(x,y,color=team)) +
    #coord_quickmap() +
    # perimeter line
    geom_rect(aes(x=NULL,y=NULL,xmin = 0, xmax = lengthPitch, ymin = 0, ymax = widthPitch), fill = NA, col = colPitch, lwd = lwd) +
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
    geom_rect(aes(x=NULL,y=NULL,xmin = lengthPitch, xmax = lengthPitch + 2, ymin = (widthPitch/2) - 3.66, ymax = (widthPitch/2) + 3.66), fill = NA, col = colPitch, lwd = lwd)+
    
    
  geom_point(data = df,aes(x,y, fill = team, colour = team), shape = 21, size = 6, stroke = 1.3) +
      #geom_map(data = vor, map = vor, aes(map_id = id),
      #       color = "#a5a5a5", fill = "#FFFFFF00", size = 0.25) +
    geom_polygon(data = subset(vor, id == "1"), size = 0.25, alpha = 0.3)+
    geom_polygon(data = subset(vor, id == "2"), size = 0.25, alpha = 0.3)+
    geom_polygon(data = subset(vor, id == "3"), size = 0.25, alpha = 0.3)+
    geom_polygon(data = subset(vor, id == "4"), size = 0.25, alpha = 0.3)+
    geom_polygon(data = subset(vor, id == "5"  ), size = 0.25, alpha = 0.3)+
    geom_polygon(data = subset(vor, id == "6"), size = 0.25, alpha = 0.3)+
    geom_polygon(data = subset(vor, id == "7"), size = 0.25, alpha = 0.3)+
    geom_polygon(data = subset(vor, id == "8"), size = 0.25, alpha = 0.3)+
    geom_polygon(data = subset(vor, id == "9"), size = 0.25, alpha = 0.3)+
    geom_polygon(data = subset(vor, id == "10"), size = 0.25, alpha = 0.3)+
    geom_polygon(data = subset(vor, id == "11"), size = 0.25, alpha = 0.3)+
    geom_polygon(data = subset(vor, id == "12"), size = 0.25, alpha = 0.3)+
    geom_polygon(data = subset(vor, id == "13"), size = 0.25, alpha = 0.3)+
    geom_polygon(data = subset(vor, id == "14"), size = 0.25, alpha = 0.3)+
    geom_polygon(data = subset(vor, id == "15"), size = 0.25, alpha = 0.3)+
    geom_polygon(data = subset(vor, id == "16"), size = 0.25, alpha = 0.3)+
    geom_polygon(data = subset(vor, id == "17"), size = 0.25, alpha = 0.3)+
    geom_polygon(data = subset(vor, id == "18"), size = 0.25, alpha = 0.3)+
    geom_polygon(data = subset(vor, id == "19"), size = 0.25, alpha = 0.3)+
    geom_polygon(data = subset(vor, id == "20"), size = 0.25, alpha = 0.3)+
    geom_polygon(data = subset(vor, id == "21"), size = 0.25, alpha = 0.3)+
    geom_polygon(data = subset(vor, id == "22"), size = 0.25, alpha = 0.3)+
    geom_polygon(data = subset(vor, id == "23"), size = 0.25, alpha = 0.3)+
      #transition_states(
      #  states = frame,
      #  transition_length = 0.01,
      #  state_length = 0,
      #  wrap = FALSE
      #) +
      scale_colour_manual(values = c("#355C7D", "#7d8a35","#F67280","#FFFFFF")) +
      scale_fill_manual(values = c("#355C7D", "#7d8a35","#F67280")) +
      guides(colour = FALSE, fill = FALSE)+
      theme_void(base_family="Roboto Condensed") 
  
  return(p)
  
}


p<-plot_pos(df,
         title = "test", 
         subtitle = "pos frame 1",
         team = "true",
         theme = c('grass'),
         homeTeam = 'h')
p
#animate(p, fps = 13, nframes = 10)
#anim_save("single_frame", animation = last_animation())
