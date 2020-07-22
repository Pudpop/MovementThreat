#set wd to where data is
setwd('C:/Users/David/OneDrive/Documents/Work/Thesis/Code/Data')

data = read.csv('groundtruth.csv')

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
    frames = 1

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
    df$speed <- sqrt(df$vX^2+df$vY^2)
    df$speedGroup <- as.factor(as.numeric(cut(df$speed, seq(0,3,by=0.1),include.lowest = T)))
    df$time <- (60/100)*as.numeric(df$frame)

#angles using for
angles <- as.matrix(seq(from = 0, to = 360, by = 6))

#taki's movement model function. Needs to be reweighted for velocities
taki <- function(angle = 0,origin = c(0,0),v = c(0,0),t = 1,a = 4.2){
  return(c(origin[1]+(0.5*a*cos(angle)*t^2+v[1]*t),origin[2]+(0.5*a*sin(angle)*t^2+v[2]*t)))
}


#fujimura, needs to be reweighted
fujsug <- function(angle = 0,origin = c(0,0),v = c(0,0),t = 1,alpha = 1.3,vM = 2.948,scale = 1){
  v[1] = scale * v[1]
  v[2] = scale * v[2]
  newX <- origin[1]+(vM*cos(angle)*(t-(1-exp(alpha*t))/(alpha))+v[1]*(1-exp(alpha*t))/(alpha))
  if (newX > 53.5){
    newX = 53.5
  }
  else if (newX < -53.5){
    newX = -53.5
  }
  newY <- origin[2]+vM*sin(angle)*(t-(1-exp(alpha*t))/(alpha))+v[2]*(1-exp(alpha*t))/(alpha)
  if (newY > 35){
    newY = 35
  }
  else if (newY < -35){
    newY = -35
  }
  return(c(newX,newY))
}

#reachable polygonal regions. Applies movement functions to every angle
reach <- function(param,FUNC = taki){
  return(data.frame(t(apply(FUN= FUNC,MARGIN=1,X=angles,
                            origin = c(param[1],param[2]),
                            v = c(param[3],param[4]),
                            t = param[5]))))
}

#list of times to be used
    #temp <- as.matrix(subset(rr,id=="18378")[,2:3])
    #times <- c(0.05,0.1,0.15,0.2,0.25,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.2,1.4,1.6,1.8,2.0,
    #           2.4,2.8,3.2,3.6,4.0,4.5,5,6,7,8,9,10)
    #times <- seq(0.01,0.5,by=0.01)
    #times <- c(0.01,0.05,0.1,0.2,0.4,0,7,1,1.5,2,2.5,3,4,5,7,10,12)
    #times <- c(seq(0.01,1.5,by = 0.01),seq(0.2,0.48,by=0.02),seq(0.5,2,by=0.3),seq(2.4,6,by=0.4))
    times <- c(seq(0.01,2,by = 0.02))
    #times <- seq(0.5,10,by=0.5)
    #times <- c(0.1,0.2,0.3,0.4,0.5,1,2,3)

#initialize list of partial Reachable regions
partialRR <- c()
#for loop for every time. For each time, calulate the partial reachable region for each player
for (l in  1:length(times)){

  #use current time
  time = times[[l]]

  #apply reachable region function to all players
  rr<-apply(FUN=reach,MARGIN=1,X=cbind(df[1:23,4:7],rep(time,nrow(df[1:23,]))),FUNC = fujsug)

  #format column names for bind_rows
  for ( i in 1:length(rr)){
    colnames(rr[[i]]) <- c("x","y")
  }
  #create one data frame
  rr<-bind_rows(rr,.id='id')

  #convert to sf object
  df.sf <- rr %>%
    st_as_sf( coords = c( "x", "y" ) )

  #find convex hulls of each region
  hulls <- df.sf %>%
    group_by( id ) %>%
    summarise( geometry = st_combine( geometry ) ) %>%
    st_convex_hull()

  #list of all intersections between polygons in sf object hulls
  inter<-st_intersects(hulls)

  #initial list of two-way intersections
  tw_inter <- data.frame(x=c(),y=c())

  # find all pairwise intersections
  for (i in 1:length(inter)){
    for (j in 1:length(inter[[i]])){
      temp <- data.frame(cbind(i,inter[[i]][j]))
      rev <- data.frame(cbind(inter[[i]][j],i))
      colnames(rev) <- c("i","V1")
      bool <- nrow(merge(tw_inter, rev)) == 0
      if (bool && temp$i[1]!=temp$V2[1]){
        tw_inter <- rbind(tw_inter,temp)
      }
    }
  }

  #find partial regions
  temp <- hulls
  if (nrow(tw_inter)>0){
    for (k in 1:nrow(tw_inter)){
      diff1<- st_difference(temp$geometry[[tw_inter$i[[k]]]],hulls$geometry[[tw_inter$V2[[k]]]])
      diff2 <- st_difference(temp$geometry[[tw_inter$V2[[k]]]],hulls$geometry[[tw_inter$i[[k]]]])
      temp$geometry[[tw_inter$i[[k]]]] <- diff1
      temp$geometry[[tw_inter$V2[[k]]]] <- diff2
    }
  }
  partialRR <- c(partialRR,temp)

}

#step 4 of algorithm: synthesize the partial dominant regions
synthesize <- function(index,parts=partialRR){
  th <- parts[[2]][[index]]
  if (class(th)[[2]]=="MULTIPOLYGON"){
    th <- st_make_valid(parts[[2]][[index]]) %>%
            st_cast("MULTIPOLYGON") %>%
            st_cast("POLYGON")
  }
  poly <- th
  sum <- th
  for (i in 2:length(times)){
    temp <- 2*i
    th <- parts[[temp]][[index]]
    if (class(th)[[2]]=="MULTIPOLYGON"){
      th <- st_make_valid(parts[[temp]][[index]])
      huh<-data.frame(cbind(rep(1,nrow(th[[1]][[1]])),
                            th[[1]][[1]]))
      colnames(huh)<-c("id" ,"x" , "y" )
      huh<-st_as_sf(huh,coords=c("x","y")) %>%
        summarise( geometry = st_combine( geometry ) ) %>%
        st_cast("POLYGON")
      p <- st_make_valid(st_difference(huh,sum))
      for (k in 2:length(th)){
        huh<-data.frame(cbind(rep(1,nrow(th[[1]][[1]])),
                              th[[1]][[1]]))
        colnames(huh)<-c("id" ,"x" , "y" )
        huh<-st_as_sf(huh,coords=c("x","y")) %>%
          summarise( geometry = st_combine( geometry ) ) %>%
          st_cast("POLYGON")
        huhp <- st_make_valid(st_difference(huh,sum))
        p<-st_make_valid(st_union(p,huhp))
      }
    }
    else{
      p <- st_make_valid(st_difference(th,sum))
    }

    if (class(p)[[2]]!="GEOMETRYCOLLECTION"){

      poly <- st_union(p,poly)
      sum <- st_union(sum,th)
    }

  }
  return(poly)
}
combined<-synthesize(partialRR,2)
#temp<-st_cast(,"POLYGON")[[1]]
#combined <- cbind(as.data.frame(temp),rep(1,length(temp)))
#colnames(combined) <- c("x","y","id")

for (i in 2:23){
  #temp<-st_cast(synthesize(partialRR,i),"POLYGON")[[1]]
  #combinedT <- cbind(as.data.frame(temp),rep(i,length(temp)))
  #colnames(combinedT) <- c("x","y","id")
  #combined <- rbind(combined,combinedT)
  combined <- c(combined,synthesize(partialRR,i))
}



combined <- combined %>%
  st_as_sf( coords = c( "x", "y" ) )
hulls <- combined %>%
  group_by( id ) %>%
  summarise( geometry = st_combine( geometry ) ) %>%
  st_convex_hull()

ggplot(hulls,aes(fill=as.factor(id))) +
#  fte_theme()+
  geom_sf() +
  geom_point(data = df,aes(x=x,y=y),position = 'jitter')

#,aes(fill=as.factor(id))
ggplot(st_sf(partialRR[[8]])) +
  #  fte_theme()+
  geom_sf() +
  geom_point(data = df,aes(x=x,y=y),position = 'jitter')

ggplot(combined,aes(fill=as.factor(id))) +
  #  fte_theme()+
  geom_sf(alpha=0.5) +
  geom_point(data = df,aes(x=x,y=y),position = 'jitter')


time = 1.4

rr<-apply(FUN=reach,MARGIN=1,X=cbind(df[,4:7],rep(time,nrow(df))),FUNC = fujsug)

#format column names for bind_rows
for ( i in 1:length(rr)){
  colnames(rr[[i]]) <- c("x","y")
}
#create one data frame
rr<-bind_rows(rr,.id='id')

#convert to sf object
df.sf <- rr %>%
  st_as_sf( coords = c( "x", "y" ) )

#find convex hulls of each region
hulls <- df.sf %>%
  group_by( id ) %>%
  summarise( geometry = st_combine( geometry ) ) %>%
  st_convex_hull()

plot(hulls)

##Brefeld

##transform the datapoint to the origin version

transform <- function(ps,pt,pu){
  r = sqrt((pt[1]-pu[1])^2+(pt[2]-pu[2])^2)
  s = sqrt((pt[1]-ps[1])^2+(pt[2]-ps[2])^2)
  theta = atan2(pu[2]-pt[2],pu[1]-pt[1])-atan2(pt[2]-ps[2],pt[1]-ps[1])
  return(rbind(c(-s,0),c(r*cos(theta),r*sin(theta))))
}

transform(c(1,1),c(2,1),c(4,3))

ggplot(df,aes(x=speed))+
  geom_histogram(binwidth=0.1)



probP <- function(tD,p,pt,v){
  
}