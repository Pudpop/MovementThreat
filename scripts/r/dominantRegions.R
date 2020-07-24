#general
  library(dplyr)

#plots
  library(ggplot2)
  library(ggforce)
  library(gganimate)
  library(RColorBrewer)
  library(ggpubr)
  library(mapview)
  library(wesanderson)

#spatial
  library(sf)
  library(sp)
  library(rgdal)
  library(smoothr)

pal <- wes_palette("Zissou1", 5, type = "discrete")

#format data as necessary
    #set wd to where data is
    setwd('C:/Users/David/OneDrive/Documents/Work/Thesis/Code/data')

    df= read.csv('ten_games_formatted.csv')
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
    df$speedGroup <- as.numeric(cut(df$speed, c(seq(0,0.1,by=0.01),seq(0.15,0.6,by=0.05),10),include.lowest = T))
    df$time <- as.numeric(df$frame)/10
    df$team <- cut(df$player,c(0,1,12,23),include.lowest = T)
    levels(df$team) <- c("b","l","r")

#angles using for
angles <- as.matrix(seq(from = -pi, to = pi, by = pi/24))

#taki's movement model function. Needs to be reweighted for velocities
taki <- function(angle = 0,origin = c(0,0),v = c(0,0),t = 1,a = 4.2*8.2/0.6,scale=8.2/0.6){
  v[1] = scale * v[1]
  v[2] = scale * v[2]
  newX <- origin[1]+(0.5*a*cos(angle)*t^2+v[1]*t)
  if (newX > 107){
    newX = 107
  }
  else if (newX < 0){
    newX = 0
  }
  newY <- origin[2]+(0.5*a*sin(angle)*t^2+v[2]*t)
  if (newY > 70){
    newY = 70
  }
  else if (newY < 0){
    newY = 0
  }
  return(c(newX,newY))

}


#fujimura, needs to be reweighted
fujsug <- function(angle = 0,origin = c(53.5,35),v = c(0,0),t = 1,alpha = 1.3,vM = 8.2,scale = 8.2/0.6){
  v[1] = scale * v[1]
  v[2] = scale * v[2]
  newX <- origin[1]+(vM*cos(angle)*(t-(1-exp(alpha*t))/(alpha))+v[1]*(1-exp(-alpha*t))/(alpha))
  if (newX > 107){
    newX = 107
  }
  else if (newX < 0){
    newX = 0
  }
  newY <- origin[2]+vM*sin(angle)*(t-(1-exp(alpha*t))/(alpha))+v[2]*(1-exp(-alpha*t))/(alpha)
  if (newY > 70){
    newY = 70
  }
  else if (newY < 0){
    newY = 0
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

player <- 15
time = 0.15
par = c(one_frame[player,]$x,one_frame[player,]$y,one_frame[player,]$vX,one_frame[player,]$vY,time)
rr<-reach(par,fujsug)
rr<-reach(c(53.5,35,0.1,0.1,0.5),fujsug)
soccerPitch()+
  fte_theme()+
  coord_fixed()+
  geom_point(data=rr,mapping=aes(X1,X2))+
  geom_text(data=one_frame,mapping=aes(x=x,y=y,label=player))+
  geom_segment(data = subset(match,frame==100),
               mapping=aes(x=x,xend = x+vX*scale,yend=y+vY*scale,y=y,color=as.factor(team)),
               arrow = arrow(length = unit(0.1, "cm")),
               show.legend = F)

#step 4 of algorithm: synthesize the partial dominant regions
synthesize <- function(index,parts=partialRR){
  precision = 10000000
  th <- (parts[[2]][[index]] %>% st_sfc() %>% st_set_precision(1000000))[[1]]
  if (class(th)[[2]]=="MULTIPOLYGON"){
    th <- st_make_valid(parts[[2]][[index]]) %>%
      st_cast("MULTIPOLYGON") %>%
      st_cast("POLYGON")
  }
  #else if (class(th)[[2]]=="GEOMETRYCOLLECTION"){
  #  th <- st_make_valid(parts[[2]][[index]]) %>%
  #    st_cast("POLYGON")
  #}
  else if (class(th)[[2]]=="LINESTRING"){
    th <- st_make_valid(parts[[2]][[index]]) %>%
      st_cast("POLYGON")
  }
  poly <- th
  sum <- th
  for (i in 2:length(times)){
    temp <- 2*i
    th <- (parts[[temp]][[index]] %>% st_sfc() %>% st_set_precision(precision))[[1]]
    if (class(th)[[2]]=="MULTIPOLYGON"){
      th <- (parts[[temp]][[index]] %>% st_sfc() %>% st_set_precision(precision) %>% st_make_valid())[[1]]
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
      p <- (st_sfc(th) %>% st_set_precision(precision) %>% st_make_valid())[[1]]
      p <- (p %>% st_difference(sum) %>% st_sfc() %>% st_set_precision(precision) %>% st_make_valid())[[1]]
    }

    if (class(p)[[2]]!="GEOMETRYCOLLECTION" && class(p)[[2]]!="data.frame"){
      ttemp <- poly
      poly <- st_union(p,poly)
      if (!(class(poly)[[2]] %in% c("POLYGON","MULTIPOLYGON") )){
        poly <- ttemp
      }
      sum <- st_union(sum,th)
    }

  }
  return(poly)
}

#list of times to be used
    #temp <- as.matrix(subset(rr,id=="18378")[,2:3])
    #times <- c(0.8,0.9,1,1.2,1.4,1.6,1.8,2.0,2.4,2.8,3.2,3.6,4.0,4.5,5,6,7,8,9,10)
    #times <- seq(0.01,1,by=0.025)
    #times <- c(0.01,0.05,0.1,0.2,0.4,0,7,1,1.5,2,2.5,3,4,5,7,10,12)
    #times <- c(seq(0.01,1.5,by = 0.01),seq(0.2,0.48,by=0.02),seq(0.5,2,by=0.3),seq(2.4,6,by=0.4))
    times <- c(seq(0.01,1,by = 0.01),seq(1.02,2,by=0.02))
    #times <- seq(0.5,10,by=0.5)
    #times <- c(0.1,0.2,0.3,0.4,0.5,1,2,3)

#initialize list of partial Reachable regions
partialRR <- c()
one_frame <-subset(match,frame==100 & !(player==1))
#for loop for every time. For each time, calulate the partial reachable region for each player
for (l in  1:length(times)){

  #use current time
  time = times[[l]]

  #apply reachable region function to all players
  rr<-apply(FUN=reach,MARGIN=1,X=cbind(one_frame[,9:12],rep(time,22)),FUNC = taki)

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
combined2<-st_cast(st_sfc(synthesize(1,partials)),"POLYGON")
combined2<- st_sf(id = rep("l",length(combined2)),geom = combined2)
for(i in 2:22){
  if(i < 12){
    team = "l"
  }
  else{
    team = "r"
  }
  temp<-st_cast(st_sfc(synthesize(i,partials)),"POLYGON")
  temp<- st_sf(id = rep(team,length(temp)),geom = temp)
  combined2 = rbind(combined2,temp)
}

temp<-st_cast(st_sfc(synthesize(1,partialRR)),"POLYGON")
temp2 <- st_cast(st_sfc(synthesize(2,partialRR)),"POLYGON")
combined <- cbind(as.data.frame(temp[[1]][[1]]),rep(1,length(temp[[1]][[1]])))
colnames(combined) <- c("x","y","id")
if (length(temp)>1){
  for (i in 2:length(temp)){
    temp2 <- cbind(as.data.frame(temp[[i]][[1]]),rep(1,length(temp[[i]][[1]])))
    colnames(temp2) <- c("x","y","id")
    combined <- rbind(combined,temp2)
  }
}

##
  for (i in 2:22){
    if(i < 12){
      team = "l"
    }
    else{
      team = "r"
    }
    temp<-st_cast(st_sfc(synthesize(i,partialRR)),"POLYGON")
    combinedT <- cbind(as.data.frame(temp[[1]][[1]]),rep(i,length(temp[[1]][[1]])))
    colnames(combinedT) <- c("x","y","id")
    if (length(temp)>1){
      for (j in 2:length(temp)){

        temp2 <- cbind(as.data.frame(temp[[j]][[1]]),rep(i,length(temp[[j]][[1]])))
        colnames(temp2) <- c("x","y","id")
        combinedT <- rbind(combinedT,temp2)
      }
    }

    combined <- rbind(combined,combinedT)
    #combined <- c(combined,synthesize(i,partialRR))
  }

combined <- combined %>%
  st_as_sf( coords = c( "x", "y" ) )
hulls <- combined %>%
  group_by( id ) %>%
  summarise( geometry = st_combine( geometry ) ) %>%
  st_convex_hull()


scale = 8.2/0.6
green = "#FFFFFF"

testSM <- smooth(combined2)
fuj<-soccerPitch() +
  fte_theme()+
  scale_color_manual(values=c(pal[5],pal[2],pal[4]))+
  scale_fill_manual(values=c(pal[2],pal[4]))+
  #coord_fixed()+
  labs(x="",y="")+
  geom_sf(data=testSM,mapping = aes(fill=as.factor(id)),alpha=0.2,show.legend = F)+
  geom_point(data = subset(match,frame==100),
             mapping=aes(x=x,y=y,color=as.factor(team)),
             position = 'jitter',
             show.legend = F)+
  geom_segment(data = subset(match,frame==100),
               mapping=aes(x=x,xend = x+vX*scale,yend=y+vY*scale,y=y,color=as.factor(team)),
               arrow = arrow(length = unit(0.1, "cm")),
               show.legend = F)



  ggplot(combined,aes(fill=as.factor(id))) +
    #fte_theme()+
    geom_sf()

#,aes(fill=as.factor(id))
ggplot(st_sf(partialRR[[10]])) +
  #  fte_theme()+
  geom_sf() +
  geom_point(data = df,aes(x=x,y=y),position = 'jitter')

green  = "#228C22"

soccerPitch() +
    fte_theme()+
  geom_sf(data= combined,alpha=0.5) +
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
  theta = atan2(pt[2]-ps[2],pt[1]-ps[1])-atan2(pu[2]-pt[2],pu[1]-pt[1])
  return(c(r*cos(theta),r*sin(theta)))
}

indices <- function(p,vt,tD){
  return(c(floor(p[1]+53.5),floor(p[2]+35),vt,which(tD == times)))
}

alg2 <- function(tD,df = temp){
  B <- rep(list(matrix(0,nrow=70,ncol=107)),length(levels(as.factor(df$speedGroup))))
  df1 <- df
  for (i in levels(as.factor(df1$player))){
    df <- subset(df1,player == i)
    gap <- tD/0.1
    for (j in 6:(nrow(df)-gap)){
      pt <- c(df[(j),9],df[(j),10])
      ps <- c(df[(j-5),9],df[(j-5),10])

      pu <- c(df[(j+gap),9],df[(j+gap),10])
      p <- transform(ps,pt,pu)
      vt <- as.numeric(df[j,14])
      abcd <- indices(p,vt,tD)
      #print(paste(j,abcd[1],abcd[2],abcd[3],abcd[4],sep=" "))
      if (!("FALSE" %in% as.list(abcd>0)) && abcd[1] <=107 && abcd[2] <=70){

        B[abcd[3]][[1]][abcd[2],abcd[1]] <- B[abcd[3]][[1]][abcd[2],abcd[1]] + 1
      }

    }
  }

  return(B)
}

#ggplot(df,aes(x=speed))+
#  geom_histogram(binwidth=0.1)

match = subset(df, matchID == 5)
td <- 0.2
times <- c(seq(0.01,1,by = 0.05),seq(1.02,2,by=0.1))
player <- 5
temp <- subset(match, !(player %in% c(1,2,13)))
#temp <- subset(match, player == 2)

A <- list()
for (i in 1:length(times)){
  print(i)
  A[i][[1]]<-alg2(times[i])
}

B<-alg2(0.2)

probP <- function(tD,ps,pt,p,v){
  abcd <- indices(transform(ps,pt,p),v,tD)
  return((A[abcd[4]][[1]][abcd[3]][[1]][abcd[2],abcd[1]])/(sum(colSums(A[abcd[4]][[1]][abcd[3]][[1]]))))
}

j = 100
tD=0.6
pt <- c(temp[(j),9],temp[(j),10])
ps <- c(temp[(j-2),9],temp[(j-2),10])
gap <- tD/0.1
pu <- c(temp[(j+gap),9],temp[(j+gap),10])
p<-transform(ps,pt,pu)
abcd <- indices(transform(ps,pt,pu),6,tD)
probP(tD,ps,pt,pu,3)

ls <- c()
for (i in 1:13){
  histDF <- data.frame(A[10][[1]][i][[1]])
  idxs <- which(histDF > 0, arr.ind=TRUE)
  if (nrow(idxs)==0){
    ls = c(ls,i)
  }
}

histDF <- data.frame(A[10][[1]][1][[1]])
idxs <- which(histDF > 0, arr.ind=TRUE)
idxs <- as.data.frame(idxs[ rep(1:nrow(idxs), histDF[histDF>0]), ])
idxs$speed <- 1
for(i in 2:21){
  #print(i)
  histDF <- data.frame(A[10][[1]][i][[1]])
  idxst <- which(histDF > 0, arr.ind=TRUE)
  if(nrow(idxst)>1){
    idxst <- as.data.frame(idxst[ rep(1:nrow(idxst), histDF[histDF>0]), ])
    idxst$speed <- i
    idxs <- rbind(idxs,idxst)
  }
}

points<-as.data.frame(cbind(rbind(ps,pt,pu),c("ps","pt","pu")))

p<-soccerPitch()+
  fte_theme()+
  labs(x="",y="")+
  coord_fixed()+
  geom_hex(data = idxs, mapping = aes(col,row),binwidth = 5,alpha=0.4,show.legend = F)+
  transition_states(
    states = speed,
    transition_length = 0.01,
    state_length = 0.3,
    wrap = TRUE
  )

animate(p)
