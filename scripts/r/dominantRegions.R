#general
  library(dplyr)
  library(foreach) #parallel
  library(doParallel) #parallel
  library(MASS) # kde
  library(stringr)

#plots
  library(ggplot2)
  library(ggforce)
  library(gganimate)
  library(RColorBrewer)
  library(ggpubr)
  library(wesanderson)

#spatial
  library(sf)
  library(sp)
  library(rgdal)
  library(smoothr)

#global constants
  scale = 8.2/0.6
  x_segmentation = 53.5
  y_segmentation = 35
  x_bin_width = 107/x_segmentation
  y_bin_width = 70/y_segmentation

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
  newX <- origin[1]+(vM*cos(angle)*(t-(1-exp(-alpha*t))/(alpha))+v[1]*(1-exp(-alpha*t))/(alpha))
  if (newX > 107){
    newX = 107
  }
  else if (newX < 0){
    newX = 0
  }
  newY <- origin[2]+vM*sin(angle)*(t-(1-exp(-alpha*t))/(alpha))+v[2]*(1-exp(-alpha*t))/(alpha)
  if (newY > 70){
    newY = 70
  }
  else if (newY < 0){
    newY = 0
  }
  return(c(newX,newY))
}

#reachable polygonal regions. Applies movement functions to every angle
reach <- function(param,mm = fujsug){
  return(data.frame(t(apply(FUN= mm,MARGIN=1,X=angles,
                            origin = c(param[1],param[2]),
                            v = c(param[3],param[4]),
                            t = param[5]))))
}

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
  sum <- th#
  for (i in 2:length(times)){
    temp <- 2*i
    th <- (parts[[temp]][[index]] %>% st_sfc() %>% st_set_precision(precision))[[1]]
    if (class(th)[[2]]=="MULTIPOLYGON"){
      #th <- (parts[[temp]][[index]] %>% st_sfc() %>% st_set_precision(precision) %>% st_make_valid())[[1]]
      #browser()
      huh<-data.frame(cbind(rep(1,nrow(th[[1]][[1]])),
                            th[[1]][[1]]))
      colnames(huh)<-c("id" ,"x" , "y" )
      huh<-st_as_sf(huh,coords=c("x","y")) %>%
        summarise( geometry = st_combine( geometry ) ) %>%
        st_cast("POLYGON")
      p <- st_make_valid(st_difference(huh,sum))
      for (k in 2:length(th)){
        huh<-data.frame(cbind(rep(1,nrow(th[[k]][[1]])),
                              th[[k]][[1]]))
        colnames(huh)<-c("id" ,"x" , "y" )
        huh<-st_as_sf(huh,coords=c("x","y")) %>%
          summarise( geometry = st_combine( geometry ) ) %>%
          st_cast("POLYGON")
        huhp <- st_make_valid(st_difference(huh,sum))
        if (nrow(huhp)>0){
          if (nrow(p)>0){
            p<-st_make_valid(st_union(p,huhp))
          }
          else{
            p <- huhp
          }
        }

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

partial <- function(time, one_frame){

  #apply reachable region function to all players
  rr<-apply(FUN=reach,MARGIN=1,X=cbind(one_frame[,9:12],rep(time,22)))

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

  # find all pairwise intersections
  tw_inter <- inter %>%
    graph_from_data_frame(directed=F) %>%
    simplify(remove.multiple = T,remove.loops = T) %>%
    get.data.frame()
  colnames(tw_inter) <- c("i","V2")
  tw_inter$i <- as.integer(tw_inter$i)
  tw_inter$V2 <- as.integer(tw_inter$V2)

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
  return(temp)

}

form_syn <- function(index){
  if(i < 12){
    team = "l"
  }
  else{
    team = "r"
  }
  combined2<-st_cast(st_sfc(synthesize(index,partialRR)),"POLYGON")
  combined2<- st_sf(id = rep(team,length(combined2)),geom = combined2)
  return(combined2)
}

#list of times to be used
times <- c(seq(0.1,2,by = 0.05),seq(2.1,4,by=0.1),seq(4.2,6,by=0.2))#,seq(0.6,2,by=0.1),seq(1.2,5,by=0.2),c(6,7))

#angles to wrap around
angles <- as.matrix(seq(from = -pi, to = pi, by = pi/60))

#take subset to demo
match = subset(df,matchID == 5)
one_frame = subset(match, player !=1 & frame == 100)

#find partial reachable regions and then synthesize them into completeted dominant regions
#start <- Sys.time()
cl <- makeCluster(8)
registerDoParallel(cl)
partialRR <- foreach(i = 1:length(times),
                     .combine="c",
                     .packages = c('dplyr','sf','igraph')) %dopar% partial(times[i],one_frame)
stopCluster(cl)
cl <- makeCluster(8)
registerDoParallel(cl)
combined2 <- foreach(i = 1:22,
                     .combine="rbind",
                     .packages = c("dplyr","sf")) %dopar% form_syn(i)
stopCluster(cl)
#print(Sys.time()-start)

#smooth regions
testSM <- smooth(combined2)

#plot with points and velocity vectors, colour by team
soccerPitch() +
  #fte_theme()+
  scale_color_manual(values=c(pal[5],pal[2],pal[4]))+
  scale_fill_manual(values=c(pal[2],pal[4]))+
  #coord_fixed()+
  labs(x="",y="")+
  geom_sf(data=combined2,mapping = aes(fill=as.factor(id)),alpha=0.2,show.legend = F)+
  geom_point(data = subset(match,frame==100),
             mapping=aes(x=x,y=y,color=as.factor(team)),
             position = 'jitter',
             show.legend = F)+
  geom_segment(data = subset(match,frame==100),
               mapping=aes(x=x,xend = x+vX*scale,yend=y+vY*scale,y=y,color=as.factor(team)),
               arrow = arrow(length = unit(0.1, "cm")),
               show.legend = F)

##Brefeld

transformedData <- read.csv('C:/Users/David/Desktop/player_movement.csv')

player = setClass("player",slots = list(id='character',team='character',densities='list'))

##transform the datapoint to the origin version
transform <- function(ps,pt,pu){
  r = sqrt((pt[1]-pu[1])^2+(pt[2]-pu[2])^2)
  s = sqrt((pt[1]-ps[1])^2+(pt[2]-ps[2])^2)
  theta = atan2(pt[2]-ps[2],pt[1]-ps[1])-atan2(pu[2]-pt[2],pu[1]-pt[1])
  return(c(r*cos(theta),r*sin(theta)))
}

##find indices of online updating matrices
indices <- function(p,vt,tD){
  return(c(floor((p[1]+53.5)/2),floor((p[2]+35)/2),vt,which(tD == times)))
}

##algorithm to create matrices for online updating
alg2 <- function(tD,td=5,data = temp){
  B <- rep(list(matrix(0,nrow=35,ncol=54)),length(levels(as.factor(data$speedGroup))))
  for (i in levels(as.factor(data$player))){
    df1 <- subset(data,player == i)
    gap <- tD/0.1
    for (j in 6:(nrow(df1)-gap)){
      pt <- c(df1[(j),9],df1[(j),10])
      ps <- c(df1[(j-5),9],df1[(j-5),10])

      pu <- c(df1[(j+gap),9],df1[(j+gap),10])
      p <- transform(ps,pt,pu)
      vt <- as.numeric(df1[j,14])
      abcd <- indices(p,vt,tD)
      #print(paste(j,abcd[1],abcd[2],abcd[3],abcd[4],sep=" "))
      if (!("FALSE" %in% as.list(abcd>0)) && abcd[1] <=54 && abcd[2] <=35){

        B[abcd[3]][[1]][abcd[2],abcd[1]] <- B[abcd[3]][[1]][abcd[2],abcd[1]] + 1
      }

    }
  }

  return(B)
}

##calculate the probability
probP <- function(tD,ps,pt,p,v){
  abcd <- indices(transform(ps,pt,p),v,tD)
  return((A[abcd[4]][[1]][abcd[3]][[1]][abcd[1],abcd[2]])/(sum(colSums(A[abcd[4]][[1]][abcd[3]][[1]]))))
}

probPKDE <- function(tD,ps,pt,p,v){

}

findTriplets<-function(data = temp,timesList =times){
  preGap <- td/0.1
  rows = nrow(data)
  triplets <- data.frame()
  for (time in  timesList){
    gap = time/0.1
    if (nrow(triplets) == 0){
      psx <- data[1:(rows-preGap-gap),]$x
      psy <- data[1:(rows-preGap-gap),]$y
      ptx <- data[(preGap+1):(rows-gap),]$x
      pty <- data[(preGap+1):(rows-gap),]$y
      pux <- data[(preGap+gap+1):rows,]$x
      puy <- data[(preGap+gap+1):rows,]$y
      speed <- data[(preGap+1):(rows-gap),]$speed
      speedGroup <- data[(preGap+1):(rows-gap),]$speedGroup
      time <- rep(time,length(psx))
      triplets <- as.data.frame(cbind(psx,psy,ptx,pty,pux,puy,speed,speedGroup,time))
    }
    else{
      psx <- data[1:(rows-preGap-gap),]$x
      psy <- data[1:(rows-preGap-gap),]$y
      ptx <- data[(preGap+1):(rows-gap),]$x
      pty <- data[(preGap+1):(rows-gap),]$y
      pux <- data[(preGap+gap+1):rows,]$x
      puy <- data[(preGap+gap+1):rows,]$y
      speed <- data[(preGap+1):(rows-gap),]$speed
      speedGroup <- data[(preGap+1):(rows-gap),]$speedGroup
      time <- rep(time,length(psx))
      temp <- as.data.frame(cbind(psx,psy,ptx,pty,pux,puy,speed,speedGroup,time))
      triplets<-rbind(triplets,temp)
    }
  }
  return(triplets)
}

filterDataTriplets <- function(data = subset(df,state==" play_on"),timesList = seq(0.1,2,by=0.1)){
  all_triplets_all_players <- data.frame(matchID = numeric(0),team = character(0),player= numeric(0),
                                         psx= numeric(0),psy= numeric(0),ptx= numeric(0),pty= numeric(0),
                                         pux= numeric(0),puy= numeric(0),speed= numeric(0),
                                         speedGroup= numeric(0),time= numeric(0))
  for (team in unique(c(levels(data$right_team),levels(data$left_team)))){
    print(team)
    team_temp <- rbind(data[data$left_team==team,],data[data$right_team==team,])
    for (match in levels(as.factor(team_temp$matchID))){
      print(match)
      match_temp <- subset(team_temp,matchID == match)
      players = seq(13,23)
      if (match_temp[1,]$left_team == team){
        players = seq(2,12)
      }
      for (playerID in players){
        player_temp <- subset(match_temp,player == playerID)
        player_triplets <- findTriplets(data = player_temp)
        player_triplets$matchID <- as.numeric(match)
        player_triplets$team <- team
        player_triplets$player <- ((as.numeric(playerID)-1)%%11)+1
        player_triplets <- player_triplets[,c(seq(10,12),seq(1,9))]
        all_triplets_all_players <- rbind(all_triplets_all_players,
                                          player_triplets)
      }
    }
  }
  return(all_triplets_all_players)
}

findDensities<-function(data,average = FALSE){
  #given a list of transformed P and V and tD
  #find the kde for each speedgroup and time
  #create player object for each player and add each kde to a list for the 'densities' attribute
  #create and return list of players
  #if average == true then also return a model for the average outfield player
  
  players <- c()
  for (this.team in levels(as.factor(data$team))){
    print(this.team)
    for (i in 1:11){
      print(i)
      this.densities <- c()
      for (this.speedGroup in levels(as.factor(data$speedGroup))){
        print(this.speedGroup)
        temp <- subset(data, team == this.team & player == i & speedGroup == this.speedGroup)
        speedDensities <- c()
        for (this.time in levels(as.factor(data$time))){
          print(this.time)
          timeSet <- subset(temp, time == this.time)
          if(nrow(timeSet)>1){
            if (var(timeSet$X0)==0 | var(timeSet$X1)==0){
              speedDensities <- c(speedDensities,0)  
            }
            else{
              dens <- kde2d(timeSet$X0,timeSet$X1,
                            h = c(ifelse(bandwidth.nrd(timeSet$X0) < 0.001, 0.1, bandwidth.nrd(timeSet$X0)),
                                  ifelse(bandwidth.nrd(timeSet$X1) < 0.001, 0.1, bandwidth.nrd(timeSet$X1))))
              speedDensities <- c(speedDensities,dens)  
            }
          }
          else{
            speedDensities <- c(speedDensities,0)
          }
          
        }
        this.densities <- c(this.densities,speedDensities)
      }
      players <- c(players,player(id = as.character(i),
                                  team = as.character(this.team),
                                  densities = this.densities))
    }
  }
  return(players)
}

densities <- findDensities(transformedData)

match = subset(df,matchID == 5)
td <- 0.2
times <- c(1,2,3,4,5,6)
player <- 5
temp <- subset(match, !(player %in% c(1,2,13)) & state == " play_on")
temp <- subset(match, player == 14 & state == " play_on")

test <- filterDataTriplets()
temp <- subset(triplets[triplets$team == "Oxsy",],player==5)
temp <- subset(temp,time==3)

td <- 0.2
tD <- 5
cl <- makeCluster(8)
registerDoParallel(cl)
start = Sys.time()
St <- as.data.frame(foreach(row = iter(temp,by='row'),
                            .combine  = "rbind") %dopar% 
                                                           transform(c(row[[4]],row[[5]]),
                                                                     c(row[[6]],row[[7]]),
                                                                     c(row[[8]],row[[9]])))#,
#c(row[[2]],
#  row[[3]],
#                                                           row[[11]],
#                                                           row[[12]]),row.names = NULL)
stopCluster(cl)
(Sys.time()-start)
#St$V4 <- temp[((td/0.1)+1):(nrow(temp)-tD/0.1),11]
#St$V5 <- temp[((td/0.1)+1):(nrow(temp)-tD/0.1),12]
#St$V6 <- 1
colnames(St) <- c("team","player","x","y","speedGroup","tD")

td <- 0.2
tD <- 5
cl <- makeCluster(8)
registerDoParallel(cl)
St <- as.data.frame(foreach(i = ((td/0.1)+1):(nrow(temp)-tD/0.1),
                            .combine  = "rbind") %dopar% c(transform(c(temp$x[i-(td/0.1)],temp$y[i-(td/0.1)]),
                                                                     c(temp$x[i],temp$y[i]),
                                                                     c(temp$x[i+(tD/0.1)],temp$y[i+(tD/0.1)])),
                                                           temp$speedGroup[i]),row.names = NULL)
stopCluster(cl)
St$V4 <- temp[((td/0.1)+1):(nrow(temp)-tD/0.1),11]
St$V5 <- temp[((td/0.1)+1):(nrow(temp)-tD/0.1),12]
St$V6 <- 1
colnames(St) <- c("x","y","vX","vY","v","speedGroup")

for (i in 2:6){
  temp <- subset(match, player == 14 & state == " play_on" & speedGroup == i)

  td <- 0.2
  tD <- 5
  cl <- makeCluster(8)
  registerDoParallel(cl)
  Stt <- as.data.frame(foreach(i = ((td/0.1)+1):(nrow(temp)-tD/0.1),
                              .combine  = "rbind") %dopar% c(transform(c(temp$x[i-(td/0.1)],temp$y[i-(td/0.1)]),
                                                                       c(temp$x[i],temp$y[i]),
                                                                       c(temp$x[i+(tD/0.1)],temp$y[i+(tD/0.1)])),
                                                             temp$speedGroup[i]),row.names = NULL)
  stopCluster(cl)
  Stt$V4 <- temp[((td/0.1)+1):(nrow(temp)-tD/0.1),11]
  Stt$V5 <- temp[((td/0.1)+1):(nrow(temp)-tD/0.1),12]
  Stt$v6 <- i
  colnames(Stt) <- c("x","y","vX","vY","v","speedGroup")

  St <- rbind(St,Stt)
}



soccerPitch(scale = c(0,107,x_bin_width,0,70,y_bin_width),linecolor="black")+
  coord_fixed()+
  labs(x="",y="")+
  scale_fill_discrete(type=brewer.pal("BuPu",n=8))+
  geom_density_2d_filled(data = subset(St,x < 53.5 & x > -53.5 & y < 35 & y > -35 & tD == 5),
                         aes(x=x+53.5,y=y+35),
                         alpha=0.8,
                         bins=8,
                         show.legend = F,
                         n=c(107,70))+
  transition_states(
    states = speedGroup,
    transition_length = 0,
    state_length = 0.3,
    wrap = F
  )


  soccerPitch()+
    coord_fixed()+
    labs(x="",y="")+
    geom_point(data = St,aes(x=x+53.5,y=y+35),size=0.1)#+
    geom_segment(data = St[1:500,],
                 mapping=aes(x=x+53.5,xend = x+53.5+v*scale,yend=y+35+vY,y=y+35),
                 arrow = arrow(length = unit(0.1, "cm")),
                 show.legend = F)


iterations <- length(times)
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
cl <- makeCluster(4)
registerDoParallel(cl)
A <- foreach( i = 1:iterations,
              .combine = "c",
              .options.snow = opts) %dopar% alg2(times[i])
close(pb)
stopCluster(cl)


for (i in 1:length(times)){
  print(i)
  A[i][[1]]<-alg2(times[i])
}

j = 100
tD=0.6
pt <- c(temp[(j),9],temp[(j),10])
ps <- c(temp[(j-2),9],temp[(j-2),10])
gap <- tD/0.1
pu <- c(temp[(j+gap),9],temp[(j+gap),10])
p<-transform(ps,pt,pu)
abcd <- indices(transform(ps,pt,pu),6,tD)
probP(tD,ps,pt,c(50,60),2)

ls <- c()
for (i in 1:13){
  histDF <- data.frame(A[10][[1]][i][[1]])
  idxs <- which(histDF > 0, arr.ind=TRUE)
  if (nrow(idxs)==0){
    ls = c(ls,i)
  }
}

histDF <- data.frame(A[6])
idxs <- which(histDF > 0, arr.ind=TRUE)
idxs <- as.data.frame(idxs[ rep(1:nrow(idxs), histDF[histDF>0]), ])
idxs$time <- 1
for(i in 2:length(times)){
  #print(i)
  histDF <- data.frame(A[6+11*i])
  idxst <- which(histDF > 0, arr.ind=TRUE)
  if(nrow(idxst)>1){
    idxst <- as.data.frame(idxst[ rep(1:nrow(idxst), histDF[histDF>0]), ])
    idxst$time <- i
    idxs <- rbind(idxs,idxst)
  }
}

idxs$row <- 2*idxs$row
idxs$col <- 2*idxs$col

points<-as.data.frame(cbind(rbind(ps,pt,pu),c("ps","pt","pu")))

p<-soccerPitch()+
  fte_theme()+
  labs(x="",y="")+
  coord_fixed()+
  geom_hex(data = idxs, mapping = aes(x=col,y=row),binwidth = 5,alpha=0.4,show.legend = F)+
  transition_states(
    states = time,
    wrap = F
  )

animate(p,nframes=135)
