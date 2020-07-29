#general
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

#spatial
  library(sf)
  library(sp)
  library(rgdal)
  library(smoothr)

pal <- c("#A6CEE3","#1F78B4","#b2df8a","#33a02c",
         "#fb9a99","#fb9a99","#fdbf6f","#ff7f00",
         "#cab2d6","#6a3d9a","#ffff99")

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
    df$speedGroup <- as.numeric(cut(df$speed, c(0,0.03,0.073,0.13,0.24,0.48,0.6),include.lowest = T))
    #df$speedGroup <- as.numeric(cut(df$speed, c(seq(0,0.1,by=0.01),seq(0.15,0.6,by=0.05),10),include.lowest = T))
    df$time <- as.numeric(df$frame)/10
    df$team <- cut(df$player,c(0,1,12,23),include.lowest = T)
    levels(df$team) <- c("b","l","r")

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

#global constants
scale = 8.2/0.6
x_segmentation = 53.5
y_segmentation = 35
x_bin_width = 107/x_segmentation
y_bin_width = 70/y_segmentation

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
  geom_sf(data=testSM,mapping = aes(fill=as.factor(id)),alpha=0.2,show.legend = F)+
  geom_point(data = subset(match,frame==100),
             mapping=aes(x=x,y=y,color=as.factor(team)),
             position = 'jitter',
             show.legend = F)+
  geom_segment(data = subset(match,frame==100),
               mapping=aes(x=x,xend = x+vX*scale,yend=y+vY*scale,y=y,color=as.factor(team)),
               arrow = arrow(length = unit(0.1, "cm")),
               show.legend = F)

##Brefeld

##transform the datapoint to the origin version

transform <- function(ps,pt,pu){
  r = sqrt((pt[1]-pu[1])^2+(pt[2]-pu[2])^2)
  s = sqrt((pt[1]-ps[1])^2+(pt[2]-ps[2])^2)
  theta = atan2(pt[2]-ps[2],pt[1]-ps[1])-atan2(pu[2]-pt[2],pu[1]-pt[1])
  return(c(r*cos(theta),r*sin(theta)))
}

indices <- function(p,vt,tD){
  return(c(floor((p[1]+53.5)/2),floor((p[2]+35)/2),vt,which(tD == times)))
}

alg2 <- function(tD,data = temp){
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

probP <- function(tD,ps,pt,p,v){
  abcd <- indices(transform(ps,pt,p),v,tD)
  return((A[abcd[4]][[1]][abcd[3]][[1]][abcd[2],abcd[1]])/(sum(colSums(A[abcd[4]][[1]][abcd[3]][[1]]))))
}

match = subset(df,matchID == 5)
td <- 0.2
times <- c(0.1,0.2,0.4,0.5,0.75,1)
player <- 5
temp <- subset(match, !(player %in% c(1,2,13)) & state == " play_on")
temp <- subset(match, player == 14)

td <- 0.2
tD <- 5
cl <- makeCluster(8)
registerDoParallel(cl)
St <- as.data.frame(foreach(i = ((td/0.1)+1):(nrow(temp)-tD/0.1),
        .combine  = "rbind") %dopar% c(transform(c(temp$x[i-(td/0.1)],temp$y[i-(td/0.1)]),
                                             c(temp$x[i],temp$y[i]),
                                             c(temp$x[i+(tD/0.1)],temp$y[i+(tD/0.1)])),
                                        temp$speed[i]),row.names = NULL)
stopCluster(cl)
St$V4 <- temp[((td/0.1)+1):(nrow(temp)-tD/0.1),11]
St$V5 <- temp[((td/0.1)+1):(nrow(temp)-tD/0.1),12]
colnames(St) <- c("x","y","vX","vY","v")


soccerPitch(scale = c(0,107,x_bin_width,0,70,y_bin_width),linecolor="black")+
  coord_fixed()+
  labs(x="",y="")+
  scale_fill_discrete(type=brewer.pal("BuPu",n=8))+
  geom_density_2d_filled(data = subset(St,x < 53.5 & x > -53.5 & y < 35 & y > -35), 
                         aes(x=x+53.5,y=y+35),
                         alpha=0.8,
                         bins=8,
                         show.legend = F)

  
  soccerPitch()+
    coord_fixed()+
    labs(x="",y="")+
    geom_point(data = St,aes(x=x+53.5,y=y+35),size=0.1)#+
    geom_segment(data = St[1:500,],
                 mapping=aes(x=x+53.5,xend = x+53.5+v*scale,yend=y+35+vY,y=y+35),
                 arrow = arrow(length = unit(0.1, "cm")),
                 show.legend = F)
  
A <- list()
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

histDF <- data.frame(A[4][[1]][1][[1]])
idxs <- which(histDF > 0, arr.ind=TRUE)
idxs <- as.data.frame(idxs[ rep(1:nrow(idxs), histDF[histDF>0]), ])
idxs$speed <- 1
for(i in 2:length(times)){
  #print(i)
  histDF <- data.frame(A[4][[1]][i][[1]])
  idxst <- which(histDF > 0, arr.ind=TRUE)
  if(nrow(idxst)>1){
    idxst <- as.data.frame(idxst[ rep(1:nrow(idxst), histDF[histDF>0]), ])
    idxst$speed <- i
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
  geom_hex(data = idxs, mapping = aes(col,row),binwidth = 5,alpha=0.4,show.legend = F)+
  transition_states(
    states = speed,
    wrap = F
  )

animate(p)
