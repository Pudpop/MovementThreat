setwd('C:/Users/David/OneDrive/Documents/Work/Thesis/Code/Data')
data = read.csv('groundtruth.csv')
library(ggplot2)
library(dplyr) 
library(RColorBrewer)
library(ggpubr)
library(sf)
library(sp)
library(rgdal)
library(mapview)

frames = 1

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

df$x<-as.numeric(df$x)
df$y<-as.numeric(df$y)
df$vX<-as.numeric(df$vX)
df$vY<-as.numeric(df$vY)

#df <- subset(df,frame==800)

angles <- as.matrix(seq(from = 0, to = 360, by = 30))

taki <- function(angle = 0,origin = c(0,0),v = c(0,0),t = 1,a = 42){
  return(c(origin[1]+(0.5*a*cos(angle)*t^2+v[1]*t),origin[2]+(0.5*a*sin(angle)*t^2+v[2]*t)))
  
}



fujsug <- function(angle = 0,origin = c(0,0),v = c(0,0),t = 1,alpha = 1.3,vM = 8){
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

reach <- function(param,FUNC = taki){
  return(data.frame(t(apply(FUN= FUNC,MARGIN=1,X=angles,
                            origin = c(param[1],param[2]),
                            v = c(param[3],param[4]),
                            t = param[5]))))
}

time = 0.2

rr<-apply(FUN=reach,MARGIN=1,X=cbind(df[,4:7],rep(time,nrow(df))))
rr<-bind_rows(rr,.id='id')


takiP<-ggplot(data = rr,aes(x=x,y=y,color=id))+
  geom_point(show.legend = F)+
  fte_theme() + coord_fixed()

rr<-apply(FUN=reach,MARGIN=1,X=cbind(df[,4:7],rep(time,nrow(df))),FUNC = fujsug)
for ( i in 1:length(rr)){
  colnames(rr[[i]]) <- c("x","y")
}
rr<-bind_rows(rr,.id='id')


fujP<-ggplot(data = rr,aes(x=x,y=y,color=id))+
  geom_point(show.legend = F)+
  fte_theme() + coord_fixed()

ggarrange(takiP,fujP,ncol=2,nrow=1)

#temp <- as.matrix(subset(rr,id=="18378")[,2:3])
#times <- c(0.05,0.1,0.15,0.2,0.25,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.2,1.4,1.6,1.8,2.0,
#           2.4,2.8,3.2,3.6,4.0,4.5,5,6,7,8,9,10,12,14,16,18,20)
#times <- seq(0.01,0.5,by=0.01)
#times <- c(0.01,0.05,0.1,0.2,0.4,0,7,1,1.5,2,2.5,3,4,5,7,10,12)
times <- seq(0.1,2,by = 0.1)
partialRR <- c()
for (l in  1:length(times)){
  
  
  time = times[[l]]
  
  rr<-apply(FUN=reach,MARGIN=1,X=cbind(df[,4:7],rep(time,nrow(df))),FUNC = fujsug)
  for ( i in 1:length(rr)){
    colnames(rr[[i]]) <- c("x","y")
  }
  rr<-bind_rows(rr,.id='id')
  
  df.sf <- rr %>%
    st_as_sf( coords = c( "x", "y" ) )
  hulls <- df.sf %>%
    group_by( id ) %>%
    summarise( geometry = st_combine( geometry ) ) %>%
    st_convex_hull()
  inter<-st_intersects(hulls)
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

synthesize <- function(parts,index){
  poly <- parts[[2]][[index]]
  sum <- parts[[2]][[index]]
  for (i in 2:4){
    temp <- 2*i
    poly <- st_union(poly,st_difference(parts[[temp]][[index]],sum))
    sum <- st_union(sum,parts[[temp]][[index]])
  }
  return(poly)
}
temp<-st_cast(synthesize(partialRR,1),"POLYGON")[[1]]
combined <- cbind(as.data.frame(temp),rep(1,length(temp)))
colnames(combined) <- c("x","y","id")

for (i in 2:23){
  temp<-st_cast(synthesize(partialRR,i),"POLYGON")[[1]]
  combinedT <- cbind(as.data.frame(temp),rep(i,length(temp)))
  colnames(combinedT) <- c("x","y","id")
  combined <- rbind(combined,combinedT) 
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
  geom_point(data = df,aes(x=x,y=y))

mapview::mapview( df.sf )

sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(as.matrix(subset(rr,id=="18378")[,2:3]))), ID=1)))
sp_poly_df <- SpatialPolygonsDataFrame(sp_poly, data=data.frame(ID=1))
writeOGR(sp_poly_df, "chull", layer="chull", driver="ESRI Shapefile")

nc <- st_read("chull/chull.shp")
ch <- chull(temp)
coords <-temp[c(ch, ch[1]),]  
plot(temp, pch=19)
lines(coords, col="red")

vor <- fortify(sp_poly_df)

vor <- cbind(vor,rep(1,times=nrow(vor)))
colnames(vor) = c("long","lat","order","hole","piece","id","group","frame")


ggplot(vor, aes(x=long,y=lat))+
  geom_polygon(aes(group=id))

ggplot(nc) +
  fte_theme()+
  geom_sf() 
