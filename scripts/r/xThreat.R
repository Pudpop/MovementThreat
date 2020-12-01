#read in events
events <- read.csv("C:/Users/David/OneDrive/Documents/Work/Thesis/Data/events.csv")

invert_right <- function(data){
  data$x <- ifelse(data$team == "l",
                   data$x,
                   pitch_length - data$x)
  data$y <- ifelse(data$team == "l",
                   data$y,
                   pitch_width - data$y)
  data$ballPosX <- ifelse(data$team == "l",
                   data$ballPosX,
                   pitch_length - data$ballPosX)
  data$ballPosY <- ifelse(data$team == "l",
                   data$ballPosY,
                   pitch_width - data$ballPosY)
  data$eventEndPosX <- ifelse(data$team == "l",
                   data$eventEndPosX,
                   pitch_length - data$eventEndPosX)
  data$eventEndPosY <- ifelse(data$team == "l",
                              data$eventEndPosY,
                              pitch_width - data$eventEndPosY)
  return(data)
}

events <- invert_right(events)

events$xBlock <- cut(events$x,breaks = seq(0,pitch_length,by=pitch_length/x_segmentation)) %>% as.numeric()
events$yBlock <- cut(events$y,breaks = seq(0,pitch_width,by=pitch_width/y_segmentation)) %>% as.numeric()
events$eventEndPosXBlock <- cut(events$eventEndPosX,
                                breaks = seq(0,pitch_length,by=pitch_length/x_segmentation)) %>%
  as.numeric()
events$eventEndPosYBlock <- cut(events$eventEndPosY,
                                breaks = seq(0,pitch_width,by=pitch_width/y_segmentation)) %>%
  as.numeric()

events$eventSuccess <- (events$eventSuccess == "True") %>% as.numeric()


shots <- events %>% subset(action == "shot")
moves <- events %>% subset(action %in% c("pass","dribble"))
dribbles <- events %>% subset(action == "dribble")
passes <- events %>% subset(action == "pass")

soccerPitch() + geom_segment(data = shots %>% subset(recPlayer != "goal"),mapping = aes(x = x,y = y,xend = eventEndPosX, yend = eventEndPosY,color = team))

#train expected goals

set.seed(1886)
train <- sample(nrow(shots),size=floor(0.9*nrow(shots)))

xG <- glm(eventSuccess ~x*abs(y-35),data=shots[train,],family="binomial")
summary(xG)
pred <- predict(xG,shots[-train,])
pred <- exp(pred)/(1+exp(pred))
(pred %>% round %>% as.numeric() %>% table(shots[-train,]$eventSuccess) %>% diag() %>% sum)/nrow(shots[-train,])

#krige

predict_krige <- function(row,krige_model){
  return(krige_model[row[2],row[1]])
}

lon <- seq(0, pitch_length, length.out = x_segmentation)
lat <- seq(0, pitch_width, length.out = y_segmentation)
grd <- expand.grid(x = lon, y = lat)

grd_sf  <-  st_as_sf(grd, coords = c("x", "y"), agr = "constant")

grd_sp <- as_Spatial(grd_sf)

sf_temp <- st_as_sf(shots[,c("eventSuccess","x","y")],coords = c("x","y"))

sp_temp <- as_Spatial(sf_temp)

dt.fit <- variogram(eventSuccess~1,sp_temp )%>%
  fit.variogram(model = vgm("Sph"))

lzn.kriged <- krige((eventSuccess) ~ 1, sf_temp[train,] %>% as_Spatial, grd_sp, model=dt.fit)
krige_model <- lzn.kriged$var1.pred %>% matrix(nrow=y_segmentation,ncol=x_segmentation,byrow=T)

kpred <- apply(X=shots[-train,c('xBlock','yBlock')],
               FUN = predict_krige,
               MARGIN=1,
               krige_model=krige_model)
(kpred %>% round %>% table(shots[-train,"eventSuccess"]) %>% diag %>% sum)/nrow(shots[-train,])

lzn.kriged %>% as.data.frame %>% rename(lon=coords.x1, lat=coords.x2) %>% 
  ggplot(aes(x=lon, y=lat)) + 
  geom_tile(aes(fill=var1.pred)) + 
  coord_equal() +
  scale_fill_gradientn(colours = myPalette(100), limits=c(0,1))+
  fte_theme()+
  labs(x="",y="",title="Indicator Kriging",fill = "xG")

lzn.kriged %>% as.data.frame %>% rename(lon=coords.x1, lat=coords.x2) %>% 
  ggplot(aes(x=lon, y=lat)) + 
  geom_tile(aes(fill=var1.var),show.legend = F) + 
  coord_equal() +
  scale_fill_gradientn(colours = myPalette(100))+
  fte_theme()+
  labs(x="",y="",title="Indicator Kriging",fill = "xG")

rm(sf_temp)
rm(sp_temp)
rm(dt.fit)
rm(lon)
rm(lat)
rm(grd)
rm(grd_sf)
rm(grd_sp)
rm(lzn.kriged)

#train expected threat

xT <- matrix(0,nrow=x_segmentation,ncol=y_segmentation)

filename <- "C:/Users/David/OneDrive/Documents/Work/Thesis/Data/threat_probs.hdf5"

if (TRUE){
  xT <- h5read(filename,"xT")
  xT_right <- xT
  xT_right <- xT_right[,c(ncol(xT_right):1)]
  xT_right <- xT_right[c(nrow(xT_right):1),]
}

if (FALSE){
  h5createFile(filename)
  filename %>% h5createDataset("probs",
                               dims = c(num_cells,num_cells),
                               H5type = "H5T_NATIVE_INT_FAST16",
                               chunk = c(1,num_cells),
                               level = 5)
  filename %>% h5createDataset("shoot",
                               dims = c(x_segmentation,y_segmentation))
  filename %>% h5createDataset("xT",
                               dims = c(x_segmentation,y_segmentation))
}

write_probs <- function(){
  for (i in 1:1120){
    print(i)
    x_no <- (floor(i/y_segmentation)+1) %>% as.integer()
    y_no <- ((i-1)%%y_segmentation)+1
    sub <- events[!is.na(events$eventEndPosYBlock) & !is.na(events$eventEndPosXBlock),] %>% 
              subset(xBlock == x_no & yBlock == y_no)
    counts <- sub %>%
      group_by(eventEndPosXBlock, eventEndPosYBlock) %>%
      summarise(
        n = n(),
        action = action,
        .groups="drop"
      ) %>%
      as.data.frame() %>%
      mutate(
        index = (eventEndPosXBlock-1)*y_segmentation + (eventEndPosYBlock),
        n = n,
        action = action
      )
    m1 <- matrix(0,num_cells)
    m1[as.matrix(counts[,c("index")])] <- counts[,c("n")]
    m1 <- as.vector(m1)
    h5write(m1,file = filename,name = "probs",index = list(i,NULL))
    h5closeAll()
    total <- nrow(sub)
    if (total == 0){
      #print("zero")
      h5write(0,file = filename,name = "shoot",index = list(x_no,y_no))
    }
    else{
      shootProb <- sub %>%
        subset(action == "shot") %>%
        nrow
      print(shootProb/total)
      h5write(shootProb/total,file = filename,name = "shoot",index = list(x_no,y_no))
    }
    h5closeAll()
    
  }
}
#write_probs()


prob_shoot <- function(x,y){
  return(h5read(filename,"shoot")[x,y])
}

cellThreat <- function(coords,threat,xG_model = "lm",data = events){
  pShoot <- prob_shoot(coords[1],coords[2])
  
  expG <- 0
  if(xG_model == "lm"){
    coords_con <- data.frame(x = coords[1]*x_bin_width,
                             y = coords[2]*y_bin_width)
    expG <- predict(xG,coords_con)
  }
  else{
    expG <- predict_krige(coords,krige_model)
  }
  expG <- exp(expG)/(1+exp(expG))

  index <- (coords[1]-1)*y_segmentation + coords[2]
  
  vec <- h5read(filename,"probs")[index,]
  mat = 0
  if (!(sum(vec) == 0)){
    mat <- (vec/sum(vec) * (threat %>% t %>% as.vector())) %>% sum()  
  }
  
  h5closeAll()
  
  th <- (pShoot * expG + (1-pShoot) * mat) %>% as.numeric()
  return(th)
  
}

iter_threat <- function(xT){
  init <- xT
  epsilon <- 100
  while(epsilon > 5.2){
    new <- (init %>%
              melt)[,1:2] %>%
              pbapply(FUN = cellThreat,
                    MARGIN = 1,
                    threat = init) %>%
              matrix(nrow = x_segmentation,ncol=y_segmentation)
    epsilon <- abs(init-new) %>% sum
    #epsilon = 0
    init <- new
    print(epsilon)
  }
  
  return(init)
}

xT <- iter_threat(xT)
xT_right <- xT
xT_right <- xT_right[,c(ncol(xT_right):1)]
xT_right <- xT_right[c(nrow(xT_right):1),]
h5write(xT,file = filename,name = "xT",index = list(NULL,NULL))
soccerPitch() + 
  geom_tile(data = xT %>% melt(),
            mapping=aes(x=(Var1-0.5)*x_bin_width,y=(Var2-0.5)*y_bin_width,fill=value),
            alpha=0.6) +
  scale_fill_gradientn(colours = myPalette(100))+
  coord_fixed() + 
  labs(x="",y="",fill="xT")

path = "C:/Users/David/OneDrive/Documents/Work/Thesis/Data/matches_formatted"
file = "/cyrus2017-vs-Gliders2016/13-20170905233758-CYRUS_3-vs-Gliders2016_8.csv"
match <- read.csv(paste0(path,file))
match <- match[,!(names(match) %in% c("X","index"))]

soccerPitch() + 
  #geom_text(data = shots ,mapping = aes(x=x,y=y,alpha = recPlayer == "goal",label=paste0(player," : ",frame),color=team),size=2.8)+
  geom_point(data = shots ,
             mapping = aes(x=x,y=y,shape = eventSuccess %>% as.factor,color=team),
             size = 2.5)+
  labs(x="",y="")+
  coord_fixed()



