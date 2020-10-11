#global constants
  scale = 8.2/0.6
  x_segmentation = 20
  y_segmentation = 14
  pitch_length = 107
  pitch_width = 70
  x_bin_width = pitch_length/x_segmentation
  y_bin_width = pitch_width/y_segmentation
  #list of robocup teams
  teams = c("Gliders2016","HELIOS2016","Rione","CYRUS","MT2017",
            "Oxsy","FRAUNIted","HELIOS2017","HfutEngine2017","CSUYunlu")
  #list of times to be used
  times <- c(seq(0.1,2,by = 0.05),seq(2.1,4,by=0.1),seq(4.2,6,by=0.2))#,seq(0.6,2,by=0.1),seq(1.2,5,by=0.2),c(6,7))
  #speedgroup names
  speeds <- seq(1,12)
  #angles to wrap around
  angles <- as.matrix(seq(from = -pi, to = pi, by = pi/60))
  #pc times
  pc_times = c(1,5,10,20,30,40,45)

#taki's movement model function
taki <- function(angle = 0,origin = c(0,0),v = c(0,0),t = 1,a = 4.2,scale=8.2/0.6){
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

#fujimura', needs to be reweighted's movement model
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

#reachable region defined in the case of brefeld
reach_brefeld <- function(param,momo,timesList = times,
                          path = "C:/Users/David/OneDrive/Documents/Work/Thesis/Code/Data/test/transformed_points"){
  
  posx=2
  posy=3
  
  precision <- c(rep(0.999,20),rep(0.99,20),rep(0.95,10),rep(0.9,10),rep(0.5,9))
  
  this.player = (as.numeric(param[1])-2) %% 11 + 1
  
  this.team = 2
  this.team.name = momo[2]
  if (param[7] == "l"){
    this.team = 1
    this.team.name = momo[1]
  }
  
  time_delta_index <- which(timesList == as.numeric(param[8]))
  
  directory <- paste0(path,"/",this.team.name,"/player_",this.player,"/speed_",param[6],"/time_",time_delta_index,".csv.gz")
  trans_points <- read.csv(directory)
  
  dens <- kde2d(trans_points$X0,trans_points$X1,
                h = c(ifelse(bandwidth.nrd(trans_points$X0) < 0.001, 0.1, bandwidth.nrd(trans_points$X0)),
                      ifelse(bandwidth.nrd(trans_points$X1) < 0.001, 0.1, bandwidth.nrd(trans_points$X1))),
                n = c(107,70),
                lims = c(-53.5,53.5,-35,35))
  
  if (length(dens) == 1){
    cl <- data.frame(x = as.numeric(param[2])-53.5,y=as.numeric(param[3])-35)
    posx=1
    posy=2
  }
  else{
    cl <-  contourLines(x = dens$x,
                        y = dens$y,
                        z = dens$z,
                        levels = c(quantile(x = dens$z %>%
                                              as.vector(),
                                            probs = precision[time_delta_index])[[1]]))
    if (length(cl) == 0){
      cl <- data.frame(x = as.numeric(param[2])-53.5,y=as.numeric(param[3])-35)
      posx=1
      posy=2
    }
    else{
      cl <- cl %>% lapply(FUN=data.frame) %>% bind_rows()
    }
    
  }
  
  rot_angle <- atan2(param[5] %>% as.numeric(),
                     param[4] %>% as.numeric())
  cl$rotx <- (cl$x)*cos(rot_angle)+(cl$y)*sin(rot_angle)
  cl$roty <- (cl$x)*(-sin(rot_angle))+ (cl$y)*cos(rot_angle)
  
  cl$x <- cl$rotx + as.numeric(param[2])
  cl$y <- cl$roty + as.numeric(param[3])
  
  return(cl[,c(posx,posy)])
  
}

#find partial reachable regions
partial <- function(time, one_frame,momo = fujsug){
  rr <- numeric()
  #apply reachable region function to all players
  if (length(momo) > 1){
    #rr <- apply(FUN=reach_brefeld,MARGIN=1, X = cbind(one_frame[,c(2,9,10,11,12,14,16)],rep(time,22)),mm = momo)
    test <- cbind(one_frame[,c(2,9,10,11,12,14,16)],rep(time,22))
    rr <- data.frame(x=numeric(),y=numeric(),id=numeric())
    for (i in 1:22){
      temp <- reach_brefeld(test[i,],momo)
      temp <- cbind(temp,rep(i,nrow(temp)))
      colnames(temp) <-  c('x',"y","id")
      rr <- rbind(rr,temp)
    }
  }
  else{
    rr<-apply(FUN=reach,MARGIN=1,X=cbind(one_frame[,9:12],rep(time,22)),mm = momo) 
    
    #format column names for bind_rows
    for ( i in 1:length(rr)){
      colnames(rr[[i]]) <- c("x","y")
    }
    #create one data frame
    rr<-bind_rows(rr,.id='id')
  }
  
  #convert to sf object
  df.sf <- rr %>%
    st_as_sf( coords = c( "x", "y" ) )
  
  #find convex hulls of each region
  hulls <- df.sf %>%
    group_by( id ) %>%
    summarise( geometry = st_combine( geometry ) ,do_union=F,.groups="drop") %>%
    st_convex_hull() %>%
    suppressWarnings()
  
  #if (!(is_empty(empty))){
  #union of previously empty regions
  #add union
  #  hulls[23,] <- list(id="1",geometry=hulls$geometry[empty] %>%
  #                                    summarise(do_union = F) %>%
  #                                    st_cast("MULTIPOLYGON") %>%
  #                                    suppressWarnings()
  #                     )
  
  #replace emptyies with empty polygon
  #  hulls[emptyempty,]<-st_sf(data.frame(id="1",geometry=st_sfc(st_polygon())))
  #}
  
  
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
  #
  #browser()
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
  
  #print(temp$geometry)
  #empty_indices <<- unique(c(empty,which(st_is_empty(subset(temp,id %in% temp$id))==TRUE)))
  #return(subset(temp,id %in% temp$id))#
  return(temp)
  
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
    p <- c()
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

#formats synthesize algorithm
form_syn <- function(index,this.partialRR){
  if(index < 12){
    team = "l"
  }
  else{
    team = "r"
  }
  combined2 <- synthesize(index,this.partialRR)
  if (!(class(combined2)[[2]]=="GEOMETRYCOLLECTION" | class(combined2)[[2]]=="POINT")){
    combined2<-st_cast(st_sfc(combined2),"POLYGON") 
    combined2<- st_sf(team = rep(team,length(combined2)),geometry = combined2)
  }
  else{
    combined2<- st_sf(team = team,geometry = st_geometrycollection() %>% st_sfc())
  }
  
  return(combined2)
}

#get csv file for positional data
get_positional_data <- function(row,time,
                                path = "C:/Users/David/OneDrive/Documents/Work/Thesis/Code/Data/test/positional"){
  #print(row)
  speed <- row["speedGroup"] %>%as.numeric()
  angle <- row["angleGroup"] %>% as.numeric()
  x <- floor((row["x"] %>% as.numeric)/x_bin_width)
  y <- floor((row["y"] %>% as.numeric)/y_bin_width)
  team <- ifelse(row["team"] == "l",1,-1) %>% as.numeric()
  
  filename <- paste0(path,"/time_",time,"/speed_",speed,"/angle_",angle,"/",x,"_",y,".csv.gz")
  
  get_kde <- function(mm){
    kde2d(mm$pux,mm$puy,
          h = c(ifelse(bandwidth.nrd(mm$pux) < 0.001, 0.1, bandwidth.nrd(mm$pux)),
                ifelse(bandwidth.nrd(mm$puy) < 0.001, 0.1, bandwidth.nrd(mm$puy))),
          n = c(x_segmentation,y_segmentation),
          lims = c(0,pitch_length,0,pitch_width))
  }
  
  readFileTC <- function(file) {
    out <- tryCatch({
        team*((read.csv(file) %>% get_kde())$z)
      },
      error=function(cond) {
        return(matrix(0L,nrow=20,ncol=14))
      },
      warning = function(cond){return(matrix(0L,nrow=20,ncol=14))}
    )    
    return(out)
  }
  readFileTC(filename) %>% return()
}

#calculate pitch control/dominant regions return sf object
calculate_pitch_control <- function(frame,model,nproc = 8,this.times = times){
  frame <- frame %>% subset(player!=1)
  if (isTRUE(all.equal(model,fujsug)) | isTRUE(all.equal(model,taki)) | length(model)>1){
    cl <- makeCluster(nproc)
    registerDoParallel(cl)
    partialRR <- foreach(i = 1:length(this.times),
                         .combine="c",
                         .packages = c('MASS','dplyr','sf','igraph'),
                         .export = ls(globalenv())) %dopar% partial(this.times[i],frame,model)
    stopCluster(cl)
    cl <- makeCluster(nproc)
    registerDoParallel(cl)
    combined <- foreach(i = 1:22,
                         .combine="rbind",
                         .packages = c("dplyr","sf"),
                        .export = ls(globalenv())) %dopar% form_syn(i,partialRR)
    stopCluster(cl)
    return(combined)
  }
  else if (model == "voronoi"){
    
    #plot voronoi tessselation
    st_voronoi_point <- function(points){
      ## points must be POINT geometry
      if(!all(st_geometry_type(points) == "POINT")){
        stop("Input not  POINT geometries")
      }
      g = st_combine(st_geometry(points)) # make multipoint
      v = st_voronoi(g)
      v = st_collection_extract(v)
      return(v[unlist(st_intersects(points, v))])
    }
    
    
    box <- c(xmin = 0, xmax = 107, ymax = 0, ymin = 70) %>%
      st_bbox() %>%
      st_as_sfc()
    voronoi <- one_frame[c('player','team','x','y')] %>%
      st_as_sf(coords= c( "x", "y" )) %>%
      st_voronoi_point() %>%
      st_intersection(box)
    voronoi <- st_sf(player = one_frame$player,
                     team = one_frame$team,
                     geometry = voronoi)
    return(voronoi)
  }
  else if (model == "ppc"){
    time_mat <- function(this.time){
      return(Reduce('+',plyr::alply(.data = frame,
                         .fun = get_positional_data,
                         .margins=1,
                         time=this.time)[1:22]))
    }
    
    discounted_sum <- function(mats,gamma){
      sum = mats[1]
      browser()
      for (i in 1:length(mats)){
        sum = sum + (gamma^i)*mats[i]
      }
      return(sum)
    }
    
    mat <- c(1,10,20,30,40,45) %>%
        lapply(FUN = time_mat) %>%
        #Reduce(f=function(x,y) x+0.9*y)
        Reduce(f='+')  
        #discounted_sum(gamma = 0.8)
    mat <- (mat+1)/2
    return(mat)
  }
}

#plot the ptch control/dominant regions
plot_pc <- function(pc,frame,sf=TRUE){
  geom_pc <- function(df){
    colfunc <- colorRampPalette(c(pal[4],pal[3],"white",pal[1],pal[2]))
    if(sf){
      return(scale_fill_manual(values=c(pal[2],pal[4]))+
               geom_sf(data=df,mapping = aes(fill=as.factor(team)),alpha=0.2,show.legend = F))
    }
    return(scale_fill_gradientn(colours=c(pal[4],pal[3],"white",pal[1],pal[2]),
                                values = c(0,0.49,0.5,0.51,1),
                                limits = c(0,1))+
             coord_fixed()+
             labs(x="",y="")+
             geom_raster(data = df %>% reshape2::melt(),
                         mapping = aes((Var1-0.5)*x_bin_width,(Var2-0.5)*y_bin_width,fill=value),
                         alpha=0.5,interpolate=TRUE))
  }
  return(soccerPitch() +
    scale_color_manual(values=c(pal[2],pal[4]))+
    labs(x="",y="")+
    geom_pc(pc)+
    geom_point(data = frame,
               mapping=aes(x=x,y=y,color=as.factor(team)),
               position = 'jitter',
               show.legend = F)+
    geom_segment(data = frame,
                 mapping=aes(x=x,xend = x+vX*scale,yend=y+vY*scale,y=y,color=as.factor(team)),
                 arrow = arrow(length = unit(0.1, "cm")),
                 show.legend = F))
}

#function to test algorithms
test <- function(path = "C:/Users/David/OneDrive/Documents/Work/Thesis/Code/Data/matches_formatted.zip",
                 file = "matches_formatted/cyrus2017-vs-Gliders2016/17-20170905234225-CYRUS_0-vs-Gliders2016_0.csv",
                 mm = "ppc"){
  con = unz(path)
  match <- read.csv(con)
  match <- match[,!(names(match) %in% c("X","index"))]
  close(con)
  one_frame = subset(match,frame == 4000 & player != 1)
  
  if (is.null(mm)){
    
  }
}

