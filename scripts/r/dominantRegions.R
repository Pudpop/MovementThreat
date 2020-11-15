
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
                                path = "C:/Users/David/OneDrive/Documents/Work/Thesis/github/data/positional"){
  #print(row)
  speed <- row["speedGroup"] %>%as.numeric()
  angle <- row["angleGroup"] %>% as.numeric()
  x <- floor((row["x"] %>% as.numeric)/x_bin_width)
  y <- floor((row["y"] %>% as.numeric)/y_bin_width)
  team <- ifelse(row["team"] == "l",1,-1) %>% as.numeric()
  
  #filename <- paste0(path,"/time_",time,"/speed_",speed,"/angle_",angle,"/",x,"_",y,".csv.gz")
  
  #get_kde <- function(mm){
  #  kde2d(mm$pux,mm$puy,
  #        h = c(ifelse(bandwidth.nrd(mm$pux) < 0.001, 0.1, bandwidth.nrd(mm$pux)),
  #              ifelse(bandwidth.nrd(mm$puy) < 0.001, 0.1, bandwidth.nrd(mm$puy))),
  #        n = c(x_segmentation,y_segmentation),
  #        lims = c(0,pitch_length,0,pitch_width))
  #}
  
  filename <- paste0(path,"/time",".hdf5")
  
  
  readFileTC <- function(file) {
    out <- tryCatch({
        temp <- h5read(file,paste0("time_",time))[,x*y_segmentation + y + 1,angle+1,speed+1] %>% 
                  matrix(nrow=x_segmentation,byrow=T)
        s <- sum(temp)
        if (s == 0){
          return(temp)
        }
        else{
          return(temp/s)
        }
        #team*((read.csv(file) %>% get_kde())$z)
      },
      error=function(cond) {
        print(cond)
        return(matrix(0L,nrow=x_segmentation,ncol=y_segmentation))
      },
      warning = function(cond){
        return(NA)
        #return(matrix(0L,nrow=x_segmentation,ncol=y_segmentation))
        }
    )    
    return(out)
  }
  readFileTC(filename) %>% return()
}

#calculate pitch control/dominant regions return sf object
calculate_pitch_control <- function(frame,model,nproc = 8,this.times = times){
  ball = frame %>% subset(player == 1)
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
    
    #voronoi tessselation
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
    
    cut_xy <- function(x,is.x=TRUE){
      if(is.x){
        return( floor((x %>% as.numeric)/x_bin_width))
      }
      return(floor((x %>% as.numeric)/y_bin_width))
    }
    
    ball_pos <- c((ball$x %>% cut_xy)+1,(ball$y %>% cut_xy(is.x=FALSE))+1)
    
    get_time <- function(time){
      #path <- "C:/Users/David/OneDrive/Documents/Work/Thesis/Data/positional"
      #if (ballDo){
      #  path <- "C:/Users/David/OneDrive/Documents/Work/Thesis/Data/ball"
      #}
      
      #temp <- h5read(paste0(path,"/time",".hdf5"),
      #               paste0("time_",time))
      
      get_position <- function(row){
        if (row[4] == 'r'){
          orig <- row[3] %>% as.integer
          orig.x <- floor(orig/y_segmentation)
          orig.x <- x_segmentation - orig.x
          orig.y <- orig%%y_segmentation
          orig.y <- y_segmentation - orig.y
          orig <- (orig.x-1)*y_segmentation + orig.y
          mattt <- (temp[,orig %>% as.integer,
                          row[2]  %>% as.integer,
                          row[1]  %>% as.integer]) %>% 
                   matrix(nrow=x_segmentation,byrow=T)
          mattt <- mattt[,c(ncol(mattt):1)]
          mattt <- mattt[c(nrow(mattt):1),]
          summatt <- sum(mattt)
          if (summatt >0){
            return(mattt/(summatt))  
          }
          return(matrix(0,nrow=x_segmentation,ncol=y_segmentation))
        }
        mattt <- temp[,(row[3]%>% trimws %>% as.integer+1),
                      row[2]  %>% as.integer,
                      row[1]  %>% as.integer]%>% 
          matrix(nrow=x_segmentation,byrow=T)
        summatt <- sum(mattt)
        if (summatt >0){
          return(mattt/(summatt))  
        }
        return(matrix(0,nrow=x_segmentation,ncol=y_segmentation))
      }
      ball_mat <- matrix(0,nrow=x_segmentation,ncol=y_segmentation)
      
      path <- "C:/Users/David/OneDrive/Documents/Work/Thesis/Data/ball"
      temp <- h5read(paste0(path,"/time",".hdf5"),
                     paste0("time_",time))
      mats <- get_position(c(ball$speedGroup,
                             ball$angleGroup,
                             (ball_pos[1]-1)*y_segmentation + ball_pos[2]-1,
                             "b"))
      ball_mat <- mats
      h5closeAll()
      path <- "C:/Users/David/OneDrive/Documents/Work/Thesis/Data/positional"
      temp <- h5read(paste0(path,"/time",".hdf5"),
                     paste0("time_",time))
      
      mats <- (frame$x %>% cut_xy) * y_segmentation + frame$y %>% cut_xy(is.x=F)
      mats <- data.frame(speedGroup = frame$speedGroup %>% as.integer,
                        angleGroup = frame$angleGroup %>% as.integer,
                        position = mats %>% as.integer,
                        team = frame$team) %>% 
          apply(FUN = get_position,MARGIN=1)
      h5closeAll()
      return(list(ball = ball_mat,players = mats))
    }
    
    dist_mat <- function(row,ball){
      d = dist(matrix(c(row[1:2],ball),byrow=T,nrow=2),method="manhattan")
      if (d == 0){
        return(1)
      }
      if (d != dist(matrix(c(row[1:2],ball),byrow=T,nrow=2))){
        x_dist <- abs(row[1]-ball[1])
        y_dist <- abs(row[2]-ball[2])
        return(min(ceiling(max(c(x_dist,y_dist))),7))  
      }
      return(min(ceiling(d),7))
    }
    
    get_cell_influence <- function(coords,mm){
      time <- coords[3] %>% as.integer
      x <- coords[1] %>% as.integer
      y <- coords[2] %>% as.integer
      
      mm <- mm[[time]]$players
      
      l_inf <- rep(0,11)
      r_inf <- rep(0,11)
      
      for (i in 1:22){
        team <- frame$team[i]
        player <- (frame$player[i]-2)%%11+1
        temp <- mm[,i] %>% matrix(nrow=x_segmentation,ncol=y_segmentation)
        max_prob <- max(temp)
        temp <- temp/max_prob 
        
        if (team == "l"){
          l_inf[player %>% as.integer] <- temp[x,y]
        }
        else{
          r_inf[player %>% as.integer] <- temp[x,y]
        }
        
      }
      return(1/(1+exp(sum(r_inf)-sum(l_inf))))
    }
    
    get_ball_probs <- function(coords,mm){
      time <- coords[3] %>% as.integer
      x <- coords[1] %>% as.integer
      y <- coords[2] %>% as.integer
      
      return(mm[[time]]$ball[x,y])
    }
    
    pos_mat <- matrix(0,nrow=x_segmentation,ncol=y_segmentation) %>%
      melt %>%
      apply(FUN = dist_mat,MARGIN=1,ball = ball_pos) %>%
      matrix(nrow=x_segmentation,ncol=y_segmentation) %>%
      melt
    
    start <- Sys.time()
    mats <- c(0,1,2,3,4,5,6) %>%
              lapply(FUN=get_time)
    print(Sys.time()-start)
    start <- Sys.time()
    mat <- pos_mat%>%
              apply(FUN = get_cell_influence,MARGIN=1,mm = mats) %>%
              matrix(nrow=x_segmentation,ncol=y_segmentation)
    print(Sys.time()-start)
    #start <- Sys.time()
    #mats <- c(0,1,2,3,4,5,6) %>%
    #          lapply(FUN = get_time,ballDo=TRUE)
    #print(Sys.time()-start)
    start <- Sys.time()
    bal <- pos_mat %>%
              apply(FUN = get_ball_probs,MARGIN=1,mm = mats) %>%
              matrix(nrow=x_segmentation,ncol=y_segmentation)
    print(Sys.time()-start)
    return(list(players = mat,ball = bal))
  }
}

#plot the pitch control/dominant regions
plot_pc <- function(pc,frame,sf=FALSE,threat = FALSE){
  if (threat){
    
    ds <- sum(pc)
    d <- data.frame(s=ds)
    return(soccerPitch() +
      scale_color_manual(values=c(pal[2],pal[4]))+
      labs(x="",y="")+
      geom_point(data = frame,
                 mapping=aes(x=x,y=y,color=as.factor(team)),
                 position = 'jitter',
                 show.legend = F)+
      geom_segment(data = frame,
                   mapping=aes(x=x,xend = x+vX*scale,yend=y+vY*scale,y=y,color=as.factor(team)),
                   arrow = arrow(length = unit(0.1, "cm")),
                   show.legend = F)+
      coord_fixed()+
      labs(x="",y="")+
        scale_fill_gradientn(colours=c("white",pal[2]),
                             values = c(0,1),
                             limits = c(0,1))+
      geom_raster(data = pc %>% reshape2::melt(),
                mapping = aes((Var1-0.5)*x_bin_width,(Var2-0.5)*y_bin_width,fill=value),
                alpha=0.5,interpolate=TRUE,show.legend = F)+
      geom_segment(data = d,
                   mapping=aes(x = 117,xend=117,y = 10,yend = 10+(s*50)))+
      geom_point(data = d,
                 mapping=aes(x = 117,y = 10+(s*50)),
                 size=1.5)+
      geom_text(data = d,
                mapping=aes(x = 122,y = 10+(s*50),label =round(ds,2)),
                size=3)+
      geom_text(data = d,
                mapping=aes(x = 118,y = 65,label = "Threat"),
                size=4))
  }
  geom_pc <- function(df){
    colfunc <- colorRampPalette(c(pal[4],pal[3],"white",pal[1],pal[2]))
    if(sf){
      return(list(scale_fill_manual(values=c(pal[2],pal[4])),
               geom_sf(data=df,mapping = aes(fill=as.factor(team)),alpha=0.2,show.legend = F)))
    }
    return(list(scale_fill_gradientn(colours=c(pal[4],pal[3],"white",pal[1],pal[2]),
                                values = c(0,0.49,0.5,0.51,1),
                                limits = c(0,1)),
             coord_fixed(),
             labs(x="",y=""),
             geom_raster(data = df %>% reshape2::melt(),
                         mapping = aes((Var1-0.5)*x_bin_width,(Var2-0.5)*y_bin_width,fill=value),
                         alpha=0.5,interpolate=TRUE),show.legend=F))
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
test <- function(path = "C:/Users/David/OneDrive/Documents/Work/Thesis/data/matches_formatted.zip",
                 file = "matches_formatted/cyrus2017-vs-Gliders2016/17-20170905234225-CYRUS_0-vs-Gliders2016_0.csv",
                 mm = "ppc"){
  print("load") 
  con = unz(description = path,filename = file)
  match <- read.csv(con)
  #close(con)
  match <- match[,!(names(match) %in% c("X","index"))]
  one_frame = subset(match,frame == 4000 & player != 1)
  rm(match)
   
  print("start")
  return(calculate_pitch_control(one_frame,mm) %>% plot_pc(one_frame))
}


run_threat <- function(){
  path = "C:/Users/David/OneDrive/Documents/Work/Thesis/data/matches_formatted.zip"
  
  pathO <- "C:/Users/David/OneDrive/Documents/Work/Thesis/Data/positional"
  playersFiles <- list()
  playersFiles[[1]] <- h5read(paste0(pathO,"/time",".hdf5"),
                              paste0("time_",0))
  playersFiles[[2]] <- h5read(paste0(pathO,"/time",".hdf5"),
                              paste0("time_",1))
  playersFiles[[3]] <- h5read(paste0(pathO,"/time",".hdf5"),
                              paste0("time_",2))
  playersFiles[[4]] <- h5read(paste0(pathO,"/time",".hdf5"),
                              paste0("time_",3))
  playersFiles[[5]] <- h5read(paste0(pathO,"/time",".hdf5"),
                              paste0("time_",4))
  playersFiles[[6]] <- h5read(paste0(pathO,"/time",".hdf5"),
                              paste0("time_",5))
  playersFiles[[7]] <- h5read(paste0(pathO,"/time",".hdf5"),
                              paste0("time_",6))
  
  pathO <- "C:/Users/David/OneDrive/Documents/Work/Thesis/Data/ball"
  ballFiles <- list()
  ballFiles[[1]] <- h5read(paste0(pathO,"/time",".hdf5"),
                           paste0("time_",0))
  ballFiles[[2]] <- h5read(paste0(pathO,"/time",".hdf5"),
                           paste0("time_",1))
  ballFiles[[3]] <- h5read(paste0(pathO,"/time",".hdf5"),
                           paste0("time_",2))
  ballFiles[[4]] <- h5read(paste0(pathO,"/time",".hdf5"),
                           paste0("time_",3))
  ballFiles[[5]] <- h5read(paste0(pathO,"/time",".hdf5"),
                           paste0("time_",4))
  ballFiles[[6]] <- h5read(paste0(pathO,"/time",".hdf5"),
                           paste0("time_",5))
  ballFiles[[7]] <- h5read(paste0(pathO,"/time",".hdf5"),
                           paste0("time_",6))
  
  match_numbers <- events$matchNo %>% unique
  
  cum_threat <- data.frame(matchNo = match_numbers,
                           xG = 0,
                           xG_left = 0,
                           xG_right = 0,
                           xT = 0,
                           xT_left = 0,
                           xT_right = 0,
                           xP = 0,
                           xP_left = 0,
                           xP_right = 0,
                           xI = 0,
                           xI_left = 0,
                           xI_right = 0,
                           score_left = 0,
                           score_right = 0)
  
  get_xT <- function(event){
    event <- as.data.frame(t(event))
    return(xT[event$xBlock %>% as.integer,event$yBlock %>% as.integer])
  }
  
  get_xP <- function(frame){
    ball = frame %>% subset(player == 1)
    frame <- frame %>% subset(player!=1)
    weights <- c(0.3,0.45,0.75,1,1,1,1)
    
    cut_xy <- function(x,is.x=TRUE){
      if(is.x){
        return( floor((x %>% as.numeric)/x_bin_width))
      }
      return(floor((x %>% as.numeric)/y_bin_width))
    }
    
    ball_pos <- c((ball$x %>% cut_xy)+1,(ball$y %>% cut_xy(is.x=FALSE))+1)
    
    
    get_time <- function(time){
      get_position <- function(row){
        if (row[4] == 'r'){
          orig <- row[3] %>% as.integer
          orig.x <- floor(orig/y_segmentation)
          orig.x <- x_segmentation - orig.x
          orig.y <- orig%%y_segmentation
          orig.y <- y_segmentation - orig.y
          orig <- (orig.x-1)*y_segmentation + orig.y
          mattt <- (temp[,orig %>% as.integer,
                         row[2]  %>% as.integer,
                         row[1]  %>% as.integer]) %>% 
            matrix(nrow=x_segmentation,byrow=T)
          mattt <- mattt[,c(ncol(mattt):1)]
          mattt <- mattt[c(nrow(mattt):1),]
          summatt <- sum(mattt)
          if (summatt >0){
            return(mattt/(summatt))  
          }
          return(matrix(0,nrow=x_segmentation,ncol=y_segmentation))
        }
        mattt <- temp[,(row[3]%>% trimws %>% as.integer+1),
                      row[2]  %>% as.integer,
                      row[1]  %>% as.integer]%>% 
          matrix(nrow=x_segmentation,byrow=T)
        summatt <- sum(mattt)
        if (summatt >0){
          return(mattt/(summatt))  
        }
        return(matrix(0,nrow=x_segmentation,ncol=y_segmentation))
      }
      ball_mat <- matrix(0,nrow=x_segmentation,ncol=y_segmentation)
      
      temp <- ballFiles[[time+1]]
      mats <- get_position(c(ball$speedGroup,
                             ball$angleGroup,
                             (ball_pos[1]-1)*y_segmentation + ball_pos[2]-1,
                             "b"))
      ball_mat <- mats
      return(ball_mat*weights[time+1])
    }
    
    mats <- c(0,1,2,3,4,5,6) %>%
      lapply(FUN=get_time)
    lon <- seq(0, pitch_length, length.out = x_segmentation)
    lat <- seq(0, pitch_width, length.out = y_segmentation)
    grd <- expand.grid(x = lon, y = lat)
    
    grd_sf  <-  st_as_sf(grd, coords = c("x", "y"), agr = "constant")
    
    
    #voronoi tessselation
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
    voronoi <- frame[c('player','team','x','y')] %>%
      st_as_sf(coords= c( "x", "y" )) %>%
      st_voronoi_point() %>%
      st_intersection(box)
    voronoi <- st_sf(player = frame$player,
                     team = frame$team,
                     geometry = voronoi,
                     value = (frame$team == 1) %>% as.numeric)
    
    r <- raster(voronoi,nrows = y_segmentation,ncols= x_segmentation)
    
    count_left <- voronoi %>%
      fasterize(raster =r, field = "value", fun = "sum") %>%
      as.matrix %>%
      t
    
    voronoi$value <- 1-voronoi$value
    
    count_right <- voronoi %>%
      fasterize(raster =r, field = "value", fun = "sum")%>%
      as.matrix %>%
      t
    
    #browser()
    
    #left_team <- voronoi %>%
    #  subset(team == "l") %>%
    #  st_union
    #right_team <- voronoi %>%
    #  subset(team == "r") %>%
    #  st_union
    #browser()
    #start <- Sys.time()
    #count_left <- grd_sf$geometry %>%
    #  lapply(FUN = st_intersects,y = left_team,sparse = FALSE) %>%
    #  unlist %>%
    #  as.numeric %>%
    #  matrix(nrow = x_segmentation,ncol=y_segmentation)
    
    #count_right <- grd_sf$geometry %>%
    #  lapply(FUN = st_within,y = right_team,sparse = FALSE) %>%
    #  unlist %>%
    #  as.numeric %>%
    #  matrix(nrow = x_segmentation,ncol=y_segmentation)

    #print(Sys.time()-start)
    left <- Reduce('+',mats)*count_left*xT
    right <- Reduce('+',mats)*count_right*xT_right
    
    c(left,right) %>% return()
  }
  
  get_xI <- function(frame){
    ball = frame %>% subset(player == 1)
    frame <- frame %>% subset(player!=1)
    
    cut_xy <- function(x,is.x=TRUE){
      if(is.x){
        return( floor((x %>% as.numeric)/x_bin_width))
      }
      return(floor((x %>% as.numeric)/y_bin_width))
    }
    
    ball_pos <- c((ball$x %>% cut_xy)+1,(ball$y %>% cut_xy(is.x=FALSE))+1)
    
    get_time <- function(time){
      get_position <- function(row){
        if (row[4] == 'r'){
          orig <- row[3] %>% as.integer
          orig.x <- floor(orig/y_segmentation)
          orig.x <- x_segmentation - orig.x
          orig.y <- orig%%y_segmentation
          orig.y <- y_segmentation - orig.y
          orig <- (orig.x-1)*y_segmentation + orig.y
          mattt <- (temp[,orig %>% as.integer,
                         row[2]  %>% as.integer,
                         row[1]  %>% as.integer]) %>% 
            matrix(nrow=x_segmentation,byrow=T)
          mattt <- mattt[,c(ncol(mattt):1)]
          mattt <- mattt[c(nrow(mattt):1),]
          summatt <- sum(mattt)
          if (summatt >0){
            return(mattt/(summatt))  
          }
          return(matrix(0,nrow=x_segmentation,ncol=y_segmentation))
        }
        mattt <- temp[,(row[3]%>% trimws %>% as.integer+1),
                      row[2]  %>% as.integer,
                      row[1]  %>% as.integer]%>% 
          matrix(nrow=x_segmentation,byrow=T)
        summatt <- sum(mattt)
        if (summatt >0){
          return(mattt/(summatt))  
        }
        return(matrix(0,nrow=x_segmentation,ncol=y_segmentation))
      }
      ball_mat <- matrix(0,nrow=x_segmentation,ncol=y_segmentation)

      temp <- ballFiles[[time+1]]
      mats <- get_position(c(ball$speedGroup,
                             ball$angleGroup,
                             (ball_pos[1]-1)*y_segmentation + ball_pos[2]-1,
                             "b"))
      ball_mat <- mats
      
      temp <- playersFiles[[time+1]]
      
      mats <- (frame$x %>% cut_xy) * y_segmentation + frame$y %>% cut_xy(is.x=F)
      mats <- data.frame(speedGroup = frame$speedGroup %>% as.integer,
                         angleGroup = frame$angleGroup %>% as.integer,
                         position = mats %>% as.integer,
                         team = frame$team) %>% 
        apply(FUN = get_position,MARGIN=1)

      return(list(ball = ball_mat,players = mats))
    }
    
    get_time_influence <- function(time,mm,teams){
      mm <- mm[[time+1]]
      play <- (mm$players %*% diag(teams)) %>%
        rowSums %>%
        (function(x){1/(1+exp(-x))}) %>%
        matrix(nrow=x_segmentation,ncol=y_segmentation)
      return(play)
    }

    add_min <- ifelse(frame$team == "l",
                      1,
                      -1)
    
    weights <- c(0.3,0.45,0.75,1,1,1,1)
    
    mats <- c(0,1,2,3,4,5,6) %>%
      lapply(FUN=get_time)
    
    .list <- c(0,1,2,3,4,5,6) %>%
      lapply(FUN = get_time_influence,mm = mats,teams = add_min) 
    .rlist <- .list %>%
      lapply(FUN = function(x){1-x})
    for (i in  1:length(.list)){
      .list[[i]] <- mats[[i]]$ball * .list[[i]] * weights[i]
      .rlist[[i]] <- mats[[i]]$ball * .rlist[[i]] * weights[i]
    }

    left <- (Reduce('+',.list)*xT) %>% sum
    right <- (Reduce('+',.rlist) * xT_right) %>% sum
    
    c(left,right) %>% return()
  }
  
  for (number in match_numbers){
    print(number)
    if (as.integer(number) == 1){
      break
    }
    file <- grep(paste0('.*/',number,'-'),unzip(path, list=TRUE)$Name,value=T)
    con = unz(description = path,filename = file)
    match <- read.csv(con)
    match <- match[,!(names(match) %in% c("X","index"))]
    
    #get match info
    score_left <- match[1,c("final_score_left")]
    score_right <- match[1,c("final_score_right")]
    
    #find total xG for game
    xG_left <- events %>%
      subset(matchNo == number & action == "shot" & team == "l") %>%
      subset(select = c("x","y"))
    xG_left <- predict(xG,xG_left)
    xG_left <- (exp(xG_left)/(1+exp(xG_left))) %>% sum
    
    xG_right <- events %>%
      subset(matchNo == number & action == "shot" & team == "r") %>%
      subset(select = c("x","y"))
    xG_right <- predict(xG,xG_right)
    xG_right <- (exp(xG_right)/(1+exp(xG_right))) %>% sum
    
    xG_tot <- xG_left+xG_right
    
    #find total xT for game
    xT_left <- events %>%
      subset(matchNo == number & team == "l") %>%
      apply(FUN = get_xT, MARGIN = 1) %>%
      sum
    
    xT_right <- events %>%
      subset(matchNo == number & team == "r") %>%
      apply(FUN = get_xT, MARGIN = 1) %>%
      sum
      
    xT_tot <- xT_left + xT_right
      
    xP <- match %>%
      subset(state == " play_on")
    xP <- xP %>% split(xP$frame)
    xP <- xP %>%
      pblapply(FUN = get_xP)
    
    xP_left <- xP %>%
      lapply(FUN = function(x){return(x[1])}) %>%
      unlist %>%
      sum
    xP_right <- xP %>%
      lapply(FUN = function(x){return(x[2])}) %>%
      unlist %>%
      sum
    
    xP_tot <- xP_left + xP_right

    xI <- match %>%
      subset(state == " play_on")
    xI <- xI %>% split(xI$frame)
    xI <- xI %>%
      pblapply(FUN = get_xI)
      
    xI_left <- xI %>%
      lapply(FUN = function(x){return(x[1])}) %>%
      unlist %>%
      sum
    xI_right <- xI %>%
      lapply(FUN = function(x){return(x[2])}) %>%
      unlist %>%
      sum
    
    xI_tot <- xI_left + xI_right
    cum_threat[cum_threat$matchNo == number,c(2:15)] <- c(xG_tot,xG_left,xG_right,
                                                          xT_tot,xT_left,xT_right,
                                                          xP_tot,xP_left,xP_right,
                                                          xI_tot,xI_left,xI_right,
                                                          score_left,score_right)
    
  }
  h5closeAll()
  return(cum_threat)
}


df <- run_threat()



path = "C:/Users/David/OneDrive/Documents/Work/Thesis/data/matches_formatted.zip"
#file = "matches_formatted/Gliders2016-vs-ri-one/246-20170904134510-Gliders2016_6-vs-Ri-one_0.csv"
file = "matches_formatted/cyrus2017-vs-Gliders2016/13-20170905233758-CYRUS_3-vs-Gliders2016_8.csv"
con = unz(description = path,filename = file)
match <- read.csv(con)
close(con)

match <- match[,!(names(match) %in% c("X","index"))]
one_frame = subset(match,frame == 1820)

temp <- calculate_pitch_control(one_frame,"ppc") %>% plot_pc(one_frame %>% subset(player != 1))
temp <- calculate_pitch_control(one_frame ,"ppc")
temp <- 1-temp
temp[temp <0.5] = 0
#temp[temp <=0.5 & temp !=0] = 1-temp[temp <=0.5 & temp !=0]

temp <- temp %>% matrix(ncol=y_segmentation,nrow=x_segmentation)

ball <- subset(match,frame == 1820 & player == 1)[,c("x","y")] %>% as.vector()
ball[1] <- floor(ball[1]/x_bin_width)
ball[2] <- floor(ball[2]/y_bin_width)
ball_ind <- ((ball[1]-1)*y_segmentation +ball[2]) %>% as.numeric

vec <- h5read(filename,"probs")[ball_ind,]
mat = matrix(0,nrow=40,ncol=28)
if (!(sum(vec) == 0)){
  mat <- (vec/sum(vec)) %>%
    matrix(nrow=40,ncol=28)
}
(1-temp$players) %>% plot_pc(one_frame %>% subset(player!=1),threat = FALSE)
(temp$players * xT) %>% plot_pc(one_frame %>% subset(player!=1),threat = TRUE)
(temp$ball * temp$players * xT) %>% plot_pc(one_frame %>% subset(player!=1),threat = TRUE)
(mat) %>% plot_pc(one_frame,threat = TRUE)

plot_threat <- function(poss,pc,threat,frame,team){
  if (team == "r"){
    threat <- threat[,c(ncol(threat):1)]
    threat <- threat[c(nrow(threat):1),]
  }
  else{
    pc <- 1-pc
  }
  pc[pc < 0.5] = 0
  
  th <- (poss)*pc*threat
  ds <- sum(th)
  d <- data.frame(s=ds)
  return(soccerPitch() +
           scale_color_manual(values=c(pal[5],pal[2],pal[4]))+
           labs(x="",y="")+
           geom_point(data = frame,
                      mapping=aes(x=x,y=y,color=as.factor(team)),
                      position = 'jitter',
                      show.legend = F)+
           geom_segment(data = frame,
                        mapping=aes(x=x,xend = x+vX*scale,yend=y+vY*scale,y=y,color=as.factor(team)),
                        arrow = arrow(length = unit(0.1, "cm")),
                        show.legend = F)+
           geom_text(data = frame,
                     mapping=aes(x=x,y=y,label = action),
                     show.legend = F)+
           coord_fixed()+
           labs(x="",y="")+
           scale_fill_gradientn(colours=c(pal[2],"white",pal[4]),
                                values = c(0,0.5,1),
                                limits = c(0,1))+
           geom_raster(data = pc %>% reshape2::melt(),
                       mapping = aes((Var1-0.5)*x_bin_width,(Var2-0.5)*y_bin_width,fill=value),
                       alpha=0.5,interpolate=TRUE,show.legend = F)+
           geom_segment(data = d,
                        mapping=aes(x = 117,xend=117,y = 10,yend = 10+(s*50)))+
           geom_point(data = d,
                      mapping=aes(x = 117,y = 10+(s*50)),
                      size=1.5)+
           geom_text(data = d,
                     mapping=aes(x = 122,y = 10+(s*50),label =round(ds,2)),
                     size=3)+
           geom_text(data = d,
                     mapping=aes(x = 118,y = 65,label = "Threat"),
                     size=4))
}

frames_for_ani <- (shots %>% subset(matchNo == 13 & recPlayer == "goal" & team == "l"))$frame %>% 
  lapply(FUN = function(x){return(c((x-30):(x+5)))}) %>% 
  unlist
frames_for_ani <- cbind(frames_for_ani,
                        rep((shots %>% 
                               subset(matchNo == 13 & recPlayer == "goal" & 
                                        frame %in% frames_for_ani))$team,
                            each=41)) %>%
  as.data.frame()

setwd("C:/Users/David/OneDrive/Documents/Work/Thesis/github/fig/animation")
for (i in 1:nrow(frames_for_ani)){
  print(frames_for_ani[i,])
  frameNo <- frames_for_ani[i,1]
  team <- frames_for_ani[i,2]
  one_frame = subset(match,frame == frameNo)
  temp <- calculate_pitch_control(one_frame ,"ppc")
  pl <- plot_threat(temp$ball,temp$players,xT,one_frame,team)
  ggsave(paste0("frame_",frameNo,".png"),pl)
}


anim <- soccerPitch() +
  scale_color_manual(values=c(pal[5],pal[2],pal[4]))+
  labs(x="",y="")+
  coord_fixed()+
  geom_point(data = match %>% subset(frame %in% seq(2900,3050,by=1)),
             mapping=aes(x=x,y=y,color=as.factor(team)),
             position = 'jitter',
             show.legend = F)+
  geom_segment(data = match %>% subset(frame %in% seq(2900,3050,by=1)),
               mapping=aes(x=x,xend = x+vX*scale,yend=y+vY*scale,y=y,color=as.factor(team)),
               arrow = arrow(length = unit(0.1, "cm")),
               show.legend = F)+
  geom_text(data = match %>% subset(frame %in% seq(2900,3050,by=1)),
            mapping=aes(x=x,y=y,color=as.factor(team),label = action),
            position = 'jitter',
            show.legend = F)+
  transition_states(states = frame)

animate(anim,nframes=150,fps=5)

#test()

#path <- "C:/Users/David/OneDrive/Documents/Work/Thesis/github/data/positional"
#temp <- h5read(paste0(path,"/time.hdf5"),
#               "time_0")

#temp[,,,] %>% sum
#h5closeAll()

player_to_state <- function(row,
){
  get_info <- function(row){
    return(c(row$matchNo[1],row$frame[1]))
  }
  
  read_file <- function(info){
    matchNo <- info[1]
    frameNo <- info[2]
    file <- grep(paste0('.*/',matchNo,'-'),unzip(path, list=TRUE)$Name,value=T)
    con = unz(description = path,filename = file)
    match <- read.csv(con)
    #close(con)
    match <- match[,names(match) %in% colnames(row)]
    one_frame = subset(match,frame == frameNo)
    
    
    
    return(one_frame)
  }
  
  
  return(read_file(get_info(row)))
}