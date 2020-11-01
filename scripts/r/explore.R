df <- reader()
match <- subset(df,matchID==3)

centroid_hull_plot <- function(match,palette = c(pal[9],pal[2],pal[4])){
  #find convex hulls of each team ignoring goalkeepers
  pos <- match %>%
    group_by(team, player) %>%
    dplyr::summarise(x.mean = mean(x), y.mean = mean(y)) %>%
    ungroup() %>%
    mutate(team = as.factor(team), player = as.factor(player)) %>%
    as.data.frame()
  
  cent <- pos %>%
    subset(!(player %in%  c(which.min(pos$x.mean),which.max(pos$x.mean))))%>%
    group_by(team) %>%
    dplyr::summarise(centX = mean(x.mean), centY = mean(y.mean)) %>%
    ungroup() %>%
    mutate(team = as.factor(team)) %>%
    as.data.frame()
  
  data_hull <- pos %>%
    filter(!(player %in% c(which.min(pos$x.mean),which.max(pos$x.mean)))) %>%
    group_by(team)%>%
    nest() %>%
    mutate(
      hull = map(data, ~ with(.x, chull(x.mean, y.mean))),
      out = map2(data, hull, ~ .x[.y,,drop=FALSE])
    )%>%
    dplyr::select(-data)%>%
    unnest(cols= c(hull,out)) %>%
    suppressMessages() %>%
    data.frame()
  
  levels(data_hull$team) <- c("Ball","Left Team","Right Team")
  levels(pos$team) <- c("Ball","Left Team","Right Team")
  levels(cent$team) <- c("Ball","Left Team","Right Team")
  
  p <- soccerPitch() +
    geom_polygon(data = data_hull, aes(x.mean, y.mean,fill = team,group=team), alpha = 0.4) +
    geom_point(data = pos, aes(x.mean, y.mean, color=team),stroke = 1.5)+
    geom_point(data = cent,aes(centX,centY, color=team),shape=5,stroke=2 )+
    scale_color_manual(values = palette) +
    scale_fill_manual(values = palette)+
    coord_fixed()+
    labs(x="",y="",color = "Team",fill="Team")
  
  return(p)
}

centroid_hull_plot(match)

#CTM
library(reshape2)
#determine possession at all frames
possession <- function(match){
  get_mins <- function(frame){
    distances <- (frame[,c('x','y')] %>%
                    as.matrix() %>%
                    dist(method="euclidean") %>%
                    as.matrix())[,1]
    return(which.min(distances[2:23])+1)
  }
  this.match <- match %>% 
    subset(frame %% 10 == 0)
  df <- this.match %>% 
    split(f = this.match$frame) %>%
    lapply(FUN = get_mins) %>%
    unlist() %>%
    as.data.frame()
  ctm <- data.frame(cbind(df,seq(1,nrow(df))))
  colnames(ctm) <- c("player","frame")
  ctm$team <- ifelse(ctm$player < 13,0,1)
  return(ctm)
}

centroid <- function(match){
  get_mins <- function(frame){
    cent <- frame %>%
      subset(!(player==1))%>%
      group_by(team) %>%
      dplyr::summarise(centX = mean(x), centY = mean(y),.groups = 'drop') %>%
      ungroup() %>%
      mutate(team = as.factor(team)) %>%
      as.data.frame()
    diffL <- 107 - subset(cent,team=="l")$centX
    diffR <- subset(cent,team=="r")$centX
    con <- c(diffL,diffR)
    return(which.min(con)-1)
  }
  this.match <- match %>% 
    subset(frame %% 10 == 0)
  df <- this.match %>% 
    split(f = this.match$frame) %>%
    lapply(FUN = get_mins) %>%
    unlist() %>%
    as.data.frame()
  
  this.ctm <- data.frame(cbind(df,seq(1,nrow(df))))
  colnames(this.ctm) <- c("team","frame")
  return(this.ctm)
}

ctm<-function(match,FUN = possession){
  
  ctm <- FUN(match)
  
  nframes <- nrow(ctm)
  
  
  sub <- seq(1,nframes) %>% as.matrix()

  this.ma <- function(n,x){stats::filter(x, rep(1/n, n), sides = 2) %>% round()}
  
  sum<-apply(FUN=this.ma,MARGIN=1,X=sub,x=ctm$team) %>%
          reshape2::melt() %>% 
          stats::setNames(c('x','y','team'))

  return(sum)
}

ctm_plot <- function(ct){
  ct$team <- as.factor(ct$team)
  levels(ct$team) <- c("Left","Right")
  
  return(geom_tile(data = ct %>% na.exclude(),mapping =aes(x,y,fill=team)))
}

pos <- match %>%
        ctm(FUN=possession)
cent <- match %>%
        ctm(FUN=centroid)
p1 <- ggplot()+
  ctm_plot(pos)+
  geom_vline(data = subset(match,state %in% c(" goal_l"," goal_r") & player ==1),
             aes(xintercept = frame/10,
                 color = state),show.legend = F)+
  fte_theme()+
  scale_fill_manual(values = c(pal[2],pal[3]))+
  scale_color_manual(values = c(pal[2],pal[4]))+
  labs(fill = "Possession",x = "Seconds Passed During Game",y="Range of Average in Seconds")+
  theme(legend.position="bottom")+
  guides(fill=guide_legend(title.vjust = 0.8))
p2 <- ggplot()+
  ctm_plot(cent)+
  geom_vline(data = subset(match,state %in% c(" goal_l"," goal_r") & player ==1),
             aes(xintercept = frame/10,
                 color = state),show.legend = F)+
  fte_theme()+
  scale_fill_manual(values = c(pal[2],pal[3]))+
  scale_color_manual(values = c(pal[2],pal[4]))+
  labs(fill = "Territorial Advantage",x = "Seconds Passed During Game",y="Range of Average in Seconds")+
  theme(legend.position="bottom")+
  guides(fill=guide_legend(title.vjust = 0.8))
p2
ggarrange(p1,p2,nrow=1,ncol=2)

#average pass positions

#plot individual paths
soccerPitch()+
  geom_path(data=subset(match,frame <2000 & team == "l"),
            mapping=aes(x=x,y=y,colour=player %>% as.factor()),
            lwd = 1,show.legend = F) +
  scale_color_manual(values=pal)+
  fte_theme() +
  coord_fixed()+
  labs(x="",y="")

soccerPitch()+
  geom_path(data=subset(match,frame %% 8 == 0 & player %in% c(5,6,14,15)),
            aes(x=x,y=y,colour=player %>% as.factor(),linetype=team),
            lwd = 1,show.legend=F) +
  scale_color_manual(values=pal)+
  fte_theme() +
  coord_fixed()+
  labs(x="",y="")


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
