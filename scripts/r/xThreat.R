#read in events
events <- read.csv("C:/Users/David/OneDrive/Documents/Work/Thesis/Data/events.csv")
shots <- events %>% subset(action == "shot")
moves <- events %>% subset(action %in% c("pass","dribble"))
dribbles <- events %>% subset(action == "dribble")
passes <- events %>% subset(action == "pass")

path = "C:/Users/David/OneDrive/Documents/Work/Thesis/Data/matches_formatted"
file = "/cyrus2017-vs-Gliders2016/13-20170905233758-CYRUS_3-vs-Gliders2016_8.csv"
match <- read.csv(paste0(path,file))
match <- match[,!(names(match) %in% c("X","index"))]
events <- subset(match, action %in% c("shot","pass","dribble"))
shots <- match %>% subset(action == "shot")
soccerPitch() + 
  geom_point(data = shots ,mapping = aes(x=x,y=y,alpha = recPlayer == "goal"))+
  coord_fixed()
