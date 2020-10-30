#read in events
events <- read.csv("C:/Users/David/OneDrive/Documents/Work/Thesis/Data/events.csv")
shots <- events %>% subset(action == "shot")
moves <- events %>% subset(action %in% c("pass","dribble"))
dribbles <- moves %>% subset(action == "dribble")
passes <- moves %>% subset(action == "pass")

path = "C:/Users/David/OneDrive/Documents/Work/Thesis/data/matches_formatted.zip"
file = "matches_formatted/cyrus2017-vs-Gliders2016/13-20170905233758-CYRUS_3-vs-Gliders2016_8.csv"

con = unz(description = path,filename = file)
match <- read.csv(con)
#close(con)
match <- match[,!(names(match) %in% c("X","index"))]
one_frame = subset(match,frame == 4000 & player != 1)