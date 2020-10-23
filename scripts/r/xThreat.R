#read in events
events <- read.csv("C:/Users/David/OneDrive/Documents/Work/Thesis/Code/Data/events.csv")
shots <- events %>% subset(action == "shot")
moves <- events %>% subset(action %in% c("pass","dribble"))
dribbles <- moves %>% subset(action == "dribble")
passes <- moves %>% subset(action == "pass")
