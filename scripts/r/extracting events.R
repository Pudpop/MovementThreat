## R code to extract every known game state of the Robo cup match and convert said states into events

rm(list = ls())

library(sqldf)

setwd("D:/School/Data/EventData/Moving")
moving <- read.csv("20170905233547-CYRUS_2-vs-HfutEngine2017_1-CYRUS_1-moving.csv", head = TRUE)
moving2 <- read.csv("20170905233547-CYRUS_2-vs-HfutEngine2017_1-HfutEngine2017_1-moving.csv", head = TRUE)
groundtruth <- read.csv("D:/School/Data/EventData/Groundtruth/20170905233547-CYRUS_2-vs-HfutEngine2017_1-groundtruth.csv", header = TRUE)
attach(moving)

# Home 1 ------------------------------------------------------------------

#### Home robot 1 ## extracts kicks  

##~~home 1~~

home1 <- matrix(c(Frame = X..time[p.CYRUS.1.state == "kg"], #kg appears to be goal kick
                  Team = rep("H", times = length(X..time[p.CYRUS.1.state == "kg"])),
                  Player = rep("1", times = length(X..time[p.CYRUS.1.state == "kg"])),
                  state = p.CYRUS.1.state[p.CYRUS.1.state == "kg"]), 
                ncol = 4)
colnames(home1) <- c("Frame", "Team", "Player", "State")

##~~home 2~~

home2 <- matrix(c(Frame = X..time[p.CYRUS.2.state == "k"],
                  Team = rep("H", times = length(X..time[p.CYRUS.2.state == "k"])), 
                  Player = rep("2", times = length(X..time[p.CYRUS.2.state == "k"])),
                  state = p.CYRUS.2.state[p.CYRUS.2.state == "k"]), 
                ncol = 4)
colnames(home2) = c("Frame", "Team", "Player", "State")

##~~home 3~~

home3 <- matrix(c(Frame = X..time[p.CYRUS.3.state == "k"],
                  Team = rep("H", times = length(X..time[p.CYRUS.3.state == "k"])), 
                  Player = rep("3", times = length(X..time[p.CYRUS.3.state == "k"])),
                  state = p.CYRUS.3.state[p.CYRUS.3.state == "k"]), 
                ncol = 4)
colnames(home3) = c("Frame", "Team", "Player", "State")

##~~home 4~~

home4 <- matrix(c(Frame = X..time[p.CYRUS.4.state == "k"],
                  Team = rep("H", times = length(X..time[p.CYRUS.4.state == "k"])), 
                  Player = rep("4", times = length(X..time[p.CYRUS.4.state == "k"])),
                  state = p.CYRUS.4.state[p.CYRUS.4.state == "k"]), 
                ncol = 4)
colnames(home4) = c("Frame", "Team", "Player", "State")

##~~home 5~~

home5 <- matrix(c(Frame = X..time[p.CYRUS.5.state == "k"],
                  Team = rep("H", times = length(X..time[p.CYRUS.5.state == "k"])), 
                  Player = rep("5", times = length(X..time[p.CYRUS.5.state == "k"])),
                  state = p.CYRUS.5.state[p.CYRUS.5.state == "k"]), 
                ncol = 4)
colnames(home5) = c("Frame", "Team", "Player", "State")

##~~home 6~~

home6 <- matrix(c(Frame = X..time[p.CYRUS.6.state == "k"],
                  Team = rep("H", times = length(X..time[p.CYRUS.6.state == "k"])), 
                  Player = rep("6", times = length(X..time[p.CYRUS.6.state == "k"])),
                  state = p.CYRUS.6.state[p.CYRUS.6.state == "k"]), 
                ncol = 4)
colnames(home6) = c("Frame", "Team", "Player", "State")

##~~home 7~~

home7 <- matrix(c(Frame = X..time[p.CYRUS.7.state == "k"],
                  Team = rep("H", times = length(X..time[p.CYRUS.7.state == "k"])), 
                  Player = rep("7", times = length(X..time[p.CYRUS.7.state == "k"])),
                  state = p.CYRUS.7.state[p.CYRUS.7.state == "k"]), 
                ncol = 4)
colnames(home7) = c("Frame", "Team", "Player", "State")

##~~home 8~~

home8 <- matrix(c(Frame = X..time[p.CYRUS.8.state == "k"],
                  Team = rep("H", times = length(X..time[p.CYRUS.8.state == "k"])), 
                  Player = rep("8", times = length(X..time[p.CYRUS.8.state == "k"])),
                  state = p.CYRUS.8.state[p.CYRUS.8.state == "k"]), 
                ncol = 4)
colnames(home8) = c("Frame", "Team", "Player", "State")

##~~home 9~~

home9 <- matrix(c(Frame = X..time[p.CYRUS.9.state == "k"],
                  Team = rep("H", times = length(X..time[p.CYRUS.9.state == "k"])), 
                  Player = rep("9", times = length(X..time[p.CYRUS.9.state == "k"])),
                  state = p.CYRUS.9.state[p.CYRUS.9.state == "k"]), 
                ncol = 4)
colnames(home9) = c("Frame", "Team", "Player", "State")

##~~home 10~~

home10 <- matrix(c(Frame = X..time[p.CYRUS.10.state == "k"],
                   Team = rep("H", times = length(X..time[p.CYRUS.10.state == "k"])), 
                   Player = rep("10", times = length(X..time[p.CYRUS.10.state == "k"])),
                   state = p.CYRUS.10.state[p.CYRUS.10.state == "k"]), 
                 ncol = 4)
colnames(home10) = c("Frame", "Team", "Player", "State")

##~~home 11~~

home11 <- matrix(c(Frame = X..time[p.CYRUS.11.state == "k"],
                   Team = rep("H", times = length(X..time[p.CYRUS.11.state == "k"])), 
                   Player = rep("11", times = length(X..time[p.CYRUS.11.state == "k"])),
                   state = p.CYRUS.11.state[p.CYRUS.11.state == "k"]), 
                 ncol = 4)
colnames(home11) = c("Frame", "Team", "Player", "State")

# Home 2 ------------------------------------------------------------------

##### Home robot 2 ## extracts kicks  

moving <- read.csv("20170905233547-CYRUS_2-vs-HfutEngine2017_1-CYRUS_2-moving.csv", head = TRUE)
attach(moving)

##~~home 1~~

home1.next <- matrix(c(Frame = X..time[p.CYRUS.1.state == "kg"], #kg appears to be goal kick
                       Team = rep("H", times = length(X..time[p.CYRUS.1.state == "kg"])),
                       Player = rep("1", times = length(X..time[p.CYRUS.1.state == "kg"])), 
                       state = p.CYRUS.1.state[p.CYRUS.1.state == "kg"]), 
                     ncol = 4)
for(i in 1:nrow(home1.next))
{
  if (!(home1.next[i, 1] %in% home1[, 1]))
  {
    home1 <- rbind(home1, home1.next[i, ])
  }
}

rm(home1.next)

##~~home 2~~

home2.next <- matrix(c(Frame = X..time[p.CYRUS.2.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.2.state == "k"])), 
                       Player = rep("2", times = length(X..time[p.CYRUS.2.state == "k"])),
                       state = p.CYRUS.2.state[p.CYRUS.2.state == "k"]), 
                     ncol = 4)
# This if statement checks that the frames extracted from the current csv file are not the same as those extracted from the 
# previous csv file. The assumption is that robots see kicks in the same frames so this just ensures that there's no double
# counting of frames
if(nrow(home2.next != 0)) 
{
  for(i in 1:nrow(home2.next))
  {
    if (!(home2.next[i, 1] %in% home2[, 1]))
    {
      home2 <- rbind(home2, home2.next[i, ])
    }
  }
}

rm(home2.next)

##~~home 3~~

home3.next <- matrix(c(Frame = X..time[p.CYRUS.3.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.3.state == "k"])), 
                       Player = rep("3", times = length(X..time[p.CYRUS.3.state == "k"])),
                       state = p.CYRUS.3.state[p.CYRUS.3.state == "k"]), 
                     ncol = 4)
if(nrow(home3.next != 0))
{
  for(i in 1:nrow(home3.next))
  {
    if (!(home3.next[i, 1] %in% home3[, 1]))
    {
      home3 <- rbind(home3, home3.next[i, ])
    }
  }
}

rm(home3.next)

##~~home 4~~

home4.next <- matrix(c(Frame = X..time[p.CYRUS.4.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.4.state == "k"])), 
                       Player = rep("4", times = length(X..time[p.CYRUS.4.state == "k"])),
                       state = p.CYRUS.4.state[p.CYRUS.4.state == "k"]), 
                     ncol = 4)
if(nrow(home4.next != 0))
{
  for(i in 1:nrow(home4.next))
  {
    if (!(home4.next[i, 1] %in% home4[, 1]))
    {
      home4 <- rbind(home4, home4.next[i, ])
    }
  }
}

rm(home4.next)

##~~home 5~~

home5.next <- matrix(c(Frame = X..time[p.CYRUS.5.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.5.state == "k"])), 
                       Player = rep("5", times = length(X..time[p.CYRUS.5.state == "k"])),
                       state = p.CYRUS.5.state[p.CYRUS.5.state == "k"]), 
                     ncol = 4)
if(nrow(home5.next != 0))
{
  for(i in 1:nrow(home5.next))
  {
    if (!(home5.next[i, 1] %in% home5[, 1]))
    {
      home5 <- rbind(home5, home5.next[i, ])
    }
  }
}

rm(home5.next)

##~~home 6~~

home6.next <- matrix(c(Frame = X..time[p.CYRUS.6.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.6.state == "k"])), 
                       Player = rep("6", times = length(X..time[p.CYRUS.6.state == "k"])),
                       state = p.CYRUS.6.state[p.CYRUS.6.state == "k"]), 
                     ncol = 4)
if(nrow(home6.next != 0))
{
  for(i in 1:nrow(home6.next))
  {
    if (!(home6.next[i, 1] %in% home6[, 1]))
    {
      home6 <- rbind(home6, home6.next[i, ])
    }
  }
}

rm(home6.next)

##~~home 7~~

home7.next <- matrix(c(Frame = X..time[p.CYRUS.7.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.7.state == "k"])), 
                       Player = rep("7", times = length(X..time[p.CYRUS.7.state == "k"])),
                       state = p.CYRUS.7.state[p.CYRUS.7.state == "k"]), 
                     ncol = 4)
if(nrow(home7.next != 0))
{
  for(i in 1:nrow(home7.next))
  {
    if (!(home7.next[i, 1] %in% home7[, 1]))
    {
      home7 <- rbind(home7, home7.next[i, ])
    }
  }
}

rm(home7.next)

##~~home 8~~

home8.next <- matrix(c(Frame = X..time[p.CYRUS.8.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.8.state == "k"])), 
                       Player = rep("8", times = length(X..time[p.CYRUS.8.state == "k"])),
                       state = p.CYRUS.8.state[p.CYRUS.8.state == "k"]), 
                     ncol = 4)
if(nrow(home8.next != 0))
{
  for(i in 1:nrow(home8.next))
  {
    if (!(home8.next[i, 1] %in% home8[, 1]))
    {
      home8 <- rbind(home8, home8.next[i, ])
    }
  }
}

rm(home8.next)

##~~home 9~~

home9.next <- matrix(c(Frame = X..time[p.CYRUS.9.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.9.state == "k"])), 
                       Player = rep("9", times = length(X..time[p.CYRUS.9.state == "k"])),
                       state = p.CYRUS.9.state[p.CYRUS.9.state == "k"]), 
                     ncol = 4)
if(nrow(home9.next != 0))
{
  for(i in 1:nrow(home9.next))
  {
    if (!(home9.next[i, 1] %in% home9[, 1]))
    {
      home9 <- rbind(home9, home9.next[i, ])
    }
  }
}

rm(home9.next)

##~~home 10~~

home10.next <- matrix(c(Frame = X..time[p.CYRUS.10.state == "k"],
                        Team = rep("H", times = length(X..time[p.CYRUS.10.state == "k"])), 
                        Player = rep("10", times = length(X..time[p.CYRUS.10.state == "k"])),
                        state = p.CYRUS.10.state[p.CYRUS.10.state == "k"]), 
                      ncol = 4)
if(nrow(home10.next != 0))
{
  for(i in 1:nrow(home10.next))
  {
    if (!(home10.next[i, 1] %in% home10[, 1]))
    {
      home10 <- rbind(home10, home10.next[i, ])
    }
  }
}

rm(home10.next)

##~~home 11~~

home11.next <- matrix(c(Frame = X..time[p.CYRUS.11.state == "k"],
                        Team = rep("H", times = length(X..time[p.CYRUS.11.state == "k"])), 
                        Player = rep("11", times = length(X..time[p.CYRUS.11.state == "k"])),
                        state = p.CYRUS.11.state[p.CYRUS.11.state == "k"]), 
                      ncol = 4)
if(nrow(home11.next != 0))
{
  for(i in 1:nrow(home11.next))
  {
    if (!(home11.next[i, 1] %in% home11[, 1]))
    {
      home11 <- rbind(home11, home11.next[i, ])
    }
  }
}

rm(home11.next)

# Home 3 ------------------------------------------------------------------

##### Home robot 3 ## extracts kicks

moving <- read.csv("20170905233547-CYRUS_2-vs-HfutEngine2017_1-CYRUS_3-moving.csv", head = TRUE)
attach(moving)

##~~home 1~~

home1.next <- matrix(c(Frame = X..time[p.CYRUS.1.state == "kg"], #kg appears to be goal kick
                       Team = rep("H", times = length(X..time[p.CYRUS.1.state == "kg"])),
                       Player = rep("1", times = length(X..time[p.CYRUS.1.state == "kg"])), 
                       state = p.CYRUS.1.state[p.CYRUS.1.state == "kg"]), 
                     ncol = 4)
for(i in 1:nrow(home1.next))
{
  if (!(home1.next[i, 1] %in% home1[, 1]))
  {
    home1 <- rbind(home1, home1.next[i, ])
  }
}

rm(home1.next)

##~~home 2~~

home2.next <- matrix(c(Frame = X..time[p.CYRUS.2.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.2.state == "k"])), 
                       Player = rep("2", times = length(X..time[p.CYRUS.2.state == "k"])),
                       state = p.CYRUS.2.state[p.CYRUS.2.state == "k"]), 
                     ncol = 4)
if(nrow(home2.next != 0))
{
  for(i in 1:nrow(home2.next))
  {
    if (!(home2.next[i, 1] %in% home2[, 1]))
    {
      home2 <- rbind(home2, home2.next[i, ])
    }
  }
}

rm(home2.next)

##~~home 3~~

home3.next <- matrix(c(Frame = X..time[p.CYRUS.3.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.3.state == "k"])), 
                       Player = rep("3", times = length(X..time[p.CYRUS.3.state == "k"])),
                       state = p.CYRUS.3.state[p.CYRUS.3.state == "k"]), 
                     ncol = 4)
if(nrow(home3.next != 0))
{
  for(i in 1:nrow(home3.next))
  {
    if (!(home3.next[i, 1] %in% home3[, 1]))
    {
      home3 <- rbind(home3, home3.next[i, ])
    }
  }
}

rm(home3.next)

##~~home 4~~

home4.next <- matrix(c(Frame = X..time[p.CYRUS.4.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.4.state == "k"])), 
                       Player = rep("4", times = length(X..time[p.CYRUS.4.state == "k"])),
                       state = p.CYRUS.4.state[p.CYRUS.4.state == "k"]), 
                     ncol = 4)
if(nrow(home4.next != 0))
{
  for(i in 1:nrow(home4.next))
  {
    if (!(home4.next[i, 1] %in% home4[, 1]))
    {
      home4 <- rbind(home4, home4.next[i, ])
    }
  }
}

rm(home4.next)

##~~home 5~~

home5.next <- matrix(c(Frame = X..time[p.CYRUS.5.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.5.state == "k"])), 
                       Player = rep("5", times = length(X..time[p.CYRUS.5.state == "k"])),
                       state = p.CYRUS.5.state[p.CYRUS.5.state == "k"]), 
                     ncol = 4)
if(nrow(home5.next != 0))
{
  for(i in 1:nrow(home5.next))
  {
    if (!(home5.next[i, 1] %in% home5[, 1]))
    {
      home5 <- rbind(home5, home5.next[i, ])
    }
  }
}

rm(home5.next)

##~~home 6~~

home6.next <- matrix(c(Frame = X..time[p.CYRUS.6.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.6.state == "k"])), 
                       Player = rep("6", times = length(X..time[p.CYRUS.6.state == "k"])),
                       state = p.CYRUS.6.state[p.CYRUS.6.state == "k"]), 
                     ncol = 4)
if(nrow(home6.next != 0))
{
  for(i in 1:nrow(home6.next))
  {
    if (!(home6.next[i, 1] %in% home6[, 1]))
    {
      home6 <- rbind(home6, home6.next[i, ])
    }
  }
}

rm(home6.next)

##~~home 7~~

home7.next <- matrix(c(Frame = X..time[p.CYRUS.7.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.7.state == "k"])), 
                       Player = rep("7", times = length(X..time[p.CYRUS.7.state == "k"])),
                       state = p.CYRUS.7.state[p.CYRUS.7.state == "k"]), 
                     ncol = 4)
if(nrow(home7.next != 0))
{
  for(i in 1:nrow(home7.next))
  {
    if (!(home7.next[i, 1] %in% home7[, 1]))
    {
      home7 <- rbind(home7, home7.next[i, ])
    }
  }
}

rm(home7.next)

##~~home 8~~

home8.next <- matrix(c(Frame = X..time[p.CYRUS.8.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.8.state == "k"])), 
                       Player = rep("8", times = length(X..time[p.CYRUS.8.state == "k"])),
                       state = p.CYRUS.8.state[p.CYRUS.8.state == "k"]), 
                     ncol = 4)
if(nrow(home8.next != 0))
{
  for(i in 1:nrow(home8.next))
  {
    if (!(home8.next[i, 1] %in% home8[, 1]))
    {
      home8 <- rbind(home8, home8.next[i, ])
    }
  }
}

rm(home8.next)

##~~home 9~~

home9.next <- matrix(c(Frame = X..time[p.CYRUS.9.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.9.state == "k"])), 
                       Player = rep("9", times = length(X..time[p.CYRUS.9.state == "k"])),
                       state = p.CYRUS.9.state[p.CYRUS.9.state == "k"]), 
                     ncol = 4)
if(nrow(home9.next != 0))
{
  for(i in 1:nrow(home9.next))
  {
    if (!(home9.next[i, 1] %in% home9[, 1]))
    {
      home9 <- rbind(home9, home9.next[i, ])
    }
  }
}

rm(home9.next)

##~~home 10~~

home10.next <- matrix(c(Frame = X..time[p.CYRUS.10.state == "k"],
                        Team = rep("H", times = length(X..time[p.CYRUS.10.state == "k"])), 
                        Player = rep("10", times = length(X..time[p.CYRUS.10.state == "k"])),
                        state = p.CYRUS.10.state[p.CYRUS.10.state == "k"]), 
                      ncol = 4)
if(nrow(home10.next != 0))
{
  for(i in 1:nrow(home10.next))
  {
    if (!(home10.next[i, 1] %in% home10[, 1]))
    {
      home10 <- rbind(home10, home10.next[i, ])
    }
  }
}

rm(home10.next)

##~~home 11~~

home11.next <- matrix(c(Frame = X..time[p.CYRUS.11.state == "k"],
                        Team = rep("H", times = length(X..time[p.CYRUS.11.state == "k"])), 
                        Player = rep("11", times = length(X..time[p.CYRUS.11.state == "k"])),
                        state = p.CYRUS.11.state[p.CYRUS.11.state == "k"]), 
                      ncol = 4)
if(nrow(home11.next != 0))
{
  for(i in 1:nrow(home11.next))
  {
    if (!(home11.next[i, 1] %in% home11[, 1]))
    {
      home11 <- rbind(home11, home11.next[i, ])
    }
  }
}

rm(home11.next)

# Home 4 ------------------------------------------------------------------

##### Home robot 4 ## extracts kicks

data <- read.csv("20170905233547-CYRUS_2-vs-HfutEngine2017_1-CYRUS_4-moving.csv", head = TRUE)
attach(data)

##~~home 1~~

home1.next <- matrix(c(Frame = X..time[p.CYRUS.1.state == "kg"], #kg appears to be goal kick
                       Team = rep("H", times = length(X..time[p.CYRUS.1.state == "kg"])),
                       Player = rep("1", times = length(X..time[p.CYRUS.1.state == "kg"])), 
                       state = p.CYRUS.1.state[p.CYRUS.1.state == "kg"]), 
                     ncol = 4)
for(i in 1:nrow(home1.next))
{
  if (!(home1.next[i, 1] %in% home1[, 1]))
  {
    home1 <- rbind(home1, home1.next[i, ])
  }
}

rm(home1.next)

##~~home 2~~

home2.next <- matrix(c(Frame = X..time[p.CYRUS.2.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.2.state == "k"])), 
                       Player = rep("2", times = length(X..time[p.CYRUS.2.state == "k"])),
                       state = p.CYRUS.2.state[p.CYRUS.2.state == "k"]), 
                     ncol = 4)
if(nrow(home2.next != 0))
{
  for(i in 1:nrow(home2.next))
  {
    if (!(home2.next[i, 1] %in% home2[, 1]))
    {
      home2 <- rbind(home2, home2.next[i, ])
    }
  }
}

rm(home2.next)

##~~home 3~~

home3.next <- matrix(c(Frame = X..time[p.CYRUS.3.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.3.state == "k"])), 
                       Player = rep("3", times = length(X..time[p.CYRUS.3.state == "k"])),
                       state = p.CYRUS.3.state[p.CYRUS.3.state == "k"]), 
                     ncol = 4)
if(nrow(home3.next != 0))
{
  for(i in 1:nrow(home3.next))
  {
    if (!(home3.next[i, 1] %in% home3[, 1]))
    {
      home3 <- rbind(home3, home3.next[i, ])
    }
  }
}

rm(home3.next)

##~~home 4~~

home4.next <- matrix(c(Frame = X..time[p.CYRUS.4.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.4.state == "k"])), 
                       Player = rep("4", times = length(X..time[p.CYRUS.4.state == "k"])),
                       state = p.CYRUS.4.state[p.CYRUS.4.state == "k"]), 
                     ncol = 4)
if(nrow(home4.next != 0))
{
  for(i in 1:nrow(home4.next))
  {
    if (!(home4.next[i, 1] %in% home4[, 1]))
    {
      home4 <- rbind(home4, home4.next[i, ])
    }
  }
}

rm(home4.next)

##~~home 5~~

home5.next <- matrix(c(Frame = X..time[p.CYRUS.5.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.5.state == "k"])), 
                       Player = rep("5", times = length(X..time[p.CYRUS.5.state == "k"])),
                       state = p.CYRUS.5.state[p.CYRUS.5.state == "k"]), 
                     ncol = 4)
if(nrow(home5.next != 0))
{
  for(i in 1:nrow(home5.next))
  {
    if (!(home5.next[i, 1] %in% home5[, 1]))
    {
      home5 <- rbind(home5, home5.next[i, ])
    }
  }
}

rm(home5.next)

##~~home 6~~

home6.next <- matrix(c(Frame = X..time[p.CYRUS.6.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.6.state == "k"])), 
                       Player = rep("6", times = length(X..time[p.CYRUS.6.state == "k"])),
                       state = p.CYRUS.6.state[p.CYRUS.6.state == "k"]), 
                     ncol = 4)
if(nrow(home6.next != 0))
{
  for(i in 1:nrow(home6.next))
  {
    if (!(home6.next[i, 1] %in% home6[, 1]))
    {
      home6 <- rbind(home6, home6.next[i, ])
    }
  }
}

rm(home6.next)

##~~home 7~~

home7.next <- matrix(c(Frame = X..time[p.CYRUS.7.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.7.state == "k"])), 
                       Player = rep("7", times = length(X..time[p.CYRUS.7.state == "k"])),
                       state = p.CYRUS.7.state[p.CYRUS.7.state == "k"]), 
                     ncol = 4)
if(nrow(home7.next != 0))
{
  for(i in 1:nrow(home7.next))
  {
    if (!(home7.next[i, 1] %in% home7[, 1]))
    {
      home7 <- rbind(home7, home7.next[i, ])
    }
  }
}

rm(home7.next)

##~~home 8~~

home8.next <- matrix(c(Frame = X..time[p.CYRUS.8.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.8.state == "k"])), 
                       Player = rep("8", times = length(X..time[p.CYRUS.8.state == "k"])),
                       state = p.CYRUS.8.state[p.CYRUS.8.state == "k"]), 
                     ncol = 4)
if(nrow(home8.next != 0))
{
  for(i in 1:nrow(home8.next))
  {
    if (!(home8.next[i, 1] %in% home8[, 1]))
    {
      home8 <- rbind(home8, home8.next[i, ])
    }
  }
}

rm(home8.next)

##~~home 9~~

home9.next <- matrix(c(Frame = X..time[p.CYRUS.9.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.9.state == "k"])), 
                       Player = rep("9", times = length(X..time[p.CYRUS.9.state == "k"])),
                       state = p.CYRUS.9.state[p.CYRUS.9.state == "k"]), 
                     ncol = 4)
if(nrow(home9.next != 0))
{
  for(i in 1:nrow(home9.next))
  {
    if (!(home9.next[i, 1] %in% home9[, 1]))
    {
      home9 <- rbind(home9, home9.next[i, ])
    }
  }
}

rm(home9.next)

##~~home 10~~

home10.next <- matrix(c(Frame = X..time[p.CYRUS.10.state == "k"],
                        Team = rep("H", times = length(X..time[p.CYRUS.10.state == "k"])), 
                        Player = rep("10", times = length(X..time[p.CYRUS.10.state == "k"])),
                        state = p.CYRUS.10.state[p.CYRUS.10.state == "k"]), 
                      ncol = 4)
if(nrow(home10.next != 0))
{
  for(i in 1:nrow(home10.next))
  {
    if (!(home10.next[i, 1] %in% home10[, 1]))
    {
      home10 <- rbind(home10, home10.next[i, ])
    }
  }
}

rm(home10.next)

##~~home 11~~

home11.next <- matrix(c(Frame = X..time[p.CYRUS.11.state == "k"],
                        Team = rep("H", times = length(X..time[p.CYRUS.11.state == "k"])), 
                        Player = rep("11", times = length(X..time[p.CYRUS.11.state == "k"])),
                        state = p.CYRUS.11.state[p.CYRUS.11.state == "k"]), 
                      ncol = 4)
if(nrow(home11.next != 0))
{
  for(i in 1:nrow(home11.next))
  {
    if (!(home11.next[i, 1] %in% home11[, 1]))
    {
      home11 <- rbind(home11, home11.next[i, ])
    }
  }
}

rm(home11.next)

# Home 5 ------------------------------------------------------------------

##### Home robot 5 ## extracts kicks

data <- read.csv("20170905233547-CYRUS_2-vs-HfutEngine2017_1-CYRUS_5-moving.csv", head = TRUE)
attach(data)

##~~home 1~~

home1.next <- matrix(c(Frame = X..time[p.CYRUS.1.state == "kg"], #kg appears to be goal kick
                       Team = rep("H", times = length(X..time[p.CYRUS.1.state == "kg"])),
                       Player = rep("1", times = length(X..time[p.CYRUS.1.state == "kg"])), 
                       state = p.CYRUS.1.state[p.CYRUS.1.state == "kg"]), 
                     ncol = 4)
for(i in 1:nrow(home1.next))
{
  if (!(home1.next[i, 1] %in% home1[, 1]))
  {
    home1 <- rbind(home1, home1.next[i, ])
  }
}

rm(home1.next)

##~~home 2~~

home2.next <- matrix(c(Frame = X..time[p.CYRUS.2.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.2.state == "k"])), 
                       Player = rep("2", times = length(X..time[p.CYRUS.2.state == "k"])),
                       state = p.CYRUS.2.state[p.CYRUS.2.state == "k"]), 
                     ncol = 4)
if(nrow(home2.next != 0))
{
  for(i in 1:nrow(home2.next))
  {
    if (!(home2.next[i, 1] %in% home2[, 1]))
    {
      home2 <- rbind(home2, home2.next[i, ])
    }
  }
}

rm(home2.next)

##~~home 3~~

home3.next <- matrix(c(Frame = X..time[p.CYRUS.3.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.3.state == "k"])), 
                       Player = rep("3", times = length(X..time[p.CYRUS.3.state == "k"])),
                       state = p.CYRUS.3.state[p.CYRUS.3.state == "k"]), 
                     ncol = 4)
if(nrow(home3.next != 0))
{
  for(i in 1:nrow(home3.next))
  {
    if (!(home3.next[i, 1] %in% home3[, 1]))
    {
      home3 <- rbind(home3, home3.next[i, ])
    }
  }
}

rm(home3.next)

##~~home 4~~

home4.next <- matrix(c(Frame = X..time[p.CYRUS.4.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.4.state == "k"])), 
                       Player = rep("4", times = length(X..time[p.CYRUS.4.state == "k"])),
                       state = p.CYRUS.4.state[p.CYRUS.4.state == "k"]), 
                     ncol = 4)
if(nrow(home4.next != 0))
{
  for(i in 1:nrow(home4.next))
  {
    if (!(home4.next[i, 1] %in% home4[, 1]))
    {
      home4 <- rbind(home4, home4.next[i, ])
    }
  }
}

rm(home4.next)

##~~home 5~~

home5.next <- matrix(c(Frame = X..time[p.CYRUS.5.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.5.state == "k"])), 
                       Player = rep("5", times = length(X..time[p.CYRUS.5.state == "k"])),
                       state = p.CYRUS.5.state[p.CYRUS.5.state == "k"]), 
                     ncol = 4)
if(nrow(home5.next != 0))
{
  for(i in 1:nrow(home5.next))
  {
    if (!(home5.next[i, 1] %in% home5[, 1]))
    {
      home5 <- rbind(home5, home5.next[i, ])
    }
  }
}

rm(home5.next)

##~~home 6~~

home6.next <- matrix(c(Frame = X..time[p.CYRUS.6.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.6.state == "k"])), 
                       Player = rep("6", times = length(X..time[p.CYRUS.6.state == "k"])),
                       state = p.CYRUS.6.state[p.CYRUS.6.state == "k"]), 
                     ncol = 4)
if(nrow(home6.next != 0))
{
  for(i in 1:nrow(home6.next))
  {
    if (!(home6.next[i, 1] %in% home6[, 1]))
    {
      home6 <- rbind(home6, home6.next[i, ])
    }
  }
}

rm(home6.next)

##~~home 7~~

home7.next <- matrix(c(Frame = X..time[p.CYRUS.7.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.7.state == "k"])), 
                       Player = rep("7", times = length(X..time[p.CYRUS.7.state == "k"])),
                       state = p.CYRUS.7.state[p.CYRUS.7.state == "k"]), 
                     ncol = 4)
if(nrow(home7.next != 0))
{
  for(i in 1:nrow(home7.next))
  {
    if (!(home7.next[i, 1] %in% home7[, 1]))
    {
      home7 <- rbind(home7, home7.next[i, ])
    }
  }
}

rm(home7.next)

##~~home 8~~

home8.next <- matrix(c(Frame = X..time[p.CYRUS.8.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.8.state == "k"])), 
                       Player = rep("8", times = length(X..time[p.CYRUS.8.state == "k"])),
                       state = p.CYRUS.8.state[p.CYRUS.8.state == "k"]), 
                     ncol = 4)
if(nrow(home8.next != 0))
{
  for(i in 1:nrow(home8.next))
  {
    if (!(home8.next[i, 1] %in% home8[, 1]))
    {
      home8 <- rbind(home8, home8.next[i, ])
    }
  }
}

rm(home8.next)

##~~home 9~~

home9.next <- matrix(c(Frame = X..time[p.CYRUS.9.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.9.state == "k"])), 
                       Player = rep("9", times = length(X..time[p.CYRUS.9.state == "k"])),
                       state = p.CYRUS.9.state[p.CYRUS.9.state == "k"]), 
                     ncol = 4)
if(nrow(home9.next != 0))
{
  for(i in 1:nrow(home9.next))
  {
    if (!(home9.next[i, 1] %in% home9[, 1]))
    {
      home9 <- rbind(home9, home9.next[i, ])
    }
  }
}

rm(home9.next)

##~~home 10~~

home10.next <- matrix(c(Frame = X..time[p.CYRUS.10.state == "k"],
                        Team = rep("H", times = length(X..time[p.CYRUS.10.state == "k"])), 
                        Player = rep("10", times = length(X..time[p.CYRUS.10.state == "k"])),
                        state = p.CYRUS.10.state[p.CYRUS.10.state == "k"]), 
                      ncol = 4)
if(nrow(home10.next != 0))
{
  for(i in 1:nrow(home10.next))
  {
    if (!(home10.next[i, 1] %in% home10[, 1]))
    {
      home10 <- rbind(home10, home10.next[i, ])
    }
  }
}

rm(home10.next)

##~~home 11~~

home11.next <- matrix(c(Frame = X..time[p.CYRUS.11.state == "k"],
                        Team = rep("H", times = length(X..time[p.CYRUS.11.state == "k"])), 
                        Player = rep("11", times = length(X..time[p.CYRUS.11.state == "k"])),
                        state = p.CYRUS.11.state[p.CYRUS.11.state == "k"]), 
                      ncol = 4)
if(nrow(home11.next != 0))
{
  for(i in 1:nrow(home11.next))
  {
    if (!(home11.next[i, 1] %in% home11[, 1]))
    {
      home11 <- rbind(home11, home11.next[i, ])
    }
  }
}

rm(home11.next)

# Home 6 ------------------------------------------------------------------

##### Home robot 6 ## extracts kicks

data <- read.csv("20170905233547-CYRUS_2-vs-HfutEngine2017_1-CYRUS_6-moving.csv", head = TRUE)
attach(data)

##~~home 1~~

home1.next <- matrix(c(Frame = X..time[p.CYRUS.1.state == "kg"], #kg appears to be goal kick
                       Team = rep("H", times = length(X..time[p.CYRUS.1.state == "kg"])),
                       Player = rep("1", times = length(X..time[p.CYRUS.1.state == "kg"])), 
                       state = p.CYRUS.1.state[p.CYRUS.1.state == "kg"]), 
                     ncol = 4)
for(i in 1:nrow(home1.next))
{
  if (!(home1.next[i, 1] %in% home1[, 1]))
  {
    home1 <- rbind(home1, home1.next[i, ])
  }
}

rm(home1.next)

##~~home 2~~

home2.next <- matrix(c(Frame = X..time[p.CYRUS.2.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.2.state == "k"])), 
                       Player = rep("2", times = length(X..time[p.CYRUS.2.state == "k"])),
                       state = p.CYRUS.2.state[p.CYRUS.2.state == "k"]), 
                     ncol = 4)
if(nrow(home2.next != 0))
{
  for(i in 1:nrow(home2.next))
  {
    if (!(home2.next[i, 1] %in% home2[, 1]))
    {
      home2 <- rbind(home2, home2.next[i, ])
    }
  }
}

rm(home2.next)

##~~home 3~~

home3.next <- matrix(c(Frame = X..time[p.CYRUS.3.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.3.state == "k"])), 
                       Player = rep("3", times = length(X..time[p.CYRUS.3.state == "k"])),
                       state = p.CYRUS.3.state[p.CYRUS.3.state == "k"]), 
                     ncol = 4)
if(nrow(home3.next != 0))
{
  for(i in 1:nrow(home3.next))
  {
    if (!(home3.next[i, 1] %in% home3[, 1]))
    {
      home3 <- rbind(home3, home3.next[i, ])
    }
  }
}

rm(home3.next)

##~~home 4~~

home4.next <- matrix(c(Frame = X..time[p.CYRUS.4.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.4.state == "k"])), 
                       Player = rep("4", times = length(X..time[p.CYRUS.4.state == "k"])),
                       state = p.CYRUS.4.state[p.CYRUS.4.state == "k"]), 
                     ncol = 4)
if(nrow(home4.next != 0))
{
  for(i in 1:nrow(home4.next))
  {
    if (!(home4.next[i, 1] %in% home4[, 1]))
    {
      home4 <- rbind(home4, home4.next[i, ])
    }
  }
}

rm(home4.next)

##~~home 5~~

home5.next <- matrix(c(Frame = X..time[p.CYRUS.5.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.5.state == "k"])), 
                       Player = rep("5", times = length(X..time[p.CYRUS.5.state == "k"])),
                       state = p.CYRUS.5.state[p.CYRUS.5.state == "k"]), 
                     ncol = 4)
if(nrow(home5.next != 0))
{
  for(i in 1:nrow(home5.next))
  {
    if (!(home5.next[i, 1] %in% home5[, 1]))
    {
      home5 <- rbind(home5, home5.next[i, ])
    }
  }
}

rm(home5.next)

##~~home 6~~

home6.next <- matrix(c(Frame = X..time[p.CYRUS.6.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.6.state == "k"])), 
                       Player = rep("6", times = length(X..time[p.CYRUS.6.state == "k"])),
                       state = p.CYRUS.6.state[p.CYRUS.6.state == "k"]), 
                     ncol = 4)
if(nrow(home6.next != 0))
{
  for(i in 1:nrow(home6.next))
  {
    if (!(home6.next[i, 1] %in% home6[, 1]))
    {
      home6 <- rbind(home6, home6.next[i, ])
    }
  }
}

rm(home6.next)

##~~home 7~~

home7.next <- matrix(c(Frame = X..time[p.CYRUS.7.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.7.state == "k"])), 
                       Player = rep("7", times = length(X..time[p.CYRUS.7.state == "k"])),
                       state = p.CYRUS.7.state[p.CYRUS.7.state == "k"]), 
                     ncol = 4)
if(nrow(home7.next != 0))
{
  for(i in 1:nrow(home7.next))
  {
    if (!(home7.next[i, 1] %in% home7[, 1]))
    {
      home7 <- rbind(home7, home7.next[i, ])
    }
  }
}

rm(home7.next)

##~~home 8~~

home8.next <- matrix(c(Frame = X..time[p.CYRUS.8.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.8.state == "k"])), 
                       Player = rep("8", times = length(X..time[p.CYRUS.8.state == "k"])),
                       state = p.CYRUS.8.state[p.CYRUS.8.state == "k"]), 
                     ncol = 4)
if(nrow(home8.next != 0))
{
  for(i in 1:nrow(home8.next))
  {
    if (!(home8.next[i, 1] %in% home8[, 1]))
    {
      home8 <- rbind(home8, home8.next[i, ])
    }
  }
}

rm(home8.next)

##~~home 9~~

home9.next <- matrix(c(Frame = X..time[p.CYRUS.9.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.9.state == "k"])), 
                       Player = rep("9", times = length(X..time[p.CYRUS.9.state == "k"])),
                       state = p.CYRUS.9.state[p.CYRUS.9.state == "k"]), 
                     ncol = 4)
if(nrow(home9.next != 0))
{
  for(i in 1:nrow(home9.next))
  {
    if (!(home9.next[i, 1] %in% home9[, 1]))
    {
      home9 <- rbind(home9, home9.next[i, ])
    }
  }
}

rm(home9.next)

##~~home 10~~

home10.next <- matrix(c(Frame = X..time[p.CYRUS.10.state == "k"],
                        Team = rep("H", times = length(X..time[p.CYRUS.10.state == "k"])), 
                        Player = rep("10", times = length(X..time[p.CYRUS.10.state == "k"])),
                        state = p.CYRUS.10.state[p.CYRUS.10.state == "k"]), 
                      ncol = 4)
if(nrow(home10.next != 0))
{
  for(i in 1:nrow(home10.next))
  {
    if (!(home10.next[i, 1] %in% home10[, 1]))
    {
      home10 <- rbind(home10, home10.next[i, ])
    }
  }
}

rm(home10.next)

##~~home 11~~

home11.next <- matrix(c(Frame = X..time[p.CYRUS.11.state == "k"],
                        Team = rep("H", times = length(X..time[p.CYRUS.11.state == "k"])), 
                        Player = rep("11", times = length(X..time[p.CYRUS.11.state == "k"])),
                        state = p.CYRUS.11.state[p.CYRUS.11.state == "k"]), 
                      ncol = 4)
if(nrow(home11.next != 0))
{
  for(i in 1:nrow(home11.next))
  {
    if (!(home11.next[i, 1] %in% home11[, 1]))
    {
      home11 <- rbind(home11, home11.next[i, ])
    }
  }
}

rm(home11.next)

# Home 7 ------------------------------------------------------------------

##### Home robot 7 ## extracts kicks

data <- read.csv("20170905233547-CYRUS_2-vs-HfutEngine2017_1-CYRUS_7-moving.csv", head = TRUE)
attach(data)

##~~home 1~~

home1.next <- matrix(c(Frame = X..time[p.CYRUS.1.state == "kg"], #kg appears to be goal kick
                       Team = rep("H", times = length(X..time[p.CYRUS.1.state == "kg"])),
                       Player = rep("1", times = length(X..time[p.CYRUS.1.state == "kg"])), 
                       state = p.CYRUS.1.state[p.CYRUS.1.state == "kg"]), 
                     ncol = 4)
for(i in 1:nrow(home1.next))
{
  if (!(home1.next[i, 1] %in% home1[, 1]))
  {
    home1 <- rbind(home1, home1.next[i, ])
  }
}

rm(home1.next)

##~~home 2~~

home2.next <- matrix(c(Frame = X..time[p.CYRUS.2.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.2.state == "k"])), 
                       Player = rep("2", times = length(X..time[p.CYRUS.2.state == "k"])),
                       state = p.CYRUS.2.state[p.CYRUS.2.state == "k"]), 
                     ncol = 4)
if(nrow(home2.next != 0))
{
  for(i in 1:nrow(home2.next))
  {
    if (!(home2.next[i, 1] %in% home2[, 1]))
    {
      home2 <- rbind(home2, home2.next[i, ])
    }
  }
}

rm(home2.next)

##~~home 3~~

home3.next <- matrix(c(Frame = X..time[p.CYRUS.3.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.3.state == "k"])), 
                       Player = rep("3", times = length(X..time[p.CYRUS.3.state == "k"])),
                       state = p.CYRUS.3.state[p.CYRUS.3.state == "k"]), 
                     ncol = 4)
if(nrow(home3.next != 0))
{
  for(i in 1:nrow(home3.next))
  {
    if (!(home3.next[i, 1] %in% home3[, 1]))
    {
      home3 <- rbind(home3, home3.next[i, ])
    }
  }
}

rm(home3.next)

##~~home 4~~

home4.next <- matrix(c(Frame = X..time[p.CYRUS.4.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.4.state == "k"])), 
                       Player = rep("4", times = length(X..time[p.CYRUS.4.state == "k"])),
                       state = p.CYRUS.4.state[p.CYRUS.4.state == "k"]), 
                     ncol = 4)
if(nrow(home4.next != 0))
{
  for(i in 1:nrow(home4.next))
  {
    if (!(home4.next[i, 1] %in% home4[, 1]))
    {
      home4 <- rbind(home4, home4.next[i, ])
    }
  }
}

rm(home4.next)

##~~home 5~~

home5.next <- matrix(c(Frame = X..time[p.CYRUS.5.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.5.state == "k"])), 
                       Player = rep("5", times = length(X..time[p.CYRUS.5.state == "k"])),
                       state = p.CYRUS.5.state[p.CYRUS.5.state == "k"]), 
                     ncol = 4)
if(nrow(home5.next != 0))
{
  for(i in 1:nrow(home5.next))
  {
    if (!(home5.next[i, 1] %in% home5[, 1]))
    {
      home5 <- rbind(home5, home5.next[i, ])
    }
  }
}

rm(home5.next)

##~~home 6~~

home6.next <- matrix(c(Frame = X..time[p.CYRUS.6.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.6.state == "k"])), 
                       Player = rep("6", times = length(X..time[p.CYRUS.6.state == "k"])),
                       state = p.CYRUS.6.state[p.CYRUS.6.state == "k"]), 
                     ncol = 4)
if(nrow(home6.next != 0))
{
  for(i in 1:nrow(home6.next))
  {
    if (!(home6.next[i, 1] %in% home6[, 1]))
    {
      home6 <- rbind(home6, home6.next[i, ])
    }
  }
}

rm(home6.next)

##~~home 7~~

home7.next <- matrix(c(Frame = X..time[p.CYRUS.7.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.7.state == "k"])), 
                       Player = rep("7", times = length(X..time[p.CYRUS.7.state == "k"])),
                       state = p.CYRUS.7.state[p.CYRUS.7.state == "k"]), 
                     ncol = 4)
if(nrow(home7.next != 0))
{
  for(i in 1:nrow(home7.next))
  {
    if (!(home7.next[i, 1] %in% home7[, 1]))
    {
      home7 <- rbind(home7, home7.next[i, ])
    }
  }
}

rm(home7.next)

##~~home 8~~

home8.next <- matrix(c(Frame = X..time[p.CYRUS.8.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.8.state == "k"])), 
                       Player = rep("8", times = length(X..time[p.CYRUS.8.state == "k"])),
                       state = p.CYRUS.8.state[p.CYRUS.8.state == "k"]), 
                     ncol = 4)
if(nrow(home8.next != 0))
{
  for(i in 1:nrow(home8.next))
  {
    if (!(home8.next[i, 1] %in% home8[, 1]))
    {
      home8 <- rbind(home8, home8.next[i, ])
    }
  }
}

rm(home8.next)

##~~home 9~~

home9.next <- matrix(c(Frame = X..time[p.CYRUS.9.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.9.state == "k"])), 
                       Player = rep("9", times = length(X..time[p.CYRUS.9.state == "k"])),
                       state = p.CYRUS.9.state[p.CYRUS.9.state == "k"]), 
                     ncol = 4)
if(nrow(home9.next != 0))
{
  for(i in 1:nrow(home9.next))
  {
    if (!(home9.next[i, 1] %in% home9[, 1]))
    {
      home9 <- rbind(home9, home9.next[i, ])
    }
  }
}

rm(home9.next)

##~~home 10~~

home10.next <- matrix(c(Frame = X..time[p.CYRUS.10.state == "k"],
                        Team = rep("H", times = length(X..time[p.CYRUS.10.state == "k"])), 
                        Player = rep("10", times = length(X..time[p.CYRUS.10.state == "k"])),
                        state = p.CYRUS.10.state[p.CYRUS.10.state == "k"]), 
                      ncol = 4)
if(nrow(home10.next != 0))
{
  for(i in 1:nrow(home10.next))
  {
    if (!(home10.next[i, 1] %in% home10[, 1]))
    {
      home10 <- rbind(home10, home10.next[i, ])
    }
  }
}

rm(home10.next)

##~~home 11~~

home11.next <- matrix(c(Frame = X..time[p.CYRUS.11.state == "k"],
                        Team = rep("H", times = length(X..time[p.CYRUS.11.state == "k"])), 
                        Player = rep("11", times = length(X..time[p.CYRUS.11.state == "k"])),
                        state = p.CYRUS.11.state[p.CYRUS.11.state == "k"]), 
                      ncol = 4)
if(nrow(home11.next != 0))
{
  for(i in 1:nrow(home11.next))
  {
    if (!(home11.next[i, 1] %in% home11[, 1]))
    {
      home11 <- rbind(home11, home11.next[i, ])
    }
  }
}

rm(home11.next)

# Home 8 ------------------------------------------------------------------

##### Home robot 8 ## extracts kicks

data <- read.csv("20170905233547-CYRUS_2-vs-HfutEngine2017_1-CYRUS_8-moving.csv", head = TRUE)
attach(data)

##~~home 1~~

home1.next <- matrix(c(Frame = X..time[p.CYRUS.1.state == "kg"], #kg appears to be goal kick
                       Team = rep("H", times = length(X..time[p.CYRUS.1.state == "kg"])),
                       Player = rep("1", times = length(X..time[p.CYRUS.1.state == "kg"])), 
                       state = p.CYRUS.1.state[p.CYRUS.1.state == "kg"]), 
                     ncol = 4)
for(i in 1:nrow(home1.next))
{
  if (!(home1.next[i, 1] %in% home1[, 1]))
  {
    home1 <- rbind(home1, home1.next[i, ])
  }
}

rm(home1.next)

##~~home 2~~

home2.next <- matrix(c(Frame = X..time[p.CYRUS.2.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.2.state == "k"])), 
                       Player = rep("2", times = length(X..time[p.CYRUS.2.state == "k"])),
                       state = p.CYRUS.2.state[p.CYRUS.2.state == "k"]), 
                     ncol = 4)
if(nrow(home2.next != 0))
{
  for(i in 1:nrow(home2.next))
  {
    if (!(home2.next[i, 1] %in% home2[, 1]))
    {
      home2 <- rbind(home2, home2.next[i, ])
    }
  }
}

rm(home2.next)

##~~home 3~~

home3.next <- matrix(c(Frame = X..time[p.CYRUS.3.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.3.state == "k"])), 
                       Player = rep("3", times = length(X..time[p.CYRUS.3.state == "k"])),
                       state = p.CYRUS.3.state[p.CYRUS.3.state == "k"]), 
                     ncol = 4)
if(nrow(home3.next != 0))
{
  for(i in 1:nrow(home3.next))
  {
    if (!(home3.next[i, 1] %in% home3[, 1]))
    {
      home3 <- rbind(home3, home3.next[i, ])
    }
  }
}

rm(home3.next)

##~~home 4~~

home4.next <- matrix(c(Frame = X..time[p.CYRUS.4.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.4.state == "k"])), 
                       Player = rep("4", times = length(X..time[p.CYRUS.4.state == "k"])),
                       state = p.CYRUS.4.state[p.CYRUS.4.state == "k"]), 
                     ncol = 4)
if(nrow(home4.next != 0))
{
  for(i in 1:nrow(home4.next))
  {
    if (!(home4.next[i, 1] %in% home4[, 1]))
    {
      home4 <- rbind(home4, home4.next[i, ])
    }
  }
}

rm(home4.next)

##~~home 5~~

home5.next <- matrix(c(Frame = X..time[p.CYRUS.5.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.5.state == "k"])), 
                       Player = rep("5", times = length(X..time[p.CYRUS.5.state == "k"])),
                       state = p.CYRUS.5.state[p.CYRUS.5.state == "k"]), 
                     ncol = 4)
if(nrow(home5.next != 0))
{
  for(i in 1:nrow(home5.next))
  {
    if (!(home5.next[i, 1] %in% home5[, 1]))
    {
      home5 <- rbind(home5, home5.next[i, ])
    }
  }
}

rm(home5.next)

##~~home 6~~

home6.next <- matrix(c(Frame = X..time[p.CYRUS.6.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.6.state == "k"])), 
                       Player = rep("6", times = length(X..time[p.CYRUS.6.state == "k"])),
                       state = p.CYRUS.6.state[p.CYRUS.6.state == "k"]), 
                     ncol = 4)
if(nrow(home6.next != 0))
{
  for(i in 1:nrow(home6.next))
  {
    if (!(home6.next[i, 1] %in% home6[, 1]))
    {
      home6 <- rbind(home6, home6.next[i, ])
    }
  }
}

rm(home6.next)

##~~home 7~~

home7.next <- matrix(c(Frame = X..time[p.CYRUS.7.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.7.state == "k"])), 
                       Player = rep("7", times = length(X..time[p.CYRUS.7.state == "k"])),
                       state = p.CYRUS.7.state[p.CYRUS.7.state == "k"]), 
                     ncol = 4)
if(nrow(home7.next != 0))
{
  for(i in 1:nrow(home7.next))
  {
    if (!(home7.next[i, 1] %in% home7[, 1]))
    {
      home7 <- rbind(home7, home7.next[i, ])
    }
  }
}

rm(home7.next)

##~~home 8~~

home8.next <- matrix(c(Frame = X..time[p.CYRUS.8.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.8.state == "k"])), 
                       Player = rep("8", times = length(X..time[p.CYRUS.8.state == "k"])),
                       state = p.CYRUS.8.state[p.CYRUS.8.state == "k"]), 
                     ncol = 4)
if(nrow(home8.next != 0))
{
  for(i in 1:nrow(home8.next))
  {
    if (!(home8.next[i, 1] %in% home8[, 1]))
    {
      home8 <- rbind(home8, home8.next[i, ])
    }
  }
}

rm(home8.next)

##~~home 9~~

home9.next <- matrix(c(Frame = X..time[p.CYRUS.9.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.9.state == "k"])), 
                       Player = rep("9", times = length(X..time[p.CYRUS.9.state == "k"])),
                       state = p.CYRUS.9.state[p.CYRUS.9.state == "k"]), 
                     ncol = 4)
if(nrow(home9.next != 0))
{
  for(i in 1:nrow(home9.next))
  {
    if (!(home9.next[i, 1] %in% home9[, 1]))
    {
      home9 <- rbind(home9, home9.next[i, ])
    }
  }
}

rm(home9.next)

##~~home 10~~

home10.next <- matrix(c(Frame = X..time[p.CYRUS.10.state == "k"],
                        Team = rep("H", times = length(X..time[p.CYRUS.10.state == "k"])), 
                        Player = rep("10", times = length(X..time[p.CYRUS.10.state == "k"])),
                        state = p.CYRUS.10.state[p.CYRUS.10.state == "k"]), 
                      ncol = 4)
if(nrow(home10.next != 0))
{
  for(i in 1:nrow(home10.next))
  {
    if (!(home10.next[i, 1] %in% home10[, 1]))
    {
      home10 <- rbind(home10, home10.next[i, ])
    }
  }
}

rm(home10.next)

##~~home 11~~

home11.next <- matrix(c(Frame = X..time[p.CYRUS.11.state == "k"],
                        Team = rep("H", times = length(X..time[p.CYRUS.11.state == "k"])), 
                        Player = rep("11", times = length(X..time[p.CYRUS.11.state == "k"])),
                        state = p.CYRUS.11.state[p.CYRUS.11.state == "k"]), 
                      ncol = 4)
if(nrow(home11.next != 0))
{
  for(i in 1:nrow(home11.next))
  {
    if (!(home11.next[i, 1] %in% home11[, 1]))
    {
      home11 <- rbind(home11, home11.next[i, ])
    }
  }
}

rm(home11.next)
# Home 9 ------------------------------------------------------------------

##### Home robot 9 ## extracts kicks

data <- read.csv("20170905233547-CYRUS_2-vs-HfutEngine2017_1-CYRUS_9-moving.csv", head = TRUE)
attach(data)

##~~home 1~~

home1.next <- matrix(c(Frame = X..time[p.CYRUS.1.state == "kg"], #kg appears to be goal kick
                       Team = rep("H", times = length(X..time[p.CYRUS.1.state == "kg"])),
                       Player = rep("1", times = length(X..time[p.CYRUS.1.state == "kg"])), 
                       state = p.CYRUS.1.state[p.CYRUS.1.state == "kg"]), 
                     ncol = 4)
for(i in 1:nrow(home1.next))
{
  if (!(home1.next[i, 1] %in% home1[, 1]))
  {
    home1 <- rbind(home1, home1.next[i, ])
  }
}

rm(home1.next)

##~~home 2~~

home2.next <- matrix(c(Frame = X..time[p.CYRUS.2.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.2.state == "k"])), 
                       Player = rep("2", times = length(X..time[p.CYRUS.2.state == "k"])),
                       state = p.CYRUS.2.state[p.CYRUS.2.state == "k"]), 
                     ncol = 4)
if(nrow(home2.next != 0))
{
  for(i in 1:nrow(home2.next))
  {
    if (!(home2.next[i, 1] %in% home2[, 1]))
    {
      home2 <- rbind(home2, home2.next[i, ])
    }
  }
}

rm(home2.next)

##~~home 3~~

home3.next <- matrix(c(Frame = X..time[p.CYRUS.3.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.3.state == "k"])), 
                       Player = rep("3", times = length(X..time[p.CYRUS.3.state == "k"])),
                       state = p.CYRUS.3.state[p.CYRUS.3.state == "k"]), 
                     ncol = 4)
if(nrow(home3.next != 0))
{
  for(i in 1:nrow(home3.next))
  {
    if (!(home3.next[i, 1] %in% home3[, 1]))
    {
      home3 <- rbind(home3, home3.next[i, ])
    }
  }
}

rm(home3.next)

##~~home 4~~

home4.next <- matrix(c(Frame = X..time[p.CYRUS.4.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.4.state == "k"])), 
                       Player = rep("4", times = length(X..time[p.CYRUS.4.state == "k"])),
                       state = p.CYRUS.4.state[p.CYRUS.4.state == "k"]), 
                     ncol = 4)
if(nrow(home4.next != 0))
{
  for(i in 1:nrow(home4.next))
  {
    if (!(home4.next[i, 1] %in% home4[, 1]))
    {
      home4 <- rbind(home4, home4.next[i, ])
    }
  }
}

rm(home4.next)

##~~home 5~~

home5.next <- matrix(c(Frame = X..time[p.CYRUS.5.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.5.state == "k"])), 
                       Player = rep("5", times = length(X..time[p.CYRUS.5.state == "k"])),
                       state = p.CYRUS.5.state[p.CYRUS.5.state == "k"]), 
                     ncol = 4)
if(nrow(home5.next != 0))
{
  for(i in 1:nrow(home5.next))
  {
    if (!(home5.next[i, 1] %in% home5[, 1]))
    {
      home5 <- rbind(home5, home5.next[i, ])
    }
  }
}

rm(home5.next)

##~~home 6~~

home6.next <- matrix(c(Frame = X..time[p.CYRUS.6.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.6.state == "k"])), 
                       Player = rep("6", times = length(X..time[p.CYRUS.6.state == "k"])),
                       state = p.CYRUS.6.state[p.CYRUS.6.state == "k"]), 
                     ncol = 4)
if(nrow(home6.next != 0))
{
  for(i in 1:nrow(home6.next))
  {
    if (!(home6.next[i, 1] %in% home6[, 1]))
    {
      home6 <- rbind(home6, home6.next[i, ])
    }
  }
}

rm(home6.next)

##~~home 7~~

home7.next <- matrix(c(Frame = X..time[p.CYRUS.7.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.7.state == "k"])), 
                       Player = rep("7", times = length(X..time[p.CYRUS.7.state == "k"])),
                       state = p.CYRUS.7.state[p.CYRUS.7.state == "k"]), 
                     ncol = 4)
if(nrow(home7.next != 0))
{
  for(i in 1:nrow(home7.next))
  {
    if (!(home7.next[i, 1] %in% home7[, 1]))
    {
      home7 <- rbind(home7, home7.next[i, ])
    }
  }
}

rm(home7.next)

##~~home 8~~

home8.next <- matrix(c(Frame = X..time[p.CYRUS.8.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.8.state == "k"])), 
                       Player = rep("8", times = length(X..time[p.CYRUS.8.state == "k"])),
                       state = p.CYRUS.8.state[p.CYRUS.8.state == "k"]), 
                     ncol = 4)
if(nrow(home8.next != 0))
{
  for(i in 1:nrow(home8.next))
  {
    if (!(home8.next[i, 1] %in% home8[, 1]))
    {
      home8 <- rbind(home8, home8.next[i, ])
    }
  }
}

rm(home8.next)

##~~home 9~~

home9.next <- matrix(c(Frame = X..time[p.CYRUS.9.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.9.state == "k"])), 
                       Player = rep("9", times = length(X..time[p.CYRUS.9.state == "k"])),
                       state = p.CYRUS.9.state[p.CYRUS.9.state == "k"]), 
                     ncol = 4)
if(nrow(home9.next != 0))
{
  for(i in 1:nrow(home9.next))
  {
    if (!(home9.next[i, 1] %in% home9[, 1]))
    {
      home9 <- rbind(home9, home9.next[i, ])
    }
  }
}

rm(home9.next)

##~~home 10~~

home10.next <- matrix(c(Frame = X..time[p.CYRUS.10.state == "k"],
                        Team = rep("H", times = length(X..time[p.CYRUS.10.state == "k"])), 
                        Player = rep("10", times = length(X..time[p.CYRUS.10.state == "k"])),
                        state = p.CYRUS.10.state[p.CYRUS.10.state == "k"]), 
                      ncol = 4)
if(nrow(home10.next != 0))
{
  for(i in 1:nrow(home10.next))
  {
    if (!(home10.next[i, 1] %in% home10[, 1]))
    {
      home10 <- rbind(home10, home10.next[i, ])
    }
  }
}

rm(home10.next)

##~~home 11~~

home11.next <- matrix(c(Frame = X..time[p.CYRUS.11.state == "k"],
                        Team = rep("H", times = length(X..time[p.CYRUS.11.state == "k"])), 
                        Player = rep("11", times = length(X..time[p.CYRUS.11.state == "k"])),
                        state = p.CYRUS.11.state[p.CYRUS.11.state == "k"]), 
                      ncol = 4)
if(nrow(home11.next != 0))
{
  for(i in 1:nrow(home11.next))
  {
    if (!(home11.next[i, 1] %in% home11[, 1]))
    {
      home11 <- rbind(home11, home11.next[i, ])
    }
  }
}

rm(home11.next)

# Home 10 -----------------------------------------------------------------

##### Home robot 10 ## extracts tackles and kicks

data <- read.csv("20170905233547-CYRUS_2-vs-HfutEngine2017_1-CYRUS_10-moving.csv", head = TRUE)
attach(data)

##~~home 1~~

home1.next <- matrix(c(Frame = X..time[p.CYRUS.1.state == "kg"],
                       Team = rep("H", times = length(X..time[p.CYRUS.1.state == "kg"])),
                       Player = rep("1", times = length(X..time[p.CYRUS.1.state == "kg"])), 
                       state = p.CYRUS.1.state[p.CYRUS.1.state == "kg"]), 
                     ncol = 4)
for(i in 1:nrow(home1.next))
{
  if (!(home1.next[i, 1] %in% home1[, 1]))
  {
    home1 <- rbind(home1, home1.next[i, ])
  }
}

rm(home1.next)

##~~home 2~~

home2.next <- matrix(c(Frame = X..time[p.CYRUS.2.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.2.state == "k"])), 
                       Player = rep("2", times = length(X..time[p.CYRUS.2.state == "k"])),
                       state = p.CYRUS.2.state[p.CYRUS.2.state == "k"]), 
                     ncol = 4)
if(nrow(home2.next != 0))
{
  for(i in 1:nrow(home2.next))
  {
    if (!(home2.next[i, 1] %in% home2[, 1]))
    {
      home2 <- rbind(home2, home2.next[i, ])
    }
  }
}

rm(home2.next)

##~~home 3~~

home3.next <- matrix(c(Frame = X..time[p.CYRUS.3.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.3.state == "k"])), 
                       Player = rep("3", times = length(X..time[p.CYRUS.3.state == "k"])),
                       state = p.CYRUS.3.state[p.CYRUS.3.state == "k"]), 
                     ncol = 4)
if(nrow(home3.next != 0))
{
  for(i in 1:nrow(home3.next))
  {
    if (!(home3.next[i, 1] %in% home3[, 1]))
    {
      home3 <- rbind(home3, home3.next[i, ])
    }
  }
}

rm(home3.next)

##~~home 4~~

home4.next <- matrix(c(Frame = X..time[p.CYRUS.4.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.4.state == "k"])), 
                       Player = rep("4", times = length(X..time[p.CYRUS.4.state == "k"])),
                       state = p.CYRUS.4.state[p.CYRUS.4.state == "k"]), 
                     ncol = 4)
if(nrow(home4.next != 0))
{
  for(i in 1:nrow(home4.next))
  {
    if (!(home4.next[i, 1] %in% home4[, 1]))
    {
      home4 <- rbind(home4, home4.next[i, ])
    }
  }
}

rm(home4.next)

##~~home 5~~

home5.next <- matrix(c(Frame = X..time[p.CYRUS.5.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.5.state == "k"])), 
                       Player = rep("5", times = length(X..time[p.CYRUS.5.state == "k"])),
                       state = p.CYRUS.5.state[p.CYRUS.5.state == "k"]), 
                     ncol = 4)
if(nrow(home5.next != 0))
{
  for(i in 1:nrow(home5.next))
  {
    if (!(home5.next[i, 1] %in% home5[, 1]))
    {
      home5 <- rbind(home5, home5.next[i, ])
    }
  }
}

rm(home5.next)

##~~home 6~~

home6.next <- matrix(c(Frame = X..time[p.CYRUS.6.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.6.state == "k"])), 
                       Player = rep("6", times = length(X..time[p.CYRUS.6.state == "k"])),
                       state = p.CYRUS.6.state[p.CYRUS.6.state == "k"]), 
                     ncol = 4)
if(nrow(home6.next != 0))
{
  for(i in 1:nrow(home6.next))
  {
    if (!(home6.next[i, 1] %in% home6[, 1]))
    {
      home6 <- rbind(home6, home6.next[i, ])
    }
  }
}

rm(home6.next)

##~~home 7~~

home7.next <- matrix(c(Frame = X..time[p.CYRUS.7.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.7.state == "k"])), 
                       Player = rep("7", times = length(X..time[p.CYRUS.7.state == "k"])),
                       state = p.CYRUS.7.state[p.CYRUS.7.state == "k"]), 
                     ncol = 4)
if(nrow(home7.next != 0))
{
  for(i in 1:nrow(home7.next))
  {
    if (!(home7.next[i, 1] %in% home7[, 1]))
    {
      home7 <- rbind(home7, home7.next[i, ])
    }
  }
}

rm(home7.next)

##~~home 8~~

home8.next <- matrix(c(Frame = X..time[p.CYRUS.8.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.8.state == "k"])), 
                       Player = rep("8", times = length(X..time[p.CYRUS.8.state == "k"])),
                       state = p.CYRUS.8.state[p.CYRUS.8.state == "k"]), 
                     ncol = 4)
if(nrow(home8.next != 0))
{
  for(i in 1:nrow(home8.next))
  {
    if (!(home8.next[i, 1] %in% home8[, 1]))
    {
      home8 <- rbind(home8, home8.next[i, ])
    }
  }
}

rm(home8.next)

##~~home 9~~

home9.next <- matrix(c(Frame = X..time[p.CYRUS.9.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.9.state == "k"])), 
                       Player = rep("9", times = length(X..time[p.CYRUS.9.state == "k"])),
                       state = p.CYRUS.9.state[p.CYRUS.9.state == "k"]), 
                     ncol = 4)
if(nrow(home9.next != 0))
{
  for(i in 1:nrow(home9.next))
  {
    if (!(home9.next[i, 1] %in% home9[, 1]))
    {
      home9 <- rbind(home9, home9.next[i, ])
    }
  }
}

rm(home9.next)

##~~home 10~~

home10.next <- matrix(c(Frame = X..time[p.CYRUS.10.state == "k"],
                        Team = rep("H", times = length(X..time[p.CYRUS.10.state == "k"])), 
                        Player = rep("10", times = length(X..time[p.CYRUS.10.state == "k"])),
                        state = p.CYRUS.10.state[p.CYRUS.10.state == "k"]), 
                      ncol = 4)
if(nrow(home10.next != 0))
{
  for(i in 1:nrow(home10.next))
  {
    if (!(home10.next[i, 1] %in% home10[, 1]))
    {
      home10 <- rbind(home10, home10.next[i, ])
    }
  }
}

rm(home10.next)

##~~home 11~~

home11.next <- matrix(c(Frame = X..time[p.CYRUS.11.state == "k"],
                        Team = rep("H", times = length(X..time[p.CYRUS.11.state == "k"])), 
                        Player = rep("11", times = length(X..time[p.CYRUS.11.state == "k"])),
                        state = p.CYRUS.11.state[p.CYRUS.11.state == "k"]), 
                      ncol = 4)
if(nrow(home11.next != 0))
{
  for(i in 1:nrow(home11.next))
  {
    if (!(home11.next[i, 1] %in% home11[, 1]))
    {
      home11 <- rbind(home11, home11.next[i, ])
    }
  }
}

rm(home11.next)

# Home 11 -----------------------------------------------------------------

##### Home robot 11 ## extracts tackles and kicks

data <- read.csv("20170905233547-CYRUS_2-vs-HfutEngine2017_1-CYRUS_11-moving.csv", head = TRUE)
attach(data)

##~~home 1~~

home1.next <- matrix(c(Frame = X..time[p.CYRUS.1.state == "kg"],
                       Team = rep("H", times = length(X..time[p.CYRUS.1.state == "kg"])),
                       Player = rep("1", times = length(X..time[p.CYRUS.1.state == "kg"])), 
                       state = p.CYRUS.1.state[p.CYRUS.1.state == "kg"]), 
                     ncol = 4)
for(i in 1:nrow(home1.next))
{
  if (!(home1.next[i, 1] %in% home1[, 1]))
  {
    home1 <- rbind(home1, home1.next[i, ])
  }
}

rm(home1.next)

##~~home 2~~

home2.next <- matrix(c(Frame = X..time[p.CYRUS.2.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.2.state == "k"])), 
                       Player = rep("2", times = length(X..time[p.CYRUS.2.state == "k"])),
                       state = p.CYRUS.2.state[p.CYRUS.2.state == "k"]), 
                     ncol = 4)
if(nrow(home2.next != 0))
{
  for(i in 1:nrow(home2.next))
  {
    if (!(home2.next[i, 1] %in% home2[, 1]))
    {
      home2 <- rbind(home2, home2.next[i, ])
    }
  }
}

rm(home2.next)

##~~home 3~~

home3.next <- matrix(c(Frame = X..time[p.CYRUS.3.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.3.state == "k"])), 
                       Player = rep("3", times = length(X..time[p.CYRUS.3.state == "k"])),
                       state = p.CYRUS.3.state[p.CYRUS.3.state == "k"]), 
                     ncol = 4)
if(nrow(home3.next != 0))
{
  for(i in 1:nrow(home3.next))
  {
    if (!(home3.next[i, 1] %in% home3[, 1]))
    {
      home3 <- rbind(home3, home3.next[i, ])
    }
  }
}

rm(home3.next)

##~~home 4~~

home4.next <- matrix(c(Frame = X..time[p.CYRUS.4.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.4.state == "k"])), 
                       Player = rep("4", times = length(X..time[p.CYRUS.4.state == "k"])),
                       state = p.CYRUS.4.state[p.CYRUS.4.state == "k"]), 
                     ncol = 4)
if(nrow(home4.next != 0))
{
  for(i in 1:nrow(home4.next))
  {
    if (!(home4.next[i, 1] %in% home4[, 1]))
    {
      home4 <- rbind(home4, home4.next[i, ])
    }
  }
}

rm(home4.next)

##~~home 5~~

home5.next <- matrix(c(Frame = X..time[p.CYRUS.5.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.5.state == "k"])), 
                       Player = rep("5", times = length(X..time[p.CYRUS.5.state == "k"])),
                       state = p.CYRUS.5.state[p.CYRUS.5.state == "k"]), 
                     ncol = 4)
if(nrow(home5.next != 0))
{
  for(i in 1:nrow(home5.next))
  {
    if (!(home5.next[i, 1] %in% home5[, 1]))
    {
      home5 <- rbind(home5, home5.next[i, ])
    }
  }
}

rm(home5.next)

##~~home 6~~

home6.next <- matrix(c(Frame = X..time[p.CYRUS.6.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.6.state == "k"])), 
                       Player = rep("6", times = length(X..time[p.CYRUS.6.state == "k"])),
                       state = p.CYRUS.6.state[p.CYRUS.6.state == "k"]), 
                     ncol = 4)
if(nrow(home6.next != 0))
{
  for(i in 1:nrow(home6.next))
  {
    if (!(home6.next[i, 1] %in% home6[, 1]))
    {
      home6 <- rbind(home6, home6.next[i, ])
    }
  }
}

rm(home6.next)

##~~home 7~~

home7.next <- matrix(c(Frame = X..time[p.CYRUS.7.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.7.state == "k"])), 
                       Player = rep("7", times = length(X..time[p.CYRUS.7.state == "k"])),
                       state = p.CYRUS.7.state[p.CYRUS.7.state == "k"]), 
                     ncol = 4)
if(nrow(home7.next != 0))
{
  for(i in 1:nrow(home7.next))
  {
    if (!(home7.next[i, 1] %in% home7[, 1]))
    {
      home7 <- rbind(home7, home7.next[i, ])
    }
  }
}

rm(home7.next)

##~~home 8~~

home8.next <- matrix(c(Frame = X..time[p.CYRUS.8.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.8.state == "k"])), 
                       Player = rep("8", times = length(X..time[p.CYRUS.8.state == "k"])),
                       state = p.CYRUS.8.state[p.CYRUS.8.state == "k"]), 
                     ncol = 4)
if(nrow(home8.next != 0))
{
  for(i in 1:nrow(home8.next))
  {
    if (!(home8.next[i, 1] %in% home8[, 1]))
    {
      home8 <- rbind(home8, home8.next[i, ])
    }
  }
}

rm(home8.next)

##~~home 9~~

home9.next <- matrix(c(Frame = X..time[p.CYRUS.9.state == "k"],
                       Team = rep("H", times = length(X..time[p.CYRUS.9.state == "k"])), 
                       Player = rep("9", times = length(X..time[p.CYRUS.9.state == "k"])),
                       state = p.CYRUS.9.state[p.CYRUS.9.state == "k"]), 
                     ncol = 4)
if(nrow(home9.next != 0))
{
  for(i in 1:nrow(home9.next))
  {
    if (!(home9.next[i, 1] %in% home9[, 1]))
    {
      home9 <- rbind(home9, home9.next[i, ])
    }
  }
}

rm(home9.next)

##~~home 10~~

home10.next <- matrix(c(Frame = X..time[p.CYRUS.10.state == "k"],
                        Team = rep("H", times = length(X..time[p.CYRUS.10.state == "k"])), 
                        Player = rep("10", times = length(X..time[p.CYRUS.10.state == "k"])),
                        state = p.CYRUS.10.state[p.CYRUS.10.state == "k"]), 
                      ncol = 4)
if(nrow(home10.next != 0))
{
  for(i in 1:nrow(home10.next))
  {
    if (!(home10.next[i, 1] %in% home10[, 1]))
    {
      home10 <- rbind(home10, home10.next[i, ])
    }
  }
}

rm(home10.next)

##~~home 11~~

home11.next <- matrix(c(Frame = X..time[p.CYRUS.11.state == "k"],
                        Team = rep("H", times = length(X..time[p.CYRUS.11.state == "k"])), 
                        Player = rep("11", times = length(X..time[p.CYRUS.11.state == "k"])),
                        state = p.CYRUS.11.state[p.CYRUS.11.state == "k"]), 
                      ncol = 4)
if(nrow(home11.next != 0))
{
  for(i in 1:nrow(home11.next))
  {
    if (!(home11.next[i, 1] %in% home11[, 1]))
    {
      home11 <- rbind(home11, home11.next[i, ])
    }
  }
}

rm(home11.next)

home1 <- home1[, 1:3]
home2 <- home2[, 1:3]
home3 <- home3[, 1:3]
home4 <- home4[, 1:3]
home5 <- home5[, 1:3]
home6 <- home6[, 1:3]
home7 <- home7[, 1:3]
home8 <- home8[, 1:3]
home9 <- home9[, 1:3]
home10 <- home10[, 1:3]
home11 <- home11[, 1:3]

# Combining groundtruth data ----------------------------------------------

attach(groundtruth)

##~~home 1~~

# Extracts the coordinates for the ball, and the opposition keeper with respect to the frames where the kicks occur. 

frames = home1[, 1]
groundtruth1 <- matrix(ncol = 7)
for (i in frames)
{
  groundtruth1 <- rbind(groundtruth1,
                        c(playmode[X..time == i],
                          unique(ball_x[X..time == as.numeric(i)])[1],
                          unique(ball_y[X..time == as.numeric(i)])[1],
                          unique(ball_x[X..time == as.numeric(i)+1][1]),
                          unique(ball_y[X..time == as.numeric(i)+1][1]),
                          unique(RG1.x[X..time == as.numeric(i)+1][1]),
                          unique(RG1.y[X..time == as.numeric(i)+1][1])))
}
groundtruth1 <- groundtruth1[2:nrow(groundtruth1), ]
colnames(groundtruth1) <- c("Play mode", "BallX", "BallY", "newBallX", "newBallY", "OppKeeperX", "OppKeeperY")
home1 <- cbind(home1, groundtruth1)
rm(groundtruth1)

##~~home 2~~

frames = home2[, 1]
groundtruth2 <- matrix(ncol = 7)
for (i in frames)
{
  groundtruth2 <- rbind(groundtruth2, 
                        c(playmode[X..time == i][1],
                          ball_x[X..time == as.numeric(i)][1],
                          ball_y[X..time == as.numeric(i)][1],
                          unique(ball_x[X..time == as.numeric(i)+1][1]),
                          unique(ball_y[X..time == as.numeric(i)+1][1]),
                          unique(RG1.x[X..time == as.numeric(i)+1][1]),
                          unique(RG1.y[X..time == as.numeric(i)+1][1])))
}
groundtruth2 <- groundtruth2[2:nrow(groundtruth2), ]
colnames(groundtruth2) <- c("Play mode", "BallX", "BallY", "newBallX", "newBallY", "OppKeeperX", "OppKeeperY")
home2 <- cbind(home2, groundtruth2)
rm(groundtruth2)

##~~home 3~~

frames = home3[, 1]
groundtruth3 <- matrix(ncol = 7)
for (i in frames)
{
  groundtruth3 <- rbind(groundtruth3, 
                        c(playmode[X..time == i][1],
                          ball_x[X..time == as.numeric(i)][1],
                          ball_y[X..time == as.numeric(i)][1],
                          unique(ball_x[X..time == as.numeric(i)+1][1]),
                          unique(ball_y[X..time == as.numeric(i)+1][1]),
                          unique(RG1.x[X..time == as.numeric(i)+1][1]),
                          unique(RG1.y[X..time == as.numeric(i)+1][1])))
}
groundtruth3 <- groundtruth3[2:nrow(groundtruth3), ]
colnames(groundtruth3) <- c("Play mode", "BallX", "BallY", "newBallX", "newBallY", "OppKeeperX", "OppKeeperY")
home3 <- cbind(home3, groundtruth3)
rm(groundtruth3)

##~~home 4~~

frames = home4[, 1]
groundtruth4 <- matrix(ncol = 7)
for (i in frames)
{
  groundtruth4 <- rbind(groundtruth4, 
                        c(playmode[X..time == i][1],
                          ball_x[X..time == as.numeric(i)][1],
                          ball_y[X..time == as.numeric(i)][1],
                          unique(ball_x[X..time == as.numeric(i)+1][1]),
                          unique(ball_y[X..time == as.numeric(i)+1][1]),
                          unique(RG1.x[X..time == as.numeric(i)+1][1]),
                          unique(RG1.y[X..time == as.numeric(i)+1][1])))
}
groundtruth4 <- groundtruth4[2:nrow(groundtruth4), ]
colnames(groundtruth4) <- c("Play mode", "BallX", "BallY", "newBallX", "newBallY", "OppKeeperX", "OppKeeperY")
home4 <- cbind(home4, groundtruth4)
rm(groundtruth4)

##~~home 5~~

frames = home5[, 1]
groundtruth5 <- matrix(ncol = 7)
for (i in frames)
{
  groundtruth5 <- rbind(groundtruth5, 
                        c(playmode[X..time == i][1],
                          ball_x[X..time == as.numeric(i)][1],
                          ball_y[X..time == as.numeric(i)][1],
                          unique(ball_x[X..time == as.numeric(i)+1][1]),
                          unique(ball_y[X..time == as.numeric(i)+1][1]),
                          unique(RG1.x[X..time == as.numeric(i)+1][1]),
                          unique(RG1.y[X..time == as.numeric(i)+1][1])))
}
groundtruth5 <- groundtruth5[2:nrow(groundtruth5), ]
colnames(groundtruth5) <- c("Play mode", "BallX", "BallY", "newBallX", "newBallY", "OppKeeperX", "OppKeeperY")
home5 <- cbind(home5, groundtruth5)
rm(groundtruth5)

##~~home 6~~

frames = home6[, 1]
groundtruth6 <- matrix(ncol = 7)
for (i in frames)
{
  groundtruth6 <- rbind(groundtruth6, 
                        c(playmode[X..time == i][1],
                          ball_x[X..time == as.numeric(i)][1],
                          ball_y[X..time == as.numeric(i)][1],
                          unique(ball_x[X..time == as.numeric(i)+1][1]),
                          unique(ball_y[X..time == as.numeric(i)+1][1]),
                          unique(RG1.x[X..time == as.numeric(i)+1][1]),
                          unique(RG1.y[X..time == as.numeric(i)+1][1])))
}
groundtruth6 <- groundtruth6[2:nrow(groundtruth6), ]
colnames(groundtruth6) <- c("Play mode", "BallX", "BallY", "newBallX", "newBallY", "OppKeeperX", "OppKeeperY")
home6 <- cbind(home6, groundtruth6)
rm(groundtruth6)

##~~home 7~~

frames = home7[, 1]
groundtruth7 <- matrix(ncol = 7)
for (i in frames)
{
  groundtruth7 <- rbind(groundtruth7, 
                        c(playmode[X..time == i][1],
                          ball_x[X..time == as.numeric(i)][1],
                          ball_y[X..time == as.numeric(i)][1],
                          unique(ball_x[X..time == as.numeric(i)+1][1]),
                          unique(ball_y[X..time == as.numeric(i)+1][1]),
                          unique(RG1.x[X..time == as.numeric(i)+1][1]),
                          unique(RG1.y[X..time == as.numeric(i)+1][1])))
}
groundtruth7 <- groundtruth7[2:nrow(groundtruth7), ]
colnames(groundtruth7) <- c("Play mode", "BallX", "BallY", "newBallX", "newBallY", "OppKeeperX", "OppKeeperY")
home7 <- cbind(home7, groundtruth7)
rm(groundtruth7)

##~~home 8~~

frames = home8[, 1]
groundtruth8 <- matrix(ncol = 7)
for (i in frames)
{
  groundtruth8 <- rbind(groundtruth8, 
                        c(playmode[X..time == i][1],
                          ball_x[X..time == as.numeric(i)][1],
                          ball_y[X..time == as.numeric(i)][1],
                          unique(ball_x[X..time == as.numeric(i)+1][1]),
                          unique(ball_y[X..time == as.numeric(i)+1][1]),
                          unique(RG1.x[X..time == as.numeric(i)+1][1]),
                          unique(RG1.y[X..time == as.numeric(i)+1][1])))
}
groundtruth8 <- groundtruth8[2:nrow(groundtruth8), ]
colnames(groundtruth8) <- c("Play mode", "BallX", "BallY", "newBallX", "newBallY", "OppKeeperX", "OppKeeperY")
home8 <- cbind(home8, groundtruth8)
rm(groundtruth8)

##~~home 9~~

frames = home9[, 1]
groundtruth9 <- matrix(ncol = 7)
for (i in frames)
{
  groundtruth9 <- rbind(groundtruth9, 
                        c(playmode[X..time == i][1],
                          ball_x[X..time == as.numeric(i)][1],
                          ball_y[X..time == as.numeric(i)][1],
                          unique(ball_x[X..time == as.numeric(i)+1][1]),
                          unique(ball_y[X..time == as.numeric(i)+1][1]),
                          unique(RG1.x[X..time == as.numeric(i)+1][1]),
                          unique(RG1.y[X..time == as.numeric(i)+1][1])))
}
groundtruth9 <- groundtruth9[2:nrow(groundtruth9), ]
colnames(groundtruth9) <- c("Play mode", "BallX", "BallY", "newBallX", "newBallY", "OppKeeperX", "OppKeeperY")
home9 <- cbind(home9, groundtruth9)
rm(groundtruth9)

##~~home 10~~

frames = home10[, 1]
groundtruth10 <- matrix(ncol = 7)
for (i in frames)
{
  groundtruth10 <- rbind(groundtruth10, 
                        c(playmode[X..time == i][1],
                          ball_x[X..time == as.numeric(i)][1],
                          ball_y[X..time == as.numeric(i)][1],
                          unique(ball_x[X..time == as.numeric(i)+1][1]),
                          unique(ball_y[X..time == as.numeric(i)+1][1]),
                          unique(RG1.x[X..time == as.numeric(i)+1][1]),
                          unique(RG1.y[X..time == as.numeric(i)+1][1])))
}
groundtruth10 <- groundtruth10[2:nrow(groundtruth10), ]
colnames(groundtruth10) <- c("Play mode", "BallX", "BallY", "newBallX", "newBallY", "OppKeeperX", "OppKeeperY")
home10 <- cbind(home10, groundtruth10)
rm(groundtruth10)

##~~home 11~~

frames = home11[, 1]
groundtruth11 <- matrix(ncol = 7)
for (i in frames)
{
  groundtruth11 <- rbind(groundtruth11, 
                        c(playmode[X..time == i][1],
                          ball_x[X..time == as.numeric(i)][1],
                          ball_y[X..time == as.numeric(i)][1],
                          unique(ball_x[X..time == as.numeric(i)+1][1]),
                          unique(ball_y[X..time == as.numeric(i)+1][1]),
                          unique(RG1.x[X..time == as.numeric(i)+1][1]),
                          unique(RG1.y[X..time == as.numeric(i)+1][1])))
}
groundtruth11 <- groundtruth11[2:nrow(groundtruth11), ]
colnames(groundtruth11) <- c("Play mode", "BallX", "BallY", "newBallX", "newBallY", "OppKeeperX", "OppKeeperY")
home11 <- cbind(home11, groundtruth11)
rm(groundtruth11)

# Calculating distances and classifying goals---------------------------------------------------

save.image(file = "C:/Users/muson/OneDrive/Documents/School/Stats Honors/Thesis/Code/R/Thesis.RData")

rm(list = ls())

load("C:/Users/muson/OneDrive/Documents/School/Stats Honors/Thesis/Code/R/Thesis.RData")

## Classifies the kicks as passes or kicks dependent on the distance between where the ball lands and the opposition goal keeper.


full.data <- as.data.frame(rbind(home1, home2, home3, home4, home5, home6, 
                   home7, home8, home9, home10, home11))
rm(home1, home2, home3, home4, home5, home6, 
   home7, home8, home9, home10, home11, 
   data, moving, moving2, 
   frames)
full.data$Frame <- as.numeric(full.data$Frame)
full.data$Team <- as.factor(full.data$Team)
full.data$Player <- as.factor(full.data$Player)
full.data$BallX <- as.numeric(full.data$BallX)
full.data$BallY <- as.numeric(full.data$BallY)
full.data$newBallX <- as.numeric(full.data$newBallX)
full.data$newBallY <- as.numeric(full.data$newBallY)
full.data$OppKeeperX <- as.numeric(full.data$OppKeeperX)
full.data$OppKeeperY<- as.numeric(full.data$OppKeeperY)
full.data$distance <- NA
full.data$actiotype <- NA
full.data$result <- "failure"

str(full.data)

for (i in 1:nrow(full.data))
{
  full.data$distance[i] <- dist(matrix(c(full.data$newBallX[i], full.data$newBallY[i], 
                                      full.data$OppKeeperX[i], full.data$OppKeeperY[i]), 
                                    byrow = TRUE, ncol = 2),
                             method = "euclidean")
  ifelse(full.data$distance[i] < 6, full.data$actiotype[i] <- "shot", full.data$actiotype[i] <- "pass") #if distance is less than 6 metres then kick was a shot.
}

full.data <- full.data[order(full.data$Frame), ]
full.data <- full.data[, c(1, 2, 3, 5, 6, 7, 8, 12, 13)]

full.data$result[full.data$Frame == 2197] <- "success"
full.data$result[full.data$Frame == 5473] <- "success"

View(full.data)

# Extracting dashes -------------------------------------------------------

attach(groundtruth)

## selects all indices where playmode == "play_on" (because dribbles can only occur during this playmode)
## then removes the frames where the kicks occur and calculates the distance between the ball and each
## player to determine which player dribbles the ball.

indices <- (sqldf('SELECT "X..time" AS "frames" FROM "groundtruth" where "playmode" == " play_on"'))

dashes.indices <- indices[!(indices[, 1] %in% full.data$Frame), ]

##~~Home 1~~

home1 <- matrix(ncol = 9)
j <- 1

for(i in dashes.indices)
{
  coords <- matrix(c(unique(ball_x[X..time == i])[1], unique(ball_y[X..time == i])[1], 
                     unique(LG1.x[X..time == i])[1], unique(LG1.y[X..time == i])[1]), 
                   ncol = 2, byrow = TRUE)
  distance <- dist(coords, method = "euclidean")
  if(distance < 2)
  {
    home1 <- rbind(home1, c(i,
                            "H", 
                            "1",
                            unique(ball_x[X..time == i])[1],
                            unique(ball_y[X..time == i])[1],
                            unique(ball_x[X..time == (i + 1)])[1],
                            unique(ball_x[X..time == (i + 1)])[1], 
                            "dribble", 
                            "failure"))
  }
  j = j + 1
}
home1 <- home1[2:nrow(home1), ]

##~~Home 2~~

dashes.indices <- dashes.indices[!(dashes.indices %in% home1[, 1])]

home2 <- matrix(ncol = 9)
j <- 1

for(i in dashes.indices)
{
  coords <- matrix(c(unique(ball_x[X..time == i])[1], unique(ball_y[X..time == i])[1], 
                     unique(L2.x[X..time == i])[1], unique(L2.y[X..time == i])[1]), 
                   ncol = 2, byrow = TRUE)
  distance <- dist(coords, method = "euclidean")
  if(distance < 2)
  {
    home2 <- rbind(home2, c(i,
                            "H", 
                            "2",
                            unique(ball_x[X..time == i])[1],
                            unique(ball_y[X..time == i])[1],
                            unique(ball_x[X..time == (i + 1)])[1],
                            unique(ball_x[X..time == (i + 1)])[1], 
                            "dribble", 
                            "failure"))
  }
  j = j + 1
}
home2 <- home2[2:nrow(home1), ]

##~~Home 3~~

dashes.indices <- dashes.indices[!(dashes.indices %in% home2[, 1])]

home3 <- matrix(ncol = 9)
j <- 1

for(i in dashes.indices)
{
  coords <- matrix(c(unique(ball_x[X..time == i])[1], unique(ball_y[X..time == i])[1], 
                     unique(L3.x[X..time == i])[1], unique(L3.y[X..time == i])[1]), 
                   ncol = 2, byrow = TRUE)
  distance <- dist(coords, method = "euclidean")
  if(distance < 2)
  {
    home3 <- rbind(home3, c(i,
                            "H", 
                            "3",
                            unique(ball_x[X..time == i])[1],
                            unique(ball_y[X..time == i])[1],
                            unique(ball_x[X..time == (i + 1)])[1],
                            unique(ball_x[X..time == (i + 1)])[1], 
                            "dribble", 
                            "failure"))
  }
  j = j + 1
}
home3 <- home3[2:nrow(home1), ]

##~~Home 4~~

dashes.indices <- dashes.indices[!(dashes.indices %in% home3[, 1])]

home4 <- matrix(ncol = 9)
j <- 1

for(i in dashes.indices)
{
  coords <- matrix(c(unique(ball_x[X..time == i])[1], unique(ball_y[X..time == i])[1], 
                     unique(L4.x[X..time == i])[1], unique(L4.y[X..time == i])[1]), 
                   ncol = 2, byrow = TRUE)
  distance <- dist(coords, method = "euclidean")
  if(distance < 2)
  {
    home4 <- rbind(home4, c(i,
                            "H", 
                            "4",
                            unique(ball_x[X..time == i])[1],
                            unique(ball_y[X..time == i])[1],
                            unique(ball_x[X..time == (i + 1)])[1],
                            unique(ball_x[X..time == (i + 1)])[1], 
                            "dribble", 
                            "failure"))
  }
  j = j + 1
}
home4 <- home4[2:nrow(home1), ]

##~~Home 5~~

dashes.indices <- dashes.indices[!(dashes.indices %in% home4[, 1])]

home5 <- matrix(ncol = 9)
j <- 1

for(i in dashes.indices)
{
  coords <- matrix(c(unique(ball_x[X..time == i])[1], unique(ball_y[X..time == i])[1], 
                     unique(L5.x[X..time == i])[1], unique(L5.y[X..time == i])[1]), 
                   ncol = 2, byrow = TRUE)
  distance <- dist(coords, method = "euclidean")
  if(distance < 2)
  {
    home5 <- rbind(home5, c(i,
                            "H", 
                            "5",
                            unique(ball_x[X..time == i])[1],
                            unique(ball_y[X..time == i])[1],
                            unique(ball_x[X..time == (i + 1)])[1],
                            unique(ball_x[X..time == (i + 1)])[1], 
                            "dribble", 
                            "failure"))
  }
  j = j + 1
}
home5 <- home5[2:nrow(home1), ]

##~~Home 6~~

dashes.indices <- dashes.indices[!(dashes.indices %in% home5[, 1])]

home6 <- matrix(ncol = 9)
j <- 1

for(i in dashes.indices)
{
  coords <- matrix(c(unique(ball_x[X..time == i])[1], unique(ball_y[X..time == i])[1], 
                     unique(L6.x[X..time == i])[1], unique(L6.y[X..time == i])[1]), 
                   ncol = 2, byrow = TRUE)
  distance <- dist(coords, method = "euclidean")
  if(distance < 2)
  {
    home6 <- rbind(home6, c(i,
                            "H", 
                            "6",
                            unique(ball_x[X..time == i])[1],
                            unique(ball_y[X..time == i])[1],
                            unique(ball_x[X..time == (i + 1)])[1],
                            unique(ball_x[X..time == (i + 1)])[1], 
                            "dribble", 
                            "failure"))
  }
  j = j + 1
}
home6 <- home6[2:nrow(home1), ]

##~~Home 7~~

dashes.indices <- dashes.indices[!(dashes.indices %in% home6[, 1])]

home7 <- matrix(ncol = 9)
j <- 1

for(i in dashes.indices)
{
  coords <- matrix(c(unique(ball_x[X..time == i])[1], unique(ball_y[X..time == i])[1], 
                     unique(L7.x[X..time == i])[1], unique(L7.y[X..time == i])[1]), 
                   ncol = 2, byrow = TRUE)
  distance <- dist(coords, method = "euclidean")
  if(distance < 2)
  {
    home7 <- rbind(home7, c(i,
                            "H", 
                            "7",
                            unique(ball_x[X..time == i])[1],
                            unique(ball_y[X..time == i])[1],
                            unique(ball_x[X..time == (i + 1)])[1],
                            unique(ball_x[X..time == (i + 1)])[1], 
                            "dribble", 
                            "failure"))
  }
  j = j + 1
}
home7 <- home7[2:nrow(home1), ]

##~~Home 8~~

dashes.indices <- dashes.indices[!(dashes.indices %in% home7[, 1])]

home8 <- matrix(ncol = 9)
j <- 1

for(i in dashes.indices)
{
  coords <- matrix(c(unique(ball_x[X..time == i])[1], unique(ball_y[X..time == i])[1], 
                     unique(L8.x[X..time == i])[1], unique(L8.y[X..time == i])[1]), 
                   ncol = 2, byrow = TRUE)
  distance <- dist(coords, method = "euclidean")
  if(distance < 2)
  {
    home8 <- rbind(home8, c(i,
                            "H", 
                            "8",
                            unique(ball_x[X..time == i])[1],
                            unique(ball_y[X..time == i])[1],
                            unique(ball_x[X..time == (i + 1)])[1],
                            unique(ball_x[X..time == (i + 1)])[1], 
                            "dribble", 
                            "failure"))
  }
  j = j + 1
}
home8 <- home8[2:nrow(home1), ]

##~~Home 9~~

dashes.indices <- dashes.indices[!(dashes.indices %in% home8[, 1])]

home9 <- matrix(ncol = 9)
j <- 1

for(i in dashes.indices)
{
  coords <- matrix(c(unique(ball_x[X..time == i])[1], unique(ball_y[X..time == i])[1], 
                     unique(L9.x[X..time == i])[1], unique(L9.y[X..time == i])[1]), 
                   ncol = 2, byrow = TRUE)
  distance <- dist(coords, method = "euclidean")
  if(distance < 2)
  {
    home9 <- rbind(home9, c(i,
                            "H", 
                            "9",
                            unique(ball_x[X..time == i])[1],
                            unique(ball_y[X..time == i])[1],
                            unique(ball_x[X..time == (i + 1)])[1],
                            unique(ball_x[X..time == (i + 1)])[1], 
                            "dribble", 
                            "failure"))
  }
  j = j + 1
}
home9 <- home9[2:nrow(home1), ]

##~~Home 10~~

dashes.indices <- dashes.indices[!(dashes.indices %in% home8[, 1])]

home10 <- matrix(ncol = 9)
j <- 1

for(i in dashes.indices)
{
  coords <- matrix(c(unique(ball_x[X..time == i])[1], unique(ball_y[X..time == i])[1], 
                     unique(L10.x[X..time == i])[1], unique(L10.y[X..time == i])[1]), 
                   ncol = 2, byrow = TRUE)
  distance <- dist(coords, method = "euclidean")
  if(distance < 2)
  {
    home10 <- rbind(home10, c(i,
                            "H", 
                            "10",
                            unique(ball_x[X..time == i])[1],
                            unique(ball_y[X..time == i])[1],
                            unique(ball_x[X..time == (i + 1)])[1],
                            unique(ball_x[X..time == (i + 1)])[1], 
                            "dribble", 
                            "failure"))
  }
  j = j + 1
}
home10 <- home10[2:nrow(home1), ]

##~~Home 11~~

dashes.indices <- dashes.indices[!(dashes.indices %in% home10[, 1])]

home11 <- matrix(ncol = 9)
j <- 1

for(i in dashes.indices)
{
  coords <- matrix(c(unique(ball_x[X..time == i])[1], unique(ball_y[X..time == i])[1], 
                     unique(L11.x[X..time == i])[1], unique(L11.y[X..time == i])[1]), 
                   ncol = 2, byrow = TRUE)
  distance <- dist(coords, method = "euclidean")
  if(distance < 2)
  {
    home11 <- rbind(home11, c(i,
                            "H", 
                            "11",
                            unique(ball_x[X..time == i])[1],
                            unique(ball_y[X..time == i])[1],
                            unique(ball_x[X..time == (i + 1)])[1],
                            unique(ball_x[X..time == (i + 1)])[1], 
                            "dribble", 
                            "failure"))
  }
  j = j + 1
}
home11 <- home11[2:nrow(home1), ]

all.dashes <- rbind(home1, home2, home3, home4, home5,
                    home6, home7, home8, home9, home10)
colnames(all.dashes) <- colnames(full.data)

full.data <- rbind(full.data, 
                   as.data.frame(all.dashes))

full.data$Frame <- as.numeric(full.data$Frame)
full.data$BallX <- as.numeric(full.data$BallX)
full.data$BallY <- as.numeric(full.data$BallY)
full.data$newBallX <- as.numeric(full.data$newBallX)
full.data$newBallY <- as.numeric(full.data$newBallY)

names(full.data) <- c("frame", "team", "player", "start_x", "start_y", "end_x", "end_y", "actiontype", "result")

str(full.data)

full.data <- full.data[order(full.data$frame), ]

save.image(file = "C:/Users/muson/OneDrive/Documents/School/Stats Honors/Thesis/Code/R/with_dashes.RData")

rm(list = ls())

load("C:/Users/muson/OneDrive/Documents/School/Stats Honors/Thesis/Code/R/with_dashes.RData")

rm(home1, home2, home3, home4, home5,
   home6, home7, home8, home9, home10,
   home11, indices, i, j, distance,
   dashes.indices, coords)

attach(full.data)

frames <- frame[actiontype == "pass"|actiontype == "dribble"][1:length(frame[actiontype == "pass"|actiontype == "dribble"])-1]

for (j in 1:(length(frames)-1))
{
  if(frames[j + 1] == (frames[j] + 1))
  {
    full.data$result[full.data$frame == frames[j]] <- "success"
  }
  j = j + 1
}

## determines whether an action was succesful or not. if the next frame collected is equal to the current frame + 1 then the team stil has 
## possession and the access was succesful.

full.data$bodypart <- "foot"

rm(all.dashes, frames, j)

save.image(file = "C:/Users/muson/OneDrive/Documents/School/Stats Honors/Thesis/Code/R/full_data.RData")

rm(list = ls())

load("C:/Users/muson/OneDrive/Documents/School/Stats Honors/Thesis/Code/R/full_data.RData")

