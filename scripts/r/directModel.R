

#read in events
events <- read.csv("C:/Users/David/OneDrive/Documents/Work/Thesis/Data/events.csv")
events_sub <- events %>%
  subset(action %in% c("shot") & 
           !is.na(eventEndPosX) &
           !is.na(eventEndPosY)) %>%
  mutate(
    eventEndPosX = ifelse(
      action == "shot",
      ifelse(
        team == "l",
        107,
        0
      ),
      eventEndPosX
    ),
    eventEndPosY = ifelse(
      action == "shot",
      35,
      eventEndPosY
    )
  )

shots <- events_sub %>% subset(action == "shot")
passes <- events_sub %>% subset(action == "pass")

get_traj_data <- function(names,                     #list of names columns which will be explanatory variables 
                          path = "C:/Users/David/OneDrive/Documents/Work/Thesis/Data/matches_formatted.zip",
                          events_data = events_sub)      #data frame containing events
{
  
  matches <- events_data$matchNo %>% unique
  #set.seed(100)
  #sub <- sample(size = 50,x = length(matches))
  #matches <- matches[sub]
  
  to_arr <- function(df){
    return(df[,3:(3+length(names))] %>% 
             split(df$team) %>% 
             lapply(FUN = function(x){return(x[,-c(1)])}) %>%
             abind(along=3))
  }
  
  standardize <- function(col){
    maxim <- max(col)
    return(col/(maxim))
  }
  
  get_in_out <- function(match_number){
    file <- grep(paste0('.*/',match_number,'-'),unzip(path, list=TRUE)$Name,value=T)
    match <- unz(description = path,filename = file) %>%
      read.csv()
    evs <- subset(match,action %in% c("shot"))$frame
    temp <- match %>%
      mutate(
        eventEndPosX = ifelse(
          action == "shot",
          ifelse(
            team == "l",
            107,
            0
          ),
          eventEndPosX
        ),
        eventEndPosY = ifelse(
          action == "shot",
          35,
          eventEndPosY
        )
      ) %>%
      subset ( select = c('frame',"matchNo","team","player",
                         names,"eventEndPosX","eventEndPosY")) %>%
      subset(frame %in% (subset(events_data,matchNo == match_number))$frame &
               frame %in% evs &
               player != 1) %>% 
      distinct(frame,matchNo,player,.keep_all = T) %>%
      subset(select = c('frame',"matchNo","team",
                        names,"eventEndPosX","eventEndPosY")) %>%
      mutate(
        team = team %>% as.factor() %>% as.numeric()
      )
    
    temp[,names] <- temp[,names] %>%
      apply(FUN = standardize,MARGIN=2)
    
    input <- (temp %>%
                subset(select = -c(eventEndPosX,eventEndPosY)) %>%
                group_by(frame,matchNo) %>%
                do(arr = to_arr(.)))$arr %>% 
                abind(rev.along = 0) %>%
                aperm(c(4,2,1,3)) %>%
                unname
    #output <- (temp %>%
    #             subset(action != "") %>%
    #             mutate(
    #               action = action %>% 
    #                 as.factor %>% 
    #                 as.integer
    #             ) %>%
    #             subset(select = action) %>%
    #             unlist %>%
    #             array(dim=length(.)) - 1) %>%
    #             to_categorical(num_classes = 3)
    
    output <- temp %>%
      subset(frame %in% evs) %>%
      subset(select = c(eventEndPosX,eventEndPosY)) %>%
      na.omit %>%
      mutate(
        eventEndPosX = eventEndPosX %>% trimws %>% as.numeric %>% standardize,
        eventEndPosY = eventEndPosY %>% trimws %>% as.numeric %>% standardize,
      ) %>%
      array(dim=length(.))
    
    return(list(x = input,y = output))
  }
  
  combine_arrays <- function(list_of_arr){
    get_in <- function(list){
      return(list$x)
    }
    get_out <- function(list){
      return(list$y)
    }
    
    input <- list_of_arr %>%
      lapply(FUN = get_in) %>%
      abind(along=1) %>%
      unname
    output <- list_of_arr %>%
      lapply(FUN = get_out) %>%
      abind(along=1) %>%
      unname
    
    return(list(x = input, y = output))
  }
  
  ret <- matches %>%
    pblapply(get_in_out) %>%
    combine_arrays()
  
  return(ret)
  
}


data <- get_traj_data(names = c("x","y","vX","vY"))

save(data,file = "C:/Users/David/OneDrive/Documents/Work/Thesis/Data/shotsInputs")

load("C:/Users/David/OneDrive/Documents/Work/Thesis/Data/endPosInputs")
load("C:/Users/David/OneDrive/Documents/Work/Thesis/Data/noDribsInputs")

input <- data$x
output <- data$y

ball_info <- events %>% 
  #subset(matchNo %in% sub) %>%
  subset((action %in% c("pass", "shot","dribble")) & 
           !is.na(eventEndPosX) & 
           !is.na(eventEndPosY), 
         select = c(ballPosX, ballPosY, ballSpeed, ballAngle)) %>%
  apply(
    FUN = function(x){x/max(x)},
    MARGIN=2
  ) %>%
  unlist %>%
  array(dim = c((input %>% dim)[1], 4))

shot_info <- shots %>%  
  subset((action %in% c("shot")), 
         select = c(eventSuccess)) %>%
  unlist %>%
  array(dim=length(.))


set.seed(1886)
train <- sample(nrow(input),0.8*nrow(input))
ball_train <- ball_info[train,]
ball_test <- ball_info[-train,]
shot_train <- shot_info[train,]
shot_test <- shot_info[-train,]
x_train <- input[train,,,]
y_train <- output[train,]
x_test <- input[-train,,,]
y_test <- output[-train,]

aux_input <- layer_input(shape = as.numeric(dim(ball_info)[2]), name = 'aux_input')

main_input <- layer_input(shape = as.numeric(dim(input)[-1]), name = 'main_input')

conv_pool_out <- main_input %>%
  layer_conv_2d(filters = 22, kernel_size = c(1, 5)) %>%
  layer_max_pooling_2d(padding = 'same') %>%
  layer_conv_2d(filters = 44, kernel_size = c(1, 5), padding = 'same') %>%
  layer_max_pooling_2d() %>%
  layer_conv_2d(filters = 44, kernel_size = c(1, 5), padding = 'same') %>%
  layer_max_pooling_2d(padding = 'same') %>%
  layer_flatten() %>% 
  layer_dense(units = 84) 

main_output <- layer_concatenate(c(conv_pool_out, aux_input)) %>%
  layer_dense(units = 128) %>%
  layer_dense(units = 64) %>%
  layer_dense(units = 32) %>%
  layer_dense(units = 16) %>%
  layer_dense(units = 2, activation = 'sigmoid')

# create and compile model
model <- keras_model(inputs = list(main_input,aux_input), outputs = main_output)
model %>% compile(
  optimizer = 'adam',
  loss = 'mean_squared_error'
)

history <- model %>% fit(
  x = list(x_train,ball_train),
  y = y_train,
  epochs = 10,
  batch_size = 2000#,
  #validation_split = 0.3
)


lm_train <- model %>% 
  predict(list(x_train,ball_train)) %>%
  cbind(y_train) %>%
  as.data.frame()

l_mod <- lm(cbind(V3, V4) ~ V1 * V2, data = lm_train)

l_mod %>% summary

eval <- model %>% 
  evaluate(list(x_test,ball_test), y_test)
pred <- model %>% 
  predict(list(x_test,ball_test)) %>%
  as.data.frame
pred <- cbind(pred,y_test)
colnames(pred) <- c("pred_x","pred_y","obs_x","obs_y")

pred <- predict(l_mod, pred)

plot(pred[1:10000, 1], y_test[1:10000, 1] - pred[1:10000, 1], 
     xlab = 'Predicted X values', ylab = 'Error')
plot(pred[1:10000, 2], y_test[1:10000, 2] - pred[1:10000, 2], 
     xlab = 'Predicted Y values', ylab = 'Error') # For higher y values, the predictions are greater than the actual values

plot(pred[1:10000, 1] + pred[1:10000, 2], y_test[1:10000, 1] + y_test[1:10000, 2] - pred[1:10000, 1] - pred[1:10000, 2], 
     xlab = 'Predicted (X + Y) values', ylab = 'Error')

xres <- ggplot(pred[1:1000,],
               mapping = aes(x = obs_x, y =obs_x- pred_x))+
  geom_point() + 
  fte_theme() +
  labs(x = "x-coordinate",y = "Fitted Value")
yres <- ggplot(pred[1:1000,],
               mapping = aes(x = obs_y, y =obs_y- pred_y))+
  geom_point() + 
  fte_theme() +
  labs(x = "y-coordinate",y = "Fitted Value")

ggarrange(xres,yres,ncol=1,nrow=2)

#######################

#shots

#######################

input <- data$x[-c(2905),,,]
output <- data$y

ball_info <- shots %>% 
  arrange(matchNo) %>%
  #subset(matchNo %in% sub) %>%
  subset((action %in% c("shot")), 
         select = c(ballPosX,ballPosY,ballSpeed,ballAngle)) %>%
  mutate(
    ballPosY = abs(ballPosY - 35)
  ) %>%
  apply(
    FUN = function(x){x/max(x)},
    MARGIN=2
  ) %>%
  unlist %>%
  array(dim = c(3884, 4))
ball_info <- ball_info[-c(2905),]

shot_info <- (shots %>%  
  arrange(matchNo) %>%
  subset((action %in% c("shot")), 
         select = c(eventSuccess)) %>%
  unlist %>%
  array(dim=length(.))) %>%
  to_categorical()
shot_info <- shot_info[-c(2905),]

set.seed(1886)
train <- sample(nrow(input),0.8*nrow(input))
shot_train <- shot_info[train,]
shot_test <- shot_info[-train,]
ball_train <- ball_info[train,]
ball_test <- ball_info[-train,]
x_train <- input[train,,,]
y_train <- output[train,]
x_test <- input[-train,,,]
y_test <- output[-train,]


aux_input <- layer_input(shape = as.numeric(dim(ball_info)[2]), name = 'aux_input')

main_input <- layer_input(shape = as.numeric(dim(input)[-1]), name = 'main_input')

conv_pool_out <- main_input %>%
  layer_conv_2d(filters = 2, kernel_size = c(1, 5)) %>%
  layer_max_pooling_2d(padding = 'same') %>%
  layer_flatten() %>% 
  layer_dense(units = 16) 

main_output <- layer_concatenate(c(conv_pool_out, aux_input)) %>%
  layer_dense(units = 8) %>%
  layer_dense(units = 8) %>%
  layer_dense(units = 8) %>%
  layer_dense(units = 2, activation = 'sigmoid')

# create and compile model
model <- keras_model(inputs = list(main_input,aux_input), outputs = main_output)
model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metric = c('accuracy')
)

history <- model %>% fit(
  x = list(x_train,ball_train),
  y = shot_train,
  epochs = 200,
  batch_size = 2000,
  validation_split = 0.3
)


eval <- model %>% 
  evaluate(list(x_test,ball_test), shot_test)
pred <- model %>% 
  predict(list(x_test,ball_test)) %>%
  as.data.frame %>%
  summarise(
    p = ifelse(V1 > V2,
               1,
               0)
  )
pred <- cbind(pred,shot_test[,1])

aux_output <- aux_input %>%
  layer_dense(units = 8) %>%
  layer_dense(units = 8) %>%
  layer_dense(units = 8) %>%
  layer_dense(units = 2, activation = 'sigmoid')

model <- keras_model(inputs = list(aux_input), outputs = aux_output)
model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metric = c('accuracy')
)

history <- model %>% fit(
  x = list(ball_train),
  y = shot_train,
  epochs = 100,
  batch_size = 2000,
  validation_split = 0.3
)


eval <- model %>% 
  evaluate(list(ball_test), shot_test)
pred <- model %>% 
  predict(list(ball_test)) %>%
  as.data.frame %>%
  summarise(
    p = ifelse(V1 > V2,
               1,
               0)
  )
pred <- cbind(pred,shot_test[,1])

get_plot_data <- function(ev,
                          path = "C:/Users/David/OneDrive/Documents/Work/Thesis/Data/matches_formatted.zip"){
  num <- ev$matchNo %>% as.numeric
  file <- grep(paste0('.*/',num,'-'),unzip(path, list=TRUE)$Name,value=T)
  match <- unz(description = path,filename = file) %>%
    read.csv() %>%
    subset(frame == ev$frame) %>%
    mutate(
      eventEndPosX = ifelse(
        action == "shot",
        ifelse(
          team == "l",
          107,
          0
        ),
        eventEndPosX
      ),
      eventEndPosY = ifelse(
        action == "shot",
        35,
        eventEndPosY
      )
    )
    
  
  return(match)
  
}

to_arr <- function(df){
  return(df[,3:(3+4)] %>% 
           split(df$team) %>% 
           lapply(FUN = function(x){return(x[,-c(1)])}) %>%
           abind(along=3))
}

plot_event <- function(ev){
  
  ev_main <- ev %>% 
    subset(team != "b",
           select = c('frame',"matchNo","team",
                      "x","y","vX","vY")) %>%
    mutate(
      x = x/max(107),
      y = y/max(70),
      vX = vX/max(events$vX),
      vY = vY/max(events$vY),
    )%>%
    to_arr() %>%
    abind(rev.along = 0) %>%
    aperm(c(4,2,1,3)) %>%
    unname
  
  ev_ball <- ev %>% 
    subset(  !is.na(eventEndPosX) & 
             !is.na(eventEndPosY), 
           select = c(ballPosX, ballPosY, ballSpeed, ballAngle)) %>%
    mutate(
      ballPosX = ballPosX/max(events$ballPosX),
      ballPosY = ballPosY/max(events$ballPosY),
      ballSpeed = ballSpeed/max(events$ballSpeed),
      ballAngle = ballAngle/max(events$ballAngle)
    ) %>%
    unlist %>%
    array(dim = c(1, 4))
  
  pred <- model %>% 
    predict(list(ev_main,ev_ball))%>%
    as.data.frame
  pred <- l_mod %>%
    predict(pred) %>%
    as.data.frame
  colnames(pred) <- c("V1","V2")
  
  soccerPitch() + 
    geom_point(data = ev,
               mapping = aes(x = x, y = y,color = team),
               show.legend = F) +
    geom_segment(data = ev %>% subset(!(action %in% c("t","k",""))),
                 mapping = aes(x = ballPosX, y = ballPosY, 
                               xend = eventEndPosX, yend = eventEndPosY),
                 arrow = arrow(length = unit(0.1, "cm")))+
    geom_segment(data = pred,
                 mapping = aes(x = ev_ball[1]*max(events$x), y = ev_ball[2]*max(events$y), 
                               xend = V1*max(events$x), yend = V2*max(events$y)),
                 color = pal[6],
                 arrow = arrow(length = unit(0.1, "cm")))+
    fte_theme()+
    scale_color_manual(values = pal[c(5,2,4)])+
    labs(x="",y="")
}


shot <- subset(shots,matchNo %in% sub)[100,] %>%
  get_plot_data %>%
  plot_event
pass <- passes[2000,] %>%
  get_plot_data %>%
  plot_event


shot <- shots[110,] %>%
  get_plot_data %>%
  plot_event
pass <- passes[40000,] %>%
  get_plot_data %>%
  plot_event

shot_ps <- shots[110,] %>%
  get_plot_data %>%
  plot_event

pass_ps <- passes[171333,] %>%
  get_plot_data %>%
  plot_event

drib_ps <- events[500000,] %>%
  get_plot_data %>%
  plot_event


shot_all <- shots[110,] %>%
  get_plot_data %>%
  plot_event

pass_all <- passes[171333,] %>%
  get_plot_data %>%
  plot_event

drib_all <- events[500000,] %>%
  get_plot_data %>%
  plot_event

ggarrange(shot_ps,shot_all,pass_ps,pass_all,drib_ps,drib_all,ncol=2,nrow=3)
