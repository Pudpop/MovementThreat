

#read in events
events <- read.csv("C:/Users/David/OneDrive/Documents/Work/Thesis/Data/events.csv")
events_sub <- events %>%
  subset(action %in% c("shot","pass","dribble") & 
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

get_traj_data <- function(names,                     #list of names columns which will be explanatory variables 
                          path = "C:/Users/David/OneDrive/Documents/Work/Thesis/Data/matches_formatted.zip",
                          events_data = events_sub)      #data frame containing events
{
  
  matches <- events_data$matchNo %>% unique
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
    evs <- subset(match,action %in% c("shot","pass","dribble"))$frame
    temp <- match %>%
      subset( select = c('frame',"matchNo","team","player",
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


input <- data$x
output <- data$y


set.seed(1886)
train <- sample(nrow(input),0.8*nrow(input))
x_train <- input[train,,,]
y_train <- output[train,]
x_test <- input[-train,,,]
y_test <- output[-train,]

weighted_cross_entropy <- function( y_true, y_pred) {
  num_cat <- 3
  w = matrix(1,nrow=num_cat,ncol=num_cat)
  
  #w[1,3] <- 1
  w[1,3] <- 100000
  #w[3,2] <- 1
  w[2,3] <- 100000
  w[1,2] <- 25000
  w[2,1] <- 10000
  nb_cl <- nrow(w)
  
  K <- backend()
  
  final_mask <- K$zeros_like(y_pred[,1])
  y_pred_arg_max <- K$argmax(y_pred)
  y_pred_max_mat <- K$one_hot(y_pred_arg_max,num_classes=3L)
  for (i in 1:nb_cl){
    for (j in 1:nb_cl){
      final_mask = final_mask + (w[i,j] * y_pred_max_mat[,i,style="R"] * y_true[,j,style="R"])
    }
  }
  return(K$categorical_crossentropy(y_true,y_pred) * final_mask)
}

model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 22, kernel_size = c(1, 3), activation = 'relu', input_shape = c(4, 11, 2)) %>% 
  layer_max_pooling_2d() %>%
  layer_dropout(rate=0.25) %>%
  layer_flatten() %>%  
  layer_dense(units = 16, activation = 'sigmoid') %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_dense(units = 2)


model %>% compile(
  optimizer = 'adam',
  loss = "mean_squared_error",
  metrics = "mean_squared_error")

history <- model %>% fit(
  x_train, 
  y_train, 
  epochs = 30, 
  batch_size = 2000, 
  validation_split = 0.3
)

lm_train <- model %>% 
  predict(x_train) %>%
  cbind(y_train) %>%
  as.data.frame()

l_mod <- lm(cbind(V3,V4) ~V1*V2,data = lm_train)

l_mod %>% summary

eval <- model %>% 
  evaluate(x_test, y_test)
pred <- model %>% 
  predict(x_test) %>%
  as.data.frame
pred <- predict(l_mod,pred)

plot(y_test[1:10000,1],y_test[1:10000,1]-pred[1:10000,1])
plot(y_test[1:10000,2],y_test[1:10000,2]-pred[1:10000,2])
