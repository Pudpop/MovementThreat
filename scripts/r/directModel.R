library(keras)
library(pbapply)
library(abind)

get_traj_data <- function(names,                     #list of names columns which will be explanatory variables 
                          path = "C:/Users/David/OneDrive/Documents/Work/Thesis/Data/matches_formatted.zip",
                          events_data = events)      #data frame containing events
{
  
  matches <- events_data$matchNo %>% unique
  sub <- sample(size = 50,x = length(matches))
  matches <- matches[sub]
  
  to_arr <- function(df){
    return(df[,3:8] %>% 
             split(df$team) %>%
             abind(along=3))
  }

  
  get_in_out <- function(match_number){
    file <- grep(paste0('.*/',match_number,'-'),unzip(path, list=TRUE)$Name,value=T)
    match <- unz(description = path,filename = file) %>%
      read.csv()
    temp <- match %>%
      subset( select = c('frame',"matchNo","team","player",
                         names,"action")) %>%
      subset(frame %in% (subset(events_data,matchNo == match_number & 
                                  action %in% c("pass","shot","dribble")))$frame &
               player != 1) %>% 
      distinct(frame,matchNo,player,.keep_all = T) %>%
      subset(select = c('frame',"matchNo","team",
                        names,"action")) %>%
      mutate(
        action  = ifelse(action == "t","",action),
        team = team %>% as.factor() %>% as.numeric()
      )
    
    input <- (temp %>%
                subset(select = -c(action)) %>%
                group_by(frame,matchNo) %>%
                do(arr = to_arr(.)))$arr %>% 
      abind(rev.along = 0) %>%
      aperm(c(4,2,1,3)) %>%
      unname
    output <- (temp %>%
                 subset(action != "") %>%
                 mutate(
                   action = action %>% 
                     as.factor %>% 
                     as.integer
                 ) %>%
                 subset(select = action) %>%
                 unlist %>%
                 array(dim=length(.)) - 1) %>%
      to_categorical(num_classes = 3)
    
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

data <- get_traj_data(names = c("x","y","vX","vY","distToBall"))

input <- data$x
output <- data$y


set.seed(1886)
train <- sample(nrow(input),0.8*nrow(input))
x_train <- input[train,,,]
y_train <- output[train,]
x_test <- input[-train,,,]
y_test <- output[-train,]

model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 22, kernel_size = c(3,3),activation = 'relu', input_shape = c(6,11,2)) %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(rate=0.25) %>%
  layer_flatten() %>% 
  layer_dense(units = 110, activation = 'relu') %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_dense(units = 3, activation = 'softmax')

weighted_cross_entropy <- function( y_true, y_pred) {
  num_cat <- 3
  w = matrix(1,nrow=num_cat,ncol=num_cat)
  
  w[1,3] <- 100
  w[3,1] <- 100
  w[3,2] <- 100
  w[2,3] <- 100
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


model %>% compile(
  loss = weighted_cross_entropy,
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 10, batch_size = 104, 
  validation_split = 0.2
)

model %>% evaluate(x_test, y_test)
model %>% predict_classes(x_test)  

